#' Convert a parquet fplida run directory to CSV via DuckDB
#'
#' Walks `src_dir` recursively. Every `.parquet` file is rewritten as a
#' sibling `.csv` under `dst_dir`, preserving the agency-dataset folder
#' structure, except datasets listed in `preserve_parquet_datasets`.
#' Preserved datasets are linked/copied as parquet. When
#' `messy_files = TRUE`, each PLIDA data product is written to its own
#' top-level `<product>/` folder, agency spine files are written once under
#' top-level `<agency>-spine-v6/` folders, and agency-dataset folders are
#' omitted. BLADE products keep the same product-folder structure under
#' `abs-blade/`, and STP parquet products keep it under
#' `ato-stp/stp-standard/` or `ato-stp/stp-extended/`.
#' Partitioned product directories (containing `part-NNN.parquet` files) are
#' rewritten as a single unified CSV at `<product>/<product>.csv`, or under
#' the dataset exception folder for BLADE.
#'
#' DuckDB's `COPY (SELECT * FROM read_parquet(...)) TO 'x.csv' (FORMAT CSV,
#' HEADER)` pipeline was benchmarked on 1 GB 30m fplida census data at
#' roughly 6x arrow::write_csv_arrow and 85x arrow::read_parquet +
#' data.table::fwrite. It streams end-to-end in C++ without any R-side
#' data.frame materialisation. Row totals are read from Parquet row-group
#' metadata, avoiding a second full data scan after each CSV write.
#'
#' @param src_dir Character. Parquet run directory (e.g. fplida_30m/).
#' @param dst_dir Character. Output root directory. Created if missing.
#'   If NULL, uses `paste0(src_dir, "_csv")`.
#' @param threads Integer. `PRAGMA threads` — DuckDB parallelism. Default
#'   is `NULL`, which uses `parallel::detectCores()` capped at 10. On a host
#'   where core detection fails, this falls back to 1.
#' @param memory_limit Character. `PRAGMA memory_limit`. Default "4GB".
#' @param log_file Character or NULL. Path to a plain-text log. NULL
#'   skips logging (default).
#' @param verbose Logical. If TRUE, print per-file progress to the
#'   console (default TRUE when interactive, FALSE otherwise).
#' @param preserve_parquet_datasets Character vector of top-level dataset
#'   directory names to keep as parquet under `dst_dir`, e.g. `"ato-stp"`.
#' @param messy_files Logical. If TRUE, emit each PLIDA data product as a
#'   top-level `<product>/<product>.csv` folder, preserve STP parquet products
#'   under `ato-stp/stp-standard/<product>/` or
#'   `ato-stp/stp-extended/<product>/`, emit BLADE products under
#'   `abs-blade/<product>/`, and emit agency spines as top-level
#'   `<agency>-spine-v6/<agency>-spine-v6.csv` folders instead of keeping
#'   other agency-dataset folders.
#' @param messy_names Logical. If TRUE, vary a small subset of variable identifiers
#'   across related PIT/PAYG, MBS, and PBS year products by using case changes
#'   and close aliases.
#'
#' @return Invisibly, a list with fields:
#'   \describe{
#'     \item{total_files}{Number of parquet inputs converted}
#'     \item{total_rows}{Total row count across all files}
#'     \item{total_bytes}{Total CSV output bytes}
#'     \item{total_elapsed_sec}{Wall-clock time in seconds}
#'     \item{per_file}{data.frame of per-file timings}
#'   }
#' @export
convert_parquet_dir_to_csv <- function(src_dir,
                                       dst_dir      = NULL,
                                       threads      = NULL,
                                       memory_limit = "4GB",
                                       log_file     = NULL,
                                       verbose      = interactive(),
                                       preserve_parquet_datasets = character(),
                                       messy_files  = TRUE,
                                       messy_names  = TRUE) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required for CSV conversion. ",
         "Install it with: install.packages('duckdb')", call. = FALSE)
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required for CSV conversion. ",
         "Install it with: install.packages('DBI')", call. = FALSE)
  }
  stopifnot("`src_dir` does not exist" = dir.exists(src_dir))
  if (is.null(threads)) {
    threads <- tryCatch(parallel::detectCores(), error = function(e) 1L)
  }
  # detectCores() returns NA in some containers and on some hosts.
  if (length(threads) != 1L || is.na(threads) || threads < 1L) threads <- 1L
  threads <- min(as.integer(threads), 10L)
  if (is.null(dst_dir)) dst_dir <- paste0(src_dir, "_csv")
  if (!dir.exists(dst_dir)) dir.create(dst_dir, recursive = TRUE)
  preserve_parquet_datasets <- as.character(preserve_parquet_datasets)
  messy_files <- isTRUE(messy_files)
  messy_names <- isTRUE(messy_names)

  log_line <- function(msg) {
    ts   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    line <- sprintf("[%s] %s", ts, msg)
    if (verbose) { cat(line, "\n", sep = ""); flush.console() }
    if (!is.null(log_file)) {
      cat(line, "\n", sep = "", file = log_file, append = TRUE)
    }
  }
  fmt_time <- function(s) {
    if (s < 60) sprintf("%.1fs", s)
    else if (s < 3600) sprintf("%.1fm", s / 60)
    else sprintf("%.2fh", s / 3600)
  }
  escape_sql <- function(path) gsub("'", "''", path, fixed = TRUE)
  rel_path <- function(path, root) {
    substring(path, nchar(root) + 2L)
  }
  link_or_copy_file <- function(src, dst) {
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(dst)) unlink(dst)
    linked <- tryCatch(
      file.link(src, dst),
      warning = function(w) FALSE,
      error = function(e) FALSE
    )
    if (!isTRUE(linked)) {
      ok <- file.copy(src, dst, overwrite = TRUE, copy.date = TRUE)
      if (!isTRUE(ok)) stop("Could not copy preserved parquet file: ", src,
                            call. = FALSE)
    }
    invisible(TRUE)
  }
  preserve_tree <- function(src, dst, skip = function(path) FALSE) {
    if (dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE)
    dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    dirs <- list.dirs(src, recursive = TRUE, full.names = TRUE)
    for (d in dirs) {
      rel <- rel_path(d, src)
      if (nzchar(rel)) {
        dir.create(file.path(dst, rel), recursive = TRUE, showWarnings = FALSE)
      }
    }
    files <- list.files(src, recursive = TRUE, all.files = TRUE,
                        no.. = TRUE, full.names = TRUE)
    if (length(files)) {
      keep <- !vapply(files, skip, logical(1))
      files <- files[keep]
    }
    for (f in files) {
      link_or_copy_file(f, file.path(dst, rel_path(f, src)))
    }
    length(files)
  }
  quote_ident <- function(x) {
    paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
  }
  parquet_column_names <- function(src_glob) {
    q <- sprintf(
      "DESCRIBE SELECT * FROM read_parquet('%s')",
      escape_sql(src_glob)
    )
    DBI::dbGetQuery(con, q)$column_name
  }
  product_year <- function(product_name) {
    year <- regmatches(product_name, regexpr("(?<=-)\\d{4}$", product_name,
                                            perl = TRUE))
    if (length(year) && nzchar(year)) return(as.integer(year))

    fy <- regmatches(product_name, regexpr("fy\\d{4}", product_name,
                                          perl = TRUE))
    if (length(fy) && nzchar(fy)) {
      yy <- as.integer(substr(fy, 5L, 6L))
      return(if (yy <= 50L) 2000L + yy else 1900L + yy)
    }
    NA_integer_
  }
  dataset_from_folder <- function(ds_name) {
    stem <- sub("^[^-]+-", "", ds_name)
    toupper(gsub("-", "_", stem, fixed = TRUE))
  }
  messy_rename_map <- function(ds_name, product_name) {
    # Product--table files are canonical DIL schemas. Preserve their exact
    # published column names even when the surrounding CSV build requests
    # historical naming variation for richer generated products.
    if (!messy_names || grepl("--", product_name, fixed = TRUE)) {
      return(character())
    }

    dataset <- dataset_from_folder(ds_name)
    year <- product_year(product_name)
    if (is.na(year)) return(character())

    out <- switch(dataset,
      PIT_PS = {
        if (year %% 3L == 0L) {
          c(GRS_AMT = "gross_amt",
            TAX_WHELD_AMT = "tax_wheld_amt",
            TOTL_TXBL_AMT = "TOTAL_TXBL_AMT")
        } else if (year %% 3L == 1L) {
          c(GRS_AMT = "GROSS_AMT",
            TAX_WHELD_AMT = "TAXWITHHELD_AMT",
            PERD_PMT_DT = "PMT_DATE")
        } else character()
      },
      PIT_ITR = {
        if (grepl("context", product_name, fixed = TRUE) &&
            year %% 2L == 0L) {
          c(INCM_YR = "income_year",
            AGE_FY_START = "AGE_AT_FY_START",
            TAXABLE_STATUS = "Taxable_Status")
        } else if (grepl("inc-loss", product_name, fixed = TRUE) &&
                   year %% 2L == 1L) {
          c(SYNTHETIC_AEUID = "synthetic_aeuid",
            INCM_YR = "Income_Year")
        } else character()
      },
      MBS = {
        if (year %% 3L == 0L) {
          c(ITEM = "ITEMNUM",
            FEECHARGED = "FEE_CHARGED",
            BENPAID = "BEN_PAID")
        } else if (year %% 3L == 1L) {
          c(DOS = "date_of_service",
            DOP = "date_of_processing",
            NUMSERV = "num_services")
        } else character()
      },
      PBS = {
        if (year %% 3L == 0L) {
          c(ITM_CD = "ITEM_CD",
            BNFT_AMT = "BENEFIT_AMT",
            PRSCRPTN_CNT = "PRESCRIPTION_CNT")
        } else if (year %% 3L == 1L) {
          c(ITM_CD = "itm_cd",
            SPPLY_DT = "supply_date",
            PTNT_CNTRBTN_AMT = "patient_contribution_amt")
        } else character()
      },
      character()
    )
    out
  }
  select_sql <- function(src_glob, rename_map) {
    if (!length(rename_map)) {
      return(sprintf("SELECT * FROM read_parquet('%s')", escape_sql(src_glob)))
    }
    cols <- parquet_column_names(src_glob)
    rename_map <- rename_map[names(rename_map) %in% cols]
    if (!length(rename_map)) {
      return(sprintf("SELECT * FROM read_parquet('%s')", escape_sql(src_glob)))
    }
    pieces <- vapply(cols, function(col) {
      out_name <- if (col %in% names(rename_map)) rename_map[[col]] else col
      sprintf("%s AS %s", quote_ident(col), quote_ident(out_name))
    }, character(1))
    sprintf(
      "SELECT %s FROM read_parquet('%s')",
      paste(pieces, collapse = ", "),
      escape_sql(src_glob)
    )
  }

  if (!is.null(log_file)) cat("", file = log_file)
  log_line("=== convert_parquet_dir_to_csv starting ===")
  log_line(sprintf("src    : %s", src_dir))
  log_line(sprintf("dst    : %s", dst_dir))
  log_line(sprintf("threads: %d", threads))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d", threads))
  DBI::dbExecute(con, sprintf("PRAGMA memory_limit = '%s'", memory_limit))
  DBI::dbExecute(con, "PRAGMA preserve_insertion_order = false")

  # --- copy_one: rewrite a single source (either a parquet path OR a
  #     glob matching multiple partition files) to a target CSV. ---
  count_rows_metadata <- function(src_glob) {
    q <- sprintf(
      paste(
        "SELECT COALESCE(sum(row_group_num_rows), 0) AS n",
        "FROM (",
        "  SELECT DISTINCT file_name, row_group_id, row_group_num_rows",
        "  FROM parquet_metadata('%s')",
        ")"
      ),
      escape_sql(src_glob)
    )
    n <- tryCatch(
      DBI::dbGetQuery(con, q)$n[[1L]],
      error = function(e) NA_real_
    )
    if (!is.na(n)) return(n)

    # Fallback for older DuckDB builds or non-standard parquet metadata.
    DBI::dbGetQuery(con, sprintf(
      "SELECT count(*) AS n FROM read_parquet('%s')",
      escape_sql(src_glob)
    ))$n[[1L]]
  }

  copy_one <- function(src_glob, csv_out, rename_map = character()) {
    t0 <- proc.time()
    DBI::dbExecute(con, sprintf(
      "COPY (%s) TO '%s' (FORMAT CSV, HEADER)",
      select_sql(src_glob, rename_map), escape_sql(csv_out)
    ))
    elapsed <- (proc.time() - t0)[["elapsed"]]
    size    <- file.size(csv_out)
    n_rows  <- count_rows_metadata(src_glob)
    list(n_rows = n_rows, elapsed = elapsed, size = size)
  }

  per_file    <- list()
  total_bytes <- 0
  total_rows  <- 0
  total_files <- 0
  overall_t0  <- proc.time()
  converted_spines <- new.env(parent = emptyenv())

  record_conversion <- function(path_label, res) {
    total_files <<- total_files + 1L
    total_rows  <<- total_rows  + res$n_rows
    total_bytes <<- total_bytes + res$size
    per_file[[length(per_file) + 1L]] <<- data.frame(
      path    = path_label,
      n_rows  = res$n_rows,
      elapsed = res$elapsed,
      size_mb = res$size / 1e6,
      stringsAsFactors = FALSE
    )
  }
  is_agency_spine <- function(path) {
    grepl("^[a-z]+-spine\\.parquet$", basename(path))
  }
  stp_category <- function(stem) {
    if (startsWith(stem, "stp_standard_")) return("stp-standard")
    if (startsWith(stem, "stp_extended_")) return("stp-extended")
    "stp-other"
  }
  messy_product_prefix <- function(stem, ds_name) {
    if (!isTRUE(messy_files)) return("")
    if (identical(ds_name, "abs-blade")) return("abs-blade")
    if (identical(ds_name, "ato-stp")) {
      return(file.path("ato-stp", stp_category(stem)))
    }
    ""
  }
  product_dir <- function(stem, ds_name) {
    prefix <- messy_product_prefix(stem, ds_name)
    if (nzchar(prefix)) {
      file.path(dst_dir, prefix, stem)
    } else {
      file.path(dst_dir, stem)
    }
  }
  product_csv_path <- function(stem, ds_name) {
    file.path(product_dir(stem, ds_name), paste0(stem, ".csv"))
  }
  product_rel_path <- function(stem, ds_name, ext = ".csv") {
    path <- file.path(stem, paste0(stem, ext))
    prefix <- messy_product_prefix(stem, ds_name)
    if (length(prefix) && nzchar(prefix)) {
      path <- file.path(prefix, path)
    }
    path
  }
  convert_spine_once <- function(pq) {
    if (!messy_files || !is_agency_spine(pq)) return(FALSE)

    agency <- sub("-spine\\.parquet$", "", basename(pq))
    if (isTRUE(converted_spines[[agency]])) return(TRUE)

    stem <- paste0(agency, "-spine-v6")
    csv_out <- file.path(dst_dir, stem, paste0(stem, ".csv"))
    dir.create(dirname(csv_out), recursive = TRUE, showWarnings = FALSE)
    res <- tryCatch(copy_one(pq, csv_out),
                    error = function(e) {
                      log_line(sprintf("!!! %s FAILED: %s",
                                       basename(pq), conditionMessage(e)))
                      NULL
                    })
    if (is.null(res)) return(TRUE)

    converted_spines[[agency]] <- TRUE
    record_conversion(file.path(stem, paste0(stem, ".csv")), res)
    log_line(sprintf("  %-55s n=%12s  %s  %.1fMB",
                     paste0(stem, "/", stem, ".csv"),
                     format(res$n_rows, big.mark = ","),
                     fmt_time(res$elapsed),
                     res$size / 1e6))
    TRUE
  }
  preserve_messy_dataset <- function(ds_dir, ds_name) {
    n_preserved <- 0L

    flat_files <- list.files(ds_dir, recursive = FALSE, all.files = TRUE,
                             no.. = TRUE, full.names = TRUE)
    flat_files <- flat_files[file.exists(flat_files) & !dir.exists(flat_files)]
    for (f in flat_files) {
      if (is_agency_spine(f)) next
      stem <- sub("\\.[^.]+$", "", basename(f))
      link_or_copy_file(f, file.path(product_dir(stem, ds_name), basename(f)))
      n_preserved <- n_preserved + 1L
    }

    subdirs <- list.dirs(ds_dir, recursive = FALSE, full.names = TRUE)
    for (sub in subdirs) {
      n_preserved <- n_preserved + preserve_tree(
        sub,
        product_dir(basename(sub), ds_name),
        skip = is_agency_spine
      )
    }

    n_preserved
  }

  datasets <- list.dirs(src_dir, recursive = FALSE, full.names = TRUE)
  for (ds_dir in datasets) {
    ds_name <- basename(ds_dir)
    dst_ds  <- file.path(dst_dir, ds_name)
    if (!messy_files && !dir.exists(dst_ds)) {
      dir.create(dst_ds, recursive = TRUE)
    }
    log_line(sprintf("--- %s ---", ds_name))

    if (messy_files && identical(ds_name, "_system")) {
      spine_files <- list.files(ds_dir, pattern = "^[a-z]+-spine\\.parquet$",
                                full.names = TRUE)
      for (pq in spine_files) convert_spine_once(pq)
      system_files <- list.files(ds_dir, recursive = TRUE, all.files = TRUE,
                                 no.. = TRUE, full.names = TRUE)
      system_files <- system_files[file.exists(system_files) &
                                   !dir.exists(system_files)]
      skipped <- length(system_files) -
        length(spine_files)
      log_line(sprintf("  skipped internal system files (%d files)", skipped))
      next
    }

    if (ds_name %in% preserve_parquet_datasets) {
      if (messy_files) {
        spine_files <- list.files(ds_dir, pattern = "^[a-z]+-spine\\.parquet$",
                                  full.names = TRUE)
        for (pq in spine_files) convert_spine_once(pq)
        n_preserved <- preserve_messy_dataset(ds_dir, ds_name)
      } else {
        n_preserved <- preserve_tree(ds_dir, dst_ds)
      }
      log_line(sprintf("  preserved parquet dataset (%d files)", n_preserved))
      next
    }

    # (a) Flat parquet files → one CSV each
    pq_files <- list.files(ds_dir, pattern = "\\.parquet$", full.names = TRUE)
    for (pq in pq_files) {
      if (convert_spine_once(pq)) next

      stem    <- sub("\\.parquet$", "", basename(pq))
      csv_out <- if (messy_files) {
        product_csv_path(stem, ds_name)
      } else {
        file.path(dst_ds, paste0(stem, ".csv"))
      }
      dir.create(dirname(csv_out), recursive = TRUE, showWarnings = FALSE)
      res     <- tryCatch(copy_one(pq, csv_out, messy_rename_map(ds_name, stem)),
                          error = function(e) {
                            log_line(sprintf("!!! %s FAILED: %s",
                                             basename(pq), conditionMessage(e)))
                            NULL
                          })
      if (is.null(res)) next
      path_label <- if (messy_files) {
        product_rel_path(stem, ds_name)
      } else {
        file.path(ds_name, paste0(stem, ".csv"))
      }
      record_conversion(path_label, res)
      log_line(sprintf("  %-55s n=%12s  %s  %.1fMB",
                       path_label,
                       format(res$n_rows, big.mark = ","),
                       fmt_time(res$elapsed),
                       res$size / 1e6))
    }

    # (b) Partitioned product subdirectories → single unified CSV
    subdirs <- list.dirs(ds_dir, recursive = FALSE, full.names = TRUE)
    for (sub in subdirs) {
      parts <- list.files(sub, pattern = "^part-.*\\.parquet$",
                          full.names = TRUE)
      if (length(parts) == 0L) next
      stem    <- basename(sub)
      csv_out <- if (messy_files) {
        product_csv_path(stem, ds_name)
      } else {
        file.path(dst_ds, paste0(stem, ".csv"))
      }
      dir.create(dirname(csv_out), recursive = TRUE, showWarnings = FALSE)
      glob    <- file.path(sub, "part-*.parquet")
      res     <- tryCatch(copy_one(glob, csv_out, messy_rename_map(ds_name, stem)),
                          error = function(e) {
                            log_line(sprintf("!!! %s FAILED: %s",
                                             stem, conditionMessage(e)))
                            NULL
                          })
      if (is.null(res)) next
      path_label <- if (messy_files) {
        product_rel_path(stem, ds_name)
      } else {
        file.path(ds_name, paste0(stem, ".csv"))
      }
      record_conversion(path_label, res)
      log_line(sprintf("  %-55s n=%12s  %s  %.1fMB",
                       paste0(path_label, " (", length(parts), " parts)"),
                       format(res$n_rows, big.mark = ","),
                       fmt_time(res$elapsed),
                       res$size / 1e6))
    }
  }

  total_elapsed <- (proc.time() - overall_t0)[["elapsed"]]

  log_line("=== convert_parquet_dir_to_csv complete ===")
  log_line(sprintf("files written : %d", total_files))
  log_line(sprintf("total rows    : %s", format(total_rows, big.mark = ",")))
  log_line(sprintf("total size    : %.2f GB",
                   total_bytes / (1024 * 1024 * 1024)))
  log_line(sprintf("total elapsed : %s", fmt_time(total_elapsed)))

  per_file_df <- if (length(per_file)) do.call(rbind, per_file) else
                 data.frame()

  invisible(list(
    total_files       = total_files,
    total_rows        = total_rows,
    total_bytes       = total_bytes,
    total_elapsed_sec = total_elapsed,
    per_file          = per_file_df
  ))
}

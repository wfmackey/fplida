# Slice-based build utilities.
#
# Supporting functions for build_fplida_sliced(): splitting the spine
# into slice spines, creating per-slice run directories, merging slice
# outputs back into a canonical run directory, and Rayon thread
# management.

#' Compute slice row ranges for N persons and K slices
#'
#' Returns a list of length K, each element a length-2 integer vector
#' giving (start, end) — 1-based, inclusive — so row `spine[start:end, ]`
#' belongs to slice id.
#'
#' @param n Integer. Total persons.
#' @param k Integer. Number of slices.
#' @return List of K integer vectors of length 2.
#' @noRd
compute_slice_ranges <- function(n, k) {
  n <- as.integer(n)
  k <- as.integer(k)
  stopifnot(n > 0L, k > 0L, k <= n)
  # Even split with remainder distributed to first `rem` slices.
  base <- n %/% k
  rem  <- n %%  k
  ranges <- vector("list", k)
  cursor <- 1L
  for (i in seq_len(k)) {
    sz <- base + if (i <= rem) 1L else 0L
    ranges[[i]] <- c(cursor, cursor + sz - 1L)
    cursor <- cursor + sz
  }
  ranges
}


#' Materialise all slice run directories from a single spine read
#'
#' Reads the canonical full-spine parquet file into an Arrow Table
#' *once* and uses zero-copy `$slice(offset, length)` to produce each
#' slice spine. Writes each slice via `arrow::write_parquet` directly
#' on the Arrow table — no R data.frame ever. At 25M this is
#' ~3 orders of magnitude faster than R data.frame row subsetting,
#' because R's copy-on-modify for a 25M x 48-col data.frame triggers
#' ~30B elementwise operations per slice and swap thrash.
#'
#' @param canonical_spine_path Character. Path to full spine parquet.
#' @param ranges List of length-2 integer vectors (start, end), 1-based
#'   inclusive. One entry per slice.
#' @param parent_dir Character. Parent directory for slice run_dirs.
#' @param n_total Integer. Total N (used in dir name).
#' @param seed Integer. Base seed (recorded in metadata).
#' @return Character vector of slice run_dir paths (in slice id order).
#' @noRd
materialise_all_slice_spines <- function(canonical_spine_path,
                                         ranges,
                                         parent_dir, n_total, seed) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for sliced builds.", call. = FALSE)
  }
  k <- length(ranges)
  message("  reading canonical spine as Arrow table...")
  reader <- arrow::ParquetFileReader$create(canonical_spine_path)
  full_tbl <- reader$ReadTable()

  # If BLADE has produced a business spine, extract a thin `bn`-only pool so
  # each slice worker can draw employer identifiers (PIT_PS, STP) from real
  # BLADE businesses. The full business spine lives only in the canonical run
  # dir; copying just `bn` keeps slice setup cheap at scale.
  business_spine_path <- file.path(dirname(canonical_spine_path),
                                   "business-spine.parquet")
  business_bn_tbl <- NULL
  if (file.exists(business_spine_path)) {
    business_bn_tbl <- tryCatch(
      read_parquet_safely(business_spine_path, col_select = "bn",
                          as_data_frame = FALSE),
      error = function(e) NULL
    )
  }
  # Arrow slice is zero-copy; the underlying buffers stay alive in
  # `full_tbl` for the duration of this function.

  slice_run_dirs <- character(k)
  canonical_label <- run_dir_name(n_total)
  for (i in seq_len(k)) {
    slice_id  <- i - 1L
    row_range <- ranges[[i]]
    # Arrow slice uses 0-based offset + length
    offset_i <- row_range[1] - 1L
    length_i <- row_range[2] - row_range[1] + 1L

    slice_dir_name <- paste0(canonical_label,
                             sprintf("_slice%03d", slice_id))
    slice_run_dir <- file.path(parent_dir, slice_dir_name)
    sys_dir <- file.path(slice_run_dir, "_system")
    if (!dir.exists(sys_dir)) dir.create(sys_dir, recursive = TRUE)

    slice_tbl <- full_tbl$Slice(offset_i, length_i)
    slice_spine_path <- file.path(sys_dir, "base-spine.parquet")
    arrow::write_parquet(slice_tbl, slice_spine_path)

    # Thin business-number pool for this slice's employer-linked products.
    if (!is.null(business_bn_tbl)) {
      arrow::write_parquet(business_bn_tbl,
                           file.path(sys_dir, "business-bn-pool.parquet"))
    }

    meta_lines <- c(
      "# fplida slice run metadata",
      "",
      sprintf("- **N_total**: %d", n_total),
      sprintf("- **Slice**: %d", slice_id),
      sprintf("- **Row range**: [%d, %d]", row_range[1], row_range[2]),
      sprintf("- **Slice N**: %d", length_i),
      sprintf("- **Base seed**: %d", seed),
      sprintf("- **Created**: %s",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
    )
    writeLines(meta_lines, file.path(sys_dir, "metadata.md"))

    slice_run_dirs[i] <- slice_run_dir
  }
  rm(full_tbl, reader)
  gc()
  slice_run_dirs
}


#' Decide Rayon threads per worker
#'
#' Given K workers and the detected core count, return a per-worker
#' thread budget. At least 1.
#'
#' @param k Integer. Number of workers.
#' @param cores Integer or NULL. Detected cores; NULL uses parallel::detectCores().
#' @return Integer. Threads per worker.
#' @noRd
rayon_threads_per_worker <- function(k, cores = NULL) {
  if (is.null(cores)) {
    cores <- tryCatch(parallel::detectCores(logical = FALSE),
                      error = function(e) 1L)
    if (is.na(cores) || cores < 1L) cores <- 1L
  }
  max(1L, cores %/% k)
}


#' Merge slice output files into the canonical run directory
#'
#' Walks each slice run directory and moves every product file into
#' the canonical run_dir's `{dataset}/{product}/part-{SS}.parquet`
#' layout. Agency spines are handled separately (concatenated).
#'
#' Two file classes:
#'   1. Single-file products (one `.parquet` under dataset dir)
#'      → `{canonical}/{dataset}/{product}/part-{SS}.parquet`
#'   2. Already-chunked products (a directory of `part-NNN.parquet`)
#'      → `{canonical}/{dataset}/{product}/part-{SS}-{NNN}.parquet`
#'
#' Agency spines (`{agency}-spine.parquet`) are concatenated into a
#' single canonical file per agency.
#'
#' @param slice_run_dirs Character vector of slice run_dir paths
#'   (in slice id order).
#' @param canonical_run_dir Character. Canonical run_dir to write into.
#' @param format Character. "parquet" or "csv".
#' @return Invisibly, a list with merge stats.
#' @noRd
merge_slice_outputs <- function(slice_run_dirs, canonical_run_dir,
                                format = "parquet") {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required.", call. = FALSE)
  }
  ext <- if (format == "parquet") "parquet" else "csv"

  stats <- list(
    moved_files = 0L,
    merged_agency_spines = 0L,
    fast_merged_agency_spines = 0L
  )
  # Agency spine buffer: keyed by "{ds_name}||{spine_filename}" so that
  # an agency spine appearing in multiple datasets is concatenated
  # independently per dataset.
  agency_spine_buf <- new.env(parent = emptyenv())

  for (s in seq_along(slice_run_dirs)) {
    slice_idx <- s - 1L  # 0-based for file naming
    sdir <- slice_run_dirs[[s]]
    if (!dir.exists(sdir)) next

    # Dataset subfolders (skip _system)
    ds_dirs <- list.dirs(sdir, recursive = FALSE, full.names = TRUE)
    ds_dirs <- ds_dirs[basename(ds_dirs) != "_system"]

    for (ds_dir in ds_dirs) {
      ds_name <- basename(ds_dir)
      dest_ds_dir <- file.path(canonical_run_dir, ds_name)
      if (!dir.exists(dest_ds_dir)) {
        dir.create(dest_ds_dir, recursive = TRUE)
      }

      entries <- list.files(ds_dir, full.names = TRUE, include.dirs = TRUE)

      for (ent in entries) {
        bname <- basename(ent)

        # Agency spine files: accumulate for concatenation, keyed by
        # dataset name + filename.
        if (grepl("-spine\\.(parquet|csv)$", bname)) {
          key <- paste0(ds_name, "||", bname)
          if (is.null(agency_spine_buf[[key]])) {
            agency_spine_buf[[key]] <- character(0)
          }
          agency_spine_buf[[key]] <- c(agency_spine_buf[[key]], ent)
          next
        }

        if (dir.info_is_dir(ent)) {
          # Class 2: pre-chunked product directory
          dest_prod_dir <- file.path(dest_ds_dir, bname)
          if (!dir.exists(dest_prod_dir)) {
            dir.create(dest_prod_dir, recursive = TRUE)
          }
          parts <- list.files(ent, pattern = sprintf("\\.%s$", ext),
                              full.names = TRUE)
          for (p in parts) {
            part_bname <- basename(p)
            # part-NNN.parquet -> part-{SS}-{NNN}.parquet
            nnn <- sub(paste0("^part-(\\d+)\\.", ext, "$"), "\\1", part_bname)
            new_name <- sprintf("part-%03d-%s.%s", slice_idx, nnn, ext)
            move_file(p, file.path(dest_prod_dir, new_name))
            stats$moved_files <- stats$moved_files + 1L
          }
          # Remove the now-empty source dir
          unlink(ent, recursive = TRUE)
        } else if (grepl(paste0("\\.", ext, "$"), bname)) {
          # Class 1: single-file product
          # foo.parquet -> foo/part-{SS}.parquet
          stem <- sub(paste0("\\.", ext, "$"), "", bname)
          dest_prod_dir <- file.path(dest_ds_dir, stem)
          if (!dir.exists(dest_prod_dir)) {
            dir.create(dest_prod_dir, recursive = TRUE)
          }
          new_name <- sprintf("part-%03d.%s", slice_idx, ext)
          move_file(ent, file.path(dest_prod_dir, new_name))
          stats$moved_files <- stats$moved_files + 1L
        }
      }
    }
  }

  # Concatenate agency spines. Keys are "ds_name||spine_filename".
  for (key in ls(agency_spine_buf)) {
    paths <- agency_spine_buf[[key]]
    parts_split <- strsplit(key, "\\|\\|", fixed = FALSE)[[1]]
    ds_name       <- parts_split[1]
    spine_fname   <- parts_split[2]

    dest <- file.path(canonical_run_dir, ds_name, spine_fname)
    if (!dir.exists(dirname(dest))) {
      dir.create(dirname(dest), recursive = TRUE)
    }

    used_fast_merge <- FALSE
    if (endsWith(spine_fname, ".parquet")) {
      used_fast_merge <- tryCatch(
        merge_parquet_files_duckdb(paths, dest),
        error = function(e) {
          warning("DuckDB agency-spine merge failed; falling back to Arrow/R: ",
                  conditionMessage(e), call. = FALSE)
          FALSE
        }
      )
    }

    if (!used_fast_merge) {
      parts <- lapply(paths, function(p) {
        if (endsWith(p, ".parquet")) as.data.frame(read_parquet_safely(p))
        else read.csv(p, stringsAsFactors = FALSE)
      })
      combined <- do.call(rbind, parts)

      if (endsWith(spine_fname, ".parquet")) {
        arrow::write_parquet(combined, dest)
      } else {
        write.csv(combined, dest, row.names = FALSE)
      }
    } else {
      stats$fast_merged_agency_spines <-
        stats$fast_merged_agency_spines + 1L
    }

    # Remove all slice source spines for this key
    for (src in paths) {
      if (file.exists(src)) file.remove(src)
    }
    stats$merged_agency_spines <- stats$merged_agency_spines + 1L
  }

  invisible(stats)
}


#' Concatenate parquet files through DuckDB without R materialisation
#'
#' @param paths Character vector of parquet input files, in row order.
#' @param dest Character. Output parquet file.
#' @return TRUE if the DuckDB path was used, FALSE if dependencies are missing.
#' @noRd
merge_parquet_files_duckdb <- function(paths, dest) {
  if (!requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) {
    return(FALSE)
  }

  escape_sql <- function(x) gsub("'", "''", x, fixed = TRUE)
  sql_paths <- paste0(
    "[",
    paste(sprintf("'%s'", escape_sql(paths)), collapse = ", "),
    "]"
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  cores <- tryCatch(parallel::detectCores(), error = function(e) 1L)
  if (is.na(cores) || cores < 1L) cores <- 1L
  DBI::dbExecute(con, sprintf("PRAGMA threads = %d", min(cores, 10L)))
  DBI::dbExecute(con, "PRAGMA preserve_insertion_order = false")

  if (file.exists(dest)) unlink(dest)
  DBI::dbExecute(con, sprintf(
    paste(
      "COPY (",
      "  SELECT * FROM read_parquet(%s, union_by_name = true)",
      ") TO '%s' (FORMAT PARQUET, COMPRESSION SNAPPY)"
    ),
    sql_paths,
    escape_sql(dest)
  ))
  TRUE
}


#' Helper: is this path a directory?
#' @noRd
dir.info_is_dir <- function(path) {
  info <- file.info(path)
  isTRUE(info$isdir)
}


#' Clean up slice run directories after successful merge
#'
#' @param slice_run_dirs Character vector of slice run_dir paths.
#' @return Invisibly, the number of directories removed.
#' @noRd
cleanup_slice_dirs <- function(slice_run_dirs) {
  removed <- 0L
  for (sdir in slice_run_dirs) {
    if (dir.exists(sdir)) {
      unlink(sdir, recursive = TRUE)
      removed <- removed + 1L
    }
  }
  invisible(removed)
}

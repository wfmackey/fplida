# Export infrastructure: folder structure, product naming, format handling.
#
# Data is organised as:
#   data_path/
#     fplida_{N}/              <- run directory (N rounded to nearest 1k)
#       _system/
#         base-spine.parquet   <- the person spine
#         metadata.md          <- N, seed, creation timestamp
#       abs-census/
#         [product].parquet    <- Census products
#         abs-spine.parquet    <- agency linkage spine
#       ato-pit_ps/
#         [product].parquet    <- PIT_PS products
#         ato-spine.parquet    <- agency linkage spine
#       ...

#' Read a parquet file without leaving it memory-mapped on Windows
#'
#' \code{arrow::read_parquet()} memory-maps by default. Windows then refuses to
#' recreate or rename that file while the mapping is open, failing with
#' \code{os error 1224}, ERROR_USER_MAPPED_FILE. The build rewrites its own
#' output — the person spine most of all — so every read has to release the
#' file. R's garbage collector clears the mapping eventually, but not
#' predictably, so ask arrow not to map in the first place.
#'
#' Memory mapping is kept on the other platforms, where it is faster and
#' harmless.
#'
#' @param file Path to a parquet file.
#' @param ... Passed to \code{arrow::read_parquet()}.
#' @return Whatever \code{arrow::read_parquet()} returns.
#' @keywords internal
#' @noRd
read_parquet_safely <- function(file, ...) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to read parquet files.", call. = FALSE)
  }
  arrow::read_parquet(file, ..., mmap = !on_windows())
}

on_windows <- function() {
  identical(.Platform$OS.type, "windows")
}


#' Move a file, falling back to copy-then-delete across filesystems
#'
#' \code{file.rename()} fails when the source and destination are on
#' different filesystems. It returns FALSE and signals only a warning, so an
#' unchecked call silently leaves the source in place. Slice output lives
#' under \code{tempdir()} while the run directory follows
#' \code{set_data_path()}, so the two are routinely on different volumes.
#'
#' @param from Source path.
#' @param to Destination path.
#' @return \code{TRUE}, invisibly. Errors if the file could not be moved.
#' @keywords internal
#' @noRd
move_file <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- tryCatch(
    suppressWarnings(file.rename(from, to)),
    error = function(e) FALSE
  )
  if (!isTRUE(ok)) {
    ok <- tryCatch(
      file.copy(from, to, overwrite = TRUE, copy.date = TRUE),
      error = function(e) FALSE
    )
    if (isTRUE(ok) && unlink(from) != 0L) {
      # The copy succeeded, so the destination is good, but the source is
      # still there. Windows refuses to delete a file another process holds
      # open, which is also what sent us down this branch.
      warning("Copied '", from, "' to '", to,
              "' but could not remove the source.", call. = FALSE)
    }
  }
  if (!isTRUE(ok)) {
    stop("Could not move '", from, "' to '", to,
         "'. Check free space and that both locations are writable.",
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Resolve the base output directory (compulsory)
#'
#' Checks \code{output_dir} argument, then \code{get_data_path()}.
#' Errors if neither is set — users must call \code{set_data_path()} or
#' provide \code{output_dir} explicitly.
#'
#' @param output_dir Character or NULL.
#' @return Character path (guaranteed non-NULL).
#' @noRd
resolve_output_dir <- function(output_dir = NULL) {
  out <- output_dir
  if (is.null(out)) out <- get_data_path()
  if (is.null(out)) {
    stop("No output directory set. Either pass `output_dir` or call ",
         "`set_data_path()` first.", call. = FALSE)
  }
  if (!dir.exists(out)) {
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(out)) {
      stop("Could not create the output directory: ", out,
           ". Check that the parent directory exists and is writable.",
           call. = FALSE)
    }
  }
  # Probe once here rather than letting the Rust writers fail deep in a
  # parallel region, where the failure surfaces as an opaque panic.
  probe <- file.path(out, ".fplida-write-probe")
  writable <- tryCatch({
    ok <- file.create(probe, showWarnings = FALSE)
    if (isTRUE(ok)) unlink(probe)
    isTRUE(ok)
  }, error = function(e) FALSE)
  if (!writable) {
    stop("The output directory is not writable: ", out,
         ". Choose another location with `set_data_path()` or the ",
         "`output_dir` argument.", call. = FALSE)
  }
  out
}


#' Format N as a human-readable label
#'
#' Rounds to the nearest 1000 (minimum 1000), then formats with
#' \code{k} or \code{m} suffix. E.g. 5000000 -> "5m", 500000 -> "500k".
#'
#' @param n Integer. Number of persons.
#' @return Character label like "5m", "500k", "1k".
#' @noRd
format_n <- function(n) {
  rounded <- max(round(as.numeric(n) / 1000) * 1000L, 1000L)
  if (rounded >= 1000000L) {
    val <- rounded / 1000000
    if (val == floor(val)) {
      paste0(as.integer(val), "m")
    } else {
      paste0(val, "m")
    }
  } else {
    paste0(as.integer(rounded / 1000), "k")
  }
}


#' Build the run directory name
#'
#' @param n Integer. Number of persons.
#' @param suffix Character or NULL. Optional suffix to avoid overwrites.
#' @return Character. Folder name like "fplida_500k" or "fplida_5m_seed3".
#' @noRd
run_dir_name <- function(n, suffix = NULL) {
  name <- paste0("fplida_", format_n(n))
  if (!is.null(suffix) && nzchar(suffix)) {
    name <- paste0(name, "_", suffix)
  }
  name
}


#' Resolve the run directory for downstream generators
#'
#' Finds the active run directory by checking (in order):
#' \enumerate{
#'   \item The \code{fplida.run_dir} option (set by \code{generate_spine()})
#'   \item Scanning the base path for \code{fplida_*/} folders
#' }
#'
#' @param output_dir Character or NULL. Base path override.
#' @return Character path to the run directory.
#' @noRd
resolve_run_dir <- function(output_dir = NULL) {
  # 1. Check option (set by generate_spine)
  run_dir <- getOption("fplida.run_dir")
  if (!is.null(run_dir) && dir.exists(run_dir)) {
    if (is.null(output_dir)) return(run_dir)
    # If output_dir specified, verify the option is under that base
    base <- normalizePath(resolve_output_dir(output_dir), mustWork = FALSE)
    rd <- normalizePath(run_dir, mustWork = FALSE)
    if (startsWith(rd, base)) return(run_dir)
    # Option points elsewhere; fall through to scan
  }

  # 2. Scan base path for fplida_*/ run folders
  base <- resolve_output_dir(output_dir)
  dirs <- list.dirs(base, recursive = FALSE, full.names = TRUE)
  run_dirs <- dirs[grepl("^fplida_\\d+[km](_.+)?$", basename(dirs))]

  if (length(run_dirs) == 0L) {
    stop("No run directory found under ", base,
         ". Run generate_spine() first.", call. = FALSE)
  }

  # Return the most recently modified
  mtimes <- file.mtime(run_dirs)
  run_dirs[which.max(mtimes)]
}


#' Build the dataset subfolder path
#'
#' Creates \code{[agency]-[dataset]/} under the output directory.
#' E.g. \code{ato-pit_ps}, \code{abs-census}.
#'
#' @param output_dir Character. Base output directory.
#' @param dataset Character. Dataset acronym (e.g. "PIT_PS", "CENSUS").
#' @return Character path to the dataset subfolder (created if needed).
#' @noRd
dataset_dir <- function(output_dir, dataset) {
  agency <- dataset_to_agency(dataset)
  if (is.null(agency)) {
    stop("Unknown dataset: ", dataset, ". Cannot determine agency.",
         call. = FALSE)
  }
  folder <- paste0(tolower(agency), "-", tolower(dataset))
  path <- file.path(output_dir, folder)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(path)) {
      stop("Could not create the output directory: ", path,
           ". Check that the parent directory exists and is writable.",
           call. = FALSE)
    }
  }
  path
}


#' Write a product file (parquet or CSV)
#'
#' Writes a data.frame to the correct dataset subfolder using the PLIDA
#' product name. Creates the subfolder if needed.
#'
#' @param data Data.frame to write.
#' @param product_name Character. PLIDA product name (used as filename stem).
#' @param dataset Character. Dataset acronym (e.g. "PIT_PS").
#' @param output_dir Character. Base output directory.
#' @param format Character. "parquet" (default) or "csv".
#' @return The file path (invisibly).
#' @noRd
write_product <- function(data, product_name, dataset, output_dir,
                          format = "parquet") {
  format <- match.arg(format, c("parquet", "csv"))
  dir <- dataset_dir(output_dir, dataset)
  ext <- if (format == "parquet") ".parquet" else ".csv"
  path <- file.path(dir, paste0(product_name, ext))

  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for parquet output. ",
           "Install it with: install.packages('arrow')", call. = FALSE)
    }
    arrow::write_parquet(data, path)
  } else {
    write.csv(data, path, row.names = FALSE)
  }

  message("Wrote ", basename(path), " to ", dir)
  invisible(path)
}


#' Convert FY end year to PLIDA FY suffix
#'
#' E.g. 2015 (FY 2014-15) -> "fy1415"; 2021 (FY 2020-21) -> "fy2021".
#'
#' @param year Integer. Financial year end year.
#' @return Character. FY suffix like "fy1415".
#' @noRd
fy_suffix <- function(year) {
  start <- year - 1L
  s <- sprintf("%02d", start %% 100L)
  e <- sprintf("%02d", year %% 100L)
  paste0("fy", s, e)
}


#' Look up PLIDA product name from metadata
#'
#' Searches \code{inst/plida_metadata/products.csv} for a matching product.
#' Falls back to generating a name from the dataset convention if not found.
#'
#' @param dataset Character. Dataset acronym (e.g. "PIT_PS").
#' @param module_name Character. Module name to match (e.g. "Payment Summaries 2014-15").
#' @return Character. Product name (filename stem).
#' @noRd
lookup_product_name <- function(dataset, module_name) {
  csv_path <- system.file("plida_metadata", "products.csv",
                          package = "fplida")
  if (nzchar(csv_path)) {
    prods <- read.csv(csv_path, stringsAsFactors = FALSE)
    match_row <- prods$Dataset == dataset &
                 prods$Module.Name == module_name
    if (any(match_row)) {
      return(prods$Product.Name[which(match_row)[1]])
    }
  }
  # Fallback: generate a conventional name
  NULL
}


#' Get the PIT_PS product name for a financial year
#'
#' @param year Integer. FY end year.
#' @return Character. Product name.
#' @noRd
pit_ps_product_name <- function(year) {
  fy_start <- year - 1L
  module <- sprintf("Payment Summaries %d-%02d", fy_start, year %% 100)
  name <- lookup_product_name("PIT_PS", module)
  if (!is.null(name)) return(name)
  # Fallback: use the newer naming convention
  paste0("madipge-ato-d-pay-sum-", fy_suffix(year))
}


#' Get an ITR product name for a financial year and sub-table type
#'
#' Real PLIDA partitions ITR into 4 sub-tables per year: context, inc-loss,
#' ded-exp-off, whld-debt. This function looks up the canonical product name
#' from the PLIDA metadata, falling back to a conventional name.
#'
#' @param year Integer. FY end year.
#' @param table_type Character. One of "context", "inc-loss", "ded-exp-off",
#'   "whld-debt".
#' @return Character. Product name.
#' @noRd
itr_product_name <- function(year, table_type) {
  valid_types <- c("context", "inc-loss", "ded-exp-off", "whld-debt")
  table_type <- match.arg(table_type, valid_types)

  fy_start <- year - 1L
  module <- sprintf("Income Tax Return %d-%02d", fy_start, year %% 100)

  # Map table_type to product name substring for lookup
  type_pattern <- switch(table_type,
    "context"     = "context",
    "inc-loss"    = "inc-loss",
    "ded-exp-off" = "ded-exp-off",
    "whld-debt"   = "whld-debt"
  )

  # Try metadata lookup
  csv_path <- system.file("plida_metadata", "products.csv",
                          package = "fplida")
  if (nzchar(csv_path)) {
    prods <- read.csv(csv_path, stringsAsFactors = FALSE)
    match_rows <- prods$Dataset == "PIT_ITR" &
                  prods$Module.Name == module &
                  grepl(type_pattern, prods$Product.Name, fixed = TRUE)
    if (any(match_rows)) {
      return(prods$Product.Name[which(match_rows)[1]])
    }
  }

  # Fallback: conventional naming
  paste0("madipge-ato-d-", table_type, "-", fy_suffix(year))
}


#' Get the Census product name for a table type
#'
#' @param table Character. One of "person", "family", "dwelling".
#' @param census_year Integer. Census year (e.g. 2021).
#' @return Character. Product name.
#' @noRd
census_product_name <- function(table, census_year = 2021L) {
  yr_short <- sprintf("%02d", census_year %% 100)
  module <- sprintf("Census of Population and Housing %d", census_year)
  # Try lookup from metadata
  prods_path <- system.file("plida_metadata", "products.csv",
                            package = "fplida")
  if (nzchar(prods_path)) {
    prods <- read.csv(prods_path, stringsAsFactors = FALSE)
    match_rows <- prods$Dataset == "CENSUS" &
                  prods$Module.Name == module &
                  grepl(table, prods$Product.Name, ignore.case = TRUE)
    if (any(match_rows)) {
      return(prods$Product.Name[which(match_rows)[1]])
    }
  }
  # Fallback
  paste0("madipge-cen", yr_short, "-d-", table, "-", census_year)
}


#' Get the CORE product name for a sub-table type
#'
#' @param table_type Character. One of "demographics", "locations",
#'   "relationships", "vitals", "residence".
#' @return Character. Product name.
#' @noRd
core_product_name <- function(table_type) {
  valid <- c("demographics", "locations", "relationships",
             "vitals", "residence")
  table_type <- match.arg(table_type, valid)

  # Map to PLIDA module names
  module <- switch(table_type,
    demographics  = "Core Demographics 2006-current (2021 Census)",
    locations     = "Core Locations 2006-current",
    relationships = "Core Relationships 2006-current (2021 Census)",
    vitals        = "Core Vitals 2006-current",
    residence     = "Core Residence 2006-current"
  )

  # Map to product name substrings for matching
  pattern <- switch(table_type,
    demographics  = "demog-cb",
    locations     = "locat-cb",
    relationships = "relat-cb-c21",
    vitals        = "core_vitals",
    residence     = "core_residence"
  )

  csv_path <- system.file("plida_metadata", "products.csv",
                          package = "fplida")
  if (nzchar(csv_path)) {
    prods <- read.csv(csv_path, stringsAsFactors = FALSE)
    match_rows <- prods$Dataset == "CORE" &
                  prods$Module.Name == module &
                  grepl(pattern, prods$Product.Name, fixed = TRUE)
    if (any(match_rows)) {
      return(prods$Product.Name[which(match_rows)[1]])
    }
  }

  # Fallback
  switch(table_type,
    demographics  = "plidage-core-demog-cb-c21-2006-latest",
    locations     = "plidage-core-locat-cb-2006-latest",
    relationships = "plidage-core-relat-cb-c21-2006-latest",
    vitals        = "core_vitals",
    residence     = "core_residence"
  )
}


#' Get the HE product name for a sub-table type
#'
#' @param table_type Character. One of "enrol", "course", "load",
#'   "completions", "help".
#' @return Character. Product name.
#' @noRd
he_product_name <- function(table_type) {
  valid <- c("enrol", "course", "load", "completions", "help")
  table_type <- match.arg(table_type, valid)

  # Map to PLIDA table name suffixes
  suffix <- switch(table_type,
    enrol       = "student-enrol",
    course      = "student-course",
    load        = "student-load",
    completions = "student-completions",
    help        = "student-help"
  )

  paste0("madipge-hied-", suffix)
}


#' Get the DOMINO product name for a module and table
#'
#' Maps a module key and table name to the PLIDA product filename.
#' Each module is a single PLIDA product; tables within are separated
#' by a double-hyphen.
#'
#' @param module Character. One of "base", "disability-carers",
#'   "families-children", "indg-stat", "older-students",
#'   "retirement-widows", "working-age", "rlt-prin-carer-beta".
#' @param table Character or NULL. Table name within module.
#' @return Character. Product name.
#' @noRd
domino_product_name <- function(module, table = NULL) {
  valid_modules <- c(
    "base", "disability-carers", "families-children", "indg-stat",
    "older-students", "retirement-widows", "working-age",
    "rlt-prin-carer-beta"
  )
  module <- match.arg(module, valid_modules)

  base_names <- c(
    "base"               = "madipge-dom-monthly-d-base",
    "disability-carers"  = "madipge-dom-monthly-d-disability-carers",
    "families-children"  = "madipge-dom-monthly-d-families-children",
    "indg-stat"          = "madipge-dom-monthly-d-indg-stat",
    "older-students"     = "madipge-dom-monthly-d-older-students",
    "retirement-widows"  = "madipge-dom-monthly-d-retirement-widows",
    "working-age"        = "madipge-dom-monthly-d-working-age",
    "rlt-prin-carer-beta" = "madipge-dom-monthly-d-rlt-prin-carer-beta"
  )

  product <- base_names[[module]]
  if (!is.null(table)) {
    product <- paste0(product, "--", gsub("_", "-", table))
  }
  product
}


#' Load spine with column selection (memory-efficient)
#'
#' Like \code{load_spine()} but only reads the requested columns from the
#' parquet file. At 25M people, the full spine is ~13GB; selecting 5 columns
#' reduces this to ~1.5GB.
#'
#' @param run_dir Character. Run directory.
#' @param cols Character vector. Variables to read.
#' @return Data.frame with only the requested columns.
#' @noRd
load_spine_select <- function(run_dir, cols) {
  sys_dir <- file.path(run_dir, "_system")
  if (!dir.exists(sys_dir)) {
    stop("No _system/ subfolder found in ", run_dir,
         ". Run generate_spine() first.", call. = FALSE)
  }
  pq  <- file.path(sys_dir, "base-spine.parquet")
  csv <- file.path(sys_dir, "base-spine.csv")
  path <- if (file.exists(pq)) pq
          else if (file.exists(csv)) csv
          else stop("No base-spine file found in ", sys_dir, call. = FALSE)

  ext <- tools::file_ext(path)
  if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read parquet spine.", call. = FALSE)
    }
    spine <- as.data.frame(read_parquet_safely(path, col_select = dplyr::all_of(cols)))
  } else {
    spine <- read.csv(path, stringsAsFactors = FALSE)
    spine <- spine[, cols, drop = FALSE]
  }
  message("Loaded spine (", length(cols), " cols) from ", path)
  spine
}


#' Write a product chunk file for large-N streaming output
#'
#' When a product is too large to fit in memory as a single file, it is written
#' as a directory of parquet chunk files. Each chunk is a self-contained parquet
#' file named \code{part-NNN.parquet}. Consumers can read all parts back using
#' \code{arrow::open_dataset(dir)}.
#'
#' @param data Data.frame. The chunk to write.
#' @param product_name Character. PLIDA product name.
#' @param dataset Character. Dataset acronym.
#' @param output_dir Character. Base output directory.
#' @param format Character. "parquet" or "csv".
#' @param chunk_idx Integer. 1-based chunk index.
#' @param n_chunks Integer. Total number of chunks.
#' @return File path (invisibly).
#' @noRd
write_product_chunk <- function(data, product_name, dataset, output_dir,
                                format = "parquet", chunk_idx, n_chunks) {
  format <- match.arg(format, c("parquet", "csv"))
  dir <- dataset_dir(output_dir, dataset)
  chunk_dir <- file.path(dir, product_name)
  if (!dir.exists(chunk_dir)) dir.create(chunk_dir, recursive = TRUE)

  ext <- if (format == "parquet") ".parquet" else ".csv"
  path <- file.path(chunk_dir, sprintf("part-%03d%s", chunk_idx, ext))

  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for parquet output.", call. = FALSE)
    }
    arrow::write_parquet(data, path)
  } else {
    write.csv(data, path, row.names = FALSE)
  }

  if (chunk_idx == n_chunks) {
    message("Wrote ", product_name, " (", n_chunks, " parts, ",
            format(nrow(data), big.mark = ","), " rows in last) to ", chunk_dir)
  }
  invisible(path)
}


#' Get the MBS product name for a calendar year
#'
#' @param year Integer. Calendar year (e.g. 2020).
#' @return Character. Product name like "madipge-mbs-d-claims-2020".
#' @noRd
mbs_product_name <- function(year) {
  paste0("madipge-mbs-d-claims-", as.integer(year))
}


#' Get the PBS product name for a calendar year
#'
#' @param year Integer. Calendar year (e.g. 2020).
#' @return Character. Product name like "madipge-pbs-d-prescriptions-2020".
#' @noRd
pbs_product_name <- function(year) {
  paste0("madipge-pbs-d-prescriptions-", as.integer(year))
}


#' Get the TVA product name for a table type and year
#'
#' @param table_type Character. One of "trn-actvty", "prog-comp".
#' @param year Integer. Calendar year (e.g. 2020).
#' @return Character. Product name like "madipge-tva-d-trn-actvty-core-2020".
#' @noRd
tva_product_name <- function(table_type, year) {
  valid <- c("trn-actvty", "prog-comp")
  table_type <- match.arg(table_type, valid)
  paste0("madipge-tva-d-", table_type, "-core-", as.integer(year))
}


#' Product name for COMBINED Indigenous status
#'
#' @param linkage_base Character. Census linkage base: "c11", "c16", "c21", "nc".
#' @return Character. Product name.
#' @noRd
combined_product_name <- function(linkage_base) {
  valid <- c("c11", "c16", "c21", "nc")
  linkage_base <- match.arg(linkage_base, valid)
  paste0("madipge-comb-d-demog-ind-", linkage_base, "-06-latest")
}

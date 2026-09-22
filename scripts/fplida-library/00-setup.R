source(file.path(library_script_root, "00-paths.R"))

# Parallel workers must use the same installed package as the parent.
package_library <- Sys.getenv("FPLIDA_LIBRARY", "")
if (nzchar(package_library)) {
  .libPaths(c(normalizePath(package_library, mustWork = TRUE), .libPaths()))
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
             R_LIBS_USER = normalizePath(package_library, mustWork = TRUE))
}
required_packages <- c("fplida", "DBI", "duckdb", "dplyr", "dbplyr",
                       "purrr", "tibble", "jsonlite")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
)]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

library_profiles <- function() {
  subset_products <- c("census", "core", "blade", "pit_ps", "pit_itr",
                       "domino", "ndis", "mbs", "pbs", "he", "tva",
                       "stp", "visa", "busown", "deaths", "lfs")
  sizes <- c(`1k` = 1000L, `100k` = 100000L, `1m` = 1000000L,
             `30m` = 30000000L)
  purrr::imap(sizes, function(n, name) {
    list(name = name, n = n, seed = 42L,
         products = if (name == "1k") "all" else subset_products,
         years = if (name == "30m") 2010L:2026L else if (name == "1k") {
           1964L:2026L
         } else 2000L:2026L,
         years_by_product = if (name == "30m") {
           list(mbs = 2024L, pbs = 2024L)
         } else list(),
         format = if (name == "30m") "parquet" else "csv",
         complete_dil_schema = name == "1k", complete_dil_rows = 1000L,
         k_slices = if (name == "30m") 60L else 10L,
         n_workers = if (name == "30m") 4L else 10L,
         rayon_threads = if (name == "30m") 2L else 1L)
  })
}

# Explicit settings leave memory for R, Rust and the operating system.
qa_memory <- Sys.getenv("FPLIDA_QA_MEMORY", "4GB")
qa_threads <- as.integer(Sys.getenv("FPLIDA_QA_THREADS", "4"))
qa_link_limit <- as.integer(Sys.getenv("FPLIDA_QA_LINK_ROWS", "100000"))
qa_links_full <- identical(Sys.getenv("FPLIDA_QA_LINK_MODE"), "full")

json_write_atomic <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  jsonlite::write_json(value, temporary, pretty = TRUE, auto_unbox = TRUE,
                       null = "null", na = "null", digits = NA)
  if (!file.rename(temporary, path)) stop("Cannot write ", path)
}

sql_string <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
sql_identifier <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
sql_files <- function(x) paste0("[", paste(sql_string(x), collapse = ","), "]")

open_audit_connection <- function(temp_dir) {
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, paste0("SET memory_limit = ", sql_string(qa_memory)))
  DBI::dbExecute(con, paste0("SET threads = ", qa_threads))
  DBI::dbExecute(con, "SET preserve_insertion_order = false")
  DBI::dbExecute(con, paste0("SET temp_directory = ", sql_string(temp_dir)))
  con
}

read_asset <- function(con, files) {
  extension <- unique(tolower(tools::file_ext(files)))
  if (length(extension) != 1L) stop("Mixed formats in one asset")
  reader <- if (extension == "parquet") {
    paste0("read_parquet(", sql_files(files), ", union_by_name = true)")
  } else {
    paste0("read_csv(", sql_files(files), ", header = true, all_varchar = true, ",
           "nullstr = ['', 'NA'], union_by_name = true, sample_size = 100000)")
  }
  dplyr::tbl(con, dbplyr::sql(paste0("SELECT * FROM ", reader))) |>
    dplyr::rename_with(tolower)
}

source(file.path(library_script_root, "10-manifest.R"))
source(file.path(library_script_root, "11-periods.R"))
source(file.path(library_script_root, "_checks", "check-business-links.R"))
source(file.path(library_script_root, "_checks", "check-library.R"))
source(file.path(library_script_root, "20-build.R"))

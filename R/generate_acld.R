#' Generate ACLD dataset (Australian Census Longitudinal Dataset)
#'
#' @section Dataset and variable information:
#' The [ABS ACLD overview](https://www.abs.gov.au/statistics/data-integration/integrated-data/australian-census-longitudinal-dataset-acld)
#' gives information about this dataset. Use `dataset_info("ACLD")` for dataset
#' information. Use `variable_info("ACLD")` for variables, sources, value
#' support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_acld <- function(spine = NULL, seed = 42L, output_dir = NULL,
                          format = c("parquet", "csv"),
                          return_data = FALSE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("acld writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "ACLD")
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to generate ACLD products.", call. = FALSE)
  }

  acld_cols <- c(
    "id", "spine_id", "aeuid_abs", "birth_year", "sex", "state",
    "indigenous", "country_of_birth", "country_of_birth_sacc",
    "year_of_arrival", "citizenship", "education", "baseline_employed",
    "baseline_hours", "baseline_income", "anzsco_code", "anzsco_major",
    "skill_level", "industry", "disability_onset_year",
    "disability_severity", "residence_seed", "sa2_code", "household_id"
  )
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, acld_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_abs = spine$aeuid_abs,
                           stringsAsFactors = FALSE)

  project_panel <- function(path, panel_seed) {
    project_acld_to_parquet__(
      aeuid             = as.character(spine$aeuid_abs),
      birth_year        = as.integer(spine$birth_year),
      sex               = as.integer(spine$sex),
      state             = as.integer(spine$state),
      indigenous        = as.integer(spine$indigenous),
      country_of_birth  = as.integer(spine$country_of_birth),
      education         = as.integer(spine$education),
      baseline_employed = as.integer(spine$baseline_employed),
      baseline_income   = as.numeric(spine$baseline_income),
      seed              = as.integer(panel_seed),
      out_path          = path
    )
  }

  primary_path <- file.path(ds_dir, "madipge-acld11-d-persons-2011.parquet")
  project_panel(primary_path, seed + 3400L)

  base <- as.data.frame(read_parquet_safely(primary_path),
                        stringsAsFactors = FALSE)

  product_1121 <- "madipge-acld11-d-persons-11-16-21"
  acld_1121 <- .acld_enrich_product(base, spine, product_1121, seed + 3400L)
  arrow::write_parquet(acld_1121, primary_path)
  file.copy(
    primary_path,
    file.path(ds_dir, "madipge-acld11-d-persons-2016.parquet"),
    overwrite = TRUE
  )
  file.copy(
    primary_path,
    file.path(ds_dir, paste0(product_1121, ".parquet")),
    overwrite = TRUE
  )
  n_rows_1121 <- nrow(acld_1121)
  rm(base)
  if (!return_data) {
    rm(acld_1121)
    gc()
  }

  product_1621 <- "madipge-acld16-d-person-16-21"
  product_1621_path <- file.path(ds_dir, paste0(product_1621, ".parquet"))
  project_panel(product_1621_path, seed + 3500L)
  base_1621 <- as.data.frame(
    read_parquet_safely(product_1621_path),
    stringsAsFactors = FALSE
  )
  acld_1621 <- .acld_enrich_product(
    base_1621, spine, product_1621, seed + 3500L
  )
  arrow::write_parquet(
    acld_1621,
    product_1621_path
  )
  n_rows_1621 <- nrow(acld_1621)
  rm(base_1621, acld_1621)
  gc()

  write_agency_spine(mini_spine, "ABS", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) return(acld_1121)
  invisible(list(
    n_rows = as.integer(n_rows_1121),
    n_rows_1121 = as.integer(n_rows_1121),
    n_rows_1621 = as.integer(n_rows_1621)
  ))
}

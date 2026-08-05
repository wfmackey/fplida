#' Generate DEX dataset (Data Exchange)
#'
#' @section Dataset and variable information:
#' The [DSS Data Exchange policy](https://dex.dss.gov.au/policy) website gives
#' information about this dataset. Use `dataset_info("DEX")` for dataset
#' information. Use `variable_info("DEX")` for variables, sources, value
#' support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_dex <- function(spine = NULL, seed = 42L, output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("dex writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "DEX")

  dex_cols <- c("spine_id", "aeuid_dss", "birth_year", "sex", "state",
                "indigenous", "country_of_birth_sacc", "education", "baseline_employed",
                "baseline_income", "sa2_code", "disability_severity")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, dex_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_dss = spine$aeuid_dss,
                           stringsAsFactors = FALSE)

  out_client     <- file.path(ds_dir, "madipge-dex-d-extended-15-current-special_client.parquet")
  out_attendance <- file.path(ds_dir, "madipge-dex-d-extended-15-current-special_attendance.parquet")
  out_assessment <- file.path(ds_dir, "madipge-dex-d-extended-15-current-special_client_assessment.parquet")
  n_rows <- project_dex_to_parquet__(
    aeuid             = as.character(spine$aeuid_dss),
    birth_year        = as.integer(spine$birth_year),
    sex               = as.integer(spine$sex),
    state             = as.integer(spine$state),
    indigenous        = as.integer(spine$indigenous),
    # SACC country code (consistent across datasets) under BIRTHCOUNTRYCODE.
    country_of_birth  = as.integer(spine$country_of_birth_sacc),
    education         = as.integer(spine$education),
    baseline_employed = as.integer(spine$baseline_employed),
    baseline_income   = as.numeric(spine$baseline_income),
    sa2               = as.integer(spine$sa2_code),
    disability_severity = as.integer(spine$disability_severity),
    seed              = as.integer(seed + 2900L),
    out_client        = out_client,
    out_attendance    = out_attendance,
    out_assessment    = out_assessment
  )

  write_agency_spine(mini_spine, "DSS", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(out_client)) {
    return(as.data.frame(read_parquet_safely(out_client)))
  }
  invisible(list(n_rows = as.integer(n_rows)))
}

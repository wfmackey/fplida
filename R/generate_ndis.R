#' Generate NDIS dataset (National Disability Insurance Scheme)
#'
#' @section Dataset and variable information:
#' The [NDIA datasets](https://dataresearch.ndis.gov.au/datasets) website gives
#' information about this dataset. Use `dataset_info("NDIS")` for dataset
#' information. Use `variable_info("NDIS")` for variables, sources, value
#' support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_ndis <- function(spine = NULL, seed = 42L, output_dir = NULL,
                          format = c("parquet", "csv"),
                          return_data = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("ndis writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "NDIS")

  ndis_cols <- c("spine_id", "aeuid_ndia", "birth_year", "sex", "state",
                 "indigenous", "disability_onset_year", "disability_severity",
                 "person_type", "comorbidity_flags", "country_of_birth_sacc",
                 "sa2_code")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, ndis_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_ndia = spine$aeuid_ndia,
                           stringsAsFactors = FALSE)

  out_path     <- file.path(ds_dir, "madipge-ndis-exp-d-participants-13-current.parquet")
  out_supports <- file.path(ds_dir, "madipge-ndis-exp-d-plansupports-13-current.parquet")
  out_payments <- file.path(ds_dir, "madipge-ndis-exp-d-payments-13-current.parquet")
  n_rows <- project_ndis_to_parquet__(
    aeuid                 = as.character(spine$aeuid_ndia),
    birth_year            = as.integer(spine$birth_year),
    sex                   = as.integer(spine$sex),
    state                 = as.integer(spine$state),
    indigenous            = as.integer(spine$indigenous),
    disability_onset_year = as.integer(spine$disability_onset_year),
    disability_severity   = as.integer(spine$disability_severity),
    person_type           = as.integer(spine$person_type),
    comorbidity_flags     = as.integer(spine$comorbidity_flags),
    country_of_birth_sacc = as.integer(spine$country_of_birth_sacc),
    sa2                   = as.integer(spine$sa2_code),
    seed                  = as.integer(seed + 2700L),
    out_participants      = out_path,
    out_plansupports      = out_supports,
    out_payments          = out_payments
  )

  write_agency_spine(mini_spine, "NDIA", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(out_path)) {
    return(as.data.frame(read_parquet_safely(out_path)))
  }
  invisible(list(n_rows = as.integer(n_rows)))
}

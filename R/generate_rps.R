#' Generate RPS dataset (Rental Property Schedule)
#'
#' @section Dataset and variable information:
#' The [ATO residential rental properties](https://www.ato.gov.au/individuals-and-families/investments-and-assets/property-and-land/residential-rental-properties)
#' website gives information about this dataset. Use `dataset_info("RPS")` for
#' dataset information. Use `variable_info("RPS")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @param years Integer vector. Financial years to generate.
#' @export
generate_rps <- function(spine = NULL, seed = 42L, years = 2000L:2023L,
                         output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = FALSE) {
  seed <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  if (format != "parquet") stop("rps writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "RPS")

  rps_cols <- c("spine_id", "aeuid_ato", "birth_year",
                "baseline_employed", "baseline_income", "state")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, rps_cols)
  stopifnot(is.data.frame(spine))
  spine <- filter_ato_records(spine, reference_year = max(years))

  mini_spine <- data.frame(spine_id = spine$spine_id,
                           aeuid_ato = spine$aeuid_ato,
                           stringsAsFactors = FALSE)

  yr_range <- range(years)
  n_rows <- project_rps_to_parquet__(
    aeuid           = as.character(spine$aeuid_ato),
    birth_year      = as.integer(spine$birth_year),
    baseline_income = as.numeric(spine$baseline_income),
    state           = as.integer(spine$state),
    seed            = as.integer(seed + 2500L),
    fy_start        = yr_range[1L],
    fy_end          = yr_range[2L],
    out_dir         = ds_dir,
    product_prefix  = "madipge-ato-d-rps-fy"
  )

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  invisible(list(n_rows = as.integer(n_rows), years = years))
}

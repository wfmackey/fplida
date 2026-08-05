#' Generate ATO_CR dataset (ATO Client Register)
#'
#' Projects ATO client register records from the fplida spine. ~90% of
#' adults (16+) have interacted with the ATO. Two products: demographics
#' and address.
#'
#' @section Dataset and variable information:
#' The [ABS PLIDA data and legislation](https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation)
#' website gives information about this dataset. Use `dataset_info("ATO_CR")`
#' for dataset information. Use `variable_info("ATO_CR")` for variables,
#' sources, value support, and topic tags.
#'
#' @param spine Data.frame or NULL.
#' @param seed Integer. Random seed.
#' @param output_dir Character or NULL.
#' @param format Character. "parquet" (default) or "csv".
#' @param return_data Logical.
#'
#' @return If \code{return_data = TRUE}: data.frame. If FALSE: metadata list.
#'
#' @export
generate_ato_cr <- function(spine = NULL, seed = 42L, output_dir = NULL,
                            format = c("parquet", "csv"),
                            return_data = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)

  run_dir <- resolve_run_dir(output_dir)

  cr_cols <- c("spine_id", "aeuid_ato", "birth_year", "sex", "state",
               "country_of_birth", "baseline_employed", "baseline_income")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, cr_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))
  spine <- filter_ato_records(spine, reference_year = 2024L)

  mini_spine <- data.frame(
    spine_id  = spine$spine_id,
    aeuid_ato = spine$aeuid_ato,
    stringsAsFactors = FALSE
  )

  raw <- project_ato_cr__(
    aeuid            = as.character(spine$aeuid_ato),
    birth_year       = as.integer(spine$birth_year),
    sex              = as.integer(spine$sex),
    state            = as.integer(spine$state),
    country_of_birth = as.integer(spine$country_of_birth),
    seed             = as.integer(seed + 1700L),
    reference_year   = 2024L
  )

  ato_cr <- as.data.frame(raw, stringsAsFactors = FALSE)

  if (spine_loaded) { rm(spine); gc() }

  # Write both products (demographics and address are in same dataframe)
  write_product(ato_cr, "madipge-ato-d-clientreg-demogs-1999-current",
                "ATO_CR", run_dir, format)
  write_product(ato_cr, "madipge-ato-d-clientregaddr-1999-current",
                "ATO_CR", run_dir, format)

  ds_dir <- dataset_dir(run_dir, "ATO_CR")
  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = 2024L)

  if (return_data) return(ato_cr)
  invisible(list(n_rows = nrow(ato_cr)))
}

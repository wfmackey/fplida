#' Generate DEATHS dataset (Death Registrations)
#'
#' @section Dataset and variable information:
#' The [ABS deaths methodology](https://www.abs.gov.au/methodologies/deaths-australia-methodology/2024)
#' gives information about this dataset. Use `dataset_info("DEATHS")` for
#' dataset information. Use `variable_info("DEATHS")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @param years Integer vector. Death registration years to generate.
#' @export
generate_deaths <- function(spine = NULL, seed = 42L, years = 2007L:2023L,
                            output_dir = NULL,
                            format = c("parquet", "csv"),
                            return_data = FALSE) {
  seed <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  if (format != "parquet") stop("deaths writes parquet only.", call. = FALSE)

  years <- gate_dataset_years("DEATHS", years)
  if (length(years) == 0L) return(invisible(NULL))

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "DEATHS")
  if (!dir.exists(ds_dir)) dir.create(ds_dir, recursive = TRUE)

  deaths_cols <- c("spine_id", "aeuid_rbdm", "birth_year", "sex", "state",
                   "indigenous", "country_of_birth_sacc", "year_of_death",
                   "month_of_death", "day_of_death",
                   # The SA2 of usual residence on the registration is the
                   # person's own, not a fresh draw.
                   "sa2_code")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, deaths_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id,
                           aeuid_rbdm = spine$aeuid_rbdm,
                           stringsAsFactors = FALSE)

  yr_range <- range(years)

  n_rows <- project_deaths_to_parquet__(
    aeuid            = as.character(spine$aeuid_rbdm),
    birth_year       = as.integer(spine$birth_year),
    sex              = as.integer(spine$sex),
    state            = as.integer(spine$state),
    indigenous       = as.integer(spine$indigenous),
    # SACC country code (cross-dataset consistent), not the 0/1 flag.
    country_of_birth = as.integer(spine$country_of_birth_sacc),
    year_of_death    = as.integer(spine$year_of_death),
    month_of_death   = as.integer(spine$month_of_death),
    day_of_death     = as.integer(spine$day_of_death),
    seed             = as.integer(seed + 1400L),
    min_year         = yr_range[1L],
    max_year         = yr_range[2L],
    out_dir          = ds_dir
  )

  # The generator writes one table a year with the registration and the
  # medical record together. The real asset publishes them separately, and
  # names its geography by vintage.
  .deaths_split_products(ds_dir, years, spine, seed)

  write_agency_spine(mini_spine, "RBDM", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) {
    out_list <- list()
    for (yr in years) {
      p <- file.path(ds_dir, .deaths_product_file("cause-of-death", yr))
      if (file.exists(p)) out_list[[as.character(yr)]] <- as.data.frame(read_parquet_safely(p))
    }
    if (length(out_list) > 0L) return(do.call(rbind, out_list))
    return(data.frame())
  }
  invisible(list(n_rows = as.integer(n_rows), years = years))
}

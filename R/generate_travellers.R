#' Generate TRAVELLERS dataset (Traveller Movements)
#'
#' @section Dataset and variable information:
#' The [Home Affairs overseas movements](https://www.homeaffairs.gov.au/research-and-statistics/statistics/visa-statistics/live/overseas-arrivals-and-departures)
#' website gives information about this dataset. Use
#' `dataset_info("TRAVELLERS")` for dataset information. Use
#' `variable_info("TRAVELLERS")` for variables, sources, value support, and
#' topic tags.
#'
#' @inheritParams generate_apsed
#' @param years Integer vector. Traveller movement years to generate.
#' @export
generate_travellers <- function(spine = NULL, seed = 42L,
                                years = 2006L:2024L,
                                output_dir = NULL,
                                format = c("parquet", "csv"),
                                return_data = FALSE) {
  seed <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  if (format != "parquet") stop("travellers writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "TRAVELLERS")
  if (!dir.exists(ds_dir)) dir.create(ds_dir, recursive = TRUE)

  trav_cols <- c("spine_id", "aeuid_ha", "birth_year", "month_of_birth",
                 "country_of_birth", "year_of_arrival", "year_of_death",
                 "month_of_death", "day_of_death", "residence_seed")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, trav_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_ha = spine$aeuid_ha,
                           stringsAsFactors = FALSE)

  yr_range <- range(years)
  combined_name <- "madipge-mig-d-travellers-2006-current"
  erp_name      <- "madipge-mig-d-erp-2006-current"
  pp_name       <- "madipge-mig-d-pp-2006-current"

  n_rows <- project_travellers_to_parquet__(
    aeuid            = as.character(spine$aeuid_ha),
    birth_year       = as.integer(spine$birth_year),
    month_of_birth   = as.integer(spine$month_of_birth),
    country_of_birth = as.integer(spine$country_of_birth),
    year_of_arrival  = as.integer(spine$year_of_arrival),
    year_of_death    = as.integer(spine$year_of_death),
    month_of_death   = as.integer(spine$month_of_death),
    day_of_death     = as.integer(spine$day_of_death),
    residence_seed   = as.integer(spine$residence_seed),
    seed             = as.integer(seed + 3600L),
    min_year         = yr_range[1L],
    max_year         = yr_range[2L],
    out_dir          = ds_dir,
    combined_name    = combined_name,
    erp_name         = erp_name,
    pp_name          = pp_name
  )

  write_agency_spine(mini_spine, "HA", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) {
    primary_path <- file.path(ds_dir, paste0(combined_name, ".parquet"))
    if (file.exists(primary_path)) {
      return(as.data.frame(read_parquet_safely(primary_path)))
    }
  }
  invisible(list(n_rows = as.integer(n_rows)))
}

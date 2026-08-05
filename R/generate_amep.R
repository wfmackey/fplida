#' Generate AMEP dataset (Adult Migrant English Program)
#'
#' @section Dataset and variable information:
#' The [Home Affairs AMEP](https://immi.homeaffairs.gov.au/settling-in-australia/amep/overview)
#' website gives information about this dataset. Use `dataset_info("AMEP")` for
#' dataset information. Use `variable_info("AMEP")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_amep <- function(spine = NULL, seed = 42L, output_dir = NULL,
                          format = c("parquet", "csv"),
                          return_data = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("amep writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "AMEP")

  amep_cols <- c("spine_id", "aeuid_ha", "birth_year",
                 "country_of_birth", "year_of_arrival", "year_of_death")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, amep_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_ha = spine$aeuid_ha,
                           stringsAsFactors = FALSE)
  death_year <- if ("year_of_death" %in% names(spine)) {
    as.integer(spine$year_of_death)
  } else {
    rep(NA_integer_, nrow(spine))
  }

  out_path <- file.path(ds_dir, "madipge-mig-d-amep-client-03-19.parquet")
  n_rows <- project_amep_to_parquet__(
    aeuid            = as.character(spine$aeuid_ha),
    birth_year       = as.integer(spine$birth_year),
    country_of_birth = as.integer(spine$country_of_birth),
    year_of_arrival  = as.integer(spine$year_of_arrival),
    year_of_death    = death_year,
    seed             = as.integer(seed + 3100L),
    out_path         = out_path
  )

  # Second product (same data)
  out_path2 <- file.path(ds_dir, "madipge-mig-d-amep-english-03-19.parquet")
  if (file.exists(out_path)) {
    file.copy(out_path, out_path2, overwrite = TRUE)
  }

  write_agency_spine(mini_spine, "HA", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(out_path)) {
    return(as.data.frame(read_parquet_safely(out_path)))
  }
  invisible(list(n_rows = as.integer(n_rows)))
}

#' Generate AIR dataset (Australian Immunisation Register)
#'
#' @section Dataset and variable information:
#' The [Department of Health AIR](https://www.health.gov.au/topics/immunisation/immunisation-information-for-health-professionals/using-the-australian-immunisation-register)
#' website gives information about this dataset. Use `dataset_info("AIR")` for
#' dataset information. Use `variable_info("AIR")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_air <- function(spine = NULL, seed = 42L, output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("air writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "AIR")

  air_cols <- c(
    "spine_id", "aeuid_dhda", "birth_year", "sex", "state", "indigenous",
    "year_of_death", "month_of_death", "day_of_death"
  )
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, air_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_dhda = spine$aeuid_dhda,
                           stringsAsFactors = FALSE)
  n <- nrow(spine)
  death_year <- if ("year_of_death" %in% names(spine)) {
    as.integer(spine$year_of_death)
  } else {
    rep(NA_integer_, n)
  }
  death_month <- if ("month_of_death" %in% names(spine)) {
    as.integer(spine$month_of_death)
  } else {
    rep(NA_integer_, n)
  }
  death_day <- if ("day_of_death" %in% names(spine)) {
    as.integer(spine$day_of_death)
  } else {
    rep(NA_integer_, n)
  }

  out_path <- file.path(ds_dir, "madip-vacstrat-air.parquet")
  n_rows <- project_air_to_parquet__(
    aeuid          = as.character(spine$aeuid_dhda),
    spine_id       = as.character(spine$spine_id),
    birth_year     = as.integer(spine$birth_year),
    sex            = as.integer(spine$sex),
    state          = as.integer(spine$state),
    indigenous     = as.integer(spine$indigenous),
    year_of_death  = death_year,
    month_of_death = death_month,
    day_of_death   = death_day,
    seed           = as.integer(seed + 3000L),
    reference_year = 2024L,
    out_path       = out_path
  )

  write_agency_spine(mini_spine, "DHDA", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(out_path)) {
    return(as.data.frame(read_parquet_safely(out_path)))
  }
  invisible(list(n_rows = as.integer(n_rows)))
}

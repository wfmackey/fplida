#' Generate MCD dataset (Medicare Consumer Directory)
#'
#' @section Dataset and variable information:
#' The [ABS PLIDA data and legislation](https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation)
#' website gives information about this dataset. Use `dataset_info("MCD")` for
#' dataset information. Use `variable_info("MCD")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_mcd <- function(spine = NULL, seed = 42L, output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = FALSE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("mcd writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "MCD")

  mcd_cols <- c("spine_id", "aeuid_sa", "birth_year", "sex", "state",
                "country_of_birth", "year_of_arrival", "year_of_death",
                "month_of_death")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, mcd_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_sa = spine$aeuid_sa,
                           stringsAsFactors = FALSE)
  n <- nrow(spine)
  optional_integer <- function(name) {
    if (name %in% names(spine)) as.integer(spine[[name]]) else rep(NA_integer_, n)
  }

  primary_path <- file.path(ds_dir, "madipge-mcd-d-enrolments-06-current.parquet")
  n_rows <- project_mcd_to_parquet__(
    aeuid            = as.character(spine$aeuid_sa),
    birth_year       = as.integer(spine$birth_year),
    sex              = as.integer(spine$sex),
    state            = as.integer(spine$state),
    country_of_birth = as.integer(spine$country_of_birth),
    year_of_arrival  = optional_integer("year_of_arrival"),
    year_of_death    = optional_integer("year_of_death"),
    month_of_death   = optional_integer("month_of_death"),
    seed             = as.integer(seed + 1600L),
    out_path         = primary_path
  )

  write_agency_spine(mini_spine, "SA", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(primary_path)) {
    return(as.data.frame(read_parquet_safely(primary_path)))
  }
  invisible(list(n_rows = as.integer(n_rows)))
}

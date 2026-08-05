#' Generate A&T dataset (Apprentice and Trainee)
#'
#' @section Dataset and variable information:
#' The [DEWR Australian Apprenticeships](https://www.dewr.gov.au/australian-apprenticeships)
#' website gives information about this dataset. Use `dataset_info("A&T")` for
#' dataset information. Use `variable_info("A&T")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_apprentice <- function(spine = NULL, seed = 42L, output_dir = NULL,
                                format = c("parquet", "csv"),
                                return_data = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("apprentice writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "A&T")

  at_cols <- c("spine_id", "aeuid_dewr", "birth_year", "sex", "state",
               "indigenous", "country_of_birth_sacc", "education", "anzsco_code",
               "industry", "year_of_death", "month_of_death", "day_of_death")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, at_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_dewr = spine$aeuid_dewr,
                           stringsAsFactors = FALSE)
  n <- nrow(spine)
  optional_integer <- function(name) {
    if (name %in% names(spine)) as.integer(spine[[name]]) else rep(NA_integer_, n)
  }

  out_path <- file.path(ds_dir, "plidage-apprentice.parquet")
  n_rows <- project_apprentice_to_parquet__(
    aeuid            = as.character(spine$aeuid_dewr),
    birth_year       = as.integer(spine$birth_year),
    sex              = as.integer(spine$sex),
    state            = as.integer(spine$state),
    indigenous       = as.integer(spine$indigenous),
    # SACC country code (cross-dataset consistent), not the 0/1 flag.
    country_of_birth = as.integer(spine$country_of_birth_sacc),
    education        = as.integer(spine$education),
    anzsco_code      = as.character(spine$anzsco_code),
    industry         = as.integer(spine$industry),
    year_of_death    = optional_integer("year_of_death"),
    month_of_death   = optional_integer("month_of_death"),
    day_of_death     = optional_integer("day_of_death"),
    seed             = as.integer(seed + 2800L),
    out_path         = out_path
  )

  write_agency_spine(mini_spine, "DEWR", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(out_path)) {
    return(as.data.frame(read_parquet_safely(out_path)))
  }
  invisible(list(n_rows = as.integer(n_rows)))
}

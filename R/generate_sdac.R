#' Generate SDAC dataset (Survey of Disability, Ageing and Carers)
#'
#' @section Dataset and variable information:
#' The [ABS disability microdata](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/disability-ageing-and-carers-australia)
#' website gives information about this dataset. Use `dataset_info("SDAC")` for
#' dataset information. Use `variable_info("SDAC")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @param survey_year Integer. SDAC survey year to generate.
#' @export
generate_sdac <- function(spine = NULL, seed = 42L, survey_year = 2018L,
                          output_dir = NULL,
                          format = c("parquet", "csv"),
                          return_data = FALSE) {
  seed <- as.integer(seed)
  survey_year <- as.integer(survey_year)
  format <- match.arg(format)
  if (format != "parquet") stop("sdac writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "SDAC")

  sdac_cols <- c("spine_id", "aeuid_abs", "birth_year", "sex", "state",
                 "indigenous", "country_of_birth", "education",
                 "baseline_employed", "disability_severity",
                 "disability_onset_year", "person_type")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, sdac_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_abs = spine$aeuid_abs,
                           stringsAsFactors = FALSE)

  pname <- if (survey_year == 2018L) {
    "madip-ge-080107d-sdac2018-sdac18outper-2018.parquet"
  } else {
    "madipge-sdac-d-survey-2022.parquet"
  }
  out_path <- file.path(ds_dir, pname)

  n_rows <- project_sdac_to_parquet__(
    aeuid               = as.character(spine$aeuid_abs),
    birth_year          = as.integer(spine$birth_year),
    sex                 = as.integer(spine$sex),
    state               = as.integer(spine$state),
    indigenous          = as.integer(spine$indigenous),
    country_of_birth    = as.integer(spine$country_of_birth),
    education           = as.integer(spine$education),
    baseline_employed   = as.integer(spine$baseline_employed),
    disability_severity = as.integer(spine$disability_severity),
    disability_onset_year = if (!is.null(spine$disability_onset_year)) {
      as.integer(spine$disability_onset_year)
    } else {
      rep(NA_integer_, nrow(spine))
    },
    person_type         = as.integer(spine$person_type),
    seed                = as.integer(seed + 3500L),
    survey_year         = survey_year,
    out_path            = out_path
  )

  write_agency_spine(mini_spine, "ABS", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(out_path)) {
    return(as.data.frame(read_parquet_safely(out_path)))
  }
  invisible(list(n_rows = as.integer(n_rows), survey_year = survey_year))
}

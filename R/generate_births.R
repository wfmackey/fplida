#' Generate BIRTHS dataset (Birth Registrations)
#'
#' @section Dataset and variable information:
#' The [ABS births methodology](https://www.abs.gov.au/methodologies/births-australia-methodology/2024)
#' gives information about this dataset. Use `dataset_info("BIRTHS")` for
#' dataset information. Use `variable_info("BIRTHS")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_births <- function(spine = NULL, seed = 42L, output_dir = NULL,
                            format = c("parquet", "csv"),
                            return_data = FALSE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("births writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "BIRTHS")
  if (!dir.exists(ds_dir)) dir.create(ds_dir, recursive = TRUE)

  births_cols <- c("spine_id", "aeuid_rbdm", "birth_year", "sex", "state",
                   "indigenous", "country_of_birth")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, births_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id,
                           aeuid_rbdm = spine$aeuid_rbdm,
                           stringsAsFactors = FALSE)

  primary_path <- file.path(ds_dir, "plidage-births-2006-latest.parquet")
  # The births product covers the 2006-latest registration window, so only
  # Australian-born persons born from 2006 onward are registered here (not the
  # entire Australian-born population back to the early 1900s).
  aus_born <- which(spine$country_of_birth == 0L & spine$birth_year >= 2006L)

  if (length(aus_born) == 0L) {
    n_rows <- project_births_to_parquet__(
      aeuid            = character(0),
      spine_id         = character(0),
      birth_year       = integer(0),
      sex              = integer(0),
      state            = integer(0),
      indigenous       = integer(0),
      country_of_birth = integer(0),
      seed             = as.integer(seed + 1300L),
      out_path         = primary_path
    )
  } else {
    n_rows <- project_births_to_parquet__(
      aeuid            = as.character(spine$aeuid_rbdm[aus_born]),
      spine_id         = as.character(spine$spine_id[aus_born]),
      birth_year       = as.integer(spine$birth_year[aus_born]),
      sex              = as.integer(spine$sex[aus_born]),
      state            = as.integer(spine$state[aus_born]),
      indigenous       = as.integer(spine$indigenous[aus_born]),
      country_of_birth = as.integer(spine$country_of_birth[aus_born]),
      seed             = as.integer(seed + 1300L),
      out_path         = primary_path
    )
  }

  write_agency_spine(mini_spine, "RBDM", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(primary_path)) {
    return(as.data.frame(read_parquet_safely(primary_path)))
  }
  invisible(list(n_rows = as.integer(n_rows)))
}

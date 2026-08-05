#' Generate SDB dataset (Settlements Database)
#'
#' @section Dataset and variable information:
#' The [Home Affairs settlement reporting caveats](https://immi.homeaffairs.gov.au/settling-in-australia/settlement-reports/settlement-reporting-caveats)
#' give information about this dataset. Use `dataset_info("SDB")` for dataset
#' information. Use `variable_info("SDB")` for variables, sources, value
#' support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_sdb <- function(spine = NULL, seed = 42L, output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = FALSE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("sdb writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "SDB")
  if (!dir.exists(ds_dir)) dir.create(ds_dir, recursive = TRUE)

  sdb_cols <- c("spine_id", "aeuid_ha", "birth_year", "sex", "state",
                "country_of_birth", "country_of_birth_sacc", "year_of_arrival", "education",
                "anzsco_code")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, sdb_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_ha = spine$aeuid_ha,
                           stringsAsFactors = FALSE)

  primary_path <- file.path(ds_dir, "madipge-mig-d-permanent-migrants-2000-current.parquet")
  overseas <- which(spine$country_of_birth != 0L)

  if (length(overseas) == 0L) {
    n_rows <- project_sdb_to_parquet__(
      aeuid            = character(0),
      birth_year       = integer(0),
      sex              = integer(0),
      state            = integer(0),
      country_of_birth = integer(0),
      year_of_arrival  = integer(0),
      education        = integer(0),
      anzsco_code      = character(0),
      seed             = as.integer(seed + 2000L),
      out_path         = primary_path
    )
  } else {
    n_rows <- project_sdb_to_parquet__(
      aeuid            = as.character(spine$aeuid_ha[overseas]),
      birth_year       = as.integer(spine$birth_year[overseas]),
      sex              = as.integer(spine$sex[overseas]),
      state            = as.integer(spine$state[overseas]),
      # Emit the SACC country code (cross-dataset consistent), not the 0/1 flag.
      # (CITIZENSHIP still mirrors this in Rust; a distinct citizenship code is
      # part of the Home Affairs phase.)
      country_of_birth = as.integer(spine$country_of_birth_sacc[overseas]),
      year_of_arrival  = as.integer(spine$year_of_arrival[overseas]),
      education        = as.integer(spine$education[overseas]),
      anzsco_code      = as.character(spine$anzsco_code[overseas]),
      seed             = as.integer(seed + 2000L),
      out_path         = primary_path
    )
  }

  write_agency_spine(mini_spine, "HA", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(primary_path)) {
    return(as.data.frame(read_parquet_safely(primary_path)))
  }
  invisible(list(n_rows = as.integer(n_rows)))
}

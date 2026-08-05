#' Generate SAE dataset (Superannuation Accounts Extract)
#'
#' @section Dataset and variable information:
#' The [ABS administrative income sources](https://www.abs.gov.au/statistics/detailed-methodology-information/concepts-sources-methods/administrative-income-comparison-studies/2019-20-2021/administrative-data-sources)
#' website gives information about this dataset. Use `dataset_info("SAE")` for
#' dataset information. Use `variable_info("SAE")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @param years Integer vector. Financial years to generate.
#' @export
generate_sae <- function(spine = NULL, seed = 42L, years = 2019L:2023L,
                         output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = FALSE) {
  seed <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  if (format != "parquet") stop("sae writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "SAE")

  sae_cols <- c("spine_id", "aeuid_ato", "birth_year", "sex", "state",
                "baseline_employed", "baseline_income")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, sae_cols)
  stopifnot(is.data.frame(spine))
  spine <- filter_ato_records(spine, reference_year = max(years))

  mini_spine <- data.frame(spine_id = spine$spine_id,
                            aeuid_ato = spine$aeuid_ato,
                            stringsAsFactors = FALSE)

  yr_range <- range(years)
  n_rows <- project_sae_to_parquet__(
    aeuid             = as.character(spine$aeuid_ato),
    birth_year        = as.integer(spine$birth_year),
    sex               = as.integer(spine$sex),
    state             = as.integer(spine$state),
    baseline_employed = as.integer(spine$baseline_employed),
    baseline_income   = as.numeric(spine$baseline_income),
    seed              = as.integer(seed + 2300L),
    fy_start          = yr_range[1L],
    fy_end            = yr_range[2L],
    out_dir           = ds_dir,
    product_prefix    = "madipmp-ato-maasmats-"
  )

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) {
    result <- list()
    for (fy in years) {
      fy_str <- sprintf("%d-%02d", fy - 1L, fy %% 100L)
      path <- file.path(ds_dir, sprintf("madipmp-ato-maasmats-%02d%02d.parquet",
                                        (fy - 1L) %% 100L, fy %% 100L))
      if (file.exists(path)) {
        result[[fy_str]] <- as.data.frame(read_parquet_safely(path))
      }
    }
    return(result)
  }
  invisible(list(n_rows = as.integer(n_rows), years = years))
}

#' Generate COMBINED dataset (Combined Demographics – Indigenous Status)
#'
#' @section Dataset and variable information:
#' The [ABS PLIDA Modular Product](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/person-level-integrated-data-asset-plida)
#' website gives information about this dataset. Use `dataset_info("COMBINED")`
#' for dataset information. Use `variable_info("COMBINED")` for variables,
#' sources, value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_combined <- function(spine = NULL, seed = 42L, output_dir = NULL,
                              format = c("parquet", "csv"),
                              return_data = FALSE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("combined writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "COMBINED")

  combined_cols <- c("spine_id", "aeuid_abs", "indigenous")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, combined_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_abs = spine$aeuid_abs,
                           stringsAsFactors = FALSE)

  product_codes <- c(
    "madipge-comb-d-demog-ind-c11-06-latest",
    "madipge-comb-d-demog-ind-c16-06-latest",
    "madipge-comb-d-demog-ind-c21-06-latest",
    "madipge-comb-d-demog-ind-nc-06-latest"
  )
  primary_path <- file.path(ds_dir, paste0(product_codes[1], ".parquet"))

  n_rows <- project_combined_indigenous_to_parquet__(
    spine_id   = as.character(spine$spine_id),
    indigenous = as.integer(spine$indigenous),
    out_path   = primary_path
  )

  for (pc in product_codes[-1]) {
    dst <- file.path(ds_dir, paste0(pc, ".parquet"))
    if (file.exists(primary_path)) file.copy(primary_path, dst, overwrite = TRUE)
  }

  write_agency_spine(mini_spine, "ABS", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(primary_path)) {
    return(as.data.frame(read_parquet_safely(primary_path)))
  }
  invisible(list(n_rows = as.integer(n_rows), products = product_codes))
}

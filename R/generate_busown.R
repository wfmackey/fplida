#' Generate BUSOWN dataset (Business Ownership)
#'
#' Thin R wrapper over `project_busown_to_parquet__`. All work —
#' owner determination, per-FY iteration, BLADE `bn` assignment, parquet
#' writing — happens in Rust. The R side loads the BLADE business pool,
#' loads spine columns and passes them through.
#'
#' @section Dataset and variable information:
#' The [ABS PLIDA Modular Product](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/person-level-integrated-data-asset-plida)
#' website gives information about this dataset. Use `dataset_info("BUSOWN")`
#' for dataset information. Use `variable_info("BUSOWN")` for variables,
#' sources, value support, and topic tags.
#'
#' @param spine Data.frame or NULL.
#' @param seed Integer. Random seed.
#' @param years Integer vector of FY end years.
#' @param output_dir Character or NULL.
#' @param format Character. "parquet" only.
#' @param return_data Logical. If TRUE, reads the per-FY files back.
#' @export
generate_busown <- function(spine = NULL, seed = 42L, years = 2010L:2023L,
                            output_dir = NULL,
                            format = c("parquet", "csv"),
                            return_data = FALSE) {
  seed <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  if (format != "parquet") {
    stop("generate_busown() now writes parquet only.", call. = FALSE)
  }

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "BUSOWN")

  # Business-owner identifiers are legacy-named `ABN_HASH_TRUNC`, but in this
  # synthetic BLADE-aware build they should resolve to real BLADE `bn` records.
  .set_business_pool_from_spine(run_dir)

  bo_cols <- c("spine_id", "aeuid_ato", "birth_year",
               "baseline_employed", "baseline_income")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, bo_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))
  spine <- filter_ato_records(spine, reference_year = max(years))

  mini_spine <- data.frame(
    spine_id  = spine$spine_id,
    aeuid_ato = spine$aeuid_ato,
    stringsAsFactors = FALSE
  )

  yr_range <- range(years)
  n_rows <- project_busown_to_parquet__(
    aeuid             = as.character(spine$aeuid_ato),
    birth_year        = as.integer(spine$birth_year),
    baseline_employed = as.integer(spine$baseline_employed),
    seed              = as.integer(seed + 2200L),
    fy_start          = yr_range[1L],
    fy_end            = yr_range[2L],
    out_dir           = ds_dir,
    product_prefix    = "madipge-ato-d-business-owners-fy"
  )

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) {
    result <- list()
    for (fy in years) {
      fy_str <- sprintf("%d-%02d", fy - 1L, fy %% 100L)
      path <- file.path(ds_dir, sprintf("madipge-ato-d-business-owners-fy%02d%02d.parquet",
                                        (fy - 1L) %% 100L, fy %% 100L))
      if (file.exists(path)) {
        result[[fy_str]] <- as.data.frame(read_parquet_safely(path),
                                          stringsAsFactors = FALSE)
      }
    }
    return(result)
  }
  invisible(list(n_rows = as.integer(n_rows), years = years))
}

#' Generate AEDC dataset (Australian Early Development Census)
#'
#' @section Dataset and variable information:
#' The [Department of Education AEDC](https://www.education.gov.au/early-childhood/about/data-and-reports/australian-early-development-census)
#' website gives information about this dataset. Use `dataset_info("AEDC")` for
#' dataset information. Use `variable_info("AEDC")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @param cycles Integer vector. AEDC cycle years to generate.
#' @export
generate_aedc <- function(spine = NULL, seed = 42L,
                          cycles = c(2009L, 2012L, 2015L, 2018L, 2021L, 2024L),
                          output_dir = NULL,
                          format = c("parquet", "csv"),
                          return_data = FALSE) {
  seed <- as.integer(seed)
  cycles <- as.integer(cycles)
  format <- match.arg(format)
  if (format != "parquet") stop("aedc writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "AEDC")

  aedc_cols <- c("spine_id", "aeuid_de", "birth_year", "sex", "state",
                 "indigenous", "country_of_birth")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, aedc_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_de = spine$aeuid_de,
                           stringsAsFactors = FALSE)
  product_families <- c("core", "domain", "indigenous", "language", "specialneeds")

  for (cy in cycles) {
    out_path <- file.path(ds_dir, sprintf("madipge-aedc-d-core-%d.parquet", cy))
    project_aedc_to_parquet__(
      aeuid            = as.character(spine$aeuid_de),
      birth_year       = as.integer(spine$birth_year),
      sex              = as.integer(spine$sex),
      state            = as.integer(spine$state),
      indigenous       = as.integer(spine$indigenous),
      country_of_birth = as.integer(spine$country_of_birth),
      seed             = as.integer(seed + 3300L + cy - 2009L),
      cycle_year       = as.integer(cy),
      out_path         = out_path
    )

    # The Rust writer emits a compact AEDC superset. Give every delivered
    # product an exact-product source so DIL routing does not borrow another
    # cycle or product when it materialises its canonical table.
    for (family in setdiff(product_families, "core")) {
      product_path <- file.path(
        ds_dir,
        sprintf("madipge-aedc-d-%s-%d.parquet", family, cy)
      )
      copied <- file.copy(out_path, product_path, overwrite = TRUE)
      if (!isTRUE(copied)) {
        stop(sprintf("Could not write AEDC %s product for %d.", family, cy),
             call. = FALSE)
      }
    }
  }

  write_agency_spine(mini_spine, "DE", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  invisible(list(cycles = cycles, products = product_families))
}

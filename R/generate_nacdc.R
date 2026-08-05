#' Generate NACDC dataset (National Aged Care Data Clearinghouse)
#'
#' @section Dataset and variable information:
#' The [AIHW NACDC overview](https://www.gen-agedcaredata.gov.au/about-our-data/national-aged-care-data-clearinghouse)
#' gives information about this dataset. Use `dataset_info("NACDC")` for
#' dataset information. Use `variable_info("NACDC")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_nacdc <- function(spine = NULL, seed = 42L, output_dir = NULL,
                           format = c("parquet", "csv"),
                           return_data = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("nacdc writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "NACDC")

  nacdc_cols <- c("spine_id", "aeuid_aihw", "birth_year", "sex", "state",
                  "indigenous", "country_of_birth_sacc", "sa2_code")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, nacdc_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_aihw = spine$aeuid_aihw,
                           stringsAsFactors = FALSE)

  product_names <- c(
    aged_recipient = "madipge-aged-care-d-agedrecipient-15-current",
    aged_service = "madipge-aged-care-d-agedservice-15-current",
    chsp = "madipge-aged-care-d-chsp-16-current",
    hcp = "madipge-aged-care-d-hcp-15-current",
    rac = "madipge-aged-care-d-rac-15-current",
    tcp = "madipge-aged-care-d-tcp-15-current"
  )
  out_paths <- stats::setNames(
    file.path(ds_dir, paste0(unname(product_names), ".parquet")),
    names(product_names)
  )
  n_rows <- project_nacdc_to_parquet__(
    aeuid            = as.character(spine$aeuid_aihw),
    birth_year       = as.integer(spine$birth_year),
    sex              = as.integer(spine$sex),
    state            = as.integer(spine$state),
    indigenous       = as.integer(spine$indigenous),
    # SACC country code (cross-dataset consistent), not the 0/1 flag.
    country_of_birth = as.integer(spine$country_of_birth_sacc),
    sa2              = as.integer(spine$sa2_code),
    seed             = as.integer(seed + 3200L),
    reference_year   = 2025L,
    aged_recipient_out_path = out_paths[["aged_recipient"]],
    aged_service_out_path   = out_paths[["aged_service"]],
    chsp_out_path           = out_paths[["chsp"]],
    hcp_out_path            = out_paths[["hcp"]],
    rac_out_path            = out_paths[["rac"]],
    tcp_out_path            = out_paths[["tcp"]]
  )

  write_agency_spine(mini_spine, "AIHW", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(out_paths[["aged_recipient"]])) {
    return(as.data.frame(read_parquet_safely(
      out_paths[["aged_recipient"]]
    )))
  }
  invisible(list(n_rows = as.integer(n_rows), product_files = out_paths))
}

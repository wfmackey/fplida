#' Generate ATO Income Tax Return data (PIT_ITR)
#'
#' `generate_pit_itr()` writes synthetic Income Tax Return (`PIT_ITR`)
#' product tables. The covered population contains people who lodged an
#' individual income tax return. Each record represents one person in one
#' financial year. The data does not include non-lodgers.
#'
#' The function reads `PIT_PS` files and the shared occupation panel. The Rust
#' pipeline writes four product-table types for each requested financial year.
#'
#' @section Dataset and variable information:
#' The [ABS administrative income sources](https://www.abs.gov.au/statistics/detailed-methodology-information/concepts-sources-methods/administrative-income-comparison-studies/2019-20-2021/administrative-data-sources)
#' website gives information about this dataset. Use `dataset_info("PIT_ITR")`
#' for dataset information. Use `variable_info("PIT_ITR")` for variables,
#' sources, value support, and topic tags.
#'
#' @section Occupation codes are not ANZSCO:
#' `IDV_OCPTN_CD` and the other occupation fields hold ATO salary and wage
#' occupation codes, not ANZSCO. Taxpayers choose a six-digit code from the
#' ATO's own
#' [published list](https://www.ato.gov.au/forms-and-instructions/salary-and-wage-occupation-codes)
#' at question 1 of the individual return, and that list does not map
#' one-to-one onto ANZSCO: of the 1,167 ATO codes for 2025-26, 951 are also
#' valid ANZSCO 2019 codes and 216 are not.
#'
#' Each person carries an ANZSCO occupation on the shared spine. The ATO
#' products map it onto a real ATO code through a bundled crosswalk, resolving
#' up the code hierarchy when there is no exact counterpart. The person's
#' occupation therefore stays consistent across products while the code system
#' matches the source. Regenerate the crosswalk with
#' `data-raw/update_ato_occupation_codes.R`.
#'
#' @param spine A data frame from [generate_spine()], or `NULL`. If the value is
#'   `NULL`, the function reads the spine from the run directory.
#'
#' @param seed An integer random seed.
#'
#' @param years A vector of financial-year end years.
#'
#' @param output_dir The base output directory, or `NULL`.
#'
#' @param format The output format. This function supports `"parquet"` only.
#'
#' @param return_data This argument has no effect. The function always writes
#'   the data to disk.
#'
#' @return An invisible metadata list with `n_filers`, `n_by_year`, `years`,
#'   and `path`.
#'
#' @seealso [generate_pit_ps()], [generate_pit_ie()], and [build_fplida()].
#'
#' @export
generate_pit_itr <- function(spine = NULL, seed = 42L, years = 2010:2024,
                             output_dir = NULL,
                             format = c("parquet", "csv"),
                             return_data = FALSE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  stopifnot(!is.na(seed))
  years <- sort(as.integer(years))
  if (format != "parquet") {
    stop("generate_pit_itr() supports parquet only.", call. = FALSE)
  }

  run_dir <- resolve_run_dir(output_dir)

  itr_cols <- c("spine_id", "aeuid_ato", "anzsco_code", "industry",
                "archetype", "birth_year", "baseline_employed",
                "baseline_income")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, itr_cols)
  }
  stopifnot(is.data.frame(spine))
  spine <- filter_ato_records(spine, reference_year = max(years))

  mini_spine <- data.frame(
    spine_id  = spine$spine_id,
    aeuid_ato = spine$aeuid_ato,
    stringsAsFactors = FALSE
  )

  ds_dir <- dataset_dir(run_dir, "PIT_ITR")
  ps_dir <- file.path(run_dir, "ato-pit_ps")
  if (!dir.exists(ps_dir)) {
    stop("PIT_PS data not found at ", ps_dir,
         ". Run generate_pit_ps() first.", call. = FALSE)
  }

  # Per-year PS parquet paths + matched year vector.
  ps_file_paths <- character(0)
  ps_years_v <- integer(0)
  for (yr in years) {
    pname <- pit_ps_product_name(yr)
    p <- file.path(ps_dir, paste0(pname, ".parquet"))
    if (file.exists(p)) {
      ps_file_paths <- c(ps_file_paths, p)
      ps_years_v    <- c(ps_years_v, as.integer(yr))
    }
  }
  if (length(ps_file_paths) == 0L) {
    stop("No PIT_PS parquet files found for years ",
         paste(years, collapse = ", "), call. = FALSE)
  }

  # Occupation panel parquet files (prefer per-year parts, fall back to
  # the consolidated single file if present).
  occ_paths <- character(0)
  occ_parts <- file.path(run_dir, "_system", "occ_panel_parts")
  if (dir.exists(occ_parts)) {
    occ_paths <- list.files(occ_parts, pattern = "\\.parquet$", full.names = TRUE)
  }
  if (!length(occ_paths)) {
    single <- file.path(run_dir, "_system", "occupation_panel.parquet")
    if (file.exists(single)) occ_paths <- single
  }

  # Flattened product names: 4 per year × n_years
  # Order per year: context, inc-loss, ded-exp-off, whld-debt.
  tbl_types <- c("context", "inc-loss", "ded-exp-off", "whld-debt")
  pnames_flat <- character(4L * length(years))
  for (i in seq_along(years)) {
    for (j in seq_along(tbl_types)) {
      pnames_flat[(i - 1L) * 4L + j] <- itr_product_name(years[i], tbl_types[j])
    }
  }

  res <- generate_pit_itr_full_to_parquet__(
    spine_aeuid             = as.character(spine$aeuid_ato),
    # ATO occupation codes, not ANZSCO — see R/ato_occupation.R.
    spine_anzsco            = .ato_occupation_code(spine$anzsco_code),
    spine_industry          = as.integer(spine$industry),
    spine_archetype         = as.integer(spine$archetype),
    spine_birth_yr          = as.integer(spine$birth_year),
    ps_file_paths           = as.character(ps_file_paths),
    ps_years                = as.integer(ps_years_v),
    occ_panel_paths         = as.character(occ_paths),
    years                   = as.integer(years),
    product_name_by_yr_type = as.character(pnames_flat),
    out_dir                 = ds_dir,
    seed                    = seed
  )

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  n_filers <- as.integer(res$n_filers)
  names(n_filers) <- as.character(res$year)
  invisible(list(n_filers = sum(n_filers), n_by_year = n_filers,
                 years = years, path = "ato-pit_itr"))
}

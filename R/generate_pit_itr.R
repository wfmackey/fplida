#' Generate ATO Income Tax Return data (PIT_ITR)
#'
#' `generate_pit_itr()` writes synthetic Income Tax Return (`PIT_ITR`)
#' product tables. The covered population contains people who lodged an
#' individual income tax return. Each record represents one person in one
#' financial year. The data does not include non-lodgers.
#'
#' The function reads `PIT_PS` files where that source covers the requested
#' year. For other years covered by `PIT_ITR`, it uses the shared employment
#' panel and wage ledger without writing additional payment-summary products.
#' The Rust pipeline writes four product-table types per financial year.
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
  years <- sort(unique(as.integer(years)))
  years <- gate_dataset_years("PIT_ITR", years)
  if (length(years) == 0L) return(invisible(NULL))
  if (format != "parquet") {
    stop("generate_pit_itr() supports parquet only.", call. = FALSE)
  }

  run_dir <- resolve_run_dir(output_dir)

  # Retain attributes for all PS filers. Filtering this lookup at the latest
  # requested year can remove an earlier filer when another year is added.
  itr_cols <- c("spine_id", "id", "aeuid_ato", "anzsco_code", "industry",
                "archetype", "residency_status", "birth_year",
                "baseline_employed", "baseline_income", "baseline_hours",
                "anzsco_major", "task_physical", "disability_onset_year",
                "is_dc", "disability_severity", "disability_dose")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, itr_cols)
  }
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(
    spine_id  = spine$spine_id,
    aeuid_ato = spine$aeuid_ato,
    stringsAsFactors = FALSE
  )

  ds_dir <- dataset_dir(run_dir, "PIT_ITR")
  ps_dir <- file.path(run_dir, "ato-pit_ps")

  # Per-year PS parquet paths + matched year vector. A financial year can be
  # published as several extracts of the same payment summaries, so this takes
  # the most complete one rather than any file matching the year.
  ps_file_paths <- character(0)
  ps_years_v <- integer(0)
  ps_required <- intersect(years, .pit_ps_valid_years())
  for (yr in ps_required) {
    p <- .pit_ps_primary_path(ps_dir, yr)
    if (is.na(p) || !file.exists(p)) {
      stop("PIT_PS input missing for financial year ending ", yr,
           ". Run generate_pit_ps() for that year first.", call. = FALSE)
    }
    ps_file_paths <- c(ps_file_paths, p)
    ps_years_v    <- c(ps_years_v, as.integer(yr))
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

  # Match the employment generator's optional-column defaults. The fallback
  # uses the base seed, so its wages agree with the other labour products.
  n <- nrow(spine)
  optional <- function(name, default) {
    if (is.null(spine[[name]])) rep(default, n) else spine[[name]]
  }
  occupation_crosswalk <- .ato_occupation_crosswalk()
  fallback_years <- setdiff(years, ps_years_v)
  fallback_eligible <- unlist(lapply(fallback_years, function(year) {
    .ato_record_mask(spine, reference_year = year)
  }), use.names = FALSE)

  res <- generate_pit_itr_full_to_parquet__(
    spine_aeuid             = as.character(spine$aeuid_ato),
    # ATO occupation codes, not ANZSCO — see R/ato_occupation.R.
    spine_anzsco            = .ato_occupation_code(spine$anzsco_code),
    spine_industry          = as.integer(spine$industry),
    spine_archetype         = as.integer(spine$archetype),
    spine_residency         = as.integer(spine$residency_status),
    spine_birth_yr          = as.integer(spine$birth_year),
    ps_file_paths           = as.character(ps_file_paths),
    ps_years                = as.integer(ps_years_v),
    occ_panel_paths         = as.character(occ_paths),
    years                   = as.integer(years),
    product_name_by_yr_type = as.character(pnames_flat),
    out_dir                 = ds_dir,
    seed                    = seed,
    spine_id                = as.character(spine$id),
    baseline_employed       = as.integer(spine$baseline_employed),
    baseline_income         = as.double(spine$baseline_income),
    baseline_hours          = as.integer(spine$baseline_hours),
    anzsco_major            = as.integer(spine$anzsco_major),
    anzsco_code             = as.integer(spine$anzsco_code),
    task_physical           = as.double(optional("task_physical", 0.3)),
    disability_onset_year   = as.integer(optional("disability_onset_year", NA_integer_)),
    disability_is_dc        = as.integer(optional("is_dc", NA_integer_)),
    disability_severity     = as.integer(optional("disability_severity", NA_integer_)),
    disability_dose         = as.double(optional("disability_dose", NA_real_)),
    fallback_eligible       = as.integer(fallback_eligible),
    crosswalk_anzsco        = as.integer(names(occupation_crosswalk)),
    crosswalk_ato           = as.integer(occupation_crosswalk)
  )

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  n_filers <- as.integer(res$n_filers)
  names(n_filers) <- as.character(res$year)
  invisible(list(n_filers = sum(n_filers), n_by_year = n_filers,
                 years = years, path = "ato-pit_itr"))
}

#' Generate ATO Payment Summary data (PIT_PS)
#'
#' `generate_pit_ps()` writes synthetic Payment Summary (`PIT_PS`) product
#' tables. The covered population contains people with an employer payment
#' summary. Each record represents one person, one payer, and one financial
#' year.
#'
#' The Rust pipeline uses the shared employment panel. It also writes the
#' occupation-panel files that [generate_pit_itr()] uses.
#'
#' @section Dataset and variable information:
#' The [ABS administrative income sources](https://www.abs.gov.au/statistics/detailed-methodology-information/concepts-sources-methods/administrative-income-comparison-studies/2019-20-2021/administrative-data-sources)
#' website gives information about this dataset. Use `dataset_info("PIT_PS")`
#' for dataset information. Use `variable_info("PIT_PS")` for variables,
#' sources, value support, and topic tags.
#'
#' @section Occupation codes are not ANZSCO:
#' The occupation fields, and the occupation panel this function writes for
#' [generate_pit_itr()], hold ATO salary and wage occupation codes rather than
#' ANZSCO. See `?generate_pit_itr` for the mapping, and the ATO's
#' [published list](https://www.ato.gov.au/forms-and-instructions/salary-and-wage-occupation-codes).
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
#' @return An invisible metadata list with `n_records`, `n_rows`, `years`, and
#'   `path`.
#'
#' @seealso [generate_pit_itr()], [generate_pit_ie()], and [build_fplida()].
#'
#' @export
generate_pit_ps <- function(spine = NULL, seed = 42L, years = 2010:2024,
                            output_dir = NULL,
                            format = c("parquet", "csv"),
                            return_data = FALSE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  stopifnot(!is.na(seed))
  years <- sort(as.integer(years))
  if (format != "parquet") {
    stop("generate_pit_ps() supports parquet only.", call. = FALSE)
  }

  run_dir <- resolve_run_dir(output_dir)

  # Draw payment-summary employers from the BLADE business universe (built
  # earlier) so EMPLOYER_ABN values resolve to BLADE businesses.
  .set_business_pool_from_spine(run_dir)

  ps_cols <- c("spine_id", "aeuid_ato", "id", "birth_year",
               "baseline_employed", "baseline_income", "baseline_hours",
               "anzsco_major", "anzsco_code", "industry",
               "task_physical", "archetype",
               "disability_onset_year", "is_dc",
               "disability_severity", "disability_dose")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, ps_cols)
  }
  stopifnot(is.data.frame(spine))
  spine <- filter_ato_records(spine, reference_year = max(years))

  mini_spine <- data.frame(
    spine_id  = spine$spine_id,
    aeuid_ato = spine$aeuid_ato,
    stringsAsFactors = FALSE
  )

  ds_dir  <- dataset_dir(run_dir, "PIT_PS")
  occ_dir <- file.path(run_dir, "_system", "occ_panel_parts")
  if (!dir.exists(occ_dir)) dir.create(occ_dir, recursive = TRUE)

  # Resolve canonical product names per year.
  product_names <- vapply(years, pit_ps_product_name, character(1))

  # Defensive defaults for optional spine columns.
  n <- nrow(spine)
  dis_onset <- if (!is.null(spine$disability_onset_year))
    as.integer(spine$disability_onset_year) else rep(NA_integer_, n)
  dis_dc <- if (!is.null(spine$is_dc))
    as.integer(spine$is_dc) else rep(NA_integer_, n)
  dis_sev <- if (!is.null(spine$disability_severity))
    as.integer(spine$disability_severity) else rep(NA_integer_, n)
  dis_dose <- if (!is.null(spine$disability_dose))
    as.double(spine$disability_dose) else rep(NA_real_, n)
  anz_code <- if (!is.null(spine$anzsco_code))
    as.integer(spine$anzsco_code) else as.integer(spine$anzsco_major * 1000L)
  # The ATO occupation field is not ANZSCO: taxpayers pick from the ATO's own
  # published code list. Map the person's ANZSCO occupation onto a real ATO
  # code so the occupation stays consistent across products while the code
  # system matches the source. See R/ato_occupation.R.
  ato_occ_code <- .ato_occupation_code(anz_code)
  t_phys <- if (!is.null(spine$task_physical))
    as.double(spine$task_physical) else rep(0.3, n)
  arch <- if (!is.null(spine$archetype))
    as.integer(spine$archetype) else rep(0L, n)

  res <- generate_pit_ps_full_to_parquet__(
    id                    = as.character(spine$id),
    aeuid_ato             = as.character(spine$aeuid_ato),
    birth_year            = as.integer(spine$birth_year),
    baseline_employed     = as.integer(spine$baseline_employed),
    baseline_income       = as.double(spine$baseline_income),
    baseline_hours        = as.integer(spine$baseline_hours),
    anzsco_major          = as.integer(spine$anzsco_major),
    industry              = as.integer(spine$industry),
    anzsco_code           = ato_occ_code,
    task_physical         = t_phys,
    archetype             = arch,
    disability_onset_year = dis_onset,
    disability_is_dc      = dis_dc,
    disability_severity   = dis_sev,
    disability_dose       = dis_dose,
    years                 = as.integer(years),
    seed                  = seed,
    out_dir               = ds_dir,
    occ_out_dir           = occ_dir,
    product_names         = as.character(product_names)
  )

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  n_rows <- as.integer(res$n_rows)
  names(n_rows) <- as.character(res$year)
  invisible(list(n_records = sum(n_rows), n_rows = n_rows,
                 years = years, path = "ato-pit_ps"))
}

#' Generate ABS Derived Income data (PIT_IE)
#'
#' `generate_pit_ie()` writes synthetic ABS Derived Income (`PIT_IE`) product
#' tables. The covered population contains people with income derived from
#' Income Tax Return and Payment Summary data. Each record represents one
#' person in one financial year.
#'
#' The Rust pipeline calculates each income component and writes one Parquet
#' file for each requested financial year.
#'
#' @section Dataset and variable information:
#' The [ABS administrative income sources](https://www.abs.gov.au/statistics/detailed-methodology-information/concepts-sources-methods/administrative-income-comparison-studies/2019-20-2021/administrative-data-sources)
#' website gives information about this dataset. Use `dataset_info("PIT_IE")`
#' for dataset information. Use `variable_info("PIT_IE")` for variables,
#' sources, value support, and topic tags.
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
#' @param return_data If `TRUE`, the function reads each output file and returns
#'   a list of data frames.
#'
#' @return If `return_data` is `TRUE`, a named list of data frames. Otherwise,
#'   an invisible metadata list with `n_rows` and `years`.
#'
#' @seealso [generate_pit_itr()], [generate_pit_ps()], and [build_fplida()].
#'
#' @export
generate_pit_ie <- function(spine = NULL, seed = 42L, years = 2011L:2023L,
                            output_dir = NULL,
                            format = c("parquet", "csv"),
                            return_data = FALSE) {
  seed <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  if (format != "parquet") {
    stop("generate_pit_ie() supports parquet only.", call. = FALSE)
  }

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "PIT_IE")

  # The wages line (WANDS) is the reconciled per-person-FY total from the shared
  # employment panel, so PIT_IE needs the same spine columns the panel reads
  # (identical to PIT_PS) — built with the BASE seed so it matches STP/PAYG.
  ie_cols <- c("spine_id", "aeuid_ato", "id", "birth_year",
               "baseline_employed", "baseline_income", "baseline_hours",
               "anzsco_major", "anzsco_code", "industry", "task_physical",
               "archetype", "disability_onset_year", "is_dc",
               "disability_severity", "disability_dose", "education")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, ie_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))
  spine <- filter_ato_records(spine, reference_year = max(years))

  mini_spine <- data.frame(
    spine_id  = spine$spine_id,
    aeuid_ato = spine$aeuid_ato,
    stringsAsFactors = FALSE
  )

  yr_range <- range(years)
  n_rows <- project_pit_ie_to_parquet__(
    id                    = as.character(spine$id),
    aeuid                 = as.character(spine$aeuid_ato),
    birth_year            = as.integer(spine$birth_year),
    baseline_employed     = as.integer(spine$baseline_employed),
    baseline_income       = as.numeric(spine$baseline_income),
    baseline_hours        = as.integer(spine$baseline_hours),
    anzsco_major          = as.integer(spine$anzsco_major),
    anzsco_code           = as.integer(spine$anzsco_code),
    industry              = as.integer(spine$industry),
    archetype             = as.integer(spine$archetype),
    disability_onset_year = as.integer(spine$disability_onset_year),
    disability_is_dc      = as.integer(spine$is_dc),
    disability_severity   = as.integer(spine$disability_severity),
    disability_dose       = as.numeric(spine$disability_dose),
    task_physical         = as.numeric(spine$task_physical),
    education             = as.integer(spine$education),
    seed                  = as.integer(seed),
    fy_start              = yr_range[1L],
    fy_end                = yr_range[2L],
    out_dir               = ds_dir,
    product_fmt           = "madipge-ato-d-pit-income-edited"
  )

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) {
    result <- list()
    for (fy in years) {
      fy_str <- sprintf("%d-%02d", fy - 1L, fy %% 100L)
      fname <- if (fy <= 2021L) {
        sprintf("madipge-ato-d-pit-income-edited-%02d-%02d.parquet",
                (fy - 1L) %% 100L, fy %% 100L)
      } else {
        sprintf("madipge-ato-d-pit-income-edited-fy%02d%02d.parquet",
                (fy - 1L) %% 100L, fy %% 100L)
      }
      path <- file.path(ds_dir, fname)
      if (file.exists(path)) {
        result[[fy_str]] <- as.data.frame(read_parquet_safely(path),
                                          stringsAsFactors = FALSE)
      }
    }
    return(result)
  }
  invisible(list(n_rows = as.integer(n_rows), years = years))
}

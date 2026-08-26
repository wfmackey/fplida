#' BUSOWN output tables for a set of financial years
#'
#' BUSOWN is published split by legal form: `ato_partnerships_fyXXXX` and
#' `ato_sole_traders_fyXXXX`, with 12- and 16-month extract variants from
#' 2021-22. The split is read from the bundled data item list rather than
#' typed here, so the emitted tables cannot drift from the registry.
#'
#' @param years Integer vector of financial-year end years.
#' @return A data.frame with one row per output file: `stem`, `form`
#'   (0 sole trader, 1 partnership), `fy`, `months`, `extract_ref` and
#'   `key_var`, the business identifier column the registry declares.
#' @keywords internal
.busown_file_plan <- function(years) {
  variables <- utils::read.csv(
    .dil_metadata_path("variables.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  variables <- variables[variables$Dataset == "BUSOWN", , drop = FALSE]
  if (!nrow(variables)) {
    stop("No BUSOWN structures in the bundled data item list.", call. = FALSE)
  }

  product <- variables[["Product Name"]]
  table <- variables[["Table Name"]]
  keep <- nzchar(product) & nzchar(table)
  product <- product[keep]
  table <- table[keep]
  variable <- variables[["Variable Name"]][keep]

  structures <- unique(data.frame(product = product, table = table,
                                  stringsAsFactors = FALSE))
  structures <- structures[order(structures$product, structures$table), ,
                           drop = FALSE]

  lower <- tolower(structures$table)
  form <- ifelse(grepl("partner", lower), 1L,
                 ifelse(grepl("sole", lower), 0L, NA_integer_))

  # `..._fy2122` and `..._fy2223_16m` both end their financial year in the
  # second pair of digits.
  fy <- rep(NA_integer_, length(lower))
  dated <- grepl("_fy[0-9]{4}", lower)
  fy[dated] <- 2000L + as.integer(
    sub("^.*_fy[0-9]{2}([0-9]{2}).*$", "\\1", lower[dated])
  )

  # An extract window longer than the financial year is named in the table,
  # e.g. `ato_sole_trader_fy2223_16m`.
  months <- rep(12L, length(lower))
  windowed <- grepl("_[0-9]+m$", lower)
  months[windowed] <- as.integer(
    sub("^.*_([0-9]+)m$", "\\1", lower[windowed])
  )

  has_extract_ref <- vapply(structures$table, function(tb) {
    any(variable[table == tb] == "EXTRACT_REF")
  }, logical(1))

  # Which identifier a table keys its businesses on. The delivery changed
  # hashing part-way through 2021-22, and the change is per table rather than
  # per financial year: the 12-month extracts for 2021-22 still carry
  # `ABN_HASH_TRUNC` while the 16-month re-extracts for the same year carry
  # `BN`. So the registry decides, never a year threshold. Exactly one of the
  # two must appear, so a registry refresh that introduces a third spelling
  # fails here rather than writing a column under the wrong name.
  key_var <- vapply(structures$table, function(tb) {
    found <- intersect(c("BN", "ABN_HASH_TRUNC"), variable[table == tb])
    if (length(found) != 1L) {
      stop("BUSOWN table ", tb, " declares ", length(found),
           " business identifier columns; expected exactly one of BN or ",
           "ABN_HASH_TRUNC.", call. = FALSE)
    }
    found
  }, character(1))

  plan <- data.frame(
    stem = vapply(seq_len(nrow(structures)), function(i) {
      .dil_structure_stem(structures$product[i], structures$table[i])
    }, character(1)),
    form = form,
    fy = fy,
    months = months,
    extract_ref = as.integer(has_extract_ref),
    key_var = unname(key_var),
    stringsAsFactors = FALSE
  )
  plan <- plan[!is.na(plan$form) & !is.na(plan$fy), , drop = FALSE]
  plan <- plan[plan$fy %in% as.integer(years), , drop = FALSE]
  rownames(plan) <- NULL
  plan
}


#' Generate BUSOWN dataset (Business Ownership)
#'
#' BUSOWN is the concordance that links a person to the business they operate,
#' published split by legal form. Businesses are drawn first and their owners
#' second: a sole trader has one owner, a partnership has two or more, drawn
#' from a household because spousal and family partnerships are the common
#' Australian form. Each business trades over a spell of financial years
#' rather than existing in every year of the window.
#'
#' The generator runs centrally on the full spine rather than per slice.
#' Slices are contiguous row ranges and households are scattered across them,
#' so a sliced build would split most multi-person households and lose their
#' partnerships.
#'
#' @section Which identifier names the business:
#' The tables delivered up to 2021-22 name the business in `ABN_HASH_TRUNC`
#' and the tables from 2021-22 on name it in `BN`. Which one a table carries
#' comes from the bundled data item list, not from the financial year: 2021-22
#' is mixed, with `ABN_HASH_TRUNC` on the 12-month extracts and `BN` on the
#' 16-month re-extracts. The two are different hashings of the same ABN and
#' never share a value, so joining across the change needs the
#' `blade-key-abn-hash-trunc-to-bn-key` product in `abs-blade`.
#'
#' `ABN_HASH_TRUNC` and `SYNTHETIC_AEUID` are both 12 hexadecimal characters
#' and look alike. They are never joined to one another: the first names a
#' business and the second names a person.
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
#' @param return_data Logical. If TRUE, reads the written tables back.
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

  # Every business drawn here is a real BLADE business, so a register or BAS
  # join lands whichever era the file belongs to. The tables from 2021-22 on
  # publish that `bn` directly; the tables to 2021-22 publish its
  # `abn_hash_trunc`, and the correspondence key bridges the two.
  .set_business_pool_from_spine(run_dir)

  plan <- .busown_file_plan(years)
  if (!nrow(plan)) {
    return(invisible(list(n_rows = 0L, years = years, files = character(0))))
  }

  bo_cols <- c("spine_id", "aeuid_ato", "birth_year", "household_id",
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
    aeuid            = as.character(spine$aeuid_ato),
    birth_year       = as.integer(spine$birth_year),
    household_id     = as.character(spine$household_id),
    seed             = as.integer(seed + 2200L),
    fy_start         = yr_range[1L],
    fy_end           = yr_range[2L],
    out_dir          = ds_dir,
    file_stem        = as.character(plan$stem),
    file_form        = as.integer(plan$form),
    file_fy          = as.integer(plan$fy),
    file_months      = as.integer(plan$months),
    file_extract_ref = as.integer(plan$extract_ref),
    file_key_var     = as.character(plan$key_var)
  )

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) {
    result <- list()
    for (i in seq_len(nrow(plan))) {
      path <- file.path(ds_dir, paste0(plan$stem[i], ".parquet"))
      if (file.exists(path)) {
        result[[plan$stem[i]]] <- as.data.frame(read_parquet_safely(path),
                                                stringsAsFactors = FALSE)
      }
    }
    return(result)
  }
  invisible(list(n_rows = as.integer(n_rows), years = years,
                 files = paste0(plan$stem, ".parquet")))
}

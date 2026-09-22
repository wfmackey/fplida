#' Financial years the ATO Payment Summary delivery covers
#'
#' `datasets.csv` gives PIT_PS a reference period of 2001-2002 to 2022-2023,
#' so the valid financial-year end years are 2002 through 2023. This is the
#' default argument of [generate_pit_ps()]; the gate that decides what gets
#' written is [.pit_ps_valid_years()], which reads the data item list, so a
#' registry that gains a year needs only this default changed to reach it.
#' @noRd
.PIT_PS_YEARS <- 2002L:2023L


.pit_ps_plan_cache <- new.env(parent = emptyenv())


#' Every PIT_PS output table the data item list declares
#'
#' Payment summaries are published one to three tables per product: a single
#' annual table up to 2018-19, and from 2019-20 several extracts of the same
#' year that differ only in how long after 30 June the file was cut. The four
#' products from 2015-16 to 2018-19 add a separate geography table. The split
#' is read from the bundled data item list rather than typed here, so the
#' emitted tables cannot drift from the registry.
#'
#' @return A data.frame with one row per output table: `product`, `table`,
#'   `stem` (the file name the package gives a product's table), `fy`,
#'   `months` (the extract window), `key_var` (the business identifier column
#'   the registry declares, `""` for a table that names no business), `geo`
#'   (1 for a geography-only table), and `variables`, a list column of the
#'   declared variable names in registry order.
#' @keywords internal
.pit_ps_structures <- function() {
  if (!is.null(.pit_ps_plan_cache$data)) return(.pit_ps_plan_cache$data)
  variables <- utils::read.csv(
    .dil_metadata_path("variables.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  variables <- variables[variables$Dataset == "PIT_PS", , drop = FALSE]
  if (!nrow(variables)) {
    stop("No PIT_PS structures in the bundled data item list.", call. = FALSE)
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

  # The financial year lives in the last four digits of the table name, after
  # any extract window: `ato_pay_sum_0910_16m` and `ps_geo2021_1516` both end
  # in the two-year pair, and the second pair is the year the FY ends in.
  base <- sub("_[0-9]+m$", "", structures$table)
  dated <- grepl("_[0-9]{4}$", base)
  fy <- rep(NA_integer_, length(base))
  fy[dated] <- 2000L + as.integer(sub("^.*_[0-9]{2}([0-9]{2})$", "\\1",
                                      base[dated]))

  # An extract window longer or shorter than the financial year is named in
  # the table, e.g. `ato_pay_sum_2223_6m`.
  months <- rep(12L, length(base))
  windowed <- grepl("_[0-9]+m$", structures$table)
  months[windowed] <- as.integer(
    sub("^.*_([0-9]+)m$", "\\1", structures$table[windowed])
  )

  # Which identifier a table keys its employers on. The delivery changed
  # hashing part-way through 2021-22, and the change is per table rather than
  # per financial year: `ato_pay_sum_2122_6m` still carries `ABN_HASH_TRUNC`
  # while `ato_pay_sum_2122_16m` in the same product carries `BN`. So the
  # registry decides, never a year threshold. A table that declares both, or
  # a third spelling, fails here rather than writing a column under the wrong
  # name.
  key_var <- vapply(structures$table, function(tb) {
    found <- intersect(c("BN", "ABN_HASH_TRUNC"), variable[table == tb])
    if (length(found) > 1L) {
      stop("PIT_PS table ", tb, " declares ", length(found),
           " business identifier columns; expected at most one of BN or ",
           "ABN_HASH_TRUNC.", call. = FALSE)
    }
    if (!length(found)) "" else found
  }, character(1))

  declared <- lapply(structures$table, function(tb) {
    unique(variable[table == tb])
  })

  plan <- data.frame(
    product = structures$product,
    table = structures$table,
    stem = vapply(seq_len(nrow(structures)), function(i) {
      .dil_structure_stem(structures$product[i], structures$table[i])
    }, character(1)),
    fy = fy,
    months = months,
    key_var = unname(key_var),
    geo = as.integer(grepl("^ps_geo", structures$table)),
    stringsAsFactors = FALSE
  )
  plan$variables <- declared
  plan <- plan[!is.na(plan$fy), , drop = FALSE]
  plan <- plan[order(plan$fy, plan$table), , drop = FALSE]
  rownames(plan) <- NULL

  registered <- .pit_ps_products()
  wrong <- plan$product != registered$product[match(plan$fy, registered$year)]
  if (any(wrong, na.rm = TRUE)) {
    stop("PIT_PS table and product disagree about the financial year: ",
         paste(plan$table[wrong], collapse = ", "), call. = FALSE)
  }
  .pit_ps_plan_cache$data <- plan
  plan
}


#' The output tables for a set of financial years
#'
#' @param years Integer vector of financial-year end years.
#' @return The rows of [.pit_ps_structures()] those years cover.
#' @keywords internal
.pit_ps_file_plan <- function(years) {
  plan <- .pit_ps_structures()
  plan <- plan[plan$fy %in% as.integer(years), , drop = FALSE]
  rownames(plan) <- NULL
  plan
}


#' Financial years the data item list gives PIT_PS a product for
#'
#' @return Integer vector of FY end years, ascending.
#' @keywords internal
.pit_ps_valid_years <- function() {
  sort(unique(.pit_ps_structures()$fy))
}


#' The payment-summary table that stands for a financial year
#'
#' Several tables can report one year and differ only in how long after 30
#' June the extract was cut. A consumer that wants the year's payment
#' summaries wants the most complete of them: the 12-month extract where there
#' is one, the 16-month re-extract otherwise, and never the partial 6-month
#' cut.
#'
#' @param year Integer. FY end year.
#' @return A one-row data.frame of the plan, or `NULL` for a year the
#'   delivery does not cover.
#' @keywords internal
.pit_ps_primary_table <- function(year) {
  plan <- .pit_ps_file_plan(as.integer(year))
  plan <- plan[plan$geo == 0L, , drop = FALSE]
  if (!nrow(plan)) return(NULL)
  rank <- ifelse(plan$months == 12L, 1L, ifelse(plan$months > 12L, 2L, 3L))
  plan[which.min(rank), , drop = FALSE]
}


#' Path of the payment-summary table that stands for a financial year
#'
#' @param ps_dir Character. The `ato-pit_ps` dataset directory.
#' @param year Integer. FY end year.
#' @return Character path, or `NA_character_` for an uncovered year.
#' @keywords internal
.pit_ps_primary_path <- function(ps_dir, year) {
  row <- .pit_ps_primary_table(year)
  if (is.null(row)) return(NA_character_)
  file.path(ps_dir, paste0(row$stem, ".parquet"))
}


#' Per-person address fields for the payment-summary geography columns
#'
#' The address comes from the shared spine resolution rather than a fresh
#' draw, so a person's mesh block, SA1 and SA2 here are the ones Core
#' Locations and the STP payroll tables give them, with the ATO's own share of
#' stale addresses. SA3 is the first five digits of the SA2 and SA4 the first
#' three, which is how the ASGS main structure nests.
#'
#' LGA is the one field that cannot be derived from the person's SA1: local
#' government areas are an ASGS non-ABS structure that does not nest in the
#' main structure, and no SA1-to-LGA correspondence is bundled. It is drawn
#' from the ABS LGA code frame within the state of the address the ATO holds
#' and keyed on the dwelling, so co-residents share a council, a person keeps
#' theirs across years, and the council and the `STE` on a row agree. It is
#' not guaranteed to be the council their SA1 sits in. The 2021 edition is
#' used throughout, matching the ASGS 2021 vintage of the rest of the spine
#' geography.
#'
#' @param spine data.frame. The filtered spine rows.
#' @param seed Integer. Random seed.
#' @return A list of per-person vectors aligned with `spine`.
#' @keywords internal
.pit_ps_geography <- function(spine, seed) {
  n <- nrow(spine)
  # A caller can pass a reduced frame. Without a state there is no address to
  # resolve, so the geography columns are written empty rather than the
  # address lookup failing on a column that is not there.
  if (!"state" %in% names(spine)) {
    return(list(sa1 = rep("", n), mb = rep("", n), lga = rep("", n),
                sa2 = rep(0L, n), sa3 = rep(0L, n), sa4 = rep(0L, n),
                ste = rep(0L, n),
                arid = as.character(.dil_address_key("PIT_PS", spine, seed))))
  }
  address <- .spine_address_lookup_rows(spine, agency = "ATO", seed = seed)

  sa1 <- as.character(address$sa1_code)
  mb <- as.character(address$mb_code)
  sa1[is.na(sa1)] <- ""
  mb[is.na(mb)] <- ""
  sa1 <- ifelse(nzchar(sa1), sprintf("%011.0f", as.numeric(sa1)), "")
  mb <- ifelse(nzchar(mb), sprintf("%011.0f", as.numeric(mb)), "")

  sa2 <- suppressWarnings(as.integer(address$sa2_code))
  sa2[is.na(sa2)] <- 0L
  sa3 <- ifelse(sa2 > 0L, as.integer(sa2 %/% 10000L), 0L)
  sa4 <- suppressWarnings(as.integer(address$sa4_code))
  sa4[is.na(sa4)] <- 0L
  ste <- suppressWarnings(as.integer(address$state))
  ste[is.na(ste)] <- 0L

  # The council is the one the ATO's address sits in, so LGA follows the
  # state on that address rather than the state the spine has moved the
  # person to. Without this a person the ATO has not caught up with carries a
  # council in one state and an `STE` in another on the same row.
  lga_rows <- spine
  lga_rows$state <- ifelse(ste %in% 1:8, ste, as.integer(spine$state))
  lga <- .dil_lga_value("LGA", lga_rows, seed,
                        list(start_year = 2020L, end_year = 2021L))
  if (is.null(lga)) lga <- rep("", n)
  lga <- as.character(lga)
  lga[is.na(lga)] <- ""

  list(
    sa1 = sa1, mb = mb, lga = lga,
    sa2 = sa2, sa3 = sa3, sa4 = sa4, ste = ste,
    arid = as.character(.dil_address_key("PIT_PS", spine, seed))
  )
}


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
#' @section What each product contains:
#' A product holds one to three tables. Each is written as its own file, named
#' `<product>--<table>`, which is the convention the package uses for every
#' product the data item list splits into tables. The schema changes by year,
#' from four variables in 2001-02 to thirty-six in 2022-23, and every table
#' carries exactly the variables the registry declares for it. From 2019-20
#' several tables report one year and differ only in how long after 30 June
#' the extract was cut; the six-month cut misses the payment summaries lodged
#' after it. The four products from 2015-16 to 2018-19 also carry a separate
#' geography table, one row per person with a payment summary that year.
#'
#' @section Which identifier names the employer:
#' The tables delivered up to 2021-22 name the employer in `ABN_HASH_TRUNC`
#' and the tables from 2021-22 on name it in `BN`. Which one a table carries
#' comes from the data item list, not from the financial year: 2021-22 is
#' mixed, with `ABN_HASH_TRUNC` on the six-month extract and `BN` on the
#' sixteen-month re-extract. The two are different hashings of the same ABN
#' and never share a value, so joining across the change needs the
#' `blade-key-abn-hash-trunc-to-bn-key` product in `abs-blade`.
#'
#' @section Dataset and variable information:
#' The [ABS administrative income sources](https://www.abs.gov.au/statistics/detailed-methodology-information/concepts-sources-methods/administrative-income-comparison-studies/2019-20-2021/administrative-data-sources)
#' website gives information about this dataset. Use `dataset_info("PIT_PS")`
#' for dataset information. Use `variable_info("PIT_PS")` for variables,
#' sources, value support, and topic tags.
#'
#' @section Occupation codes are not ANZSCO:
#' The occupation panel this function writes for [generate_pit_itr()] holds
#' ATO salary and wage occupation codes rather than ANZSCO. See
#' `?generate_pit_itr` for the mapping, and the ATO's
#' [published list](https://www.ato.gov.au/forms-and-instructions/salary-and-wage-occupation-codes).
#'
#' @param spine A data frame from [generate_spine()], or `NULL`. If the value is
#'   `NULL`, the function reads the spine from the run directory.
#'
#' @param seed An integer random seed.
#'
#' @param years A vector of financial-year end years. The delivery covers 2002
#'   to 2023; any other year is dropped and no file is written for it.
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
generate_pit_ps <- function(spine = NULL, seed = 42L, years = 2002:2023,
                            output_dir = NULL,
                            format = c("parquet", "csv"),
                            return_data = FALSE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  stopifnot(!is.na(seed))
  requested <- sort(unique(as.integer(years)))
  # Two gates, and the stricter one wins. gate_dataset_years() applies the
  # reference period the dataset declares; .pit_ps_valid_years() narrows to the
  # years the item list actually gives a product, so a gap inside the declared
  # span cannot slip through. The gate stays quiet because the message below
  # names the covered range and the years it dropped.
  valid <- .pit_ps_valid_years()
  years <- intersect(gate_dataset_years("PIT_PS", requested, quiet = TRUE), valid)
  if (format != "parquet") {
    stop("generate_pit_ps() supports parquet only.", call. = FALSE)
  }
  if (length(years) < length(requested)) {
    message("  PIT_PS covers financial years ending ",
            min(valid), " to ", max(valid), "; skipping ",
            paste(setdiff(requested, years), collapse = ", "))
  }
  if (!length(years)) {
    return(invisible(list(n_records = 0L, n_rows = integer(0),
                          years = integer(0), path = "ato-pit_ps")))
  }

  run_dir <- resolve_run_dir(output_dir)

  # Draw payment-summary employers from the BLADE business universe (built
  # earlier) so the employer identifier resolves to a BLADE business in both
  # eras: the tables keyed on `BN` publish it directly, and the tables keyed
  # on `ABN_HASH_TRUNC` publish the same hashing of it that
  # `blade-key-abn-hash-trunc-to-bn-key` carries.
  .set_business_pool_from_spine(run_dir)

  ps_cols <- c("spine_id", "aeuid_ato", "id", "birth_year", "month_of_birth",
               "baseline_employed", "baseline_income", "baseline_hours",
               "anzsco_major", "anzsco_code", "industry",
               "task_physical", "archetype",
               "disability_onset_year", "is_dc",
               "disability_severity", "disability_dose",
               "state", "sa2_code", "sa3_code", "sa4_code", "dwelling_id")
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

  plan <- .pit_ps_file_plan(years)
  years <- sort(unique(plan$fy))

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
  birth_month <- if (!is.null(spine$month_of_birth))
    as.integer(spine$month_of_birth) else rep(6L, n)
  birth_month[is.na(birth_month)] <- 6L

  geo <- .pit_ps_geography(spine, seed)

  res <- generate_pit_ps_full_to_parquet__(
    id                    = as.character(spine$id),
    aeuid_ato             = as.character(spine$aeuid_ato),
    birth_year            = as.integer(spine$birth_year),
    birth_month           = birth_month,
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
    geo_sa1               = geo$sa1,
    geo_mb                = geo$mb,
    geo_lga               = geo$lga,
    geo_arid              = geo$arid,
    geo_sa2               = geo$sa2,
    geo_sa3               = geo$sa3,
    geo_sa4               = geo$sa4,
    geo_ste               = geo$ste,
    years                 = as.integer(years),
    seed                  = seed,
    out_dir               = ds_dir,
    occ_out_dir           = occ_dir,
    tbl_stem              = as.character(plan$stem),
    tbl_table             = as.character(plan$table),
    tbl_year              = as.integer(plan$fy),
    tbl_months            = as.integer(plan$months),
    tbl_key_var           = as.character(plan$key_var),
    tbl_geo               = as.integer(plan$geo),
    tbl_variables         = as.character(unlist(plan$variables)),
    tbl_var_offsets       = as.integer(c(0L, cumsum(lengths(plan$variables))))
  )

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  n_rows <- as.integer(res$n_rows)
  names(n_rows) <- as.character(res$year)
  invisible(list(n_records = sum(n_rows), n_rows = n_rows,
                 years = years, path = "ato-pit_ps"))
}

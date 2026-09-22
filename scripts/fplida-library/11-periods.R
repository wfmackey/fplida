# Reporting fields exclude birth, arrival and historical spell attributes.
reporting_columns <- function(asset, family, columns) {
  candidates <- switch(family,
    blade = "tsid",
    linking = if (grepl("blade-key", asset)) "tsid" else character(),
    core = "pp_period",
    visa = c("va_lodged_dt", "tr_visa_grant_dt", "nm_lodged_dt", "nm_apprvl_dt"),
    domino = "period_start_date",
    mbs = c("dos", "date_of_service"),
    pbs = c("spply_dt", "supply_date"),
    pit_ps = c("fin_year", "perd_end_dt"),
    pit_itr = c("income_year", "incm_yr", "financial_year"),
    busown = "fin_year",
    stp = if ("pmt_dt" %in% columns) "pmt_dt" else "pyrl_fncl_yr",
    lfs = if ("survyear" %in% columns) "survyear" else "absmid",
    he = "year",
    tva = c("date_program_completed", "activity_start_date"),
    deaths = "reference_year",
    ndis = c("financialyear", "sfof_dt", "fy_claim", "pymtrqstcrtddt"),
    character()
  )
  found <- intersect(candidates, columns)
  if (!length(found)) {
    found <- intersect(c("reference_year", "reporting_year", "survey_year",
                         "collection_year", "income_year", "financial_year", "year"),
                       columns)
  }
  found
}

period_year_sql <- function(column, value_sql = sql_identifier(column)) {
  field <- paste0("trim(CAST(", value_sql, " AS VARCHAR))")
  if (column == "tsid") {
    return(paste0("CASE WHEN try_cast(", field, " AS INTEGER) BETWEEN 0 AND 69 ",
                  "THEN 2000 + try_cast(", field, " AS INTEGER) ",
                  "WHEN try_cast(", field, " AS INTEGER) BETWEEN 70 AND 99 ",
                  "THEN 1900 + try_cast(", field, " AS INTEGER) END"))
  }
  fiscal <- column %in% c("fin_year", "fin_yr", "financial_year", "financialyear",
                          "fy_claim", "income_year", "incm_yr", "pyrl_fncl_yr")
  # Only fiscal fields interpret a year pair as a financial ending year.
  financial <- if (fiscal) paste0(
    "WHEN regexp_full_match(", field, ", '(19|20)[0-9]{2}[-/][0-9]{2}') ",
    "THEN try_cast(substr(", field, ",1,4) AS INTEGER) + 1 ",
    "WHEN regexp_full_match(", field, ", '(19|20)[0-9]{2}[-/](19|20)[0-9]{2}') ",
    "THEN try_cast(right(", field, ",4) AS INTEGER) "
  ) else ""
  paste0("CASE ", financial,
         "WHEN regexp_full_match(", field, ", '(19|20)[0-9]{2}[-/](0[1-9]|1[0-2])') ",
         "THEN try_cast(substr(", field, ",1,4) AS INTEGER) ",
         "WHEN regexp_full_match(", field, ", '(19|20)[0-9]{2}([0-9]{2})?') ",
         "THEN try_cast(substr(", field, ",1,4) AS INTEGER) ELSE year(coalesce(",
         "try_cast(", field, " AS DATE), try_strptime(", field,
         ", ['%d%b%y','%d%b%Y','%d/%m/%Y','%d-%b-%Y','%d %b %Y','%Y%m%d']))) END")
}

reporting_period_counts <- function(con, data, fields) {
  query <- as.character(dbplyr::sql_render(data))
  raw <- paste0("trim(CAST(", vapply(fields, sql_identifier, ""), " AS VARCHAR))")
  expressions <- vapply(fields, function(field) {
    period_year_sql(field, value_sql = sql_identifier("raw_value"))
  }, "")
  branches <- paste0("WHEN field_name = ", sql_string(fields), " THEN ", expressions)
  # Dates repeat often. Count each raw field value before parsing, then retain
  # its frequency so missing and unparsed counts still refer to all records.
  sql <- paste0(
    "WITH field_values AS (SELECT unnest(", sql_files(fields), ") AS field_name, ",
    "unnest([", paste(raw, collapse = ","), "]) AS raw_value FROM (", query, ") source), ",
    "grouped AS (SELECT field_name, raw_value, count(*) AS frequency ",
    "FROM field_values GROUP BY field_name, raw_value), ",
    "parsed AS (SELECT raw_value, frequency, CASE ", paste(branches, collapse = " "),
    " END AS reporting_year FROM grouped) ",
    "SELECT reporting_year, sum(frequency) AS n, ",
    "sum(CASE WHEN reporting_year IS NULL AND raw_value IS NOT NULL ",
    "AND raw_value NOT IN ('', 'NA', '-1', '-2', '-3', '-9') ",
    "THEN frequency ELSE 0 END) AS unparsed FROM parsed ",
    "GROUP BY reporting_year ORDER BY reporting_year"
  )
  DBI::dbGetQuery(con, sql)
}

audit_reporting_periods <- function(root, inventory, temp_dir) {
  con <- open_audit_connection(temp_dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  groups <- split(seq_len(nrow(inventory)), inventory$asset)
  checked_assets <- 0L
  purrr::imap_dfr(groups, function(indices, asset) {
    data <- read_asset(con, file.path(root, inventory$path[indices]))
    fields <- reporting_columns(asset, inventory$family[indices[1L]], colnames(data))
    years <- integer()
    invalid <- 0L
    unparsed <- 0L
    if (length(fields)) {
      values <- reporting_period_counts(con, data, fields)
      years <- as.integer(values$reporting_year[!is.na(values$reporting_year)])
      invalid <- sum(values$n[is.na(values$reporting_year)])
      unparsed <- sum(values$unparsed)
    }
    checked_assets <<- checked_assets + 1L
    if (checked_assets %% 20L == 0L) {
      cat(sprintf("Reporting years: checked %d of %d assets.\n",
                  checked_assets, length(groups)))
      flush.console()
    }
    filename <- filename_period(asset)
    label <- if (length(years)) paste(years, collapse = ", ") else if (length(fields)) {
      "No observed reporting years"
    } else if (!is.na(filename)) {
      paste0(filename, " (file label)")
    } else "No reporting-year field"
    tibble::tibble(asset = asset, reporting_fields = paste(fields, collapse = ", "),
                   observed_year_min = if (length(years)) min(years) else NA_integer_,
                   observed_year_max = if (length(years)) max(years) else NA_integer_,
                   observed_years = paste(years, collapse = ", "),
                   year_values_missing_or_unparsed = invalid,
                   nonmissing_unparsed_years = as.numeric(unparsed),
                   reporting_period = label)
  })
}

build_product_coverage <- function(result, inventory) {
  if (is.null(result)) return(list(passed = FALSE, products = list(),
                                  reason = "Build result unavailable"))
  expected <- result$products
  metadata <- fplida:::.dil_structure_inventory()$structures
  dataset_map <- fplida:::.dil_build_target_datasets
  schema_stems <- paste0(metadata[["Product Name"]], "--", metadata[["Table Name"]])
  purrr::map(expected, function(product) {
    family_assets <- inventory$asset[inventory$family == product]
    dataset <- unname(dataset_map[product])
    prefixes <- unique(metadata[["Product Name"]][metadata$Dataset %in% dataset])
    matching <- inventory$asset %in% family_assets
    for (prefix in prefixes) {
      matching <- matching | inventory$asset == prefix |
        startsWith(inventory$asset, paste0(prefix, "--"))
    }
    if (product == "spine") matching <- inventory$family == "linking"
    workers <- lapply(result$worker_results, function(worker) worker$product_results[[product]])
    central <- product %in% c("spine", "core", "blade", "busown", "lfs")
    completed <- central || (length(workers) > 0L && all(vapply(workers, function(x) {
      !is.null(x) && is.null(x$metadata$error)
    }, logical(1))))
    rows <- sum(inventory$rows[matching])
    status <- if (any(matching)) {
      if (rows > 0) "generated" else "empty tables"
    } else if (completed && all(vapply(workers, function(x) is.null(x$metadata), logical(1)))) {
      "no eligible output"
    } else "missing output"
    list(product = product, completed = completed, status = status,
         files = sum(matching), rows = rows,
         schema_files = sum(matching & inventory$asset %in% schema_stems))
  }) |> (function(products) list(
    passed = length(products) > 0L && all(vapply(products, function(x) {
      x$completed && x$status != "missing output"
    }, logical(1))), products = products
  ))()
}

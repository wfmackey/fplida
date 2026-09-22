#!/usr/bin/env Rscript
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
library_script_root <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
source(file.path(library_script_root, "00-setup.R"))

check_reporting_period_fixture <- function() {
  temporary <- tempfile("reporting-period-fixture-")
  con <- open_audit_connection(temporary)
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(temporary, recursive = TRUE)
  }, add = TRUE)
  frame <- data.frame(
    dos = c("16Jul24", "16Jul24", "2009-01-01", "junk", "junk", NA, "", "NA", "-9"),
    fin_year = c("1999-00", "2009-10", "2020", NA, "2024/2025", "-1", "junk", "2017", "2017"),
    pp_period = c("2024-01", "2024-12", "2021-01", NA, "2023/09", "", "-1", "wrong", "wrong"),
    birth_date = rep("1930-01-01", 9L), start_date = rep("2006-01-01", 9L)
  )
  DBI::dbWriteTable(con, "fixture", frame)
  data <- dplyr::tbl(con, "fixture")
  summary <- function(values) list(
    years = as.integer(values$reporting_year[!is.na(values$reporting_year)]),
    missing = sum(values$n[is.na(values$reporting_year)]),
    unparsed = sum(values$unparsed)
  )
  previous <- function(data, fields) {
    query <- as.character(dbplyr::sql_render(data))
    expressions <- vapply(fields, period_year_sql, "")
    values <- DBI::dbGetQuery(con, paste0(
      "SELECT reporting_year, count(*) AS n FROM (SELECT unnest([",
      paste(expressions, collapse = ","), "]) AS reporting_year FROM (", query,
      ") source) p GROUP BY reporting_year ORDER BY reporting_year"
    ))
    raw <- paste0("trim(CAST(", vapply(fields, sql_identifier, ""), " AS VARCHAR))")
    conditions <- paste0("CASE WHEN ", raw, " IS NOT NULL AND ", raw,
      " NOT IN ('', 'NA', '-1', '-2', '-3', '-9') AND (", expressions,
      ") IS NULL THEN 1 ELSE 0 END")
    unparsed <- DBI::dbGetQuery(con, paste0("SELECT sum(",
      paste(conditions, collapse = " + "), ") AS n FROM (", query, ") source"))$n
    list(years = as.integer(values$reporting_year[!is.na(values$reporting_year)]),
         missing = sum(values$n[is.na(values$reporting_year)]),
         unparsed = if (is.na(unparsed)) 0 else as.numeric(unparsed))
  }
  for (fields in list("dos", c("dos", "fin_year", "pp_period"))) {
    actual <- summary(reporting_period_counts(con, data, fields))
    stopifnot(identical(actual, previous(data, fields)))
    empty <- data |> dplyr::filter(FALSE)
    stopifnot(identical(summary(reporting_period_counts(con, empty, fields)),
                        previous(empty, fields)))
  }
  health <- summary(reporting_period_counts(con, data, "dos"))
  stopifnot(identical(health, list(years = c(2009L, 2024L), missing = 6, unparsed = 2)))
  stopifnot(identical(reporting_columns("core_residence", "core", names(frame)), "pp_period"))
  stopifnot(identical(reporting_columns("mbs", "mbs", names(frame)), "dos"))
  stopifnot(!length(reporting_columns("core_location", "core", c("birth_date", "start_date"))))
  cat("Repeated values, weighted parsing counts, multiple fields, empty data and historical exclusions: PASS\n")
}

check_reporting_period_fixture()

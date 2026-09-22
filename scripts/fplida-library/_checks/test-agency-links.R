#!/usr/bin/env Rscript
# Run directly to compare the aggregate with the previous three-query check.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
library_script_root <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
source(file.path(library_script_root, "00-setup.R"))

check_agency_link_fixture <- function() {
  temporary <- tempfile("agency-links-fixture-")
  con <- open_audit_connection(temporary)
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(temporary, recursive = TRUE)
  }, add = TRUE)
  DBI::dbWriteTable(con, "records", data.frame(
    synthetic_aeuid = c("A", "A", "B", "C", "D", NA, "")
  ))
  DBI::dbWriteTable(con, "known", data.frame(
    synthetic_aeuid = c("A", "A", "B", "C", NA, "")
  ))
  DBI::dbWriteTable(con, "linked", data.frame(
    synthetic_aeuid = c("A", "A", "C")
  ))
  known <- dplyr::tbl(con, "known")
  linked <- dplyr::tbl(con, "linked")
  records <- dplyr::tbl(con, "records")
  previous <- function(present) {
    counts <- present |>
      dplyr::summarise(records = dplyr::n(), ids = dplyr::n_distinct(synthetic_aeuid)) |>
      dplyr::collect()
    unmatched <- present |>
      dplyr::anti_join(known, by = "synthetic_aeuid") |>
      dplyr::summarise(records = dplyr::n(), ids = dplyr::n_distinct(synthetic_aeuid)) |>
      dplyr::collect()
    matched <- present |>
      dplyr::semi_join(linked, by = "synthetic_aeuid") |>
      dplyr::summarise(records = dplyr::n()) |> dplyr::collect()
    c(records = counts$records, ids = counts$ids,
      unmatched = unmatched$records, unmatched_ids = unmatched$ids,
      linked = matched$records)
  }
  scenarios <- list(all = records, empty = records |> dplyr::filter(FALSE),
                    limited = utils::head(records, 3L),
                    missing = records |> dplyr::filter(is.na(synthetic_aeuid) |
                                                        synthetic_aeuid == ""))
  expected <- list(all = c(records = 5, ids = 4, unmatched = 1, unmatched_ids = 1, linked = 3),
                    empty = c(records = 0, ids = 0, unmatched = 0, unmatched_ids = 0, linked = 0),
                    limited = c(records = 3, ids = 2, unmatched = 0, unmatched_ids = 0, linked = 2),
                    missing = c(records = 0, ids = 0, unmatched = 0, unmatched_ids = 0, linked = 0))
  purrr::iwalk(scenarios, function(data, name) {
    present <- data |> dplyr::filter(!is.na(synthetic_aeuid), synthetic_aeuid != "")
    actual <- unlist(summarise_agency_links(present, known, linked), use.names = TRUE)
    stopifnot(identical(actual, previous(present)), identical(actual, expected[[name]]))
    cat(name, ": PASS\n", sep = "")
  })
}

check_agency_link_fixture()

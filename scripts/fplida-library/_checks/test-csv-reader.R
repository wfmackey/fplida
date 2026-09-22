#!/usr/bin/env Rscript
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
library_script_root <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
source(file.path(library_script_root, "00-setup.R"))

check_csv_reader_fixture <- function() {
  temporary <- tempfile("csv-reader-fixture-")
  dir.create(temporary)
  con <- open_audit_connection(file.path(temporary, "scratch"))
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(temporary, recursive = TRUE)
  }, add = TRUE)
  first <- file.path(temporary, "quoted.csv")
  second <- file.path(temporary, "reordered.csv")
  empty <- file.path(temporary, "empty.csv")
  frame <- data.frame(
    ID = c("00123", "00000", "99999999999999999999", "04000", "NA", ""),
    Text = c('comma, and "quote"', "line one\nline two", "café / 中文", "", NA, "  padded  "),
    Number = c("0001", "01.50", "-9", "NA", "", "6")
  )
  reordered <- data.frame(
    Number = c("007", "-1"), ID = c("00007", "00008"),
    Text = c("carriage\rreturn", "both\r\nlines"), Extra = c("new", "column")
  )
  utils::write.csv(frame, first, row.names = FALSE, na = "NA")
  utils::write.csv(reordered, second, row.names = FALSE, na = "NA")
  utils::write.csv(frame[0L, ], empty, row.names = FALSE, na = "NA")
  for (files in list(first, c(first, second), empty)) {
    previous <- DBI::dbGetQuery(con, paste0(
      "SELECT * FROM read_csv(", sql_files(files), ", header = true, ",
      "all_varchar = true, nullstr = ['', 'NA'], union_by_name = true, ",
      "sample_size = 100000) ORDER BY ID NULLS LAST"
    ))
    names(previous) <- tolower(names(previous))
    actual <- read_asset(con, files) |> dplyr::arrange(id) |>
      dplyr::collect() |> as.data.frame()
    stopifnot(identical(actual, previous), all(vapply(actual, is.character, logical(1))))
  }
  values <- read_asset(con, first) |> dplyr::collect()
  stopifnot("00123" %in% values$id, "99999999999999999999" %in% values$id,
            "line one\nline two" %in% values$text,
            'comma, and "quote"' %in% values$text)
  cat("Quoted and multiline fields, Unicode, leading zeros, nulls, union schemas and header-only CSV: PASS\n")
}

check_csv_reader_fixture()

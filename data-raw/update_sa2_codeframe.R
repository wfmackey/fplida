#!/usr/bin/env Rscript

# Refresh the ASGS 2021 Statistical Area Level 2 code-name codeframe from
# the official ABS ArcGIS service.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.", call. = FALSE)
}

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]),
                         mustWork = TRUE))
  }
  normalizePath(
    file.path("data-raw", "update_sa2_codeframe.R"),
    mustWork = TRUE
  )
}

repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)
service_url <- paste0(
  "https://geo.abs.gov.au/arcgis/rest/services/",
  "ASGS2021/SA2/MapServer/0"
)
query_url <- paste0(service_url, "/query")
page_size <- 2000L

count_response <- jsonlite::fromJSON(paste0(
  query_url,
  "?where=1%3D1&returnCountOnly=true&f=json"
))
if (!is.null(count_response$error)) {
  stop("ABS SA2 service failed: ", count_response$error$message,
       call. = FALSE)
}
expected_rows <- as.integer(count_response$count)

pages <- list()
offset <- 0L
while (offset < expected_rows) {
  url <- paste0(
    query_url,
    "?where=1%3D1",
    "&outFields=sa2_code_2021,sa2_name_2021,state_code_2021",
    "&returnGeometry=false",
    "&orderByFields=sa2_code_2021",
    "&resultOffset=", offset,
    "&resultRecordCount=", page_size,
    "&f=json"
  )
  response <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
  if (!is.null(response$error)) {
    stop("ABS SA2 service failed at offset ", offset, ": ",
         response$error$message, call. = FALSE)
  }
  attributes <- response$features$attributes
  if (!is.data.frame(attributes) || !nrow(attributes)) {
    stop("ABS SA2 service returned no rows at offset ", offset,
         call. = FALSE)
  }
  pages[[length(pages) + 1L]] <- attributes
  offset <- offset + nrow(attributes)
}

raw <- do.call(rbind, pages)
names(raw) <- tolower(names(raw))
sa2 <- data.frame(
  year = 2021L,
  code = as.character(raw$sa2_code_2021),
  name = as.character(raw$sa2_name_2021),
  state = as.character(raw$state_code_2021),
  source_url = sub("/0$", "", service_url),
  stringsAsFactors = FALSE
)
sa2 <- sa2[order(sa2$code), , drop = FALSE]
rownames(sa2) <- NULL

stopifnot(
  nrow(sa2) == expected_rows,
  expected_rows == 2473L,
  !anyDuplicated(sa2$code),
  all(grepl("^(?:[1-9][0-9]{8}|Z{9})$", sa2$code)),
  all(nzchar(sa2$name)),
  all(sa2$state %in% c(as.character(1:9), "Z"))
)

output <- file.path(
  repo_root, "inst", "extdata", "codeframes", "sa2_2021.tsv"
)
utils::write.table(
  sa2,
  output,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)
cat("Wrote ", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("Rows: ", nrow(sa2), "\n", sep = "")

#!/usr/bin/env Rscript

# Refresh Indigenous Region codeframes from the official ABS ArcGIS services.

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
    file.path("data-raw", "update_indigenous_region_codeframe.R"),
    mustWork = TRUE
  )
}

repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)
years <- c(2011L, 2016L, 2021L)

read_ireg_year <- function(year) {
  code_name <- if (year == 2011L) "ireg_code" else paste0("ireg_code_", year)
  label_name <- if (year == 2011L) "ireg_name" else paste0("ireg_name_", year)
  fields <- paste(c(code_name, label_name), collapse = ",")
  url <- paste0(
    "https://geo.abs.gov.au/arcgis/rest/services/ASGS", year,
    "/IREG/MapServer/0/query?where=1%3D1",
    "&outFields=", utils::URLencode(fields, reserved = TRUE),
    "&returnGeometry=false&f=json"
  )
  response <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
  if (!is.null(response$error)) {
    stop("ABS Indigenous Region service failed for ", year, ": ",
         response$error$message, call. = FALSE)
  }
  attributes <- response$features$attributes
  names(attributes) <- tolower(names(attributes))
  data.frame(
    year = year,
    code = as.character(attributes[[code_name]]),
    name = as.character(attributes[[label_name]]),
    state = ifelse(
      as.character(attributes[[code_name]]) == "ZZZ",
      "9",
      substr(as.character(attributes[[code_name]]), 1L, 1L)
    ),
    source_url = paste0(
      "https://geo.abs.gov.au/arcgis/rest/services/ASGS", year,
      "/IREG/MapServer"
    ),
    stringsAsFactors = FALSE
  )
}

ireg <- do.call(rbind, lapply(years, read_ireg_year))
ireg <- ireg[order(ireg$year, ireg$code), , drop = FALSE]
rownames(ireg) <- NULL
stopifnot(
  all(table(ireg$year) > 30L),
  !anyDuplicated(paste(ireg$year, ireg$code, sep = "\r")),
  all(grepl("^(?:[1-9][0-9]{2}|ZZZ)$", ireg$code)),
  all(nzchar(ireg$name))
)

output <- file.path(repo_root, "inst", "extdata", "codeframes", "ireg.tsv")
utils::write.table(
  ireg,
  output,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)
cat("Wrote ", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("Rows: ", nrow(ireg), "\n", sep = "")

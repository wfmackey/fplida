#!/usr/bin/env Rscript

# Refresh the Local Government Area codeframe from the ABS ArcGIS services.
# The services are the official ASGS lists for each reference year.

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
  normalizePath(file.path("data-raw", "update_lga_codeframe.R"),
                mustWork = TRUE)
}

repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)
years <- c(2011L, 2016L, 2021L, 2022L, 2023L, 2024L)

read_lga_year <- function(year) {
  code_name <- if (year == 2011L) "lga_code" else paste0("lga_code_", year)
  label_name <- if (year == 2011L) "lga_name" else paste0("lga_name_", year)
  fields <- paste(c(code_name, label_name), collapse = ",")
  url <- paste0(
    "https://geo.abs.gov.au/arcgis/rest/services/ASGS", year,
    "/LGA/MapServer/0/query?where=1%3D1",
    "&outFields=", utils::URLencode(fields, reserved = TRUE),
    "&returnGeometry=false&f=json"
  )
  response <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
  if (!is.null(response$error)) {
    stop("ABS LGA service failed for ", year, ": ",
         response$error$message, call. = FALSE)
  }
  attributes <- response$features$attributes
  names(attributes) <- tolower(names(attributes))
  required <- c(code_name, label_name)
  missing <- setdiff(required, names(attributes))
  if (length(missing)) {
    stop("ABS LGA response is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  data.frame(
    year = year,
    code = as.character(attributes[[code_name]]),
    name = as.character(attributes[[label_name]]),
    state = ifelse(
      as.character(attributes[[code_name]]) == "ZZZZZ",
      "9",
      substr(as.character(attributes[[code_name]]), 1L, 1L)
    ),
    source_url = paste0(
      "https://geo.abs.gov.au/arcgis/rest/services/ASGS", year,
      "/LGA/MapServer"
    ),
    stringsAsFactors = FALSE
  )
}

lga <- do.call(rbind, lapply(years, read_lga_year))
lga <- lga[order(lga$year, lga$code), , drop = FALSE]
rownames(lga) <- NULL

stopifnot(
  all(table(lga$year) > 500L),
  !anyDuplicated(paste(lga$year, lga$code, sep = "\r")),
  all(grepl("^(?:[1-9][0-9]{4}|ZZZZZ)$", lga$code)),
  all(nzchar(lga$name)),
  all(grepl("^[1-9]$", lga$state))
)

output <- file.path(repo_root, "inst", "extdata", "codeframes", "lga.tsv")
utils::write.table(
  lga,
  output,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)
cat("Wrote ", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("Rows: ", nrow(lga), "\n", sep = "")

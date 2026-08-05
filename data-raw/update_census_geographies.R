#!/usr/bin/env Rscript

# Refresh the geography code lists used by Census detailed microdata. The
# values come from the official vintage-specific ABS ASGS ArcGIS services.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.", call. = FALSE)
}

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(
      sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE
    ))
  }
  normalizePath(
    file.path("data-raw", "update_census_geographies.R"),
    mustWork = TRUE
  )
}

repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."), winslash = "/", mustWork = TRUE
)

services <- rbind(
  expand.grid(
    year = c(2011L, 2016L),
    layer = c("SA1", "SA2", "SA3", "SA4", "RA", "NRMR"),
    stringsAsFactors = FALSE
  ),
  data.frame(
    year = c(rep(2021L, 17L), 2022L),
    layer = c(
      "ADD", "AUS", "CED", "DZN", "GCCSA", "IARE", "ILOC", "POA",
      "RA", "SAL", "SED", "SOS", "SOSR", "SUA", "TR", "UCL", "SA3",
      "SED"
    ),
    stringsAsFactors = FALSE
  )
)

fetch_service <- function(year, layer) {
  service_url <- paste0(
    "https://geo.abs.gov.au/arcgis/rest/services/ASGS", year, "/",
    layer, "/MapServer/0"
  )
  metadata <- jsonlite::fromJSON(paste0(service_url, "?f=pjson"))
  field_names <- metadata$fields$name
  code_pattern <- if (year == 2011L && layer == "SA1") {
    "^sa1_7digit$"
  } else if (year == 2011L && layer == "SA2") {
    "^sa2_main$"
  } else if (year == 2011L) {
    paste0("^", tolower(layer), "_code$")
  } else if (year == 2016L && layer == "SA1") {
    "^sa1_7digitcode_2016$"
  } else if (year == 2016L && layer == "SA2") {
    "^sa2_maincode_2016$"
  } else {
    paste0("^", tolower(layer), "_code_", year, "$")
  }
  code_field <- field_names[grepl(
    code_pattern, field_names, ignore.case = TRUE
  )]
  name_field <- field_names[grepl(
    paste0("^", tolower(layer), "_name_", year, "$"),
    field_names,
    ignore.case = TRUE
  )]
  if (!length(code_field)) stop("Could not identify a code field for ", service_url)
  state_field <- field_names[grepl(
    "^state_code(?:_20[0-9]{2})?$", field_names, ignore.case = TRUE
  )]
  fields <- code_field[[1L]]
  if (length(name_field)) fields <- c(fields, name_field[[1L]])
  if (length(state_field)) fields <- c(fields, state_field[[1L]])

  page_size <- as.integer(metadata$maxRecordCount %||% 2000L)
  offset <- 0L
  pages <- list()
  page_n <- 0L
  repeat {
    query <- paste0(
      service_url,
      "/query?where=1%3D1&outFields=",
      utils::URLencode(paste(fields, collapse = ","), reserved = TRUE),
      "&returnGeometry=false&resultOffset=", offset,
      "&resultRecordCount=", page_size,
      "&orderByFields=", utils::URLencode(code_field[[1L]], reserved = TRUE),
      "&f=json"
    )
    response <- jsonlite::fromJSON(query, simplifyDataFrame = TRUE)
    if (!is.null(response$error)) {
      stop(service_url, ": ", response$error$message, call. = FALSE)
    }
    attributes <- response$features$attributes
    if (is.null(attributes) || !nrow(attributes)) break
    page_n <- page_n + 1L
    pages[[page_n]] <- attributes
    if (nrow(attributes) < page_size) break
    offset <- offset + nrow(attributes)
  }
  attributes <- do.call(rbind, pages)
  names(attributes) <- tolower(names(attributes))
  code <- as.character(attributes[[tolower(code_field[[1L]])]])
  name <- if (length(name_field)) {
    as.character(attributes[[tolower(name_field[[1L]])]])
  } else {
    code
  }
  state <- if (length(state_field)) {
    as.character(attributes[[tolower(state_field[[1L]])]])
  } else {
    rep("", length(code))
  }
  if (layer == "POA") {
    postcode <- suppressWarnings(as.integer(code))
    state <- ifelse(
      postcode %in% 200:299 | postcode %in% 2600:2618 |
        postcode %in% 2900:2920,
      "8",
      ifelse(
        postcode %in% 800:999, "7",
        ifelse(
          postcode %in% 1000:2599 | postcode %in% 2619:2899 |
            postcode %in% 2921:2999,
          "1",
          ifelse(
            postcode %in% 3000:3999 | postcode %in% 8000:8999, "2",
            ifelse(
              postcode %in% 4000:4999 | postcode %in% 9000:9999, "3",
              ifelse(
                postcode %in% 5000:5999, "4",
                ifelse(
                  postcode %in% 6000:6999, "5",
                  ifelse(postcode %in% 7000:7999, "6", "")
                )
              )
            )
          )
        )
      )
    )
  }
  data.frame(
    year = year,
    layer = layer,
    code = code,
    name = name,
    state = state,
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

values <- do.call(rbind, lapply(seq_len(nrow(services)), function(i) {
  fetch_service(services$year[[i]], services$layer[[i]])
}))
values <- values[
  nzchar(values$code) & nzchar(values$name) &
    values$state %in% c("", as.character(1:9)) &
    !grepl(
      "No usual address|Migratory|Offshore|Shipping|Outside Australia",
      values$name,
      ignore.case = TRUE
    ),
  ,
  drop = FALSE
]
values <- values[order(values$year, values$layer, values$code), , drop = FALSE]
rownames(values) <- NULL

sources <- services
sources$source_url <- paste0(
  "https://geo.abs.gov.au/arcgis/rest/services/ASGS", sources$year, "/",
  sources$layer, "/MapServer"
)

stopifnot(
  nrow(values) > 50000L,
  !anyDuplicated(paste(values$year, values$layer, values$code, sep = "\r")),
  all(values$state %in% c("", as.character(1:9)))
)

output_dir <- file.path(repo_root, "inst", "extdata", "codeframes")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.table(
  values,
  file.path(output_dir, "census-geography-values.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)
utils::write.csv(
  sources,
  file.path(output_dir, "census-geography-sources.csv"),
  row.names = FALSE,
  na = ""
)
cat("Wrote ", nrow(values), " official Census geography values.\n", sep = "")

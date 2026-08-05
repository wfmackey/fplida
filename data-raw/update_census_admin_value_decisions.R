#!/usr/bin/env Rscript

# Build one reviewed decision row for every Census dataset-variable pair in
# the original administrative value-gap register.

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
  }
  normalizePath(
    file.path("data-raw", "update_census_admin_value_decisions.R"),
    mustWork = TRUE
  )
}

repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)

register <- utils::read.csv(
  file.path(
    repo_root, "inst", "internal-docs",
    "admin-value-remediation-register.csv"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
register <- register[register$dataset == "CENSUS", , drop = FALSE]

mapping <- utils::read.csv(
  file.path(
    repo_root, "inst", "extdata", "codeframes",
    "census-variable-codeframes.csv"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

collapse_unique <- function(value) {
  value <- unique(trimws(as.character(value)))
  value <- value[!is.na(value) & nzchar(value)]
  paste(value, collapse = " | ")
}

official_workbooks <- paste(
  "ABS 2011 Expanded CSF data-item list",
  "ABS 2016 detailed-microdata data-item list and official DataLab test file",
  "ABS 2021 detailed-microdata data-item list and Census Dictionary",
  sep = "; "
)

is_geography <- function(variable) {
  grepl(
    paste0(
      "^(?:ADDIV|AUST|CED|GCCSA|IARE|ILOC|IREG|LGA|MB|NRMR|PHN|",
      "POA|POW|PUR|RA[DINP]|SA[1-4]|SAL|SED|SOS|SUA|TR[DP]|UCL|",
      "IMP(?:SA2|STE))"
    ),
    variable,
    perl = TRUE
  )
}

is_coherent_rule <- function(variable) {
  grepl(
    paste0(
      "^(?:ADCP|C3SP|CO|EETP|EMFP|HLTHP|HOLHP|HOSD|IF|INDP|",
      "LFHRP|LFFP|MTW|OCC|OCSK|UEFP|WTNSQP|YARRP|YR12C)"
    ),
    variable,
    perl = TRUE
  )
}

evidence_for <- function(variable) {
  if (variable %in% c("EMPCD", "EMPCP")) {
    return(paste(
      paste0(
        "https://www.abs.gov.au/statistics/microdata-tablebuilder/",
        "available-microdata-tablebuilder/census-population-and-housing/",
        "Data%20item%20list%20-%202021%20Census%20detailed%20microdata_",
        "18052023.xlsx"
      ),
      paste0(
        "https://www.abs.gov.au/statistics/people/aboriginal-and-torres-",
        "strait-islander-peoples/census-population-and-housing-counts-",
        "aboriginal-and-torres-strait-islander-australians/latest-release"
      ),
      sep = " | "
    ))
  }
  source <- collapse_unique(mapping$source_url[
    toupper(mapping$variable) == toupper(variable) & mapping$supported
  ])
  if (is_geography(variable)) {
    geography_source <- paste(
      "Official ABS ASGS ArcGIS services recorded in",
      "inst/extdata/codeframes/census-geography-sources.csv, lga.tsv,",
      "ireg.tsv, and phn.tsv"
    )
    if (nzchar(source)) paste(source, geography_source, sep = " | ") else geography_source
  } else if (nzchar(source)) {
    source
  } else {
    official_workbooks
  }
}

decision <- lapply(seq_len(nrow(register)), function(i) {
  variable <- register$variable[[i]]
  typed_missing <- variable %in% c("EMPCD", "EMPCP")
  geography <- is_geography(variable)
  coherent <- is_coherent_rule(variable)

  status <- if (typed_missing) "typed_missing" else "populated"
  determination <- if (typed_missing) {
    "typed_missing_no_authoritative_code_map"
  } else if (geography) {
    "populated_official_geography"
  } else if (coherent) {
    "populated_coherent_official_domain"
  } else {
    "populated_official_codeframe"
  }

  rule <- if (typed_missing) {
    paste(
      "Retain a typed character missing value until an authoritative mapping",
      "between the 10 Empowered Communities regions and the four-character",
      "EMPC microdata codes is published or supplied."
    )
  } else if (variable == "HOSD") {
    paste(
      "Derive bedroom need or surplus from synthetic household composition",
      "and bedroom capacity; encode the result with official HOSD codes 01-09."
    )
  } else if (variable == "INDP") {
    paste(
      "Choose an official ANZSIC class within the spine industry division and",
      "translate it to the year-specific 2011, 2016, or 2021 Census encoding;",
      "use the official structural not-applicable code when not employed."
    )
  } else if (variable %in% c("PUR1P", "PUR5P")) {
    paste(
      "Assign a state-consistent official SA2 or an official overseas, not-stated,",
      "or age-scope supplementary code; use arrival year and age for scope."
    )
  } else if (geography) {
    paste(
      "Assign an official geography code for the field's reference year,",
      "consistent with spine state where the classification permits; apply",
      "age and employment structural codes to conditional fields."
    )
  } else if (coherent) {
    paste(
      "Derive the field from related spine, labour-force, household, education,",
      "health, or imputation context and encode it in the official year-specific",
      "Census domain."
    )
  } else {
    paste(
      "Draw a deterministic synthetic value from the exact official",
      "year-specific Census codeframe and apply documented scope or",
      "not-stated values where relevant."
    )
  }

  caveat <- if (typed_missing) {
    paste(
      "The official 2021 detailed-microdata workbook establishes a",
      "four-character field, and ABS names 10 regions, but neither source",
      "publishes the exact four-character region-code crosswalk. No code is invented."
    )
  } else if (variable %in% c("NRMRD", "NRMRP")) {
    paste(
      "NRMR is a non-ASGS output. The 2021 field uses the latest official",
      "machine-readable ABS NRMR list available in the registry (2016).",
      "Row allocation is synthetic and is not population-calibrated."
    )
  } else if (variable == "HOSD") {
    paste(
      "The 2016 product metadata names HOSD but does not enumerate its codes;",
      "the implementation uses the official 2021 HOSD classification as the",
      "nearest published codeframe. Household allocation is synthetic."
    )
  } else if (variable %in% c("PUR1P", "PUR5P")) {
    paste(
      "Supplementary values are verified in the official 2016 DataLab test file.",
      "Historical moves and their frequencies are synthetic and not calibrated."
    )
  } else if (geography) {
    paste(
      "The codes are official, but assignment within state and historical",
      "movement or workplace location is synthetic and not population-calibrated."
    )
  } else {
    paste(
      "The code domain is source-backed. Conditional relationships are enforced",
      "where implemented, but synthetic frequencies are not ABS population estimates."
    )
  }

  data.frame(
    dataset = "CENSUS",
    variable = variable,
    occurrence_count = as.integer(register$occurrence_count[[i]]),
    status = status,
    determination = determination,
    rule = rule,
    evidence_source = evidence_for(variable),
    caveat = caveat,
    official_descriptions = register$official_descriptions[[i]],
    implementation = paste(
      "R/dil_census_values.R; R/dil_geography_values.R;",
      "src/rust/src/census_2021.rs"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

decision <- do.call(rbind, decision)
decision <- decision[order(toupper(decision$variable)), , drop = FALSE]
rownames(decision) <- NULL

required_text <- c(
  "dataset", "variable", "status", "determination", "rule",
  "evidence_source", "caveat"
)
stopifnot(
  nrow(decision) == 225L,
  sum(decision$occurrence_count) == 413L,
  !anyDuplicated(paste(decision$dataset, toupper(decision$variable), sep = "\r")),
  all(vapply(decision[required_text], function(x) {
    all(!is.na(x) & nzchar(trimws(x)))
  }, logical(1))),
  identical(decision$variable[decision$status == "typed_missing"], c("EMPCD", "EMPCP"))
)

output <- file.path(
  repo_root, "inst", "internal-docs",
  "admin-value-decision-ledger-census.csv"
)
utils::write.csv(
  decision, output,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
cat("Wrote ", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("Rows: ", nrow(decision), "; baseline occurrences: ",
    sum(decision$occurrence_count), "\n", sep = "")
print(table(decision$status), quote = FALSE)

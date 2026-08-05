# Build the ACLD administrative-value decision ledger.

mapping <- utils::read.csv(
  file.path("inst", "extdata", "codeframes", "acld-variable-codeframes.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
gap <- utils::read.csv(
  file.path("inst", "internal-docs", "admin-value-gap-register.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
gap <- gap[gap$dataset == "ACLD", , drop = FALSE]
remediation <- utils::read.csv(
  file.path("inst", "internal-docs", "admin-value-remediation-register.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
remediation <- remediation[remediation$dataset == "ACLD", , drop = FALSE]
census_mapping <- utils::read.csv(
  file.path("inst", "extdata", "codeframes", "census-variable-codeframes.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)

manual_variables <- c(
  "LEVEL", "SYNTHETIC_AEUID", "WEIGHT4_11_16",
  "WEIGHT4_11_16_21", "WEIGHT4_16_21"
)
public_census_bases <- c(
  "ANC1P", "ANC2P", "BPLP", "BPFPR", "BPMPR", "LANP",
  "INDP", "INDP_1DIG", "INDP_2DIG", "INDP_3DIG", "OCCP",
  "QALFP", "YARP", "EETP", "LFSF", "CLTHP", "HLTHP", "IFNMFD"
)
public_asgs_2021_bases <- c(
  "IAREA_UR", "RA_UR", "MBUCD", "MBUCP", "SA1UCD", "SA1UCP",
  "SA2UCD", "SA2UCP", "POWP", "POWP_SA2"
)

field_spec <- function(variable) {
  upper <- toupper(variable)
  year_match <- regmatches(
    upper, regexec("_(11|16|21)$", upper, perl = TRUE)
  )[[1L]]
  year <- if (length(year_match)) {
    as.integer(paste0("20", year_match[[2L]]))
  } else {
    NA_integer_
  }
  base <- sub(
    "_(FP|MP|SP|S|P)_(11|16|21)$", "", upper, perl = TRUE
  )
  base <- sub("_(11|16|21)$", "", base, perl = TRUE)
  list(variable = upper, year = year, base = base)
}

unsupported_decision <- function(variable) {
  spec <- field_spec(variable)
  if (spec$variable %in% manual_variables) return("existing_manual")
  if (spec$base %in% public_census_bases) {
    return("public_census_classification")
  }
  if (spec$base == "LGA_UR" ||
      (!is.na(spec$year) && spec$year == 2021L &&
       spec$base %in% public_asgs_2021_bases)) {
    return("public_asgs_geography")
  }
  "unresolved"
}

mapping$unsupported_decision <- ifelse(
  mapping$supported,
  "official_acld_workbook_codeframe",
  vapply(mapping$variable, unsupported_decision, character(1))
)

gap_key <- paste(gap$official_product, toupper(gap$variable), sep = "\r")
mapping_key <- paste(mapping$product, toupper(mapping$variable), sep = "\r")
gap_mapping <- mapping[match(gap_key, mapping_key), , drop = FALSE]
stopifnot(!anyNA(gap_mapping$variable))
gap$resolution <- ifelse(
  gap_mapping$supported,
  "official_acld_workbook_codeframe",
  gap_mapping$unsupported_decision
)
gap$resolution[
  gap$variable %in% manual_variables & gap$resolution == "unresolved"
] <- "existing_manual"

variables <- sort(unique(c(
  remediation$variable,
  mapping$variable[!mapping$supported]
)))

source_url_for <- function(variable, decisions) {
  rows <- mapping[mapping$variable == variable, , drop = FALSE]
  public <- decisions[decisions %in% c(
    "public_census_classification", "public_asgs_geography"
  )]
  if (!length(public)) return(paste(unique(rows$source_url), collapse = " | "))

  spec <- field_spec(variable)
  if ("public_asgs_geography" %in% public) {
    if (spec$base == "LGA_UR") {
      lga <- utils::read.delim(
        file.path("inst", "extdata", "codeframes", "lga.tsv"),
        stringsAsFactors = FALSE, colClasses = "character"
      )
      hit <- unique(lga$source_url[lga$year == as.character(spec$year)])
      if (length(hit)) return(hit[[1L]])
    }
    sources <- utils::read.csv(
      file.path(
        "inst", "extdata", "codeframes", "census-geography-sources.csv"
      ),
      stringsAsFactors = FALSE
    )
    layer <- switch(
      spec$base,
      IAREA_UR = "IARE", RA_UR = "RA",
      MBUCD = "SA1", MBUCP = "SA1", SA1UCD = "SA1", SA1UCP = "SA1",
      SA2UCD = "SA2", SA2UCP = "SA2", POWP = "SA2", POWP_SA2 = "SA2",
      NA_character_
    )
    hit <- sources$source_url[sources$year == 2021L & sources$layer == layer]
    if (length(hit)) return(hit[[1L]])
    return(rows$source_url[[1L]])
  }

  census_variable <- switch(
    spec$base, BPFPR = "BPFP", BPMPR = "BPMP",
    INDP_1DIG = "INDP", INDP_2DIG = "INDP", INDP_3DIG = "INDP",
    spec$base
  )
  census_year <- spec$year
  if (census_variable %in% c("ANC1P", "ANC2P") && census_year == 2011L) {
    census_year <- 2016L
  }
  if (census_variable %in% c(
    "QALFP", "EETP", "LFSF", "CLTHP", "HLTHP", "IFNMFD"
  )) {
    census_year <- 2021L
  }
  hit <- census_mapping$source_url[
    census_mapping$year == census_year &
      toupper(census_mapping$variable) == census_variable
  ]
  if (length(hit)) hit[[1L]] else rows$source_url[[1L]]
}

rationale_for <- function(variable, decisions) {
  spec <- field_spec(variable)
  if ("unresolved" %in% decisions) {
    if (spec$base == "IEO_UR") {
      return(paste(
        "The workbook does not enumerate the SEIFA Index of Education and",
        "Occupation score. The person spine cannot reconstruct the exact",
        "area-level score without a vintage-specific SEIFA lookup."
      ))
    }
    if (grepl("ASGS", spec$variable, fixed = TRUE)) {
      return(paste(
        "This field is a cross-vintage ASGS concordance. No checked-in",
        "official concordance maps the source-area vintage to the target",
        "vintage, so copying a current code would be false precision."
      ))
    }
    return(paste(
      "The field needs a", spec$year,
      "vintage-specific geography or place-of-work code. The checked-in",
      "official lookups do not contain that vintage and no defensible",
      "crosswalk is available."
    ))
  }
  if ("public_census_classification" %in% decisions) {
    return(paste(
      "The ACLD workbook points to a public Census classification. The",
      "generator uses the checked-in official Census or classification",
      "domain and derives a coherent code from the linked spine state."
    ))
  }
  if ("public_asgs_geography" %in% decisions) {
    return(paste(
      "The generator uses the checked-in official ASGS codeframe for the",
      "field's reference year and selects a code that is consistent with",
      "the person's state or 2021 SA2."
    ))
  }
  if ("existing_manual" %in% decisions) {
    return(paste(
      "This is an identifier, record-level label, or ACLD analysis weight.",
      "The existing generator supplies it directly rather than treating it",
      "as a classification."
    ))
  }
  paste(
    "The official ACLD detailed-microdata workbook enumerates the domain,",
    "and the existing ACLD codeframe generator uses that domain."
  )
}

ledger <- do.call(rbind, lapply(variables, function(variable) {
  map_rows <- mapping[mapping$variable == variable, , drop = FALSE]
  gap_rows <- gap[gap$variable == variable, , drop = FALSE]
  decisions <- unique(map_rows$unsupported_decision)
  if (!length(decisions) && nrow(gap_rows)) decisions <- unique(gap_rows$resolution)
  unresolved <- sum(gap_rows$resolution == "unresolved")
  existing <- sum(gap_rows$resolution %in% c(
    "official_acld_workbook_codeframe", "existing_manual"
  ))
  public <- sum(gap_rows$resolution %in% c(
    "public_census_classification", "public_asgs_geography"
  ))
  determination <- if (!nrow(gap_rows)) {
    if (all(decisions == "unresolved")) "unresolved_non_gap_field" else {
      "resolved_non_gap_field"
    }
  } else if (unresolved == nrow(gap_rows)) {
    "unresolved"
  } else if (unresolved > 0L) {
    "partly_resolved"
  } else {
    "resolved"
  }
  data.frame(
    dataset = "ACLD",
    variable = variable,
    original_gap_occurrence_count = nrow(gap_rows),
    workbook_mapping_occurrence_count = nrow(map_rows),
    workbook_supported_occurrence_count = sum(map_rows$supported),
    workbook_unsupported_occurrence_count = sum(!map_rows$supported),
    existing_resolved_gap_occurrence_count = existing,
    new_public_resolved_gap_occurrence_count = public,
    remaining_unresolved_gap_occurrence_count = unresolved,
    determination = determination,
    decision_basis = paste(sort(unique(decisions)), collapse = " | "),
    exact_rationale = rationale_for(variable, decisions),
    evidence_url = source_url_for(variable, decisions),
    stringsAsFactors = FALSE
  )
}))

stopifnot(
  nrow(ledger) == 507L,
  sum(ledger$original_gap_occurrence_count) == 766L,
  sum(ledger$workbook_unsupported_occurrence_count) == 208L,
  sum(ledger$existing_resolved_gap_occurrence_count) == 625L,
  sum(ledger$new_public_resolved_gap_occurrence_count) == 125L,
  sum(ledger$remaining_unresolved_gap_occurrence_count) == 16L
)

utils::write.csv(
  ledger,
  file.path("inst", "internal-docs", "acld-admin-value-decisions.csv"),
  row.names = FALSE, na = ""
)

message(
  "Wrote ", nrow(ledger), " ACLD dataset-variable decisions: ",
  sum(ledger$existing_resolved_gap_occurrence_count), " existing, ",
  sum(ledger$new_public_resolved_gap_occurrence_count), " newly public, ",
  sum(ledger$remaining_unresolved_gap_occurrence_count), " unresolved."
)

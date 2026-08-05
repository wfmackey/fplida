#!/usr/bin/env Rscript

register_path <- file.path(
  "inst", "internal-docs", "admin-value-remediation-register.csv"
)
output_path <- file.path(
  "inst", "internal-docs", "admin-value-decision-ledger-aedc.csv"
)

register <- utils::read.csv(
  register_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
ledger <- register[
  register$dataset == "AEDC",
  c("dataset", "variable", "occurrence_count")
]

structural <- scan(
  text = paste(
    "CYCLE MOC AGCAMPUSID COMMUNITYID JCAMPUSID",
    "LOCALCOMMUNITYID LOCATIONAGEID REGIONID SCHOOLAGEID SCHOOLID TEACHERID"
  ),
  what = "character",
  quiet = TRUE
)
consult_types <- paste0("CONSULTTYPE", 1:13)
lookup <- c(
  "COUNTRY", "PLACEOFBIRTH", "PARENT1COUNTRY",
  "COMMUNITY", "LOCALCOMMUNITY", "REGION",
  "GCCSACODE", "GCCSANAME", "IARECODE", "IARENAME",
  "ILOCCODE", "ILOCNAME", "RDACODE", "RDANAME",
  "SA2NAME", "SA3NAME", "SA4NAME", "STATEELECTORATE",
  "STATEELECTORATECODE", "LCARIACODE", "LCARIANAME", "SEIFARANK",
  "LANGUAGEID", "ILANGUAGEID", paste0("OTHERLANGUAGEID", 1:4),
  paste0("OTHERILANGUAGEID", 1:4)
)
aggregate_rule <- c(
  "CPROFILE", "CPUBLIC", "IAREPUBLIC", "ILOCPUBLIC",
  "LCABSSEIFACATEGORY", "LCMAPPABLE", "LCPROFILE", "LCPUBLIC"
)
proprietary <- c(
  "COMGEN_1", paste0("EMOT_", 1:4), paste0("EMOT_", 1:4, "_VULN"),
  paste0("LANGCOG_", 1:4), paste0("LANGCOG_", 1:4, "_VULN"),
  "MSI", "MSICATEGORY", "MSIVALID",
  paste0("PHYS_", 1:3), paste0("PHYS_", 1:3, "_VULN"),
  paste0("SOC_", 1:4), paste0("SOC_", 1:4, "_VULN")
)
unavailable_aggregate <- c(
  "LCABSERP", "LCABSMOVED", "LCABSUNEMPLOYED",
  "LCABSYEAR12", "LCABSYSPARENTS"
)
unverified_school <- c("SCHOOLCLUSTER", "SCHOOLREGION", "SCHOOLSUBURB")
unsupported <- c(
  consult_types, lookup, aggregate_rule, proprietary,
  unavailable_aggregate, unverified_school, "REFUGEESTATUS"
)
public_instrument <- c(
  "A1Z", "A1AZ", "A1BZ", "A1CZ", "A1DZ",
  paste0("B1", LETTERS[1:4]), "CONSULT", "CONSULTROLE",
  paste0("D", 1:9), "D10", "D10A", "D10B", "D10C",
  grep("^D10.*Y", ledger$variable, value = TRUE), "D11",
  grep("^DIAGNOSIS", ledger$variable, value = TRUE),
  "E1", paste0("LANGSOURCE", 0:5),
  "PARENT1SCHOOL", "PARENT1POSTSCHOOL",
  "PARENT2SCHOOL", "PARENT2POSTSCHOOL"
)
direct <- c(
  "AGECAT", "AGEGROUP", "DAYCARE", "DAYCARENO", "DEVDIFF",
  paste0("ONTRACK", 0:5), paste0("OT", 1:4),
  "PRESCHOOL", "PSDC", "SEIFAEXCLUDED"
)
populated <- setdiff(ledger$variable, c(structural, unsupported))

stopifnot(
  length(structural) == 11L,
  length(unsupported) == 96L,
  length(public_instrument) == 96L,
  length(direct) == 18L,
  length(populated) == 287L,
  !anyDuplicated(c(structural, unsupported, populated)),
  setequal(c(structural, unsupported, populated), ledger$variable),
  all(public_instrument %in% populated),
  all(direct %in% populated)
)

url_2025 <- paste0(
  "https://www.aedc.gov.au/resources/detail/",
  "aedc-2025-data-dictionary"
)
url_2019 <- paste0(
  "https://www.aedc.gov.au/resources/detail/",
  "aedc-data-dictionary-2019"
)
url_questions <- paste0(
  "https://www.aedc.gov.au/resources/detail/",
  "2024-early-development-instrument-questions"
)
source_exact <- paste(
  "AEDC 2025 Data Dictionary; src/rust/src/aedc.rs;",
  "R/generate_aedc.R; tests/testthat/test-generate-aedc.R;",
  url_2025
)
source_rule <- paste(
  "AEDC 2025 Data Dictionary; 2024 EDI questionnaire;",
  "R/dil_aedc_values.R; tests/testthat/test-dil-aedc-values.R;",
  url_2025, url_questions
)
source_history <- paste(
  "AEDC Data Dictionary 2019; AEDC 2025 Data Dictionary;",
  "R/dil_aedc_values.R; src/rust/src/aedc.rs;",
  url_2019, url_2025
)
source_geo <- paste(
  "AEDC 2025 Data Dictionary; R/dil_geography_values.R;",
  "bundled ABS LGA, Indigenous Region and PHN codeframes;",
  url_2025
)
source_missing <- paste(
  "AEDC 2025 Data Dictionary; R/dil_aedc_values.R;",
  url_2025
)

row_result <- function(status, determination, rule, evidence, caveat) {
  list(
    status = status,
    determination = determination,
    rule = rule,
    evidence_source = evidence,
    caveat = caveat
  )
}

structural_result <- function(name) {
  if (name == "CYCLE") {
    return(row_result(
      "structural", "implemented-collection-structure",
      paste(
        "Exact source maps collection years 2009, 2012, 2015, 2018,",
        "2021 and 2024 to official cycle codes 1 to 6; YEAR keeps the year."
      ),
      source_exact,
      paste(
        "The mapping is exact for the six released cycles.",
        "Unsupported cycle years become typed missing."
      )
    ))
  }
  if (name == "MOC") {
    return(row_result(
      "structural", "implemented-collection-structure",
      paste(
        "AEDC rule samples instrument-completion month from official values",
        "4 to 9, with most records in May to July."
      ),
      source_rule,
      paste(
        "The domain is official. The month distribution is synthetic and",
        "is not calibrated to collection operations."
      )
    ))
  }
  entity <- switch(
    name,
    AGCAMPUSID = "Australian Government campus",
    COMMUNITYID = "AEDC community",
    JCAMPUSID = "jurisdictional campus",
    LOCALCOMMUNITYID = "AEDC local community",
    LOCATIONAGEID = "location-age campus",
    REGIONID = "AEDC region",
    SCHOOLAGEID = "school-age",
    SCHOOLID = "school",
    TEACHERID = "teacher"
  )
  row_result(
    "structural", "implemented-synthetic-identifier",
    paste0(
      "AEDC rule creates a stable prefixed ", entity,
      " identifier. Deterministic records share nested groups within a build."
    ),
    paste(
      "PLIDA DIL variable definition; R/dil_aedc_values.R;",
      "tests/testthat/test-dil-aedc-values.R"
    ),
    paste0(
      "The value preserves the ", entity,
      " relationship and identifier shape only. It is not an operational ID."
    )
  )
}

unsupported_result <- function(name) {
  if (name %in% consult_types) {
    return(row_result(
      "unsupported-codeframe", "typed-missing-unverified-legacy-code",
      paste0(
        "Typed NA_character_. The exact source omits ", name,
        ", and the 2025 dictionary does not define the 13 legacy fields or",
        " their stored checkbox codes."
      ),
      paste(
        "2024 EDI questionnaire P3 and P4; PLIDA DIL labels;",
        "R/dil_aedc_values.R;", url_questions, url_2025
      ),
      paste(
        "The public job-title labels are candidates after cycle-specific",
        "encoding is confirmed. Do not invent 0/1 values."
      )
    ))
  }
  if (name %in% lookup) {
    basis <- if (grepl("LANGUAGE|COUNTRY|PLACEOFBIRTH", name)) {
      "AEDC country or language reference table"
    } else if (name == "PARENT1COUNTRY") {
      "AEDC country reference table and generated parent record"
    } else {
      "AEDC or ABS geography table linked to generated location"
    }
    return(row_result(
      "unsupported-codeframe", "typed-missing-public-lookup",
      paste0(
        "Typed NA_character_. The exact source omits ", name,
        ", and no ", basis, " is imported and linked in the AEDC rule."
      ),
      source_missing,
      paste0(
        "Candidate after lookup import: use the exact vintage-specific ",
        basis,
        ". The official reference workbook was not inspected in this audit."
      )
    ))
  }
  if (name %in% aggregate_rule) {
    return(row_result(
      "unsupported-codeframe", "typed-missing-aggregate-rule",
      paste0(
        "Typed NA_character_. ", name,
        " depends on linked area counts and AEDC publication or quintile",
        " criteria, but no aggregate disclosure calculation exists."
      ),
      source_missing,
      paste(
        "Candidate after geography grouping: implement the published group",
        "rule. A random record-level flag would be misleading."
      )
    ))
  }
  if (name %in% proprietary) {
    return(row_result(
      "unsupported-codeframe", "typed-missing-proprietary-score",
      paste0(
        "Typed NA_character_. The dictionary names ", name,
        " but does not publish the licensed subdomain or multiple-strength",
        " score formula and required numeric cut-offs."
      ),
      source_missing,
      paste(
        "Not a public-definition candidate. Populate only from an authorised",
        "scoring specification; do not reuse the synthetic domain proxy."
      )
    ))
  }
  if (name %in% unavailable_aggregate) {
    return(row_result(
      "unsupported-codeframe", "typed-missing-unavailable-aggregate",
      paste0(
        "Typed NA_character_. ", name,
        " requires a linked local-community population or Census aggregate",
        " that is absent from the generator and bundled codeframes."
      ),
      source_missing,
      paste(
        "Not a definition-only candidate. A verified aggregate, geography",
        "vintage and denominator are required."
      )
    ))
  }
  if (name %in% unverified_school) {
    return(row_result(
      "unsupported-codeframe", "typed-missing-unverified-school-label",
      paste0(
        "Typed NA_character_. The source has no school-location hierarchy,",
        " and no public cycle-specific codeframe was verified for ", name, "."
      ),
      source_missing,
      paste(
        "Not a current public-definition candidate. Do not create placeholder",
        "school cluster, region or suburb labels."
      )
    ))
  }
  if (name == "REFUGEESTATUS") {
    return(row_result(
      "unsupported-codeframe", "typed-missing-no-refugee-source",
      paste(
        "Typed NA_character_. The official pre-populated field uses 1 for",
        "Yes and blank for unknown, but AEDC input has no refugee-status source."
      ),
      source_missing,
      paste(
        "Not a definition-only candidate. Populate from a linked source;",
        "random assignment would confound No with Unknown."
      )
    ))
  }
  stop("Unmapped unsupported AEDC variable: ", name)
}

instrument_result <- function(name) {
  evidence <- source_rule
  if (name == "A1Z") {
    rule <- paste(
      "AEDC rule maps A1 absence bands to A1Z codes 0 to 4 and",
      "deterministically separates zero from one day within A1 code 1."
    )
    caveat <- paste(
      "The domain is public. The zero-versus-one split is synthetic because",
      "A1 combines those values."
    )
  } else if (name %in% c("A1AZ", "A1BZ", "A1CZ", "A1DZ")) {
    rule <- paste0(
      "AEDC rule gives ", name,
      " a one-digit raw absence count only when its paired A1 reason is 1."
    )
    caveat <- paste(
      "The meaning and format are public. Counts are synthetic and are not",
      "exact decompositions of total absence days."
    )
  } else if (name %in% paste0("B1", LETTERS[1:4])) {
    rule <- paste0(
      "AEDC rule gives ", name,
      " official ratings 1 to 3, 88 and the vintage-specific N/A code, with",
      " Indigenous, language, state and cycle filters."
    )
    caveat <- paste(
      "Ratings are synthetic. Cycles through 2018 use 999; later cycles use",
      "99 for Not applicable."
    )
    evidence <- source_history
  } else if (name == "CONSULT") {
    rule <- paste(
      "AEDC rule gives 0 No or 1 Yes only for Aboriginal or Torres Strait",
      "Islander children."
    )
    caveat <- "The public domain and filter are exact. The rate is synthetic."
  } else if (name == "CONSULTROLE") {
    rule <- paste(
      "AEDC rule gives official role codes 1 to 4 only when CONSULT is 1;",
      "other rows are structurally missing."
    )
    caveat <- "The role domain is public. Role frequencies are synthetic."
  } else if (grepl("^D(?:[1-9]|10.*|11)$", name)) {
    rule <- paste0(
      "AEDC rule generates ", name,
      " from its official emerging-needs domain and keeps D10 group and",
      " specific-condition fields coherent."
    )
    caveat <- if (grepl("^D[1-9]$", name)) {
      paste(
        "Cycles 1 and 2 use 0, 3 and 88. Later cycles use 0, 1, 2 and 88.",
        "Prevalence is synthetic."
      )
    } else {
      "The domain and conditional structure are public. Prevalence is synthetic."
    }
  } else if (grepl("^DIAGNOSIS", name)) {
    rule <- paste0(
      "AEDC rule gives ", name,
      " a cycle-available 0/1 value only within SPECIALNEEDS records."
    )
    caveat <- paste(
      "The label and binary domain are public. Diagnosis prevalence and",
      "co-occurrence are synthetic and are not clinical data."
    )
  } else if (name == "E1") {
    rule <- paste(
      "AEDC rule gives official early-intervention values 0, 1 and 88,",
      "conditional on the shared emerging-needs state."
    )
    caveat <- "The domain is public. Participation rates are synthetic."
  } else if (grepl("^LANGSOURCE[0-5]$", name)) {
    rule <- paste0(
      "AEDC rule gives ", name,
      " official values 0 Unselected, 1 No and 2 Yes only when language",
      " communication is applicable."
    )
    caveat <- "The domain and filter are public. Frequencies are synthetic."
  } else if (grepl("^PARENT[12](?:POST)?SCHOOL$", name)) {
    rule <- paste0(
      "AEDC rule gives ", name,
      " official education codes 1 to 4 and 88 Not known."
    )
    caveat <- paste(
      "The domain is public. Parent education is synthetic and is not linked",
      "to generated parent records."
    )
  } else {
    stop("Unmapped instrument AEDC variable: ", name)
  }
  row_result(
    "populated", "implemented-public-instrument-rule",
    rule, evidence, caveat
  )
}

direct_result <- function(name) {
  if (name == "AGECAT") {
    rule <- paste(
      "AEDC rule maps AGEINMONTHS to official narrow age codes 0 to 15",
      "using published month cut-points."
    )
    caveat <- paste(
      "The codeframe is public. AGEINMONTHS uses a synthetic birth month",
      "because Rust input contains birth year only."
    )
  } else if (name == "AGEGROUP") {
    rule <- "AEDC rule maps AGEINMONTHS to official labels <5, 5, 6 and >6."
    caveat <- "The codeframe is public. AGEINMONTHS uses a synthetic birth month."
  } else if (name %in% c("DAYCARE", "DAYCARENO", "PRESCHOOL", "PSDC")) {
    rule <- paste0(
      "AEDC rule derives ", name,
      " from E2Y preschool and E3AY long-day-care attendance, preserving 88",
      " where the result is unknown."
    )
    caveat <- paste(
      "The derivation is public. Underlying E2Y and E3AY distributions are",
      "synthetic."
    )
  } else if (name == "DEVDIFF") {
    rule <- paste(
      "AEDC rule sets 1 when any generated D1 to D10 emerging-needs",
      "condition is present; otherwise it sets 0."
    )
    caveat <- "The derivation is public. Emerging-needs prevalence is synthetic."
  } else if (grepl("^OT[1-4]$", name)) {
    threshold <- sub("OT", "", name)
    rule <- paste0(
      "AEDC rule sets ", name, " to 1 when at least ", threshold,
      " of five complete domain categories are 3 or 4; incomplete records",
      " remain missing."
    )
    caveat <- "The definition is public. It inherits synthetic domain categories."
  } else if (grepl("^ONTRACK[0-5]$", name)) {
    threshold <- sub("ONTRACK", "", name)
    rule <- if (threshold == "0") {
      paste(
        "AEDC rule sets ONTRACK0 to 1 when no complete domain is on track;",
        "incomplete records remain missing."
      )
    } else {
      paste0(
        "AEDC rule sets ", name, " to 1 when at least ", threshold,
        " complete domains are on track; incomplete records remain missing."
      )
    }
    caveat <- paste(
      "This implements the legacy public concept and inherits synthetic",
      "domain categories."
    )
  } else if (name == "SEIFAEXCLUDED") {
    rule <- "AEDC rule sets 1 only when SEIFADECILE is unavailable; otherwise 0."
    caveat <- paste(
      "The missingness rule is public. Generated SEIFADECILE is always present,",
      "so generated records normally receive 0."
    )
  } else {
    stop("Unmapped direct AEDC variable: ", name)
  }
  row_result(
    "populated", "implemented-public-derivation",
    rule, source_rule, caveat
  )
}

exact_result <- function(name) {
  if (grepl("^[ABC][0-9]", name)) {
    rule <- if (name == "A1") {
      "Exact source uses official absence bands 1 to 4."
    } else if (name %in% c("A1A", "A1B", "A1C", "A1D")) {
      "Exact source uses official 0 zero-days and 1 one-or-more-days indicators."
    } else if (name == "A4A") {
      paste(
        "Exact source uses official values 0, 1, 88 and the vintage-specific",
        "Not applicable code."
      )
    } else if (name == "B1") {
      paste(
        "Exact source uses official ratings 1 to 3, 88 and the",
        "vintage-specific Not applicable code."
      )
    } else if (name %in% c(
      "A2", "A3", "A3A", "A3B", "A4", paste0("A", 5:7),
      paste0("B", 8:40)
    )) {
      paste(
        "Exact source uses official binary development responses 1, 2 and 88,",
        "with public structural skips."
      )
    } else {
      paste(
        "Exact source uses official ordinal responses 1 to 3 and 88,",
        "with public structural skips."
      )
    }
    historical <- name %in% c("A4A", "B1")
    caveat <- if (historical) {
      paste(
        "Probabilities are synthetic. Cycles through 2018 use 999; later",
        "cycles use 99 for Not applicable."
      )
    } else {
      paste(
        "The domain and skip rule are public. Item probabilities are",
        "synthetic and are not calibrated to AEDC microdata."
      )
    }
    return(row_result(
      "populated", "implemented-exact-source", rule,
      if (historical) source_history else source_exact, caveat
    ))
  }

  preliminary <- c(
    "CANASSESS", "CANCOM", "CLASSTYPEA_1", "CLASSTYPEA_2",
    "CLASSTYPEB", "CLASSTYPEC", "CLASSTYPEC_2", "ESL", "LANG", "TMSCH"
  )
  if (name %in% preliminary) {
    domain <- c(
      CANASSESS = "0 or 1", CANCOM = "0, 1 or 88",
      CLASSTYPEA_1 = "0 or 1", CLASSTYPEA_2 = "1 to 3",
      CLASSTYPEB = "0 or 1", CLASSTYPEC = "0 or 1",
      CLASSTYPEC_2 = "1 to 3", ESL = "0 or 1", LANG = "0 or 1",
      TMSCH = "0 to 2"
    )[[name]]
    return(row_result(
      "populated", "implemented-exact-source",
      paste0(
        "Exact source uses official ", name, " values ", domain,
        " and applies the public conditional skip."
      ),
      source_exact,
      "The public domain and skip are exact. Prevalence is synthetic."
    ))
  }

  if (grepl("^E(?:2|3|4|5|6|7)", name)) {
    rule <- if (name == "E2Y") {
      "Exact source uses official preschool values 1 Yes, 2 No and 88 Don’t know."
    } else if (name %in% c("E2AY", "E2BY")) {
      paste(
        "Exact source uses official conditional preschool codes 1 to 3;",
        "structural non-applicability remains missing."
      )
    } else if (grepl("^E3", name)) {
      "Exact source uses official non-parental-care codes 1 to 4 and 88."
    } else if (name == "E4") {
      "Exact source uses official playgroup values 0, 1 and 88."
    } else {
      "Exact source uses official transition-to-school ratings 1 to 3 and 88."
    }
    return(row_result(
      "populated", "implemented-exact-source", rule, source_exact,
      paste(
        "The public domain and conditional structure are exact. Response",
        "probabilities are synthetic."
      )
    ))
  }

  if (name %in% c("AGECUT", "AGEGROUP3TO7")) {
    rule <- if (name == "AGECUT") {
      paste(
        "Exact source maps age to 0 under 5, 1 age 5 and 2 age 6 or older."
      )
    } else {
      "Exact source stores integer age in years; generated entrants are 4 to 6."
    }
    return(row_result(
      "populated", "implemented-exact-source", rule, source_exact,
      "The public domain is exact. Birth month within birth year is synthetic."
    ))
  }

  base_domains <- c(
    SCHOOLTYPE = "official governance codes C, G and I",
    SEIFADECILE = "integer SEIFA-IRSD deciles 1 to 10",
    SEIFACATEGORY = "public SEIFA quintiles 1 to 5 derived from decile",
    SPECIALNEEDS = "official 0 not special needs and 1 special needs",
    LBOTE = "official 0 No and 1 Yes"
  )
  if (name %in% names(base_domains)) {
    return(row_result(
      "populated", "implemented-exact-source",
      paste0("Exact source uses ", base_domains[[name]], "."),
      source_exact,
      if (name %in% c("SEIFADECILE", "SEIFACATEGORY")) {
        "The domain is public. Values are synthetic and are not linked to geography."
      } else {
        "The domain is public. Generated prevalence or weights are synthetic."
      }
    ))
  }

  if (name %in% c("PHYS", "SOC", "EMOT", "LANGCOG", "COMGEN")) {
    return(row_result(
      "populated", "implemented-synthetic-domain-score",
      paste0("Exact source supplies a bounded 0 to 10 latent-trait proxy for ", name, "."),
      source_exact,
      paste(
        "The range is public. Licensed item weights and the official numeric",
        "formula are unavailable, so values are explicitly synthetic proxies."
      )
    ))
  }

  if (name %in% c(
    "PHYSCATEGORY", "SOCCATEGORY", "EMOTCATEGORY",
    "LANGCOGCATEGORY", "COMGENCATEGORY"
  )) {
    return(row_result(
      "populated", "implemented-public-category-proxy",
      paste0(
        "Exact source assigns ", name,
        " codes 1 to 4 using public 10th, 25th and 50th percentile bands."
      ),
      source_exact,
      paste(
        "Meanings are public. Synthetic normal cut-points replace unavailable",
        "official score cut-offs."
      )
    ))
  }

  if (name %in% c(
    "PHYSVALID", "SOCVALID", "EMOTVALID", "LANGCOGVALID", "COMGENVALID"
  )) {
    return(row_result(
      "populated", "implemented-public-validity-rule",
      paste0(
        "Exact source sets ", name,
        " from the public 75 percent completion threshold, assessability and",
        " special-needs exclusions."
      ),
      source_exact,
      "The public rule operates on synthetic item responses."
    ))
  }

  if (name %in% c("VALIDDOMAINS", "DV1", "DV2", "OT5")) {
    rule <- switch(
      name,
      VALIDDOMAINS = "Exact source counts the five domain-validity flags, giving 0 to 5.",
      DV1 = "Exact source sets 1 when one or more complete domains are vulnerable.",
      DV2 = "Exact source sets 1 when two or more complete domains are vulnerable.",
      OT5 = "Exact source sets 1 when all five complete domains are on track."
    )
    return(row_result(
      "populated", "implemented-public-summary-rule", rule, source_exact,
      paste(
        "The public concept is applied conservatively when all five domains",
        "are valid and inherits synthetic category cut-points."
      )
    ))
  }

  geography <- c(
    "IREGCODE", "IREGNAME", "IREGPUBLIC", "LCLGACODE", "LCLGANAME",
    "LGACODE", "LGANAME", "LGAPUBLIC", "PHNCODE", "PHNNAME"
  )
  if (name %in% geography) {
    public <- grepl("PUBLIC$", name)
    rule <- if (public) {
      paste0("Shared geography rule uses official publishable code 1 for ", name, ".")
    } else {
      paste0(
        "Shared geography rule samples a state-consistent ", name,
        " code or paired label from a bundled official codeframe."
      )
    }
    caveat <- if (public) {
      paste(
        "The code is public. Synthetic areas are treated as publishable",
        "because disclosure counts are not calculated."
      )
    } else {
      paste(
        "The code-label pair is valid and state-consistent but is not",
        "calculated from a child address."
      )
    }
    return(row_result(
      "populated", "implemented-public-codeframe", rule, source_geo, caveat
    ))
  }

  if (name %in% c("REMOTENESS", "REMOTENESSCODE")) {
    rule <- if (name == "REMOTENESS") {
      "Exact source maps codes 1 to 5 to the five official ASGS remoteness names."
    } else {
      "Exact source stores official remoteness-area codes 1 to 5 separately."
    }
    return(row_result(
      "populated", "implemented-exact-source", rule, source_exact,
      paste(
        "The mapping is public. Weights are synthetic and are not linked to",
        "generated geography."
      )
    ))
  }

  stop("Unmapped exact or shared AEDC variable: ", name)
}

results <- lapply(ledger$variable, function(name) {
  if (name %in% structural) return(structural_result(name))
  if (name %in% unsupported) return(unsupported_result(name))
  if (name %in% public_instrument) return(instrument_result(name))
  if (name %in% direct) return(direct_result(name))
  exact_result(name)
})

ledger$status <- vapply(results, `[[`, character(1), "status")
ledger$determination <- vapply(results, `[[`, character(1), "determination")
ledger$rule <- vapply(results, `[[`, character(1), "rule")
ledger$implemented_domain_or_derivation <- ledger$rule
ledger$evidence_source <- vapply(results, `[[`, character(1), "evidence_source")
ledger$caveat <- vapply(results, `[[`, character(1), "caveat")
ledger <- ledger[c(
  "dataset", "variable", "occurrence_count", "status", "determination",
  "rule", "implemented_domain_or_derivation", "evidence_source", "caveat"
)]

status_levels <- c("populated", "structural", "unsupported-codeframe")
stopifnot(
  nrow(ledger) == 394L,
  sum(ledger$occurrence_count) == 1910L,
  !anyDuplicated(ledger[c("dataset", "variable")]),
  !any(vapply(
    ledger,
    function(value) any(is.na(value) | !nzchar(as.character(value))),
    logical(1)
  )),
  identical(
    as.integer(table(factor(ledger$status, levels = status_levels))),
    c(287L, 11L, 96L)
  ),
  identical(
    as.integer(tapply(
      ledger$occurrence_count,
      factor(ledger$status, levels = status_levels),
      sum
    )),
    c(1364L, 74L, 472L)
  )
)

utils::write.csv(ledger, output_path, row.names = FALSE, na = "")
cat(
  "Wrote ", nrow(ledger), " AEDC decisions covering ",
  sum(ledger$occurrence_count), " occurrences to ", output_path, ".\n",
  sep = ""
)

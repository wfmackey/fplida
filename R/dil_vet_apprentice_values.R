# Source-backed values for canonical Total VET Activity and Apprentice and
# Trainee tables.
#
# Public code frames come from the NCVER AVETMISS data element definitions:
# https://www.ncver.edu.au/__data/assets/pdf_file/0022/62383/AVETMISS-Data-element-definitions-2_3-Nov-2022.pdf
# The historical employment-arrangement codes come from edition 2.2:
# https://www.ncver.edu.au/__data/assets/file/0011/10316/AVETMISS_Data_element_definitions_2_2.pdf
# Qualification codes, titles, packages, ASCED fields, and levels below are
# exact National Training Register entries from https://training.gov.au/.

.dil_vet_weighted_code <- function(key, codes, weights) {
  stopifnot(length(codes) == length(weights), all(weights >= 0))
  cumulative <- cumsum(weights) / sum(weights)
  unit <- ((as.numeric(key) %% 1000000) + 0.5) / 1000000
  index <- findInterval(
    unit,
    c(0, cumulative),
    rightmost.closed = TRUE,
    all.inside = TRUE
  )
  codes[pmin(index, length(codes))]
}

.dil_vet_string_key <- function(value, seed, salt = 0L) {
  value <- as.character(value)
  vapply(seq_along(value), function(i) {
    text <- value[[i]]
    if (is.na(text) || !nzchar(text)) text <- paste0("ROW", i)
    chars <- as.numeric(utf8ToInt(text))
    hash <- sum(chars * seq_along(chars))
    (hash + as.numeric(seed) * 9176 + as.numeric(salt) * 104729) %%
      999999999
  }, numeric(1))
}

.dil_vet_as_date <- function(value) {
  if (inherits(value, "Date")) return(value)
  if (is.numeric(value)) return(as.Date(value, origin = "1970-01-01"))
  suppressWarnings(as.Date(as.character(value)))
}

.dil_tva_unsupported_variables <- c(
  "PROVIDER_REPORTING_TYPE_ID_GF", "GF_PROV_REPORT_TYPE_ID",
  "STUDY_MODE_ID", "REGISTRATION_STATUS_ID_DRVD"
)

.dil_tva_source_value <- function(name, description, source_frame,
                                  spine_rows, seed, period) {
  upper <- toupper(name)
  n <- nrow(spine_rows)

  # Legacy DIL names for exact columns emitted by the TVA generator.
  aliases <- list(
    AT_SCHOOL_FG = "AT_SCHOOL_FG",
    HIGHEST_ED_LVL_ST = "HIGHEST_ED_LVL_ST",
    HIGHEST_SCHL_LVL_ST = "HIGHEST_SCHL_LVL_ST",
    LABOUR_FORCE_STATUS_ID = "LABOUR_FORCE_STATUS_ID",
    PRIOR_ED_ACHIEVE_FG = "PRIOR_ED_ACHIEVE_FG",
    PROGRAM_ID = "PROGRAM_ID",
    PROGRAM_LOE_ID = "PROGRAM_LOE_ID",
    PROGRAM_RECOGNITION_ID = "PROGRAM_RECOGNITION_ID",
    RTO_ID = "RTO_ID",
    CLIENT_REMOTE_ID_DERIVED = "CLIENT_REMOTENESS_ID_DERIVED",
    CLIENT_REMOTENESS_ID_DRVD = "CLIENT_REMOTENESS_ID_DERIVED",
    CLIENT_REMOTENESS_ID_DERIVED = "CLIENT_REMOTENESS_ID_DERIVED",
    CLIENT_SEIFA_IRSD_QUINT_DRVD = "CLIENT_SEIFA_IRSD_QUINT_DRVD",
    PROGRAM_FOE_ID = "PROGRAM_FOE_ID",
    TRAIN_PACKAGE_ID = "PROGRAM_TRAINING_PACKAGE_ID",
    PROGRAM_HIGHEST_FUNDING_SOURCE = "NATIONAL_FUNDING_SOURCE_ID",
    PROG_HIGHEST_FUNDING_SOURCE_ID = "NATIONAL_FUNDING_SOURCE_ID"
  )
  if (upper %in% names(aliases)) {
    value <- .dil_source_alias(source_frame, aliases[[upper]])
    if (!is.null(value)) return(value)
  }

  if (upper %in% .dil_tva_unsupported_variables) return(NULL)

  if (upper %in% c("COUNTRY_OF_DELIVERY", "DEL_LOC_COUNTRY_ID")) {
    # The generated TVA delivery locations are Australian postcodes. SACC
    # 1101 is Australia.
    return(rep("1101", n))
  }

  if (upper == "DEL_LOC_REMOTENESS_ID_DRVD") {
    # TVA currently locates delivery in the client's postcode. Preserve the
    # exact generated remoteness classification for that location.
    return(.dil_source_alias(
      source_frame, "CLIENT_REMOTENESS_ID_DERIVED"
    ))
  }

  rto <- .dil_source_alias(source_frame, "RTO_ID")
  if (upper %in% c(
    "RTO_HO_REMOTENESS_ID_DRVD", "RTO_HO_SEIFA_IRSD_QUIN_DRVD"
  )) {
    if (is.null(rto)) return(NULL)
    key <- .dil_vet_string_key(
      rto, seed, .stable_name_seed("TVA RTO head office geography")
    )
    if (upper == "RTO_HO_REMOTENESS_ID_DRVD") {
      return(as.integer(.dil_vet_weighted_code(
        key, 0L:4L, c(45, 25, 18, 8, 4)
      )))
    }
    return(as.integer(.dil_vet_weighted_code(
      key, 1L:5L, c(22, 21, 20, 19, 18)
    )))
  }

  if (upper == "DEL_LOC_ID_ENCRYPT") {
    postcode <- .dil_source_alias(
      source_frame,
      c("DEL_LOC_POSTCODE", "DELIVERY_LOCATION_POSTCODE",
        "CLIENT_POSTCODE_DERIVED")
    )
    state <- .dil_source_alias(
      source_frame,
      c("DEL_LOC_STATE", "CLIENT_STATE_RESIDENCE_DERIVED")
    )
    if (is.null(postcode)) return(NULL)
    if (is.null(state)) state <- rep("", length(postcode))
    location <- paste(as.character(state), as.character(postcode), sep = "|")
    key <- .dil_vet_string_key(
      location, seed, .stable_name_seed("TVA delivery location")
    )
    return(paste0("DL", sprintf("%08.0f", key %% 1e8)))
  }

  if (upper == "SUBMITTER_TYPE") {
    # AVETMISS submission entities are STA, BOS, or RTO. Link school records
    # to BOS and use provider type for a plausible STA/RTO split.
    school <- .dil_source_alias(
      source_frame, c("VET_IN_SCHOOLS_FG", "AT_SCHOOL_FG")
    )
    organisation_type <- .dil_source_alias(
      source_frame, "TRAIN_ORG_TYPE_ID"
    )
    identity <- if (!is.null(rto)) rto else seq_len(n)
    key <- .dil_vet_string_key(
      identity, seed, .stable_name_seed("TVA submitter type")
    )
    out <- rep("STA", n)
    if (!is.null(organisation_type)) {
      private <- as.integer(organisation_type) %in% c(91L, 93L, 95L, 97L)
      out[private & key %% 100L < 65L] <- "RTO"
    } else {
      out[key %% 100L < 30L] <- "RTO"
    }
    if (!is.null(school)) {
      school_rows <- which(toupper(as.character(school)) == "Y")
      out[school_rows] <- "BOS"
    }
    return(out)
  }

  NULL
}

.dil_at_status_code <- function(source_frame, n) {
  status <- .dil_source_alias(source_frame, "STATUS")
  if (is.null(status)) return(NULL)
  labels <- toupper(trimws(as.character(status)))
  map <- c(
    "IN TRAINING" = "01", "ACTIVE" = "01", "RECOMMENCED" = "02",
    "WITHDRAWN" = "03", "COMPLETED" = "04", "EXPIRED" = "05",
    "CANCELLED" = "06", "CANCELED" = "06", "SUSPENDED" = "07",
    "TRANSFERRED" = "11"
  )
  out <- unname(map[labels])
  if (length(out) != n) return(NULL)
  out
}

.dil_at_contract_dates <- function(source_frame, n) {
  start <- .dil_source_alias(source_frame, "START_DATE")
  if (is.null(start)) return(NULL)
  start <- .dil_vet_as_date(start)
  if (length(start) != n) return(NULL)

  days <- .dil_source_alias(source_frame, "DAYS_IN_TRAINING")
  expected_end <- rep(as.Date(NA), n)
  if (!is.null(days)) expected_end <- start + as.integer(days)

  end <- .dil_source_column(source_frame, "END_DATE")
  if (is.null(end)) {
    end <- expected_end
  } else {
    end <- .dil_vet_as_date(end)
    missing <- is.na(end)
    end[missing] <- expected_end[missing]
  }
  list(start = start, end = end)
}

.dil_at_school_codes <- function(source_frame, spine_rows, seed) {
  n <- nrow(spine_rows)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("A&T school levels")
  )
  education <- if ("education" %in% names(spine_rows)) {
    as.integer(spine_rows$education)
  } else {
    rep(NA_integer_, n)
  }
  education[is.na(education)] <- 1L
  highest <- rep("12", n)
  below_year_12 <- education <= 1L
  choices <- c("08", "09", "10", "11")
  weights <- c(8, 12, 35, 45)
  highest[below_year_12] <- .dil_vet_weighted_code(
    key[below_year_12], choices, weights
  )
  no_school <- education == 0L & key %% 100L < 2L
  highest[no_school] <- "02"

  school_based <- .dil_source_alias(source_frame, "SCHOOL_BASED")
  current <- rep("99", n)
  if (!is.null(school_based)) {
    at_school <- toupper(as.character(school_based)) == "Y"
    at_school[is.na(at_school)] <- FALSE
    current_number <- 10L + as.integer(key %% 3L)
    current[at_school] <- sprintf("%02d", current_number[at_school])
    # AVETMISS requires the completed level to precede the level currently
    # undertaken (for example, current Year 10 means completed Year 9).
    highest[at_school] <- sprintf(
      "%02d", pmax(current_number[at_school] - 1L, 8L)
    )
  }
  list(highest = highest, current = current)
}

.dil_at_language_frame <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file(
      "extdata", "codeframes", "ascl_language.tsv", package = "fplida"
    )
    if (!nzchar(path)) {
      path <- file.path("inst", "extdata", "codeframes", "ascl_language.tsv")
    }
    values <- utils::read.delim(
      path,
      stringsAsFactors = FALSE,
      colClasses = c("character", "character", "numeric"),
      check.names = FALSE
    )
    values <- values[
      grepl("^[0-9]{4}$", values$code) & values$code != "1201" &
        is.finite(values$weight) & values$weight > 0,
      ,
      drop = FALSE
    ]
    stopifnot(nrow(values) > 100L, !anyDuplicated(values$code))
    cache <<- values
    cache
  }
})

.dil_at_language_home <- function(spine_rows, key) {
  n <- nrow(spine_rows)
  country <- if ("country_of_birth_sacc" %in% names(spine_rows)) {
    sprintf("%04d", as.integer(spine_rows$country_of_birth_sacc))
  } else {
    rep("1101", n)
  }
  country[is.na(country) | !grepl("^[0-9]{4}$", country)] <- "9999"
  major <- suppressWarnings(as.integer(substr(country, 1L, 1L)))
  english_probability <- c(
    `1` = 0.82, `2` = 0.72, `3` = 0.25, `4` = 0.18, `5` = 0.16,
    `6` = 0.14, `7` = 0.18, `8` = 0.55, `9` = 0.30
  )[as.character(major)]
  english_probability[is.na(english_probability)] <- 0.70
  english_probability[which(country == "1101")] <- 0.82
  unit <- ((as.numeric(key) %% 1000000) + 0.5) / 1000000
  non_english <- unit >= english_probability
  out <- rep("1201", n)
  if (any(non_english)) {
    codeframe <- .dil_at_language_frame()
    out[non_english] <- .dil_vet_weighted_code(
      key[non_english] + 104729,
      codeframe$code,
      codeframe$weight
    )
  }
  out
}

.dil_at_qualification_frame <- data.frame(
  qualification_level = c(
    2L, 2L, 2L, 2L,
    3L, 3L, 3L, 3L,
    4L, 4L, 4L, 4L,
    5L, 5L, 5L
  ),
  qualification_code = c(
    "BSB20120", "AUR20720", "CHC22015", "CPC20120",
    "BSB30120", "CPC30220", "CHC33021", "UEE30820",
    "BSB40120", "CPC40120", "CHC43015", "ICP40120",
    "BSB50120", "CPC50220", "ICT50120"
  ),
  qualification_title = c(
    "Certificate II in Workplace Skills",
    "Certificate II in Automotive Vocational Preparation",
    "Certificate II in Community Services",
    "Certificate II in Construction",
    "Certificate III in Business",
    "Certificate III in Carpentry",
    "Certificate III in Individual Support",
    "Certificate III in Electrotechnology Electrician",
    "Certificate IV in Business",
    "Certificate IV in Building and Construction",
    "Certificate IV in Ageing Support",
    "Certificate IV in Printing and Graphic Arts Management",
    "Diploma of Business",
    "Diploma of Building and Construction (Building)",
    "Diploma of Information Technology"
  ),
  program_level = c(
    rep("521", 4L), rep("514", 4L), rep("511", 4L), rep("421", 3L)
  ),
  field_of_education = c(
    "0809", "0305", "0905", "0403",
    "0809", "0403", "0905", "0313",
    "0803", "0403", "0905", "0301",
    "0803", "0403", "0203"
  ),
  training_package_code = c(
    "BSB", "AUR", "CHC", "CPC",
    "BSB", "CPC", "CHC", "UEE",
    "BSB", "CPC", "CHC", "ICP",
    "BSB", "CPC", "ICT"
  ),
  training_package_title = c(
    "Business Services Training Package",
    "Automotive Retail, Service and Repair Training Package",
    "Community Services",
    "Construction, Plumbing and Services Training Package",
    "Business Services Training Package",
    "Construction, Plumbing and Services Training Package",
    "Community Services",
    "Electrotechnology Training Package",
    "Business Services Training Package",
    "Construction, Plumbing and Services Training Package",
    "Community Services",
    "Printing and Graphic Arts",
    "Business Services Training Package",
    "Construction, Plumbing and Services Training Package",
    "Information and Communications Technology"
  ),
  stringsAsFactors = FALSE
)

.dil_at_qualification_rows <- function(source_frame, spine_rows, seed) {
  n <- nrow(spine_rows)
  level <- .dil_source_alias(source_frame, "QUALIFICATION_LEVEL")
  if (is.null(level)) return(NULL)
  level <- as.integer(level)
  if (length(level) != n) return(NULL)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("A&T qualification")
  )
  occupation <- .dil_source_alias(source_frame, c("ANZSCO", "ANZSCOCODE"))
  if (is.null(occupation)) occupation <- rep("", n)
  occupation <- as.character(occupation)
  preferred <- rep("", n)
  preferred[grepl("^(33|821|1331|3121)", occupation)] <- "CPC"
  preferred[grepl("^34", occupation)] <- "UEE"
  preferred[grepl("^(32|8999)", occupation)] <- "AUR"
  preferred[grepl("^(4|411|423)", occupation)] <- "CHC"
  preferred[grepl("^26", occupation)] <- "ICT"
  preferred[preferred == ""] <- "BSB"

  selected <- rep(NA_integer_, n)
  for (i in seq_len(n)) {
    choices <- which(.dil_at_qualification_frame$qualification_level == level[[i]])
    matching <- choices[
      .dil_at_qualification_frame$training_package_code[choices] ==
        preferred[[i]]
    ]
    if (length(matching)) choices <- matching
    if (length(choices)) {
      selected[[i]] <- choices[[1L + as.integer(key[[i]] %% length(choices))]]
    }
  }
  out <- .dil_at_qualification_frame[selected, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.dil_at_prior_education <- function(spine_rows, key) {
  if (!"education" %in% names(spine_rows)) return(NULL)
  education <- as.integer(spine_rows$education)
  out <- rep(NA_character_, length(education))
  certificate <- education == 3L
  diploma <- education == 4L
  out[certificate] <- ifelse(key[certificate] %% 2L == 0L, "514", "511")
  out[diploma] <- ifelse(key[diploma] %% 3L == 0L, "410", "420")
  out[which(education >= 5L)] <- "008"
  out
}

.dil_at_unsupported_variables <- c(
  "ALL_INDUSTRIES", "APPLICATIONSTATUSCODE", "APPRENTICESHIPTYPE",
  "ARTS_AND_ENTERTAINMENT", "AUTOMOTIVE", "BASEQUALCODE",
  "BUILDING_AND_CONSTRUCTION", "BUSINESSRELATIONSHIP", "CITIZENSHIPCODE",
  "COMMENCEMENTTYPECODE", "COMMENCEMENTTYPESUBCODE", "COMMUNICATIONS",
  "COMMUNITY_SERVICES_AND_HEALTH", "CONTACTMETHOD", "DESCRIPTION",
  "FIN_INSUR_AND_BUS_S", "FURN_LGHT_MNFCTRNG", "INDUSTRIES",
  "ISEQUIVALENT", "MANUFACTURING_ENGINEERING", "MINING", "PAYMENTGROUP",
  "PAYMENTGROUPCODE", "PAYMENTGROUPDESCRIPTION", "PAYMENTSTATUSCODE",
  "PAYMENTTYPECODE", "PROCESS_MANUFACTURING", "PROPERTY_SERVICES",
  "QUALCODESUPERCEDEDBY", "QUALCODESUPERCEDES", "REGION",
  "RETAIL_AND_WHOLESALE", "SPORT_AND_RECREATION", "TOURISM",
  "TRANSPORT_AND_DISTRIBUTION", "UTILITIES_AND_ELCTRTECH"
)

.dil_apprentice_source_value <- function(name, description, source_frame,
                                         spine_rows, seed, period) {
  upper <- toupper(name)
  n <- nrow(spine_rows)

  if (upper %in% .dil_at_unsupported_variables) return(NULL)

  if (upper %in% c("CURRENTSTATUS", "TRAININGCONTRACTSTATUS")) {
    return(.dil_at_status_code(source_frame, n))
  }
  if (upper == "DAYSINTRAINING") {
    return(.dil_source_alias(source_frame, "DAYS_IN_TRAINING"))
  }
  if (upper == "DAYSONSUSPENSION") {
    status <- .dil_at_status_code(source_frame, n)
    if (is.null(status)) return(NULL)
    # The compact source generator has no suspended state. Zero is therefore
    # a structural result, not a placeholder duration.
    return(rep(0L, n))
  }

  sequence_fields <- c(
    "APPRENTICEAPPRENTICESHIPNUMBER", "APPRENTICESHIPTRNGCONTRANO",
    "APPRENTICESHPEMPLRELATSHIP", "APPRENTICETRNGCONTRANO"
  )
  if (upper %in% sequence_fields) {
    # The source generator creates one apprenticeship, training contract, and
    # employer relationship for each selected apprentice.
    return(rep(1L, n))
  }

  dates <- NULL
  earliest <- c(
    "APPRENTICEEARLIEST", "APPRENTICESHIPEARLIEST",
    "EMPLOYERRELATIONSHIPEARLIEST", "TRAININGCONTRACTEARLIEST"
  )
  latest <- c(
    "APPRENTICELATEST", "APPRENTICESHIPLATEST",
    "EMPLOYERRELATIONSHIPLATEST", "TRAININGCONTRACTLATEST"
  )
  if (upper %in% c(earliest, latest, "IN_MAY_23", "EMPLOYMENTARRANGEMENTCODE")) {
    dates <- .dil_at_contract_dates(source_frame, n)
    if (is.null(dates)) return(NULL)
  }
  if (upper %in% earliest) return(dates$start)
  if (upper %in% latest) return(dates$end)
  if (upper == "IN_MAY_23") {
    in_window <- !is.na(dates$start) & dates$start <= as.Date("2023-05-31") &
      !is.na(dates$end) & dates$end >= as.Date("2023-05-01")
    return(as.integer(in_window))
  }

  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed(paste("A&T", upper, sep = "|"))
  )
  if (upper == "EMPLOYMENTARRANGEMENTCODE") {
    out <- rep(NA_character_, n)
    legacy <- !is.na(dates$start) & dates$start < as.Date("2016-07-01")
    out[legacy] <- .dil_vet_weighted_code(
      key[legacy], sprintf("%02d", 11L:16L), c(15, 5, 45, 5, 20, 10)
    )
    return(out)
  }

  if (upper %in% c("HIGHESTSCHOOLLEVELCODE", "ATSCHOOLLEVELCODE")) {
    school <- .dil_at_school_codes(source_frame, spine_rows, seed)
    if (upper == "HIGHESTSCHOOLLEVELCODE") return(school$highest)
    return(school$current)
  }
  if (upper == "LANGUAGEATHOMECODE") {
    return(.dil_at_language_home(spine_rows, key))
  }
  if (upper == "SELFASSESSEDDISABILITYCODE") {
    if (!"disability_onset_year" %in% names(spine_rows)) return(NULL)
    onset <- as.integer(spine_rows$disability_onset_year)
    return(ifelse(!is.na(onset) & onset <= as.integer(period$end_year), "Y", "N"))
  }
  if (upper == "PRIOREDUACHIEVEMENTIDENT") {
    return(.dil_at_prior_education(spine_rows, key))
  }
  if (upper == "STA") {
    state <- .dil_source_alias(source_frame, "STATE_ASGS_2021")
    if (is.null(state)) return(NULL)
    state <- as.integer(state)
    return(ifelse(state %in% 1L:8L, sprintf("%02d", state), NA_character_))
  }
  if (upper == "RTOTYPE") {
    school_based <- .dil_source_alias(source_frame, "SCHOOL_BASED")
    school <- rep(FALSE, n)
    if (!is.null(school_based)) {
      school <- toupper(as.character(school_based)) == "Y"
      school[is.na(school)] <- FALSE
    }
    out <- .dil_vet_weighted_code(
      key,
      c("31", "41", "51", "53", "61", "91", "93", "95", "97"),
      c(30, 2, 3, 5, 5, 45, 2, 5, 3)
    )
    if (any(school)) {
      out[school] <- .dil_vet_weighted_code(
        key[school], c("21", "25", "27"), c(65, 20, 15)
      )
    }
    return(out)
  }

  qualification_fields <- c(
    QUALCODE = "qualification_code",
    QUALIFICATIONCODE = "qualification_code",
    QUALLEVEL = "program_level",
    QUALTITLE = "qualification_title",
    FOECODE = "field_of_education",
    TPCODE = "training_package_code",
    TPTITLE = "training_package_title"
  )
  if (upper %in% names(qualification_fields)) {
    qualification <- .dil_at_qualification_rows(
      source_frame, spine_rows, seed
    )
    if (is.null(qualification)) return(NULL)
    return(qualification[[qualification_fields[[upper]]]])
  }

  NULL
}

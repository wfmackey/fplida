# Home Affairs and APS Employment Database values for canonical DIL tables.
#
# Public sources do not expose every internal ARMS, SDB, visa, or APSED code.
# This file therefore separates public classifications from small, explicit
# local synthetic domains. It does not emit generic C01-style placeholders.

.dil_ha_pick <- function(values, key) {
  values[1L + as.integer(key %% length(values))]
}

.dil_ha_weighted_pick <- function(values, weights, key) {
  stopifnot(length(values) == length(weights), all(weights >= 0))
  cumulative <- cumsum(weights) / sum(weights)
  u <- (key %% 1000003) / 1000003
  index <- findInterval(u, c(0, cumulative), rightmost.closed = TRUE)
  values[pmin(pmax(index, 1L), length(values))]
}

.dil_ha_codeframe <- function(filename) {
  path <- system.file("extdata", "codeframes", filename, package = "fplida")
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "codeframes", filename)
  }
  utils::read.delim(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character"
  )
}

.dil_ha_codeframe_value <- function(filename, key, exclude = character()) {
  frame <- .dil_ha_codeframe(filename)
  keep <- !frame$code %in% exclude & !frame$label %in% exclude
  frame <- frame[keep, , drop = FALSE]
  weights <- suppressWarnings(as.numeric(frame$weight))
  weights[!is.finite(weights) | weights < 0] <- 0
  if (!length(weights) || !sum(weights)) weights <- rep(1, nrow(frame))
  .dil_ha_weighted_pick(frame$code, weights, key)
}

.dil_ha_visa_context <- function(source_frame, spine_rows, seed) {
  n <- nrow(spine_rows)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("Home Affairs visa context")
  )
  subclass <- .dil_source_alias(
    source_frame, c("VISA_SUBCLASS_CD", "TR_VISA_SUBCLASS_CD", "VISA_SUB_CLASS")
  )
  if (is.null(subclass)) {
    values <- c(
      "189", "190", "491", "186", "482", "820", "801", "309",
      "300", "100", "500", "485", "417", "462", "200", "202", "204"
    )
    weights <- c(10, 8, 4, 6, 8, 6, 3, 4, 3, 2, 15, 2, 6, 3, 3, 2, 1)
    subclass <- .dil_ha_weighted_pick(values, weights, key)
  }
  subclass <- sprintf("%03d", suppressWarnings(as.integer(subclass)))

  codeframe <- .dil_ha_codeframe("visa_subclass.tsv")
  description <- codeframe$label[match(subclass, codeframe$code)]
  description[is.na(description)] <- paste("Visa subclass", subclass[is.na(description)])

  skilled <- subclass %in% c(
    "124", "132", "186", "187", "188", "189", "190", "191", "457",
    "476", "482", "485", "489", "491", "494", "858", "887", "888"
  )
  family <- subclass %in% c(
    "100", "101", "102", "103", "110", "114", "115", "116", "117",
    "143", "173", "300", "309", "445", "801", "802", "804", "820",
    "835", "836", "837", "838", "864", "870", "884"
  )
  humanitarian <- subclass %in% c(
    "200", "201", "202", "203", "204", "449", "785", "786", "790",
    "851", "866"
  )
  student <- subclass == "500"
  working_holiday <- subclass %in% c("417", "462")
  visitor <- subclass %in% c("600", "601", "602", "651", "771")

  report_group <- ifelse(
    skilled, "Skilled",
    ifelse(
      family, "Family",
      ifelse(
        humanitarian, "Humanitarian",
        ifelse(student, "Student",
               ifelse(working_holiday, "Working Holiday",
                      ifelse(visitor, "Visitor", "Other Temporary")))
      )
    )
  )
  program <- ifelse(
    humanitarian, "Humanitarian Program",
    ifelse(skilled | family, "Migration Program", "Temporary Visa Program")
  )
  primary <- .dil_source_alias(source_frame, "VA_PRIMARY_FL")
  if (is.null(primary)) primary <- ifelse(key %% 10L < 6L, "Y", "N")
  primary <- toupper(as.character(primary)) %in% c("Y", "P", "1", "TRUE")

  list(
    key = key,
    subclass = subclass,
    description = description,
    skilled = skilled,
    family = family,
    humanitarian = humanitarian,
    student = student,
    working_holiday = working_holiday,
    visitor = visitor,
    report_group = report_group,
    program = program,
    primary = primary
  )
}

.dil_amep_award_context <- function(spine_rows, seed) {
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("AMEP CSWE award")
  )
  awards <- data.frame(
    code = c("10725NAT", "10727NAT", "10728NAT", "10729NAT", "10730NAT"),
    level = c(
      "Preliminary", "Certificate I", "Certificate II", "Certificate III",
      "Certificate IV"
    ),
    name = c(
      "Course in Preliminary Spoken and Written English",
      "Certificate I in Spoken and Written English",
      "Certificate II in Spoken and Written English",
      "Certificate III in Spoken and Written English",
      "Certificate IV in Spoken and Written English for Further Study"
    ),
    description = c(
      "Preliminary English language course",
      "Beginner English language course",
      "Post-beginner English language course",
      "Intermediate English language course",
      "Advanced English for further study course"
    ),
    stringsAsFactors = FALSE
  )
  index <- 1L + as.integer(key %% nrow(awards))
  awards[index, , drop = FALSE]
}

.dil_amep_stream <- function(spine_rows, seed, current = FALSE) {
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("AMEP targeted tuition stream")
  )
  age <- 2019L - as.integer(spine_rows$birth_year)
  social <- ifelse(age >= 55L, key %% 100L < 48L, key %% 100L < 6L)
  if (current) {
    social <- social & key %% 10L != 0L
  }
  ifelse(social, "Social English", "Pre-Employment English")
}

.dil_amep_acsf_value <- function(name, source_frame, spine_rows, seed) {
  upper <- toupper(name)
  if (!grepl("^(?:INITIAL|LATEST)_(?:LEARNING|READING|WRITING|ORAL_COMMUNICATION)_[0-9]{2}$",
             upper, perl = TRUE)) {
    return(NULL)
  }
  indicator <- sub("^(?:INITIAL|LATEST)_", "", upper)
  key <- .dil_numeric_key(
    spine_rows, seed,
    .stable_name_seed(paste("AMEP ACSF", indicator, sep = "|"))
  )
  initial <- 1L + as.integer(key %% 3L)
  if (startsWith(upper, "INITIAL_")) return(initial)
  improvement <- as.integer((key %/% 7L) %% 3L)
  pmin(initial + improvement, 5L)
}

.dil_amep_source_value <- function(name, source_frame, spine_rows, seed,
                                   period) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed(paste("AMEP", upper, sep = "|"))
  )

  acsf <- .dil_amep_acsf_value(
    name, source_frame, spine_rows, seed
  )
  if (!is.null(acsf)) return(acsf)

  award <- .dil_amep_award_context(spine_rows, seed)
  if (upper == "AWARD_CODE") return(award$code)
  if (upper == "AWARD_NAME") return(award$name)
  if (upper == "AWARD_DESCRIPTION") return(award$description)
  if (upper == "CSWE_LEVEL") return(award$level)
  if (upper == "ID") return(paste0("ARMS-CSWE-", award$code))

  if (upper == "CSWE_ENTRY_LEVEL") {
    scores <- lapply(
      c("INITIAL_ASLPR_SPEAK", "INITIAL_ASLPR_LISTEN",
        "INITIAL_ASLPR_READ", "INITIAL_ASLPR_WRITE"),
      function(variable) .dil_source_column(source_frame, variable)
    )
    complete <- lengths(scores) == n
    level <- if (all(complete)) {
      floor(rowMeans(as.data.frame(scores), na.rm = TRUE))
    } else {
      as.integer(key %% 3L)
    }
    labels <- c("Preliminary", "Certificate I", "Certificate II")
    return(labels[pmin(pmax(level + 1L, 1L), length(labels))])
  }
  if (upper == "INITIAL_STREAM") {
    return(.dil_amep_stream(spine_rows, seed, current = FALSE))
  }
  if (upper == "CURRENT_STREAM") {
    return(.dil_amep_stream(spine_rows, seed, current = TRUE))
  }

  enrolment_year <- .dil_source_alias(source_frame, "ENROLLED_FY")
  if (is.null(enrolment_year)) enrolment_year <- rep(period$end_year, n)
  enrolment_year <- suppressWarnings(as.integer(substr(enrolment_year, 1L, 4L)))
  age <- pmax(enrolment_year - as.integer(spine_rows$birth_year), 0L)
  if (upper == "AGE_GROUP_AT_REGISTRATION") {
    return(cut(
      age, breaks = c(-Inf, 17, 24, 34, 44, 54, 64, Inf),
      labels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
      right = TRUE
    ) |> as.character())
  }
  if (upper == "LANGUAGE") {
    return(.dil_ha_codeframe_value(
      "ascl_language.tsv", key,
      exclude = c("1201", "&&&&", "English", "Not stated")
    ))
  }
  if (upper == "EMP_STATUS_AT_REGISTRATION") {
    return(.dil_ha_weighted_pick(
      c("Employed", "Unemployed", "Not in the labour force"),
      c(18, 42, 40), key
    ))
  }
  if (upper == "HIGHEST_QUAL_AT_REGISTRATION") {
    return(.dil_ha_weighted_pick(
      c("Bachelor degree or higher", "Diploma", "Certificate",
        "Year 12", "No post-school qualification"),
      c(18, 12, 18, 20, 32), key
    ))
  }
  if (upper == "HEARD_ABOUT_AMEP_FROM") {
    return(.dil_ha_weighted_pick(
      c("Home Affairs", "Services Australia", "Settlement service",
        "Family or friend", "AMEP provider", "Community organisation"),
      c(34, 16, 16, 14, 12, 8), key
    ))
  }
  if (upper == "LEGISLATIVE_ELIGIBILITY") {
    return(ifelse(key %% 100L < 90L, "Legislative", "Policy"))
  }
  if (upper == "RECEIVING_CENTRELINK_BENEFITS") {
    status <- .dil_amep_source_value(
      "EMP_STATUS_AT_REGISTRATION", source_frame, spine_rows, seed, period
    )
    return(ifelse(status == "Employed" & key %% 10L < 7L, "N", "Y"))
  }
  if (upper == "REGISTRATION_CENTRE") {
    state <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
    return(sprintf(
      "AMEP-%s-%03d", state[pmin(pmax(as.integer(spine_rows$state), 1L), 8L)],
      1L + as.integer(key %% 24L)
    ))
  }
  if (upper == "CHILD_CHILDCARE_CENTRE") {
    eligible <- age >= 18L & age <= 50L & key %% 100L < 22L
    value <- sprintf(
      "CHILD%02d|CENTRE%05d", 1L + as.integer(key %% 3L),
      1L + as.integer((key %/% 11L) %% 99999L)
    )
    value[!eligible] <- NA_character_
    return(value)
  }

  visa <- .dil_ha_visa_context(source_frame, spine_rows, seed)
  if (upper == "VISA_SUBCLASS") return(visa$subclass)
  if (upper == "VISA_SUBCLASS_DESC") return(visa$description)
  if (upper == "VISA_MIGRATION_CATEGORY") return(visa$report_group)
  NULL
}

.dil_apsed_job_context <- function(spine_rows, seed) {
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("APSED job family")
  )
  jobs <- data.frame(
    code = c("SD", "CR", "AD", "PP", "PO", "ICT", "DR", "AF", "HR", "LP"),
    family = c(
      "Service Delivery", "Compliance and Regulation", "Administration",
      "Portfolio, Program and Project Management", "Policy",
      "ICT and Digital Solutions", "Data and Research",
      "Accounting and Finance", "Human Resources", "Legal and Parliamentary"
    ),
    function_name = c(
      "Customer services", "Compliance", "Administrative services",
      "Program management", "Policy development", "ICT operations",
      "Research and analysis", "Financial management", "People management",
      "Legal services"
    ),
    role = c(
      "Customer service officer", "Compliance officer",
      "Administrative support officer", "Program officer", "Policy officer",
      "ICT support officer", "Data analyst", "Accountant",
      "Human resources adviser", "Legal officer"
    ),
    weight = c(26, 12, 9, 9, 9, 8, 6, 6, 5, 4),
    stringsAsFactors = FALSE
  )
  index_code <- .dil_ha_weighted_pick(jobs$code, jobs$weight, key)
  jobs[match(index_code, jobs$code), , drop = FALSE]
}

.dil_apsed_classification <- function(spine_rows, seed, acting = FALSE) {
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("APSED classification")
  )
  levels <- c(
    "Trainee", "Graduate", "APS 1", "APS 2", "APS 3", "APS 4", "APS 5",
    "APS 6", "EL 1", "EL 2", "SES 1", "SES 2", "SES 3"
  )
  weights <- c(1, 2, 1, 3, 14, 23, 17, 21, 11, 5, 1.5, 0.4, 0.1)
  substantive <- .dil_ha_weighted_pick(levels, weights, key)
  if (!acting) return(substantive)
  acting_value <- levels[pmin(match(substantive, levels) + 1L, length(levels))]
  acting_value[key %% 5L != 0L | substantive == "SES 3"] <- NA_character_
  acting_value
}

.dil_apsed_education_value <- function(name, spine_rows, seed) {
  upper <- toupper(name)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("APSED education")
  )
  education <- if ("education" %in% names(spine_rows)) {
    suppressWarnings(as.integer(spine_rows$education))
  } else {
    rep(3L, nrow(spine_rows))
  }
  education[is.na(education)] <- 3L
  education <- pmin(pmax(education, 1L), 6L)
  if (upper == "HIGHEST_EDUCATION_QUALIFICATION") {
    # ASCED level-of-education codes used in ABS collections.
    return(c("998", "621", "611", "421", "310", "120")[education])
  }
  if (upper %in% c("FIELD_OF_STUDY_MAIN", "FIELD_OF_STUDY_OTHER")) {
    field <- c(
      "0101", "0201", "0301", "0401", "0501", "0603", "0701", "0801",
      "0901", "1001", "1101", "1201"
    )
    offset <- if (upper == "FIELD_OF_STUDY_OTHER") 5L else 0L
    value <- field[1L + as.integer((key + offset) %% length(field))]
    if (upper == "FIELD_OF_STUDY_OTHER") {
      value[key %% 100L >= 28L] <- NA_character_
      if (length(value) && all(is.na(value))) {
        selected <- which.min(key %% 100L)
        value[[selected]] <- field[
          1L + as.integer((key[[selected]] + offset) %% length(field))
        ]
      }
    }
    return(value)
  }
  NULL
}

.dil_apsed_source_value <- function(name, source_frame, spine_rows, seed,
                                    period) {
  upper <- toupper(name)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed(paste("APSED", upper, sep = "|"))
  )
  if (upper == "ACTING_CLASSIFICATION") {
    return(.dil_apsed_classification(spine_rows, seed, acting = TRUE))
  }
  if (upper == "SUBSTANTIVE_CLASSIFICATION") {
    return(.dil_apsed_classification(spine_rows, seed, acting = FALSE))
  }
  if (upper == "AGENCY_SIZE") {
    return(.dil_ha_weighted_pick(
      c("Micro", "Extra Small", "Small", "Medium", "Large", "Extra Large"),
      c(0.1, 0.8, 2.9, 7.9, 32.5, 55.8), key
    ))
  }
  education <- .dil_apsed_education_value(name, spine_rows, seed)
  if (!is.null(education)) return(education)

  job <- .dil_apsed_job_context(spine_rows, seed)
  if (upper == "JOBFAMILY") return(job$family)
  if (upper == "JOBFAMILY_CODE") return(job$code)
  if (upper == "JOBFAMILY_FUNCTION") return(job$function_name)
  if (upper == "JOBFAMILY_ROLE") return(job$role)

  if (upper == "LEAVE_CODE") {
    # Local APSED-compatible domain; internal source codes are not public.
    return(.dil_ha_weighted_pick(
      c("A", "M", "L", "I", "O"), c(88, 3, 3, 2, 4), key
    ))
  }
  if (upper == "MOVEMENT_CODE") {
    # Local codes: classification, agency, location, hours, separation, engagement.
    return(.dil_ha_weighted_pick(
      c("CLS", "AGY", "LOC", "HRS", "SEP", "ENG"),
      c(28, 14, 10, 8, 20, 20), key
    ))
  }
  NULL
}

.dil_mt_demogs_source_value <- function(name, source_frame, spine_rows, seed) {
  upper <- toupper(name)
  if (upper == "CNTRY_CDE") return(rep("1101", nrow(spine_rows)))
  if (upper == "MARITAL_STATUS") {
    return(.dil_source_alias(source_frame, "MARITAL_STS"))
  }
  if (upper == "SRC_CD") {
    source <- .dil_source_alias(source_frame, "SOURCE")
    if (is.null(source)) return(NULL)
    return(ifelse(toupper(source) == "VISA", "V", "S"))
  }
  NULL
}

.dil_sdb_source_value <- function(name, source_frame, spine_rows, seed) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed(paste("SDB", upper, sep = "|"))
  )
  common_key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("SDB original grant context")
  )
  if (upper == "CSM_SERV_REQ_ID") {
    return(.dil_character_id(
      "CSR", spine_rows, seed, .stable_name_seed("SDB service request"), 12L
    ))
  }
  if (upper == "VISA_GRANT_NO") {
    return(.dil_character_id(
      "VG", spine_rows, seed, .stable_name_seed("Home Affairs visa grant"), 12L
    ))
  }
  if (upper == "DOB_MMMYYYY") {
    month <- .dil_source_alias(source_frame, "MONTH_OF_BIRTH")
    year <- .dil_source_alias(source_frame, "YEAR_OF_BIRTH")
    if (is.null(month)) month <- 1L + as.integer(key %% 12L)
    if (is.null(year)) year <- as.integer(spine_rows$birth_year)
    return(paste0(toupper(month.abb[pmin(pmax(as.integer(month), 1L), 12L)]),
                  sprintf("%04d", as.integer(year))))
  }
  location <- ifelse(common_key %% 10L < 6L, "3", "4")
  if (upper == "LOCATION") return(location)
  if (upper == "DEPENDENT_CODE") {
    principal <- .dil_source_alias(source_frame, "PRINCIPAL_FLAG")
    if (is.null(principal)) principal <- ifelse(common_key %% 10L < 6L, "P", "S")
    dependent <- .dil_ha_weighted_pick(c("S", "C", "O"), c(45, 45, 10), common_key)
    dependent[toupper(principal) == "P" | location == "3"] <- NA_character_
    return(dependent)
  }
  if (upper == "PREF_LANG") {
    return(.dil_ha_codeframe_value(
      "ascl_language.tsv", key, exclude = c("&&&&", "Not stated")
    ))
  }
  if (upper == "RELIGION") {
    return(.dil_ha_codeframe_value("ascrg_religion.tsv", key))
  }
  if (upper == "ETHNICITY") {
    # The public SDB dictionary publishes labels but not its internal codes.
    return(.dil_ha_weighted_pick(
      c("Chinese (NFD)", "Indian", "Filipino (NFD)", "Vietnamese",
        "English", "Afghan", "Iraqi", "Other ethnicity"),
      c(18, 18, 10, 9, 10, 8, 7, 20), key
    ))
  }
  NULL
}

.dil_visa_course_context <- function(rank, source_frame, spine_rows, seed) {
  visa <- .dil_ha_visa_context(source_frame, spine_rows, seed)
  eligible <- visa$student & visa$primary
  if (rank > 1L) eligible <- eligible & visa$key %% (2L^(rank - 1L)) == 0L
  level <- c(
    "Schools Sector",
    "English Language Intensive Courses for Overseas Students",
    "Vocational Education and Training Sector",
    "Higher Education Sector",
    "Postgraduate Research Sector"
  )
  base <- 1L + as.integer((visa$key %/% 13L) %% length(level))
  index <- pmax(base - rank + 1L, 1L)
  suffix <- LETTERS[1L + as.integer((visa$key + rank) %% 26L)]
  course <- sprintf(
    "%06d%s", 1L + as.integer((visa$key + rank * 7919L) %% 999999L), suffix
  )
  provider <- sprintf(
    "%05d%s", 1L + as.integer((visa$key %/% 17L + rank * 101L) %% 99999L), suffix
  )
  course[!eligible] <- NA_character_
  provider[!eligible] <- NA_character_
  level_value <- level[index]
  level_value[!eligible] <- NA_character_
  list(
    eligible = eligible,
    course = course,
    level = level_value,
    provider = provider,
    visa = visa
  )
}

.dil_visa_source_value <- function(name, source_frame, spine_rows, seed) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  visa <- .dil_ha_visa_context(source_frame, spine_rows, seed)

  coe_match <- regexec("^COE([123])_(COURSE_CD|COURSE_LEVEL_DS|PRVDR_CD|PRVDR_ABN_TX)$", upper)
  coe_parts <- regmatches(upper, coe_match)[[1L]]
  if (length(coe_parts)) {
    rank <- as.integer(coe_parts[[2L]])
    field <- coe_parts[[3L]]
    course <- .dil_visa_course_context(
      rank, source_frame, spine_rows, seed
    )
    if (field == "COURSE_CD") return(course$course)
    if (field == "COURSE_LEVEL_DS") return(course$level)
    if (field == "PRVDR_CD") return(course$provider)
    value <- paste0("H", sprintf(
      "%015.0f",
      .dil_numeric_key(
        spine_rows, seed,
        .stable_name_seed(paste("visa CoE provider", rank, sep = "|"))
      ) %% 1e15
    ))
    value[!course$eligible] <- NA_character_
    return(value)
  }

  if (upper == "TR_APLCTN_ID") {
    return(.dil_character_id(
      "APP", spine_rows, seed, .stable_name_seed("Home Affairs visa application"), 12L
    ))
  }
  if (upper %in% c("TR_VISA_GRANT_NUMBER", "VA_VISA_GRANT_NR")) {
    return(.dil_character_id(
      "VG", spine_rows, seed, .stable_name_seed("Home Affairs visa grant"), 12L
    ))
  }
  if (upper == "TR_VISA_SUBCLASS_CD") return(visa$subclass)
  if (upper == "VISA_SUBCLASS_CD") return(visa$subclass)
  if (upper == "VISA_SUBCLASS_DS") return(visa$description)
  if (upper == "VISA_PRGRM_DS") return(visa$program)
  if (upper == "VISA_REPORT_GROUP_DS") return(visa$report_group)

  nominated <- visa$skilled & visa$primary
  occupation_code <- if ("anzsco_code" %in% names(spine_rows)) {
    sprintf("%06d", as.integer(spine_rows$anzsco_code))
  } else {
    rep(NA_character_, n)
  }
  occupation_code[occupation_code == "000000"] <- NA_character_
  occupation_title <- if ("anzsco_title" %in% names(spine_rows)) {
    as.character(spine_rows$anzsco_title)
  } else {
    rep(NA_character_, n)
  }
  if (upper == "NM_OCPTN_FULL_DS") {
    occupation_title[!nominated] <- NA_character_
    return(occupation_title)
  }
  if (upper == "NM_OCPTN_TYPE_CD") {
    value <- rep("ANZSCO", n)
    value[!nominated | is.na(occupation_code)] <- NA_character_
    return(value)
  }
  if (upper == "VA_OCPTN_CD") return(occupation_code)
  if (upper == "VA_OCPTN_FULL_DS") return(occupation_title)
  if (upper == "VA_OCPTN_TYPE_CD") {
    value <- rep("ANZSCO", n)
    value[is.na(occupation_code)] <- NA_character_
    return(value)
  }
  if (upper == "LA_FL") {
    value <- ifelse(visa$key %% 10L < 2L, "Y", "N")
    value[!nominated] <- NA_character_
    return(value)
  }
  if (upper == "SPNSR_BY_INDVDL") {
    sponsored <- visa$family | (visa$skilled & visa$subclass %in% c("186", "187", "482", "494"))
    value <- ifelse(visa$family, "Y", "N")
    value[!sponsored] <- NA_character_
    return(value)
  }
  if (upper == "SP_ABN_TX") {
    business <- visa$skilled & visa$subclass %in% c("186", "187", "482", "494")
    value <- paste0("H", sprintf(
      "%015.0f",
      .dil_numeric_key(
        spine_rows, seed, .stable_name_seed("visa business sponsor")
      ) %% 1e15
    ))
    value[!business] <- NA_character_
    return(value)
  }

  ielts <- .dil_source_alias(source_frame, c("IELTS_OVRL", "VA_IELTS_OVRL"))
  if (is.null(ielts)) {
    ielts <- 4 + round(10 * ((visa$key %% 1000) / 999)) / 2
    ielts[!visa$skilled] <- NA_real_
  }
  ielts <- as.numeric(ielts)
  ielts[ielts <= 0] <- NA_real_
  if (upper %in% c("VA_IELTS_OVRL", "IELTS_OVRL")) return(ielts)
  if (upper %in% c("IELTS_ENGLISH_PROF", "VA_IELTS_ENGLISH_PROF")) {
    value <- cut(
      ielts, breaks = c(-Inf, 4.5, 5.5, 6.5, 7.5, Inf),
      labels = c("Limited", "Modest", "Competent", "Proficient", "Very high"),
      right = FALSE
    ) |> as.character()
    return(value)
  }

  if (upper == "VA_SOURCE_SYSTEM_CD") {
    return(.dil_ha_pick(c("TRIPS", "ICSE", "IRIS", "MPMS"), visa$key))
  }
  if (upper == "VA_STREAM_LONG_DS") {
    return(ifelse(
      visa$subclass %in% c("186", "187", "482", "494"), "Employer sponsored",
      ifelse(visa$subclass %in% c("189", "190", "491"), "Points tested skilled",
             ifelse(visa$family, "Family and partner",
                    ifelse(visa$humanitarian, "Refugee and humanitarian",
                           visa$report_group)))
    ))
  }
  if (upper == "VA_TYPE_LONG_DS") return(visa$report_group)
  if (upper == "VA_TYPE_L2_LONG_DS") return(visa$description)
  NULL
}

.dil_home_affairs_source_value <- function(name, description, dataset,
                                           source_frame, spine_rows, seed,
                                           period, product_name = "",
                                           table_name = "") {
  if (identical(dataset, "AMEP")) {
    return(.dil_amep_source_value(
      name, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "APSED")) {
    return(.dil_apsed_source_value(
      name, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "MT_DEMOGS")) {
    return(.dil_mt_demogs_source_value(
      name, source_frame, spine_rows, seed
    ))
  }
  if (identical(dataset, "SDB")) {
    return(.dil_sdb_source_value(
      name, source_frame, spine_rows, seed
    ))
  }
  if (identical(dataset, "VISA")) {
    return(.dil_visa_source_value(
      name, source_frame, spine_rows, seed
    ))
  }
  NULL
}

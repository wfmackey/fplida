# Canonical values for social-services and selected ATO products.
#
# DEX labels follow the DSS Data Exchange Protocols. DOMINO values use public
# codeframes or direct spine derivations. Unpublished DOMINO, SAE, and RPS
# codeframes remain typed missing unless the exact source column is present.

.dil_social_unsupported_codeframes <- list(
  DEX = c(
    "ADDRESSSTATUSCODE", "CLIENTADDSOURCEVALIDCOD", "NAME", "PRIORITYGRP"
  ),
  DOMINO = c(
    "ACTV_PRTCPN_CODE", "ADDR_PRTY", "ADDR_TYPE_CODE", "ADR_TYP",
    "AGE_GROUP", "BEN_STATUS", "BEN_TYPE", "BEN_TYPE_CODE",
    "BUS_DLY_INC_REAL_ESTATE_CODE", "CARD_ANY", "CARD_HEALTH_CARE_ECH",
    "CARD_HEALTH_CARE_FST", "CDEP_FREQ_CODE", "CHNL", "CMPNT_TYPE",
    "CMTY_CODE", "CODE_TYPE", "CODE_VALUE", "CONCESSION",
    "COURSE_LVL", "COURSE_TYPE", "COURSE_TYPE_DESC",
    "DVA_TYPE_CODE", "END_REASON_CODE", "END_RSN", "END_RSN_CODE",
    "ENT_TYPE", "FREQ_CODE", "FTBA_BEN_STATUS", "FTBA_TYPE_CODE",
    "FTBB_BEN_STATUS", "FTBB_TYPE_CODE", "HSE_ACCOM_CODE", "HSE_HO_CODE",
    "HSE_RENT_TYPE", "IMPRMT_CODE", "INST_TYPE", "ISP_BEN_STATUS",
    "ISP_TYPE_CODE", "LVL_ATTAINED", "LVL_ATTAINED_DESC", "MAN_CODE",
    "MED_PRMY_GRP", "MED_SCNDRY_GRP", "NSPP_PRCPL_CARER_RSN_CODE",
    "POSTALTYPE", "REASON_CODE", "REL_CODE",
    "REL_QUAL_CODE", "RENT_BEN_STATUS", "RENT_TYPE_CODE", "RFRL_RSN_CODE",
    "ROW_TYPE", "STDNT_PRTCPN_STS", "STDNT_STS_CODE", "SUP_BEN_STATUS",
    "SUP_TYPE_CODE", "TASK_CODE", "TYPE_CODE", "ZERO_PAY_RSN"
  ),
  SAE = c(
    "ACNT_PHS_CD", "AGE_RANGE", "LOST_MBR_REGR_MBR_STS_CD",
    "MBR_ACNT_STS_CD", "MBSHP_ACNT_GOVT_RLVR_ACPTDIND",
    "MBSHP_ACNT_OUTWD_RLVRACPTDIND",
    "MBSHPACNTINVTVINCMSTRMPRDCTIND", "PRVDR_TYP_CD",
    "SPNTN_MBRSHPACNTRLVRACPTDIND"
  ),
  RPS = c("GEOCODED_INDEX", "ROLE_CD", "ROLE_CD_UPDATED")
)

.dil_social_is_unsupported_codeframe <- function(dataset, name) {
  upper <- toupper(name)
  values <- .dil_social_unsupported_codeframes[[toupper(dataset)]]
  !is.null(values) && upper %in% values
}

.dil_social_pick <- function(values, spine_rows, seed, salt,
                             weights = NULL) {
  .dil_sample_values(
    values,
    nrow(spine_rows),
    seed,
    .stable_name_seed(salt),
    weights
  )
}

.dil_social_key <- function(spine_rows, seed, salt) {
  .dil_numeric_key(
    spine_rows, seed, .stable_name_seed(salt)
  )
}

.dil_social_id <- function(prefix, spine_rows, seed, salt,
                           width = 9L, groups = NULL) {
  key <- .dil_social_key(spine_rows, seed, salt)
  if (!is.null(groups)) key <- key %% as.integer(groups)
  paste0(prefix, sprintf(paste0("%0", width, ".0f"), key))
}

.dil_social_state <- function(spine_rows, key = NULL) {
  n <- nrow(spine_rows)
  state <- if ("state" %in% names(spine_rows)) {
    as.integer(spine_rows$state)
  } else if ("state_code" %in% names(spine_rows)) {
    as.integer(spine_rows$state_code)
  } else {
    rep(NA_integer_, n)
  }
  if (is.null(key)) key <- seq_len(n)
  invalid <- is.na(state) | !state %in% 1:8
  state[invalid] <- 1L + as.integer(key[invalid] %% 8L)
  state
}

.dil_social_age <- function(spine_rows, period) {
  if (!"birth_year" %in% names(spine_rows)) {
    return(rep(NA_integer_, nrow(spine_rows)))
  }
  pmin(
    pmax(as.integer(period$end_year) - as.integer(spine_rows$birth_year), 0L),
    115L
  )
}

.dil_social_ced_value <- function(spine_rows, seed, salt = "CED|2021") {
  # ASGS Edition 3 uses a state digit and a two-digit division identifier.
  # The counts exclude non-spatial special-purpose codes.
  counts <- c(`1` = 47L, `2` = 39L, `3` = 30L, `4` = 10L,
              `5` = 15L, `6` = 5L, `7` = 2L, `8` = 3L)
  key <- .dil_social_key(spine_rows, seed, salt)
  state <- .dil_social_state(spine_rows, key)
  division <- 1L + as.integer(key %% counts[as.character(state)])
  sprintf("%d%02d", state, division)
}

.dil_social_acpr_value <- function(spine_rows, seed, year) {
  # GEN publishes 73 valid ACPR codes in these state-specific ranges.
  counts <- c(`1` = 16L, `2` = 9L, `3` = 16L, `4` = 11L,
              `5` = 11L, `6` = 3L, `7` = 5L, `8` = 1L)
  key <- .dil_social_key(
    spine_rows, seed, paste("DEX ACPR", year, sep = "|")
  )
  state <- .dil_social_state(spine_rows, key)
  region <- 1L + as.integer(key %% counts[as.character(state)])
  sprintf("%d%02d", state, region)
}

.dil_social_locality_value <- function(spine_rows, seed) {
  path <- system.file(
    "extdata", "codeframes", "sa2_2021.tsv", package = "fplida"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "codeframes", "sa2_2021.tsv")
  }
  if (!file.exists(path)) return(NULL)
  values <- utils::read.delim(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character"
  )
  n <- nrow(spine_rows)
  key <- .dil_social_key(spine_rows, seed, "DEX locality|SA2 2021")
  state <- .dil_social_state(spine_rows, key)
  code <- if ("sa2_code" %in% names(spine_rows)) {
    sprintf("%09d", as.integer(spine_rows$sa2_code))
  } else {
    rep(NA_character_, n)
  }
  matched <- match(code, values$code)
  out <- values$name[matched]
  for (state_value in 1:8) {
    rows <- which(state == state_value & (is.na(out) | !nzchar(out)))
    if (!length(rows)) next
    choices <- values[values$state == as.character(state_value), , drop = FALSE]
    index <- 1L + as.integer(key[rows] %% nrow(choices))
    out[rows] <- choices$name[index]
  }
  out
}

.dil_social_language_value <- function(spine_rows, seed, salt) {
  path <- system.file(
    "extdata", "codeframes", "ascl_language.tsv", package = "fplida"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "codeframes", "ascl_language.tsv")
  }
  if (!file.exists(path)) return(rep("1201", nrow(spine_rows)))
  values <- utils::read.delim(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character"
  )
  values <- values[grepl("^[0-9]{4}$", values$code), , drop = FALSE]
  weight <- suppressWarnings(as.numeric(values$weight))
  weight[!is.finite(weight) | weight < 0] <- 0
  .dil_social_pick(
    values$code, spine_rows, seed, salt, pmax(weight, 0)
  )
}

.dil_dex_calendar <- function(spine_rows, seed, period) {
  n <- nrow(spine_rows)
  start <- max(as.Date("2015-01-01"), as.Date(period$start))
  end <- as.Date(period$end)
  if (is.na(end) || end < start) end <- as.Date("2024-12-31")
  key <- .dil_social_key(spine_rows, seed, "DEX reporting calendar")
  day <- start + as.integer(key %% (as.integer(end - start) + 1L))
  month <- as.integer(format(day, "%m"))
  year <- as.integer(format(day, "%Y"))
  first_half <- month >= 7L
  fy_start <- ifelse(first_half, year, year - 1L)
  half <- ifelse(first_half, "H1", "H2")
  reporting_period <- sprintf(
    "%04d-%02d-%s", fy_start, (fy_start + 1L) %% 100L, half
  )
  reporting_end <- as.Date(ifelse(
    first_half,
    sprintf("%04d-12-31", year),
    sprintf("%04d-06-30", year)
  ))
  list(
    day = day,
    day_name = weekdays(day),
    day_of_week = as.integer(format(day, "%u")),
    iso_week = format(day, "%G-W%V"),
    month_name = months(day),
    week = day - (as.integer(format(day, "%u")) - 1L),
    reporting_period = reporting_period,
    reporting_end = reporting_end
  )
}

.dil_dex_assistance_index <- function(spine_rows, seed) {
  key <- .dil_social_key(spine_rows, seed, "DEX assistance domains")
  list(
    primary = 1L + as.integer(key %% 13L),
    secondary = 1L + as.integer((key %/% 17L) %% 13L)
  )
}

.dil_dex_outcome <- function(spine_rows, seed, table_name) {
  key <- .dil_social_key(
    spine_rows, seed, paste("DEX SCORE", table_name, sep = "|")
  )
  if (identical(tolower(table_name), "special_community_assessment")) {
    type <- rep("Community", nrow(spine_rows))
    domains <- c(
      "Community infrastructure and networks",
      "Organisational knowledge, skills and practices",
      "Group/community knowledge, skills, attitudes and behaviours",
      "Social cohesion"
    )
    domain_codes <- c(
      "GROUPNETWORKS", "ORGSKILLS", "GROUPSKILLS", "SOCIALCOHESION"
    )
  } else {
    types <- c("Circumstances", "Goals", "Satisfaction")
    type <- types[1L + as.integer(key %% length(types))]
    circumstance <- c(
      "Physical health",
      "Mental health, wellbeing and self-care",
      "Personal and family safety",
      "Age-appropriate development",
      "Community participation and networks",
      "Family functioning",
      "Financial resilience",
      "Employment",
      "Education and skills training",
      "Material wellbeing and basic necessities",
      "Housing"
    )
    goals <- c(
      "Changed knowledge and access to information",
      "Changed skills",
      "Changed behaviours",
      "Empowerment, choice and control to make own decisions",
      "Engagement with relevant support services",
      "Changed impact of immediate crisis"
    )
    satisfaction <- c(
      "I am satisfied with the services I have received",
      "The service listened to me and understood my issues",
      "I am better able to deal with issues that I sought help with"
    )
    domain <- character(length(type))
    domain_code <- character(length(type))
    circumstance_codes <- c(
      "PHYSICAL", "MENTAL", "PERSONAL", "AGE", "COMMUNITY", "FAMILY",
      "MONEY", "EMPLOYMENT", "EDUCEMPL", "MATERIAL", "HOUSING"
    )
    goal_codes <- c(
      "KNOWLEDGE", "SKILLS", "BEHAVIOURS", "EMPOWERMENT", "ENGAGEMENT",
      "CRISIS"
    )
    satisfaction_codes <- c("SATISFIED", "LISTENED", "DEAL")
    for (value in unique(type)) {
      rows <- which(type == value)
      choices <- switch(
        value,
        Circumstances = circumstance,
        Goals = goals,
        Satisfaction = satisfaction
      )
      codes <- switch(
        value,
        Circumstances = circumstance_codes,
        Goals = goal_codes,
        Satisfaction = satisfaction_codes
      )
      index <- 1L + as.integer((key[rows] %/% 19L) %% length(choices))
      domain[rows] <- choices[index]
      domain_code[rows] <- codes[index]
    }
    return(list(type = type, domain = domain, domain_code = domain_code))
  }
  index <- 1L + as.integer((key %/% 19L) %% length(domains))
  list(
    type = type,
    domain = domains[index],
    domain_code = domain_codes[index]
  )
}

.dil_dex_source_value <- function(name, description, source_frame,
                                  spine_rows, seed, period,
                                  product_name = "", table_name = "",
                                  module_name = "") {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_social_key(spine_rows, seed, "DEX canonical values")

  direct <- .dil_source_column(source_frame, upper)
  if (!is.null(direct) && any(!is.na(direct))) return(direct)
  if (.dil_social_is_unsupported_codeframe("DEX", upper)) {
    return(rep(NA_character_, n))
  }

  accommodation <- c(
    "Boarding house",
    "Crisis, emergency or transition",
    "Independent living unit",
    "Indigenous community/settlement",
    "Institutional setting (i.e. residential aged care, hospital)",
    "Private residence\u2014client or family owned/purchasing",
    "Private residence\u2014private rental",
    "Private residence\u2014public rental",
    "Public shelter",
    "Supported accommodation",
    "Other",
    "Not stated"
  )
  household <- c(
    "Single (person living alone)",
    "Sole parent with dependant(s)",
    "Couple",
    "Couple with dependant(s)",
    "Group (related adults)",
    "Group (unrelated adults)",
    "Homeless/No household",
    "Not stated or inadequately described"
  )
  education <- c(
    "Pre-primary education", "Primary education", "Secondary education",
    "Certificate level", "Advanced diploma and diploma level",
    "Bachelor\u2019s degree level",
    "Graduate diploma and graduate certificate level",
    "Postgraduate degree level", "Other education"
  )
  employment <- c(
    "Paid work full-time", "Paid work part-time",
    "Unpaid work (includes volunteering)",
    "Not working and not looking for work",
    "Unemployed (not working but looking for work)",
    "Studying full-time", "Studying part-time", "Caring", "Parenting"
  )
  income_source <- c(
    "Nil income", "Employee salary/wages",
    "Other income including superannuation and investments",
    "Self-employed (unincorporated business income)",
    "Government payments/pensions/allowances",
    "Not stated/Inadequately described"
  )
  exit_reason <- c(
    "Client no longer requires assistance",
    "Service unable to provide assistance",
    "Client now requires higher level of care",
    "Client has moved out of area",
    "Client terminated the service", "Client died",
    "Client no longer eligible", "Client needs have been met",
    "None of the above"
  )
  service_setting <- c(
    "Organisation outlet/office", "Clients\u2019 residence", "Community venue",
    "Partner organisation", "Telephone", "Video", "Online service",
    "Healthcare facility", "Education facility", "Justice facility"
  )
  assistance <- .dil_dex_assistance_index(spine_rows, seed)
  assistance_columns <- c(
    PHYSICAL = 1L, MENTAL = 2L, PERSONAL = 3L, DEVELOPMENT = 4L,
    COMMUNITY = 5L, FAMILY = 6L, MONEY = 7L, EMPLOYMENT = 8L,
    TRAINING = 9L, EDUCEMPL = 9L, MATERIAL = 10L, HOUSING = 11L,
    CARING = 12L, OTHER = 13L
  )

  if (upper == "ACCOMMODATIONTYPECODE") {
    return(.dil_social_pick(
      accommodation, spine_rows, seed, "DEX accommodation",
      c(0.03, 0.03, 0.02, 0.01, 0.02, 0.43, 0.25, 0.10,
        0.01, 0.05, 0.03, 0.02)
    ))
  }
  if (upper == "HOUSEHOLDCOMPOSITIONCODE") {
    return(.dil_social_pick(
      household, spine_rows, seed, "DEX household composition",
      c(0.28, 0.14, 0.20, 0.24, 0.05, 0.04, 0.03, 0.02)
    ))
  }
  if (upper == "EDUCATIONLEVELCODE") {
    if ("education" %in% names(spine_rows)) {
      index <- pmin(pmax(as.integer(spine_rows$education), 0L), 6L)
      mapped <- c(2L, 3L, 3L, 4L, 5L, 6L, 8L)[index + 1L]
      return(education[mapped])
    }
    return(.dil_social_pick(
      education, spine_rows, seed, "DEX education level",
      c(0.01, 0.08, 0.38, 0.22, 0.11, 0.12, 0.02, 0.04, 0.02)
    ))
  }
  if (upper == "EMPLOYMENTSTATUSCODE") {
    employed <- if ("baseline_employed" %in% names(spine_rows)) {
      as.integer(spine_rows$baseline_employed) == 1L
    } else {
      key %% 4L == 0L
    }
    out <- employment[5L + as.integer((key %/% 13L) %% 5L)]
    out[employed] <- employment[
      1L + as.integer((key[employed] %/% 11L) %% 2L)
    ]
    return(out)
  }
  if (upper == "INCOMESOURCECODE") {
    employed <- if ("baseline_employed" %in% names(spine_rows)) {
      as.integer(spine_rows$baseline_employed) == 1L
    } else {
      key %% 4L == 0L
    }
    return(ifelse(employed, income_source[[2L]], income_source[[5L]]))
  }
  if (upper == "INCOMEFREQUENCYCODE") {
    return(.dil_social_pick(
      c("Weekly", "Fortnightly", "Monthly", "Annually"),
      spine_rows, seed, "DEX income frequency", c(0.08, 0.62, 0.25, 0.05)
    ))
  }
  if (upper == "HOMELESSCODE") {
    return(.dil_social_pick(
      c("No", "At Risk", "Yes"), spine_rows, seed,
      "DEX homeless indicator", c(0.88, 0.08, 0.04)
    ))
  }
  if (upper == "NDISELIGIBILITYCODE") {
    severity <- if ("disability_severity" %in% names(spine_rows)) {
      as.integer(spine_rows$disability_severity)
    } else {
      rep(NA_integer_, n)
    }
    return(ifelse(
      !is.na(severity) & severity <= 2L, "NDIS eligible",
      ifelse(key %% 10L == 0L, "NDIS in-progress access request",
             "NDIS ineligible")
    ))
  }
  if (upper == "DISABILITYCODE") {
    return(.dil_social_pick(
      c("Intellectual/learning", "Psychiatric", "Sensory/speech",
        "Physical/diverse", "None (no disability)",
        "Not stated/inadequately described"),
      spine_rows, seed, "DEX disability",
      c(0.08, 0.15, 0.07, 0.15, 0.52, 0.03)
    ))
  }
  if (upper == "DVACARDSTATUSCODE") {
    return(.dil_social_pick(
      c("DVA Gold Card", "DVA White Card", "DVA Orange Card or other",
        "No DVA entitlement"),
      spine_rows, seed, "DEX DVA card", c(0.03, 0.03, 0.01, 0.93)
    ))
  }
  if (upper == "EXITREASONCODE") {
    return(.dil_social_pick(
      exit_reason, spine_rows, seed, "DEX exit reason",
      c(0.22, 0.07, 0.06, 0.10, 0.14, 0.02, 0.05, 0.32, 0.02)
    ))
  }
  if (upper == "SERVICESETTINGCODE") {
    return(.dil_social_pick(
      service_setting, spine_rows, seed, "DEX service setting",
      c(0.40, 0.15, 0.12, 0.04, 0.10, 0.03, 0.09, 0.03, 0.03, 0.01)
    ))
  }
  if (upper == "ATTENDANCEPROFILECODE") {
    return(.dil_social_pick(
      c("Family", "Community event", "Peer support group", "Couple",
        "Cohabitants"),
      spine_rows, seed, "DEX attendance profile",
      c(0.30, 0.20, 0.20, 0.20, 0.10)
    ))
  }
  if (upper == "PARTICIPATIONTYPE") {
    return(.dil_social_pick(
      c("Client", "Support person", "Unidentified client"),
      spine_rows, seed, "DEX participation type", c(0.88, 0.08, 0.04)
    ))
  }
  if (upper == "REFERRALFROMSOURCE") {
    return(.dil_social_pick(
      c("Health agency", "Community services agency", "Educational agency",
        "Internal", "Legal agency", "Employment/job placement agency",
        "Lender/financial agency", "Accounting agency",
        "Centrelink/Department of Human Services (DHS)", "Other Agency",
        "Self", "Family", "Friends", "General Medical Practitioner",
        "My Aged Care Gateway", "NDIS referral", "Linkages Package",
        "Continuity of Support (CoS) Programme",
        "Humanitarian Settlement Program", "LAC Referral", "Other party",
        "Not stated/inadequately described"),
      spine_rows, seed, "DEX referral source"
    ))
  }
  if (upper == "EXTERNREFERDETINCODE") {
    return(.dil_social_pick(
      c("Health professional", "Financial institution", "Legal aid/solicitor",
        "Accountant/financial advisor", "Real estate agent", "Agronomist",
        "Succession planner", "Social support group", "Training organisation",
        "Other government agency", "Asset agent", "Other"),
      spine_rows, seed, "DEX external referral destination"
    ))
  }
  if (upper == "MONEYWORKSHOPCODE") {
    return(.dil_social_pick(
      c("Workshop 1 - Making Money Last Until Payday",
        "Workshop 2 - Planning For the Future",
        "Workshop 3 - How Can Banks Help",
        "Workshop 4 - Internet and Phone Banking",
        "Workshop 5 - Credit Can Be a Hazard",
        "Workshop 6 - Money Loans Sharks and Traps",
        "Workshop 7 - A Roof Overhead - Home Ownership",
        "Workshop 8 - A Roof Overhead Tenancy",
        "Workshop 9 - Managing Paperwork", "Other workshop"),
      spine_rows, seed, "DEX money workshop"
    ))
  }
  if (upper == "TOPICCODE") {
    return(.dil_social_pick(
      c("Abuse/Neglect/Violence", "Access to non-NDIS service",
        "Child Protection", "Community Inclusion\u2014Social/Family",
        "Disability services complaints", "Discrimination/rights",
        "Education", "Employment", "Equipment/aids", "Finances",
        "Government payments", "Health/Mental Health",
        "Housing/Homelessness", "Legal/Access to Justice",
        "NDIS\u2014Internal Review", "NDIS\u2014Access/Planning",
        "NDIS\u2014Support implementing plan/Accessing services", "Other",
        "Physical access", "Transport", "Vulnerable/isolated"),
      spine_rows, seed, "DEX NDAP topic"
    ))
  }
  if (upper %in% names(assistance_columns)) {
    index <- unname(assistance_columns[[upper]])
    return(as.integer(
      assistance$primary == index | assistance$secondary == index
    ))
  }
  if (upper == "PRIMARYASSISTANCENEEDEDKEY") return(assistance$primary)
  if (upper == "ASSISTANCENEEDEDKEY") return(assistance$secondary)
  if (upper == "REFERRALTOKEY") {
    return(1L + as.integer((key %/% 23L) %% 13L))
  }
  if (upper %in% c("REFERRALTOEXTERNALKEY", "REFERRALTOINTERNALKEY")) {
    offset <- if (upper == "REFERRALTOEXTERNALKEY") 29L else 31L
    value <- 1L + as.integer((key %/% offset) %% 13L)
    value[key %% 5L == 0L] <- NA_integer_
    return(value)
  }

  outcome <- .dil_dex_outcome(spine_rows, seed, table_name)
  if (upper == "OUTCOMETYPE") return(outcome$type)
  if (upper == "OUTCOMEDOMAIN") return(outcome$domain)
  if (upper == "OUTCOMEDOMAINSCORECODE") {
    phase <- 1L + as.integer((key %/% 37L) %% 5L)
    post <- key %% 2L == 1L
    score <- pmin(5L, phase + as.integer(post & key %% 4L != 0L))
    return(paste0(outcome$domain_code, score))
  }
  # SDJOINT is the code-label pair published in the current bulk-upload spec.
  if (upper == "ASSESSEDBY") return(rep("SCORE directly \u2013 joint", n))
  if (upper == "ASSESSEDBYCODE") return(rep("SDJOINT", n))
  phase <- ifelse(key %% 3L == 0L, "Post", "Pre")
  if (upper == "ASSESSMENTTYPE") return(phase)
  if (upper == "ASSESSMENTTYPECODE") return(toupper(phase))

  if (upper == "ANCESTRYCODE") {
    indigenous <- if ("indigenous" %in% names(spine_rows)) {
      as.integer(spine_rows$indigenous)
    } else {
      rep(1L, n)
    }
    return(ifelse(indigenous %in% 2:4, "1102", "1101"))
  }
  if (upper == "MAINLANGUAGECODE") {
    return(.dil_social_language_value(
      spine_rows, seed, "DEX main language|ASCL 2016"
    ))
  }
  if (upper %in% c("CLIENTLOCALITY", "OUTLETLOCALITY")) {
    return(.dil_social_locality_value(spine_rows, seed))
  }
  if (upper %in% c("STE2016BOUNDARYCODE", "STE2021BOUNDARYCODE")) {
    return(as.character(.dil_social_state(spine_rows, key)))
  }
  if (upper %in% c("LGA2016BOUNDARYCODE", "LGA2021BOUNDARYCODE")) {
    return(.dil_admin_geography_value(upper, spine_rows, seed, period))
  }
  if (upper == "FED2022BOUNDARYCODE") {
    return(.dil_social_ced_value(spine_rows, seed, "CED|2021 boundaries"))
  }
  if (upper == "ACPR2015BOUNDARYCODE") {
    return(.dil_social_acpr_value(spine_rows, seed, 2015L))
  }
  if (upper == "ACPR2018BOUNDARYCODE") {
    return(.dil_social_acpr_value(spine_rows, seed, 2018L))
  }
  id_specs <- list(
    ACTIVITYID = c("ACT", "8", "250"),
    ASSESSMENTID = c("ASM", "10", ""),
    ATTENDANCEID = c("ATT", "10", ""),
    DELIVERYORGID = c("ORG", "6", "400"),
    GPSORGANISATIONID = c("GPSORG", "6", "400"),
    GPSPROGRAMID = c("GPSPRG", "5", "80"),
    LEADORGID = c("LEAD", "5", "100"),
    ORGANISATIONACTIVITYID = c("ORGA", "8", ""),
    ORGANISATIONID = c("ORG", "6", "400"),
    OUTLETACTIVITYID = c("OUTACT", "8", ""),
    OUTLETID = c("OUT", "7", "1200"),
    PROGRAMID = c("PRG", "5", "80"),
    SESSIONID = c("SES", "10", "")
  )
  if (upper %in% names(id_specs)) {
    spec <- id_specs[[upper]]
    groups <- if (nzchar(spec[[3L]])) as.integer(spec[[3L]]) else NULL
    return(.dil_social_id(
      spec[[1L]], spine_rows, seed, paste("DEX", upper, sep = "|"),
      as.integer(spec[[2L]]), groups
    ))
  }
  if (upper == "SERVICETYPEID") {
    return(101L + as.integer(key %% 8L))
  }
  if (upper == "RECORDSEQNBR") {
    return(1L + as.integer((key %/% 43L) %% 3L))
  }
  if (upper == "SOURCESYSTEMCODE") {
    return(ifelse(key %% 10L < 8L, "FOFMS", "EXTNL"))
  }
  calendar <- .dil_dex_calendar(spine_rows, seed, period)
  if (upper == "THEDAY") return(calendar$day)
  if (upper == "THEDAYNAME") return(calendar$day_name)
  if (upper == "THEDAYOFWEEK") return(calendar$day_of_week)
  if (upper == "THEISOWEEK") return(calendar$iso_week)
  if (upper == "THEMONTHNAME") return(calendar$month_name)
  if (upper == "THEWEEK") return(calendar$week)
  if (upper == "THEREPORTINGPERIOD") return(calendar$reporting_period)
  if (upper == "REPORTINGPERIODENDTIME") return(calendar$reporting_end)

  NULL
}

.dil_domino_source_value <- function(name, description, source_frame,
                                     spine_rows, seed, period,
                                     product_name = "", table_name = "",
                                     module_name = "") {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_social_key(spine_rows, seed, "DOMINO canonical values")
  direct <- .dil_source_column(source_frame, upper)
  if (!is.null(direct) && any(!is.na(direct))) return(direct)
  if (.dil_social_is_unsupported_codeframe("DOMINO", upper)) {
    return(rep(NA_character_, n))
  }

  alias <- switch(
    upper,
    CNTRY_CDE = "CTRY_CODE",
    EMP_INC_VAR_HRS = "EMP_INC_CONT_HRS",
    NULL
  )
  if (!is.null(alias)) {
    value <- .dil_source_column(source_frame, alias)
    if (!is.null(value) && any(!is.na(value))) return(value)
  }

  if (upper == "DURN_DAYS") {
    return(30L + as.integer(key %% 3650L))
  }
  if (upper %in% c("EMP_INC_CONT_HRS", "EMP_INC_VAR_HRS",
                   "INCAP_WK_WRK_HRS")) {
    hours <- if ("baseline_hours" %in% names(spine_rows)) {
      pmax(as.numeric(spine_rows$baseline_hours), 0) * 2
    } else if ("baseline_income" %in% names(spine_rows)) {
      pmax(as.numeric(spine_rows$baseline_income), 0) / 1250
    } else {
      as.numeric(key %% 81L)
    }
    maximum <- if (upper == "INCAP_WK_WRK_HRS") 30 else 80
    return(as.integer(round(pmin(hours, maximum))))
  }
  # DOMINO location products use four-digit SACC country codes. The synthetic
  # population currently has an Australian residential address.
  if (upper %in% c("CTRY_CODE", "CNTRY_CDE")) return(rep("1101", n))
  if (upper == "LGA") {
    return(.dil_admin_geography_value(upper, spine_rows, seed, period))
  }
  if (upper == "CED") {
    return(.dil_social_ced_value(spine_rows, seed, "DOMINO CED|2021"))
  }

  if (upper == "BIRTH_CTRY_CODE") {
    if ("country_of_birth_sacc" %in% names(spine_rows)) {
      value <- as.integer(spine_rows$country_of_birth_sacc)
      value[is.na(value)] <- 1101L
      return(sprintf("%04d", value))
    }
    return(rep("1101", n))
  }
  if (upper == "LANG_CODE") {
    return(.dil_social_language_value(
      spine_rows, seed, "DOMINO preferred language|ASCL 2016"
    ))
  }
  if (upper == "INDIG_CODE") {
    indigenous <- if ("indigenous" %in% names(spine_rows)) {
      as.integer(spine_rows$indigenous)
    } else {
      rep(1L, n)
    }
    return(ifelse(indigenous %in% 2:4, "Y", "N"))
  }
  if (upper == "OBJECT_TYPE_CODE") return(rep("PER", n))

  id_specs <- list(
    AMR = c("R", "6"), ASSMT_ID = c("A", "8"), CHILD_ID = c("C", "10"),
    EMPLYR_ID = c("E", "8"), EUED_ID = c("EUED", "8"),
    EUEA_ID = c("EUEA", "8"), EXT_DOM_REL_ID = c("R", "12"),
    THP_ID = c("THP", "8")
  )
  if (upper %in% names(id_specs)) {
    spec <- id_specs[[upper]]
    return(.dil_social_id(
      spec[[1L]], spine_rows, seed, paste("DOMINO", upper, sep = "|"),
      as.integer(spec[[2L]])
    ))
  }

  NULL
}

.dil_sae_source_value <- function(name, description, source_frame,
                                  spine_rows, seed, period,
                                  product_name = "", table_name = "",
                                  module_name = "") {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  direct <- .dil_source_column(source_frame, upper)
  if (!is.null(direct) && any(!is.na(direct))) return(direct)
  if (.dil_social_is_unsupported_codeframe("SAE", upper)) {
    return(rep(NA_character_, n))
  }
  if (upper == "EXTRACT_REF") {
    return(rep(sprintf(
      "FY%04d-%02d", as.integer(period$start_year),
      as.integer(period$end_year) %% 100L
    ), n))
  }
  NULL
}

.dil_rps_source_value <- function(name, description, source_frame,
                                  spine_rows, seed, period,
                                  product_name = "", table_name = "",
                                  module_name = "") {
  upper <- toupper(name)
  direct <- .dil_source_column(source_frame, upper)
  if (!is.null(direct) && any(!is.na(direct))) return(direct)
  if (.dil_social_is_unsupported_codeframe("RPS", upper)) {
    return(rep(NA_character_, nrow(spine_rows)))
  }
  if (upper %in% c("LGA_CODE_2023", "LGA_CODE_2024")) {
    return(.dil_admin_geography_value(upper, spine_rows, seed, period))
  }
  NULL
}

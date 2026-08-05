# Source-backed and coherent fallback values for canonical NDIS tables.
#
# Exact non-missing columns from the Rust generator take precedence. These
# rules are used when a canonical NDIS table has no native product source, or
# when the sparse participant source cannot be aligned to the bounded spine
# pool. The rules intentionally preserve six unresolved/structural fields as
# typed missing; see `.dil_ndis_value_status()`.
#
# Official domain sources used here:
# - participant datasets and downloadable data rules:
#   https://dataresearch.ndis.gov.au/datasets/participant-datasets
# - plan-management data rules:
#   https://dataresearch.ndis.gov.au/media/4267/download?attachment=
# - participant-goal data rules:
#   https://dataresearch.ndis.gov.au/media/2169/download?attachment=
# - CALD data rules:
#   https://dataresearch.ndis.gov.au/media/4258/download?attachment=
# - baseline-outcomes data and rules:
#   https://dataresearch.ndis.gov.au/media/4223/download?attachment=
#   https://dataresearch.ndis.gov.au/media/4275/download?attachment=
# - NDIS Pricing Schedule 2026-27:
#   https://ndis.gov.au/media/8703/download?attachment=

.dil_ndis_structural_variables <- c(
  "DECIND",
  "GOALTOLEAVEIND",
  "OTHERDSBLTY",
  "POSTRACHSGSUPP",
  "RSDSINMMMCD",
  "VISATYPE"
)

.dil_ndis_supported_gap_variables <- c(
  "ABNPRMRYPRVDRIND", "ACSRQSTDCSNADJ", "ACSRQSTDCSNRSN",
  "ACTIVEPLANIND", "ACTVPRTCPNTIND", "ANSDTLDESC", "ANSWER",
  "ANSWERTEXT", "APLCTNSUBSTSDESC", "BUSINESSSYSTEM", "CALDSTS",
  "CAPACITY_BUILDING", "CAPITAL", "CHNLDESC", "CLAIMTYPDESC",
  "CNTRYOFBRTHCD", "COHORT", "COHORT_ADJ", "CORE",
  "CRNTAPRVDPLANIND", "CSNIND", "CURRYPIRACIND",
  "DISPLAYONPRVDRLISTIND", "DSBLTYIND", "EARLYINTRVTNIND",
  "ENTRYQRTR", "EVERACTVPRVDRINDADJ", "EVEREARLYINTRVTNIND",
  "EVERELIGBLIND", "EVERINELIGBLIND", "EVERYPIRACIND",
  "EXITRSNDESC", "FORM", "FY_CLAIM", "GNDRTYP", "GOAL_CAT", "GOAL_PERIOD",
  "GOALSEQNMBR", "GOALSTSDESC", "ICDDSBLTYNM",
  "JRSDCTNBEINGINVCDCD", "LANGSPKNATHOMENM", "LCTN_ASMPTN",
  "LEGALENTYTYPNM", "LGANM2021", "LTSTPLANSILIND", "MGTTYPDESC",
  "NDIAAGEBND", "NDISDSBLTYGRPNM", "NDISMMMSCD", "NXTSTPSDESC",
  "OTCMDESC", "OTCMRSNDESC", "PLAN_TYPE", "PLANINCLDSDAIND",
  "PLANMGTMTHDDESC", "PLANTIMING", "PLANTIMING_ADJ",
  "PLANTIMING_N", "PLANTIMING_N_ADJ", "PMTRQSTPRVDR_BN", "POSTCD",
  "PRSNCNTCTRLTNSHPTYP", "PRTCPNTPTHWYSTGNM", "PRTCPNTPTHWYSTPNM",
  "PRVDR_BN", "PRVDRSTS", "QUESTION", "QUESTIONTEXT", "RACD2011",
  "RACIND", "REMOTENESS_DESCRIPTION_MMM", "RGSTRTNGRPSTSDESC",
  "RGSTRTNSTSDESC", "RLTNDESC", "RLTNSHPCNTCTID", "RMTNS_ASMPTN",
  "RPRTNGACSENTRYTYP", "RPRTNGCHRT", "RQSTDCSN_PREV",
  "RQSTNMBR_PREV", "RSDSINSRVCDSTRCTNM", "RSDSINSRVCDSTRCTNM_COAG",
  "SCHMENTRYPHASENM", "SPRSADRSONPRVDRLSTIND", "STSCHNGDTM_PREV",
  "STSDESC", "SUPPCATNM", "SUPPCATNMSFC", "SUPPCLASS",
  "SUPPITEMDESC", "SVRTYSCR", "TF_AWAITEV_TTL", "TRIALPRTCPNTIND",
  "TYPCD", "TYPDESC", "UNITPRICE", "WTHDRWLRSNDESC"
)

.dil_ndis_value_status <- function(name) {
  upper <- toupper(name)
  if (upper %in% .dil_ndis_structural_variables) {
    return("structural_or_unsupported")
  }
  if (upper %in% .dil_ndis_supported_gap_variables) return("supported")
  "outside_original_gap_register"
}

.dil_ndis_pick <- function(values, key) {
  values[1L + as.integer(key %% length(values))]
}

.dil_ndis_spine_integer <- function(spine_rows, name, fallback = NA_integer_) {
  n <- nrow(spine_rows)
  if (!name %in% names(spine_rows)) return(rep(as.integer(fallback), n))
  value <- suppressWarnings(as.integer(spine_rows[[name]]))
  value[!is.finite(value)] <- as.integer(fallback)
  value
}

.dil_ndis_state <- function(spine_rows, key) {
  state <- if ("state" %in% names(spine_rows)) {
    suppressWarnings(as.integer(spine_rows$state))
  } else if ("state_code" %in% names(spine_rows)) {
    suppressWarnings(as.integer(spine_rows$state_code))
  } else {
    rep(NA_integer_, nrow(spine_rows))
  }
  invalid <- is.na(state) | !state %in% 1:8
  state[invalid] <- 1L + as.integer(key[invalid] %% 8L)
  state
}

.dil_ndis_state_label <- function(state) {
  c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")[state]
}

.dil_ndis_age <- function(spine_rows, period) {
  birth_year <- .dil_ndis_spine_integer(spine_rows, "birth_year", 1980L)
  pmax(0L, as.integer(period$end_year) - birth_year)
}

.dil_ndis_age_band <- function(age, reference_year) {
  if (as.integer(reference_year) >= 2024L) {
    ifelse(
      age <= 8L, "0 to 8",
      ifelse(age <= 14L, "9 to 14",
             ifelse(age <= 18L, "15 to 18",
                    ifelse(age <= 24L, "19 to 24",
                           ifelse(age <= 34L, "25 to 34",
                                  ifelse(age <= 44L, "35 to 44",
                                         ifelse(age <= 54L, "45 to 54",
                                                ifelse(age <= 64L, "55 to 64", "65+")))))))
    )
  } else {
    ifelse(
      age <= 6L, "0 to 6",
      ifelse(age <= 14L, "7 to 14",
             ifelse(age <= 18L, "15 to 18",
                    ifelse(age <= 24L, "19 to 24",
                           ifelse(age <= 34L, "25 to 34",
                                  ifelse(age <= 44L, "35 to 44",
                                         ifelse(age <= 54L, "45 to 54",
                                                ifelse(age <= 64L, "55 to 64", "65+")))))))
    )
  }
}

.dil_ndis_disability_group <- function(person_type) {
  value <- rep("Other", length(person_type))
  value[person_type %in% c(2L, 3L, 4L, 21L, 27L)] <-
    "Psychosocial disability"
  value[person_type == 10L] <- "Multiple Sclerosis"
  value[person_type == 11L] <- "Hearing Impairment"
  value[person_type == 12L] <- "Visual Impairment"
  value[person_type == 13L] <- "Spinal Cord Injury"
  value[person_type == 14L] <- "Stroke"
  value[person_type %in% c(9L, 15L, 16L)] <- "Other Neurological"
  value[person_type == 23L] <- "Intellectual Disability"
  value[person_type == 24L] <- "Autism"
  value[person_type == 26L] <- "Other Sensory/Speech"
  value[person_type %in% c(1L, 5:8, 17:20, 22L, 25L)] <- "Other Physical"
  value
}

.dil_ndis_icd_name <- function(person_type) {
  values <- c(
    "Chronic back pain", "Depression and anxiety", "Schizophrenia",
    "Bipolar affective disorder", "Type 2 diabetes mellitus",
    "Type 1 diabetes mellitus", "Chronic obstructive pulmonary disease",
    "Rheumatoid arthritis", "Epilepsy", "Multiple sclerosis",
    "Sensorineural hearing loss", "Visual impairment", "Spinal cord injury",
    "Stroke / acquired brain injury", "Parkinson's disease", "Dementia",
    "Upper limb impairment", "Lower limb impairment",
    "Cardiovascular disease", "Chronic kidney disease",
    "Post-traumatic stress disorder", "Carpal tunnel syndrome / RSI",
    "Intellectual disability", "Autism spectrum disorder", "Fibromyalgia",
    "Speech and language disorder", "Social and behavioural disorder"
  )
  valid <- !is.na(person_type) & person_type >= 1L & person_type <= length(values)
  out <- rep("Not stated", length(person_type))
  out[valid] <- values[person_type[valid]]
  out
}

.dil_ndis_support_category <- function(key) {
  .dil_ndis_pick(c(
    "Assistance with Daily Life", "Transport", "Consumables",
    "Assistance with Social & Community Participation",
    "Coordination of Supports", "Improved Living Arrangements",
    "Increased Social & Community Participation", "Finding & Keeping a Job",
    "Improved Relationships", "Improved Health & Wellbeing",
    "Improved Learning", "Improved Life Choices",
    "Improved Daily Living Skills", "Assistive Technology",
    "Home Modifications", "Specialist Disability Accommodation"
  ), key)
}

.dil_ndis_support_class <- function(category) {
  ifelse(
    category %in% c(
      "Assistance with Daily Life", "Transport", "Consumables",
      "Assistance with Social & Community Participation"
    ),
    "Core",
    ifelse(
      category %in% c(
        "Assistive Technology", "Home Modifications",
        "Specialist Disability Accommodation"
      ),
      "Capital", "Capacity Building"
    )
  )
}

.dil_ndis_support_item <- function(category) {
  map <- c(
    "Assistance with Daily Life" =
      "Assistance With Self-Care Activities - Standard - Weekday Daytime",
    "Transport" = "Activity Based Transport",
    "Consumables" = "Low Cost Disability-related Health Consumables",
    "Assistance with Social & Community Participation" =
      "Access Community Social and Rec Activ - Standard - Weekday Daytime",
    "Coordination of Supports" = "Level 2: Coordination of Supports",
    "Improved Living Arrangements" =
      "Assistance With Accommodation And Tenancy Obligations",
    "Increased Social & Community Participation" =
      "Skills Development And Training",
    "Finding & Keeping a Job" = "Employment Assistance",
    "Improved Relationships" = "Individual Social Skills Development",
    "Improved Health & Wellbeing" = "Exercise Physiology",
    "Improved Learning" = "Transition Through School And To Further Education",
    "Improved Life Choices" =
      "Capacity Building and Training in Self-Management and Plan Management",
    "Improved Daily Living Skills" = "Skills Development And Training",
    "Assistive Technology" = "Low Cost Assistive Technology",
    "Home Modifications" = "Home Modification",
    "Specialist Disability Accommodation" =
      "Unplanned onsite shared supports in Specialist Disability Accommodation"
  )
  unname(map[category])
}

.dil_ndis_unit_price <- function(category) {
  prices <- c(
    "Assistance with Daily Life" = 73.58,
    "Transport" = 1.00,
    "Consumables" = 1.00,
    "Assistance with Social & Community Participation" = 73.58,
    "Coordination of Supports" = 100.14,
    "Improved Living Arrangements" = 83.87,
    "Increased Social & Community Participation" = 83.87,
    "Finding & Keeping a Job" = 83.87,
    "Improved Relationships" = 83.87,
    "Improved Health & Wellbeing" = 166.99,
    "Improved Learning" = 83.87,
    "Improved Life Choices" = 83.87,
    "Improved Daily Living Skills" = 83.87,
    "Assistive Technology" = 1.00,
    "Home Modifications" = 1.00,
    "Specialist Disability Accommodation" = 1616.26
  )
  unname(prices[category])
}

.dil_ndis_service_district <- function(state, key) {
  districts <- list(
    c("Sydney", "Western Sydney", "Hunter New England", "Northern NSW"),
    c("Barwon", "Western Melbourne", "Goulburn", "Outer East Melbourne"),
    c("Brisbane", "Cairns", "Townsville", "Toowoomba"),
    c("Northern Adelaide", "Southern Adelaide", "Limestone Coast"),
    c("North Metro", "South Metro", "Kimberley-Pilbara", "South West"),
    c("TAS North", "TAS North West", "TAS South East", "TAS South West"),
    c("Darwin Urban", "Darwin Remote", "Central Australia", "Katherine"),
    c("ACT")
  )
  vapply(seq_along(state), function(i) {
    values <- districts[[state[[i]]]]
    values[[1L + as.integer(key[[i]] %% length(values))]]
  }, character(1))
}

.dil_ndis_outcome_form <- function(table_name, age, key) {
  family <- grepl("familyoutcomes", tolower(table_name), fixed = TRUE)
  qa_map <- grepl("qa_maps", tolower(table_name), fixed = TRUE)
  family <- family | (qa_map & key %% 3L == 0L)
  ifelse(
    family,
    ifelse(age <= 14L, "Family/carer of participant 0 to 14",
           "Family/carer of participant 15 and over"),
    ifelse(
      age < 5L, "Participant 0 to before school",
      ifelse(age <= 14L, "Participant starting school to 14",
             ifelse(age <= 24L, "Participant 15 to 24",
                    "Participant 25 and over"))
    )
  )
}

.dil_ndis_outcome_question <- function(form, key) {
  participant_early <- c(
    "% with concerns in 6 or more developmental areas",
    "% able to tell their family what they want",
    "% who can make friends with people outside the family",
    "% who participate in age-appropriate community activities",
    "% welcomed or actively included in community activities"
  )
  participant_school <- c(
    "% developing functional, learning and coping skills",
    "% becoming more independent",
    "% who have a genuine say in decisions about themselves",
    "% who can make friends with people outside the family",
    "% who spend time after school or on weekends with friends",
    "% welcomed or actively included in community activities",
    "% who spend time with friends without an adult present",
    "% attending school in a mainstream class"
  )
  participant_15_24 <- c(
    "% happy with their level of independence and control",
    "% who choose who supports them",
    "% who choose what they do each day",
    "% offered the opportunity to join a self-advocacy group",
    "% who want more choice and control in their life",
    "% with no friends other than family or paid staff",
    "% actively involved in a community, cultural or religious group",
    "% who are happy with their home",
    "% who feel safe or very safe in their home",
    "% who rate their health as good, very good or excellent",
    "% who did not have difficulties accessing health services",
    "% who attended school in a mainstream class",
    "% who have a paid job", "% who volunteer"
  )
  participant_25_plus <- c(
    "% who choose who supports them",
    "% who choose what they do each day",
    "% offered the opportunity to join a self-advocacy group",
    "% who want more choice and control in their life",
    "% with no friends other than family or paid staff",
    "% actively involved in a community, cultural or religious group",
    "% who are happy with their home",
    "% who feel safe or very safe in their home",
    "% who rate their health as good, very good or excellent",
    "% who did not have difficulties accessing health services",
    "% who participate in education, training or skill development",
    "% who participate in mainstream education or training",
    "% unable to undertake desired education or training",
    "% who have a paid job", "% who volunteer"
  )
  family <- c(
    "% receiving Carer Payment", "% receiving Carer Allowance",
    "% working in a paid job", "% in permanent employment",
    "% working 15 hours or more",
    "% able to work as much as they want",
    "% able to advocate for their family member",
    "% who see friends and family as often as they like",
    "% confident in supporting their family member",
    "% who feel in control selecting services",
    "% who rate their health as good, very good or excellent",
    "% reporting sufficient flexibility in work arrangements"
  )
  vapply(seq_along(form), function(i) {
    values <- switch(
      form[[i]],
      "Participant 0 to before school" = participant_early,
      "Participant starting school to 14" = participant_school,
      "Participant 15 to 24" = participant_15_24,
      "Participant 25 and over" = participant_25_plus,
      family
    )
    values[[1L + as.integer(key[[i]] %% length(values))]]
  }, character(1))
}

.dil_ndis_outcome_question_count <- function(form) {
  unname(c(
    "Participant 0 to before school" = 5L,
    "Participant starting school to 14" = 8L,
    "Participant 15 to 24" = 14L,
    "Participant 25 and over" = 15L,
    "Family/carer of participant 0 to 14" = 12L,
    "Family/carer of participant 15 and over" = 12L
  )[form])
}

.dil_ndis_table_quarter <- function(table_name, period) {
  parts <- regmatches(
    tolower(table_name),
    regexec("_((?:19|20)[0-9]{2})_([1-4])$", tolower(table_name), perl = TRUE)
  )[[1L]]
  if (length(parts)) {
    return(c(year = as.integer(parts[[2L]]), quarter = as.integer(parts[[3L]])))
  }
  c(year = as.integer(period$end_year), quarter = 4L)
}

.dil_ndis_source_value <- function(name, description, source_frame,
                                   spine_rows, seed, period,
                                   product_name = "", table_name = "",
                                   module_name = "") {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  if (!n) return(character())
  if (upper == "FY_CLAIM") {
    quarter <- .dil_ndis_table_quarter(table_name, period)
    financial_year <- quarter[["year"]] + as.integer(quarter[["quarter"]] >= 3L)
    return(rep(as.integer(financial_year), n))
  }
  if (upper %in% .dil_ndis_structural_variables) {
    return(rep(NA_character_, n))
  }

  key <- .dil_numeric_key(
    spine_rows, seed,
    .stable_name_seed(paste("NDIS", product_name, table_name, sep = "|"))
  )
  state <- .dil_ndis_state(spine_rows, key)
  state_label <- .dil_ndis_state_label(state)
  age <- .dil_ndis_age(spine_rows, period)
  person_type <- .dil_ndis_spine_integer(spine_rows, "person_type", 1L)
  severity <- .dil_ndis_spine_integer(spine_rows, "disability_severity", 3L)
  indigenous <- .dil_ndis_spine_integer(spine_rows, "indigenous", 1L)
  sacc <- .dil_ndis_spine_integer(spine_rows, "country_of_birth_sacc", 1101L)
  active <- key %% 10L != 0L
  early <- age < 9L
  severe <- severity %in% c(1L, 2L)
  residential_aged_care <- age < 65L & severe & key %% 100L < 4L

  language <- ifelse(
    sacc == 1101L,
    "English",
    .dil_ndis_pick(c("English", "Mandarin", "Arabic", "Vietnamese", "Punjabi"), key)
  )
  excluded_cald_country <- sacc %in% c(
    1101L, 1201L, 2100L, 2102L, 2103L, 2104L, 2105L, 2106L, 2107L,
    2201L, 8102L, 8104L, 9225L
  )
  cald <- !(excluded_cald_country & language == "English") &
    !indigenous %in% 2:4
  disability_group <- .dil_ndis_disability_group(person_type)
  mmm <- 1L + as.integer(key %% 7L)
  remoteness <- ifelse(
    mmm == 1L, "City",
    ifelse(mmm <= 5L, "Rural", ifelse(mmm == 6L, "Remote", "Very remote"))
  )
  support_category <- .dil_source_alias(
    source_frame, c("SUPPCATNM", "PYMTRQSTSUPPCATNM")
  )
  if (is.null(support_category)) {
    support_category <- .dil_ndis_support_category(key)
  }
  support_class <- .dil_ndis_support_class(support_category)

  if (upper == "GNDRTYP") {
    sex <- .dil_ndis_spine_integer(spine_rows, "sex", 1L)
    return(ifelse(sex == 2L, "F", "M"))
  }
  if (upper == "CNTRYOFBRTHCD") return(sprintf("%04d", sacc))
  if (upper == "LANGSPKNATHOMENM") return(language)
  if (upper == "CALDSTS") return(ifelse(cald, "Y", "N"))
  if (upper == "NDISDSBLTYGRPNM") return(disability_group)
  if (upper == "ICDDSBLTYNM") return(.dil_ndis_icd_name(person_type))
  if (upper == "SVRTYSCR") {
    centre <- c(`1` = 0.85, `2` = 0.65, `3` = 0.45, `4` = 0.28)
    value <- unname(centre[as.character(pmin(pmax(severity, 1L), 4L))])
    return(round(pmin(pmax(value + ((key %% 141L) - 70L) / 1000, 0), 1), 3))
  }
  if (upper == "ACTVPRTCPNTIND") return(ifelse(active, "Y", "N"))
  if (upper == "EVERELIGBLIND") return(rep("Y", n))
  if (upper == "EVERINELIGBLIND") return(ifelse(key %% 25L == 0L, "Y", "N"))
  if (upper == "CSNIND") return(ifelse(severe & key %% 5L == 0L, "Y", "N"))
  if (upper == "CURRYPIRACIND") return(ifelse(residential_aged_care, "Y", "N"))
  if (upper == "EVERYPIRACIND") {
    return(ifelse(residential_aged_care | (age < 65L & severe & key %% 50L < 4L),
                  "Y", "N"))
  }
  if (upper == "RACIND") return(ifelse(residential_aged_care, "Y", "N"))
  if (upper == "NDIAAGEBND") {
    age_2019 <- pmax(0L, 2019L - .dil_ndis_spine_integer(
      spine_rows, "birth_year", 1980L
    ))
    return(.dil_ndis_age_band(age_2019, 2019L))
  }
  if (upper == "POSTCD") {
    ranges <- list(c(2000L, 2999L), c(3000L, 3999L), c(4000L, 4999L),
                   c(5000L, 5799L), c(6000L, 6797L), c(7000L, 7799L),
                   c(800L, 899L), c(2600L, 2618L))
    return(vapply(seq_len(n), function(i) {
      range <- ranges[[state[[i]]]]
      sprintf("%04d", range[[1L]] + as.integer(key[[i]] %%
        (range[[2L]] - range[[1L]] + 1L)))
    }, character(1)))
  }
  if (upper == "NDISMMMSCD") return(as.character(mmm))
  if (upper == "REMOTENESS_DESCRIPTION_MMM") return(remoteness)
  if (upper == "RACD2011") {
    ra <- ifelse(mmm == 1L, 0L, ifelse(mmm <= 3L, 1L,
      ifelse(mmm <= 5L, 2L, ifelse(mmm == 6L, 3L, 4L))))
    return(as.character(state * 10L + ra))
  }
  if (upper %in% c("RSDSINSRVCDSTRCTNM", "RSDSINSRVCDSTRCTNM_COAG")) {
    return(.dil_ndis_service_district(state, key))
  }
  if (upper == "LGANM2021") {
    return(.dil_admin_geography_value(upper, spine_rows, seed, period))
  }

  if (upper == "DSBLTYIND") return(ifelse(key %% 100L < 18L, "Y", "N"))
  if (upper == "PRSNCNTCTRLTNSHPTYP") {
    return(ifelse(age < 18L, "Parent",
      .dil_ndis_pick(c("Parent", "Spouse/partner", "Sibling", "Other family member"), key)))
  }
  if (upper == "RLTNSHPCNTCTID") {
    return(.dil_character_id("CNT", spine_rows, seed,
      .stable_name_seed("NDIS contact"), width = 10L))
  }

  access_status <- ifelse(
    key %% 100L < 91L, "Access met",
    ifelse(key %% 100L < 97L, "Access not met", "Withdrawn")
  )
  if (upper == "ACSRQSTDCSNADJ") return(access_status)
  if (upper == "ACSRQSTDCSNRSN") {
    return(ifelse(access_status == "Access met", "Meets access criteria",
      ifelse(access_status == "Access not met",
             "Does not meet disability requirements", "Application withdrawn")))
  }
  if (upper == "APLCTNSUBSTSDESC") return(access_status)
  if (upper %in% c("EARLYINTRVTNIND", "EVEREARLYINTRVTNIND")) {
    return(ifelse(early, "Y", "N"))
  }
  if (upper == "EXITRSNDESC") {
    return(ifelse(active, NA_character_, .dil_ndis_pick(c(
      "Participant requested exit", "No longer meets disability requirements",
      "Deceased", "Moved overseas"
    ), key)))
  }
  if (upper == "JRSDCTNBEINGINVCDCD") return(state_label)
  if (upper == "PRTCPNTPTHWYSTGNM") {
    return(ifelse(access_status != "Access met", "Access",
                  ifelse(active, "Plan implementation", "Exit")))
  }
  if (upper == "PRTCPNTPTHWYSTPNM") {
    return(ifelse(access_status == "Access not met", "Access decision - not met",
      ifelse(access_status == "Withdrawn", "Access request withdrawn",
             ifelse(active, "Approved plan in effect", "Participant exited"))))
  }
  if (upper == "RPRTNGACSENTRYTYP") {
    return(.dil_ndis_pick(c("new", "new", "new", "existing", "commonwealth"), key))
  }
  trial <- key %% 20L == 0L
  if (upper == "TRIALPRTCPNTIND") return(ifelse(trial, "Y", "N"))
  if (upper == "RPRTNGCHRT") return(ifelse(trial, "Trial", "Full Scheme"))
  if (upper == "SCHMENTRYPHASENM") {
    return(ifelse(access_status != "Access met", "Access",
                  ifelse(active, "Approved plan", "Exited")))
  }
  if (upper == "TF_AWAITEV_TTL") return(as.character(key %% 91L))

  goal_category <- .dil_ndis_pick(c(
    "Choice and control over my life", "Daily life", "Health and wellbeing",
    "Learning", "Relationships", "Social and community activities",
    "Where I live", "Work"
  ), key)
  if (upper %in% c("ANSDTLDESC", "GOAL_CAT")) return(goal_category)
  if (upper == "GOALSEQNMBR") return(as.character(1L + key %% 5L))
  if (upper == "GOALSTSDESC") return(ifelse(key %% 10L < 8L, "Active", "Inactive"))
  if (upper == "GOAL_PERIOD") {
    return(.dil_ndis_pick(c("Short term", "Medium term", "Long term"), key))
  }
  goal_class <- ifelse(
    goal_category == "Daily life", "Core",
    ifelse(goal_category == "Where I live", "Capital", "Capacity Building")
  )
  if (upper == "CORE") return(ifelse(goal_class == "Core", "Y", "N"))
  if (upper == "CAPACITY_BUILDING") {
    return(ifelse(goal_class == "Capacity Building", "Y", "N"))
  }
  if (upper == "CAPITAL") return(ifelse(goal_class == "Capital", "Y", "N"))
  if (upper == "NXTSTPSDESC") {
    return(.dil_ndis_pick(c(
      "Identify supports and actions", "Begin agreed supports",
      "Review progress with support coordinator", "Continue agreed supports"
    ), key))
  }

  if (upper == "CHNLDESC") {
    return(.dil_ndis_pick(c("Phone", "Email", "Letter", "my NDIS portal"), key))
  }
  review_status <- .dil_ndis_pick(c(
    "Initiated", "Open", "On hold", "Approved", "Rejected", "Withdrawn",
    "Completed"
  ), key)
  if (upper == "STSDESC") return(review_status)
  if (upper == "OTCMDESC") {
    return(ifelse(review_status %in% c("Initiated", "Open", "On hold"),
                  "Pending", review_status))
  }
  if (upper == "OTCMRSNDESC") {
    return(ifelse(review_status == "Approved", "Changed support needs",
      ifelse(review_status == "Rejected", "Insufficient evidence",
        ifelse(review_status == "Withdrawn", "Participant withdrew request",
               "Decision pending or completed"))))
  }
  if (upper == "RQSTDCSN_PREV") {
    return(.dil_ndis_pick(c("Approved", "Rejected", "Withdrawn"), key))
  }
  if (upper == "RQSTNMBR_PREV") {
    return(.dil_character_id("S48", spine_rows, seed,
      .stable_name_seed("NDIS previous S48 request"), width = 9L))
  }
  if (upper == "STSCHNGDTM_PREV") {
    offset <- as.integer(key %% 365L)
    return(format(period$end - offset, "%Y-%m-%d"))
  }
  review_type <- ifelse(key %% 4L == 0L, "S48_VARIATION", "S48_REASSESSMENT")
  if (upper == "TYPCD") return(review_type)
  if (upper == "TYPDESC") {
    return(ifelse(review_type == "S48_VARIATION",
                  "Participant-requested plan variation",
                  "Participant-requested plan reassessment"))
  }
  if (upper == "WTHDRWLRSNDESC") {
    return(ifelse(review_status == "Withdrawn",
                  .dil_ndis_pick(c(
                    "Request no longer required", "Circumstances changed",
                    "Participant withdrew request"
                  ), key), NA_character_))
  }

  if (upper %in% c("ACTIVEPLANIND", "CRNTAPRVDPLANIND")) {
    return(ifelse(active, "Y", "N"))
  }
  if (upper == "LTSTPLANSILIND") return(ifelse(severe & key %% 5L == 0L, "Y", "N"))
  if (upper == "PLANINCLDSDAIND") return(ifelse(severe & key %% 12L == 0L, "Y", "N"))
  if (upper == "PLANMGTMTHDDESC") {
    return(.dil_ndis_pick(c(
      "Agency Managed", "Agency Managed", "Plan Managed",
      "Plan Managed", "Self Managed Fully", "Self Managed Partly",
      "Not recorded"
    ), key))
  }
  if (upper == "PLAN_TYPE") {
    return(.dil_ndis_pick(c(
      "Initial plan", "Result of a scheduled review",
      "Result of an unscheduled review"
    ), key))
  }
  if (upper %in% c("SUPPCATNM", "SUPPCATNMSFC")) return(support_category)
  if (upper == "SUPPCLASS") return(support_class)

  if (upper == "LCTN_ASMPTN") {
    return(ifelse(remoteness %in% c("Remote", "Very remote"),
                  remoteness, "National"))
  }
  if (upper == "RMTNS_ASMPTN") {
    return(ifelse(remoteness == "Very remote", "very-remote",
                  ifelse(remoteness == "Remote", "remote", "non-remote")))
  }
  if (upper == "SUPPITEMDESC") return(.dil_ndis_support_item(support_category))
  if (upper == "UNITPRICE") {
    price <- .dil_ndis_unit_price(support_category)
    multiplier <- ifelse(remoteness == "Remote", 1.40,
                         ifelse(remoteness == "Very remote", 1.50, 1.00))
    return(sprintf("%.2f", price * multiplier))
  }

  if (upper == "CLAIMTYPDESC") {
    return(.dil_ndis_pick(c(
      "Standard", "Standard", "Standard", "Provider travel", "Cancellation"
    ), key))
  }
  if (upper == "MGTTYPDESC") {
    return(.dil_ndis_pick(c("agency", "agency", "plan", "plan", "self"), key))
  }
  if (upper %in% c("PMTRQSTPRVDR_BN", "PRVDR_BN")) {
    value <- .dil_source_alias(source_frame, c("PRVDR_BN", "PMTRQSTPRVDR_BN", "BN"))
    if (!is.null(value)) return(as.character(value))
    # The central identifier rule generates deterministic 11-digit values;
    # provider ABNs normally come from the Rust source, where the checksum is
    # validated. Return NULL if no provider source exists rather than inventing
    # a second ABN implementation here.
    return(NULL)
  }

  if (upper == "ABNPRMRYPRVDRIND") return(rep("1", n))
  if (upper == "DISPLAYONPRVDRLISTIND") return(ifelse(key %% 20L == 0L, "N", "Y"))
  if (upper == "EVERACTVPRVDRINDADJ") return(rep("Y", n))
  if (upper == "SPRSADRSONPRVDRLSTIND") return(ifelse(key %% 20L == 0L, "Y", "N"))
  if (upper == "LEGALENTYTYPNM") {
    return(.dil_ndis_pick(c(
      "Australian Private Company", "Individual/Sole Trader",
      "Incorporated Association", "Other Incorporated Entity"
    ), key))
  }
  if (upper == "PRVDRSTS") return(ifelse(key %% 20L == 0L, "Inactive", "Active"))
  if (upper %in% c("RGSTRTNSTSDESC", "RGSTRTNGRPSTSDESC")) {
    return(ifelse(key %% 25L == 0L, "Revoked", "Approved"))
  }
  if (upper == "RLTNDESC") {
    return(rep(if (as.integer(period$end_year) >= 2023L) {
      "Endorsed provider relationship"
    } else {
      "Service booking"
    }, n))
  }

  if (upper == "BUSINESSSYSTEM") {
    return(rep(if (as.integer(period$end_year) >= 2023L) {
      "PACE"
    } else {
      "SAP CRM"
    }, n))
  }
  form <- .dil_ndis_outcome_form(table_name, age, key)
  question_text <- .dil_ndis_outcome_question(form, key)
  question_number <- 1L + as.integer(
    key %% .dil_ndis_outcome_question_count(form)
  )
  answer_code <- ifelse(key %% 20L == 0L, "3", ifelse(key %% 100L < 68L, "1", "2"))
  if (upper == "FORM") return(form)
  if (upper == "QUESTION") return(as.character(question_number))
  if (upper == "QUESTIONTEXT") return(question_text)
  if (upper == "ANSWER") return(answer_code)
  if (upper == "ANSWERTEXT") {
    return(c(`1` = "Yes", `2` = "No", `3` = "Not applicable")[answer_code])
  }
  review_number <- as.integer(key %% 4L)
  timing <- c("Baseline", "First review", "Second review", "Third review")[
    review_number + 1L
  ]
  cohort <- c(
    "Baseline only", "Baseline and first review", "Baseline to second review",
    "Baseline to third review"
  )[review_number + 1L]
  if (upper %in% c("COHORT", "COHORT_ADJ")) return(cohort)
  if (upper %in% c("PLANTIMING", "PLANTIMING_ADJ")) return(timing)
  if (upper %in% c("PLANTIMING_N", "PLANTIMING_N_ADJ")) {
    return(as.character(review_number))
  }
  if (upper == "ENTRYQRTR") {
    quarter <- .dil_ndis_table_quarter(table_name, period)
    return(sprintf("%dQ%d", quarter[["year"]] - review_number,
                   quarter[["quarter"]]))
  }

  NULL
}

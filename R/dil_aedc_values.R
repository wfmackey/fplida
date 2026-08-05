# AEDC-specific values for canonical DIL tables.

.dil_aedc_source <- function(source_frame, name, n, mode = "integer") {
  value <- .dil_source_alias(source_frame, name)
  if (is.null(value)) {
    return(switch(
      mode,
      character = rep(NA_character_, n),
      numeric = rep(NA_real_, n),
      rep(NA_integer_, n)
    ))
  }
  value <- rep_len(value, n)
  switch(
    mode,
    character = as.character(value),
    numeric = suppressWarnings(as.numeric(value)),
    suppressWarnings(as.integer(value))
  )
}

.dil_aedc_year <- function(source_frame, n, period) {
  year <- .dil_aedc_source(source_frame, "YEAR", n)
  year[is.na(year)] <- as.integer(period$end_year)
  year
}

.dil_aedc_na_code <- function(year) {
  # The 2019 dictionary uses 999. The 2022 and 2025 dictionaries use 99.
  ifelse(year <= 2018L, 999L, 99L)
}

.dil_aedc_key <- function(spine_rows, seed, salt) {
  .dil_numeric_key(
    spine_rows,
    seed,
    .stable_name_seed(paste("AEDC", salt, sep = "|"))
  )
}

.dil_aedc_state <- function(source_frame, spine_rows) {
  n <- nrow(spine_rows)
  if ("state" %in% names(spine_rows)) {
    state <- suppressWarnings(as.integer(spine_rows$state))
  } else {
    state_name <- .dil_aedc_source(
      source_frame, "STATE", n, mode = "character"
    )
    state <- match(
      state_name,
      c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
    )
  }
  state[is.na(state) | !state %in% 1:8] <- 1L
  state
}

.dil_aedc_structural_value <- function(name, source_frame, spine_rows, seed) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  if (upper == "MOC") {
    draw <- .dil_aedc_key(spine_rows, seed, "completion month") %% 100L
    return(ifelse(
      draw < 5L, 4L,
      ifelse(draw < 38L, 5L,
             ifelse(draw < 73L, 6L,
                    ifelse(draw < 91L, 7L,
                           ifelse(draw < 98L, 8L, 9L))))
    ))
  }

  id_names <- c(
    "AGCAMPUSID", "COMMUNITYID", "JCAMPUSID", "LOCALCOMMUNITYID",
    "LOCATIONAGEID", "REGIONID", "SCHOOLAGEID", "SCHOOLID",
    "TEACHERID"
  )
  if (!upper %in% id_names) return(NULL)

  state <- .dil_aedc_state(source_frame, spine_rows)
  school_key <- .dil_aedc_key(spine_rows, seed, "school grouping")
  place_key <- .dil_aedc_key(spine_rows, seed, "community grouping")
  teacher_key <- .dil_aedc_key(spine_rows, seed, "teacher grouping")
  school <- state * 1000L + as.integer(school_key %% 40L)
  teacher <- school * 10L + as.integer(teacher_key %% 4L)
  region <- state * 100L + as.integer(place_key %% 4L)
  community <- state * 1000L + as.integer(place_key %% 14L)
  local_community <- state * 10000L + as.integer(place_key %% 32L)
  value <- switch(
    upper,
    AGCAMPUSID = sprintf("AGC%07d", school),
    JCAMPUSID = sprintf("JCA%07d", school),
    LOCATIONAGEID = sprintf("LOC%07d", school),
    SCHOOLAGEID = sprintf("SAG%07d", school),
    SCHOOLID = sprintf("SCH%07d", school),
    TEACHERID = sprintf("TCH%08d", teacher),
    REGIONID = sprintf("REG%05d", region),
    COMMUNITYID = sprintf("COM%06d", community),
    LOCALCOMMUNITYID = sprintf("LCO%07d", local_community)
  )
  rep_len(value, n)
}

.dil_aedc_age_value <- function(name, source_frame, n) {
  upper <- toupper(name)
  if (!upper %in% c("AGECAT", "AGEGROUP")) return(NULL)
  age <- .dil_aedc_source(source_frame, "AGEINMONTHS", n)
  if (upper == "AGEGROUP") {
    out <- ifelse(
      age < 60L, "<5",
      ifelse(age < 72L, "5", ifelse(age < 84L, "6", ">6"))
    )
    out[is.na(age)] <- NA_character_
    return(out)
  }
  cut_points <- c(44L, 46L, 49L, 52L, 55L, 58L, 61L, 64L,
                  67L, 70L, 73L, 76L, 79L, 82L, 90L)
  out <- findInterval(age, cut_points)
  out[is.na(age)] <- NA_integer_
  as.integer(out)
}

.dil_aedc_absence_value <- function(name, source_frame, spine_rows, seed) {
  upper <- toupper(name)
  names <- c("A1Z", "A1AZ", "A1BZ", "A1CZ", "A1DZ")
  if (!upper %in% names) return(NULL)
  n <- nrow(spine_rows)
  band <- .dil_aedc_source(source_frame, "A1", n)
  reasons <- lapply(
    c("A1A", "A1B", "A1C", "A1D"),
    .dil_aedc_source,
    source_frame = source_frame,
    n = n
  )
  key <- .dil_aedc_key(spine_rows, seed, "raw absence values")
  any_reason <- Reduce(`|`, lapply(reasons, function(value) value == 1L))
  any_reason[is.na(any_reason)] <- FALSE
  raw_band <- band
  first_band <- band == 1L
  raw_band[first_band] <- as.integer(any_reason[first_band] |
                                      key[first_band] %% 2L == 1L)
  if (upper == "A1Z") return(as.integer(raw_band))

  index <- match(upper, names[-1L])
  indicator <- reasons[[index]]
  out <- rep(NA_integer_, n)
  out[indicator == 0L & !is.na(indicator)] <- 0L
  selected <- indicator == 1L & !is.na(indicator)
  out[selected] <- 1L + as.integer(
    (key[selected] + index * 17L) %% 5L
  )
  out
}

.dil_aedc_indigenous_language_value <- function(name, source_frame,
                                                 spine_rows, seed, year) {
  upper <- toupper(name)
  fields <- paste0("B1", LETTERS[1:4])
  if (!upper %in% fields) return(NULL)
  n <- nrow(spine_rows)
  atsi_type <- .dil_aedc_source(source_frame, "ATSITYPE", n)
  state <- .dil_aedc_source(source_frame, "STATE", n, "character")
  lang <- .dil_aedc_source(source_frame, "LANG", n)
  asked <- atsi_type %in% 1:3
  asked[state == "NSW" & year <= 2012L] <- FALSE
  if (upper == "B1C") asked <- asked & lang == 1L
  asked[is.na(asked)] <- FALSE
  key <- .dil_aedc_key(spine_rows, seed, paste("indigenous language", upper))
  draw <- key %% 1000L
  out <- rep(NA_integer_, n)
  out[asked & draw < 15L] <- 88L
  out[asked & draw >= 15L & draw < 35L] <- .dil_aedc_na_code(
    year[asked & draw >= 15L & draw < 35L]
  )
  scored <- asked & draw >= 35L
  out[scored] <- 1L + as.integer((key[scored] %/% 37L) %% 3L)
  out
}

.dil_aedc_consult_value <- function(name, source_frame, spine_rows, seed) {
  upper <- toupper(name)
  if (!upper %in% c("CONSULT", "CONSULTROLE")) return(NULL)
  n <- nrow(spine_rows)
  atsi_type <- .dil_aedc_source(source_frame, "ATSITYPE", n)
  key <- .dil_aedc_key(spine_rows, seed, "cultural consultation")
  asked <- atsi_type %in% 1:3
  consult <- as.integer(asked & key %% 100L < 32L)
  consult[is.na(atsi_type)] <- NA_integer_
  if (upper == "CONSULT") return(consult)
  role <- rep(NA_integer_, n)
  selected <- consult == 1L & !is.na(consult)
  role[selected] <- 1L + as.integer((key[selected] %/% 101L) %% 4L)
  role
}

.dil_aedc_emerging_state <- function(source_frame, spine_rows, seed, year) {
  n <- nrow(spine_rows)
  special <- .dil_aedc_source(source_frame, "SPECIALNEEDS", n)
  key <- .dil_aedc_key(spine_rows, seed, "emerging needs")
  conditions <- matrix(0L, nrow = n, ncol = 9L)
  for (index in seq_len(9L)) {
    draw <- (key + index * 73L) %% 1000L
    unknown <- draw >= 990L
    present <- draw < ifelse(special == 1L, 105L, 22L)
    if (all(year <= 2012L, na.rm = TRUE)) {
      conditions[, index] <- ifelse(present, 3L, 0L)
    } else {
      conditions[, index] <- ifelse(
        present,
        ifelse((key + index) %% 4L == 0L, 2L, 1L),
        0L
      )
    }
    conditions[unknown, index] <- 88L
  }
  d10_draw <- (key %/% 17L) %% 1000L
  d10 <- ifelse(
    d10_draw >= 990L,
    88L,
    as.integer(d10_draw < ifelse(special == 1L, 360L, 55L))
  )
  condition_names <- c(
    "D10AY", "D10CY", "D10DY", "D10FYA", "D10GY", "D10HY",
    "D10IY", "D10JY", "D10KY", "D10LY", "D10MY", "D10NY",
    "D10OY", "D10PY", "D10QY", "D10RY", "D10SY", "D10TY",
    "D10UY", "D10VY", "D10WY", "D10XY", "D10YY", "D10ZY",
    "D10AAY", "D10ABY", "D10ACY", "D10ADY"
  )
  condition_index <- 1L + as.integer((key %/% 1009L) %% length(condition_names))
  names(condition_index) <- NULL
  any_condition <- rowSums(
    conditions >= 1L & conditions <= 3L,
    na.rm = TRUE
  ) > 0L |
    d10 == 1L
  list(
    conditions = conditions,
    d10 = as.integer(d10),
    condition_names = condition_names,
    condition_index = condition_index,
    any_condition = any_condition,
    special = special,
    key = key
  )
}

.dil_aedc_emerging_value <- function(name, source_frame, spine_rows, seed,
                                     year) {
  upper <- toupper(name)
  emerging_names <- c(
    paste0("D", 1:11), "D10A", "D10B", "D10C",
    "DEVDIFF", "E1",
    "D10AY", "D10CY", "D10DY", "D10FYA", "D10GY", "D10HY",
    "D10IY", "D10JY", "D10KY", "D10LY", "D10MY", "D10NY",
    "D10OY", "D10PY", "D10QY", "D10RY", "D10SY", "D10TY",
    "D10UY", "D10VY", "D10WY", "D10XY", "D10YY", "D10ZY",
    "D10AAY", "D10ABY", "D10ACY", "D10ADY"
  )
  if (!upper %in% emerging_names) return(NULL)
  state <- .dil_aedc_emerging_state(
    source_frame, spine_rows, seed, year
  )
  n <- nrow(spine_rows)
  if (grepl("^D[1-9]$", upper)) {
    return(state$conditions[, as.integer(sub("D", "", upper))])
  }
  if (upper == "D10") return(state$d10)
  if (upper %in% c("D10A", "D10B", "D10C")) {
    group <- match(upper, c("D10A", "D10B", "D10C"))
    selected_group <- 1L + (state$condition_index - 1L) %% 3L
    out <- ifelse(state$d10 == 1L & selected_group == group, 1L, 0L)
    out[state$d10 == 88L] <- 88L
    out[is.na(state$d10)] <- NA_integer_
    return(as.integer(out))
  }
  if (upper %in% state$condition_names) {
    index <- match(upper, state$condition_names)
    out <- as.integer(state$d10 == 1L & state$condition_index == index)
    out[state$d10 == 88L | is.na(state$d10)] <- NA_integer_
    return(out)
  }
  if (upper == "D11") {
    draw <- (state$key %/% 29L) %% 100L
    out <- ifelse(
      draw >= 98L,
      88L,
      as.integer(draw < ifelse(state$any_condition, 62L, 8L))
    )
    return(as.integer(out))
  }
  if (upper == "DEVDIFF") return(as.integer(state$any_condition))
  if (upper == "E1") {
    draw <- (state$key %/% 43L) %% 100L
    return(as.integer(ifelse(
      draw >= 97L,
      88L,
      draw < ifelse(state$any_condition | state$special == 1L, 46L, 9L)
    )))
  }
  rep(NA_integer_, n)
}

.dil_aedc_diagnosis_value <- function(name, source_frame, spine_rows, seed,
                                      year) {
  upper <- toupper(name)
  if (!grepl("^DIAGNOSIS(?:[0-9]+|6A)$", upper)) return(NULL)
  n <- nrow(spine_rows)
  special <- .dil_aedc_source(source_frame, "SPECIALNEEDS", n)
  common <- c(paste0("DIAGNOSIS", c(1L, 3L, 4L, 7:21)))
  available <- if (all(year <= 2015L, na.rm = TRUE)) {
    c(common, "DIAGNOSIS2", "DIAGNOSIS5", "DIAGNOSIS6")
  } else {
    c(common, "DIAGNOSIS6A", paste0("DIAGNOSIS", 22:32))
  }
  if (!upper %in% available) return(rep(NA_integer_, n))
  key <- .dil_aedc_key(spine_rows, seed, "special-needs diagnoses")
  first <- 1L + as.integer(key %% length(available))
  second <- 1L + as.integer((key %/% 37L) %% length(available))
  selected <- first == match(upper, available) |
    (key %% 5L == 0L & second == match(upper, available))
  out <- as.integer(special == 1L & selected)
  out[is.na(special)] <- NA_integer_
  out
}

.dil_aedc_language_source_value <- function(name, source_frame, spine_rows,
                                            seed) {
  upper <- toupper(name)
  fields <- paste0("LANGSOURCE", 0:5)
  if (!upper %in% fields) return(NULL)
  n <- nrow(spine_rows)
  lang <- .dil_aedc_source(source_frame, "LANG", n)
  can_com <- .dil_aedc_source(source_frame, "CANCOM", n)
  key <- .dil_aedc_key(spine_rows, seed, "language information sources")
  index <- match(upper, fields) - 1L
  asked <- lang == 1L & !is.na(can_com)
  out <- rep(NA_integer_, n)
  out[asked] <- 1L
  yes <- asked & ((key %/% (index + 2L) + index * 19L) %% 100L <
                    c(56L, 62L, 86L, 28L, 12L, 18L)[index + 1L])
  out[yes] <- 2L
  out[asked & index != 2L & key %% 41L == index] <- 0L
  out
}

.dil_aedc_parent_education_value <- function(name, spine_rows, seed) {
  upper <- toupper(name)
  fields <- c(
    "PARENT1SCHOOL", "PARENT1POSTSCHOOL",
    "PARENT2SCHOOL", "PARENT2POSTSCHOOL"
  )
  if (!upper %in% fields) return(NULL)
  key <- .dil_aedc_key(spine_rows, seed, paste("parent education", upper))
  draw <- key %% 100L
  if (grepl("SCHOOL$", upper)) {
    out <- ifelse(
      draw < 45L, 1L,
      ifelse(draw < 58L, 2L,
             ifelse(draw < 81L, 3L, ifelse(draw < 94L, 4L, 88L)))
    )
  } else {
    out <- ifelse(
      draw < 32L, 1L,
      ifelse(draw < 48L, 2L,
             ifelse(draw < 76L, 3L, ifelse(draw < 94L, 4L, 88L)))
    )
  }
  as.integer(out)
}

.dil_aedc_care_value <- function(name, source_frame, n) {
  upper <- toupper(name)
  fields <- c("DAYCARE", "DAYCARENO", "PRESCHOOL", "PSDC")
  if (!upper %in% fields) return(NULL)
  preschool <- .dil_aedc_source(source_frame, "E2Y", n)
  daycare <- .dil_aedc_source(source_frame, "E3AY", n)
  preschool_yes <- preschool == 1L
  preschool_no <- preschool == 2L
  daycare_yes <- daycare %in% 1:3
  daycare_no <- daycare == 4L
  unknown_preschool <- preschool == 88L | is.na(preschool)
  unknown_daycare <- daycare == 88L | is.na(daycare)
  if (upper == "PRESCHOOL") {
    out <- ifelse(preschool_yes, 1L, ifelse(preschool_no, 0L, NA_integer_))
    return(as.integer(out))
  }
  if (upper == "DAYCARE") {
    out <- ifelse(daycare_yes, 1L, ifelse(daycare_no, 0L, 88L))
    out[is.na(daycare)] <- NA_integer_
    return(as.integer(out))
  }
  if (upper == "DAYCARENO") {
    out <- ifelse(
      unknown_daycare | unknown_preschool,
      88L,
      as.integer(daycare_yes & preschool_no)
    )
    out[is.na(daycare) & is.na(preschool)] <- NA_integer_
    return(as.integer(out))
  }
  out <- ifelse(
    preschool_yes | daycare_yes,
    1L,
    ifelse(preschool_no & daycare_no, 0L, 88L)
  )
  out[is.na(preschool) & is.na(daycare)] <- NA_integer_
  as.integer(out)
}

.dil_aedc_on_track_value <- function(name, source_frame, n) {
  upper <- toupper(name)
  fields <- c(paste0("ONTRACK", 0:5), paste0("OT", 1:4))
  if (!upper %in% fields) return(NULL)
  categories <- vapply(
    c(
      "PHYSCATEGORY", "SOCCATEGORY", "EMOTCATEGORY",
      "LANGCOGCATEGORY", "COMGENCATEGORY"
    ),
    function(field) .dil_aedc_source(source_frame, field, n),
    integer(n)
  )
  complete <- rowSums(is.na(categories)) == 0L
  count <- rowSums(categories >= 3L, na.rm = TRUE)
  threshold <- as.integer(sub("^(?:ONTRACK|OT)", "", upper))
  out <- if (upper == "ONTRACK0") {
    as.integer(count == 0L)
  } else {
    as.integer(count >= threshold)
  }
  out[!complete] <- NA_integer_
  out
}

.dil_aedc_blocked_names <- c(
  paste0("CONSULTTYPE", 1:13),
  "COUNTRY", "PLACEOFBIRTH", "PARENT1COUNTRY", "REFUGEESTATUS",
  "COMMUNITY", "LOCALCOMMUNITY", "REGION",
  "GCCSACODE", "GCCSANAME", "IARECODE", "IARENAME", "IAREPUBLIC",
  "ILOCCODE", "ILOCNAME", "ILOCPUBLIC", "RDACODE", "RDANAME",
  "SA2NAME", "SA3NAME", "SA4NAME", "STATEELECTORATE",
  "STATEELECTORATECODE", "LCARIACODE", "LCARIANAME", "SEIFARANK",
  "LANGUAGEID", "ILANGUAGEID", paste0("OTHERLANGUAGEID", 1:4),
  paste0("OTHERILANGUAGEID", 1:4),
  "CPROFILE", "CPUBLIC", "LCMAPPABLE", "LCPROFILE", "LCPUBLIC",
  "LCABSSEIFACATEGORY", "LCABSERP", "LCABSMOVED", "LCABSUNEMPLOYED",
  "LCABSYEAR12", "LCABSYSPARENTS",
  "COMGEN_1", paste0("EMOT_", 1:4), paste0("EMOT_", 1:4, "_VULN"),
  paste0("LANGCOG_", 1:4), paste0("LANGCOG_", 1:4, "_VULN"),
  "MSI", "MSICATEGORY", "MSIVALID",
  paste0("PHYS_", 1:3), paste0("PHYS_", 1:3, "_VULN"),
  paste0("SOC_", 1:4), paste0("SOC_", 1:4, "_VULN"),
  "SCHOOLCLUSTER", "SCHOOLREGION", "SCHOOLSUBURB"
)

.dil_aedc_source_value <- function(name, description, source_frame,
                                   spine_rows, seed, period,
                                   product_name = "", table_name = "") {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  year <- .dil_aedc_year(source_frame, n, period)

  value <- .dil_aedc_structural_value(
    upper, source_frame, spine_rows, seed
  )
  if (!is.null(value)) return(value)
  value <- .dil_aedc_age_value(upper, source_frame, n)
  if (!is.null(value)) return(value)
  value <- .dil_aedc_absence_value(
    upper, source_frame, spine_rows, seed
  )
  if (!is.null(value)) return(value)
  value <- .dil_aedc_indigenous_language_value(
    upper, source_frame, spine_rows, seed, year
  )
  if (!is.null(value)) return(value)
  value <- .dil_aedc_consult_value(
    upper, source_frame, spine_rows, seed
  )
  if (!is.null(value)) return(value)
  value <- .dil_aedc_emerging_value(
    upper, source_frame, spine_rows, seed, year
  )
  if (!is.null(value)) return(value)
  value <- .dil_aedc_diagnosis_value(
    upper, source_frame, spine_rows, seed, year
  )
  if (!is.null(value)) return(value)
  value <- .dil_aedc_language_source_value(
    upper, source_frame, spine_rows, seed
  )
  if (!is.null(value)) return(value)
  value <- .dil_aedc_parent_education_value(upper, spine_rows, seed)
  if (!is.null(value)) return(value)
  value <- .dil_aedc_care_value(upper, source_frame, n)
  if (!is.null(value)) return(value)
  value <- .dil_aedc_on_track_value(upper, source_frame, n)
  if (!is.null(value)) return(value)

  if (upper == "SEIFAEXCLUDED") {
    seifa <- .dil_aedc_source(source_frame, "SEIFADECILE", n)
    return(as.integer(is.na(seifa)))
  }
  if (upper %in% .dil_aedc_blocked_names) {
    return(rep(NA_character_, n))
  }
  NULL
}

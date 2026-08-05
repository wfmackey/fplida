# ACLD detailed-microdata codeframes ---------------------------------------

.acld_codeframe_registry <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) return(cache)
    registry_path <- function(file) {
      path <- system.file("extdata", "codeframes", file, package = "fplida")
      if (!nzchar(path)) path <- file.path("inst", "extdata", "codeframes", file)
      path
    }
    mapping <- utils::read.csv(
      registry_path("acld-variable-codeframes.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    values <- utils::read.csv(
      registry_path("acld-codeframe-values.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )
    values$display_order <- as.integer(values$display_order)
    cache <<- list(mapping = mapping, values = values)
    cache
  }
})

.acld_field_spec <- function(variable) {
  upper <- toupper(variable)
  year_hit <- regmatches(
    upper,
    regexec("_(11|16|21)$", upper, perl = TRUE)
  )[[1L]]
  year <- if (length(year_hit)) {
    switch(year_hit[[2L]], `11` = 2011L, `16` = 2016L, `21` = 2021L)
  } else {
    NA_integer_
  }
  role_hit <- regmatches(
    upper,
    regexec("_(FP|MP|SP|S|P)_(?:11|16|21)$", upper, perl = TRUE)
  )[[1L]]
  role <- if (length(role_hit)) role_hit[[2L]] else "P"
  base <- sub("_(FP|MP|SP|S|P)_(11|16|21)$", "", upper, perl = TRUE)
  base <- sub("_(11|16|21)$", "", base, perl = TRUE)
  list(
    variable = upper,
    year = year,
    role = role,
    base = base,
    concept = sub("_(11|16|21)$", "", upper, perl = TRUE)
  )
}

.acld_text_salt <- function(text) {
  ints <- utf8ToInt(enc2utf8(text))
  if (!length(ints)) return(0)
  sum(as.double(ints) * seq_along(ints)) %% 1000003
}

.acld_key <- function(row_id, seed, concept, extra = 0) {
  (
    as.double(row_id) * 1000003 + as.double(seed) * 9176 +
      .acld_text_salt(concept) * 104729 + as.double(extra) * 13007
  ) %% 999999937
}

.acld_status_code <- function(frame_values, kind) {
  hit <- frame_values$code[frame_values$value_kind == kind]
  if (length(hit)) hit[[1L]] else NA_character_
}

.acld_manual_variables <- c(
  "LEVEL", "SYNTHETIC_AEUID", "WEIGHT4_11_16",
  "WEIGHT4_11_16_21", "WEIGHT4_16_21"
)

.acld_public_census_bases <- c(
  "ANC1P", "ANC2P", "BPLP", "BPFPR", "BPMPR", "LANP",
  "INDP", "INDP_1DIG", "INDP_2DIG", "INDP_3DIG", "OCCP",
  "QALFP", "YARP", "EETP", "LFSF", "CLTHP", "HLTHP", "IFNMFD"
)

.acld_public_asgs_2021_bases <- c(
  "IAREA_UR", "RA_UR", "MBUCD", "MBUCP", "SA1UCD", "SA1UCP",
  "SA2UCD", "SA2UCP", "POWP", "POWP_SA2"
)

.acld_unsupported_decision <- function(variable) {
  spec <- .acld_field_spec(variable)
  if (spec$variable %in% .acld_manual_variables) return("existing_manual")
  if (spec$base %in% .acld_public_census_bases) {
    return("public_census_classification")
  }
  if (spec$base == "LGA_UR" ||
      (!is.na(spec$year) && spec$year == 2021L &&
       spec$base %in% .acld_public_asgs_2021_bases)) {
    return("public_asgs_geography")
  }
  "unresolved"
}

.acld_public_registry <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    registry_path <- function(file, codeframes = TRUE) {
      parts <- if (codeframes) c("extdata", "codeframes", file) else {
        c("extdata", file)
      }
      path <- do.call(system.file, c(as.list(parts), list(package = "fplida")))
      if (!nzchar(path)) path <- do.call(file.path, as.list(c("inst", parts)))
      path
    }
    census <- utils::read.csv(
      registry_path("census-codeframe-values.csv"),
      stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"
    )
    census$year <- as.integer(census$year)
    sacc <- utils::read.delim(
      registry_path("sacc_country.tsv"), stringsAsFactors = FALSE,
      check.names = FALSE, colClasses = "character"
    )
    language <- utils::read.delim(
      registry_path("ascl_language.tsv"), stringsAsFactors = FALSE,
      check.names = FALSE, colClasses = "character"
    )
    anzsic <- utils::read.delim(
      registry_path("anzsic2006.tsv"), stringsAsFactors = FALSE,
      check.names = FALSE, colClasses = "character"
    )
    geography <- utils::read.delim(
      registry_path("census-geography-values.tsv"),
      stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"
    )
    geography$year <- as.integer(geography$year)
    lga <- utils::read.delim(
      registry_path("lga.tsv"), stringsAsFactors = FALSE,
      check.names = FALSE, colClasses = "character"
    )
    lga$year <- as.integer(lga$year)
    meshblock <- utils::read.csv(
      gzfile(registry_path("mb_lookup.csv.gz", codeframes = FALSE)),
      stringsAsFactors = FALSE, check.names = FALSE,
      colClasses = "character"
    )
    cache <<- list(
      census = census, sacc = sacc, language = language,
      anzsic = anzsic, geography = geography, lga = lga,
      meshblock = meshblock
    )
    cache
  }
})

.acld_public_census_frame <- function(spec) {
  variable <- switch(
    spec$base,
    BPFPR = "BPFP",
    BPMPR = "BPMP",
    spec$base
  )
  year <- spec$year
  if (variable %in% c("ANC1P", "ANC2P") && year == 2011L) year <- 2016L
  if (variable %in% c("QALFP", "EETP", "LFSF", "CLTHP", "HLTHP", "IFNMFD")) {
    year <- 2021L
  }
  values <- .acld_public_registry()$census
  values[
    values$year == year & toupper(values$variable) == variable,
    c("code", "label", "value_kind"), drop = FALSE
  ]
}

.acld_public_status_code <- function(original_frame, public_frame, kind) {
  code <- .acld_status_code(original_frame, kind)
  if (!is.na(code)) return(code)
  .acld_status_code(public_frame, kind)
}

.acld_pick_by_state <- function(values, state, key) {
  out <- rep(NA_character_, length(key))
  for (state_value in 1:8) {
    rows <- which(state == state_value)
    if (!length(rows)) next
    choices <- values[values$state == as.character(state_value), , drop = FALSE]
    if (!nrow(choices)) next
    out[rows] <- choices$code[1L + as.integer(key[rows] %% nrow(choices))]
  }
  out
}

.acld_public_substantive <- function(frame, width = NULL) {
  values <- frame$code[frame$value_kind == "substantive"]
  values <- unique(values[!is.na(values) & nzchar(values)])
  if (!is.null(width)) values <- values[nchar(values) == width]
  values
}

.acld_public_direct_value <- function(spec, role_spine, public_frame, key,
                                      not_applicable) {
  n <- length(key)
  age <- pmin(pmax(spec$year - role_spine$birth_year, 0L), 115L)
  registry <- .acld_public_registry()

  if (spec$base %in% c("ANC1P", "ANC2P")) {
    patterns <- c(
      "^Australian$", "^English$", "^Irish$", "^Scottish$", "Chinese",
      "Indian", "Italian", "German", "Aboriginal", "Torres Strait"
    )
    codes <- vapply(
      patterns,
      function(pattern) .dil_census_label_code(public_frame, pattern),
      character(1)
    )
    keep <- !is.na(codes)
    out <- .dil_census_weighted_pick(
      key, codes[keep], c(35, 30, 8, 7, 6, 4, 4, 3, 2, 1)[keep]
    )
    if (spec$base == "ANC1P") {
      aboriginal <- .dil_census_label_code(public_frame, "Aboriginal")
      torres <- .dil_census_label_code(public_frame, "Torres Strait")
      if (!is.na(aboriginal)) {
        out[role_spine$indigenous %in% c(2L, 4L)] <- aboriginal
      }
      if (!is.na(torres)) out[role_spine$indigenous == 3L] <- torres
    } else if (!is.na(not_applicable)) {
      out[key %% 100L < 35L] <- not_applicable
    }
    return(out)
  }

  if (spec$base %in% c("BPLP", "BPFPR", "BPMPR")) {
    code <- as.character(role_spine$country_of_birth_sacc)
    valid <- code %in% registry$sacc$code
    fallback <- .dil_census_weighted_pick(
      key, registry$sacc$code, as.numeric(registry$sacc$weight)
    )
    code[!valid] <- fallback[!valid]
    return(code)
  }

  if (spec$base == "LANP") {
    language <- registry$language
    substantive <- !grepl("^[@&V]+$", language$code)
    language <- language[substantive, , drop = FALSE]
    other <- language$code[language$code != "1201"]
    other_weight <- as.numeric(language$weight[language$code != "1201"])
    selected <- .dil_census_weighted_pick(key, other, other_weight)
    australia <- role_spine$country_of_birth_sacc == 1101L
    english <- ifelse(australia, key %% 100L < 94L, key %% 100L < 45L)
    selected[english | is.na(english)] <- "1201"
    return(selected)
  }

  if (spec$base %in% c("INDP", "INDP_1DIG", "INDP_2DIG", "INDP_3DIG")) {
    industry <- pmin(pmax(as.integer(role_spine$industry), 1L), 19L)
    division <- LETTERS[industry]
    class_code <- rep(NA_character_, n)
    for (letter in LETTERS[1:19]) {
      rows <- which(division == letter)
      if (!length(rows)) next
      choices <- registry$anzsic$anzsic_class_code[
        registry$anzsic$anzsic_division_code == letter
      ]
      class_code[rows] <- choices[1L + as.integer(key[rows] %% length(choices))]
    }
    out <- switch(
      spec$base,
      INDP = class_code,
      INDP_1DIG = as.character(industry),
      INDP_2DIG = substr(class_code, 1L, 2L),
      INDP_3DIG = substr(class_code, 1L, 3L)
    )
    out[!role_spine$employed] <- not_applicable
    return(out)
  }

  if (spec$base == "OCCP") {
    occupation <- as.integer(role_spine$anzsco_code)
    valid <- !is.na(occupation) & occupation >= 100000L & occupation <= 999999L
    out <- sprintf("%06d", pmin(pmax(occupation, 0L), 999999L))
    out[!role_spine$employed | !valid] <- not_applicable
    return(out)
  }

  if (spec$base == "QALFP") {
    codes <- .acld_public_substantive(public_frame, width = 6L)
    out <- codes[1L + as.integer(key %% length(codes))]
    out[age < 15L | role_spine$education < 2L] <- not_applicable
    return(out)
  }

  if (spec$base == "YARP") {
    year <- as.integer(role_spine$year_of_arrival)
    overseas <- role_spine$country_of_birth_sacc != 1101L
    valid <- overseas & !is.na(year) & year >= 1900L & year <= spec$year
    out <- rep(not_applicable, n)
    out[valid] <- sprintf("%04d", year[valid])
    return(out)
  }

  studying <- age >= 15L & age <= 24L & role_spine$education > 0L &
    key %% 4L < 2L
  if (spec$base == "EETP") {
    out <- ifelse(
      role_spine$employed & studying,
      ifelse(role_spine$hours >= 35L, "11", "14"),
      ifelse(studying, "12", ifelse(role_spine$employed, "13", "41"))
    )
    out[age < 15L] <- not_applicable
    return(out)
  }

  if (spec$base == "LFSF") {
    one_parent <- key %% 10L < 3L
    out <- ifelse(
      one_parent,
      ifelse(role_spine$employed,
        ifelse(role_spine$hours >= 35L, "22", "23"), "26"
      ),
      ifelse(role_spine$employed,
        ifelse(role_spine$hours >= 35L, "01", "07"), "19"
      )
    )
    out[age < 15L] <- not_applicable
    return(out)
  }

  if (spec$base == "CLTHP") {
    severity <- as.integer(role_spine$disability_severity)
    count <- ifelse(is.na(severity) | severity > 4L, 0L,
      pmin(3L, 1L + (4L - severity) %/% 2L)
    )
    return(as.character(count))
  }

  if (spec$base == "HLTHP") {
    severity <- as.integer(role_spine$disability_severity)
    has_condition <- !is.na(severity) & severity <= 4L
    return(ifelse(has_condition, "122", "121"))
  }

  if (spec$base == "IFNMFD") {
    return(ifelse(key %% 1000L < 35L, "2", "1"))
  }

  if (spec$base == "LGA_UR") {
    values <- registry$lga[
      registry$lga$year == spec$year &
        registry$lga$state %in% as.character(1:8),
      , drop = FALSE
    ]
    return(.acld_pick_by_state(values, role_spine$state, key))
  }

  if (spec$year == 2021L && spec$base %in% c("IAREA_UR", "RA_UR")) {
    layer <- if (spec$base == "IAREA_UR") "IARE" else "RA"
    values <- registry$geography[
      registry$geography$year == 2021L &
        registry$geography$layer == layer,
      , drop = FALSE
    ]
    return(.acld_pick_by_state(values, role_spine$state, key))
  }

  if (spec$year == 2021L && spec$base %in%
      c("MBUCD", "MBUCP", "SA1UCD", "SA1UCP")) {
    out <- rep(NA_character_, n)
    for (sa2 in unique(role_spine$sa2_code[!is.na(role_spine$sa2_code)])) {
      rows <- which(role_spine$sa2_code == sa2)
      choices <- registry$meshblock[
        registry$meshblock$sa2_code == as.character(sa2), , drop = FALSE
      ]
      if (!nrow(choices)) next
      chosen <- 1L + as.integer(key[rows] %% nrow(choices))
      out[rows] <- if (spec$base %in% c("MBUCD", "MBUCP")) {
        choices$mb_code[chosen]
      } else {
        choices$sa1_code[chosen]
      }
    }
    if (spec$base == "SA1UCD") {
      out <- substr(out, pmax(1L, nchar(out) - 6L), nchar(out))
    }
    return(out)
  }

  if (spec$year == 2021L && spec$base %in%
      c("SA2UCD", "SA2UCP", "POWP", "POWP_SA2")) {
    out <- sprintf("%09d", as.integer(role_spine$sa2_code))
    if (spec$base %in% c("POWP", "POWP_SA2")) {
      out[!role_spine$employed] <- NA_character_
    }
    return(out)
  }

  NULL
}

.acld_public_value <- function(variable, original_frame, spine, row_id, seed,
                               links) {
  spec <- .acld_field_spec(variable)
  key <- .acld_key(row_id, seed, spec$concept)
  role_spine <- .acld_role_spine(spine, spec, key)
  public_frame <- .acld_public_census_frame(spec)
  not_applicable <- .acld_public_status_code(
    original_frame, public_frame, "not_applicable"
  )
  not_stated <- .acld_public_status_code(
    original_frame, public_frame, "not_stated"
  )
  unlinked <- .acld_public_status_code(
    original_frame, public_frame, "unlinked"
  )
  direct <- .acld_public_direct_value(
    spec, role_spine, public_frame, key, not_applicable
  )
  if (is.null(direct)) return(NULL)

  linked <- if (!is.na(spec$year)) {
    links[[as.character(spec$year)]]
  } else {
    rep(TRUE, length(key))
  }
  age <- spec$year - role_spine$birth_year
  applicable <- rep(TRUE, length(key))
  if (spec$role %in% c("FP", "MP")) {
    threshold <- ifelse(age < 25L, 85, ifelse(age < 45L, 55, 20))
    applicable <- key %% 100L < threshold
  } else if (spec$role %in% c("SP", "S")) {
    threshold <- ifelse(age < 18L, 0, ifelse(age < 25L, 20,
      ifelse(age < 65L, 60, 45)
    ))
    applicable <- key %% 100L < threshold
  }
  if (!is.na(not_applicable)) direct[!applicable] <- not_applicable
  if (!is.na(not_stated)) {
    stated_key <- .acld_key(row_id, seed, spec$concept, extra = 17L)
    direct[linked & applicable & stated_key %% 1000L < 12L] <- not_stated
  }
  direct[!linked] <- if (is.na(unlinked)) NA_character_ else unlinked
  direct
}

.acld_product_links <- function(data, product, row_id, seed) {
  n <- nrow(data)
  if (identical(product, "madipge-acld11-d-persons-11-16-21")) {
    linked_16 <- if ("LINKFLAG_16" %in% names(data)) {
      as.integer(data$LINKFLAG_16) == 1L
    } else {
      .acld_key(row_id, seed, "link_2011_2016") / 999999937 < 0.77
    }
    linked_21 <- linked_16 &
      .acld_key(row_id, seed, "link_2016_2021") / 999999937 < 0.83
  } else {
    linked_16 <- rep(TRUE, n)
    linked_21 <-
      .acld_key(row_id, seed, "link_2016_panel_2021") / 999999937 < 0.83
  }
  list(`2011` = rep(TRUE, n), `2016` = linked_16, `2021` = linked_21)
}

.acld_role_spine <- function(spine, spec, key) {
  birth_year <- as.integer(spine$birth_year)
  sex <- as.integer(spine$sex)
  income <- as.numeric(spine$baseline_income)
  hours <- as.integer(spine$baseline_hours)

  if (spec$role %in% c("FP", "MP")) {
    birth_year <- birth_year - 20L - as.integer(key %% 21)
    sex <- if (spec$role == "FP") rep(2L, length(sex)) else rep(1L, length(sex))
    income <- income * if (spec$role == "FP") 0.82 else 1.08
    hours <- pmax(0L, hours + if (spec$role == "FP") -4L else 2L)
  } else if (spec$role %in% c("SP", "S")) {
    birth_year <- birth_year - 4L + as.integer(key %% 9)
    sex <- ifelse(sex == 1L, 2L, 1L)
    income <- income * 0.92
    hours <- pmax(0L, hours - 2L + as.integer(key %% 5))
  }

  list(
    birth_year = birth_year,
    sex = sex,
    income = income,
    hours = hours,
    indigenous = as.integer(spine$indigenous),
    citizenship = as.integer(spine$citizenship),
    year_of_arrival = as.integer(spine$year_of_arrival),
    employed = as.logical(spine$baseline_employed),
    education = as.integer(spine$education),
    anzsco_major = as.integer(spine$anzsco_major),
    skill_level = as.integer(spine$skill_level),
    industry = as.integer(spine$industry),
    anzsco_code = as.integer(spine$anzsco_code),
    disability_onset_year = as.integer(spine$disability_onset_year),
    disability_severity = as.integer(spine$disability_severity),
    country_of_birth_sacc = as.integer(spine$country_of_birth_sacc),
    state = as.integer(spine$state),
    residence_seed = as.numeric(spine$residence_seed),
    sa2_code = as.integer(spine$sa2_code),
    household_id = as.numeric(spine$household_id)
  )
}

.acld_income_code <- function(income, frame_values) {
  labels <- gsub("[$,]", "", frame_values$label)
  codes <- frame_values$code
  substantive <- frame_values$value_kind == "substantive"
  out <- rep(NA_character_, length(income))

  negative <- which(substantive & grepl("^Negative income", labels))
  nil <- which(substantive & grepl("^Nil income", labels))
  if (length(negative)) out[income < 0] <- codes[negative[[1L]]]
  if (length(nil)) out[income == 0] <- codes[nil[[1L]]]

  for (i in which(substantive)) {
    range <- regmatches(labels[[i]], regexec("\\(([0-9]+)-([0-9]+)\\)", labels[[i]]))[[1L]]
    if (length(range)) {
      rows <- is.na(out) & income >= as.numeric(range[[2L]]) &
        income <= as.numeric(range[[3L]])
      out[rows] <- codes[[i]]
      next
    }
    lower <- regmatches(
      labels[[i]],
      regexec("\\(([0-9]+) or more\\)", labels[[i]])
    )[[1L]]
    if (length(lower)) {
      out[is.na(out) & income >= as.numeric(lower[[2L]])] <- codes[[i]]
    }
  }
  fallback <- utils::tail(codes[substantive], 1L)
  out[is.na(out) & is.finite(income)] <- fallback
  out
}

.acld_year_arrival_code <- function(year, frame_values) {
  labels <- frame_values$label
  codes <- frame_values$code
  substantive <- frame_values$value_kind == "substantive"
  out <- rep(NA_character_, length(year))
  for (i in which(substantive)) {
    interval <- regmatches(
      labels[[i]],
      regexec("([0-9]{4})-([0-9]{4})", labels[[i]])
    )[[1L]]
    if (length(interval)) {
      rows <- is.na(out) & year >= as.integer(interval[[2L]]) &
        year <= as.integer(interval[[3L]])
      out[rows] <- codes[[i]]
      next
    }
    earlier <- regmatches(
      labels[[i]],
      regexec("([0-9]{4}) or earlier", labels[[i]], ignore.case = TRUE)
    )[[1L]]
    if (length(earlier)) {
      out[is.na(out) & year <= as.integer(earlier[[2L]])] <- codes[[i]]
      next
    }
    years <- regmatches(labels[[i]], gregexpr("[0-9]{4}", labels[[i]]))[[1L]]
    years <- suppressWarnings(as.integer(years))
    years <- years[!is.na(years)]
    if (length(years)) {
      out[is.na(out) & year >= min(years) & year <= max(years)] <- codes[[i]]
    }
  }
  out
}

.acld_age_band_code <- function(age, frame_values) {
  labels <- frame_values$label
  codes <- frame_values$code
  substantive <- frame_values$value_kind == "substantive"
  out <- rep(NA_character_, length(age))
  for (i in which(substantive)) {
    interval <- regmatches(
      labels[[i]],
      regexec("([0-9]+)-([0-9]+) years", labels[[i]])
    )[[1L]]
    if (length(interval)) {
      rows <- is.na(out) & age >= as.integer(interval[[2L]]) &
        age <= as.integer(interval[[3L]])
      out[rows] <- codes[[i]]
      next
    }
    lower <- regmatches(
      labels[[i]],
      regexec("([0-9]+) years and over", labels[[i]], ignore.case = TRUE)
    )[[1L]]
    if (length(lower)) {
      out[is.na(out) & age >= as.integer(lower[[2L]])] <- codes[[i]]
    }
  }
  out
}

.acld_direct_value <- function(spec, role_spine, frame_values, key) {
  n <- length(key)
  year <- spec$year
  if (is.na(year)) return(NULL)
  age <- pmin(pmax(year - role_spine$birth_year, 0L), 115L)
  not_applicable <- .acld_status_code(frame_values, "not_applicable")

  if (spec$base %in% c("AGEP", "AGEN")) {
    has_age_bands <- any(grepl(
      "[0-9]+-[0-9]+ years",
      frame_values$label[frame_values$value_kind == "substantive"]
    ))
    if (has_age_bands) {
      age_band <- .acld_age_band_code(age, frame_values)
      age_band[is.na(age_band)] <- not_applicable
      return(age_band)
    }
    range <- frame_values$code[frame_values$value_kind == "range"]
    if (length(range)) {
      maximum <- as.integer(sub("^[0-9]+-", "", range[[1L]]))
    } else {
      numeric_codes <- suppressWarnings(as.integer(
        frame_values$code[frame_values$value_kind == "substantive"]
      ))
      maximum <- max(numeric_codes, na.rm = TRUE)
    }
    return(as.character(pmin(age, maximum)))
  }
  if (spec$base == "SEXP") return(as.character(role_spine$sex))
  if (spec$base == "INGP") {
    substantive <- frame_values$code[
      frame_values$value_kind == "substantive"
    ]
    value <- if (length(substantive) == 2L) {
      ifelse(role_spine$indigenous == 1L, 1L, 2L)
    } else {
      role_spine$indigenous
    }
    return(as.character(value))
  }
  if (spec$base == "CITP") return(as.character(role_spine$citizenship))
  if (spec$base == "MSTP") {
    value <- ifelse(
      age < 18L, 1L,
      ifelse(
        age < 30L, ifelse(key %% 10 < 6, 1L, 5L),
        ifelse(
          age < 60L,
          ifelse(key %% 20 < 12, 5L, ifelse(key %% 4 == 0, 3L, 1L)),
          ifelse(key %% 20 < 9, 5L, ifelse(key %% 3 == 0, 2L, 3L))
        )
      )
    )
    return(as.character(value))
  }
  if (spec$base == "LFSP") {
    value <- ifelse(
      age < 15L, NA_integer_,
      ifelse(
        role_spine$employed,
        ifelse(role_spine$hours >= 35L, 1L, 2L),
        ifelse(key %% 10 < 2, ifelse(key %% 2 == 0, 4L, 5L), 6L)
      )
    )
    out <- as.character(value)
    out[is.na(value)] <- not_applicable
    return(out)
  }
  if (spec$base == "HRWRP") {
    hours <- pmax(role_spine$hours, 0L)
    value <- ifelse(
      !role_spine$employed, 0L,
      ifelse(hours <= 15L, 1L,
        ifelse(hours <= 24L, 2L,
          ifelse(hours <= 34L, 3L,
            ifelse(hours <= 39L, 4L,
              ifelse(hours == 40L, 5L, ifelse(hours <= 48L, 6L, 7L))
            )
          )
        )
      )
    )
    return(as.character(value))
  }
  if (spec$base == "HRSP") {
    value <- pmin(pmax(role_spine$hours, 0L), 99L)
    value[!role_spine$employed] <- 0L
    return(as.character(value))
  }
  if (spec$base == "HSCP") {
    value <- ifelse(
      age < 15L | role_spine$education <= 0L, NA_integer_,
      ifelse(
        role_spine$education == 1L,
        3L + as.integer(key %% 3),
        ifelse(key %% 20 == 0, 2L, 1L)
      )
    )
    out <- as.character(value)
    out[is.na(value)] <- not_applicable
    return(out)
  }
  if (spec$base == "INCP") {
    factor <- switch(as.character(year), `2011` = 0.85, `2016` = 0.94, 1)
    return(.acld_income_code(role_spine$income * factor, frame_values))
  }
  if (spec$base == "ASSNP") {
    has_need <- !is.na(role_spine$disability_onset_year) &
      role_spine$disability_onset_year <= year &
      !is.na(role_spine$disability_severity) &
      role_spine$disability_severity <= 2L
    return(ifelse(has_need, "1", "2"))
  }
  if (spec$base == "STUP") {
    value <- ifelse(
      age >= 5L & age <= 17L, 2L,
      ifelse(age <= 24L & role_spine$education > 0L,
        ifelse(key %% 4 == 0, 3L, 2L), 1L
      )
    )
    return(as.character(value))
  }
  if (spec$base == "OCSKP") {
    value <- role_spine$skill_level
    value[!role_spine$employed | is.na(value)] <- NA_integer_
    out <- as.character(value)
    out[is.na(value)] <- not_applicable
    return(out)
  }
  if (spec$base == "OCCP") {
    value <- role_spine$anzsco_major
    value[!role_spine$employed | is.na(value)] <- NA_integer_
    out <- as.character(value)
    out[is.na(value)] <- not_applicable
    return(out)
  }
  if (spec$base == "INDP") {
    value <- role_spine$industry - 1L
    value[!role_spine$employed | is.na(value)] <- NA_integer_
    out <- as.character(value)
    out[is.na(value)] <- not_applicable
    return(out)
  }
  if (spec$base == "YARP") {
    out <- .acld_year_arrival_code(role_spine$year_of_arrival, frame_values)
    out[is.na(out)] <- not_applicable
    return(out)
  }
  if (spec$base == "ENGP") {
    australia <- role_spine$country_of_birth_sacc == 1101L
    value <- ifelse(key %% 100 < 65, 1L,
      ifelse(key %% 100 < 90, 2L, ifelse(key %% 100 < 98, 3L, 4L))
    )
    out <- as.character(value)
    out[australia | is.na(australia)] <- not_applicable
    return(out)
  }
  if (spec$base == "ENGLP") {
    australia <- role_spine$country_of_birth_sacc == 1101L
    value <- ifelse(key %% 100 < 45, 1L,
      ifelse(key %% 100 < 78, 2L,
        ifelse(key %% 100 < 93, 3L, ifelse(key %% 100 < 99, 4L, 5L))
      )
    )
    value[australia & !is.na(australia)] <- 1L
    return(as.character(value))
  }
  NULL
}

.acld_generate_value <- function(variable, frame_values, spine, row_id, seed,
                                 links) {
  spec <- .acld_field_spec(variable)
  key <- .acld_key(row_id, seed, spec$concept)
  role_spine <- .acld_role_spine(spine, spec, key)
  n <- length(key)

  if (spec$base == "LINKFLAG") {
    linked <- links[[as.character(spec$year)]]
    return(ifelse(linked, "1", "2"))
  }

  linked <- if (!is.na(spec$year)) links[[as.character(spec$year)]] else rep(TRUE, n)
  unlinked <- .acld_status_code(frame_values, "unlinked")
  not_applicable <- .acld_status_code(frame_values, "not_applicable")
  not_stated <- .acld_status_code(frame_values, "not_stated")

  age <- if (is.na(spec$year)) {
    2021L - role_spine$birth_year
  } else {
    spec$year - role_spine$birth_year
  }
  applicable <- rep(TRUE, n)
  if (spec$role %in% c("FP", "MP")) {
    threshold <- ifelse(age < 25L, 85, ifelse(age < 45L, 55, 20))
    applicable <- key %% 100 < threshold
  } else if (spec$role %in% c("SP", "S")) {
    threshold <- ifelse(age < 18L, 0, ifelse(age < 25L, 20,
      ifelse(age < 65L, 60, 45)
    ))
    applicable <- key %% 100 < threshold
  } else if (grepl("_P_(11|16|21)$", spec$variable, perl = TRUE)) {
    applicable <- key %% 100 >= 4
  }

  direct <- .acld_direct_value(spec, role_spine, frame_values, key)
  substantive <- frame_values$code[
    frame_values$value_kind == "substantive"
  ]
  ranges <- frame_values$code[frame_values$value_kind == "range"]
  if (is.null(direct)) {
    if (length(ranges)) {
      bounds <- as.integer(strsplit(ranges[[1L]], "-", fixed = TRUE)[[1L]])
      direct <- as.character(
        bounds[[1L]] + as.integer(key %% (bounds[[2L]] - bounds[[1L]] + 1L))
      )
    } else {
      quantile <- key / 999999937
      index <- pmin(
        length(substantive),
        1L + as.integer(floor(quantile * length(substantive)))
      )
      direct <- substantive[index]
    }
  }

  if (!is.na(not_applicable)) direct[!applicable] <- not_applicable
  if (!is.na(not_stated)) {
    stated_key <- .acld_key(row_id, seed, spec$concept, extra = 17L)
    direct[linked & applicable & stated_key %% 1000 < 12] <- not_stated
  }
  if (!is.na(unlinked)) direct[!linked] <- unlinked
  direct
}

.acld_enrich_product <- function(data, spine, product, seed) {
  registry <- .acld_codeframe_registry()
  product_mapping <- registry$mapping[
    registry$mapping$product == product, , drop = FALSE
  ]
  mapping <- product_mapping[product_mapping$supported, , drop = FALSE]
  match_row <- match(as.character(data$SYNTHETIC_AEUID), as.character(spine$aeuid_abs))
  if (anyNA(match_row)) {
    stop("ACLD sample identifiers do not align with the ABS spine.", call. = FALSE)
  }
  panel_year <- if (
    identical(product, "madipge-acld11-d-persons-11-16-21")
  ) 2011L else 2016L
  eligible <- as.integer(spine$birth_year[match_row]) <= panel_year
  data <- data[eligible, , drop = FALSE]
  match_row <- match_row[eligible]
  aligned_spine <- spine[match_row, , drop = FALSE]
  links <- .acld_product_links(data, product, match_row, seed)

  frame_values <- split(registry$values, registry$values$frame_id)
  for (i in seq_len(nrow(mapping))) {
    variable <- mapping$variable[[i]]
    data[[variable]] <- .acld_generate_value(
      variable,
      frame_values[[mapping$frame_id[[i]]]],
      aligned_spine,
      match_row,
      seed,
      links
    )
  }

  public_mapping <- product_mapping[
    !product_mapping$supported &
      vapply(
        product_mapping$variable,
        function(variable) .acld_unsupported_decision(variable) %in%
          c("public_census_classification", "public_asgs_geography"),
        logical(1)
      ),
    , drop = FALSE
  ]
  for (i in seq_len(nrow(public_mapping))) {
    variable <- public_mapping$variable[[i]]
    original_frame <- frame_values[[public_mapping$frame_id[[i]]]]
    if (is.null(original_frame)) {
      original_frame <- registry$values[0L, , drop = FALSE]
    }
    data[[variable]] <- .acld_public_value(
      variable, original_frame, aligned_spine, match_row, seed, links
    )
  }

  # These DIL fields are not classifications in the ABS workbook. LEVEL is
  # the record level. Weight values remain synthetic around the inverse 5%
  # ACLD panel sampling rate; their names and analysis scopes are official.
  data$LEVEL <- rep("Person", nrow(data))
  weight <- as.numeric(data$WEIGHT4)
  if (identical(product, "madipge-acld11-d-persons-11-16-21")) {
    data$WEIGHT4_11_16 <- round(weight, 2L)
    data$WEIGHT4_11_16_21 <- round(weight * 1.03, 2L)
  } else {
    data$WEIGHT4_16_21 <- round(weight, 2L)
  }
  data
}

.acld_unresolved_variables <- function() {
  mapping <- .acld_codeframe_registry()$mapping
  unresolved <- !mapping$supported & vapply(
    mapping$variable,
    function(variable) identical(
      .acld_unsupported_decision(variable), "unresolved"
    ),
    logical(1)
  )
  sort(unique(toupper(mapping$variable[unresolved])))
}

.dil_acld_source_value <- function(name, source_frame, spine_rows, seed,
                                   product_name = "", table_name = "",
                                   module_name = "") {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  if (!n) return(character())
  if (upper %in% .acld_unresolved_variables()) {
    return(rep(NA_character_, n))
  }

  registry <- .acld_codeframe_registry()
  hit <- which(
    registry$mapping$product == product_name &
      toupper(registry$mapping$variable) == upper &
      !registry$mapping$supported
  )
  if (!length(hit)) return(NULL)
  mapping <- registry$mapping[hit[[1L]], , drop = FALSE]
  decision <- .acld_unsupported_decision(mapping$variable[[1L]])
  if (!decision %in% c(
    "public_census_classification", "public_asgs_geography"
  )) {
    return(NULL)
  }

  original_frame <- registry$values[
    registry$values$frame_id == mapping$frame_id[[1L]], , drop = FALSE
  ]
  row_id <- .dil_numeric_key(
    spine_rows, seed,
    .stable_name_seed(paste("ACLD", product_name, table_name, sep = "|"))
  )
  link_frame <- source_frame
  if (nrow(link_frame) != n) link_frame <- data.frame(.row = seq_len(n))
  links <- .acld_product_links(link_frame, product_name, row_id, seed)
  .acld_public_value(
    mapping$variable[[1L]], original_frame, spine_rows, row_id, seed, links
  )
}

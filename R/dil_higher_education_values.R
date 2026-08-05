# Source-backed values for the canonical Higher Education tables.
#
# The code sets legacy HEIMS values through 2020 and TCSI values from 2021.
# Official element definitions are at https://www.tcsisupport.gov.au/element/.

.dil_he_year <- function(source_frame, n, period) {
  value <- .dil_source_alias(
    source_frame,
    c("YEAR", "REPORTING_YEAR", "REFERENCE_YEAR")
  )
  if (!is.null(value)) return(as.integer(value))
  value <- .dil_source_alias(
    source_frame,
    c("REPORTING_YEAR_PERIOD", "REPORTING_PERIOD")
  )
  if (!is.null(value)) {
    year <- suppressWarnings(as.integer(substr(as.character(value), 1L, 4L)))
    year[is.na(year)] <- as.integer(period$end_year)
    return(year)
  }
  rep(as.integer(period$end_year), n)
}

.dil_he_weighted_code <- function(key, codes, weights) {
  stopifnot(length(codes) == length(weights), all(weights >= 0))
  cumulative <- cumsum(weights) / sum(weights)
  unit <- ((as.numeric(key) %% 1000000) + 0.5) / 1000000
  index <- findInterval(
    unit,
    c(0, cumulative),
    rightmost.closed = TRUE,
    all.inside = TRUE
  )
  as.character(codes[pmin(index, length(codes))])
}

.dil_he_parent_education <- function(spine_rows, key) {
  education <- if ("education" %in% names(spine_rows)) {
    pmin(pmax(as.integer(spine_rows$education), 0L), 5L)
  } else {
    rep(2L, nrow(spine_rows))
  }
  education[is.na(education)] <- 2L
  codes <- c("20", "21", "22", "23", "24", "25", "26", "49", "99")
  weights <- list(
    c(2, 6, 16, 25, 17, 15, 12, 4, 3),
    c(2, 7, 17, 25, 16, 14, 12, 4, 3),
    c(3, 9, 20, 25, 14, 12, 10, 4, 3),
    c(4, 13, 25, 24, 11, 10, 7, 3, 3),
    c(8, 20, 28, 22, 8, 7, 4, 2, 1),
    c(16, 30, 23, 18, 5, 4, 2, 1, 1)
  )
  out <- character(length(education))
  for (level in 0:5) {
    rows <- which(education == level)
    if (!length(rows)) next
    out[rows] <- .dil_he_weighted_code(
      key[rows], codes, weights[[level + 1L]]
    )
  }
  out
}

.dil_he_language_frame <- local({
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

.dil_he_language_home <- function(spine_rows, key) {
  n <- nrow(spine_rows)
  sacc <- if ("country_of_birth_sacc" %in% names(spine_rows)) {
    sprintf("%04d", as.integer(spine_rows$country_of_birth_sacc))
  } else if ("country_of_birth" %in% names(spine_rows)) {
    ifelse(as.integer(spine_rows$country_of_birth) == 0L, "1101", "9999")
  } else {
    rep("9999", n)
  }
  major <- suppressWarnings(as.integer(substr(sacc, 1L, 1L)))
  english_probability <- c(
    `1` = 0.82, `2` = 0.72, `3` = 0.25, `4` = 0.18, `5` = 0.16,
    `6` = 0.14, `7` = 0.18, `8` = 0.55, `9` = 0.30
  )[as.character(major)]
  english_probability[is.na(english_probability)] <- 0.70
  english_probability[sacc %in% c("1101", "1102", "1199")] <- 0.82

  unit <- ((as.numeric(key) %% 1000000) + 0.5) / 1000000
  out <- rep("0001", n)
  non_english <- unit >= english_probability

  indigenous <- if ("indigenous" %in% names(spine_rows)) {
    as.integer(spine_rows$indigenous) %in% 2:4
  } else {
    rep(FALSE, n)
  }
  indigenous_language <- indigenous & (as.numeric(key) %% 100L < 8L)
  out[indigenous_language] <- "8000"
  non_english[indigenous_language] <- FALSE

  candidates <- list(
    `1` = c("9308", "9301", "1403"),
    `2` = c("1301", "2101", "1401"),
    `3` = c("2201", "2401", "3402", "3503", "3602"),
    `4` = c("4202", "4106", "4301", "4204"),
    `5` = c("6302", "6511", "6504", "6402", "6301"),
    `6` = c("7104", "7101", "7301", "7201"),
    `7` = c("5207", "5203", "5206", "5212", "5103"),
    `8` = c("2303", "2302", "2101"),
    `9` = c("1403", "9211", "9216", "2101")
  )
  codeframe <- .dil_he_language_frame()
  for (group in as.character(1:9)) {
    rows <- which(non_english & major == as.integer(group))
    if (!length(rows)) next
    choices <- codeframe[codeframe$code %in% candidates[[group]], , drop = FALSE]
    out[rows] <- .dil_he_weighted_code(
      key[rows] + 104729,
      choices$code,
      choices$weight
    )
  }
  unknown_group <- which(non_english & (is.na(major) | !major %in% 1:9))
  if (length(unknown_group)) {
    out[unknown_group] <- .dil_he_weighted_code(
      key[unknown_group] + 104729,
      codeframe$code,
      codeframe$weight
    )
  }
  out[as.numeric(key) %% 1000L < 10L] <- "9999"
  out[as.numeric(key) %% 1000L >= 10L & as.numeric(key) %% 1000L < 20L] <-
    "9998"
  out
}

.dil_he_admission <- function(source_frame, spine_rows, key, year) {
  n <- nrow(spine_rows)
  commencing <- .dil_source_alias(source_frame, "COM_INDICATOR")
  if (is.null(commencing)) commencing <- rep(1L, n)
  commencing <- as.integer(commencing) == 1L
  age <- pmax(year - as.integer(spine_rows$birth_year), 0L)
  education <- if ("education" %in% names(spine_rows)) {
    as.integer(spine_rows$education)
  } else {
    rep(2L, n)
  }
  education[is.na(education)] <- 2L

  out <- character(n)
  legacy <- year <= 2020L
  out[legacy & !commencing] <- "01"
  rows <- which(legacy & commencing)
  if (length(rows)) {
    school <- rows[age[rows] <= 20L]
    vet <- setdiff(rows[education[rows] %in% 3:4], school)
    prior_he <- setdiff(rows[education[rows] >= 5L], c(school, vet))
    other <- setdiff(rows, c(school, vet, prior_he))
    out[school] <- "33"
    out[vet] <- "34"
    out[prior_he] <- "31"
    if (length(other)) {
      out[other] <- .dil_he_weighted_code(
        key[other], c("36", "37", "29"), c(65, 10, 25)
      )
    }
  }

  rows <- which(!legacy)
  if (length(rows)) {
    school <- rows[age[rows] <= 20L]
    vet <- setdiff(rows[education[rows] %in% 3:4], school)
    prior_he <- setdiff(rows[education[rows] >= 5L], c(school, vet))
    other <- setdiff(rows, c(school, vet, prior_he))
    if (length(school)) {
      out[school] <- .dil_he_weighted_code(
        key[school], c("41", "42", "43"), c(45, 35, 20)
      )
    }
    out[vet] <- "34"
    out[prior_he] <- "31"
    if (length(other)) {
      out[other] <- .dil_he_weighted_code(
        key[other], c("40", "32", "31"), c(80, 10, 10)
      )
    }
  }
  out
}

.dil_he_scholarship <- function(spine_rows, key, year) {
  n <- nrow(spine_rows)
  education <- if ("education" %in% names(spine_rows)) {
    as.integer(spine_rows$education)
  } else {
    rep(2L, n)
  }
  likely_research <- education >= 5L & as.numeric(key) %% 100L < 8L
  out <- rep("00", n)

  rows <- which(likely_research & year <= 2011L)
  if (length(rows)) {
    out[rows] <- .dil_he_weighted_code(
      key[rows], c("01", "02", "06"), c(70, 25, 5)
    )
  }
  rows <- which(likely_research & year >= 2012L & year <= 2016L)
  if (length(rows)) {
    out[rows] <- .dil_he_weighted_code(
      key[rows], c("01", "02", "06", "07"), c(55, 20, 5, 20)
    )
  }
  rows <- which(likely_research & year >= 2017L & year <= 2020L)
  if (length(rows)) {
    out[rows] <- .dil_he_weighted_code(
      key[rows], c("06", "08", "09"), c(5, 55, 40)
    )
  }
  current <- year >= 2021L
  out[current & !likely_research] <- NA_character_
  rows <- which(current & likely_research)
  if (length(rows)) {
    out[rows] <- .dil_he_weighted_code(
      key[rows], c("09", "10", "11"), c(45, 45, 10)
    )
  }
  out
}

.dil_he_source_value <- function(name, source_frame, spine_rows, seed,
                                 period) {
  upper <- toupper(name)
  supported <- c(
    "EDUCATION_PARENT1", "EDUCATION_PARENT2", "LANGUAGE_HOME",
    "NEW_ADMISSION", "SCHOLARSHIP_TYPE", "SEPARATION_STATUS_CODE",
    "CAMPUS_GLOBAL_REGION"
  )
  if (!upper %in% supported) return(NULL)
  n <- nrow(spine_rows)
  year <- .dil_he_year(source_frame, n, period)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed(paste("HE", upper, sep = "|"))
  )

  if (upper %in% c("EDUCATION_PARENT1", "EDUCATION_PARENT2")) {
    parent <- .dil_he_parent_education(spine_rows, key)
    commencing <- .dil_source_alias(source_frame, "COM_INDICATOR")
    if (is.null(commencing)) commencing <- rep(1L, n)
    commencing <- as.integer(commencing)
    commencing[is.na(commencing)] <- 0L
    citizenship <- if ("citizenship" %in% names(spine_rows)) {
      as.integer(spine_rows$citizenship)
    } else {
      rep(1L, n)
    }
    citizenship[is.na(citizenship)] <- 2L
    parent[year < 2010L] <- NA_character_
    parent[year >= 2010L & year <= 2020L & commencing != 1L] <- "01"
    parent[
      year >= 2010L & year <= 2020L & commencing == 1L & citizenship != 1L
    ] <-
      "98"
    parent[year >= 2021L & citizenship != 1L] <- NA_character_
    if (upper == "EDUCATION_PARENT2") {
      one_parent <- as.numeric(key) %% 100L < 12L
      parent[one_parent] <- NA_character_
    }
    return(parent)
  }
  if (upper == "LANGUAGE_HOME") {
    return(.dil_he_language_home(spine_rows, key))
  }
  if (upper == "NEW_ADMISSION") {
    return(.dil_he_admission(source_frame, spine_rows, key, year))
  }
  if (upper == "SCHOLARSHIP_TYPE") {
    return(.dil_he_scholarship(spine_rows, key, year))
  }
  if (upper == "SEPARATION_STATUS_CODE") {
    out <- rep("9", n)
    commencing <- .dil_source_alias(source_frame, "COM_INDICATOR")
    if (is.null(commencing)) commencing <- rep(1L, n)
    education <- if ("education" %in% names(spine_rows)) {
      as.integer(spine_rows$education)
    } else {
      rep(2L, n)
    }
    likely_research <- education >= 5L & as.integer(commencing) == 1L &
      as.numeric(key) %% 100L < 8L
    rows <- which(likely_research & year <= 2020L)
    if (length(rows)) {
      out[rows] <- .dil_he_weighted_code(
        key[rows], c("1", "2", "3", "9"), c(45, 25, 5, 25)
      )
    }
    out[year >= 2021L] <- NA_character_
    return(out)
  }
  if (upper == "CAMPUS_GLOBAL_REGION") {
    return(rep("1", n))
  }
  NULL
}

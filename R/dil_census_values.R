# Official-source value rules for canonical Census DIL tables.
#
# Code domains come from the ABS 2011 Expanded CSF data-item list, the ABS
# 2016 detailed-microdata data-item list, the 2021 Census Dictionary, and the
# 2021 detailed-microdata data-item list. Geography values come from official
# ABS ASGS ArcGIS services. The corresponding refresh scripts are in data-raw/.

.dil_census_codeframe_registry <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    registry_path <- function(file) {
      path <- system.file("extdata", "codeframes", file, package = "fplida")
      if (!nzchar(path)) path <- file.path("inst", "extdata", "codeframes", file)
      path
    }
    mapping <- utils::read.csv(
      registry_path("census-variable-codeframes.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    values <- utils::read.csv(
      registry_path("census-codeframe-values.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )
    mapping$year <- as.integer(mapping$year)
    mapping$data_item_length <- suppressWarnings(as.integer(
      mapping$data_item_length
    ))
    values$year <- as.integer(values$year)
    cache <<- list(mapping = mapping, values = values)
    cache
  }
})

.dil_census_geography_registry <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file(
      "extdata", "codeframes", "census-geography-values.tsv",
      package = "fplida"
    )
    if (!nzchar(path)) {
      path <- file.path(
        "inst", "extdata", "codeframes", "census-geography-values.tsv"
      )
    }
    values <- utils::read.delim(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )
    values$year <- as.integer(values$year)
    cache <<- values
    cache
  }
})

.dil_census_anzsic_registry <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file(
      "extdata", "codeframes", "anzsic2006.tsv", package = "fplida"
    )
    if (!nzchar(path)) {
      path <- file.path("inst", "extdata", "codeframes", "anzsic2006.tsv")
    }
    cache <<- utils::read.delim(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )
    cache
  }
})

.dil_census_data_length <- function(year, variable) {
  mapping <- .dil_census_codeframe_registry()$mapping
  hit <- which(
    mapping$year == as.integer(year) &
      toupper(mapping$variable) == toupper(variable)
  )
  if (!length(hit)) return(NA_integer_)
  mapping$data_item_length[[hit[[1L]]]]
}

.dil_census_field_frame <- function(year, variable) {
  registry <- .dil_census_codeframe_registry()
  mapping <- registry$mapping[
    registry$mapping$year == as.integer(year) &
      toupper(registry$mapping$variable) == toupper(variable),
    ,
    drop = FALSE
  ]
  values <- registry$values[
    registry$values$year == as.integer(year) &
      toupper(registry$values$variable) == toupper(variable),
    ,
    drop = FALSE
  ]
  if (!nrow(values)) return(values)
  if (nrow(mapping) && nzchar(mapping$source_sheet[[1L]])) {
    selected <- values$source_sheet == mapping$source_sheet[[1L]] &
      values$source_url == mapping$source_url[[1L]]
    if (any(selected)) values <- values[selected, , drop = FALSE]
  }
  values <- values[grepl(
    "^[0-9A-Za-z@&]+(?:-[0-9A-Za-z@&]+)?$",
    values$code,
    perl = TRUE
  ), , drop = FALSE]
  length_required <- .dil_census_data_length(year, variable)
  dictionary_frame <- nrow(mapping) && grepl(
    "Census Dictionary", mapping$source_sheet[[1L]], fixed = TRUE
  )
  if (!is.na(length_required) && dictionary_frame) {
    range <- grepl("^[0-9]+-[0-9]+$", values$code)
    range_width <- rep(NA_integer_, nrow(values))
    range_width[range] <- nchar(sub("-.*$", "", values$code[range]))
    values <- values[
      nchar(values$code) == length_required |
        (range & range_width == length_required),
      ,
      drop = FALSE
    ]
  }
  values
}

.dil_census_status_code <- function(frame, kind, year, variable) {
  hit <- frame$code[frame$value_kind == kind]
  if (length(hit)) return(hit[[1L]])
  length_required <- .dil_census_data_length(year, variable)
  if (kind == "not_applicable" && year == 2021L &&
      !is.na(length_required)) {
    return(strrep("@", length_required))
  }
  NA_character_
}

.dil_census_label_code <- function(frame, pattern, fallback = NA_character_) {
  hit <- which(grepl(pattern, frame$label, ignore.case = TRUE, perl = TRUE))
  if (length(hit)) frame$code[[hit[[1L]]]] else fallback
}

.dil_census_weighted_pick <- function(key, values, weights = NULL) {
  keep <- !is.na(values) & nzchar(values)
  values <- as.character(values[keep])
  if (!is.null(weights)) weights <- as.numeric(weights[keep])
  if (!length(values)) return(rep(NA_character_, length(key)))
  # Mix the canonical linear key before converting it to a unit interval.
  # Taking only its final digits would give adjacent spine rows nearly the
  # same draw because .dil_numeric_key() advances by 1,000,003 per person.
  modulus <- 2147483647
  mixed <- ((as.numeric(key) %% modulus) * 48271 + 1) %% modulus
  unit <- (mixed + 0.5) / modulus
  if (is.null(weights)) {
    index <- 1L + floor(unit * length(values))
  } else {
    stopifnot(length(values) == length(weights), all(weights >= 0))
    cumulative <- cumsum(weights) / sum(weights)
    index <- findInterval(
      unit, c(0, cumulative), rightmost.closed = TRUE, all.inside = TRUE
    )
  }
  values[pmin(pmax(index, 1L), length(values))]
}

.dil_census_spine_integer <- function(spine_rows, name, default) {
  if (!name %in% names(spine_rows)) return(rep(as.integer(default), nrow(spine_rows)))
  out <- suppressWarnings(as.integer(spine_rows[[name]]))
  out[is.na(out)] <- as.integer(default)
  out
}

.dil_census_spine_numeric <- function(spine_rows, name, default) {
  if (!name %in% names(spine_rows)) return(rep(as.numeric(default), nrow(spine_rows)))
  out <- suppressWarnings(as.numeric(spine_rows[[name]]))
  out[!is.finite(out)] <- as.numeric(default)
  out
}

.dil_census_household_context <- function(spine_rows, key) {
  n <- nrow(spine_rows)
  household_key <- if ("household_id" %in% names(spine_rows)) {
    suppressWarnings(as.numeric(spine_rows$household_id))
  } else {
    key
  }
  household_key[!is.finite(household_key)] <- key[!is.finite(household_key)]
  selector <- as.integer(household_key %% 100L)
  persons <- ifelse(
    selector < 25L, 1L,
    ifelse(selector < 58L, 2L,
      ifelse(selector < 76L, 3L,
        ifelse(selector < 90L, 4L, ifelse(selector < 97L, 5L, 6L))
      )
    )
  )
  children <- pmin(
    pmax(persons - 1L, 0L),
    ifelse(selector < 45L, 0L,
      ifelse(selector < 75L, 1L, ifelse(selector < 92L, 2L, 3L))
    )
  )
  couple <- persons >= 2L & selector %% 10L < 7L
  one_parent <- children > 0L & !couple
  list(
    key = household_key,
    persons = as.integer(persons),
    children = as.integer(children),
    adults = as.integer(pmax(persons - children, 1L)),
    couple = couple,
    one_parent = one_parent,
    tenure = ifelse(
      selector < 31L, 1L,
      ifelse(selector < 66L, 2L, ifelse(selector < 96L, 4L, 6L))
    )
  )
}

.dil_census_income_code <- function(income_weekly, frame) {
  labels <- gsub("[$,]", "", frame$label)
  substantive <- frame$value_kind == "substantive"
  out <- rep(NA_character_, length(income_weekly))
  for (i in which(substantive)) {
    numbers <- suppressWarnings(as.numeric(regmatches(
      labels[[i]], gregexpr("[0-9]+", labels[[i]])
    )[[1L]]))
    numbers <- numbers[is.finite(numbers)]
    if (grepl("negative", labels[[i]], ignore.case = TRUE)) {
      out[income_weekly < 0] <- frame$code[[i]]
    } else if (grepl("nil|zero", labels[[i]], ignore.case = TRUE)) {
      out[income_weekly == 0] <- frame$code[[i]]
    } else if (length(numbers) >= 2L) {
      rows <- is.na(out) & income_weekly >= min(numbers) &
        income_weekly <= max(numbers)
      out[rows] <- frame$code[[i]]
    } else if (length(numbers) == 1L && grepl(
      "or more|and over|more than", labels[[i]], ignore.case = TRUE
    )) {
      out[is.na(out) & income_weekly >= numbers[[1L]]] <- frame$code[[i]]
    }
  }
  fallback <- utils::tail(frame$code[substantive], 1L)
  if (length(fallback)) out[is.na(out)] <- fallback
  out
}

.dil_census_generic_code <- function(frame, key, year, variable) {
  if (!nrow(frame)) return(NULL)
  substantive <- frame[
    frame$value_kind == "substantive" &
      !grepl("^[0-9]+-[0-9]+$", frame$code),
    ,
    drop = FALSE
  ]
  values <- unique(substantive$code)
  ranges <- unique(frame$code[
    frame$value_kind %in% c("substantive", "range") &
      grepl("^[0-9]+-[0-9]+$", frame$code)
  ])
  if (length(ranges)) {
    expanded <- unlist(lapply(ranges, function(value_range) {
      bounds <- strsplit(value_range, "-", fixed = TRUE)[[1L]]
      lower <- as.integer(bounds[[1L]])
      upper <- as.integer(bounds[[2L]])
      width <- nchar(bounds[[1L]])
      if (upper < lower || upper - lower > 1000L) return(character())
      sprintf(paste0("%0", width, "d"), seq.int(lower, upper))
    }), use.names = FALSE)
    values <- unique(c(values, expanded))
  }
  if (!length(values)) return(NULL)
  out <- .dil_census_weighted_pick(key, values)
  not_stated <- .dil_census_status_code(
    frame, "not_stated", year, variable
  )
  if (!is.na(not_stated)) out[key %% 100L == 0L] <- not_stated
  out
}

.dil_census_geography_value <- function(variable, spine_rows, key, year,
                                         employed, age) {
  upper <- toupper(variable)
  n <- nrow(spine_rows)
  state <- .dil_census_spine_integer(spine_rows, "state", 1L)
  state[!state %in% 1:9] <- 1L + as.integer(key[!state %in% 1:9] %% 8L)
  length_required <- .dil_census_data_length(year, upper)
  not_applicable <- if (!is.na(length_required)) {
    strrep("@", length_required)
  } else {
    NA_character_
  }

  pick_registry <- function(registry_year, layer) {
    values <- .dil_census_geography_registry()
    values <- values[
      values$year == as.integer(registry_year) & values$layer == layer,
      ,
      drop = FALSE
    ]
    if (!nrow(values)) return(NULL)
    out <- character(n)
    for (state_value in unique(state)) {
      rows <- which(state == state_value)
      choices <- values[
        values$state == as.character(state_value) |
          !nzchar(values$state),
        ,
        drop = FALSE
      ]
      if (layer == "SUA") {
        state_choices <- startsWith(choices$code, as.character(state_value))
        if (any(state_choices)) choices <- choices[state_choices, , drop = FALSE]
      }
      if (!nrow(choices)) choices <- values
      index <- 1L + as.integer(key[rows] %% nrow(choices))
      out[rows] <- choices$code[index]
    }
    out
  }

  # Place of usual residence one or five years ago uses a standard nine-digit
  # SA2 for Australian addresses and ten-digit detailed-microdata
  # supplementary codes. These exact supplementary codes are present in the
  # official 2016 detailed-microdata test file and retained in 2021.
  if (upper %in% c("PUR1P", "PUR5P")) {
    years_ago <- if (upper == "PUR1P") 1L else 5L
    if (year == 2021L && "sa2_code" %in% names(spine_rows)) {
      out <- as.character(spine_rows$sa2_code)
      valid <- grepl("^[1-9][0-9]{8}$", out)
      fallback <- pick_registry(2016L, "SA2")
      if (!is.null(fallback)) out[!valid] <- fallback[!valid]
    } else {
      out <- pick_registry(year, "SA2")
    }
    if (is.null(out)) return(NULL)
    arrival <- .dil_census_spine_integer(
      spine_rows, "year_of_arrival", year - years_ago - 1L
    )
    australia <- .dil_census_spine_integer(
      spine_rows, "country_of_birth_sacc", 1101L
    ) == 1101L
    out[!australia & arrival > year - years_ago] <- "1000009299"
    out[key %% 200L == 0L] <- "9999999997"
    out[age < years_ago] <- "9999999998"
    return(out)
  }

  # The 2021 Census exposes the non-ASGS Natural Resource Management Regions
  # using the same seven-character NRMR + three-digit convention. The latest
  # official machine-readable NRMR layer available from ABS is ASGS 2016.
  if (year == 2021L && upper %in% c("NRMRD", "NRMRP")) {
    out <- pick_registry(2016L, "NRMR")
    if (is.null(out)) return(NULL)
    return(paste0("NRMR", out))
  }

  if (year %in% c(2011L, 2016L)) {
    layer <- switch(
      upper,
      SA1U1P = "SA1", SA1U5P = "SA1", SA1UCD = "SA1", SA1UCP = "SA1",
      SA2U1P = "SA2", SA2U5P = "SA2", SA2UCD = "SA2", SA2UCP = "SA2",
      SA2WKADP = "SA2", IMPSA2WKADP = "SA2",
      SA3UCD = "SA3", SA3UCP = "SA3",
      SA4UCD = "SA4", SA4UCP = "SA4",
      RAP = "RA",
      NULL
    )
    if (upper == "IMPSTEWKADP") {
      out <- as.character(state)
      out[!employed] <- "9"
      return(out)
    }
    if (is.null(layer)) return(NULL)
    out <- pick_registry(year, layer)
    if (is.null(out)) return(NULL)
    width <- switch(layer, SA1 = 7L, SA2 = 9L, SA3 = 5L, SA4 = 3L, RA = 4L)
    if (layer == "RA") out <- paste0("RA", out)
    if (upper %in% c("SA1U1P", "SA2U1P")) out[age < 1L] <- strrep("9", width)
    if (upper %in% c("SA1U5P", "SA2U5P")) out[age < 5L] <- strrep("9", width)
    if (upper %in% c("SA2WKADP", "IMPSA2WKADP")) {
      out[!employed] <- strrep("9", width)
    }
    return(out)
  }

  if (year != 2021L) return(NULL)

  if (upper %in% c("AUSTP", "AUSTU1P", "AUSTU5P", "POWAUST")) {
    out <- rep("A", n)
    if (upper == "AUSTU1P") out[age < 1L] <- "@"
    if (upper == "AUSTU5P") out[age < 5L] <- "@"
    if (upper == "POWAUST") out[!employed] <- "@"
    return(out)
  }

  current_mb <- function(level) {
    if (level %in% c("MB", "SA1")) {
      return(.dil_asgs_2021_value(spine_rows, key, level))
    }
    column <- paste0(tolower(level), "_code")
    if (!column %in% names(spine_rows)) return(rep(NA_character_, n))
    as.character(spine_rows[[column]])
  }
  if (upper %in% c("MBU1P", "MBU5P")) {
    out <- current_mb("MB")
    out[age < if (upper == "MBU1P") 1L else 5L] <- not_applicable
    return(out)
  }
  if (upper %in% c("SA1U1P", "SA1U5P", "SA2U1P", "SA2U5P")) {
    level <- substr(upper, 1L, 3L)
    out <- current_mb(level)
    out[age < if (grepl("U1P$", upper)) 1L else 5L] <- not_applicable
    return(out)
  }
  if (upper %in% c("POWMB", "POWSA2", "POWSA3", "POWSA4", "POWSTE")) {
    level <- sub("^POW", "", upper)
    out <- if (level == "STE") as.character(state) else current_mb(level)
    out[!employed] <- not_applicable
    return(out)
  }

  layer <- switch(
    upper,
    ADDIVD = "ADD", ADDIVP = "ADD",
    CEDD = "CED", CEDP = "CED",
    GCCSAD = "GCCSA", GCCSAP = "GCCSA", POWGCCSA = "GCCSA",
    IARED = "IARE", IAREP = "IARE",
    ILOCD = "ILOC", ILOCP = "ILOC",
    POAD = "POA", POAP = "POA",
    POWDZN = "DZN",
    RAD = "RA", RAP = "RA", RAND = "RA", RANP = "RA",
    SALD = "SAL", SALP = "SAL",
    SEDD = "SED", SEDP = "SED", SED22D = "SED", SEDP22 = "SED",
    SOSD = "SOS", SOSP = "SOS",
    SOSRD = "SOSR", SOSRP = "SOSR",
    SUAD = "SUA", SUAP = "SUA",
    TRD = "TR", TRP = "TR",
    UCLD = "UCL", UCLP = "UCL",
    NULL
  )
  if (is.null(layer)) return(NULL)
  geography_year <- if (upper %in% c("SED22D", "SEDP22")) 2022L else 2021L
  out <- pick_registry(geography_year, layer)
  if (is.null(out)) return(NULL)
  prefix <- switch(
    layer,
    CED = "CED", IARE = "IARE", ILOC = "ILOC", POA = "POA",
    RA = "RA", SAL = "SAL", SED = "SED", SOS = "SOS",
    SOSR = "SOSR", UCL = "UCL", ""
  )
  out <- paste0(prefix, out)
  if (upper %in% c("RAND", "RANP")) out <- substr(out, nchar(out), nchar(out))
  if (grepl("^POW", upper)) out[!employed] <- not_applicable
  out
}

.dil_census_source_value <- function(name, spine_rows, seed, salt,
                                     table_name) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  if (!n) return(character())
  year <- if (grepl("2011", table_name)) {
    2011L
  } else if (grepl("2016", table_name)) {
    2016L
  } else {
    2021L
  }
  key <- .dil_numeric_key(spine_rows, seed, salt)
  age <- pmin(pmax(
    year - .dil_census_spine_integer(spine_rows, "birth_year", year - 40L),
    0L
  ), 115L)
  sex <- .dil_census_spine_integer(spine_rows, "sex", 1L)
  employed <- .dil_census_spine_integer(
    spine_rows, "baseline_employed", 0L
  ) == 1L & age >= 15L
  hours <- pmax(.dil_census_spine_integer(
    spine_rows, "baseline_hours", 0L
  ), 0L)
  education <- pmin(pmax(.dil_census_spine_integer(
    spine_rows, "education", 2L
  ), 0L), 5L)
  income_annual <- .dil_census_spine_numeric(
    spine_rows, "baseline_income", 52000
  )
  income_weekly <- income_annual / 52
  indigenous <- .dil_census_spine_integer(spine_rows, "indigenous", 0L)
  arrival <- .dil_census_spine_integer(
    spine_rows, "year_of_arrival", year
  )
  frame <- .dil_census_field_frame(year, upper)
  not_applicable <- .dil_census_status_code(
    frame, "not_applicable", year, upper
  )

  geography <- .dil_census_geography_value(
    upper, spine_rows, key, year, employed, age
  )
  if (!is.null(geography)) return(geography)

  if (upper == "INDP") {
    registry <- .dil_census_anzsic_registry()
    industry <- pmin(pmax(.dil_census_spine_integer(
      spine_rows, "industry", 1L
    ), 1L), 19L)
    division <- LETTERS[industry]
    class_code <- rep(NA_character_, n)
    for (letter in LETTERS[1:19]) {
      rows <- which(division == letter)
      if (!length(rows)) next
      choices <- registry$anzsic_class_code[
        registry$anzsic_division_code == letter
      ]
      class_code[rows] <- choices[
        1L + as.integer(key[rows] %% length(choices))
      ]
    }
    if (year == 2011L) {
      division_pattern <- c(
        "Agriculture|Forestry|Fishing", "Mining", "Manufactur",
        "Electricity|Gas|Water|Waste", "Construction", "Wholesale",
        "Retail", "Accommodation|Food Services",
        "Transport|Postal|Warehous", "Publishing|Broadcast|Information|Telecommun",
        "Finance|Insurance|Superannuation", "Rental|Hiring|Real Estate",
        "Professional|Scientific|Technical|Computer Systems",
        "Administrative|Support Services", "Public Administration|Safety",
        "Education|Training", "Health Care|Social Assistance|Residential Care",
        "Arts|Recreation", "Other Services|Personal Services|Repair|Religious|Private Households"
      )
      out <- rep(NA_character_, n)
      for (division_i in seq_along(division_pattern)) {
        rows <- which(industry == division_i)
        if (!length(rows)) next
        choices <- frame$code[
          frame$value_kind == "substantive" &
            grepl(
              division_pattern[[division_i]], frame$label,
              ignore.case = TRUE, perl = TRUE
            )
        ]
        if (!length(choices)) {
          choices <- frame$code[frame$value_kind == "substantive"]
        }
        out[rows] <- .dil_census_weighted_pick(key[rows], choices)
      }
    } else if (year == 2016L) {
      # The detailed DataLab file stores ANZSIC classes as numeric
      # classification values, so leading zeroes are absent (0112 -> 112).
      out <- as.character(as.integer(class_code))
    } else {
      out <- class_code
    }
    status <- if (!is.na(not_applicable)) {
      not_applicable
    } else {
      strrep("@", 4L)
    }
    out[!employed] <- status
    return(out)
  }

  if (upper == "HOSD") {
    household <- .dil_census_household_context(spine_rows, key)
    required <- ceiling(household$adults / 2) + ceiling(household$children / 2)
    bedrooms <- pmax(
      0L,
      ceiling(household$persons / 1.7) + as.integer(key %% 3L) - 1L
    )
    balance <- pmin(pmax(bedrooms - required, -4L), 4L)
    return(sprintf("%02d", 5L + balance))
  }

  if (upper == "SAFD") {
    out <- rep(if (!is.na(not_applicable)) not_applicable else "@", n)
    out[key %% 1000L < 15L] <- "1"
    return(out)
  }

  if (upper == "SVFP" && year == 2021L) {
    return(ifelse(age >= 15L & age <= 64L, "1", "@"))
  }

  if (upper == "AGE5P" && year == 2021L) {
    return(sprintf("%02d", pmin(1L + age %/% 5L, 21L)))
  }
  if (upper %in% c("IFAGEP", "IFSEXP", "SXFP")) {
    codes <- if (upper == "IFSEXP") c("01", "02") else c("1", "2")
    if (nrow(frame)) {
      codes <- unique(frame$code[frame$value_kind == "substantive"])
    }
    return(.dil_census_weighted_pick(key, codes, c(95.5, 4.5)))
  }
  if (upper == "IFMSTP") {
    out <- .dil_census_weighted_pick(key, c("1", "2"), c(94.5, 5.5))
    out[age < 15L & !is.na(not_applicable)] <- not_applicable
    return(out)
  }
  if (upper == "IFNMFD") {
    codes <- unique(frame$code[frame$value_kind == "substantive"])
    weights <- c(
      96,
      rep(4 / pmax(length(codes) - 1L, 1L), length(codes) - 1L)
    )
    return(.dil_census_weighted_pick(key, codes, weights))
  }
  if (upper == "IFPURP") {
    codes <- unique(frame$code[frame$value_kind == "substantive"])
    return(.dil_census_weighted_pick(key, codes, c(93, 4, 2, 1)))
  }
  if (upper == "IFPOWP") {
    codes <- unique(frame$code[frame$value_kind == "substantive"])
    out <- .dil_census_weighted_pick(
      key, codes, c(94, rep(6 / pmax(length(codes) - 1L, 1L), length(codes) - 1L))
    )
    if (!is.na(not_applicable)) out[!employed] <- not_applicable
    return(out)
  }

  if (upper %in% c("ANC1P", "ANC2P")) {
    patterns <- c(
      "^Australian$", "^English$", "^Irish$", "^Scottish$", "Chinese",
      "Indian", "Italian", "German", "Aboriginal", "Torres Strait"
    )
    codes <- vapply(
      patterns, function(pattern) .dil_census_label_code(frame, pattern),
      character(1)
    )
    keep <- !is.na(codes)
    out <- .dil_census_weighted_pick(
      key, codes[keep], c(35, 30, 8, 7, 6, 4, 4, 3, 2, 1)[keep]
    )
    if (upper == "ANC1P" && any(indigenous %in% 2:4)) {
      aboriginal <- .dil_census_label_code(frame, "Aboriginal")
      torres <- .dil_census_label_code(frame, "Torres Strait")
      if (!is.na(aboriginal)) out[indigenous %in% c(2L, 4L)] <- aboriginal
      if (!is.na(torres)) out[indigenous == 3L] <- torres
    }
    if (upper == "ANC2P" && !is.na(not_applicable)) {
      out[key %% 100L < 35L] <- not_applicable
    }
    return(out)
  }
  if (upper == "ANCRP" && year == 2021L) {
    return(ifelse(key %% 100L < 79L, "1", "2"))
  }

  if (upper == "ADCP" && year == 2021L) {
    out <- rep("8", n)
    service <- age >= 18L & key %% 1000L < 35L
    out[service] <- as.character(1L + key[service] %% 7L)
    out[age < 15L] <- not_applicable
    return(out)
  }
  if (upper %in% c("C3SP", "YR12C2P", "YR12C3P") && year == 2021L) {
    attained <- switch(
      upper,
      C3SP = education >= 3L,
      YR12C2P = education >= 2L,
      YR12C3P = education >= 3L
    )
    out <- ifelse(attained, "1", "2")
    out[age < 15L] <- not_applicable
    return(out)
  }
  if (upper == "WTNSQP" && year == 2021L) {
    out <- ifelse(
      education >= 2L,
      ifelse(age <= 30L & key %% 10L < 2L, "3", "1"),
      ifelse(age <= 30L & key %% 10L < 2L, "2", "4")
    )
    out[age < 15L] <- not_applicable
    return(out)
  }

  unemployed <- age >= 15L & !employed & key %% 10L < 2L
  in_labour_force <- employed | unemployed
  if (upper %in% c("EMFP", "LFFP", "UEFP", "LFHRP") && year == 2021L) {
    out <- switch(
      upper,
      EMFP = ifelse(employed, "1", "2"),
      LFFP = ifelse(in_labour_force, "1", "2"),
      UEFP = ifelse(unemployed, "1", ifelse(employed, "2", "@")),
      LFHRP = ifelse(
        employed,
        ifelse(hours >= 35L, "1", ifelse(hours > 0L, "2", "3")),
        ifelse(unemployed, ifelse(key %% 2L == 0L, "5", "6"), "7")
      )
    )
    out[age < 15L] <- not_applicable
    return(out)
  }
  if (upper == "EETP" && year == 2021L) {
    studying <- age >= 15L & age <= 24L & education > 0L & key %% 4L < 2L
    out <- ifelse(
      employed & studying, ifelse(hours >= 35L, "11", "14"),
      ifelse(studying, "12", ifelse(employed, "13", "41"))
    )
    out[age < 15L] <- not_applicable
    return(out)
  }
  if (upper %in% c("OCSKP", "OCSKEV1P") && year == 2021L) {
    skill <- .dil_census_spine_integer(spine_rows, "skill_level", 3L)
    out <- as.character(pmin(pmax(skill, 1L), 5L))
    out[!employed] <- not_applicable
    return(out)
  }
  if (upper == "OCCEV1P" && year == 2021L) {
    occupation <- .dil_census_spine_integer(spine_rows, "anzsco_code", 0L)
    out <- sprintf("%06d", pmin(pmax(occupation, 0L), 999999L))
    length_required <- .dil_census_data_length(year, upper)
    if (is.na(length_required)) length_required <- 6L
    out[!employed | occupation <= 0L] <- strrep("@", length_required)
    return(out)
  }
  if (upper %in% c("MTW06P", "MTW15P") && year == 2021L) {
    if (upper == "MTW06P") {
      out <- .dil_census_weighted_pick(
        key, as.character(1:5), c(12, 69, 7, 3, 9)
      )
    } else {
      out <- .dil_census_weighted_pick(
        key,
        sprintf("%02d", 1:14),
        c(4, 4, 1, 2, 1, 58, 5, 2, 1, 2, 1, 4, 7, 8)
      )
    }
    out[!employed] <- not_applicable
    return(out)
  }

  if (upper == "YARRP") {
    out <- ifelse(
      arrival <= 1950L, "1",
      ifelse(arrival <= 1960L, "2",
        ifelse(arrival <= 1970L, "3",
          ifelse(arrival <= 1980L, "4",
            ifelse(arrival <= 1990L, "5",
              ifelse(arrival <= 2000L, "6",
                ifelse(arrival <= 2010L, "7",
                  ifelse(arrival <= 2020L, "8", "9")
                )
              )
            )
          )
        )
      )
    )
    australia <- .dil_census_spine_integer(
      spine_rows, "country_of_birth_sacc", 1101L
    ) == 1101L
    out[australia] <- not_applicable
    return(out)
  }

  if (upper %in% c("HLTHP", "HOLHP") && year == 2021L) {
    severity <- .dil_census_spine_integer(
      spine_rows, "disability_severity", 9L
    )
    has_condition <- severity <= 3L |
      key %% 100L < pmin(45L, pmax(4L, age %/% 2L))
    if (upper == "HLTHP") return(ifelse(has_condition, "122", "121"))
    return(ifelse(has_condition & key %% 4L == 0L, "111", "112"))
  }
  comorbidity <- c(
    "COARASP", "COARDBP", "COARHDP", "COARMHP", "COASHDP", "COASLCP",
    "COCNHDP", "CODBHDP", "CODBKDP", "COHDKDP", "COHDMHP", "COLCMHP"
  )
  if (upper %in% comorbidity && year == 2021L) {
    prevalence <- pmin(25L, pmax(1L, (age - 20L) %/% 5L))
    return(ifelse(key %% 100L < prevalence, "1", "2"))
  }

  household <- .dil_census_household_context(spine_rows, key)
  count_rules <- c(
    CACF = "children", CDCUF = "children", CDSF = "students",
    CNDCF = "nondependent", CPRF = "persons", CPAD = "absent",
    CPAF = "absent", CDCAF = "absent", CDSAF = "absent", CNDAF = "absent"
  )
  if (upper %in% names(count_rules) && year == 2021L) {
    kind <- count_rules[[upper]]
    count <- switch(
      kind,
      children = household$children,
      students = ifelse(household$children > 0L & key %% 4L == 0L, 1L, 0L),
      nondependent = ifelse(household$persons >= 4L & key %% 6L == 0L, 1L, 0L),
      persons = household$persons,
      absent = ifelse(key %% 20L == 0L, 1L, 0L)
    )
    if (upper %in% c("CDCUF", "CDSF", "CNDCF")) {
      family_offset <- ifelse(household$one_parent, 7L, 0L)
      return(sprintf("%02d", pmin(count, 6L) + family_offset))
    }
    return(as.character(pmin(count, 6L)))
  }
  if (upper == "CDCF" && year == 2021L) {
    family_offset <- ifelse(household$one_parent, 7L, 0L)
    return(sprintf("%02d", pmin(household$children, 6L) + family_offset))
  }
  if (upper == "FNOF") {
    codes <- unique(frame$code[frame$value_kind == "substantive"])
    weights <- if (length(codes) == 2L) c(96, 4) else c(96, 3.5, 0.5)
    return(.dil_census_weighted_pick(key, codes, weights))
  }
  if (upper == "INGF" && year == 2021L) {
    return(ifelse(indigenous %in% 2:4, "1", "2"))
  }
  if (upper == "SSCF" && year == 2021L) {
    out <- ifelse(key %% 1000L < 8L, "1", ifelse(key %% 1000L < 16L, "2", "3"))
    out[!household$couple] <- not_applicable
    return(out)
  }
  if (upper == "LFSF" && year == 2021L) {
    out <- ifelse(
      household$one_parent,
      ifelse(employed, ifelse(hours >= 35L, "22", "23"), "26"),
      ifelse(employed, ifelse(hours >= 35L, "01", "07"), "19")
    )
    out[household$persons < 2L] <- not_applicable
    return(out)
  }

  if (upper %in% c("BEDD", "VEHD") && year == 2021L) {
    count <- if (upper == "BEDD") {
      pmin(pmax(1L, ceiling(household$persons / 1.7)), 6L)
    } else {
      pmin(ifelse(household$persons == 1L, key %% 2L, 1L + key %% 3L), 6L)
    }
    width <- .dil_census_data_length(year, upper)
    return(sprintf(paste0("%0", width, "d"), count))
  }
  if (upper %in% c("CALTHD", "CCLTHD", "CPLTHD", "CPLTHRD") &&
      year == 2021L) {
    risk <- pmin(0.65, 0.04 + age / 180)
    affected <- as.integer(key %% 1000L < 1000L * risk)
    count <- switch(
      upper,
      CALTHD = pmin(affected, household$adults),
      CCLTHD = pmin(affected, household$children),
      CPLTHD = pmin(affected, household$persons),
      CPLTHRD = pmin(affected, household$persons)
    )
    if (upper == "CPLTHD") return(sprintf("%02d", count))
    out <- as.character(count)
    if (upper == "CCLTHD") out[household$children == 0L] <- not_applicable
    return(out)
  }
  if (upper %in% c("MAID", "RAID") && year == 2021L) {
    applicable <- if (upper == "MAID") household$tenure == 2L else household$tenure == 4L
    share <- if (upper == "MAID") {
      (700 + key %% 2600L) / pmax(income_annual / 12, 1)
    } else {
      (180 + key %% 650L) / pmax(income_weekly, 1)
    }
    out <- ifelse(share <= 0.30, "1", "2")
    out[!applicable] <- not_applicable
    return(out)
  }

  if (upper %in% c("FINASF", "FINF", "HIED", "HINASD", "HIND")) {
    scale <- if (grepl("^F", upper)) household$persons * 0.75 else household$persons * 0.65
    return(.dil_census_income_code(income_weekly * scale, frame))
  }
  if (upper %in% c("FIDF", "HIDD")) {
    all_stated <- .dil_census_label_code(frame, "All incomes stated")
    if (!is.na(all_stated)) return(rep(all_stated, n))
  }

  .dil_census_generic_code(frame, key, year, upper)
}

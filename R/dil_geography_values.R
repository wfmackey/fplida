# Official geography values for canonical DIL tables.

.dil_lga_codeframe_cache <- new.env(parent = emptyenv())

.dil_load_lga_codeframe <- function() {
  if (!is.null(.dil_lga_codeframe_cache$data)) {
    return(.dil_lga_codeframe_cache$data)
  }
  path <- system.file(
    "extdata", "codeframes", "lga.tsv", package = "fplida"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "codeframes", "lga.tsv")
  }
  if (!file.exists(path)) {
    stop("LGA codeframe not found: ", path, call. = FALSE)
  }
  values <- utils::read.delim(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE
  )
  required <- c("year", "code", "name", "state", "source_url")
  missing <- setdiff(required, names(values))
  if (length(missing)) {
    stop("LGA codeframe is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  values$year <- as.integer(values$year)
  values <- values[
    values$state %in% as.character(1:8) &
      grepl("^[1-8][0-9]{4}$", values$code) &
      !grepl(
        "No usual address|Migratory|Offshore|Shipping|Outside Australia",
        values$name,
        ignore.case = TRUE
      ),
    ,
    drop = FALSE
  ]
  stopifnot(
    nrow(values) > 3000L,
    !anyDuplicated(paste(values$year, values$code, sep = "\r"))
  )
  .dil_lga_codeframe_cache$data <- values
  values
}

.dil_lga_reference_year <- function(name, period) {
  upper <- toupper(name)
  explicit <- regmatches(
    upper,
    regexpr("20(?:11|16|21|22|23|24)", upper, perl = TRUE)
  )
  if (length(explicit) && nzchar(explicit)) return(as.integer(explicit))
  short <- regmatches(
    upper,
    regexpr(
      "(?:LGA[^0-9]*|_)(?:11|16|21|22|23|24)(?:[^0-9]|$)",
      upper,
      perl = TRUE
    )
  )
  if (length(short) && nzchar(short)) {
    digits <- sub(
      ".*(11|16|21|22|23|24).*",
      "\\1",
      short,
      perl = TRUE
    )
    return(as.integer(paste0("20", digits)))
  }
  suffix <- regmatches(
    upper,
    regexpr("_(?:11|16|21|22|23|24)(?:_|$)", upper, perl = TRUE)
  )
  if (length(suffix) && nzchar(suffix)) {
    return(as.integer(paste0("20", gsub("_", "", suffix))))
  }
  end_year <- as.integer(period$end_year)
  if (end_year < 2016L) return(2011L)
  if (end_year < 2021L) return(2016L)
  min(end_year, 2024L)
}

.dil_lga_value <- function(name, spine_rows, seed, period) {
  upper <- toupper(name)
  if (!grepl("LGA", upper, fixed = TRUE)) return(NULL)
  n <- nrow(spine_rows)
  if (!n) return(character())
  if (grepl("PUBLIC|PUBLISH", upper)) return(rep(1L, n))

  year <- .dil_lga_reference_year(upper, period)
  codeframe <- .dil_load_lga_codeframe()
  codeframe <- codeframe[codeframe$year == year, , drop = FALSE]
  if (!nrow(codeframe)) return(NULL)

  state <- if ("state" %in% names(spine_rows)) {
    as.integer(spine_rows$state)
  } else if ("state_code" %in% names(spine_rows)) {
    as.integer(spine_rows$state_code)
  } else {
    rep(NA_integer_, n)
  }
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed(paste("LGA", year, sep = "|"))
  )
  state[is.na(state) | !state %in% 1:8] <-
    1L + as.integer(key[is.na(state) | !state %in% 1:8] %% 8L)

  code <- character(n)
  label <- character(n)
  for (state_value in 1:8) {
    rows <- which(state == state_value)
    if (!length(rows)) next
    choices <- codeframe[codeframe$state == as.character(state_value),
                         , drop = FALSE]
    index <- 1L + as.integer(key[rows] %% nrow(choices))
    code[rows] <- choices$code[index]
    label[rows] <- choices$name[index]
  }
  if (grepl("NAME|NM", upper)) return(label)
  census_fields <- c(
    "LGAD", "LGAP", "LGA22D", "LGAP22", "LGAU1P", "LGAU5P",
    "POWLGA", "POWLGAP22"
  )
  if (!upper %in% census_fields) return(code)
  out <- paste0("LGA", code)
  if (upper %in% c("POWLGA", "POWLGAP22") &&
      "baseline_employed" %in% names(spine_rows)) {
    out[as.integer(spine_rows$baseline_employed) != 1L] <- "@@@@@@@@"
  }
  if (upper %in% c("LGAU1P", "LGAU5P") &&
      "birth_year" %in% names(spine_rows)) {
    minimum_age <- if (upper == "LGAU1P") 1L else 5L
    age <- as.integer(period$end_year) - as.integer(spine_rows$birth_year)
    out[is.na(age) | age < minimum_age] <- "@@@@@@@@"
  }
  out
}

.dil_area_codeframe <- function(file, minimum_rows) {
  key <- paste0("area_", file)
  if (!is.null(.dil_lga_codeframe_cache[[key]])) {
    return(.dil_lga_codeframe_cache[[key]])
  }
  path <- system.file(
    "extdata", "codeframes", file, package = "fplida"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "codeframes", file)
  }
  values <- utils::read.delim(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE
  )
  stopifnot(
    nrow(values) >= minimum_rows,
    all(c("code", "name", "state", "source_url") %in% names(values))
  )
  if ("year" %in% names(values)) values$year <- as.integer(values$year)
  .dil_lga_codeframe_cache[[key]] <- values
  values
}

.dil_pick_area_value <- function(values, name, spine_rows, seed, salt) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  if (!n) return(character())
  state <- if ("state" %in% names(spine_rows)) {
    as.integer(spine_rows$state)
  } else if ("state_code" %in% names(spine_rows)) {
    as.integer(spine_rows$state_code)
  } else {
    rep(NA_integer_, n)
  }
  key <- .dil_numeric_key(spine_rows, seed, .stable_name_seed(salt))
  invalid <- is.na(state) | !state %in% 1:8
  state[invalid] <- 1L + as.integer(key[invalid] %% 8L)
  code <- character(n)
  label <- character(n)
  for (state_value in 1:8) {
    rows <- which(state == state_value)
    if (!length(rows)) next
    choices <- values[values$state == as.character(state_value), , drop = FALSE]
    index <- 1L + as.integer(key[rows] %% nrow(choices))
    code[rows] <- choices$code[index]
    label[rows] <- choices$name[index]
  }
  if (grepl("NAME|NM", upper)) label else code
}

.dil_ireg_value <- function(name, spine_rows, seed) {
  upper <- toupper(name)
  if (!grepl("IREG", upper, fixed = TRUE)) return(NULL)
  if (grepl("PUBLIC|PUBLISH", upper)) return(rep(1L, nrow(spine_rows)))
  year <- if (grepl("(?:_|P)11(?:_|$)", upper, perl = TRUE)) {
    2011L
  } else if (grepl("(?:_|P)16(?:_|$)", upper, perl = TRUE)) {
    2016L
  } else {
    2021L
  }
  values <- .dil_area_codeframe("ireg.tsv", 100L)
  values <- values[
    values$year == year & values$state %in% as.character(1:8) &
      grepl("^[1-8][0-9]{2}$", values$code) &
      !grepl(
        "No usual address|Migratory|Offshore|Shipping|Outside Australia",
        values$name,
        ignore.case = TRUE
      ),
    ,
    drop = FALSE
  ]
  value <- .dil_pick_area_value(
    values, upper, spine_rows, seed, paste("IREG", year, sep = "|")
  )
  if (upper %in% c("IREGD", "IREGP")) paste0("IREG", value) else value
}

.dil_phn_value <- function(name, spine_rows, seed) {
  upper <- toupper(name)
  if (!grepl("PHN", upper, fixed = TRUE)) return(NULL)
  values <- .dil_area_codeframe("phn.tsv", 31L)
  .dil_pick_area_value(values, upper, spine_rows, seed, "PHN|2015")
}

.dil_admin_geography_value <- function(name, spine_rows, seed, period) {
  value <- .dil_lga_value(name, spine_rows, seed, period)
  if (!is.null(value)) return(value)
  value <- .dil_ireg_value(name, spine_rows, seed)
  if (!is.null(value)) return(value)
  .dil_phn_value(name, spine_rows, seed)
}

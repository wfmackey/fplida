#' Generate CORE dataset (Demographics, Locations, Relationships, Core Scope)
#'
#' Projects the three CORE sub-tables from the fplida spine. CORE is the
#' PLIDA spine-level infrastructure combining demographic, location, and
#' relationship information from multiple administrative sources.
#'
#' We generate the Combined (cb) 2021 Census version of each sub-table:
#' \itemize{
#'   \item \strong{Demographics}: one row per person --birth, gender, death
#'   \item \strong{Locations}: one row per person --current address with
#'         real ASGS 2021 SA1/SA2/SA4 codes
#'   \item \strong{Relationships}: one row per relationship pair --partner
#'         and parent-child links
#' }
#'
#' @section Dataset and variable information:
#' The [ABS PLIDA Modular Product](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/person-level-integrated-data-asset-plida)
#' website gives information about this dataset. Use `dataset_info("CORE")` for
#' dataset information. Use `variable_info("CORE")` for variables, sources,
#' value support, and topic tags.
#'
#' @param spine Data.frame (from \code{generate_spine()}) or NULL. If NULL,
#'   the most recent spine is loaded from the run directory.
#' @param seed Integer. Random seed for CORE-specific generation.
#' @param years Integer vector. Calendar years for the monthly
#'   \code{core_residence} person-month product.
#' @param output_dir Character or NULL. Base output directory. If NULL, uses
#'   \code{get_data_path()}. One of the two must be set.
#' @param format Character. Output format: "parquet" (default) or "csv".
#' @param return_data Logical. If TRUE (default), return data.frames in memory;
#'   if FALSE, write to disk and return metadata only.
#'
#' @return A named list with three data.frames:
#'   \describe{
#'     \item{demographics}{Person-level demographics (SPINE_ID, birth, gender, death)}
#'     \item{locations}{Person-level current address (SPINE_ID, state, SA4/SA2/SA1)}
#'     \item{relationships}{Pair-level relationships (partner + parent-child)}
#'   }
#'
#' @examples
#' \dontrun{
#' spine <- generate_spine(n = 1000L, seed = 1L)
#' core <- generate_core(spine = spine, seed = 1L)
#' str(core$demographics)
#' }
#'
#' @export
generate_core <- function(spine = NULL, seed = 42L, output_dir = NULL,
                          years = 2006L:2025L,
                          format = c("parquet", "csv"),
                          return_data = TRUE) {
  seed <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  return_data <- as.logical(return_data)
  stopifnot("`seed` must be an integer" = !is.na(seed))
  stopifnot("`years` must contain at least one year" = length(years) > 0L)

  run_dir <- resolve_run_dir(output_dir)

  # ---- Selective spine loading (memory-efficient) ----
  core_cols <- c("spine_id", "aeuid_abs", "birth_year", "sex",
                 "country_of_birth", "country_of_birth_sacc", "state",
                 "sa2_code",
                 "household_id", "month_of_birth", "year_of_arrival",
                 "year_of_death", "month_of_death", "day_of_death",
                 "residence_seed")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, core_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))

  # Mini spine for agency spine writing (spine_id + aeuid_abs only)
  mini_spine <- data.frame(
    spine_id  = spine$spine_id,
    aeuid_abs = spine$aeuid_abs,
    stringsAsFactors = FALSE
  )

  if (return_data) {
    # === Original path: build all, write all, return all ===
    demographics  <- project_core_demographics(spine, seed)
    vitals        <- project_core_vitals(spine, demographics)
    locations     <- project_core_locations(spine, seed)
    relationships <- project_core_relationships(spine, seed)
    residence     <- project_core_residence(spine, years)

    write_product(demographics, core_product_name("demographics"),
                  "CORE", run_dir, format)
    write_product(vitals, core_product_name("vitals"),
                  "CORE", run_dir, format)
    write_product(locations, core_product_name("locations"),
                  "CORE", run_dir, format)
    write_product(relationships, core_product_name("relationships"),
                  "CORE", run_dir, format)
    write_product(residence, core_product_name("residence"),
                  "CORE", run_dir, format)

    ds_dir <- dataset_dir(run_dir, "CORE")
    write_agency_spine(mini_spine, "ABS", ds_dir, format = format)

    list(
      demographics  = demographics,
      vitals        = vitals,
      locations     = locations,
      relationships = relationships,
      residence     = residence
    )
  } else {
    # === Memory-efficient path: sequential build->write->free ===
    demographics <- project_core_demographics(spine, seed)
    n_demo <- nrow(demographics)
    write_product(demographics, core_product_name("demographics"),
                  "CORE", run_dir, format)
    vitals <- project_core_vitals(spine, demographics)
    n_vitals <- nrow(vitals)
    write_product(vitals, core_product_name("vitals"),
                  "CORE", run_dir, format)
    rm(demographics); gc()
    rm(vitals); gc()

    locations <- project_core_locations(spine, seed)
    n_loc <- nrow(locations)
    write_product(locations, core_product_name("locations"),
                  "CORE", run_dir, format)
    rm(locations); gc()

    relationships <- project_core_relationships(spine, seed)
    n_rel <- nrow(relationships)
    write_product(relationships, core_product_name("relationships"),
                  "CORE", run_dir, format)
    rm(relationships); gc()

    n_residence <- write_core_residence(spine, years, run_dir, format)

    if (spine_loaded) { rm(spine); gc() }

    ds_dir <- dataset_dir(run_dir, "CORE")
    write_agency_spine(mini_spine, "ABS", ds_dir, format = format)

    invisible(list(
      n_demographics  = n_demo,
      n_vitals        = n_vitals,
      n_locations     = n_loc,
      n_relationships = n_rel,
      n_residence     = n_residence,
      path            = "abs-core"
    ))
  }
}


# -- Core Demographics --------------------------------------------------------

# Annual mortality probability by 8 age bands.
# Source: ABS 3302.0 Deaths, Australia 2022
# Bands: <20, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+
.CORE_MORTALITY <- c(0.0003, 0.0005, 0.0007, 0.0012, 0.0030, 0.0070,
                     0.0170, 0.0550)

# Top overseas SACC codes (from core.toml)
.CORE_OVERSEAS_CODES <- c(
  "2100", "7100", "6100", "5101", "5201", "5203", "5105", "6102",
  "7103", "6104", "3206", "3103", "2201", "7105", "3207", "5204",
  "7108", "6105", "3104", "2304"
)
.CORE_OVERSEAS_WEIGHTS <- c(
  927490, 673354, 549628, 530491, 293899, 257997, 189204, 165605,
  163329, 131907, 122507, 102087, 101309, 101256, 100158, 92925,
  92305, 89636, 87343, 87068
)


#' Project Core Demographics from the spine
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @return data.frame with CORE demographics columns.
#' @keywords internal
project_core_demographics <- function(spine_df, seed) {
  vital_cols <- c("month_of_birth", "year_of_death",
                  "month_of_death", "day_of_death")
  if (all(vital_cols %in% names(spine_df))) {
    return(data.frame(
      SPINE_ID        = spine_df$spine_id,
      YEAR_OF_BIRTH   = spine_df$birth_year,
      MONTH_OF_BIRTH  = spine_df$month_of_birth,
      BIRTH_CTRY_CODE = as.character(spine_df$country_of_birth_sacc),
      CORE_GENDER     = ifelse(spine_df$sex == 1L, "M", "F"),
      YEAR_OF_DEATH   = spine_df$year_of_death,
      MONTH_OF_DEATH  = spine_df$month_of_death,
      DAY_OF_DEATH    = spine_df$day_of_death,
      stringsAsFactors = FALSE
    ))
  }

  # Try Rust implementation first
  if (exists("project_core_demographics__", mode = "function")) {
    raw <- project_core_demographics__(
      spine_id              = as.character(spine_df$spine_id),
      birth_year            = as.integer(spine_df$birth_year),
      sex                   = as.integer(spine_df$sex),
      country_of_birth_sacc = as.integer(spine_df$country_of_birth_sacc),
      seed                  = as.integer(seed)
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }

  n <- nrow(spine_df)

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 700L)

  age <- 2021L - spine_df$birth_year

  # MONTH_OF_BIRTH: uniform 1-12
  month_of_birth <- sample.int(12L, n, replace = TRUE)

  # BIRTH_CTRY_CODE: SACC codes
  birth_ctry_code <- character(n)
  aus <- spine_df$country_of_birth == 0L
  birth_ctry_code[aus] <- "1101"
  overseas_idx <- which(!aus)
  if (length(overseas_idx) > 0L) {
    birth_ctry_code[overseas_idx] <- sample(
      .CORE_OVERSEAS_CODES,
      length(overseas_idx),
      replace = TRUE,
      prob = .CORE_OVERSEAS_WEIGHTS
    )
  }

  # CORE_GENDER
  core_gender <- ifelse(spine_df$sex == 1L, "M", "F")

  # Death: cumulative mortality over ~15 year observation window (2006-2021)
  # Use age-band-specific annual rate, applied over 15 years
  band <- age_band_8(age)
  annual_rate <- .CORE_MORTALITY[band]
  # P(died in 15yr window) = 1 - (1 - annual_rate)^15
  p_dead <- 1.0 - (1.0 - annual_rate)^15
  is_dead <- runif(n) < p_dead

  year_of_death  <- rep(NA_integer_, n)
  month_of_death <- rep(NA_integer_, n)
  day_of_death   <- rep(NA_integer_, n)

  dead_idx <- which(is_dead)
  if (length(dead_idx) > 0L) {
    year_of_death[dead_idx]  <- sample(2006L:2024L, length(dead_idx),
                                       replace = TRUE)
    month_of_death[dead_idx] <- sample.int(12L, length(dead_idx),
                                           replace = TRUE)
    day_of_death[dead_idx]   <- sample.int(28L, length(dead_idx),
                                           replace = TRUE)
  }

  data.frame(
    SPINE_ID        = spine_df$spine_id,
    YEAR_OF_BIRTH   = spine_df$birth_year,
    MONTH_OF_BIRTH  = month_of_birth,
    BIRTH_CTRY_CODE = birth_ctry_code,
    CORE_GENDER     = core_gender,
    YEAR_OF_DEATH   = year_of_death,
    MONTH_OF_DEATH  = month_of_death,
    DAY_OF_DEATH    = day_of_death,
    stringsAsFactors = FALSE
  )
}


# -- Core Scope Vitals --------------------------------------------------------

#' Project Core Vitals from the spine
#' @param spine_df data.frame from generate_spine().
#' @param demographics Optional CORE demographics frame.
#' @return data.frame with core_vitals columns.
#' @keywords internal
project_core_vitals <- function(spine_df, demographics = NULL) {
  vital_cols <- c("month_of_birth", "year_of_death",
                  "month_of_death", "day_of_death")
  if (all(vital_cols %in% names(spine_df))) {
    return(data.frame(
      spine_id       = spine_df$spine_id,
      month_of_birth = as.integer(spine_df$month_of_birth),
      year_of_birth  = as.integer(spine_df$birth_year),
      day_of_death   = as.integer(spine_df$day_of_death),
      month_of_death = as.integer(spine_df$month_of_death),
      year_of_death  = as.integer(spine_df$year_of_death),
      stringsAsFactors = FALSE
    ))
  }

  if (is.null(demographics)) {
    demographics <- project_core_demographics(spine_df, seed = 42L)
  }
  data.frame(
    spine_id       = demographics$SPINE_ID,
    month_of_birth = as.integer(demographics$MONTH_OF_BIRTH),
    year_of_birth  = as.integer(demographics$YEAR_OF_BIRTH),
    day_of_death   = as.integer(demographics$DAY_OF_DEATH),
    month_of_death = as.integer(demographics$MONTH_OF_DEATH),
    year_of_death  = as.integer(demographics$YEAR_OF_DEATH),
    stringsAsFactors = FALSE
  )
}


# -- Core Scope Residence -----------------------------------------------------

#' Project Core Residence from the spine
#' @param spine_df data.frame from generate_spine().
#' @param years Integer vector.
#' @return data.frame with core_residence columns.
#' @keywords internal
project_core_residence <- function(spine_df, years = 2006L:2025L) {
  years <- as.integer(years)
  months <- .core_month_periods(years)
  out <- vector("list", length(months))

  for (i in seq_along(months)) {
    yr <- as.integer(format(months[[i]], "%Y"))
    mo <- as.integer(format(months[[i]], "%m"))
    out[[i]] <- data.frame(
      spine_id = spine_df$spine_id,
      pp_period = .month_end(months[[i]]),
      pp_weight = .core_pp_weight_r(spine_df, yr, mo),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, out)
}

write_core_residence <- function(spine_df, years, run_dir, format) {
  years <- as.integer(years)
  product_name <- core_product_name("residence")
  ds_dir <- dataset_dir(run_dir, "CORE")

  if (format == "parquet" &&
      exists("project_core_residence_to_parquet__", mode = "function")) {
    project_core_residence_to_parquet__(
      spine_id         = as.character(spine_df$spine_id),
      birth_year       = as.integer(spine_df$birth_year),
      month_of_birth   = as.integer(spine_df$month_of_birth),
      country_of_birth = as.integer(spine_df$country_of_birth),
      year_of_arrival  = as.integer(spine_df$year_of_arrival),
      year_of_death    = as.integer(spine_df$year_of_death),
      month_of_death   = as.integer(spine_df$month_of_death),
      day_of_death     = as.integer(spine_df$day_of_death),
      residence_seed   = as.integer(spine_df$residence_seed),
      min_year         = min(years),
      max_year         = max(years),
      out_dir          = ds_dir,
      product_name     = product_name
    )
    return(as.numeric(nrow(spine_df)) * length(.core_month_periods(years)))
  }

  residence <- project_core_residence(spine_df, years)
  n_residence <- nrow(residence)
  write_product(residence, product_name, "CORE", run_dir, format)
  rm(residence); gc()
  n_residence
}

.core_month_periods <- function(years) {
  years <- range(as.integer(years))
  seq.Date(as.Date(sprintf("%d-01-01", years[1L])),
           as.Date(sprintf("%d-12-01", years[2L])),
           by = "month")
}

.month_end <- function(month_start) {
  next_month <- seq.Date(month_start, by = "month", length.out = 2L)[2L]
  next_month - 1L
}

.core_pp_weight_r <- function(spine_df, year, month) {
  n <- nrow(spine_df)
  w <- rep(1, n)
  mob <- if ("month_of_birth" %in% names(spine_df)) {
    as.integer(spine_df$month_of_birth)
  } else {
    rep(1L, n)
  }
  before_birth <- year < spine_df$birth_year |
    (year == spine_df$birth_year & month < mob)
  w[before_birth] <- 0

  yoa <- if ("year_of_arrival" %in% names(spine_df)) {
    as.integer(spine_df$year_of_arrival)
  } else {
    rep(NA_integer_, n)
  }
  overseas <- as.integer(spine_df$country_of_birth) != 0L
  before_arrival <- overseas & !is.na(yoa) & year < yoa
  w[before_arrival] <- 0

  yod <- if ("year_of_death" %in% names(spine_df)) {
    as.integer(spine_df$year_of_death)
  } else {
    rep(NA_integer_, n)
  }
  mod <- if ("month_of_death" %in% names(spine_df)) {
    as.integer(spine_df$month_of_death)
  } else {
    rep(NA_integer_, n)
  }
  dod <- if ("day_of_death" %in% names(spine_df)) {
    as.integer(spine_df$day_of_death)
  } else {
    rep(NA_integer_, n)
  }
  after_death <- !is.na(yod) & (year > yod | (year == yod & month > mod))
  w[after_death] <- 0

  eligible <- w > 0
  seed <- if ("residence_seed" %in% names(spine_df)) {
    as.numeric(spine_df$residence_seed)
  } else {
    seq_len(n)
  }
  draw <- (abs(sin(seed * 12.9898 + year * 78.233 + month * 37.719)) *
             100000) %% 1
  full_prob <- ifelse(overseas, 0.012, 0.005)
  part_prob <- ifelse(overseas, 0.025, 0.012)
  w[eligible & draw < full_prob] <- 0

  partial <- eligible & draw >= full_prob & draw < full_prob + part_prob
  if (any(partial)) {
    dim <- as.integer(format(.month_end(as.Date(sprintf("%d-%02d-01",
                                                        year, month))), "%d"))
    draw2 <- (abs(sin(seed[partial] * 93.989 + year * 17.37 + month * 9.91)) *
                100000) %% 1
    present_days <- 1L + floor(draw2 * (dim - 1L))
    w[partial] <- present_days / dim
  }

  death_month <- !is.na(yod) & year == yod & month == mod & w > 0
  if (any(death_month)) {
    dim <- as.integer(format(.month_end(as.Date(sprintf("%d-%02d-01",
                                                        year, month))), "%d"))
    w[death_month] <- pmin(w[death_month], pmax(1L, dod[death_month]) / dim)
  }
  round(w, 6)
}


# -- Core Locations -----------------------------------------------------------

# SA1 lookup loaded once per session (lazy).
.sa1_lookup_env <- new.env(parent = emptyenv())
.mb_lookup_env <- new.env(parent = emptyenv())

#' Load SA1 -> SA2 -> SA4 -> state lookup (cached)
#' @return data.frame with sa1_code, sa2_code, sa4_code, state.
#' @keywords internal
.load_sa1_lookup <- function() {
  if (!is.null(.sa1_lookup_env$data)) return(.sa1_lookup_env$data)

  csv_path <- system.file("extdata", "sa1_lookup.csv", package = "fplida")
  if (!nzchar(csv_path)) {
    stop("SA1 lookup not found. Reinstall fplida or run ",
         "Rscript scripts/generate_sa1_lookup.R", call. = FALSE)
  }
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$sa4_code <- suppressWarnings(as.integer(df$sa4_code))
  df$state <- suppressWarnings(as.integer(df$state))
  .sa1_lookup_env$data <- df
  df
}

#' Load MB -> SA1 -> SA2 -> SA4 -> state lookup (cached)
#' @return data.frame with mb_code, sa1_code, sa2_code, sa4_code, state.
#' @keywords internal
.load_mb_lookup <- function() {
  if (!is.null(.mb_lookup_env$data)) return(.mb_lookup_env$data)

  csv_path <- system.file("extdata", "mb_lookup.csv.gz", package = "fplida")
  if (!nzchar(csv_path)) {
    csv_path <- file.path("inst", "extdata", "mb_lookup.csv.gz")
  }
  if (!file.exists(csv_path)) {
    stop("Mesh Block lookup not found. Reinstall fplida or run ",
         "Rscript data-raw/update_asgs_mb_lookup.R", call. = FALSE)
  }
  con <- gzfile(csv_path, open = "rt")
  on.exit(close(con), add = TRUE)
  df <- read.csv(con, stringsAsFactors = FALSE)

  required <- c("mb_code", "sa1_code", "sa2_code", "sa4_code", "state")
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop("Mesh Block lookup missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  df$sa4_code <- suppressWarnings(as.integer(df$sa4_code))
  df$state <- suppressWarnings(as.integer(df$state))
  df <- df[!is.na(df$state) & df$state %in% 1:8, required, drop = FALSE]
  .mb_lookup_env$data <- df
  df
}


#' Spine SA2 as an integer vector, 0 where absent or unknown
#'
#' CORE addresses are drawn inside the SA2 the spine already assigned, so
#' ASGS 2021 geography agrees with Census (SA2UCP) and every other product
#' that carries the spine SA2. 0 means "no usable SA2" and triggers the
#' state-level fallback.
#' @param spine_df data.frame from generate_spine().
#' @return Integer vector, length nrow(spine_df).
#' @keywords internal
.core_spine_sa2 <- function(spine_df) {
  n <- nrow(spine_df)
  if (!"sa2_code" %in% names(spine_df)) return(rep(0L, n))
  sa2 <- suppressWarnings(as.integer(spine_df$sa2_code))
  sa2[is.na(sa2) | sa2 < 0L] <- 0L
  sa2
}


#' Project Core Locations from the spine
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @return data.frame with CORE location columns.
#' @keywords internal
project_core_locations <- function(spine_df, seed) {
  if (exists("project_core_locations__", mode = "function")) {
    mb_lookup <- .load_mb_lookup()
    raw <- project_core_locations__(
      spine_id         = as.character(spine_df$spine_id),
      state            = as.integer(spine_df$state),
      sa2              = .core_spine_sa2(spine_df),
      lookup_state     = as.integer(mb_lookup$state),
      lookup_mb_code   = as.character(mb_lookup$mb_code),
      lookup_sa1_code  = as.character(mb_lookup$sa1_code),
      lookup_sa2_code  = as.character(mb_lookup$sa2_code),
      lookup_sa4_code  = as.integer(mb_lookup$sa4_code),
      seed             = as.integer(seed)
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }

  n <- nrow(spine_df)

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 701L)

  mb_lookup <- .load_mb_lookup()

  # For each person, sample a real Mesh Block from inside the SA2 the spine
  # already assigned them, and derive the higher ASGS geography from the same
  # ABS allocation row. That keeps CORE's SA2 identical to Census SA2UCP.
  # People with no usable spine SA2 fall back to a draw from their state.
  mb_code   <- character(n)
  sa1_code  <- character(n)
  sa2_code  <- character(n)
  sa4_code  <- integer(n)

  spine_sa2 <- .core_spine_sa2(spine_df)
  lookup_sa2 <- suppressWarnings(as.integer(mb_lookup$sa2_code))
  by_sa2 <- split(seq_len(nrow(mb_lookup)), lookup_sa2)

  take_rows <- function(idx, rows) {
    sampled <- rows[sample.int(length(rows), length(idx), replace = TRUE)]
    mb_code[idx]  <<- mb_lookup$mb_code[sampled]
    sa1_code[idx] <<- mb_lookup$sa1_code[sampled]
    sa2_code[idx] <<- mb_lookup$sa2_code[sampled]
    sa4_code[idx] <<- mb_lookup$sa4_code[sampled]
  }

  matched <- as.character(spine_sa2) %in% names(by_sa2) & spine_sa2 > 0L
  for (code in sort(unique(spine_sa2[matched]))) {
    take_rows(which(matched & spine_sa2 == code), by_sa2[[as.character(code)]])
  }

  for (st in 1:8) {
    idx <- which(!matched & spine_df$state == st)
    if (length(idx) == 0L) next

    state_rows <- which(mb_lookup$state == st)
    if (length(state_rows) == 0L) next

    take_rows(idx, state_rows)
  }

  # Synthetic ARID (address register ID).
  # Was: sample.int(2^31, n, replace = FALSE). At n = 25e6 this forced
  # R into a memory-heavy rejection-sampling path that OOM-killed the
  # main R process during CORE locations on 2026-04-13. replace = TRUE
  # gives ~150 duplicates out of 25M (birthday math), which is fine for
  # simulated address IDs and keeps memory bounded.
  arid <- sprintf("%012X", sample.int(.Machine$integer.max, n, replace = TRUE))

  data.frame(
    SPINE_ID       = spine_df$spine_id,
    STATE          = spine_df$state,
    SA4_ASGS_2021  = sa4_code,
    SA2_ASGS_2021  = sa2_code,
    SA1_ASGS_2021  = sa1_code,
    MB_ASGS_2021   = mb_code,
    ARID           = arid,
    ADR_TYP        = "R",
    SOURCE_FLAG    = "CENSUS",
    START_DATE     = "2006-01-01",
    END_DATE       = NA_character_,
    stringsAsFactors = FALSE
  )
}


# -- Core Relationships -------------------------------------------------------

#' Project Core Relationships from the spine
#'
#' Generates partner and parent-child relationship pairs.
#'
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @return data.frame with CORE relationship columns.
#' @keywords internal
project_core_relationships <- function(spine_df, seed) {
  # Try Rust implementation first
  if (exists("project_core_relationships__", mode = "function")) {
    raw <- project_core_relationships__(
      spine_id     = as.character(spine_df$spine_id),
      birth_year   = as.integer(spine_df$birth_year),
      household_id = as.integer(spine_df$household_id),
      seed         = as.integer(seed)
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }

  n <- nrow(spine_df)

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 702L)

  age <- 2021L - spine_df$birth_year

  # -- Partner relationships --
  # Adults 18+ eligible for partnership
  adult_idx <- which(age >= 18L)
  n_adults <- length(adult_idx)

  # ~50% of adults are partnered (married + de facto)
  n_partnered <- as.integer(n_adults * 0.50)
  # Need even number for pairing
  n_partnered <- n_partnered - (n_partnered %% 2L)

  partner_rows <- NULL
  if (n_partnered >= 2L) {
    # Select partnered adults randomly
    partnered_idx <- adult_idx[sample.int(n_adults, n_partnered)]
    # Pair adjacent selections
    person_a <- partnered_idx[seq(1L, n_partnered, by = 2L)]
    person_b <- partnered_idx[seq(2L, n_partnered, by = 2L)]
    n_pairs <- length(person_a)

    # Status: 80% married, 20% de facto
    status <- sample(c("Married", "De facto"), n_pairs, replace = TRUE,
                     prob = c(0.80, 0.20))

    # RECORD_START: synthetic partnership start date
    # Approximate: partners met 1-20 years before 2021
    years_ago <- sample.int(20L, n_pairs, replace = TRUE)
    start_year <- 2021L - years_ago
    record_start <- sprintf("%d-01-01", start_year)

    # replace = TRUE: collisions at n_pairs ≤ ~6M out of 2^31 are
    # vanishingly rare. Default (replace = FALSE) has inconsistent
    # performance at large n; stay on the fast path.
    pairids <- sprintf("PR%010X",
                       sample.int(.Machine$integer.max, n_pairs,
                                  replace = TRUE))

    partner_rows <- data.frame(
      SPINE_ID_ORIGINAL = spine_df$spine_id[person_a],
      SPINE_ID_MAIN_REL = spine_df$spine_id[person_b],
      PAIRID            = pairids,
      COMBINED_CATEGORY = "Partner",
      COMBINED_STATUS   = status,
      RECORD_START      = record_start,
      RECORD_END        = NA_character_,
      SOURCES           = "CENSUS",
      SOURCE_FLAG       = "CENSUS",
      stringsAsFactors  = FALSE
    )
  }

  # -- Parent-child relationships --
  child_idx  <- which(age < 18L)
  parent_pool <- which(age >= 25L & age <= 55L)

  pc_rows <- NULL
  if (length(child_idx) > 0L && length(parent_pool) > 0L) {
    # 90% of children linked to a parent
    n_link <- as.integer(length(child_idx) * 0.90)
    if (n_link > 0L) {
      linked_children <- child_idx[sample.int(length(child_idx), n_link)]

      # Assign each child a parent from the pool (random, with replacement)
      assigned_parents <- parent_pool[
        sample.int(length(parent_pool), n_link, replace = TRUE)
      ]

      # Status: 90% biological, 10% step
      pc_status <- sample(c("Biological", "Step"), n_link, replace = TRUE,
                          prob = c(0.90, 0.10))

      # RECORD_START: child's birth year
      pc_start <- sprintf("%d-01-01", spine_df$birth_year[linked_children])

      pc_pairids <- sprintf("PC%010X",
                            sample.int(.Machine$integer.max, n_link,
                                       replace = TRUE))

      pc_rows <- data.frame(
        SPINE_ID_ORIGINAL = spine_df$spine_id[linked_children],
        SPINE_ID_MAIN_REL = spine_df$spine_id[assigned_parents],
        PAIRID            = pc_pairids,
        COMBINED_CATEGORY = "Parent-Child",
        COMBINED_STATUS   = pc_status,
        RECORD_START      = pc_start,
        RECORD_END        = NA_character_,
        SOURCES           = "CENSUS",
        SOURCE_FLAG       = "CENSUS",
        stringsAsFactors  = FALSE
      )
    }
  }

  # Combine partner + parent-child rows
  result <- rbind(partner_rows, pc_rows)

  if (is.null(result) || nrow(result) == 0L) {
    # Return empty data.frame with correct structure
    result <- data.frame(
      SPINE_ID_ORIGINAL = character(0),
      SPINE_ID_MAIN_REL = character(0),
      PAIRID            = character(0),
      COMBINED_CATEGORY = character(0),
      COMBINED_STATUS   = character(0),
      RECORD_START      = character(0),
      RECORD_END        = character(0),
      SOURCES           = character(0),
      SOURCE_FLAG       = character(0),
      stringsAsFactors  = FALSE
    )
  }

  result
}

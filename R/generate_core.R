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
                 "household_id", "dwelling_id", "month_of_birth", "year_of_arrival",
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
    # Relationships come first: a separation moves somebody out of a shared
    # dwelling, and Core Locations has to carry that move.
    events        <- .core_household_events(spine, seed)
    locations     <- project_core_locations(spine, seed, moves = events$moves)
    relationships <- events$relationships
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

    # The household pass produces both frames at once, and Core Locations needs
    # the moves, so the relationship frame has to outlive the location build.
    events <- .core_household_events(spine, seed)
    moves <- events$moves
    relationships <- events$relationships
    rm(events); gc()

    locations <- project_core_locations(spine, seed, moves = moves)
    n_loc <- nrow(locations)
    write_product(locations, core_product_name("locations"),
                  "CORE", run_dir, format)
    rm(locations); rm(moves); gc()

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


# CORE Demographics is assembled from several agency sources, so a person's
# reported month of birth is near-complete but not complete. At full coverage
# no consumer can exercise a demographics fallback, and every PLIDA-based
# pipeline has one -- so the bad path is never taken and a defect on it never
# shows up locally. The share is a modelling choice, not a published rate.
.CORE_MONTH_OF_BIRTH_MISSING <- 0.012

#' Month of birth as CORE Demographics reports it
#'
#' @param spine_df data.frame. Spine rows.
#' @param seed Integer. Random seed.
#' @return Integer vector with a small share left missing, stable for a
#'   person across runs and products.
#' @keywords internal
.core_reported_month_of_birth <- function(spine_df, seed) {
  month <- as.integer(spine_df$month_of_birth)
  # Keyed on the person alone, not the seed: CORE demographics are
  # spine-derived, and which records are incomplete is a property of the
  # person's records rather than of this run.
  person <- suppressWarnings(as.numeric(gsub("[^0-9]", "",
                                             as.character(spine_df$spine_id))))
  person[is.na(person)] <- seq_len(sum(is.na(person)))
  key <- ((person * 2654435761) %% 100000) / 100000
  month[key < .CORE_MONTH_OF_BIRTH_MISSING] <- NA_integer_
  month
}

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
      MONTH_OF_BIRTH  = .core_reported_month_of_birth(spine_df, seed),
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

  csv_path <- registry_file("extdata", "sa1_lookup.csv")
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

  csv_path <- registry_file("extdata", "mb_lookup.csv.gz")
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


#' Spine dwelling as an integer vector, 0 where absent
#'
#' The address is drawn once per dwelling and reused, so co-residents share a
#' mesh block, an SA1 and an ARID. 0 means "no dwelling" and sends the person
#' back to drawing an address of their own.
#' @param spine_df data.frame from generate_spine().
#' @return Integer vector, length nrow(spine_df).
#' @keywords internal
.core_spine_dwelling <- function(spine_df) {
  n <- nrow(spine_df)
  if (!"dwelling_id" %in% names(spine_df)) return(rep(0L, n))
  dwelling <- suppressWarnings(as.integer(spine_df$dwelling_id))
  dwelling[is.na(dwelling) | dwelling < 0L] <- 0L
  dwelling
}


#' Address register identifiers for CORE Locations
#'
#' The residential half of the key space, computed exactly as the DIL
#' generators compute it, so a person's CORE address matches their address in
#' the agency products.
#'
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @return Character vector of 12 hexadecimal digits.
#' @keywords internal
#' @noRd
.core_address_key <- function(spine_df, seed) {
  .address_key_hex(.dwelling_number(spine_df), seed)
}

#' Leave the addresses that could not be resolved unresolved
#'
#' The ABS could tie 91% of the 25.7 million people on its 2021 administrative
#' population snapshot to a dwelling; the remaining 9% could be coded to an
#' area but not to an address. A model in which every person has an address
#' lets a pipeline that would break on real PLIDA run clean here, because the
#' branch that handles a missing ARID is never taken.
#'
#' The area survives: state, SA4 and SA2 stay, and the address itself -- the
#' ARID, the mesh block and the SA1 -- does not.
#'
#' @param locations data.frame. Core Locations rows.
#' @param spine_df data.frame. Spine rows, in the same order.
#' @param seed Integer. Random seed.
#' @return The same data.frame with unresolved addresses left missing.
#' @keywords internal
.core_unresolve_addresses <- function(locations, spine_df, seed) {
  if (!nrow(locations)) return(locations)
  unresolved <- .mobility_no_address(spine_df, seed)
  if (!any(unresolved)) return(locations)
  for (column in c("ARID", "MB_ASGS_2021", "SA1_ASGS_2021")) {
    if (column %in% names(locations)) {
      locations[[column]][unresolved] <- NA_character_
    }
  }
  locations
}

#' Project Core Locations from the spine
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @param moves data.frame or NULL. `SPINE_ID` and `LEAVE_DATE` from
#'   [.core_household_events()], one row per person who left a shared dwelling
#'   when their relationship ended.
#' @return data.frame with CORE location columns.
#' @keywords internal
project_core_locations <- function(spine_df, seed, moves = NULL) {
  if (exists("project_core_locations__", mode = "function")) {
    mb_lookup <- .load_mb_lookup()
    raw <- project_core_locations__(
      spine_id         = as.character(spine_df$spine_id),
      state            = as.integer(spine_df$state),
      sa2              = .core_spine_sa2(spine_df),
      dwelling_id      = as.integer(.core_spine_dwelling(spine_df)),
      lookup_state     = as.integer(mb_lookup$state),
      lookup_mb_code   = as.character(mb_lookup$mb_code),
      lookup_sa1_code  = as.character(mb_lookup$sa1_code),
      lookup_sa2_code  = as.character(mb_lookup$sa2_code),
      lookup_sa4_code  = as.integer(mb_lookup$sa4_code),
      seed             = as.integer(seed)
    )
    locations <- .core_unresolve_addresses(
      as.data.frame(raw, stringsAsFactors = FALSE), spine_df, seed)
    # Core Locations holds an address history, so a person who moved has a
    # closed spell where they used to live and an open one where they live
    # now.
    return(.core_address_spells(locations, spine_df, seed, moves))
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

  # One address per dwelling: everyone in a dwelling takes the mesh block drawn
  # for its first member, so co-residents share an SA1 and a mesh block instead
  # of each landing somewhere else inside the same SA2. The Rust path does the
  # same thing by caching the drawn row per dwelling; keep the two in step.
  # Below the SA2 the address belongs to the dwelling, so the mesh block is a
  # pure function of it rather than a draw: `dwelling mod pool size` over the
  # same pool, indexed the same way, in the Rust path and in
  # `.dil_asgs_2021_value()`. Every resident of one dwelling therefore lands on
  # the same mesh block and SA1 in every product, without any of them having to
  # see the others.
  dwelling <- .core_spine_dwelling(spine_df)
  shared <- which(dwelling > 0L)
  for (i in shared) {
    rows <- if (matched[i]) {
      by_sa2[[as.character(spine_sa2[i])]]
    } else {
      which(mb_lookup$state == spine_df$state[i])
    }
    if (!length(rows)) next
    pick <- rows[1L + (as.numeric(dwelling[i]) %% length(rows))]
    mb_code[i]  <- mb_lookup$mb_code[pick]
    sa1_code[i] <- mb_lookup$sa1_code[pick]
    sa2_code[i] <- mb_lookup$sa2_code[pick]
    sa4_code[i] <- mb_lookup$sa4_code[pick]
  }

  # Synthetic ARID (address register ID). Derived from the dwelling and the
  # seed rather than drawn, so one household's address here carries the same
  # value as their address in the ATO, Centrelink and Medicare products, which
  # is what the identifier means. See `.dil_address_key()`.
  arid <- .core_address_key(spine_df, seed)

  locations <- data.frame(
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
  # The same two steps the Rust path takes, so the fallback carries the same
  # unresolved addresses and the same address history rather than a single
  # perfect open spell per person.
  locations <- .core_unresolve_addresses(locations, spine_df, seed)
  .core_address_spells(locations, spine_df, seed, moves)
}

# -- Core Relationships -------------------------------------------------------
#
# Relationships and residence are one story. A couple that separates leaves one
# address and opens another, and a household that never separates still reports
# its move one person at a time. Both come out of the same household pass, so
# the relationship record and the address history cannot contradict each other.
#
# Every parameter below has a Rust twin in `src/rust/src/core_gen.rs` carrying
# the same value. Keep the two in step.

# The year household composition is read at. The spine builds its households
# around the 2021 Census, so every age test here is an age at that Census.
.CORE_REFERENCE_YEAR <- 2021L

# 2021 Census night, the date `.census_present_on_night()` also uses.
.CORE_CENSUS_NIGHT <- "2021-08-10"

# The last year a relationship can end in. The generated window runs to 2025.
.CORE_LAST_YEAR <- 2025L

# A couple is two adults close in age. Beyond this gap they read as a parent
# and an adult child. `.CENSUS_COUPLE_MAX_AGE_GAP` holds the same value, and the
# two rules must agree or CORE and the Census name different couples.
.CORE_COUPLE_MAX_AGE_GAP <- 18L

# Share of couples in a registered marriage rather than a de facto one. The
# draw is the same per-dwelling draw the Census reads, so CORE COMBINED_STATUS
# and Census RLHP agree about the same couple.
.CORE_REGISTERED_SHARE <- 0.80

# A partnership starts one to twenty years before the reference year.
.CORE_PARTNERSHIP_MAX_YEARS <- 20L

# Share of partner pairs whose two members are at different dwellings. A
# population in which every couple is co-resident lets a pipeline that reads
# co-residence off the address run clean here and find nothing in the lab. The
# share is a modelling choice, not a published rate.
.CORE_LIVING_APART_SHARE <- 0.08

# Age band in which living apart together is a real arrangement rather than a
# young adult who has not partnered yet or a widowed pensioner.
.CORE_LIVING_APART_MIN_AGE <- 25L
.CORE_LIVING_APART_MAX_AGE <- 70L

# Share of children with no parent link at all. The old blanket 90% link rate
# left the unlinked child unrepresentative of anything; this is the share that
# gives a consumer a bad path to exercise. A modelling choice.
.CORE_CHILD_UNLINKED <- 0.06

# Share of children with fewer than two co-resident candidate parents who carry
# a link to an adult at another dwelling -- roughly one child in six. A
# modelling choice, not a published rate.
.CORE_CHILD_NON_RESIDENT_PARENT <- 0.16

# Minimum years between a child and a recorded parent. Without it a
# twenty-two-year-old housemate is recorded as the parent of a ten-year-old.
.CORE_PARENT_MIN_AGE_GAP <- 16L

# The age window a non-resident parent is drawn from, in years above the child.
# A modelling choice: below the lower bound the adult is too young to be a
# parent, above the upper bound they read as a grandparent.
.CORE_NON_RESIDENT_MIN_AGE_GAP <- 18L
.CORE_NON_RESIDENT_MAX_AGE_GAP <- 50L

# Share of parent-child links recorded as a step relationship.
.CORE_PARENT_STEP_SHARE <- 0.10

# Share of parent-child links sourced from the births register rather than the
# Census. A modelling choice; BIRTHS is in the SOURCES code frame and a
# single-valued SOURCES column lets a consumer ignore it.
.CORE_PARENT_BIRTHS_SHARE <- 0.35

# Annual hazard that a live partner pair separates. Over a ten-year mean
# exposure this ends about a fifth of pairs. A modelling choice.
.CORE_SEPARATION_HAZARD <- 0.020

# Share of separations whose RECORD_END carries neither amendment flag -- the
# unobserved separation, where the end date came from neither a single-status
# start nor a death. A modelling choice.
.CORE_SEPARATION_UNOBSERVED <- 0.35

# Share of partner pairs recorded twice, once from each of two sources. A
# modelling choice.
.CORE_MULTI_SOURCE_SHARE <- 0.22

# Of those, the share whose administrative spell comes from the ATO rather than
# DOMINO. Both are in the SOURCES code frame.
.CORE_MULTI_SOURCE_ATO_SHARE <- 0.25

# How far before Census night the administrative spell of a twice-recorded pair
# starts, in years.
.CORE_MULTI_SOURCE_MIN_YEARS <- 2L
.CORE_MULTI_SOURCE_MAX_YEARS <- 6L


#' A spine column as an integer vector, missing where the column is absent
#'
#' @param spine_df data.frame. Spine rows.
#' @param column Character. Column name.
#' @return Integer vector, length `nrow(spine_df)`.
#' @keywords internal
.core_int_column <- function(spine_df, column) {
  n <- nrow(spine_df)
  if (!column %in% names(spine_df)) return(rep(NA_integer_, n))
  suppressWarnings(as.integer(spine_df[[column]]))
}


#' A person's date of death as an ISO string
#'
#' ISO dates sort as strings, so the comparisons below need no conversion. An
#' unknown month or day resolves late in the year, which is what
#' `.census_present_on_night()` does: a person is dropped only when the parts
#' that are known already establish the death came first.
#'
#' @param spine_df data.frame. Spine rows.
#' @return Character vector, `NA` where the person is alive.
#' @keywords internal
.core_death_date <- function(spine_df) {
  year <- .core_int_column(spine_df, "year_of_death")
  month <- .core_int_column(spine_df, "month_of_death")
  day <- .core_int_column(spine_df, "day_of_death")
  month[is.na(month)] <- 12L
  day[is.na(day)] <- 28L
  ifelse(is.na(year), NA_character_,
         sprintf("%04d-%02d-%02d", year, month, day))
}


#' The empty relationship frame
#'
#' @return data.frame with the published columns and no rows.
#' @keywords internal
.core_empty_relationships <- function() {
  data.frame(
    SPINE_ID_ORIGINAL = character(0),
    SPINE_ID_MAIN_REL = character(0),
    PAIRID            = character(0),
    COMBINED_CATEGORY = character(0),
    COMBINED_STATUS   = character(0),
    RECORD_START      = character(0),
    RECORD_END        = character(0),
    SINGLE_AMENDED    = integer(0),
    DEATH_AMENDED     = integer(0),
    SOURCES           = character(0),
    SOURCE_FLAG       = character(0),
    stringsAsFactors  = FALSE
  )
}

#' The empty move frame
#'
#' @return data.frame with `SPINE_ID` and `LEAVE_DATE` and no rows.
#' @keywords internal
.core_empty_moves <- function() {
  data.frame(SPINE_ID = character(0), LEAVE_DATE = character(0),
             stringsAsFactors = FALSE)
}


#' One block of relationship rows
#'
#' @param original,related Character vectors. The two spine identifiers.
#' @param pairid Character vector.
#' @param category,status,start Character vectors or scalars.
#' @param end Character vector.
#' @param single_amended,death_amended Integer vectors or scalars.
#' @param source Character vector or scalar. SOURCES and SOURCE_FLAG.
#' @return data.frame with the published columns.
#' @keywords internal
.core_relationship_block <- function(original, related, pairid, category,
                                     status, start, end, single_amended,
                                     death_amended, source) {
  if (!length(original)) return(.core_empty_relationships())
  data.frame(
    SPINE_ID_ORIGINAL = original,
    SPINE_ID_MAIN_REL = related,
    PAIRID            = pairid,
    COMBINED_CATEGORY = category,
    COMBINED_STATUS   = status,
    RECORD_START      = start,
    RECORD_END        = end,
    SINGLE_AMENDED    = single_amended,
    DEATH_AMENDED     = death_amended,
    SOURCES           = source,
    # SOURCE_FLAG names the source that contributed the record, which for a
    # single-source row is the source itself.
    SOURCE_FLAG       = source,
    stringsAsFactors  = FALSE
  )
}


#' PAIRID as a function of the unordered pair
#'
#' A pair recorded from two sources is one pair, so both rows have to carry one
#' identifier; a drawn value cannot. Forty-eight bits puts a collision over
#' three million pairs below one in ten thousand, where the drawn 32-bit value
#' it replaces collided about a thousand times. The Rust path derives its own
#' 48-bit value from the same unordered pair; the two need not agree bit for
#' bit, only within a run.
#'
#' @param prefix Character. `"PR"` or `"PC"`.
#' @param a,b Numeric. The two person numbers, in either order.
#' @param seed Integer. Random seed.
#' @return Character vector of the prefix and 12 hexadecimal digits.
#' @keywords internal
.core_pairid <- function(prefix, a, b, seed) {
  # Reduced before multiplying: this arithmetic runs in doubles, exact only
  # below 2^53.
  lo <- pmin(a, b) %% (2^24)
  hi <- pmax(a, b) %% (2^24)
  key <- (lo * 16777259 + hi * 4093 + abs(as.numeric(seed)) * 104729) %% (2^48)
  top <- floor(key / 2^24)
  paste0(prefix, sprintf("%06X", as.integer(top)),
         sprintf("%06X", as.integer(key - top * 2^24)))
}


#' A key for the unordered pair, for the deterministic draws
#'
#' @param a,b Numeric. The two person numbers.
#' @return Numeric vector.
#' @keywords internal
.core_pair_key <- function(a, b) {
  # Two different multipliers, so the key depends on both members rather than
  # collapsing to one of them once it is reduced modulo the draw's prime.
  pmin(a, b) * 7919 + pmax(a, b) * 104729
}


#' The year a partner pair separates, and the date within it
#'
#' Year by year from the record start, the same shape as the mortality loop.
#' The first year the hazard fires is the separation. The loop opens the year
#' after the start so a relationship cannot end in the month it began, which
#' would let RECORD_END precede RECORD_START on a record whose start carries a
#' month.
#'
#' @param key Numeric vector. The pair key.
#' @param seed Integer. Random seed.
#' @param start_year Integer vector. The year the record starts.
#' @param purpose Character. Distinguishes this pass from any other.
#' @return Character vector of ISO dates, `NA` where the pair did not separate.
#' @keywords internal
.core_separation_date <- function(key, seed, start_year, purpose) {
  n <- length(key)
  if (!n) return(character(0))
  year <- rep(NA_integer_, n)
  for (y in (min(start_year) + 1L):.CORE_LAST_YEAR) {
    open <- which(is.na(year) & y > start_year)
    if (!length(open)) next
    draw <- .mobility_draw_for(key[open], seed, paste(purpose, y))
    year[open[draw < .CORE_SEPARATION_HAZARD]] <- y
  }
  month <- 1L + as.integer(
    .mobility_draw_for(key, seed, paste(purpose, "month")) * 12)
  day <- 1L + as.integer(
    .mobility_draw_for(key, seed, paste(purpose, "day")) * 28)
  ifelse(is.na(year), NA_character_,
         sprintf("%04d-%02d-%02d", year, month, day))
}


#' The earliest of two dates that is not before a record starts
#'
#' @param start Character vector. RECORD_START, as an ISO date.
#' @param first,second Character vectors of ISO dates, possibly missing.
#' @return Character vector, missing where neither date qualifies.
#' @keywords internal
.core_first_death <- function(start, first, second) {
  a <- ifelse(!is.na(first) & first >= start, first, NA_character_)
  b <- ifelse(!is.na(second) & second >= start, second, NA_character_)
  out <- suppressWarnings(pmin(a, b, na.rm = TRUE))
  out[is.na(a) & is.na(b)] <- NA_character_
  out
}


#' How a partner pair ends, and which amendment flag says so
#'
#' A death at or before the separation ends the relationship and sets
#' DEATH_AMENDED; otherwise a separation sets SINGLE_AMENDED, except for the
#' share that carries no flag at all. A death before the record starts is not
#' this relationship's ending.
#'
#' @param start Character vector. RECORD_START, as an ISO date.
#' @param death_a,death_b Character vectors. The members' death dates.
#' @param separation Character vector. The separation date, or `NA`.
#' @param key Numeric vector. The pair key.
#' @param seed Integer. Random seed.
#' @param purpose Character. Distinguishes this pass from any other.
#' @return A list with `end`, `single_amended` and `death_amended`.
#' @keywords internal
.core_resolve_end <- function(start, death_a, death_b, separation, key, seed,
                              purpose) {
  death <- .core_first_death(start, death_a, death_b)
  death_first <- !is.na(death) & (is.na(separation) | death <= separation)
  silent <- .mobility_draw_for(key, seed, paste(purpose, "unobserved")) <
    .CORE_SEPARATION_UNOBSERVED
  list(
    end = ifelse(death_first, death, separation),
    single_amended = as.integer(!death_first & !is.na(separation) & !silent),
    death_amended = as.integer(death_first)
  )
}


#' Project CORE relationships, and the residential events they imply
#'
#' One household pass produces both: the flat partner and parent-child record,
#' and one row per person who left a shared dwelling when their relationship
#' ended. Core Locations reads the second, so a separated couple's address
#' history actually parts.
#'
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @return A list with `relationships` and `moves` data.frames.
#' @keywords internal
.core_household_events <- function(spine_df, seed) {
  if (exists("project_core_relationships__", mode = "function")) {
    raw <- project_core_relationships__(
      spine_id       = as.character(spine_df$spine_id),
      birth_year     = .core_int_column(spine_df, "birth_year"),
      state          = .core_int_column(spine_df, "state"),
      household_id   = .core_int_column(spine_df, "household_id"),
      dwelling_id    = as.integer(.core_spine_dwelling(spine_df)),
      year_of_death  = .core_int_column(spine_df, "year_of_death"),
      month_of_death = .core_int_column(spine_df, "month_of_death"),
      day_of_death   = .core_int_column(spine_df, "day_of_death"),
      seed           = as.integer(seed)
    )
    return(list(
      relationships = as.data.frame(raw$relationships,
                                    stringsAsFactors = FALSE),
      moves = as.data.frame(raw$moves, stringsAsFactors = FALSE)
    ))
  }
  .core_household_events_r(spine_df, seed)
}


#' Project Core Relationships from the spine
#'
#' Generates partner and parent-child relationship pairs.
#'
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @param events list or NULL. The result of [.core_household_events()], when
#'   the caller has already built it.
#' @return data.frame with CORE relationship columns.
#' @keywords internal
project_core_relationships <- function(spine_df, seed, events = NULL) {
  if (is.null(events)) events <- .core_household_events(spine_df, seed)
  events$relationships
}


#' The household pass in R
#'
#' The fallback for a build without the compiled library. It applies the same
#' rules and the same parameters as the Rust path and emits the same columns.
#' The draws are keyed the same way but not by the same generator, so the two
#' paths agree in distribution rather than row for row.
#'
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @return A list with `relationships` and `moves` data.frames.
#' @keywords internal
.core_household_events_r <- function(spine_df, seed) {
  n <- nrow(spine_df)
  if (!n) {
    return(list(relationships = .core_empty_relationships(),
                moves = .core_empty_moves()))
  }

  spine_id <- as.character(spine_df$spine_id)
  person <- .person_number(spine_id, n)
  # An unknown birth year reads as age 40, which is what
  # `census_household_roles()` does, so the two agree about who is an adult.
  age <- .CORE_REFERENCE_YEAR - .core_int_column(spine_df, "birth_year")
  age[is.na(age)] <- 40L
  born <- .CORE_REFERENCE_YEAR - age
  death <- .core_death_date(spine_df)
  state <- .core_int_column(spine_df, "state")
  state[is.na(state)] <- 0L
  # A person with no household is skipped rather than joined to everyone else
  # who has none: a hand-built spine with a zero household on many rows would
  # otherwise read as one enormous household.
  household <- .core_int_column(spine_df, "household_id")
  household[is.na(household)] <- 0L
  key_of <- as.character(household)

  adults <- which(household > 0L & age >= 18L)
  children <- which(household > 0L & age < 18L)

  # The couple, by exactly the rule `census_household_roles()` uses: the oldest
  # adult is the reference person, and their partner is the other adult closest
  # to them in age, taken only when the gap is small enough to read as a couple
  # rather than as a parent and an adult child. Ties go to the lower spine row,
  # which is what R's `which.max`/`which.min` do inside that function.
  ranked <- adults[order(household[adults], -age[adults], adults)]
  reference_rows <- ranked[!duplicated(household[ranked])]
  reference_of <- reference_rows
  names(reference_of) <- as.character(household[reference_rows])
  reference <- unname(reference_of[key_of])

  candidates <- adults[!is.na(reference[adults]) &
                         adults != reference[adults]]
  gap <- abs(age[candidates] - age[reference[candidates]])
  close <- gap <= .CORE_COUPLE_MAX_AGE_GAP
  candidates <- candidates[close]
  gap <- gap[close]
  ranked <- order(household[candidates], gap, candidates)
  candidates <- candidates[ranked]
  partner_rows <- candidates[!duplicated(household[candidates])]
  partner_of <- partner_rows
  names(partner_of) <- as.character(household[partner_rows])
  partner <- unname(partner_of[key_of])

  pair_a <- reference_rows[!is.na(partner[reference_rows])]
  pair_b <- partner[pair_a]

  # -- Couples who live apart ------------------------------------------------
  #
  # Selected from a fixed total order rather than a draw, so a change upstream
  # cannot shift which references pair with which.
  adults_in <- tabulate(household[adults],
                        nbins = max(c(household, 1L), na.rm = TRUE))
  alone <- reference_rows[is.na(partner[reference_rows]) &
                            adults_in[household[reference_rows]] == 1L]
  alone <- alone[age[alone] >= .CORE_LIVING_APART_MIN_AGE &
                   age[alone] <= .CORE_LIVING_APART_MAX_AGE]
  alone <- alone[order(state[alone], born[alone], alone)]
  n_apart <- round(length(pair_a) * .CORE_LIVING_APART_SHARE /
                     (1 - .CORE_LIVING_APART_SHARE))
  take <- min(2L * as.integer(n_apart), length(alone))
  if (take >= 2L) {
    left <- alone[seq(1L, take - 1L, by = 2L)]
    right <- alone[seq(2L, take, by = 2L)]
    # A couple lives in one state even when it lives at two addresses, so a
    # pair that straddles the sort's state boundary is skipped.
    same_state <- state[left] == state[right]
    pair_a <- c(pair_a, left[same_state])
    pair_b <- c(pair_b, right[same_state])
  }

  # -- Parent-child links ----------------------------------------------------
  #
  # A share of children carry no parent link at all, so a consumer has an
  # unlinked child to handle.
  children <- children[
    .mobility_draw_for(person[children], seed, "child unlinked") >=
      .CORE_CHILD_UNLINKED]
  child_reference <- reference[children]
  child_partner <- partner[children]
  by_reference <- !is.na(child_reference) &
    (age[child_reference] - age[children]) >= .CORE_PARENT_MIN_AGE_GAP
  by_partner <- !is.na(child_partner) &
    (age[child_partner] - age[children]) >= .CORE_PARENT_MIN_AGE_GAP
  link_child <- c(children[by_reference], children[by_partner])
  link_parent <- c(child_reference[by_reference], child_partner[by_partner])
  short <- children[(as.integer(by_reference) + as.integer(by_partner)) < 2L]

  # -- The parent at another dwelling ---------------------------------------
  wanted <- short[.mobility_draw_for(person[short], seed,
                                     "non-resident parent") <
                    .CORE_CHILD_NON_RESIDENT_PARENT]
  if (length(wanted)) {
    pick <- .mobility_draw_for(person[wanted], seed,
                               "non-resident parent pick")
    for (st in sort(unique(state[wanted]))) {
      pool <- adults[state[adults] == st]
      if (!length(pool)) next
      # Ordered by age then row index, so the age window a child needs is a
      # contiguous slice and the pick inside it is a pure function of the child.
      pool <- pool[order(age[pool], pool)]
      here <- which(state[wanted] == st)
      low <- findInterval(age[wanted[here]] +
                            .CORE_NON_RESIDENT_MIN_AGE_GAP - 1L,
                          age[pool]) + 1L
      high <- findInterval(age[wanted[here]] + .CORE_NON_RESIDENT_MAX_AGE_GAP,
                           age[pool])
      inside <- high >= low
      here <- here[inside]
      low <- low[inside]
      high <- high[inside]
      if (!length(here)) next
      width <- high - low + 1L
      at <- pmin(low + as.integer(pick[here] * width), high)
      # Step past the child's own household: a non-resident parent who lives
      # with the child is not a non-resident parent.
      for (attempt in seq_len(8L)) {
        clash <- household[pool[at]] == household[wanted[here]]
        if (!any(clash)) break
        at[clash] <- low[clash] + (at[clash] + 1L - low[clash]) %% width[clash]
      }
      keep <- household[pool[at]] != household[wanted[here]]
      link_child <- c(link_child, wanted[here][keep])
      link_parent <- c(link_parent, pool[at][keep])
    }
  }

  span <- .core_partner_span(pair_a, pair_b, person, born, death, seed,
                             spine_df)
  list(
    relationships = rbind(
      .core_partner_rows(pair_a, pair_b, spine_id, span),
      .core_parent_rows(link_child, link_parent, spine_id, person, born,
                        death, seed)
    ),
    moves = .core_leaver_moves(pair_a, pair_b, spine_id, person, household,
                               span, seed)
  )
}


#' Every date and flag a partner pair carries
#'
#' @param pair_a,pair_b Integer vectors. The two members' spine rows.
#' @param person Numeric vector. Person numbers.
#' @param born Integer vector. Effective birth years.
#' @param death Character vector. Death dates.
#' @param seed Integer seed.
#' @param spine_df data.frame. Spine rows.
#' @return A list of vectors, one entry per pair.
#' @keywords internal
.core_partner_span <- function(pair_a, pair_b, person, born, death, seed,
                               spine_df) {
  key <- .core_pair_key(person[pair_a], person[pair_b])

  # The same per-dwelling draw the Census reads, so CORE COMBINED_STATUS and
  # Census RLHP agree about the same couple.
  registered <- .mobility_dwelling_draw(spine_df[pair_a, , drop = FALSE], seed,
                                        "registered marriage") <
    .CORE_REGISTERED_SHARE

  years_ago <- 1L + as.integer(
    .mobility_draw_for(key, seed, "partnership start") *
      .CORE_PARTNERSHIP_MAX_YEARS)
  # A relationship cannot start before the younger member turned 18.
  start_year <- pmax(.CORE_REFERENCE_YEAR - years_ago,
                     pmax(born[pair_a], born[pair_b]) + 18L)
  start <- sprintf("%04d-01-01", start_year)

  separation <- .core_separation_date(key, seed, start_year,
                                      "partner separation")
  ending <- .core_resolve_end(start, death[pair_a], death[pair_b], separation,
                              key, seed, "partner separation")

  twice <- .mobility_draw_for(key, seed, "multi source") <
    .CORE_MULTI_SOURCE_SHARE
  alive <- (is.na(death[pair_a]) | death[pair_a] >= .CORE_CENSUS_NIGHT) &
    (is.na(death[pair_b]) | death[pair_b] >= .CORE_CENSUS_NIGHT)
  covers <- start <= .CORE_CENSUS_NIGHT &
    (is.na(ending$end) | ending$end >= .CORE_CENSUS_NIGHT)

  back <- .CORE_MULTI_SOURCE_MIN_YEARS + as.integer(
    .mobility_draw_for(key, seed, "multi source span") *
      (.CORE_MULTI_SOURCE_MAX_YEARS - .CORE_MULTI_SOURCE_MIN_YEARS + 1L))
  admin_year <- .CORE_REFERENCE_YEAR - back
  admin_start <- sprintf("%04d-%s", admin_year,
                         substr(.CORE_CENSUS_NIGHT, 6L, 10L))
  admin_separation <- .core_separation_date(key, seed, admin_year,
                                            "admin separation")
  admin <- .core_resolve_end(admin_start, death[pair_a], death[pair_b],
                             admin_separation, key, seed, "admin separation")

  list(
    pairid = .core_pairid("PR", person[pair_a], person[pair_b], seed),
    status = ifelse(registered, "Married", "De facto"),
    start = start, end = ending$end,
    single_amended = ending$single_amended,
    death_amended = ending$death_amended,
    twice = twice, on_census_night = alive & covers,
    admin_start = admin_start, admin_end = admin$end,
    admin_single = admin$single_amended, admin_death = admin$death_amended,
    admin_source = ifelse(
      .mobility_draw_for(key, seed, "multi source agency") <
        .CORE_MULTI_SOURCE_ATO_SHARE, "ATO", "DOMINO"),
    # The span the address history follows. For a pair recorded twice it is the
    # administrative spell, which is the row that carries a span at all; the
    # Census row is a point observation on one night.
    span_end = ifelse(twice, admin$end, ending$end),
    span_death = ifelse(twice, admin$death_amended, ending$death_amended)
  )
}


#' The partner half of the relationship frame
#'
#' @param pair_a,pair_b Integer vectors. The two members' spine rows.
#' @param spine_id Character vector. Spine identifiers.
#' @param span list. The result of [.core_partner_span()].
#' @return data.frame of partner rows.
#' @keywords internal
.core_partner_rows <- function(pair_a, pair_b, spine_id, span) {
  if (!length(pair_a)) return(.core_empty_relationships())
  once <- which(!span$twice)
  # A pair recorded twice becomes a Census point record and an administrative
  # spell, the second mirrored so a consumer folding the pair has to reach the
  # same pair from either row.
  point <- which(span$twice & span$on_census_night)
  spell <- which(span$twice)

  rbind(
    .core_relationship_block(
      spine_id[pair_a[once]], spine_id[pair_b[once]], span$pairid[once],
      "Partner", span$status[once], span$start[once], span$end[once],
      span$single_amended[once], span$death_amended[once], "CENSUS"),
    .core_relationship_block(
      spine_id[pair_a[point]], spine_id[pair_b[point]], span$pairid[point],
      "Partner", span$status[point], .CORE_CENSUS_NIGHT, .CORE_CENSUS_NIGHT,
      0L, 0L, "CENSUS"),
    .core_relationship_block(
      spine_id[pair_b[spell]], spine_id[pair_a[spell]], span$pairid[spell],
      "Partner", span$status[spell], span$admin_start[spell],
      span$admin_end[spell], span$admin_single[spell],
      span$admin_death[spell], span$admin_source[spell])
  )
}


#' The parent-child half of the relationship frame
#'
#' @param link_child,link_parent Integer vectors. The two spine rows.
#' @param spine_id Character vector. Spine identifiers.
#' @param person Numeric vector. Person numbers.
#' @param born Integer vector. Effective birth years.
#' @param death Character vector. Death dates.
#' @param seed Integer seed.
#' @return data.frame of parent-child rows.
#' @keywords internal
.core_parent_rows <- function(link_child, link_parent, spine_id, person, born,
                              death, seed) {
  if (!length(link_child)) return(.core_empty_relationships())
  key <- .core_pair_key(person[link_child], person[link_parent])
  start <- sprintf("%04d-01-01", born[link_child])
  source <- ifelse(.mobility_draw_for(key, seed, "parent link source") <
                     .CORE_PARENT_BIRTHS_SHARE, "BIRTHS", "CENSUS")
  .core_relationship_block(
    spine_id[link_child], spine_id[link_parent],
    .core_pairid("PC", person[link_child], person[link_parent], seed),
    "Parent-Child",
    ifelse(.mobility_draw_for(key, seed, "parent link type") <
             .CORE_PARENT_STEP_SHARE, "Step", "Biological"),
    start,
    # A parent-child link ends only with a death.
    .core_first_death(start, death[link_child], death[link_parent]),
    # The registry declares the amendment flags on the partner tables alone.
    NA_integer_, NA_integer_, source)
}


#' Who leaves a shared dwelling when a relationship ends
#'
#' A co-resident couple that separates cannot both stay. One of them closes the
#' shared address and opens another, which is the event a co-residence rule
#' needs to see. A death does not move anybody, and a separation before Core
#' Locations opens has no spell to close.
#'
#' @param pair_a,pair_b Integer vectors. The two members' spine rows.
#' @param spine_id Character vector. Spine identifiers.
#' @param person Numeric vector. Person numbers.
#' @param household Integer vector. Household identifiers.
#' @param span list. The result of [.core_partner_span()].
#' @param seed Integer seed.
#' @return data.frame with `SPINE_ID` and `LEAVE_DATE`.
#' @keywords internal
.core_leaver_moves <- function(pair_a, pair_b, spine_id, person, household,
                               span, seed) {
  if (!length(pair_a)) return(.core_empty_moves())
  leaves <- which(household[pair_a] == household[pair_b] &
                    span$span_death == 0L & !is.na(span$span_end) &
                    span$span_end > as.character(.MOBILITY_HISTORY_START))
  if (!length(leaves)) return(.core_empty_moves())
  key <- .core_pair_key(person[pair_a[leaves]], person[pair_b[leaves]])
  first <- .mobility_draw_for(key, seed, "which partner leaves") < 0.5
  data.frame(
    SPINE_ID = ifelse(first, spine_id[pair_a[leaves]],
                      spine_id[pair_b[leaves]]),
    LEAVE_DATE = span$span_end[leaves],
    stringsAsFactors = FALSE
  )
}

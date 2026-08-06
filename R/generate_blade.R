# BLADE business-level generators.
#
# BLADE is a business longitudinal asset, so it uses a separate
# business spine/source-of-truth rather than the person spine as its
# analytical unit. The business spine is still derived from the person
# spine so aggregate employment, wages, industry and state remain
# coherent with generated PLIDA persons.

.blade_metadata_path <- function(filename) {
  metadata_dir <- getOption("fplida.blade_metadata_dir", NULL)
  if (!is.null(metadata_dir)) {
    path <- file.path(metadata_dir, filename)
    if (file.exists(path)) {
      return(path)
    }
  }
  path <- system.file("blade_metadata", filename, package = "fplida")
  if (!nzchar(path)) {
    path <- file.path("inst", "blade_metadata", filename)
  }
  path
}

.blade_tables <- function() {
  path <- .blade_metadata_path("tables.csv")
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

.blade_variables <- function(table_number = NULL, product_name = NULL) {
  path <- .blade_metadata_path("variables.csv")
  variables <- utils::read.csv(path, stringsAsFactors = FALSE,
                               check.names = FALSE)
  rows <- rep(TRUE, nrow(variables))
  if (!is.null(table_number)) {
    rows <- rows & variables[["Table.Number"]] %in% as.integer(table_number)
  }
  if (!is.null(product_name)) {
    rows <- rows & variables[["Product.Name"]] %in% product_name
  }
  variables[rows, , drop = FALSE]
}

.blade_key_variables <- function(key_name = NULL, product_name = NULL) {
  path <- .blade_metadata_path("keys.csv")
  keys <- utils::read.csv(path, stringsAsFactors = FALSE,
                          check.names = FALSE)
  rows <- rep(TRUE, nrow(keys))
  if (!is.null(key_name)) {
    rows <- rows & keys[["Key.Name"]] %in% key_name
  }
  if (!is.null(product_name)) {
    rows <- rows & keys[["Product.Name"]] %in% product_name
  }
  keys[rows, , drop = FALSE]
}

.blade_domain_cache <- new.env(parent = emptyenv())

.blade_domains <- function() {
  path <- .blade_metadata_path("domains.csv")
  info <- file.info(path)
  stamp <- if (nrow(info) && !is.na(info[["mtime"]])) {
    as.numeric(info[["mtime"]])
  } else {
    NA_real_
  }
  if (identical(get0("path", envir = .blade_domain_cache), path) &&
      identical(get0("stamp", envir = .blade_domain_cache), stamp)) {
    return(get("data", envir = .blade_domain_cache))
  }
  if (!file.exists(path)) {
    out <- data.frame(
      Domain = character(), Variable.Name = character(),
      Value = character(), Source.Sheet = character(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE,
      colClasses = "character", na.strings = character()
    )
  }
  assign("path", path, envir = .blade_domain_cache)
  assign("stamp", stamp, envir = .blade_domain_cache)
  assign("data", out, envir = .blade_domain_cache)
  out
}

.blade_domain_values <- function(domain = NULL, variable_name = NULL) {
  domains <- .blade_domains()
  rows <- rep(TRUE, nrow(domains))
  if (!is.null(domain)) rows <- rows & domains[["Domain"]] == domain
  if (!is.null(variable_name)) {
    rows <- rows & tolower(domains[["Variable.Name"]]) ==
      tolower(variable_name)
  }
  values <- unique(domains[["Value"]][rows])
  values[!is.na(values) & nzchar(values) & values != "."]
}

.blade_business_spine_path <- function(run_dir, format = "parquet") {
  ext <- if (format == "parquet") "parquet" else "csv"
  file.path(run_dir, "_system", paste0("business-spine.", ext))
}

.blade_plida_link_path <- function(run_dir, format = "parquet") {
  ext <- if (format == "parquet") "parquet" else "csv"
  file.path(run_dir, "_system", paste0("plida-blade-link.", ext))
}

.blade_spine_cols <- function() {
  c("spine_id", "aeuid_ato", "aeuid_abs", "aeuid_dhda",
    "birth_year", "sex", "state", "industry", "sector",
    "baseline_employed", "baseline_income",
    "anzsco_code", "anzsco_title", "anzsco_major")
}

.optional_chr <- function(data, name, idx = NULL, default = NA_character_) {
  n <- if (is.null(idx)) nrow(data) else length(idx)
  if (!name %in% names(data)) return(rep(default, n))
  values <- as.character(data[[name]])
  if (is.null(idx)) values else values[idx]
}

.optional_num <- function(data, name, idx = NULL, default = NA_real_) {
  n <- if (is.null(idx)) nrow(data) else length(idx)
  if (!name %in% names(data)) return(rep(default, n))
  values <- suppressWarnings(as.numeric(data[[name]]))
  if (is.null(idx)) values else values[idx]
}

.load_blade_business_spine <- function(run_dir) {
  pq <- .blade_business_spine_path(run_dir, "parquet")
  csv <- .blade_business_spine_path(run_dir, "csv")
  if (file.exists(pq)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read BLADE business spine.",
           call. = FALSE)
    }
    return(as.data.frame(read_parquet_safely(pq), stringsAsFactors = FALSE))
  }
  if (file.exists(csv)) {
    return(utils::read.csv(csv, stringsAsFactors = FALSE,
                           check.names = FALSE))
  }
  stop("No BLADE business spine found in ", file.path(run_dir, "_system"),
       ". Run generate_blade_business_spine() first.", call. = FALSE)
}

# R-side mirror of the Rust business pool, for the R fallback id generators.
.fplida_bn_pool_env <- new.env(parent = emptyenv())

.get_business_pool_r <- function() {
  get0("bns", envir = .fplida_bn_pool_env, ifnotfound = character(0))
}

.set_business_pool_r <- function(bns) {
  assign("bns", as.character(bns), envir = .fplida_bn_pool_env)
  invisible(NULL)
}

# Map a deterministic numeric hash to a pooled `bn`, falling back to the
# generator's legacy identifier when the pool is empty. Mirrors the Rust
# `business_pool::bn_for_hash`.
.bn_for_hash_r <- function(hash, fallback) {
  pool <- .get_business_pool_r()
  n <- length(pool)
  if (n == 0L) return(fallback)
  pool[(abs(hash) %% n) + 1L]
}

# Load the BLADE business-spine `bn` values into the business pool (both the
# Rust pool used by the fast PIT_PS/STP paths and the R mirror used by the
# fallback generators) so employer-linked products draw their employer
# identifiers from real BLADE businesses. Called at the start of those
# products' builders. If no business spine exists (e.g. a build without the
# BLADE stage), the pool is cleared so generators fall back to legacy
# synthetic identifiers.
.set_business_pool_from_spine <- function(run_dir) {
  # Slice workers carry only a thin `bn`-only pool file; full builds and
  # single-process runs read `bn` from the full business spine. Check the
  # thin file first so the slice path does not depend on the full spine.
  thin <- file.path(run_dir, "_system", "business-bn-pool.parquet")
  pq <- .blade_business_spine_path(run_dir, "parquet")
  csv <- .blade_business_spine_path(run_dir, "csv")
  bns <- NULL
  if (file.exists(thin) && requireNamespace("arrow", quietly = TRUE)) {
    bns <- as.data.frame(
      read_parquet_safely(thin, col_select = "bn"),
      stringsAsFactors = FALSE
    )$bn
  } else if (file.exists(pq) && requireNamespace("arrow", quietly = TRUE)) {
    bns <- as.data.frame(
      read_parquet_safely(pq, col_select = "bn"),
      stringsAsFactors = FALSE
    )$bn
  } else if (file.exists(csv)) {
    bns <- utils::read.csv(csv, stringsAsFactors = FALSE,
                           check.names = FALSE)$bn
  }
  if (is.null(bns)) {
    bns <- character(0)
  } else {
    bns <- as.character(bns)
    bns <- bns[!is.na(bns) & nzchar(bns)]
  }
  .set_business_pool_r(bns)
  if (exists("set_business_pool__", mode = "function")) {
    set_business_pool__(bns)
  }
  invisible(length(bns) > 0L)
}

.blade_numeric_id <- function(prefix, values, width = 9L) {
  paste0(prefix, sprintf(paste0("%0", width, ".0f"), values))
}

.blade_id_number <- function(values) {
  values <- gsub("[^0-9]", "", as.character(values))
  out <- suppressWarnings(as.numeric(values))
  out[is.na(out)] <- seq_len(sum(is.na(out)))
  out
}

.blade_financial_year_label <- function(start_year) {
  start_year <- as.integer(start_year)
  out <- rep(NA_character_, length(start_year))
  valid <- !is.na(start_year)
  out[valid] <- sprintf("%04d-%02d", start_year[valid],
                        (start_year[valid] + 1L) %% 100L)
  out
}

.blade_table_variable_names <- function(variables, table_number) {
  variable_names <- unique(variables[["Variable.Name"]])
  variable_names <- variable_names[nzchar(variable_names)]
  variable_names
}

.blade_deidentified_id <- function(prefix, values, seed = 0L, width = 14L) {
  values <- as.character(values)
  hash <- vapply(
    seq_along(values),
    function(i) .stable_name_seed(paste(values[[i]], i, seed, sep = "|")),
    integer(1)
  )
  modulus <- 10^as.integer(width)
  numeric_values <- (hash * 1000003 + seq_along(values) * 9176 + seed) %%
    modulus
  .blade_numeric_id(prefix, numeric_values, width)
}

.blade_split_periods <- function(periods) {
  periods <- as.character(periods)
  periods <- periods[!is.na(periods)]
  if (!length(periods)) return(character(0))
  periods <- unlist(strsplit(as.character(periods), ";", fixed = TRUE),
                    use.names = FALSE)
  periods <- trimws(periods)
  periods[!is.na(periods) & nzchar(periods)]
}

.blade_period_end_year <- function(period) {
  if (is.na(period)) return(NA_integer_)
  period <- trimws(gsub("[[:space:]]+", "", as.character(period)))
  if (is.na(period) || !nzchar(period)) return(NA_integer_)
  if (grepl("^[0-9]{4}-[0-9]{2}$", period)) {
    start_year <- as.integer(substr(period, 1L, 4L))
    end_two <- as.integer(substr(period, 6L, 7L))
    century <- start_year - start_year %% 100L
    end_year <- century + end_two
    if (end_year < start_year) end_year <- end_year + 100L
    return(end_year)
  }
  if (grepl("^[0-9]{4}$", period)) {
    return(as.integer(period))
  }
  years <- regmatches(period, gregexpr("[0-9]{4}", period))[[1L]]
  if (length(years)) return(max(as.integer(years), na.rm = TRUE))
  NA_integer_
}

.blade_latest_period_from_values <- function(periods, default = "2023-24") {
  periods <- .blade_split_periods(periods)
  if (!length(periods)) return(default)
  years <- vapply(periods, .blade_period_end_year, integer(1))
  if (all(is.na(years))) return(default)
  periods[[which.max(years)]]
}

.blade_latest_period_from_reference <- function(reference,
                                                default = "2023-24") {
  if (is.na(reference) || !nzchar(trimws(reference))) return(default)
  parts <- strsplit(trimws(reference), "[[:space:]]+to[[:space:]]+")[[1L]]
  period <- trimws(parts[[length(parts)]])
  period <- gsub("[[:space:]]+", "", period)
  if (grepl("^[0-9]{4}-[0-9]{2}$", period) ||
      grepl("^[0-9]{4}$", period)) {
    return(period)
  }
  default
}

.blade_latest_period <- function(table_number) {
  variables <- .blade_variables(table_number = table_number)
  period <- .blade_latest_period_from_values(
    variables[["Available.Periods"]],
    default = NA_character_
  )
  if (!is.na(period)) return(period)
  tables <- .blade_tables()
  reference <- tables[["Reference.Period"]][
    tables[["Table.Number"]] == as.integer(table_number)
  ]
  if (length(reference)) {
    return(.blade_latest_period_from_reference(reference[[1L]]))
  }
  "2023-24"
}

.blade_latest_key_period <- function(product_name) {
  keys <- .blade_key_variables(product_name = product_name)
  .blade_latest_period_from_values(keys[["Available.Periods"]])
}

.blade_tsid_from_period <- function(period) {
  sprintf("%02d", .blade_period_end_year(period) %% 100L)
}

.blade_tsid <- function(table_number) {
  table_number <- as.integer(table_number)
  if (!is.na(table_number) && table_number == 5L) {
    return(.blade_tsid_from_period(.blade_latest_period(1L)))
  }
  .blade_tsid_from_period(.blade_latest_period(table_number))
}

.blade_key_tsid <- function(product_name) {
  .blade_tsid_from_period(.blade_latest_key_period(product_name))
}

.blade_financial_year_code <- function(table_number) {
  period <- trimws(gsub("[[:space:]]+", "", .blade_latest_period(table_number)))
  if (grepl("^[0-9]{4}-[0-9]{2}$", period)) {
    return(paste0(substr(period, 3L, 4L), substr(period, 6L, 7L)))
  }
  as.character(.blade_period_end_year(period))
}

.blade_end_year <- function(table_number) {
  .blade_period_end_year(.blade_latest_period(table_number))
}

# Which year and basis a table's period means, for a nominal index lookup.
#
# Most BLADE periods are financial years written 2023-24, and
# `.blade_period_end_year()` turns those into the year they END in. The nominal
# tables name a financial year by the year it STARTS in, so those need a minus
# one; the two conventions differ by about four per cent and nothing complains
# if they are mixed up.
#
# A handful of periods are a bare four-digit year instead -- table 23 (higher
# education research and development) and table 59 (Dealroom) are calendar-year
# collections -- and `.blade_period_end_year()` returns those verbatim.
# Subtracting one from those would move them a year into the past for no reason,
# so the period string decides, not the derived year.
.blade_nominal_period <- function(period) {
  period <- trimws(gsub("[[:space:]]+", "", as.character(period)))
  end_year <- .blade_period_end_year(period)
  if (is.na(end_year)) return(list(year = NA_integer_, basis = "financial"))
  if (grepl("^[0-9]{4}$", period)) {
    return(list(year = as.integer(end_year), basis = "calendar"))
  }
  list(year = as.integer(end_year) - 1L, basis = "financial")
}

.blade_nominal_period_for_table <- function(table_number) {
  .blade_nominal_period(.blade_latest_period(table_number))
}

.blade_reference_date <- function(table_number) {
  period <- trimws(gsub("[[:space:]]+", "", .blade_latest_period(table_number)))
  year <- .blade_period_end_year(period)
  if (is.na(year)) year <- 2024L
  if (grepl("^[0-9]{4}-[0-9]{2}$", period)) {
    as.Date(sprintf("%04d-06-30", year))
  } else {
    as.Date(sprintf("%04d-12-31", year))
  }
}

.blade_default_business_count <- function(spine) {
  n <- nrow(spine)
  employed <- spine$baseline_employed == 1L &
    !is.na(spine$baseline_income) &
    spine$baseline_income > 0
  # Australia has many non-employing businesses. This ratio keeps the
  # synthetic business universe large enough to include both employing
  # and non-employing units without exploding output size.
  max(1L, as.integer(ceiling(max(sum(employed), n * 0.6) / 3.5)))
}

.normalise_blade_industry <- function(x) {
  x <- as.integer(x)
  x[is.na(x)] <- 1L
  pmin(pmax(x, 1L), 19L)
}

.normalise_blade_state <- function(x) {
  x <- as.integer(x)
  x[is.na(x)] <- 1L
  pmin(pmax(x, 1L), 8L)
}

.blade_assign_business_by_state <- function(person_state, business_state, seed,
                                            salt = 0L,
                                            same_state_rate = 0.9,
                                            avoid = NULL) {
  n <- length(person_state)
  if (n == 0L) return(integer(0))
  n_businesses <- length(business_state)
  if (n_businesses == 0L) return(integer(n))

  person_state <- .normalise_blade_state(person_state)
  business_state <- .normalise_blade_state(business_state)
  same_state_rate <- pmin(pmax(as.numeric(same_state_rate), 0), 1)

  assigned <- integer(n)
  all_businesses <- seq_len(n_businesses)
  draw <- ((as.numeric(seq_len(n)) * 1103515245 + seed * 1009 + salt) %%
             1000) / 1000

  for (st in sort(unique(person_state))) {
    idx <- which(person_state == st)
    state_pool <- which(business_state == st)
    use_state <- length(state_pool) > 0L & draw[idx] < same_state_rate

    if (any(use_state)) {
      state_idx <- idx[use_state]
      pick <- as.integer(
        (as.numeric(seq_along(state_idx)) * 2654435761 +
           seed * 37 + salt + st * 9176) %% length(state_pool)
      ) + 1L
      assigned[state_idx] <- state_pool[pick]
    }

    other_idx <- idx[!use_state]
    if (length(other_idx)) {
      pick <- as.integer(
        (as.numeric(seq_along(other_idx)) * 2246822519 +
           seed * 101 + salt + st * 3571) %% n_businesses
      ) + 1L
      assigned[other_idx] <- all_businesses[pick]
    }
  }

  if (!is.null(avoid) && length(avoid) == n) {
    same <- assigned == avoid
    if (any(same) && n_businesses > 1L) {
      assigned[same] <- (assigned[same] %% n_businesses) + 1L
    }
  }

  assigned
}

.blade_anzsic06 <- function(industry, seq_business, seed) {
  industry <- .normalise_blade_industry(industry)
  base <- c(111L, 600L, 1111L, 2611L, 3011L, 3211L, 3911L,
            4511L, 4610L, 5801L, 6221L, 6711L, 6910L, 7211L,
            7510L, 8010L, 8401L, 8910L, 9201L)
  out <- sprintf("%04d", base[industry] +
                   as.integer((seq_business * 7L + seed) %% 20L))

  health <- industry == 17L
  if (any(health)) {
    health_codes <- c("8401", "8511", "8520", "8531", "8532",
                      "8533", "8534", "8591", "8601", "8790")
    out[health] <- health_codes[
      as.integer((seq_business[health] + seed) %% length(health_codes)) + 1L
    ]
  }
  out
}

.blade_health_occupation_reference <- function() {
  data.frame(
    ANZSCO_CODE = c("253111", "253112", "253999", "254411",
                   "254412", "254499", "251211", "252411",
                   "423111", "411711"),
    ANZSCO_TITLE = c("General Practitioner", "Resident Medical Officer",
                     "Medical Practitioners nec", "Nurse Practitioner",
                     "Registered Nurse (Aged Care)",
                     "Registered Nurses nec", "Medical Diagnostic Radiographer",
                     "Occupational Therapist", "Aged or Disabled Carer",
                     "Community Worker"),
    stringsAsFactors = FALSE
  )
}

.normalise_blade_anzsco <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x) | x == "0"] <- "000000"
  out <- sprintf("%06.0f", suppressWarnings(as.numeric(x)))
  out[is.na(out)] <- "000000"
  out
}

.blade_anzsco_group <- function(code, digits) {
  code <- .normalise_blade_anzsco(code)
  substr(code, 1L, digits)
}

.make_blade_business_spine <- function(spine, seed, n_businesses = NULL) {
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))
  n_persons <- nrow(spine)
  if (n_persons == 0L) {
    stop("Cannot create a BLADE business spine from an empty person spine.",
         call. = FALSE)
  }

  # Rust-backed business-spine builder (port stage 1). The R implementation
  # below is retained as a fallback when the compiled function is unavailable.
  if (exists("make_blade_business_spine__", mode = "function")) {
    has <- function(col) col %in% names(spine)
    raw <- make_blade_business_spine__(
      spine_state       = as.integer(spine$state),
      spine_industry    = if (has("industry")) as.integer(spine$industry) else NULL,
      spine_sector      = if (has("sector")) as.integer(spine$sector) else NULL,
      baseline_employed = as.integer(spine$baseline_employed),
      baseline_income   = as.numeric(spine$baseline_income),
      spine_id          = as.character(spine$spine_id),
      aeuid_ato         = if (has("aeuid_ato")) as.character(spine$aeuid_ato) else NULL,
      anzsco_code       = if (has("anzsco_code")) as.character(spine$anzsco_code) else NULL,
      anzsco_title      = if (has("anzsco_title")) as.character(spine$anzsco_title) else NULL,
      seed              = as.integer(seed),
      n_businesses      = if (is.null(n_businesses)) NULL else as.integer(n_businesses)
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
    names(df)[names(df) == "fn_"] <- "fn"
    return(df)
  }
  if (is.null(n_businesses)) {
    n_businesses <- .blade_default_business_count(spine)
  }
  n_businesses <- as.integer(n_businesses)
  stopifnot(n_businesses > 0L)

  seq_business <- seq_len(n_businesses)
  bg_group <- ((seq_business - 1L) %/% 4L) + 1L
  representative_idx <- as.integer(
    ((as.numeric(seq_business) * 7919 + seed * 101) %% n_persons) + 1
  )

  state <- .normalise_blade_state(spine$state[representative_idx])
  industry <- if ("industry" %in% names(spine)) {
    .normalise_blade_industry(spine$industry[representative_idx])
  } else {
    ((seq_business + seed) %% 19L) + 1L
  }
  sector <- if ("sector" %in% names(spine)) {
    as.integer(spine$sector[representative_idx])
  } else {
    rep(1L, n_businesses)
  }
  sector[is.na(sector)] <- 1L

  employment_count <- integer(n_businesses)
  annual_wages <- numeric(n_businesses)

  employed_idx <- which(spine$baseline_employed == 1L &
                          !is.na(spine$baseline_income) &
                          spine$baseline_income > 0)
  if (length(employed_idx) > 0L) {
    assigned_business <- .blade_assign_business_by_state(
      person_state = spine$state[employed_idx],
      business_state = state,
      seed = seed,
      salt = 11L,
      same_state_rate = 0.92
    )
    employment_count <- tabulate(assigned_business, nbins = n_businesses)

    wages_by_business <- rowsum(
      as.numeric(spine$baseline_income[employed_idx]),
      assigned_business,
      reorder = FALSE
    )
    wage_rows <- as.integer(rownames(wages_by_business))
    annual_wages[wage_rows] <- as.numeric(wages_by_business[, 1L])

    first_employee_pos <- match(seq_business, assigned_business)
    has_employee <- !is.na(first_employee_pos)
    employee_rep <- employed_idx[first_employee_pos[has_employee]]
    state[has_employee] <- .normalise_blade_state(spine$state[employee_rep])
    if ("industry" %in% names(spine)) {
      industry[has_employee] <-
        .normalise_blade_industry(spine$industry[employee_rep])
    }
    if ("sector" %in% names(spine)) {
      sector[has_employee] <- as.integer(spine$sector[employee_rep])
      sector[is.na(sector)] <- 1L
    }
    representative_idx[has_employee] <- employee_rep
  }

  force_health <- ((bg_group + seed) %% 9L) == 0L
  if (!any(force_health) && n_businesses >= 5L) {
    force_health[bg_group == bg_group[ceiling(n_businesses / 2)]] <- TRUE
  }
  industry[force_health] <- 17L
  health_group <- as.logical(ave(as.integer(industry == 17L), bg_group,
                                 FUN = max))
  industry[health_group] <- 17L

  industry_division <- LETTERS[industry]
  anzsic06 <- .blade_anzsic06(industry, seq_business, seed)
  business_birth_year <- 1992L +
    as.integer((as.numeric(seq_business) * 37 + seed) %% 33)
  business_birth_year[((seq_business + seed) %% 11L) == 0L] <- 1993L
  business_birth_year[((seq_business * 3L + seed) %% 13L) == 0L] <- 2001L
  business_birth_year[((seq_business * 7L + seed) %% 23L) == 0L] <-
    NA_integer_
  exit_draw <- as.integer((as.numeric(seq_business) * 53 + seed) %% 100)
  exit_birth_year <- ifelse(is.na(business_birth_year),
                            2001L, business_birth_year)
  exit_year <- ifelse(
    exit_draw < 12L & exit_birth_year < 2020L,
    pmin(2024L, exit_birth_year + 1L +
           as.integer((seq_business * 17L + seed) %%
                        pmax(1L, 2025L - exit_birth_year))),
    NA_integer_
  )
  alive_status <- ifelse(is.na(exit_year) | exit_year >= 2024L, 1L, 0L)

  non_employer_turnover <- 30000 + ((seq_business * 7919 + seed) %% 170000)
  turnover <- pmax(
    annual_wages * (1.65 + industry / 25),
    ifelse(employment_count > 0L, annual_wages * 1.25,
           non_employer_turnover)
  )
  turnover <- round(turnover, 2)

  profiled_seed <- employment_count >= 8L | turnover >= 1e6 |
    ((bg_group + seed) %% 5L) == 0L
  profiled <- as.logical(ave(as.integer(profiled_seed), bg_group,
                             FUN = max))
  profiled[tabulate(bg_group)[bg_group] < 2L] <- FALSE
  public_sector <- industry_division == "Q" &
    ((seq_business + seed) %% 4L) == 0L
  legal_form <- ifelse(
    employment_count == 0L | ((seq_business + seed) %% 7L) == 0L,
    "Sole trader",
    c("Company", "Partnership", "Trust")[
      ((seq_business + seed) %% 3L) + 1L
    ]
  )
  legal_form[industry_division == "Q" & !public_sector] <- "Company"
  tolo <- match(legal_form, c("Company", "Sole trader", "Partnership",
                              "Trust"))
  x_sector <- ifelse(public_sector, 2L, 1L)
  non_financial_investment <- !public_sector & legal_form != "Sole trader" &
    ((seq_business + seed) %% 19L) == 0L
  sisca08 <- ifelse(
    public_sector,
    "3000",
    ifelse(legal_form == "Sole trader", "4000",
           ifelse(non_financial_investment, "1001", "1009"))
  )
  sisca_sector <- ifelse(public_sector, "Public",
                         ifelse(legal_form == "Sole trader",
                                "Household business", "Private"))
  hcnt_delta <- ifelse(
    employment_count > 0L & ((seq_business + seed) %% 5L) == 0L,
    c(-1L, 1L, 2L)[((seq_business + seed) %% 3L) + 1L],
    0L
  )
  hcnt <- pmax(0L, employment_count + hcnt_delta)
  fte <- round(pmax(0, hcnt * (0.68 + ((seq_business + seed) %% 23L) / 100)), 1)

  representative_anzsco_code <- .normalise_blade_anzsco(
    .optional_chr(spine, "anzsco_code", representative_idx, "000000")
  )
  representative_anzsco_title <- .optional_chr(
    spine, "anzsco_title", representative_idx, "Occupation not stated"
  )
  representative_anzsco_title[
    is.na(representative_anzsco_title) | !nzchar(representative_anzsco_title)
  ] <- "Occupation not stated"
  force_representative_health <- industry_division == "Q"
  if (any(force_representative_health)) {
    health_ref <- .blade_health_occupation_reference()
    ref_idx <- as.integer(
      (seq_len(sum(force_representative_health)) + seed) %% nrow(health_ref)
    ) + 1L
    representative_anzsco_code[force_representative_health] <-
      health_ref$ANZSCO_CODE[ref_idx]
    representative_anzsco_title[force_representative_health] <-
      health_ref$ANZSCO_TITLE[ref_idx]
  }

  data.frame(
    blade_business_id = .blade_numeric_id("B", seq_business, 9L),
    bn = .blade_numeric_id(
      "BN",
      (as.numeric(seq_business) * 1000003 + seed * 9176) %% 100000000000,
      11L
    ),
    id = .blade_numeric_id("E",
                           (seq_business * 100003 + seed) %% 1000000000,
                           9L),
    bg_id = ifelse(
      profiled,
      .blade_numeric_id("BG",
                        (bg_group + seed * 13L) %% 10000000000,
                        10L),
      ""
    ),
    cn = .blade_numeric_id("C",
                           (seq_business * 300007 + seed) %% 1000000000,
                           9L),
    fn = .blade_numeric_id("F",
                           (seq_business * 700001 + seed) %% 1000000000,
                           9L),
    representative_spine_id = spine$spine_id[representative_idx],
    representative_aeuid_ato = if ("aeuid_ato" %in% names(spine)) {
      spine$aeuid_ato[representative_idx]
    } else {
      NA_character_
    },
    state = state,
    state_code = state,
    anzsic06 = anzsic06,
    industry_division = industry_division,
    d_div06 = industry_division,
    latest_div06 = industry_division,
    x_anzsic06 = anzsic06,
    # The bundled DIL has no historical classification crosswalk. Use the
    # current synthetic classification as a local compatibility value instead
    # of filling every historical field with an unknown-value sentinel.
    x_anzsic93 = anzsic06,
    d_anzsic06 = anzsic06,
    cast_anzsic06 = anzsic06,
    latest_anzsic06 = anzsic06,
    x_sisca08 = sisca08,
    x_sisca06 = sisca08,
    x_sisca93 = sisca08,
    d_sisca08 = sisca08,
    cast_sisca08 = sisca08,
    latest_sisca08 = sisca08,
    sisca_sector = sisca_sector,
    x_sector = x_sector,
    cast_sector = x_sector,
    x_state = state,
    cast_state = state,
    x_st_op = sprintf("%09d", state * 10000000L + seq_business),
    cast_st_op = sprintf("%09d", state * 10000000L + seq_business),
    x_al_st = alive_status,
    x_pcode = sprintf("%04d", 2000L + (state * 173L + seq_business) %% 7000L),
    cast_pcode = sprintf("%04d", 2000L + (state * 173L + seq_business) %% 7000L),
    x_npi = ifelse(public_sector, 1L, 2L),
    cast_npi = ifelse(public_sector, 1L, 2L),
    industry = industry,
    sector = sector,
    business_birth_year = business_birth_year,
    business_exit_year = exit_year,
    alive_status = alive_status,
    employment_count = employment_count,
    payg_employee_count = employment_count,
    stp_employee_count = employment_count,
    annual_wages = round(annual_wages, 2),
    hcnt = hcnt,
    fte = fte,
    payg_reported_hcnt = hcnt,
    payg_actual_hcnt = employment_count,
    linked_payg_rows = employment_count,
    linked_distinct_persons = employment_count,
    payg_link_hcnt_gap = hcnt - employment_count,
    payg_hcnt_delta = hcnt - employment_count,
    payg_headcount_mismatch = as.integer(hcnt != employment_count),
    d_total_payees = employment_count,
    turnover = turnover,
    bas_total_sales = turnover,
    bas_wages = round(annual_wages, 2),
    bit_total_income = round(turnover * 0.97, 2),
    bit_taxable_income = round(pmax(turnover - annual_wages * 1.08, 0), 2),
    gst_payable = round(pmax(turnover * 0.1 - annual_wages * 0.015, 0), 2),
    capital_expenditure = round(turnover * (0.02 + industry / 1000), 2),
    rd_expenditure = round(ifelse(industry %in% c(3L, 10L, 11L, 18L),
                                  turnover * 0.025, turnover * 0.004), 2),
    export_value = round(ifelse(industry %in% c(1L, 2L, 3L, 11L),
                                turnover * 0.12, turnover * 0.015), 2),
    import_value = round(ifelse(industry %in% c(3L, 7L, 9L),
                                turnover * 0.10, turnover * 0.02), 2),
    legal_form = legal_form,
    x_tolo = tolo,
    cast_tolo = tolo,
    tolo = tolo,
    private_public = ifelse(public_sector, "public", "private"),
    health_industry_flag = as.integer(industry_division == "Q"),
    public_health_flag = as.integer(public_sector),
    representative_anzsco_code = representative_anzsco_code,
    representative_anzsco_title = representative_anzsco_title,
    representative_health_occupation_flag =
      as.integer(force_representative_health),
    is_employing = as.integer(employment_count > 0L),
    is_profiled = as.integer(profiled),
    source_person_n = employment_count,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Move a slice of the business spine from the anchor to a table's reference year.
#
# The spine draws turnover and wages once, at the calendar-2021 anchor, and every
# BLADE table is one cross-section of it. The tables do not share a period: the
# 62 of them run from 2019-20 to 2025-26, so leaving the slice at its anchor
# value gives a business the same turnover six years apart, and a user comparing
# two BLADE tables measures nominal growth of zero.
#
# Turnover follows the business series and wages the wage series, because the two
# grew at different rates. That divergence is the point: it is what makes the
# labour share of turnover move between one table's period and another's, and a
# single series applied to both would freeze it. It does mean turnover stops
# being the exact multiple of wages that `.make_blade_business_spine()` builds it
# as, and that is deliberate: holding that ratio fixed forever is the same thing
# as declaring the labour share constant for thirty years.
#
# Only turnover carries the per-business deviation. `annual_wages` is a rowsum of
# its employees' incomes, and the Rust spine records the invariant
# `sum(annual_wages) == sum(income[employed])`; giving the wage bill its own
# business-sized shock would break it and put BLADE's payroll out of step with
# the same people's payment summaries. Individual employees do depart from the
# headline, but a firm's wage bill is a sum over them, and those departures
# largely cancel in the sum.
.blade_reprice_business_rows <- function(business_rows, table_number, seed) {
  n <- nrow(business_rows)
  if (n == 0L) return(business_rows)
  if (!all(c("turnover", "annual_wages", "bn") %in% names(business_rows))) {
    return(business_rows)
  }
  period <- .blade_nominal_period_for_table(table_number)
  if (is.na(period$year)) return(business_rows)

  business_level <- nominal_index("business", period$year, period$basis)
  wage_level <- nominal_index("wage", period$year, period$basis)
  if (length(business_level) != 1L || is.na(business_level)) business_level <- 1
  if (length(wage_level) != 1L || is.na(wage_level)) wage_level <- 1

  unit <- .nominal_unit_key(business_rows$bn)
  deviation <- exp(.nominal_unit_deviation(unit, seed, period$year,
                                           .NOMINAL_BUSINESS_DISPERSION))
  turnover <- round(as.numeric(business_rows$turnover) *
                      business_level * deviation, 2)
  wages <- round(as.numeric(business_rows$annual_wages) * wage_level, 2)
  business_rows$turnover <- turnover
  business_rows$annual_wages <- wages

  # The rest of the money columns are fixed multiples of turnover and wages, so
  # they are rebuilt from the repriced pair rather than scaled one by one.
  # Scaling each separately would leave bit_taxable_income disagreeing with the
  # turnover and wages it is defined from, and the accounting identities in the
  # spine are the thing a user checks first. These expressions must stay in step
  # with .make_blade_business_spine().
  industry <- as.integer(business_rows$industry)
  derived <- list(
    bas_total_sales = turnover,
    bas_wages = wages,
    bit_total_income = round(turnover * 0.97, 2),
    bit_taxable_income = round(pmax(turnover - wages * 1.08, 0), 2),
    gst_payable = round(pmax(turnover * 0.1 - wages * 0.015, 0), 2),
    capital_expenditure = round(turnover * (0.02 + industry / 1000), 2),
    rd_expenditure = round(ifelse(industry %in% c(3L, 10L, 11L, 18L),
                                  turnover * 0.025, turnover * 0.004), 2),
    export_value = round(ifelse(industry %in% c(1L, 2L, 3L, 11L),
                                turnover * 0.12, turnover * 0.015), 2),
    import_value = round(ifelse(industry %in% c(3L, 7L, 9L),
                                turnover * 0.10, turnover * 0.02), 2)
  )
  for (name in names(derived)) {
    if (name %in% names(business_rows)) business_rows[[name]] <- derived[[name]]
  }
  business_rows
}

.write_blade_business_spine <- function(business_spine, run_dir, format) {
  sys_dir <- file.path(run_dir, "_system")
  if (!dir.exists(sys_dir)) dir.create(sys_dir, recursive = TRUE)
  path <- .blade_business_spine_path(run_dir, format)
  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for parquet output.",
           call. = FALSE)
    }
    arrow::write_parquet(business_spine, path)
  } else {
    utils::write.csv(business_spine, path, row.names = FALSE)
  }
  message("BLADE business spine saved to ", path)
  invisible(path)
}

.blade_employee_assignments <- function(spine, business_spine, seed) {
  n_businesses <- nrow(business_spine)
  empty <- data.frame(
    spine_row = integer(0),
    spine_id = character(0),
    SYNTHETIC_AEUID = character(0),
    SYNTHETIC_AEUID_ABS = character(0),
    SYNTHETIC_AEUID_DHDA = character(0),
    bn = character(0),
    id = character(0),
    bg_id = character(0),
    job_number = integer(0),
    primary_job = integer(0),
    annual_wage = numeric(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (is.null(spine) || n_businesses == 0L) return(empty)

  employed_idx <- which(spine$baseline_employed == 1L &
                          !is.na(spine$baseline_income) &
                          spine$baseline_income > 0)
  if (length(employed_idx) == 0L) return(empty)

  assigned_business <- .blade_assign_business_by_state(
    person_state = spine$state[employed_idx],
    business_state = business_spine$state,
    seed = seed,
    salt = 31L,
    same_state_rate = 0.92
  )
  rows <- business_spine[assigned_business, , drop = FALSE]
  primary <- data.frame(
    spine_row = employed_idx,
    spine_id = spine$spine_id[employed_idx],
    SYNTHETIC_AEUID = .optional_chr(spine, "aeuid_ato", employed_idx),
    SYNTHETIC_AEUID_ABS = .optional_chr(spine, "aeuid_abs", employed_idx),
    SYNTHETIC_AEUID_DHDA = .optional_chr(spine, "aeuid_dhda", employed_idx),
    bn = rows$bn,
    id = rows$id,
    bg_id = rows$bg_id,
    job_number = 1L,
    primary_job = 1L,
    annual_wage = as.numeric(spine$baseline_income[employed_idx]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  secondary_pos <- which(((as.numeric(seq_along(employed_idx)) * 48271 +
                            seed * 37) %% 100) < 12)
  if (length(secondary_pos) == 0L && length(employed_idx) >= 8L) {
    secondary_pos <- 1L
  }
  if (length(secondary_pos) == 0L) return(primary)

  secondary_idx <- employed_idx[secondary_pos]
  assigned_secondary <- .blade_assign_business_by_state(
    person_state = spine$state[secondary_idx],
    business_state = business_spine$state,
    seed = seed,
    salt = 73L,
    same_state_rate = 0.8,
    avoid = assigned_business[secondary_pos]
  )
  secondary_rows <- business_spine[assigned_secondary, , drop = FALSE]
  secondary <- data.frame(
    spine_row = secondary_idx,
    spine_id = spine$spine_id[secondary_idx],
    SYNTHETIC_AEUID = .optional_chr(spine, "aeuid_ato", secondary_idx),
    SYNTHETIC_AEUID_ABS = .optional_chr(spine, "aeuid_abs", secondary_idx),
    SYNTHETIC_AEUID_DHDA = .optional_chr(spine, "aeuid_dhda", secondary_idx),
    bn = secondary_rows$bn,
    id = secondary_rows$id,
    bg_id = secondary_rows$bg_id,
    job_number = 2L,
    primary_job = 0L,
    annual_wage = round(as.numeric(spine$baseline_income[secondary_idx]) *
                          (0.12 + (secondary_pos %% 9L) / 100), 2),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  rbind(primary, secondary)
}

.make_blade_person_link <- function(spine, business_spine, seed) {
  n_businesses <- nrow(business_spine)
  if (n_businesses == 0L) return(data.frame())

  # Rust-backed link builder (port stage 2); R fallback retained below.
  if (exists("make_blade_person_link__", mode = "function")) {
    n <- nrow(spine)
    col_or <- function(name, default) {
      if (name %in% names(spine)) spine[[name]] else rep(default, n)
    }
    raw <- make_blade_person_link__(
      spine_state       = as.integer(spine$state),
      baseline_employed = as.integer(spine$baseline_employed),
      baseline_income   = as.numeric(spine$baseline_income),
      spine_id          = as.character(spine$spine_id),
      aeuid_ato         = as.character(col_or("aeuid_ato", NA_character_)),
      aeuid_abs         = as.character(col_or("aeuid_abs", NA_character_)),
      aeuid_dhda        = as.character(col_or("aeuid_dhda", NA_character_)),
      anzsco_code       = as.character(col_or("anzsco_code", NA_character_)),
      anzsco_title      = as.character(col_or("anzsco_title", NA_character_)),
      birth_year        = as.integer(col_or("birth_year", NA_integer_)),
      sex               = as.integer(col_or("sex", NA_integer_)),
      bs_state          = as.integer(business_spine$state),
      bs_bn             = as.character(business_spine$bn),
      bs_id             = as.character(business_spine$id),
      bs_bg_id          = as.character(business_spine$bg_id),
      bs_health_flag    = as.integer(business_spine$health_industry_flag),
      seed              = as.integer(seed)
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE))
  }

  aeuid_ato <- .optional_chr(spine, "aeuid_ato")
  aeuid_abs <- .optional_chr(spine, "aeuid_abs")
  aeuid_dhda <- .optional_chr(spine, "aeuid_dhda")

  links <- list()
  k <- 0L

  employee_links <- .blade_employee_assignments(spine, business_spine, seed)
  if (nrow(employee_links) > 0L) {
    occ <- .blade_assignment_occupation_codes(
      spine, employee_links, business_spine, seed
    )
    employee_birth_year <- .optional_num(
      spine, "birth_year", employee_links$spine_row, NA_real_
    )
    employee_sex <- .optional_num(
      spine, "sex", employee_links$spine_row, NA_real_
    )
    # Every row below is stamped FIN_YEAR 2023-24, so its wage is moved to
    # 2023-24. The same factor is applied to the EEH frame built from this link,
    # keyed on the same identifier, so the two agree on a person's pay.
    link_wage_factor <- .nominal_unit_factor(
      "wage", .blade_nominal_period("2023-24")$year,
      unit = .nominal_unit_key(employee_links$SYNTHETIC_AEUID),
      seed = seed,
      dispersion = .NOMINAL_PERSON_DISPERSION
    )
    k <- k + 1L
    links[[k]] <- data.frame(
      spine_id = employee_links$spine_id,
      SYNTHETIC_AEUID = employee_links$SYNTHETIC_AEUID,
      SYNTHETIC_AEUID_ATO = employee_links$SYNTHETIC_AEUID,
      SYNTHETIC_AEUID_ABS = employee_links$SYNTHETIC_AEUID_ABS,
      SYNTHETIC_AEUID_DHDA = employee_links$SYNTHETIC_AEUID_DHDA,
      synthetic_aeuid_abs = employee_links$SYNTHETIC_AEUID_ABS,
      synthetic_aeuid_dhda = employee_links$SYNTHETIC_AEUID_DHDA,
      bn = employee_links$bn,
      BN = employee_links$bn,
      ABN_HASH_TRUNC = employee_links$bn,
      id = employee_links$id,
      bg_id = employee_links$bg_id,
      relationship_type = ifelse(employee_links$primary_job == 1L,
                                 "employee", "employee_secondary_job"),
      source_dataset = "STP",
      job_number = employee_links$job_number,
      primary_job = employee_links$primary_job,
      FIN_YEAR = "2023-24",
      tsid = "24",
      ANZSCO_CODE = occ$ANZSCO_CODE,
      ANZSCO_TITLE = occ$ANZSCO_TITLE,
      occupation_health_flag = occ$occupation_health_flag,
      # The row declares FIN_YEAR 2023-24, so the wage on it has to be a
      # 2023-24 wage. `employee_links$annual_wage` comes off the spine and is a
      # calendar-2021 amount, and the EEH frame built from this same link moves
      # it; leaving it here would put the same person's pay at two different
      # vintages in one run.
      annual_wage = round(employee_links$annual_wage * link_wage_factor, 2),
      PAYG_GROSS_WAGES = round(employee_links$annual_wage * link_wage_factor, 2),
      birth_year = as.integer(employee_birth_year),
      age = as.integer(2024L - employee_birth_year),
      sex = as.integer(employee_sex),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  age <- if ("birth_year" %in% names(spine)) {
    2024L - as.integer(spine$birth_year)
  } else {
    rep(40L, nrow(spine))
  }
  adult_idx <- which(age >= 20L & age <= 70L)
  if (length(adult_idx) > 0L) {
    draw <- (seq_along(adult_idx) * 2654435761 + seed * 2200L) %%
      1000000L
    owner_idx <- adult_idx[draw < 80000L]
    if (length(owner_idx) > 0L) {
      assigned_business <- .blade_assign_business_by_state(
        person_state = spine$state[owner_idx],
        business_state = business_spine$state,
        seed = seed,
        salt = 101L,
        same_state_rate = 0.9
      )
      rows <- business_spine[assigned_business, , drop = FALSE]
      k <- k + 1L
      links[[k]] <- data.frame(
        spine_id = spine$spine_id[owner_idx],
        SYNTHETIC_AEUID = aeuid_ato[owner_idx],
        SYNTHETIC_AEUID_ATO = aeuid_ato[owner_idx],
        SYNTHETIC_AEUID_ABS = aeuid_abs[owner_idx],
        SYNTHETIC_AEUID_DHDA = aeuid_dhda[owner_idx],
        synthetic_aeuid_abs = aeuid_abs[owner_idx],
        synthetic_aeuid_dhda = aeuid_dhda[owner_idx],
        bn = rows$bn,
        BN = rows$bn,
        ABN_HASH_TRUNC = rows$bn,
        id = rows$id,
        bg_id = rows$bg_id,
        relationship_type = "owner",
        source_dataset = "BUSOWN",
        job_number = NA_integer_,
        primary_job = NA_integer_,
        FIN_YEAR = "2023-24",
        tsid = "24",
        ANZSCO_CODE = NA_character_,
        ANZSCO_TITLE = NA_character_,
        occupation_health_flag = 0L,
        annual_wage = NA_real_,
        PAYG_GROSS_WAGES = NA_real_,
        birth_year = as.integer(.optional_num(spine, "birth_year", owner_idx,
                                              NA_real_)),
        age = as.integer(age[owner_idx]),
        sex = as.integer(.optional_num(spine, "sex", owner_idx, NA_real_)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }

  if (!length(links)) {
    return(data.frame(
      spine_id = character(0),
      SYNTHETIC_AEUID = character(0),
      SYNTHETIC_AEUID_ATO = character(0),
      SYNTHETIC_AEUID_ABS = character(0),
      SYNTHETIC_AEUID_DHDA = character(0),
      synthetic_aeuid_abs = character(0),
      synthetic_aeuid_dhda = character(0),
      bn = character(0),
      BN = character(0),
      ABN_HASH_TRUNC = character(0),
      id = character(0),
      bg_id = character(0),
      relationship_type = character(0),
      source_dataset = character(0),
      job_number = integer(0),
      primary_job = integer(0),
      FIN_YEAR = character(0),
      tsid = character(0),
      ANZSCO_CODE = character(0),
      ANZSCO_TITLE = character(0),
      occupation_health_flag = integer(0),
      annual_wage = numeric(0),
      PAYG_GROSS_WAGES = numeric(0),
      birth_year = integer(0),
      age = integer(0),
      sex = integer(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  do.call(rbind, links)
}

.write_blade_plida_link <- function(link, run_dir, format) {
  sys_dir <- file.path(run_dir, "_system")
  if (!dir.exists(sys_dir)) dir.create(sys_dir, recursive = TRUE)
  path <- .blade_plida_link_path(run_dir, format)
  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for parquet output.",
           call. = FALSE)
    }
    arrow::write_parquet(link, path)
  } else {
    utils::write.csv(link, path, row.names = FALSE)
  }
  message("PLIDA-BLADE link saved to ", path)
  invisible(path)
}

.add_blade_link_reconciliation <- function(business_spine, link) {
  if (is.null(link) || nrow(link) == 0L) {
    business_spine$linked_payg_rows <- 0L
    business_spine$linked_distinct_persons <- 0L
  } else {
    employee <- link$relationship_type %in%
      c("employee", "employee_secondary_job")
    employee_link <- link[employee, , drop = FALSE]
    if (nrow(employee_link) == 0L) {
      row_counts <- stats::setNames(integer(0), character(0))
      person_counts <- stats::setNames(integer(0), character(0))
    } else {
      row_counts <- table(employee_link$BN)
      person_counts <- tapply(
        employee_link$SYNTHETIC_AEUID,
        employee_link$BN,
        function(x) length(unique(x))
      )
    }
    business_spine$linked_payg_rows <-
      as.integer(row_counts[business_spine$bn])
    business_spine$linked_distinct_persons <-
      as.integer(person_counts[business_spine$bn])
    business_spine$linked_payg_rows[
      is.na(business_spine$linked_payg_rows)
    ] <- 0L
    business_spine$linked_distinct_persons[
      is.na(business_spine$linked_distinct_persons)
    ] <- 0L
  }
  business_spine$payg_actual_hcnt <- business_spine$linked_distinct_persons
  business_spine$d_total_payees <- business_spine$linked_distinct_persons
  business_spine$payg_link_hcnt_gap <-
    business_spine$hcnt - business_spine$linked_distinct_persons
  business_spine$payg_hcnt_delta <- business_spine$payg_link_hcnt_gap
  business_spine$payg_headcount_mismatch <-
    as.integer(business_spine$payg_link_hcnt_gap != 0L)
  business_spine
}

.select_blade_frame_rows <- function(frame, seed, sample_rate, max_rows) {
  n <- nrow(frame)
  if (n == 0L) return(frame)
  target <- ceiling(n * sample_rate)
  if (is.finite(max_rows)) target <- min(target, as.integer(max_rows))
  target <- max(1L, min(n, as.integer(target)))
  if (target >= n) return(frame)
  key <- (seq_len(n) * 1103515245 + seed * 12345) %% 2147483647
  frame[sort(order(key)[seq_len(target)]), , drop = FALSE]
}

.blade_survey_table_numbers <- function() {
  # ABS business surveys and their requestable survey-weight/commodity tables.
  # The remaining BLADE tables are treated as administrative/key records and
  # are generated for the full business spine.
  c(8L:23L, 38L:46L)
}

.blade_is_survey_table <- function(table_number, product_name = NULL) {
  table_number <- as.integer(table_number)
  table_number %in% .blade_survey_table_numbers()
}

.blade_table_source_type <- function(table_number, product_name = NULL) {
  ifelse(
    .blade_is_survey_table(table_number, product_name),
    "survey",
    "administrative"
  )
}

.blade_employee_link_rows <- function(link) {
  if (is.null(link)) return(data.frame())
  if (nrow(link) == 0L) return(link[0L, , drop = FALSE])
  link[link$relationship_type %in% c("employee", "employee_secondary_job"),
       , drop = FALSE]
}

.make_blade_eeh_frame <- function(variable_names, link, business_spine, seed) {
  variable_names <- unique(variable_names[nzchar(variable_names)])
  link <- .blade_employee_link_rows(link)
  if (nrow(link) == 0L) {
    out <- stats::setNames(rep(list(logical(0)), length(variable_names)),
                           variable_names)
    return(as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE))
  }

  business_rows <- business_spine[
    match(link$BN, business_spine$bn), , drop = FALSE
  ]
  n <- nrow(link)
  wage <- suppressWarnings(as.numeric(link$annual_wage))
  wage[is.na(wage)] <- 0
  # EEH earnings come off the person link, which carries the spine's anchor-year
  # income and never passes through the repriced business slice, so it needs its
  # own move to the table's period. The dispersion is the person one, not the
  # business one: these are one employee's weekly earnings, and they do not
  # scatter the way a firm's turnover does.
  eeh_period <- .blade_nominal_period_for_table(17L)
  wage <- wage * .nominal_unit_factor(
    "wage", eeh_period$year,
    unit = .nominal_unit_key(link$SYNTHETIC_AEUID),
    seed = seed,
    dispersion = .NOMINAL_PERSON_DISPERSION,
    basis = eeh_period$basis
  )
  weekly <- round(wage / 52, 2)
  hourly <- round(weekly / 38, 2)
  anzsco <- .normalise_blade_anzsco(link$ANZSCO_CODE)
  reference_year <- .blade_end_year(17L)
  birth_year <- suppressWarnings(as.integer(link$birth_year))
  age <- ifelse(!is.na(birth_year),
                as.integer(reference_year - birth_year),
                suppressWarnings(as.integer(link$age)))
  sex <- suppressWarnings(as.integer(link$sex))
  primary <- suppressWarnings(as.integer(link$primary_job))
  primary[is.na(primary)] <- 1L
  employee_indicator <- .blade_deidentified_id(
    "P",
    paste(link$SYNTHETIC_AEUID, link$BN, link$job_number, sep = "|"),
    seed = seed,
    width = 14L
  )

  value_for <- function(name) {
    lower <- tolower(name)
    if (lower == "id") return(link$id)
    if (lower == "eeh_version") return(rep("in217_v1", n))
    if (lower == "tsid") return(rep(.blade_tsid(17L), n))
    if (lower == "s_groupid_eeh") {
      return(ifelse(!is.na(link$bg_id) & nzchar(link$bg_id),
                    link$bg_id, link$id))
    }
    if (lower == "eid_eeh") return(employee_indicator)
    if (lower %in% c("swte_eeh", "sawote_eeh")) return(weekly)
    if (lower == "wovte_eeh") return(round(weekly * (1 - primary) * 0.15, 2))
    if (lower == "wass_eeh") return(round(weekly * 0.03, 2))
    if (lower %in% c("sahte_eeh", "sahote_eeh")) return(hourly)
    if (lower == "hovte_eeh") return(round(hourly * 1.5 * (1 - primary), 2))
    if (lower == "totwhpf_eeh") return(ifelse(primary == 1L, 38, 8))
    if (lower == "ordwhpf_eeh") return(ifelse(primary == 1L, 36, 8))
    if (lower == "ovtwhpf_eeh") return(ifelse(primary == 1L, 2, 0))
    if (lower == "agecat_eeh") {
      return(cut(age, c(-Inf, 24, 34, 44, 54, 64, Inf),
                 labels = FALSE))
    }
    if (lower == "age_eeh") return(age)
    if (lower == "casload_eeh") return(as.integer(primary == 0L))
    if (lower == "ftpt_eeh") return(ifelse(primary == 1L, 1L, 2L))
    if (lower == "manage_eeh") return(as.integer(substr(anzsco, 1L, 1L) == "1"))
    if (lower == "mannohrs_eeh") return(as.integer(primary == 1L))
    if (lower == "mosp_eeh") return(((seq_len(n) + seed) %% 6L) + 1L)
    if (grepl("anzsco[0-9]+_1_eeh", lower)) {
      return(.blade_anzsco_group(anzsco, 1L))
    }
    if (grepl("anzsco[0-9]+_2_eeh", lower)) {
      return(.blade_anzsco_group(anzsco, 2L))
    }
    if (grepl("anzsco[0-9]+_3_eeh", lower)) {
      return(.blade_anzsco_group(anzsco, 3L))
    }
    if (grepl("anzsco[0-9]+_4_eeh", lower)) {
      return(.blade_anzsco_group(anzsco, 4L))
    }
    # EEH pay frequency / rate-of-pay are numeric codes (metadata: "1 = Weekly",
    # "1 = Adult rate"), not the letters "W"/"H". Deterministic draw weighted to
    # the dominant code (weekly / adult rate).
    if (lower == "payfreq_eeh") {
      draw <- (seq_len(n) * 31L + seed) %% 100L
      return(ifelse(draw < 62L, 1L, ifelse(draw < 92L, 2L,
             ifelse(draw < 96L, 3L, 4L))))
    }
    if (lower == "rop_eeh") {
      draw <- (seq_len(n) * 17L + seed) %% 100L
      return(ifelse(draw < 82L, 1L, ifelse(draw < 94L, 2L, 3L)))
    }
    if (lower == "aj2012_eeh") return(rep(1L, n))
    if (lower == "sex_eeh") return(sex)
    if (lower == "typeemp_eeh") return(ifelse(primary == 1L, 1L, 2L))
    if (lower == "empstate_eeh") return(as.integer(business_rows$state))
    if (lower == "stateops_eeh") return(rep(1L, n))
    if (lower == "state_eeh") return(as.integer(business_rows$state))
    .blade_value_for(name, business_rows, 17L,
                     "blade-table-17-employee-earning-and-hours-eeh", seed)
  }

  out <- lapply(variable_names, value_for)
  names(out) <- variable_names
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Generate the BLADE business source-of-truth spine
#'
#' Creates the business-level source-of-truth dataset used by BLADE
#' projections. The output is written to
#' `_system/business-spine.parquet` in the active fplida run directory.
#' The companion `_system/plida-blade-link.parquet` is a synthetic
#' fplida relationship file for PLIDA-to-BLADE exercises; the public
#' BLADE correspondences are the DIL Appendix key products written by
#' `generate_blade(include_keys = TRUE)`.
#'
#' @param spine Data.frame from `generate_spine()` or NULL. If NULL,
#'   selected person-spine columns are loaded from the active run
#'   directory.
#' @param seed Integer. Random seed.
#' @param n_businesses Integer or NULL. Optional business universe size.
#'   NULL derives a size from the person spine employment base.
#' @param output_dir Character or NULL. Base output directory.
#' @param format Character. "parquet" or "csv".
#' @param return_data Logical. If TRUE, returns the business spine.
#' @param overwrite Logical. If FALSE and a business spine already
#'   exists, it is reused.
#'
#' @return A data.frame when `return_data = TRUE`, otherwise an
#'   invisible metadata list.
#' @export
generate_blade_business_spine <- function(spine = NULL, seed = 42L,
                                          n_businesses = NULL,
                                          output_dir = NULL,
                                          format = c("parquet", "csv"),
                                          return_data = TRUE,
                                          overwrite = FALSE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  run_dir <- resolve_run_dir(output_dir)
  existing_path <- .blade_business_spine_path(run_dir, format)
  if (!overwrite && file.exists(existing_path)) {
    business_spine <- .load_blade_business_spine(run_dir)
    if (return_data) return(business_spine)
    link_path <- .blade_plida_link_path(run_dir, format)
    return(invisible(list(
      n_businesses = nrow(business_spine),
      path = existing_path,
      link_path = if (file.exists(link_path)) link_path else NA_character_,
      reused = TRUE
    )))
  }

  spine_cols <- .blade_spine_cols()
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, spine_cols)
  }

  business_spine <- .make_blade_business_spine(
    spine = spine,
    seed = seed,
    n_businesses = n_businesses
  )
  link <- .make_blade_person_link(spine, business_spine, seed)
  business_spine <- .add_blade_link_reconciliation(business_spine, link)
  path <- .write_blade_business_spine(business_spine, run_dir, format)
  link_path <- .write_blade_plida_link(link, run_dir, format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) return(business_spine)
  invisible(list(
    n_businesses = nrow(business_spine),
    path = path,
    link_path = link_path,
    n_links = nrow(link),
    reused = FALSE
  ))
}

.resolve_blade_tables <- function(tables) {
  table_meta <- .blade_tables()
  if (identical(tables, "all")) return(table_meta)

  requested <- as.character(tables)
  requested_numbers <- suppressWarnings(as.integer(requested))
  rows <- table_meta[["Table.Number"]] %in% requested_numbers |
    table_meta[["Product.Name"]] %in% requested |
    table_meta[["Table.Name"]] %in% requested
  bad <- requested[!requested %in% as.character(table_meta[["Table.Number"]]) &
                     !requested %in% table_meta[["Product.Name"]] &
                     !requested %in% table_meta[["Table.Name"]]]
  if (length(bad) > 0L) {
    stop("Unknown BLADE tables: ", paste(bad, collapse = ", "),
         call. = FALSE)
  }
  table_meta[rows, , drop = FALSE]
}

.select_blade_rows <- function(business_spine, table_number, product_name,
                               seed, sample_rate, max_rows) {
  n <- nrow(business_spine)
  if (n == 0L) return(integer(0))
  target <- ceiling(n * sample_rate)
  if (is.finite(max_rows)) target <- min(target, as.integer(max_rows))
  target <- max(1L, min(n, as.integer(target)))
  if (target >= n) return(seq_len(n))

  salt <- .stable_name_seed(product_name) + as.integer(table_number) * 1009L
  key <- (as.numeric(seq_len(n)) * 1103515245 +
            as.numeric(seed) * 12345 + as.numeric(salt)) %% 2147483647
  sort(order(key)[seq_len(target)])
}

.blade_related_id <- function(prefix, business_rows, seed, salt,
                              width = 9L) {
  base <- seq_len(nrow(business_rows)) +
    as.integer((.blade_id_number(business_rows$bn) + seed + salt) %%
                 1000000000)
  .blade_numeric_id(prefix, base %% (10^width), width)
}

.blade_name_salt <- function(value) {
  code <- utf8ToInt(tolower(paste(value, collapse = "|")))
  hash <- 17
  for (i in seq_along(code)) {
    hash <- (hash * 131 + code[[i]] + i * 17) %% 2147483647
  }
  as.integer(hash)
}

.blade_draw <- function(business_rows, seed, salt, modulus) {
  n <- nrow(business_rows)
  if (n == 0L) return(integer())
  modulus <- as.integer(modulus)
  stopifnot(modulus > 0L)
  key <- .blade_id_number(business_rows$bn) %% 2147483647
  coefficient <- 104729 + as.integer(salt %% 1009L)
  row_coefficient <- 13007 + as.integer(salt %% 97L)
  mixed <- (
    key * coefficient +
      seq_len(n) * row_coefficient +
      as.numeric(seed) * 8191 +
      as.numeric(salt) * 65537
  ) %% 2147483647
  # A second modular mixing pass breaks the short cycles that a single linear
  # congruence can produce for small categorical domains (especially binary,
  # month and 12-level fields).
  scrambler <- 48271 + as.integer(salt %% 997L)
  mixed <- (
    mixed * scrambler +
      as.numeric(seq_len(n))^2 * 104729 +
      as.numeric(salt) * 8191
  ) %% 2147483647
  as.integer(mixed %% modulus)
}

.blade_amount <- function(business_rows, seed, salt, scale = 1) {
  draw <- .blade_draw(business_rows, seed, salt, 10000L) / 10000
  round(pmax(business_rows$turnover * scale * (0.6 + draw),
             business_rows$annual_wages * 0.1 * scale), 2)
}

.blade_pick_codes <- function(codes, n, seed, salt, include_missing = TRUE,
                              business_rows = NULL) {
  codes <- unique(as.integer(codes[!is.na(codes)]))
  if (!length(codes)) return(integer(n))
  missing_codes <- codes[codes %in% c(7777777L, 88888888L, 999999999L)]
  substantive <- setdiff(codes, missing_codes)
  if (!length(substantive)) substantive <- codes
  n <- as.integer(n)
  draw <- if (is.null(business_rows)) {
    (as.numeric(seq_len(n)) * (17L + salt %% 89L) +
       as.numeric(seed) + as.numeric(salt)) %%
      length(substantive)
  } else {
    .blade_draw(business_rows, seed, salt, length(substantive))
  }
  out <- substantive[draw + 1L]
  if (include_missing && length(missing_codes)) {
    missing_draw <- if (is.null(business_rows)) {
      (as.numeric(seq_len(n)) * (41L + salt %% 73L) +
         as.numeric(seed) + as.numeric(salt)) %% 100L
    } else {
      .blade_draw(business_rows, seed, salt + 7919L, 100L)
    }
    out[missing_draw >= 92L] <- missing_codes[
      ((as.numeric(seq_along(out[missing_draw >= 92L])) +
          as.numeric(seed) + as.numeric(salt)) %%
         length(missing_codes)) + 1L
    ]
  }
  as.integer(out)
}

.blade_valid_response_codes <- function(valid_response) {
  if (is.null(valid_response) || length(valid_response) == 0L ||
      is.na(valid_response)) {
    return(integer())
  }
  text <- as.character(valid_response)
  if (!nzchar(text)) return(integer())
  matches <- gregexpr("(?<![0-9])([0-9]{1,9})\\s*(=|-)", text,
                      perl = TRUE)
  pieces <- regmatches(text, matches)[[1L]]
  if (!length(pieces) || identical(pieces, character(0))) return(integer())
  as.integer(unique(gsub("[^0-9]", "", pieces)))
}

.blade_character_code <- function(n, seed, salt, width = 3L,
                                  prefix = "C") {
  values <- (as.numeric(seq_len(n)) * 17 + as.numeric(seed) +
               as.numeric(salt)) %% (10^width)
  paste0(prefix, sprintf(paste0("%0", width, "d"), values))
}

.blade_cycle_values <- function(values, n, seed, salt = 0L,
                                business_rows = NULL) {
  if (!length(values) || n == 0L) return(values[0L])
  draw <- if (is.null(business_rows)) {
    (as.numeric(seq_len(n)) * (17L + salt %% 89L) +
       as.numeric(seed) + as.numeric(salt)) %% length(values)
  } else {
    .blade_draw(business_rows, seed, salt, length(values))
  }
  values[draw + 1L]
}

.blade_pick_values <- function(values, business_rows, seed, salt,
                               missing_rate = 0) {
  values <- unique(values[!is.na(values) & nzchar(as.character(values))])
  n <- nrow(business_rows)
  if (!length(values) || n == 0L) return(values[0L])
  draw <- .blade_draw(business_rows, seed, salt, length(values))
  out <- values[draw + 1L]
  if (missing_rate > 0) {
    missing <- .blade_draw(business_rows, seed, salt + 48611L, 1000L) <
      round(1000 * missing_rate)
    out[missing] <- NA
  }
  out
}

.blade_ip_right_type <- function(business_rows, seed, table_number) {
  .blade_pick_values(
    c("design", "patent", "trade_mark"), business_rows, seed,
    9109L + as.integer(table_number)
  )
}

.blade_admin_character_value <- function(name, business_rows, table_number,
                                          seed, valid_response = "") {
  lower <- tolower(name)
  table_number <- as.integer(table_number)
  salt <- .blade_name_salt(name)
  pick <- function(values, missing_rate = 0) {
    .blade_pick_values(values, business_rows, seed, salt, missing_rate)
  }
  domain <- function(name) .blade_domain_values(domain = name)

  variable_values <- .blade_domain_values(variable_name = name)
  local_trade_fields <- c(
    "commodity_code_ex", "sitc_item_code_ex", "bec_group_code_im",
    "commodity_code_im", "preference_code_im", "sitc_item_code_im",
    "treatment_code_im"
  )
  if (lower %in% local_trade_fields && length(variable_values)) {
    # Keep fixed-width categorical codes as character values, including
    # leading zeroes. domains.csv identifies these as local plausible values,
    # not official CSM codeframe values.
    return(pick(variable_values))
  }
  if (length(variable_values) && table_number != 6L) {
    out <- pick(variable_values)
    if (all(grepl("^[0-9]+$", variable_values)) &&
        !grepl("character|alphanumeric|string", valid_response,
               ignore.case = TRUE)) {
      return(as.integer(out))
    }
    return(out)
  }

  if (table_number == 26L && lower == "appointment_type") {
    return(pick(c(
      "Controller appointed (except receiver or managing controller)",
      "Court liquidation", "Creditors' voluntary liquidation",
      "Deed of company arrangement", "Managing controller",
      "Provisional liquidation", "Receiver", "Receiver and manager",
      "Restructuring", "Scheme administrator appointed",
      "Simplified liquidation", "Voluntary administration"
    )))
  }

  patent_classes <- domain("iplord_technology")
  if (table_number == 28L && grepl("^p_.*_modal_class$|^p_(filed|granted|retired|le)_modal_class$",
                                   lower, perl = TRUE)) {
    return(pick(patent_classes, missing_rate = 0.05))
  }
  if (table_number == 28L && grepl("^tm_.*_modal_class$|^tm_(filed|granted|retired|le)_modal_class$",
                                   lower, perl = TRUE)) {
    return(pick(sprintf("%02d", 1:45), missing_rate = 0.05))
  }
  if (table_number == 28L && grepl("^d_.*_modal_class$|^d_(filed|granted|retired|le)_modal_class$",
                                   lower, perl = TRUE)) {
    return(pick(sprintf("%02d", c(1:32, 99)), missing_rate = 0.05))
  }
  if (table_number == 28L && grepl("^pbr_.*_modal_class$|^pbr_(filed|granted|retired|le)_modal_class$",
                                   lower, perl = TRUE)) {
    return(pick(c("Brassica", "Gossypium", "Hordeum", "Lolium",
                  "Rosa", "Solanum", "Triticum", "Vitis"),
                missing_rate = 0.08))
  }

  ip_type <- if (table_number %in% 29L:34L) {
    .blade_ip_right_type(business_rows, seed, table_number)
  } else {
    NULL
  }
  if (!is.null(ip_type) && lower == "ip_right_type") return(ip_type)
  if (table_number == 29L && lower == "auexamsection") {
    return(pick(c(paste0("CHEM", 1:5), paste0("ELEC", 1:4),
                  paste0("MECH", 1:5))))
  }
  if (lower %in% c(
    "country_code", "f_country_of_earliest_filing",
    "f_earliest_country_of_grant", "classifying_country_code",
    "linked_application_country"
  ) && table_number %in% c(28L:34L)) {
    return(pick(domain("iso_country_alpha2")))
  }
  if (table_number == 29L && lower == "ip_right_sub_type") {
    return(pick(domain("ip_right_sub_type"), missing_rate = 0.03))
  }
  if (table_number == 29L && lower == "status") {
    return(pick(domain("ip_status")))
  }
  if (table_number == 31L && lower == "classification") {
    patent <- c("A01B", "A61K", "C07D", "G06F", "H04L")
    trade_mark <- sprintf("%02d", 1:45)
    design <- sprintf("%02d-%02d", rep(1:32, each = 2L), 1:2)
    return(vapply(seq_along(ip_type), function(i) {
      values <- switch(ip_type[[i]], patent = patent,
                       trade_mark = trade_mark, design = design)
      values[[.blade_draw(business_rows[i, , drop = FALSE], seed,
                           salt, length(values)) + 1L]]
    }, character(1)))
  }
  if (table_number == 31L && lower == "classification_area") {
    return(ifelse(ip_type == "patent", pick(patent_classes),
                  ifelse(ip_type == "trade_mark", pick(sprintf("%02d", 1:45)),
                         pick(sprintf("%02d", c(1:32, 99))))))
  }
  if (table_number == 31L && lower == "classification_importance") {
    return(pick(c("primary", "secondary")))
  }
  if (table_number == 31L && lower == "classification_inventiveness") {
    return(pick(c("inventive", "non-inventive", "additional", "unknown")))
  }
  if (table_number == 31L && lower == "classification_source") {
    return(pick(c("human", "machine", "unknown")))
  }
  if (table_number == 31L && lower == "classification_system") {
    draw <- .blade_draw(business_rows, seed, salt, 2L)
    return(ifelse(ip_type == "patent", ifelse(draw == 0L, "cpc_mark", "ipc_mark"),
                  ifelse(ip_type == "trade_mark", "nice", "locarno")))
  }
  if (table_number == 31L && lower == "coarse_classification_area") {
    return(ifelse(
      ip_type == "patent",
      pick(c("chemistry", "electrical_engineering", "instruments",
             "mechanical_engineering", "other_fields")),
      ifelse(ip_type == "trade_mark",
             pick(c("goods", "services", "goods_and_services")), "other_fields")
    ))
  }
  if (table_number == 32L && lower == "event_category") {
    return(pick(domain("ip_event_category")))
  }
  if (table_number == 32L && lower == "event_type") {
    return(pick(domain("ip_event_type")))
  }
  if (table_number == 33L && lower == "link_type") {
    return(pick(domain("ip_link_type")))
  }

  if (lower %in% c("country_of_final_dest_ex", "country_of_origin_im")) {
    return(pick(domain("trade_country_code")))
  }
  if (lower %in% c("port_of_discharge_ex", "port_of_loading_im")) {
    return(pick(domain("trade_foreign_port")))
  }
  if (lower == "port_of_loading_ex") {
    return(pick(domain("trade_australian_port")))
  }
  if (lower %in% c("invoice_currency_ex", "invoice_currency_im")) {
    return(pick(domain("trade_currency")))
  }
  if (lower %in% c("unit_of_quantity_ex", "unit_of_quantity_im")) {
    return(pick(domain("trade_unit")))
  }
  if (table_number == 48L && lower == "form_id_im") {
    return(pick(c("Nature 10", "Nature 20", "Nature 30")))
  }

  if (table_number == 35L && lower == "entity_type") {
    return(pick(c("C", "I", "S", "T")))
  }
  if (table_number == 52L && lower %in% c("australian_hq", "diversified_corporate")) {
    return(pick("YES", missing_rate = 0.45))
  }
  if (table_number == 52L && lower == "theme") {
    return(rep("Agricultural and food biosensors", nrow(business_rows)))
  }
  if (table_number == 53L && lower == "rdentityasicregistrationtype") {
    return(as.integer(pick(as.character(1:3))))
  }
  if (table_number == 53L && lower %in% c(
    "rdti_incorporationcountry", "rdti_residencecountryname",
    "rdti_uhcincorporationcountry"
  )) {
    return(pick(c("AUSTRALIA", "FOREIGN")))
  }
  if (table_number == 53L && lower %in% c(
    "rdti_companyisheadofconsolidated", "companyiscontrolledbytaxexempt"
  )) {
    return(pick(c("Yes", "No", "N/A")))
  }
  if (table_number == 53L && lower %in% c("indigenousowned", "indigenouscontrolled")) {
    return(pick(c("Yes", "No", "N/A", "Prefer not to answer")))
  }
  if (table_number == 53L && lower == "activitiesexcludedfrombeingcore") {
    return(pick(c("1", "0", "N/A")))
  }
  if (table_number == 53L && lower == "rdti_advancedoroverseasfinding") {
    return(pick(c("1", "0", "UNKNOWN")))
  }
  if (table_number == 53L && lower == "rdti_anzsrccode") {
    return(pick(c("0101", "0301", "0502", "0806", "1005", "1103",
                  "1502", "1701", "2002", "2101", "3004", "3202")))
  }
  if (table_number == 59L && lower == "client_focus") {
    return(pick(c("business", "consumer", "both")))
  }
  if (table_number == 61L && lower == "segment") {
    return(pick(c("Born Global", "Expanding Exporter", "Global Leader",
                  "Novice Exporter", "Stable Exporter")))
  }
  if (table_number == 62L && lower == "mktdescription") {
    return(pick(c(
      "Active", "Export sales", "Foreign direct investment with export outcomes",
      "Growth in annual sales", "Interested",
      "International agreements, tender or project bid",
      "Outwards Investment Project", "Winning a Contract or Tender",
      "csbusinessmatch", "csexportguidance", "csintroductions",
      "csmarketexperience", "csmarketresearch", "csmarketselect",
      "cspracticalassist", "csprofilesupport", "cstroubleshoot",
      "expenditure", "ftip", "ggtwebinar", "landingpad"
    )))
  }
  if (table_number == 62L && lower == "mktevent") {
    return(pick(c("Client Services", "EMDG", "Excelerate",
                  "Go Global Toolkit", "Tailored Services", "Trade Outcome")))
  }
  if (table_number == 62L && lower == "mktselected") {
    return(pick(domain("max_market")))
  }
  NULL
}

.blade_character_response_values <- function(valid_response) {
  if (is.null(valid_response) || length(valid_response) == 0L ||
      is.na(valid_response) || !nzchar(valid_response)) {
    return(character())
  }
  has_yes <- grepl("\\bY\\s*[-=]\\s*Yes\\b", valid_response,
                   ignore.case = TRUE, perl = TRUE)
  has_no <- grepl("\\bN\\s*[-=]\\s*No\\b", valid_response,
                  ignore.case = TRUE, perl = TRUE)
  if (has_yes && has_no) return(c("Y", "N"))
  if (has_yes) return(c("Y", NA_character_))
  if (has_no) return(c("N", NA_character_))
  character()
}

.blade_australian_port_code <- function(state, seed, salt = 0L) {
  # Operative Australian port codes from Appendix 7 of the April 2026
  # BLADE data item list. The pools retain several high-volume ports in each
  # state or territory rather than inventing a generic categorical code.
  pools <- list(
    c("101", "102", "103", "104", "106", "107", "112", "118"),
    c("201", "202", "203", "204", "212", "298"),
    c("301", "303", "304", "305", "306", "307", "309", "311"),
    c("401", "403", "408", "409", "411", "413", "417", "418"),
    c("501", "502", "504", "505", "508", "510", "512", "513"),
    c("601", "602", "603", "605", "610", "611", "698"),
    c("701", "703", "704", "780", "798"),
    c("801", "898")
  )
  state <- .normalise_blade_state(state)
  vapply(seq_along(state), function(i) {
    pool <- pools[[state[[i]]]]
    pool[((i + seed + salt) %% length(pool)) + 1L]
  }, character(1))
}

.blade_count_value <- function(name, business_rows, seed, salt) {
  n <- nrow(business_rows)
  lower <- tolower(name)
  base <- as.integer(pmax(0L, business_rows$d_total_payees))
  draw <- .blade_draw(business_rows, seed, salt, 1000L)
  if (grepl("loc|site|premis", lower)) {
    return(as.integer(pmax(1L, 1L + draw %% 6L)))
  }
  if (grepl("manager|director|proprietor|partner", lower)) {
    return(as.integer(pmin(pmax(1L, base - draw %% 3L), 5L)))
  }
  as.integer(pmax(0L, base + draw %% 4L - 1L))
}

.blade_period_value <- function(name, business_rows, table_number, seed) {
  lower <- tolower(name)
  n <- nrow(business_rows)
  if (grepl("jandec", lower)) return(as.integer((seq_len(n) + seed) %% 5L == 0L))
  if (grepl("juljun", lower)) return(as.integer((seq_len(n) + seed) %% 5L != 0L))
  if (grepl("periodoth|other_period", lower)) {
    return(as.integer((seq_len(n) + seed) %% 17L == 0L))
  }
  if (grepl("periodbeg|period_start|start_period", lower)) {
    return(.blade_reference_date(table_number) - 364L)
  }
  if (grepl("periodend|period_end|end_period", lower)) {
    return(.blade_reference_date(table_number))
  }
  rep(.blade_latest_period(table_number), n)
}

.blade_metadata_value_for <- function(name, business_rows, table_number,
                                      seed, item = "",
                                      valid_response = "",
                                      location_rows = NULL) {
  lower <- tolower(name)
  if (is.null(item) || length(item) == 0L || is.na(item)) item <- ""
  if (is.null(valid_response) || length(valid_response) == 0L ||
      is.na(valid_response)) {
    valid_response <- ""
  }
  item_lower <- tolower(item)
  valid_lower <- tolower(valid_response)
  context <- paste(lower, item_lower, valid_lower)
  n <- nrow(business_rows)
  salt <- .blade_name_salt(name)

  # These fields have published categorical domains or are genuine free-text
  # survey fields. Handle them before the general metadata classifier so they
  # cannot fall through to synthetic Cnn/Cnnn codes.
  if (lower == "periodc") {
    draw <- (seq_len(n) + seed) %% 85L
    return(ifelse(draw == 0L, "Other",
                  ifelse(draw %% 5L == 0L, "Jan-Dec", "Jul-Jun")))
  }
  if (lower == "digtecoth_s") {
    return(.blade_cycle_values(
      c("Cloud computing", "Cyber security", "Data analytics",
        "Online collaboration"),
      n, seed, salt, business_rows
    ))
  }
  if (lower == "sklegal_im") {
    return(.blade_pick_codes(c(0L, 1L, 88888888L, 999999999L),
                             n, seed, salt,
                             business_rows = business_rows))
  }
  if (lower == "nature_of_tariff_code_im") {
    draw <- .blade_draw(business_rows, seed, salt, 100L)
    return(ifelse(draw < 78L, "N",
                  ifelse(draw < 90L, "R",
                         ifelse(draw < 95L, "G",
                                ifelse(draw < 99L, "Q", "C")))))
  }
  if (lower == "port_of_discharge_im") {
    return(.blade_australian_port_code(business_rows$state, seed, salt))
  }
  if (lower == "typeofvariation") {
    return(.blade_cycle_values(
      c("wages", "conditions", "wages/conditions"), n, seed, salt,
      business_rows
    ))
  }
  if (lower == "type" && as.integer(table_number) == 52L) {
    return(rep("ADOPTER", n))
  }
  if (lower == "companyhasultimateholdingcompany") {
    draw <- .blade_draw(business_rows, seed, salt, 100L)
    return(ifelse(draw < 35L, "Yes", ifelse(draw < 92L, "No", "N/A")))
  }
  if (lower == "applicationforheadorsubsidiary") {
    return(.blade_cycle_values(
      c("The head company", "Subsidiary members",
        "The head company and Subsidiary members"),
      n, seed, salt, business_rows
    ))
  }
  if (lower == "impute_capex") {
    return(.blade_cycle_values(
      c("Reported", "Impute partial", "Impute full", "Winsorised"),
      n, seed, salt, business_rows
    ))
  }
  if (lower == "d_ag_anzsic06_group") {
    return(suppressWarnings(as.integer(substr(business_rows$anzsic06,
                                               1L, 3L))))
  }
  if (lower == "cast_anzsic93") return(rep(9999L, n))
  if (lower %in% c("cast_sisca06", "cast_sisca93")) {
    return(rep(9999L, n))
  }
  if (lower == "cii_group") {
    return(.blade_cycle_values(
      c("CII-Likely-MoM", "CII-Unlikely-MoM", "Non-CII"),
      n, seed, salt, business_rows
    ))
  }
  if (lower == "sector_group") {
    return(.blade_cycle_values(
      c("Consumer Goods and Services", NA_character_),
      n, seed, salt, business_rows
    ))
  }
  admin_character <- .blade_admin_character_value(
    name, business_rows, table_number, seed,
    valid_response = valid_response
  )
  if (!is.null(admin_character)) return(admin_character)
  if (lower == "previous_id_capex") {
    return(.blade_related_id("", business_rows, seed, salt, width = 10L))
  }
  if (grepl("(^|_)date($|_)|_dt$|(^|_)(start|end|birth)($|_)|commenc",
            lower, perl = TRUE)) {
    return(.blade_reference_date(table_number) -
             ((as.numeric(seq_len(n)) + as.numeric(seed) +
                 as.numeric(salt)) %% 365L))
  }
  if (lower == "year" || lower %in% c("financial_year", "z_year") ||
      grepl("(_fy|_yr_cd|_year)$", lower, perl = TRUE)) {
    year <- .blade_end_year(table_number)
    if (grepl("first|application|gained|round|launch|seed|valuation|sim_",
              lower)) {
      year <- pmax(1900L,
                   year - ((as.numeric(seq_len(n)) + as.numeric(seed) +
                              as.numeric(salt)) %% 20L))
    }
    return(as.integer(year))
  }
  if (grepl("quarter|periodbeg|periodend|periodjuljun|periodjandec",
            lower)) {
    return(.blade_period_value(name, business_rows, table_number, seed))
  }

  state <- .normalise_blade_state(business_rows$state)
  if (lower %in% c("x_state", "cast_state", "d_ag_state", "z_state")) {
    return(as.integer(state))
  }
  if (lower == "state_code") {
    return(c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")[state])
  }
  if (grepl("^state_of_", lower)) {
    return(c(
      "NEW SOUTH WALES", "VICTORIA", "QUEENSLAND", "SOUTH AUSTRALIA",
      "WESTERN AUSTRALIA", "TASMANIA", "NORTHERN TERRITORY",
      "AUSTRALIAN CAPITAL TERRITORY"
    )[state])
  }

  codes <- .blade_valid_response_codes(valid_response)
  sentinel_measure <- grepl("^numeric", valid_lower) && length(codes) > 0L &&
    all(codes %in% c(9999L, 111111111L, 222222222L, 7777777L,
                     88888888L, 999999999L))
  example_only <- length(codes) == 1L &&
    grepl("example|e\\.g\\.", valid_lower)
  documented_single_code <- length(codes) == 1L &&
    grepl("everything else.*invalid", valid_lower)
  if (length(codes) >= 2L ||
      (documented_single_code && !sentinel_measure && !example_only)) {
    return(.blade_pick_codes(
      codes, n, seed, salt,
      include_missing = grepl("missing|sequencing", valid_lower),
      business_rows = business_rows
    ))
  }

  if (grepl("date format|dd/mm/yyyy", valid_lower)) {
    return(.blade_reference_date(table_number) -
             ((as.numeric(seq_len(n)) + as.numeric(seed) +
                 as.numeric(salt)) %% 365L))
  }
  if (grepl("month", lower) || grepl("\\bmmm\\b", valid_lower)) {
    index <- (as.numeric(seq_len(n)) + as.numeric(seed) +
                as.numeric(salt)) %% 12L
    return(month.abb[index + 1L])
  }
  if (grepl("anzsic|business industry codes", context)) {
    return(business_rows$anzsic06)
  }
  if (grepl("anzsco", context)) {
    width <- if (grepl("1 - digit|1 digit", context)) 1L else
      if (grepl("2 - digit|2 digit", context)) 2L else
        if (grepl("3 - digit|3 digit", context)) 3L else
          if (grepl("4 - digit|4 digit", context)) 4L else 6L
    return(.blade_anzsco_group(business_rows$representative_anzsco_code,
                               width))
  }
  if (grepl("asgs|sa1|sa2|mesh block", context)) {
    if (is.null(location_rows)) {
      location_rows <- .blade_location_lookup_rows(business_rows, seed)
    }
    if (grepl("geocode", lower)) {
      return(as.integer(((as.numeric(seq_len(n)) + as.numeric(seed) +
                            as.numeric(salt)) %% 3L) + 1L))
    }
    if (grepl("sa2.*name|full name of sa2", context)) {
      return(paste("SA2", location_rows$sa2_code))
    }
    if (grepl("sa1", lower) || grepl("statistical area 1", context)) {
      return(location_rows$sa1_code)
    }
    if (grepl("sa2", lower) || grepl("statistical area 2|maincode of sa2", context)) {
      return(location_rows$sa2_code)
    }
    return(location_rows$mb_code)
  }
  if (grepl("postcode", context)) return(business_rows$x_pcode)

  if (grepl("numeric|number|\\$|%'?000|\\bha\\b|\\(t\\)|tonne|kilogram|kg|million|percentage|%",
            valid_lower)) {
    count_like <- grepl(
      "person|employee|job|manager|director|vacanc|location|number|count|cnt|headcount|fte|sequence|duration|days|filter|(^|_)n($|_)",
      lower, perl = TRUE
    ) || grepl("total number|number of|sequence number|count of",
               item_lower, perl = TRUE)
    amount_like <- grepl(
      "income|expenditure|amount|amt|sales|cost|wage|salary|capital|value|val|revenue|tax|fee",
      lower
    ) || grepl("\\$|million|%'?000|monetary", valid_lower, perl = TRUE)
    if (count_like && !amount_like) {
      return(.blade_count_value(name, business_rows, seed, salt))
    }
    scale <- if (grepl("million", valid_lower)) 1 / 1000000 else
      if (grepl("000", valid_lower)) 1 / 1000 else 1
    if (grepl("percentage|%", valid_lower)) {
      return(round(.blade_draw(business_rows, seed, salt, 1000L) / 10, 1))
    }
    if (grepl("\\bha\\b|hectare", valid_lower)) {
      return(round(pmax(1, business_rows$turnover / 100000) *
                     (0.5 + .blade_draw(business_rows, seed, salt, 200L) /
                        100), 2))
    }
    if (grepl("\\(t\\)|tonne|kilogram|kg", valid_lower)) {
      return(round(pmax(1, business_rows$turnover / 50000) *
                     (1 + .blade_draw(business_rows, seed, salt, 300L) /
                        100), 2))
    }
    return(.blade_amount(business_rows, seed, salt, scale = scale))
  }

  if (grepl("alphanumeric|character|categorical|text", valid_lower)) {
    if (grepl("10 digit.*abn|deidentified abn", context)) return(business_rows$bn)
    if (grepl("unit_id|deidentified unit", context)) return(business_rows$id)
    if (grepl("acn|can", context)) return(business_rows$cn)
    if (grepl("sbid", context)) return(business_rows$fn)
    if (!.blade_is_survey_table(table_number)) {
      return(rep(NA_character_, n))
    }
    width <- suppressWarnings(as.integer(sub(".*?([0-9]{1,2}) digit.*", "\\1",
                                             valid_lower)))
    if (is.na(width) || width < 1L || width > 12L) width <- 3L
    return(.blade_character_code(n, seed, salt, width = width))
  }

  NULL
}

.blade_bas_value_for <- function(name, business_rows, table_number, seed) {
  lower <- tolower(name)
  n <- nrow(business_rows)
  turnover <- round(as.numeric(business_rows$turnover), 2)
  # The flat dollar addition below is an anchor-year amount. The wages it sits on
  # top of arrive already repriced, so left alone it would shrink in real terms
  # across the tables; it moves on the headline wage series instead. It gets no
  # per-business deviation, because the multiplicative term already carries the
  # business's own and a second draw on a nuisance term would only widen it.
  bas_period <- .blade_nominal_period_for_table(table_number)
  wage_level <- nominal_index("wage", bas_period$year, bas_period$basis)
  if (length(wage_level) != 1L || is.na(wage_level)) wage_level <- 1
  wages <- round(
    ifelse(
      business_rows$annual_wages > 0,
      business_rows$annual_wages *
        (1.03 + ((seq_len(n) + seed) %% 11L) / 100) +
        (500 + ((seq_len(n) * 97L + seed) %% 2500L)) * wage_level,
      0
    ),
    2
  )
  oexp <- round(
    turnover * (0.42 + ((seq_len(n) * 13L + seed) %% 35L) / 100),
    2
  )
  capex <- round(as.numeric(business_rows$capital_expenditure), 2)
  exports <- round(
    pmin(turnover * (0.01 + ((seq_len(n) * 7L + seed) %% 18L) / 100),
         turnover * 0.75),
    2
  )
  if (lower == "turnover") return(turnover)
  if (lower == "exports_amt") return(exports)
  if (lower == "other_gst_free_sales") {
    return(round(pmax(exports, turnover * 0.02), 2))
  }
  if (lower == "capex") return(capex)
  if (lower == "oexp") return(oexp)
  if (lower == "tot_expenses") return(round(oexp + capex, 2))
  if (lower == "wages") return(wages)
  if (lower == "p_wrk_amt") return(round(business_rows$annual_wages * 0.22, 2))
  if (lower == "a_it_w_amt") return(round(turnover * 0.005, 2))
  if (lower == "t_it_w_amt") return(round(turnover * 0.003, 2))
  if (lower == "payg_tax_withheld") {
    return(round(business_rows$annual_wages * 0.22, 2))
  }
  if (lower == "gst_payable") {
    return(round(pmax(turnover * 0.1 - oexp * 0.1, 0), 2))
  }
  if (lower == "credit_for_gst_paid") return(round(oexp * 0.1, 2))
  if (lower == "d_imprt_amt") {
    return(round(pmin(turnover * 0.15, business_rows$import_value), 2))
  }
  if (lower == "month_actioned") {
    draw <- .blade_draw(
      business_rows, seed, .stable_name_seed(name), length(month.abb)
    )
    return(month.abb[draw + 1L])
  }
  NULL
}

.blade_role_code <- function(n, seed, salt = 0L) {
  if (n == 0L) return(character(0))
  draw <- (as.numeric(seq_len(n)) * 37 + as.numeric(seed) +
             as.numeric(salt)) %% 100L
  ifelse(draw < 10L, "n",
         ifelse(draw < 18L, "0",
                ifelse(draw < 92L, "1", "L")))
}

.blade_frame_value_for <- function(name, business_rows, table_number, seed) {
  lower <- tolower(name)
  n <- nrow(business_rows)

  if (lower %in% c("x_itip", "x_itw", "x_gstp")) {
    return(.blade_role_code(n, seed, .stable_name_seed(name)))
  }
  if (lower == "x_gst_bn") {
    out <- rep("", n)
    grouped <- business_rows$is_profiled == 1L &
      business_rows$bg_id != "" &
      ((.blade_id_number(business_rows$bg_id) + seed) %% 3L) == 0L
    if (any(grouped)) {
      group_ids <- .blade_id_number(business_rows$bg_id[grouped])
      out[grouped] <- sprintf("GST%07d",
                              (group_ids + seed +
                                 .stable_name_seed(name)) %% 10000000L)
    }
    return(out)
  }
  NULL
}

.blade_form_prefix <- function(legal_form) {
  out <- rep("c", length(legal_form))
  out[legal_form == "Sole trader"] <- "i"
  out[legal_form == "Partnership"] <- "p"
  out[legal_form == "Trust"] <- "t"
  out
}

.blade_financial_name <- function(lower) {
  grepl(
    paste(
      c("sales", "turnover", "income", "incm", "inclo", "inc",
        "revenue", "gross", "gros", "grss", "amount", "amt",
        "expense", "expn", "exps", "cost", "ddct", "ded", "depr",
        "rent", "super", "wage", "salary", "salwg", "labr", "asset",
        "asst", "liab", "debt", "stock", "credit", "debtor", "tax",
        "profit", "loss", "gain", "cgt", "frank", "loan", "pay",
        "fees", "val", "tofa", "tnovr"),
      collapse = "|"
    ),
    lower
  )
}

.blade_code_name <- function(lower) {
  grepl("(_cd$|code|type|typ|status|sts|ind$|flag|rt$|rng|marin|cntry|sgmt)",
        lower)
}

.blade_bit_value_for <- function(name, business_rows, table_number, seed,
                                 valid_response = "") {
  lower <- tolower(name)
  n <- nrow(business_rows)
  active_prefix <- .blade_form_prefix(business_rows$legal_form)
  prefix <- if (grepl("^[cipt]_", lower)) substr(lower, 1L, 1L) else NA_character_
  applicable <- if (is.na(prefix)) rep(TRUE, n) else active_prefix == prefix
  income <- round(as.numeric(business_rows$turnover) *
                    (0.92 + ((seq_len(n) * 5L + seed) %% 13L) / 100), 2)
  expenses <- round(pmin(
    income * 0.96,
    as.numeric(business_rows$annual_wages) * 1.12 +
      as.numeric(business_rows$turnover) *
      (0.35 + ((seq_len(n) * 7L + seed) %% 25L) / 100)
  ), 2)
  profit <- round(pmax(income - expenses, 0), 2)
  wages <- round(as.numeric(business_rows$annual_wages), 2)

  bit_flags <- c(
    bit_comp_yyyy = "c", bit_ind_yyyy = "i",
    bit_part_yyyy = "p", bit_trust_yyyy = "t"
  )
  if (lower %in% names(bit_flags)) {
    return(ifelse(active_prefix == unname(bit_flags[[lower]]), 1L,
                  NA_integer_))
  }
  character_values <- .blade_character_response_values(valid_response)
  if (length(character_values)) {
    out <- .blade_cycle_values(character_values, n, seed,
                               .blade_name_salt(name), business_rows)
    out[!applicable] <- NA_character_
    return(out)
  }
  codes <- .blade_valid_response_codes(valid_response)
  if (length(codes)) {
    out <- .blade_pick_codes(codes, n, seed, .blade_name_salt(name),
                             include_missing = TRUE,
                             business_rows = business_rows)
    out[!applicable] <- NA_integer_
    return(out)
  }
  salt <- .blade_name_salt(name)
  source_values <- .blade_domain_values(variable_name = name)
  if (length(source_values)) {
    out <- .blade_pick_values(source_values, business_rows, seed, salt)
    if (all(grepl("^[0-9]+$", source_values)) &&
        !grepl("character|alphanumeric|string", valid_response,
               ignore.case = TRUE)) {
      out <- as.integer(out)
    }
    out[!applicable] <- NA
    return(out)
  }
  if (grepl("busmarin$", lower)) {
    out <- .blade_pick_values(
      c("GOV", "LGE", "SME", "INB", "MIC", "NFP"),
      business_rows, seed, salt
    )
    out[!applicable] <- NA_character_
    return(out)
  }
  if (lower == "c_coy_cntry_dcd") {
    out <- .blade_pick_values(
      c("Australia", "New Zealand", "United Kingdom", "United States",
        "Singapore", "India", "China", "Japan"),
      business_rows, seed, salt
    )
    out[!applicable] <- NA_character_
    return(out)
  }
  if (lower == "c_fcrncyrt") {
    out <- rep(NA_real_, n)
    draw <- .blade_draw(business_rows, seed, salt, 25001L)
    out[applicable] <- round(0.5 + draw[applicable] / 10000, 5)
    return(out)
  }
  if (grepl("digit character", valid_response, ignore.case = TRUE) &&
      grepl("_num$", lower)) {
    width <- suppressWarnings(as.integer(sub(
      ".*?([0-9]+) digit.*", "\\1", tolower(valid_response)
    )))
    if (is.na(width)) width <- 5L
    values <- .blade_count_value(name, business_rows, seed, salt)
    out <- rep(NA_character_, n)
    out[applicable] <- sprintf(paste0("%0", width, "d"),
                               values[applicable])
    return(out)
  }
  if (grepl("numeric response \\(%\\)", valid_response,
            ignore.case = TRUE, perl = TRUE)) {
    out <- rep(NA_real_, n)
    draw <- .blade_draw(business_rows, seed, salt, 1000L) / 10
    out[applicable] <- round(draw[applicable], 1)
    return(out)
  }
  if (grepl("4 digit character.*yyyy", valid_response,
            ignore.case = TRUE, perl = TRUE)) {
    out <- rep(NA_integer_, n)
    years_ago <- .blade_draw(business_rows, seed, salt, 6L)
    out[applicable] <- .blade_end_year(table_number) - years_ago[applicable]
    return(out)
  }
  if (grepl("anzsic", lower)) {
    out <- rep(NA_integer_, n)
    out[applicable] <- suppressWarnings(
      as.integer(business_rows$anzsic06[applicable])
    )
    return(out)
  }
  if (grepl("poscode|postcode", lower)) {
    out <- rep(NA_integer_, n)
    out[applicable] <- suppressWarnings(
      as.integer(business_rows$x_pcode[applicable])
    )
    return(out)
  }
  value_factor <- 0.25 +
    .blade_draw(business_rows, seed, salt + 1543L, 6501L) / 10000
  if (grepl("totlwage|totlwg|salwg|salary|wage|labr", lower)) {
    out <- rep(NA_real_, n)
    is_primary_total <- grepl("^[cipt]_totl(wage|wg|salwg)$", lower)
    values <- if (is_primary_total) wages else
      round(pmax(wages * value_factor,
                 as.numeric(business_rows$turnover) * 0.005 * value_factor),
            2)
    out[applicable] <- values[applicable]
    return(out)
  }
  if (grepl("sales|turnover|tnovr", lower)) {
    out <- rep(NA_real_, n)
    values <- round(as.numeric(business_rows$turnover) *
                      (0.72 + 0.28 * value_factor), 2)
    out[applicable] <- values[applicable]
    return(out)
  }
  if (grepl("taxinc|taxable|profit|net|loss", lower)) {
    out <- rep(NA_real_, n)
    values <- round(profit * (0.55 + 0.45 * value_factor), 2)
    out[applicable] <- values[applicable]
    return(out)
  }
  if (grepl("totlexps|cost|expense|expn|exps|ddct|ded|depr|rent|super",
            lower)) {
    out <- rep(NA_real_, n)
    is_primary_total <- grepl("^[cipt]_totlexps$", lower)
    values <- if (is_primary_total) expenses else
      round(expenses * value_factor, 2)
    out[applicable] <- values[applicable]
    return(out)
  }
  if (grepl("totlinc|income|incm|inclo|inc|revenue|gross|gros|grss",
            lower)) {
    out <- rep(NA_real_, n)
    is_primary_total <- grepl("^[cipt]_totlincm?$", lower)
    values <- if (is_primary_total) income else
      round(income * value_factor, 2)
    out[applicable] <- values[applicable]
    return(out)
  }
  if (.blade_financial_name(lower)) {
    out <- rep(NA_real_, n)
    out[applicable] <- round(
      pmax(as.numeric(business_rows$turnover[applicable]) *
             (0.02 + value_factor[applicable] * 0.3),
           wages[applicable] * (0.03 + value_factor[applicable] * 0.08)),
      2
    )
    return(out)
  }
  NULL
}

.blade_stp_value_for <- function(name, business_rows, table_number, seed) {
  lower <- tolower(name)
  n <- nrow(business_rows)
  wages <- round(as.numeric(business_rows$annual_wages), 2)
  if (lower == "d_total_payees" ||
      grepl("payee|employee|headcount|hcnt", lower)) {
    return(as.integer(business_rows$d_total_payees))
  }
  if (lower == "ed_sg_emplr_cntrbtn" ||
      grepl("super|spr|sg_emplr|emplr_cntrbtn", lower)) {
    return(round(wages * 0.115, 2))
  }
  if (grepl("tax|withheld|whld", lower)) return(round(wages * 0.22, 2))
  if (grepl("allow|alwnc", lower)) return(round(wages * 0.025, 2))
  # Lump-sum / unused-leave dollar amounts (metadata: "Numeric response ($)").
  # The annual-leave / long-service unused lump-sum components (ed_*_ls_[a-e])
  # are dollar amounts, not 0/1 flags; most employees have none, a minority a
  # payout.
  if (grepl("lump|bonus|termination|etp|_ls_[a-e]$|lng_srvc|unsd", lower)) {
    return(round(ifelse(((seq_len(n) + seed) %% 9L) == 0L,
                        wages * 0.04, 0), 2))
  }
  if (grepl("gross|grs|wage|salary|pay|pmt|amount|amt|total|totl|sumry",
            lower)) {
    return(wages)
  }
  if (grepl("date|start|end", lower)) {
    return(.blade_reference_date(table_number) -
             ((seq_len(n) + seed) %% 365L))
  }
  NULL
}

.blade_bcs_code <- function(n, seed, include_multi = FALSE) {
  draw <- (seq_len(n) * 1103515245 + seed * 97L) %% 100L
  out <- ifelse(draw < 50L, 0L,
                ifelse(draw < 78L, 1L,
                       ifelse(draw < 89L, 88888888L, 999999999L)))
  if (include_multi) out[draw >= 78L & draw < 84L] <- 7777777L
  as.integer(out)
}

.blade_bcs_value_for <- function(name, business_rows, table_number, seed) {
  lower <- tolower(name)
  n <- nrow(business_rows)
  turnover <- round(as.numeric(business_rows$turnover), 2)
  wages <- round(as.numeric(business_rows$annual_wages), 2)
  emp_total <- as.integer(pmax(0L, business_rows$d_total_payees))
  income_sales <- round(turnover * 0.92, 2)
  income_other <- round(turnover * 0.08, 2)
  income_total <- round(income_sales + income_other, 2)
  expenses <- round(turnover *
                      (0.46 + ((seq_len(n) + seed) %% 24L) / 100), 2)

  if (lower == "period_bcs" || lower == "period") {
    return(rep(.blade_latest_period(table_number), n))
  }
  if (lower == "incsalgs_bcs") return(income_sales)
  if (lower == "incothin_bcs") return(income_other)
  if (lower == "inctotal_bcs") return(income_total)
  if (lower == "capextot_bcs") {
    return(round(as.numeric(business_rows$capital_expenditure), 2))
  }
  if (lower == "allexpto_bcs") return(expenses)
  if (lower == "exptotal_bcs") return(round(expenses + wages, 2))
  if (lower == "locs_bcs") {
    return(as.integer(pmax(1L, 1L + ((seq_len(n) + seed) %% 4L))))
  }
  if (lower == "emptotal_bcs") return(emp_total)
  if (lower == "empoth_bcs") return(emp_total)
  if (lower == "empft_bcs") {
    return(as.integer(pmin(emp_total,
                           floor(emp_total *
                                   (0.45 + ((seq_len(n) + seed) %% 30L) /
                                      100)))))
  }
  if (lower == "casuals_bcs") {
    return(as.integer(pmin(emp_total,
                           ceiling(emp_total *
                                     (0.08 + ((seq_len(n) + seed) %% 20L) /
                                        100)))))
  }
  if (lower == "empprop_bcs") {
    return(as.integer(ifelse(business_rows$legal_form %in%
                               c("Sole trader", "Partnership"),
                             pmax(1L, pmin(4L, emp_total + 1L)), 0L)))
  }
  if (lower == "empsaldr_bcs") {
    return(as.integer(ifelse(business_rows$legal_form == "Company" &
                               emp_total > 0L,
                             pmin(3L, 1L + ((seq_len(n) + seed) %% 3L)),
                             0L)))
  }
  if (lower %in% c("perscomm_bcs", "persceas_bcs")) {
    return(as.integer(pmin(emp_total,
                           ((seq_len(n) + seed +
                               .stable_name_seed(name)) %% 6L))))
  }
  if (lower %in% c("busownyr_bcs", "busopyr_bcs")) {
    age <- .blade_end_year(table_number) - business_rows$business_birth_year
    return(as.integer(pmax(0L, pmin(40L, age))))
  }
  if (lower == "d_gsnewy") {
    return(.blade_bcs_code(n, seed, include_multi = TRUE))
  }
  if (grepl("date|start|end", lower)) {
    return(.blade_reference_date(table_number) -
             ((seq_len(n) + seed) %% 365L))
  }
  if (grepl("inc|income|sales|capex|expto|exp|cost|wage|salary|pay|amt|amount|turnover|assets|liab|debt|fund|loan|fee|val",
            lower)) {
    return(round(
      pmax(turnover * (0.02 + ((seq_len(n) * 3L + seed) %% 35L) / 100),
           wages * 0.03),
      2
    ))
  }
  .blade_bcs_code(n, seed, include_multi = grepl("^d_|new|innov|gs", lower))
}

.blade_birthdate_value_for <- function(name, business_rows, table_number, seed) {
  lower <- tolower(name)
  if (lower == "birth_date") {
    return(.blade_financial_year_label(business_rows$business_birth_year))
  }
  NULL
}

.blade_special_value_for <- function(name, business_rows, table_number,
                                     product_name, seed,
                                     valid_response = "") {
  if (as.integer(table_number) == 1L) {
    return(.blade_frame_value_for(name, business_rows, table_number, seed))
  }
  if (as.integer(table_number) == 4L) {
    return(.blade_bas_value_for(name, business_rows, table_number, seed))
  }
  if (as.integer(table_number) == 6L) {
    return(.blade_bit_value_for(name, business_rows, table_number, seed,
                                valid_response = valid_response))
  }
  if (as.integer(table_number) == 7L) {
    return(.blade_stp_value_for(name, business_rows, table_number, seed))
  }
  if (as.integer(table_number) == 8L) {
    return(.blade_bcs_value_for(name, business_rows, table_number, seed))
  }
  if (as.integer(table_number) == 27L) {
    return(.blade_birthdate_value_for(name, business_rows, table_number, seed))
  }
  NULL
}

.blade_value_for <- function(name, business_rows, table_number,
                             product_name, seed, item = "",
                             valid_response = "",
                             location_rows = NULL) {
  lower <- tolower(name)
  upper <- toupper(name)
  n <- nrow(business_rows)
  name_map <- match(lower, tolower(names(business_rows)))
  if (!is.na(name_map)) return(business_rows[[name_map]])

  if (lower == "bn") {
    return(business_rows$bn)
  }
  if (lower %in% c("id", "unit_id") || grepl("unitid|unit_id", lower)) {
    return(business_rows$id)
  }
  if (grepl("version", lower)) {
    code <- (as.integer(table_number) * 7L + seed) %% 300L
    return(sprintf("in%03d_v1", code))
  }
  if (lower == "tsid" || grepl("time.*series", lower)) {
    return(rep(.blade_tsid(table_number), n))
  }
  if (grepl("quarter", lower)) {
    return(rep(paste0(.blade_financial_year_code(table_number), "Q4"), n))
  }
  special <- .blade_special_value_for(name, business_rows, table_number,
                                      product_name, seed,
                                      valid_response = valid_response)
  if (!is.null(special)) return(special)
  character_values <- .blade_character_response_values(valid_response)
  if (length(character_values)) {
    return(.blade_cycle_values(character_values, n, seed,
                               .stable_name_seed(name), business_rows))
  }
  if (grepl("(^|_)bn($|_)|(^|_)abn($|_)", lower)) {
    return(.blade_related_id("BN", business_rows, seed,
                             .stable_name_seed(name), 11L))
  }
  if (grepl("(^|_)cn($|_)|(^|_)acn($|_)", lower)) {
    return(business_rows$cn)
  }
  if (grepl("(^|_)fn($|_)|(^|_)sbid($|_)|(^|_)employer(_id|$)|(^|_)emplr(_id|$)",
            lower)) {
    return(business_rows$fn)
  }
  metadata_value <- .blade_metadata_value_for(
    name = name,
    business_rows = business_rows,
    table_number = table_number,
    seed = seed,
    item = item,
    valid_response = valid_response,
    location_rows = location_rows
  )
  if (!is.null(metadata_value)) return(metadata_value)
  if (grepl("month", lower)) {
    months <- month.abb[((seq_len(n) + seed) %% 12L) + 1L]
    return(months)
  }
  if (grepl("financial.*year|income.*year|^year$|_yr$|yr_", lower)) {
    return(rep(.blade_end_year(table_number), n))
  }
  if (grepl("date|_dt$|commenc|start|end|birth", lower)) {
    return(.blade_reference_date(table_number) -
             ((seq_len(n) + seed + as.integer(table_number)) %% 365L))
  }
  if (grepl("state|ste|main_state", lower)) {
    return(as.integer(business_rows$state))
  }
  if (grepl("postcode|post_code", lower)) {
    return(sprintf("%04d", 2000L +
                     ((as.integer(business_rows$state) * 173L +
                         seq_len(n)) %% 7000L)))
  }
  if (grepl("sa2", lower)) {
    if (is.null(location_rows)) {
      location_rows <- .blade_location_lookup_rows(business_rows, seed)
    }
    return(location_rows$sa2_code)
  }
  if (grepl("sa1", lower)) {
    if (is.null(location_rows)) {
      location_rows <- .blade_location_lookup_rows(business_rows, seed)
    }
    return(location_rows$sa1_code)
  }
  if (grepl("mesh|mb_", lower)) {
    if (is.null(location_rows)) {
      location_rows <- .blade_location_lookup_rows(business_rows, seed)
    }
    return(location_rows$mb_code)
  }
  if (grepl("anzsic|industry|indus", lower)) {
    return(business_rows$anzsic06)
  }
  if (grepl("sisca", lower)) {
    return(business_rows$x_sisca08)
  }
  if (grepl("anzsco", lower)) {
    if (grepl("_[1]_", lower)) {
      return(.blade_anzsco_group(business_rows$representative_anzsco_code, 1L))
    }
    if (grepl("_[2]_", lower)) {
      return(.blade_anzsco_group(business_rows$representative_anzsco_code, 2L))
    }
    if (grepl("_[3]_", lower)) {
      return(.blade_anzsco_group(business_rows$representative_anzsco_code, 3L))
    }
    if (grepl("_[4]_", lower)) {
      return(.blade_anzsco_group(business_rows$representative_anzsco_code, 4L))
    }
    return(business_rows$representative_anzsco_code)
  }
  if (grepl("d_div|division", lower)) {
    return(business_rows$industry_division)
  }
  if (grepl("employee|employment|employ|headcount|hdcnt|fte|jobs|vacanc",
            lower)) {
    return(as.integer(business_rows$employment_count))
  }
  if (grepl("count|cnt|num|number|qty|quantity|volume|weight|tonne|kg|ha|hectare|area|locs|persons|positions|managers",
            lower)) {
    return(.blade_count_value(name, business_rows, seed,
                              .blade_name_salt(name)))
  }
  if (grepl("wage|salary|payg|payroll|pyrl|gross|grs|super", lower)) {
    return(round(business_rows$annual_wages, 2))
  }
  if (grepl("sales|turnover|income|revenue|amount|amt|expense|exp|value|val|cost|tax|gst|bas|bit|assets|liab|capital|fob|import|export|round",
            lower)) {
    return(.blade_amount(business_rows, seed, .blade_name_salt(name)))
  }
  if (grepl("flag|indicator|ind$|binary|dummy|active|alive|status", lower)) {
    return(.blade_draw(
      business_rows, seed,
      .blade_name_salt(name) + as.integer(table_number) * 1009L,
      2L
    ))
  }
  if (grepl("tolo|legal|type|category|class|role", lower)) {
    if (!.blade_is_survey_table(table_number)) return(rep(NA_character_, n))
    return(sprintf("C%02d", ((seq_len(n) + seed) %% 10L) + 1L))
  }
  if (grepl("agreement|project|patent|application|party|round|transaction|identifier|number|key|_id$|^s_",
            lower)) {
    return(.blade_related_id("K", business_rows, seed,
                             .blade_name_salt(name), 9L))
  }
  if (grepl("code|_cd$|currency|country", lower)) {
    if (!.blade_is_survey_table(table_number)) return(rep(NA_character_, n))
    return(sprintf("C%03d", ((seq_len(n) + seed) %% 200L) + 1L))
  }

  numeric_tables <- c(12L, 13L, 14L, 15L, 18L, 23L, 35L, 36L, 37L,
                      44L, 45L, 46L, 47L, 48L, 53L, 57L, 58L, 61L,
                      62L)
  table_number <- as.integer(table_number)
  if (.blade_is_survey_table(table_number)) {
    return(.blade_bcs_code(n, seed + .stable_name_seed(name),
                           include_multi = grepl("^d_|new|innov|gs|change",
                                                 lower)))
  }
  if (table_number %in% numeric_tables) {
    return(.blade_amount(business_rows, seed, .blade_name_salt(name),
                         scale = 0.08))
  }
  if (table_number %in% c(28L, 29L, 30L, 31L, 32L, 33L, 34L, 49L,
                          50L, 51L, 54L, 55L, 56L)) {
    return(.blade_character_code(n, seed, .blade_name_salt(name),
                                 width = 6L, prefix = "K"))
  }
  rep(NA_character_, n)
}

.blade_location_lookup_rows <- function(business_rows, seed) {
  n <- nrow(business_rows)
  if (n == 0L) {
    return(.load_mb_lookup()[0L, , drop = FALSE])
  }

  lookup <- .load_mb_lookup()
  states <- .normalise_blade_state(business_rows$state)
  business_key <- .blade_id_number(business_rows$bn)
  selected <- integer(n)

  for (st in sort(unique(states))) {
    idx <- which(states == st)
    pool <- which(lookup$state == st)
    if (!length(pool)) {
      stop("No Mesh Block lookup rows for state ", st, call. = FALSE)
    }
    pick <- as.integer(
      (business_key[idx] + seq_along(idx) * 2654435761 +
         seed * 1009 + st * 9176) %% length(pool)
    ) + 1L
    selected[idx] <- pool[pick]
  }

  lookup[selected, , drop = FALSE]
}

.blade_business_location_frame <- function(variable_names, business_rows,
                                           table_number, product_name, seed) {
  variable_names <- unique(variable_names[nzchar(variable_names)])
  n <- nrow(business_rows)
  lookup_rows <- .blade_location_lookup_rows(business_rows, seed)

  business_key <- .blade_id_number(business_rows$bn)
  precision_draw <- (business_key + seed * 37 + seq_len(n) * 17L) %% 100L
  geocode_precision <- ifelse(precision_draw < 70L, 1L,
                              ifelse(precision_draw < 95L, 2L, 3L))

  address_draw <- (business_key + seed * 19 + seq_len(n) * 29L) %% 100L
  address_type <- rep("", n)
  address_type[address_draw >= 82L & address_draw < 92L] <- "POSTAL"
  address_type[address_draw >= 92L & address_draw < 98L] <- "ACCOUNTANT"
  address_type[address_draw >= 98L] <- "POSTAL, ACCOUNTANT"

  hashed_arid <- .blade_deidentified_id(
    "A",
    paste(business_rows$bn, lookup_rows$mb_code, sep = "|"),
    seed = seed,
    width = 23L
  )

  value_for <- function(name) {
    lower <- tolower(name)
    if (lower == "bn") return(business_rows$bn)
    if (lower == "hashed_arid") return(hashed_arid)
    if (lower == "locations_version") return(rep("in166_v1", n))
    if (lower == "busloc_version") return(rep("in271_v1", n))
    if (lower == "tsid") return(rep(.blade_tsid(table_number), n))
    if (lower == "quarter") {
      return(rep(paste0(.blade_financial_year_code(table_number), "Q4"), n))
    }
    if (lower == "mesh_block_21") return(lookup_rows$mb_code)
    if (lower == "sa2_code_21") return(lookup_rows$sa2_code)
    if (lower == "geocode_precision") return(as.integer(geocode_precision))
    if (lower == "address_type") return(address_type)
    .blade_value_for(
      name,
      business_rows = business_rows,
      table_number = table_number,
      product_name = product_name,
      seed = seed
    )
  }

  out <- lapply(variable_names, value_for)
  names(out) <- variable_names
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

.blade_cap_date <- function(x, upper) {
  as.Date(pmin(as.numeric(x), as.numeric(upper)), origin = "1970-01-01")
}

.blade_enforce_admin_relationships <- function(frame, business_rows,
                                               table_number, seed) {
  table_number <- as.integer(table_number)
  has <- function(name) name %in% names(frame)
  set_date <- function(name, value) {
    if (has(name)) frame[[name]] <<- as.Date(value, origin = "1970-01-01")
  }
  draw <- function(salt, modulus) {
    .blade_draw(business_rows, seed, salt, modulus)
  }
  reference <- .blade_reference_date(table_number)

  if (table_number == 29L) {
    application <- reference - (730L + draw(2901L, 4380L))
    priority <- application - draw(2902L, 731L)
    earliest_filed <- application - draw(2903L, 366L)
    international_filing <- earliest_filed - draw(2904L, 366L)
    grant <- .blade_cap_date(
      application + 180L + draw(2905L, 1096L), reference - 366L
    )
    registration <- .blade_cap_date(
      grant + 30L + draw(2906L, 151L), reference - 185L
    )
    enforceable <- .blade_cap_date(
      registration + draw(2907L, 61L), reference - 124L
    )
    gained_enforceable <- .blade_cap_date(
      enforceable + draw(2908L, 61L), reference - 63L
    )
    retired <- .blade_cap_date(
      gained_enforceable + 30L + draw(2909L, 366L), reference
    )

    set_date("application_date", application)
    set_date("priority_date", priority)
    set_date("earliest_filed_date", earliest_filed)
    set_date("f_intl_earliest_filing_date", international_filing)
    set_date("f_earliest_grant_date", grant)
    set_date("gained_registration_status_date", registration)
    set_date("enforceable_from_date", enforceable)
    set_date("gained_enforceable_status_date", gained_enforceable)
    set_date("deemed_retired_date", retired)
  }

  if (table_number == 49L) {
    certification <- reference - (365L + draw(4901L, 2922L))
    commencement <- certification + 7L + draw(4902L, 84L)
    expiry <- commencement + 1095L + draw(4903L, 2557L)
    termination <- commencement + 180L + draw(4904L, 731L)
    termination <- .blade_cap_date(termination, expiry)

    set_date("certificationdt", certification)
    set_date("commencementdt", commencement)
    set_date("expirydt", expiry)
    set_date("terminationdt", termination)
    for (k in seq_len(18L)) {
      date_name <- paste0("wageincreasedate", k)
      if (!has(date_name)) next
      increase <- commencement + k * 180L + draw(5000L + k, 61L)
      after_expiry <- increase > expiry
      increase[after_expiry] <- as.Date(NA)
      set_date(date_name, increase)
      amount_name <- paste0("wageincreaseamount", k)
      if (has(amount_name)) frame[[amount_name]][after_expiry] <- NA_real_
    }
  }

  if (table_number == 53L) {
    income_end <- reference - draw(5301L, 92L)
    income_start <- income_end - 364L
    project_start <- reference - (365L + draw(5302L, 1827L))
    project_end <- .blade_cap_date(
      project_start + 180L + draw(5303L, 1096L), reference
    )
    set_date("rdti_incomeperiodstarton", income_start)
    set_date("rdti_incomeperiodstendon", income_end)
    set_date("rdti_projectstartdate", project_start)
    set_date("rdti_projectenddate", project_end)
    if (has("rdti_totalnumberofemployees") &&
        has("numberofemployeesengagedinrd")) {
      total <- as.integer(pmax(0L, frame$rdti_totalnumberofemployees))
      gap <- 1L + draw(5304L, 3L)
      frame$rdti_totalnumberofemployees <- total
      frame$numberofemployeesengagedinrd <- as.integer(
        pmin(frame$numberofemployeesengagedinrd, pmax(0L, total - gap))
      )
    }
  }

  if (table_number == 59L) {
    if (has("round_amount") && has("total_funding")) {
      frame$total_funding <- pmax(frame$total_funding, frame$round_amount)
    }
    if (has("valuation_min") && has("valuation_max")) {
      frame$valuation_max <- pmax(frame$valuation_min, frame$valuation_max)
    }
  }

  frame
}

.make_blade_frame <- function(variable_names, business_rows, table_number,
                              product_name, seed, variables = NULL) {
  variable_names <- unique(variable_names[nzchar(variable_names)])
  location_rows <- NULL
  if (!is.null(variables)) {
    context <- paste(
      variables[["Variable.Name"]],
      variables[["Item"]],
      variables[["Valid.Response"]]
    )
    if (any(grepl("asgs|sa1|sa2|mesh block", context, ignore.case = TRUE))) {
      location_rows <- .blade_location_lookup_rows(business_rows, seed)
    }
  }
  out <- lapply(variable_names, function(name) {
    meta <- NULL
    if (!is.null(variables)) {
      meta <- variables[match(name, variables[["Variable.Name"]]), ,
                        drop = FALSE]
      if (nrow(meta) == 0L || is.na(meta[["Variable.Name"]][1L])) {
        meta <- NULL
      }
    }
    .blade_value_for(
      name,
      business_rows = business_rows,
      table_number = table_number,
      product_name = product_name,
      seed = seed,
      item = if (is.null(meta)) "" else meta[["Item"]][1L],
      valid_response = if (is.null(meta)) "" else meta[["Valid.Response"]][1L],
      location_rows = location_rows
    )
  })
  names(out) <- variable_names
  frame <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  if (!.blade_is_survey_table(table_number)) {
    frame <- .blade_enforce_admin_relationships(
      frame, business_rows, table_number, seed
    )
  }
  frame
}

.select_key_columns <- function(frame, product_name) {
  vars <- .blade_key_variables(product_name = product_name)
  wanted <- unique(vars[["Variable.Name"]])
  missing <- setdiff(wanted, names(frame))
  if (length(missing)) {
    stop("BLADE key frame missing variables: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  frame[, wanted, drop = FALSE]
}

.make_blade_id_bn_key <- function(business_spine) {
  key_tsids <- unique(c(.blade_key_tsid("blade-key-id-to-bn-key"),
                        .blade_tsid(8L)))
  do.call(rbind, lapply(key_tsids, function(tsid) {
    data.frame(
      bn = business_spine$bn,
      id = business_spine$id,
      bg_id = business_spine$bg_id,
      key_version = "in148_v1",
      tsid = tsid,
      # One ABN mapping to many tax/accounting units (TAUs) occurs mainly for
      # larger profiled businesses; many ABNs mapping to one TAU occurs for
      # businesses consolidated under a BLADE Enterprise Group (bg_id). Derived
      # deterministically from the business identifier so they vary rather than
      # being a constant 0.
      one_abn_to_many_tau = as.integer(
        business_spine$is_profiled == 1L &
          (.blade_id_number(business_spine$id) %% 5L) == 0L),
      many_abn_to_one_tau = as.integer(
        nzchar(business_spine$bg_id) &
          (.blade_id_number(business_spine$id) %% 7L) %in% c(1L, 2L)),
      match = ifelse(business_spine$is_profiled == 1L,
                     "One-TAU-BG", "NPP"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
}

.make_blade_cn_bn_key <- function(business_spine) {
  data.frame(
    cn = business_spine$cn,
    bn = business_spine$bn,
    cn_bn_version = "in164_v1",
    tsid = .blade_key_tsid("blade-key-cn-to-bn-key"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.write_blade_keys <- function(business_spine, run_dir, format = "parquet") {
  key_meta <- .blade_key_variables()
  key_products <- unique(key_meta[["Product.Name"]])
  out <- list()

  for (product_name in key_products) {
    frame <- switch(product_name,
      "blade-key-id-to-bn-key" = .make_blade_id_bn_key(business_spine),
      "blade-key-cn-to-bn-key" = .make_blade_cn_bn_key(business_spine),
      stop("Unknown BLADE key product: ", product_name, call. = FALSE)
    )
    frame <- .select_key_columns(frame, product_name)
    path <- write_product(frame, product_name, "BLADE", run_dir,
                          format = format)
    out[[product_name]] <- list(
      n_rows = nrow(frame),
      n_variables = ncol(frame),
      path = path
    )
  }
  out
}

.blade_assignment_occupation_codes <- function(spine, assignments,
                                               business_spine, seed) {
  n <- nrow(assignments)
  if (n == 0L) {
    return(data.frame(
      ANZSCO_CODE = character(0),
      ANZSCO_TITLE = character(0),
      occupation_health_flag = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  occ <- .optional_chr(spine, "anzsco_code", assignments$spine_row, "000000")
  occ[is.na(occ) | occ == "0"] <- "000000"
  occ <- sprintf("%06.0f", suppressWarnings(as.numeric(occ)))
  occ[is.na(occ)] <- "000000"
  title <- .optional_chr(spine, "anzsco_title", assignments$spine_row,
                         "Occupation not stated")
  title[is.na(title) | !nzchar(title)] <- "Occupation not stated"

  business_rows <- match(assignments$bn, business_spine$bn)
  health_business <- business_spine$health_industry_flag[business_rows] == 1L
  draw <- (seq_len(n) * 1103515245 + seed * 2654435761) %% 100L
  force_health <- (health_business & draw < 72L) |
    (!health_business & draw < 4L)

  health_ref <- .blade_health_occupation_reference()
  if (any(force_health)) {
    ref_idx <- as.integer(
      (seq_len(sum(force_health)) + seed) %% nrow(health_ref)
    ) + 1L
    occ[force_health] <- health_ref$ANZSCO_CODE[ref_idx]
    title[force_health] <- health_ref$ANZSCO_TITLE[ref_idx]
  }

  data.frame(
    ANZSCO_CODE = occ,
    ANZSCO_TITLE = title,
    occupation_health_flag = as.integer(force_health),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.try_load_blade_spine <- function(run_dir) {
  tryCatch(
    load_spine_select(run_dir, .blade_spine_cols()),
    error = function(e) {
      message("PLIDA-BLADE person link could not be created from spine: ",
              conditionMessage(e))
      NULL
    }
  )
}

.load_blade_plida_link <- function(run_dir, format = "parquet") {
  path <- .blade_plida_link_path(run_dir, format)
  if (!file.exists(path)) return(NULL)
  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read BLADE link.", call. = FALSE)
    }
    return(as.data.frame(read_parquet_safely(path), stringsAsFactors = FALSE))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Generate BLADE business-level DIL tables
#'
#' Generates DIL-backed synthetic BLADE tables from the business
#' source-of-truth spine. The business spine is created first if it is
#' not already present in the active run directory.
#' Generated tables follow the April 2026 BLADE DIL schemas and derive
#' generic time fields from each table's public available periods. The
#' optional Appendix key products are the public BLADE correspondences;
#' `_system/plida-blade-link.parquet` is a local fplida linkage
#' artifact for person-to-business training examples.
#'
#' @section Dataset and variable information:
#' The [ABS BLADE overview](https://www.abs.gov.au/statistics/data-integration/integrated-data/business-longitudinal-analysis-data-environment-blade)
#' gives information about this dataset. Use `dataset_info("BLADE")` for dataset
#' information. Use `variable_info("BLADE")` for variables, sources, value
#' support, and topic tags.
#'
#' @param business_spine Data.frame or NULL. If NULL, uses the existing
#'   `_system/business-spine.parquet` or creates one from the person
#'   spine.
#' @param spine Optional person spine used when the business spine must
#'   be created.
#' @param seed Integer. Random seed.
#' @param tables `"all"` or a vector of BLADE table numbers, table names,
#'   or generated product names.
#' @param n_businesses Optional business universe size when the business
#'   spine must be created.
#' @param output_dir Character or NULL. Base output directory.
#' @param format Character. Currently parquet only.
#' @param return_data Logical. If TRUE, returns generated data.frames.
#' @param sample_rate Numeric row sampling rate applied to BLADE survey
#'   tables. Administrative/key tables are generated for the full business
#'   spine so every `bn` is present in administrative BLADE records.
#' @param max_rows Integer maximum rows per emitted BLADE survey table.
#'   Administrative/key tables ignore this cap.
#' @param include_keys Logical. If TRUE, writes BLADE Appendix 1 and
#'   Appendix 2 key products for `bn`/`id`/`bg_id` and `cn`/`bn`
#'   correspondences.
#'
#' @return If `return_data = TRUE`, a named list of data.frames keyed by
#'   BLADE product name. Otherwise an invisible metadata list.
#' @export
generate_blade <- function(business_spine = NULL,
                           spine = NULL,
                           seed = 42L,
                           tables = "all",
                           n_businesses = NULL,
                           output_dir = NULL,
                           format = c("parquet", "csv"),
                           return_data = FALSE,
                           sample_rate = 1,
                           max_rows = 10000L,
                           include_keys = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") {
    stop("generate_blade() writes parquet only.", call. = FALSE)
  }
  sample_rate <- as.numeric(sample_rate)
  stopifnot(sample_rate > 0)

  run_dir <- resolve_run_dir(output_dir)
  spine_for_link <- spine
  if (is.null(business_spine)) {
    business_spine <- if (file.exists(.blade_business_spine_path(run_dir))) {
      .load_blade_business_spine(run_dir)
    } else {
      if (is.null(spine_for_link)) {
        spine_for_link <- .try_load_blade_spine(run_dir)
      }
      generate_blade_business_spine(
        spine = spine_for_link,
        seed = seed,
        n_businesses = n_businesses,
        output_dir = output_dir,
        format = format,
        return_data = TRUE
      )
    }
  }
  stopifnot("`business_spine` must be a data.frame" =
              is.data.frame(business_spine))

  selected_tables <- .resolve_blade_tables(tables)
  results <- list()
  key_results <- list()
  return_frames <- list()
  link <- .load_blade_plida_link(run_dir, format)
  if (is.null(link) && !is.null(spine_for_link)) {
    link <- .make_blade_person_link(spine_for_link, business_spine, seed)
    business_spine <- .add_blade_link_reconciliation(business_spine, link)
    .write_blade_business_spine(business_spine, run_dir, format)
    .write_blade_plida_link(link, run_dir, format)
  }

  for (i in seq_len(nrow(selected_tables))) {
    table_number <- selected_tables[["Table.Number"]][i]
    product_name <- selected_tables[["Product.Name"]][i]
    source_type <- .blade_table_source_type(table_number, product_name)
    is_survey <- identical(source_type, "survey")
    table_sample_rate <- if (is_survey) sample_rate else 1
    table_max_rows <- if (is_survey) max_rows else Inf
    variables <- .blade_variables(table_number = table_number)
    variable_names <- .blade_table_variable_names(variables, table_number)
    if (table_number == 17L && !is.null(link)) {
      frame <- .make_blade_eeh_frame(
        variable_names = variable_names,
        link = link,
        business_spine = business_spine,
        seed = seed + i
      )
      frame <- .select_blade_frame_rows(frame, seed + i,
                                        table_sample_rate, table_max_rows)
    } else {
      row_idx <- .select_blade_rows(
        business_spine = business_spine,
        table_number = table_number,
        product_name = product_name,
        seed = seed,
        sample_rate = table_sample_rate,
        max_rows = table_max_rows
      )
      business_rows <- business_spine[row_idx, , drop = FALSE]
      # Every money generator downstream reads turnover, wages and their
      # derivatives off this slice, so moving the slice to the table's own
      # reference period is enough to give the whole table nominal values for
      # its year. The spine on disk keeps its anchor-year figures.
      business_rows <- .blade_reprice_business_rows(business_rows, table_number,
                                                    seed)
      if (table_number %in% c(24L, 25L)) {
        frame <- .blade_business_location_frame(
          variable_names = variable_names,
          business_rows = business_rows,
          table_number = table_number,
          product_name = product_name,
          seed = seed + i
        )
      } else {
        frame <- .make_blade_frame(
          variable_names = variable_names,
          business_rows = business_rows,
          table_number = table_number,
          product_name = product_name,
          seed = seed + i,
          variables = variables
        )
      }
    }
    path <- write_product(frame, product_name, "BLADE", run_dir,
                          format = format)
    results[[product_name]] <- list(
      table_number = table_number,
      source_type = source_type,
      n_rows = nrow(frame),
      n_variables = ncol(frame),
      path = path
    )
    if (return_data) return_frames[[product_name]] <- frame
  }

  if (isTRUE(include_keys)) {
    key_results <- .write_blade_keys(business_spine, run_dir,
                                     format = format)
  }

  if (return_data) return(return_frames)
  invisible(list(
    dataset = "BLADE",
    n_businesses = nrow(business_spine),
    tables = results,
    keys = key_results,
    link = list(
      path = .blade_plida_link_path(run_dir, format),
      n_rows = if (is.null(link)) 0L else nrow(link)
    )
  ))
}

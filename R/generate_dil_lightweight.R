# DIL-backed lightweight generators.
#
# These datasets are represented in the 19 March 2026 DIL but do not
# yet have bespoke behavioural Rust generators. The helper below keeps
# their output contract tied to the DIL: one file per PLIDA product,
# with all listed DIL variables present, and capped row counts so
# products = "all" remains usable at large N.

.dil_metadata_path <- function(filename) {
  path <- system.file("plida_metadata", filename, package = "fplida")
  if (!nzchar(path)) {
    path <- file.path("inst", "plida_metadata", filename)
  }
  path
}

.dil_products <- function(dataset) {
  path <- .dil_metadata_path("products.csv")
  products <- utils::read.csv(path, stringsAsFactors = FALSE,
                              check.names = FALSE)
  products[products$Dataset == dataset, , drop = FALSE]
}

.dil_variables <- function(dataset, product_name = NULL) {
  path <- .dil_metadata_path("variables.csv")
  variables <- utils::read.csv(path, stringsAsFactors = FALSE,
                               check.names = FALSE)
  rows <- variables$Dataset == dataset
  if (!is.null(product_name)) {
    rows <- rows & variables[["Product Name"]] == product_name
  }
  variables[rows, , drop = FALSE]
}

.stable_name_seed <- function(value) {
  ints <- utf8ToInt(as.character(value))
  as.integer(sum(ints * seq_along(ints)) %% 100000L)
}

.select_lightweight_rows <- function(spine, dataset, seed, sample_rate,
                                     max_rows) {
  n <- nrow(spine)
  if (n == 0L) return(integer(0))

  set.seed(seed + .stable_name_seed(dataset))
  target <- min(n, max(1L, as.integer(ceiling(n * sample_rate))), max_rows)
  if (target >= n) seq_len(n) else sort(sample.int(n, target))
}

.select_admin_rows <- function(spine, dataset, seed, sample_rate, max_rows) {
  reference_year <- switch(dataset,
                           APSED = 2024L,
                           ATO_MCS = 2018L,
                           ERS = 2020L,
                           JK = 2020L,
                           JM = 2021L,
                           SMSF = 2023L,
                           2024L)
  eligible <- !is.na(spine$birth_year) &
    spine$birth_year <= reference_year - 18L
  if (dataset %in% c("APSED", "JK", "JM") &&
      "baseline_employed" %in% names(spine)) {
    eligible <- eligible & !is.na(spine$baseline_employed) &
      spine$baseline_employed
  }
  eligible_rows <- which(eligible)
  if (!length(eligible_rows)) return(integer(0))
  selected <- .select_lightweight_rows(spine[eligible_rows, , drop = FALSE],
                                       dataset, seed, sample_rate, max_rows)
  eligible_rows[selected]
}

.dil_location_lookup_rows <- function(spine_rows, seed) {
  n <- nrow(spine_rows)
  if (n == 0L) {
    return(.load_mb_lookup()[0L, , drop = FALSE])
  }

  lookup <- .load_mb_lookup()
  states <- as.integer(spine_rows$state)
  states[is.na(states)] <- 1L
  states <- pmin(pmax(states, 1L), 8L)
  selected <- integer(n)

  row_key <- seq_len(n) + seed * 1009L
  for (st in sort(unique(states))) {
    idx <- which(states == st)
    pool <- which(lookup$state == st)
    if (!length(pool)) {
      stop("No Mesh Block lookup rows for state ", st, call. = FALSE)
    }
    pick <- as.integer((row_key[idx] * 2654435761 + st * 9176) %%
                         length(pool)) + 1L
    selected[idx] <- pool[pick]
  }

  lookup[selected, , drop = FALSE]
}

.dil_value_for <- function(name, spine_rows, aeuid, dataset, product_name,
                           seed, location_rows = NULL) {
  upper <- toupper(name)
  n <- length(aeuid)

  if (upper == "SYNTHETIC_AEUID" || grepl("AEUID|ESID", upper)) {
    return(aeuid)
  }
  if (upper %in% c("BN", "ABN", "ABN_HASH_TRUNC") ||
      grepl("(^|_)BN$|ABN|BUSINESS", upper)) {
    base <- seq_len(n) + seed + .stable_name_seed(product_name)
    return(sprintf("BN%012X", base %% 281474976710655))
  }
  if (grepl("FIN_YEAR|FNCL_YR|FINANCIAL_YEAR|PYRL_FNCL_YR", upper)) {
    return(rep("2023-24", n))
  }
  if (grepl("YEAR_MONTH|BIRTH_YEAR_MONTH", upper)) {
    return(sprintf("%04d%02d", spine_rows$birth_year,
                   ((seq_len(n) + seed) %% 12L) + 1L))
  }
  if (grepl("BIRTH_YEAR|DOB_YEAR", upper)) {
    return(as.integer(spine_rows$birth_year))
  }
  if (grepl("MONTH", upper)) {
    return(as.integer(((seq_len(n) + seed) %% 12L) + 1L))
  }
  if (grepl("YEAR|YR", upper)) {
    return(as.integer(2024L))
  }
  if (grepl("DATE|_DT$|START|END|CMNCMT|CESTN", upper)) {
    return(as.Date("2024-01-01") + ((seq_len(n) + seed) %% 365L))
  }
  if (grepl("AMT|INCM|INCOME|PMT|PAY|TAX|WHELD|GROSS|GRS|SUPER|CNTRBTN|CONTRIBUT|BAL|VALUE|VAL|COST|EXP|LOSS|DEBT", upper)) {
    base_income <- if ("baseline_income" %in% names(spine_rows)) {
      pmax(as.numeric(spine_rows$baseline_income), 0)
    } else {
      rep(50000, n)
    }
    return(round(base_income * (0.01 + ((seq_len(n) + seed) %% 100L) / 1000), 2))
  }
  if (grepl("FLAG|DUMMY|IND$", upper)) {
    return(as.integer((seq_len(n) + seed) %% 2L))
  }
  if (grepl("SEX", upper)) {
    return(ifelse(as.integer(spine_rows$sex) == 1L, "M", "F"))
  }
  if (grepl("STATE", upper)) {
    return(as.integer(spine_rows$state))
  }
  if (grepl("SA1", upper) && !is.null(location_rows)) {
    return(location_rows$sa1_code)
  }
  if (grepl("SA2", upper)) {
    if (!is.null(location_rows)) return(location_rows$sa2_code)
    return(sprintf("%09d", 100000000L + (seq_len(n) %% 90000000L)))
  }
  if (grepl("SA4", upper) && !is.null(location_rows)) {
    return(as.integer(location_rows$sa4_code))
  }
  if (grepl("LGA", upper)) {
    return(sprintf("%05d", 10000L + (seq_len(n) %% 80000L)))
  }
  if (grepl("MESH|(^|_)MB(_|$)", upper)) {
    if (!is.null(location_rows)) return(location_rows$mb_code)
    return(sprintf("%011.0f", 10000000000 + (seq_len(n) %% 900000000L)))
  }
  if (grepl("KEY|ID|IDENT|NUMBER|NMBR|SEQ", upper)) {
    return(sprintf("%s%08d", gsub("[^A-Z0-9]", "", dataset), seq_len(n)))
  }
  if (grepl("CODE|_CD$|CDE$|TYPE|TYP", upper)) {
    return(sprintf("C%02d", ((seq_len(n) + seed) %% 10L) + 1L))
  }

  sprintf("%s_%06d", tolower(dataset), seq_len(n))
}

# Administrative DIL values -------------------------------------------------
#
# The generic name-based generator above remains the explicitly deferred path
# for NHS, NSMHW, and PEX. Administrative products use the evidence-backed
# path below. The DIL supplies the released variable names and descriptions.
# The ALife manual confirms that the MCS and SMSF amount, balance,
# contribution, and count concepts are numerical. Neither source supplies
# complete value domains for every categorical code. Those codes remain typed
# missing rather than receiving a made-up code frame.

.dil_admin_datasets <- c("APSED", "ATO_MCS", "ERS", "JK", "JM", "SMSF")

.admin_unit_interval <- function(n, seed, salt = 0L) {
  if (n == 0L) return(numeric(0))
  key <- seq_len(n) + as.numeric(seed) * 1009 + as.numeric(salt) * 9176
  ((key * 104729 + 12345) %% 1000003) / 1000003
}

.admin_numeric_id <- function(n, seed, name, width = 12L) {
  if (n == 0L) return(character(0))
  salt <- .stable_name_seed(name)
  group_size <- if (grepl("ABN_HASH|EMPLOYER_ID", toupper(name))) {
    5L
  } else if (grepl("(^|_)(SF_SID|PRVDR_SID)$", toupper(name))) {
    3L
  } else {
    1L
  }
  row_key <- ((seq_len(n) - 1L) %/% group_size) + 1L
  value <- (row_key * 1000003 + as.numeric(seed) * 9176 + salt * 104729) %%
    (10^width)
  sprintf(paste0("%0", width, ".0f"), value)
}

.admin_abn <- function(n, seed, name) {
  if (n == 0L) return(character(0))
  salt <- .stable_name_seed(name)
  body <- (seq_len(n) * 1000003 + as.numeric(seed) * 9176 + salt * 104729) %%
    1000000000
  weights <- c(10L, 1L, 3L, 5L, 7L, 9L, 11L, 13L, 15L, 17L, 19L)

  vapply(body, function(value) {
    trailing <- as.integer(strsplit(sprintf("%09.0f", value), "", fixed = TRUE)[[1L]])
    for (prefix in 10L:99L) {
      digits <- c(prefix %/% 10L, prefix %% 10L, trailing)
      adjusted <- digits
      adjusted[1L] <- adjusted[1L] - 1L
      if (sum(adjusted * weights) %% 89L == 0L) {
        return(paste0(digits, collapse = ""))
      }
    }
    stop("Unable to construct a valid synthetic ABN.", call. = FALSE)
  }, character(1))
}

.admin_product_start_year <- function(dataset, product_name, n, seed) {
  match <- regexec("(20[0-9]{2})-([0-9]{2})$", product_name)
  parts <- regmatches(product_name, match)[[1L]]
  if (length(parts)) return(rep(as.integer(parts[2L]), n))
  if (dataset == "ATO_MCS") {
    return(1999L + ((seq_len(n) + seed) %% 19L))
  }
  if (dataset == "ERS") {
    return(2019L + ((seq_len(n) + seed) %% 2L))
  }
  rep(switch(dataset,
             APSED = 2023L,
             ERS = 2019L,
             JK = 2019L,
             JM = 2020L,
             2022L), n)
}

.admin_period <- function(dataset, product_name, n, seed) {
  start_year <- .admin_product_start_year(dataset, product_name, n, seed)
  start <- as.Date(sprintf("%04d-07-01", start_year))
  end <- as.Date(sprintf("%04d-06-30", start_year + 1L))
  if (dataset == "ERS") {
    start[] <- as.Date("2020-04-20")
    end[] <- as.Date("2020-12-31")
  } else if (dataset == "JK") {
    start[] <- as.Date("2020-03-30")
    end[] <- as.Date("2021-03-28")
  } else if (dataset == "JM") {
    start[] <- as.Date("2020-10-07")
    end[] <- as.Date("2022-10-06")
  }
  list(start_year = start_year, start = start, end = end)
}

.admin_date_between <- function(start, end, seed, salt = 0L) {
  span <- pmax(as.integer(end - start), 0L)
  start + floor(.admin_unit_interval(length(start), seed, salt) * (span + 1L))
}

.admin_postcode <- function(state, seed, salt = 0L) {
  ranges <- list(
    c(2000L, 2999L), c(3000L, 3999L), c(4000L, 4999L),
    c(5000L, 5799L), c(6000L, 6797L), c(7000L, 7999L),
    c(800L, 899L), c(2600L, 2618L)
  )
  state <- pmin(pmax(as.integer(state), 1L), 8L)
  out <- integer(length(state))
  u <- .admin_unit_interval(length(state), seed, salt)
  for (st in 1L:8L) {
    idx <- which(state == st)
    bounds <- ranges[[st]]
    out[idx] <- bounds[1L] + floor(u[idx] * (bounds[2L] - bounds[1L] + 1L))
  }
  sprintf("%04d", out)
}

.admin_codeframe_values <- function(filename, n, seed, salt = 0L) {
  path <- system.file("extdata", "codeframes", filename, package = "fplida")
  if (!nzchar(path)) path <- file.path("inst", "extdata", "codeframes", filename)
  frame <- utils::read.delim(path, stringsAsFactors = FALSE,
                             check.names = FALSE)
  weight <- suppressWarnings(as.numeric(frame$weight))
  weight[!is.finite(weight) | weight < 0] <- 0
  if (!sum(weight)) weight[] <- 1
  cumulative <- cumsum(weight) / sum(weight)
  u <- .admin_unit_interval(n, seed, salt)
  frame$code[vapply(u, function(value) which(cumulative >= value)[1L],
                    integer(1))]
}

.admin_amount <- function(name, spine_rows, seed) {
  n <- nrow(spine_rows)
  income <- if ("baseline_income" %in% names(spine_rows)) {
    pmax(as.numeric(spine_rows$baseline_income), 20000)
  } else {
    rep(50000, n)
  }
  upper <- toupper(name)
  u <- .admin_unit_interval(n, seed, .stable_name_seed(name))
  multiplier <- if (grepl("BAL|ASTS|ASSET|PRPTY|SHRS|TRSTS|INVST", upper)) {
    0.5 + 5 * u
  } else if (grepl("FEE|LEVY|EXPNS|DDCTN|DEDUCTION|COST", upper)) {
    0.002 + 0.08 * u
  } else if (grepl("CNTRBTN|CONTRIBUT|RLVR|SUPER", upper)) {
    0.01 + 0.25 * u
  } else if (grepl("TAX|CR_|OFSTS|OFFSET", upper)) {
    0.01 + 0.20 * u
  } else {
    0.02 + 0.80 * u
  }
  value <- round(income * multiplier, 2)
  if (grepl("LSS|LOSS|DUE_RFNDBL|TI_LSS", upper)) {
    negative <- (seq_len(n) + seed) %% 5L == 0L
    value[negative] <- -value[negative]
  }
  value
}

.admin_value_for <- function(name, description, spine_rows, aeuid, dataset,
                             product_name, seed, location_rows) {
  upper <- toupper(name)
  n <- length(aeuid)
  period <- .admin_period(dataset, product_name, n, seed)
  salt <- .stable_name_seed(name)

  if (upper == "SYNTHETIC_AEUID") return(aeuid)
  if (upper == "SPINE_ID") return(as.character(spine_rows$spine_id))
  if (upper %in% c("BN", "ABN")) return(.admin_abn(n, seed, name))
  if (grepl("ABN_HASH|ARID_HASH", upper)) {
    return(paste0("H", .admin_numeric_id(n, seed, name, 15L)))
  }
  if (grepl("(^|_)(SID|ID)$|EMPLOYER_ID|AGENCYID", upper)) {
    return(.admin_numeric_id(n, seed, name))
  }

  if (upper %in% c("FIN_YEAR", "REQUEST_FINANCIAL_YEAR")) {
    return(sprintf("%04d-%02d", period$start_year,
                   (period$start_year + 1L) %% 100L))
  }
  if (upper == "FIN_YR") return(period$start_year)
  if (upper == "INCM_YR") return(period$start_year + 1L)
  if (grepl("BRTH|DOB|YEAR_OF_BIRTH", upper) && grepl("YEAR|_YR|_DT", upper)) {
    return(as.integer(spine_rows$birth_year))
  }
  if (grepl("BRTH|DOB|MONTH_OF_BIRTH", upper) && grepl("MONTH|_DT", upper)) {
    return(as.integer(((seq_len(n) + seed) %% 12L) + 1L))
  }
  if (grepl("DTH|DEATH", upper) && grepl("MONTH", upper)) {
    if ("month_of_death" %in% names(spine_rows)) {
      return(as.integer(spine_rows$month_of_death))
    }
    return(rep(NA_integer_, n))
  }
  if (grepl("DTH|DEATH", upper) && grepl("YEAR|_YR|_DT", upper)) {
    if ("year_of_death" %in% names(spine_rows)) {
      return(as.integer(spine_rows$year_of_death))
    }
    return(rep(NA_integer_, n))
  }

  if (dataset == "ATO_MCS" && upper == "MBR_IDV_DCSD_IND") {
    if ("year_of_death" %in% names(spine_rows)) {
      return(!is.na(spine_rows$year_of_death))
    }
    return(rep(FALSE, n))
  }
  if (dataset == "ATO_MCS" && upper == "SF_MBRSHP_ACNT_OPND_DT") {
    earliest <- as.Date(sprintf("%04d-07-01",
                                as.integer(spine_rows$birth_year) + 18L))
    latest <- period$start
    earliest <- pmin(earliest, latest)
    return(.admin_date_between(earliest, latest, seed, salt))
  }

  if (dataset == "APSED" && upper == "SNAPSHOT_DATE") return(period$end)
  if (dataset == "APSED" && upper == "DATECHANGED") {
    return(.admin_date_between(period$start, period$end, seed, salt))
  }
  if (dataset == "APSED" && upper == "MOVEMENT_START_DATE") {
    return(.admin_date_between(period$start, period$end - 30L, seed, salt))
  }
  if (dataset == "APSED" && upper == "MOVEMENT_END_DATE") {
    start <- .admin_date_between(period$start, period$end - 30L, seed,
                                 .stable_name_seed("MOVEMENT_START_DATE"))
    return(pmin(start + 30L + floor(.admin_unit_interval(n, seed, salt) * 180L),
                 period$end))
  }
  if (dataset == "ERS" && upper == "REQUEST_DATE") {
    request_start <- ifelse(period$start_year == 2019L,
                            as.Date("2020-04-20"), as.Date("2020-07-01"))
    request_end <- ifelse(period$start_year == 2019L,
                          as.Date("2020-06-30"), as.Date("2020-12-31"))
    return(.admin_date_between(as.Date(request_start, origin = "1970-01-01"),
                               as.Date(request_end, origin = "1970-01-01") - 7L,
                               seed, salt))
  }
  if (dataset == "ERS" && upper == "APPROVAL_DATE") {
    request_start <- ifelse(period$start_year == 2019L,
                            as.Date("2020-04-20"), as.Date("2020-07-01"))
    request_end <- ifelse(period$start_year == 2019L,
                          as.Date("2020-06-30"), as.Date("2020-12-31"))
    request <- .admin_date_between(
      as.Date(request_start, origin = "1970-01-01"),
      as.Date(request_end, origin = "1970-01-01") - 7L, seed,
      .stable_name_seed("REQUEST_DATE"))
    return(request + 1L + floor(.admin_unit_interval(n, seed, salt) * 6L))
  }
  if (dataset == "JK" && upper == "DT_ELIGIBLE_EFFECT") {
    return(rep(as.Date("2020-03-30"), n))
  }
  if (dataset == "JK" && upper == "DT_ELIGIBLE_END") {
    return(rep(as.Date("2021-03-28"), n))
  }
  if (dataset == "JM" && upper == "EMPLE_JMHC_COMMENCE_DT") {
    return(.admin_date_between(period$start, as.Date("2021-10-06"), seed, salt))
  }
  if (dataset == "JM" && upper == "EMPLE_JMHC_CEASED_DT") {
    commence <- .admin_date_between(period$start, as.Date("2021-10-06"), seed,
                                    .stable_name_seed("EMPLE_JMHC_COMMENCE_DT"))
    return(pmin(commence + 90L +
                  floor(.admin_unit_interval(n, seed, salt) * 365L), period$end))
  }
  if (dataset == "JM" && upper == "RUN_DT") return(period$end)
  if (grepl("(_DT|_DATE|DATECHANGED)$", upper)) {
    return(.admin_date_between(period$start, period$end, seed, salt))
  }

  if (upper == "MB_CODE_2021") return(location_rows$mb_code)
  if (upper == "SA1_CODE_2021") return(location_rows$sa1_code)
  if (upper == "SA2_CODE_2021") return(location_rows$sa2_code)
  if (upper %in% c("STATE_CODE_2021", "ATO_STATE",
                   "STATE_OF_EMPLOYEES_WORKPLACE")) {
    return(as.integer(spine_rows$state))
  }
  if (grepl("POST_CODE|POSTCODE", upper)) {
    return(.admin_postcode(spine_rows$state, seed, salt))
  }
  if (upper %in% c("LGA_CODE_2024", "GEOCODED_INDEX")) {
    # The package has no authoritative MB-to-LGA allocation or geocoding frame.
    return(rep(NA_character_, n))
  }

  if (dataset == "APSED" && upper %in% c("COUNTRY_OF_BIRTH",
                                          "HIGHEST_EDUCATION_COUNTRY")) {
    return(.admin_codeframe_values("sacc_country.tsv", n, seed, salt))
  }
  if (dataset == "APSED" && grepl("LANGUAGE$", upper)) {
    return(.admin_codeframe_values("ascl_language.tsv", n, seed, salt))
  }
  if (dataset == "APSED" && upper == "MONTH_OF_BIRTH") {
    return(as.integer(((seq_len(n) + seed) %% 12L) + 1L))
  }
  if (dataset == "APSED" && upper == "YEAR_OF_BIRTH") {
    return(as.integer(spine_rows$birth_year))
  }
  if (dataset == "APSED" && upper == "ARRIVAL_YEAR") {
    age_at_arrival <- 1L + floor(.admin_unit_interval(n, seed, salt) * 35L)
    return(pmin(as.integer(spine_rows$birth_year) + age_at_arrival, 2023L))
  }
  if (dataset == "APSED" && upper == "HIGHEST_EDUCATION_YEAR_COMPLETED") {
    age_completed <- 18L + floor(.admin_unit_interval(n, seed, salt) * 12L)
    return(pmin(as.integer(spine_rows$birth_year) + age_completed, 2023L))
  }
  if (dataset == "APSED" && upper == "HOURS_WORKED_PER_WEEK") {
    hours <- c(37.5, 40, 30.4, 22.8, 15.2)
    return(hours[1L + floor(.admin_unit_interval(n, seed, salt) * length(hours))])
  }
  if (dataset == "APSED" && upper == "AGENCY_FUNCTION") {
    values <- c("Policy", "Smaller operational", "Larger operational",
                "Regulatory", "Specialist", "National Cultural Institution")
    return(values[1L + floor(.admin_unit_interval(n, seed, salt) * length(values))])
  }
  if (dataset == "APSED" && upper == "EMPLOYMENT_STATUS") {
    values <- c("Ongoing", "Non-ongoing - specified term",
                "Non-ongoing - specified task", "Non-ongoing - casual")
    return(values[1L + floor(.admin_unit_interval(n, seed, salt) * length(values))])
  }
  if (dataset == "APSED" && upper == "PREVIOUS_EMPLOYMENT") {
    values <- c("Private sector", "State government", "Student", "Unemployed")
    return(values[1L + floor(.admin_unit_interval(n, seed, salt) * length(values))])
  }
  if (dataset == "APSED" && upper == "DISABILITY") {
    if ("disability_type" %in% names(spine_rows)) {
      return(!is.na(spine_rows$disability_type))
    }
    return(.admin_unit_interval(n, seed, salt) < 0.12)
  }

  if (dataset == "ERS" && grepl("^ACCOUNT[1-5]_TYPE$", upper)) {
    values <- c("apra", "smsf")
    return(values[1L + floor(.admin_unit_interval(n, seed, salt) * 2L)])
  }
  if (dataset == "ERS" && upper == "REQUEST_APPROVED_STATUS") {
    return(rep("approved", n))
  }
  if (dataset == "ERS" && upper == "REQUEST_CATEGORY") {
    return(rep(NA_character_, n))
  }
  if (dataset == "JK" && upper == "CD_EMPLOYEE_TIER") {
    return(1L + as.integer(.admin_unit_interval(n, seed, salt) >= 0.55))
  }
  if (dataset == "JK" && upper == "JK_BUS_PRTCPNT") {
    return(ifelse(.admin_unit_interval(n, seed, salt) < 0.08, "Y", "N"))
  }
  if (dataset == "JK" && grepl("^JK_FN_[0-9]+$", upper)) {
    values <- c(10L, 20L, 30L)
    return(values[1L + floor(.admin_unit_interval(n, seed, salt) * 3L)])
  }
  if (dataset == "JK" &&
      grepl("^(JK_FN_ELIG_[0-9]+|CD_FN_ELGBLTY_[0-9]+)$", upper)) {
    return(.admin_unit_interval(n, seed, salt) < 0.9)
  }
  if (dataset == "JM" && upper == "AGE") {
    return(pmin(pmax(2021L - as.integer(spine_rows$birth_year), 18L), 69L))
  }
  if (dataset == "JM" && grepl("^EMPLE_(JMHC_ELGBLTY|MINHRS)_P[1-8]$", upper)) {
    return(.admin_unit_interval(n, seed, salt) < 0.82)
  }
  if (dataset == "JM" && grepl("^EMPLE_STP_EMPLR_CNT_", upper)) {
    return(1L + as.integer(floor(.admin_unit_interval(n, seed, salt) * 3L)))
  }
  if (dataset == "JM" && grepl("^EMPLE_STP_GRS_PAY_", upper)) {
    return(.admin_amount(name, spine_rows, seed))
  }

  if (grepl("(_IND|_IND_CD)$", upper)) {
    return(.admin_unit_interval(n, seed, salt) < 0.8)
  }
  if (grepl("(_CNT|_NUM)$", upper)) {
    return(as.integer(floor(.admin_unit_interval(n, seed, salt) * 6L)))
  }
  if (grepl("(_AMT|_BALANCE_[0-9]{4}|JK_DRVD_AMT)$", upper)) {
    return(.admin_amount(name, spine_rows, seed))
  }

  # The DIL describes these fields as codes or categories but does not give a
  # valid-response list. A typed missing value is safer than a plausible-looking
  # code that would be wrong for real analysis code.
  if (grepl("(_CD|_CODE)$|CLASSIFICATION|STATUS|GENDER|DISABILITY|INDIGENOUS|JOBFAMILY|FIELD_OF_STUDY|QUALIFICATION|PREVIOUS_EMPLOYMENT|AGENCY_SIZE", upper)) {
    return(rep(NA_character_, n))
  }

  rep(NA_character_, n)
}

.reconcile_admin_frame <- function(frame, dataset, seed) {
  n <- nrow(frame)
  if (dataset == "APSED" && all(c("COUNTRY_OF_BIRTH", "ARRIVAL_YEAR") %in%
                                  names(frame))) {
    frame$ARRIVAL_YEAR[frame$COUNTRY_OF_BIRTH == "1101"] <- NA_integer_
  }
  if (dataset == "ATO_MCS") {
    components <- intersect(c("ALCTD_SRPLS_AMT", "EMPLR_CNTRBTN_ACMLTD_AMT",
                              "EMPLR_DBS_CNTRBTN_AMT", "OTHR_CNTRBTNS_AMT",
                              "PRSNL_CNTRBTN_TOTL_AMT",
                              "PST_20081996_ETP_CMPNT_AMT"), names(frame))
    if (length(components) && "TOTL_CNTRBTN_AMT" %in% names(frame)) {
      frame$TOTL_CNTRBTN_AMT <- rowSums(frame[components], na.rm = TRUE)
    }
    transferred <- grep("^TRSF_.*_AMT$", names(frame), value = TRUE)
    for (name in transferred) {
      source <- sub("^TRSF_", "", name)
      if (source %in% names(frame)) {
        frame[[name]] <- round(frame[[source]] *
                                 (0.05 + 0.25 * .admin_unit_interval(
                                   n, seed, .stable_name_seed(name))), 2)
      }
    }
    transfer_components <- setdiff(transferred, "TRSF_TOTL_CNTRBTN_AMT")
    if (length(transfer_components) &&
        "TRSF_TOTL_CNTRBTN_AMT" %in% names(frame)) {
      frame$TRSF_TOTL_CNTRBTN_AMT <-
        rowSums(frame[transfer_components], na.rm = TRUE)
    }
  }
  if (dataset == "ERS") {
    approved <- grep("^ACCOUNT[1-5]_FUND_AMOUNT_APPROVED$", names(frame),
                     value = TRUE)
    balances_2020 <- grep("^ACCOUNT[1-5]_BALANCE_2020$", names(frame),
                          value = TRUE)
    account_count <- 1L + as.integer(floor(.admin_unit_interval(
      n, seed, .stable_name_seed("NUMBER_OF_ACCOUNTS_APPROVED")) * 5L))
    rejected <- .admin_unit_interval(
      n, seed, .stable_name_seed("REQUEST_APPROVED_STATUS")) >= 0.88
    account_count[rejected] <- 0L
    requested_total <- 1000 * (1L + floor(.admin_unit_interval(
      n, seed, .stable_name_seed("AMOUNT_REQUESTED_FOR_RELEASE")) * 10L))
    approved_total <- requested_total
    approved_total[rejected] <- 0
    for (i in seq_along(approved)) {
      value <- floor(approved_total / pmax(account_count, 1L) / 100) * 100
      last_account <- account_count == i
      allocated_before <- value * pmax(account_count - 1L, 0L)
      value[last_account] <- approved_total[last_account] -
        allocated_before[last_account]
      absent <- account_count < i
      value[absent] <- NA_real_
      frame[[approved[i]]] <- value
      if (i <= length(balances_2020)) {
        frame[[balances_2020[i]]] <- value + 1000 *
          (1L + floor(.admin_unit_interval(
            n, seed, .stable_name_seed(balances_2020[i])) * 40L))
      }
      type_name <- paste0("ACCOUNT", i, "_TYPE")
      if (type_name %in% names(frame)) {
        frame[[type_name]][absent] <- NA_character_
      }
    }
    if (length(approved)) {
      frame$NUMBER_OF_ACCOUNTS_APPROVED <- account_count
      frame$TOTAL_AMOUNT_APPROVED <- rowSums(frame[approved], na.rm = TRUE)
      frame$AMOUNT_REQUESTED_FOR_RELEASE <- requested_total
      frame$REQUEST_APPROVED_STATUS <- ifelse(rejected, "rejected", "approved")
      if ("APPROVAL_DATE" %in% names(frame)) {
        frame$APPROVAL_DATE[rejected] <- as.Date(NA)
      }
    }
  }
  if (dataset == "JK" && "JK_DRVD_AMT" %in% names(frame)) {
    tier <- if ("CD_EMPLOYEE_TIER" %in% names(frame)) {
      as.integer(frame$CD_EMPLOYEE_TIER)
    } else {
      rep(1L, n)
    }
    # JK2 had seven fortnights at $1,200/$750 and six at $1,000/$650.
    frame$JK_DRVD_AMT <- ifelse(tier == 1L,
                                7L * 1200 + 6L * 1000,
                                7L * 750 + 6L * 650)
  }
  if (dataset == "SMSF" && all(c("INCM_YR_FND_WND_UP_IND",
                                  "FND_WND_UP_DT") %in% names(frame))) {
    wound_up <- .admin_unit_interval(
      n, seed, .stable_name_seed("INCM_YR_FND_WND_UP_IND")) < 0.03
    frame$INCM_YR_FND_WND_UP_IND <- wound_up
    frame$FND_WND_UP_DT[!wound_up] <- as.Date(NA)
  }
  frame
}

.make_dil_frame <- function(variable_names, spine_rows, aeuid, dataset,
                            product_name, seed, variable_descriptions = NULL) {
  variable_names <- unique(variable_names[nzchar(variable_names)])
  if (!"SYNTHETIC_AEUID" %in% variable_names) {
    variable_names <- c("SYNTHETIC_AEUID", variable_names)
  }
  location_rows <- .dil_location_lookup_rows(spine_rows, seed)
  if (dataset %in% .dil_admin_datasets) {
    descriptions <- rep("", length(variable_names))
    if (!is.null(variable_descriptions)) {
      descriptions <- variable_descriptions[match(variable_names,
                                                    names(variable_descriptions))]
      descriptions[is.na(descriptions)] <- ""
    }
    out <- Map(.admin_value_for, variable_names, descriptions,
               MoreArgs = list(spine_rows = spine_rows, aeuid = aeuid,
                               dataset = dataset, product_name = product_name,
                               seed = seed, location_rows = location_rows))
  } else {
    out <- lapply(variable_names, .dil_value_for,
                  spine_rows = spine_rows,
                  aeuid = aeuid,
                  dataset = dataset,
                  product_name = product_name,
                  seed = seed,
                  location_rows = location_rows)
  }
  names(out) <- variable_names
  frame <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  if (dataset %in% .dil_admin_datasets) {
    frame <- .reconcile_admin_frame(frame, dataset, seed)
  }
  frame
}

.generate_dil_lightweight <- function(dataset, agency, spine = NULL,
                                      seed = 42L, output_dir = NULL,
                                      format = c("parquet", "csv"),
                                      return_data = TRUE,
                                      sample_rate = 0.01,
                                      max_rows = 10000L) {
  seed <- as.integer(seed)
  max_rows <- as.integer(max_rows)
  format <- match.arg(format)
  if (format != "parquet") {
    stop("DIL lightweight generators write parquet only.", call. = FALSE)
  }

  run_dir <- resolve_run_dir(output_dir)
  ds_dir <- dataset_dir(run_dir, dataset)
  aeuid_col <- paste0("aeuid_", tolower(agency))

  cols <- c("spine_id", aeuid_col, "birth_year", "sex", "state",
            "year_of_death", "month_of_death", "disability_type",
            "baseline_income", "baseline_employed")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))

  row_idx <- if (dataset %in% .dil_admin_datasets) {
    .select_admin_rows(spine, dataset, seed, sample_rate, max_rows)
  } else {
    .select_lightweight_rows(spine, dataset, seed, sample_rate, max_rows)
  }
  spine_rows <- spine[row_idx, , drop = FALSE]
  aeuid <- as.character(spine_rows[[aeuid_col]])

  mini_spine <- data.frame(
    spine_id = spine$spine_id,
    stringsAsFactors = FALSE
  )
  mini_spine[[aeuid_col]] <- spine[[aeuid_col]]

  products <- .dil_products(dataset)
  results <- list()
  return_frames <- list()

  for (i in seq_len(nrow(products))) {
    product_name <- products[["Product Name"]][i]
    vars <- .dil_variables(dataset, product_name)
    var_names <- vars[["Variable Name"]]
    descriptions <- vars[["Variable Description"]]
    names(descriptions) <- var_names
    frame <- .make_dil_frame(var_names, spine_rows, aeuid, dataset,
                             product_name, seed + i,
                             variable_descriptions = descriptions)
    path <- write_product(frame, product_name, dataset, run_dir,
                          format = format)
    results[[product_name]] <- list(
      n_rows = nrow(frame),
      n_variables = length(unique(var_names[nzchar(var_names)])),
      path = path
    )
    if (return_data) {
      return_frames[[product_name]] <- frame
    }
  }

  write_agency_spine(mini_spine, agency, ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) return(return_frames)
  invisible(list(dataset = dataset, products = results))
}

#' Generate nine PLIDA datasets
#'
#' These functions generate the APSED, ATO_MCS, ERS, JK, JM, NHS, NSMHW, PEX,
#' and SMSF datasets.
#'
#' @section Dataset and variable information:
#' These official websites give information about the datasets:
#'
#' - `APSED`: [APSC APSED overview](https://www.apsc.gov.au/initiatives-and-programs/workforce-information/workforce-data/aps-employment-database-apsed).
#'   Use `dataset_info("APSED")` and `variable_info("APSED")`.
#' - `ATO_MCS`: [ATO MCS specification](https://softwaredevelopers.ato.gov.au/MCSspecification).
#'   Use `dataset_info("ATO_MCS")` and `variable_info("ATO_MCS")`.
#' - `ERS`: [ABS PLIDA data and legislation](https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation).
#'   Use `dataset_info("ERS")` and `variable_info("ERS")`.
#' - `JK`: [ABS PLIDA data and legislation](https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation).
#'   Use `dataset_info("JK")` and `variable_info("JK")`.
#' - `JM`: [ABS PLIDA data and legislation](https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation).
#'   Use `dataset_info("JM")` and `variable_info("JM")`.
#' - `NHS`: [ABS National Health Survey microdata](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/national-health-survey).
#'   Use `dataset_info("NHS")` and `variable_info("NHS")`.
#' - `NSMHW`: [ABS mental-health-study microdata](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/national-study-mental-health-and-wellbeing).
#'   Use `dataset_info("NSMHW")` and `variable_info("NSMHW")`.
#' - `PEX`: [ABS Patient Experiences microdata](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/patient-experiences-australia).
#'   Use `dataset_info("PEX")` and `variable_info("PEX")`.
#' - `SMSF`: [ATO SMSF statistics](https://www.ato.gov.au/about-ato/research-and-statistics/in-detail/super-statistics/smsf-statistics).
#'   Use `dataset_info("SMSF")` and `variable_info("SMSF")`.
#'
#' @param spine Data.frame from `generate_spine()` or NULL. If NULL,
#'   the spine is loaded from the active run directory.
#' @param seed Integer. Random seed.
#' @param output_dir Character or NULL. Base output directory. If NULL,
#'   uses `get_data_path()`.
#' @param format Character. Currently parquet only. Passing `"csv"` is
#'   accepted by the argument matcher but rejected at runtime.
#' @param return_data Logical. If TRUE, returns generated data.frames in
#'   addition to writing parquet.
#'
#' @return If `return_data = TRUE`, a named list of data.frames keyed by
#'   PLIDA product name. If FALSE, an invisible metadata list with row
#'   counts, variable counts, and output paths.
#'
#' @rdname generate_dil_lightweight
#' @export
generate_apsed <- function(spine = NULL, seed = 42L, output_dir = NULL,
                           format = c("parquet", "csv"),
                           return_data = TRUE) {
  .generate_dil_lightweight("APSED", "APSC", spine, seed, output_dir,
                            format, return_data, sample_rate = 0.01,
                            max_rows = 20000L)
}

#' @rdname generate_dil_lightweight
#' @export
generate_ato_mcs <- function(spine = NULL, seed = 42L, output_dir = NULL,
                             format = c("parquet", "csv"),
                             return_data = TRUE) {
  .generate_dil_lightweight("ATO_MCS", "ATO", spine, seed, output_dir,
                            format, return_data, sample_rate = 0.05,
                            max_rows = 50000L)
}

#' @rdname generate_dil_lightweight
#' @export
generate_ers <- function(spine = NULL, seed = 42L, output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = TRUE) {
  .generate_dil_lightweight("ERS", "ATO", spine, seed, output_dir,
                            format, return_data, sample_rate = 0.03,
                            max_rows = 50000L)
}

#' @rdname generate_dil_lightweight
#' @export
generate_jk <- function(spine = NULL, seed = 42L, output_dir = NULL,
                        format = c("parquet", "csv"),
                        return_data = TRUE) {
  .generate_dil_lightweight("JK", "ATO", spine, seed, output_dir,
                            format, return_data, sample_rate = 0.08,
                            max_rows = 80000L)
}

#' @rdname generate_dil_lightweight
#' @export
generate_jm <- function(spine = NULL, seed = 42L, output_dir = NULL,
                        format = c("parquet", "csv"),
                        return_data = TRUE) {
  .generate_dil_lightweight("JM", "ATO", spine, seed, output_dir,
                            format, return_data, sample_rate = 0.02,
                            max_rows = 30000L)
}

#' @rdname generate_dil_lightweight
#' @export
generate_nhs <- function(spine = NULL, seed = 42L, output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = TRUE) {
  .generate_dil_lightweight("NHS", "ABS", spine, seed, output_dir,
                            format, return_data, sample_rate = 0.003,
                            max_rows = 5000L)
}

#' @rdname generate_dil_lightweight
#' @export
generate_nsmhw <- function(spine = NULL, seed = 42L, output_dir = NULL,
                           format = c("parquet", "csv"),
                           return_data = TRUE) {
  .generate_dil_lightweight("NSMHW", "ABS", spine, seed, output_dir,
                            format, return_data, sample_rate = 0.001,
                            max_rows = 1500L)
}

#' @rdname generate_dil_lightweight
#' @export
generate_pex <- function(spine = NULL, seed = 42L, output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = TRUE) {
  .generate_dil_lightweight("PEX", "ABS", spine, seed, output_dir,
                            format, return_data, sample_rate = 0.002,
                            max_rows = 3000L)
}

#' @rdname generate_dil_lightweight
#' @export
generate_smsf <- function(spine = NULL, seed = 42L, output_dir = NULL,
                          format = c("parquet", "csv"),
                          return_data = TRUE) {
  .generate_dil_lightweight("SMSF", "ATO", spine, seed, output_dir,
                            format, return_data, sample_rate = 0.01,
                            max_rows = 12000L)
}

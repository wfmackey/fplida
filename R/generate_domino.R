#' Generate DOMINO dataset (Centrelink Administrative Data)
#'
#' Projects DOMINO tables from the fplida spine, matching the real PLIDA
#' DOMINO structure. DOMINO is a spell-based longitudinal dataset tracking
#' Centrelink benefit receipt, medical assessments, payment history, and
#' demographics.
#'
#' 8 PLIDA modules are generated (excluding Special Benefits and Special
#' Tables), producing 14 tables total:
#' \describe{
#'   \item{det_ben}{Benefit spells (one row per spell per person)}
#'   \item{static_demogs}{Person-level demographics}
#'   \item{inc_emp_cont}{Employment income spells}
#'   \item{loc_dtls}{Location spells}
#'   \item{edu_lvl}{Education level spells}
#'   \item{hh_hse_dtls}{Housing details}
#'   \item{mcd_dtls}{Medical certificate details (DSP/CAR recipients)}
#'   \item{pyh_combined_dis}{Disability & Carers payment history}
#'   \item{pyh_combined_fam}{Families & Children payment history}
#'   \item{indg_stat}{Indigenous status}
#'   \item{pyh_combined_std}{Older Students payment history}
#'   \item{pyh_combined_age}{Retirement & Widows payment history}
#'   \item{pyh_combined_wrk}{Working Age payment history}
#'   \item{rlt_prin_carer_beta}{Principal Carer relationships}
#' }
#'
#' All code values use real Centrelink codes from the DSS Aristotle Metadata
#' Registry. Any mapping to SDAC or other frameworks happens in analysis.
#'
#' @section Dataset and variable information:
#' The [DSS research-dataset metadata](https://www.dss.gov.au/doing-business-us/corporate-policies/metadata-research-datasets)
#' website gives information about this dataset. Use `dataset_info("DOMINO")`
#' for dataset information. Use `variable_info("DOMINO")` for variables,
#' sources, value support, and topic tags.
#'
#' @param spine Data.frame (from \code{generate_spine()}) or NULL.
#' @param seed Integer. Random seed for DOMINO-specific generation.
#' @param years Integer vector. Observation years (default 2005:2024).
#' @param output_dir Character or NULL. Base output directory.
#' @param format Character. Output format: "parquet" (default) or "csv".
#' @param return_data Logical. If TRUE (default), return data.frames in memory;
#'   if FALSE, write to disk and return metadata only.
#'
#' @return A named list with 14 data.frames.
#'
#' @examples
#' \dontrun{
#' spine <- generate_spine(n = 1000L, seed = 1L)
#' domino <- generate_domino(spine = spine, seed = 1L)
#' str(domino$det_ben)
#' }
#'
#' @export
generate_domino <- function(spine = NULL, seed = 42L, years = 2005L:2024L,
                            output_dir = NULL,
                            format = c("parquet", "csv"),
                            return_data = TRUE) {
  seed <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  return_data <- as.logical(return_data)
  stopifnot(
    "`seed` must be an integer" = !is.na(seed),
    "`years` must be integer"   = all(!is.na(years))
  )

  run_dir <- resolve_run_dir(output_dir)

  # ---- Selective spine loading (memory-efficient) ----
  dom_cols <- c("spine_id", "aeuid_dss", "aeuid_dhda", "birth_year", "sex",
                "baseline_income", "education", "state",
                "country_of_birth", "country_of_birth_sacc", "indigenous",
                "disability_onset_year", "disability_type",
                "is_dc", "disability_severity",
                # Shared vitals: static_demogs reads these rather than
                # inventing its own birth month and death dates.
                "month_of_birth", "year_of_death", "month_of_death")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, dom_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))

  # Mini spine for agency spine writing
  mini_spine <- data.frame(
    spine_id  = spine$spine_id,
    aeuid_dss = spine$aeuid_dss,
    stringsAsFactors = FALSE
  )

  yr_range <- range(years)

  # Step 1: Select who interacts with Centrelink
  participants <- select_domino_participants(spine, seed, yr_range)

  # Step 2: Generate benefit spells (det_ben)
  det_ben <- generate_benefit_spells(participants, spine, seed, yr_range)

  # Step 3: Medical assessments (mcd_dtls) for DSP/CAR recipients.
  mcd_dtls <- generate_medical_assessments(det_ben, spine, seed)

  # Step 4: Payment history tables
  pyh <- generate_payment_history(det_ben, seed)

  # Step 5: Supplementary tables — build, write, free sequentially
  static_demogs <- project_domino_demogs(participants, spine, seed)
  write_product(static_demogs, domino_product_name("base", "static_demogs"),
                "DOMINO", run_dir, format)

  inc_emp_cont <- project_domino_income(participants, spine, seed, yr_range)
  write_product(inc_emp_cont, domino_product_name("base", "inc_emp_cont"),
                "DOMINO", run_dir, format)

  loc_dtls <- project_domino_locations(participants, spine, seed, yr_range)
  write_product(loc_dtls, domino_product_name("base", "loc_dtls"),
                "DOMINO", run_dir, format)

  edu_lvl <- project_domino_education(participants, spine, seed, yr_range)
  write_product(edu_lvl, domino_product_name("base", "edu_lvl"),
                "DOMINO", run_dir, format)

  hh_hse_dtls <- project_domino_housing(participants, spine, seed, yr_range)
  write_product(hh_hse_dtls, domino_product_name("base", "hh_hse_dtls"),
                "DOMINO", run_dir, format)

  indg_stat <- project_domino_indigenous(participants, spine)
  write_product(indg_stat, domino_product_name("indg-stat", "indg_stat"),
                "DOMINO", run_dir, format)

  rlt_beta <- project_domino_principal_carer(det_ben, spine, seed)
  write_product(rlt_beta,
                domino_product_name("rlt-prin-carer-beta", "rlt_prin_carer_beta"),
                "DOMINO", run_dir, format)

  # Free spine (no longer needed)
  if (spine_loaded && !return_data) { rm(spine); gc() }

  # Write core tables
  write_product(det_ben, domino_product_name("base", "det_ben"),
                "DOMINO", run_dir, format)
  write_product(mcd_dtls, domino_product_name("disability-carers", "mcd_dtls"),
                "DOMINO", run_dir, format)
  write_product(pyh$pyh_combined_dis,
                domino_product_name("disability-carers", "pyh_combined_dis"),
                "DOMINO", run_dir, format)
  write_product(pyh$pyh_combined_fam,
                domino_product_name("families-children", "pyh_combined_fam"),
                "DOMINO", run_dir, format)
  write_product(pyh$pyh_combined_std,
                domino_product_name("older-students", "pyh_combined_std"),
                "DOMINO", run_dir, format)
  write_product(pyh$pyh_combined_age,
                domino_product_name("retirement-widows", "pyh_combined_age"),
                "DOMINO", run_dir, format)
  write_product(pyh$pyh_combined_wrk,
                domino_product_name("working-age", "pyh_combined_wrk"),
                "DOMINO", run_dir, format)

  # Write DSS agency spine (mini)
  ds_dir <- dataset_dir(run_dir, "DOMINO")
  write_agency_spine(mini_spine, "DSS", ds_dir, format = format)

  if (return_data) {
    list(
      det_ben             = det_ben,
      static_demogs       = static_demogs,
      inc_emp_cont        = inc_emp_cont,
      loc_dtls            = loc_dtls,
      edu_lvl             = edu_lvl,
      hh_hse_dtls         = hh_hse_dtls,
      mcd_dtls            = mcd_dtls,
      pyh_combined_dis    = pyh$pyh_combined_dis,
      pyh_combined_fam    = pyh$pyh_combined_fam,
      indg_stat           = indg_stat,
      pyh_combined_std    = pyh$pyh_combined_std,
      pyh_combined_age    = pyh$pyh_combined_age,
      pyh_combined_wrk    = pyh$pyh_combined_wrk,
      rlt_prin_carer_beta = rlt_beta
    )
  } else {
    invisible(list(
      n_participants = nrow(participants),
      n_spells       = nrow(det_ben),
      years          = years,
      path           = "dss-domino"
    ))
  }
}


# =============================================================================
# Constants — real Centrelink codes from DSS Aristotle Metadata Registry
# =============================================================================

# Benefit types generated in Phase 1 and their relative shares.
# Full code lists per module are in inst/foundations/domino.toml.
.DOMINO_BEN_TYPES <- c("DSP", "NSA", "JSP", "YAL", "PPP", "PPS",
                        "AGE", "CAR", "CDA", "ABY", "AUS", "FTB")

# Module membership: which BEN_TYPE_CODE values belong to each module
.DOMINO_MODULE_CODES <- list(
  dis = c("BRV", "BSW", "CAR", "CDA", "DSP", "DWS", "MEP", "MOB", "WFD", "WFR"),
  wrk = c("JSA", "JSP", "MAA", "MPA", "NMA", "NSA", "NSS", "PPP", "PPS", "PTA"),
  fam = c("AEP", "BBY", "CCF", "CCI", "DAP", "DOP", "EIC", "FPA", "FTB", "FTP", "PPL"),
  std = c("ABA", "ABS", "ABY", "AUS", "EPA", "EPF", "PES", "TAP", "YAL", "YLA", "YLS", "YTA"),
  age = c("AGE", "BVA", "PDB", "PEN", "SHC", "WDA", "WFA", "WID")
)

# Duration: median days and gamma shape per benefit type
.DOMINO_DURATION <- list(
  DSP = c(median = 2190, shape = 0.8),
  NSA = c(median = 240,  shape = 1.5),
  JSP = c(median = 240,  shape = 1.5),
  YAL = c(median = 540,  shape = 1.2),
  PPP = c(median = 1095, shape = 1.5),
  PPS = c(median = 1460, shape = 1.2),
  AGE = c(median = 5840, shape = 0.5),
  CAR = c(median = 730,  shape = 1.0),
  CDA = c(median = 1825, shape = 0.8),
  FTB = c(median = 2190, shape = 1.0),
  ABY = c(median = 730,  shape = 1.2),
  AUS = c(median = 730,  shape = 1.2)
)

# Payment rates: fortnightly (2024 AUD)
.DOMINO_RATES_2024 <- c(
  DSP = 1020.40, NSA = 749.20, JSP = 749.20, YAL = 527.80,
  PPP = 660.80, PPS = 940.40, AGE = 1064.00, CAR = 1020.40,
  CDA = 144.40, FTB = 213.36, ABY = 527.80, AUS = 527.80
)

# The calendar year those rates are in force.
.DOMINO_RATE_YEAR <- 2024L

# Which published series a payment rate follows.
#
# Pensions -- Age, Disability Support, Carer Payment and Parenting Payment
# Single -- take the higher of the CPI and the Pensioner and Beneficiary Living
# Cost Index and are then benchmarked to Male Total Average Weekly Earnings,
# which binds in most years. Allowances and family payments are indexed to the
# CPI alone. The difference compounds: pensions have grown roughly a third
# faster than allowances since 2006, and the gap between the Age Pension and
# JobSeeker is one of the most-studied features of Australian income support.
#
# Keep in step with `rate_series()` in src/rust/src/domino.rs.
.domino_rate_series <- function(ben_type) {
  if (ben_type %in% c("AGE", "DSP", "CAR", "PPS")) "transfer" else "price"
}

# End reason codes and weights
.DOMINO_END_RSN_CODES <- c("EMP", "INC", "ASS", "NFR", "APR", "ADE",
                            "AGE", "OTH", "FSD", "NRQ", "CLR", "6WK")
.DOMINO_END_RSN_WEIGHTS <- c(0.25, 0.15, 0.05, 0.10, 0.10, 0.01,
                              0.05, 0.15, 0.05, 0.02, 0.02, 0.05)

# Medical condition group codes (MED_PRMY_GRP)
# Source: https://dss.aristotlecloud.io/item/107942
.DOMINO_MED_GROUPS <- c(
  "MUS", "PSY", "INT", "NER", "CIR", "RES", "SEN", "ABI",
  "CAN", "CHR", "CFS", "CGA", "EIS", "GIS", "IFD", "IHD",
  "AMP", "REP", "SDB", "URO", "VIS", "PFC"
)
.DOMINO_MED_SHARES <- c(
  0.280, 0.250, 0.050, 0.080, 0.040, 0.040, 0.030, 0.030,
  0.030, 0.050, 0.010, 0.010, 0.020, 0.020, 0.010, 0.010,
  0.010, 0.010, 0.005, 0.005, 0.005, 0.005
)


# =============================================================================
# Step 1: Participant Selection
# =============================================================================

#' Select DOMINO participants from the spine
#'
#' Determines who interacts with Centrelink over the observation window,
#' based on age, income, and employment status. Assigns each participant a
#' primary benefit type and first contact year.
#'
#' Phase 2 hook: when the spine contains \code{disability_onset_year} and
#' \code{disability_type} columns, DC/NC-specific DSP patterns are layered on.
#'
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @param yr_range Integer vector of length 2 (min_year, max_year).
#' @return data.frame of participants with columns: spine_idx, aeuid,
#'   primary_ben, first_year, last_year, phase2_dc, phase2_nc.
#' @keywords internal
select_domino_participants <- function(spine_df, seed, yr_range) {
  n <- nrow(spine_df)
  min_yr <- yr_range[1L]
  max_yr <- yr_range[2L]

  if (exists("select_domino_participants__", mode = "function")) {
    onset_year <- if ("disability_onset_year" %in% names(spine_df)) {
      as.integer(spine_df$disability_onset_year)
    } else {
      rep(NA_integer_, n)
    }
    is_dc <- if ("is_dc" %in% names(spine_df)) {
      as.integer(spine_df$is_dc)
    } else {
      rep(NA_integer_, n)
    }
    severity <- if ("disability_severity" %in% names(spine_df)) {
      as.integer(spine_df$disability_severity)
    } else {
      rep(NA_integer_, n)
    }
    raw <- select_domino_participants__(
      birth_year            = as.integer(spine_df$birth_year),
      baseline_income       = as.numeric(spine_df$baseline_income),
      sex                   = as.integer(spine_df$sex),
      education             = as.integer(spine_df$education),
      aeuid_dss             = as.character(spine_df$aeuid_dss),
      disability_onset_year = onset_year,
      disability_is_dc      = is_dc,
      disability_severity   = severity,
      seed                  = as.integer(seed),
      min_year              = min_yr,
      max_year              = max_yr
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    df$phase2_dc <- as.logical(df$phase2_dc)
    df$phase2_nc <- as.logical(df$phase2_nc)
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 900L)

  birth_year <- spine_df$birth_year
  income     <- spine_df$baseline_income
  sex        <- spine_df$sex

  # Age at midpoint of observation window
  mid_yr <- as.integer(round(mean(yr_range)))
  age_mid <- mid_yr - birth_year

  # Probability of ever contacting Centrelink
  p_contact <- ifelse(income < 25000, 0.08,
               ifelse(income < 50000, 0.03, 0.005))

  # Employment multiplier: low income → likely not employed
  # Use baseline_income < 15000 as proxy for unemployed
  p_contact <- ifelse(income < 15000, pmin(p_contact * 4, 0.5), p_contact)

  # Convert annual probability to ever-in-window probability
  n_years <- max_yr - min_yr + 1L
  p_ever <- 1 - (1 - p_contact)^n_years

  # Age 65+: high probability of Age Pension
  is_age_pension <- age_mid >= 65L
  p_ever <- ifelse(is_age_pension, 0.70, p_ever)

  # Young adults (16-24): elevated Youth Allowance probability
  is_young <- age_mid >= 16L & age_mid <= 24L
  p_ever <- ifelse(is_young & !is_age_pension,
                   pmax(p_ever, 0.20), p_ever)

  # Children and very young: FTB via parents (proxy: mothers 25-45)
  is_parent_proxy <- sex == 2L & age_mid >= 25L & age_mid <= 45L
  p_ever <- ifelse(is_parent_proxy, pmax(p_ever, 0.25), p_ever)

  # Disability: only severity 1-2 (profound/moderate) interact with Centrelink
  # at high rates for DSP/welfare receipt. Severity 3-4 (CHC-only) do not get
  # elevated Centrelink participation.
  has_phase2 <- all(c("disability_onset_year", "is_dc", "disability_severity") %in% names(spine_df))
  if (has_phase2) {
    dis_has <- !is.na(spine_df$disability_onset_year)
    dis_severe <- dis_has & !is.na(spine_df$disability_severity) &
                  spine_df$disability_severity <= 2L
    dis_dc  <- dis_severe & !is.na(spine_df$is_dc) & spine_df$is_dc
    dis_nc  <- dis_severe & !is.na(spine_df$is_dc) & !spine_df$is_dc
    p_ever[dis_dc] <- pmax(p_ever[dis_dc], 0.95)
    p_ever[dis_nc] <- pmax(p_ever[dis_nc], 0.80)
  }

  # Draw participation
  draws <- runif(n)
  is_participant <- draws < p_ever

  # Must be at least 16 during the window
  age_at_start <- min_yr - birth_year
  is_participant <- is_participant & (age_at_start + n_years > 16L)

  idx <- which(is_participant)
  if (length(idx) == 0L) return(.empty_domino_participants())

  n_part <- length(idx)

  # Assign primary benefit type
  primary_ben <- character(n_part)

  age_at_start_p <- min_yr - birth_year[idx]
  age_at_end_p   <- max_yr - birth_year[idx]
  income_p       <- income[idx]
  sex_p          <- sex[idx]

  # Age Pension (65+ at some point in window)
  on_age <- age_at_end_p >= 65L & age_at_start_p < 90L
  primary_ben[on_age] <- "AGE"

  # Parenting Payment (mothers 25-45, low/mid income)
  on_ppp <- primary_ben == "" & sex_p == 2L &
            age_mid[idx] >= 25L & age_mid[idx] <= 45L & income_p < 50000
  primary_ben[on_ppp] <- sample(c("PPP", "PPS"), sum(on_ppp), replace = TRUE,
                                 prob = c(0.55, 0.45))

  # FTB (parents not already assigned)
  on_ftb <- primary_ben == "" & is_parent_proxy[idx] & income_p < 80000
  primary_ben[on_ftb] <- "FTB"

  # Youth Allowance (16-24)
  on_yal <- primary_ben == "" & age_at_start_p >= 16L & age_at_start_p <= 24L
  primary_ben[on_yal] <- "YAL"

  # Student payments (subset of young adults)
  on_student <- primary_ben == "" &
                age_at_start_p >= 18L & age_at_start_p <= 30L &
                spine_df$education[idx] >= 4L
  primary_ben[on_student] <- sample(c("ABY", "AUS"), sum(on_student),
                                     replace = TRUE, prob = c(0.4, 0.6))

  # DSP (background rate ~1-2% of working age)
  on_dsp <- primary_ben == "" &
            age_at_start_p >= 18L & age_at_end_p < 65L &
            runif(n_part) < 0.03
  primary_ben[on_dsp] <- "DSP"

  # CAR/CDA (carers, ~2%)
  on_car <- primary_ben == "" &
            age_at_start_p >= 18L &
            runif(n_part) < 0.02
  primary_ben[on_car] <- sample(c("CAR", "CDA"), sum(on_car), replace = TRUE,
                                 prob = c(0.4, 0.6))

  # JobSeeker / Newstart (remaining low-income working age)
  on_jsp <- primary_ben == "" & income_p < 40000
  primary_ben[on_jsp] <- "NSA"  # Will be switched to JSP for years >= 2020

  # Any remaining: assign NSA as fallback

  primary_ben[primary_ben == ""] <- "NSA"

  # First contact year: based on age eligibility and benefit type
  first_year <- integer(n_part)
  first_year <- pmax(min_yr, birth_year[idx] + 16L)  # at least 16

  # For Age Pension: first eligible year
  first_year[on_age] <- pmax(min_yr, birth_year[idx[on_age]] + 65L)

  # Add random offset (0-3 years) for non-age-pension
  offset <- sample(0L:3L, n_part, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))
  first_year[!on_age] <- pmin(first_year[!on_age] + offset[!on_age], max_yr)

  # Clamp to observation window
  first_year <- pmax(first_year, min_yr)
  first_year <- pmin(first_year, max_yr)

  # Last year: some exit before end of window
  last_year <- rep(max_yr, n_part)
  exits_early <- runif(n_part) < 0.30
  if (any(exits_early)) {
    exit_offset <- sample(1L:8L, sum(exits_early), replace = TRUE)
    last_year[exits_early] <- pmin(first_year[exits_early] + exit_offset, max_yr)
  }

  # Phase 2: disability-driven DSP receipt
  has_phase2 <- all(c("disability_onset_year", "disability_type",
                       "is_dc", "disability_severity") %in% names(spine_df))

  phase2_dc <- rep(FALSE, n_part)
  phase2_nc <- rep(FALSE, n_part)

  if (has_phase2) {
    dis_onset <- spine_df$disability_onset_year[idx]
    dis_is_dc <- spine_df$is_dc[idx]
    dis_sev   <- spine_df$disability_severity[idx]

    has_dis <- !is.na(dis_onset)
    # Only severity 1-2 get DSP and DC/NC employment treatment effects.
    # Severity 3-4 (CHC-only) get condition-specific MBS/PBS but no DSP.
    dis_severe <- has_dis & !is.na(dis_sev) & dis_sev <= 2L
    is_dc   <- dis_severe & !is.na(dis_is_dc) & dis_is_dc
    is_nc   <- dis_severe & !is.na(dis_is_dc) & !dis_is_dc

    phase2_dc <- is_dc
    phase2_nc <- is_nc

    # DC workers (severity 1-2): ~40% of ALL DC receive DSP
    # With ~95% DOMINO participation, need ~42% conditional rate
    # Severity: 1=profound -> 0.65, 2=moderate -> 0.48
    dc_dsp_rate <- ifelse(dis_sev == 1L, 0.65,
                   ifelse(dis_sev == 2L, 0.48, 0.0))
    dc_gets_dsp <- is_dc & runif(n_part) < dc_dsp_rate

    # NC workers (severity 1-2): ~15% of ALL NC receive DSP
    # With ~80% DOMINO participation, need ~19% conditional rate
    # Severity: 1=profound -> 0.35, 2=moderate -> 0.22
    nc_dsp_rate <- ifelse(dis_sev == 1L, 0.35,
                   ifelse(dis_sev == 2L, 0.22, 0.0))
    nc_gets_dsp <- is_nc & runif(n_part) < nc_dsp_rate

    # Override benefit type to DSP for disability-driven recipients
    gets_dsp <- dc_gets_dsp | nc_gets_dsp
    primary_ben[gets_dsp] <- "DSP"

    # Set first_year to onset year for DSP recipients (admin lag ~1 year)
    first_year[gets_dsp] <- pmax(
      min_yr,
      pmin(max_yr, dis_onset[gets_dsp] + sample(0L:2L, sum(gets_dsp),
                                                  replace = TRUE))
    )

    # DSP spells are long-term: extend last_year to max
    last_year[gets_dsp] <- max_yr
  }

  data.frame(
    spine_idx  = idx,
    aeuid      = spine_df$aeuid_dss[idx],
    primary_ben = primary_ben,
    first_year = first_year,
    last_year  = last_year,
    phase2_dc  = phase2_dc,
    phase2_nc  = phase2_nc,
    stringsAsFactors = FALSE
  )
}

.empty_domino_participants <- function() {
  data.frame(
    spine_idx   = integer(0),
    aeuid       = character(0),
    primary_ben = character(0),
    first_year  = integer(0),
    last_year   = integer(0),
    phase2_dc   = logical(0),
    phase2_nc   = logical(0),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Step 2: Benefit Spell Generation
# =============================================================================

#' Generate benefit spells (det_ben table)
#'
#' Creates the det_ben table: one row per benefit spell per person.
#' Spells have start/end dates, duration, status, and end reason.
#'
#' @param participants data.frame from select_domino_participants().
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @param yr_range Integer vector of length 2.
#' @return data.frame with det_ben columns.
#' @keywords internal
generate_benefit_spells <- function(participants, spine_df, seed, yr_range) {
  if (nrow(participants) == 0L) return(.empty_det_ben())

  # Payments stop at death. Resolve each participant's spine death date to
  # days since epoch; NA for anyone with no recorded death.
  death_day <- rep(NA_integer_, nrow(participants))
  if (!is.null(spine_df$year_of_death) && !is.null(spine_df$aeuid_dss)) {
    idx <- match(as.character(participants$aeuid), as.character(spine_df$aeuid_dss))
    dy <- spine_df$year_of_death[idx]
    dm <- if (!is.null(spine_df$month_of_death)) spine_df$month_of_death[idx] else NA_integer_
    dm[is.na(dm)] <- 12L
    has <- !is.na(dy)
    if (any(has)) {
      death_day[has] <- as.integer(as.Date(sprintf("%04d-%02d-01", dy[has], dm[has])))
    }
  }

  # Try Rust implementation first
  if (exists("generate_benefit_spells__", mode = "function")) {
    raw <- generate_benefit_spells__(
      aeuid       = as.character(participants$aeuid),
      primary_ben = as.character(participants$primary_ben),
      first_year  = as.integer(participants$first_year),
      last_year   = as.integer(participants$last_year),
      death_day   = as.integer(death_day),
      seed        = as.integer(seed),
      min_year    = as.integer(yr_range[1L]),
      max_year    = as.integer(yr_range[2L])
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    # Convert date strings to Date objects
    df$PERIOD_START_DATE <- as.Date(df$PERIOD_START_DATE)
    df$PERIOD_END_DATE   <- as.Date(df$PERIOD_END_DATE)
    # Sort by person then start date
    ord <- order(df$SYNTHETIC_AEUID, df$PERIOD_START_DATE)
    df <- df[ord, ]
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 901L)

  n <- nrow(participants)
  min_date <- as.Date(paste0(yr_range[1L], "-01-01"))
  max_date <- as.Date(paste0(yr_range[2L], "-12-31"))

  # Pre-allocate lists for speed
  all_aeuid  <- vector("list", n)
  all_ben    <- vector("list", n)
  all_status <- vector("list", n)
  all_start  <- vector("list", n)
  all_end    <- vector("list", n)
  all_durn   <- vector("list", n)
  all_endrsn <- vector("list", n)
  all_endrsnc <- vector("list", n)

  for (i in seq_len(n)) {
    aeuid      <- participants$aeuid[i]
    ben_type   <- participants$primary_ben[i]
    first_yr   <- participants$first_year[i]
    last_yr    <- participants$last_year[i]

    # Historical code switch: NSA → JSP from 2020-03-20
    if (ben_type == "NSA" && first_yr >= 2020L) {
      ben_type <- "JSP"
    }

    # Draw spell duration
    dur_params <- .DOMINO_DURATION[[ben_type]]
    if (is.null(dur_params)) dur_params <- c(median = 365, shape = 1.0)
    dur_days <- .draw_spell_duration(dur_params[1], dur_params[2])

    # Spell start: random day in first_year
    start_month <- sample.int(12L, 1L)
    start_day   <- sample.int(28L, 1L)  # safe day range
    spell_start <- as.Date(sprintf("%04d-%02d-%02d", first_yr, start_month, start_day))
    spell_start <- max(spell_start, min_date)

    spell_end <- spell_start + dur_days

    # Determine status
    if (spell_end > max_date) {
      # Ongoing spell: no end date
      status  <- "CUR"
      end_rsn <- NA_character_
      end_rsn_code <- NA_character_
      spell_end_out <- NA
      durn <- as.integer(max_date - spell_start)
    } else if (spell_end > as.Date(sprintf("%04d-12-31", last_yr))) {
      # Ended after their last observed year
      spell_end <- as.Date(sprintf("%04d-%02d-%02d", last_yr,
                                    sample.int(12L, 1L),
                                    sample.int(28L, 1L)))
      if (spell_end <= spell_start) spell_end <- spell_start + 30L
      status <- "CUR"
      end_rsn <- "CAN"
      end_rsn_code <- sample(.DOMINO_END_RSN_CODES, 1L,
                              prob = .DOMINO_END_RSN_WEIGHTS)
      spell_end_out <- spell_end
      durn <- as.integer(spell_end - spell_start)
    } else {
      status <- "CUR"
      end_rsn <- "CAN"
      end_rsn_code <- sample(.DOMINO_END_RSN_CODES, 1L,
                              prob = .DOMINO_END_RSN_WEIGHTS)
      spell_end_out <- spell_end
      durn <- dur_days
    }

    # Some spells are suspended rather than cancelled
    if (!is.na(end_rsn) && runif(1) < 0.08) {
      status  <- "SUS"
      end_rsn <- "SUS"
    }

    # NSA → JSP switchover: split spell if it crosses 2020-03-20
    cutover <- as.Date("2020-03-20")
    if (ben_type == "NSA" && spell_start < cutover &&
        (is.na(spell_end_out) || spell_end_out > cutover)) {
      # Two spells: NSA until cutover, JSP from cutover
      all_aeuid[[i]]   <- c(aeuid, aeuid)
      all_ben[[i]]     <- c("NSA", "JSP")
      all_status[[i]]  <- c("CUR", status)
      all_start[[i]]   <- c(spell_start, cutover)
      all_end[[i]]     <- c(cutover - 1L, spell_end_out)
      all_durn[[i]]    <- c(as.integer(cutover - 1L - spell_start),
                            if (is.na(spell_end_out)) as.integer(max_date - cutover)
                            else as.integer(spell_end_out - cutover))
      all_endrsn[[i]]  <- c("CAN", end_rsn)
      all_endrsnc[[i]] <- c("OTH", end_rsn_code)
    } else {
      all_aeuid[[i]]   <- aeuid
      all_ben[[i]]     <- ben_type
      all_status[[i]]  <- status
      all_start[[i]]   <- spell_start
      all_end[[i]]     <- spell_end_out
      all_durn[[i]]    <- durn
      all_endrsn[[i]]  <- end_rsn
      all_endrsnc[[i]] <- end_rsn_code
    }
  }

  det_ben <- data.frame(
    SYNTHETIC_AEUID  = unlist(all_aeuid),
    BEN_TYPE_CODE    = unlist(all_ben),
    BEN_STATUS       = unlist(all_status),
    PERIOD_START_DATE = do.call(c, all_start),
    PERIOD_END_DATE   = do.call(c, all_end),
    DURN_DAYS        = unlist(all_durn),
    END_RSN          = unlist(all_endrsn),
    END_RSN_CODE     = unlist(all_endrsnc),
    stringsAsFactors = FALSE
  )

  # Sort by person then start date
  ord <- order(det_ben$SYNTHETIC_AEUID, det_ben$PERIOD_START_DATE)
  det_ben <- det_ben[ord, ]
  rownames(det_ben) <- NULL
  det_ben
}

.draw_spell_duration <- function(median_days, shape) {
  # Gamma distribution calibrated to target median
  scale <- median_days / qgamma(0.5, shape = shape)
  days <- round(rgamma(1L, shape = shape, scale = scale))
  max(days, 30L)  # minimum 30 days
}

.empty_det_ben <- function() {
  data.frame(
    SYNTHETIC_AEUID   = character(0),
    BEN_TYPE_CODE     = character(0),
    BEN_STATUS        = character(0),
    PERIOD_START_DATE = as.Date(character(0)),
    PERIOD_END_DATE   = as.Date(character(0)),
    DURN_DAYS         = integer(0),
    END_RSN           = character(0),
    END_RSN_CODE      = character(0),
    stringsAsFactors  = FALSE
  )
}


# =============================================================================
# Step 3: Medical Assessments
# =============================================================================

#' Generate medical assessment records (mcd_dtls table)
#'
#' Creates the mcd_dtls table for DSP and CAR spell recipients.
#' Uses real Centrelink MED_PRMY_GRP codes.
#'
#' @param det_ben data.frame from generate_benefit_spells().
#' @param spine_df Optional person spine. It is not used.
#' @param seed Integer seed.
#' @return data.frame with mcd_dtls columns.
#' @keywords internal
generate_medical_assessments <- function(det_ben, spine_df = NULL, seed) {
  if (nrow(det_ben) == 0L) return(.empty_mcd_dtls())

  if (exists("generate_medical_assessments__", mode = "function")) {
    raw <- generate_medical_assessments__(
      det_ben_aeuid                 = as.character(det_ben$SYNTHETIC_AEUID),
      det_ben_type_code             = as.character(det_ben$BEN_TYPE_CODE),
      det_ben_period_start_date     = as.character(det_ben$PERIOD_START_DATE),
      seed                          = as.integer(seed)
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    date_cols <- c(
      "PERIOD_START_DATE", "PERIOD_END_DATE", "IMPRMT_VER_DATE",
      "INCAP_START", "INCAP_END", "TEMP_LMT_CAP_START_DATE",
      "TEMP_LMT_CAP_END_DATE", "TEMP_REDN_CAP_END_DATE"
    )
    for (col in date_cols) {
      df[[col]] <- as.Date(df[[col]])
    }
    ord <- order(df$SYNTHETIC_AEUID, df$PERIOD_START_DATE)
    df <- df[ord, , drop = FALSE]
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 902L)

  # Only DSP and CAR spells get medical assessments
  med_idx <- which(det_ben$BEN_TYPE_CODE %in% c("DSP", "CAR"))
  if (length(med_idx) == 0L) return(.empty_mcd_dtls())

  n_med <- length(med_idx)

  # Primary medical code
  med_prmy <- sample(.DOMINO_MED_GROUPS, n_med, replace = TRUE,
                      prob = .DOMINO_MED_SHARES)

  # Secondary medical code (~30% have one)
  has_secondary <- runif(n_med) < 0.30
  med_scndry <- ifelse(has_secondary,
                        sample(.DOMINO_MED_GROUPS, n_med, replace = TRUE,
                               prob = .DOMINO_MED_SHARES),
                        NA_character_)

  # Impairment rating (20-40 scale, truncated normal)
  imprmt_rate <- pmin(40L, pmax(20L,
    as.integer(round(rnorm(n_med, mean = 28, sd = 5)))))

  # Impairment code: 1-3 scale (1=fast improvement, 2=slow, 3=none)
  imprmt_code <- sample(1L:3L, n_med, replace = TRUE,
                         prob = c(0.15, 0.25, 0.60))

  # Current work capacity (hours per week)
  cap_bins <- c(0, 7, 14, 22, 30)
  cap_probs <- c(0.45, 0.15, 0.15, 0.15, 0.10)
  cap_idx <- sample.int(length(cap_bins), n_med, replace = TRUE,
                         prob = cap_probs)
  curr_capcty <- cap_bins[cap_idx]
  # Add jitter within bin
  curr_capcty <- pmin(30L, pmax(0L, curr_capcty +
                                     sample(-2L:2L, n_med, replace = TRUE)))

  # With intervention capacity: slightly higher
  with_intrvn <- pmin(40L, curr_capcty + sample(0L:8L, n_med, replace = TRUE))

  # Assessment dates: near spell start
  spell_starts <- det_ben$PERIOD_START_DATE[med_idx]
  assmt_offset <- sample(-180L:30L, n_med, replace = TRUE)
  period_start <- spell_starts + assmt_offset

  # Assessment end: start + 90-365 days
  assmt_dur <- sample(90L:365L, n_med, replace = TRUE)
  period_end <- period_start + assmt_dur

  # Incapacity dates: near assessment period
  incap_start <- period_start + sample(0L:30L, n_med, replace = TRUE)
  has_incap_end <- runif(n_med) < 0.60
  incap_end <- ifelse(has_incap_end,
                       incap_start + sample(90L:730L, n_med, replace = TRUE),
                       NA_real_)
  incap_end <- as.Date(incap_end, origin = "1970-01-01")

  # Incapacity work hours (adjusted)
  incap_wk_hrs <- pmin(30L, pmax(0L,
    curr_capcty - sample(0L:5L, n_med, replace = TRUE)))

  # Manifestly disabled code: ~15% of DSP
  man_code <- ifelse(det_ben$BEN_TYPE_CODE[med_idx] == "DSP" & runif(n_med) < 0.15,
                      "Y", "N")

  # Activity participation code
  actv_codes <- c("A", "E", "N", "P")
  actv_prtcpn <- sample(actv_codes, n_med, replace = TRUE,
                          prob = c(0.30, 0.20, 0.35, 0.15))

  # Secondary medical code identifier and permanent indicator
  med_scndry_id <- ifelse(has_secondary,
                           sprintf("S%04d", sample.int(9999L, n_med, replace = TRUE)),
                           NA_character_)
  med_scndry_perm <- ifelse(has_secondary,
                             sample(c("Y", "N"), n_med, replace = TRUE,
                                    prob = c(0.60, 0.40)),
                             NA_character_)

  # Temp limited capacity dates (subset)
  has_temp <- runif(n_med) < 0.20
  temp_start <- ifelse(has_temp, incap_start + sample(0L:60L, n_med, replace = TRUE),
                        NA_real_)
  temp_start <- as.Date(temp_start, origin = "1970-01-01")
  temp_end <- ifelse(has_temp,
                      as.numeric(temp_start) + sample(90L:365L, n_med, replace = TRUE),
                      NA_real_)
  temp_end <- as.Date(temp_end, origin = "1970-01-01")
  temp_hrs <- ifelse(has_temp, sample(8L:22L, n_med, replace = TRUE),
                      NA_integer_)
  temp_redn_end <- temp_end  # same as temp_end for simplicity

  mcd <- data.frame(
    SYNTHETIC_AEUID          = det_ben$SYNTHETIC_AEUID[med_idx],
    PERIOD_START_DATE        = period_start,
    PERIOD_END_DATE          = period_end,
    ASSMT_ID                 = sprintf("A%08d", seq_len(n_med)),
    MED_PRMY_GRP             = med_prmy,
    MED_SCNDRY_GRP           = med_scndry,
    MED_SCNDRY_ID            = med_scndry_id,
    MED_SCNDRY_PERM          = med_scndry_perm,
    IMPRMT_CODE              = imprmt_code,
    IMPRMT_RATE              = imprmt_rate,
    IMPRMT_VER_DATE          = period_start + sample(0L:14L, n_med, replace = TRUE),
    CURR_CAPCTY_NUM          = curr_capcty,
    WITH_INTRVN_NUM          = with_intrvn,
    INCAP_START              = incap_start,
    INCAP_END                = incap_end,
    INCAP_EXEMPT             = sample(c("Y", "N"), n_med, replace = TRUE,
                                       prob = c(0.05, 0.95)),
    INCAP_WK_WRK_HRS        = incap_wk_hrs,
    TEMP_LMT_CAP_START_DATE  = temp_start,
    TEMP_LMT_CAP_END_DATE    = temp_end,
    TEMP_REDN_CAP_HRS_NUM   = temp_hrs,
    TEMP_REDN_CAP_END_DATE   = temp_redn_end,
    MAN_CODE                 = man_code,
    ACTV_PRTCPN_CODE         = actv_prtcpn,
    AMR                      = sprintf("R%06d", sample.int(999999L, n_med, replace = TRUE)),
    RFRL_RSN_CODE            = sample(c("MED", "WCA", "RVW", "OTH"), n_med,
                                       replace = TRUE, prob = c(0.40, 0.30, 0.20, 0.10)),
    CHNL                     = sample(c("ONL", "PHN", "FTF"), n_med,
                                       replace = TRUE, prob = c(0.50, 0.30, 0.20)),
    stringsAsFactors         = FALSE
  )

  ord <- order(mcd$SYNTHETIC_AEUID, mcd$PERIOD_START_DATE)
  mcd <- mcd[ord, ]
  rownames(mcd) <- NULL
  mcd
}

.empty_mcd_dtls <- function() {
  data.frame(
    SYNTHETIC_AEUID          = character(0),
    PERIOD_START_DATE        = as.Date(character(0)),
    PERIOD_END_DATE          = as.Date(character(0)),
    ASSMT_ID                 = character(0),
    MED_PRMY_GRP             = character(0),
    MED_SCNDRY_GRP           = character(0),
    MED_SCNDRY_ID            = character(0),
    MED_SCNDRY_PERM          = character(0),
    IMPRMT_CODE              = integer(0),
    IMPRMT_RATE              = integer(0),
    IMPRMT_VER_DATE          = as.Date(character(0)),
    CURR_CAPCTY_NUM          = integer(0),
    WITH_INTRVN_NUM          = integer(0),
    INCAP_START              = as.Date(character(0)),
    INCAP_END                = as.Date(character(0)),
    INCAP_EXEMPT             = character(0),
    INCAP_WK_WRK_HRS        = integer(0),
    TEMP_LMT_CAP_START_DATE  = as.Date(character(0)),
    TEMP_LMT_CAP_END_DATE    = as.Date(character(0)),
    TEMP_REDN_CAP_HRS_NUM   = integer(0),
    TEMP_REDN_CAP_END_DATE   = as.Date(character(0)),
    MAN_CODE                 = character(0),
    ACTV_PRTCPN_CODE         = character(0),
    AMR                      = character(0),
    RFRL_RSN_CODE            = character(0),
    CHNL                     = character(0),
    stringsAsFactors         = FALSE
  )
}


# =============================================================================
# Step 4: Payment History
# =============================================================================

#' Generate payment history tables (pyh_combined_*)
#'
#' Creates one pyh_combined table per module from benefit spells.
#' All share the same schema. Payment component amounts are based on
#' real 2024 rates deflated for earlier years.
#'
#' @param det_ben data.frame from generate_benefit_spells().
#' @param seed Integer seed.
#' @return Named list of 5 data.frames.
#' @keywords internal
generate_payment_history <- function(det_ben, seed) {
  if (nrow(det_ben) == 0L) {
    empty <- .empty_pyh()
    return(list(
      pyh_combined_dis = empty,
      pyh_combined_wrk = empty,
      pyh_combined_fam = empty,
      pyh_combined_std = empty,
      pyh_combined_age = empty
    ))
  }

  if (exists("generate_payment_history__", mode = "function")) {
    raw <- generate_payment_history__(
      det_ben_aeuid             = as.character(det_ben$SYNTHETIC_AEUID),
      det_ben_type_code         = as.character(det_ben$BEN_TYPE_CODE),
      det_ben_period_start_date = as.character(det_ben$PERIOD_START_DATE),
      det_ben_period_end_date   = as.character(det_ben$PERIOD_END_DATE),
      seed                      = as.integer(seed)
    )
    out <- lapply(raw, function(tbl_raw) {
      tbl <- as.data.frame(tbl_raw, stringsAsFactors = FALSE)
      tbl$PERIOD_START_DATE <- as.Date(tbl$PERIOD_START_DATE)
      tbl$PERIOD_END_DATE <- as.Date(tbl$PERIOD_END_DATE)
      rownames(tbl) <- NULL
      tbl
    })
    return(out)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 903L)

  # Build one payment record per spell with BASIC component
  n <- nrow(det_ben)

  # Compute daily amount from fortnightly rate
  ben_types <- det_ben$BEN_TYPE_CODE
  daily_base <- numeric(n)
  for (bt in names(.DOMINO_RATES_2024)) {
    mask <- ben_types == bt
    if (any(mask)) {
      rate <- .DOMINO_RATES_2024[bt] / 14
      # The stored rates are 2024 rates, so the index is taken relative to 2024
      # rather than to the nominal module's own anchor. Pensions and allowances
      # are indexed differently and the gap compounds; see `.domino_rate_series()`.
      # Keep in step with `rate_series()` in src/rust/src/domino.rs.
      yr <- as.integer(format(det_ben$PERIOD_START_DATE[mask], "%Y"))
      series <- .domino_rate_series(bt)
      deflator <- nominal_index(series, yr, basis = "calendar") /
        nominal_index(series, .DOMINO_RATE_YEAR, basis = "calendar")
      daily_base[mask] <- rate * deflator
    }
  }
  # Any unmatched: use median rate
  daily_base[daily_base == 0] <- 50

  # A rate is the legislated MAXIMUM, so what someone is actually paid can fall
  # below it on the income test or a part period but can never sit above it.
  # The reduction is one-sided for that reason.
  daily_base <- daily_base * (1 - runif(n, 0, 0.10))
  daily_base <- round(daily_base, 2)

  pyh_all <- data.frame(
    SYNTHETIC_AEUID   = det_ben$SYNTHETIC_AEUID,
    BEN_TYPE          = ben_types,
    PERIOD_START_DATE = det_ben$PERIOD_START_DATE,
    PERIOD_END_DATE   = det_ben$PERIOD_END_DATE,
    CMPNT_ID          = sprintf("C%08d", seq_len(n)),
    CMPNT_TYPE        = rep("BASIC", n),
    CMPNT_DLY_AMT     = daily_base,
    PYH_SOURCE        = rep("CLK", n),
    stringsAsFactors  = FALSE
  )

  # Split into module-specific tables
  list(
    pyh_combined_dis = pyh_all[ben_types %in% .DOMINO_MODULE_CODES$dis, ],
    pyh_combined_wrk = pyh_all[ben_types %in% .DOMINO_MODULE_CODES$wrk, ],
    pyh_combined_fam = pyh_all[ben_types %in% .DOMINO_MODULE_CODES$fam, ],
    pyh_combined_std = pyh_all[ben_types %in% .DOMINO_MODULE_CODES$std, ],
    pyh_combined_age = pyh_all[ben_types %in% .DOMINO_MODULE_CODES$age, ]
  )
}

.empty_pyh <- function() {
  data.frame(
    SYNTHETIC_AEUID   = character(0),
    BEN_TYPE          = character(0),
    PERIOD_START_DATE = as.Date(character(0)),
    PERIOD_END_DATE   = as.Date(character(0)),
    CMPNT_ID          = character(0),
    CMPNT_TYPE        = character(0),
    CMPNT_DLY_AMT     = numeric(0),
    PYH_SOURCE        = character(0),
    stringsAsFactors  = FALSE
  )
}


# =============================================================================
# Step 5: Supplementary Tables
# =============================================================================

#' Project DOMINO demographics (static_demogs table)
#' @keywords internal
project_domino_demogs <- function(participants, spine_df, seed) {
  if (nrow(participants) == 0L) return(.empty_static_demogs())

  vital_cols <- c("month_of_birth", "year_of_death", "month_of_death")

  if (exists("project_domino_demogs__", mode = "function") &&
      all(c("birth_year", "sex", "country_of_birth", vital_cols) %in%
            names(spine_df))) {
    raw <- project_domino_demogs__(
      participant_aeuid           = as.character(participants$aeuid),
      participant_spine_idx       = as.integer(participants$spine_idx),
      spine_birth_year            = as.integer(spine_df$birth_year),
      spine_month_of_birth        = as.integer(spine_df$month_of_birth),
      spine_sex                   = as.integer(spine_df$sex),
      # Emit the SACC country code (cross-dataset consistent) as BIRTH_CTRY_CODE.
      spine_country_of_birth      = as.integer(spine_df$country_of_birth_sacc),
      spine_year_of_death         = as.integer(spine_df$year_of_death),
      spine_month_of_death        = as.integer(spine_df$month_of_death),
      seed                        = as.integer(seed)
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    df$PEN_BLIND_START_DATE <- as.Date(df$PEN_BLIND_START_DATE)
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 904L)

  idx <- participants$spine_idx
  n <- length(idx)

  birth_year <- spine_df$birth_year[idx]
  # Birth month comes from the spine, so DOMINO agrees with every other product.
  birth_month <- if ("month_of_birth" %in% names(spine_df)) {
    as.integer(spine_df$month_of_birth)[idx]
  } else {
    rep(NA_integer_, n)
  }
  sex <- spine_df$sex[idx]
  age <- 2024L - birth_year  # age as of extract

  # Country of birth code (from spine)
  cob <- if ("country_of_birth" %in% names(spine_df)) {
    spine_df$country_of_birth[idx]
  } else {
    sample(c("1101", "2100", "3100", "5100", "6100"), n, replace = TRUE,
           prob = c(0.70, 0.10, 0.08, 0.07, 0.05))
  }

  # Language code: mostly English
  lang <- sample(c("1201", "4202", "6513", "9200", "3503"), n, replace = TRUE,
                  prob = c(0.80, 0.05, 0.05, 0.05, 0.05))

  # Death indicator: taken from the spine, flagged only once the spine death
  # year has arrived by the DOMINO reference year.
  spine_yod <- if ("year_of_death" %in% names(spine_df)) {
    as.integer(spine_df$year_of_death)[idx]
  } else {
    rep(NA_integer_, n)
  }
  spine_mod <- if ("month_of_death" %in% names(spine_df)) {
    as.integer(spine_df$month_of_death)[idx]
  } else {
    rep(NA_integer_, n)
  }
  is_dead <- !is.na(spine_yod) & spine_yod <= 2024L
  death_year  <- ifelse(is_dead, spine_yod, NA_integer_)
  death_month <- ifelse(is_dead, spine_mod, NA_integer_)

  demogs <- data.frame(
    SYNTHETIC_AEUID   = participants$aeuid,
    YEAR_OF_BIRTH     = birth_year,
    MONTH_OF_BIRTH    = birth_month,
    GENDER            = ifelse(sex == 1L, "M", "F"),
    BIRTH_CTRY_CODE   = cob,
    LANG_CODE         = lang,
    INTERPRETER_IND   = sample(c("Y", "N"), n, replace = TRUE,
                                prob = c(0.02, 0.98)),
    AGE               = age,
    DEATH_IND         = ifelse(is_dead, "Y", "N"),
    YEAR_OF_DEATH     = death_year,
    MONTH_OF_DEATH    = death_month,
    PEN_BLIND_START_DATE = as.Date(NA),
    OBJECT_TYPE_CODE  = rep("PER", n),
    stringsAsFactors  = FALSE
  )

  rownames(demogs) <- NULL
  demogs
}

.empty_static_demogs <- function() {
  data.frame(
    SYNTHETIC_AEUID      = character(0),
    YEAR_OF_BIRTH        = integer(0),
    MONTH_OF_BIRTH       = integer(0),
    GENDER               = character(0),
    BIRTH_CTRY_CODE      = character(0),
    LANG_CODE            = character(0),
    INTERPRETER_IND      = character(0),
    AGE                  = integer(0),
    DEATH_IND            = character(0),
    YEAR_OF_DEATH        = integer(0),
    MONTH_OF_DEATH       = integer(0),
    PEN_BLIND_START_DATE = as.Date(character(0)),
    OBJECT_TYPE_CODE     = character(0),
    stringsAsFactors     = FALSE
  )
}


#' Project DOMINO employment income (inc_emp_cont table)
#' @keywords internal
project_domino_income <- function(participants, spine_df, seed, yr_range) {
  if (nrow(participants) == 0L) return(.empty_inc_emp_cont())

  if (exists("project_domino_income__", mode = "function") &&
      "baseline_income" %in% names(spine_df)) {
    raw <- project_domino_income__(
      participant_aeuid         = as.character(participants$aeuid),
      participant_spine_idx     = as.integer(participants$spine_idx),
      participant_first_year    = as.integer(participants$first_year),
      participant_last_year     = as.integer(participants$last_year),
      spine_baseline_income     = as.double(spine_df$baseline_income),
      seed                      = as.integer(seed)
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    df$PERIOD_START_DATE <- as.Date(df$PERIOD_START_DATE)
    df$PERIOD_END_DATE   <- as.Date(df$PERIOD_END_DATE)
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 904L + 1L)

  # Only participants with some employment (income > 15000) report income
  idx <- participants$spine_idx
  income <- spine_df$baseline_income[idx]
  has_income <- income > 15000
  inc_idx <- which(has_income)

  if (length(inc_idx) == 0L) return(.empty_inc_emp_cont())

  n_inc <- length(inc_idx)
  aeuid <- participants$aeuid[inc_idx]

  # Spell dates: one income spell per person covering their participation
  start_yr <- participants$first_year[inc_idx]
  end_yr   <- participants$last_year[inc_idx]

  # `baseline_income` is a calendar-2021 amount, and the spell starts in
  # `start_yr`, so the reported income is moved to that year. Keep in step with
  # `project_domino_income__` in src/rust/src/domino.rs.
  #
  # Hours stay on the unindexed amount. They are a proxy computed by dividing
  # income by a fixed dollar rate, so indexing the income first would report a
  # participant working more hours every year for the same job.
  hours <- pmin(40L, pmax(0L, as.integer(round(income[inc_idx] / 2500))))
  paid <- income[inc_idx] * .nominal_unit_factor(
    "wage", start_yr,
    unit = .nominal_unit_key(aeuid), seed = seed,
    dispersion = .NOMINAL_PERSON_DISPERSION, basis = "calendar"
  )
  daily_inc <- round(paid / 365, 2)

  inc <- data.frame(
    SYNTHETIC_AEUID   = aeuid,
    PERIOD_START_DATE = as.Date(sprintf("%04d-01-01", start_yr)),
    PERIOD_END_DATE   = as.Date(sprintf("%04d-12-31", end_yr)),
    EMPLYR_ID         = sprintf("E%08d", sample.int(99999999L, n_inc, replace = TRUE)),
    EMP_INC_CONT      = round(daily_inc * 14, 2),  # fortnightly
    EMP_DLY_INC_CONT  = daily_inc,
    EMP_INC_CONT_HRS  = hours,
    FREQ_CODE         = rep("FTN", n_inc),  # fortnightly
    AVG_IND           = sample(c("Y", "N"), n_inc, replace = TRUE,
                                prob = c(0.10, 0.90)),
    stringsAsFactors  = FALSE
  )

  rownames(inc) <- NULL
  inc
}

.empty_inc_emp_cont <- function() {
  data.frame(
    SYNTHETIC_AEUID   = character(0),
    PERIOD_START_DATE = as.Date(character(0)),
    PERIOD_END_DATE   = as.Date(character(0)),
    EMPLYR_ID         = character(0),
    EMP_INC_CONT      = numeric(0),
    EMP_DLY_INC_CONT  = numeric(0),
    EMP_INC_CONT_HRS  = integer(0),
    FREQ_CODE         = character(0),
    AVG_IND           = character(0),
    stringsAsFactors  = FALSE
  )
}


#' Project DOMINO location details (loc_dtls table)
#' @keywords internal
project_domino_locations <- function(participants, spine_df, seed, yr_range) {
  if (nrow(participants) == 0L) return(.empty_loc_dtls())

  if (exists("project_domino_locations__", mode = "function") &&
      "state" %in% names(spine_df)) {
    raw <- project_domino_locations__(
      participant_aeuid         = as.character(participants$aeuid),
      participant_spine_idx     = as.integer(participants$spine_idx),
      participant_first_year    = as.integer(participants$first_year),
      participant_last_year     = as.integer(participants$last_year),
      spine_state               = as.integer(spine_df$state),
      seed                      = as.integer(seed)
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    df$PERIOD_START_DATE <- as.Date(df$PERIOD_START_DATE)
    df$PERIOD_END_DATE   <- as.Date(df$PERIOD_END_DATE)
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  # +908: locations previously shared +905 with the income projector.
  set.seed(seed + 908L)

  idx <- participants$spine_idx
  n <- length(idx)
  state <- spine_df$state[idx]

  # State code mapping
  state_codes <- c("1" = "NSW", "2" = "VIC", "3" = "QLD", "4" = "SA",
                    "5" = "WA", "6" = "TAS", "7" = "NT", "8" = "ACT")
  state_names <- state_codes[as.character(state)]
  state_names[is.na(state_names)] <- "NSW"

  # Postcode: realistic ranges by state
  postcode <- ifelse(state == 1L, sprintf("%04d", sample(2000:2999, n, replace = TRUE)),
              ifelse(state == 2L, sprintf("%04d", sample(3000:3999, n, replace = TRUE)),
              ifelse(state == 3L, sprintf("%04d", sample(4000:4999, n, replace = TRUE)),
              ifelse(state == 4L, sprintf("%04d", sample(5000:5799, n, replace = TRUE)),
              ifelse(state == 5L, sprintf("%04d", sample(6000:6999, n, replace = TRUE)),
              ifelse(state == 6L, sprintf("%04d", sample(7000:7999, n, replace = TRUE)),
              ifelse(state == 7L, sprintf("%04d", sample(800:899, n, replace = TRUE)),
                                  sprintf("%04d", sample(2600:2619, n, replace = TRUE)))))))))

  # SA1/SA2: synthetic codes (built as strings to avoid integer overflow)
  sa1 <- paste0(state, sprintf("%010d", sample.int(9999999L, n, replace = TRUE)))
  sa2 <- paste0(state, sprintf("%08d", sample.int(9999999L, n, replace = TRUE)))

  loc <- data.frame(
    SYNTHETIC_AEUID   = participants$aeuid,
    PERIOD_START_DATE = as.Date(sprintf("%04d-01-01", participants$first_year)),
    PERIOD_END_DATE   = as.Date(sprintf("%04d-12-31", participants$last_year)),
    ADDR_TYPE_CODE    = rep("RES", n),
    ADDR_PRTY         = rep(1L, n),
    STATE             = state_names,
    POSTCODE          = postcode,
    CTRY_CODE         = rep("1101", n),  # Australia
    CMTY_CODE         = rep(NA_character_, n),
    RMT_IND           = sample(c("Y", "N"), n, replace = TRUE,
                                prob = c(0.15, 0.85)),
    MESHBLOCK         = sprintf("%011d", sample.int(99999999L, n, replace = TRUE)),
    MESHMATCH         = sample(c("ADDR", "COORDS"), n, replace = TRUE,
                                prob = c(0.90, 0.10)),
    SA1_MAINCODE      = sa1,
    SA2_MAINCODE      = sa2,
    stringsAsFactors  = FALSE
  )

  rownames(loc) <- NULL
  loc
}

.empty_loc_dtls <- function() {
  data.frame(
    SYNTHETIC_AEUID   = character(0),
    PERIOD_START_DATE = as.Date(character(0)),
    PERIOD_END_DATE   = as.Date(character(0)),
    ADDR_TYPE_CODE    = character(0),
    ADDR_PRTY         = integer(0),
    STATE             = character(0),
    POSTCODE          = character(0),
    CTRY_CODE         = character(0),
    CMTY_CODE         = character(0),
    RMT_IND           = character(0),
    MESHBLOCK         = character(0),
    MESHMATCH         = character(0),
    SA1_MAINCODE      = character(0),
    SA2_MAINCODE      = character(0),
    stringsAsFactors  = FALSE
  )
}


#' Project DOMINO education level (edu_lvl table)
#' @keywords internal
project_domino_education <- function(participants, spine_df, seed, yr_range) {
  if (nrow(participants) == 0L) return(.empty_edu_lvl())

  if (exists("project_domino_education__", mode = "function") &&
      "education" %in% names(spine_df)) {
    raw <- project_domino_education__(
      participant_aeuid         = as.character(participants$aeuid),
      participant_spine_idx     = as.integer(participants$spine_idx),
      participant_first_year    = as.integer(participants$first_year),
      participant_last_year     = as.integer(participants$last_year),
      spine_education           = as.integer(spine_df$education),
      seed                      = as.integer(seed)
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    df$PERIOD_START_DATE <- as.Date(df$PERIOD_START_DATE)
    df$PERIOD_END_DATE   <- as.Date(df$PERIOD_END_DATE)
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 906L)

  idx <- participants$spine_idx
  n <- length(idx)
  edu <- spine_df$education[idx]

  # Map spine education to Centrelink education codes
  edu_codes <- c("0" = "00", "1" = "01", "2" = "02",
                  "3" = "03", "4" = "04", "5" = "05")
  lvl <- edu_codes[as.character(edu)]
  lvl[is.na(lvl)] <- "00"

  edu_descs <- c("00" = "Not stated", "01" = "Below Year 12",
                  "02" = "Year 12", "03" = "Certificate III/IV",
                  "04" = "Diploma/Advanced Diploma", "05" = "Bachelor or higher")

  # Student status
  stdnt_sts <- sample(c("FTS", "PTS", "NST"), n, replace = TRUE,
                       prob = c(0.05, 0.03, 0.92))

  edu_tbl <- data.frame(
    SYNTHETIC_AEUID   = participants$aeuid,
    PERIOD_START_DATE = as.Date(sprintf("%04d-01-01", participants$first_year)),
    PERIOD_END_DATE   = as.Date(sprintf("%04d-12-31", participants$last_year)),
    LVL_ATTAINED      = lvl,
    LVL_ATTAINED_DESC = edu_descs[lvl],
    STDNT_STS_CODE    = stdnt_sts,
    stringsAsFactors  = FALSE
  )

  rownames(edu_tbl) <- NULL
  edu_tbl
}

.empty_edu_lvl <- function() {
  data.frame(
    SYNTHETIC_AEUID   = character(0),
    PERIOD_START_DATE = as.Date(character(0)),
    PERIOD_END_DATE   = as.Date(character(0)),
    LVL_ATTAINED      = character(0),
    LVL_ATTAINED_DESC = character(0),
    STDNT_STS_CODE    = character(0),
    stringsAsFactors  = FALSE
  )
}


#' Project DOMINO housing details (hh_hse_dtls table)
#' @keywords internal
project_domino_housing <- function(participants, spine_df, seed, yr_range) {
  if (nrow(participants) == 0L) return(.empty_hh_hse_dtls())

  if (exists("project_domino_housing__", mode = "function") &&
      "baseline_income" %in% names(spine_df)) {
    raw <- project_domino_housing__(
      participant_aeuid         = as.character(participants$aeuid),
      participant_spine_idx     = as.integer(participants$spine_idx),
      participant_first_year    = as.integer(participants$first_year),
      participant_last_year     = as.integer(participants$last_year),
      spine_baseline_income     = as.double(spine_df$baseline_income),
      seed                      = as.integer(seed)
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    df$PERIOD_START_DATE <- as.Date(df$PERIOD_START_DATE)
    df$PERIOD_END_DATE   <- as.Date(df$PERIOD_END_DATE)
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 906L + 1L)

  idx <- participants$spine_idx
  n <- length(idx)
  income <- spine_df$baseline_income[idx]

  # Accommodation type
  accom_codes <- c("HOS", "FLT", "BRD", "OTH")
  accom <- sample(accom_codes, n, replace = TRUE,
                   prob = c(0.60, 0.20, 0.10, 0.10))

  # Home ownership: lower income → more likely renting
  ho_codes <- c("OWN", "MRG", "PRV", "GOV", "OTH")
  ho_probs_low  <- c(0.10, 0.10, 0.45, 0.25, 0.10)
  ho_probs_high <- c(0.35, 0.30, 0.25, 0.05, 0.05)
  is_low <- income < 30000
  ho <- character(n)
  if (any(is_low)) {
    ho[is_low] <- sample(ho_codes, sum(is_low), replace = TRUE,
                          prob = ho_probs_low)
  }
  if (any(!is_low)) {
    ho[!is_low] <- sample(ho_codes, sum(!is_low), replace = TRUE,
                           prob = ho_probs_high)
  }

  # Rent type
  rent_type <- ifelse(ho %in% c("PRV", "GOV"), ho, "NRP")

  # Weekly rent. The $100-$500 range is an anchor-year range and rent is a
  # price, so it is moved to the year the spell starts in. Keep in step with
  # `project_domino_housing__` in src/rust/src/domino.rs.
  rent_factor <- .nominal_unit_factor(
    "price", participants$first_year,
    unit = .nominal_unit_key(participants$aeuid), seed = seed,
    dispersion = .NOMINAL_PERSON_DISPERSION, basis = "calendar"
  )
  wk_rent <- ifelse(ho %in% c("PRV", "GOV"),
                     round(runif(n, 100, 500) * rent_factor, 0),
                     0)

  hse <- data.frame(
    SYNTHETIC_AEUID   = participants$aeuid,
    PERIOD_START_DATE = as.Date(sprintf("%04d-01-01", participants$first_year)),
    PERIOD_END_DATE   = as.Date(sprintf("%04d-12-31", participants$last_year)),
    HSE_ACCOM_CODE    = accom,
    HSE_HO_CODE       = ho,
    HSE_RENT_TYPE     = rent_type,
    HSE_WK_RENT       = wk_rent,
    stringsAsFactors  = FALSE
  )

  rownames(hse) <- NULL
  hse
}

.empty_hh_hse_dtls <- function() {
  data.frame(
    SYNTHETIC_AEUID   = character(0),
    PERIOD_START_DATE = as.Date(character(0)),
    PERIOD_END_DATE   = as.Date(character(0)),
    HSE_ACCOM_CODE    = character(0),
    HSE_HO_CODE       = character(0),
    HSE_RENT_TYPE     = character(0),
    HSE_WK_RENT       = numeric(0),
    stringsAsFactors  = FALSE
  )
}


#' Project DOMINO Indigenous status (indg_stat table)
#' @keywords internal
project_domino_indigenous <- function(participants, spine_df) {
  if (nrow(participants) == 0L) {
    return(data.frame(
      SYNTHETIC_AEUID = character(0),
      INDIG_CODE      = character(0),
      stringsAsFactors = FALSE
    ))
  }

  if (exists("project_domino_indigenous__", mode = "function") &&
      "indigenous" %in% names(spine_df)) {
    raw <- project_domino_indigenous__(
      participant_aeuid         = as.character(participants$aeuid),
      participant_spine_idx     = as.integer(participants$spine_idx),
      spine_indigenous          = as.integer(spine_df$indigenous)
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }

  idx <- participants$spine_idx
  indig <- spine_df$indigenous[idx]

  # Spine code 1 is non-Indigenous; codes 2 to 4 are Indigenous.
  indig_code <- ifelse(indig %in% 2:4, "Y", "N")

  data.frame(
    SYNTHETIC_AEUID = participants$aeuid,
    INDIG_CODE      = indig_code,
    stringsAsFactors = FALSE
  )
}


#' Project DOMINO principal carer relationships (rlt_prin_carer_beta table)
#' @keywords internal
project_domino_principal_carer <- function(det_ben, spine_df, seed) {
  if (nrow(det_ben) == 0L) return(.empty_rlt_prin_carer())

  if (exists("project_domino_principal_carer__", mode = "function")) {
    raw <- project_domino_principal_carer__(
      det_ben_aeuid             = as.character(det_ben$SYNTHETIC_AEUID),
      det_ben_type_code         = as.character(det_ben$BEN_TYPE_CODE),
      det_ben_period_start_date = as.character(det_ben$PERIOD_START_DATE),
      det_ben_period_end_date   = as.character(det_ben$PERIOD_END_DATE),
      seed                      = as.integer(seed)
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    df$PERIOD_START_DATE <- as.Date(df$PERIOD_START_DATE)
    df$PERIOD_END_DATE   <- as.Date(df$PERIOD_END_DATE)
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  # +909: principal carer previously shared +907 with the housing projector.
  set.seed(seed + 909L)

  # Principal carer: PPP/PPS/CAR recipients
  carer_idx <- which(det_ben$BEN_TYPE_CODE %in% c("PPP", "PPS", "CAR"))
  if (length(carer_idx) == 0L) return(.empty_rlt_prin_carer())

  n_car <- length(carer_idx)

  rlt <- data.frame(
    SYNTHETIC_AEUID   = det_ben$SYNTHETIC_AEUID[carer_idx],
    BEN_TYPE_CODE     = det_ben$BEN_TYPE_CODE[carer_idx],
    PERIOD_START_DATE = det_ben$PERIOD_START_DATE[carer_idx],
    PERIOD_END_DATE   = det_ben$PERIOD_END_DATE[carer_idx],
    EXT_DOM_REL_ID    = sprintf("R%012d", sample.int(999999999L, n_car, replace = TRUE)),
    stringsAsFactors  = FALSE
  )

  rownames(rlt) <- NULL
  rlt
}

.empty_rlt_prin_carer <- function() {
  data.frame(
    SYNTHETIC_AEUID   = character(0),
    BEN_TYPE_CODE     = character(0),
    PERIOD_START_DATE = as.Date(character(0)),
    PERIOD_END_DATE   = as.Date(character(0)),
    EXT_DOM_REL_ID    = character(0),
    stringsAsFactors  = FALSE
  )
}

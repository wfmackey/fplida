#' Generate TVA dataset (Total VET Activity)
#'
#' Projects VET training activity and program completion records from the
#' fplida spine, matching the real PLIDA TVA data structure. Two core
#' product types per year: training activity (enrolment-level) and
#' program completions.
#'
#' @section Dataset and variable information:
#' The [NCVER Total VET collection](https://www.ncver.edu.au/research-and-statistics/collections/students-and-courses-collection/total-vet-students-and-courses)
#' website gives information about this dataset. Use `dataset_info("TVA")` for
#' dataset information. Use `variable_info("TVA")` for variables, sources,
#' value support, and topic tags.
#'
#' @param spine Data.frame (from \code{generate_spine()}) or NULL. If NULL,
#'   the most recent spine is loaded from the run directory.
#' @param seed Integer. Random seed for TVA-specific generation.
#' @param years Integer vector. Reporting years to generate (default 2015:2023).
#' @param output_dir Character or NULL. Base output directory.
#' @param format Character. Output format: "parquet" (default) or "csv".
#' @param return_data Logical. If TRUE, return data.frames in memory;
#'   if FALSE (default), stream to disk and return metadata only.
#'
#' @return If \code{return_data = TRUE}, a named list with two elements
#'   (\code{training_activity}, \code{completions}), each a named list of
#'   data.frames keyed by year. If \code{return_data = FALSE}, a metadata
#'   summary with record counts, years, and output path.
#'
#' @examples
#' \dontrun{
#' spine <- generate_spine(n = 1000L, seed = 1L)
#' tva <- generate_tva(spine = spine, seed = 1L, years = 2020L:2023L,
#'                     return_data = TRUE)
#' str(tva$training_activity[["2020"]])
#' str(tva$completions[["2020"]])
#' }
#'
#' @export
generate_tva <- function(spine = NULL, seed = 42L, years = 2015L:2023L,
                         output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = FALSE) {
  seed  <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  return_data <- as.logical(return_data)
  stopifnot(
    "`seed` must be an integer"  = !is.na(seed),
    "`years` must be integer"    = all(!is.na(years))
  )

  run_dir <- resolve_run_dir(output_dir)

  # ---- Selective spine loading (memory-efficient) ----
  tva_cols <- c("spine_id", "aeuid_ncver", "birth_year", "sex", "state",
                "education", "archetype", "anzsco_code",
                "disability_onset_year", "is_dc")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, tva_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))

  # Mini spine for agency spine writing (spine_id + aeuid_ncver only)
  mini_spine <- data.frame(
    spine_id    = spine$spine_id,
    aeuid_ncver = spine$aeuid_ncver,
    stringsAsFactors = FALSE
  )

  yr_range <- range(years)

  # Step 1: Select VET participants and build enrolment spells
  spells <- select_tva_participants(spine, seed, yr_range)

  # Free spine — spells captures everything needed downstream
  if (spine_loaded && !return_data) { rm(spine); gc() }

  # Step 2+3: Stream training activity year-by-year (vectorized)
  ta_result <- .stream_tva_activity(spells, seed, years, run_dir, format,
                                    return_data)

  # Step 4: Completions (vectorized) + year-streaming write
  # Note: project_tva_completions only uses spells, not spine_df
  comp_result <- .stream_tva_completions(spells, NULL, seed, years, run_dir,
                                         format, return_data)

  # Write NCVER agency spine
  ds_dir <- dataset_dir(run_dir, "TVA")
  write_agency_spine(mini_spine, "NCVER", ds_dir, format = format)

  if (return_data) {
    list(
      training_activity = ta_result,
      completions       = comp_result
    )
  } else {
    list(
      training_activity = list(
        n_records = ta_result$total_records,
        years     = years
      ),
      completions = list(
        n_records = comp_result$total_records,
        years     = years
      ),
      path = "ncver-tva"
    )
  }
}


# ===========================================================================
# Constants
# ===========================================================================

# Participation rate by spine education code (0..5).
.TVA_PARTICIPATION_RATE <- c(
  0.00,  # 0 = under 15
  0.15,  # 1 = below year 12
  0.30,  # 2 = year 12
  0.50,  # 3 = cert III-IV
  0.25,  # 4 = diploma
  0.05   # 5 = bachelor+
)

# Age modifiers for VET participation.
.TVA_AGE_BOOST_YOUNG <- 1.3
.TVA_AGE_BOOST_MID   <- 1.0
.TVA_AGE_PEN_OLD     <- 0.6

# Qualification level codes (PROGRAM_LOE_ID).
.TVA_QUAL_CODES  <- c(511L, 514L, 521L, 524L, 611L, 613L)
.TVA_QUAL_LABELS <- c("Certificate I", "Certificate II", "Certificate III",
                       "Certificate IV", "Diploma", "Advanced Diploma")

# Unconditional shares.
.TVA_QUAL_SHARES <- c(0.08, 0.18, 0.35, 0.18, 0.15, 0.06)

# Shares conditional on spine education (list indexed by education + 1).
.TVA_QUAL_SHARES_BY_EDU <- list(
  c(0.08, 0.18, 0.35, 0.18, 0.15, 0.06),  # edu 0 (fallback)
  c(0.20, 0.35, 0.30, 0.10, 0.04, 0.01),  # edu 1
  c(0.10, 0.25, 0.35, 0.18, 0.08, 0.04),  # edu 2
  c(0.05, 0.10, 0.40, 0.25, 0.14, 0.06),  # edu 3
  c(0.02, 0.05, 0.15, 0.25, 0.38, 0.15),  # edu 4
  c(0.02, 0.05, 0.15, 0.20, 0.40, 0.18)   # edu 5
)

# Duration (FT years) by qualification level.
.TVA_FT_YEARS       <- c(0.5, 0.5, 1.0, 1.0, 2.0, 2.0)
.TVA_PT_MULTIPLIER  <- 1.5
.TVA_FT_SHARE       <- 0.45

# Completion rates by qualification level.
.TVA_COMPLETION_RATES <- c(0.40, 0.42, 0.47, 0.50, 0.55, 0.58)

# FOE codes (ASCED 2-digit) and shares.
.TVA_FOE_CODES  <- c("03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
.TVA_FOE_SHARES <- c(0.12, 0.08, 0.04, 0.12, 0.05, 0.20, 0.12, 0.04, 0.13, 0.10)

# Archetype-to-FOE affinity (8 x 10).
.TVA_ARCHETYPE_FOE <- matrix(c(
  0.15, 0.10, 0.10, 0.02, 0.02, 0.10, 0.08, 0.03, 0.20, 0.20,
  0.30, 0.25, 0.05, 0.02, 0.02, 0.05, 0.03, 0.03, 0.10, 0.15,
  0.02, 0.02, 0.02, 0.55, 0.10, 0.05, 0.10, 0.02, 0.07, 0.05,
  0.03, 0.03, 0.02, 0.05, 0.05, 0.40, 0.15, 0.05, 0.07, 0.15,
  0.10, 0.05, 0.03, 0.08, 0.10, 0.25, 0.15, 0.08, 0.06, 0.10,
  0.05, 0.05, 0.03, 0.05, 0.08, 0.35, 0.15, 0.04, 0.08, 0.12,
  0.25, 0.10, 0.03, 0.05, 0.05, 0.15, 0.10, 0.07, 0.05, 0.15,
  0.05, 0.03, 0.02, 0.08, 0.05, 0.15, 0.15, 0.07, 0.30, 0.10
), nrow = 8, ncol = 10, byrow = TRUE)

# Provider type codes and shares.
.TVA_PROV_CODES  <- c(21L, 31L, 61L, 91L, 95L, 97L)
.TVA_PROV_SHARES <- c(0.30, 0.02, 0.08, 0.05, 0.50, 0.05)

# Delivery mode codes and shares.
.TVA_DELIV_CODES  <- c(10L, 20L, 30L, 40L, 90L)
.TVA_DELIV_SHARES <- c(0.45, 0.15, 0.25, 0.10, 0.05)

# Funding source codes and shares.
.TVA_FUND_CODES  <- c(11L, 20L, 30L, 31L, 80L, 99L)
.TVA_FUND_SHARES <- c(0.12, 0.40, 0.28, 0.03, 0.12, 0.05)

# Subject outcome codes and shares.
.TVA_OUTCOME_CODES  <- c(20L, 30L, 40L, 51L, 60L, 70L, 81L, 90L)
.TVA_OUTCOME_SHARES <- c(0.55, 0.05, 0.10, 0.08, 0.05, 0.10, 0.04, 0.03)

# State -> postcode range mapping.
.TVA_STATE_PC <- list(
  "1" = c(2000L, 2999L), "2" = c(3000L, 3999L),
  "3" = c(4000L, 4999L), "4" = c(5000L, 5799L),
  "5" = c(6000L, 6797L), "6" = c(7000L, 7999L),
  "7" = c(0800L, 0899L), "8" = c(2600L, 2618L)
)

# Remoteness codes.
.TVA_REMOTE_CODES  <- c(0L, 1L, 2L, 3L, 4L)
.TVA_REMOTE_SHARES <- c(0.45, 0.25, 0.18, 0.08, 0.04)

# SEIFA IRSD quintiles.
.TVA_SEIFA_CODES  <- 1L:5L
.TVA_SEIFA_SHARES <- c(0.22, 0.21, 0.20, 0.19, 0.18)

# Labour force status codes.
.TVA_LFS_CODES  <- c(1L, 2L, 3L, 4L)
.TVA_LFS_SHARES <- c(0.45, 0.25, 0.15, 0.15)

# Highest school level codes.
.TVA_SCHOOL_CODES  <- c(8L, 9L, 10L, 11L, 12L)
.TVA_SCHOOL_SHARES <- c(0.02, 0.05, 0.15, 0.18, 0.60)

# Study reason codes.
.TVA_STUDY_REASON_CODES  <- c(1L, 2L, 4L, 7L, 8L, 11L, 12L)
.TVA_STUDY_REASON_SHARES <- c(0.40, 0.20, 0.10, 0.08, 0.07, 0.10, 0.05)

# Nominal hours per subject (typical range).
.TVA_SUBJECT_HOURS_MEAN <- 40
.TVA_SUBJECT_HOURS_SD   <- 20

# Subjects per student-year.
.TVA_SUBJECTS_FT <- 8L
.TVA_SUBJECTS_PT <- 4L

# Education -> highest_ed_lvl_st mapping (indexed by edu + 1).
.TVA_EDU_TO_HED <- c(0L, 1L, 2L, 4L, 5L, 6L)


# ===========================================================================
# Empty data.frame helpers
# ===========================================================================

.empty_tva_activity <- function() {
  data.frame(
    SYNTHETIC_AEUID                = character(0),
    TVA_ROW_ID                     = character(0),
    COLLECTION_YR                  = integer(0),
    AGE                            = integer(0),
    YEAR_OF_BIRTH                  = integer(0),
    MONTH_OF_BIRTH                 = integer(0),
    GENDER_ID                      = integer(0),
    HIGHEST_SCHL_LVL_ST            = integer(0),
    HIGHEST_ED_LVL_ST              = integer(0),
    PRIOR_ED_ACHIEVE_FG            = character(0),
    AT_SCHOOL_FG                   = character(0),
    LABOUR_FORCE_STATUS_ID         = integer(0),
    ANZSCO_ID                      = character(0),
    PROGRAM_ID                     = character(0),
    PROGRAM_FOE_ID                 = character(0),
    PROGRAM_LOE_ID                 = integer(0),
    PROGRAM_RECOGNITION_ID         = integer(0),
    PROGRAM_TRAINING_PACKAGE_ID    = character(0),
    PROGRAM_TYPE_OF_TRAINING_ID    = integer(0),
    PROGRAM_NOMINAL_HOURS          = integer(0),
    PROGRAM_VET_FG                 = character(0),
    SUBJECT_ID                     = character(0),
    SUBJECT_FOE_ID                 = character(0),
    SUBJECT_NOMINAL_HOURS          = integer(0),
    SUBJECT_VET_FG                 = character(0),
    SUBJECT_FG                     = character(0),
    APPRENTICESHIP_FG              = character(0),
    VET_IN_SCHOOLS_FG              = character(0),
    STUDENT_COMMENCING_FG          = character(0),
    STUDY_REASON_ID                = integer(0),
    ACTIVITY_START_DATE            = as.Date(character(0)),
    ACTIVITY_END_DATE              = as.Date(character(0)),
    DELIVERY_MODE_ID               = integer(0),
    NATIONAL_FUNDING_SOURCE_ID     = integer(0),
    NATIONAL_OUTCOME_ID            = integer(0),
    CLIENT_POSTCODE_DERIVED        = character(0),
    CLIENT_STATE_RESIDENCE_DERIVED = integer(0),
    CLIENT_REMOTENESS_ID_DERIVED   = integer(0),
    CLIENT_SEIFA_IRSD_QUINT_DRVD   = integer(0),
    RTO_ID                         = character(0),
    TRAIN_ORG_TYPE_ID              = integer(0),
    HEAD_OFFICE_STATE              = integer(0),
    HEAD_OFFICE_POSTCODE           = character(0),
    STATE_OF_FUNDING_GF            = integer(0),
    stringsAsFactors = FALSE
  )
}

.empty_tva_completions <- function() {
  data.frame(
    SYNTHETIC_AEUID                = character(0),
    TVA_ROW_ID                     = character(0),
    COLLECTION_YR                  = integer(0),
    AGE                            = integer(0),
    YEAR_OF_BIRTH                  = integer(0),
    MONTH_OF_BIRTH                 = integer(0),
    GENDER_ID                      = integer(0),
    HIGHEST_SCHL_LVL_ST            = integer(0),
    HIGHEST_ED_LVL_ST              = integer(0),
    PRIOR_ED_ACHIEVE_FG            = character(0),
    AT_SCHOOL_FG                   = character(0),
    LABOUR_FORCE_STATUS_ID         = integer(0),
    ANZSCO_ID                      = character(0),
    PROGRAM_ID                     = character(0),
    PROGRAM_FOE_ID                 = character(0),
    PROGRAM_LOE_ID                 = integer(0),
    PROGRAM_RECOGNITION_ID         = integer(0),
    PROGRAM_TRAINING_PACKAGE_ID    = character(0),
    PROGRAM_TYPE_OF_TRAINING_ID    = integer(0),
    DATE_PROGRAM_COMPLETED         = as.Date(character(0)),
    YR_PROGRAM_COMPLETED           = integer(0),
    CLIENT_POSTCODE_DERIVED        = character(0),
    CLIENT_STATE_RESIDENCE_DERIVED = integer(0),
    CLIENT_REMOTENESS_ID_DERIVED   = integer(0),
    CLIENT_SEIFA_IRSD_QUINT_DRVD   = integer(0),
    RTO_ID                         = character(0),
    TRAIN_ORG_TYPE_ID              = integer(0),
    HEAD_OFFICE_STATE              = integer(0),
    HEAD_OFFICE_POSTCODE           = character(0),
    STATE_OF_FUNDING_GF            = integer(0),
    stringsAsFactors = FALSE
  )
}


# ===========================================================================
# Step 1: Participant selection (enrolment spells) — vectorized
# ===========================================================================

#' Select VET participants and assign program attributes
#'
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @param yr_range Integer vector of length 2.
#' @return data.frame of enrolment spell metadata.
#' @keywords internal
select_tva_participants <- function(spine_df, seed, yr_range) {
  n <- nrow(spine_df)
  min_yr <- yr_range[1L]
  max_yr <- yr_range[2L]

  # Try Rust implementation first
  if (exists("select_tva_participants__", mode = "function")) {
    anzsco <- if ("anzsco_code" %in% names(spine_df)) {
      as.character(spine_df$anzsco_code)
    } else {
      rep("000000", n)
    }
    raw <- select_tva_participants__(
      birth_year   = as.integer(spine_df$birth_year),
      education    = as.integer(spine_df$education),
      archetype    = as.integer(spine_df$archetype),
      aeuid_ncver  = as.character(spine_df$aeuid_ncver),
      anzsco_code  = anzsco,
      sex          = as.integer(spine_df$sex),
      state        = as.integer(spine_df$state),
      seed         = as.integer(seed),
      min_year     = min_yr,
      max_year     = max_yr
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    df$is_ft <- as.logical(df$is_ft)
    df$completed <- as.logical(df$completed)
    spells <- df
  } else {

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 1200L)

  birth_year <- spine_df$birth_year
  education  <- spine_df$education
  archetype  <- spine_df$archetype
  mid_yr <- as.integer(round(mean(yr_range)))

  # Base participation probability from education level
  p_base <- .TVA_PARTICIPATION_RATE[pmin(education + 1L, 6L)]

  # Age modifier
  age_mid <- mid_yr - birth_year
  age_mod <- ifelse(age_mid < 25L, .TVA_AGE_BOOST_YOUNG,
             ifelse(age_mid < 45L, .TVA_AGE_BOOST_MID,
                                   .TVA_AGE_PEN_OLD))
  p_ever <- pmin(p_base * age_mod, 1.0)

  draws <- runif(n)
  is_participant <- draws < p_ever

  # Must be alive and >= 15 at some point in the window
  is_participant <- is_participant & (birth_year + 15L <= max_yr)

  idx <- which(is_participant)
  if (length(idx) == 0L) {
    return(data.frame(
      spine_idx = integer(0), aeuid = character(0),
      birth_year = integer(0), sex = integer(0), state = integer(0),
      education = integer(0), archetype = integer(0),
      anzsco = character(0),
      qual_idx = integer(0), foe = character(0),
      commence_year = integer(0), duration_yrs = numeric(0),
      is_ft = logical(0), completed = logical(0),
      completion_year = integer(0),
      prov_type = integer(0), rto_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  n_part <- length(idx)

  # Qualification assignment — group by education level (vectorized)
  edu_vals <- education[idx]
  qual_idx <- integer(n_part)
  for (ed in 0L:5L) {
    mask <- edu_vals == ed
    n_mask <- sum(mask)
    if (n_mask > 0L) {
      ed_key <- min(ed + 1L, 6L)
      shares <- .TVA_QUAL_SHARES_BY_EDU[[ed_key]]
      qual_idx[mask] <- sample.int(6L, n_mask, replace = TRUE, prob = shares)
    }
  }
  # Fallback for education values outside 0-5
  unassigned <- qual_idx == 0L
  if (any(unassigned)) {
    n_ua <- sum(unassigned)
    qual_idx[unassigned] <- sample.int(6L, n_ua, replace = TRUE,
                                       prob = .TVA_QUAL_SHARES)
  }

  # FOE assignment — group by archetype (vectorized)
  arch_vals <- archetype[idx]
  foe <- character(n_part)
  for (a in 0L:7L) {
    mask <- arch_vals == a
    n_mask <- sum(mask)
    if (n_mask > 0L) {
      probs <- .TVA_ARCHETYPE_FOE[a + 1L, ]
      foe[mask] <- .TVA_FOE_CODES[sample.int(10L, n_mask, replace = TRUE,
                                              prob = probs)]
    }
  }
  # Fallback for archetypes outside 0-7
  unassigned_foe <- foe == ""
  if (any(unassigned_foe)) {
    n_uf <- sum(unassigned_foe)
    foe[unassigned_foe] <- .TVA_FOE_CODES[sample.int(10L, n_uf, replace = TRUE,
                                                      prob = .TVA_FOE_SHARES)]
  }

  # Commencement year
  byr <- birth_year[idx]
  standard_age <- sample(c(16L, 17L, 18L, 19L, 20L), n_part, replace = TRUE,
                         prob = c(0.10, 0.20, 0.30, 0.25, 0.15))
  is_mature <- runif(n_part) < 0.35
  mature_age <- pmin(pmax(as.integer(round(rnorm(n_part, 30, 8))), 21L), 55L)
  entry_age <- ifelse(is_mature, mature_age, standard_age)
  commence_year <- byr + entry_age
  commence_year <- pmax(commence_year, min_yr)
  commence_year <- pmin(commence_year, max_yr)

  # Duration and completion
  ft_yrs <- .TVA_FT_YEARS[qual_idx]
  is_ft <- runif(n_part) < .TVA_FT_SHARE
  cal_yrs <- ifelse(is_ft, ft_yrs, ft_yrs * .TVA_PT_MULTIPLIER)
  cal_yrs <- pmax(cal_yrs + rnorm(n_part, 0, 0.2), 0.25)
  duration_yrs <- round(cal_yrs, 1)

  comp_rates <- .TVA_COMPLETION_RATES[qual_idx]
  completed <- runif(n_part) < comp_rates
  completion_year <- as.integer(commence_year + ceiling(duration_yrs))
  completion_year <- ifelse(completed, completion_year, NA_integer_)

  # Provider assignment
  prov_type <- sample(.TVA_PROV_CODES, n_part, replace = TRUE,
                      prob = .TVA_PROV_SHARES)
  n_rtos <- min(9999999L, max(200L, as.integer(nrow(spine_df) / 20)))
  rto_pool <- sprintf("RTO%07d", sample.int(9999999L, n_rtos))
  rto_id <- sample(rto_pool, n_part, replace = TRUE)

  # ANZSCO from spine
  anzsco <- if ("anzsco_code" %in% names(spine_df)) {
    as.character(spine_df$anzsco_code[idx])
  } else {
    rep("000000", n_part)
  }

  spells <- data.frame(
    spine_idx     = idx,
    aeuid         = spine_df$aeuid_ncver[idx],
    birth_year    = byr,
    sex           = spine_df$sex[idx],
    state         = spine_df$state[idx],
    education     = edu_vals,
    archetype     = arch_vals,
    anzsco        = anzsco,
    qual_idx      = qual_idx,
    foe           = foe,
    commence_year = commence_year,
    duration_yrs  = duration_yrs,
    is_ft         = is_ft,
    completed     = completed,
    completion_year = completion_year,
    prov_type     = prov_type,
    rto_id        = rto_id,
    stringsAsFactors = FALSE
  )
  } # end R fallback

  # ---- Disability-driven reskilling: DC workers get elevated VET at k=0-3 ----
  if (!is.null(spine_df$disability_onset_year) &&
      !is.null(spine_df$is_dc)) {
    set.seed(seed + 1205L)
    already_tva <- spells$spine_idx
    dc_mask <- !is.na(spine_df$disability_onset_year) &
               !is.na(spine_df$is_dc) & spine_df$is_dc &
               !(seq_len(n) %in% already_tva)
    dc_candidates <- which(dc_mask)

    if (length(dc_candidates) > 0L) {
      # ~35% of DC workers not already in VET get reskilling enrolment
      dc_enrol <- dc_candidates[runif(length(dc_candidates)) < 0.35]

      if (length(dc_enrol) > 0L) {
        n_dc <- length(dc_enrol)
        dc_onset <- spine_df$disability_onset_year[dc_enrol]
        dc_delay <- sample(0L:3L, n_dc, replace = TRUE,
                           prob = c(0.20, 0.40, 0.30, 0.10))
        dc_commence <- dc_onset + dc_delay
        dc_ok <- dc_commence >= min_yr & dc_commence <= max_yr
        dc_enrol <- dc_enrol[dc_ok]
        dc_commence <- dc_commence[dc_ok]
        n_dc <- length(dc_enrol)

        if (n_dc > 0L) {
          # Reskilling qualifications: Cert III (50%), Cert IV (30%), Diploma (20%)
          dc_qual <- sample(c(3L, 4L, 5L), n_dc, replace = TRUE,
                            prob = c(0.50, 0.30, 0.20))
          dc_ft_yrs <- .TVA_FT_YEARS[dc_qual]
          dc_is_ft <- runif(n_dc) < .TVA_FT_SHARE
          dc_cal_yrs <- ifelse(dc_is_ft, dc_ft_yrs,
                               dc_ft_yrs * .TVA_PT_MULTIPLIER)
          dc_cal_yrs <- pmax(dc_cal_yrs + rnorm(n_dc, 0, 0.2), 0.25)
          dc_dur <- round(dc_cal_yrs, 1)
          dc_comp_rate <- .TVA_COMPLETION_RATES[dc_qual]
          dc_completed <- runif(n_dc) < dc_comp_rate
          dc_comp_yr <- as.integer(dc_commence + ceiling(dc_dur))
          dc_comp_yr <- ifelse(dc_completed, dc_comp_yr, NA_integer_)

          # FOE biased towards lower-physical occupations:
          # Health/welfare (05), Management/commerce (08), IT (03)
          dc_foe_codes  <- c("03", "05", "08", "09", "10")
          dc_foe_shares <- c(0.15, 0.20, 0.30, 0.20, 0.15)
          dc_foe <- sample(dc_foe_codes, n_dc, replace = TRUE,
                           prob = dc_foe_shares)

          dc_prov <- sample(.TVA_PROV_CODES, n_dc, replace = TRUE,
                            prob = .TVA_PROV_SHARES)
          n_rtos <- max(200L, as.integer(nrow(spine_df) / 20))
          rto_pool_dc <- sprintf("RTO%07d", sample.int(9999999L, n_rtos))
          dc_rto <- sample(rto_pool_dc, n_dc, replace = TRUE)
          dc_anzsco <- if ("anzsco_code" %in% names(spine_df)) {
            as.character(spine_df$anzsco_code[dc_enrol])
          } else {
            rep("000000", n_dc)
          }

          dc_spells <- data.frame(
            spine_idx     = dc_enrol,
            aeuid         = spine_df$aeuid_ncver[dc_enrol],
            birth_year    = spine_df$birth_year[dc_enrol],
            sex           = spine_df$sex[dc_enrol],
            state         = spine_df$state[dc_enrol],
            education     = spine_df$education[dc_enrol],
            archetype     = spine_df$archetype[dc_enrol],
            anzsco        = dc_anzsco,
            qual_idx      = dc_qual,
            foe           = dc_foe,
            commence_year = dc_commence,
            duration_yrs  = dc_dur,
            is_ft         = dc_is_ft,
            completed     = dc_completed,
            completion_year = dc_comp_yr,
            prov_type     = dc_prov,
            rto_id        = dc_rto,
            stringsAsFactors = FALSE
          )
          spells <- rbind(spells, dc_spells)
        }
      }
    }
  }

  spells
}


# ===========================================================================
# Step 2+3: Year-streaming vectorized activity generation
# ===========================================================================

#' Stream training activity records year-by-year to disk
#'
#' Pre-computes spell-level constants once, then for each year: filters
#' active spells, expands to subject-level via rep(), does bulk random
#' draws, writes to disk, and (optionally) accumulates for return.
#'
#' @param spells data.frame from select_tva_participants().
#' @param seed Integer seed.
#' @param years Integer vector of requested years.
#' @param run_dir Character. Run directory path.
#' @param format Character. "parquet" or "csv".
#' @param return_data Logical.
#' @return If return_data, named list of data.frames keyed by year.
#'   Otherwise, list with total_records count.
#' @keywords internal
.stream_tva_activity <- function(spells, seed, years, run_dir, format,
                                 return_data) {
  n_spells <- nrow(spells)

  # Handle empty spells
  if (n_spells == 0L) {
    result <- if (return_data) list() else NULL
    for (yr in years) {
      empty <- .empty_tva_activity()
      write_product(empty, tva_product_name("trn-actvty", yr), "TVA",
                    run_dir, format)
      if (return_data) result[[as.character(yr)]] <- empty
    }
    if (return_data) return(result)
    return(list(total_records = 0L))
  }

  if (exists("project_tva_activity__", mode = "function")) {
    raw <- project_tva_activity__(
      spell_aeuid           = as.character(spells$aeuid),
      spell_birth_year      = as.integer(spells$birth_year),
      spell_sex             = as.integer(spells$sex),
      spell_state           = as.integer(spells$state),
      spell_education       = as.integer(spells$education),
      spell_anzsco          = as.character(spells$anzsco),
      spell_qual_idx        = as.integer(spells$qual_idx),
      spell_foe             = as.character(spells$foe),
      spell_commence_year   = as.integer(spells$commence_year),
      spell_duration_yrs    = as.numeric(spells$duration_yrs),
      spell_is_ft           = as.integer(spells$is_ft),
      spell_completion_year = as.integer(spells$completion_year),
      spell_rto_id          = as.character(spells$rto_id),
      spell_prov_type       = as.integer(spells$prov_type),
      years                 = as.integer(years),
      seed                  = as.integer(seed)
    )
    activity <- as.data.frame(raw, stringsAsFactors = FALSE)
    activity$ACTIVITY_START_DATE <- as.Date(activity$ACTIVITY_START_DATE)
    activity$ACTIVITY_END_DATE <- as.Date(activity$ACTIVITY_END_DATE)

    result <- if (return_data) list() else NULL
    total_records <- 0L
    for (yr in years) {
      yr_df <- activity[activity$COLLECTION_YR == yr, , drop = FALSE]
      rownames(yr_df) <- NULL
      write_product(yr_df, tva_product_name("trn-actvty", yr), "TVA",
                    run_dir, format)
      total_records <- total_records + nrow(yr_df)
      if (return_data) result[[as.character(yr)]] <- yr_df
    }

    if (return_data) return(result)
    return(list(total_records = total_records))
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 1201L)

  # --- Pre-compute spell-level constants (RNG consumed once) ---------------

  birth_months  <- sample.int(12L, n_spells, replace = TRUE)
  school_lvls   <- sample(.TVA_SCHOOL_CODES, n_spells, replace = TRUE,
                           prob = .TVA_SCHOOL_SHARES)
  lfs_ids       <- sample(.TVA_LFS_CODES, n_spells, replace = TRUE,
                           prob = .TVA_LFS_SHARES)
  study_reasons <- sample(.TVA_STUDY_REASON_CODES, n_spells, replace = TRUE,
                           prob = .TVA_STUDY_REASON_SHARES)
  deliv_modes   <- sample(.TVA_DELIV_CODES, n_spells, replace = TRUE,
                           prob = .TVA_DELIV_SHARES)
  fund_srcs     <- sample(.TVA_FUND_CODES, n_spells, replace = TRUE,
                           prob = .TVA_FUND_SHARES)
  remote_ids    <- sample(.TVA_REMOTE_CODES, n_spells, replace = TRUE,
                           prob = .TVA_REMOTE_SHARES)
  seifa_qs      <- sample(.TVA_SEIFA_CODES, n_spells, replace = TRUE,
                           prob = .TVA_SEIFA_SHARES)

  # Geography: postcodes by state
  client_pc <- character(n_spells)
  ho_pc     <- character(n_spells)
  for (st in names(.TVA_STATE_PC)) {
    mask <- as.character(spells$state) == st
    n_mask <- sum(mask)
    if (n_mask > 0L) {
      rng <- .TVA_STATE_PC[[st]]
      client_pc[mask] <- sprintf("%04d", sample(rng[1]:rng[2], n_mask,
                                                replace = TRUE))
      ho_pc[mask] <- sprintf("%04d", sample(rng[1]:rng[2], n_mask,
                                            replace = TRUE))
    }
  }
  missing_pc <- client_pc == ""
  if (any(missing_pc)) {
    n_miss <- sum(missing_pc)
    client_pc[missing_pc] <- sprintf("%04d", sample(2000L:6999L, n_miss,
                                                    replace = TRUE))
    ho_pc[missing_pc] <- client_pc[missing_pc]
  }

  # Deterministic spell-level derived fields
  hed <- .TVA_EDU_TO_HED[pmin(spells$education + 1L, 6L)]
  prior_ed <- ifelse(spells$education >= 3L, "Y", "N")
  is_apprentice <- (spells$qual_idx %in% c(3L, 4L)) & (deliv_modes == 30L)
  age_at_commence <- spells$commence_year - spells$birth_year
  vet_schools <- (age_at_commence <= 18L) & (spells$prov_type == 97L)
  n_subj_per <- ifelse(spells$is_ft, .TVA_SUBJECTS_FT, .TVA_SUBJECTS_PT)
  prog_hours <- as.integer(n_subj_per * ceiling(spells$duration_yrs) *
                           .TVA_SUBJECT_HOURS_MEAN)
  prog_ids <- sprintf("P%s%03d%04d", spells$foe, spells$qual_idx,
                      seq_len(n_spells) %% 10000L)
  pkg_ids <- sprintf("PKG%s%d", spells$foe, spells$qual_idx)

  # Spell active year range
  spell_end_yr <- ifelse(!is.na(spells$completion_year),
                         spells$completion_year,
                         spells$commence_year +
                           as.integer(ceiling(spells$duration_yrs)))

  # Pack spell constants for efficient indexing in year loop
  sc <- list(
    birth_month   = birth_months,
    school_lvl    = school_lvls,
    lfs_id        = lfs_ids,
    study_reason  = study_reasons,
    deliv_mode    = deliv_modes,
    fund_src      = fund_srcs,
    remote_id     = remote_ids,
    seifa_q       = seifa_qs,
    client_pc     = client_pc,
    ho_pc         = ho_pc,
    hed           = hed,
    prior_ed      = prior_ed,
    is_apprentice = is_apprentice,
    vet_schools   = vet_schools,
    n_subj        = n_subj_per,
    prog_hours    = prog_hours,
    prog_id       = prog_ids,
    pkg_id        = pkg_ids
  )

  # --- Year-streaming loop ------------------------------------------------

  result <- if (return_data) list() else NULL
  total_records <- 0L
  row_offset <- 0L

  for (yr in years) {
    yr_df <- .generate_tva_year(yr, spells, sc, spell_end_yr, row_offset)

    write_product(yr_df, tva_product_name("trn-actvty", yr), "TVA",
                  run_dir, format)

    n_rows <- nrow(yr_df)
    total_records <- total_records + n_rows
    row_offset <- row_offset + n_rows

    if (return_data) result[[as.character(yr)]] <- yr_df
  }

  if (return_data) result
  else list(total_records = total_records)
}


#' Generate training activity records for one year (vectorized)
#'
#' @param yr Integer. Calendar year.
#' @param spells data.frame of spell metadata.
#' @param sc List of pre-computed spell-level constants.
#' @param spell_end_yr Integer vector. End year for each spell.
#' @param row_offset Integer. Global row counter offset.
#' @return data.frame with PLIDA TVA training activity columns.
#' @keywords internal
.generate_tva_year <- function(yr, spells, sc, spell_end_yr, row_offset) {
  # Filter active spells for this year
  active <- which(spells$commence_year <= yr & spell_end_yr >= yr)
  n_active <- length(active)
  if (n_active == 0L) return(.empty_tva_activity())

  # Number of subjects per active spell and total
  n_subj <- sc$n_subj[active]
  total <- sum(n_subj)

  # Expand spell indices to subject level
  spell_exp <- rep(active, n_subj)
  subj_within <- sequence(n_subj)
  row_ids <- row_offset + seq_len(total)

  # Year-dependent columns
  age <- as.integer(yr - spells$birth_year[spell_exp])
  is_commence <- yr == spells$commence_year[spell_exp]

  # Subject-level random draws (bulk)
  subj_hours <- pmax(10L, as.integer(round(
    rnorm(total, .TVA_SUBJECT_HOURS_MEAN, .TVA_SUBJECT_HOURS_SD)
  )))
  outcomes <- sample(.TVA_OUTCOME_CODES, total, replace = TRUE,
                     prob = .TVA_OUTCOME_SHARES)
  start_months <- sample.int(6L, total, replace = TRUE)
  start_days   <- sample.int(28L, total, replace = TRUE)
  start_dates  <- as.Date(sprintf("%d-%02d-%02d", yr, start_months, start_days))
  end_dates    <- start_dates + as.integer(round(subj_hours / 5 * 7))
  yr_end       <- as.Date(paste0(yr, "-12-31"))
  end_dates    <- pmin(end_dates, yr_end)

  # Build data.frame — all columns via vectorized indexing
  data.frame(
    SYNTHETIC_AEUID                = spells$aeuid[spell_exp],
    TVA_ROW_ID                     = sprintf("TVA%010d", row_ids),
    COLLECTION_YR                  = yr,
    AGE                            = age,
    YEAR_OF_BIRTH                  = spells$birth_year[spell_exp],
    MONTH_OF_BIRTH                 = sc$birth_month[spell_exp],
    GENDER_ID                      = spells$sex[spell_exp],
    HIGHEST_SCHL_LVL_ST            = sc$school_lvl[spell_exp],
    HIGHEST_ED_LVL_ST              = sc$hed[spell_exp],
    PRIOR_ED_ACHIEVE_FG            = sc$prior_ed[spell_exp],
    AT_SCHOOL_FG                   = ifelse(sc$vet_schools[spell_exp] &
                                            is_commence, "Y", "N"),
    LABOUR_FORCE_STATUS_ID         = sc$lfs_id[spell_exp],
    ANZSCO_ID                      = spells$anzsco[spell_exp],
    PROGRAM_ID                     = sc$prog_id[spell_exp],
    PROGRAM_FOE_ID                 = spells$foe[spell_exp],
    PROGRAM_LOE_ID                 = .TVA_QUAL_CODES[spells$qual_idx[spell_exp]],
    PROGRAM_RECOGNITION_ID         = 11L,
    PROGRAM_TRAINING_PACKAGE_ID    = sc$pkg_id[spell_exp],
    PROGRAM_TYPE_OF_TRAINING_ID    = ifelse(sc$is_apprentice[spell_exp],
                                            11L, 21L),
    PROGRAM_NOMINAL_HOURS          = sc$prog_hours[spell_exp],
    PROGRAM_VET_FG                 = "Y",
    SUBJECT_ID                     = sprintf("S%s%03d%02d",
                                             spells$foe[spell_exp],
                                             spell_exp %% 1000L,
                                             subj_within),
    SUBJECT_FOE_ID                 = spells$foe[spell_exp],
    SUBJECT_NOMINAL_HOURS          = subj_hours,
    SUBJECT_VET_FG                 = "Y",
    SUBJECT_FG                     = "Y",
    APPRENTICESHIP_FG              = ifelse(sc$is_apprentice[spell_exp],
                                            "Y", "N"),
    VET_IN_SCHOOLS_FG              = ifelse(sc$vet_schools[spell_exp] &
                                            is_commence, "Y", "N"),
    STUDENT_COMMENCING_FG          = ifelse(is_commence, "Y", "N"),
    STUDY_REASON_ID                = sc$study_reason[spell_exp],
    ACTIVITY_START_DATE            = start_dates,
    ACTIVITY_END_DATE              = end_dates,
    DELIVERY_MODE_ID               = sc$deliv_mode[spell_exp],
    NATIONAL_FUNDING_SOURCE_ID     = sc$fund_src[spell_exp],
    NATIONAL_OUTCOME_ID            = outcomes,
    CLIENT_POSTCODE_DERIVED        = sc$client_pc[spell_exp],
    CLIENT_STATE_RESIDENCE_DERIVED = spells$state[spell_exp],
    CLIENT_REMOTENESS_ID_DERIVED   = sc$remote_id[spell_exp],
    CLIENT_SEIFA_IRSD_QUINT_DRVD   = sc$seifa_q[spell_exp],
    RTO_ID                         = spells$rto_id[spell_exp],
    TRAIN_ORG_TYPE_ID              = spells$prov_type[spell_exp],
    HEAD_OFFICE_STATE              = spells$state[spell_exp],
    HEAD_OFFICE_POSTCODE           = sc$ho_pc[spell_exp],
    STATE_OF_FUNDING_GF            = spells$state[spell_exp],
    stringsAsFactors = FALSE
  )
}


# ===========================================================================
# Step 4: Completions — vectorized projection + year-streaming write
# ===========================================================================

#' Stream completions: generate vectorized, split by year, write each
#'
#' @param spells data.frame from select_tva_participants().
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @param years Integer vector of requested years.
#' @param run_dir Character. Run directory path.
#' @param format Character. "parquet" or "csv".
#' @param return_data Logical.
#' @return If return_data, named list of data.frames keyed by year.
#'   Otherwise, list with total_records count.
#' @keywords internal
.stream_tva_completions <- function(spells, spine_df, seed, years, run_dir,
                                    format, return_data) {
  yr_range <- range(years)
  completions <- project_tva_completions(spells, spine_df, seed, yr_range)

  result <- if (return_data) list() else NULL
  total_records <- 0L

  if (nrow(completions) == 0L) {
    for (yr in years) {
      empty <- .empty_tva_completions()
      write_product(empty, tva_product_name("prog-comp", yr), "TVA",
                    run_dir, format)
      if (return_data) result[[as.character(yr)]] <- empty
    }
    if (return_data) return(result)
    return(list(total_records = 0L))
  }

  rec_year <- completions$COLLECTION_YR

  for (yr in years) {
    mask <- rec_year == yr
    yr_df <- completions[mask, , drop = FALSE]
    rownames(yr_df) <- NULL

    write_product(yr_df, tva_product_name("prog-comp", yr), "TVA",
                  run_dir, format)
    total_records <- total_records + nrow(yr_df)

    if (return_data) result[[as.character(yr)]] <- yr_df
  }

  if (return_data) result
  else list(total_records = total_records)
}


#' Project program completion records (vectorized)
#'
#' @param spells data.frame from select_tva_participants().
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @param yr_range Integer vector of length 2.
#' @return data.frame with PLIDA TVA completions columns.
#' @keywords internal
project_tva_completions <- function(spells, spine_df, seed, yr_range) {
  if (nrow(spells) == 0L) return(.empty_tva_completions())

  if (exists("project_tva_completions__", mode = "function")) {
    raw <- project_tva_completions__(
      spell_aeuid           = as.character(spells$aeuid),
      spell_birth_year      = as.integer(spells$birth_year),
      spell_sex             = as.integer(spells$sex),
      spell_state           = as.integer(spells$state),
      spell_education       = as.integer(spells$education),
      spell_anzsco          = as.character(spells$anzsco),
      spell_qual_idx        = as.integer(spells$qual_idx),
      spell_foe             = as.character(spells$foe),
      spell_completion_year = as.integer(spells$completion_year),
      spell_completed       = as.integer(spells$completed),
      spell_rto_id          = as.character(spells$rto_id),
      spell_prov_type       = as.integer(spells$prov_type),
      seed                  = as.integer(seed),
      min_year              = as.integer(yr_range[1L]),
      max_year              = as.integer(yr_range[2L])
    )
    df <- as.data.frame(raw, stringsAsFactors = FALSE)
    df$DATE_PROGRAM_COMPLETED <- as.Date(df$DATE_PROGRAM_COMPLETED)
    ord <- order(df$SYNTHETIC_AEUID, df$COLLECTION_YR)
    df <- df[ord, , drop = FALSE]
    rownames(df) <- NULL
    return(df)
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 1202L)

  min_yr <- yr_range[1L]
  max_yr <- yr_range[2L]

  # Filter to completed spells within year range
  comp_mask <- spells$completed &
               !is.na(spells$completion_year) &
               spells$completion_year >= min_yr &
               spells$completion_year <= max_yr
  comp_spells <- spells[comp_mask, , drop = FALSE]

  if (nrow(comp_spells) == 0L) return(.empty_tva_completions())

  n_comp <- nrow(comp_spells)

  # Generate birth months and demographics
  birth_months <- sample.int(12L, n_comp, replace = TRUE)
  school_lvls <- sample(.TVA_SCHOOL_CODES, n_comp, replace = TRUE,
                         prob = .TVA_SCHOOL_SHARES)
  lfs_ids <- sample(.TVA_LFS_CODES, n_comp, replace = TRUE,
                     prob = .TVA_LFS_SHARES)

  # Map education to highest_ed_lvl_st
  hed <- .TVA_EDU_TO_HED[pmin(comp_spells$education + 1L, 6L)]

  # Geography
  client_pc <- character(n_comp)
  ho_pc <- character(n_comp)
  for (st in names(.TVA_STATE_PC)) {
    mask <- as.character(comp_spells$state) == st
    n_mask <- sum(mask)
    if (n_mask > 0L) {
      rng <- .TVA_STATE_PC[[st]]
      client_pc[mask] <- sprintf("%04d", sample(rng[1]:rng[2], n_mask,
                                                replace = TRUE))
      ho_pc[mask] <- sprintf("%04d", sample(rng[1]:rng[2], n_mask,
                                            replace = TRUE))
    }
  }
  missing <- client_pc == ""
  if (any(missing)) {
    client_pc[missing] <- sprintf("%04d", sample(2000L:6999L, sum(missing),
                                                 replace = TRUE))
    ho_pc[missing] <- client_pc[missing]
  }

  remote_ids <- sample(.TVA_REMOTE_CODES, n_comp, replace = TRUE,
                        prob = .TVA_REMOTE_SHARES)
  seifa_qs <- sample(.TVA_SEIFA_CODES, n_comp, replace = TRUE,
                      prob = .TVA_SEIFA_SHARES)

  comp_yr <- comp_spells$completion_year
  comp_dates <- as.Date(paste0(comp_yr, "-",
    sprintf("%02d", sample(1L:12L, n_comp, replace = TRUE)), "-",
    sprintf("%02d", sample(1L:28L, n_comp, replace = TRUE))))
  age_at_comp <- comp_yr - comp_spells$birth_year

  prog_ids <- sprintf("P%s%03d%04d", comp_spells$foe, comp_spells$qual_idx,
                       seq_len(n_comp) %% 10000L)
  pkg_ids <- sprintf("PKG%s%d", comp_spells$foe, comp_spells$qual_idx)

  result <- data.frame(
    SYNTHETIC_AEUID                = comp_spells$aeuid,
    TVA_ROW_ID                     = sprintf("TVAC%08d", seq_len(n_comp)),
    COLLECTION_YR                  = comp_yr,
    AGE                            = age_at_comp,
    YEAR_OF_BIRTH                  = comp_spells$birth_year,
    MONTH_OF_BIRTH                 = birth_months,
    GENDER_ID                      = comp_spells$sex,
    HIGHEST_SCHL_LVL_ST            = school_lvls,
    HIGHEST_ED_LVL_ST              = hed,
    PRIOR_ED_ACHIEVE_FG            = ifelse(comp_spells$education >= 3L,
                                            "Y", "N"),
    AT_SCHOOL_FG                   = "N",
    LABOUR_FORCE_STATUS_ID         = lfs_ids,
    ANZSCO_ID                      = comp_spells$anzsco,
    PROGRAM_ID                     = prog_ids,
    PROGRAM_FOE_ID                 = comp_spells$foe,
    PROGRAM_LOE_ID                 = .TVA_QUAL_CODES[comp_spells$qual_idx],
    PROGRAM_RECOGNITION_ID         = 11L,
    PROGRAM_TRAINING_PACKAGE_ID    = pkg_ids,
    PROGRAM_TYPE_OF_TRAINING_ID    = 21L,
    DATE_PROGRAM_COMPLETED         = comp_dates,
    YR_PROGRAM_COMPLETED           = comp_yr,
    CLIENT_POSTCODE_DERIVED        = client_pc,
    CLIENT_STATE_RESIDENCE_DERIVED = comp_spells$state,
    CLIENT_REMOTENESS_ID_DERIVED   = remote_ids,
    CLIENT_SEIFA_IRSD_QUINT_DRVD   = seifa_qs,
    RTO_ID                         = comp_spells$rto_id,
    TRAIN_ORG_TYPE_ID              = comp_spells$prov_type,
    HEAD_OFFICE_STATE              = comp_spells$state,
    HEAD_OFFICE_POSTCODE           = ho_pc,
    STATE_OF_FUNDING_GF            = comp_spells$state,
    stringsAsFactors = FALSE
  )

  ord <- order(result$SYNTHETIC_AEUID, result$COLLECTION_YR)
  result <- result[ord, ]
  rownames(result) <- NULL
  result
}

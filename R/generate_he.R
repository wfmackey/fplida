#' Generate Higher Education (HE) dataset
#'
#' Projects five HE sub-tables from the fplida spine, matching the real PLIDA
#' Higher Education data structure. One row per student-year (enrol), per
#' course (course), per unit-of-study (load), per completion (completions),
#' and per student-year HELP debt (help).
#'
#' For large N (>chunk_size participants), the load and help tables are
#' generated in spell-chunks to keep peak memory bounded. Each chunk of
#' spells is processed separately and written to disk before the next chunk,
#' allowing 25M+ person runs on 32GB machines.
#'
#' @section Dataset and variable information:
#' The [Department of Education student data](https://www.education.gov.au/higher-education-statistics/student-data)
#' website gives information about this dataset. Use `dataset_info("HE")` for
#' dataset information. Use `variable_info("HE")` for variables, sources,
#' value support, and topic tags.
#'
#' @param spine Data.frame (from \code{generate_spine()}) or NULL. If NULL,
#'   the most recent spine is loaded from the run directory (only the columns
#'   needed for HE generation are read, saving memory at large N).
#' @param seed Integer. Random seed for HE-specific generation.
#' @param years Integer vector. Reporting years to generate (default 2005:2021).
#' @param output_dir Character or NULL. Base output directory. If NULL, uses
#'   \code{get_data_path()}.
#' @param format Character. Output format: "parquet" (default) or "csv".
#' @param return_data Logical. If TRUE, return data.frames in memory;
#'   if FALSE (default), write to disk and return metadata only. Tests should
#'   pass TRUE.
#' @param chunk_size Integer. Maximum number of HE spells to process per
#'   Rust call for the load table. Default 250000. At 25M people (~6M HE
#'   spells), this yields ~25 chunks, each producing ~5-6M load rows
#'   (~1.5GB in R). Reduce if memory-constrained.
#'
#' @return If \code{return_data = TRUE}, a named list with five data.frames.
#'   If \code{return_data = FALSE}, a metadata summary.
#'
#' @examples
#' \dontrun{
#' spine <- generate_spine(n = 1000L, seed = 1L)
#' he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)
#' str(he$enrol)
#' }
#'
#' @export
generate_he <- function(spine = NULL, seed = 42L, years = 2005L:2021L,
                        output_dir = NULL,
                        format = c("parquet", "csv"),
                        return_data = FALSE,
                        chunk_size = 250000L) {
  seed <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  chunk_size <- as.integer(chunk_size)
  stopifnot(
    "`seed` must be an integer"     = !is.na(seed),
    "`years` must be integer"       = all(!is.na(years)),
    "`chunk_size` must be positive" = chunk_size > 0L
  )

  run_dir <- resolve_run_dir(output_dir)

  # ---- Load spine (column-selective when possible) -------------------------
  he_cols <- c(
    "spine_id", "aeuid_de", "birth_year", "sex", "state", "education",
    "archetype", "country_of_birth", "country_of_birth_sacc", "indigenous",
    "year_of_death", "disability_onset_year", "disability_type", "is_dc"
  )
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, he_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))

  # Build mini agency spine before we free the full spine
  mini_spine <- data.frame(
    spine_id = spine$spine_id,
    aeuid_de = spine$aeuid_de,
    stringsAsFactors = FALSE
  )

  # Build enrolment spells (one row per person per course)
  spells <- select_he_participants(spine, seed, years)

  # Free spine — spells has all data needed for projections
  if (spine_loaded) { rm(spine); gc() }

  yr_range <- range(years)
  n_spells <- nrow(spells)

  # ---- Project enrol (student-year) — build, write, free ------------------
  enrol <- project_he_enrol(spells, NULL, seed, yr_range)
  write_product(enrol, he_product_name("enrol"), "HE", run_dir, format)
  n_enrol <- nrow(enrol)
  if (!return_data) { rm(enrol); gc() }

  # ---- Project course (one row per spell) ---------------------------------
  course <- project_he_course(spells, seed)
  write_product(course, he_product_name("course"), "HE", run_dir, format)
  n_course <- nrow(course)
  if (!return_data) { rm(course); gc() }

  # ---- Project completions ------------------------------------------------
  completions <- project_he_completions(spells, yr_range)
  write_product(completions, he_product_name("completions"), "HE", run_dir,
                format)
  n_completions <- nrow(completions)
  if (!return_data) { rm(completions); gc() }

  # ---- Determine chunking for load + help ---------------------------------
  n_chunks     <- ceiling(n_spells / chunk_size)
  use_chunking <- n_chunks > 1L

  if (!use_chunking) {
    # === Single chunk: backward-compatible path ===
    load <- project_he_load(spells, seed, yr_range)
    write_product(load, he_product_name("load"), "HE", run_dir, format)
    n_load <- nrow(load)

    help_tbl <- project_he_help(load)
    write_product(help_tbl, he_product_name("help"), "HE", run_dir, format)
    n_help <- nrow(help_tbl)

    if (!return_data) { rm(load, help_tbl); gc() }
  } else {
    # === Multiple chunks: process spells in batches ===
    load_chunks <- if (return_data) list() else NULL
    help_chunks <- if (return_data) list() else NULL
    n_load <- 0L
    n_help <- 0L

    for (ci in seq_len(n_chunks)) {
      start   <- (ci - 1L) * chunk_size + 1L
      end     <- min(ci * chunk_size, n_spells)
      chunk_sp <- spells[start:end, , drop = FALSE]

      chunk_seed <- as.integer(seed + (ci - 1L) * 10000L)

      chunk_raw <- project_he_load__(
        spell_aeuid            = as.character(chunk_sp$aeuid),
        spell_commence_year    = as.integer(chunk_sp$commence_year),
        spell_actual_duration  = as.integer(chunk_sp$actual_duration),
        spell_completed        = as.integer(chunk_sp$completed),
        spell_is_ft            = as.integer(chunk_sp$is_ft),
        spell_qual_idx         = as.integer(chunk_sp$qual_idx),
        spell_foe              = as.character(chunk_sp$foe),
        spell_inst_code        = as.character(chunk_sp$inst_code),
        spell_inst_state       = as.integer(chunk_sp$inst_state),
        spell_country_of_birth = as.integer(chunk_sp$country_of_birth),
        spell_attend_mode      = as.integer(chunk_sp$attend_mode),
        spell_course_code      = as.character(chunk_sp$course_code),
        min_year               = yr_range[1L],
        max_year               = yr_range[2L],
        seed                   = chunk_seed
      )

      chunk_load <- as.data.frame(chunk_raw, stringsAsFactors = FALSE)
      rm(chunk_raw)

      write_product_chunk(chunk_load, he_product_name("load"), "HE",
                          run_dir, format, ci, n_chunks)
      n_load <- n_load + nrow(chunk_load)

      chunk_help <- project_he_help(chunk_load)
      write_product_chunk(chunk_help, he_product_name("help"), "HE",
                          run_dir, format, ci, n_chunks)
      n_help <- n_help + nrow(chunk_help)

      if (return_data) {
        load_chunks[[ci]] <- chunk_load
        help_chunks[[ci]] <- chunk_help
      }

      if (!return_data) rm(chunk_load, chunk_help)
      gc()
    }

    if (return_data) {
      load     <- do.call(rbind, load_chunks)
      help_tbl <- do.call(rbind, help_chunks)
    }
  }

  # Write DE agency spine (idempotent)
  ds_dir <- dataset_dir(run_dir, "HE")
  write_agency_spine(mini_spine, "DE", ds_dir, format = format)

  if (return_data) {
    list(
      enrol       = enrol,
      course      = course,
      load        = load,
      completions = completions,
      help        = help_tbl
    )
  } else {
    invisible(list(
      n_records = list(
        enrol       = n_enrol,
        course      = n_course,
        load        = n_load,
        completions = n_completions,
        help        = n_help
      ),
      years = years,
      path  = "de-he"
    ))
  }
}


# =============================================================================
# Constants
# =============================================================================

# Participation rate by spine education code (0..5).
# Index position = education + 1.
# Calibrated so ND any-training (HE) rate ≈ 5%/year.
.HE_PARTICIPATION_RATE <- c(
  0.00,  # 0 = under 15
  0.04,  # 1 = below year 12
  0.05,  # 2 = year 12
  0.08,  # 3 = cert III-IV
  0.25,  # 4 = diploma
  1.00   # 5 = bachelor+
)

# Qualification labels, DEEWR course type codes, and distributions.
.HE_QUAL_LABELS <- c("Bachelor", "Honours", "GradDip",
                      "Masters_CW", "Masters_Res", "PhD")
.HE_COURSE_TYPES <- c(1L, 2L, 3L, 4L, 5L, 6L)

# Shares for education==5 (primary qualification).
# Bachelor, Honours, GradDip, Masters_CW, Masters_Res, PhD
.HE_QUAL_SHARES_EDU5 <- c(0.65, 0.03, 0.08, 0.12, 0.03, 0.05)

# Shares for education<5 (attempt distribution).
.HE_QUAL_SHARES_ATTEMPT <- c(0.85, 0.00, 0.05, 0.05, 0.00, 0.00)

# Full-time duration in years by qualification.
.HE_FT_DURATION <- c(3L, 1L, 1L, 2L, 2L, 4L)
names(.HE_FT_DURATION) <- .HE_QUAL_LABELS

# Completion rate by qualification.
.HE_COMPLETION_RATE <- c(0.75, 0.85, 0.80, 0.80, 0.70, 0.65)
names(.HE_COMPLETION_RATE) <- .HE_QUAL_LABELS

# Field of education codes and baseline shares.
.HE_FOE_CODES  <- c("0801", "0901", "0401", "0301", "0501", "0603",
                     "0201", "0701", "1001", "0101", "0999")
.HE_FOE_SHARES <- c(0.20,   0.12,   0.10,   0.10,   0.08,   0.08,
                     0.05,   0.05,   0.07,   0.02,   0.13)

# Archetype-to-FOE affinity weights (8 archetypes x 11 FOE codes).
# Rows: 0=Labourer, 1=SkilledTrade, 2=Health, 3=Office, 4=Professional,
#        5=Manager, 6=Technical, 7=Service
# Cols: same order as .HE_FOE_CODES
.HE_ARCHETYPE_FOE <- matrix(c(
  # 0801 0901 0401 0301 0501 0603 0201 0701 1001 0101 0999
    0.15,0.15,0.15,0.05,0.05,0.05,0.05,0.10,0.05,0.02,0.18, # Labourer
    0.15,0.05,0.05,0.30,0.02,0.02,0.05,0.03,0.08,0.05,0.20, # SkilledTrade
    0.05,0.05,0.05,0.02,0.25,0.30,0.02,0.02,0.08,0.02,0.14, # Health
    0.30,0.10,0.10,0.05,0.05,0.03,0.10,0.05,0.03,0.04,0.15, # Office
    0.10,0.12,0.05,0.15,0.08,0.03,0.10,0.05,0.15,0.05,0.12, # Professional
    0.25,0.12,0.12,0.08,0.05,0.03,0.08,0.05,0.05,0.02,0.15, # Manager
    0.08,0.05,0.03,0.25,0.03,0.02,0.15,0.03,0.18,0.08,0.10, # Technical
    0.12,0.18,0.15,0.03,0.08,0.08,0.03,0.10,0.05,0.02,0.16  # Service
), nrow = 8, ncol = 11, byrow = TRUE)

# Institution lookup: code, type (1=TableA, 2=DualSector, 3=Other), state
# 43 Table A + 6 dual-sector + 10 other = 59 institutions
.HE_INSTITUTIONS <- data.frame(
  code = sprintf("%04d", 1:59),
  type = c(rep(1L, 43), rep(2L, 6), rep(3L, 10)),
  state = c(
    # 43 Table A: distributed across states roughly proportional to population
    # STE 2021 order: NSW(12), VIC(10), QLD(8), SA(3), WA(5), TAS(1),
    # NT(1), ACT(3) = 43
    rep(1L, 12), rep(2L, 10), rep(3L, 8), rep(4L, 3),
    rep(5L, 5), rep(6L, 1), rep(7L, 1), rep(8L, 3),
    # 6 dual-sector (VIC: RMIT, Swinburne, VU; NT: CDU; QLD: CQU; VIC: Fed)
    2L, 2L, 2L, 7L, 3L, 2L,
    # 10 other providers (spread across states)
    1L, 1L, 2L, 2L, 3L, 4L, 5L, 1L, 3L, 2L
  ),
  stringsAsFactors = FALSE
)

# Institution type labels for output
.HE_INST_TYPE_LABELS <- c("Table A", "Dual-sector", "Other")

# Study mode weights: attendance type (FT/PT)
.HE_FT_SHARE <- 0.65

# Attendance mode weights: internal, external, multi-modal
.HE_ATTEND_MODE_WEIGHTS <- c(0.55, 0.25, 0.20)

# Financial parameters
.HE_CSP_SHARE     <- 0.85
.HE_UPFRONT_SHARE <- 0.10
# Annual HELP debt by qualification index (Bachelor..PhD)
.HE_ANNUAL_HELP <- c(8500, 8500, 10000, 12000, 0, 0)
# Annual student contribution (CSP)
.HE_CSP_ANNUAL  <- 7500
# Annual full-fee
.HE_FULL_FEE_ANNUAL <- 22000


# =============================================================================
# Participant selection
# =============================================================================

.he_disability_code <- function(disability_type, support_indicator) {
  disability_type <- as.integer(disability_type)
  support_indicator <- as.integer(support_indicator)
  n <- length(disability_type)
  disabled <- !is.na(disability_type)

  # E386 positions are: any disability, hearing, learning, mobility, vision,
  # medical, other, and whether the student requests support advice.
  category_position <- rep(7L, n)
  category_position[disability_type == 2L & disabled] <- 2L
  category_position[disability_type %in% c(7L, 15L) & disabled] <- 3L
  category_position[
    disability_type %in% c(4L, 5L, 8L, 9L, 10L, 12L) & disabled
  ] <- 4L
  category_position[disability_type == 1L & disabled] <- 5L
  category_position[
    disability_type %in% c(6L, 14L) & disabled
  ] <- 6L

  characters <- matrix("0", nrow = n, ncol = 8L)
  characters[, 1L] <- ifelse(disabled, "1", "2")
  if (any(disabled)) {
    row <- which(disabled)
    characters[cbind(row, category_position[row])] <- "1"
    characters[row, 8L] <- ifelse(
      !is.na(support_indicator[row]) & support_indicator[row] == 1L,
      "1", "2"
    )
  }
  apply(characters, 1L, paste0, collapse = "")
}

#' Select HE participants and build enrolment spells
#'
#' Determines who enrols in HE based on spine education level and demographics,
#' assigns course attributes (qualification, FOE, institution, timing).
#'
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @param years Integer vector of reporting years.
#' @return data.frame of enrolment spells (one row per person per course).
#' @keywords internal
select_he_participants <- function(spine_df, seed, years) {
  n <- nrow(spine_df)
  min_yr <- min(years)
  max_yr <- max(years)

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 800L)

  country_of_birth_sacc <- if (
    "country_of_birth_sacc" %in% names(spine_df)
  ) {
    as.integer(spine_df$country_of_birth_sacc)
  } else {
    ifelse(as.integer(spine_df$country_of_birth) == 0L, 1101L, 9999L)
  }
  disability_type <- if ("disability_type" %in% names(spine_df)) {
    as.integer(spine_df$disability_type)
  } else {
    rep(NA_integer_, n)
  }
  disability_support <- if ("is_dc" %in% names(spine_df)) {
    as.integer(spine_df$is_dc)
  } else {
    rep(NA_integer_, n)
  }
  year_of_death <- if ("year_of_death" %in% names(spine_df)) {
    as.integer(spine_df$year_of_death)
  } else {
    rep(NA_integer_, n)
  }

  # Step 1: Determine who ever participated in HE
  edu <- spine_df$education
  p_he <- .HE_PARTICIPATION_RATE[pmin(pmax(edu + 1L, 1L), 6L)]
  # Those with edu==5 always participate; others random
  is_he <- (edu == 5L) | (runif(n) < p_he)
  he_idx <- which(is_he)

  if (length(he_idx) == 0L) {
    return(.empty_spells())
  }

  # Step 2: Assign primary qualification
  n_he <- length(he_idx)
  qual_idx <- integer(n_he)
  is_edu5 <- edu[he_idx] == 5L

  # Education 5: sample from full qualification distribution
  n5 <- sum(is_edu5)
  if (n5 > 0L) {
    qual_idx[is_edu5] <- sample.int(6L, n5, replace = TRUE,
                                     prob = .HE_QUAL_SHARES_EDU5)
  }
  # Education < 5: sample from attempt distribution
  n_other <- n_he - n5
  if (n_other > 0L) {
    # Normalise attempt shares (drop zeros)
    p_att <- .HE_QUAL_SHARES_ATTEMPT
    p_att[p_att == 0] <- 0.005  # tiny prob to avoid zero
    qual_idx[!is_edu5] <- sample.int(6L, n_other, replace = TRUE,
                                      prob = p_att)
  }

  # Step 3: Commencement year
  birth_year <- spine_df$birth_year[he_idx]
  # Standard entry: birth_year + 18 (with ±1 jitter)
  entry_offset <- sample(17L:19L, n_he, replace = TRUE,
                          prob = c(0.15, 0.50, 0.35))
  # Mature-age: override ~30% of commencing students
  is_mature <- runif(n_he) < 0.30
  if (any(is_mature)) {
    mature_offset <- pmin(pmax(
      as.integer(round(rnorm(sum(is_mature), 28, 6))),
      22L), 50L)
    entry_offset[is_mature] <- mature_offset
  }
  commence_year <- birth_year + entry_offset

  # Postgrad spells for edu==5: some get a second degree
  # ~25% of bachelor holders pursue postgrad (Masters_CW, Masters_Res, PhD, GradDip, Honours)
  # These are already captured in qual_idx — persons with qual > 1 get their

  # primary degree as bachelor first, then postgrad.
  # For simplicity, persons with non-bachelor qual_idx are assumed to have
  # ALSO done a bachelor; we generate only the stated qualification spell.
  # The bachelor is implicit (their education==5 guarantees it).

  # Step 4: Duration and completion
  ft_dur <- .HE_FT_DURATION[qual_idx]
  is_ft <- runif(n_he) < .HE_FT_SHARE
  duration <- ifelse(is_ft, ft_dur, ceiling(ft_dur * 1.8))

  # Completion: determined by qualification-specific rate
  comp_rate <- .HE_COMPLETION_RATE[qual_idx]
  # Education < 5 attempting HE: lower completion
  comp_rate[!is_edu5] <- comp_rate[!is_edu5] * 0.50
  completed <- runif(n_he) < comp_rate
  # Withdrawn: truncated duration
  withdrawn_dur <- pmax(1L, ceiling(duration * runif(n_he, 0.1, 0.8)))
  actual_duration <- ifelse(completed, duration, withdrawn_dur)
  death_year <- year_of_death[he_idx]
  planned_end <- commence_year + actual_duration - 1L
  interrupted <- !is.na(death_year) & planned_end > death_year
  completed[interrupted] <- FALSE
  actual_duration[interrupted] <- pmax(
    death_year[interrupted] - commence_year[interrupted] + 1L,
    0L
  )
  completion_year <- commence_year + actual_duration

  # Step 5: Filter to observation window
  # Must have at least one year of enrolment within the years range
  in_window <- actual_duration > 0L & commence_year <= max_yr &
               (commence_year + actual_duration - 1L) >= min_yr
  keep <- which(in_window)

  if (length(keep) == 0L) {
    return(.empty_spells())
  }

  # Step 6: Assign FOE (archetype-dependent)
  archetype <- spine_df$archetype[he_idx[keep]]
  n_keep <- length(keep)
  foe <- character(n_keep)
  for (a in 0:7) {
    a_idx <- which(archetype == a)
    if (length(a_idx) > 0L) {
      foe[a_idx] <- sample(.HE_FOE_CODES, length(a_idx), replace = TRUE,
                            prob = .HE_ARCHETYPE_FOE[a + 1L, ])
    }
  }
  # Handle any archetypes outside 0-7 (shouldn't happen, but safe)
  missing_foe <- which(nchar(foe) == 0L)
  if (length(missing_foe) > 0L) {
    foe[missing_foe] <- sample(.HE_FOE_CODES, length(missing_foe),
                                replace = TRUE, prob = .HE_FOE_SHARES)
  }

  # Step 7: Assign institution (state-affinity) — vectorized
  person_state <- spine_df$state[he_idx[keep]]
  same_state <- runif(n_keep) < 0.80
  chosen_row <- integer(n_keep)

  # Interstate draws: sample uniformly from all institutions
  n_inter <- sum(!same_state)
  if (n_inter > 0L) {
    chosen_row[!same_state] <- sample.int(nrow(.HE_INSTITUTIONS), n_inter,
                                           replace = TRUE)
  }

  # Same-state draws: group by person_state, sample from state institutions
  if (any(same_state)) {
    n_inst <- nrow(.HE_INSTITUTIONS)
    all_idx <- seq_len(n_inst)
    for (st in unique(person_state[same_state])) {
      in_group <- which(same_state & person_state == st)
      candidates <- which(.HE_INSTITUTIONS$state == st)
      if (length(candidates) == 0L) candidates <- all_idx
      chosen_row[in_group] <- candidates[sample.int(length(candidates),
                                                     length(in_group),
                                                     replace = TRUE)]
    }
  }

  inst_code  <- .HE_INSTITUTIONS$code[chosen_row]
  inst_type  <- .HE_INSTITUTIONS$type[chosen_row]
  inst_state <- .HE_INSTITUTIONS$state[chosen_row]

  # Step 8: Attendance mode
  attend_mode <- sample(1L:3L, n_keep, replace = TRUE,
                         prob = .HE_ATTEND_MODE_WEIGHTS)

  # Build spell data.frame
  spells <- data.frame(
    spine_idx       = he_idx[keep],
    aeuid           = spine_df[[paste0("aeuid_", tolower("DE"))]][he_idx[keep]],
    birth_year      = birth_year[keep],
    sex             = spine_df$sex[he_idx[keep]],
    country_of_birth = spine_df$country_of_birth[he_idx[keep]],
    country_of_birth_sacc = country_of_birth_sacc[he_idx[keep]],
    indigenous      = spine_df$indigenous[he_idx[keep]],
    disability_type = disability_type[he_idx[keep]],
    disability_support = disability_support[he_idx[keep]],
    state           = person_state,
    education       = edu[he_idx[keep]],
    qual_idx        = qual_idx[keep],
    commence_year   = commence_year[keep],
    actual_duration = actual_duration[keep],
    completed       = completed[keep],
    completion_year = completion_year[keep],
    is_ft           = is_ft[keep],
    foe             = foe,
    inst_code       = inst_code,
    inst_type       = inst_type,
    inst_state      = inst_state,
    attend_mode     = attend_mode,
    course_code     = sprintf("C%s%05d", inst_code, seq_len(n_keep)),
    stringsAsFactors = FALSE
  )

  # ---- Disability-driven reskilling: DC workers get +7-10pp HE at k=1-3 ----
  if (!is.null(spine_df$disability_onset_year) &&
      !is.null(spine_df$is_dc)) {
    already_he <- he_idx[keep]
    dc_mask <- !is.na(spine_df$disability_onset_year) &
               !is.na(spine_df$is_dc) & spine_df$is_dc &
               !(seq_len(n) %in% already_he)
    dc_candidates <- which(dc_mask)

    if (length(dc_candidates) > 0L) {
      dc_enrol <- dc_candidates[runif(length(dc_candidates)) < 0.35]

      if (length(dc_enrol) > 0L) {
        n_dc <- length(dc_enrol)
        dc_onset <- spine_df$disability_onset_year[dc_enrol]
        dc_delay <- sample(1L:3L, n_dc, replace = TRUE,
                           prob = c(0.30, 0.50, 0.20))
        dc_commence <- dc_onset + dc_delay

        # Filter to observation window
        dc_ok <- dc_commence >= min_yr & dc_commence <= max_yr
        dc_death <- year_of_death[dc_enrol]
        dc_ok <- dc_ok & (is.na(dc_death) | dc_commence <= dc_death)
        dc_enrol <- dc_enrol[dc_ok]
        dc_commence <- dc_commence[dc_ok]
        dc_death <- dc_death[dc_ok]
        n_dc <- length(dc_enrol)

        if (n_dc > 0L) {
          # Reskilling qualifications: GradDip (60%) or Masters_CW (40%)
          dc_qual <- sample(c(3L, 4L), n_dc, replace = TRUE,
                            prob = c(0.60, 0.40))
          dc_ft_dur <- .HE_FT_DURATION[dc_qual]
          dc_is_ft <- runif(n_dc) < .HE_FT_SHARE
          dc_duration <- ifelse(dc_is_ft, dc_ft_dur,
                                ceiling(dc_ft_dur * 1.8))
          dc_comp_rate <- .HE_COMPLETION_RATE[dc_qual]
          dc_completed <- runif(n_dc) < dc_comp_rate
          dc_wd_dur <- pmax(1L, ceiling(dc_duration *
                                        runif(n_dc, 0.1, 0.8)))
          dc_actual_dur <- ifelse(dc_completed, dc_duration, dc_wd_dur)
          dc_planned_end <- dc_commence + dc_actual_dur - 1L
          dc_interrupted <- !is.na(dc_death) & dc_planned_end > dc_death
          dc_completed[dc_interrupted] <- FALSE
          dc_actual_dur[dc_interrupted] <- pmax(
            dc_death[dc_interrupted] - dc_commence[dc_interrupted] + 1L,
            1L
          )
          dc_comp_year <- dc_commence + dc_actual_dur

          # FOE (archetype-based)
          dc_archetype <- spine_df$archetype[dc_enrol]
          dc_foe <- character(n_dc)
          for (a in 0:7) {
            a_idx <- which(dc_archetype == a)
            if (length(a_idx) > 0L)
              dc_foe[a_idx] <- sample(.HE_FOE_CODES, length(a_idx),
                                       replace = TRUE,
                                       prob = .HE_ARCHETYPE_FOE[a + 1L, ])
          }
          missing <- which(nchar(dc_foe) == 0L)
          if (length(missing) > 0L)
            dc_foe[missing] <- sample(.HE_FOE_CODES, length(missing),
                                       replace = TRUE, prob = .HE_FOE_SHARES)

          # Institution (state-affinity)
          dc_state <- spine_df$state[dc_enrol]
          dc_same <- runif(n_dc) < 0.80
          dc_inst_row <- integer(n_dc)
          n_inter <- sum(!dc_same)
          if (n_inter > 0L)
            dc_inst_row[!dc_same] <- sample.int(nrow(.HE_INSTITUTIONS),
                                                 n_inter, replace = TRUE)
          if (any(dc_same)) {
            for (st in unique(dc_state[dc_same])) {
              ig <- which(dc_same & dc_state == st)
              cands <- which(.HE_INSTITUTIONS$state == st)
              if (length(cands) == 0L) cands <- seq_len(nrow(.HE_INSTITUTIONS))
              dc_inst_row[ig] <- cands[sample.int(length(cands),
                                                   length(ig), replace = TRUE)]
            }
          }

          dc_inst_code <- .HE_INSTITUTIONS$code[dc_inst_row]
          dc_attend <- sample(1L:3L, n_dc, replace = TRUE,
                              prob = .HE_ATTEND_MODE_WEIGHTS)

          dc_spells <- data.frame(
            spine_idx       = dc_enrol,
            aeuid           = spine_df$aeuid_de[dc_enrol],
            birth_year      = spine_df$birth_year[dc_enrol],
            sex             = spine_df$sex[dc_enrol],
            country_of_birth = spine_df$country_of_birth[dc_enrol],
            country_of_birth_sacc = country_of_birth_sacc[dc_enrol],
            indigenous      = spine_df$indigenous[dc_enrol],
            disability_type = disability_type[dc_enrol],
            disability_support = disability_support[dc_enrol],
            state           = dc_state,
            education       = edu[dc_enrol],
            qual_idx        = dc_qual,
            commence_year   = dc_commence,
            actual_duration = dc_actual_dur,
            completed       = dc_completed,
            completion_year = dc_comp_year,
            is_ft           = dc_is_ft,
            foe             = dc_foe,
            inst_code       = dc_inst_code,
            inst_type       = .HE_INSTITUTIONS$type[dc_inst_row],
            inst_state      = .HE_INSTITUTIONS$state[dc_inst_row],
            attend_mode     = dc_attend,
            course_code     = sprintf("C%s%05d", dc_inst_code,
                                       n_keep + seq_len(n_dc)),
            stringsAsFactors = FALSE
          )
          spells <- rbind(spells, dc_spells)
        }
      }
    }
  }

  spells
}


#' Empty spells data.frame
#' @keywords internal
.empty_spells <- function() {
  data.frame(
    spine_idx = integer(0), aeuid = character(0),
    birth_year = integer(0), sex = integer(0),
    country_of_birth = integer(0), country_of_birth_sacc = integer(0),
    indigenous = integer(0), disability_type = integer(0),
    disability_support = integer(0),
    state = integer(0), education = integer(0),
    qual_idx = integer(0), commence_year = integer(0),
    actual_duration = integer(0), completed = logical(0),
    completion_year = integer(0), is_ft = logical(0),
    foe = character(0), inst_code = character(0),
    inst_type = integer(0), inst_state = integer(0),
    attend_mode = integer(0), course_code = character(0),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Projection: student enrolment (student-year)
# =============================================================================

#' Project HE student enrolment records
#'
#' Expands spells to one row per student per year of enrolment.
#'
#' @param spells data.frame from select_he_participants().
#' @param spine_df data.frame from generate_spine().
#' @param seed Integer seed.
#' @return data.frame matching hes_madip_student_enrol schema.
#' @keywords internal
project_he_enrol <- function(spells, spine_df, seed, yr_range) {
  if (nrow(spells) == 0L) return(.empty_he_enrol())

  if (exists("project_he_enrol__", mode = "function")) {
    raw <- project_he_enrol__(
      spell_aeuid            = as.character(spells$aeuid),
      spell_commence_year    = as.integer(spells$commence_year),
      spell_actual_duration  = as.integer(spells$actual_duration),
      spell_is_ft            = as.integer(spells$is_ft),
      spell_course_code      = as.character(spells$course_code),
      spell_inst_code        = as.character(spells$inst_code),
      spell_attend_mode      = as.integer(spells$attend_mode),
      spell_sex              = as.integer(spells$sex),
      spell_country_of_birth = as.integer(spells$country_of_birth_sacc),
      spell_indigenous       = as.integer(spells$indigenous),
      spell_disability_type  = as.integer(spells$disability_type),
      spell_disability_support = as.integer(spells$disability_support),
      spell_education        = as.integer(spells$education),
      spell_birth_year       = as.integer(spells$birth_year),
      min_year               = as.integer(yr_range[1L]),
      max_year               = as.integer(yr_range[2L])
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 801L)

  # Expand spells to student-year rows — vectorized
  n_sp <- nrow(spells)
  yr_start <- pmax(spells$commence_year, yr_range[1])
  yr_end   <- pmin(spells$commence_year + spells$actual_duration - 1L,
                    yr_range[2])
  n_years  <- pmax(yr_end - yr_start + 1L, 0L)

  # Filter out spells with no active years
  active <- n_years > 0L
  if (!any(active)) return(.empty_he_enrol())

  yr_start <- yr_start[active]
  yr_end   <- yr_end[active]
  n_years  <- n_years[active]
  sp_idx   <- which(active)
  total    <- sum(n_years)

  # Expansion index: which spell does each row come from
  spell_exp <- rep(sp_idx, n_years)

  # Generate year values: yr_start[i], yr_start[i]+1, ..., yr_end[i]
  yrs <- sequence(n_years, from = yr_start, by = 1L)

  # Pre-compute per-spell constants
  attend_type <- ifelse(spells$is_ft, 1L, 2L)
  country_str <- sprintf("%04d", as.integer(spells$country_of_birth_sacc))
  country_str[is.na(spells$country_of_birth_sacc)] <- "9999"
  aborig <- c(`1` = 2L, `2` = 3L, `3` = 4L, `4` = 5L)[
    as.character(spells$indigenous)
  ]
  aborig[is.na(aborig)] <- 9L
  disability <- .he_disability_code(
    spells$disability_type, spells$disability_support
  )
  yr_left     <- spells$birth_year + 18L

  data.frame(
    SYNTHETIC_AEUID      = spells$aeuid[spell_exp],
    YEAR                 = yrs,
    COURSE               = spells$course_code[spell_exp],
    INSTITUTION          = spells$inst_code[spell_exp],
    ATTENDANCE_MODE      = spells$attend_mode[spell_exp],
    ATTENDANCE_TYPE      = attend_type[spell_exp],
    COM_INDICATOR        = as.integer(yrs == spells$commence_year[spell_exp]),
    GENDER               = spells$sex[spell_exp],
    COUNTRY_BIRTH        = country_str[spell_exp],
    ABORIG_TORRES        = aborig[spell_exp],
    DISABILITY           = disability[spell_exp],
    HIGHEST_PARTICIPATION = spells$education[spell_exp],
    YEAR_LEFT_SCHOOL     = yr_left[spell_exp],
    REPORTING_YEAR_PERIOD = paste0(yrs, "-1"),
    MAJOR_COURSE         = rep(1L, total),
    stringsAsFactors     = FALSE
  )
}


#' Empty enrol data.frame
#' @keywords internal
.empty_he_enrol <- function() {
  data.frame(
    SYNTHETIC_AEUID = character(0), YEAR = integer(0),
    COURSE = character(0), INSTITUTION = character(0),
    ATTENDANCE_MODE = integer(0), ATTENDANCE_TYPE = integer(0),
    COM_INDICATOR = integer(0), GENDER = integer(0),
    COUNTRY_BIRTH = character(0), ABORIG_TORRES = integer(0),
    DISABILITY = character(0), HIGHEST_PARTICIPATION = integer(0),
    YEAR_LEFT_SCHOOL = integer(0), REPORTING_YEAR_PERIOD = character(0),
    MAJOR_COURSE = integer(0), stringsAsFactors = FALSE
  )
}


# =============================================================================
# Projection: student course (one row per spell)
# =============================================================================

#' Project HE student course records
#' @param spells data.frame from select_he_participants().
#' @param seed Integer seed.
#' @return data.frame matching hes_madip_student_course schema.
#' @keywords internal
project_he_course <- function(spells, seed) {
  if (nrow(spells) == 0L) return(.empty_he_course())

  if (exists("project_he_course__", mode = "function")) {
    raw <- project_he_course__(
      spell_aeuid            = as.character(spells$aeuid),
      spell_commence_year    = as.integer(spells$commence_year),
      spell_course_code      = as.character(spells$course_code),
      spell_qual_idx         = as.integer(spells$qual_idx),
      spell_foe              = as.character(spells$foe),
      spell_inst_code        = as.character(spells$inst_code),
      spell_is_ft            = as.integer(spells$is_ft),
      spell_actual_duration  = as.integer(spells$actual_duration)
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }

  n_sp <- nrow(spells)
  data.frame(
    SYNTHETIC_AEUID    = spells$aeuid,
    YEAR               = spells$commence_year,
    COURSE             = spells$course_code,
    COURSE_OF_STUDY_CODE = sprintf("NCS%06d", seq_len(n_sp)),
    COURSE_TYPE        = .HE_COURSE_TYPES[spells$qual_idx],
    FOE                = spells$foe,
    FOE_SUPP           = paste0(spells$foe, "00"),
    INSTITUTION        = spells$inst_code,
    SPECIAL_COURSE     = rep(0L, n_sp),
    COURSE_LOAD        = ifelse(spells$is_ft, 1.0, 0.5) *
                          spells$actual_duration,
    stringsAsFactors   = FALSE
  )
}


#' Empty course data.frame
#' @keywords internal
.empty_he_course <- function() {
  data.frame(
    SYNTHETIC_AEUID = character(0), YEAR = integer(0),
    COURSE = character(0), COURSE_OF_STUDY_CODE = character(0),
    COURSE_TYPE = integer(0), FOE = character(0),
    FOE_SUPP = character(0), INSTITUTION = character(0),
    SPECIAL_COURSE = integer(0), COURSE_LOAD = numeric(0),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Projection: student load (unit-of-study)
# =============================================================================

#' Project HE student load records
#'
#' Expands each spell-year into unit-of-study rows.
#' Full-time: 8 units/year (0.125 EFTSL each). Part-time: 4 units/year.
#'
#' @param spells data.frame from select_he_participants().
#' @param seed Integer seed.
#' @return data.frame matching hes_madip_student_load schema.
#' @keywords internal
project_he_load <- function(spells, seed, yr_range) {
  if (nrow(spells) == 0L) return(.empty_he_load())

  # Use Rust implementation if available (O(n) vs O(n²) in pure R)
  if (exists("project_he_load__", mode = "function")) {
    raw <- project_he_load__(
      spell_aeuid            = as.character(spells$aeuid),
      spell_commence_year    = as.integer(spells$commence_year),
      spell_actual_duration  = as.integer(spells$actual_duration),
      spell_completed        = as.integer(spells$completed),
      spell_is_ft            = as.integer(spells$is_ft),
      spell_qual_idx         = as.integer(spells$qual_idx),
      spell_foe              = as.character(spells$foe),
      spell_inst_code        = as.character(spells$inst_code),
      spell_inst_state       = as.integer(spells$inst_state),
      spell_country_of_birth = as.integer(spells$country_of_birth),
      spell_attend_mode      = as.integer(spells$attend_mode),
      spell_course_code      = as.character(spells$course_code),
      min_year               = yr_range[1L],
      max_year               = yr_range[2L],
      seed                   = as.integer(seed)
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }

  old_seed <- if (exists(".Random.seed", globalenv())) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
    else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed + 803L)

  n_sp <- nrow(spells)

  # --- Step 1: Expand spells to spell-year level (vectorized) ---
  yr_start <- pmax(spells$commence_year, yr_range[1])
  yr_end   <- pmin(spells$commence_year + spells$actual_duration - 1L,
                    yr_range[2])
  n_years  <- pmax(yr_end - yr_start + 1L, 0L)

  active <- n_years > 0L
  if (!any(active)) return(.empty_he_load())

  yr_start <- yr_start[active]
  yr_end   <- yr_end[active]
  n_years  <- n_years[active]
  sp_idx   <- which(active)
  n_active <- length(sp_idx)

  # Spell-level constants (one RNG draw per active spell)
  is_csp <- runif(n_active) < .HE_CSP_SHARE
  upfront_draw <- runif(n_active)

  units_per_yr  <- ifelse(spells$is_ft[sp_idx], 8L, 4L)
  student_status <- ifelse(is_csp, 10L, 11L)
  annual_help   <- .HE_ANNUAL_HELP[spells$qual_idx[sp_idx]]
  help_per_unit <- annual_help / units_per_yr
  loan_fee_per_unit <- ifelse(is_csp, 0, help_per_unit * 0.20)
  unit_charge   <- ifelse(is_csp,
                           .HE_CSP_ANNUAL / units_per_yr,
                           .HE_FULL_FEE_ANNUAL / units_per_yr)
  pays_upfront  <- (!is_csp) & (upfront_draw < .HE_UPFRONT_SHARE)
  upfront_per_unit <- ifelse(pays_upfront, unit_charge, 0)

  # Campus postcode lookup (vectorized), STE 2021 order:
  # 1 NSW, 2 VIC, 3 QLD, 4 SA, 5 WA, 6 TAS, 7 NT, 8 ACT
  .STATE_TO_PC <- c("2000", "3000", "4000", "5000",
                     "6000", "7000", "0800", "2600")
  campus_pc <- .STATE_TO_PC[pmin(pmax(spells$inst_state[sp_idx], 1L), 8L)]
  cit_res   <- ifelse(spells$country_of_birth[sp_idx] == 0L, 1L, 2L)

  # Expand to spell-year level
  sy_spell <- rep(seq_len(n_active), n_years)  # which active spell
  sy_year  <- sequence(n_years, from = yr_start, by = 1L)
  total_sy <- length(sy_spell)

  # --- Step 2: Expand spell-years to unit level ---
  sy_units <- units_per_yr[sy_spell]
  total_units <- sum(sy_units)
  unit_sy  <- rep(seq_len(total_sy), sy_units)  # which spell-year
  unit_spell <- sy_spell[unit_sy]  # which active spell (for spell constants)

  # Unit index within each spell-year (1..units_per_yr)
  unit_within <- sequence(sy_units)

  # Year for each unit
  unit_year <- sy_year[unit_sy]

  # --- Step 3: Unit status (withdrawn units in final year) ---
  is_final_yr <- unit_year == yr_end[unit_spell] & !spells$completed[sp_idx[unit_spell]]
  unit_status <- rep(4L, total_units)

  if (any(is_final_yr)) {
    # For each spell-year that is a withdrawn final year, draw withdrawal cutoff
    final_sy <- unique(unit_sy[is_final_yr])
    n_final <- length(final_sy)
    upy_final <- units_per_yr[sy_spell[final_sy]]
    # Draw random cutoff per final spell-year (vectorized via runif)
    n_cut <- pmax(1L, as.integer(ceiling(runif(n_final) * upy_final)) - 1L)

    # Mark units after cutoff as withdrawn (status 7)
    for (k in seq_len(n_final)) {
      sy_k <- final_sy[k]
      rows_k <- which(unit_sy == sy_k)
      if (n_cut[k] < length(rows_k)) {
        unit_status[rows_k[(n_cut[k] + 1L):length(rows_k)]] <- 7L
      }
    }
  }

  # --- Step 4: Census dates ---
  sem1_count <- ceiling(units_per_yr[unit_spell] / 2L)
  is_sem1 <- unit_within <= sem1_count
  census_dates <- ifelse(is_sem1,
                          sprintf("%d-03-31", unit_year),
                          sprintf("%d-08-31", unit_year))

  # --- Step 5: Unit codes ---
  # Cumulative unit position within each spell group (O(n) vectorized).
  # sy_spell is sorted (contiguous per spell from rep()), so we can use
  # group-boundary detection + cumsum subtraction.
  is_group_start <- c(TRUE, diff(sy_spell) != 0L)
  group_id <- cumsum(is_group_start)
  prev_cumsum <- cumsum(c(0L, sy_units[-total_sy]))
  group_start_idx <- which(is_group_start)
  group_start_cumsum <- prev_cumsum[group_start_idx]
  sy_units_before <- prev_cumsum - group_start_cumsum[group_id]
  unit_global_idx <- sy_units_before[unit_sy] + unit_within

  unit_codes <- sprintf("U%04s%04d%02d",
                         spells$inst_code[sp_idx[unit_spell]],
                         unit_global_idx,
                         unit_year %% 100L)

  # --- Build data.frame ---
  data.frame(
    SYNTHETIC_AEUID            = spells$aeuid[sp_idx[unit_spell]],
    YEAR                       = unit_year,
    COURSE                     = spells$course_code[sp_idx[unit_spell]],
    INSTITUTION                = spells$inst_code[sp_idx[unit_spell]],
    UNIT_STUDY                 = unit_codes,
    DISCIPLINE_CODE            = spells$foe[sp_idx[unit_spell]],
    EQUIVALENT_FT_STUDENT_LOAD = rep(0.125, total_units),
    STUDENT_STATUS             = student_status[unit_spell],
    UNIT_STATUS                = unit_status,
    MODE_ATTENDANCE            = spells$attend_mode[sp_idx[unit_spell]],
    HELP_DEBT                  = help_per_unit[unit_spell],
    LOAN_FEE                   = loan_fee_per_unit[unit_spell],
    TOTAL_AMOUNT_CHARGED       = unit_charge[unit_spell],
    AMOUNT_PAID_UPFRONT        = upfront_per_unit[unit_spell],
    CAMPUS_STATE               = spells$inst_state[sp_idx[unit_spell]],
    CAMPUS_POSTCODE            = campus_pc[unit_spell],
    CITIZEN_RESIDENT           = cit_res[unit_spell],
    UNIT_STUDY_CENSUS          = census_dates,
    SUMMER_SCHOOL_INDICATOR    = rep(0L, total_units),
    INDUSTRY                   = rep(0L, total_units),
    stringsAsFactors           = FALSE
  )
}


#' Empty load data.frame
#' @keywords internal
.empty_he_load <- function() {
  data.frame(
    SYNTHETIC_AEUID = character(0), YEAR = integer(0),
    COURSE = character(0), INSTITUTION = character(0),
    UNIT_STUDY = character(0), DISCIPLINE_CODE = character(0),
    EQUIVALENT_FT_STUDENT_LOAD = numeric(0),
    STUDENT_STATUS = integer(0), UNIT_STATUS = integer(0),
    MODE_ATTENDANCE = integer(0), HELP_DEBT = numeric(0),
    LOAN_FEE = numeric(0), TOTAL_AMOUNT_CHARGED = numeric(0),
    AMOUNT_PAID_UPFRONT = numeric(0), CAMPUS_STATE = integer(0),
    CAMPUS_POSTCODE = character(0), CITIZEN_RESIDENT = integer(0),
    UNIT_STUDY_CENSUS = character(0),
    SUMMER_SCHOOL_INDICATOR = integer(0), INDUSTRY = integer(0),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Projection: student completions
# =============================================================================

#' Project HE student completion records
#' @param spells data.frame from select_he_participants().
#' @return data.frame matching hes_madip_student_completions schema.
#' @keywords internal
project_he_completions <- function(spells, yr_range) {
  if (nrow(spells) == 0L) return(.empty_he_completions())

  if (exists("project_he_completions__", mode = "function")) {
    raw <- project_he_completions__(
      spell_aeuid            = as.character(spells$aeuid),
      spell_completed        = as.integer(spells$completed),
      spell_completion_year  = as.integer(spells$completion_year),
      spell_course_code      = as.character(spells$course_code),
      spell_qual_idx         = as.integer(spells$qual_idx),
      spell_foe              = as.character(spells$foe),
      spell_inst_code        = as.character(spells$inst_code),
      max_year               = as.integer(yr_range[2L])
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }

  comp <- spells[spells$completed & spells$completion_year <= yr_range[2], ,
                 drop = FALSE]
  if (nrow(comp) == 0L) return(.empty_he_completions())

  data.frame(
    SYNTHETIC_AEUID = comp$aeuid,
    YEAR            = comp$completion_year,
    COURSE_CODE     = comp$course_code,
    COURSE_TYPE     = .HE_COURSE_TYPES[comp$qual_idx],
    FOE             = comp$foe,
    FOE_SUPP        = paste0(comp$foe, "00"),
    INSTITUTION     = comp$inst_code,
    stringsAsFactors = FALSE
  )
}


#' Empty completions data.frame
#' @keywords internal
.empty_he_completions <- function() {
  data.frame(
    SYNTHETIC_AEUID = character(0), YEAR = integer(0),
    COURSE_CODE = character(0), COURSE_TYPE = integer(0),
    FOE = character(0), FOE_SUPP = character(0),
    INSTITUTION = character(0), stringsAsFactors = FALSE
  )
}


# =============================================================================
# Projection: student HELP (aggregated from load)
# =============================================================================

#' Project HE HELP loan records
#'
#' Aggregates unit-level HELP debt to student-year level.
#'
#' @param load_df data.frame from project_he_load().
#' @return data.frame matching hes_madip_student_help schema.
#' @keywords internal
project_he_help <- function(load_df) {
  if (nrow(load_df) == 0L) return(.empty_he_help())

  if (exists("project_he_help__", mode = "function")) {
    raw <- project_he_help__(
      load_aeuid          = as.character(load_df$SYNTHETIC_AEUID),
      load_year           = as.integer(load_df$YEAR),
      load_institution    = as.character(load_df$INSTITUTION),
      load_help_debt      = as.double(load_df$HELP_DEBT),
      load_student_status = as.integer(load_df$STUDENT_STATUS),
      load_loan_fee       = as.double(load_df$LOAN_FEE)
    )
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }

  # Only include rows with HELP debt > 0
  has_help <- load_df$HELP_DEBT > 0
  if (!any(has_help)) return(.empty_he_help())

  ld <- load_df[has_help, , drop = FALSE]

  # Aggregate by SYNTHETIC_AEUID + YEAR + INSTITUTION — using rowsum()
  key <- paste(ld$SYNTHETIC_AEUID, ld$YEAR, ld$INSTITUTION, sep = "|")
  sums <- rowsum(cbind(ld$HELP_DEBT, ld$LOAN_FEE), key, reorder = FALSE)
  first_idx <- match(rownames(sums), key)

  data.frame(
    SYNTHETIC_AEUID = ld$SYNTHETIC_AEUID[first_idx],
    YEAR            = ld$YEAR[first_idx],
    INSTITUTION     = ld$INSTITUTION[first_idx],
    HELP_DEBT       = unname(sums[, 1]),
    STUDENT_STATUS  = ld$STUDENT_STATUS[first_idx],
    LOAN_FEE        = unname(sums[, 2]),
    stringsAsFactors = FALSE
  )
}


#' Empty HELP data.frame
#' @keywords internal
.empty_he_help <- function() {
  data.frame(
    SYNTHETIC_AEUID = character(0), YEAR = integer(0),
    INSTITUTION = character(0), HELP_DEBT = numeric(0),
    STUDENT_STATUS = integer(0), LOAN_FEE = numeric(0),
    stringsAsFactors = FALSE
  )
}

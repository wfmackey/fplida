#' Generate longitudinal employment panel from spine
#'
#' Walks backward and forward from the 2021 spine anchor to produce a
#' person-year employment panel. Job-to-job transition rates are calibrated
#' to Deutscher (2019); earnings follow a log-normal process with age,
#' occupation, and individual fixed effects.
#'
#' @param spine Data.frame from \code{generate_spine()}.
#' @param seed Integer. Base seed (uses \code{seed + 300L} internally).
#' @param years Integer vector of financial-year end years
#'   (e.g. 2021 = FY 2020-21). Default 2015:2024.
#' @param target_year Integer or NULL. Optional single financial-year
#'   end year to return after generating the panel.
#' @return A data.frame with one row per employed person-year (plus secondary
#'   job rows for multi-job holders). Columns: person_id, aeuid_ato, year,
#'   employer_id, primary_job, anzsco_major, industry, hours_weekly,
#'   gross_annual, switched_employer, new_to_employment.
#' @export
generate_employment_panel <- function(spine, seed = 42L, years = 2010:2024,
                                      target_year = NULL) {
  seed <- as.integer(seed)
  stopifnot(is.data.frame(spine), !is.na(seed))
  years <- sort(as.integer(years))
  stopifnot(2021L %in% years)
  if (!is.null(target_year)) {
    target_year <- as.integer(target_year)
    stopifnot(target_year %in% years)
  }

  # Try Rust implementation first
  if (exists("generate_employment_panel__", mode = "function")) {
    n <- nrow(spine)
    # Defensive: provide NA vectors if disability columns are missing
    dis_onset <- if (!is.null(spine$disability_onset_year))
      as.integer(spine$disability_onset_year) else rep(NA_integer_, n)
    dis_dc <- if (!is.null(spine$is_dc))
      as.integer(spine$is_dc) else rep(NA_integer_, n)
    dis_sev <- if (!is.null(spine$disability_severity))
      as.integer(spine$disability_severity) else rep(NA_integer_, n)
    dis_dose <- if (!is.null(spine$disability_dose))
      as.double(spine$disability_dose) else rep(NA_real_, n)

    # Defensive: provide defaults for new columns if missing
    anz_code <- if (!is.null(spine$anzsco_code))
      as.integer(spine$anzsco_code) else as.integer(spine$anzsco_major * 1000L)
    t_phys <- if (!is.null(spine$task_physical))
      as.double(spine$task_physical) else rep(0.3, n)
    arch <- if (!is.null(spine$archetype))
      as.integer(spine$archetype) else rep(0L, n)

    raw <- generate_employment_panel__(
      id                    = as.character(spine$id),
      aeuid_ato             = as.character(spine$aeuid_ato),
      birth_year            = as.integer(spine$birth_year),
      baseline_employed     = as.integer(spine$baseline_employed),
      baseline_income       = as.double(spine$baseline_income),
      baseline_hours        = as.integer(spine$baseline_hours),
      anzsco_major          = as.integer(spine$anzsco_major),
      industry              = as.integer(spine$industry),
      seed                  = seed,
      years                 = years,
      disability_onset_year = dis_onset,
      disability_is_dc      = dis_dc,
      disability_severity   = dis_sev,
      disability_dose       = dis_dose,
      target_year           = if (is.null(target_year)) 0L else target_year,
      anzsco_code           = anz_code,
      task_physical         = t_phys,
      archetype             = arch
    )
    panel <- as.data.frame(raw, stringsAsFactors = FALSE)
    # Sort by person and year (skip when streaming single year — caller sorts)
    if (is.null(target_year)) {
      panel <- panel[order(panel$person_id, panel$year, !panel$primary_job), ]
    }
    rownames(panel) <- NULL
    return(panel)
  }

  set.seed(seed + 300L)

  n <- nrow(spine)
  anchor_year <- 2021L

  # Pre-compute person-level constants
  age_2021   <- anchor_year - spine$birth_year
  employed   <- spine$baseline_employed
  log_income <- ifelse(spine$baseline_income > 0,
                       log(spine$baseline_income), NA_real_)

  # Initialize per-person state vectors (2021 anchor)
  cur_employed   <- employed
  cur_spell      <- integer(n)  # employer spell counter
  cur_log_earn   <- log_income
  cur_hours      <- spine$baseline_hours
  cur_occ_major  <- spine$anzsco_major
  cur_industry   <- spine$industry

  # Collect panel rows in a list (faster than growing a data.frame)
  panel_rows <- vector("list", length(years))

  # -- Helper: Deutscher age-band switch probability -----------------------
  switch_prob <- function(age) {
    # 11 bands: 15-19, 20-24, ..., 60-64, 65+
    rates <- c(0.28, 0.22, 0.16, 0.12, 0.10,
               0.09, 0.08, 0.07, 0.06, 0.05, 0.04)
    band <- pmin(pmax(((age - 15L) %/% 5L) + 1L, 1L), 11L)
    rates[band]
  }

  # -- Helper: age-specific employment exit/entry probabilities ------------
  exit_prob <- function(age) {
    # Probability of employed -> not-employed (annual)
    # Low for prime age, higher for young/old
    base <- rep(0.04, length(age))
    base[age < 25] <- 0.10
    base[age >= 60] <- 0.08
    base[age >= 65] <- 0.15
    base[age < 15]  <- 1.0   # children never employed
    base
  }

  entry_prob <- function(age) {
    # Probability of not-employed -> employed (annual)
    base <- rep(0.30, length(age))
    base[age < 20] <- 0.15
    base[age >= 60] <- 0.10
    base[age >= 65] <- 0.05
    base[age >= 70] <- 0.02
    base[age < 15]  <- 0.0
    base
  }

  # -- Helper: deterministic employer ABN from person + spell + seed -------
  make_employer_abn <- function(person_idx, spell, seed) {
    h <- (as.numeric(person_idx) * 1000003 + spell * 999983 + seed) %%
         (10^11)
    .bn_for_hash_r(abs(h), sprintf("%011.0f", abs(h)))
  }

  # -- Helper: collect one year's rows into a data.frame -------------------
  collect_year <- function(yr, emp, spell, log_earn, hours, occ_major,
                           industry, switched, new_entry) {
    mask <- emp
    if (!any(mask)) return(NULL)
    data.frame(
      person_id  = spine$id[mask],
      aeuid_ato  = spine$aeuid_ato[mask],
      year       = yr,
      employer_id = make_employer_abn(which(mask), spell[mask], seed),
      primary_job = TRUE,
      anzsco_major = occ_major[mask],
      industry     = industry[mask],
      hours_weekly = hours[mask],
      gross_annual = pmax(round(exp(log_earn[mask]), 2), 1),
      switched_employer  = switched[mask],
      new_to_employment  = new_entry[mask],
      stringsAsFactors = FALSE
    )
  }

  # ======================================================================
  # STEP 1: Record 2021 anchor
  # ======================================================================
  switched_2021 <- rep(FALSE, n)
  new_entry_2021 <- rep(FALSE, n)
  anchor_idx <- which(years == anchor_year)

  panel_rows[[anchor_idx]] <- collect_year(
    anchor_year, cur_employed, cur_spell, cur_log_earn, cur_hours,
    cur_occ_major, cur_industry, switched_2021, new_entry_2021
  )

  # Save 2021 state for forward walk
  anchor_employed  <- cur_employed
  anchor_spell     <- cur_spell
  anchor_log_earn  <- cur_log_earn
  anchor_hours     <- cur_hours
  anchor_occ_major <- cur_occ_major
  anchor_industry  <- cur_industry

  # ======================================================================
  # STEP 2: Walk backward (2020, 2019, ..., first year)
  # ======================================================================
  backward_years <- years[years < anchor_year]
  if (length(backward_years) > 0) {
    backward_years <- sort(backward_years, decreasing = TRUE)
    for (yr in backward_years) {
      age_yr <- age_2021 - (anchor_year - yr)
      yr_idx <- which(years == yr)

      # --- Employment transitions (looking backward) ---
      # If employed at t+1, were they employed at t?
      # Use high retention (1 - exit_prob)
      p_stay_emp  <- 1 - exit_prob(age_yr)
      # If NOT employed at t+1, were they employed at t?
      # Some fraction were (they exited between t and t+1)
      p_was_emp   <- exit_prob(age_yr) * 0.5  # rough: half who exit were employed year before

      new_employed <- logical(n)
      u_emp <- runif(n)
      # Currently employed at t+1 -> probably employed at t
      new_employed[cur_employed] <- u_emp[cur_employed] < p_stay_emp[cur_employed]
      # Currently not employed at t+1 -> maybe employed at t
      new_employed[!cur_employed] <- u_emp[!cur_employed] < p_was_emp[!cur_employed]
      # Children never employed
      new_employed[age_yr < 15] <- FALSE

      # --- Job switching (employed at both t and t+1) ---
      both_emp <- new_employed & cur_employed
      p_switch <- switch_prob(age_yr)
      switched <- both_emp & (runif(n) < p_switch)

      # Update spell for switchers (going backward = different prior employer)
      new_spell <- cur_spell
      new_spell[switched] <- new_spell[switched] + 1L

      # --- Earnings (walk backward) ---
      # log(Y_t) = log(Y_{t+1}) - growth - transitory_shock
      # Growth ~ 5.8% nominal for stayers, higher for switchers
      stayer_growth <- rnorm(n, mean = 0.058, sd = 0.10)
      switcher_premium <- rnorm(n, mean = 0.10, sd = 0.05)

      new_log_earn <- cur_log_earn
      # Stayers: subtract forward growth to get prior earnings
      stayer_mask <- new_employed & cur_employed & !switched
      new_log_earn[stayer_mask] <- cur_log_earn[stayer_mask] -
        stayer_growth[stayer_mask]
      # Switchers: subtract growth + premium
      new_log_earn[switched] <- cur_log_earn[switched] -
        stayer_growth[switched] - switcher_premium[switched]
      # New-to-employment at t (employed at t but not at t+1)
      newly_emp_t <- new_employed & !cur_employed
      if (any(newly_emp_t)) {
        # Assign earnings based on spine baseline (adjusted for distance)
        yrs_from_anchor <- anchor_year - yr
        new_log_earn[newly_emp_t] <- ifelse(
          !is.na(log_income[newly_emp_t]),
          log_income[newly_emp_t] - 0.058 * yrs_from_anchor +
            rnorm(sum(newly_emp_t), 0, 0.15),
          rnorm(sum(newly_emp_t), mean = 10.8, sd = 0.5)
        )
      }

      # --- Hours (small variation) ---
      new_hours <- cur_hours
      # Randomly vary hours for some
      h_change <- runif(n) < 0.10
      new_hours[new_employed & h_change] <- ifelse(
        runif(sum(new_employed & h_change)) < 0.65, 38L, 20L
      )

      # Track new-to-employment (employed at t but NOT at t+1)
      new_entry <- new_employed & !cur_employed

      # Record this year
      panel_rows[[yr_idx]] <- collect_year(
        yr, new_employed, new_spell, new_log_earn, new_hours,
        cur_occ_major, cur_industry, switched, new_entry
      )

      # Update state for next backward step
      cur_employed  <- new_employed
      cur_spell     <- new_spell
      cur_log_earn  <- new_log_earn
      cur_hours     <- new_hours
    }
  }

  # ======================================================================
  # STEP 3: Walk forward (2022, 2023, 2024, ...)
  # ======================================================================
  # Reset state to 2021 anchor
  cur_employed  <- anchor_employed
  cur_spell     <- anchor_spell
  cur_log_earn  <- anchor_log_earn
  cur_hours     <- anchor_hours
  cur_occ_major <- anchor_occ_major
  cur_industry  <- anchor_industry

  forward_years <- years[years > anchor_year]
  if (length(forward_years) > 0) {
    forward_years <- sort(forward_years)
    for (yr in forward_years) {
      age_yr <- age_2021 + (yr - anchor_year)
      yr_idx <- which(years == yr)

      # --- Employment transitions ---
      p_stay_emp <- 1 - exit_prob(age_yr)
      p_enter    <- entry_prob(age_yr)

      new_employed <- logical(n)
      u_emp <- runif(n)
      new_employed[cur_employed] <- u_emp[cur_employed] < p_stay_emp[cur_employed]
      new_employed[!cur_employed] <- u_emp[!cur_employed] < p_enter[!cur_employed]
      new_employed[age_yr < 15] <- FALSE

      # --- Job switching ---
      both_emp <- new_employed & cur_employed
      p_switch <- switch_prob(age_yr)
      switched <- both_emp & (runif(n) < p_switch)

      new_spell <- cur_spell
      new_spell[switched] <- new_spell[switched] + 1L

      # --- Earnings ---
      stayer_growth <- rnorm(n, mean = 0.058, sd = 0.10)
      switcher_premium <- rnorm(n, mean = 0.10, sd = 0.05)

      new_log_earn <- cur_log_earn
      stayer_mask <- new_employed & cur_employed & !switched
      new_log_earn[stayer_mask] <- cur_log_earn[stayer_mask] +
        stayer_growth[stayer_mask]
      new_log_earn[switched] <- cur_log_earn[switched] +
        stayer_growth[switched] + switcher_premium[switched]

      # New entrants
      new_entry <- new_employed & !cur_employed
      if (any(new_entry)) {
        yrs_from_anchor <- yr - anchor_year
        new_log_earn[new_entry] <- ifelse(
          !is.na(log_income[new_entry]),
          log_income[new_entry] + 0.058 * yrs_from_anchor +
            rnorm(sum(new_entry), 0, 0.15),
          rnorm(sum(new_entry), mean = 10.8, sd = 0.5)
        )
        new_spell[new_entry] <- new_spell[new_entry] + 1L
      }

      # --- Hours ---
      new_hours <- cur_hours
      h_change <- runif(n) < 0.10
      new_hours[new_employed & h_change] <- ifelse(
        runif(sum(new_employed & h_change)) < 0.65, 38L, 20L
      )

      panel_rows[[yr_idx]] <- collect_year(
        yr, new_employed, new_spell, new_log_earn, new_hours,
        cur_occ_major, cur_industry, switched, new_entry
      )

      cur_employed  <- new_employed
      cur_spell     <- new_spell
      cur_log_earn  <- new_log_earn
      cur_hours     <- new_hours
    }
  }

  # ======================================================================
  # STEP 4: Combine and add multi-job expansion
  # ======================================================================
  panel <- do.call(rbind, panel_rows[!vapply(panel_rows, is.null, logical(1))])
  rownames(panel) <- NULL

  # Multi-job expansion: ~7% of employed person-years get a secondary job
  n_panel <- nrow(panel)
  multi_mask <- runif(n_panel) < 0.07
  if (any(multi_mask)) {
    secondary <- panel[multi_mask, , drop = FALSE]
    secondary$primary_job <- FALSE
    # Secondary earnings: 20-40% of primary
    sec_share <- runif(sum(multi_mask), 0.20, 0.40)
    secondary$gross_annual <- round(secondary$gross_annual * sec_share, 2)
    # Different employer
    secondary$employer_id <- make_employer_abn(
      match(secondary$person_id, spine$id),
      spell = 9999L,  # distinct spell for secondary
      seed = seed + 500L
    )
    secondary$switched_employer <- FALSE
    secondary$new_to_employment <- FALSE
    panel <- rbind(panel, secondary)
  }

  # Sort by person and year
  panel <- panel[order(panel$person_id, panel$year, !panel$primary_job), ]
  rownames(panel) <- NULL

  panel
}

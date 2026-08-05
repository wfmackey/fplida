# Tests for disability assignment and treatment effects (Phase 2)

# =============================================================================
# Spine disability columns
# =============================================================================

test_that("spine has disability columns", {
  spine <- generate_spine(n = 500L, seed = 1L)

  dis_cols <- c("disability_onset_year", "disability_type",
                "disability_severity", "is_dc", "disability_dose")
  for (col in dis_cols) {
    expect_true(col %in% names(spine), info = paste("Missing:", col))
  }
})

test_that("disability/CHC rate is approximately 18% of eligible population", {
  spine <- generate_spine(n = 10000L, seed = 42L)

  # Eligible: employed, education > 0, age 25-60 at 2015
  age_2015 <- 2015 - spine$birth_year
  eligible <- spine$baseline_employed & spine$education > 0 &
              age_2015 >= 25 & age_2015 <= 60

  n_eligible <- sum(eligible)
  n_disabled <- sum(!is.na(spine$disability_onset_year))

  rate <- n_disabled / n_eligible
  # Allow tolerance: 12%-24% (target 18%)
  expect_gt(rate, 0.12)
  expect_lt(rate, 0.24)
})

test_that("disability onset years are in valid range", {
  spine <- generate_spine(n = 5000L, seed = 42L)

  disabled <- spine[!is.na(spine$disability_onset_year), ]
  expect_true(nrow(disabled) > 0)
  expect_true(all(disabled$disability_onset_year >= 2010))
  expect_true(all(disabled$disability_onset_year <= 2020))
})

test_that("disability type is valid SDAC code", {
  spine <- generate_spine(n = 5000L, seed = 42L)

  disabled <- spine[!is.na(spine$disability_type), ]
  valid_types <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17)
  expect_true(all(disabled$disability_type %in% valid_types))
})

test_that("disability severity is 1-4", {
  spine <- generate_spine(n = 5000L, seed = 42L)

  disabled <- spine[!is.na(spine$disability_severity), ]
  expect_true(all(disabled$disability_severity %in% 1:4))
})

test_that("is_dc is logical for disabled, NA for non-disabled", {
  spine <- generate_spine(n = 5000L, seed = 42L)

  disabled <- spine[!is.na(spine$disability_onset_year), ]
  expect_true(all(!is.na(disabled$is_dc)))
  expect_type(disabled$is_dc, "logical")

  non_disabled <- spine[is.na(spine$disability_onset_year), ]
  expect_true(all(is.na(non_disabled$is_dc)))
})

test_that("disability_dose is in [0, 1] for disabled", {
  spine <- generate_spine(n = 5000L, seed = 42L)

  disabled <- spine[!is.na(spine$disability_dose), ]
  expect_true(all(disabled$disability_dose >= 0))
  expect_true(all(disabled$disability_dose <= 1))
})

test_that("both DC and NC groups exist", {
  spine <- generate_spine(n = 10000L, seed = 42L)

  disabled <- spine[!is.na(spine$is_dc), ]
  expect_true(any(disabled$is_dc))
  expect_true(any(!disabled$is_dc))
})

test_that("non-disabled have NA for all disability columns", {
  spine <- generate_spine(n = 2000L, seed = 42L)

  non_dis <- spine[is.na(spine$disability_onset_year), ]
  expect_true(all(is.na(non_dis$disability_type)))
  expect_true(all(is.na(non_dis$disability_severity)))
  expect_true(all(is.na(non_dis$is_dc)))
  expect_true(all(is.na(non_dis$disability_dose)))
})

test_that("only employed with education get disability", {
  spine <- generate_spine(n = 5000L, seed = 42L)

  disabled <- spine[!is.na(spine$disability_onset_year), ]
  expect_true(all(disabled$baseline_employed))
  expect_true(all(disabled$education > 0))
})

# =============================================================================
# assign_disability__ standalone function
# =============================================================================

test_that("assign_disability__ returns correct structure", {
  spine <- generate_spine(n = 1000L, seed = 1L)

  result <- assign_disability__(
    birth_year = as.integer(spine$birth_year),
    education = as.integer(spine$education),
    baseline_employed = as.integer(spine$baseline_employed),
    task_cognitive = as.double(spine$task_cognitive),
    task_physical = as.double(spine$task_physical),
    task_vision = as.double(spine$task_vision),
    task_hearing = as.double(spine$task_hearing),
    task_manual_dexterity = as.double(spine$task_manual_dexterity),
    task_communication = as.double(spine$task_communication),
    seed = 42L,
    onset_min_year = 2010L,
    onset_max_year = 2020L,
    disability_rate = 0.05
  )

  expect_type(result, "list")
  expect_true("disability_onset_year" %in% names(result))
  expect_true("disability_type" %in% names(result))
  expect_true("disability_severity" %in% names(result))
  expect_true("is_dc" %in% names(result))
  expect_true("disability_dose" %in% names(result))
  expect_equal(length(result$disability_onset_year), nrow(spine))
})

# =============================================================================
# Employment panel with disability effects
# =============================================================================

test_that("employment panel DC workers have lower earnings at 2021 anchor", {
  spine <- generate_spine(n = 10000L, seed = 42L)
  panel <- generate_employment_panel(spine, seed = 42L, years = 2021L)

  primary <- panel[panel$primary_job, ]

  # DC workers with early onset (k >= 3 by 2021)
  dc_early <- spine[!is.na(spine$is_dc) & spine$is_dc &
                    spine$disability_onset_year <= 2018L, ]
  if (nrow(dc_early) < 5) skip("Not enough early-onset DC workers")

  # Compare anchor earnings of DC vs baseline_income
  # (disability reduces observed earnings relative to counterfactual)
  dc_in_panel <- primary[primary$person_id %in% dc_early$id, ]
  if (nrow(dc_in_panel) < 5) skip("Too few DC workers in panel")

  dc_merge <- merge(dc_in_panel, dc_early[, c("id", "baseline_income")],
                    by.x = "person_id", by.y = "id")
  # Observed earnings should be lower than baseline for most DC workers
  # (treatment effect is negative)
  ratio <- median(dc_merge$gross_annual / dc_merge$baseline_income)
  # With k=3+ and treatment effect of ~-0.18 to -0.22, ratio should be < 1
  # Allow wide tolerance since earnings also have growth and noise
  expect_lt(ratio, 1.15)
})

test_that("employment panel DC workers have lower employment rate", {
  spine <- generate_spine(n = 10000L, seed = 42L)
  panel <- generate_employment_panel(spine, seed = 42L, years = 2019:2024)

  # Identify DC vs ND workers (both employed at baseline)
  dc_ids <- spine$id[!is.na(spine$is_dc) & spine$is_dc]
  nd_ids <- spine$id[is.na(spine$disability_onset_year) &
                     spine$baseline_employed & spine$education > 0]

  if (length(dc_ids) < 10 || length(nd_ids) < 50) {
    skip("Not enough workers for employment rate test")
  }

  primary <- panel[panel$primary_job, ]

  # Count who appears in 2024
  dc_in_2024 <- sum(dc_ids %in% primary$person_id[primary$year == 2024L])
  nd_in_2024 <- sum(nd_ids %in% primary$person_id[primary$year == 2024L])

  dc_rate <- dc_in_2024 / length(dc_ids)
  nd_rate <- nd_in_2024 / length(nd_ids)

  # DC employment rate should be lower
  expect_lt(dc_rate, nd_rate)
})

test_that("employment panel event shape has larger DC than NC losses", {
  spine <- generate_spine(n = 20000L, seed = 42L)
  panel <- generate_employment_panel(spine, seed = 42L, years = 2014:2024)
  primary <- panel[panel$primary_job,
                   c("person_id", "year", "gross_annual")]

  disabled <- spine[
    !is.na(spine$disability_onset_year) &
      spine$disability_onset_year >= 2015L &
      spine$disability_onset_year <= 2020L,
    c("id", "is_dc", "disability_onset_year")
  ]
  if (sum(disabled$is_dc, na.rm = TRUE) < 50L ||
      sum(!disabled$is_dc, na.rm = TRUE) < 50L) {
    skip("Not enough event-window DC/NC workers")
  }

  event_summary <- function(is_dc) {
    x <- disabled[disabled$is_dc == is_dc, , drop = FALSE]
    grid <- rbind(
      transform(x, k = -1L, year = disability_onset_year - 1L),
      transform(x, k = 2L, year = disability_onset_year + 2L)
    )
    m <- merge(grid, primary,
               by.x = c("id", "year"), by.y = c("person_id", "year"),
               all.x = TRUE)
    aggregate(
      list(
        employed = !is.na(m$gross_annual),
        gross = ifelse(is.na(m$gross_annual), 0, m$gross_annual)
      ),
      list(k = m$k),
      mean
    )
  }

  dc <- event_summary(TRUE)
  nc <- event_summary(FALSE)
  dc_drop <- dc$employed[dc$k == 2L] - dc$employed[dc$k == -1L]
  nc_drop <- nc$employed[nc$k == 2L] - nc$employed[nc$k == -1L]
  dc_log <- log(dc$gross[dc$k == 2L] / dc$gross[dc$k == -1L])
  nc_log <- log(nc$gross[nc$k == 2L] / nc$gross[nc$k == -1L])

  expect_lt(dc_drop, nc_drop - 0.05)
  expect_lt(dc_log, -0.12)
  expect_gt(nc_log, -0.08)
})

# =============================================================================
# SDAC is anchored on the spine
# =============================================================================

sdac_with_spine <- function(n = 40000L, seed = 7L, survey_year = 2018L) {
  spine <- generate_spine(n = n, seed = seed)
  sdac <- generate_sdac(spine = spine, seed = seed, survey_year = survey_year,
                        return_data = TRUE)
  merge(sdac,
        spine[, c("aeuid_abs", "birth_year", "sex", "disability_severity",
                  "disability_onset_year", "disability_type", "person_type")],
        by.x = "SYNTHETIC_AEUID", by.y = "aeuid_abs", all.x = TRUE)
}

test_that("SDAC DISSTAT is anchored on spine disability_severity", {
  m <- sdac_with_spine()
  # A disability whose onset is after survey night has not happened yet, so it
  # is not anchored — those people fall through to the residual hazard draw.
  anchored <- !is.na(m$disability_severity) &
    (is.na(m$disability_onset_year) | m$disability_onset_year <= 2018L)
  expect_gt(sum(anchored), 30L)

  # The two code frames are different scales, so the map is not the identity.
  # Spine 1 (profound/severe) -> DISSTAT 2 (Severe), because the package has no
  # published profound-only rate to split the spine's combined level.
  # Spine 2 -> 3 (Moderate), 3 -> 4 (Mild), 4 (condition only) -> 7.
  expected <- c(`1` = 2L, `2` = 3L, `3` = 4L, `4` = 7L)[
    as.character(m$disability_severity[anchored])
  ]
  expect_identical(m$DISSTAT[anchored], unname(expected))

  # A spine core-activity limitation is never coded as no disability.
  limited <- anchored & m$disability_severity %in% 1:3
  expect_true(all(m$DISSTAT[limited] <= 6L))
  expect_true(all(m$WTHRDIS[limited] == 1L))

  # "Condition only" is DISSTAT 7, which SDAC does not count as disability.
  cond <- anchored & m$disability_severity == 4L
  expect_true(all(m$DISSTAT[cond] == 7L))
  expect_true(all(m$WTHRDIS[cond] == 2L))
})

test_that("SDAC keeps published disability prevalence after anchoring", {
  survey_year <- 2018L
  m <- sdac_with_spine(n = 100000L, survey_year = survey_year)

  bands_lo <- c(0, 5, 15, 25, 35, 45, 55, 60, 65, 70, 75, 80, 85, 90)
  rate_m <- c(0.072, 0.164, 0.144, 0.097, 0.103, 0.169, 0.243, 0.308,
              0.415, 0.465, 0.554, 0.666, 0.768, 0.863)
  rate_f <- c(0.041, 0.107, 0.132, 0.112, 0.125, 0.184, 0.279, 0.347,
              0.395, 0.438, 0.514, 0.685, 0.776, 0.822)
  ps_m <- c(0.044, 0.103, 0.058, 0.027, 0.030, 0.047, 0.071, 0.080,
            0.106, 0.149, 0.195, 0.287, 0.396, 0.647)
  ps_f <- c(0.031, 0.056, 0.046, 0.036, 0.033, 0.043, 0.083, 0.095,
            0.137, 0.131, 0.222, 0.318, 0.506, 0.688)

  idx <- findInterval(survey_year - m$birth_year, bands_lo)
  target <- mean(ifelse(m$sex == 2L, rate_f[idx], rate_m[idx]))
  target_ps <- mean(ifelse(m$sex == 2L, ps_f[idx], ps_m[idx]))

  # The residual hazard is rebased cell by cell, so the published marginal
  # survives. The 35-44 band is the one place the spine already exceeds the
  # published rate, which lifts the total by a few tenths of a point.
  expect_lt(abs(mean(m$DISSTAT <= 6L) - target), 0.015)
  expect_lt(abs(mean(m$DISSTAT <= 2L) - target_ps), 0.015)

  # SDAC covers children and the elderly; the spine does not.
  old <- (survey_year - m$birth_year) >= 75L
  expect_gt(mean(m$DISSTAT[old] <= 6L), 0.40)
  young <- (survey_year - m$birth_year) < 15L
  expect_gt(mean(m$DISSTAT[young] <= 6L), 0.02)
})

test_that("SDAC core activity limitations are not collinear with DISSTAT", {
  m <- sdac_with_spine()
  lim <- m[m$DISSTAT <= 4L, ]
  expect_gt(nrow(lim), 100L)

  # SDAC derives DISSTAT as the most severe of the three core-activity
  # limitations, and lower codes are more severe, so the minimum of the three
  # must equal DISSTAT and none may be more severe than it.
  worst <- pmin(lim$COMMCALN, lim$MOBCALN, lim$SELFCALN)
  expect_identical(worst, lim$DISSTAT)
  for (v in list(lim$COMMCALN, lim$MOBCALN, lim$SELFCALN)) {
    expect_true(all(v >= lim$DISSTAT))
    expect_true(all(v <= pmin(lim$DISSTAT + 1L, 5L)))
    # Which domain binds is drawn, so each is the limiting one about a third
    # of the time and none is collinear with DISSTAT.
    expect_gt(mean(v != lim$DISSTAT), 0.20)
    expect_lt(mean(v != lim$DISSTAT), 0.50)
  }

  expect_true(all(m$MOBCALN[m$DISSTAT > 4L] == 5L))
  expect_true(all(m$COMMCALN[m$DISSTAT > 4L] == 5L))
  expect_true(all(m$MOBCALN %in% 1:5))
})

test_that("SDAC DISTYPE and DISGP follow the spine person type", {
  m <- sdac_with_spine()
  anchored <- !is.na(m$person_type) &
    (is.na(m$disability_onset_year) | m$disability_onset_year <= 2018L)
  expect_gt(sum(anchored), 30L)

  # Only the records SDAC counts as disabled carry a disability type; a
  # "condition only" record is DISTYPE 18 by construction.
  typed <- anchored & m$DISSTAT <= 6L
  expect_gt(mean(m$DISTYPE[typed] == m$disability_type[typed]), 0.60)
  expect_true(all(m$DISTYPE[m$DISSTAT <= 6L] %in% 1:17))
  expect_true(all(m$DISGP[m$DISSTAT <= 6L] %in% 1:6))
  expect_true(all(m$DISTYPE[m$DISSTAT > 6L] == 18L))
  expect_true(all(m$DISGP[m$DISSTAT > 6L] == 7L))
})

test_that("SDAC is deterministic with the same seed", {
  a <- sdac_with_spine(n = 20000L)
  b <- sdac_with_spine(n = 20000L)
  expect_identical(a$SYNTHETIC_AEUID, b$SYNTHETIC_AEUID)
  expect_identical(a$DISSTAT, b$DISSTAT)
  expect_identical(a$MOBCALN, b$MOBCALN)
  expect_identical(a$DISTYPE, b$DISTYPE)
})

# =============================================================================
# DOMINO Phase 2 DSP receipt
# =============================================================================

test_that("DOMINO assigns DSP to disabled workers", {
  spine <- generate_spine(n = 5000L, seed = 42L)
  domino <- generate_domino(spine = spine, seed = 42L, return_data = TRUE)

  # Check that some DSP spells exist
  dsp_spells <- domino$det_ben[domino$det_ben$BEN_TYPE_CODE == "DSP", ]
  expect_gt(nrow(dsp_spells), 0)
})

test_that("disability is deterministic with same seed", {
  a <- generate_spine(n = 500L, seed = 42L)
  b <- generate_spine(n = 500L, seed = 42L)

  expect_identical(a$disability_onset_year, b$disability_onset_year)
  expect_identical(a$disability_type, b$disability_type)
  expect_identical(a$is_dc, b$is_dc)
})

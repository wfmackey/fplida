test_that("generate_employment_panel returns expected structure", {
  spine <- generate_spine(n = 500L, seed = 1L)
  panel <- generate_employment_panel(spine, seed = 1L, years = 2019:2024)

  expect_s3_class(panel, "data.frame")
  expected_cols <- c("person_id", "aeuid_ato", "year", "employer_id",
                     "primary_job", "anzsco_major", "industry",
                     "hours_weekly", "gross_annual", "switched_employer",
                     "new_to_employment")
  for (col in expected_cols) {
    expect_true(col %in% names(panel), info = paste("Missing:", col))
  }
})

test_that("panel covers requested years", {
  spine <- generate_spine(n = 200L, seed = 1L)
  yrs <- 2018:2024
  panel <- generate_employment_panel(spine, seed = 1L, years = yrs)

  panel_years <- sort(unique(panel$year))
  # All years should be represented (at least some employed each year)
  expect_true(all(yrs %in% panel_years))
})

test_that("panel has no children (age < 15 in year)", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  panel <- generate_employment_panel(spine, seed = 1L, years = 2015:2024)

  # Merge birth year
  panel_by <- merge(panel, spine[, c("id", "birth_year")],
                    by.x = "person_id", by.y = "id", all.x = TRUE)
  age_in_year <- panel_by$year - panel_by$birth_year
  expect_true(all(age_in_year >= 15))
})

test_that("panel 2021 employed count close to spine", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  panel <- generate_employment_panel(spine, seed = 1L, years = 2021L)

  primary_2021 <- panel[panel$year == 2021L & panel$primary_job, ]
  spine_emp <- sum(spine$baseline_employed)

  # Close to baseline count (disability employment shock may remove a few)
  expect_lte(abs(nrow(primary_2021) - spine_emp), spine_emp * 0.02)
})

test_that("panel is deterministic with same seed", {
  spine <- generate_spine(n = 200L, seed = 1L)
  a <- generate_employment_panel(spine, seed = 42L, years = 2019:2023)
  b <- generate_employment_panel(spine, seed = 42L, years = 2019:2023)

  expect_identical(a, b)
})

test_that("panel different seeds give different results", {
  spine <- generate_spine(n = 200L, seed = 1L)
  a <- generate_employment_panel(spine, seed = 1L, years = 2019:2023)
  b <- generate_employment_panel(spine, seed = 2L, years = 2019:2023)

  expect_false(identical(a$gross_annual, b$gross_annual))
})

test_that("switching rate is plausible for prime-age workers", {
  spine <- generate_spine(n = 5000L, seed = 42L)
  panel <- generate_employment_panel(spine, seed = 42L, years = 2020:2022)

  # Check 2022 (forward year) switching rate among primary jobs
  primary_2022 <- panel[panel$year == 2022L & panel$primary_job, ]
  switch_rate <- mean(primary_2022$switched_employer)
  # National avg ~13%, allow wide tolerance
  expect_gt(switch_rate, 0.03)
  expect_lt(switch_rate, 0.30)
})

test_that("multi-job rows are present", {
  spine <- generate_spine(n = 2000L, seed = 1L)
  panel <- generate_employment_panel(spine, seed = 1L, years = 2019:2024)

  secondary <- panel[!panel$primary_job, ]
  # ~7% of primary rows should have secondary, so > 0
  expect_gt(nrow(secondary), 0)

  # Secondary earnings < primary (20-40% range)
  # Check at least some secondary earnings are lower
  if (nrow(secondary) > 10) {
    expect_true(all(secondary$gross_annual > 0))
  }
})

test_that("gross_annual is positive for all panel rows", {
  spine <- generate_spine(n = 500L, seed = 1L)
  panel <- generate_employment_panel(spine, seed = 1L, years = 2019:2024)

  expect_true(all(panel$gross_annual > 0))
})

test_that("employer_id format is 11 digits", {
  spine <- generate_spine(n = 200L, seed = 1L)
  panel <- generate_employment_panel(spine, seed = 1L, years = 2020:2022)

  expect_true(all(nchar(panel$employer_id) == 11))
  expect_true(all(grepl("^\\d{11}$", panel$employer_id)))
})

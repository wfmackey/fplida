test_that("every headline index is 1 in the anchor year", {
  for (series in c("wage", "price", "business", "transfer", "health")) {
    expect_equal(nominal_index(series, 2021L, basis = "calendar"), 1,
                 info = series)
  }
})

test_that("the indices reproduce the published series", {
  # A handful of figures checked against the source publications. If a rebuild
  # of data-raw/update_nominal_indices.R moves any of these, the series has
  # changed and the change needs explaining, not accepting.
  growth <- function(series, from, to, basis = "financial") {
    nominal_index(series, to, basis) / nominal_index(series, from, basis) - 1
  }

  # ABS CPI: prices rose about 62 per cent between 2005-06 and 2023-24.
  expect_equal(growth("price", 2005L, 2023L), 0.62, tolerance = 0.06)
  # The 2022-23 inflation spike: a 7.0 per cent financial-year average rise.
  expect_equal(growth("price", 2021L, 2022L), 0.070, tolerance = 0.005)
  # ATO average salary or wages roughly doubled over the same window, so wages
  # outgrew prices. A build where they did not has the columns crossed.
  expect_equal(growth("wage", 2005L, 2023L), 0.94, tolerance = 0.06)
  expect_gt(growth("wage", 2005L, 2023L), growth("price", 2005L, 2023L))
})

test_that("the MBS indexation freeze is in the health series", {
  # The schedule fee for item 23, a standard GP consultation, was $37.05 from
  # 2014-15 through 2017-18 and did not move once. Health is therefore the one
  # headline series that is flat for four consecutive years, which is why
  # Medicare amounts must not be indexed to the CPI.
  frozen <- vapply(2014:2017, function(y) nominal_index("health", y), numeric(1))
  expect_equal(length(unique(round(frozen, 8))), 1L)

  # Prices kept rising through the freeze, so a Medicare benefit fell about
  # five per cent in real terms over those four years.
  expect_gt(nominal_index("price", 2017L) / nominal_index("price", 2014L), 1.04)

  # Indexation resumed after it.
  expect_gt(nominal_index("health", 2018L), nominal_index("health", 2017L))
})

test_that("pensions and allowances follow different paths", {
  # Pensions are benchmarked to Male Total Average Weekly Earnings; allowances
  # are indexed to the CPI alone. The gap is the single best-known feature of
  # Australian income support and DOMINO now carries it.
  pension <- nominal_index("transfer", 2023L) / nominal_index("transfer", 2005L)
  allowance <- nominal_index("price", 2023L) / nominal_index("price", 2005L)
  expect_gt(pension, allowance * 1.05)
})

test_that("the financial and calendar tables are on the same base", {
  # A financial year straddles two calendar years, so its index has to sit
  # between them. If it does not, the two tables were normalised separately and
  # a person's tax return would not line up with their Census record.
  for (y in c(2010L, 2016L, 2021L, 2024L)) {
    fy <- nominal_index("wage", y, basis = "financial")
    lo <- nominal_index("wage", y, basis = "calendar")
    hi <- nominal_index("wage", y + 1L, basis = "calendar")
    expect_gte(fy, lo)
    expect_lte(fy, hi)
  }
})

test_that("years outside the published data are flagged as projected", {
  tbl <- nominal_indices()
  headline <- tbl[tbl$role == "headline" & tbl$basis == "financial_year", ]

  # The published data is there for the years the package actually generates.
  mid <- headline[headline$year >= 2006L & headline$year <= 2022L, ]
  expect_false(any(mid$projected[mid$series %in% c("price", "wage")]))

  # The tails are not, and say so. ATO taxation statistics lag by about two
  # years, so the most recent years are always this package's own projection.
  expect_true(all(headline$projected[headline$year >= 2028L]))
})

test_that("the employment panel carries nominal wage growth", {
  spine <- generate_spine(n = 4000L, seed = 77L)
  years <- 2015L:2025L
  panel <- generate_employment_panel(spine, seed = 77L, years = years)
  primary <- panel[panel$primary_job, ]

  mean_wage <- as.numeric(tapply(primary$gross_annual, primary$year,
                                 mean)[as.character(years)])

  # A panel year is a financial-year END year, and the index names a financial
  # year by the year it starts in, hence the offset.
  index <- vapply(years, function(y) nominal_index("wage", y - 1L,
                                                   basis = "financial"),
                  numeric(1))

  panel_growth <- diff(mean_wage) / utils::head(mean_wage, -1L)
  index_growth <- diff(index) / utils::head(index, -1L)

  # The point of the change. Before it, wages grew at a flat 5.8 per cent
  # whatever the year; now a fast year in the published series is a fast year in
  # the panel. Correlation, not equality, is what is being claimed here.
  expect_gt(stats::cor(panel_growth, index_growth), 0.85)

  # The panel grows a little faster than the headline, and should. The index is
  # an average over a whole population whose composition is roughly stationary,
  # while the panel is a closed cohort that ages ten years and changes jobs
  # along the way. Career progression sits on top of the price of labour. What
  # matters is that the gap is small and steady rather than the drift being
  # invented: about one point a year, not the two and a half the old flat 5.8
  # per cent added.
  gap <- mean(panel_growth - index_growth)
  expect_gt(gap, 0)
  expect_lt(gap, 0.02)

  # And real wages rise over the window, as they did.
  cpi <- vapply(years, function(y) nominal_index("price", y - 1L,
                                                 basis = "financial"),
                numeric(1))
  expect_gt(mean_wage[length(years)] / mean_wage[1L],
            cpi[length(years)] / cpi[1L])
})

test_that("the employment panel is still deterministic and slice-independent", {
  spine <- generate_spine(n = 600L, seed = 5L)
  full <- generate_employment_panel(spine, seed = 5L, years = 2018L:2023L)

  # Same seed, same answer.
  again <- generate_employment_panel(spine, seed = 5L, years = 2018L:2023L)
  expect_identical(full, again)

  # And the same answer for a person whether or not the rest of the population
  # was generated alongside them. This is what lets the build be sliced across
  # a variable number of workers, so a nominal factor keyed to anything
  # slice-local would break it here.
  half <- generate_employment_panel(spine[1:300, ], seed = 5L,
                                    years = 2018L:2023L)
  shared <- merge(full, half, by = c("person_id", "year", "primary_job"),
                  suffixes = c(".full", ".half"))
  expect_gt(nrow(shared), 0L)
  expect_equal(shared$gross_annual.full, shared$gross_annual.half)
})

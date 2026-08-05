test_that("BIRTHS parent birthplaces and residence durations are coherent", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("births_parent_values_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 1500L, seed = 1300L, output_dir = tmp)
  spine$birth_year <- rep(2020L, nrow(spine))
  spine$country_of_birth <- rep(0L, nrow(spine))
  births <- generate_births(
    spine = spine, seed = 1300L, output_dir = tmp, return_data = TRUE
  )

  overseas_sacc <- c(
    2102L, 7103L, 6101L, 1201L, 5204L, 5105L, 9225L,
    5203L, 3104L, 8104L, 2304L, 6102L, 3207L, 7106L
  )
  for (parent in c("MOTHER", "FATHER")) {
    birthplace <- births[[paste0("BIRTH_PLACE_CODE_", parent)]]
    age <- births[[paste0("AGE_", parent)]]
    months <- births[[paste0("RESIDENCE_MONTHS_", parent)]]
    years <- births[[paste0("RESIDENCE_YEARS_", parent)]]

    expect_true(all(birthplace %in% c(1101L, overseas_sacc)), info = parent)
    expect_false(any(birthplace == 0L), info = parent)
    expect_true(any(birthplace != 1101L), info = parent)
    expect_true(all(months >= 0L & months <= age * 12L), info = parent)
    expect_equal(years, months %/% 12L, info = parent)
    expect_equal(months[birthplace == 1101L], age[birthplace == 1101L] * 12L,
                 info = parent)
  }
})

test_that("MBS and PBS clinical events stop at the spine death date", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("health_death_cutoff_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 400L, seed = 2300L, output_dir = tmp)
  spine$birth_year <- rep(1970L, nrow(spine))
  spine$year_of_death <- rep(2010L, nrow(spine))
  spine$month_of_death <- rep(1L, nrow(spine))
  spine$day_of_death <- rep(1L, nrow(spine))

  mbs <- suppressMessages(generate_mbs(
    spine = spine, seed = 2300L, years = 2009L:2011L,
    output_dir = tmp, return_data = TRUE, chunk_size = 100L
  ))
  pbs <- suppressMessages(generate_pbs(
    spine = spine, seed = 2300L, years = 2009L:2011L,
    output_dir = tmp, return_data = TRUE, chunk_size = 100L
  ))

  expect_gt(nrow(mbs[["2009"]]), 0L)
  expect_gt(nrow(mbs[["2010"]]), 0L)
  expect_true(all(mbs[["2010"]]$DOS == "01Jan10"))
  expect_equal(nrow(mbs[["2011"]]), 0L)

  expect_gt(nrow(pbs[["2009"]]), 0L)
  expect_gt(nrow(pbs[["2010"]]), 0L)
  expect_true(all(pbs[["2010"]]$SPPLY_DT == "01Jan10"))
  expect_equal(nrow(pbs[["2011"]]), 0L)
})

test_that("AIR encounters stop at the spine death date", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("air_death_cutoff_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 400L, seed = 3300L, output_dir = tmp)
  spine$birth_year <- rep(1970L, nrow(spine))
  spine$year_of_death <- rep(2022L, nrow(spine))
  spine$month_of_death <- rep(5L, nrow(spine))
  spine$day_of_death <- rep(10L, nrow(spine))

  air <- suppressMessages(generate_air(
    spine = spine, seed = 3300L, output_dir = tmp, return_data = TRUE
  ))

  expect_gt(nrow(air), 0L)
  expect_true(all(air$ENCOUNTER_DATE <= as.Date("2022-05-10")))
})

test_that("MCD carries spine deaths and uses arrival-based consumer starts", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("mcd_spine_timing_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 400L, seed = 4300L, output_dir = tmp)
  spine$birth_year <- rep(1970L, nrow(spine))
  spine$country_of_birth <- rep(c(0L, 1L), length.out = nrow(spine))
  spine$year_of_arrival <- rep(c(NA_integer_, 2008L), length.out = nrow(spine))
  spine$year_of_death <- rep(c(2015L, NA_integer_), length.out = nrow(spine))
  spine$month_of_death <- rep(c(6L, NA_integer_), length.out = nrow(spine))
  death_before_product_ids <- spine$aeuid_sa[seq_len(20L)]
  spine$year_of_death[seq_len(20L)] <- 1999L
  spine$month_of_death[seq_len(20L)] <- 12L

  mcd <- suppressMessages(generate_mcd(
    spine = spine, seed = 4300L, output_dir = tmp, return_data = TRUE
  ))
  matched <- match(mcd$SYNTHETIC_AEUID, spine$aeuid_sa)
  expect_false(anyNA(matched))
  expect_false(any(mcd$SYNTHETIC_AEUID %in% death_before_product_ids))
  expect_equal(mcd$YEAR_OF_DEATH, spine$year_of_death[matched])
  expect_equal(mcd$MONTH_OF_DEATH, spine$month_of_death[matched])

  expected_start <- as.Date(ifelse(
    spine$country_of_birth[matched] == 0L, "2000-01-01", "2008-01-01"
  ))
  expect_equal(mcd$CNSMR_STS, expected_start)
})

test_that("AMEP enrolment years do not follow the spine death year", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("amep_death_cutoff_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 1000L, seed = 5300L, output_dir = tmp)
  spine$birth_year <- rep(1980L, nrow(spine))
  spine$country_of_birth <- rep(1L, nrow(spine))
  spine$year_of_arrival <- rep(2019L, nrow(spine))
  spine$year_of_death <- rep(2019L, nrow(spine))

  amep <- suppressMessages(generate_amep(
    spine = spine, seed = 5300L, output_dir = tmp, return_data = TRUE
  ))
  expect_gt(nrow(amep), 0L)
  expect_true(all(amep$ENROLLED_FY <= 2019L))
})

test_that("apprenticeships start after age 16 and stop at death", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("apprentice_death_cutoff_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 2000L, seed = 6300L, output_dir = tmp)
  spine$birth_year <- rep(2000L, nrow(spine))
  spine$education <- rep(2L, nrow(spine))
  spine$year_of_death <- rep(2018L, nrow(spine))
  spine$month_of_death <- rep(1L, nrow(spine))
  spine$day_of_death <- rep(1L, nrow(spine))

  apprentice <- suppressMessages(generate_apprentice(
    spine = spine, seed = 6300L, output_dir = tmp, return_data = TRUE
  ))
  expect_gt(nrow(apprentice), 0L)
  expect_true(all(apprentice$START_DATE >= as.Date("2016-01-01")))
  expect_true(all(apprentice$START_DATE <= as.Date("2018-01-01")))
  expect_true(all(apprentice$END_DATE <= as.Date("2018-01-01")))
})


# ---------------------------------------------------------------------------
# No record may predate the person. The spine dates birth at the first of
# month_of_birth; claims, supplies, prescriptions and referrals must fall on
# or after that, and birth-year volumes scale with the observable part of
# the year.
# ---------------------------------------------------------------------------

.atr_parse_date <- function(x) {
  old <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  as.Date(x, format = "%d%b%y")
}

.atr_fixture <- function() {
  tmp <- tempfile("fplida-admin-temporal-")
  dir.create(tmp, recursive = TRUE)

  years <- 2006L:2015L
  spine <- suppressMessages(
    generate_spine(n = 4000L, seed = 42L, output_dir = tmp, format = "parquet")
  )
  mbs <- suppressMessages(
    generate_mbs(spine = spine, seed = 42L, years = years,
                 output_dir = tmp, return_data = TRUE, chunk_size = 2000L)
  )
  pbs <- suppressMessages(
    generate_pbs(spine = spine, seed = 42L, years = years,
                 output_dir = tmp, return_data = TRUE, chunk_size = 2000L)
  )

  key <- data.frame(
    SYNTHETIC_AEUID = spine$aeuid_dhda,
    birth_year      = as.integer(spine$birth_year),
    spine_mob       = as.integer(spine$month_of_birth),
    dob = as.Date(sprintf("%04d-%02d-01", spine$birth_year, spine$month_of_birth)),
    stringsAsFactors = FALSE
  )

  list(
    dir = tmp,
    key = key,
    mbs = do.call(rbind, mbs),
    pbs = do.call(rbind, pbs)
  )
}

test_that("no MBS claim is dated before the person is born", {
  fx <- .atr_fixture()
  on.exit(unlink(fx$dir, recursive = TRUE, force = TRUE), add = TRUE)

  m <- merge(fx$mbs, fx$key, by = "SYNTHETIC_AEUID")
  expect_gt(nrow(m), 0)

  dos <- .atr_parse_date(m$DOS)
  expect_equal(sum(dos < m$dob), 0L)

  # Birth-year rows are the ones the whole-year draw window used to break;
  # there must still be some, so the fix has not simply emptied them.
  birth_year_rows <- sum(as.integer(format(dos, "%Y")) == m$birth_year)
  expect_gt(birth_year_rows, 0)

  # Referrals precede the service, but never the person.
  rpdate <- .atr_parse_date(m$RPDATE)
  expect_equal(sum(!is.na(rpdate) & rpdate < m$dob), 0L)
})

test_that("no PBS supply or prescription is dated before the person is born", {
  fx <- .atr_fixture()
  on.exit(unlink(fx$dir, recursive = TRUE, force = TRUE), add = TRUE)

  p <- merge(fx$pbs, fx$key, by = "SYNTHETIC_AEUID")
  expect_gt(nrow(p), 0)

  spply <- .atr_parse_date(p$SPPLY_DT)
  prscrb <- .atr_parse_date(p$PRSCRB_DT)
  expect_equal(sum(spply < p$dob), 0L)
  expect_equal(sum(prscrb < p$dob), 0L)

  birth_year_rows <- sum(as.integer(format(spply, "%Y")) == p$birth_year)
  expect_gt(birth_year_rows, 0)
})

test_that("PBS patient birth month is the spine birth month on every row", {
  fx <- .atr_fixture()
  on.exit(unlink(fx$dir, recursive = TRUE, force = TRUE), add = TRUE)

  p <- merge(fx$pbs, fx$key, by = "SYNTHETIC_AEUID")
  expect_gt(nrow(p), 0)

  # Row level: every row carries the spine value.
  expect_equal(sum(p$PTNT_BRTH_MTH != p$spine_mob), 0L)
  expect_equal(sum(p$PTNT_BRTH_YR != p$birth_year), 0L)

  # Person level: constant within a person, and equal to the spine.
  by_person <- split(seq_len(nrow(p)), p$SYNTHETIC_AEUID)
  ok <- vapply(by_person, function(i) {
    length(unique(p$PTNT_BRTH_MTH[i])) == 1L &&
      p$PTNT_BRTH_MTH[i][1] == p$spine_mob[i][1]
  }, logical(1))
  expect_gt(length(by_person), 0)
  expect_true(all(ok))

  # The column still varies across people — it has not been made constant.
  expect_gt(length(unique(p$PTNT_BRTH_MTH)), 1L)
})

test_that("birth-year volumes are scaled by the observable part of the year", {
  fx <- .atr_fixture()
  on.exit(unlink(fx$dir, recursive = TRUE, force = TRUE), add = TRUE)

  m <- merge(fx$mbs, fx$key, by = "SYNTHETIC_AEUID")
  dos <- .atr_parse_date(m$DOS)
  in_birth_year <- as.integer(format(dos, "%Y")) == m$birth_year
  expect_gt(sum(in_birth_year), 0)

  # A person born in month k is observable for (13 - k) months. Claims in
  # the birth year should therefore fall away as the birth month gets
  # later: the second half of the year carries fewer than the first.
  mob <- m$spine_mob[in_birth_year]
  expect_gt(sum(mob <= 6L), sum(mob >= 7L))
})

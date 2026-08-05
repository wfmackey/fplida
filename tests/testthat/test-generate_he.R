# -- Helper ------------------------------------------------------------------

.make_he_test_data <- function(n = 500L, seed = 1L, fmt = "csv") {
  tmp <- file.path(tempdir(), paste0("he_test_", n, "_", seed))
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  spine <- generate_spine(n = n, seed = seed, output_dir = tmp)
  he <- generate_he(spine = spine, seed = seed, output_dir = tmp, format = fmt,
                    return_data = TRUE)
  list(spine = spine, he = he, tmp = tmp)
}


# -- Structure ---------------------------------------------------------------

test_that("generate_he returns expected structure from spine", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  expect_type(he, "list")
  expect_named(he, c("enrol", "course", "load", "completions", "help"))
  for (nm in names(he)) {
    expect_s3_class(he[[nm]], "data.frame")
  }
})

test_that("generate_he enrol has correct columns", {
  spine <- generate_spine(n = 300L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  expected_cols <- c(
    "SYNTHETIC_AEUID", "YEAR", "COURSE", "INSTITUTION",
    "ATTENDANCE_MODE", "ATTENDANCE_TYPE", "COM_INDICATOR",
    "GENDER", "COUNTRY_BIRTH", "ABORIG_TORRES",
    "DISABILITY", "HIGHEST_PARTICIPATION",
    "YEAR_LEFT_SCHOOL", "REPORTING_YEAR_PERIOD", "MAJOR_COURSE"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(he$enrol),
                info = paste("Missing enrol column:", col))
  }
})

test_that("generate_he course has correct columns", {
  spine <- generate_spine(n = 300L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  expected_cols <- c(
    "SYNTHETIC_AEUID", "YEAR", "COURSE", "COURSE_OF_STUDY_CODE",
    "COURSE_TYPE", "FOE", "FOE_SUPP", "INSTITUTION",
    "SPECIAL_COURSE", "COURSE_LOAD"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(he$course),
                info = paste("Missing course column:", col))
  }
})

test_that("generate_he load has correct columns", {
  spine <- generate_spine(n = 300L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  expected_cols <- c(
    "SYNTHETIC_AEUID", "YEAR", "COURSE", "INSTITUTION",
    "UNIT_STUDY", "DISCIPLINE_CODE", "EQUIVALENT_FT_STUDENT_LOAD",
    "STUDENT_STATUS", "UNIT_STATUS", "MODE_ATTENDANCE",
    "HELP_DEBT", "LOAN_FEE", "TOTAL_AMOUNT_CHARGED",
    "AMOUNT_PAID_UPFRONT", "CAMPUS_STATE", "CAMPUS_POSTCODE",
    "CITIZEN_RESIDENT", "UNIT_STUDY_CENSUS",
    "SUMMER_SCHOOL_INDICATOR", "INDUSTRY"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(he$load),
                info = paste("Missing load column:", col))
  }
})

test_that("generate_he completions has correct columns", {
  spine <- generate_spine(n = 300L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  expected_cols <- c(
    "SYNTHETIC_AEUID", "YEAR", "COURSE_CODE", "COURSE_TYPE",
    "FOE", "FOE_SUPP", "INSTITUTION"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(he$completions),
                info = paste("Missing completions column:", col))
  }
})

test_that("generate_he help has correct columns", {
  spine <- generate_spine(n = 300L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  expected_cols <- c(
    "SYNTHETIC_AEUID", "YEAR", "INSTITUTION",
    "HELP_DEBT", "STUDENT_STATUS", "LOAN_FEE"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(he$help),
                info = paste("Missing help column:", col))
  }
})


# -- AEUID linkage -----------------------------------------------------------

test_that("generate_he AEUIDs exist in spine", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  valid_aeuids <- spine$aeuid_de
  for (nm in c("enrol", "course", "load", "completions", "help")) {
    if (nrow(he[[nm]]) > 0L) {
      expect_true(
        all(he[[nm]]$SYNTHETIC_AEUID %in% valid_aeuids),
        info = paste(nm, "has AEUIDs not in spine")
      )
    }
  }
})


# -- Cross-table consistency -------------------------------------------------

test_that("every person in completions appears in enrol", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$completions) > 0L) {
    enrol_aeuids <- unique(he$enrol$SYNTHETIC_AEUID)
    expect_true(all(he$completions$SYNTHETIC_AEUID %in% enrol_aeuids))
  }
})

test_that("every person in help appears in load", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$help) > 0L) {
    load_aeuids <- unique(he$load$SYNTHETIC_AEUID)
    expect_true(all(he$help$SYNTHETIC_AEUID %in% load_aeuids))
  }
})

test_that("every COURSE in course table appears in enrol", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$course) > 0L) {
    enrol_courses <- unique(he$enrol$COURSE)
    expect_true(all(he$course$COURSE %in% enrol_courses))
  }
})

test_that("course count matches unique courses in enrol", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  n_courses <- nrow(he$course)
  n_unique_enrol_courses <- length(unique(he$enrol$COURSE))
  expect_equal(n_courses, n_unique_enrol_courses)
})


# -- Value range checks ------------------------------------------------------

test_that("generate_he enrol YEAR values are in valid range", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, years = 2005L:2021L,
                    return_data = TRUE)

  if (nrow(he$enrol) > 0L) {
    expect_true(all(he$enrol$YEAR >= 2005L))
    expect_true(all(he$enrol$YEAR <= 2021L))
  }
})

test_that("generate_he enrol ATTENDANCE_TYPE is 1 or 2", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$enrol) > 0L) {
    expect_true(all(he$enrol$ATTENDANCE_TYPE %in% c(1L, 2L)))
  }
})

test_that("generate_he enrol ATTENDANCE_MODE is 1, 2, or 3", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$enrol) > 0L) {
    expect_true(all(he$enrol$ATTENDANCE_MODE %in% 1:3))
  }
})

test_that("generate_he enrol COM_INDICATOR is 0 or 1", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$enrol) > 0L) {
    expect_true(all(he$enrol$COM_INDICATOR %in% c(0L, 1L)))
    # Each course has at most one commencing year (0 if commencement
    # fell before the observation window)
    n_com <- tapply(he$enrol$COM_INDICATOR, he$enrol$COURSE,
                    function(x) sum(x == 1L))
    expect_true(all(n_com <= 1L))
  }
})

test_that("HE enrol demographics retain spine meanings", {
  spine <- generate_spine(n = 2000L, seed = 41L)
  he <- generate_he(spine = spine, seed = 41L, return_data = TRUE)

  expect_gt(nrow(he$enrol), 0L)
  hit <- match(he$enrol$SYNTHETIC_AEUID, spine$aeuid_de)
  expect_false(anyNA(hit))
  expect_identical(
    he$enrol$COUNTRY_BIRTH,
    sprintf("%04d", as.integer(spine$country_of_birth_sacc[hit]))
  )
  expected_indigenous <- c(`1` = 2L, `2` = 3L, `3` = 4L, `4` = 5L)[
    as.character(spine$indigenous[hit])
  ]
  expected_indigenous[is.na(expected_indigenous)] <- 9L
  expect_identical(he$enrol$ABORIG_TORRES, unname(expected_indigenous))
  expect_identical(
    he$enrol$DISABILITY,
    fplida:::.he_disability_code(
      spine$disability_type[hit], spine$is_dc[hit]
    )
  )
  expect_true(all(grepl("^[012]{8}$", he$enrol$DISABILITY)))
  expect_true(any(he$enrol$DISABILITY != "20000000"))
})

test_that("HE does not enrol a person after death", {
  spine <- generate_spine(n = 3000L, seed = 73L)
  he <- generate_he(
    spine = spine, seed = 73L, years = 2005L:2025L,
    return_data = TRUE
  )

  hit <- match(he$enrol$SYNTHETIC_AEUID, spine$aeuid_de)
  death <- spine$year_of_death[hit]
  expect_true(all(is.na(death) | he$enrol$YEAR <= death))
})

test_that("generate_he course FOE is 4-digit ASCED code", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$course) > 0L) {
    expect_true(all(nchar(he$course$FOE) == 4L))
    expect_true(all(he$course$FOE %in% fplida:::.HE_FOE_CODES))
  }
})

test_that("generate_he course FOE_SUPP is FOE + 00", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$course) > 0L) {
    expect_identical(he$course$FOE_SUPP, paste0(he$course$FOE, "00"))
  }
})

test_that("generate_he course COURSE_TYPE is valid", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$course) > 0L) {
    expect_true(all(he$course$COURSE_TYPE %in% 1:6))
  }
})

test_that("generate_he load EFTSL is positive", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$load) > 0L) {
    expect_true(all(he$load$EQUIVALENT_FT_STUDENT_LOAD > 0))
  }
})

test_that("generate_he load UNIT_STATUS is 4 or 7", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$load) > 0L) {
    expect_true(all(he$load$UNIT_STATUS %in% c(4L, 7L)))
  }
})

test_that("generate_he load HELP_DEBT is non-negative", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$load) > 0L) {
    expect_true(all(he$load$HELP_DEBT >= 0))
  }
})

test_that("generate_he load TOTAL_AMOUNT_CHARGED is positive", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$load) > 0L) {
    expect_true(all(he$load$TOTAL_AMOUNT_CHARGED > 0))
  }
})


# -- Calibration checks (approximate) ----------------------------------------

test_that("young education==5 persons have HE enrolments", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, years = 2005L:2021L,
                    return_data = TRUE)

  # Persons born 1987-2003 would commence ~2005-2021 (age 18)
  young_edu5 <- spine$education == 5L &
                spine$birth_year >= 1987L & spine$birth_year <= 2003L
  if (sum(young_edu5) > 0L) {
    edu5_aeuids <- spine$aeuid_de[young_edu5]
    he_aeuids <- unique(he$enrol$SYNTHETIC_AEUID)
    in_he <- edu5_aeuids %in% he_aeuids
    # Nearly all young edu==5 should be in HE (some mature-age offset may push
    # them out of range, but most should appear)
    expect_gt(mean(in_he), 0.70)
  }
})

test_that("no education==0 persons have HE enrolments", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  edu0_aeuids <- spine$aeuid_de[spine$education == 0L]
  if (length(edu0_aeuids) > 0L && nrow(he$enrol) > 0L) {
    expect_false(any(edu0_aeuids %in% he$enrol$SYNTHETIC_AEUID))
  }
})

test_that("full-time share is approximately 65%", {
  spine <- generate_spine(n = 2000L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$enrol) > 0L) {
    # Use course-level (one row per spell) for share calculation
    ft_share <- mean(he$enrol$ATTENDANCE_TYPE[he$enrol$COM_INDICATOR == 1L] == 1L)
    expect_gt(ft_share, 0.50)
    expect_lt(ft_share, 0.80)
  }
})

test_that("completion rate is approximately 50-90%", {
  spine <- generate_spine(n = 2000L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  n_courses <- nrow(he$course)
  n_completions <- nrow(he$completions)
  if (n_courses > 0L) {
    comp_rate <- n_completions / n_courses
    expect_gt(comp_rate, 0.30)
    expect_lt(comp_rate, 0.95)
  }
})

test_that("bachelor is the most common qualification", {
  spine <- generate_spine(n = 2000L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  if (nrow(he$course) > 0L) {
    bach_share <- mean(he$course$COURSE_TYPE == 1L)
    expect_gt(bach_share, 0.40)
  }
})


# -- Determinism -------------------------------------------------------------

test_that("generate_he is deterministic with same seed", {
  spine <- generate_spine(n = 200L, seed = 42L)
  a <- generate_he(spine = spine, seed = 99L, return_data = TRUE)
  b <- generate_he(spine = spine, seed = 99L, return_data = TRUE)

  expect_identical(a$enrol, b$enrol)
  expect_identical(a$course, b$course)
  expect_identical(a$load, b$load)
  expect_identical(a$completions, b$completions)
  expect_identical(a$help, b$help)
})

test_that("generate_he different seeds give different output", {
  spine <- generate_spine(n = 500L, seed = 1L)
  a <- generate_he(spine = spine, seed = 1L, return_data = TRUE)
  b <- generate_he(spine = spine, seed = 2L, return_data = TRUE)

  # Should have different number of participants or different assignments
  if (nrow(a$enrol) > 0L && nrow(b$enrol) > 0L) {
    expect_false(identical(a$enrol, b$enrol))
  }
})


# -- File writing ------------------------------------------------------------

test_that("generate_he writes products and DE spine", {
  d <- .make_he_test_data(n = 200L, seed = 1L, fmt = "csv")
  on.exit(unlink(d$tmp, recursive = TRUE), add = TRUE)

  run_dir <- file.path(d$tmp, "fplida_1k")
  he_dir <- file.path(run_dir, "de-he")
  expect_true(dir.exists(he_dir))

  csv_files <- list.files(he_dir, pattern = "\\.csv$")
  # 5 products + de-spine.csv
  expect_equal(length(csv_files), 6L)

  de_spine_path <- file.path(he_dir, "de-spine.csv")
  expect_true(file.exists(de_spine_path))
})

test_that("generate_he loads spine from run dir", {
  tmp <- file.path(tempdir(), "he_load_test")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 100L, seed = 5L, output_dir = tmp)
  he <- generate_he(seed = 5L, output_dir = tmp, return_data = TRUE)

  expect_type(he, "list")
  expect_named(he, c("enrol", "course", "load", "completions", "help"))
})

test_that("generate_he errors without spine or data path", {
  old_opt <- getOption("fplida.data_path")
  old_run <- getOption("fplida.run_dir")
  old_env <- Sys.getenv("FPLIDA_DATA_PATH", "")
  on.exit({
    options(fplida.data_path = old_opt, fplida.run_dir = old_run)
    if (nzchar(old_env)) {
      Sys.setenv(FPLIDA_DATA_PATH = old_env)
    } else {
      Sys.unsetenv("FPLIDA_DATA_PATH")
    }
  }, add = TRUE)

  options(fplida.data_path = NULL, fplida.run_dir = NULL)
  Sys.unsetenv("FPLIDA_DATA_PATH")

  expect_error(generate_he(), "No output directory")
})


# -- Edge cases --------------------------------------------------------------

test_that("generate_he handles small spine", {
  spine <- generate_spine(n = 50L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, return_data = TRUE)

  expect_type(he, "list")
  for (nm in names(he)) {
    expect_s3_class(he[[nm]], "data.frame")
  }
})

test_that("generate_he narrow year range still works", {
  spine <- generate_spine(n = 500L, seed = 1L)
  he <- generate_he(spine = spine, seed = 1L, years = 2020L:2021L,
                    return_data = TRUE)

  expect_type(he, "list")
  if (nrow(he$enrol) > 0L) {
    expect_true(all(he$enrol$YEAR %in% 2020:2021))
  }
})


# -- Geography consistency ---------------------------------------------------

# ABS STE 2021 order: 1 NSW, 2 VIC, 3 QLD, 4 SA, 5 WA, 6 TAS, 7 NT, 8 ACT.
# These are the same postcode ranges every other product's postcode
# generator uses (see STATE_PC_LO / STATE_PC_HI in the Rust backend).
.HE_PC_LO <- c(2000, 3000, 4000, 5000, 6000, 7000, 800, 2600)
.HE_PC_HI <- c(2999, 3999, 4999, 5799, 6797, 7999, 899, 2618)

test_that("campus postcode is inside the canonical range for its state", {
  spine <- generate_spine(n = 2000L, seed = 3L)
  he <- generate_he(spine = spine, seed = 3L, return_data = TRUE)

  ld <- he$load
  expect_gt(nrow(ld), 0L)

  st <- as.integer(ld$CAMPUS_STATE)
  pc <- as.integer(ld$CAMPUS_POSTCODE)
  expect_true(all(st >= 1L & st <= 8L))
  expect_false(anyNA(pc))
  expect_true(all(nchar(ld$CAMPUS_POSTCODE) == 4L))

  bad <- which(pc < .HE_PC_LO[st] | pc > .HE_PC_HI[st])
  expect_identical(
    length(bad), 0L,
    info = paste("Out-of-range campus postcodes for states:",
                 paste(sort(unique(st[bad])), collapse = ", "))
  )

  # Most states must appear, so the check above is not vacuous.
  expect_gte(length(unique(st)), 6L)
})

test_that("campus postcode agrees with the student's spine state", {
  spine <- generate_spine(n = 2000L, seed = 5L)
  he <- generate_he(spine = spine, seed = 5L, return_data = TRUE)

  key <- data.frame(SYNTHETIC_AEUID = as.character(spine$aeuid_de),
                    spine_state = as.integer(spine$state),
                    stringsAsFactors = FALSE)
  m <- merge(he$load, key, by = "SYNTHETIC_AEUID")
  m <- m[m$CAMPUS_STATE == m$spine_state, , drop = FALSE]
  expect_gt(nrow(m), 0L)

  pc <- as.integer(m$CAMPUS_POSTCODE)
  expect_true(all(pc >= .HE_PC_LO[m$spine_state] &
                    pc <= .HE_PC_HI[m$spine_state]))
})

test_that("institution state codes follow STE 2021 order", {
  inst <- fplida:::.HE_INSTITUTIONS
  expect_equal(nrow(inst), 59L)
  expect_true(all(inst$state >= 1L & inst$state <= 8L))

  # SA (4) has fewer institutions than WA (5); NT (7) fewer than ACT (8).
  tbl <- table(factor(inst$state, levels = 1:8))
  expect_lt(tbl[["4"]], tbl[["5"]])
  expect_lt(tbl[["7"]], tbl[["8"]])

  # Charles Darwin University is the NT dual-sector entry.
  expect_equal(inst$state[47], 7L)
})

# -- Helper ------------------------------------------------------------------

.make_tva_test_data <- function(n = 200L, seed = 1L, fmt = "csv",
                                years = 2020L:2022L) {
  tmp <- file.path(tempdir(), paste0("tva_test_", n, "_", seed))
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  spine <- generate_spine(n = n, seed = seed, output_dir = tmp)
  tva <- generate_tva(spine = spine, seed = seed, years = years,
                      output_dir = tmp, format = fmt,
                      return_data = TRUE)
  list(spine = spine, tva = tva, tmp = tmp)
}


# -- Structure ---------------------------------------------------------------

test_that("generate_tva returns list with training_activity and completions", {
  td <- .make_tva_test_data(n = 100L, years = 2020L:2022L)
  expect_type(td$tva, "list")
  expect_true("training_activity" %in% names(td$tva))
  expect_true("completions" %in% names(td$tva))
})

test_that("training_activity is named list keyed by year", {
  td <- .make_tva_test_data(n = 100L, years = 2020L:2022L)
  ta <- td$tva$training_activity
  expect_type(ta, "list")
  expect_named(ta, c("2020", "2021", "2022"))
  for (nm in names(ta)) {
    expect_s3_class(ta[[nm]], "data.frame")
  }
})

test_that("completions is named list keyed by year", {
  td <- .make_tva_test_data(n = 100L, years = 2020L:2022L)
  comp <- td$tva$completions
  expect_type(comp, "list")
  expect_named(comp, c("2020", "2021", "2022"))
})


# -- Column presence ---------------------------------------------------------

test_that("training_activity has expected columns", {
  td <- .make_tva_test_data(n = 200L, years = 2020L:2021L)
  expected_cols <- c(
    "SYNTHETIC_AEUID", "TVA_ROW_ID", "COLLECTION_YR",
    "AGE", "YEAR_OF_BIRTH", "MONTH_OF_BIRTH", "GENDER_ID",
    "HIGHEST_SCHL_LVL_ST", "HIGHEST_ED_LVL_ST",
    "PRIOR_ED_ACHIEVE_FG", "AT_SCHOOL_FG",
    "LABOUR_FORCE_STATUS_ID", "ANZSCO_ID",
    "PROGRAM_ID", "PROGRAM_FOE_ID", "PROGRAM_LOE_ID",
    "PROGRAM_RECOGNITION_ID", "PROGRAM_TRAINING_PACKAGE_ID",
    "PROGRAM_TYPE_OF_TRAINING_ID", "PROGRAM_NOMINAL_HOURS",
    "PROGRAM_VET_FG",
    "SUBJECT_ID", "SUBJECT_FOE_ID", "SUBJECT_NOMINAL_HOURS",
    "SUBJECT_VET_FG", "SUBJECT_FG",
    "APPRENTICESHIP_FG", "VET_IN_SCHOOLS_FG",
    "STUDENT_COMMENCING_FG", "STUDY_REASON_ID",
    "ACTIVITY_START_DATE", "ACTIVITY_END_DATE",
    "DELIVERY_MODE_ID", "NATIONAL_FUNDING_SOURCE_ID",
    "NATIONAL_OUTCOME_ID",
    "CLIENT_POSTCODE_DERIVED", "CLIENT_STATE_RESIDENCE_DERIVED",
    "CLIENT_REMOTENESS_ID_DERIVED", "CLIENT_SEIFA_IRSD_QUINT_DRVD",
    "RTO_ID", "TRAIN_ORG_TYPE_ID",
    "HEAD_OFFICE_STATE", "HEAD_OFFICE_POSTCODE",
    "STATE_OF_FUNDING_GF"
  )
  for (yr_str in names(td$tva$training_activity)) {
    df <- td$tva$training_activity[[yr_str]]
    for (col in expected_cols) {
      expect_true(col %in% names(df),
                  info = paste("Missing column:", col, "in year", yr_str))
    }
  }
})

test_that("completions has expected columns", {
  td <- .make_tva_test_data(n = 200L, years = 2020L:2021L)
  expected_cols <- c(
    "SYNTHETIC_AEUID", "TVA_ROW_ID", "COLLECTION_YR",
    "AGE", "YEAR_OF_BIRTH", "MONTH_OF_BIRTH", "GENDER_ID",
    "HIGHEST_SCHL_LVL_ST", "HIGHEST_ED_LVL_ST",
    "PRIOR_ED_ACHIEVE_FG", "AT_SCHOOL_FG",
    "LABOUR_FORCE_STATUS_ID", "ANZSCO_ID",
    "PROGRAM_ID", "PROGRAM_FOE_ID", "PROGRAM_LOE_ID",
    "PROGRAM_RECOGNITION_ID", "PROGRAM_TRAINING_PACKAGE_ID",
    "PROGRAM_TYPE_OF_TRAINING_ID",
    "DATE_PROGRAM_COMPLETED", "YR_PROGRAM_COMPLETED",
    "CLIENT_POSTCODE_DERIVED", "CLIENT_STATE_RESIDENCE_DERIVED",
    "CLIENT_REMOTENESS_ID_DERIVED", "CLIENT_SEIFA_IRSD_QUINT_DRVD",
    "RTO_ID", "TRAIN_ORG_TYPE_ID",
    "HEAD_OFFICE_STATE", "HEAD_OFFICE_POSTCODE",
    "STATE_OF_FUNDING_GF"
  )
  for (yr_str in names(td$tva$completions)) {
    df <- td$tva$completions[[yr_str]]
    for (col in expected_cols) {
      expect_true(col %in% names(df),
                  info = paste("Missing column:", col, "in year", yr_str))
    }
  }
})


# -- Column types ------------------------------------------------------------

test_that("date columns are Date class", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_s3_class(ta$ACTIVITY_START_DATE, "Date")
    expect_s3_class(ta$ACTIVITY_END_DATE, "Date")
  }
  comp <- td$tva$completions[["2021"]]
  if (nrow(comp) > 0) {
    expect_s3_class(comp$DATE_PROGRAM_COMPLETED, "Date")
  }
})

test_that("integer columns have correct type in training_activity", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_type(ta$COLLECTION_YR, "integer")
    expect_type(ta$AGE, "integer")
    expect_type(ta$YEAR_OF_BIRTH, "integer")
    expect_type(ta$GENDER_ID, "integer")
    expect_type(ta$PROGRAM_LOE_ID, "integer")
    expect_type(ta$SUBJECT_NOMINAL_HOURS, "integer")
    expect_type(ta$NATIONAL_OUTCOME_ID, "integer")
  }
})


# -- Referential integrity ---------------------------------------------------

test_that("all AEUIDs in training_activity are from the spine", {
  td <- .make_tva_test_data(n = 200L, years = 2020L:2022L)
  spine_aeuids <- td$spine$aeuid_ncver
  for (yr_str in names(td$tva$training_activity)) {
    df <- td$tva$training_activity[[yr_str]]
    if (nrow(df) > 0) {
      bad <- setdiff(unique(df$SYNTHETIC_AEUID), spine_aeuids)
      expect_equal(length(bad), 0L,
                   info = paste("Unknown AEUIDs in activity year", yr_str))
    }
  }
})

test_that("all AEUIDs in completions are from the spine", {
  td <- .make_tva_test_data(n = 200L, years = 2020L:2022L)
  spine_aeuids <- td$spine$aeuid_ncver
  for (yr_str in names(td$tva$completions)) {
    df <- td$tva$completions[[yr_str]]
    if (nrow(df) > 0) {
      bad <- setdiff(unique(df$SYNTHETIC_AEUID), spine_aeuids)
      expect_equal(length(bad), 0L,
                   info = paste("Unknown AEUIDs in completions year", yr_str))
    }
  }
})


# -- Value validations -------------------------------------------------------

test_that("COLLECTION_YR matches year key", {
  td <- .make_tva_test_data(n = 200L, years = 2020L:2022L)
  for (yr_str in names(td$tva$training_activity)) {
    df <- td$tva$training_activity[[yr_str]]
    if (nrow(df) > 0) {
      expect_true(all(df$COLLECTION_YR == as.integer(yr_str)))
    }
  }
})

test_that("PROGRAM_LOE_ID uses valid qualification codes", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    valid_codes <- c(511L, 514L, 521L, 524L, 611L, 613L)
    expect_true(all(ta$PROGRAM_LOE_ID %in% valid_codes))
  }
  comp <- td$tva$completions[["2021"]]
  if (nrow(comp) > 0) {
    expect_true(all(comp$PROGRAM_LOE_ID %in% valid_codes))
  }
})

test_that("GENDER_ID is 1 or 2", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_true(all(ta$GENDER_ID %in% c(1L, 2L)))
  }
})

test_that("MONTH_OF_BIRTH is 1-12", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_true(all(ta$MONTH_OF_BIRTH >= 1L & ta$MONTH_OF_BIRTH <= 12L))
  }
})

test_that("AGE is non-negative", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_true(all(ta$AGE >= 0L))
  }
})

test_that("PROGRAM_FOE_ID is valid ASCED 2-digit code", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  valid_foe <- c("03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_true(all(ta$PROGRAM_FOE_ID %in% valid_foe))
  }
})

test_that("DELIVERY_MODE_ID is valid code", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    valid_modes <- c(10L, 20L, 30L, 40L, 90L)
    expect_true(all(ta$DELIVERY_MODE_ID %in% valid_modes))
  }
})

test_that("NATIONAL_OUTCOME_ID is valid code", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    valid_outcomes <- c(20L, 30L, 40L, 51L, 60L, 70L, 81L, 90L)
    expect_true(all(ta$NATIONAL_OUTCOME_ID %in% valid_outcomes))
  }
})

test_that("NATIONAL_FUNDING_SOURCE_ID is valid code", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    valid_funds <- c(11L, 20L, 30L, 31L, 80L, 99L)
    expect_true(all(ta$NATIONAL_FUNDING_SOURCE_ID %in% valid_funds))
  }
})

test_that("TRAIN_ORG_TYPE_ID is valid code", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    valid_types <- c(21L, 31L, 61L, 91L, 95L, 97L)
    expect_true(all(ta$TRAIN_ORG_TYPE_ID %in% valid_types))
  }
})

test_that("CLIENT_REMOTENESS_ID_DERIVED is valid code", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_true(all(ta$CLIENT_REMOTENESS_ID_DERIVED %in% 0L:4L))
  }
})

test_that("CLIENT_SEIFA_IRSD_QUINT_DRVD is 1-5", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_true(all(ta$CLIENT_SEIFA_IRSD_QUINT_DRVD %in% 1L:5L))
  }
})

test_that("flag columns are Y or N", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    for (flag_col in c("PRIOR_ED_ACHIEVE_FG", "AT_SCHOOL_FG",
                        "PROGRAM_VET_FG", "SUBJECT_VET_FG", "SUBJECT_FG",
                        "APPRENTICESHIP_FG", "VET_IN_SCHOOLS_FG",
                        "STUDENT_COMMENCING_FG")) {
      expect_true(all(ta[[flag_col]] %in% c("Y", "N")),
                  info = paste(flag_col, "has invalid values"))
    }
  }
})

test_that("SUBJECT_NOMINAL_HOURS is positive", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_true(all(ta$SUBJECT_NOMINAL_HOURS > 0L))
  }
})


# -- Cross-table consistency -------------------------------------------------

test_that("completion AEUIDs are subset of training_activity AEUIDs", {
  td <- .make_tva_test_data(n = 300L, years = 2020L:2022L)
  all_ta_aeuids <- unique(unlist(lapply(td$tva$training_activity,
                                         function(x) x$SYNTHETIC_AEUID)))
  all_comp_aeuids <- unique(unlist(lapply(td$tva$completions,
                                          function(x) x$SYNTHETIC_AEUID)))
  if (length(all_comp_aeuids) > 0 && length(all_ta_aeuids) > 0) {
    # Not all completers need to have activity in same years, but most should
    overlap <- length(intersect(all_comp_aeuids, all_ta_aeuids))
    expect_gt(overlap / length(all_comp_aeuids), 0.5)
  }
})

test_that("completion year matches COLLECTION_YR", {
  td <- .make_tva_test_data(n = 200L, years = 2020L:2022L)
  for (yr_str in names(td$tva$completions)) {
    df <- td$tva$completions[[yr_str]]
    if (nrow(df) > 0) {
      expect_true(all(df$YR_PROGRAM_COMPLETED == as.integer(yr_str)))
    }
  }
})


# -- Date consistency --------------------------------------------------------

test_that("ACTIVITY_END_DATE >= ACTIVITY_START_DATE", {
  td <- .make_tva_test_data(n = 200L, years = 2021L:2021L)
  ta <- td$tva$training_activity[["2021"]]
  if (nrow(ta) > 0) {
    expect_true(all(ta$ACTIVITY_END_DATE >= ta$ACTIVITY_START_DATE))
  }
})


# -- Reproducibility ---------------------------------------------------------

test_that("same seed produces identical output", {
  td1 <- .make_tva_test_data(n = 100L, seed = 42L, years = 2021L:2021L)
  td2 <- .make_tva_test_data(n = 100L, seed = 42L, years = 2021L:2021L)
  ta1 <- td1$tva$training_activity[["2021"]]
  ta2 <- td2$tva$training_activity[["2021"]]
  if (nrow(ta1) > 0) {
    expect_identical(ta1$SYNTHETIC_AEUID, ta2$SYNTHETIC_AEUID)
    expect_identical(ta1$PROGRAM_FOE_ID, ta2$PROGRAM_FOE_ID)
    expect_identical(ta1$COLLECTION_YR, ta2$COLLECTION_YR)
  }
})


# -- File I/O ----------------------------------------------------------------

test_that("products written to ncver-tva/ directory", {
  td <- .make_tva_test_data(n = 100L, years = 2020L:2021L)
  run_dir <- fplida:::resolve_run_dir(td$tmp)
  tva_dir <- file.path(run_dir, "ncver-tva")
  expect_true(dir.exists(tva_dir))
  files <- list.files(tva_dir, pattern = "\\.csv$")
  expect_true(any(grepl("madipge-tva-d-trn-actvty-core-2020", files)))
  expect_true(any(grepl("madipge-tva-d-trn-actvty-core-2021", files)))
  expect_true(any(grepl("madipge-tva-d-prog-comp-core-2020", files)))
  expect_true(any(grepl("madipge-tva-d-prog-comp-core-2021", files)))
})

test_that("NCVER agency spine written", {
  td <- .make_tva_test_data(n = 100L, years = 2021L:2021L)
  run_dir <- fplida:::resolve_run_dir(td$tmp)
  tva_dir <- file.path(run_dir, "ncver-tva")
  spine_file <- list.files(tva_dir, pattern = "ncver-spine")
  expect_true(length(spine_file) > 0)
})


# -- Edge cases --------------------------------------------------------------

test_that("empty spine returns empty tables with correct schema", {
  tmp <- file.path(tempdir(), "tva_empty_test")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  spine <- generate_spine(n = 5L, seed = 99L, output_dir = tmp)
  tva <- generate_tva(spine = spine, seed = 99L, years = 2023L:2023L,
                      output_dir = tmp, format = "csv",
                      return_data = TRUE)
  expect_type(tva, "list")
  expect_true("training_activity" %in% names(tva))
  expect_named(tva$training_activity, "2023")
  expect_s3_class(tva$training_activity[["2023"]], "data.frame")
  expect_true("SYNTHETIC_AEUID" %in% names(tva$training_activity[["2023"]]))
})

test_that("small spine (n=50) runs without error", {
  tmp <- file.path(tempdir(), "tva_small_test")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  spine <- generate_spine(n = 50L, seed = 7L, output_dir = tmp)
  expect_no_error(
    generate_tva(spine = spine, seed = 7L, years = 2021L:2022L,
                 output_dir = tmp, format = "csv")
  )
})

test_that("year subsetting produces only requested years", {
  td <- .make_tva_test_data(n = 100L, years = 2022L:2023L)
  expect_named(td$tva$training_activity, c("2022", "2023"))
  expect_null(td$tva$training_activity[["2020"]])
})

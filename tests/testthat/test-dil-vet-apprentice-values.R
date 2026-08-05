test_that("TVA source rules preserve exact fields and official domains", {
  n <- 12L
  spine <- data.frame(
    id = seq_len(n),
    birth_year = 1980L,
    stringsAsFactors = FALSE
  )
  source <- data.frame(
    AT_SCHOOL_FG = rep(c("N", "Y"), length.out = n),
    HIGHEST_ED_LVL_ST = rep(0L:5L, length.out = n),
    HIGHEST_SCHL_LVL_ST = rep(8L:12L, length.out = n),
    LABOUR_FORCE_STATUS_ID = rep(1L:4L, length.out = n),
    PRIOR_ED_ACHIEVE_FG = rep(c("N", "Y"), length.out = n),
    PROGRAM_ID = sprintf("P%09d", seq_len(n)),
    PROGRAM_FOE_ID = rep(c("0301", "0403"), length.out = n),
    PROGRAM_LOE_ID = rep(c(511L, 514L, 521L), length.out = n),
    PROGRAM_RECOGNITION_ID = 11L,
    PROGRAM_TRAINING_PACKAGE_ID = rep(c("BSB", "CPC"), length.out = n),
    NATIONAL_FUNDING_SOURCE_ID = rep(c(11L, 20L, 30L), length.out = n),
    CLIENT_POSTCODE_DERIVED = rep(c("2000", "3000", "4000"), each = 4L),
    CLIENT_STATE_RESIDENCE_DERIVED = rep(1L:3L, each = 4L),
    CLIENT_REMOTENESS_ID_DERIVED = rep(0L:4L, length.out = n),
    CLIENT_SEIFA_IRSD_QUINT_DRVD = rep(1L:5L, length.out = n),
    RTO_ID = rep(c("RTO00001", "RTO00002", "RTO00003"), each = 4L),
    TRAIN_ORG_TYPE_ID = rep(c(31L, 91L, 95L), each = 4L),
    stringsAsFactors = FALSE
  )
  period <- list(end_year = 2024L)
  value <- function(name) {
    fplida:::.dil_tva_source_value(
      name, "", source, spine, 20260803L, period
    )
  }

  expect_identical(value("CLIENT_REMOTE_ID_DERIVED"),
                   source$CLIENT_REMOTENESS_ID_DERIVED)
  expect_identical(value("TRAIN_PACKAGE_ID"),
                   source$PROGRAM_TRAINING_PACKAGE_ID)
  expect_identical(value("PROGRAM_HIGHEST_FUNDING_SOURCE"),
                   source$NATIONAL_FUNDING_SOURCE_ID)
  expect_identical(value("COUNTRY_OF_DELIVERY"), rep("1101", n))
  expect_identical(value("DEL_LOC_REMOTENESS_ID_DRVD"),
                   source$CLIENT_REMOTENESS_ID_DERIVED)

  delivery_id <- value("DEL_LOC_ID_ENCRYPT")
  expect_match(delivery_id, "^DL[0-9]{8}$", all = TRUE)
  expect_length(unique(delivery_id[source$CLIENT_POSTCODE_DERIVED == "2000"]),
                1L)

  rto_remote <- value("RTO_HO_REMOTENESS_ID_DRVD")
  rto_seifa <- value("RTO_HO_SEIFA_IRSD_QUIN_DRVD")
  expect_true(all(rto_remote %in% 0L:4L))
  expect_true(all(rto_seifa %in% 1L:5L))
  expect_length(unique(rto_remote[source$RTO_ID == "RTO00001"]), 1L)
  expect_length(unique(rto_seifa[source$RTO_ID == "RTO00001"]), 1L)
  expect_true(all(value("SUBMITTER_TYPE") %in% c("STA", "BOS", "RTO")))

  for (name in fplida:::.dil_tva_unsupported_variables) {
    expect_null(value(name), info = name)
  }
})

test_that("A&T status, duration, date, and school relationships are coherent", {
  n <- 16L
  start <- as.Date("2013-01-01") + seq(0L, by = 240L, length.out = n)
  days <- rep(c(365L, 540L, 730L, 1095L), length.out = n)
  status <- rep(c("Completed", "In Training", "Cancelled", "Expired"),
                length.out = n)
  end <- start + days
  end[status %in% c("In Training", "Expired")] <- as.Date(NA)
  source <- data.frame(
    STATUS = status,
    START_DATE = start,
    END_DATE = end,
    DAYS_IN_TRAINING = days,
    QUALIFICATION_LEVEL = rep(2L:5L, length.out = n),
    ANZSCO = rep(c("531111", "331212", "423111", "341111"),
                 length.out = n),
    SCHOOL_BASED = rep(c("Y", "N", "N", "Y"), length.out = n),
    STATE_ASGS_2021 = rep(1L:8L, length.out = n),
    stringsAsFactors = FALSE
  )
  spine <- data.frame(
    id = seq_len(n),
    birth_year = rep(1988L:2003L, length.out = n),
    education = rep(0L:5L, length.out = n),
    country_of_birth_sacc = rep(c(1101L, 7106L, 5204L, 6104L),
                                length.out = n),
    disability_onset_year = rep(c(NA_integer_, 2010L, NA_integer_, 2020L),
                                length.out = n),
    stringsAsFactors = FALSE
  )
  period <- list(end_year = 2024L)
  value <- function(name) {
    fplida:::.dil_apprentice_source_value(
      name, "", source, spine, 20260803L, period
    )
  }

  expect_identical(
    value("CURRENTSTATUS"),
    rep(c("04", "01", "06", "05"), length.out = n)
  )
  expect_identical(value("TRAININGCONTRACTSTATUS"),
                   value("CURRENTSTATUS"))
  expect_identical(value("DAYSINTRAINING"), days)
  expect_identical(value("DAYSONSUSPENSION"), rep(0L, n))
  expect_identical(value("APPRENTICEEARLIEST"), start)
  expect_identical(value("APPRENTICELATEST"), start + days)
  expect_identical(value("TRAININGCONTRACTEARLIEST"), start)
  expect_identical(value("TRAININGCONTRACTLATEST"), start + days)
  expect_identical(value("APPRENTICEAPPRENTICESHIPNUMBER"), rep(1L, n))
  expect_identical(value("APPRENTICETRNGCONTRANO"), rep(1L, n))

  highest <- value("HIGHESTSCHOOLLEVELCODE")
  at_school <- value("ATSCHOOLLEVELCODE")
  expect_true(all(highest %in% c("02", "08", "09", "10", "11", "12")))
  expect_true(all(at_school %in% c("08", "09", "10", "11", "12", "99")))
  school_rows <- source$SCHOOL_BASED == "Y"
  expect_true(all(as.integer(highest[school_rows]) <
                    as.integer(at_school[school_rows])))
  expect_true(all(at_school[!school_rows] == "99"))

  in_may <- value("IN_MAY_23")
  expected_in_may <- as.integer(
    start <= as.Date("2023-05-31") & start + days >= as.Date("2023-05-01")
  )
  expect_identical(in_may, expected_in_may)
})

test_that("A&T demographic and VET codes use official code frames", {
  n <- 24L
  source <- data.frame(
    STATUS = rep(c("Completed", "In Training"), length.out = n),
    START_DATE = as.Date("2010-01-01") + seq(0L, by = 240L, length.out = n),
    END_DATE = as.Date(NA) + seq_len(n),
    DAYS_IN_TRAINING = rep(730L, n),
    QUALIFICATION_LEVEL = rep(2L:5L, length.out = n),
    ANZSCO = rep(c("531111", "331212", "423111", "341111", "262100"),
                 length.out = n),
    SCHOOL_BASED = rep(c("N", "Y"), length.out = n),
    STATE_ASGS_2021 = rep(1L:8L, length.out = n),
    stringsAsFactors = FALSE
  )
  spine <- data.frame(
    id = seq_len(n),
    birth_year = 1980L + seq_len(n),
    education = rep(0L:5L, length.out = n),
    country_of_birth_sacc = rep(c(1101L, 7106L, 5204L), length.out = n),
    disability_onset_year = rep(c(NA_integer_, 2018L), length.out = n),
    stringsAsFactors = FALSE
  )
  period <- list(end_year = 2024L)
  value <- function(name) {
    fplida:::.dil_apprentice_source_value(
      name, "", source, spine, 20260803L, period
    )
  }

  expect_setequal(unique(value("STA")), sprintf("%02d", 1L:8L))
  expect_true(all(value("RTOTYPE") %in% c(
    "21", "25", "27", "31", "41", "51", "53", "61", "91", "93",
    "95", "97"
  )))
  expect_true(all(value("SELFASSESSEDDISABILITYCODE") %in% c("Y", "N")))

  prior <- stats::na.omit(value("PRIOREDUACHIEVEMENTIDENT"))
  expect_true(all(prior %in% c("008", "410", "420", "511", "514")))
  expect_true(all(is.na(value("PRIOREDUACHIEVEMENTIDENT"))[
    spine$education <= 2L
  ]))

  language <- value("LANGUAGEATHOMECODE")
  valid_languages <- c("1201", fplida:::.dil_at_language_frame()$code)
  expect_true(all(grepl("^[0-9]{4}$", language)))
  expect_true(all(language %in% valid_languages))

  employment <- value("EMPLOYMENTARRANGEMENTCODE")
  legacy <- source$START_DATE < as.Date("2016-07-01")
  expect_true(all(employment[legacy] %in% sprintf("%02d", 11L:16L)))
  expect_true(all(is.na(employment[!legacy])))
})

test_that("A&T qualification fields are exact and mutually coherent", {
  n <- 40L
  source <- data.frame(
    QUALIFICATION_LEVEL = rep(2L:5L, each = 10L),
    ANZSCO = rep(c("531111", "331212", "423111", "341111", "262100"),
                 length.out = n),
    stringsAsFactors = FALSE
  )
  spine <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  period <- list(end_year = 2024L)
  value <- function(name) {
    fplida:::.dil_apprentice_source_value(
      name, "", source, spine, 20260803L, period
    )
  }

  generated <- data.frame(
    qualification_code = value("QUALCODE"),
    qualification_title = value("QUALTITLE"),
    program_level = value("QUALLEVEL"),
    field_of_education = value("FOECODE"),
    training_package_code = value("TPCODE"),
    training_package_title = value("TPTITLE"),
    stringsAsFactors = FALSE
  )
  reference <- fplida:::.dil_at_qualification_frame
  matched <- match(generated$qualification_code, reference$qualification_code)
  expect_false(anyNA(matched))
  for (field in names(generated)[-1L]) {
    expect_identical(generated[[field]], reference[[field]][matched],
                     info = field)
  }
  expect_identical(value("QUALIFICATIONCODE"), value("QUALCODE"))
  expected_level <- c(`2` = "521", `3` = "514", `4` = "511", `5` = "421")
  expect_identical(
    generated$program_level,
    unname(expected_level[as.character(source$QUALIFICATION_LEVEL)])
  )
})

test_that("A&T unsupported code frames remain explicit", {
  source <- data.frame(
    STATUS = "Completed",
    START_DATE = as.Date("2010-01-01"),
    DAYS_IN_TRAINING = 365L,
    QUALIFICATION_LEVEL = 3L,
    stringsAsFactors = FALSE
  )
  spine <- data.frame(id = 1L, stringsAsFactors = FALSE)
  period <- list(end_year = 2024L)
  for (name in fplida:::.dil_at_unsupported_variables) {
    expect_null(
      fplida:::.dil_apprentice_source_value(
        name, "", source, spine, 20260803L, period
      ),
      info = name
    )
  }
})

he_value_fixture <- function(n = 680L) {
  years <- rep(2005:2021, length.out = n)
  source <- data.frame(
    YEAR = years,
    COM_INDICATOR = rep(c(1L, 0L, 1L, 1L), length.out = n),
    CAMPUS_POSTCODE = rep(c("2000", "3000", "4000", "5000"),
                          length.out = n),
    stringsAsFactors = FALSE
  )
  countries <- c(1101L, 2102L, 3104L, 4202L, 5202L,
                 6101L, 7103L, 8104L, 9225L)
  spine <- data.frame(
    id = seq_len(n),
    birth_year = years - rep(c(18L, 20L, 24L, 35L, 45L), length.out = n),
    education = rep(0:5, length.out = n),
    citizenship = rep(c(1L, 1L, 1L, 2L), length.out = n),
    country_of_birth_sacc = rep(countries, length.out = n),
    indigenous = rep(c(4L, 4L, 4L, 1L), length.out = n),
    stringsAsFactors = FALSE
  )
  list(
    source = source,
    spine = spine,
    period = list(end_year = 2021L)
  )
}

test_that("HE parent education respects HEIMS and TCSI applicability", {
  fixture <- he_value_fixture()
  value <- function(name) {
    fplida:::.dil_he_source_value(
      name, fixture$source, fixture$spine, 20260803L, fixture$period
    )
  }
  parent1 <- value("EDUCATION_PARENT1")
  parent2 <- value("EDUCATION_PARENT2")
  year <- fixture$source$YEAR
  commencing <- fixture$source$COM_INDICATOR == 1L
  citizen <- fixture$spine$citizenship == 1L

  expect_true(all(is.na(parent1[year < 2010L])))
  expect_true(all(parent1[year >= 2010L & year <= 2020L & !commencing] ==
                    "01"))
  expect_true(all(parent1[
    year >= 2010L & year <= 2020L & commencing & !citizen
  ] == "98"))
  expect_true(all(is.na(parent1[year >= 2021L & !citizen])))
  substantive <- parent1[parent1 %in% c(
    "20", "21", "22", "23", "24", "25", "26", "49", "99"
  )]
  expect_gt(length(substantive), 0L)
  expect_true(anyNA(parent2))
})

test_that("HE language at home uses the TCSI language domain", {
  fixture <- he_value_fixture()
  language <- fplida:::.dil_he_source_value(
    "LANGUAGE_HOME", fixture$source, fixture$spine,
    20260803L, fixture$period
  )
  ascl <- fplida:::.dil_he_language_frame()
  expect_true(all(language %in% c(
    "0001", "8000", "9998", "9999", ascl$code
  )))
  expect_false(any(language == "1201"))
  expect_true(any(language == "0001"))
  expect_true(any(language %in% ascl$code))
})

test_that("HE language mapping treats only spine codes 2 to 4 as Indigenous", {
  spine <- data.frame(
    country_of_birth_sacc = rep(1101L, 4L),
    indigenous = 1:4
  )
  language <- fplida:::.dil_he_language_home(spine, rep(100L, 4L))
  expect_identical(language, c("0001", "8000", "8000", "8000"))
})

test_that("HE admission and scholarship codes follow their vintages", {
  fixture <- he_value_fixture()
  year <- fixture$source$YEAR
  commencing <- fixture$source$COM_INDICATOR == 1L
  admission <- fplida:::.dil_he_source_value(
    "NEW_ADMISSION", fixture$source, fixture$spine,
    20260803L, fixture$period
  )
  scholarship <- fplida:::.dil_he_source_value(
    "SCHOLARSHIP_TYPE", fixture$source, fixture$spine,
    20260803L, fixture$period
  )

  expect_true(all(admission[year <= 2020L & !commencing] == "01"))
  expect_true(all(admission[year <= 2020L & commencing] %in%
                    c("29", "31", "33", "34", "36", "37")))
  expect_true(all(admission[year >= 2021L] %in%
                    c("31", "32", "34", "40", "41", "42", "43")))
  expect_true(all(scholarship[year <= 2011L] %in%
                    c("00", "01", "02", "06")))
  expect_true(all(scholarship[year >= 2012L & year <= 2016L] %in%
                    c("00", "01", "02", "06", "07")))
  expect_true(all(scholarship[year >= 2017L & year <= 2020L] %in%
                    c("00", "06", "08", "09")))
  expect_true(all(stats::na.omit(scholarship[year >= 2021L]) %in%
                    c("09", "10", "11")))
})

test_that("HE separation and campus values retain their real meanings", {
  fixture <- he_value_fixture()
  separation <- fplida:::.dil_he_source_value(
    "SEPARATION_STATUS_CODE", fixture$source, fixture$spine,
    20260803L, fixture$period
  )
  campus <- fplida:::.dil_he_source_value(
    "CAMPUS_GLOBAL_REGION", fixture$source, fixture$spine,
    20260803L, fixture$period
  )
  expect_true(all(separation[fixture$source$YEAR <= 2020L] %in%
                    c("1", "2", "3", "9")))
  expect_true(all(is.na(separation[fixture$source$YEAR >= 2021L])))
  expect_identical(campus, rep("1", nrow(fixture$spine)))
})

test_that("the HE dispatcher resolves all seven original gap names", {
  fixture <- he_value_fixture()
  names <- c(
    "EDUCATION_PARENT1", "EDUCATION_PARENT2", "LANGUAGE_HOME",
    "NEW_ADMISSION", "SCHOLARSHIP_TYPE", "SEPARATION_STATUS_CODE",
    "CAMPUS_GLOBAL_REGION"
  )
  values <- lapply(names, function(name) {
    fplida:::.dil_dataset_source_value(
      name, "", "HE", fixture$source, fixture$spine,
      20260803L, fixture$period
    )
  })
  expect_true(all(vapply(values, length, integer(1)) == nrow(fixture$spine)))
  expect_true(all(vapply(values, function(value) any(!is.na(value)), logical(1))))
  expect_null(fplida:::.dil_he_source_value(
    "UNSUPPORTED", fixture$source, fixture$spine,
    20260803L, fixture$period
  ))
})

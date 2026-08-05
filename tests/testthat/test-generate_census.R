valid_census_occp_codes <- function() {
  # The bundled list, the same one the package validates OCCP against.
  as.character(fplida:::.census_valid_anzsco_codes())
}

census_night_spine <- function(spine) {
  spine[fplida:::.census_present_on_night(spine), , drop = FALSE]
}

test_that("generate_census returns expected structure from spine", {
  spine <- generate_spine(n = 500L, seed = 1L)
  census <- generate_census(spine = spine, seed = 1L)

  expect_type(census, "list")
  expect_named(census, c("person", "family", "dwelling"))
  expect_s3_class(census$person, "data.frame")
  expect_s3_class(census$family, "data.frame")
  expect_s3_class(census$dwelling, "data.frame")
})

test_that("generate_census person table has correct row count", {
  n <- 300L
  spine <- generate_spine(n = n, seed = 1L)
  census <- generate_census(spine = spine, seed = 1L)
  eligible_spine <- census_night_spine(spine)
  links <- fplida:::.census_household_links(eligible_spine)

  expect_equal(nrow(census$person), nrow(eligible_spine))
  expect_equal(nrow(census$dwelling), length(links$dwelling_id))
  expect_equal(nrow(census$family), length(links$family_id))
})

test_that("generate_census person table has expected columns", {
  spine <- generate_spine(n = 100L, seed = 1L)
  census <- generate_census(spine = spine, seed = 1L)
  person <- census$person

  expected_cols <- c(
    "SYNTHETIC_AEUID", "SEXP", "AGEP", "AGE5P", "AGE10P", "DOBYP",
    "STEUCP", "INGP", "BPLP", "YARP", "LANP", "RELP", "CITP",
    "HEAP", "HSCP", "MSTP", "INCP", "LFSP", "OCCP", "INDP", "HRWRP", "HRSP",
    "MTWP", "EMFP", "LFFP", "UEFP", "LFHRP", "EETP",
    "IFAGEP", "IFSEXP", "IFMSTP", "YARRP",
    "CLTHP", "HLTHP", "HOLHP",
    "COARASP", "COARDBP", "COARHDP", "COARMHP", "COASHDP", "COASLCP",
    "COCNHDP", "CODBHDP", "CODBKDP", "COHDKDP", "COHDMHP", "COLCMHP",
    "HMHCP", "HARTP", "HASTP", "HDIAP", "HHEDP",
    "HCANP", "HLUNP", "HKIDP", "HSTRP", "HDEMP",
    "VOLWP", "UNCAREP", "CHCAREP", "DOMP", "ADFP", "STUP", "ASSNP",
    "DWELLING_ID", "FAMILY_ID"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(person), info = paste("Missing column:", col))
  }

  expect_true(all(c("CALTHD", "CCLTHD", "CPLTHD", "CPLTHRD") %in%
                    names(census$dwelling)))
})

test_that("generate_census SYNTHETIC_AEUID matches spine ABS AEUID", {
  spine <- generate_spine(n = 200L, seed = 1L)
  census <- generate_census(spine = spine, seed = 1L)
  eligible_spine <- census_night_spine(spine)

  expect_identical(census$person$SYNTHETIC_AEUID, eligible_spine$aeuid_abs)
})

test_that("generate_census demographics match spine", {
  spine <- generate_spine(n = 500L, seed = 1L)
  census <- generate_census(spine = spine, seed = 1L)
  eligible_spine <- census_night_spine(spine)
  person <- census$person

  expect_identical(person$SEXP, eligible_spine$sex)
  expect_identical(person$AGEP, 2021L - eligible_spine$birth_year)
  expect_identical(person$DOBYP, eligible_spine$birth_year)
  expect_identical(person$STEUCP, eligible_spine$state)
})

test_that("generate_census excludes deaths before Census night", {
  spine <- generate_spine(n = 8L, seed = 51L)
  spine$year_of_death <- c(2020L, 2021L, 2021L, 2022L,
                           2021L, 2021L, NA_integer_, NA_integer_)
  spine$month_of_death <- c(12L, 8L, 8L, 1L, NA_integer_, 7L,
                            NA_integer_, NA_integer_)
  spine$day_of_death <- c(31L, 9L, 10L, 1L, 5L, NA_integer_,
                          NA_integer_, NA_integer_)

  person <- generate_census(spine = spine, seed = 52L)$person

  # Deaths on Census day, after Census day, and incompletely dated deaths
  # remain. Only deaths known to precede 10 August are excluded.
  expect_identical(
    person$SYNTHETIC_AEUID,
    spine$aeuid_abs[c(3L, 4L, 5L, 7L, 8L)]
  )
})

test_that("generate_census household links drive family and dwelling values", {
  spine <- generate_spine(n = 10L, seed = 61L)
  spine$year_of_death <- NA_integer_
  spine$month_of_death <- NA_integer_
  spine$day_of_death <- NA_integer_
  spine$household_id <- c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 4L, 4L, 5L)
  spine$state <- c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 4L, 5L, 6L)
  spine$birth_year <- c(1980L, 1982L, 1975L, 1978L, 2010L,
                        1980L, 2012L, 1990L, 1992L, 2008L)
  spine$sex <- c(1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L)

  census <- generate_census(spine = spine, seed = 62L)
  person <- census$person
  family <- census$family
  dwelling <- census$dwelling

  expect_identical(
    person$DWELLING_ID,
    sprintf("D%010d", c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 4L, 5L, 6L))
  )
  expect_identical(
    person$FAMILY_ID,
    c(rep("F0000000001", 2L), rep("F0000000002", 3L),
      rep("F0000000003", 2L), rep(NA_character_, 3L))
  )
  expect_identical(family$FMCF, c("1", "2", "3"))
  expect_identical(family$CDCF, c("00", "01", "08"))
  expect_identical(family$CPRF, c("2", "3", "2"))
  expect_identical(family$SSCF, c("1", "2", "@"))

  dwelling_index <- match(person$DWELLING_ID, dwelling$DWELLING_ID)
  expect_identical(dwelling$STEUCD[dwelling_index], person$STEUCP)
  expect_identical(dwelling$NPRD, c("2", "3", "2", "1", "1", "1"))
})

test_that("generate_census ASSNP follows disability onset and severity", {
  spine <- generate_spine(n = 600L, seed = 71L)
  spine$year_of_death <- NA_integer_
  spine$month_of_death <- NA_integer_
  spine$day_of_death <- NA_integer_
  spine$disability_onset_year <- NA_integer_
  spine$disability_severity <- NA_integer_

  spine$disability_onset_year[1:100] <- 2015L
  spine$disability_severity[1:100] <- 1L
  spine$disability_onset_year[101:200] <- 2015L
  spine$disability_severity[101:200] <- 2L
  spine$disability_onset_year[201:300] <- 2015L
  spine$disability_severity[201:300] <- 3L
  spine$disability_onset_year[301:400] <- 2022L
  spine$disability_severity[301:400] <- 1L
  spine$disability_severity[401:500] <- 1L

  assnp <- generate_census(spine = spine, seed = 72L)$person$ASSNP
  active_need <- seq_along(assnp) <= 200L
  no_active_need <- seq_along(assnp) > 200L & seq_along(assnp) <= 400L

  expect_true(all(assnp %in% c("1", "2", "&")))
  expect_true(all(assnp[active_need] %in% c("1", "&")))
  expect_true(all(assnp[active_need & assnp != "&"] == "1"))
  expect_true(all(assnp[no_active_need] %in% c("2", "&")))
  expect_true(all(assnp[no_active_need & assnp != "&"] == "2"))
  expect_true(all(assnp[401:500] == "&"))
  expect_true(all(assnp[501:600] %in% c("2", "&")))
})

test_that("generate_census is deterministic with same seed", {
  spine <- generate_spine(n = 200L, seed = 42L)
  a <- generate_census(spine = spine, seed = 99L)
  b <- generate_census(spine = spine, seed = 99L)

  expect_identical(a$person, b$person)
  expect_identical(a$dwelling, b$dwelling)
  expect_identical(a$family, b$family)
})

test_that("generate_census different seeds give different Census-specific data", {
  spine <- generate_spine(n = 200L, seed = 1L)
  a <- generate_census(spine = spine, seed = 1L)
  b <- generate_census(spine = spine, seed = 2L)

  # Spine-derived cols should be identical
  expect_identical(a$person$SEXP, b$person$SEXP)
  # Census-specific cols should differ (different seed)
  expect_false(identical(a$person$MSTP, b$person$MSTP))
})

test_that("generate_census SEXP values are valid", {
  spine <- generate_spine(n = 1000L, seed = 42L)
  census <- generate_census(spine = spine, seed = 42L)
  sexp_vals <- unique(census$person$SEXP)

  expect_true(all(sexp_vals %in% c(1L, 2L)))
})

test_that("generate_census AGE5P and AGE10P are derived from AGEP", {
  spine <- generate_spine(n = 200L, seed = 1L)
  census <- generate_census(spine = spine, seed = 1L)
  person <- census$person

  expect_identical(person$AGE5P, sprintf("%02d", pmin(person$AGEP %/% 5L + 1L, 21L)))
  expect_identical(person$AGE10P, sprintf("%02d", pmin(person$AGEP %/% 10L + 1L, 11L)))
})

test_that("generate_census under-15 get not-applicable codes", {
  spine <- generate_spine(n = 2000L, seed = 1L)
  census <- generate_census(spine = spine, seed = 1L)
  person <- census$person

  under15 <- person$AGEP < 15L
  if (any(under15)) {
    expect_true(all(person$MSTP[under15] == "@"))
    expect_true(all(person$HSCP[under15] == "@"))
    expect_true(all(person$INCP[under15] == "@@"))
    expect_true(all(person$LFSP[under15] == "@"))
    expect_true(all(person$OCCP[under15] == "@@@@@@"))
    expect_true(all(person$INDP[under15] == "@@@@"))
    expect_true(all(person$VOLWP[under15] == "@"))
    expect_true(all(person$HRWRP[under15] == "@@"))
    expect_true(all(person$MTWP[under15] == "@@@"))
  }
})

test_that("generate_census OCCP uses valid 6-digit ANZSCO codes", {
  valid_occp <- valid_census_occp_codes()
  spine <- generate_spine(n = 10000L, seed = 11L)
  census <- generate_census(spine = spine, seed = 12L)
  eligible_spine <- census_night_spine(spine)
  person <- census$person

  employed_adult <- eligible_spine$baseline_employed == 1L & person$AGEP >= 15L
  not_applicable <- !employed_adult

  expect_true(all(grepl("^([0-9]{6}|&&&&&&|@@@@@@|VVVVVV)$", person$OCCP)))
  expect_true(all(person$OCCP[not_applicable] == "@@@@@@"))
  expect_false(any(person$OCCP[employed_adult] == "@@@@@@"))
  expect_true(all(person$OCCP[employed_adult] %in% valid_occp))

  employed_occp <- person$OCCP[employed_adult]
  employed_major <- eligible_spine$anzsco_major[employed_adult]
  nfd <- grepl("^[1-8]00000$", employed_occp)
  manager_nfd_rate <- mean(employed_occp[employed_major == 1L] == "100000")
  other_nfd_rate <- mean(nfd[employed_major != 1L])

  expect_gt(sum(nfd), 0)
  expect_gt(manager_nfd_rate, other_nfd_rate)
})

test_that("generate_census health conditions are age-correlated", {
  spine <- generate_spine(n = 10000L, seed = 1L)
  census <- generate_census(spine = spine, seed = 1L)
  person <- census$person

  # Arthritis should be more prevalent in older people
  young <- person$AGEP < 30L
  old <- person$AGEP >= 60L

  arthritis_young <- mean(person$HARTP[young] == "011")
  arthritis_old <- mean(person$HARTP[old] == "011")
  expect_gt(arthritis_old, arthritis_young)

  # Dementia should be ~0 for young people
  dementia_young <- mean(person$HDEMP[young] == "041")
  expect_lt(dementia_young, 0.01)
})

test_that("generate_census CLTHP is derived from condition flags", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  census <- generate_census(spine = spine, seed = 1L)
  person <- census$person

  cond_sum <- (person$HMHCP == "091") + (person$HARTP == "011") +
              (person$HASTP == "021") + (person$HDIAP == "051") +
              (person$HHEDP == "061") + (person$HCANP == "031") +
              (person$HLUNP == "081") + (person$HKIDP == "071") +
              (person$HSTRP == "101") + (person$HDEMP == "041")

  valid <- person$CLTHP != "&"
  expect_true(all(person$CLTHP[valid & cond_sum == 0] == "0"))
  expect_true(all(person$CLTHP[valid & cond_sum == 1] == "1"))
  expect_true(all(person$CLTHP[valid & cond_sum == 2] == "2"))
  expect_true(all(person$CLTHP[valid & cond_sum >= 3] == "3"))
})

test_that("generate_census health derivations are internally coherent", {
  spine <- generate_spine(n = 5000L, seed = 21L)
  census <- generate_census(spine = spine, seed = 22L)
  person <- census$person

  selected <- person$CLTHP %in% c("1", "2", "3")
  expect_true(all(person$HLTHP[selected | person$HOLHP == "111"] == "122"))
  expect_true(all(person$HLTHP[person$CLTHP == "0" & person$HOLHP == "112"] == "121"))

  condition_columns <- c(
    "HARTP", "HASTP", "HCANP", "HDEMP", "HDIAP",
    "HHEDP", "HKIDP", "HLUNP", "HMHCP", "HSTRP"
  )
  comorbidity_columns <- c(
    "COARASP", "COARDBP", "COARHDP", "COARMHP", "COASHDP", "COASLCP",
    "COCNHDP", "CODBHDP", "CODBKDP", "COHDKDP", "COHDMHP", "COLCMHP"
  )
  not_stated <- person$CLTHP == "&"
  if (any(not_stated)) {
    expect_true(all(person$HLTHP[not_stated] == "&&&"))
    expect_true(all(person$HOLHP[not_stated] == "&&&"))
    expect_true(all(as.matrix(person[not_stated, condition_columns]) == "&&&"))
    expect_true(all(as.matrix(person[not_stated, comorbidity_columns]) == "&"))
  }

  pair_map <- list(
    COARASP = c("HARTP", "011", "HASTP", "021"),
    COARDBP = c("HARTP", "011", "HDIAP", "051"),
    COARHDP = c("HARTP", "011", "HHEDP", "061"),
    COARMHP = c("HARTP", "011", "HMHCP", "091"),
    COASHDP = c("HASTP", "021", "HHEDP", "061"),
    COASLCP = c("HASTP", "021", "HLUNP", "081"),
    COCNHDP = c("HCANP", "031", "HHEDP", "061"),
    CODBHDP = c("HDIAP", "051", "HHEDP", "061"),
    CODBKDP = c("HDIAP", "051", "HKIDP", "071"),
    COHDKDP = c("HHEDP", "061", "HKIDP", "071"),
    COHDMHP = c("HHEDP", "061", "HMHCP", "091"),
    COLCMHP = c("HLUNP", "081", "HMHCP", "091")
  )
  stated <- !not_stated
  for (nm in names(pair_map)) {
    spec <- pair_map[[nm]]
    expected <- ifelse(
      person[[spec[1L]]] == spec[2L] & person[[spec[3L]]] == spec[4L],
      "1",
      "2"
    )
    expect_identical(person[[nm]][stated], expected[stated], info = nm)
  }
})

test_that("generate_census labour and arrival derivations match their source fields", {
  spine <- generate_spine(n = 3000L, seed = 31L)
  person <- generate_census(spine = spine, seed = 32L)$person

  expected_emfp <- ifelse(person$LFSP %in% c("1", "2", "3"), "1",
                          ifelse(person$LFSP %in% c("4", "5", "6"), "2", "@"))
  expected_lffp <- ifelse(person$LFSP %in% as.character(1:5), "1",
                          ifelse(person$LFSP == "6", "2", "@"))
  expected_uefp <- ifelse(person$LFSP %in% c("4", "5"), "1",
                          ifelse(person$LFSP %in% c("1", "2", "3"), "2", "@"))

  expect_identical(person$EMFP, expected_emfp)
  expect_identical(person$LFFP, expected_lffp)
  expect_identical(person$UEFP, expected_uefp)

  expected_lfhrp <- ifelse(
    person$LFSP %in% c("1", "2") & person$HRSP == "&&", "4",
    ifelse(person$LFSP == "1", "1",
      ifelse(person$LFSP == "2", "2",
        ifelse(person$LFSP == "3", "3",
          ifelse(person$LFSP == "4", "5",
            ifelse(person$LFSP == "5", "6",
              ifelse(person$LFSP == "6", "7",
                ifelse(person$LFSP == "&", "&", "@")
              )
            )
          )
        )
      )
    )
  )
  expect_identical(person$LFHRP, expected_lfhrp)
  expect_true(all(person$HRSP[person$LFSP == "3"] == "00"))

  employed <- person$LFSP %in% c("1", "2", "3")
  not_employed <- person$LFSP %in% c("4", "5", "6")
  attending <- person$STUP %in% c("2", "3", "4")
  expected_eetp <- rep("&", nrow(person))
  expected_eetp[person$AGEP < 15L] <- "@"
  adult <- person$AGEP >= 15L
  expected_eetp[adult &
    (person$LFSP == "1" | person$STUP == "2" | (employed & attending))] <- "1"
  expected_eetp[adult &
    ((person$LFSP == "2" & person$STUP == "1") |
      (not_employed & person$STUP == "3"))] <- "2"
  expected_eetp[adult &
    ((person$LFSP %in% c("2", "3") & person$STUP == "&") |
      (person$LFSP == "3" & person$STUP == "1") |
      (person$LFSP == "&" & person$STUP %in% c("3", "4")) |
      (not_employed & person$STUP == "4"))] <- "3"
  expected_eetp[adult & not_employed & person$STUP == "1"] <- "4"
  expect_identical(person$EETP, expected_eetp)
  expect_true(all(person$EETP[person$AGEP < 15L] == "@"))
  expect_true(all(person$EETP %in% c("1", "2", "3", "4", "&", "@", "V")))

  applicable_yarrp <- grepl("^[0-9]{4}$", person$YARP)
  arrival_year <- suppressWarnings(as.integer(person$YARP))
  expected_yarrp <- cut(
    arrival_year,
    breaks = c(-Inf, 1950L, 1960L, 1970L, 1980L, 1990L, 2000L,
               2010L, 2020L, Inf),
    labels = as.character(1:9),
    right = TRUE
  )
  expect_identical(person$YARRP[applicable_yarrp],
                   as.character(expected_yarrp[applicable_yarrp]))
  expect_true(all(person$YARRP[person$YARP == "@@@@"] == "@"))
})

test_that("generate_census dwelling health counts match linked people", {
  spine <- generate_spine(n = 4000L, seed = 41L)
  census <- generate_census(spine = spine, seed = 42L)
  person <- census$person
  dwelling <- census$dwelling

  dwelling_index <- match(person$DWELLING_ID, dwelling$DWELLING_ID)
  occupied <- tabulate(dwelling_index, nbins = nrow(dwelling))
  selected <- person$CLTHP %in% c("1", "2", "3")
  missing <- person$CLTHP == "&"
  selected_n <- tabulate(dwelling_index[selected], nbins = nrow(dwelling))
  missing_n <- tabulate(dwelling_index[missing], nbins = nrow(dwelling))

  expected_nprd <- ifelse(occupied == 0L, "@",
                          as.character(pmin(occupied, 8L)))
  expect_identical(dwelling$NPRD, expected_nprd)

  complete <- occupied > 0L & missing_n == 0L
  expect_identical(dwelling$CPLTHD[complete],
                   sprintf("%02d", pmin(selected_n[complete], 10L)))
  expect_identical(dwelling$CPLTHRD[complete],
                   as.character(pmin(selected_n[complete], 5L)))
  expect_true(all(dwelling$CPLTHD[occupied == 0L] == "@@"))
  expect_true(all(dwelling$CPLTHRD[occupied == 0L] == "@"))
})

test_that("generate_census uses ABS 2021 Census Dictionary code sets", {
  spine <- generate_spine(n = 5000L, seed = 11L)
  census <- generate_census(spine = spine, seed = 12L)
  person <- census$person
  dwelling <- census$dwelling
  family <- census$family

  expect_true(all(person$SEXP %in% c(1L, 2L)))
  expect_true(all(person$AGEP >= 0L & person$AGEP <= 115L))
  expect_true(all(person$AGE5P %in% sprintf("%02d", 1:21)))
  expect_true(all(person$AGE10P %in% sprintf("%02d", 1:11)))
  expect_true(all(person$DOBYP >= 1906L & person$DOBYP <= 2021L))
  expect_true(all(person$STEUCP %in% 1:9))
  expect_true(all(person$HSCP %in% c("1", "2", "3", "4", "5", "6", "&", "@", "V")))
  expect_true(all(person$INGP %in% c("1", "2", "3", "4", "&", "V")))
  expect_true(all(grepl("^([0-9]{4}|&|&&|&&&&|V|VV|VVVV)$", person$BPLP)))
  expect_false(any(person$BPLP == "9999"))
  expect_true(all(grepl("^([0-9]{4}|&|&&|&&&&|V|VV|VVVV)$", person$LANP)))
  expect_false(any(person$LANP == "9999"))
  expect_true(all(person$YARP %in% c(as.character(1905:2021), "&&&&", "@@@@", "VVVV")))
  expect_true(all(person$YARRP %in% c(as.character(1:9), "&", "@", "V")))
  # RELP is now the full ASCRG narrow-group frame; validate against the
  # shipped code-frame table (self-maintaining) rather than a fixed list.
  relp_tbl <- read.delim(system.file("extdata/codeframes/ascrg_religion.tsv",
                                     package = "fplida"), colClasses = "character")
  expect_true(all(person$RELP %in% relp_tbl$code))
  expect_gt(length(unique(person$RELP)), 8L)  # not a degenerate subset
  expect_true(all(person$CITP %in% c("1", "2", "&", "V")))
  expect_true(all(person$HEAP %in% c(as.character(1:8), "001", "90", "91", "998", "&", "&&", "&&&", "@", "@@", "@@@", "V", "VV", "VVV")))
  expect_true(all(person$MSTP %in% c("1", "2", "3", "4", "5", "@")))
  expect_true(all(person$INCP %in% c(sprintf("%02d", 1:16), "&&", "@@", "VV")))
  expect_true(all(person$LFSP %in% c(as.character(1:6), "&", "@", "V")))
  expect_true(all(person$EMFP %in% c("1", "2", "@", "V")))
  expect_true(all(person$LFFP %in% c("1", "2", "@", "V")))
  expect_true(all(person$UEFP %in% c("1", "2", "@", "V")))
  expect_true(all(person$LFHRP %in% c(as.character(1:7), "&", "@", "V")))
  expect_true(all(person$EETP %in% c("1", "2", "3", "4", "&", "@", "V")))
  expect_true(all(person$IFAGEP %in% c("1", "2")))
  expect_true(all(person$IFSEXP %in% c("01", "02")))
  expect_true(all(person$IFMSTP %in% c("1", "2", "@")))
  expect_true(all(grepl("^([0-9]{6}|&&&&&&|@@@@@@|VVVVVV)$", person$OCCP)))
  expect_true(all(person$INDP %in% c(LETTERS[1:19], "T", "&&&&", "@@@@", "VVVV")))
  expect_true(all(person$HRWRP %in% c(sprintf("%02d", 0:10), "&&", "@@", "VV")))
  expect_true(all(person$HRSP %in% c(sprintf("%02d", 0:99), "&&", "@@", "VV")))
  expect_true(all(person$MTWP %in% c("001", "002", "003", "006", "007", "010", "011", "232", "233", "234", "&&&", "@@@", "VVV")))
  expect_true(all(person$CLTHP %in% c("0", "1", "2", "3", "&", "V")))
  expect_true(all(person$HLTHP %in% c("121", "122", "&&&", "VVV")))
  expect_true(all(person$HOLHP %in% c("111", "112", "&&&", "VVV")))
  expect_true(all(person$COARASP %in% c("1", "2", "&", "V")))
  expect_true(all(person$COARDBP %in% c("1", "2", "&", "V")))
  expect_true(all(person$COARHDP %in% c("1", "2", "&", "V")))
  expect_true(all(person$COARMHP %in% c("1", "2", "&", "V")))
  expect_true(all(person$COASHDP %in% c("1", "2", "&", "V")))
  expect_true(all(person$COASLCP %in% c("1", "2", "&", "V")))
  expect_true(all(person$COCNHDP %in% c("1", "2", "&", "V")))
  expect_true(all(person$CODBHDP %in% c("1", "2", "&", "V")))
  expect_true(all(person$CODBKDP %in% c("1", "2", "&", "V")))
  expect_true(all(person$COHDKDP %in% c("1", "2", "&", "V")))
  expect_true(all(person$COHDMHP %in% c("1", "2", "&", "V")))
  expect_true(all(person$COLCMHP %in% c("1", "2", "&", "V")))
  expect_true(all(person$HARTP %in% c("011", "012", "&&&", "VVV")))
  expect_true(all(person$HASTP %in% c("021", "022", "&&&", "VVV")))
  expect_true(all(person$HCANP %in% c("031", "032", "&&&", "VVV")))
  expect_true(all(person$HDEMP %in% c("041", "042", "&&&", "VVV")))
  expect_true(all(person$HDIAP %in% c("051", "052", "&&&", "VVV")))
  expect_true(all(person$HHEDP %in% c("061", "062", "&&&", "VVV")))
  expect_true(all(person$HKIDP %in% c("071", "072", "&&&", "VVV")))
  expect_true(all(person$HLUNP %in% c("081", "082", "&&&", "VVV")))
  expect_true(all(person$HMHCP %in% c("091", "092", "&&&", "VVV")))
  expect_true(all(person$HSTRP %in% c("101", "102", "&&&", "VVV")))
  expect_true(all(person$VOLWP %in% c("1", "2", "&", "@", "V")))
  expect_true(all(person$UNCAREP %in% c("1", "2", "&", "@", "V")))
  expect_true(all(person$CHCAREP %in% c("1", "2", "3", "4", "&", "@", "V")))
  expect_true(all(person$DOMP %in% c("1", "2", "3", "4", "5", "&", "@", "V")))
  expect_true(all(person$ADFP %in% c("1", "2", "3", "4", "&", "@", "V")))
  expect_true(all(person$STUP %in% c("1", "2", "3", "4", "&", "V")))
  expect_true(all(person$ASSNP %in% c("1", "2", "&", "V")))

  expect_true(all(dwelling$STRD %in% c("11", "21", "22", "31", "32", "33", "34", "35", "91", "92", "93", "94", "&&", "@@")))
  expect_true(all(dwelling$TEND %in% c(as.character(1:7), "&", "@")))
  expect_true(all(dwelling$NPRD %in% c(as.character(1:8), "@")))
  expect_true(all(dwelling$CALTHD %in% c(as.character(0:6), "&", "@")))
  expect_true(all(dwelling$CCLTHD %in% c(as.character(0:6), "&", "@")))
  expect_true(all(dwelling$CPLTHD %in% c(sprintf("%02d", 0:11), "&&", "@@")))
  expect_true(all(dwelling$CPLTHRD %in% c(as.character(0:6), "&", "@")))
  expect_true(all(dwelling$BEDRD %in% c(as.character(0:6), "&", "@")))
  expect_true(all(dwelling$VEHRD %in% c(as.character(0:4), "&", "@")))
  expect_true(all(dwelling$STEUCD %in% 1:9))
  expect_true(all(grepl("^(0[0-9]{3}|[1-9][0-9]{3}|&&&&|@@@@)$", dwelling$RNTD)))
  expect_true(all(grepl("^([0-9]{4}|&&&&|@@@@)$", dwelling$MRED)))
  expect_true(all(as.integer(dwelling$MRED[grepl("^[0-9]{4}$", dwelling$MRED)]) <= 9999L))

  expect_true(all(family$FMCF %in% c("1", "12", "122", "1222", "2", "21", "211", "2111", "2112", "212", "2121", "2122", "22", "221", "2211", "2212", "222", "2221", "3", "31", "311", "3111", "3112", "312", "3121", "3122", "32", "321", "3211", "3212", "322", "3221", "9", "92", "922", "9222", "@@@@")))
  expect_true(all(family$CDCF %in% c(sprintf("%02d", 0:13), "@@")))
  expect_true(all(family$CPRF %in% c(as.character(2:6), "@")))
  expect_true(all(family$SSCF %in% c("1", "2", "3", "@")))
})

test_that("generate_census errors without spine or data path", {
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

  expect_error(generate_census(), "No output directory")
})

test_that("generate_census loads spine from run dir", {
  tmp <- file.path(tempdir(), "census_dp_test")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 100L, seed = 5L, output_dir = tmp)
  # generate_spine sets fplida.run_dir option

  census <- generate_census(seed = 5L, output_dir = tmp)
  eligible_spine <- census_night_spine(spine)
  expect_equal(nrow(census$person), nrow(eligible_spine))
  expect_identical(census$person$SYNTHETIC_AEUID,
                   eligible_spine$aeuid_abs)
})

test_that("generate_census writes products and ABS spine", {
  tmp <- file.path(tempdir(), "census_abs_test")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 100L, seed = 1L, output_dir = tmp)
  generate_census(spine = spine, seed = 1L, output_dir = tmp, format = "csv")

  run_dir <- file.path(tmp, "fplida_1k")

  # Census products + agency spine written to abs-census/ subfolder
  census_dir <- file.path(run_dir, "abs-census")
  expect_true(dir.exists(census_dir))
  csv_files <- list.files(census_dir, pattern = "\\.csv$")
  expect_equal(length(csv_files), 4L)  # 3 products + abs-spine.csv

  # ABS agency spine in the dataset folder
  abs_spine_path <- file.path(census_dir, "abs-spine.csv")
  expect_true(file.exists(abs_spine_path))

  csv <- read.csv(abs_spine_path, stringsAsFactors = FALSE)
  expect_named(csv, c("spine_id", "SYNTHETIC_AEUID"))
  expect_equal(nrow(csv), nrow(census_night_spine(spine)))
})

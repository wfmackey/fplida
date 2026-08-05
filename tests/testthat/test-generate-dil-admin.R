admin_test_spine <- function(n = 60L) {
  data.frame(
    spine_id = sprintf("SP%010d", seq_len(n)),
    aeuid_ato = sprintf("ATO%09d", seq_len(n)),
    aeuid_apsc = sprintf("APSC%08d", seq_len(n)),
    birth_year = 1955L + (seq_len(n) %% 45L),
    sex = 1L + (seq_len(n) %% 2L),
    state = 1L + (seq_len(n) %% 8L),
    year_of_death = ifelse(seq_len(n) %% 20L == 0L, 2022L, NA_integer_),
    month_of_death = ifelse(seq_len(n) %% 20L == 0L, 6L, NA_integer_),
    disability_type = ifelse(seq_len(n) %% 8L == 0L, "physical", NA_character_),
    baseline_income = 30000 + seq_len(n) * 1250,
    baseline_employed = TRUE,
    stringsAsFactors = FALSE
  )
}

admin_test_frame <- function(dataset, product_name, seed = 31L,
                             spine = admin_test_spine()) {
  variables <- fplida:::.dil_variables(dataset, product_name)
  names <- variables[["Variable Name"]]
  descriptions <- variables[["Variable Description"]]
  names(descriptions) <- names
  agency_column <- if (dataset == "APSED") "aeuid_apsc" else "aeuid_ato"
  fplida:::.make_dil_frame(
    names, spine, spine[[agency_column]], dataset, product_name, seed,
    variable_descriptions = descriptions
  )
}

admin_abn_is_valid <- function(value) {
  digits <- as.integer(strsplit(value, "", fixed = TRUE)[[1L]])
  digits[1L] <- digits[1L] - 1L
  sum(digits * c(10L, 1L, 3L, 5L, 7L, 9L, 11L, 13L, 15L, 17L, 19L)) %%
    89L == 0L
}

test_that("administrative lightweight products preserve DIL schemas without placeholders", {
  products <- fplida:::.dil_products(c("APSED"))
  product_map <- list(
    APSED = products[["Product Name"]],
    ATO_MCS = fplida:::.dil_products("ATO_MCS")[["Product Name"]],
    ERS = fplida:::.dil_products("ERS")[["Product Name"]],
    JK = fplida:::.dil_products("JK")[["Product Name"]],
    JM = fplida:::.dil_products("JM")[["Product Name"]],
    SMSF = fplida:::.dil_products("SMSF")[["Product Name"]]
  )

  for (dataset in names(product_map)) {
    for (product_name in product_map[[dataset]]) {
      frame <- admin_test_frame(dataset, product_name)
      expected <- unique(fplida:::.dil_variables(
        dataset, product_name
      )[["Variable Name"]])
      expect_setequal(names(frame), expected)

      character_values <- unlist(frame[vapply(frame, is.character, logical(1))],
                                 use.names = FALSE)
      character_values <- character_values[!is.na(character_values)]
      expect_false(any(grepl(
        "^(apsed|ato_mcs|ers|jk|jm|smsf)_[0-9]{6}$|^C[0-9]{2}$",
        character_values
      )))
    }
  }
})

test_that("APSED uses published standards and withholds undocumented codes", {
  frame <- admin_test_frame("APSED", "pmp-apsed")
  country <- read.delim(
    system.file("extdata", "codeframes", "sacc_country.tsv",
                package = "fplida"),
    stringsAsFactors = FALSE
  )
  language <- read.delim(
    system.file("extdata", "codeframes", "ascl_language.tsv",
                package = "fplida"),
    stringsAsFactors = FALSE
  )

  expect_true(all(frame$COUNTRY_OF_BIRTH %in% country$code))
  expect_true(all(frame$FIRST_LANGUAGE %in% language$code))
  expect_true(all(frame$MONTH_OF_BIRTH %in% 1L:12L))
  expect_true(all(frame$STATE_OF_EMPLOYEES_WORKPLACE %in% 1L:8L))
  expect_true(all(frame$HOURS_WORKED_PER_WEEK > 0))
  expect_true(is.logical(frame$DISABILITY))
  expect_true(all(frame$PREVIOUS_EMPLOYMENT %in%
                    c("Private sector", "State government", "Student",
                      "Unemployed")))
  expect_true(all(is.na(frame$ACTING_CLASSIFICATION)))
  expect_true(all(is.na(frame$JOBFAMILY_CODE)))
})

test_that("MCS contribution amounts and financial periods are coherent", {
  frame <- admin_test_frame("ATO_MCS", "pmp-ato-mcs")
  components <- c("ALCTD_SRPLS_AMT", "EMPLR_CNTRBTN_ACMLTD_AMT",
                  "EMPLR_DBS_CNTRBTN_AMT", "OTHR_CNTRBTNS_AMT",
                  "PRSNL_CNTRBTN_TOTL_AMT", "PST_20081996_ETP_CMPNT_AMT")

  expect_equal(frame$TOTL_CNTRBTN_AMT, rowSums(frame[components]))
  expect_true(all(frame$FIN_YR >= 1999L & frame$FIN_YR <= 2017L))
  expect_match(frame$FIN_YEAR, "^(19|20)[0-9]{2}-[0-9]{2}$", all = TRUE)
  expect_true(is.logical(frame$MBR_IDV_DCSD_IND))
  expect_equal(frame$MBR_IDV_DCSD_IND, !is.na(frame$MBR_IDV_DTH_YEAR))
  expect_true(all(is.na(frame$SF_MBRSHP_ACNT_STS_CD)))
})

test_that("early release values respect documented domains and totals", {
  frame <- admin_test_frame("ERS", "madipmp-ato-early-release-super")
  approved <- paste0("ACCOUNT", 1L:5L, "_FUND_AMOUNT_APPROVED")

  expect_true(all(frame$REQUEST_APPROVED_STATUS %in% c("approved", "rejected")))
  account_types <- unlist(frame[paste0("ACCOUNT", 1L:5L, "_TYPE")])
  expect_true(all(account_types[!is.na(account_types)] %in% c("apra", "smsf")))
  expect_equal(frame$TOTAL_AMOUNT_APPROVED,
               rowSums(frame[approved], na.rm = TRUE))
  expect_true(all(frame$AMOUNT_REQUESTED_FOR_RELEASE >=
                    frame$TOTAL_AMOUNT_APPROVED))
  expect_true(all(frame$AMOUNT_REQUESTED_FOR_RELEASE <= 10000))
  expect_true(all(frame$REQUEST_FINANCIAL_YEAR %in% c("2019-20", "2020-21")))
  approved_rows <- frame$REQUEST_APPROVED_STATUS == "approved"
  expect_true(all(frame$APPROVAL_DATE[approved_rows] >
                    frame$REQUEST_DATE[approved_rows]))
  expect_true(all(is.na(frame$APPROVAL_DATE[!approved_rows])))
  expect_true(all(format(frame$REQUEST_DATE, "%Y") == "2020"))
  expect_true(all(frame$REQUEST_DATE[frame$REQUEST_FINANCIAL_YEAR == "2019-20"] <=
                    as.Date("2020-06-30")))
  expect_true(all(frame$REQUEST_DATE[frame$REQUEST_FINANCIAL_YEAR == "2020-21"] >=
                    as.Date("2020-07-01")))
})

test_that("JobKeeper and JobMaker use scheme dates and meaningful types", {
  jk <- admin_test_frame("JK", "madipmp-ato-jobkeeper")
  jm <- admin_test_frame("JM", "madipmp-ato-jobmaker")

  expect_true(all(jk$CD_EMPLOYEE_TIER %in% 1L:2L))
  expect_true(all(jk$JK_BUS_PRTCPNT %in% c("Y", "N")))
  expect_true(all(jk$DT_ELIGIBLE_EFFECT == as.Date("2020-03-30")))
  expect_true(all(jk$DT_ELIGIBLE_END == as.Date("2021-03-28")))
  expect_true(all(jk$JK_DRVD_AMT %in% c(14400, 9150)))
  expect_true(is.logical(jk$JK_FN_ELIG_14))

  expect_true(all(jm$AGE >= 18L & jm$AGE <= 69L))
  expect_true(is.logical(jm$EMPLE_JMHC_ELGBLTY_P1))
  expect_true(is.integer(jm$EMPLE_STP_EMPLR_CNT_FY21_YTD))
  expect_true(is.numeric(jm$EMPLE_STP_GRS_PAY_FY21_YTD_ALL))
  expect_true(all(jm$EMPLE_JMHC_CEASED_DT >= jm$EMPLE_JMHC_COMMENCE_DT))
})

test_that("SMSF uses product years, valid synthetic ABNs, and typed gaps", {
  frame <- admin_test_frame("SMSF", "pmp-smsf-2022-23")

  expect_true(all(frame$FIN_YEAR == "2022-23"))
  expect_true(all(frame$INCM_YR == 2023L))
  expect_true(all(vapply(frame$BN, admin_abn_is_valid, logical(1))))
  expect_true(is.numeric(frame$AI_TOTL_AMT))
  expect_true(is.logical(frame$ACTRL_CERT_OBTND_IND))
  expect_true(all(is.na(frame$FND_BNFT_STRCTR_CD)))
})

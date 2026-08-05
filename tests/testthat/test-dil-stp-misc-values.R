test_that("STP conditional fields use linked official domains", {
  n <- 240L
  spine <- data.frame(
    id = seq_len(n),
    spine_id = sprintf("S%010d", seq_len(n)),
    stringsAsFactors = FALSE
  )
  amount <- rep(0, n)
  amount[seq(1L, n, by = 7L)] <- 2500
  source <- data.frame(
    ANL_LNG_SRVC_UNSD_LS_A_AMT = amount,
    stringsAsFactors = FALSE
  )
  period <- list(end_year = 2024L)
  value <- function(name, frame = source) {
    fplida:::.dil_stp_source_value(
      name, name, frame, spine, 20260803L, period,
      "stp_extended_pay_events_2024_m06"
    )
  }

  leave_code <- value("ANL_LNG_SRVC_UNSD_LS_A_CD")
  expect_identical(is.na(leave_code), amount == 0)
  expect_true(all(stats::na.omit(leave_code) %in% c("R", "T")))

  etp <- value("ETP_PMT_TYP_CD")
  expect_true(all(etp %in% c("O", "R", "D", "N", "T")))
  expect_gt(length(unique(etp)), 2L)

  contractor <- value("CNTRCTR_BN")
  expect_true(anyNA(contractor))
  expect_true(any(!is.na(contractor)))
  expect_match(stats::na.omit(contractor), "^BN[0-9]{11}$", all = TRUE)
  expect_identical(contractor, value("CNTRCTR_BN"))
})

test_that("BIRTHS and ERS one-field rules replace unresolved placeholders", {
  n <- 160L
  spine <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  source <- data.frame(.row = seq_len(n))
  postal <- fplida:::.dil_births_source_value(
    "POSTALTYPE", source, spine, 17L
  )
  expect_true(all(postal %in% c("R", "P")))
  expect_gt(length(unique(postal)), 1L)

  category <- fplida:::.dil_ers_source_value(
    "REQUEST_CATEGORY", "", source, spine, 17L, list(end_year = 2020L)
  )
  expect_identical(category, rep("COVID-19 early release", n))
})

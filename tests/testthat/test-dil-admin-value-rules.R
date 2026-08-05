test_that("CGT source rules resolve amounts and durations, not code frames", {
  source <- data.frame(
    CG_CY_TOTL_AMT = c(100000, 25000, 8000, 450000),
    CG_TOTL_REAL_EST_AMT = c(50000, 10000, 0, 300000),
    CG_TOTL_SHARES_UNITS_AMT = c(30000, 10000, 6000, 100000),
    CG_TOTL_OTH_AMT = c(20000, 5000, 2000, 50000),
    CL_CY_TOTL_AMT = c(10000, 0, 3000, 25000),
    CGT_TOTL_DSCNT_APLD_AMT = c(30000, 8000, 1000, 120000),
    SB_CNCSNS_APLD_TOTL_AMT = c(5000, 0, 0, 20000),
    stringsAsFactors = FALSE
  )
  spine <- data.frame(id = 1:4)
  period <- list(end_year = 2024L)
  register <- utils::read.csv(
    fplida_test_inst_path(
      "internal-docs", "admin-value-remediation-register.csv"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  cgt <- register[register$dataset == "CGT", , drop = FALSE]
  values <- lapply(seq_len(nrow(cgt)), function(i) {
    fplida:::.dil_cgt_source_value(
      cgt$variable[[i]], cgt$official_descriptions[[i]], source, spine,
      20260803L, period
    )
  })
  unresolved <- cgt$variable[vapply(values, is.null, logical(1))]
  expect_setequal(unresolved, c(
    "CG_ERNT_ARNGMTS_PRTY_CD", "CG_EXMPTN_15_YR_FR_SB_CD",
    "ENTITY_TYPE", "K_SCRPT_RLOVR_CD_A", "L_SCRPT_ACQ_ENT_CD_E",
    "L_SCRPT_ACQ_ENT_JNT_RLOVR_L", "M_EMPLYEE_SHARE_SCHM_N",
    "N_DIV149_O"
  ))
  expect_equal(length(values) - length(unresolved), 68L)
  expect_true(all(vapply(values[!vapply(values, is.null, logical(1))],
                         length, integer(1)) == nrow(source)))
})

test_that("CGT source rules retain the generated totals", {
  source <- data.frame(
    CG_CY_TOTL_AMT = c(100000, 25000),
    CG_TOTL_REAL_EST_AMT = c(50000, 10000),
    CG_TOTL_SHARES_UNITS_AMT = c(30000, 10000),
    CG_TOTL_OTH_AMT = c(20000, 5000),
    CL_CY_TOTL_AMT = c(10000, 0),
    CGT_TOTL_DSCNT_APLD_AMT = c(30000, 8000),
    SB_CNCSNS_APLD_TOTL_AMT = c(5000, 0),
    stringsAsFactors = FALSE
  )
  spine <- data.frame(id = 1:2)
  period <- list(end_year = 2024L)
  value <- function(name, description) {
    fplida:::.dil_cgt_source_value(
      name, description, source, spine, 20260803L, period
    )
  }

  method_total <- value(
    "TOTL_CY_CAPTL_GAIN_DSCNT", "Total current year capital gains"
  ) + value(
    "TOTL_CY_CAPTL_GAIN_IDXTN", "Total current year capital gains"
  ) + value(
    "OTH_TOTL_CY_CAPTL_GAIN", "Total current year capital gains"
  )
  expect_equal(method_total, source$CG_CY_TOTL_AMT, tolerance = 2)

  loss_total <- value(
    "CYCL_REAL_EST_B", "Current year capital losses - Real estate"
  ) + value(
    "CYCL_SHARES_UNITS_A", "Current year capital losses - Shares"
  ) + value(
    "CYCL_FMIS_T", "Current year capital losses - FMIS"
  ) + value(
    "CYCL_HEDG_FINC_U", "Current year capital losses - Hedging"
  )
  expect_equal(loss_total, source$CL_CY_TOTL_AMT, tolerance = 2)
})

test_that("vital-event and MCD rules derive only supported fields", {
  spine <- data.frame(
    id = 1:4,
    birth_year = c(1970L, 1980L, 1990L, 2000L),
    year_of_arrival = c(NA, 2005L, 2010L, NA),
    country_of_birth = c(0L, 1L, 1L, 0L),
    country_of_birth_sacc = c(1101L, 2100L, 7103L, 1101L)
  )
  births_source <- data.frame(
    SYNTHETIC_AEUID = paste0("B", 1:4),
    stringsAsFactors = FALSE
  )
  expect_identical(
    fplida:::.dil_births_source_value(
      "SYTHETIC_AEUID", births_source, spine, 20260803L
    ),
    births_source$SYNTHETIC_AEUID
  )
  expect_identical(
    fplida:::.dil_births_source_value(
      "CNTRY_CDE", births_source, spine, 20260803L
    ),
    rep("1101", 4L)
  )
  expect_true(any(!is.na(fplida:::.dil_births_source_value(
    "DUP_CLUSTER_ID", births_source, spine, 20260803L
  ))))
  expect_true(all(fplida:::.dil_births_source_value(
    "QUALITY", births_source, spine, 20260803L
  ) %in% 1:3))

  deaths_source <- data.frame(
    YEAR_OF_DEATH = c(2020L, 2021L, 2022L, 2023L),
    BIRTH_PLACE = c(0L, 1L, 1L, 0L),
    REMOTENESS_AREA_2021 = 1:4,
    SEIFA_IRSAD_DEC_2021 = 2:5,
    SEIFA_IRSD_DEC_2021 = 3:6
  )
  expect_identical(
    fplida:::.dil_deaths_source_value(
      "BIRTHPLACE", deaths_source, spine, 20260803L,
      list(end_year = 2024L)
    ),
    deaths_source$BIRTH_PLACE
  )
  residence <- fplida:::.dil_deaths_source_value(
    "PERIOD_RESIDENCE", deaths_source, spine, 20260803L,
    list(end_year = 2024L)
  )
  expect_identical(residence, c(50L, 16L, 12L, 23L))
  expect_null(fplida:::.dil_deaths_source_value(
    "LGA_CODE", deaths_source, spine, 20260803L,
    list(end_year = 2024L)
  ))

  empty_deaths_source <- data.frame(.row = seq_len(nrow(spine)))
  expect_identical(
    fplida:::.dil_deaths_source_value(
      "BIRTH_PLACE", empty_deaths_source, spine, 20260803L,
      list(end_year = 2024L)
    ),
    spine$country_of_birth_sacc
  )
  expect_true(all(fplida:::.dil_deaths_source_value(
    "MARITAL_STATUS", empty_deaths_source, spine, 20260803L,
    list(end_year = 2024L)
  ) %in% 1:4))
  expect_true(all(fplida:::.dil_deaths_source_value(
    "REMOTENESS_AREA_2021", empty_deaths_source, spine, 20260803L,
    list(end_year = 2024L)
  ) %in% 1:5))
  expect_true(all(fplida:::.dil_deaths_source_value(
    "SEIFA_IRSD_DEC_2021", empty_deaths_source, spine, 20260803L,
    list(end_year = 2024L)
  ) %in% 1:10))

  mcd_source <- data.frame(CNSMR_STS = as.Date(c(
    "2001-01-01", "2005-01-01", "2010-01-01", "2000-01-01"
  )))
  period <- list(end = as.Date("2024-06-30"), end_year = 2024L)
  expect_identical(
    fplida:::.dil_mcd_source_value(
      "CNSMR_CHRTC_STS", mcd_source, spine, 20260803L, period
    ),
    mcd_source$CNSMR_STS
  )
  expect_true(any(!is.na(fplida:::.dil_mcd_source_value(
    "CNSMR_ETS", mcd_source, spine, 20260803L, period
  ))))
  expect_identical(
    fplida:::.dil_mcd_source_value(
      "CNTRY_CDE", mcd_source, spine, 20260803L, period
    ),
    c("1101", "2100", "7103", "1101")
  )
})

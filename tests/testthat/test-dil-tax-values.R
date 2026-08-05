tax_value_fixture <- function(n = 400L) {
  spine <- data.frame(
    id = seq_len(n),
    birth_year = rep(1940:2019, length.out = n),
    anzsco_code = rep(c(111111L, 254411L, 351311L, 621111L),
                      length.out = n),
    baseline_income = seq(20000, 100000, length.out = n)
  )
  alternating <- rep(c(0, 1000), length.out = n)
  source <- data.frame(
    MBR_IDV_DCSD_IND = rep(c(FALSE, FALSE, FALSE, TRUE), length.out = n),
    CG_CY_TOTL_AMT = seq(10000, 100000, length.out = n),
    CG_TOTL_REAL_EST_AMT = seq(5000, 50000, length.out = n),
    CG_TOTL_SHARES_UNITS_AMT = seq(3000, 30000, length.out = n),
    CG_TOTL_OTH_AMT = seq(2000, 20000, length.out = n),
    CL_CY_TOTL_AMT = seq(0, 10000, length.out = n),
    CGT_TOTL_DSCNT_APLD_AMT = seq(1000, 10000, length.out = n),
    SB_CNCSNS_APLD_TOTL_AMT = alternating,
    CRNT_PNSN_EXMT_INCM_AMT = alternating,
    GRS_TRST_DSTRBTNS_AMT = alternating,
    INCM_OTHR_AMT = alternating,
    OD_AMT = alternating,
    NON_DDCTBL_EXPNS_OTHR_AMT = alternating,
    LSP_AMT = alternating,
    INCM_STRM_PMT_AMT = alternating,
    WRK_RLTD_CAR_EXPNSS_AMT = alternating,
    WRK_RLTD_CLTHG_EXPNSS_AMT = alternating,
    WRK_RLTD_SELF_EDUCN_EXPNSS_AMT = alternating,
    LUMP_SUM_A = alternating,
    CLOSG_STK_TOTL_AMT = alternating,
    DPRCTN_EXPNS_AMT = alternating,
    MTR_VHCL_BUS_EXPNSS_TOTL_AMT = alternating,
    SLRY_AND_WG_EXPNSS_TOTL_AMT = alternating,
    ASSBL_GOVT_INDY_PMTS_NPP_AMT = alternating,
    ASSBL_GOVT_INDY_PMTS_PP_AMT = alternating,
    BUS_ACTY_CNT = rep(c(0, 1, 0, 1), length.out = n),
    BBI_ELGBL_AST_OPT_OUT_NUM = rep(c(NA, 0, NA, 2), length.out = n),
    TFE_ELGBL_AST_OPT_OUT_NUM = rep(c(NA, 0, NA, 2), length.out = n),
    BUS_LSS_ACTY1_NET_LSS_AMT = -alternating,
    BUS_LSS_ACTY2_NET_LSS_AMT = -alternating,
    BUS_LSS_ACTY3_NET_LSS_AMT = -alternating,
    OTHR_BUS_INCM_NPP_AMT = rep(c(1000, -1000), length.out = n),
    OTHR_BUS_INCM_PP_AMT = rep(c(1000, -1000), length.out = n),
    NONPP_OTHR_DDCTNS_DSTBN_AMT = alternating,
    PP_OTHR_DDCTNS_DSTBN_AMT = alternating,
    AVLBL_OTHR_RFNDBL_TOS_AMT = alternating,
    ETPS_OTHRTHN_EXCSVCMPNT_AMT = alternating
  )
  list(
    spine = spine,
    source = source,
    period = list(
      start_year = 2023L, end_year = 2024L,
      start = as.Date("2023-07-01"), end = as.Date("2024-06-30")
    )
  )
}

test_that("CGT schedule codes use published domains and structural missing", {
  fixture <- tax_value_fixture()
  value <- function(name) {
    fplida:::.dil_tax_cgt_source_value(
      name, fixture$source, fixture$spine, 20260803L
    )
  }

  expect_true(all(value("CG_ERNT_ARNGMTS_PRTY_CD") %in% c("Y", "N")))
  expect_identical(value("ENTITY_TYPE"), rep("I", nrow(fixture$spine)))
  exemption <- value("CG_EXMPTN_15_YR_FR_SB_CD")
  expect_true(all(exemption[!is.na(exemption)] %in% c("S", "U", "R", "G", "O")))
  expect_true(all(is.na(exemption[fixture$source$SB_CNCSNS_APLD_TOTL_AMT == 0])))

  rollover <- value("K_SCRPT_RLOVR_CD_A")
  acquiring <- value("L_SCRPT_ACQ_ENT_CD_E")
  joint <- value("L_SCRPT_ACQ_ENT_JNT_RLOVR_L")
  expect_identical(rollover, acquiring)
  expect_true(all(joint[rollover == "N"] == "N"))
  expect_true(all(is.na(value("M_EMPLYEE_SHARE_SCHM_N"))))
  expect_true(all(is.na(value("N_DIV149_O"))))
})

test_that("SMSF return codes stay within the published form domains", {
  fixture <- tax_value_fixture()
  value <- function(name) {
    fplida:::.dil_tax_smsf_source_value(
      name, fixture$source, fixture$spine, 20260803L, fixture$period
    )
  }
  domains <- list(
    FND_BNFT_STRCTR_CD = c("A", "D", "E"),
    CRNT_PNSN_INC_EXMT_CAL_MTHD_CD = c("B", "C"),
    GRS_TRST_DSTRBTNS_CD = c("D", "E", "F", "H", "S", "T", "I", "M", "U", "P", "Q"),
    OI_TYP_CD = c("B", "C", "F", "R", "T", "W", "O"),
    DDCTNS_OTHR_DDCTN_TYP_CD = c("A", "B", "C", "E", "F", "I", "N", "R", "T", "O"),
    NON_DDCTBL_EXPNS_OTHR_CD = c("A", "B", "C", "E", "F", "I", "N", "R", "T", "O"),
    LSP_CD = c("A", "B", "C", "D", "E", "F", "G"),
    INCM_STRM_PMT_CD = c("M", "N", "O", "P", "Q", "R"),
    MBR_RCRD_CD = c("F", "G")
  )
  for (name in names(domains)) {
    observed <- value(name)
    expect_true(
      all(observed[!is.na(observed)] %in% domains[[name]]),
      info = name
    )
  }
  age <- fixture$period$end_year - fixture$spine$birth_year
  key <- fplida:::.dil_tax_key(fixture$spine, 20260803L, "SMSF|LSP_CD")
  ordinary <- fixture$source$LSP_AMT != 0 & key %% 10L < 8L
  lump_sum <- value("LSP_CD")
  expect_true(all(lump_sum[ordinary & age >= 60L] == "A"))
  expect_true(all(lump_sum[ordinary & age < 60L] == "B"))

  key <- fplida:::.dil_tax_key(
    fixture$spine, 20260803L, "SMSF|INCM_STRM_PMT_CD"
  )
  ordinary <- fixture$source$INCM_STRM_PMT_AMT != 0 & key %% 10L < 7L
  income_stream <- value("INCM_STRM_PMT_CD")
  expect_true(all(income_stream[ordinary & age >= 60L] == "M"))
  expect_true(all(income_stream[ordinary & age < 60L] == "N"))
})

test_that("MCS account codes change with the reporting specification vintage", {
  fixture <- tax_value_fixture()
  value <- function(name, end_year) {
    fplida:::.dil_tax_mcs_source_value(
      name, fixture$source, fixture$spine, 20260803L,
      list(start_year = end_year - 1L, end_year = end_year)
    )
  }

  phase <- value("ACNT_PHS_CD", 2018L)
  status <- value("SF_MBRSHP_ACNT_STS_CD", 2018L)
  expect_true(all(phase %in% c("A", "P", "B")))
  expect_true(all(status[phase %in% c("P", "B")] == "C"))
  expect_true(all(is.na(value("ACNT_PHS_CD", 2012L))))
  expect_true(all(value("SF_MBRSHP_ACNT_STS_CD", 2012L) %in% c("A", "C")))
  expect_true(all(status %in% c("O", "L", "C")))
  expect_true(all(value("PRVDR_TYP_CD", 2018L) %in%
                    c("P", "N", "S", "X", "D", "E", "A", "C", "R")))
  expect_true(all(is.na(value("PRVDR_TYP_CD", 2012L))))
  supplier <- value("SUPLR_PRVDR_RLTNSHP", 2018L)
  expect_true(all(supplier[!is.na(supplier)] %in%
                    c("A", "C", "F", "I", "L", "R", "S", "T", "U", "W", "X")))
  expect_true(all(value("LDGMT_TYP_CD", 2018L) %in% c("O", "A")))
  expect_true(all(is.na(value("LDGMT_TYP_CD", 2007L))))
  expect_null(value("GEOCODED_INDEX", 2018L))
})

test_that("ATO client-register values match the documented field semantics", {
  fixture <- tax_value_fixture()
  value <- function(name) {
    fplida:::.dil_tax_ato_cr_source_value(
      name, fixture$source, fixture$spine, 20260803L
    )
  }

  expect_true(all(value("ADDR_RELIABLE_STS") %in% c("R", "U")))
  expect_true(all(value("ADDR_STS_CD") %in% c("A", "I")))
  expect_identical(value("ADDR_TYP"), value("ADR_TYP"))
  expect_true(all(value("ADDR_TYP") %in% c("R", "P")))
  expect_true(all(value("CLNT_STS_CD") %in% c("A", "I")))
  expect_true(all(grepl("^[A-Z][a-z]{2}[0-9]{4}$", value("DOB_MONYYYY"))))
  expect_true(all(value("IN_MAY_24") %in% c(0L, 1L)))
  expect_null(value("POSTALTYPE"))
})

test_that("PIT codes are conditional on amounts, age, and shared contexts", {
  fixture <- tax_value_fixture()
  value <- function(name) {
    fplida:::.dil_tax_pit_source_value(
      name, fixture$source, fixture$spine, 20260803L, fixture$period
    )
  }
  even <- fixture$source$WRK_RLTD_CAR_EXPNSS_AMT == 0
  expect_true(all(is.na(value("WRK_RLTD_CAR_EXPNSS_CLM_TYP_CD")[even])))
  expect_true(all(value("WRK_RLTD_CAR_EXPNSS_CLM_TYP_CD")[!even] %in% c("S", "B")))
  expect_true(all(value("WRK_RLTD_CLTHG_EXPNSS_TYP_CD")[!even] %in%
                    c("C", "N", "S", "P")))
  expect_true(all(value("WRK_RLTD_SELF_EDUCN_TYP_CD")[!even] %in% c("K", "I")))

  expect_identical(value("LSPA_TYP"), value("LSPS_AMTA_CD"))
  expect_true(all(value("LSPA_TYP")[!even] %in% c("R", "T")))
  expect_identical(value("IDV_OCPTN_CD"), sprintf("%06d", fixture$spine$anzsco_code))
  expect_identical(value("OCPTN_GRP_CD"), substr(sprintf("%06d", fixture$spine$anzsco_code), 1L, 4L))

  spouse_full <- value("HAD_SPS_CRNT_FY_FULL_PERD_CD")
  spouse_days <- value("HAD_SPS_CRNT_FY_DAYS")
  expect_true(all(is.na(spouse_days[is.na(spouse_full) | spouse_full == "Y"])))
  part_year <- which(!is.na(spouse_full) & spouse_full == "N")
  expect_true(all(spouse_days[part_year] >= 1L &
                    spouse_days[part_year] <= 365L))

  rollover_ind <- value("HVYOUAPLDANEXMPTNORRLVRIND")
  rollover_code <- value("HVYOUAPLDANEXMPTNORRLVRCD")
  expect_true(all(is.na(rollover_code[rollover_ind == "N"])))
  expect_true(all(!is.na(rollover_code[rollover_ind == "Y"])))

  under_18 <- fixture$period$end_year - fixture$spine$birth_year < 18L
  expect_true(all(is.na(value("UNDR_18_INCM_TYP_CD")[!under_18])))
  expect_true(all(value("UNDR_18_INCM_TYP_CD")[under_18] %in% c("A", "M")))
  expect_identical(value("EXTRACT_REF"), rep("FY2023-24", nrow(fixture$spine)))

  business <- fixture$source$BUS_ACTY_CNT > 0
  segment <- value("BUSINESS_MARKET_SEGMENT")
  expect_true(all(segment[!business] == "INB"))
  expect_true(all(segment[business] %in% c("MIC", "SME")))
  expect_true(all(segment %in% c("GOV", "LGE", "SME", "INB", "MIC", "NFP")))

  for (name in c("BBI_ELGBL_AST_BBI_CD", "TFE_ELGBL_AST_OPT_OUT_CD")) {
    opt_out <- value(name)
    expect_true(all(opt_out[!is.na(opt_out)] %in% c("5", "10", "15")))
    expect_true(all(is.na(opt_out[!business])))
    expect_true(any(!is.na(opt_out)))
  }

  fund_ids <- fplida:::.dil_tax_health_insurer_ids()
  health <- lapply(
    c("HLTH_FND_CD_1", "HLTH_FND_CD_2", "HLTH_FND_CD_3", "HLTH_FND_CD_4"),
    value
  )
  expect_identical(value("HLTH_FND_CD"), health[[1L]])
  expect_true(any(!is.na(health[[1L]])))
  expect_true(any(is.na(health[[1L]])))
  for (slot in seq_along(health)) {
    expect_true(all(health[[slot]][!is.na(health[[slot]])] %in% fund_ids$code))
    if (slot > 1L) {
      expect_true(all(is.na(health[[slot]]) | !is.na(health[[slot - 1L]])))
      both <- !is.na(health[[slot]]) & !is.na(health[[slot - 1L]])
      expect_true(all(health[[slot]][both] != health[[slot - 1L]][both]))
    }
  }
})

test_that("the current ATO health-insurer register is checked in with scope", {
  insurers <- fplida:::.dil_tax_health_insurer_ids()
  expect_equal(nrow(insurers), 33L)
  expect_true(all(grepl("^[A-Z]{3}$", insurers$code)))
  expect_equal(anyDuplicated(insurers$code), 0L)
  expect_true(all(insurers$membership_type %in% c("Open", "Restricted")))
  expect_true(all(grepl("not a verified historical", insurers$scope_note)))
  expect_true(all(
    insurers$source_url ==
      "https://www.privatehealth.gov.au/dynamic/Insurer/Index/"
  ))
})

test_that("PIT lodgement channels use ALife codes and ATO annual weights", {
  weights <- fplida:::.dil_tax_pit_lodgement_weights()
  expect_identical(weights$end_year, 2010L:2024L)

  spine <- data.frame(id = seq_len(100000L))
  value <- function(end_year, seed = 20260803L) {
    fplida:::.dil_tax_pit_lodgement_source(
      spine, seed, list(start_year = end_year - 1L, end_year = end_year)
    )
  }
  emitted_domain <- c("AGNT_ELS", "ETAX", "SP_MYTAX", "SP_PPR")

  for (end_year in weights$end_year) {
    observed <- value(end_year)
    expect_type(observed, "character")
    expect_false(anyNA(observed), info = end_year)
    expect_true(all(observed %in% emitted_domain), info = end_year)
    if (end_year <= 2013L) {
      expect_false(any(observed == "SP_MYTAX"), info = end_year)
    }
    if (end_year >= 2017L) {
      expect_false(any(observed == "ETAX"), info = end_year)
    }

    year_row <- weights[weights$end_year == end_year, , drop = FALSE]
    expected <- as.numeric(year_row[1L, emitted_domain])
    expected <- expected / sum(expected)
    observed_share <- table(factor(
      observed, levels = emitted_domain
    )) / nrow(spine)
    expect_equal(
      as.numeric(observed_share), expected, tolerance = 0.002,
      info = end_year
    )
  }

  expect_identical(value(2024L), value(2024L))
})

test_that("PIT business opt-out choices remain non-degenerate without amounts", {
  n <- 2000L
  spine <- data.frame(
    id = seq_len(n),
    birth_year = rep(1940:2019, length.out = n),
    baseline_income = seq(20000, 100000, length.out = n)
  )
  source <- data.frame(row = seq_len(n))
  period <- list(start_year = 2020L, end_year = 2021L)
  for (name in c("BBI_ELGBL_AST_BBI_CD", "TFE_ELGBL_AST_OPT_OUT_CD")) {
    value <- fplida:::.dil_tax_pit_source_value(
      name, source, spine, 20260803L, period
    )
    observed <- value[!is.na(value)]
    expect_true(length(observed) > 0L)
    expect_true(length(observed) < n)
    expect_true(all(observed %in% c("5", "10", "15")))
    expect_true(any(observed %in% c("5", "10")))
    expect_true(any(observed == "15"))
  }
})

test_that("the dataset dispatcher uses tax rules before the CGT amount fallback", {
  fixture <- tax_value_fixture()
  dispatch <- function(name, description, dataset) {
    fplida:::.dil_dataset_source_value(
      name, description, dataset, fixture$source, fixture$spine,
      20260803L, fixture$period
    )
  }

  expect_true(all(dispatch("FND_BNFT_STRCTR_CD", "", "SMSF") %in%
                    c("A", "D", "E")))
  expect_identical(
    dispatch("ENTITY_TYPE", "Entity type", "CGT"),
    rep("I", nrow(fixture$spine))
  )
  expect_length(
    dispatch("H_CG_DISC_E", "Net capital gains from discount method", "CGT"),
    nrow(fixture$spine)
  )
  expect_true(all(dispatch(
    "BUSINESS_MARKET_SEGMENT", "Business market segment", "PIT_ITR"
  ) %in% c("GOV", "LGE", "SME", "INB", "MIC", "NFP")))
  health <- dispatch("HLTH_FND_CD_1", "Health Fund ID1", "PIT_ITR")
  expect_true(all(
    health[!is.na(health)] %in% fplida:::.dil_tax_health_insurer_ids()$code
  ))
  expect_null(dispatch("POSTALTYPE", "Type of postal address", "ATO_CR"))
})

test_that("tax remediation coverage is explicit for every register entry", {
  fixture <- tax_value_fixture()
  register <- utils::read.csv(
    fplida_test_inst_path(
      "internal-docs", "admin-value-remediation-register.csv"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  datasets <- c(
    "PIT_ITR", "PIT_PS", "PIT_IE", "ATO_MCS", "ATO_CR", "SMSF",
    "BUSOWN", "CGT"
  )
  register <- register[register$dataset %in% datasets, , drop = FALSE]
  resolved <- vapply(seq_len(nrow(register)), function(i) {
    period <- fixture$period
    if (register$dataset[[i]] == "ATO_MCS") {
      period <- list(start_year = 2017L, end_year = 2018L)
    }
    value <- fplida:::.dil_tax_source_value(
      register$variable[[i]], register$official_descriptions[[i]],
      register$dataset[[i]], fixture$source, fixture$spine, 20260803L,
      period
    )
    !is.null(value)
  }, logical(1))

  expected <- list(
    PIT_ITR = c(
      "AMDT_CD", "ASSBL_GOVT_INDY_PMTS_NPP_CD",
      "ASSBL_GOVT_INDY_PMTS_PP_CD", "BUS_LSS_ACTY1_TYP_CD",
      "BBI_ELGBL_AST_BBI_CD", "BUSINESS_MARKET_SEGMENT",
      "BUS_LSS_ACTY2_TYP_CD", "BUS_LSS_ACTY3_TYP_CD",
      "BUS_LSSACTY1_PSHPORSOLETRDR_CD",
      "BUS_LSSACTY2_PSHPORSOLETRDR_CD",
      "BUS_LSSACTY3_PSHPORSOLETRDR_CD", "CESD_OR_CMNCD_BUS_STS_CD",
      "CLOSG_STK_ACTN_CD", "DPRCTN_EXPNS_CD", "ETP_TYP_CD",
      "EXTRACT_REF", "HAD_SPS_CRNT_FY_DAYS",
      "HAD_SPS_CRNT_FY_FULL_PERD_CD", "HVYOUAPLDANEXMPTNORRLVRCD",
      "HLTH_FND_CD", "HLTH_FND_CD_1", "HLTH_FND_CD_2",
      "HLTH_FND_CD_3", "HLTH_FND_CD_4",
      "HVYOUAPLDANEXMPTNORRLVRIND", "IDV_OCPTN_CD", "LSPA_TYP",
      "LODGMENT_SOURCE", "LSPS_AMTA_CD",
      "MDCRE_FULLLVY_EXMTN_CLM_TYP_CD", "MRTL_STS_CD",
      "MTR_VHCL_BUS_EXPNSS_TYP_CD", "NONELGBLTAXEXMPTNSPSFBTTOTL",
      "NPP_OTHR_DDCTNS_BUS_LSS_CD", "OCPTN_GRP_CD",
      "OTHR_BUS_INCM_NPP_CD", "OTHR_BUS_INCM_PP_CD",
      "OTHR_RFNDBL_TOS_CD", "PNSNR_TAX_OFST_CD", "PNSNR_VETERAN_CD",
      "PP_OTHR_DDCTNS_DSTBN_BUSLSS_CD", "PRT_YR_TFT_ELGBL_MTHS_CD",
      "RECORD_ID", "SATO_CD", "SLRY_AND_WG_EXPNSS_TOTL_CD",
      "SLS_PMT_TYP_CD", "SNR_AUSNS_VETERAN_CD",
      "SPS_OR_HSKPR_TAX_OFST_CLM_CD", "SUB_OCPTN_GRP_CD",
      "TFE_ELGBL_AST_OPT_OUT_CD",
      "UNDR_18_INCM_TYP_CD", "WRK_RLTD_CAR_EXPNSS_CLM_TYP_CD",
      "WRK_RLTD_CLTHG_EXPNSS_TYP_CD", "WRK_RLTD_SELF_EDUCN_TYP_CD"
    ),
    PIT_PS = c("AMDT_CD", "EXTRACT_REF", "LSPA_TYP"),
    PIT_IE = "EXTRACT_REF",
    ATO_MCS = c(
      "ACNT_PHS_CD", "LDGMT_TYP_CD", "PRVDR_TYP_CD",
      "SF_MBRSHP_ACNT_STS_CD", "SUPLR_PRVDR_RLTNSHP"
    ),
    ATO_CR = c(
      "ADDR_RELIABLE_STS", "ADDR_STS_CD", "ADDR_TYP", "ADR_TYP",
      "CLNT_STS_CD", "DOB_MONYYYY", "IN_MAY_22", "IN_MAY_23",
      "IN_MAY_24", "IN_MAY_25"
    ),
    SMSF = c(
      "CRNT_PNSN_INC_EXMT_CAL_MTHD_CD", "DDCTNS_OTHR_DDCTN_TYP_CD",
      "FND_BNFT_STRCTR_CD", "GRS_TRST_DSTRBTNS_CD", "INCM_STRM_PMT_CD",
      "LSP_CD", "MBR_RCRD_CD", "NON_DDCTBL_EXPNS_OTHR_CD", "OI_TYP_CD"
    ),
    BUSOWN = "EXTRACT_REF",
    CGT = c(
      "CG_ERNT_ARNGMTS_PRTY_CD", "CG_EXMPTN_15_YR_FR_SB_CD",
      "ENTITY_TYPE",
      "K_SCRPT_RLOVR_CD_A", "L_SCRPT_ACQ_ENT_CD_E",
      "L_SCRPT_ACQ_ENT_JNT_RLOVR_L", "M_EMPLYEE_SHARE_SCHM_N",
      "N_DIV149_O"
    )
  )

  for (dataset in names(expected)) {
    rows <- register$dataset == dataset
    expect_setequal(register$variable[rows & resolved], expected[[dataset]])
  }
  expect_equal(sum(register$occurrence_count[resolved]), 1104L)
})

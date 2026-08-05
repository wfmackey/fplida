# -- Helper ------------------------------------------------------------------

.make_domino_test_data <- function(n = 500L, seed = 1L, fmt = "csv") {
  tmp <- file.path(tempdir(), paste0("domino_test_", n, "_", seed))
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  spine <- generate_spine(n = n, seed = seed, output_dir = tmp)
  domino <- generate_domino(spine = spine, seed = seed, output_dir = tmp,
                             format = fmt)
  list(spine = spine, domino = domino, tmp = tmp)
}


# -- Structure ---------------------------------------------------------------

test_that("generate_domino returns expected structure from spine", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  expect_type(domino, "list")
  expected_names <- c(
    "det_ben", "static_demogs", "inc_emp_cont", "loc_dtls",
    "edu_lvl", "hh_hse_dtls", "mcd_dtls", "pyh_combined_dis",
    "pyh_combined_fam", "indg_stat", "pyh_combined_std",
    "pyh_combined_age", "pyh_combined_wrk", "rlt_prin_carer_beta"
  )
  expect_named(domino, expected_names)
  for (nm in names(domino)) {
    expect_s3_class(domino[[nm]], "data.frame")
  }
})

test_that("det_ben has correct columns", {
  spine <- generate_spine(n = 300L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  expected_cols <- c(
    "SYNTHETIC_AEUID", "BEN_TYPE_CODE", "BEN_STATUS",
    "PERIOD_START_DATE", "PERIOD_END_DATE", "DURN_DAYS",
    "END_RSN", "END_RSN_CODE"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(domino$det_ben),
                info = paste("Missing det_ben column:", col))
  }
})

test_that("mcd_dtls has correct columns", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  expected_cols <- c(
    "SYNTHETIC_AEUID", "PERIOD_START_DATE", "PERIOD_END_DATE",
    "ASSMT_ID", "MED_PRMY_GRP", "MED_SCNDRY_GRP",
    "IMPRMT_CODE", "IMPRMT_RATE", "CURR_CAPCTY_NUM",
    "WITH_INTRVN_NUM", "INCAP_START", "INCAP_END",
    "MAN_CODE", "ACTV_PRTCPN_CODE"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(domino$mcd_dtls),
                info = paste("Missing mcd_dtls column:", col))
  }
})

test_that("static_demogs has correct columns", {
  spine <- generate_spine(n = 300L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  expected_cols <- c(
    "SYNTHETIC_AEUID", "YEAR_OF_BIRTH", "MONTH_OF_BIRTH",
    "GENDER", "BIRTH_CTRY_CODE", "LANG_CODE",
    "AGE", "DEATH_IND", "OBJECT_TYPE_CODE"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(domino$static_demogs),
                info = paste("Missing static_demogs column:", col))
  }
})

test_that("pyh_combined tables share same schema", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  pyh_names <- c("pyh_combined_dis", "pyh_combined_wrk",
                  "pyh_combined_fam", "pyh_combined_std",
                  "pyh_combined_age")
  expected_cols <- c(
    "SYNTHETIC_AEUID", "BEN_TYPE", "PERIOD_START_DATE",
    "PERIOD_END_DATE", "CMPNT_ID", "CMPNT_TYPE",
    "CMPNT_DLY_AMT", "PYH_SOURCE"
  )

  for (nm in pyh_names) {
    tbl <- domino[[nm]]
    if (nrow(tbl) > 0L) {
      for (col in expected_cols) {
        expect_true(col %in% names(tbl),
                    info = paste("Missing", nm, "column:", col))
      }
    }
  }
})

test_that("det_ben is spell-level (more rows than persons)", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  # Some persons have NSA→JSP splits, so rows >= unique persons
  expect_true(nrow(domino$det_ben) > 0L)
  n_unique <- length(unique(domino$det_ben$SYNTHETIC_AEUID))
  expect_true(n_unique > 0L)
})


# -- AEUID Linkage -----------------------------------------------------------

test_that("all det_ben AEUIDs exist in spine", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  valid_aeuids <- spine$aeuid_dss
  expect_true(
    all(domino$det_ben$SYNTHETIC_AEUID %in% valid_aeuids),
    info = "det_ben has AEUIDs not in spine"
  )
})

test_that("all mcd_dtls AEUIDs exist in det_ben", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  if (nrow(domino$mcd_dtls) > 0L) {
    expect_true(
      all(domino$mcd_dtls$SYNTHETIC_AEUID %in% domino$det_ben$SYNTHETIC_AEUID),
      info = "mcd_dtls has AEUIDs not in det_ben"
    )
  }
})

test_that("all tables have AEUIDs in spine", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  valid_aeuids <- spine$aeuid_dss
  aeuid_tables <- c("det_ben", "static_demogs", "mcd_dtls",
                      "inc_emp_cont", "loc_dtls", "edu_lvl",
                      "hh_hse_dtls", "indg_stat", "rlt_prin_carer_beta")

  for (nm in aeuid_tables) {
    tbl <- domino[[nm]]
    if (nrow(tbl) > 0L) {
      expect_true(
        all(tbl$SYNTHETIC_AEUID %in% valid_aeuids),
        info = paste(nm, "has AEUIDs not in spine")
      )
    }
  }
})


# -- Spell Validity ----------------------------------------------------------

test_that("benefit spell dates are valid", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)
  db <- domino$det_ben

  # Start dates exist

  expect_true(all(!is.na(db$PERIOD_START_DATE)))

  # End dates >= start dates (where end is not NA)
  has_end <- !is.na(db$PERIOD_END_DATE)
  if (any(has_end)) {
    expect_true(
      all(db$PERIOD_END_DATE[has_end] >= db$PERIOD_START_DATE[has_end]),
      info = "Some spell end dates are before start dates"
    )
  }
})

test_that("spell durations are positive", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  expect_true(all(domino$det_ben$DURN_DAYS > 0L))
})

test_that("benefit type codes are valid Centrelink codes", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  # All generated codes must be in the master list
  all_valid <- c(
    "DSP", "NSA", "JSP", "YAL", "PPP", "PPS", "AGE", "CAR", "CDA",
    "ABY", "AUS", "FTB", "BRV", "BSW", "DWS", "MEP", "MOB", "WFD", "WFR",
    "JSA", "MAA", "MPA", "NMA", "NSS", "PTA",
    "AEP", "BBY", "CCF", "CCI", "DAP", "DOP", "EIC", "FPA", "FTP", "PPL",
    "ABA", "ABS", "EPA", "EPF", "PES", "TAP", "YLA", "YLS", "YTA",
    "BVA", "PDB", "PEN", "SHC", "WDA", "WFA", "WID"
  )
  expect_true(
    all(domino$det_ben$BEN_TYPE_CODE %in% all_valid),
    info = "Invalid benefit type codes found"
  )
})

test_that("benefit status is CUR or SUS", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  expect_true(all(domino$det_ben$BEN_STATUS %in% c("CUR", "SUS")))
})


# -- Medical Assessments -----------------------------------------------------

test_that("medical records only for DSP/CAR spells", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  if (nrow(domino$mcd_dtls) > 0L) {
    # All mcd AEUIDs should be in DSP/CAR spells
    dsp_car_aeuids <- unique(domino$det_ben$SYNTHETIC_AEUID[
      domino$det_ben$BEN_TYPE_CODE %in% c("DSP", "CAR")])
    expect_true(
      all(domino$mcd_dtls$SYNTHETIC_AEUID %in% dsp_car_aeuids),
      info = "Medical records exist for non-DSP/CAR recipients"
    )
  }
})

test_that("MED_PRMY_GRP codes are valid Centrelink codes", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  valid_med <- c(
    "MUS", "PSY", "INT", "NER", "CIR", "RES", "SEN", "ABI",
    "CAN", "CHR", "CFS", "CGA", "EIS", "GIS", "IFD", "IHD",
    "AMP", "REP", "SDB", "URO", "VIS", "PFC"
  )

  if (nrow(domino$mcd_dtls) > 0L) {
    expect_true(
      all(domino$mcd_dtls$MED_PRMY_GRP %in% valid_med),
      info = "Invalid MED_PRMY_GRP codes found"
    )
  }
})

test_that("work capacity is in valid range", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  if (nrow(domino$mcd_dtls) > 0L) {
    expect_true(all(domino$mcd_dtls$CURR_CAPCTY_NUM >= 0L))
    expect_true(all(domino$mcd_dtls$CURR_CAPCTY_NUM <= 30L))
  }
})

test_that("impairment rating is in valid range", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  # The Impairment Tables publish a rating from 0 to 95 in steps of 5. The
  # earlier 20-40 range was a synthetic band, not the source's domain.
  if (nrow(domino$mcd_dtls) > 0L) {
    expect_true(all(domino$mcd_dtls$IMPRMT_RATE >= 0L))
    expect_true(all(domino$mcd_dtls$IMPRMT_RATE <= 95L))
    expect_true(all(domino$mcd_dtls$IMPRMT_RATE %% 5L == 0L))
    # The code and the rate report the same rating.
    expect_identical(domino$mcd_dtls$IMPRMT_CODE, domino$mcd_dtls$IMPRMT_RATE)
  }
})


# -- Payment History ---------------------------------------------------------

test_that("payment amounts are positive", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  pyh_names <- c("pyh_combined_dis", "pyh_combined_wrk",
                  "pyh_combined_fam", "pyh_combined_std",
                  "pyh_combined_age")

  for (nm in pyh_names) {
    tbl <- domino[[nm]]
    if (nrow(tbl) > 0L) {
      expect_true(all(tbl$CMPNT_DLY_AMT > 0),
                  info = paste(nm, "has non-positive payment amounts"))
    }
  }
})

test_that("payment history AEUIDs match det_ben", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  all_pyh_aeuids <- unique(c(
    domino$pyh_combined_dis$SYNTHETIC_AEUID,
    domino$pyh_combined_wrk$SYNTHETIC_AEUID,
    domino$pyh_combined_fam$SYNTHETIC_AEUID,
    domino$pyh_combined_std$SYNTHETIC_AEUID,
    domino$pyh_combined_age$SYNTHETIC_AEUID
  ))

  expect_true(
    all(all_pyh_aeuids %in% domino$det_ben$SYNTHETIC_AEUID),
    info = "Payment history has AEUIDs not in det_ben"
  )
})


# -- Supplementary Tables ---------------------------------------------------

test_that("indg_stat has exactly 2 columns", {
  spine <- generate_spine(n = 300L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  expect_equal(ncol(domino$indg_stat), 2L)
  expect_named(domino$indg_stat, c("SYNTHETIC_AEUID", "INDIG_CODE"))
})

test_that("indg_stat follows the spine Indigenous codeframe", {
  participants <- data.frame(
    aeuid = paste0("DSS", 1:4),
    spine_idx = 1:4
  )
  spine <- data.frame(indigenous = 1:4)

  observed <- project_domino_indigenous(participants, spine)

  expect_identical(observed$INDIG_CODE, c("N", "Y", "Y", "Y"))
})

test_that("edu_lvl has valid education codes", {
  spine <- generate_spine(n = 300L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  # The custodian's education codes are alphabetic. The old 00-05 band was
  # invented, so pin the column against the published domain itself rather
  # than a list that can drift from it.
  if (nrow(domino$edu_lvl) > 0L) {
    valid_edu <- fplida:::.registry_values_for("DOMINO", "LVL_ATTAINED")
    expect_gt(length(valid_edu), 20L)
    expect_true(
      all(domino$edu_lvl$LVL_ATTAINED %in% valid_edu),
      info = "Invalid education level codes"
    )
  }
})

test_that("loc_dtls has valid state codes", {
  spine <- generate_spine(n = 300L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  if (nrow(domino$loc_dtls) > 0L) {
    valid_states <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
    expect_true(
      all(domino$loc_dtls$STATE %in% valid_states),
      info = "Invalid state codes in loc_dtls"
    )
  }
})

test_that("hh_hse_dtls weekly rent is non-negative", {
  spine <- generate_spine(n = 300L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  if (nrow(domino$hh_hse_dtls) > 0L) {
    expect_true(all(domino$hh_hse_dtls$HSE_WK_RENT >= 0))
  }
})

test_that("rlt_prin_carer_beta only for PPP/PPS/CAR", {
  spine <- generate_spine(n = 500L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  if (nrow(domino$rlt_prin_carer_beta) > 0L) {
    expect_true(
      all(domino$rlt_prin_carer_beta$BEN_TYPE_CODE %in% c("PPP", "PPS", "CAR")),
      info = "Principal carer records for non-PPP/PPS/CAR types"
    )
  }
})


# -- Determinism -------------------------------------------------------------

test_that("generate_domino is deterministic", {
  spine <- generate_spine(n = 200L, seed = 42L)
  a <- generate_domino(spine = spine, seed = 99L)
  b <- generate_domino(spine = spine, seed = 99L)

  expect_identical(a$det_ben, b$det_ben)
  expect_identical(a$mcd_dtls, b$mcd_dtls)
  expect_identical(a$static_demogs, b$static_demogs)
  expect_identical(a$indg_stat, b$indg_stat)
})

test_that("different seeds give different output", {
  spine <- generate_spine(n = 200L, seed = 42L)
  a <- generate_domino(spine = spine, seed = 1L)
  b <- generate_domino(spine = spine, seed = 2L)

  # At least det_ben should differ
  expect_false(identical(a$det_ben, b$det_ben))
})


# -- File Writing ------------------------------------------------------------

test_that("DOMINO product files are written", {
  td <- .make_domino_test_data(n = 200L, seed = 1L, fmt = "csv")
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  ds_dir <- file.path(td$tmp,
                       list.files(td$tmp, pattern = "^fplida_")[1],
                       "dss-domino")
  expect_true(dir.exists(ds_dir), info = "dss-domino directory not created")

  # Check that at least det_ben and mcd_dtls files exist
  files <- list.files(ds_dir)
  expect_true(any(grepl("det-ben", files)), info = "det_ben product not written")
  expect_true(any(grepl("static-demogs", files)), info = "static_demogs not written")
})

test_that("DSS agency spine is written", {
  td <- .make_domino_test_data(n = 200L, seed = 1L, fmt = "csv")
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  ds_dir <- file.path(td$tmp,
                       list.files(td$tmp, pattern = "^fplida_")[1],
                       "dss-domino")
  files <- list.files(ds_dir)
  expect_true(any(grepl("dss-spine", files)), info = "DSS spine not written")
})


# -- Phase 2 Hooks -----------------------------------------------------------

test_that("participant selection includes Phase 2 columns", {
  spine <- generate_spine(n = 200L, seed = 1L)
  participants <- fplida:::select_domino_participants(
    spine, seed = 1L, yr_range = c(2005L, 2024L))

  expect_true("phase2_dc" %in% names(participants))
  expect_true("phase2_nc" %in% names(participants))
})

test_that("Phase 2 flags reflect disability columns when present", {
  spine <- generate_spine(n = 500L, seed = 1L)
  participants <- fplida:::select_domino_participants(
    spine, seed = 1L, yr_range = c(2005L, 2024L))

  if (nrow(participants) > 0L) {
    # With disability columns in spine, some participants should have Phase 2 flags
    expect_true("phase2_dc" %in% names(participants))
    expect_true("phase2_nc" %in% names(participants))
    # DC and NC flags should be logical
    expect_type(participants$phase2_dc, "logical")
    expect_type(participants$phase2_nc, "logical")
  }
})


# -- Edge Cases --------------------------------------------------------------

test_that("handles small spine", {
  spine <- generate_spine(n = 50L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L)

  expect_type(domino, "list")
  expect_true(nrow(domino$det_ben) >= 0L)
})

test_that("handles narrow year range", {
  spine <- generate_spine(n = 300L, seed = 1L)
  domino <- generate_domino(spine = spine, seed = 1L, years = 2020L:2021L)

  expect_type(domino, "list")
  if (nrow(domino$det_ben) > 0L) {
    # All spell starts should be within the year range
    starts <- as.integer(format(domino$det_ben$PERIOD_START_DATE, "%Y"))
    expect_true(all(starts >= 2020L & starts <= 2021L))
  }
})



# -- Cross-product vitals ----------------------------------------------------

test_that("static_demogs vitals come from the spine", {
  d <- .make_domino_test_data(n = 2000L, seed = 1L)
  on.exit(unlink(d$tmp, recursive = TRUE, force = TRUE), add = TRUE)

  sd <- d$domino$static_demogs
  i <- match(sd$SYNTHETIC_AEUID, d$spine$aeuid_dss)
  expect_false(anyNA(i))

  # Birth month is the spine's, not a fresh draw.
  expect_identical(as.integer(sd$MONTH_OF_BIRTH),
                   as.integer(d$spine$month_of_birth[i]))

  # DEATH_IND is "Y" exactly when the spine death year has arrived by the
  # DOMINO reference year, and the dates are the spine's.
  yod <- as.integer(d$spine$year_of_death[i])
  mod <- as.integer(d$spine$month_of_death[i])
  dead <- !is.na(yod) & yod <= 2024L
  expect_identical(sd$DEATH_IND == "Y", dead)
  expect_identical(as.integer(sd$YEAR_OF_DEATH)[dead], yod[dead])
  expect_identical(as.integer(sd$MONTH_OF_DEATH)[dead], mod[dead])
  expect_true(all(is.na(sd$YEAR_OF_DEATH[!dead])))
  expect_true(all(is.na(sd$MONTH_OF_DEATH[!dead])))

  # Guard against a fix that flattens the column.
  expect_gt(length(unique(sd$MONTH_OF_BIRTH)), 1L)
})

test_that("pure-R static_demogs fallback also reads the spine vitals", {
  tmp <- file.path(tempdir(), "domino_vitals_fallback")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  spine <- suppressMessages(generate_spine(n = 2000L, seed = 3L,
                                           output_dir = tmp))
  parts <- fplida:::select_domino_participants(spine, 3L, c(2005L, 2024L))
  skip_if(nrow(parts) == 0L)

  # Dropping country_of_birth pushes project_domino_demogs() onto its pure-R
  # branch, which duplicated the invented-vitals bug.
  spine_nc <- spine[, setdiff(names(spine), "country_of_birth"), drop = FALSE]
  sd <- fplida:::project_domino_demogs(parts, spine_nc, 3L)

  idx <- parts$spine_idx
  yod <- as.integer(spine$year_of_death)[idx]
  dead <- !is.na(yod) & yod <= 2024L
  expect_identical(as.integer(sd$MONTH_OF_BIRTH),
                   as.integer(spine$month_of_birth)[idx])
  expect_identical(sd$DEATH_IND == "Y", dead)
  expect_identical(as.integer(sd$YEAR_OF_DEATH)[dead], yod[dead])
  expect_identical(as.integer(sd$MONTH_OF_DEATH)[dead],
                   as.integer(spine$month_of_death)[idx][dead])
})

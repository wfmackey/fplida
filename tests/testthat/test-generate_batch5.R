# Tests for Batch 5: NDIS, A&T, DEX, AIR, AMEP, NACDC

# --- NDIS ---
test_that("generate_ndis returns enriched participants and 3 products", {
  tmp <- file.path(tempdir(), paste0("ndis_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 3000L, seed = 1L, output_dir = tmp)
  ndis <- generate_ndis(spine = spine, seed = 1L, output_dir = tmp, return_data = TRUE)
  expect_s3_class(ndis, "data.frame")
  expect_gt(nrow(ndis), 0L)
  expect_lt(nrow(ndis), nrow(spine))
  # Enriched participant columns use the official PLIDA NDIA names.
  for (col in c("NDISDSBLTYGRPNM", "ICDDSBLTYNM", "SVRTYSCR", "CHC_FLAG",
                "CSNIND", "ACTVPRTCPNTIND", "SA2CD2021", "NDISMMMSCD",
                "REMOTENESS_DESCRIPTION_MMM", "EVERELIGBLIND",
                "EVERINELIGBLIND"))
    expect_true(col %in% names(ndis), info = paste("Missing:", col))
  # plansupports and payments products are written.
  expect_gt(length(Sys.glob(file.path(tmp, "*", "*", "*plansupports*.parquet"))), 0L)
  expect_gt(length(Sys.glob(file.path(tmp, "*", "*", "*payments*.parquet"))), 0L)
})

test_that("NDIS severity score in [0,1] and plansupports budgets valid", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("ndis_test2_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 5000L, seed = 2L, output_dir = tmp)
  ndis <- generate_ndis(spine = spine, seed = 2L, output_dir = tmp, return_data = TRUE)
  if (nrow(ndis) > 0L) {
    expect_true(all(ndis$SVRTYSCR >= 0 & ndis$SVRTYSCR <= 1))
    ps_f <- Sys.glob(file.path(tmp, "*", "*", "*plansupports*.parquet"))[1]
    ps <- as.data.frame(arrow::read_parquet(ps_f))
    expect_true(all(ps$CMTDSUPPBDGTAMT > 0))
    expect_true(all(ps$SUPPCLASS %in% c("Core", "Capacity Building", "Capital")))
    expect_true(all(ps$PLANMGTMTHDDESC %in%
                      c("Agency Managed", "Plan Managed", "Self Managed Fully",
                        "Self Managed Partly", "Not recorded")))
    expect_equal(ps$ACTIVEPLANIND, ps$CRNTAPRVDPLANIND)
    expect_equal(ps$LTSTPLANSILIND, ps$EVERSILIND)
    expect_equal(ps$PLANINCLDSDAIND, ps$EVERSDAIND)
  }
})

test_that("NDIS active indicator is valid", {
  tmp <- file.path(tempdir(), paste0("ndis_test3_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 5000L, seed = 3L, output_dir = tmp)
  ndis <- generate_ndis(spine = spine, seed = 3L, output_dir = tmp, return_data = TRUE)
  if (nrow(ndis) > 0L) {
    expect_true(all(ndis$ACTVPRTCPNTIND %in% c("Y", "N")))
    expect_true(all(ndis$EVERELIGBLIND == "Y"))
    expect_true(all(ndis$EVERINELIGBLIND %in% c("Y", "N")))
    expected_remoteness <- c(
      "City", "Rural", "Rural", "Rural", "Rural", "Remote", "Very remote"
    )
    expect_equal(
      ndis$REMOTENESS_DESCRIPTION_MMM,
      expected_remoteness[ndis$NDISMMMSCD]
    )
  }
})

test_that("NDIS payment management and provider fields are coherent", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("ndis_test4_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 5000L, seed = 4L, output_dir = tmp)
  generate_ndis(
    spine = spine, seed = 4L, output_dir = tmp, return_data = FALSE
  )
  payment_file <- Sys.glob(
    file.path(tmp, "*", "*", "*payments*.parquet")
  )[1]
  payments <- as.data.frame(arrow::read_parquet(payment_file))

  expect_gt(nrow(payments), 0L)
  expect_true(all(payments$MGTTYPDESC %in% c("agency", "plan", "self")))
  expect_equal(payments$PMTRQSTPRVDR_BN, payments$PRVDR_BN)
  expect_true(all(grepl("^[0-9]{11}$", payments$PRVDR_BN)))
  payment_date <- as.Date(payments$PYMTRQSTCRTDDT)
  expected_fy <- as.integer(format(payment_date, "%Y")) +
    as.integer(as.integer(format(payment_date, "%m")) >= 7L)
  expect_equal(payments$FY_CLAIM, expected_fy)

  abn_weights <- c(10L, 1L, 3L, 5L, 7L, 9L, 11L, 13L, 15L, 17L, 19L)
  valid_abn <- function(value) {
    digits <- as.integer(strsplit(value, "", fixed = TRUE)[[1L]])
    digits[[1L]] <- digits[[1L]] - 1L
    sum(digits * abn_weights) %% 89L == 0L
  }
  expect_true(all(vapply(payments$PRVDR_BN, valid_abn, logical(1))))
})

# --- A&T ---
test_that("generate_apprentice returns records", {
  tmp <- file.path(tempdir(), paste0("at_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 2000L, seed = 1L, output_dir = tmp)
  at <- generate_apprentice(spine = spine, seed = 1L, output_dir = tmp, return_data = TRUE)
  expect_s3_class(at, "data.frame")
  expect_gt(nrow(at), 0L)
  expect_true("QUALIFICATION_LEVEL" %in% names(at))
  expect_true("STATUS" %in% names(at))
})

test_that("A&T qualification levels are valid", {
  tmp <- file.path(tempdir(), paste0("at_test2_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 2000L, seed = 2L, output_dir = tmp)
  at <- generate_apprentice(spine = spine, seed = 2L, output_dir = tmp, return_data = TRUE)
  expect_true(all(at$QUALIFICATION_LEVEL %in% c(2L, 3L, 4L, 5L)))
})

test_that("A&T status codes are valid", {
  tmp <- file.path(tempdir(), paste0("at_test3_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 2000L, seed = 3L, output_dir = tmp)
  at <- generate_apprentice(spine = spine, seed = 3L, output_dir = tmp, return_data = TRUE)
  expect_true(all(at$STATUS %in% c("Completed", "In Training", "Cancelled", "Expired")))
})

# --- DEX ---
test_that("generate_dex returns special_client and 3 normalised tables", {
  tmp <- file.path(tempdir(), paste0("dex_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 2000L, seed = 1L, output_dir = tmp)
  dex <- generate_dex(spine = spine, seed = 1L, output_dir = tmp, return_data = TRUE)
  expect_s3_class(dex, "data.frame")
  expect_gt(nrow(dex), 0L)
  # special_client uses the official DEX field names.
  for (col in c("GENDERCODE", "EMPLOYMENTSTATUSCODE", "EDUCATIONLEVELCODE",
                "HOMELESSCODE", "HOUSEHOLDCOMPOSITIONCODE", "NDISELIGIBILITYCODE",
                "SA22021BOUNDARYCODE"))
    expect_true(col %in% names(dex), info = paste("Missing:", col))
  # attendance + assessment tables are written.
  expect_gt(length(Sys.glob(file.path(tmp, "*", "*", "*attendance*.parquet"))), 0L)
  expect_gt(length(Sys.glob(file.path(tmp, "*", "*", "*assessment*.parquet"))), 0L)
})

test_that("DEX coded fields use the sourced DSS value frames", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("dex_test2_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 4000L, seed = 2L, output_dir = tmp)
  dex <- generate_dex(spine = spine, seed = 2L, output_dir = tmp, return_data = TRUE)
  if (nrow(dex) > 0L) {
    expect_true(all(dex$HOMELESSCODE %in% c("Yes", "No", "At Risk")))
    expect_true(all(dex$NDISELIGIBILITYCODE %in%
      c("NDIS eligible", "NDIS ineligible", "NDIS in-progress access request")))
    # SCORE outcome-domain scores are on a 1-5 scale.
    as_f <- Sys.glob(file.path(tmp, "*", "*", "*assessment*.parquet"))[1]
    asmt <- as.data.frame(arrow::read_parquet(as_f))
    if (nrow(asmt) > 0L)
      expect_true(all(asmt$OUTCOMEDOMAINSCORE >= 1L & asmt$OUTCOMEDOMAINSCORE <= 5L))
  }
})

# --- AIR ---
test_that("generate_air returns vaccination records", {
  tmp <- file.path(tempdir(), paste0("air_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 500L, seed = 1L, output_dir = tmp)
  air <- generate_air(spine = spine, seed = 1L, output_dir = tmp, return_data = TRUE)
  expect_s3_class(air, "data.frame")
  expect_gt(nrow(air), nrow(spine))  # multiple events per person
  expect_true("ANTIGEN_CODE" %in% names(air))
  expect_true("VACCINE_SEQUENCE" %in% names(air))
})

test_that("AIR antigen codes are valid", {
  tmp <- file.path(tempdir(), paste0("air_test2_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 500L, seed = 2L, output_dir = tmp)
  air <- generate_air(spine = spine, seed = 2L, output_dir = tmp, return_data = TRUE)
  valid_antigens <- c("DTPA", "IPV", "HEP_B", "HIB", "PCV13", "ROTA",
                      "MMR", "VAR", "MENACWY", "HPV", "DTPA_BOOST", "FLU_CHILD",
                      "FLU", "COVID", "PNEU", "ZOSTER")
  expect_true(all(air$ANTIGEN_CODE %in% valid_antigens))
})

# --- AMEP ---
test_that("generate_amep returns records for overseas-born", {
  tmp <- file.path(tempdir(), paste0("amep_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 1000L, seed = 1L, output_dir = tmp)
  amep <- generate_amep(spine = spine, seed = 1L, output_dir = tmp, return_data = TRUE)
  expect_s3_class(amep, "data.frame")
  expect_gt(nrow(amep), 0L)
  expect_true("HOURS_TOTAL" %in% names(amep))
  expect_true("INITIAL_ASLPR_SPEAK" %in% names(amep))
})

test_that("AMEP hours are 50-510", {
  tmp <- file.path(tempdir(), paste0("amep_test2_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 1000L, seed = 2L, output_dir = tmp)
  amep <- generate_amep(spine = spine, seed = 2L, output_dir = tmp, return_data = TRUE)
  if (nrow(amep) > 0L) {
    expect_true(all(amep$HOURS_TOTAL >= 50 & amep$HOURS_TOTAL <= 510))
  }
})

test_that("AMEP ASLPR scores are 0-5", {
  tmp <- file.path(tempdir(), paste0("amep_test3_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 1000L, seed = 3L, output_dir = tmp)
  amep <- generate_amep(spine = spine, seed = 3L, output_dir = tmp, return_data = TRUE)
  if (nrow(amep) > 0L) {
    for (col in c("INITIAL_ASLPR_SPEAK", "LATEST_ASLPR_SPEAK",
                  "INITIAL_ASLPR_LISTEN", "LATEST_ASLPR_LISTEN")) {
      expect_true(all(amep[[col]] >= 0L & amep[[col]] <= 5L), info = col)
    }
  }
})

# --- NACDC ---
test_that("generate_nacdc returns aged care records", {
  tmp <- file.path(tempdir(), paste0("nacdc_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 2000L, seed = 1L, output_dir = tmp)
  nacdc <- generate_nacdc(spine = spine, seed = 1L, output_dir = tmp, return_data = TRUE)
  expect_s3_class(nacdc, "data.frame")
  expect_gt(nrow(nacdc), 0L)
  expect_true("CARE_TYPE" %in% names(nacdc))
  expect_true("FUNCTIONAL_CAPACITY_SCORE" %in% names(nacdc))
})

test_that("NACDC care types are valid", {
  tmp <- file.path(tempdir(), paste0("nacdc_test2_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 2000L, seed = 2L, output_dir = tmp)
  nacdc <- generate_nacdc(spine = spine, seed = 2L, output_dir = tmp, return_data = TRUE)
  if (nrow(nacdc) > 0L) {
    expect_true(all(nacdc$CARE_TYPE %in% c(
      "Commonwealth Home Support Program", "Home Care Packages Program",
      "Residential care", "Transition Care Program"
    )))
  }
})

test_that("NACDC functional score is 0-100", {
  tmp <- file.path(tempdir(), paste0("nacdc_test3_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 2000L, seed = 3L, output_dir = tmp)
  nacdc <- generate_nacdc(spine = spine, seed = 3L, output_dir = tmp, return_data = TRUE)
  if (nrow(nacdc) > 0L) {
    expect_true(all(nacdc$FUNCTIONAL_CAPACITY_SCORE >= 0 &
                    nacdc$FUNCTIONAL_CAPACITY_SCORE <= 100))
  }
})



# -- Payroll demographics agree with the spine -------------------------------

test_that("the STP birth month is the person's, not a hash of their id", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_stp_dob_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- build_fplida(n = 20000L, seed = 12L, years = 2020L,
                         products = c("spine", "core", "stp"),
                         k_slices = 2L, output_dir = tmp,
                         complete_dil_schema = FALSE,
                         export_base_file = TRUE)
  run_dir <- result$canonical_run_dir

  files <- list.files(file.path(run_dir, "ato-stp"), pattern = "\\.parquet$",
                      recursive = TRUE, full.names = TRUE)
  files <- files[grepl("pay_events", files)]
  skip_if(length(files) == 0L, "no pay events generated")

  pay <- as.data.frame(arrow::open_dataset(files, unify_schemas = TRUE))
  spine <- as.data.frame(arrow::read_parquet(
    file.path(run_dir, "_system", "base-spine.parquet")))

  merged <- merge(
    unique(pay[, c("SYNTHETIC_AEUID", "BIRTH_YEAR_MONTH_ABS")]),
    spine[, c("aeuid_ato", "birth_year", "month_of_birth")],
    by.x = "SYNTHETIC_AEUID", by.y = "aeuid_ato")
  skip_if(nrow(merged) == 0L, "no payroll rows joined to the spine")

  year <- as.integer(substr(merged$BIRTH_YEAR_MONTH_ABS, 1, 4))
  month <- as.integer(substr(merged$BIRTH_YEAR_MONTH_ABS, 5, 6))

  # A month drawn independently of the person matches one time in twelve,
  # so an age derived from payroll is right to the year and wrong by up to
  # eleven months.
  expect_true(all(year == merged$birth_year))
  expect_true(all(month == merged$month_of_birth))
})


# -- The payroll financial year ----------------------------------------------

test_that("PYRL_FNCL_YR is an integer ending year in every STP table family", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_stp_fy_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # 2022 and 2023 together span the whole of the 2022-23 financial year, which
  # is what gives the monthly tables rows to check on both sides of 30 June.
  result <- build_fplida(n = 4000L, seed = 12L, years = 2022L:2023L,
                         products = c("spine", "core", "stp"),
                         k_slices = 2L, output_dir = tmp,
                         complete_dil_schema = FALSE)
  run_dir <- result$canonical_run_dir

  files <- list.files(file.path(run_dir, "ato-stp"), pattern = "\\.parquet$",
                      recursive = TRUE, full.names = TRUE)
  skip_if(length(files) == 0L, "no STP tables generated")

  # The financial year each table must carry, read from its own name. A
  # monthly table names its calendar year, and a financial year ends on 30
  # June, so July to December belongs to the year that ends the following
  # June. A jobs or ETP table names the financial year outright.
  expected_year <- function(path) {
    name <- basename(dirname(path))
    monthly <- regmatches(name, regexec("_((?:19|20)[0-9]{2})_m([0-9]{2})$",
                                        name))[[1L]]
    if (length(monthly)) {
      year <- as.integer(monthly[[2L]])
      month <- as.integer(monthly[[3L]])
      return(year + as.integer(month >= 7L))
    }
    financial <- regmatches(name, regexec("_((?:19|20)[0-9]{2})_([0-9]{2})$",
                                          name))[[1L]]
    if (length(financial)) {
      return(2000L + as.integer(financial[[3L]]))
    }
    NA_integer_
  }

  # Only stp_jobs was ever confirmed in the lab, so pay and ETP are the reason
  # this test exists: a two-part label here forces every consumer to parse it.
  seen <- character(0)
  for (path in files) {
    want <- expected_year(path)
    if (is.na(want)) next
    frame <- as.data.frame(arrow::read_parquet(path))
    if (!nrow(frame)) next
    family <- if (grepl("pay_events", path)) "pay_events" else
      if (grepl("_etp_", path)) "etp" else "jobs"
    seen <- union(seen, family)
    expect_true("PYRL_FNCL_YR" %in% names(frame), info = path)
    expect_type(frame$PYRL_FNCL_YR, "integer")
    expect_false(anyNA(frame$PYRL_FNCL_YR), info = path)
    # An ending year, not a calendar year and not a two-part label.
    expect_true(all(frame$PYRL_FNCL_YR == want), info = path)
  }
  expect_setequal(seen, c("pay_events", "jobs", "etp"))
})

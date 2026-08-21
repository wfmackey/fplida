

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

test_that("write_agency_spine creates correct parquet by default", {
  spine <- generate_spine(n = 200L, seed = 1L)
  tmp <- file.path(tempdir(), "linkage_test")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- write_agency_spine(spine, "ABS", tmp)

  expect_true(file.exists(path))
  expect_match(basename(path), "abs-spine\\.parquet")

  df <- as.data.frame(arrow::read_parquet(path))
  expect_equal(nrow(df), nrow(spine))
  expect_named(df, c("spine_id", "SYNTHETIC_AEUID"))
  expect_identical(df$SYNTHETIC_AEUID, spine$aeuid_abs)
})

test_that("write_agency_spine creates CSV when format = 'csv'", {
  spine <- generate_spine(n = 100L, seed = 1L)
  tmp <- file.path(tempdir(), "linkage_test_csv")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- write_agency_spine(spine, "ABS", tmp, format = "csv")

  expect_true(file.exists(path))
  expect_match(basename(path), "abs-spine\\.csv")

  csv <- read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(csv), nrow(spine))
  expect_named(csv, c("spine_id", "SYNTHETIC_AEUID"))
})

test_that("write_agency_spine is idempotent", {
  spine <- generate_spine(n = 100L, seed = 1L)
  tmp <- file.path(tempdir(), "linkage_test_idem")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path1 <- write_agency_spine(spine, "ATO", tmp)
  mtime1 <- file.mtime(path1)

  # Small delay to ensure mtime would differ if file was rewritten
  Sys.sleep(1.1)

  path2 <- write_agency_spine(spine, "ATO", tmp)
  mtime2 <- file.mtime(path2)

  expect_equal(path1, path2)
  expect_equal(mtime1, mtime2)
})

test_that("write_agency_spine applies default linkage rate", {
  spine <- generate_spine(n = 2000L, seed = 1L)
  tmp <- file.path(tempdir(), "linkage_test_na")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  write_agency_spine(spine, "DSS", tmp)
  df <- as.data.frame(arrow::read_parquet(
    file.path(tmp, "dss-spine.parquet")
  ))

  # Some spine_ids should be NA (linkage failures)
  expect_true(any(is.na(df$spine_id)))
  expect_equal(mean(!is.na(df$spine_id)), 0.90)
})

test_that("write_agency_spine applies ATO non-lodgement before linkage", {
  spine <- generate_spine(n = 5000L, seed = 1L)
  tmp <- file.path(tempdir(), "linkage_test_ato_rate")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  write_agency_spine(spine, "DSS", tmp)
  write_agency_spine(spine, "ATO", tmp)

  dss <- as.data.frame(arrow::read_parquet(
    file.path(tmp, "dss-spine.parquet")
  ))
  ato <- as.data.frame(arrow::read_parquet(
    file.path(tmp, "ato-spine.parquet")
  ))

  dss_rate <- mean(!is.na(dss$spine_id))
  ato_record_rate <- mean(!is.na(ato$spine_id))
  ato_population_rate <- sum(!is.na(ato$spine_id)) / nrow(spine)
  expected_ato_rows <- sum(fplida:::.ato_record_mask(spine))

  expect_equal(dss_rate, 0.90)
  expect_equal(nrow(ato), expected_ato_rows)
  expect_lt(nrow(ato), nrow(dss))
  expect_equal(ato_record_rate,
               round(expected_ato_rows * 0.98) / expected_ato_rows)
  expect_lt(ato_population_rate, dss_rate)
})

test_that("ATO non-lodgement is correlated with income and retirement", {
  n <- 1000L
  spine <- data.frame(
    aeuid_ato = fplida:::generate_aeuids(4L * n, "ATO", 1L),
    birth_year = c(
      rep(1984L, n), # working age, above threshold
      rep(1984L, n), # working age, below threshold
      rep(1954L, n), # retirement age, below threshold
      rep(1954L, n)  # retirement age, above threshold
    ),
    baseline_employed = c(
      rep(1L, n),
      rep(1L, n),
      rep(0L, n),
      rep(0L, n)
    ),
    baseline_income = c(
      rep(60000, n),
      rep(10000, n),
      rep(10000, n),
      rep(60000, n)
    )
  )

  p <- fplida:::.ato_record_probability(spine)
  group <- rep(
    c("working_above", "working_below", "retired_below", "retired_above"),
    each = n
  )

  expect_gt(mean(p[group == "working_above"]),
            mean(p[group == "working_below"]))
  expect_gt(mean(p[group == "retired_above"]),
            mean(p[group == "retired_below"]))
  expect_gt(mean(p[group == "working_below"]),
            mean(p[group == "retired_below"]))
})

test_that("write_agency_spine rejects unknown agency", {
  spine <- generate_spine(n = 10L, seed = 1L)
  expect_error(write_agency_spine(spine, "FAKE", tempdir()), "known agency")
})

test_that("write_agency_spine accepts lowercase agency code", {
  spine <- generate_spine(n = 50L, seed = 1L)
  tmp <- file.path(tempdir(), "linkage_test_lower")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- write_agency_spine(spine, "abs", tmp)
  expect_true(file.exists(path))
})

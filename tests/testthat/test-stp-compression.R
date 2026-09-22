test_that("STP compression validates its optional level before building", {
  expect_null(.validate_stp_zstd_level(NULL))
  expect_identical(.validate_stp_zstd_level(6), 6L)
  for (level in list(0, 23, 1.5, NA_real_, Inf, c(1, 2), "6", TRUE)) {
    expect_error(build_fplida(stp_zstd_level = level), "stp_zstd_level")
  }
})

test_that("STP compression preserves types, metadata, missing and float values", {
  directory <- tempfile("stp-compression-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "events.parquet")
  original <- arrow::Table$create(data.frame(
    id = c("0001", "0001", NA, ""), value = c(NA_real_, NaN, Inf, -0),
    count = c(1L, NA_integer_, -4L, 2L),
    date = as.Date(c("2024-01-01", NA, "2024-01-03", "2024-12-31"))
  ))
  original <- original$ReplaceSchemaMetadata(list(source = "test-fixture"))
  arrow::write_parquet(original, path, compression = "snappy")
  result <- .reencode_stp_parquet_file(path, 6L)
  actual <- arrow::read_parquet(path, as_data_frame = FALSE)
  expect_true(original$schema$Equals(actual$schema, check_metadata = TRUE))
  expect_true(identical(as.data.frame(original), as.data.frame(actual), num.eq = FALSE))
  expect_equal(result$rows, 4)
  expect_equal(result$output_bytes, file.size(path))
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "events.parquet")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  codecs <- DBI::dbGetQuery(con, paste0(
    "SELECT DISTINCT compression FROM parquet_metadata(",
    DBI::dbQuoteString(con, path), ")"
  ))
  expect_identical(codecs$compression, "ZSTD")
})

test_that("STP compression preserves empty file schemas", {
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path), add = TRUE)
  original <- arrow::Table$create(id = character(), amount = double())
  arrow::write_parquet(original, path)
  result <- .reencode_stp_parquet_file(path, 6L)
  actual <- arrow::read_parquet(path, as_data_frame = FALSE)
  expect_equal(result$rows, 0)
  expect_true(original$Equals(actual, check_metadata = TRUE))
})

test_that("STP compression leaves the original file on write or validation failure", {
  directory <- tempfile("stp-failure-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "events.parquet")
  arrow::write_parquet(data.frame(id = "001", value = 1), path)
  before <- tools::md5sum(path)
  writers <- list(
    function(table, path, level) {
      writeLines("incomplete file", path)
      stop("injected write failure")
    },
    function(table, path, level) {
      arrow::write_parquet(data.frame(id = "001", value = 2), path)
    },
    function(table, path, level) {
      arrow::write_parquet(data.frame(different_column = "001", value = 1), path)
    }
  )
  for (writer in writers) {
    reencode <- .reencode_stp_parquet_file
    environment(reencode) <- list2env(list(.write_stp_zstd = writer),
                                     parent = environment(reencode))
    expect_error(reencode(path, 6L), "write failure|verification failed")
    expect_identical(tools::md5sum(path), before)
    expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "events.parquet")
  }
})

test_that("STP compression leaves the original file on replacement failure", {
  directory <- tempfile("stp-replace-failure-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "events.parquet")
  arrow::write_parquet(data.frame(id = "001", value = 1), path)
  before <- tools::md5sum(path)
  reencode <- .reencode_stp_parquet_file
  environment(reencode) <- list2env(list(
    .replace_stp_parquet = function(temporary, path) stop("injected replacement failure")
  ), parent = environment(reencode))
  expect_error(reencode(path, 6L), "replacement failure")
  expect_identical(tools::md5sum(path), before)
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "events.parquet")
})

test_that("STP slice compression is opt-in and excludes agency spines", {
  directory <- tempfile("stp-slice-compression-")
  product <- file.path(directory, "ato-stp", "stp_standard_pay_events_2024_m01")
  dir.create(product, recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(product, "part-001.parquet")
  spine <- file.path(directory, "ato-stp", "ato-spine.parquet")
  arrow::write_parquet(data.frame(id = "001", value = 1), path)
  arrow::write_parquet(data.frame(id = "001", spine_id = 1L), spine)
  before <- tools::md5sum(c(path, spine))
  expect_null(.compress_slice_stp(directory, NULL))
  expect_identical(tools::md5sum(c(path, spine)), before)
  previous_threads <- arrow::cpu_count()
  result <- .compress_slice_stp(directory, 6L)
  expect_identical(result$codec, "zstd")
  expect_identical(result$level, 6L)
  expect_identical(result$files, 1L)
  expect_equal(result$rows, 1)
  expect_identical(tools::md5sum(spine), before[spine])
  expect_identical(arrow::cpu_count(), previous_threads)
})

test_that("STP slice compression reports an empty population without error", {
  directory <- tempfile("stp-empty-slice-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  result <- .compress_slice_stp(directory, 6L)
  expect_identical(result$codec, "zstd")
  expect_identical(result$level, 6L)
  expect_identical(result$files, 0L)
  expect_equal(result$input_bytes, 0)
  expect_equal(result$output_bytes, 0)
  expect_equal(result$rows, 0)
})

test_that("slice workers compress STP before the next product and record results", {
  directory <- tempfile("stp-worker-order-")
  dir.create(file.path(directory, "_system"), recursive = TRUE)
  file.create(file.path(directory, "_system", "base-spine.parquet"))
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  withr::local_options(fplida.run_dir = NULL)
  calls <- character()
  worker <- build_fplida_slice_worker
  environment(worker) <- list2env(list(
    .dispatch_slice_product = function(product, ...) {
      calls <<- c(calls, product)
      list(n_records = 2L)
    },
    .compress_slice_stp = function(slice_run_dir, level) {
      calls <<- c(calls, "compression")
      list(codec = "zstd", level = level, files = 1L,
           input_bytes = 100, output_bytes = 70, rows = 2)
    }
  ), parent = environment(worker))
  result <- worker(directory, 0L, 42L, 2024L, c("stp", "ndis"),
                   product_years = list(stp = 2024L, ndis = 2024L),
                   stp_zstd_level = 6L)
  expect_identical(calls, c("stp", "compression", "ndis"))
  expect_equal(result$product_results$stp$metadata$compression$output_bytes, 70)
  expect_equal(result$product_results$stp$metadata$n_records, 2L)
  calls <- character()
  default <- worker(directory, 0L, 42L, 2024L, "stp",
                    product_years = list(stp = 2024L))
  expect_identical(calls, "stp")
  expect_null(default$product_results$stp$metadata$compression)
})

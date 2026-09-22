test_that("STP compression runs in fresh PSOCK workers and survives consolidation", {
  directory <- tempfile("stp-compressed-build-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  withr::local_options(fplida.data_path = directory, fplida.run_dir = NULL)
  result <- build_fplida(n = 120L, seed = 42L, years = 2024L, products = "stp",
                         k_slices = 2L, n_workers = 1L, rayon_threads = 1L,
                         output_dir = directory, stp_zstd_level = 6L)
  expect_identical(result$stp_zstd_level, 6L)
  expect_length(result$worker_results, 2L)
  for (worker in result$worker_results) {
    expect_null(worker$product_results$stp$metadata$error)
    compression <- worker$product_results$stp$metadata$compression
    expect_identical(compression$codec, "zstd")
    expect_identical(compression$level, 6L)
    expect_gt(compression$files, 0)
    expect_gt(compression$input_bytes, 0)
    expect_gt(compression$output_bytes, 0)
  }
  paths <- list.files(file.path(result$canonical_run_dir, "ato-stp"),
                       pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  paths <- paths[grepl("^stp_(standard|extended)_", basename(dirname(paths)))]
  expect_gt(length(paths), 0)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  quoted <- paste(DBI::dbQuoteString(con, paths), collapse = ",")
  codecs <- DBI::dbGetQuery(con, paste0(
    "SELECT DISTINCT compression FROM parquet_metadata([", quoted, "])"
  ))
  expect_identical(codecs$compression, "ZSTD")
})

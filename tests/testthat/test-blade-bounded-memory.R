test_that("BLADE column assembly preserves rows across parallel Parquet scans", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  tmp <- tempfile("blade-columns-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  index <- seq_len(150013L)
  expected <- data.frame(
    bn = sprintf("BN%011d", index),
    amount = sin(index) * 100000,
    nullable = ifelse(index %% 11L == 0L, NA_integer_, index),
    label = ifelse(index %% 17L == 0L, NA_character_, paste0("row-", index)),
    date = as.Date("2000-01-01") + index %% 9000L,
    flag = index %% 3L == 0L
  )
  path <- file.path(tmp, "assembled.parquet")
  count <- .write_blade_columns(names(expected),
    function(columns) expected[, columns, drop = FALSE], path, nrow(expected),
    batch_columns = 2L, threads = 4L)
  expect_equal(count, nrow(expected))
  expect_equal(as.data.frame(arrow::read_parquet(path)), expected)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  groups <- DBI::dbGetQuery(con, paste0(
    "SELECT count(DISTINCT row_group_id) AS n FROM parquet_metadata(",
    DBI::dbQuoteString(con, path), ")"))$n
  expect_gt(as.numeric(groups), 1L)
  expect_equal(list.files(tmp, all.files = TRUE, pattern = "^\\.blade-"), character())
})

test_that("failed BLADE column assembly leaves the published file intact", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  tmp <- tempfile("blade-columns-failure-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  path <- file.path(tmp, "existing.parquet")
  arrow::write_parquet(data.frame(original = 1:3), path)
  checksum <- unname(tools::md5sum(path))
  expect_error(.write_blade_columns(c("first", "second"), function(columns) {
    if (columns == "second") stop("interrupted generation")
    data.frame(first = 1:3)
  }, path, 3L, batch_columns = 1L), "interrupted generation")
  expect_equal(unname(tools::md5sum(path)), checksum)
  expect_equal(list.files(tmp, all.files = TRUE, pattern = "^\\.blade-"), character())
  expect_error(.write_blade_columns("first", function(columns) {
    data.frame(first = 1:2)
  }, path, 3L), "row count or columns")
  expect_equal(unname(tools::md5sum(path)), checksum)
})

test_that("bounded BLADE generation matches full frames and relationships", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  tmp <- tempfile("blade-bounded-parity-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  generate_spine(n = 400L, seed = 47L, output_dir = tmp, use_template = FALSE)
  tables <- c(6L, 29L, 49L, 53L, 59L)
  expected <- generate_blade(tables = tables, seed = 47L,
    output_dir = tmp, return_data = TRUE, include_keys = FALSE)
  old <- options(fplida.blade_max_frame_cells = 0)
  on.exit(options(old), add = TRUE)
  actual <- generate_blade(tables = tables, seed = 47L,
    output_dir = tmp, return_data = FALSE, include_keys = FALSE)
  expect_named(actual$tables, names(expected))
  for (name in names(expected)) {
    rownames(expected[[name]]) <- NULL
    expect_equal(as.data.frame(arrow::read_parquet(actual$tables[[name]]$path)),
      expected[[name]], info = name)
  }
  expect_error(generate_blade(output_dir = tmp, format = "csv"), "parquet only")
})

test_that("sampling EEH before generation keeps employee IDs and historical earnings", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  tmp <- tempfile("blade-eeh-sampled-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  generate_spine(n = 400L, seed = 47L, output_dir = tmp, use_template = FALSE)
  for (year in c(2018L, 2023L)) {
    expected <- generate_blade(tables = 17L, seed = 47L, years = year,
      sample_rate = 0.8, max_rows = 17L, output_dir = tmp,
      return_data = TRUE, include_keys = FALSE)[[1L]]
    actual <- generate_blade(tables = 17L, seed = 47L, years = year,
      sample_rate = 0.8, max_rows = 17L, output_dir = tmp,
      return_data = FALSE, include_keys = FALSE)
    rownames(expected) <- NULL
    expect_equal(nrow(expected), 17L)
    expect_equal(as.data.frame(arrow::read_parquet(actual$tables[[1L]]$path)), expected,
      info = paste("ending year", year))
  }
})

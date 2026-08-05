# --- Input validation ---

test_that("build_fplida rejects unknown products", {
  expect_error(
    build_fplida(products = c("spine", "bogus")),
    "Unknown products: bogus"
  )
})

test_that("build_fplida rejects unknown exclude_products", {
  expect_error(
    build_fplida(exclude_products = c("bogus")),
    "Unknown exclude_products: bogus"
  )
})

test_that("build_fplida rejects excluding spine", {
  expect_error(
    build_fplida(exclude_products = "spine"),
    "Cannot exclude 'spine'"
  )
})

test_that("build_fplida rejects n <= 0", {
  expect_error(build_fplida(n = 0))
  expect_error(build_fplida(n = -1))
})

test_that("build_fplida rejects empty years", {
  expect_error(build_fplida(years = integer(0)))
})


# --- Product resolution ---

test_that("build_fplida always includes spine even when not listed", {
  result <- build_fplida(n = 500L, seed = 1L, products = c("census"),
                         k_slices = 1L, years = 2020:2021)
  expect_true("spine" %in% result$products)
})

test_that("build_fplida auto-adds pit_ps when pit_itr requested", {
  result <- build_fplida(n = 500L, seed = 1L,
                         products = c("pit_itr"),
                         k_slices = 1L, years = 2020:2021)
  expect_true("pit_ps" %in% result$products)
  expect_true("pit_itr" %in% result$products)
})

test_that("build_fplida respects exclude_products", {
  result <- build_fplida(n = 500L, seed = 1L,
                         exclude_products = c("mbs", "pbs"),
                         k_slices = 1L, years = 2020:2021)
  expect_false("mbs" %in% result$products)
  expect_false("pbs" %in% result$products)
  expect_true("spine" %in% result$products)
  expect_true("census" %in% result$products)
})


# --- Integration: small build ---

test_that("build_fplida builds spine + census + core end-to-end", {
  result <- build_fplida(n = 500L, seed = 1L,
                         products = c("census", "core"),
                         k_slices = 1L, years = 2020:2021)

  expect_type(result, "list")
  expect_equal(result$n, 500L)
  expect_equal(result$seed, 1L)
  expect_equal(sort(result$products), c("census", "core", "spine"))
  expect_true(result$total_elapsed_seconds > 0)
  expect_equal(result$format, "parquet")
  expect_equal(result$build_format, "parquet")
  expect_true(!is.null(result$canonical_run_dir))
  expect_true(dir.exists(result$canonical_run_dir))

  # Stage timings recorded
  expect_true(!is.null(result$stage_timings$spine))
  expect_true(!is.null(result$stage_timings$core))
})

test_that("build_fplida builds pit_ps + pit_itr in correct order", {
  result <- build_fplida(n = 500L, seed = 1L,
                         products = c("pit_ps", "pit_itr"),
                         k_slices = 1L, years = 2020:2021)

  # pit_ps must precede pit_itr in the build order
  idx_ps  <- which(result$products == "pit_ps")
  idx_itr <- which(result$products == "pit_itr")
  expect_true(idx_ps < idx_itr)
})

test_that("build_fplida omits the internal base file by default", {
  result <- build_fplida(n = 500L, seed = 1L,
                         products = c("census"),
                         k_slices = 1L, years = 2020:2021)

  run_dir <- result$canonical_run_dir
  expect_true(dir.exists(run_dir))

  # The build uses the base spine internally but does not export it.
  spine_path <- file.path(run_dir, "_system", "base-spine.parquet")
  expect_false(file.exists(spine_path))

  # Census dataset folder on disk
  census_dir <- file.path(run_dir, "abs-census")
  expect_true(dir.exists(census_dir))
})

test_that("build_fplida exports the base file only when requested", {
  result <- build_fplida(n = 500L, seed = 1L,
                         products = c("census"),
                         k_slices = 1L, years = 2020:2021,
                         export_base_file = TRUE)

  expect_true(result$export_base_file)
  expect_true(file.exists(file.path(
    result$canonical_run_dir, "_system", "base-spine.parquet"
  )))
})

test_that("build_fplida parallel k_slices=2 works", {
  # Verify the sliced path spawns workers and merges cleanly.
  result <- build_fplida(n = 1000L, seed = 1L,
                         products = c("census", "mbs"),
                         k_slices = 2L, years = 2020:2021)

  expect_true("mbs" %in% result$products)
  expect_equal(length(result$worker_results), 2L)
  # Merged output present in canonical run dir
  expect_true(dir.exists(file.path(result$canonical_run_dir, "dhda-mbs")))
})


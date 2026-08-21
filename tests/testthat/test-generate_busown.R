# BUSOWN is the person-to-business concordance, published split by legal
# form. These tests guard the property the split exists to carry: a
# partnership has more than one owner and a sole trader has exactly one.

.make_busown_test_data <- function(n = 20000L, seed = 3L,
                                   years = 2015L:2022L) {
  tmp <- tempfile("fplida_busown_")
  dir.create(tmp)
  result <- build_fplida(n = n, seed = seed, years = years,
                         products = c("spine", "core", "blade", "busown"),
                         k_slices = 2L, output_dir = tmp,
                         complete_dil_schema = FALSE,
                         export_base_file = TRUE)
  run_dir <- result$canonical_run_dir
  files <- list.files(file.path(run_dir, "ato-busown"),
                      pattern = "\\.parquet$", full.names = TRUE)
  read_all <- function(paths) {
    if (!length(paths)) return(NULL)
    do.call(rbind, lapply(paths, function(p) {
      as.data.frame(arrow::read_parquet(p))[
        , c("SYNTHETIC_AEUID", "FIN_YEAR", "ABN_HASH_TRUNC")]
    }))
  }
  list(
    tmp = tmp,
    run_dir = run_dir,
    partnerships = read_all(files[grepl("partner", basename(files))]),
    sole_traders = read_all(files[grepl("sole", basename(files))])
  )
}

owners_per_business_year <- function(df) {
  tapply(df$SYNTHETIC_AEUID, paste(df$ABN_HASH_TRUNC, df$FIN_YEAR),
         function(x) length(unique(x)))
}


test_that("BUSOWN emits both legal forms", {
  skip_if_not_installed("arrow")
  td <- .make_busown_test_data()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  expect_gt(nrow(td$partnerships), 0L)
  expect_gt(nrow(td$sole_traders), 0L)
})

test_that("a partnership has more than one owner", {
  skip_if_not_installed("arrow")
  td <- .make_busown_test_data()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  owners <- owners_per_business_year(td$partnerships)
  expect_true(all(owners >= 2L))
  # A rule that counts co-owners must have something to count: if every
  # partnership had exactly two, partner-count variation would be untestable.
  expect_gt(max(owners), 2L)
})

test_that("a sole trader has exactly one owner", {
  skip_if_not_installed("arrow")
  td <- .make_busown_test_data()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  expect_true(all(owners_per_business_year(td$sole_traders) == 1L))
})

test_that("a business holds one legal form only", {
  skip_if_not_installed("arrow")
  td <- .make_busown_test_data()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  expect_length(intersect(td$partnerships$ABN_HASH_TRUNC,
                          td$sole_traders$ABN_HASH_TRUNC), 0L)
})

test_that("businesses have ownership spells rather than a single year", {
  skip_if_not_installed("arrow")
  td <- .make_busown_test_data()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  all_rows <- rbind(td$partnerships, td$sole_traders)
  years <- tapply(all_rows$FIN_YEAR, all_rows$ABN_HASH_TRUNC,
                  function(x) length(unique(x)))
  expect_gt(stats::median(years), 1)
  # Entry and exit: not every business may span the whole window.
  expect_lt(min(years), max(years))
})

test_that("most two-partner partnerships are co-resident", {
  skip_if_not_installed("arrow")
  td <- .make_busown_test_data()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  spine <- as.data.frame(arrow::read_parquet(
    file.path(td$run_dir, "_system", "base-spine.parquet")))
  merged <- merge(td$partnerships, spine[, c("aeuid_ato", "household_id")],
                  by.x = "SYNTHETIC_AEUID", by.y = "aeuid_ato")
  partners <- tapply(merged$SYNTHETIC_AEUID, merged$ABN_HASH_TRUNC,
                     function(x) length(unique(x)))
  households <- tapply(merged$household_id, merged$ABN_HASH_TRUNC,
                       function(x) length(unique(x)))

  # Spousal partnerships are the common Australian form, so a pair should
  # usually be co-resident. No spine household holds more than two adults,
  # so larger partnerships necessarily reach outside one.
  pairs <- partners == 2L
  expect_gt(mean(households[pairs] == 1L), 0.5)
  # Partnerships are not all family: some partners are unrelated.
  expect_true(any(households > 1L))
})

test_that("BUSOWN businesses are BLADE businesses", {
  skip_if_not_installed("arrow")
  td <- .make_busown_test_data()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  business <- as.data.frame(arrow::read_parquet(
    file.path(td$run_dir, "_system", "business-spine.parquet")))
  all_rows <- rbind(td$partnerships, td$sole_traders)

  # The `ABN` prefix is the fallback that fires when the pool is empty, and
  # it lands outside the `BN` space BLADE and STP share.
  expect_false(any(grepl("^ABN", all_rows$ABN_HASH_TRUNC)))
  expect_true(all(unique(all_rows$ABN_HASH_TRUNC) %in% business$bn))
})

test_that("the emitted tables match the bundled data item list", {
  plan <- fplida:::.busown_file_plan(2010L:2024L)

  expect_gt(nrow(plan), 0L)
  expect_true(all(plan$form %in% c(0L, 1L)))
  expect_true(all(plan$months %in% c(12L, 16L)))
  # Partnerships are published from 2009-10, sole traders from 2010-11.
  expect_true(all(grepl("partner", plan$stem[plan$form == 1L])))
  expect_true(all(grepl("sole", plan$stem[plan$form == 0L])))
  expect_true(any(plan$extract_ref == 1L))
})

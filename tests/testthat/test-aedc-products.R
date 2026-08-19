# AEDC is published as five products a cycle. The generator wrote the core
# table and copied it to the four sibling paths, so a product whose data item
# list gives it fourteen variables arrived with two hundred.

.aedc_products_run <- function(n = 20000L, seed = 8L,
                               cycles = c(2009L, 2015L, 2021L, 2024L)) {
  tmp <- tempfile("fplida_aedc_")
  dir.create(tmp)
  spine <- generate_spine(n = n, seed = seed, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  generate_aedc(spine = spine, seed = seed, cycles = cycles,
                output_dir = tmp, return_data = FALSE)
  list(tmp = tmp, ds_dir = file.path(getOption("fplida.run_dir"), "de-aedc"),
       cycles = cycles)
}

.aedc_read <- function(ds_dir, family, cycle) {
  path <- file.path(ds_dir, sprintf("madipge-aedc-d-%s-%d.parquet",
                                    family, cycle))
  if (!file.exists(path)) return(NULL)
  as.data.frame(arrow::read_parquet(path))
}


test_that("each AEDC sibling carries its own variable list", {
  skip_if_not_installed("arrow")
  td <- .aedc_products_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  info <- as.data.frame(variable_info("AEDC"))
  for (family in c("indigenous", "language", "specialneeds")) {
    frame <- .aedc_read(td$ds_dir, family, 2021L)
    expect_false(is.null(frame), info = family)
    tables <- unique(info$table[
      info$product == sprintf("madipge-aedc-d-%s-2021", family)])
    expected <- sort(unique(info$variable[info$table %in% tables]))
    expect_setequal(names(frame), expected)
  }
})

test_that("a sibling is not a copy of the core record", {
  skip_if_not_installed("arrow")
  td <- .aedc_products_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  core <- .aedc_read(td$ds_dir, "core", 2021L)
  language <- .aedc_read(td$ds_dir, "language", 2021L)
  # The language instrument has fourteen variables; the core record has
  # well over a hundred.
  expect_lt(ncol(language), 30L)
  expect_gt(ncol(core), 100L)
  # But it describes the same children, in the same order.
  expect_identical(language$SYNTHETIC_AEUID, core$SYNTHETIC_AEUID)
})

test_that("vulnerability is cut at the 2009 baseline percentiles", {
  skip_if_not_installed("arrow")
  td <- .aedc_products_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  domains <- c("PHYS", "SOC", "EMOT", "LANGCOG")
  baseline <- .aedc_read(td$ds_dir, "core", 2009L)
  skip_if(is.null(baseline), "no 2009 cycle")

  # A child is developmentally vulnerable below the 10th percentile of the
  # 2009 national baseline, and at risk between the 10th and the 25th.
  for (domain in domains) {
    category <- baseline[[paste0(domain, "CATEGORY")]]
    expect_lt(abs(mean(category == 1L, na.rm = TRUE) - 0.10), 0.02)
    expect_lt(abs(mean(category <= 2L, na.rm = TRUE) - 0.25), 0.03)
    # Three categories, not four: the AEDC measure has vulnerable, at risk
    # and on track.
    expect_true(all(category[!is.na(category)] %in% 1:3))
  }
})

test_that("later cycles are measured against the same ruler", {
  skip_if_not_installed("arrow")
  td <- .aedc_products_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  shares <- vapply(td$cycles, function(cycle) {
    frame <- .aedc_read(td$ds_dir, "core", cycle)
    if (is.null(frame)) return(NA_real_)
    mean(frame$PHYSCATEGORY == 1L, na.rm = TRUE)
  }, numeric(1))
  shares <- shares[!is.na(shares)]
  skip_if(length(shares) < 2L, "fewer than two cycles")

  # Recutting the percentile each cycle would hold every one at 10% and make
  # the headline measure impossible to move.
  expect_gt(stats::sd(shares), 0)
  expect_true(all(shares > 0.02 & shares < 0.25))
})

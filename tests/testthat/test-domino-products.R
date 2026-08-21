# DOMINO publishes 35 products across 64 tables and the bespoke generator
# wrote nine. A pipeline that follows a payment from the base record into its
# history found the tables absent rather than empty, which looks like a broken
# path rather than a missing product.

test_that("every published DOMINO product is written", {
  skip_if_not_installed("arrow")
  tmp <- tempfile("fplida_domino_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 5000L, seed = 8L, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  generate_domino(spine = spine, seed = 8L, output_dir = tmp,
                  return_data = FALSE)
  ds_dir <- file.path(getOption("fplida.run_dir"), "dss-domino")

  files <- list.files(ds_dir, pattern = "\\.parquet$")
  files <- files[grepl("--", files, fixed = TRUE)]
  products <- unique(sub("--.*$", "", files))

  info <- as.data.frame(variable_info("DOMINO"))
  expect_length(setdiff(unique(info$product), products), 0L)
  # The income supplement and entitlement history families are the ones that
  # were absent.
  expect_true(any(grepl("-inc-", products)))
  expect_true(any(grepl("-een-", products)))
  expect_true(any(grepl("-pyh-", products)))
})

test_that("a DOMINO subtable describes the people the base record does", {
  skip_if_not_installed("arrow")
  tmp <- tempfile("fplida_domino_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 5000L, seed = 8L, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  generate_domino(spine = spine, seed = 8L, output_dir = tmp,
                  return_data = FALSE)
  ds_dir <- file.path(getOption("fplida.run_dir"), "dss-domino")

  base <- as.data.frame(arrow::read_parquet(file.path(
    ds_dir, "madipge-dom-monthly-d-base--static-demogs.parquet")))
  added <- list.files(ds_dir, pattern = "^madipge-dom-monthly-d-inc-.*\\.parquet$",
                      full.names = TRUE)
  skip_if(!length(added), "no income supplement tables")

  frame <- as.data.frame(arrow::read_parquet(added[[1L]]))
  expect_true("SYNTHETIC_AEUID" %in% names(frame))
  # A subtable covers the recipients the base record holds, not the whole
  # population and not somebody else.
  expect_true(all(frame$SYNTHETIC_AEUID %in% base$SYNTHETIC_AEUID))
})

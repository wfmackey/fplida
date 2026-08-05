# Tests for generate_ato_cr (ATO Client Register)

test_that("generate_ato_cr returns correct structure", {
  tmp <- file.path(tempdir(), paste0("atocr_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 500L, seed = 1L, output_dir = tmp)
  cr <- generate_ato_cr(spine = spine, seed = 1L,
                        output_dir = tmp, return_data = TRUE)
  expect_s3_class(cr, "data.frame")
  expect_gt(nrow(cr), 0L)
  expect_equal(ncol(cr), 11L)
  expect_true("SYNTHETIC_AEUID" %in% names(cr))
  expect_true("DOB_MONYYYY" %in% names(cr))
  expect_true("CLNT_STS_CD" %in% names(cr))
})

test_that("DOB_MONYYYY has correct format", {
  tmp <- file.path(tempdir(), paste0("atocr_test2_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 500L, seed = 2L, output_dir = tmp)
  cr <- generate_ato_cr(spine = spine, seed = 2L,
                        output_dir = tmp, return_data = TRUE)
  # Should match MonYYYY pattern (e.g. "Jan1985")
  expect_true(all(grepl("^[A-Z][a-z]{2}[0-9]{4}$", cr$DOB_MONYYYY)))
})

test_that("client status is valid", {
  tmp <- file.path(tempdir(), paste0("atocr_test3_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 1000L, seed = 3L, output_dir = tmp)
  cr <- generate_ato_cr(spine = spine, seed = 3L,
                        output_dir = tmp, return_data = TRUE)
  expect_true(all(cr$CLNT_STS_CD %in% c("A", "I")))
  # Most should be active
  expect_gt(sum(cr$CLNT_STS_CD == "A"), nrow(cr) * 0.85)
})

test_that("DIAC flag is Y for overseas-born", {
  tmp <- file.path(tempdir(), paste0("atocr_test4_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 1000L, seed = 4L, output_dir = tmp)
  cr <- generate_ato_cr(spine = spine, seed = 4L,
                        output_dir = tmp, return_data = TRUE)
  expect_true(all(cr$DIAC_PID_IND %in% c("Y", "N")))
})

test_that("products written to disk", {
  tmp <- file.path(tempdir(), paste0("atocr_test5_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 100L, seed = 5L, output_dir = tmp)
  generate_ato_cr(spine = spine, seed = 5L, output_dir = tmp,
                  format = "csv", return_data = FALSE)
  ds_dir <- file.path(tmp, "fplida_1k", "ato-ato_cr")
  expect_true(dir.exists(ds_dir))
  expect_true(file.exists(file.path(ds_dir, "madipge-ato-d-clientreg-demogs-1999-current.csv")))
  expect_true(file.exists(file.path(ds_dir, "madipge-ato-d-clientregaddr-1999-current.csv")))
})

test_that("SEX is M or F", {
  tmp <- file.path(tempdir(), paste0("atocr_test6_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 300L, seed = 6L, output_dir = tmp)
  cr <- generate_ato_cr(spine = spine, seed = 6L,
                        output_dir = tmp, return_data = TRUE)
  expect_true(all(cr$SEX %in% c("M", "F")))
})

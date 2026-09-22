test_that("VISA observation windows remove rows without changing dates", {
  skip_if_not_installed("arrow")
  folder <- tempfile("visa-window-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  original <- data.frame(
    SYNTHETIC_AEUID = letters[1:5],
    VA_LODGED_DT = as.Date(c("2008-01-01", "2009-01-01", "2010-01-01",
                             "2011-01-01", "2012-01-01")),
    TR_VISA_GRANT_DT = as.Date(c("2009-01-01", "2010-01-01", "2010-06-01",
                                 "2011-06-01", "2012-06-01")),
    BIRTH_DATE = as.Date(rep("1980-01-01", 5L))
  )
  file <- file.path(folder, "madipge-mig-d-visa-2000-current.parquet")
  arrow::write_parquet(original, file)
  .visa_filter_observation_years(file, c(2010L, 2012L))
  actual <- arrow::read_parquet(file)
  expect_equal(as.data.frame(actual), original[c(3L, 5L), ], ignore_attr = TRUE)
  expect_equal(as.character(actual$BIRTH_DATE), rep("1980-01-01", 2L))
})

test_that("VISA canonical application companions use their event date", {
  skip_if_not_installed("arrow")
  folder <- tempfile("visa-companion-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  file <- file.path(folder, "visa--applications.parquet")
  arrow::write_parquet(data.frame(
    id = 1:3, VA_LODGED_DT = as.Date(c("2008-01-01", "2010-03-01", NA))
  ), file)
  .visa_filter_observation_years(file, 2010L)
  expect_equal(arrow::read_parquet(file)$id, c(2L, 3L))
  .visa_filter_observation_years(file, 2020L)
  expect_equal(arrow::read_parquet(file)$id, 3L)
})

test_that("VISA empty observation windows keep the file schema", {
  skip_if_not_installed("arrow")
  folder <- tempfile("visa-empty-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  file <- file.path(folder, "visa.parquet")
  arrow::write_parquet(data.frame(
    id = 1L, TR_VISA_GRANT_DT = as.Date("2001-01-01")
  ), file)
  .visa_filter_observation_years(file, 2010L)
  actual <- arrow::read_parquet(file)
  expect_equal(nrow(actual), 0L)
  expect_named(actual, c("id", "TR_VISA_GRANT_DT"))
})

test_that("VISA nomination dates follow the observed application", {
  rows <- data.frame(spine_id = c("1", "2"), birth_year = c(1980L, 1981L),
                     anzsco_code = c("221111", "221111"))
  original <- data.frame(
    VISA_SUBCLASS_CD = c(189L, 500L), VA_PRIMARY_FL = c("Y", "Y"),
    VA_LODGED_DT = as.Date(c("2011-03-01", "2011-05-01")),
    TR_VISA_GRANT_DT = as.Date(c("2011-09-01", "2011-11-01"))
  )
  lodged <- .dil_visa_source_value("NM_LODGED_DT", original, rows, 42L)
  approved <- .dil_visa_source_value("NM_APPRVL_DT", original, rows, 42L)
  expect_equal(lodged, as.Date(c("2011-03-01", NA)))
  expect_equal(approved, as.Date(c("2011-04-30", NA)))
})

test_that("completed VISA output keeps valid records inside its window", {
  skip_if_not_installed("arrow")
  folder <- tempfile("visa-completed-window-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 1000L, seed = 8L, output_dir = folder,
                           return_data = TRUE, use_template = FALSE)
  visa <- generate_visa(spine = spine, seed = 8L, output_dir = folder,
                         years = 2010L:2023L, return_data = TRUE)
  expect_gt(nrow(visa), 0L)
  for (field in c("VA_LODGED_DT", "TR_VISA_GRANT_DT",
                  "NM_LODGED_DT", "NM_APPRVL_DT")) {
    present <- visa[[field]][!is.na(visa[[field]])]
    expect_true(all(present >= as.Date("2010-01-01")))
    expect_true(all(present < as.Date("2024-01-01")))
  }
  nominated <- !is.na(visa$NM_LODGED_DT) & !is.na(visa$NM_APPRVL_DT)
  expect_true(all(visa$NM_LODGED_DT[nominated] <= visa$NM_APPRVL_DT[nominated]))
  expect_true(all(visa$NM_APPRVL_DT[nominated] <= visa$TR_VISA_GRANT_DT[nominated]))
})

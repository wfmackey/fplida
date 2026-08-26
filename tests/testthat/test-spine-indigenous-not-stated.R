test_that("the spine emits the not-stated Indigenous code", {
  spine <- generate_spine(n = 20000L, seed = 20260826L)
  expect_true(all(spine$indigenous %in% c(1L, 2L, 3L, 4L, 9L)))
  share <- mean(spine$indigenous == 9L)
  # 4% target; the 100k template is the sampling unit, 3 sd is ~0.19pp
  expect_gt(share, 0.037)
  expect_lt(share, 0.043)
})

test_that("COMBINED reads not stated as never identified", {
  spine <- generate_spine(n = 20000L, seed = 20260826L)
  combined <- generate_combined(spine = spine, return_data = TRUE)
  not_stated <- spine$indigenous == 9L
  expect_gt(sum(not_stated), 0L)
  expect_true(all(combined$EVER_ABORIGINAL_PERSON[not_stated] == 0L))
  expect_true(all(combined$EVER_TSI_PERSON[not_stated] == 0L))
  expect_true(all(combined$EVER_INDIGENOUS_PERSON[not_stated] == 0L))
  expect_identical(
    combined$EVER_INDIGENOUS_PERSON,
    as.integer(combined$EVER_ABORIGINAL_PERSON | combined$EVER_TSI_PERSON)
  )
})

test_that("ACLD translates the spine not-stated code to its own", {
  spine <- generate_spine(n = 8000L, seed = 20260826L)
  acld <- generate_acld(spine = spine, seed = 20260826L, return_data = TRUE)
  # ACLD's published frame: 1-4 substantive, 97 not stated, 99 unlinked.
  for (wave in c("INGP_11", "INGP_16", "INGP_21")) {
    expect_true(all(acld[[wave]] %in% c(1:4, 97L, 99L)), info = wave)
  }
  hit <- match(acld$SYNTHETIC_AEUID, spine$aeuid_abs)
  expect_false(anyNA(hit))
  not_stated <- spine$indigenous[hit] == 9L
  expect_gt(sum(not_stated), 0L)
  expect_true(all(acld$INGP_11[not_stated] == 97L))
})

test_that("Census carries the spine not-stated share, and only it", {
  spine <- generate_spine(n = 8000L, seed = 20260826L)
  person <- generate_census(spine = spine, seed = 20260826L)$person
  expect_true(all(person$INGP %in% c("1", "2", "3", "4", "&", "V")))
  # The "&" share is the spine's not-stated share, not that plus the 5%
  # overlay census_2021.rs used to add on top of it.
  expect_lt(mean(person$INGP == "&"), 0.06)
  expect_gt(mean(person$INGP == "&"), 0.02)
})

test_that("AEDC codes a not-stated child as not stated, not as neither", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("aedc_ind_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  old_run_dir <- getOption("fplida.run_dir")
  on.exit(options(fplida.run_dir = old_run_dir), add = TRUE)

  spine <- generate_spine(n = 12000L, seed = 20260826L, output_dir = tmp)
  generate_aedc(spine = spine, seed = 20260826L, output_dir = tmp)
  ds <- fplida:::dataset_dir(getOption("fplida.run_dir"), "AEDC")
  core <- as.data.frame(arrow::read_parquet(
    file.path(ds, "madipge-aedc-d-core-2021.parquet")
  ), stringsAsFactors = FALSE)
  skip_if(nrow(core) == 0L, "no AEDC 2021 records for this spine")

  # AEDC Data Dictionary v7.0: 1 Aboriginal, 2 TSI, 3 both, 4 neither,
  # 9 not stated or unknown.
  expect_true(all(core$ATSITYPE %in% c(1:4, 9L)))
  key <- match(core$SYNTHETIC_AEUID, as.character(spine$aeuid_de))
  expect_false(anyNA(key))
  expect_true(all(core$ATSITYPE[spine$indigenous[key] == 9L] == 9L))
  expect_true(all(core$ATSITYPE[spine$indigenous[key] == 1L] == 4L))
})

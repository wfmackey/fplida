test_that("generate_core returns expected structure from spine", {
  spine <- generate_spine(n = 500L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expect_type(core, "list")
  expect_named(core, c("demographics", "vitals", "locations",
                       "relationships", "residence"))
  expect_s3_class(core$demographics, "data.frame")
  expect_s3_class(core$vitals, "data.frame")
  expect_s3_class(core$locations, "data.frame")
  expect_s3_class(core$relationships, "data.frame")
  expect_s3_class(core$residence, "data.frame")
})

test_that("generate_core demographics has correct row count and columns", {
  n <- 300L
  spine <- generate_spine(n = n, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expect_equal(nrow(core$demographics), n)

  expected_cols <- c(
    "SPINE_ID", "YEAR_OF_BIRTH", "MONTH_OF_BIRTH", "BIRTH_CTRY_CODE",
    "CORE_GENDER", "YEAR_OF_DEATH", "MONTH_OF_DEATH", "DAY_OF_DEATH"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(core$demographics),
                info = paste("Missing column:", col))
  }
})

test_that("generate_core demographics SPINE_ID matches spine", {
  spine <- generate_spine(n = 200L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expect_identical(core$demographics$SPINE_ID, spine$spine_id)
})

test_that("generate_core demographics YEAR_OF_BIRTH matches spine", {
  spine <- generate_spine(n = 200L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expect_identical(core$demographics$YEAR_OF_BIRTH, spine$birth_year)
})

test_that("generate_core demographics MONTH_OF_BIRTH is valid", {
  spine <- generate_spine(n = 500L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expect_true(all(core$demographics$MONTH_OF_BIRTH %in% 1:12))
})

test_that("generate_core vitals has requested core-scope columns", {
  n <- 200L
  spine <- generate_spine(n = n, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L, years = 2021L)

  expect_equal(nrow(core$vitals), n)
  expect_named(core$vitals, c("spine_id", "month_of_birth", "year_of_birth",
                              "day_of_death", "month_of_death",
                              "year_of_death"))
  expect_identical(core$vitals$spine_id, spine$spine_id)
  expect_identical(core$vitals$year_of_birth, spine$birth_year)
  expect_identical(core$vitals$month_of_birth, spine$month_of_birth)
})

test_that("generate_core residence is person-month and bounded", {
  n <- 50L
  spine <- generate_spine(n = n, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L, years = 2020L:2021L)

  expect_equal(nrow(core$residence), n * 24L)
  expect_named(core$residence, c("spine_id", "pp_period", "pp_weight"))
  expect_true(inherits(core$residence$pp_period, "Date"))
  expect_true(all(core$residence$pp_weight >= 0 &
                  core$residence$pp_weight <= 1))
})

test_that("generate_core demographics CORE_GENDER matches spine sex", {
  spine <- generate_spine(n = 200L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expected <- ifelse(spine$sex == 1L, "M", "F")
  expect_identical(core$demographics$CORE_GENDER, expected)
})

test_that("generate_core demographics BIRTH_CTRY_CODE is valid SACC", {
  spine <- generate_spine(n = 500L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  # All Australia-born should have "1101"
  aus <- spine$country_of_birth == 0L
  expect_true(all(core$demographics$BIRTH_CTRY_CODE[aus] == "1101"))
  # All overseas-born should have a non-"1101" code
  expect_true(all(core$demographics$BIRTH_CTRY_CODE[!aus] != "1101"))
})

test_that("generate_core demographics death fields are consistent", {
  spine <- generate_spine(n = 2000L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)
  d <- core$demographics

  # Dead persons have all three fields non-NA
  dead <- !is.na(d$YEAR_OF_DEATH)
  expect_true(all(!is.na(d$MONTH_OF_DEATH[dead])))
  expect_true(all(!is.na(d$DAY_OF_DEATH[dead])))

  # Living persons have all three fields NA
  alive <- is.na(d$YEAR_OF_DEATH)
  expect_true(all(is.na(d$MONTH_OF_DEATH[alive])))
  expect_true(all(is.na(d$DAY_OF_DEATH[alive])))

  # Death year in valid range
  if (any(dead)) {
    expect_true(all(d$YEAR_OF_DEATH[dead] >= 2006L))
    expect_true(all(d$YEAR_OF_DEATH[dead] <= 2025L))
    expect_true(all(d$MONTH_OF_DEATH[dead] %in% 1:12))
    expect_true(all(d$DAY_OF_DEATH[dead] %in% 1:28))
  }
})

test_that("generate_core demographics some people are deceased", {
  spine <- generate_spine(n = 5000L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  n_dead <- sum(!is.na(core$demographics$YEAR_OF_DEATH))
  # Expect roughly 1-5% deceased (mortality over 15 years)
  expect_gt(n_dead, 0L)
  expect_lt(n_dead / 5000, 0.20)
})


# -- Locations ----------------------------------------------------------------

test_that("generate_core locations has correct row count and columns", {
  n <- 300L
  spine <- generate_spine(n = n, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expect_equal(nrow(core$locations), n)

  expected_cols <- c(
    "SPINE_ID", "STATE", "SA4_ASGS_2021", "SA2_ASGS_2021",
    "SA1_ASGS_2021", "MB_ASGS_2021", "ARID", "ADR_TYP",
    "SOURCE_FLAG", "START_DATE", "END_DATE"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(core$locations),
                info = paste("Missing column:", col))
  }
})

test_that("generate_core locations SPINE_ID matches spine", {
  spine <- generate_spine(n = 200L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expect_identical(core$locations$SPINE_ID, spine$spine_id)
})

test_that("generate_core locations STATE matches spine", {
  spine <- generate_spine(n = 200L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expect_identical(core$locations$STATE, spine$state)
})

test_that("generate_core locations SA codes are real ASGS 2021", {
  spine <- generate_spine(n = 500L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)
  mb_lookup <- fplida:::.load_mb_lookup()

  # SA1 codes should be 11-digit strings
  sa1s <- core$locations$SA1_ASGS_2021
  expect_true(all(nchar(sa1s) == 11L))

  # SA2 codes should be 9-digit strings
  sa2s <- core$locations$SA2_ASGS_2021
  expect_true(all(nchar(sa2s) == 9L))

  # SA4 codes should be 3-digit integers
  sa4s <- core$locations$SA4_ASGS_2021
  expect_true(all(sa4s >= 100L & sa4s <= 899L))

  # Mesh Block codes should be real ABS ASGS 2021 rows, with SA1/SA2/SA4
  # inherited from the same allocation row.
  mb_match <- match(core$locations$MB_ASGS_2021, mb_lookup$mb_code)
  expect_false(anyNA(mb_match))
  expect_identical(sa1s, mb_lookup$sa1_code[mb_match])
  expect_identical(sa2s, mb_lookup$sa2_code[mb_match])
  expect_identical(sa4s, mb_lookup$sa4_code[mb_match])
})

test_that("generate_core locations SA1 is nested within state", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  # First digit of SA1 code should equal the state code
  sa1_state <- as.integer(substr(core$locations$SA1_ASGS_2021, 1L, 1L))
  expect_identical(sa1_state, core$locations$STATE)
})

test_that("generate_core locations SA2 matches the spine SA2", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  # ASGS 2021 geography must agree across products: CORE draws its mesh
  # block from inside the spine SA2, so SA2_ASGS_2021 is the spine value.
  expect_identical(core$locations$SA2_ASGS_2021,
                   as.character(spine$sa2_code))
})

test_that("generate_core locations SA2 matches Census SA2UCP", {
  out <- file.path(tempdir(), "core_census_sa2_test")
  spine <- generate_spine(n = 500L, seed = 3L, output_dir = out)
  core <- generate_core(spine = spine, seed = 3L, output_dir = out)
  census <- generate_census(spine = spine, seed = 3L, output_dir = out)

  # Census only covers people alive on Census night, so compare on the
  # people present in both rather than assuming the spine maps one to one.
  key <- match(as.character(spine$aeuid_abs),
               as.character(census$person$SYNTHETIC_AEUID))
  both <- !is.na(key)
  expect_gt(sum(both), 100L)
  expect_identical(as.integer(core$locations$SA2_ASGS_2021)[both],
                   as.integer(census$person$SA2UCP)[key[both]])
})

test_that("generate_core locations fall back to the state when SA2 is unusable", {
  spine <- generate_spine(n = 500L, seed = 4L)
  spine$sa2_code[seq(1L, 500L, by = 5L)] <- 0L

  loc <- fplida:::project_core_locations(spine, 4L)
  mb_lookup <- fplida:::.load_mb_lookup()

  # Every person still gets a real mesh block whose SA1/SA2/SA4 come from
  # that same allocation row, and which sits in their spine state.
  mb_match <- match(loc$MB_ASGS_2021, mb_lookup$mb_code)
  expect_false(anyNA(mb_match))
  expect_identical(loc$SA1_ASGS_2021, mb_lookup$sa1_code[mb_match])
  expect_identical(loc$SA2_ASGS_2021, mb_lookup$sa2_code[mb_match])
  expect_identical(as.integer(substr(loc$SA1_ASGS_2021, 1L, 1L)), loc$STATE)

  # People with a usable spine SA2 are unaffected.
  keep <- setdiff(seq_len(500L), seq(1L, 500L, by = 5L))
  expect_identical(loc$SA2_ASGS_2021[keep],
                   as.character(spine$sa2_code[keep]))
})

test_that("generate_core locations ARID values are unique", {
  spine <- generate_spine(n = 500L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expect_equal(length(unique(core$locations$ARID)), 500L)
})


# -- Relationships ------------------------------------------------------------

test_that("generate_core relationships has expected columns", {
  spine <- generate_spine(n = 500L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)

  expected_cols <- c(
    "SPINE_ID_ORIGINAL", "SPINE_ID_MAIN_REL", "PAIRID",
    "COMBINED_CATEGORY", "COMBINED_STATUS",
    "RECORD_START", "RECORD_END", "SOURCES", "SOURCE_FLAG"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(core$relationships),
                info = paste("Missing column:", col))
  }
})

test_that("generate_core relationships SPINE_IDs exist in spine", {
  spine <- generate_spine(n = 500L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)
  rel <- core$relationships

  if (nrow(rel) > 0L) {
    valid_ids <- spine$spine_id
    expect_true(all(rel$SPINE_ID_ORIGINAL %in% valid_ids))
    expect_true(all(rel$SPINE_ID_MAIN_REL %in% valid_ids))
  }
})

test_that("generate_core relationships has both partner and parent-child", {
  spine <- generate_spine(n = 2000L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)
  rel <- core$relationships

  cats <- unique(rel$COMBINED_CATEGORY)
  expect_true("Partner" %in% cats)
  expect_true("Parent-Child" %in% cats)
})

test_that("generate_core relationships partner statuses are valid", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)
  rel <- core$relationships

  partner_rows <- rel[rel$COMBINED_CATEGORY == "Partner", ]
  if (nrow(partner_rows) > 0L) {
    expect_true(all(partner_rows$COMBINED_STATUS %in%
                    c("Married", "De facto")))
  }
})

test_that("generate_core relationships parent-child statuses are valid", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)
  rel <- core$relationships

  pc_rows <- rel[rel$COMBINED_CATEGORY == "Parent-Child", ]
  if (nrow(pc_rows) > 0L) {
    expect_true(all(pc_rows$COMBINED_STATUS %in%
                    c("Biological", "Step")))
  }
})

test_that("generate_core relationships PAIRID values are unique", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)
  rel <- core$relationships

  if (nrow(rel) > 0L) {
    expect_equal(length(unique(rel$PAIRID)), nrow(rel))
  }
})

test_that("generate_core relationships RECORD_START are valid dates", {
  spine <- generate_spine(n = 1000L, seed = 1L)
  core <- generate_core(spine = spine, seed = 1L)
  rel <- core$relationships

  if (nrow(rel) > 0L) {
    dates <- as.Date(rel$RECORD_START)
    expect_true(all(!is.na(dates)))
    expect_true(all(dates >= as.Date("1950-01-01")))
    expect_true(all(dates <= as.Date("2024-12-31")))
  }
})


# -- Determinism & file writing -----------------------------------------------

test_that("generate_core is deterministic with same seed", {
  spine <- generate_spine(n = 200L, seed = 42L)
  a <- generate_core(spine = spine, seed = 99L)
  b <- generate_core(spine = spine, seed = 99L)

  expect_identical(a$demographics, b$demographics)
  expect_identical(a$vitals, b$vitals)
  expect_identical(a$locations, b$locations)
  expect_identical(a$relationships, b$relationships)
  expect_identical(a$residence, b$residence)
})

test_that("generate_core different seeds give different output", {
  spine <- generate_spine(n = 200L, seed = 1L)
  a <- generate_core(spine = spine, seed = 1L)
  b <- generate_core(spine = spine, seed = 2L)

  # SPINE_ID should be identical (same spine)
  expect_identical(a$demographics$SPINE_ID, b$demographics$SPINE_ID)
  # Demographics and vitals are now spine-derived; CORE-specific products
  # still vary by seed.
  expect_identical(a$demographics, b$demographics)
  expect_false(identical(a$locations$ARID, b$locations$ARID))
})

test_that("generate_core writes products and ABS spine", {
  tmp <- file.path(tempdir(), "core_abs_test")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 100L, seed = 1L, output_dir = tmp)
  generate_core(spine = spine, seed = 1L, output_dir = tmp, format = "csv")

  run_dir <- file.path(tmp, "fplida_1k")
  core_dir <- file.path(run_dir, "abs-core")
  expect_true(dir.exists(core_dir))

  csv_files <- list.files(core_dir, pattern = "\\.csv$")
  # 5 products + abs-spine.csv
  expect_equal(length(csv_files), 6L)

  abs_spine_path <- file.path(core_dir, "abs-spine.csv")
  expect_true(file.exists(abs_spine_path))
})

test_that("generate_core loads spine from run dir", {
  tmp <- file.path(tempdir(), "core_load_test")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 100L, seed = 5L, output_dir = tmp)
  core <- generate_core(seed = 5L, output_dir = tmp)

  expect_equal(nrow(core$demographics), 100L)
  expect_identical(core$demographics$SPINE_ID, spine$spine_id)
})

test_that("generate_core errors without spine or data path", {
  old_opt <- getOption("fplida.data_path")
  old_run <- getOption("fplida.run_dir")
  old_env <- Sys.getenv("FPLIDA_DATA_PATH", "")
  on.exit({
    options(fplida.data_path = old_opt, fplida.run_dir = old_run)
    if (nzchar(old_env)) {
      Sys.setenv(FPLIDA_DATA_PATH = old_env)
    } else {
      Sys.unsetenv("FPLIDA_DATA_PATH")
    }
  }, add = TRUE)

  options(fplida.data_path = NULL, fplida.run_dir = NULL)
  Sys.unsetenv("FPLIDA_DATA_PATH")

  expect_error(generate_core(), "No output directory")
})

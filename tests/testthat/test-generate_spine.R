test_that("generate_spine returns a data.frame", {
  spine <- generate_spine(n = 500L, seed = 1L)

  expect_s3_class(spine, "data.frame")
  expect_equal(nrow(spine), 500L)
})

test_that("generate_spine has expected columns", {
  spine <- generate_spine(n = 100L, seed = 1L)

  # Linkage
  expect_true("spine_id" %in% names(spine))

  # Demographics
  expect_true("id" %in% names(spine))
  expect_true("birth_year" %in% names(spine))
  expect_true("sex" %in% names(spine))
  expect_true("state" %in% names(spine))

  # Education
  expect_true("education" %in% names(spine))

  # Occupation (ANZSCO)
  expect_true("anzsco_code" %in% names(spine))
  expect_true("anzsco_major" %in% names(spine))
  expect_true("archetype" %in% names(spine))

  # 6D task scores
  task_dims <- c("task_physical", "task_cognitive", "task_vision",
                 "task_hearing", "task_manual_dexterity", "task_communication")
  for (dim in task_dims) {
    expect_true(dim %in% names(spine), info = paste("Missing:", dim))
  }
})

test_that("generate_spine has 59 columns", {
  spine <- generate_spine(n = 100L, seed = 1L)
  # 46 from Rust (incl spine_id + baseline_income + 5 disability cols +
  # person_type + comorbidity_flags + country_of_birth_sacc + sa2/sa3/sa4_code
  # + household_id + 5 core-scope columns) + 13 agency AEUID columns
  expect_equal(ncol(spine), 59L)
  # Cross-cutting code-frame + household columns added in the 2026 upgrade.
  expect_true(all(c("country_of_birth_sacc", "sa2_code", "sa3_code",
                    "sa4_code", "household_id", "month_of_birth",
                    "year_of_death", "month_of_death", "day_of_death",
                    "residence_seed") %in% names(spine)))
})

test_that("generate_spine is deterministic with same seed", {
  a <- generate_spine(n = 200L, seed = 42L)
  b <- generate_spine(n = 200L, seed = 42L)

  expect_identical(a, b)
})

test_that("generate_spine different seeds give different data", {
  a <- generate_spine(n = 200L, seed = 1L)
  b <- generate_spine(n = 200L, seed = 2L)

  expect_false(identical(a, b))
})

test_that("generate_spine validates inputs", {
  expect_error(generate_spine(n = 0L, seed = 1L))
  expect_error(generate_spine(n = -1L, seed = 1L))
})

test_that("generate_spine id is unique", {
  spine <- generate_spine(n = 1000L, seed = 1L)

  expect_equal(length(unique(spine$id)), nrow(spine))
})

test_that("generate_spine birth_year is in plausible range", {
  spine <- generate_spine(n = 1000L, seed = 1L)

  expect_true(all(spine$birth_year >= 1900))
  expect_true(all(spine$birth_year <= 2025))
})

test_that("generate_spine task scores are bounded [0, 1]", {
  spine <- generate_spine(n = 500L, seed = 1L)

  task_dims <- c("task_physical", "task_cognitive", "task_vision",
                 "task_hearing", "task_manual_dexterity", "task_communication")
  for (dim in task_dims) {
    vals <- spine[[dim]]
    expect_true(all(vals >= 0 & vals <= 1),
                info = paste(dim, "has values outside [0,1]"))
  }
})

test_that("generate_spine saves to run dir _system/base-spine.parquet", {
  tmp <- file.path(tempdir(), "fplida_test_output")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 100L, seed = 7L, output_dir = tmp)

  expected_path <- file.path(tmp, "fplida_1k", "_system", "base-spine.parquet")
  expect_true(file.exists(expected_path))

  reloaded <- as.data.frame(arrow::read_parquet(expected_path))
  expect_equal(nrow(reloaded), nrow(spine))
  expect_equal(ncol(reloaded), ncol(spine))
})

test_that("generate_spine writes metadata.md with N, seed, timestamp", {
  tmp <- file.path(tempdir(), "fplida_test_meta")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 100L, seed = 7L, output_dir = tmp)

  meta_path <- file.path(tmp, "fplida_1k", "_system", "metadata.md")
  expect_true(file.exists(meta_path))

  lines <- readLines(meta_path)
  expect_true(any(grepl("N.*100", lines)))
  expect_true(any(grepl("Seed.*7", lines)))
  expect_true(any(grepl("Created", lines)))
})

test_that("generate_spine suffix creates separate run dir", {
  tmp <- file.path(tempdir(), "fplida_test_suffix")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 100L, seed = 1L, output_dir = tmp)
  generate_spine(n = 100L, seed = 2L, output_dir = tmp, suffix = "alt")

  expect_true(dir.exists(file.path(tmp, "fplida_1k")))
  expect_true(dir.exists(file.path(tmp, "fplida_1k_alt")))
})

test_that("generate_spine overwrites same run dir by default", {
  tmp <- file.path(tempdir(), "fplida_test_overwrite")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 100L, seed = 1L, output_dir = tmp)
  generate_spine(n = 100L, seed = 2L, output_dir = tmp)

  # Only one run dir
  dirs <- list.dirs(tmp, recursive = FALSE)
  expect_equal(length(dirs), 1L)

  # Metadata should reflect the second seed
  meta <- readLines(file.path(tmp, "fplida_1k", "_system", "metadata.md"))
  expect_true(any(grepl("Seed.*2", meta)))
})

test_that("generate_spine ANZSCO codes are valid", {
  spine <- generate_spine(n = 500L, seed = 1L)

  codes <- spine$anzsco_code
  expect_true(all(!is.na(codes)))
  # 0 = not employed; otherwise valid 6-digit ANZSCO
  employed <- codes[codes > 0]
  expect_true(all(employed >= 100000 & employed <= 899999),
              info = "Employed codes should be 6-digit ANZSCO")
})

test_that("base spine_id is complete and character", {
  spine <- generate_spine(n = 2000L, seed = 1L)

  expect_type(spine$spine_id, "character")
  expect_false(anyNA(spine$spine_id))
})

test_that("spine_id values follow SP format", {
  spine <- generate_spine(n = 500L, seed = 1L)

  linked <- spine$spine_id[!is.na(spine$spine_id)]
  expect_true(all(grepl("^SP\\d{10}$", linked)))
})

test_that("all 13 agency AEUID columns are present", {
  spine <- generate_spine(n = 100L, seed = 1L)

  agencies <- c("abs", "aihw", "ato", "apsc", "de", "dewr", "dhda",
                "dss", "ha", "ncver", "ndia", "rbdm", "sa")
  for (a in agencies) {
    col <- paste0("aeuid_", a)
    expect_true(col %in% names(spine), info = paste("Missing:", col))
  }
})

test_that("agency AEUIDs are unique within each agency", {
  spine <- generate_spine(n = 100000L, seed = 1L)

  agencies <- c("abs", "ato", "dss", "dhda")
  for (a in agencies) {
    col <- paste0("aeuid_", a)
    expect_equal(length(unique(spine[[col]])), nrow(spine),
                 info = paste(a, "AEUIDs not unique"))
    expect_true(all(grepl("^[0-9A-F]{12}$", spine[[col]])),
                info = paste(a, "AEUIDs should be 12-character hex"))
  }
})

test_that("agency AEUIDs differ across agencies for the same person", {
  spine <- generate_spine(n = 100L, seed = 1L)

  expect_false(identical(spine$aeuid_abs, spine$aeuid_ato))
  expect_false(identical(spine$aeuid_abs, spine$aeuid_dss))
})

test_that("baseline_income is positive for employed, zero for others", {
  spine <- generate_spine(n = 2000L, seed = 1L)

  employed <- spine$baseline_employed
  expect_true(all(spine$baseline_income[employed] > 0))
  expect_true(all(spine$baseline_income[!employed] == 0))
})

test_that("baseline_income has plausible median for FT workers", {
  spine <- generate_spine(n = 5000L, seed = 42L)

  ft_employed <- spine$baseline_employed & spine$baseline_hours == 38L
  med <- median(spine$baseline_income[ft_employed])
  # Target median FT earnings ~$60-70K
  expect_gt(med, 40000)
  expect_lt(med, 100000)
})

test_that("generate_spine errors without output_dir or data_path", {
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

  expect_error(generate_spine(n = 10L, seed = 1L), "No output directory")
})

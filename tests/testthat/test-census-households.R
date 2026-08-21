# A dwelling and a family belong to a household, and a household spans build
# slices. These tests guard both halves: that the identifiers survive slicing,
# and that a person's role is derived from the whole household rather than the
# part one slice happens to hold.

.census_household_build <- function(n = 20000L, seed = 5L) {
  tmp <- tempfile("fplida_cenhh_")
  dir.create(tmp)
  result <- build_fplida(n = n, seed = seed, products = c("spine", "census"),
                         k_slices = 2L, output_dir = tmp,
                         complete_dil_schema = FALSE,
                         export_base_file = TRUE)
  run_dir <- result$canonical_run_dir
  read_product <- function(stem) {
    dir <- file.path(run_dir, "abs-census", stem)
    files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
    as.data.frame(arrow::open_dataset(files, unify_schemas = TRUE))
  }
  list(
    tmp = tmp,
    run_dir = run_dir,
    person = read_product(census_product_name("person")),
    family = read_product(census_product_name("family")),
    dwelling = read_product(census_product_name("dwelling")),
    spine = as.data.frame(arrow::read_parquet(
      file.path(run_dir, "_system", "base-spine.parquet")))
  )
}

.census_household_merge <- function(td) {
  merge(td$person[, c("SYNTHETIC_AEUID", "DWELLING_ID", "FAMILY_ID", "RLHP",
                      "FPIP", "SPIP")],
        td$spine[, c("aeuid_abs", "household_id", "birth_year", "sex")],
        by.x = "SYNTHETIC_AEUID", by.y = "aeuid_abs")
}


test_that("a dwelling identifier means one household in every slice", {
  skip_if_not_installed("arrow")
  td <- .census_household_build()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)
  merged <- .census_household_merge(td)

  # Numbering dwellings from one inside each slice made `D0000000001` mean a
  # different household in every slice, so a join on it merged unrelated
  # people, and it split the households that straddled a boundary.
  expect_true(all(tapply(merged$DWELLING_ID, merged$household_id,
                         function(x) length(unique(x))) == 1L))
  expect_true(all(tapply(merged$household_id, merged$DWELLING_ID,
                         function(x) length(unique(x))) == 1L))
})

test_that("the dwelling and family tables hold one row per household", {
  skip_if_not_installed("arrow")
  td <- .census_household_build()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  expect_false(any(duplicated(td$dwelling$DWELLING_ID)))
  expect_false(any(duplicated(td$family$FAMILY_ID)))
  expect_true(all(td$person$DWELLING_ID %in% td$dwelling$DWELLING_ID))
  expect_true(all(is.na(td$person$FAMILY_ID) |
                    td$person$FAMILY_ID %in% td$family$FAMILY_ID))
})

test_that("relationship in household uses the published category list", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  td <- .census_household_build()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  info <- as.data.frame(variable_info("CENSUS"))
  published <- sub(":.*", "", jsonlite::fromJSON(
    info$valid_values[info$variable == "RLHP"][1]))
  expect_true(all(td$person$RLHP %in% published))
  # A model that only knew couples and children could not describe an adult
  # child at home or a housemate.
  expect_true(any(td$person$RLHP %in% c("41", "51", "61", "72")))
})

test_that("a person's role matches their household's composition", {
  skip_if_not_installed("arrow")
  td <- .census_household_build()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)
  merged <- .census_household_merge(td)
  age <- 2021L - merged$birth_year

  # A child in a family is a natural or adopted child; the few in a household
  # with no adult are unrelated children, or a lone person if alone there.
  expect_true(all(merged$RLHP[age < 15L] %in% c("31", "36", "73")))
  expect_gt(mean(merged$RLHP[age < 15L] == "31"), 0.9)

  lone <- names(which(table(merged$household_id) == 1L))
  adult_alone <- merged$household_id %in% lone & age >= 18L
  expect_true(all(merged$RLHP[adult_alone] == "73"))
})

test_that("every household has one reference person and at most one partner", {
  skip_if_not_installed("arrow")
  td <- .census_household_build()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)
  person <- td$person

  expect_true(all(person$FPIP %in% c("1", "2", "@", "V")))
  expect_true(all(person$SPIP %in% c("1", "2", "@", "V")))
  expect_true(all(tapply(person$SPIP, person$DWELLING_ID,
                         function(x) sum(x == "1")) == 1L))
  expect_true(all(tapply(person$SPIP, person$DWELLING_ID,
                         function(x) sum(x == "2")) <= 1L))
  # A partner is half of a couple, so their relationship code says so.
  expect_true(all(person$RLHP[person$SPIP == "2"] %in%
                    c("11", "15", "17", "18")))
})

test_that("only households with children have a parent indicator", {
  skip_if_not_installed("arrow")
  td <- .census_household_build()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)
  merged <- .census_household_merge(td)
  age <- 2021L - merged$birth_year

  has_parent <- tapply(merged$FPIP, merged$DWELLING_ID,
                       function(x) any(x != "@"))
  has_child <- tapply(age, merged$DWELLING_ID, function(x) any(x < 18L))
  expect_true(all(!has_parent | has_child[names(has_parent)]))
})

test_that("the roles describe the household the Census sees", {
  skip_if_not_installed("arrow")
  spine <- generate_spine(n = 20000L, seed = 5L)
  roles <- census_household_roles(spine, 42L)

  # Counting people who died before Census night leaves a household whose
  # reference person is dead with no reference person at all.
  present <- fplida:::.census_present_on_night(spine)
  living <- census_household_roles(spine[present, , drop = FALSE], 42L)
  expect_true(all(tapply(living$SPIP, living$DWELLING_ID,
                         function(x) sum(x == "1")) == 1L))
  expect_equal(nrow(roles), nrow(spine))
})

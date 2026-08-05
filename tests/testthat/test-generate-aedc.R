aedc_test_spine <- function(n = 3000L, cycle = 2024L) {
  data.frame(
    spine_id = seq_len(n),
    aeuid_de = sprintf("AEDCTEST%08d", seq_len(n)),
    birth_year = rep(cycle - 5L, n),
    sex = rep(c(1L, 2L), length.out = n),
    state = rep(1:8, length.out = n),
    indigenous = rep(c(1L, 1L, 1L, 2L, 1L, 3L, 1L, 4L), length.out = n),
    country_of_birth = rep(c(0L, 0L, 0L, 1L, 0L, 1L), length.out = n),
    stringsAsFactors = FALSE
  )
}

read_aedc_test_product <- function(root, family, cycle = 2024L) {
  path <- list.files(
    root,
    pattern = sprintf("^madipge-aedc-d-%s-%d[.]parquet$", family, cycle),
    recursive = TRUE,
    full.names = TRUE
  )
  testthat::expect_length(path, 1L)
  as.data.frame(arrow::read_parquet(path[[1L]]))
}

prepare_aedc_test_run <- function(root) {
  invisible(generate_spine(n = 20L, seed = 900L, output_dir = root))
}

test_that("AEDC writes an exact source for every product and cycle", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("aedc_products_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  prepare_aedc_test_run(tmp)
  cycles <- c(2009L, 2012L, 2015L, 2018L, 2021L, 2024L)

  result <- generate_aedc(
    spine = aedc_test_spine(400L),
    seed = 91L,
    cycles = cycles,
    output_dir = tmp
  )
  expected <- as.vector(outer(
    c("core", "domain", "indigenous", "language", "specialneeds"),
    cycles,
    function(family, cycle) sprintf("madipge-aedc-d-%s-%d.parquet", family, cycle)
  ))
  actual <- basename(list.files(
    tmp,
    pattern = "^madipge-aedc-d-.*-[0-9]{4}[.]parquet$",
    recursive = TRUE,
    full.names = TRUE
  ))

  expect_setequal(actual, expected)
  expect_identical(result$cycles, cycles)
  expect_setequal(result$products,
                  c("core", "domain", "indigenous", "language", "specialneeds"))
})

test_that("AEDC source separates collection cycle and remoteness fields", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("aedc_source_codes_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  prepare_aedc_test_run(tmp)
  generate_aedc(
    spine = aedc_test_spine(3000L),
    seed = 97L,
    cycles = 2024L,
    output_dir = tmp
  )
  x <- read_aedc_test_product(tmp, "core")

  expect_identical(unique(x$CYCLE), 6L)
  expect_identical(unique(x$YEAR), 2024L)
  expect_true(all(x$REMOTENESSCODE %in% 1:5))
  expect_setequal(unique(x$REMOTENESS), c(
    "Major Cities of Australia", "Inner Regional Australia",
    "Outer Regional Australia", "Remote Australia",
    "Very Remote Australia"
  ))
  expect_identical(
    unname(c(
      "Major Cities of Australia" = 1L,
      "Inner Regional Australia" = 2L,
      "Outer Regional Australia" = 3L,
      "Remote Australia" = 4L,
      "Very Remote Australia" = 5L
    )[x$REMOTENESS]),
    x$REMOTENESSCODE
  )
})

test_that("AEDC not-applicable codes follow the collection vintage", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("aedc_missing_codes_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  prepare_aedc_test_run(tmp)
  generate_aedc(
    spine = aedc_test_spine(6000L, cycle = 2018L),
    seed = 98L,
    cycles = 2018L,
    output_dir = tmp
  )
  historical <- read_aedc_test_product(tmp, "core", cycle = 2018L)
  expect_true(any(historical$A4A == 999L, na.rm = TRUE))
  expect_false(any(historical$A4A == 99L, na.rm = TRUE))
  expect_true(all(historical$B1[!is.na(historical$B1)] %in%
                    c(1L, 2L, 3L, 88L, 999L)))

  generate_aedc(
    spine = aedc_test_spine(6000L),
    seed = 99L,
    cycles = 2024L,
    output_dir = tmp
  )
  current <- read_aedc_test_product(tmp, "core")
  expect_true(any(current$A4A == 99L, na.rm = TRUE))
  expect_false(any(current$A4A == 999L, na.rm = TRUE))
})

test_that("AEDC domain products contain all public questionnaire item families", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("aedc_items_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  prepare_aedc_test_run(tmp)
  generate_aedc(
    spine = aedc_test_spine(),
    seed = 92L,
    cycles = 2024L,
    output_dir = tmp
  )
  x <- read_aedc_test_product(tmp, "domain")

  physical <- c("A2", "A3", "A3A", "A3B", "A4", "A4A", paste0("A", 5:13))
  communication <- c(paste0("B", 1:7), "C24")
  language_cognitive <- paste0("B", 8:33)
  social <- c(paste0("C", 1:12), "C12A", paste0("C", 13:23), "C25")
  emotional <- paste0("C", 26:51)
  background <- c(
    "A1", paste0("A1", LETTERS[1:4]), paste0("B", 34:40),
    "CLASSTYPEA_1", "CLASSTYPEA_2", "CLASSTYPEB", "CLASSTYPEC",
    "CLASSTYPEC_2", "TMSCH", "CANASSESS", "ESL", "LANG", "CANCOM",
    "E2Y", "E2AY", "E2BY", paste0("E3", LETTERS[1:6], "Y"),
    "E4", "E5", "E6", "E7"
  )

  expect_true(all(c(
    physical, communication, language_cognitive, social, emotional, background
  ) %in% names(x)))
  expect_equal(length(unique(c(
    physical, communication, language_cognitive, social, emotional
  ))), 100L)
})

test_that("AEDC questionnaire fields retain official response representations", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("aedc_codes_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  prepare_aedc_test_run(tmp)
  generate_aedc(
    spine = aedc_test_spine(),
    seed = 93L,
    cycles = 2024L,
    output_dir = tmp
  )
  x <- read_aedc_test_product(tmp, "domain")
  values_in <- function(column, allowed) {
    all(x[[column]][!is.na(x[[column]])] %in% allowed)
  }

  for (column in c("A2", "A3", "A4")) {
    expect_true(values_in(column, c(1L, 2L, 88L)), info = column)
  }
  for (column in c(paste0("A", 5:7), paste0("B", 8:40))) {
    expect_true(values_in(column, c(1L, 2L, 88L)), info = column)
  }
  for (column in c(paste0("A", 8:13), paste0("B", 2:7),
                   paste0("C", 1:33), paste0("C", 34:51), "E5", "E6", "E7")) {
    expect_true(values_in(column, c(1L, 2L, 3L, 88L)), info = column)
  }
  expect_true(values_in("B1", c(1L, 2L, 3L, 88L, 99L)))
  expect_true(values_in("A4A", c(0L, 1L, 88L, 99L)))
  expect_true(values_in("E2Y", c(1L, 2L, 88L)))
  for (column in paste0("E3", LETTERS[1:6], "Y")) {
    expect_true(values_in(column, c(1L, 2L, 3L, 4L, 88L)), info = column)
  }

  expect_true(all(is.na(x$A3A[x$A3 != 1L | is.na(x$A3)])))
  expect_true(all(is.na(x$A3B[x$A3 != 1L | is.na(x$A3)])))
  expect_true(all(is.na(x$C12A[x$C12 != 1L | is.na(x$C12)])))
  expect_true(all(is.na(x$CLASSTYPEA_2[x$CLASSTYPEA_1 == 0L])))
  expect_true(all(is.na(x$CLASSTYPEC_2[x$CLASSTYPEC == 0L])))
  expect_true(all(is.na(x$CANCOM[x$LANG == 0L])))
  expect_true(all(is.na(x$E2AY[x$E2Y != 1L | is.na(x$E2Y)])))
  expect_true(all(is.na(x$E2BY[x$E2Y != 1L | is.na(x$E2Y)])))
})

test_that("AEDC preserves early-cycle NSW structural skips", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("aedc_early_filters_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  prepare_aedc_test_run(tmp)
  spine <- aedc_test_spine(2500L, cycle = 2012L)
  spine$state <- rep(c(1L, 2L), length.out = nrow(spine))
  generate_aedc(
    spine = spine,
    seed = 96L,
    cycles = 2012L,
    output_dir = tmp
  )
  x <- read_aedc_test_product(tmp, "domain", cycle = 2012L)
  nsw <- x$STATE == "NSW"
  vic_completed <- x$STATE == "VIC" & x$TMSCH != 1L

  expect_true(all(is.na(x$A3A[nsw])))
  expect_true(all(is.na(x$A3B[nsw])))
  expect_true(all(is.na(x$C12A[nsw])))
  expect_false(any(is.na(x$A3A[vic_completed])))
  expect_false(any(is.na(x$A3B[vic_completed])))
})

test_that("AEDC public validity and category derivations are coherent", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("aedc_derived_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  prepare_aedc_test_run(tmp)
  generate_aedc(
    spine = aedc_test_spine(),
    seed = 94L,
    cycles = 2024L,
    output_dir = tmp
  )
  x <- read_aedc_test_product(tmp, "core")

  categories <- c(
    "PHYSCATEGORY", "SOCCATEGORY", "EMOTCATEGORY",
    "LANGCOGCATEGORY", "COMGENCATEGORY"
  )
  valid <- c("PHYSVALID", "SOCVALID", "EMOTVALID", "LANGCOGVALID", "COMGENVALID")
  scores <- c("PHYS", "SOC", "EMOT", "LANGCOG", "COMGEN")
  for (index in seq_along(categories)) {
    expect_true(all(x[[categories[[index]]]][!is.na(x[[categories[[index]]]])] %in% 1:4))
    expect_true(all(is.na(x[[categories[[index]]]][x[[valid[[index]]]] == 0L])))
    expect_true(all(x[[scores[[index]]]][x[[valid[[index]]]] == 1L] >= 0 &
                    x[[scores[[index]]]][x[[valid[[index]]]] == 1L] <= 10))
  }
  expect_equal(x$VALIDDOMAINS, rowSums(x[valid]))
  expect_equal(x$VALIDINSTRUMENT, as.integer(x$VALIDDOMAINS >= 4L))

  complete <- x$VALIDDOMAINS == 5L
  category_matrix <- as.matrix(x[complete, categories, drop = FALSE])
  expect_equal(x$DV1[complete], as.integer(rowSums(category_matrix == 1L) >= 1L))
  expect_equal(x$DV2[complete], as.integer(rowSums(category_matrix == 1L) >= 2L))
  expect_equal(x$OT5[complete], as.integer(rowSums(category_matrix >= 3L) == 5L))
  expect_true(all(is.na(x$DV1[!complete])))
  expect_true(all(is.na(x$DV2[!complete])))
  expect_true(all(is.na(x$OT5[!complete])))

  special_needs <- x$SPECIALNEEDS == 1L
  if (any(special_needs)) {
    expect_true(all(as.matrix(x[special_needs, valid]) == 0L))
    expect_true(all(is.na(as.matrix(x[special_needs, categories]))))
  }
})

test_that("AEDC latent domains create correlated item responses", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("aedc_traits_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  prepare_aedc_test_run(tmp)
  generate_aedc(
    spine = aedc_test_spine(5000L),
    seed = 95L,
    cycles = 2024L,
    output_dir = tmp
  )
  x <- read_aedc_test_product(tmp, "domain")
  item_mean <- function(columns) {
    item_matrix <- as.matrix(x[columns])
    item_matrix[item_matrix %in% c(88L, 99L)] <- NA_integer_
    rowMeans(item_matrix, na.rm = TRUE)
  }

  expect_gt(cor(x$PHYS, item_mean(c("A2", "A3", "A4", paste0("A", 5:13))),
                use = "complete.obs"), 0.35)
  expect_gt(cor(x$LANGCOG, item_mean(paste0("B", 8:33)),
                use = "complete.obs"), 0.35)
  expect_gt(cor(x$SOC, item_mean(c(paste0("C", 1:23), "C25")),
                use = "complete.obs"), 0.35)
  expect_gt(cor(x$EMOT, item_mean(paste0("C", 26:51)),
                use = "complete.obs"), 0.35)
  expect_gt(cor(x$COMGEN, item_mean(c(paste0("B", 1:7), "C24")),
                use = "complete.obs"), 0.35)
})

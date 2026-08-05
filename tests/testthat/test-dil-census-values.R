census_decision_ledger <- function() {
  path <- system.file(
    "internal-docs", "admin-value-decision-ledger-census.csv",
    package = "fplida"
  )
  if (!nzchar(path)) {
    path <- file.path(
      "..", "..", "inst", "internal-docs",
      "admin-value-decision-ledger-census.csv"
    )
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

census_gap_register <- function() {
  path <- system.file(
    "internal-docs", "admin-value-gap-register.csv", package = "fplida"
  )
  if (!nzchar(path)) {
    path <- file.path(
      "..", "..", "inst", "internal-docs", "admin-value-gap-register.csv"
    )
  }
  values <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )
  values[values$dataset == "CENSUS", , drop = FALSE]
}

test_that("Census decision ledger classifies the exact 413-gap baseline", {
  ledger <- census_decision_ledger()

  expect_equal(nrow(ledger), 225L)
  expect_equal(sum(ledger$occurrence_count), 413L)
  expect_equal(anyDuplicated(paste(ledger$dataset, ledger$variable)), 0L)
  expect_true(all(ledger$dataset == "CENSUS"))
  expect_true(all(ledger$status %in% c("populated", "typed_missing")))
  expect_equal(sum(ledger$status == "populated"), 223L)
  expect_identical(
    ledger$variable[ledger$status == "typed_missing"],
    c("EMPCD", "EMPCP")
  )

  required_text <- c(
    "determination", "rule", "evidence_source", "caveat"
  )
  expect_true(all(vapply(ledger[required_text], function(value) {
    all(!is.na(value) & nzchar(trimws(value)))
  }, logical(1))))
})

test_that("Census official registries retain product-specific code encodings", {
  registry <- fplida:::.dil_census_codeframe_registry()
  geographies <- fplida:::.dil_census_geography_registry()

  expect_equal(nrow(registry$mapping), 508L)
  expect_gt(nrow(registry$values), 16000L)
  expect_gt(nrow(geographies), 150000L)
  expect_equal(anyDuplicated(paste(
    registry$mapping$year, registry$mapping$variable
  )), 0L)
  expect_equal(anyDuplicated(paste(
    registry$values$year, registry$values$variable,
    registry$values$code
  )), 0L)

  expect_identical(
    fplida:::.dil_census_field_frame(2011L, "INGP")$code,
    as.character(1:4)
  )
  expect_true(all(c("97", "99") %in%
                    fplida:::.dil_census_field_frame(2016L, "INGP")$code))
  expect_true(all(c("&", "V") %in%
                    fplida:::.dil_census_field_frame(2021L, "INGP")$code))
  expect_equal(fplida:::.dil_census_data_length(2021L, "EETP"), 2L)
  expect_equal(fplida:::.dil_census_data_length(2021L, "FMCF"), 4L)
  expect_equal(fplida:::.dil_census_data_length(2021L, "ANC1P"), 4L)
  expect_equal(fplida:::.dil_census_data_length(2021L, "HEAP"), 3L)
  expect_equal(fplida:::.dil_census_data_length(2021L, "INDP"), 4L)
})

test_that("Census historical geography and movement codes use official domains", {
  lookup <- fplida:::.load_mb_lookup()
  n <- 120L
  spine <- data.frame(
    id = seq_len(n),
    spine_id = sprintf("S%010d", seq_len(n)),
    birth_year = rep(1970L, n),
    sex = rep(1:2, length.out = n),
    state = rep(1:8, length.out = n),
    sa2_code = lookup$sa2_code[seq_len(n)],
    sa3_code = as.integer(substr(lookup$sa2_code[seq_len(n)], 1L, 5L)),
    sa4_code = lookup$sa4_code[seq_len(n)],
    baseline_employed = rep(c(1L, 0L), length.out = n),
    baseline_hours = rep(c(38L, 0L), length.out = n),
    baseline_income = 65000,
    industry = rep(1:19, length.out = n),
    country_of_birth_sacc = 1101L,
    year_of_arrival = 1980L,
    stringsAsFactors = FALSE
  )
  spine$birth_year[[1L]] <- 2016L
  spine$birth_year[[2L]] <- 2013L

  value <- function(variable, table = "census_2016_person") {
    fplida:::.dil_census_value(variable, spine, 20260803L, 17L, table)
  }
  geography <- fplida:::.dil_census_geography_registry()

  sa1 <- value("SA1UCP")
  sa2 <- value("SA2UCP")
  sa3 <- value("SA3UCP")
  sa4 <- value("SA4UCP")
  expect_true(all(sa1 %in% geography$code[
    geography$year == 2016L & geography$layer == "SA1"
  ]))
  expect_true(all(sa2 %in% geography$code[
    geography$year == 2016L & geography$layer == "SA2"
  ]))
  expect_true(all(sa3 %in% geography$code[
    geography$year == 2016L & geography$layer == "SA3"
  ]))
  expect_true(all(sa4 %in% geography$code[
    geography$year == 2016L & geography$layer == "SA4"
  ]))

  expect_equal(value("SA1U1P")[[1L]], "9999999")
  expect_equal(value("SA2U5P")[[2L]], "999999999")
  expect_true(all(grepl("^(?:RA[1-8][0-9]|RA99)$", value("RAP"))))

  pur1 <- value("PUR1P")
  pur5 <- value("PUR5P")
  expect_equal(pur1[[1L]], "9999999998")
  expect_equal(pur5[[1L]], "9999999998")
  expect_true(all(grepl("^(?:[1-9][0-9]{8}|[0-9]{10})$", pur1)))
  expect_true(all(grepl("^(?:[1-9][0-9]{8}|[0-9]{10})$", pur5)))
})

test_that("Census industry and housing rules are scoped and non-placeholder", {
  spine <- generate_spine(n = 400L, seed = 20260803L)
  adult <- 2021L - spine$birth_year >= 15L
  employed <- spine$baseline_employed == 1L & adult

  value <- function(variable, year) {
    fplida:::.dil_census_value(
      variable, spine, 20260803L, 29L,
      paste0("census_", year, "_person")
    )
  }

  ind11 <- value("INDP", 2011L)
  ind16 <- value("INDP", 2016L)
  ind21 <- value("INDP", 2021L)
  expect_true(all(ind11[employed] %in%
                    fplida:::.dil_census_field_frame(2011L, "INDP")$code))
  expect_true(all(ind16[employed] %in%
                    fplida:::.dil_census_field_frame(2016L, "INDP")$code))
  expect_true(all(grepl("^[0-9]{4}$", ind21[employed])))
  expect_true(all(ind11[!employed] == "69"))
  expect_true(all(ind16[!employed] == "99998"))
  expect_true(all(ind21[!employed] == "@@@@"))

  housing <- value("HOSD", 2021L)
  expect_true(all(housing %in% sprintf("%02d", 1:9)))
  expect_gt(length(unique(housing)), 3L)
})

test_that("the 413 original Census occurrences resolve except reviewed EMPC fields", {
  gap <- census_gap_register()
  spine <- generate_spine(n = 80L, seed = 20260803L)

  result <- lapply(seq_len(nrow(gap)), function(i) {
    row <- gap[i, , drop = FALSE]
    value <- fplida:::.dil_general_value(
      name = row$variable,
      description = ifelse(
        is.na(row$official_description), "", row$official_description
      ),
      dataset = row$dataset,
      product_name = row$official_product,
      table_name = row$official_table,
      module_name = "",
      spine_rows = spine,
      seed = 20260803L
    )
    data.frame(
      variable = row$variable,
      all_missing = all(is.na(value)),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, result)

  expect_equal(nrow(result), 413L)
  expect_equal(sum(!result$all_missing), 411L)
  expect_identical(
    sort(result$variable[result$all_missing]),
    c("EMPCD", "EMPCP")
  )
})

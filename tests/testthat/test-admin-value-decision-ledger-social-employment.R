social_employment_decision_ledger <- function() {
  path <- system.file(
    "internal-docs",
    "admin-value-decision-ledger-social-employment.csv",
    package = "fplida"
  )
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "../../inst/internal-docs/",
      "admin-value-decision-ledger-social-employment.csv"
    )
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

social_employment_remediation_register <- function() {
  path <- system.file(
    "internal-docs", "admin-value-remediation-register.csv",
    package = "fplida"
  )
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "../../inst/internal-docs/admin-value-remediation-register.csv"
    )
  }
  values <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )
  values[
    values$dataset %in% c("DEX", "DOMINO", "SAE", "RPS"),
    c("dataset", "variable", "occurrence_count")
  ]
}

test_that("social-employment decision ledger matches the remediation scope", {
  ledger <- social_employment_decision_ledger()
  register <- social_employment_remediation_register()

  expect_identical(names(ledger), c(
    "dataset", "variable", "occurrence_count", "status",
    "implemented_domain_or_derivation", "evidence_source", "caveat"
  ))
  expect_equal(nrow(ledger), 176L)
  expect_equal(nrow(unique(ledger[c("dataset", "variable")])), 176L)
  expect_equal(sum(ledger$occurrence_count), 534L)
  expect_setequal(
    ledger$status,
    c("populated", "structural", "unsupported-codeframe")
  )
  expect_false(any(vapply(
    ledger,
    function(value) any(is.na(value) | !nzchar(as.character(value))),
    logical(1)
  )))

  joined <- merge(
    register, ledger,
    by = c("dataset", "variable"), all = TRUE,
    suffixes = c(".register", ".ledger")
  )
  expect_equal(nrow(joined), 176L)
  expect_false(any(is.na(joined$status)))
  expect_identical(
    joined$occurrence_count.register,
    joined$occurrence_count.ledger
  )
})

test_that("ledger status counts record the evidence boundary", {
  ledger <- social_employment_decision_ledger()

  expect_identical(
    as.integer(table(factor(
      ledger$status,
      levels = c("populated", "structural", "unsupported-codeframe")
    ))),
    c(58L, 42L, 76L)
  )
  expect_identical(
    as.integer(tapply(
      ledger$occurrence_count,
      factor(
        ledger$status,
        levels = c("populated", "structural", "unsupported-codeframe")
      ),
      sum
    )),
    c(122L, 94L, 318L)
  )
})

test_that("locally invented categorical domains remain unsupported", {
  ledger <- social_employment_decision_ledger()
  unsupported_rows <- ledger$status == "unsupported-codeframe"
  expect_true(all(grepl(
    "^Exact source column; otherwise NA_character_\\.",
    ledger$implemented_domain_or_derivation[unsupported_rows]
  )))
  unsupported <- function(dataset) {
    sort(ledger$variable[
      ledger$dataset == dataset &
        ledger$status == "unsupported-codeframe"
    ])
  }

  expect_setequal(unsupported("DEX"), c(
    "ADDRESSSTATUSCODE", "CLIENTADDSOURCEVALIDCOD", "NAME", "PRIORITYGRP"
  ))

  domino_supported_or_structural <- c(
    "AMR", "ASSMT_ID", "BIRTH_CTRY_CODE", "CED", "CHILD_ID", "CNTRY_CDE",
    "CTRY_CODE", "DURN_DAYS", "EMP_INC_CONT_HRS", "EMP_INC_VAR_HRS",
    "EMPLYR_ID", "EUEA_ID",
    "EUED_ID", "EXT_DOM_REL_ID", "INCAP_WK_WRK_HRS", "INDIG_CODE",
    "LANG_CODE", "LGA", "OBJECT_TYPE_CODE", "THP_ID"
  )
  domino_variables <- ledger$variable[ledger$dataset == "DOMINO"]
  expect_setequal(
    unsupported("DOMINO"),
    setdiff(domino_variables, domino_supported_or_structural)
  )

  expect_setequal(
    unsupported("SAE"),
    setdiff(ledger$variable[ledger$dataset == "SAE"], "EXTRACT_REF")
  )
  expect_setequal(
    unsupported("RPS"),
    c("GEOCODED_INDEX", "ROLE_CD", "ROLE_CD_UPDATED")
  )
})

aedc_decision_ledger <- function() {
  path <- system.file(
    "internal-docs", "admin-value-decision-ledger-aedc.csv",
    package = "fplida"
  )
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "../../inst/internal-docs/admin-value-decision-ledger-aedc.csv"
    )
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

aedc_remediation_register <- function() {
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
    values$dataset == "AEDC",
    c("dataset", "variable", "occurrence_count")
  ]
}

test_that("AEDC ledger exhausts the remediation register", {
  ledger <- aedc_decision_ledger()
  register <- aedc_remediation_register()
  required <- c(
    "dataset", "variable", "status", "determination", "rule",
    "evidence_source", "caveat"
  )

  expect_true(all(required %in% names(ledger)))
  expect_true(all(c(
    "occurrence_count", "implemented_domain_or_derivation"
  ) %in% names(ledger)))
  expect_equal(nrow(ledger), 394L)
  expect_equal(nrow(unique(ledger[c("dataset", "variable")])), 394L)
  expect_equal(sum(ledger$occurrence_count), 1910L)
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
  expect_equal(nrow(joined), 394L)
  expect_false(any(is.na(joined$status)))
  expect_identical(
    joined$occurrence_count.register,
    joined$occurrence_count.ledger
  )
})

test_that("AEDC ledger records the implemented evidence boundary", {
  ledger <- aedc_decision_ledger()
  levels <- c("populated", "structural", "unsupported-codeframe")

  expect_identical(
    as.integer(table(factor(ledger$status, levels = levels))),
    c(287L, 11L, 96L)
  )
  expect_identical(
    as.integer(tapply(
      ledger$occurrence_count,
      factor(ledger$status, levels = levels),
      sum
    )),
    c(1364L, 74L, 472L)
  )
  unsupported <- ledger$status == "unsupported-codeframe"
  expect_setequal(
    ledger$variable[unsupported],
    .dil_aedc_blocked_names
  )
  expect_true(all(grepl(
    "^Typed NA_character_[.]",
    ledger$rule[unsupported]
  )))
})

test_that("AEDC ledger exposes corrected source and fallback decisions", {
  ledger <- aedc_decision_ledger()
  row <- function(name) ledger[ledger$variable == name, , drop = FALSE]

  expect_identical(row("CYCLE")$status, "structural")
  expect_match(row("CYCLE")$rule, "cycle codes 1 to 6")
  expect_match(row("CYCLE")$rule, "YEAR keeps the year")
  expect_identical(row("REMOTENESS")$status, "populated")
  expect_match(row("REMOTENESS")$rule, "official ASGS remoteness names")
  expect_identical(row("REMOTENESSCODE")$status, "populated")
  expect_match(row("MOC")$rule, "official values 4 to 9")

  for (name in c(
    "AGECAT", "DIAGNOSIS7", "SEIFAEXCLUDED"
  )) {
    expect_identical(row(name)$status, "populated", info = name)
    expect_match(row(name)$evidence_source, "R/dil_aedc_values[.]R",
                 info = name)
  }
  for (name in c("MSI", "MSICATEGORY", "MSIVALID")) {
    expect_identical(
      row(name)$determination,
      "typed-missing-proprietary-score",
      info = name
    )
  }
  expect_match(row("B1A")$caveat, "through 2018 use 999")
  expect_match(row("B1")$caveat, "through 2018 use 999")
})

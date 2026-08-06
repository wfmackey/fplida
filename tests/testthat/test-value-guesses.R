test_that("guessed is a distinct, well-formed status", {
  info <- variable_info()
  guessed <- info[info$value_support_status == "guessed", , drop = FALSE]

  expect_gt(nrow(guessed), 0L)
  # Every guess says so in `limitation`, which is the one field guaranteed to
  # carry the warning.
  #
  # `value_source` cannot carry it. A guess reached by research often cites a
  # real document that establishes the SHAPE without confirming the mapping —
  # a departmental guideline naming the categories while the codes behind them
  # stay unpublished. Naming that document is more useful than hiding it, and
  # the status plus the limitation are what mark the claim as inferred.
  expect_true(all(grepl("inferred from the variable name", guessed$limitation,
                        fixed = TRUE)))
  expect_true(all(nzchar(guessed$value_definition)))
  expect_true(all(nzchar(guessed$value_source)))
  # No guess may claim the source confirms it, however it was reached.
  expect_false(any(grepl(
    "The source publishes this value domain", guessed$value_definition,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "The source defines the value domain", guessed$limitation, fixed = TRUE
  )))
})

test_that("guessing never overwrites a sourced or unsupported determination", {
  info <- variable_info()

  # `unsupported` now means research looked and found nothing defensible,
  # rather than nobody having asked. Three variables reach that bar: two AEDC
  # school-administration units with no published national list, and an ATO
  # code box that does not exist on the only return year the field covers.
  #
  # A name rule must never overturn one of those: a guess from the shape of a
  # name is a weaker claim than a finding that the domain is unknowable.
  unsupported <- info[info$value_support_status == "unsupported", ,
                      drop = FALSE]
  expect_setequal(
    unique(paste(unsupported$dataset, toupper(unsupported$variable))),
    c("AEDC SCHOOLCLUSTER", "AEDC SCHOOLREGION",
      "PIT_ITR UNPLMT_SKNS_BNFT_ACTN_CD")
  )
  expect_true(all(trimws(unsupported$valid_values) == "[]"))

  # Sourced variables keep a real provenance, never the guess sentinel.
  sourced <- info[info$value_support_status == "sourced", , drop = FALSE]
  expect_false(any(sourced$value_source == "Inferred from the variable name"))
  expect_false(any(grepl("(inferred)", sourced$value_domain, fixed = TRUE)))
})

test_that("the screenshot cases resolve to their inferred domains", {
  info <- variable_info()

  state <- unique(info$value_domain[toupper(info$variable) == "CAMPUS_STATE"])
  expect_identical(state, "Australian state or territory (inferred)")

  postcode <- unique(
    info$value_domain[toupper(info$variable) == "CAMPUS_POSTCODE"]
  )
  expect_identical(postcode, "Australian postcode (inferred)")
})

test_that("rule order keeps specific patterns ahead of general ones", {
  info <- variable_info()
  domain_of <- function(v) {
    d <- unique(info$value_domain[toupper(info$variable) == v])
    if (length(d) != 1L) NA_character_ else d
  }

  # A postcode must not be captured by the generic code rule, nor a country
  # identifier by the generic identifier rule.
  expect_identical(domain_of("CAMPUS_POSTCODE"),
                   "Australian postcode (inferred)")
  expect_identical(domain_of("DEL_LOC_COUNTRY_ID"),
                   "SACC country code (inferred)")

  # AMEP panel columns end _YYYY_Q1..Q4 but hold hours attended, not a
  # quarter number. Regressing this silently mislabels 232 columns.
  expect_identical(domain_of("AMEPDL_2017_Q4"),
                   "Hours attended in the quarter (inferred)")
})

test_that("survey occurrences may be guessed but never sourced", {
  info <- variable_info()
  survey <- info[info$collection_type == "survey", , drop = FALSE]

  expect_true(all(
    survey$value_support_status %in% c("not_applicable", "guessed")
  ))
  expect_gt(sum(survey$value_support_status == "guessed"), 0L)
})

test_that("generation draws from the registry's documented codes", {
  skip_if_not_installed("jsonlite")

  # A documented domain reaches the generator, rather than the generator's own
  # name heuristics inventing something of the right shape but wrong content.
  codes <- fplida:::.registry_values_for("HE", "CAMPUS_STATE")
  expect_setequal(codes, as.character(1:9))

  drawn <- fplida:::.registry_value_column("HE", "CAMPUS_STATE", 200L, 42L)
  expect_length(drawn, 200L)
  expect_true(all(drawn %in% codes))

  # Deterministic in (seed, salt), like the rest of the generator.
  expect_identical(
    drawn,
    fplida:::.registry_value_column("HE", "CAMPUS_STATE", 200L, 42L)
  )

  # An open domain carries no code list, so the caller falls through.
  expect_null(fplida:::.registry_value_column("HE", "CAMPUS_POSTCODE", 10L, 1L))
})

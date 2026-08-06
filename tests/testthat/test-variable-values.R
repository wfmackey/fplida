test_that("variable_values returns one row per code", {
  skip_if_not_installed("jsonlite")

  codes <- variable_values("mbs", "billtypecd")
  expect_identical(
    names(codes),
    c("dataset", "variable", "code", "label", "value_domain", "value_source",
      "value_support_status")
  )
  expect_setequal(codes$code, c("D", "P"))
  expect_true(all(nzchar(codes$label)))
  expect_false(any(codes$label == codes$code))
})

test_that("a code frame is returned once, not once per occurrence", {
  skip_if_not_installed("jsonlite")

  # BILLTYPECD appears in hundreds of MBS tables with the same two codes.
  # Returning them hundreds of times is what makes the raw registry awkward.
  info <- variable_info("mbs")
  occurrences <- sum(toupper(info$variable) == "BILLTYPECD")
  expect_gt(occurrences, 100L)
  expect_identical(nrow(variable_values("mbs", "billtypecd")), 2L)
})

test_that("only variables with a code list come back", {
  skip_if_not_installed("jsonlite")

  all_codes <- variable_values()
  expect_gt(nrow(all_codes), 40000L)
  pairs <- unique(paste(all_codes$dataset, toupper(all_codes$variable)))
  expect_gt(length(pairs), 3000L)

  # Every pair returned really does carry values in the registry, and every
  # pair that carries values is returned.
  info <- variable_info()
  has <- nzchar(trimws(info$valid_values)) & trimws(info$valid_values) != "[]"
  expect_setequal(
    pairs, unique(paste(info$dataset[has], toupper(info$variable[has])))
  )

  # An identifier with an open domain is absent rather than present and empty.
  expect_identical(nrow(variable_values("pit_ps", "arid_hash_trunc")), 0L)
})

test_that("the filters compose", {
  skip_if_not_installed("jsonlite")

  sourced <- variable_values("domino", value_support_status = "sourced")
  expect_gt(nrow(sourced), 0L)
  expect_true(all(sourced$value_support_status == "sourced"))
  expect_true(all(toupper(sourced$dataset) == "DOMINO"))

  # Unknown selections give an empty frame with the right shape, not an error.
  none <- variable_values("mbs", "no_such_variable")
  expect_identical(nrow(none), 0L)
  expect_identical(names(none), names(sourced))
})

test_that("a label containing a colon survives the split", {
  skip_if_not_installed("jsonlite")

  # Entries are "code: label" and labels do contain colons, so splitting on
  # the last one would move part of the label into the code.
  all_codes <- variable_values()
  expect_false(any(grepl(":", all_codes$code, fixed = TRUE)))
  expect_gt(sum(grepl(":", all_codes$label, fixed = TRUE)), 0L)
})

test_that("bundled ASGS 2021 SA2 code-name frame is complete", {
  path <- system.file(
    "extdata", "codeframes", "sa2_2021.tsv", package = "fplida"
  )
  sa2 <- utils::read.delim(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE
  )

  expect_equal(nrow(sa2), 2473L)
  expect_identical(unique(sa2$year), "2021")
  expect_false(anyDuplicated(sa2$code) > 0L)
  expect_true(all(grepl("^(?:[1-9][0-9]{8}|Z{9})$", sa2$code)))
  expect_true(all(nzchar(sa2$name)))
  expect_identical(sa2$name[sa2$code == "503021295"], "East Perth")
  expect_identical(sa2$name[sa2$code == "ZZZZZZZZZ"], "Outside Australia")
  expect_true(all(grepl(
    "^https://geo\\.abs\\.gov\\.au/arcgis/rest/services/ASGS2021/SA2/MapServer$",
    sa2$source_url
  )))

  sampled <- utils::read.delim(
    system.file(
      "extdata", "codeframes", "sa2_lookup.tsv", package = "fplida"
    ),
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE
  )
  expect_true(all(sampled$sa2_code %in% sa2$code))
})

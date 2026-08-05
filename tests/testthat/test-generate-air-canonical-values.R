test_that("AIR canonical aliases and event dates are coherent", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("air_canonical_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 500L, seed = 71L, output_dir = tmp)
  air <- generate_air(
    spine = spine, seed = 71L, output_dir = tmp, return_data = TRUE
  )

  canonical_fields <- c(
    "ANTGNCDE", "VACCSEQ", "EPSSTAT", "SCHEDCDE",
    "CLAIMID", "CLAIMSEQ", "ENCSEQ", "BATCHNUM",
    "CPROVNUM", "PROVNUM", "CIMMPROV", "IMMPROVN",
    "PRACLOCN", "IMMPLOCN", "RPLCD_ID", "REGSTECD",
    paste0("SPINE_V", 4:9, "_ID")
  )
  expect_true(all(canonical_fields %in% names(air)))
  expect_true(all(vapply(
    air[canonical_fields],
    function(value) all(!is.na(value) & nzchar(as.character(value))),
    logical(1)
  )))

  expect_equal(air$ANTGNCDE, air$ANTIGEN_CODE)
  expect_equal(as.integer(air$VACCSEQ), air$VACCINE_SEQUENCE)
  expect_equal(air$EPSSTAT, air$EPISODE_STATUS)
  expect_equal(air$SCHEDCDE, air$SCHEDULE_CLASSIFICATION)
  expect_true(all(air$EPSSTAT == "Valid"))
  expect_true(all(air$SCHEDCDE == "NIP"))

  expect_equal(air$ENCDATE, air$ENCOUNTER_DATE)
  expect_true(all(air$RCPTDATE >= air$ENCDATE))
  expect_true(all(air$EPSPROC >= air$RCPTDATE))

  expect_true(all(grepl("^AIRCLM[0-9A-F]{16}$", air$CLAIMID)))
  expect_equal(anyDuplicated(air$CLAIMID), 0L)
  expect_true(all(air$CLAIMSEQ == "1"))
  expect_equal(as.integer(air$ENCSEQ), air$VACCINE_SEQUENCE)
  expect_true(all(grepl("^AIRB[0-9A-F]{12}$", air$BATCHNUM)))

  expect_equal(air$CPROVNUM, air$PROVNUM)
  expect_equal(air$CPROVNUM, air$CIMMPROV)
  expect_equal(air$CPROVNUM, air$IMMPROVN)
  expect_equal(air$CPROVNUM, air$RPLCD_ID)
  expect_equal(air$PRACLOCN, air$IMMPLOCN)
  expect_true(all(grepl("^AIRP[1-8][0-9]{4}$", air$CPROVNUM)))
  expect_true(all(grepl("^AIRL[1-8][0-9]{4}$", air$PRACLOCN)))
  expect_lt(length(unique(air$CPROVNUM)), nrow(air))
  expect_equal(as.integer(air$REGSTECD), air$STATE_ASGS_2021)
  expect_equal(
    as.integer(substr(air$CPROVNUM, 5L, 5L)),
    air$STATE_ASGS_2021
  )

  expected_spine <- spine$spine_id[
    match(air$SYNTHETIC_AEUID, spine$aeuid_dhda)
  ]
  expect_false(anyNA(expected_spine))
  for (field in paste0("SPINE_V", 4:9, "_ID")) {
    expect_equal(air[[field]], expected_spine, info = field)
  }
})

test_that("AIR synthetic identifiers are stable for a fixed input and seed", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("air_stable_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 250L, seed = 72L, output_dir = tmp)
  first <- generate_air(
    spine = spine, seed = 72L, output_dir = tmp, return_data = TRUE
  )
  second <- generate_air(
    spine = spine, seed = 72L, output_dir = tmp, return_data = TRUE
  )

  stable_fields <- c(
    "CLAIMID", "CLAIMSEQ", "ENCSEQ", "BATCHNUM",
    "CPROVNUM", "PROVNUM", "PRACLOCN", "RPLCD_ID",
    "RCPTDATE", "EPSPROC", paste0("SPINE_V", 4:9, "_ID")
  )
  expect_equal(first[stable_fields], second[stable_fields])
})

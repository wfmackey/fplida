test_that("historical EEH windows use their own period and wage prices", {
  skip_if_not_installed("arrow")
  tmp <- tempfile("blade-eeh-window-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  generate_spine(n = 400L, seed = 47L, output_dir = tmp,
                 use_template = FALSE)

  historical <- generate_blade(tables = 17L, years = 2018L,
    seed = 47L, output_dir = tmp, return_data = TRUE)[[1L]]
  latest <- generate_blade(tables = 17L, years = 2023L,
    seed = 47L, output_dir = tmp, return_data = TRUE)[[1L]]
  expect_gt(nrow(historical), 0L)
  expect_equal(unique(historical$tsid), "18")
  expect_equal(unique(latest$tsid), "23")
  expect_equal(historical$eid_eeh, latest$eid_eeh)
  expect_lt(sum(historical$swte_eeh), sum(latest$swte_eeh))

  # The person link is the independent anchor-year wage ledger. FY2017-18
  # prices use start year 2017, even though the requested ending year is 2018.
  run_dir <- fplida:::resolve_run_dir(tmp)
  link <- fplida:::.blade_employee_link_rows(
    fplida:::.load_blade_plida_link(run_dir)
  )
  ids <- fplida:::.blade_deidentified_id("P",
    paste(link$SYNTHETIC_AEUID, link$BN, link$job_number, sep = "|"),
    seed = 48L, width = 14L)
  wage <- link$annual_wage * fplida:::.nominal_unit_factor(
    "wage", 2017L,
    unit = fplida:::.nominal_unit_key(link$SYNTHETIC_AEUID), seed = 48L,
    dispersion = fplida:::.NOMINAL_PERSON_DISPERSION, basis = "financial"
  )
  index <- match(historical$eid_eeh, ids)
  expect_false(anyNA(index))
  expect_equal(historical$swte_eeh, round(wage[index] / 52, 2))
})

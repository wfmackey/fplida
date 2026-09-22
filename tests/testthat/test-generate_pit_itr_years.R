pit_itr_year_fixture <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      options(fplida.run_dir = cache$run)
      return(cache)
    }
    out <- tempfile("pit_itr_years_")
    dir.create(out)
    spine <- generate_spine(n = 1500L, seed = 47L, output_dir = out)
    run <- getOption("fplida.run_dir")
    result <- suppressMessages(generate_pit_itr(
      spine = spine, seed = 47L, years = 2024:2025, output_dir = out
    ))
    cache <<- list(out = out, run = run, spine = spine, result = result)
    cache
  }
})

pit_itr_year_path <- function(fixture, year, type) {
  file.path(fixture$run, "ato-pit_itr",
            paste0(fplida:::itr_product_name(year, type), ".parquet"))
}

pit_itr_year_read <- function(fixture, year, type) {
  as.data.frame(arrow::read_parquet(pit_itr_year_path(fixture, year, type)))
}

test_that("ITR writes FY2023-24 without inventing a payment-summary product", {
  skip_if_not_installed("arrow")
  fixture <- pit_itr_year_fixture()
  expect_identical(fixture$result$years, 2024L)
  expect_gt(fixture$result$n_filers, 0L)
  expect_false(dir.exists(file.path(fixture$run, "ato-pit_ps")))
  for (type in c("context", "inc-loss", "ded-exp-off", "whld-debt")) {
    rows <- pit_itr_year_read(fixture, 2024L, type)
    expect_equal(nrow(rows), fixture$result$n_filers)
    expect_identical(unique(rows$INCM_YR), 2024L)
    expect_false(anyDuplicated(rows$SYNTHETIC_AEUID) > 0L)
  }
  expect_false(any(grepl("2425", list.files(file.path(fixture$run, "ato-pit_itr")))))
})

test_that("ITR-only years share wages, jobs and occupations with the labour panel", {
  skip_if_not_installed("arrow")
  fixture <- pit_itr_year_fixture()
  spine <- fplida:::filter_ato_records(fixture$spine, reference_year = 2024L)
  # The independent public panel takes a wider window; the 2024 endpoint
  # must agree with the fallback's one-year output from the same trajectory.
  panel <- generate_employment_panel(spine, seed = 47L, years = 2010:2025)
  panel <- panel[panel$year == 2024L, ]
  wages <- aggregate(gross_annual ~ aeuid_ato, panel, sum)
  income <- pit_itr_year_read(fixture, 2024L, "inc-loss")
  index <- match(income$SYNTHETIC_AEUID, wages$aeuid_ato)
  expect_false(anyNA(index))
  expected <- wages$gross_annual[index]
  observed <- income$GRS_PMT_TOTL_CALCD_AMT
  # Returns intentionally retain existing reporting noise: 85% exact,
  # the remainder within ten percent of the employer wage total.
  expect_true(all(abs(observed - expected) <= 0.10001 * expected + 0.02))
  expect_gt(mean(abs(observed - expected) < 0.02), 0.75)
  primary <- panel[panel$primary_job, ]
  context <- pit_itr_year_read(fixture, 2024L, "context")
  index <- match(context$SYNTHETIC_AEUID, primary$aeuid_ato)
  expect_false(anyNA(index))
  expect_equal(as.integer(context$SUB_OCPTN_GRP_CD),
               fplida:::.ato_occupation_code(primary$anzsco_code[index]))

  ledger <- as.data.frame(fplida:::person_fy_wages__(
    as.character(spine$id), as.character(spine$aeuid_ato),
    as.integer(spine$birth_year), as.integer(spine$baseline_employed),
    as.double(spine$baseline_income), as.integer(spine$baseline_hours),
    as.integer(spine$anzsco_major), as.integer(spine$industry),
    47L, 2010:2025, as.integer(spine$disability_onset_year),
    as.integer(spine$is_dc), as.integer(spine$disability_severity),
    as.double(spine$disability_dose), as.integer(spine$anzsco_code),
    as.double(spine$task_physical), as.integer(spine$archetype)
  ))
  ledger <- ledger[ledger$fy == 2024L, ]
  tax <- pit_itr_year_read(fixture, 2024L, "whld-debt")
  index <- match(tax$SYNTHETIC_AEUID, ledger$aeuid_ato)
  expect_false(anyNA(index))
  expect_equal(tax$NET_TAX_AMT - tax$BAL_PYBLE_RFNDBL_AMT,
               ledger$withholding[index], tolerance = 1e-7)
})

test_that("ITR fallback is deterministic and covers years before PIT_PS", {
  skip_if_not_installed("arrow")
  fixture <- pit_itr_year_fixture()
  before <- pit_itr_year_read(fixture, 2024L, "inc-loss")
  result <- generate_pit_itr(spine = fixture$spine, seed = 47L,
                             years = c(2000L, 2001L, 2024L),
                             output_dir = fixture$out)
  expect_identical(result$years, c(2000L, 2001L, 2024L))
  expect_true(all(result$n_by_year > 0L))
  expect_identical(pit_itr_year_read(fixture, 2024L, "inc-loss"), before)
  for (year in 2000:2001) {
    for (type in c("context", "inc-loss", "ded-exp-off", "whld-debt")) {
      rows <- pit_itr_year_read(fixture, year, type)
      expect_identical(unique(rows$INCM_YR), as.integer(year))
    }
  }
  early <- pit_itr_year_read(fixture, 2000L, "inc-loss")
  generate_pit_itr(spine = fixture$spine, seed = 47L, years = 2000L,
                    output_dir = fixture$out)
  expect_identical(pit_itr_year_read(fixture, 2000L, "inc-loss"), early)
  expect_false(dir.exists(file.path(fixture$run, "ato-pit_ps")))
})

test_that("a missing supported payment-summary year is an error", {
  skip_if_not_installed("arrow")
  fixture <- pit_itr_year_fixture()
  expect_error(generate_pit_itr(spine = fixture$spine, seed = 47L,
                                years = c(2022L, 2024L),
                                output_dir = fixture$out),
               "PIT_PS input missing for financial year ending 2022")
})

test_that("adding an ITR-only year leaves supported PS-based returns unchanged", {
  skip_if_not_installed("arrow")
  fixture <- pit_itr_year_fixture()
  generate_pit_ps(spine = fixture$spine, seed = 47L, years = 2021:2023,
                   output_dir = fixture$out)
  generate_pit_itr(spine = fixture$spine, seed = 47L, years = 2023L,
                    output_dir = fixture$out)
  before <- lapply(c("context", "inc-loss", "ded-exp-off", "whld-debt"),
                    function(type) pit_itr_year_read(fixture, 2023L, type))
  result <- generate_pit_itr(spine = fixture$spine, seed = 47L,
                             years = 2023:2024, output_dir = fixture$out)
  expect_identical(result$years, 2023:2024)
  after <- lapply(c("context", "inc-loss", "ded-exp-off", "whld-debt"),
                   function(type) pit_itr_year_read(fixture, 2023L, type))
  expect_identical(after, before)
  expect_false(any(grepl("2324", list.files(file.path(fixture$run, "ato-pit_ps")))))
})

test_that("an ITR-only year with no earners still writes its four empty tables", {
  skip_if_not_installed("arrow")
  out <- tempfile("pit_itr_empty_")
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  spine <- generate_spine(n = 10L, seed = 51L, output_dir = out)
  # Keep an otherwise valid synthetic spine but place everyone below working age.
  spine$birth_year <- 2023L
  spine$baseline_employed <- 0L
  spine$baseline_income <- 0
  result <- generate_pit_itr(spine = spine, seed = 51L, years = 2024L,
                             output_dir = out)
  expect_identical(result$n_filers, 0L)
  fixture <- list(run = getOption("fplida.run_dir"))
  for (type in c("context", "inc-loss", "ded-exp-off", "whld-debt")) {
    rows <- pit_itr_year_read(fixture, 2024L, type)
    expect_equal(nrow(rows), 0L)
    expect_true(all(c("SYNTHETIC_AEUID", "INCM_YR") %in% names(rows)))
  }
})

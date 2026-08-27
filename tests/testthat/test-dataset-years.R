# Every dataset publishes its own reference period, and a build must not write
# a product year that PLIDA does not have. These tests hold the parser to each
# form the registry uses, and the gate to the products that were inventing
# years before it existed.


# -- Parsing every form the registry uses ------------------------------------

test_that("parse_reference_period reads all eight registry forms", {
  expect_equal(parse_reference_period("2006 to 2023"), 2006:2023)
  expect_equal(parse_reference_period("2010 to current"),
               2010:PLIDA_CURRENT_PERIOD_YEAR)
  expect_equal(parse_reference_period("2009, 2012, 2015, 2018, 2021, 2024"),
               c(2009L, 2012L, 2015L, 2018L, 2021L, 2024L))
  expect_equal(parse_reference_period("2005-2006 to 2023-2024"), 2006:2024)
  expect_equal(
    parse_reference_period("2014-2015, 2017-2018, 2020-2021, 2022-2023"),
    c(2015L, 2018L, 2021L, 2023L)
  )
  expect_equal(parse_reference_period("2020"), 2020L)
  expect_equal(parse_reference_period("2020 to 2025-2026"), 2020:2026)
  expect_equal(parse_reference_period("2011, 2016, 2021"),
               c(2011L, 2016L, 2021L))
})

test_that("each form parses the same way through the shipped registry", {
  expect_equal(plida_dataset_years("A&T"), 2006:2023)
  expect_equal(plida_dataset_years("AIR"), 2010:2026)
  expect_equal(plida_dataset_years("AEDC"),
               c(2009L, 2012L, 2015L, 2018L, 2021L, 2024L))
  expect_equal(plida_dataset_years("APSED"), 2006:2024)
  expect_equal(plida_dataset_years("NHS"), c(2015L, 2018L, 2021L, 2023L))
  expect_equal(plida_dataset_years("ERS"), 2020L)
  expect_equal(plida_dataset_years("STP"), 2020:2026)
  expect_equal(plida_dataset_years("CENSUS"), c(2011L, 2016L, 2021L))
})

test_that("a period with no year at all declares nothing", {
  # How the registry writes BLADE, whose periods sit in its own table metadata.
  blade <- "Table-specific; periods are recorded by variable_info()."
  expect_equal(parse_reference_period(blade), integer(0))
})

test_that("an acronym on several registry rows takes the union", {
  # CORE appears five times and STP twice, one row per module.
  expect_equal(plida_dataset_years("CORE"), 2006:2026)
  expect_equal(plida_dataset_years("STP"), 2020:2026)
  expect_equal(parse_reference_period(c("2015 to 2018", "2020 to 2022")),
               c(2015:2018, 2020:2022))
})

test_that("\"current\" resolves to a fixed year, not the clock", {
  expect_equal(PLIDA_CURRENT_PERIOD_YEAR, 2026L)
  expect_equal(max(plida_dataset_years("MBS")), PLIDA_CURRENT_PERIOD_YEAR)
})


# -- Naming ------------------------------------------------------------------

test_that("build product names and case both resolve", {
  expect_equal(plida_dataset_years("tva"), plida_dataset_years("TVA"))
  expect_equal(plida_dataset_years("pit_ps"), plida_dataset_years("PIT_PS"))
  expect_equal(plida_dataset_years("apprentice"), plida_dataset_years("A&T"))
})

test_that("a dataset with no declared period is unrestricted, not empty", {
  expect_null(plida_dataset_years("BLADE"))
  expect_null(plida_dataset_years("spine"))
  expect_null(plida_dataset_years("NOT_A_DATASET"))
  # Unrestricted keeps every year asked for.
  expect_equal(gate_dataset_years("blade", 1990:2100), 1990:2100)
})


# -- The gate ----------------------------------------------------------------

test_that("TVA covers 2015 to 2023 and generates nothing outside it", {
  expect_equal(plida_dataset_years("TVA"), 2015:2023)
  expect_equal(suppressMessages(gate_dataset_years("TVA", 2010:2025)),
               2015:2023)
  expect_message(gate_dataset_years("TVA", 2010:2025),
                 "TVA covers 2015-2023")
  expect_message(gate_dataset_years("TVA", 2010:2025),
                 "generating 2015-2023")
})

test_that("HE stops in 2021 and PIT_PS at the 2022-23 financial year", {
  expect_equal(suppressMessages(gate_dataset_years("HE", 2015:2025)),
               2015:2021)
  expect_equal(suppressMessages(gate_dataset_years("PIT_PS", 2010:2025)),
               2010:2023)
  expect_equal(suppressMessages(gate_dataset_years("PIT_ITR", 2010:2025)),
               2010:2024)
})

test_that("an empty intersection says so and keeps nothing", {
  expect_equal(suppressMessages(gate_dataset_years("TVA", 2024:2025)),
               integer(0))
  expect_message(gate_dataset_years("TVA", 2024:2025),
                 "excludes every requested year")
  expect_message(gate_dataset_years("HE", 2030L), "Writing nothing for HE")
})

test_that("the gate is silent when it drops nothing", {
  expect_silent(gate_dataset_years("TVA", 2016:2019))
  expect_silent(gate_dataset_years("MBS", 2015:2025))
})

test_that("quiet gating still drops the years", {
  expect_silent(kept <- gate_dataset_years("TVA", 2010:2025, quiet = TRUE))
  expect_equal(kept, 2015:2023)
})


# -- The build plan ----------------------------------------------------------

test_that("the year plan covers only products the build steers", {
  # CGT is left out because a build never passes it `years`: it generates its
  # whole published span whatever window was asked for.
  plan <- plan_product_years(c("spine", "census", "tva", "he", "aedc", "cgt"),
                             2015:2025)
  expect_named(plan, c("he", "tva"), ignore.order = TRUE)
  expect_equal(plan$tva, 2015:2023)
  expect_equal(plan$he, 2015:2021)
})

test_that("the tax products reach back to 2010 whatever the window", {
  plan <- plan_product_years(c("pit_ps", "pit_itr"), 2020:2021)
  expect_equal(plan$pit_ps, 2010:2021)
  expect_equal(plan$pit_itr, 2010:2021)
})

test_that("the plan reports what it narrowed", {
  plan <- plan_product_years(c("tva", "he", "mbs"), 2015:2025)
  expect_message(report_product_year_plan(plan, 2015:2025),
                 "Years by product")
  expect_message(report_product_year_plan(plan, 2015:2025),
                 "TVA covers 2015-2023; dropped 2024-2025")
  expect_message(report_product_year_plan(plan[c("mbs")], 2015:2025),
                 "every product covers 2015-2025")
})

test_that("a year the caller did not ask for is named and explained", {
  # PIT_PS is always built back to 2010, so a narrow window still writes the
  # earlier years. That is a surprise unless the build says so.
  plan <- plan_product_years("pit_ps", 2016:2018)
  expect_equal(plan$pit_ps, 2010:2018)
  expect_message(report_product_year_plan(plan, 2016:2018),
                 "added 2010-2015")
  expect_message(report_product_year_plan(plan, 2016:2018),
                 "always built back to 2010")
})


# -- The companion -----------------------------------------------------------

test_that("plida_dataset_periods reports every dataset in the registry", {
  periods <- plida_dataset_periods()
  expect_s3_class(periods, "data.frame")
  expect_true(all(c("dataset", "reference_period", "years", "first_year",
                    "last_year", "n_years") %in% names(periods)))
  expect_true(all(c("TVA", "HE", "PIT_PS", "CENSUS") %in% periods$dataset))
  expect_false(any(duplicated(periods$dataset)))

  tva <- periods[periods$dataset == "TVA", ]
  expect_equal(tva$reference_period, "2015 to 2023")
  expect_equal(tva$years, "2015-2023")
  expect_equal(tva$first_year, 2015L)
  expect_equal(tva$last_year, 2023L)
  expect_equal(tva$n_years, 9L)

  census <- periods[periods$dataset == "CENSUS", ]
  expect_equal(census$years, "2011, 2016, 2021")
  expect_equal(census$n_years, 3L)
})

test_that("format_year_span collapses runs and keeps gaps", {
  expect_equal(format_year_span(2015:2023), "2015-2023")
  expect_equal(format_year_span(c(2011L, 2016L, 2021L)), "2011, 2016, 2021")
  expect_equal(format_year_span(c(2015:2018, 2020:2021)),
               "2015-2018, 2020-2021")
  expect_equal(format_year_span(integer(0)), "no years")
})


# -- Generators honour the gate ----------------------------------------------

test_that("generate_tva writes nothing for years TVA does not cover", {
  tmp <- file.path(tempdir(), "tva_gate_test")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 100L, seed = 5L, output_dir = tmp)
  run_dir <- getOption("fplida.run_dir")

  expect_message(
    out <- generate_tva(spine = spine, seed = 5L, years = 2024L:2025L,
                        output_dir = tmp, format = "parquet",
                        return_data = FALSE),
    "excludes every requested year"
  )
  expect_null(out)
  expect_false(dir.exists(file.path(run_dir, "ncver-tva")))
})

test_that("generate_tva keeps only the covered years it was asked for", {
  tmp <- file.path(tempdir(), "tva_gate_partial_test")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 100L, seed = 6L, output_dir = tmp)
  run_dir <- getOption("fplida.run_dir")

  out <- suppressMessages(
    generate_tva(spine = spine, seed = 6L, years = 2022L:2025L,
                 output_dir = tmp, format = "parquet", return_data = FALSE)
  )
  expect_equal(out$training_activity$years, 2022:2023)

  written <- list.files(file.path(run_dir, "ncver-tva"))
  written_years <- unique(unlist(regmatches(
    written, gregexpr("(?<![0-9])20[0-9]{2}(?![0-9])", written, perl = TRUE)
  )))
  expect_setequal(written_years, c("2022", "2023"))
})

test_that("a build drops an uncovered product and keeps going", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_build_gate_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # HE ends in 2021, so this build has no HE to write. It must still produce a
  # spine rather than failing, and must say why HE is missing.
  expect_message(
    result <- build_fplida(n = 200L, seed = 9L, years = 2024L:2025L,
                           products = c("spine", "he"), k_slices = 1L,
                           output_dir = tmp),
    "HE covers 2005-2021"
  )
  expect_false("he" %in% result$products)
  expect_true("spine" %in% result$products)
  expect_false(dir.exists(file.path(result$canonical_run_dir, "de-he")))
})

test_that("generate_he stops at 2021", {
  tmp <- file.path(tempdir(), "he_gate_test")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 200L, seed = 7L, output_dir = tmp)
  run_dir <- getOption("fplida.run_dir")

  expect_message(
    generate_he(spine = spine, seed = 7L, years = 2015L:2025L,
                output_dir = tmp, format = "parquet", return_data = FALSE),
    "HE covers 2005-2021"
  )
  enrol <- arrow::read_parquet(list.files(file.path(run_dir, "de-he"),
                                          pattern = "enrol.*\\.parquet$",
                                          full.names = TRUE)[1])
  expect_equal(range(enrol$YEAR, na.rm = TRUE), c(2015L, 2021L))
})

test_that("generate_pit_ps stops at the 2022-23 financial year", {
  tmp <- file.path(tempdir(), "pit_ps_gate_test")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 200L, seed = 8L, output_dir = tmp)
  run_dir <- getOption("fplida.run_dir")

  out <- suppressMessages(
    generate_pit_ps(spine = spine, seed = 8L, years = 2022L:2025L,
                    output_dir = tmp, format = "parquet", return_data = FALSE)
  )
  expect_equal(as.integer(out$years), 2022:2023)

  written <- list.files(file.path(run_dir, "ato-pit_ps"),
                        pattern = "\\.parquet$")
  written <- written[!grepl("spine", written)]
  expect_false(any(grepl("2324|2425", written)))
  expect_true(any(grepl("2122|2223", written)))
})

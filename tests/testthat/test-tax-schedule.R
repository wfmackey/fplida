# Rates, thresholds and the low income tax offset are legislated and change,
# and the window fplida generates spans three schedules plus the 2024-25
# restructure. A single hard-coded schedule made an effective tax rate
# computed across the extract flat by construction.

test_that("the tax schedule changes with the financial year", {
  skip_if_not_installed("arrow")
  tmp <- tempfile("fplida_tax_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 20000L, seed = 7L, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  years <- 2015L:2025L
  generate_pit_ps(spine = spine, seed = 7L, years = years,
                  output_dir = tmp, return_data = FALSE)
  generate_pit_itr(spine = spine, seed = 7L, years = years,
                   output_dir = tmp, return_data = FALSE)

  run_dir <- getOption("fplida.run_dir")
  files <- list.files(file.path(run_dir, "ato-pit_itr"),
                      pattern = "whld-debt.*\\.parquet$", full.names = TRUE)
  skip_if(!length(files), "no withholding tables generated")
  returns <- do.call(rbind, lapply(files, function(path) {
    as.data.frame(arrow::read_parquet(path))
  }))

  offset <- tapply(returns$LOW_INCM_ERNR_TAX_OFST_AMT, returns$INCM_YR, max)
  # The low income tax offset rose from $445 to $700 for 2020-21.
  early <- offset[names(offset) %in% as.character(2015:2020)]
  late <- offset[names(offset) %in% as.character(2021:2025)]
  skip_if(!length(early) || !length(late), "years missing")
  expect_true(all(early == 445))
  expect_true(all(late == 700))
})

test_that("the offset and the levy stay inside their legislated shape", {
  skip_if_not_installed("arrow")
  tmp <- tempfile("fplida_tax_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 10000L, seed = 7L, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  generate_pit_ps(spine = spine, seed = 7L, years = 2022L:2024L,
                  output_dir = tmp, return_data = FALSE)
  generate_pit_itr(spine = spine, seed = 7L, years = 2022L:2024L,
                   output_dir = tmp, return_data = FALSE)

  run_dir <- getOption("fplida.run_dir")
  files <- list.files(file.path(run_dir, "ato-pit_itr"),
                      pattern = "whld-debt.*\\.parquet$", full.names = TRUE)
  skip_if(!length(files), "no withholding tables generated")
  returns <- do.call(rbind, lapply(files, function(path) {
    as.data.frame(arrow::read_parquet(path))
  }))

  expect_true(all(returns$LOW_INCM_ERNR_TAX_OFST_AMT >= 0))
  expect_true(all(returns$LOW_INCM_ERNR_TAX_OFST_AMT <= 700))
  expect_true(all(returns$BSC_ML_CALCD_AMT >= 0))
  expect_true(all(returns$GRS_TAX_AMT >= 0))
  # Net tax is gross less the offset, plus the levy, and cannot go below the
  # levy alone.
  expect_true(all(returns$NET_TAX_AMT >= returns$BSC_ML_CALCD_AMT - 0.01))
})

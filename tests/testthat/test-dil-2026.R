test_that("2026 DIL metadata includes new dataset acronyms and agencies", {
  products_path <- system.file("plida_metadata", "products.csv",
                               package = "fplida")
  products <- read.csv(products_path, stringsAsFactors = FALSE)
  datasets <- unique(products$Dataset)

  expect_true(all(c("APSED", "ATO_MCS", "LFS", "SMSF") %in% datasets))
  expect_true(all(c("ERS", "JK", "JM", "NHS", "NSMHW", "PEX") %in% datasets))

  expect_equal(fplida:::dataset_to_agency("APSED"), "APSC")
  expect_equal(fplida:::dataset_to_agency("ATO_MCS"), "ATO")
  expect_equal(fplida:::dataset_to_agency("LFS"), "ABS")
  expect_equal(fplida:::dataset_to_agency("SMSF"), "ATO")
})

test_that("DIL-backed APSED generator writes all DIL variables", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_apsed_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 200L, seed = 10L, output_dir = tmp,
                 return_data = FALSE)
  generate_apsed(seed = 10L, output_dir = tmp, return_data = FALSE)

  run_dir <- getOption("fplida.run_dir")
  out_path <- file.path(run_dir, "apsc-apsed", "pmp-apsed.parquet")
  expect_true(file.exists(out_path))

  got <- names(as.data.frame(arrow::read_parquet(out_path)))
  variables_path <- system.file("plida_metadata", "variables.csv",
                                package = "fplida")
  variables <- read.csv(variables_path,
                        stringsAsFactors = FALSE, check.names = FALSE)
  expected <- unique(variables[variables$Dataset == "APSED", "Variable Name"])
  expect_true(all(expected %in% got))
})

test_that("STP generator writes 2026 DIL standard and extended raw table shapes", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_stp_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 250L, seed = 11L, output_dir = tmp,
                 return_data = FALSE)
  generate_stp(seed = 11L, years = 2020L, output_dir = tmp,
               return_data = FALSE, chunk_size = 1000L)

  run_dir <- getOption("fplida.run_dir")
  stp_dir <- file.path(run_dir, "ato-stp")
  variables_path <- system.file("plida_metadata", "variables.csv",
                                package = "fplida")
  variables <- read.csv(variables_path,
                        stringsAsFactors = FALSE, check.names = FALSE)
  stp_vars <- variables[variables$Dataset == "STP", , drop = FALSE]

  expect_dil_cols <- function(table_name) {
    product_dir <- file.path(stp_dir, table_name)
    expect_true(dir.exists(product_dir))
    got <- names(as.data.frame(arrow::open_dataset(product_dir, format = "parquet")))
    expected <- unique(stp_vars[stp_vars[["Table Name"]] == table_name,
                                "Variable Name"])
    expect_setequal(got, expected)
  }

  expect_dil_cols("stp_standard_pay_events_2020_m01")
  expect_dil_cols("stp_extended_pay_events_2020_m01")
  expect_dil_cols("stp_standard_jobs_2019_20")
  expect_dil_cols("stp_extended_jobs_2019_20")
  expect_dil_cols("stp_extended_etp_2019_20")

  mb_lookup <- fplida:::.load_mb_lookup()
  sa2_state <- unique(mb_lookup[c("sa2_code", "state")])

  standard <- as.data.frame(arrow::open_dataset(
    file.path(stp_dir, "stp_standard_pay_events_2020_m01"),
    format = "parquet"
  ))
  standard_geo <- sa2_state[match(standard$SA2_ASGS_2021,
                                  sa2_state$sa2_code), , drop = FALSE]
  expect_false(anyNA(standard_geo$state))
  expect_equal(as.integer(standard$STATE_ASGS_2021), standard_geo$state)

  extended <- as.data.frame(arrow::open_dataset(
    file.path(stp_dir, "stp_extended_pay_events_2020_m01"),
    format = "parquet"
  ))
  extended_geo <- mb_lookup[match(extended$STP_MESHBLOCK_ABS,
                                  mb_lookup$mb_code), , drop = FALSE]
  expect_false(anyNA(extended_geo$state))
  expect_equal(extended$SA2_ASGS_2021, extended_geo$sa2_code)
  expect_equal(as.integer(extended$STATE_ASGS_2021), extended_geo$state)
})

test_that("STP generator includes zero, positive, and overlapping job gaps", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_stp_gaps_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 1500L, seed = 12L, output_dir = tmp,
                 return_data = FALSE)
  generate_stp(seed = 12L, years = 2020L:2025L, output_dir = tmp,
               return_data = FALSE, chunk_size = 1000L)

  run_dir <- getOption("fplida.run_dir")
  stp_dir <- file.path(run_dir, "ato-stp")
  pay_files <- list.files(stp_dir, pattern = "\\.parquet$",
                          recursive = TRUE, full.names = TRUE)
  pay_files <- pay_files[grepl("/stp_extended_pay_events_", pay_files)]
  pay <- as.data.frame(arrow::open_dataset(pay_files, format = "parquet"))

  # STP/MBS/PBS dates are emitted as "ddmmmYY" strings (e.g. 31Jan20).
  pay$spell_start <- as.Date(pay$DRVPAYSTARTDATE, format = "%d%b%y")
  pay$spell_end <- as.Date(pay$DRVPAYENDDATE, format = "%d%b%y")
  pay$payment_date <- as.Date(pay$PMT_DT, format = "%d%b%y")
  pay$period_days <- as.integer(pay$spell_end - pay$spell_start) + 1L
  positive_pay <- pay[pay$PMT_SUMRY_TOTL_GRS_PMT_AMT > 0 &
                        !grepl("_CORR", pay$SEQUENCE_KEY), , drop = FALSE]

  expect_gt(mean(positive_pay$period_days == 14L), 0.60)
  expect_gt(sum(positive_pay$period_days == 14L),
            sum(positive_pay$period_days %in% 28L:31L))
  expect_gt(sum(positive_pay$period_days %in% 28L:31L),
            sum(positive_pay$period_days == 7L))
  expect_gt(sum(!(positive_pay$period_days %in% c(7L, 14L, 28L:31L))), 0L)

  long_periods <- positive_pay[positive_pay$period_days > 180L, , drop = FALSE]
  expect_gt(nrow(long_periods), 0L)
  expect_gt(mean(format(long_periods$spell_end, "%m-%d") == "06-30"), 0.8)

  positive_pay$gross_per_day <- positive_pay$PMT_SUMRY_TOTL_GRS_PMT_AMT /
    pmax(positive_pay$period_days, 1L)
  median_daily <- aggregate(
    gross_per_day ~ SYNTHETIC_AEUID + BN,
    positive_pay[positive_pay$period_days <= 60L, , drop = FALSE],
    stats::median
  )
  names(median_daily)[names(median_daily) == "gross_per_day"] <-
    "median_gross_per_day"
  spike_pay <- merge(positive_pay, median_daily,
                     by = c("SYNTHETIC_AEUID", "BN"))
  spike_pay <- spike_pay[spike_pay$period_days <= 60L &
                           spike_pay$gross_per_day >
                             spike_pay$median_gross_per_day * 1.2,
                         , drop = FALSE]
  expect_gt(nrow(spike_pay), 0L)
  spike_june_share <- mean(format(spike_pay$payment_date, "%m") == "06")
  pay_june_share <- mean(format(positive_pay$payment_date, "%m") == "06")
  expect_gt(spike_june_share, pay_june_share * 1.5)

  leave_lump_sum_a <- pay[pay$ANL_LNG_SRVC_UNSD_LS_A_AMT > 0, ,
                          drop = FALSE]
  expect_gt(nrow(leave_lump_sum_a), 0L)
  expect_true(all(!is.na(leave_lump_sum_a$PYR_PYE_RLTNSHP_CESTN_DT)))
  expect_true(all(leave_lump_sum_a$ANL_LNG_SRVC_UNSD_LS_A_CD %in% c("R", "T")))
  expect_true(all(leave_lump_sum_a$PMT_SUMRY_TOTL_GRS_PMT_AMT > 0))

  expect_equal(sum(abs(pay$F_EMPLT_INCM_GRS_AMT)), 0)
  expect_equal(sum(abs(pay$F_EMPLT_INCM_TOTL_AMT)), 0)
  expect_equal(sum(abs(pay$F_EMPLT_INCM_TAX_CR_WHELD_AMT)), 0)
  expect_equal(sum(abs(pay$F_EMPLT_INCM_TAX_PMT_AMT)), 0)
  expect_equal(pay$INCM_GRS_AMT, pay$PMT_SUMRY_TOTL_GRS_PMT_AMT)
  expect_equal(pay$WHELD_AMT, pay$TAX_WHELD_TOTL_AMT)
  expect_gt(sum(!is.na(pay$CNTRCTR_BN)), 0L)

  monthly_pay <- aggregate(
    PMT_SUMRY_TOTL_GRS_PMT_AMT ~ SYNTHETIC_AEUID + BN +
      ym,
    transform(positive_pay, ym = format(payment_date, "%Y-%m")),
    sum
  )
  monthly_cv <- aggregate(
    PMT_SUMRY_TOTL_GRS_PMT_AMT ~ SYNTHETIC_AEUID + BN,
    monthly_pay,
    function(x) if (length(x) > 5L && mean(x) > 0) stats::sd(x) / mean(x) else NA_real_
  )
  expect_gt(sum(monthly_cv$PMT_SUMRY_TOTL_GRS_PMT_AMT > 0.6,
                na.rm = TRUE), 70L)

  overlap_groups <- unlist(lapply(
    split(positive_pay[, c("spell_start", "spell_end")],
          paste(positive_pay$EMPLOYEE_KEY, format(positive_pay$payment_date, "%Y-%m"))),
    function(x) {
      if (nrow(x) < 2L) return(FALSE)
      x <- x[order(x$spell_start, x$spell_end), , drop = FALSE]
      any(x$spell_start[-1L] <= x$spell_end[-nrow(x)])
    }
  ), use.names = FALSE)
  expect_gt(sum(overlap_groups), 0L)

  starts <- aggregate(spell_start ~ SYNTHETIC_AEUID + BN, pay, min)
  ends <- aggregate(spell_end ~ SYNTHETIC_AEUID + BN, pay, max)
  spells <- merge(starts, ends, by = c("SYNTHETIC_AEUID", "BN"))
  spells <- spells[order(spells$SYNTHETIC_AEUID, spells$spell_start,
                         spells$spell_end, spells$BN), ]

  gap_days <- unlist(lapply(split(spells, spells$SYNTHETIC_AEUID), function(x) {
    if (nrow(x) < 2L) return(integer(0))
    as.integer(x$spell_start[-1] - x$spell_end[-nrow(x)]) - 1L
  }), use.names = FALSE)

  expect_true(any(gap_days == 0L))
  expect_true(any(gap_days > 0L))
  expect_true(any(gap_days < 0L))
  # Most job-to-job transitions carry a real between-job gap, but continuous
  # hand-overs and overlapping spells both stay well represented.
  expect_gt(sum(gap_days > 0L), sum(gap_days == 0L))
  expect_gt(sum(gap_days == 0L) / length(gap_days), 0.05)
  expect_gt(sum(gap_days < 0L) / length(gap_days), 0.05)

  corrections <- pay[pay$PMT_SUMRY_TOTL_GRS_PMT_AMT < 0, , drop = FALSE]
  overpayments <- pay[grepl("_OP$", pay$SEQUENCE_KEY), , drop = FALSE]
  expect_gt(nrow(corrections), 0L)
  expect_equal(nrow(corrections), nrow(overpayments))

  correction_rate <- nrow(corrections) / nrow(pay)
  expect_gt(correction_rate, 0.0002)
  expect_lt(correction_rate, 0.001)
  expect_true(all(grepl("_CORR[0-9]{6}P[0-9]{2}$",
                        corrections$SEQUENCE_KEY)))
  expect_true(all(corrections$INCM_GRS_AMT < 0))
  expect_true(all(corrections$F_EMPLT_INCM_GRS_AMT == 0))
  expect_true(all(corrections$F_EMPLT_INCM_TOTL_AMT == 0))
  expect_true(all(corrections$TAX_WHELD_TOTL_AMT <= 0))

  overpayment_keys <- paste(
    overpayments$EMPLOYEE_KEY,
    sub("^.*_([0-9]{6})_J([0-9]+)_P([0-9]{2})_OP$", "\\1",
        overpayments$SEQUENCE_KEY),
    sub("^.*_([0-9]{6})_J([0-9]+)_P([0-9]{2})_OP$", "\\2",
        overpayments$SEQUENCE_KEY),
    sub("^.*_([0-9]{6})_J([0-9]+)_P([0-9]{2})_OP$", "\\3",
        overpayments$SEQUENCE_KEY)
  )
  correction_keys <- paste(
    corrections$EMPLOYEE_KEY,
    sub("^.*_CORR([0-9]{6})P[0-9]{2}$", "\\1", corrections$SEQUENCE_KEY),
    sub("^.*_J([0-9]+)_P[0-9]{2}_CORR[0-9]{6}P[0-9]{2}$", "\\1",
        corrections$SEQUENCE_KEY),
    sub("^.*_CORR[0-9]{6}P([0-9]{2})$", "\\1", corrections$SEQUENCE_KEY)
  )
  expect_true(all(correction_keys %in% overpayment_keys))
})


test_that("STP emits Phase 2 nonstandard income, job gaps and ETP evidence", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_stp_labour_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 3000L, seed = 13L, output_dir = tmp,
                 return_data = FALSE)
  generate_stp(seed = 13L, years = 2020L:2025L, output_dir = tmp,
               return_data = FALSE, chunk_size = 1000L)

  run_dir <- getOption("fplida.run_dir")
  stp_dir <- file.path(run_dir, "ato-stp")
  pay_files <- list.files(stp_dir, pattern = "\\.parquet$",
                          recursive = TRUE, full.names = TRUE)
  pay_files <- pay_files[grepl("/stp_extended_pay_events_", pay_files)]
  pay <- as.data.frame(arrow::open_dataset(pay_files, format = "parquet"))

  expect_gt(sum(!is.na(pay$CNTRCTR_BN)), 0L)
  expect_gt(sum(abs(pay$LABR_HIR_TOTL_AMT)), 0)
  expect_gt(sum(abs(pay$INCM_LABR_HIR_ARNGMT_GRS_AMT)), 0)
  expect_gt(
    sum(abs(pay$INCM_WHM_GRS_AMT)) +
      sum(abs(pay$INCM_VA_GRS_AMT)) +
      sum(abs(pay$INCM_CMNTY_DEV_EMPLT_PRJCT_AMT)),
    0
  )
  expect_equal(pay$INCM_GRS_AMT, pay$PMT_SUMRY_TOTL_GRS_PMT_AMT)
  expect_equal(pay$WHELD_AMT, pay$TAX_WHELD_TOTL_AMT)
  expect_gt(sum(!is.na(pay$PYR_PYE_RLTNSHP_CESTN_DT)), 0L)
  expect_gt(sum(pay$PYR_PYE_RLTNSHP_TRMNTD_IND == 1L), 0L)

  jobs_files <- list.files(stp_dir, pattern = "\\.parquet$",
                           recursive = TRUE, full.names = TRUE)
  jobs_files <- jobs_files[grepl("/stp_extended_jobs_", jobs_files)]
  jobs <- as.data.frame(arrow::open_dataset(jobs_files, format = "parquet"))
  jobs$spell_start <- as.Date(jobs$PYR_PYE_RLTNSHP_CMNCMT_DT,
                              format = "%d%b%y")
  jobs$spell_end <- as.Date(jobs$PYR_PYE_RLTNSHP_CESTN_DT,
                            format = "%d%b%y")
  jobs <- jobs[, c("SYNTHETIC_AEUID", "BN", "spell_start", "spell_end")]
  jobs <- aggregate(
    spell_end ~ SYNTHETIC_AEUID + BN + spell_start,
    jobs,
    function(x) {
      x <- x[!is.na(x)]
      if (length(x)) max(x) else as.Date(NA)
    },
    na.action = NULL
  )
  jobs <- jobs[order(jobs$SYNTHETIC_AEUID, jobs$spell_start,
                     jobs$spell_end, jobs$BN), , drop = FALSE]

  gap_parts <- lapply(split(jobs, jobs$SYNTHETIC_AEUID), function(x) {
    if (nrow(x) < 2L) return(NULL)
    end_order <- x$spell_end
    end_order[is.na(end_order)] <- as.Date("2025-10-31")
    x <- x[order(x$spell_start, end_order, x$BN), , drop = FALSE]
    prior_end <- x$spell_end[-nrow(x)]
    ok <- !is.na(prior_end)
    if (!any(ok)) return(NULL)
    data.frame(
      gap_days = as.integer(x$spell_start[-1L][ok] - prior_end[ok]) - 1L,
      same_employer_recall = x$BN[-1L][ok] == x$BN[-nrow(x)][ok],
      stringsAsFactors = FALSE
    )
  })
  gap_parts <- Filter(Negate(is.null), gap_parts)
  expect_gt(length(gap_parts), 0L)
  gap_rows <- do.call(rbind, gap_parts)

  expect_gt(nrow(gap_rows), 0L)
  expect_true(any(gap_rows$gap_days == 0L))
  expect_true(any(gap_rows$gap_days >= 15L))
  expect_true(any(gap_rows$gap_days < 0L))
  expect_true(any(gap_rows$same_employer_recall & gap_rows$gap_days >= 15L))

  etp_files <- list.files(stp_dir, pattern = "\\.parquet$",
                          recursive = TRUE, full.names = TRUE)
  etp_files <- etp_files[grepl("/stp_extended_etp_", etp_files)]
  etp <- as.data.frame(arrow::open_dataset(etp_files, format = "parquet"))
  expect_gt(nrow(etp), 0L)
  etp$etp_date <- as.Date(etp$ETP_PMT_DT, format = "%d%b%y")
  ended_jobs <- jobs[!is.na(jobs$spell_end), , drop = FALSE]
  etp_near_exit <- merge(etp, ended_jobs, by = c("SYNTHETIC_AEUID", "BN"))
  expect_gt(nrow(etp_near_exit), 0L)
  expect_gt(mean(abs(as.integer(etp_near_exit$etp_date -
                                  etp_near_exit$spell_end)) <= 45L), 0.8)
})

#' Generate STP dataset (Single Touch Payroll)
#'
#' The 19 March 2026 DIL represents STP as two products:
#' \code{pmp-stp-standard} and \code{pmp-stp-extended}. The raw tables
#' are monthly pay-event tables plus financial-year jobs and ETP
#' tables. The employment behaviour model uses person-level employer
#' spells, annual earnings, allowances, superannuation and ETP records.
#' Job histories include a large mass of direct job-to-job moves,
#' positive payroll gaps of varying length, and overlapping jobs where
#' the next employer starts before the previous job has ended. Pay-event
#' rows are mostly fortnightly, with smaller monthly, weekly and chaotic
#' cadence profiles. The chaotic profiles create overlapping pay periods,
#' occasional financial-year-anchored long periods, highly variable pay
#' and end-of-financial-year irregular gross-payment concentration. A
#' rare subset of pay events are overpaid and then followed by negative
#' corrective payment rows. Phase 1 bonuses and commissions are included
#' only in gross payment fields; lump-sum fields are reserved for final
#' pay, leave, redundancy and backpay concepts.
#'
#' @section Dataset and variable information:
#' The [ATO Single Touch Payroll](https://www.ato.gov.au/businesses-and-organisations/hiring-and-paying-your-workers/single-touch-payroll)
#' website gives information about this dataset. Use `dataset_info("STP")` for
#' dataset information. Use `variable_info("STP")` for variables, sources,
#' value support, and topic tags.
#'
#' @param spine Data.frame or NULL.
#' @param seed Integer. Random seed.
#' @param years Integer vector. Calendar years to emit. The current DIL
#'   has monthly pay events from 2020-01 through 2025-10.
#' @param output_dir Character or NULL.
#' @param format Character. "parquet" only.
#' @param return_data Logical. If TRUE, reads generated products back.
#'   Intended only for small test builds.
#' @param chunk_size Integer. Max participants per output chunk.
#'
#' @export
generate_stp <- function(spine = NULL, seed = 42L, years = 2020L:2025L,
                         output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = FALSE,
                         chunk_size = 100000L) {
  seed <- as.integer(seed)
  years <- sort(unique(as.integer(years)))
  format <- match.arg(format)
  chunk_size <- as.integer(chunk_size)
  if (format != "parquet") {
    stop("generate_stp() writes parquet only.", call. = FALSE)
  }

  run_dir <- resolve_run_dir(output_dir)

  # Draw STP employers from the BLADE business universe (built earlier) so BN
  # values resolve to BLADE businesses.
  .set_business_pool_from_spine(run_dir)

  ds_dir <- dataset_dir(run_dir, "STP")

  # STP income is anchored to the reconciled employment panel, so STP needs the
  # same spine columns the panel reads (as PIT_PS / PIT_IE do).
  stp_cols <- c("spine_id", "aeuid_ato", "id", "birth_year", "sex", "state",
                "baseline_employed", "baseline_income", "baseline_hours",
                "anzsco_major", "anzsco_code", "industry", "task_physical",
                "archetype", "disability_onset_year", "is_dc",
                "disability_severity", "disability_dose")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, stp_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))
  spine <- filter_ato_records(spine, reference_year = max(years))

  # Reconciled per-(person, FY) wage total from the shared panel (built ONCE
  # with the BASE seed). STP pay events are scaled to this so a person's STP
  # income tracks their PAYG payment summaries and PIT salary line.
  wage_by_key <- numeric(0)
  if (exists("person_fy_wages__", mode = "function")) {
    wt <- person_fy_wages__(
      id = as.character(spine$id), aeuid_ato = as.character(spine$aeuid_ato),
      birth_year = as.integer(spine$birth_year),
      baseline_employed = as.integer(spine$baseline_employed),
      baseline_income = as.numeric(spine$baseline_income),
      baseline_hours = as.integer(spine$baseline_hours),
      anzsco_major = as.integer(spine$anzsco_major),
      industry = as.integer(spine$industry),
      seed = as.integer(seed), years = as.integer(sort(unique(years))),
      disability_onset_year = as.integer(spine$disability_onset_year),
      disability_is_dc = as.integer(spine$is_dc),
      disability_severity = as.integer(spine$disability_severity),
      disability_dose = as.numeric(spine$disability_dose),
      anzsco_code = as.integer(spine$anzsco_code),
      task_physical = as.numeric(spine$task_physical),
      archetype = as.integer(spine$archetype)
    )
    wage_by_key <- stats::setNames(as.numeric(wt$gross),
                                   paste0(wt$aeuid_ato, "_", wt$fy))
  }

  mini_spine <- data.frame(
    spine_id = spine$spine_id,
    aeuid_ato = spine$aeuid_ato,
    stringsAsFactors = FALSE
  )

  employed_idx <- which(spine$baseline_employed == 1L &
                          !is.na(spine$baseline_income) &
                          spine$baseline_income > 0)
  if (length(employed_idx) == 0L) {
    write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                       reference_year = max(years))
    if (return_data) return(list())
    return(invisible(list(n_records = integer(0), years = years)))
  }

  month_grid <- .stp_month_grid(years)
  n_records <- list()

  for (row in seq_len(nrow(month_grid))) {
    year <- month_grid$year[row]
    month <- month_grid$month[row]
    n_records[[.stp_table_name("standard_pay_events", year, month)]] <-
      .write_stp_pay_events(spine, employed_idx, run_dir, seed,
                            year, month, extended = FALSE,
                            chunk_size = chunk_size,
                            wage_by_key = wage_by_key)
    n_records[[.stp_table_name("extended_pay_events", year, month)]] <-
      .write_stp_pay_events(spine, employed_idx, run_dir, seed,
                            year, month, extended = TRUE,
                            chunk_size = chunk_size,
                            wage_by_key = wage_by_key)
  }

  fy_ends <- sort(unique(.stp_fy_end(month_grid$year, month_grid$month)))
  for (fy_end in fy_ends) {
    n_records[[.stp_fy_table_name("standard_jobs", fy_end)]] <-
      .write_stp_jobs(spine, employed_idx, run_dir, seed,
                      fy_end, extended = FALSE, chunk_size = chunk_size)
    n_records[[.stp_fy_table_name("extended_jobs", fy_end)]] <-
      .write_stp_jobs(spine, employed_idx, run_dir, seed,
                      fy_end, extended = TRUE, chunk_size = chunk_size)
    n_records[[.stp_fy_table_name("extended_etp", fy_end)]] <-
      .write_stp_etp(spine, employed_idx, run_dir, seed,
                     fy_end, chunk_size = chunk_size)
  }

  write_agency_spine(mini_spine, "ATO", ds_dir, format = format,
                     reference_year = max(years))
  if (spine_loaded) { rm(spine); gc() }

  if (return_data) {
    return(.read_stp_generated(ds_dir))
  }

  invisible(list(n_records = unlist(n_records), years = years))
}

.stp_month_grid <- function(years) {
  rows <- do.call(rbind, lapply(years, function(year) {
    if (year < 2020L) return(NULL)
    if (year > 2025L) return(NULL)
    months <- if (year == 2025L) 1L:10L else 1L:12L
    data.frame(year = year, month = months)
  }))
  if (is.null(rows)) {
    data.frame(year = integer(0), month = integer(0))
  } else {
    rows
  }
}

.stp_fy_end <- function(year, month) {
  ifelse(month >= 7L, year + 1L, year)
}

.stp_fy_label <- function(year, month) {
  fy_end <- .stp_fy_end(year, month)
  sprintf("%d-%02d", fy_end - 1L, fy_end %% 100L)
}

.stp_fy_suffix <- function(fy_end) {
  sprintf("%d_%02d", fy_end - 1L, fy_end %% 100L)
}

.stp_table_name <- function(kind, year, month) {
  sprintf("stp_%s_%d_m%02d", kind, as.integer(year), as.integer(month))
}

.stp_fy_table_name <- function(kind, fy_end) {
  sprintf("stp_%s_%s", kind, .stp_fy_suffix(as.integer(fy_end)))
}

.stp_sg_rate <- function(fy_end) {
  if (fy_end <= 2021L) 0.095
  else if (fy_end == 2022L) 0.10
  else if (fy_end == 2023L) 0.105
  else if (fy_end == 2024L) 0.11
  else if (fy_end == 2025L) 0.115
  else 0.12
}

.stp_chunks <- function(indices, chunk_size) {
  n <- length(indices)
  if (n == 0L) return(list(integer(0)))
  starts <- seq.int(1L, n, by = chunk_size)
  lapply(starts, function(s) indices[s:min(s + chunk_size - 1L, n)])
}

.stp_business_id <- function(spine_id, seed, year = 2024L) {
  raw <- (.stp_spine_number(spine_id) * 1103515245 + seed + year * 1009) %% 1e12
  .bn_for_hash_r(raw, sprintf("BN%012.0f", raw))
}

.stp_spine_number <- function(spine_id) {
  as.numeric(gsub("[^0-9]", "", spine_id))
}

.stp_employee_key <- function(aeuid, bn) {
  paste0(aeuid, "_", bn)
}

.stp_draw <- function(spine_id, seed, salt) {
  spine_num <- .stp_spine_number(spine_id)
  (spine_num * 1103515245 + seed * 12345 + salt * 2654435761) %% 2147483647
}

.stp_job_business_id <- function(spine_id, seed, job_no) {
  spine_num <- .stp_spine_number(spine_id) + job_no * 7919
  raw <- (spine_num * 1103515245 + seed + job_no * 100003 + 2024 * 1009) %% 1e12
  .bn_for_hash_r(raw, sprintf("BN%012.0f", raw))
}

.stp_positive_gap_days <- function(draw) {
  c(1L, 7L, 14L, 21L, 30L, 60L, 90L, 180L)[draw %% 8L + 1L]
}

.stp_labour_gap_days <- function(draw) {
  c(15L, 21L, 30L, 45L, 60L, 90L, 120L, 180L)[draw %% 8L + 1L]
}

.stp_overlap_gap_days <- function(draw) {
  -c(1L, 7L, 14L, 21L, 30L)[draw %% 5L + 1L]
}

.stp_payment_draw <- function(spine_id, seed, year, month, job_no, pay_seq = 0L,
                              salt = 0L) {
  spine_num <- .stp_spine_number(spine_id)
  (spine_num * 1103515245 + seed * 12345 + year * 1000003 +
     month * 9176 + job_no * 1009 + pay_seq * 104729 +
     salt * 2654435761) %% 2147483647
}

.stp_is_overpayment_event <- function(spine_id, seed, year, month, job_no,
                                      pay_seq) {
  .stp_payment_draw(spine_id, seed, year, month, job_no, pay_seq,
                    salt = 1L) %% 2000L == 0L
}

.stp_overpayment_extra <- function(base_gross, draw) {
  round(base_gross * (0.10 + (draw %% 21L) / 100), 2)
}

.stp_pay_cadence <- function(spine_id, seed, job_no) {
  draw <- .stp_draw(spine_id, seed, 90L + job_no) %% 100L
  if (draw < 70L) "fortnightly"
  else if (draw < 92L) "monthly"
  else if (draw < 96L) "weekly"
  else "chaotic"
}

.stp_is_variable_pay_profile <- function(spine_id, seed, job_no) {
  .stp_draw(spine_id, seed, 100L + job_no) %% 100L < 30L
}

.stp_make_period <- function(job, start, end, pay_date, seq_no, gross_day_cap) {
  row_start <- max(start, job$start)
  row_end <- min(end, if (is.na(job$end)) end else job$end)
  if (row_start > row_end) return(NULL)
  actual_days <- as.integer(row_end - row_start) + 1L
  data.frame(
    start = row_start,
    end = row_end,
    pay_date = pay_date,
    seq_no = as.integer(seq_no),
    gross_days = max(1L, min(actual_days, gross_day_cap)),
    stringsAsFactors = FALSE
  )
}

.stp_regular_pay_periods <- function(spine_id, seed, job, month_start,
                                     month_end, interval_days) {
  offset <- as.integer(.stp_draw(spine_id, seed, 110L + job$job_no) %%
                         interval_days)
  lag <- as.integer(.stp_draw(spine_id, seed, 111L + job$job_no) %% 4L)
  anchor <- as.Date("2020-01-01") - offset
  period_start <- month_start - interval_days - 7L
  period_start <- period_start -
    (as.integer(period_start - anchor) %% interval_days)

  out <- list()
  seq_no <- 1L
  while (period_start <= month_end) {
    period_end <- period_start + interval_days - 1L
    pay_date <- period_end + lag
    if (pay_date >= month_start && pay_date <= month_end) {
      period <- .stp_make_period(job, period_start, period_end, pay_date,
                                 seq_no, interval_days)
      if (!is.null(period)) out[[length(out) + 1L]] <- period
      seq_no <- seq_no + 1L
    }
    period_start <- period_start + interval_days
  }
  out
}

.stp_chaotic_pay_periods <- function(spine_id, seed, year, month, job,
                                     month_start, month_end) {
  month_days <- as.integer(month_end - month_start) + 1L
  count <- 2L + as.integer(.stp_payment_draw(spine_id, seed, year, month,
                                             job$job_no, 0L,
                                             salt = 10L) %% 3L)
  lengths <- c(3L, 5L, 7L, 9L, 14L, 21L, 28L, 45L, 60L)
  out <- list()
  for (seq_no in seq_len(count)) {
    draw <- .stp_payment_draw(spine_id, seed, year, month, job$job_no,
                              seq_no, salt = 11L)
    len <- lengths[draw %% length(lengths) + 1L]
    offset <- as.integer(draw %% (month_days + 12L)) - 6L
    overlap_shift <- if (seq_no > 1L && draw %% 3L == 0L) len %/% 2L else 0L
    period_start <- month_start + offset - overlap_shift
    period_end <- period_start + len - 1L
    pay_date <- month_start + as.integer((draw %/% 17L) %% month_days)
    period <- .stp_make_period(job, period_start, period_end, pay_date,
                               seq_no, 60L)
    if (!is.null(period)) out[[length(out) + 1L]] <- period
  }
  out
}

.stp_fy_anchored_period <- function(spine_id, seed, year, month, job) {
  if (month != 6L) return(NULL)
  draw <- .stp_payment_draw(spine_id, seed, year, month, job$job_no, 90L,
                            salt = 12L)
  if (draw %% 20L != 0L) return(NULL)
  fy_start <- as.Date(sprintf("%d-07-01", year - 1L)) + as.integer(draw %% 31L)
  fy_end <- as.Date(sprintf("%d-06-30", year))
  .stp_make_period(job, fy_start, fy_end, fy_end, 90L, 31L)
}

.stp_pay_periods <- function(spine_id, seed, year, month, job, month_start,
                             month_end) {
  cadence <- .stp_pay_cadence(spine_id, seed, job$job_no)
  out <- switch(
    cadence,
    monthly = list(.stp_make_period(job, month_start, month_end, month_end,
                                    1L, as.integer(month_end - month_start) + 1L)),
    weekly = .stp_regular_pay_periods(spine_id, seed, job, month_start,
                                      month_end, 7L),
    chaotic = .stp_chaotic_pay_periods(spine_id, seed, year, month, job,
                                       month_start, month_end),
    .stp_regular_pay_periods(spine_id, seed, job, month_start, month_end, 14L)
  )
  out <- Filter(Negate(is.null), out)
  fy_period <- .stp_fy_anchored_period(spine_id, seed, year, month, job)
  if (!is.null(fy_period)) out[[length(out) + 1L]] <- fy_period
  if (length(out) == 0L && job$start <= month_end &&
      (is.na(job$end) || job$end >= month_start)) {
    out[[1L]] <- .stp_make_period(job, max(month_start, job$start),
                                  min(month_end, if (is.na(job$end)) month_end else job$end),
                                  month_end, 1L,
                                  as.integer(month_end - month_start) + 1L)
  }
  if (length(out) == 0L) {
    return(data.frame(start = as.Date(character(0)),
                      end = as.Date(character(0)),
                      pay_date = as.Date(character(0)),
                      seq_no = integer(0),
                      gross_days = integer(0)))
  }
  out <- do.call(rbind, out)
  out[order(out$pay_date, out$start, out$end, out$seq_no), , drop = FALSE]
}

.stp_pay_variability_multiplier <- function(spine_id, seed, year, month,
                                            job_no, pay_seq, cadence) {
  draw <- .stp_payment_draw(spine_id, seed, year, month, job_no, pay_seq,
                            salt = 3L)
  if (identical(cadence, "chaotic")) {
    return(c(0.12, 0.30, 0.55, 0.85, 1.15, 1.55, 2.10, 2.80, 3.60, 4.50)
           [draw %% 10L + 1L])
  }
  if (.stp_is_variable_pay_profile(spine_id, seed, job_no)) {
    return(c(0.35, 0.55, 0.75, 0.95, 1.20, 1.50, 1.90, 2.40, 3.00)
           [draw %% 9L + 1L])
  }
  0.96 + (draw %% 9L) / 100
}

.stp_pay_event_gross <- function(annual_income, period, spine_id, seed, year,
                                 month, job_no, cadence) {
  daily_base <- max(annual_income, 0) / 365.25
  multiplier <- .stp_pay_variability_multiplier(
    spine_id, seed, year, month, job_no, period$seq_no, cadence
  )
  round(daily_base * period$gross_days * multiplier, 2)
}

.stp_irregular_gross_amount <- function(base_gross, spine_id, seed, year,
                                        month, job_no, pay_seq) {
  if (base_gross <= 0) return(0)
  draw <- .stp_payment_draw(spine_id, seed, year, month, job_no, pay_seq,
                            salt = 4L)
  if (month == 6L && draw %% 4L == 0L) {
    round(base_gross * (0.50 + (draw %% 51L) / 100), 2)
  } else if (draw %% 250L == 0L) {
    round(base_gross * 0.15, 2)
  } else {
    0
  }
}

.stp_leave_lump_sum_a_amount <- function(base_gross, spine_id, seed, year,
                                         month, job_no, pay_seq, cessation) {
  if (base_gross <= 0 || is.na(cessation)) return(0)
  draw <- .stp_payment_draw(spine_id, seed, year, month, job_no, pay_seq,
                            salt = 5L)
  if (draw %% 3L != 0L) return(0)
  round(base_gross * (0.35 + (draw %% 66L) / 100), 2)
}

.stp_leave_lump_sum_a_code <- function(spine_id, seed, year, month, job_no,
                                       pay_seq, amount) {
  if (amount <= 0) return(NA_character_)
  draw <- .stp_payment_draw(spine_id, seed, year, month, job_no, pay_seq,
                            salt = 6L)
  if (draw %% 5L == 0L) "R" else "T"
}

.stp_month_bounds <- function(year, month) {
  start <- as.Date(sprintf("%d-%02d-01", year, month))
  end <- seq(start, length = 2L, by = "1 month")[2L] - 1L
  list(start = start, end = end)
}

.stp_next_dil_month <- function(year, month) {
  if (month == 12L) {
    year <- year + 1L
    month <- 1L
  } else {
    month <- month + 1L
  }
  if (year > 2025L || (year == 2025L && month > 10L)) return(NULL)
  c(year = year, month = month)
}

.stp_prev_dil_month <- function(year, month) {
  if (month == 1L) {
    year <- year - 1L
    month <- 12L
  } else {
    month <- month - 1L
  }
  if (year < 2020L) return(NULL)
  c(year = year, month = month)
}

.stp_job_history <- function(spine_id, seed) {
  obs_start <- as.Date("2020-01-01")
  obs_end <- as.Date("2025-10-31")
  first_start <- obs_start - as.integer(.stp_draw(spine_id, seed, 1L) %% 1825L)
  anchor_min <- obs_start + 180L
  anchor_max <- obs_end - 300L
  anchor <- anchor_min +
    as.integer(.stp_draw(spine_id, seed, 2L) %% as.integer(anchor_max - anchor_min))
  profile <- as.integer(.stp_draw(spine_id, seed, 3L) %% 100L)

  first_bn <- .stp_job_business_id(spine_id, seed, 1L)
  if (profile < 30L) {
    return(data.frame(job_no = 1L, bn = first_bn, start = first_start,
                      end = as.Date(NA), stringsAsFactors = FALSE))
  }
  if (profile >= 92L && profile < 97L) {
    return(data.frame(job_no = 1L, bn = first_bn, start = first_start,
                      end = anchor, stringsAsFactors = FALSE))
  }
  if (profile < 82L) {
    gap_days <- if (profile < 42L) {
      0L
    } else if (profile < 64L) {
      .stp_labour_gap_days(.stp_draw(spine_id, seed, 4L))
    } else if (profile < 74L) {
      .stp_labour_gap_days(.stp_draw(spine_id, seed, 14L))
    } else {
      .stp_overlap_gap_days(.stp_draw(spine_id, seed, 5L))
    }
    second_bn <- if (profile >= 64L && profile < 74L) {
      first_bn
    } else {
      .stp_job_business_id(spine_id, seed, 2L)
    }
    return(data.frame(
      job_no = c(1L, 2L),
      bn = c(first_bn, second_bn),
      start = c(first_start, anchor + gap_days + 1L),
      end = c(anchor, as.Date(NA)),
      stringsAsFactors = FALSE
    ))
  }

  gap1 <- .stp_labour_gap_days(.stp_draw(spine_id, seed, 24L))
  second_start <- anchor + gap1 + 1L
  second_len <- 90L + as.integer(.stp_draw(spine_id, seed, 25L) %% 300L)
  second_end <- min(second_start + second_len, obs_end - 90L)
  gap2 <- if (profile >= 97L) {
    .stp_overlap_gap_days(.stp_draw(spine_id, seed, 26L))
  } else {
    .stp_labour_gap_days(.stp_draw(spine_id, seed, 26L))
  }
  third_start <- second_end + gap2 + 1L
  jobs <- data.frame(
    job_no = c(1L, 2L, 3L),
    bn = c(first_bn,
           .stp_job_business_id(spine_id, seed, 2L),
           .stp_job_business_id(spine_id, seed, 3L)),
    start = c(first_start, second_start, third_start),
    end = c(anchor, second_end, as.Date(NA)),
    stringsAsFactors = FALSE
  )
  if (profile >= 97L) {
    third_len <- 60L + as.integer(.stp_draw(spine_id, seed, 27L) %% 180L)
    third_end <- min(third_start + third_len, obs_end - 30L)
    gap3 <- .stp_labour_gap_days(.stp_draw(spine_id, seed, 28L))
    jobs$end[3L] <- third_end
    jobs <- rbind(jobs, data.frame(
      job_no = 4L,
      bn = .stp_job_business_id(spine_id, seed, 4L),
      start = third_end + gap3 + 1L,
      end = as.Date(NA),
      stringsAsFactors = FALSE
    ))
  }
  return(jobs)
}

.stp_empty_standard_event_frame <- function() {
  data.frame(
    ALWNC_INCM_TOTL_AMT = numeric(0),
    BIRTH_YEAR_MONTH_ABS = character(0),
    BN = character(0),
    BRANCH_NUMBER = integer(0),
    CNTRCTR_BN = character(0),
    DRVPAYENDDATE = as.Date(character(0)),
    DRVPAYSTARTDATE = as.Date(character(0)),
    DUMMY_FLAG = character(0),
    EMPLOYEE_KEY = character(0),
    PMT_DT = as.Date(character(0)),
    PMT_SUMRY_TOTL_GRS_PMT_AMT = numeric(0),
    PYRL_FNCL_YR = character(0),
    PYR_PYE_RLTNSHP_CESTN_DT = as.Date(character(0)),
    PYR_PYE_RLTNSHP_CMNCMT_DT = as.Date(character(0)),
    PYR_SPNTN_CNTRBTN_RPRTBL_AMT = numeric(0),
    SA2_ASGS_2021 = character(0),
    SEQUENCE_KEY = character(0),
    SG_EMPLR_CNTRBTN_AMT = numeric(0),
    STATE_ASGS_2021 = integer(0),
    SYNTHETIC_AEUID = character(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.stp_location_lookup_rows <- function(rows, seed) {
  n <- nrow(rows)
  lookup <- .load_mb_lookup()
  if (n == 0L) {
    return(lookup[0L, , drop = FALSE])
  }

  states <- as.integer(rows$state)
  states[is.na(states)] <- 1L
  states <- pmin(pmax(states, 1L), 8L)
  row_key <- .stp_spine_number(rows$spine_id)
  row_key[is.na(row_key)] <- seq_len(sum(is.na(row_key)))
  selected <- integer(n)

  for (st in sort(unique(states))) {
    idx <- which(states == st)
    pool <- which(lookup$state == st)
    if (!length(pool)) {
      stop("No Mesh Block lookup rows for state ", st, call. = FALSE)
    }
    pick <- as.integer(
      (row_key[idx] + as.numeric(seq_along(idx)) * 2654435761 +
         seed * 1009 + st * 9176) %% length(pool)
    ) + 1L
    selected[idx] <- pool[pick]
  }

  lookup[selected, , drop = FALSE]
}

.stp_labour_contractor_bn <- function(spine_id, seed, year, month, job_no,
                                      pay_seq, gross, correction) {
  if (correction || gross <= 0) return(NA_character_)
  draw <- .stp_payment_draw(spine_id, seed, year, month, job_no, pay_seq,
                            salt = 150L)
  if (draw %% 29L != 0L) return(NA_character_)
  spine_num <- .stp_spine_number(spine_id) + 88003 + job_no * 17231 +
    draw %% 97L
  .bn_for_hash_r(spine_num, .stp_business_id(sprintf("SPINE%012.0f", spine_num),
                                             seed + 555L, year))
}

.stp_standard_event_frame <- function(rows, seed, year, month) {
  n <- nrow(rows)
  fy_end <- .stp_fy_end(year, month)
  sg_rate <- .stp_sg_rate(fy_end)
  pmt_start <- as.Date(sprintf("%d-%02d-01", year, month))
  pmt_end <- seq(pmt_start, length = 2L, by = "1 month")[2L] - 1L
  location_rows <- .stp_location_lookup_rows(rows, seed + year * 100L + month)

  out <- vector("list", n * 8L)
  lump_sum_a_out <- numeric(n * 8L)
  lump_sum_a_code_out <- rep(NA_character_, n * 8L)
  k <- 0L
  for (i in seq_len(n)) {
    spine_id <- rows$spine_id[i]
    spine_num <- .stp_spine_number(spine_id)
    annual_income <- max(as.numeric(rows$baseline_income[i]), 0)
    if (annual_income <= 0) next
    jobs <- .stp_job_history(spine_id, seed)
    active <- jobs[jobs$start <= pmt_end & (is.na(jobs$end) | jobs$end >= pmt_start), ,
                   drop = FALSE]
    if (nrow(active) == 0L) next

    for (j in seq_len(nrow(active))) {
      job <- active[j, , drop = FALSE]
      row_serial <- i * 10L + job$job_no
      cadence <- .stp_pay_cadence(spine_id, seed, job$job_no)
      periods <- .stp_pay_periods(spine_id, seed, year, month, job,
                                  pmt_start, pmt_end)
      next_month <- .stp_next_dil_month(year, month)
      next_job_active <- FALSE
      if (!is.null(next_month)) {
        next_bounds <- .stp_month_bounds(next_month[["year"]], next_month[["month"]])
        next_job_active <- job$start <= next_bounds$end &&
          (is.na(job$end) || job$end >= next_bounds$start)
      }
      bn <- job$bn
      employee_key <- .stp_employee_key(rows$aeuid_ato[i], bn)

      for (p in seq_len(nrow(periods))) {
        period <- periods[p, , drop = FALSE]
        base_gross <- .stp_pay_event_gross(
          annual_income, period, spine_id, seed, year, month,
          job$job_no, cadence
        )
        irregular_gross <- .stp_irregular_gross_amount(
          base_gross, spine_id, seed, year, month,
          job$job_no, period$seq_no
        )
        gross_before_overpayment <- round(base_gross + irregular_gross, 2)
        overpayment_extra <- if (next_job_active &&
            .stp_is_overpayment_event(spine_id, seed, year, month,
                                      job$job_no, period$seq_no)) {
          .stp_overpayment_extra(
            gross_before_overpayment,
            .stp_payment_draw(spine_id, seed, year, month, job$job_no,
                              period$seq_no, salt = 2L)
          )
        } else {
          0
        }
        gross <- round(gross_before_overpayment + overpayment_extra, 2)
        allowance <- round(gross * ifelse(.stp_draw(spine_id, seed, 10L + job$job_no) %% 5L == 0L, 0.04, 0), 2)
        reportable_super <- round(gross * ifelse(.stp_draw(spine_id, seed, 20L + job$job_no) %% 10L == 0L, 0.05, 0), 2)
        cessation <- if (!is.na(job$end) && job$end >= period$start && job$end <= period$end) {
          job$end
        } else {
          as.Date(NA)
        }
        lump_sum_a <- .stp_leave_lump_sum_a_amount(
          gross_before_overpayment, spine_id, seed, year, month,
          job$job_no, period$seq_no, cessation
        )
        lump_sum_a_code <- .stp_leave_lump_sum_a_code(
          spine_id, seed, year, month, job$job_no, period$seq_no,
          lump_sum_a
        )
        sequence_suffix <- if (overpayment_extra > 0) "_OP" else ""
        contractor_bn <- .stp_labour_contractor_bn(
          spine_id, seed, year, month, job$job_no, period$seq_no,
          gross, correction = FALSE
        )

        k <- k + 1L
        lump_sum_a_out[k] <- lump_sum_a
        lump_sum_a_code_out[k] <- lump_sum_a_code
        out[[k]] <- data.frame(
          ALWNC_INCM_TOTL_AMT = allowance,
          BIRTH_YEAR_MONTH_ABS = sprintf("%04d%02d", rows$birth_year[i],
                                         ((spine_num + seed) %% 12L) + 1L),
          BN = bn,
          BRANCH_NUMBER = as.integer((spine_num + seed + job$job_no) %% 25L),
          CNTRCTR_BN = contractor_bn,
          DRVPAYENDDATE = period$end,
          DRVPAYSTARTDATE = period$start,
          DUMMY_FLAG = "False",
          EMPLOYEE_KEY = employee_key,
          PMT_DT = period$pay_date,
          PMT_SUMRY_TOTL_GRS_PMT_AMT = gross,
          PYRL_FNCL_YR = .stp_fy_label(year, month),
          PYR_PYE_RLTNSHP_CESTN_DT = cessation,
          PYR_PYE_RLTNSHP_CMNCMT_DT = job$start,
          PYR_SPNTN_CNTRBTN_RPRTBL_AMT = reportable_super,
          SA2_ASGS_2021 = location_rows$sa2_code[i],
          SEQUENCE_KEY = paste0(employee_key, "_", year, sprintf("%02d", month),
                                "_J", job$job_no, "_P", sprintf("%02d", period$seq_no),
                                sequence_suffix),
          SG_EMPLR_CNTRBTN_AMT = round(gross * sg_rate, 2),
          STATE_ASGS_2021 = as.integer(rows$state[i]),
          SYNTHETIC_AEUID = as.character(rows$aeuid_ato[i]),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }

      prev_month <- .stp_prev_dil_month(year, month)
      if (!is.null(prev_month)) {
        prev_bounds <- .stp_month_bounds(prev_month[["year"]], prev_month[["month"]])
        if (job$start <= prev_bounds$end &&
            (is.na(job$end) || job$end >= prev_bounds$start)) {
          prev_cadence <- .stp_pay_cadence(spine_id, seed, job$job_no)
          prev_periods <- .stp_pay_periods(spine_id, seed, prev_month[["year"]],
                                           prev_month[["month"]], job,
                                           prev_bounds$start, prev_bounds$end)
          correction_pay_date <- if (nrow(periods) > 0L) periods$pay_date[1L] else pmt_end
          for (pp in seq_len(nrow(prev_periods))) {
            prev_period <- prev_periods[pp, , drop = FALSE]
            if (!.stp_is_overpayment_event(spine_id, seed, prev_month[["year"]],
                                           prev_month[["month"]], job$job_no,
                                           prev_period$seq_no)) next
            prev_base_gross <- .stp_pay_event_gross(
              annual_income, prev_period, spine_id, seed,
              prev_month[["year"]], prev_month[["month"]],
              job$job_no, prev_cadence
            )
            prev_irregular_gross <- .stp_irregular_gross_amount(
              prev_base_gross, spine_id, seed,
              prev_month[["year"]], prev_month[["month"]],
              job$job_no, prev_period$seq_no
            )
            correction <- -.stp_overpayment_extra(
              round(prev_base_gross + prev_irregular_gross, 2),
              .stp_payment_draw(spine_id, seed, prev_month[["year"]],
                                prev_month[["month"]], job$job_no,
                                prev_period$seq_no, salt = 2L)
            )

            k <- k + 1L
            lump_sum_a_out[k] <- 0
            lump_sum_a_code_out[k] <- NA_character_
            out[[k]] <- data.frame(
              ALWNC_INCM_TOTL_AMT = 0,
              BIRTH_YEAR_MONTH_ABS = sprintf("%04d%02d", rows$birth_year[i],
                                             ((spine_num + seed) %% 12L) + 1L),
              BN = bn,
              BRANCH_NUMBER = as.integer((spine_num + seed + job$job_no) %% 25L),
              CNTRCTR_BN = NA_character_,
              DRVPAYENDDATE = prev_period$end,
              DRVPAYSTARTDATE = prev_period$start,
              DUMMY_FLAG = "False",
              EMPLOYEE_KEY = employee_key,
              PMT_DT = correction_pay_date,
              PMT_SUMRY_TOTL_GRS_PMT_AMT = correction,
              PYRL_FNCL_YR = .stp_fy_label(year, month),
              PYR_PYE_RLTNSHP_CESTN_DT = as.Date(NA),
              PYR_PYE_RLTNSHP_CMNCMT_DT = job$start,
              PYR_SPNTN_CNTRBTN_RPRTBL_AMT = 0,
              SA2_ASGS_2021 = location_rows$sa2_code[i],
              SEQUENCE_KEY = paste0(employee_key, "_", year, sprintf("%02d", month),
                                    "_J", job$job_no, "_P", sprintf("%02d", prev_period$seq_no),
                                    "_CORR", prev_month[["year"]],
                                    sprintf("%02d", prev_month[["month"]]),
                                    "P", sprintf("%02d", prev_period$seq_no)),
              SG_EMPLR_CNTRBTN_AMT = round(correction * sg_rate, 2),
              STATE_ASGS_2021 = as.integer(rows$state[i]),
              SYNTHETIC_AEUID = as.character(rows$aeuid_ato[i]),
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        }
      }
    }
  }
  if (k == 0L) {
    empty <- .stp_empty_standard_event_frame()
    attr(empty, "stp_lump_sum_a_amount") <- numeric(0)
    attr(empty, "stp_lump_sum_a_code") <- character(0)
    return(empty)
  }
  frame <- do.call(rbind, out[seq_len(k)])
  attr(frame, "stp_lump_sum_a_amount") <- lump_sum_a_out[seq_len(k)]
  attr(frame, "stp_lump_sum_a_code") <- lump_sum_a_code_out[seq_len(k)]
  frame
}

.stp_extended_event_frame <- function(rows, seed, year, month) {
  out <- .stp_standard_event_frame(rows, seed, year, month)
  location_rows <- .stp_location_lookup_rows(rows, seed + year * 100L + month)
  mesh_by_aeuid <- stats::setNames(location_rows$mb_code,
                                   as.character(rows$aeuid_ato))
  meshblock <- unname(mesh_by_aeuid[as.character(out$SYNTHETIC_AEUID)])
  gross <- out$PMT_SUMRY_TOTL_GRS_PMT_AMT
  positive_payment <- gross > 0
  lump_sum_a <- attr(out, "stp_lump_sum_a_amount", exact = TRUE)
  if (is.null(lump_sum_a) || length(lump_sum_a) != nrow(out)) {
    lump_sum_a <- numeric(nrow(out))
  }
  lump_sum_a <- ifelse(positive_payment, lump_sum_a, 0)
  lump_sum_a_code <- attr(out, "stp_lump_sum_a_code", exact = TRUE)
  if (is.null(lump_sum_a_code) || length(lump_sum_a_code) != nrow(out)) {
    lump_sum_a_code <- rep(NA_character_, nrow(out))
  }
  lump_sum_a_code <- ifelse(lump_sum_a > 0, lump_sum_a_code, NA_character_)
  withheld <- ifelse(
    gross < 0,
    round(gross * 0.25, 2),
    round(pmax(gross + lump_sum_a - 18200 / 12, 0) * 0.25, 2)
  )
  signal_draw <- (seq_len(nrow(out)) * 1103515245 + seed * 12345 +
                    year * 1000003 + month * 9176) %% 2147483647
  labour_on <- positive_payment
  labour_hire <- ifelse(labour_on & signal_draw %% 17L == 0L,
                        round(gross * 0.35, 2), 0)
  whm <- ifelse(labour_on & signal_draw %% 23L == 0L,
                round(gross * 0.25, 2), 0)
  voluntary <- ifelse(labour_on & signal_draw %% 37L == 0L,
                      round(gross * 0.20, 2), 0)
  community <- ifelse(labour_on & signal_draw %% 97L == 0L,
                      round(gross * 0.30, 2), 0)

  extra <- data.frame(
    ANL_LNG_SRVC_UNSD_LS_A_AMT = lump_sum_a,
    ANL_LNG_SRVC_UNSD_LS_A_CD = lump_sum_a_code,
    ANL_LNG_SRVC_UNSD_LS_B_AMT = 0,
    ANL_LNG_SRVC_UNSD_LS_D_AMT = 0,
    ANL_LNG_SRVC_UNSD_LS_E_AMT = 0,
    F_EMPLT_INCM_GRS_AMT = 0,
    F_EMPLT_INCM_JPDA_GRS_AMT = 0,
    F_EMPLT_INCM_JPDA_TOTL_AMT = 0,
    F_EMPLT_INCM_TAX_CR_WHELD_AMT = 0,
    F_EMPLT_INCM_TAX_PMT_AMT = 0,
    F_EMPLT_INCM_TOTL_AMT = 0,
    F_INCM_EXMT_AMT = 0,
    IDV_WRKPLC_GVING_TOTL_AMT = ifelse(positive_payment & (seq_len(nrow(out)) + seed) %% 50L == 0L, 10, 0),
    INCM_CMNTY_DEV_EMPLT_PRJCT_AMT = community,
    INCM_GRS_AMT = gross,
    INCM_LABR_HIR_ARNGMT_GRS_AMT = labour_hire,
    INCM_OTHR_AMT = 0,
    INCM_VA_GRS_AMT = voluntary,
    INCM_WHM_GRS_AMT = whm,
    LABR_HIR_TOTL_AMT = labour_hire,
    LGA_ASGS_2022 = sprintf("%05d", 10000L + (seq_len(nrow(out)) %% 80000L)),
    OTHR_SPCFD_TOTL_AMT = 0,
    PYR_PYE_RLTNSHP_TRMNTD_IND = as.integer(!is.na(out$PYR_PYE_RLTNSHP_CESTN_DT)),
    RFB_EXMT_AMT = 0,
    RFB_TXBL_AMT = 0,
    STP_MESHBLOCK_ABS = meshblock,
    TAX_WHELD_TOTL_AMT = withheld,
    VA_TOTL_AMT = voluntary,
    WHELD_AMT = withheld,
    WHM_TOTL_AMT = whm,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  target_order <- c(
    "ALWNC_INCM_TOTL_AMT", "ANL_LNG_SRVC_UNSD_LS_A_AMT",
    "ANL_LNG_SRVC_UNSD_LS_A_CD", "ANL_LNG_SRVC_UNSD_LS_B_AMT",
    "ANL_LNG_SRVC_UNSD_LS_D_AMT", "ANL_LNG_SRVC_UNSD_LS_E_AMT",
    "BIRTH_YEAR_MONTH_ABS", "BN", "BRANCH_NUMBER", "CNTRCTR_BN",
    "DRVPAYENDDATE", "DRVPAYSTARTDATE", "DUMMY_FLAG", "EMPLOYEE_KEY",
    "F_EMPLT_INCM_GRS_AMT", "F_EMPLT_INCM_JPDA_GRS_AMT",
    "F_EMPLT_INCM_JPDA_TOTL_AMT", "F_EMPLT_INCM_TAX_CR_WHELD_AMT",
    "F_EMPLT_INCM_TAX_PMT_AMT", "F_EMPLT_INCM_TOTL_AMT",
    "F_INCM_EXMT_AMT", "IDV_WRKPLC_GVING_TOTL_AMT",
    "INCM_CMNTY_DEV_EMPLT_PRJCT_AMT", "INCM_GRS_AMT",
    "INCM_LABR_HIR_ARNGMT_GRS_AMT", "INCM_OTHR_AMT", "INCM_VA_GRS_AMT",
    "INCM_WHM_GRS_AMT", "LABR_HIR_TOTL_AMT", "LGA_ASGS_2022",
    "OTHR_SPCFD_TOTL_AMT", "PMT_DT", "PMT_SUMRY_TOTL_GRS_PMT_AMT",
    "PYRL_FNCL_YR", "PYR_PYE_RLTNSHP_CESTN_DT",
    "PYR_PYE_RLTNSHP_CMNCMT_DT", "PYR_PYE_RLTNSHP_TRMNTD_IND",
    "PYR_SPNTN_CNTRBTN_RPRTBL_AMT", "RFB_EXMT_AMT", "RFB_TXBL_AMT",
    "SA2_ASGS_2021", "SEQUENCE_KEY", "SG_EMPLR_CNTRBTN_AMT",
    "STATE_ASGS_2021", "STP_MESHBLOCK_ABS", "SYNTHETIC_AEUID",
    "TAX_WHELD_TOTL_AMT", "VA_TOTL_AMT", "WHELD_AMT", "WHM_TOTL_AMT"
  )
  combined <- cbind(out, extra)
  combined[, target_order, drop = FALSE]
}

.write_stp_pay_events <- function(spine, employed_idx, run_dir, seed,
                                  year, month, extended, chunk_size,
                                  wage_by_key = numeric(0)) {
  product_name <- .stp_table_name(
    if (extended) "extended_pay_events" else "standard_pay_events",
    year, month
  )
  chunks <- .stp_chunks(employed_idx, chunk_size)
  fy <- .stp_fy_end(year, month)

  if (exists("write_stp_dil_pay_events_to_parquet__", mode = "function")) {
    chunk_dir <- file.path(dataset_dir(run_dir, "STP"), product_name)
    if (!dir.exists(chunk_dir)) dir.create(chunk_dir, recursive = TRUE)
    total <- 0L
    for (i in seq_along(chunks)) {
      rows <- spine[chunks[[i]], , drop = FALSE]
      location_rows <- .stp_location_lookup_rows(
        rows,
        seed + year * 100L + month
      )
      # Reconciled per-person FY wage total; missing key => not employed in the
      # panel this FY => 0 => no pay events (consistent with no payment summary).
      panel_fy_gross <- if (length(wage_by_key)) {
        g <- unname(wage_by_key[paste0(rows$aeuid_ato, "_", fy)])
        g[is.na(g)] <- 0
        as.numeric(g)
      } else {
        rep(-1, nrow(rows))  # panel unavailable: fall back to spine baseline
      }
      path <- file.path(chunk_dir, sprintf("part-%03d.parquet", i))
      total <- total + write_stp_dil_pay_events_to_parquet__(
        spine_id        = as.character(rows$spine_id),
        aeuid_ato       = as.character(rows$aeuid_ato),
        birth_year      = as.integer(rows$birth_year),
        state           = as.integer(rows$state),
        baseline_income = as.numeric(rows$baseline_income),
        sa2_asgs_2021   = as.character(location_rows$sa2_code),
        stp_meshblock_abs = as.character(location_rows$mb_code),
        seed            = as.integer(seed),
        year            = as.integer(year),
        month           = as.integer(month),
        extended        = as.logical(extended),
        panel_fy_gross  = panel_fy_gross,
        out_path        = path
      )
    }
    message("Wrote ", product_name, " (", length(chunks), " parts, ",
            format(nrow(spine[chunks[[length(chunks)]], , drop = FALSE]),
                   big.mark = ","), " rows in last) to ", chunk_dir)
    return(total)
  }

  total <- 0L
  for (i in seq_along(chunks)) {
    rows <- spine[chunks[[i]], , drop = FALSE]
    frame <- if (extended) {
      .stp_extended_event_frame(rows, seed, year, month)
    } else {
      .stp_standard_event_frame(rows, seed, year, month)
    }
    write_product_chunk(frame, product_name, "STP", run_dir,
                        format = "parquet", chunk_idx = i,
                        n_chunks = length(chunks))
    total <- total + nrow(frame)
  }
  total
}

.stp_jobs_frame <- function(rows, seed, fy_end) {
  n <- nrow(rows)
  fy_start <- as.Date(sprintf("%d-07-01", fy_end - 1L))
  fy_end_date <- as.Date(sprintf("%d-06-30", fy_end))
  out <- vector("list", n * 2L)
  k <- 0L
  for (i in seq_len(n)) {
    jobs <- .stp_job_history(rows$spine_id[i], seed)
    active <- jobs[jobs$start <= fy_end_date & (is.na(jobs$end) | jobs$end >= fy_start), ,
                   drop = FALSE]
    if (nrow(active) == 0L) next
    for (j in seq_len(nrow(active))) {
      job <- active[j, , drop = FALSE]
      cessation <- if (!is.na(job$end) && job$end >= fy_start && job$end <= fy_end_date) {
        job$end
      } else {
        as.Date(NA)
      }
      k <- k + 1L
      out[[k]] <- data.frame(
        BN = job$bn,
        PYRL_FNCL_YR = sprintf("%d-%02d", fy_end - 1L, fy_end %% 100L),
        PYR_PYE_RLTNSHP_CESTN_DT = cessation,
        PYR_PYE_RLTNSHP_CMNCMT_DT = job$start,
        SYNTHETIC_AEUID = as.character(rows$aeuid_ato[i]),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  if (k == 0L) {
    return(data.frame(
      BN = character(0),
      PYRL_FNCL_YR = character(0),
      PYR_PYE_RLTNSHP_CESTN_DT = as.Date(character(0)),
      PYR_PYE_RLTNSHP_CMNCMT_DT = as.Date(character(0)),
      SYNTHETIC_AEUID = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  do.call(rbind, out[seq_len(k)])
}

.write_stp_jobs <- function(spine, employed_idx, run_dir, seed, fy_end,
                            extended, chunk_size) {
  product_name <- .stp_fy_table_name(
    if (extended) "extended_jobs" else "standard_jobs", fy_end
  )
  chunks <- .stp_chunks(employed_idx, chunk_size)

  if (exists("write_stp_dil_jobs_to_parquet__", mode = "function")) {
    chunk_dir <- file.path(dataset_dir(run_dir, "STP"), product_name)
    if (!dir.exists(chunk_dir)) dir.create(chunk_dir, recursive = TRUE)
    total <- 0L
    for (i in seq_along(chunks)) {
      rows <- spine[chunks[[i]], , drop = FALSE]
      path <- file.path(chunk_dir, sprintf("part-%03d.parquet", i))
      total <- total + write_stp_dil_jobs_to_parquet__(
        spine_id  = as.character(rows$spine_id),
        aeuid_ato = as.character(rows$aeuid_ato),
        seed      = as.integer(seed),
        fy_end    = as.integer(fy_end),
        out_path  = path
      )
    }
    message("Wrote ", product_name, " (", length(chunks), " parts, ",
            format(nrow(spine[chunks[[length(chunks)]], , drop = FALSE]),
                   big.mark = ","), " rows in last) to ", chunk_dir)
    return(total)
  }

  total <- 0L
  for (i in seq_along(chunks)) {
    rows <- spine[chunks[[i]], , drop = FALSE]
    frame <- .stp_jobs_frame(rows, seed, fy_end)
    write_product_chunk(frame, product_name, "STP", run_dir,
                        format = "parquet", chunk_idx = i,
                        n_chunks = length(chunks))
    total <- total + nrow(frame)
  }
  total
}

.stp_etp_frame <- function(rows, seed, fy_end) {
  n <- nrow(rows)
  fy_start <- as.Date(sprintf("%d-07-01", fy_end - 1L))
  fy_end_date <- as.Date(sprintf("%d-06-30", fy_end))
  out <- vector("list", n)
  k <- 0L
  for (i in seq_len(n)) {
    jobs <- .stp_job_history(rows$spine_id[i], seed)
    ended <- jobs[!is.na(jobs$end) & jobs$end >= fy_start & jobs$end <= fy_end_date, ,
                  drop = FALSE]
    if (nrow(ended) == 0L) next
    for (j in seq_len(nrow(ended))) {
      job <- ended[j, , drop = FALSE]
      etp_modulus <- 3L
      if (.stp_draw(rows$spine_id[i], seed, 70L + job$job_no) %% etp_modulus != 0L) next
      taxable <- round(max(as.numeric(rows$baseline_income[i]), 0) * 0.08, 2)
      bn <- job$bn
      employee_key <- .stp_employee_key(rows$aeuid_ato[i], bn)
      etp_date <- min(job$end + as.integer(.stp_draw(rows$spine_id[i], seed, 80L + job$job_no) %% 31L),
                      fy_end_date)
      k <- k + 1L
      out[[k]] <- data.frame(
        BN = bn,
        DUMMY_FLAG = "False",
        EMPLOYEE_KEY = employee_key,
        ETP_PMT_DT = etp_date,
        ETP_PMT_TYP_CD = "R",
        ETP_TAX_FREE_AMT = round(taxable * 0.2, 2),
        ETP_TAX_WHELD_TOTL_AMT = round(taxable * 0.22, 2),
        ETP_TXBL_CMPNT_AMT = taxable,
        LATEST = TRUE,
        PYRL_FNCL_YR = sprintf("%d-%02d", fy_end - 1L, fy_end %% 100L),
        SEQUENCE_KEY = paste0(employee_key, "_ETP_", fy_end, "_J", job$job_no),
        SYNTHETIC_AEUID = as.character(rows$aeuid_ato[i]),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  if (k == 0L) {
    rows <- rows[integer(0), , drop = FALSE]
    return(data.frame(
      BN = character(0),
      DUMMY_FLAG = character(0),
      EMPLOYEE_KEY = character(0),
      ETP_PMT_DT = as.Date(character(0)),
      ETP_PMT_TYP_CD = character(0),
      ETP_TAX_FREE_AMT = numeric(0),
      ETP_TAX_WHELD_TOTL_AMT = numeric(0),
      ETP_TXBL_CMPNT_AMT = numeric(0),
      LATEST = logical(0),
      PYRL_FNCL_YR = character(0),
      SEQUENCE_KEY = character(0),
      SYNTHETIC_AEUID = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  do.call(rbind, out[seq_len(k)])
}

.write_stp_etp <- function(spine, employed_idx, run_dir, seed, fy_end,
                           chunk_size) {
  product_name <- .stp_fy_table_name("extended_etp", fy_end)
  chunks <- .stp_chunks(employed_idx, chunk_size)

  if (exists("write_stp_dil_etp_to_parquet__", mode = "function")) {
    chunk_dir <- file.path(dataset_dir(run_dir, "STP"), product_name)
    if (!dir.exists(chunk_dir)) dir.create(chunk_dir, recursive = TRUE)
    total <- 0L
    last_n <- 0L
    for (i in seq_along(chunks)) {
      rows <- spine[chunks[[i]], , drop = FALSE]
      path <- file.path(chunk_dir, sprintf("part-%03d.parquet", i))
      last_n <- write_stp_dil_etp_to_parquet__(
        spine_id        = as.character(rows$spine_id),
        aeuid_ato       = as.character(rows$aeuid_ato),
        baseline_income = as.numeric(rows$baseline_income),
        seed            = as.integer(seed),
        fy_end          = as.integer(fy_end),
        out_path        = path
      )
      total <- total + last_n
    }
    message("Wrote ", product_name, " (", length(chunks), " parts, ",
            format(last_n, big.mark = ","), " rows in last) to ", chunk_dir)
    return(total)
  }

  total <- 0L
  for (i in seq_along(chunks)) {
    rows <- spine[chunks[[i]], , drop = FALSE]
    frame <- .stp_etp_frame(rows, seed, fy_end)
    write_product_chunk(frame, product_name, "STP", run_dir,
                        format = "parquet", chunk_idx = i,
                        n_chunks = length(chunks))
    total <- total + nrow(frame)
  }
  total
}

.read_stp_generated <- function(ds_dir) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to read generated STP parquet.",
         call. = FALSE)
  }
  product_dirs <- list.dirs(ds_dir, recursive = FALSE, full.names = TRUE)
  out <- list()
  for (dir in product_dirs) {
    parts <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(parts) == 0L) next
    out[[basename(dir)]] <- as.data.frame(
      arrow::open_dataset(dir, format = "parquet"),
      stringsAsFactors = FALSE
    )
  }
  out
}

use extendr_api::prelude::*;

use crate::mbs::days_since_epoch;

const STP_OPEN_END: i32 = i32::MAX;
const STP_OVERPAYMENT_RATE_DENOMINATOR: u64 = 2_000;

const STP_CADENCE_MONTHLY: i32 = 1;
const STP_CADENCE_FORTNIGHTLY: i32 = 2;
const STP_CADENCE_WEEKLY: i32 = 3;
const STP_CADENCE_CHAOTIC: i32 = 4;

#[derive(Clone)]
struct StpDilJob {
    job_no: i32,
    bn: String,
    start: i32,
    end: i32,
}

#[derive(Clone, Copy)]
struct StpPayPeriod {
    start: i32,
    end: i32,
    pay_date: i32,
    seq_no: i32,
    gross_days: i32,
}

fn stp_mix(mut x: u64) -> u64 {
    x = x.wrapping_add(0x9E37_79B9_7F4A_7C15);
    x = (x ^ (x >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^ (x >> 31)
}

fn stp_draw(spine_num: u64, seed: i64, salt: u64) -> u64 {
    stp_mix(
        spine_num
            ^ (seed as u64).wrapping_mul(0xD134_2543_DE82_EF95)
            ^ salt.wrapping_mul(0x9E37_79B9_7F4A_7C15),
    )
}

fn stp_payment_draw(
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job_no: i32,
    pay_seq: i32,
    salt: u64,
) -> u64 {
    stp_mix(
        spine_num
            ^ (seed as u64).wrapping_mul(0xD134_2543_DE82_EF95)
            ^ (year as u64).wrapping_mul(0x94D0_49BB_1331_11EB)
            ^ (month as u64).wrapping_mul(0xBF58_476D_1CE4_E5B9)
            ^ (job_no as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
            ^ (pay_seq as u64).wrapping_mul(0xD6E8_FEB8_6659_FD93)
            ^ salt.wrapping_mul(0xA24B_AED4_963E_E407),
    )
}

fn stp_is_overpayment_event(
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job_no: i32,
    pay_seq: i32,
) -> bool {
    stp_payment_draw(spine_num, seed, year, month, job_no, pay_seq, 1)
        % STP_OVERPAYMENT_RATE_DENOMINATOR
        == 0
}

fn stp_overpayment_extra(base_gross: f64, draw: u64) -> f64 {
    let rate = 0.10 + (draw % 21) as f64 / 100.0;
    round2(base_gross * rate)
}

fn stp_job_business_id(spine_num: u64, seed: i64, job_no: i32) -> String {
    // Share the per-person employer slots with PIT_PS so a person's primary
    // (job 1 -> slot 0) and secondary (job 2 -> SECONDARY_SLOT) employers are
    // the same BLADE business in both products. Falls back to the legacy
    // synthetic BN when no business pool is set.
    let slot = if job_no <= 1 {
        0
    } else {
        crate::business_pool::SECONDARY_SLOT
    };
    let pool = crate::business_pool::snapshot();
    crate::business_pool::employer_bn(&pool, spine_num as i64, slot, seed).unwrap_or_else(|| {
        stp_business_id(
            spine_num.wrapping_add((job_no as u64).wrapping_mul(7_919)),
            seed + (job_no as i64) * 100_003,
            2024,
        )
    })
}

fn stp_contractor_business_id(
    pool: &[String],
    spine_num: u64,
    seed: i64,
    year: i32,
    job_no: i32,
    contractor_draw: u64,
) -> String {
    let contractor_num = spine_num
        .wrapping_add(88_003)
        .wrapping_add((job_no as u64).wrapping_mul(17_231))
        .wrapping_add(contractor_draw % 97);
    crate::business_pool::bn_for_hash(pool, contractor_num as i64)
        .unwrap_or_else(|| stp_business_id(contractor_num, seed + 555, year))
}

fn stp_labour_gap_days(draw: u64) -> i32 {
    match draw % 8 {
        0 => 15,
        1 => 21,
        2 => 30,
        3 => 45,
        4 => 60,
        5 => 90,
        6 => 120,
        _ => 180,
    }
}

fn stp_overlap_gap_days(draw: u64) -> i32 {
    -match draw % 5 {
        0 => 1,
        1 => 7,
        2 => 14,
        3 => 21,
        _ => 30,
    }
}

/// Per-person STP job history: employer spells over the observation window,
/// including short and long between-job gaps, same-employer recalls, and
/// multi-job sequences.
fn stp_job_history(spine_num: u64, seed: i64) -> Vec<StpDilJob> {
    let obs_start = days_since_epoch(2020, 1, 1);
    let obs_end = days_since_epoch(2025, 10, 31);
    let first_start = obs_start - (stp_draw(spine_num, seed, 1) % 1_825) as i32;
    let anchor_min = obs_start + 180;
    let anchor_max = obs_end - 300;
    let anchor_span = (anchor_max - anchor_min).max(1);
    let anchor = anchor_min + (stp_draw(spine_num, seed, 2) % anchor_span as u64) as i32;
    let profile = (stp_draw(spine_num, seed, 3) % 100) as i32;

    let first_bn = stp_job_business_id(spine_num, seed, 1);
    if profile < 30 {
        return vec![StpDilJob {
            job_no: 1,
            bn: first_bn,
            start: first_start,
            end: STP_OPEN_END,
        }];
    }

    if profile >= 92 && profile < 97 {
        return vec![StpDilJob {
            job_no: 1,
            bn: first_bn,
            start: first_start,
            end: anchor,
        }];
    }

    if profile < 82 {
        let gap_days = if profile < 42 {
            0
        } else if profile < 64 {
            stp_labour_gap_days(stp_draw(spine_num, seed, 4))
        } else if profile < 74 {
            stp_labour_gap_days(stp_draw(spine_num, seed, 14))
        } else {
            stp_overlap_gap_days(stp_draw(spine_num, seed, 5))
        };
        let second_bn = if (64..74).contains(&profile) {
            first_bn.clone()
        } else {
            stp_job_business_id(spine_num, seed, 2)
        };
        return vec![
            StpDilJob {
                job_no: 1,
                bn: first_bn,
                start: first_start,
                end: anchor,
            },
            StpDilJob {
                job_no: 2,
                bn: second_bn,
                start: anchor + gap_days + 1,
                end: STP_OPEN_END,
            },
        ];
    }

    let gap1 = stp_labour_gap_days(stp_draw(spine_num, seed, 24));
    let second_start = anchor + gap1 + 1;
    let second_len = 90 + (stp_draw(spine_num, seed, 25) % 300) as i32;
    let second_end = (second_start + second_len).min(obs_end - 90);
    let gap2 = if profile >= 97 {
        stp_overlap_gap_days(stp_draw(spine_num, seed, 26))
    } else {
        stp_labour_gap_days(stp_draw(spine_num, seed, 26))
    };
    let third_start = second_end + gap2 + 1;

    let mut jobs = vec![
        StpDilJob {
            job_no: 1,
            bn: first_bn,
            start: first_start,
            end: anchor,
        },
        StpDilJob {
            job_no: 2,
            bn: stp_job_business_id(spine_num, seed, 2),
            start: second_start,
            end: second_end,
        },
        StpDilJob {
            job_no: 3,
            bn: stp_job_business_id(spine_num, seed, 3),
            start: third_start,
            end: STP_OPEN_END,
        },
    ];

    if profile >= 97 {
        let third_len = 60 + (stp_draw(spine_num, seed, 27) % 180) as i32;
        let third_end = (third_start + third_len).min(obs_end - 30);
        let gap3 = stp_labour_gap_days(stp_draw(spine_num, seed, 28));
        jobs[2].end = third_end;
        jobs.push(StpDilJob {
            job_no: 4,
            bn: stp_job_business_id(spine_num, seed, 4),
            start: third_end + gap3 + 1,
            end: STP_OPEN_END,
        });
    }
    jobs
}

fn stp_job_overlaps(job: &StpDilJob, start: i32, end: i32) -> bool {
    job.start <= end && job.end >= start
}

fn stp_spine_number(spine_id: &str) -> u64 {
    let mut out = 0u64;
    let mut seen_digit = false;
    for b in spine_id.bytes() {
        if b.is_ascii_digit() {
            seen_digit = true;
            let digit = (b - b'0') as u64;
            out = match out.checked_mul(10).and_then(|x| x.checked_add(digit)) {
                Some(x) => x,
                None => return 0,
            };
        }
    }
    if seen_digit {
        out
    } else {
        0
    }
}

/// Legacy synthetic 12-digit BN, used only as the no-pool fallback for
/// `stp_job_business_id`. When a business pool is set, employer identifiers
/// come from the shared `business_pool::employer_bn` scheme instead.
fn stp_business_id(spine_num: u64, seed: i64, year: i32) -> String {
    let modulus = 1_000_000_000_000i128;
    let raw = ((spine_num as i128) * 1_103_515_245i128 + seed as i128 + (year as i128) * 1009i128)
        .rem_euclid(modulus);
    format!("BN{:012}", raw)
}

fn stp_fy_end(year: i32, month: i32) -> i32 {
    if month >= 7 {
        year + 1
    } else {
        year
    }
}

fn stp_fy_label_from_end(fy_end: i32) -> String {
    format!("{}-{:02}", fy_end - 1, fy_end % 100)
}

fn stp_fy_label(year: i32, month: i32) -> String {
    stp_fy_label_from_end(stp_fy_end(year, month))
}

fn stp_dil_sg_rate(fy_end: i32) -> f64 {
    match fy_end {
        y if y <= 2021 => 0.095,
        2022 => 0.10,
        2023 => 0.105,
        2024 => 0.11,
        2025 => 0.115,
        _ => 0.12,
    }
}

fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

fn month_start_days(year: i32, month: i32) -> i32 {
    days_since_epoch(year, month as u32, 1)
}

fn month_end_days(year: i32, month: i32) -> i32 {
    let (next_year, next_month) = if month == 12 {
        (year + 1, 1)
    } else {
        (year, month + 1)
    };
    days_since_epoch(next_year, next_month as u32, 1) - 1
}

fn stp_next_dil_month(year: i32, month: i32) -> Option<(i32, i32)> {
    let (next_year, next_month) = if month == 12 {
        (year + 1, 1)
    } else {
        (year, month + 1)
    };
    if next_year > 2025 || (next_year == 2025 && next_month > 10) {
        None
    } else {
        Some((next_year, next_month))
    }
}

fn stp_prev_dil_month(year: i32, month: i32) -> Option<(i32, i32)> {
    let (prev_year, prev_month) = if month == 1 {
        (year - 1, 12)
    } else {
        (year, month - 1)
    };
    if prev_year < 2020 {
        None
    } else {
        Some((prev_year, prev_month))
    }
}

fn stp_pay_cadence(spine_num: u64, seed: i64, job_no: i32) -> i32 {
    match stp_draw(spine_num, seed, 90 + job_no as u64) % 100 {
        0..=69 => STP_CADENCE_FORTNIGHTLY,
        70..=91 => STP_CADENCE_MONTHLY,
        92..=95 => STP_CADENCE_WEEKLY,
        _ => STP_CADENCE_CHAOTIC,
    }
}

fn stp_is_variable_pay_profile(spine_num: u64, seed: i64, job_no: i32) -> bool {
    stp_draw(spine_num, seed, 100 + job_no as u64) % 100 < 30
}

fn stp_push_pay_period(
    out: &mut Vec<StpPayPeriod>,
    job: &StpDilJob,
    start: i32,
    end: i32,
    pay_date: i32,
    seq_no: i32,
    gross_day_cap: i32,
) {
    let row_start = start.max(job.start);
    let row_end = end.min(job.end);
    if row_start > row_end {
        return;
    }
    let actual_days = row_end - row_start + 1;
    out.push(StpPayPeriod {
        start: row_start,
        end: row_end,
        pay_date,
        seq_no,
        gross_days: actual_days.min(gross_day_cap).max(1),
    });
}

fn stp_add_regular_pay_periods(
    out: &mut Vec<StpPayPeriod>,
    spine_num: u64,
    seed: i64,
    job: &StpDilJob,
    month_start: i32,
    month_end: i32,
    interval_days: i32,
) {
    let offset = (stp_draw(spine_num, seed, 110 + job.job_no as u64) % interval_days as u64) as i32;
    let lag = (stp_draw(spine_num, seed, 111 + job.job_no as u64) % 4) as i32;
    let anchor = days_since_epoch(2020, 1, 1) - offset;
    let mut period_start = month_start - interval_days - 7;
    period_start -= (period_start - anchor).rem_euclid(interval_days);

    let mut seq_no = 1;
    while period_start <= month_end {
        let period_end = period_start + interval_days - 1;
        let pay_date = period_end + lag;
        if pay_date >= month_start && pay_date <= month_end {
            stp_push_pay_period(
                out,
                job,
                period_start,
                period_end,
                pay_date,
                seq_no,
                interval_days,
            );
            seq_no += 1;
        }
        period_start += interval_days;
    }
}

fn stp_add_chaotic_pay_periods(
    out: &mut Vec<StpPayPeriod>,
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job: &StpDilJob,
    month_start: i32,
    month_end: i32,
) {
    let month_days = month_end - month_start + 1;
    let count = 2 + (stp_payment_draw(spine_num, seed, year, month, job.job_no, 0, 10) % 3) as i32;
    let lengths = [3, 5, 7, 9, 14, 21, 28, 45, 60];
    for seq_no in 1..=count {
        let draw = stp_payment_draw(spine_num, seed, year, month, job.job_no, seq_no, 11);
        let len = lengths[(draw % lengths.len() as u64) as usize];
        let offset = (draw % (month_days as u64 + 12)) as i32 - 6;
        let overlap_shift = if seq_no > 1 && draw % 3 == 0 {
            len / 2
        } else {
            0
        };
        let period_start = month_start + offset - overlap_shift;
        let period_end = period_start + len - 1;
        let pay_date = month_start + ((draw / 17) % month_days as u64) as i32;
        stp_push_pay_period(out, job, period_start, period_end, pay_date, seq_no, 60);
    }
}

fn stp_add_fy_anchored_period(
    out: &mut Vec<StpPayPeriod>,
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job: &StpDilJob,
) {
    if month != 6 {
        return;
    }
    let draw = stp_payment_draw(spine_num, seed, year, month, job.job_no, 90, 12);
    if draw % 20 != 0 {
        return;
    }
    let fy_start = days_since_epoch(year - 1, 7, 1) + (draw % 31) as i32;
    let fy_end = days_since_epoch(year, 6, 30);
    stp_push_pay_period(out, job, fy_start, fy_end, fy_end, 90, 31);
}

fn stp_pay_periods(
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job: &StpDilJob,
    month_start: i32,
    month_end: i32,
) -> Vec<StpPayPeriod> {
    let cadence = stp_pay_cadence(spine_num, seed, job.job_no);
    let mut out = Vec::with_capacity(6);
    match cadence {
        STP_CADENCE_MONTHLY => {
            stp_push_pay_period(
                &mut out,
                job,
                month_start,
                month_end,
                month_end,
                1,
                month_end - month_start + 1,
            );
        }
        STP_CADENCE_WEEKLY => {
            stp_add_regular_pay_periods(&mut out, spine_num, seed, job, month_start, month_end, 7);
        }
        STP_CADENCE_CHAOTIC => {
            stp_add_chaotic_pay_periods(
                &mut out,
                spine_num,
                seed,
                year,
                month,
                job,
                month_start,
                month_end,
            );
        }
        _ => {
            stp_add_regular_pay_periods(&mut out, spine_num, seed, job, month_start, month_end, 14);
        }
    }

    stp_add_fy_anchored_period(&mut out, spine_num, seed, year, month, job);

    if out.is_empty() && stp_job_overlaps(job, month_start, month_end) {
        let start = month_start.max(job.start);
        let end = month_end.min(job.end);
        stp_push_pay_period(
            &mut out,
            job,
            start,
            end,
            month_end,
            1,
            month_end - month_start + 1,
        );
    }
    out.sort_by_key(|p| (p.pay_date, p.start, p.end, p.seq_no));
    out
}

fn stp_pay_variability_multiplier(
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job_no: i32,
    pay_seq: i32,
    cadence: i32,
) -> f64 {
    let draw = stp_payment_draw(spine_num, seed, year, month, job_no, pay_seq, 3);
    if cadence == STP_CADENCE_CHAOTIC {
        return match draw % 10 {
            0 => 0.12,
            1 => 0.30,
            2 => 0.55,
            3 => 0.85,
            4 => 1.15,
            5 => 1.55,
            6 => 2.10,
            7 => 2.80,
            8 => 3.60,
            _ => 4.50,
        };
    }
    if stp_is_variable_pay_profile(spine_num, seed, job_no) {
        return match draw % 9 {
            0 => 0.35,
            1 => 0.55,
            2 => 0.75,
            3 => 0.95,
            4 => 1.20,
            5 => 1.50,
            6 => 1.90,
            7 => 2.40,
            _ => 3.00,
        };
    }
    0.96 + (draw % 9) as f64 / 100.0
}

fn stp_pay_event_gross(
    annual_income: f64,
    period: &StpPayPeriod,
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job_no: i32,
    cadence: i32,
) -> f64 {
    let daily_base = annual_income.max(0.0) / 365.25;
    let multiplier = stp_pay_variability_multiplier(
        spine_num,
        seed,
        year,
        month,
        job_no,
        period.seq_no,
        cadence,
    );
    round2(daily_base * period.gross_days as f64 * multiplier)
}

fn stp_irregular_gross_amount(
    base_gross: f64,
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job_no: i32,
    pay_seq: i32,
) -> f64 {
    if base_gross <= 0.0 {
        return 0.0;
    }
    let draw = stp_payment_draw(spine_num, seed, year, month, job_no, pay_seq, 4);
    if month == 6 && draw % 4 == 0 {
        round2(base_gross * (0.50 + (draw % 51) as f64 / 100.0))
    } else if draw % 250 == 0 {
        round2(base_gross * 0.15)
    } else {
        0.0
    }
}

fn stp_leave_lump_sum_a_amount(
    base_gross: f64,
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job_no: i32,
    pay_seq: i32,
    cessation: i32,
) -> f64 {
    if base_gross <= 0.0 || cessation == i32::MIN {
        return 0.0;
    }
    let draw = stp_payment_draw(spine_num, seed, year, month, job_no, pay_seq, 5);
    if draw % 3 != 0 {
        return 0.0;
    }
    round2(base_gross * (0.35 + (draw % 66) as f64 / 100.0))
}

fn stp_leave_lump_sum_a_code(
    spine_num: u64,
    seed: i64,
    year: i32,
    month: i32,
    job_no: i32,
    pay_seq: i32,
    amount: f64,
) -> Option<String> {
    if amount <= 0.0 {
        return None;
    }
    let draw = stp_payment_draw(spine_num, seed, year, month, job_no, pay_seq, 6);
    Some(if draw % 5 == 0 { "R" } else { "T" }.to_string())
}

/// Write a 2026-DIL STP monthly pay-events chunk directly to parquet.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn write_stp_dil_pay_events_to_parquet__(
    spine_id: Strings,
    aeuid_ato: Strings,
    birth_year: &[i32],
    state: &[i32],
    baseline_income: &[f64],
    sa2_asgs_2021: Strings,
    stp_meshblock_abs: Strings,
    seed: i64,
    year: i32,
    month: i32,
    extended: bool,
    panel_fy_gross: &[f64],
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};

    let n = aeuid_ato.len();
    let spine_ids = spine_id.as_slice();
    let aeuid_vals = aeuid_ato.as_slice();
    let sa2_vals: Vec<String> = sa2_asgs_2021.iter().map(|s| s.to_string()).collect();
    let meshblock_vals: Vec<String> = stp_meshblock_abs.iter().map(|s| s.to_string()).collect();
    let fy_end = stp_fy_end(year, month);
    let fy_label = stp_fy_label(year, month);
    let sg_rate = stp_dil_sg_rate(fy_end);
    let pmt_start = month_start_days(year, month);
    let pmt_end = month_end_days(year, month);
    let estimated_rows = n.saturating_mul(4);

    let mut allowance = Vec::with_capacity(estimated_rows);
    let mut birth_year_month = Vec::with_capacity(estimated_rows);
    let mut bn = Vec::with_capacity(estimated_rows);
    let mut branch_number = Vec::with_capacity(estimated_rows);
    let mut contractor_bn: Vec<Option<String>> = Vec::with_capacity(estimated_rows);
    let mut drv_pay_end = Vec::with_capacity(estimated_rows);
    let mut drv_pay_start = Vec::with_capacity(estimated_rows);
    let mut dummy_flag = Vec::with_capacity(estimated_rows);
    let mut employee_key = Vec::with_capacity(estimated_rows);
    let mut pmt_dt = Vec::with_capacity(estimated_rows);
    let mut gross = Vec::with_capacity(estimated_rows);
    let mut payroll_fy = Vec::with_capacity(estimated_rows);
    let mut cessation = Vec::with_capacity(estimated_rows);
    let mut commencement = Vec::with_capacity(estimated_rows);
    let mut reportable_super = Vec::with_capacity(estimated_rows);
    let mut sa2 = Vec::with_capacity(estimated_rows);
    let mut sequence_key = Vec::with_capacity(estimated_rows);
    let mut sg = Vec::with_capacity(estimated_rows);
    let mut state_out = Vec::with_capacity(estimated_rows);
    let mut aeuid_out = Vec::with_capacity(estimated_rows);

    let mut long_service_a_amt = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut long_service_a_cd: Vec<Option<String>> =
        Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut withheld = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut workplace_giving = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut community_dev = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut labour_hire_arrangement = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut voluntary_agreement = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut whm_income = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut labour_hire_total = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut voluntary_agreement_total =
        Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut whm_total = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut lga = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut terminated_ind = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let mut meshblock = Vec::with_capacity(if extended { estimated_rows } else { 0 });
    let business_pool = crate::business_pool::snapshot();

    let mut push_pay_row = |i: usize,
                            spine_num: u64,
                            row_aeuid: &str,
                            job: &StpDilJob,
                            row_serial: i64,
                            row_gross: f64,
                            row_allowance: f64,
                            row_reportable_super: f64,
                            drv_start: i32,
                            drv_end: i32,
                            row_pmt_date: i32,
                            row_cessation: i32,
                            pay_seq: i32,
                            sequence_suffix: &str,
                            row_lump_sum_a: f64,
                            row_lump_sum_a_code: Option<String>,
                            correction: bool| {
        let row_bn = job.bn.clone();
        let row_employee_key = format!("{}_{}", row_aeuid, row_bn);
        let positive_payment = row_gross > 0.0 && !correction;
        let contractor_draw =
            stp_payment_draw(spine_num, seed, year, month, job.job_no, pay_seq, 150);
        let row_contractor_bn = if positive_payment && contractor_draw % 29 == 0 {
            Some(stp_contractor_business_id(
                &business_pool,
                spine_num,
                seed,
                year,
                job.job_no,
                contractor_draw,
            ))
        } else {
            None
        };

        allowance.push(row_allowance);
        birth_year_month.push(format!(
            "{:04}{:02}",
            birth_year[i],
            ((spine_num as i64 + seed).rem_euclid(12) + 1)
        ));
        bn.push(row_bn.clone());
        branch_number.push((spine_num as i64 + seed + job.job_no as i64).rem_euclid(25) as i32);
        contractor_bn.push(row_contractor_bn);
        drv_pay_end.push(drv_end);
        drv_pay_start.push(drv_start);
        dummy_flag.push("False".to_string());
        employee_key.push(row_employee_key.clone());
        pmt_dt.push(row_pmt_date);
        gross.push(row_gross);
        payroll_fy.push(fy_label.clone());
        cessation.push(row_cessation);
        commencement.push(job.start);
        reportable_super.push(row_reportable_super);
        sa2.push(sa2_vals[i].clone());
        sequence_key.push(format!(
            "{}_{:04}{:02}_J{}_P{:02}{}",
            row_employee_key, year, month, job.job_no, pay_seq, sequence_suffix
        ));
        sg.push(round2(row_gross * sg_rate));
        state_out.push(state[i]);
        aeuid_out.push(row_aeuid.to_string());

        if extended {
            let signal_draw =
                stp_payment_draw(spine_num, seed, year, month, job.job_no, pay_seq, 151);
            let signal_base = row_gross.max(0.0);
            let labour_hire = if positive_payment && signal_draw % 17 == 0 {
                round2(signal_base * (0.25 + (signal_draw % 16) as f64 / 100.0))
            } else {
                0.0
            };
            let whm = if positive_payment
                && birth_year[i] >= year - 35
                && signal_draw % 23 == 0
            {
                round2(signal_base * (0.20 + (signal_draw % 11) as f64 / 100.0))
            } else {
                0.0
            };
            let voluntary = if positive_payment && signal_draw % 37 == 0 {
                round2(signal_base * (0.15 + (signal_draw % 10) as f64 / 100.0))
            } else {
                0.0
            };
            let community = if positive_payment
                && ((state[i] == 7 && signal_draw % 43 == 0) || signal_draw % 211 == 0)
            {
                round2(signal_base * 0.30)
            } else {
                0.0
            };
            let row_withheld = if row_gross < 0.0 {
                round2(row_gross * 0.25)
            } else {
                round2((row_gross + row_lump_sum_a - 18_200.0 / 12.0).max(0.0) * 0.25)
            };
            long_service_a_amt.push(if correction { 0.0 } else { row_lump_sum_a });
            long_service_a_cd.push(if correction {
                None
            } else {
                row_lump_sum_a_code
            });
            withheld.push(row_withheld);
            workplace_giving.push(
                if !correction && stp_draw(spine_num, seed, 40 + job.job_no as u64) % 50 == 0 {
                    10.0
                } else {
                    0.0
                },
            );
            community_dev.push(community);
            labour_hire_arrangement.push(labour_hire);
            voluntary_agreement.push(voluntary);
            whm_income.push(whm);
            labour_hire_total.push(labour_hire);
            voluntary_agreement_total.push(voluntary);
            whm_total.push(whm);
            lga.push(format!("{:05}", 10_000i64 + row_serial.rem_euclid(80_000)));
            terminated_ind.push(if row_cessation == i32::MIN { 0 } else { 1 });
            meshblock.push(meshblock_vals[i].clone());
        }
    };

    for i in 0..n {
        let row_no = (i + 1) as i64;
        let spine_num = stp_spine_number(spine_ids[i].as_ref());
        let row_aeuid = aeuid_vals[i].as_ref();
        // Anchor STP income to the reconciled panel total for this person-FY (so
        // a person's STP pay events track the same wage as their PAYG payment
        // summaries and PIT salary line). 0 => not employed in the panel this FY
        // => no STP pays (consistent with no payment summary). Falls back to the
        // spine baseline only if the panel total is unavailable (negative).
        let annual_income = if panel_fy_gross[i] >= 0.0 {
            panel_fy_gross[i]
        } else {
            baseline_income[i].max(0.0)
        };
        if annual_income <= 0.0 {
            continue;
        }

        for job in stp_job_history(spine_num, seed) {
            if !stp_job_overlaps(&job, pmt_start, pmt_end) {
                continue;
            }

            let row_serial = row_no * 10 + job.job_no as i64;
            let cadence = stp_pay_cadence(spine_num, seed, job.job_no);
            let periods = stp_pay_periods(spine_num, seed, year, month, &job, pmt_start, pmt_end);
            let next_job_active =
                if let Some((next_year, next_month)) = stp_next_dil_month(year, month) {
                    let next_start = month_start_days(next_year, next_month);
                    let next_end = month_end_days(next_year, next_month);
                    stp_job_overlaps(&job, next_start, next_end)
                } else {
                    false
                };

            for period in &periods {
                let base_gross = stp_pay_event_gross(
                    annual_income,
                    period,
                    spine_num,
                    seed,
                    year,
                    month,
                    job.job_no,
                    cadence,
                );
                let irregular_gross = stp_irregular_gross_amount(
                    base_gross,
                    spine_num,
                    seed,
                    year,
                    month,
                    job.job_no,
                    period.seq_no,
                );
                let gross_before_overpayment = round2(base_gross + irregular_gross);
                let overpayment_extra = if next_job_active
                    && stp_is_overpayment_event(
                        spine_num,
                        seed,
                        year,
                        month,
                        job.job_no,
                        period.seq_no,
                    ) {
                    stp_overpayment_extra(
                        gross_before_overpayment,
                        stp_payment_draw(
                            spine_num,
                            seed,
                            year,
                            month,
                            job.job_no,
                            period.seq_no,
                            2,
                        ),
                    )
                } else {
                    0.0
                };
                let row_gross = round2(gross_before_overpayment + overpayment_extra);
                let row_allowance = if stp_draw(spine_num, seed, 10 + job.job_no as u64) % 5 == 0 {
                    round2(row_gross * 0.04)
                } else {
                    0.0
                };
                let row_reportable_super =
                    if stp_draw(spine_num, seed, 20 + job.job_no as u64) % 10 == 0 {
                        round2(row_gross * 0.05)
                    } else {
                        0.0
                    };
                let row_cessation = if job.end != STP_OPEN_END
                    && job.end >= period.start
                    && job.end <= period.end
                {
                    job.end
                } else {
                    i32::MIN
                };
                let lump_sum_a = stp_leave_lump_sum_a_amount(
                    gross_before_overpayment,
                    spine_num,
                    seed,
                    year,
                    month,
                    job.job_no,
                    period.seq_no,
                    row_cessation,
                );
                let lump_sum_a_code = stp_leave_lump_sum_a_code(
                    spine_num,
                    seed,
                    year,
                    month,
                    job.job_no,
                    period.seq_no,
                    lump_sum_a,
                );
                let sequence_suffix = if overpayment_extra > 0.0 { "_OP" } else { "" };

                push_pay_row(
                    i,
                    spine_num,
                    &row_aeuid,
                    &job,
                    row_serial * 100 + period.seq_no as i64,
                    row_gross,
                    row_allowance,
                    row_reportable_super,
                    period.start,
                    period.end,
                    period.pay_date,
                    row_cessation,
                    period.seq_no,
                    sequence_suffix,
                    lump_sum_a,
                    lump_sum_a_code,
                    false,
                );
            }

            if let Some((prev_year, prev_month)) = stp_prev_dil_month(year, month) {
                let prev_start = month_start_days(prev_year, prev_month);
                let prev_end = month_end_days(prev_year, prev_month);
                if stp_job_overlaps(&job, prev_start, prev_end) {
                    let prev_cadence = stp_pay_cadence(spine_num, seed, job.job_no);
                    let prev_periods = stp_pay_periods(
                        spine_num, seed, prev_year, prev_month, &job, prev_start, prev_end,
                    );
                    let correction_pay_date =
                        periods.first().map(|p| p.pay_date).unwrap_or(pmt_end);
                    for prev_period in &prev_periods {
                        if !stp_is_overpayment_event(
                            spine_num,
                            seed,
                            prev_year,
                            prev_month,
                            job.job_no,
                            prev_period.seq_no,
                        ) {
                            continue;
                        }
                        let prev_base_gross = stp_pay_event_gross(
                            annual_income,
                            prev_period,
                            spine_num,
                            seed,
                            prev_year,
                            prev_month,
                            job.job_no,
                            prev_cadence,
                        );
                        let prev_irregular_gross = stp_irregular_gross_amount(
                            prev_base_gross,
                            spine_num,
                            seed,
                            prev_year,
                            prev_month,
                            job.job_no,
                            prev_period.seq_no,
                        );
                        let prev_gross_before_overpayment =
                            round2(prev_base_gross + prev_irregular_gross);
                        let correction = -stp_overpayment_extra(
                            prev_gross_before_overpayment,
                            stp_payment_draw(
                                spine_num,
                                seed,
                                prev_year,
                                prev_month,
                                job.job_no,
                                prev_period.seq_no,
                                2,
                            ),
                        );
                        let correction_suffix = format!(
                            "_CORR{:04}{:02}P{:02}",
                            prev_year, prev_month, prev_period.seq_no
                        );
                        push_pay_row(
                            i,
                            spine_num,
                            &row_aeuid,
                            &job,
                            row_serial,
                            correction,
                            0.0,
                            0.0,
                            prev_period.start,
                            prev_period.end,
                            correction_pay_date,
                            i32::MIN,
                            prev_period.seq_no,
                            &correction_suffix,
                            0.0,
                            None,
                            true,
                        );
                    }
                }
            }
        }
    }

    let n_rows = aeuid_out.len();
    let mut columns = vec![NamedCol {
        name: "ALWNC_INCM_TOTL_AMT",
        col: Col::F64(allowance),
    }];

    if extended {
        columns.extend(vec![
            NamedCol {
                name: "ANL_LNG_SRVC_UNSD_LS_A_AMT",
                col: Col::F64(long_service_a_amt),
            },
            NamedCol {
                name: "ANL_LNG_SRVC_UNSD_LS_A_CD",
                col: Col::StrOpt(long_service_a_cd),
            },
            NamedCol {
                name: "ANL_LNG_SRVC_UNSD_LS_B_AMT",
                col: Col::F64(vec![0.0; n_rows]),
            },
            NamedCol {
                name: "ANL_LNG_SRVC_UNSD_LS_D_AMT",
                col: Col::F64(vec![0.0; n_rows]),
            },
            NamedCol {
                name: "ANL_LNG_SRVC_UNSD_LS_E_AMT",
                col: Col::F64(vec![0.0; n_rows]),
            },
        ]);
    }

    columns.extend(vec![
        NamedCol {
            name: "BIRTH_YEAR_MONTH_ABS",
            col: Col::Str(birth_year_month),
        },
        NamedCol {
            name: "BN",
            col: Col::Str(bn),
        },
        NamedCol {
            name: "BRANCH_NUMBER",
            col: Col::I32(branch_number),
        },
        NamedCol {
            name: "CNTRCTR_BN",
            col: Col::StrOpt(contractor_bn),
        },
        NamedCol {
            name: "DRVPAYENDDATE",
            col: Col::DateStr(drv_pay_end),
        },
        NamedCol {
            name: "DRVPAYSTARTDATE",
            col: Col::DateStr(drv_pay_start),
        },
        NamedCol {
            name: "DUMMY_FLAG",
            col: Col::Str(dummy_flag),
        },
        NamedCol {
            name: "EMPLOYEE_KEY",
            col: Col::Str(employee_key.clone()),
        },
    ]);

    if extended {
        let zero = || vec![0.0; n_rows];
        columns.extend(vec![
            NamedCol {
                name: "F_EMPLT_INCM_GRS_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "F_EMPLT_INCM_JPDA_GRS_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "F_EMPLT_INCM_JPDA_TOTL_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "F_EMPLT_INCM_TAX_CR_WHELD_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "F_EMPLT_INCM_TAX_PMT_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "F_EMPLT_INCM_TOTL_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "F_INCM_EXMT_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "IDV_WRKPLC_GVING_TOTL_AMT",
                col: Col::F64(workplace_giving),
            },
            NamedCol {
                name: "INCM_CMNTY_DEV_EMPLT_PRJCT_AMT",
                col: Col::F64(community_dev),
            },
            NamedCol {
                name: "INCM_GRS_AMT",
                col: Col::F64(gross.clone()),
            },
            NamedCol {
                name: "INCM_LABR_HIR_ARNGMT_GRS_AMT",
                col: Col::F64(labour_hire_arrangement),
            },
            NamedCol {
                name: "INCM_OTHR_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "INCM_VA_GRS_AMT",
                col: Col::F64(voluntary_agreement),
            },
            NamedCol {
                name: "INCM_WHM_GRS_AMT",
                col: Col::F64(whm_income),
            },
            NamedCol {
                name: "LABR_HIR_TOTL_AMT",
                col: Col::F64(labour_hire_total),
            },
            NamedCol {
                name: "LGA_ASGS_2022",
                col: Col::Str(lga),
            },
            NamedCol {
                name: "OTHR_SPCFD_TOTL_AMT",
                col: Col::F64(zero()),
            },
        ]);
    }

    columns.extend(vec![
        NamedCol {
            name: "PMT_DT",
            col: Col::DateStr(pmt_dt),
        },
        NamedCol {
            name: "PMT_SUMRY_TOTL_GRS_PMT_AMT",
            col: Col::F64(gross.clone()),
        },
        NamedCol {
            name: "PYRL_FNCL_YR",
            col: Col::Str(payroll_fy),
        },
        NamedCol {
            name: "PYR_PYE_RLTNSHP_CESTN_DT",
            col: Col::DateStr(cessation),
        },
        NamedCol {
            name: "PYR_PYE_RLTNSHP_CMNCMT_DT",
            col: Col::DateStr(commencement),
        },
    ]);

    if extended {
        let zero = || vec![0.0; n_rows];
        columns.extend(vec![
            NamedCol {
                name: "PYR_PYE_RLTNSHP_TRMNTD_IND",
                col: Col::I32(terminated_ind),
            },
            NamedCol {
                name: "PYR_SPNTN_CNTRBTN_RPRTBL_AMT",
                col: Col::F64(reportable_super),
            },
            NamedCol {
                name: "RFB_EXMT_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "RFB_TXBL_AMT",
                col: Col::F64(zero()),
            },
            NamedCol {
                name: "SA2_ASGS_2021",
                col: Col::Str(sa2),
            },
            NamedCol {
                name: "SEQUENCE_KEY",
                col: Col::Str(sequence_key),
            },
            NamedCol {
                name: "SG_EMPLR_CNTRBTN_AMT",
                col: Col::F64(sg),
            },
            NamedCol {
                name: "STATE_ASGS_2021",
                col: Col::I32(state_out),
            },
            NamedCol {
                name: "STP_MESHBLOCK_ABS",
                col: Col::Str(meshblock),
            },
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(aeuid_out),
            },
            NamedCol {
                name: "TAX_WHELD_TOTL_AMT",
                col: Col::F64(withheld.clone()),
            },
            NamedCol {
                name: "VA_TOTL_AMT",
                col: Col::F64(voluntary_agreement_total),
            },
            NamedCol {
                name: "WHELD_AMT",
                col: Col::F64(withheld),
            },
            NamedCol {
                name: "WHM_TOTL_AMT",
                col: Col::F64(whm_total),
            },
        ]);
    } else {
        columns.extend(vec![
            NamedCol {
                name: "PYR_SPNTN_CNTRBTN_RPRTBL_AMT",
                col: Col::F64(reportable_super),
            },
            NamedCol {
                name: "SA2_ASGS_2021",
                col: Col::Str(sa2),
            },
            NamedCol {
                name: "SEQUENCE_KEY",
                col: Col::Str(sequence_key),
            },
            NamedCol {
                name: "SG_EMPLR_CNTRBTN_AMT",
                col: Col::F64(sg),
            },
            NamedCol {
                name: "STATE_ASGS_2021",
                col: Col::I32(state_out),
            },
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(aeuid_out),
            },
        ]);
    }

    write_columns_to_parquet(out_path, columns)
        .unwrap_or_else(|e| panic!("stp DIL pay-events parquet write: {}", e));
    n_rows as i32
}

/// Write a 2026-DIL STP financial-year jobs chunk directly to parquet.
/// @export
#[extendr]
fn write_stp_dil_jobs_to_parquet__(
    spine_id: Strings,
    aeuid_ato: Strings,
    seed: i64,
    fy_end: i32,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};

    let n = aeuid_ato.len();
    let spine_ids = spine_id.as_slice();
    let aeuid_vals = aeuid_ato.as_slice();
    let fy_label = stp_fy_label_from_end(fy_end);
    let fy_start = days_since_epoch(fy_end - 1, 7, 1);
    let fy_end_date = days_since_epoch(fy_end, 6, 30);

    let mut bn = Vec::new();
    let mut payroll_fy = Vec::new();
    let mut cessation = Vec::new();
    let mut commencement = Vec::new();
    let mut aeuid_out = Vec::new();

    for i in 0..n {
        let spine_num = stp_spine_number(spine_ids[i].as_ref());
        for job in stp_job_history(spine_num, seed) {
            if !stp_job_overlaps(&job, fy_start, fy_end_date) {
                continue;
            }
            bn.push(job.bn);
            payroll_fy.push(fy_label.clone());
            cessation.push(
                if job.end != STP_OPEN_END && job.end >= fy_start && job.end <= fy_end_date {
                    job.end
                } else {
                    i32::MIN
                },
            );
            commencement.push(job.start);
            aeuid_out.push(aeuid_vals[i].as_ref().to_string());
        }
    }

    let n_rows = aeuid_out.len() as i32;
    let columns = vec![
        NamedCol {
            name: "BN",
            col: Col::Str(bn),
        },
        NamedCol {
            name: "PYRL_FNCL_YR",
            col: Col::Str(payroll_fy),
        },
        NamedCol {
            name: "PYR_PYE_RLTNSHP_CESTN_DT",
            col: Col::DateStr(cessation),
        },
        NamedCol {
            name: "PYR_PYE_RLTNSHP_CMNCMT_DT",
            col: Col::DateStr(commencement),
        },
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(aeuid_out),
        },
    ];
    write_columns_to_parquet(out_path, columns)
        .unwrap_or_else(|e| panic!("stp DIL jobs parquet write: {}", e));
    n_rows
}

/// Write a 2026-DIL STP financial-year ETP chunk directly to parquet.
/// @export
#[extendr]
fn write_stp_dil_etp_to_parquet__(
    spine_id: Strings,
    aeuid_ato: Strings,
    baseline_income: &[f64],
    seed: i64,
    fy_end: i32,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};

    let fy_label = stp_fy_label_from_end(fy_end);
    let fy_start = days_since_epoch(fy_end - 1, 7, 1);
    let fy_end_date = days_since_epoch(fy_end, 6, 30);
    let spine_ids = spine_id.as_slice();
    let aeuid_vals = aeuid_ato.as_slice();

    let mut bn = Vec::new();
    let mut dummy_flag = Vec::new();
    let mut employee_key = Vec::new();
    let mut etp_pmt_dt = Vec::new();
    let mut etp_type = Vec::new();
    let mut etp_tax_free = Vec::new();
    let mut etp_tax_withheld = Vec::new();
    let mut etp_taxable = Vec::new();
    let mut latest = Vec::new();
    let mut payroll_fy = Vec::new();
    let mut sequence_key = Vec::new();
    let mut aeuid_out = Vec::new();

    for i in 0..aeuid_ato.len() {
        let spine_num = stp_spine_number(spine_ids[i].as_ref());
        let row_aeuid = aeuid_vals[i].as_ref();
        for job in stp_job_history(spine_num, seed) {
            if job.end == STP_OPEN_END || job.end < fy_start || job.end > fy_end_date {
                continue;
            }
            let etp_modulus = 3;
            if stp_draw(spine_num, seed, 70 + job.job_no as u64) % etp_modulus != 0 {
                continue;
            }

            let row_bn = job.bn.clone();
            let row_employee_key = format!("{}_{}", row_aeuid, row_bn);
            let taxable = round2(baseline_income[i].max(0.0) * 0.08);
            let etp_date = (job.end
                + (stp_draw(spine_num, seed, 80 + job.job_no as u64) % 31) as i32)
                .min(fy_end_date);

            bn.push(row_bn);
            dummy_flag.push("False".to_string());
            employee_key.push(row_employee_key.clone());
            etp_pmt_dt.push(etp_date);
            etp_type.push("R".to_string());
            etp_tax_free.push(round2(taxable * 0.2));
            etp_tax_withheld.push(round2(taxable * 0.22));
            etp_taxable.push(taxable);
            latest.push(true);
            payroll_fy.push(fy_label.clone());
            sequence_key.push(format!(
                "{}_ETP_{}_J{}",
                row_employee_key, fy_end, job.job_no
            ));
            aeuid_out.push(row_aeuid.to_string());
        }
    }

    let n_rows = aeuid_out.len() as i32;
    let columns = vec![
        NamedCol {
            name: "BN",
            col: Col::Str(bn),
        },
        NamedCol {
            name: "DUMMY_FLAG",
            col: Col::Str(dummy_flag),
        },
        NamedCol {
            name: "EMPLOYEE_KEY",
            col: Col::Str(employee_key),
        },
        NamedCol {
            name: "ETP_PMT_DT",
            col: Col::DateStr(etp_pmt_dt),
        },
        NamedCol {
            name: "ETP_PMT_TYP_CD",
            col: Col::Str(etp_type),
        },
        NamedCol {
            name: "ETP_TAX_FREE_AMT",
            col: Col::F64(etp_tax_free),
        },
        NamedCol {
            name: "ETP_TAX_WHELD_TOTL_AMT",
            col: Col::F64(etp_tax_withheld),
        },
        NamedCol {
            name: "ETP_TXBL_CMPNT_AMT",
            col: Col::F64(etp_taxable),
        },
        NamedCol {
            name: "LATEST",
            col: Col::Bool(latest),
        },
        NamedCol {
            name: "PYRL_FNCL_YR",
            col: Col::Str(payroll_fy),
        },
        NamedCol {
            name: "SEQUENCE_KEY",
            col: Col::Str(sequence_key),
        },
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(aeuid_out),
        },
    ];
    write_columns_to_parquet(out_path, columns)
        .unwrap_or_else(|e| panic!("stp DIL ETP parquet write: {}", e));
    n_rows
}

extendr_module! {
    mod stp;
    fn write_stp_dil_pay_events_to_parquet__;
    fn write_stp_dil_jobs_to_parquet__;
    fn write_stp_dil_etp_to_parquet__;
}

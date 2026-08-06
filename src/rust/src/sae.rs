use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::sampling::{normal_sample, weighted_sample};

// ~90% of employed have super accounts; ~1.5 accounts per person average
const SUPER_RATE: f64 = 0.90;

// Account phase: A=accumulation, R=retirement
const PHASE_CODES: [&str; 2] = ["A", "R"];

// Account status: A=active, I=inactive, L=lost
// Internal shorthand for the account state, used by the balance and
// contribution logic below. It is NOT what the column holds.
const STATUS_CODES: [&str; 3] = ["A", "I", "L"];
const STATUS_WEIGHTS: [f64; 3] = [0.70, 0.20, 0.10];

/// The value the member account status column carries.
///
/// The ATO's member-attribute message specification enumerates these in full
/// rather than as letter codes, so "A" and "L" were not members of the
/// published domain at all. The internal shorthand stays, because the balance
/// and contribution rules key off it; only what reaches the column changes.
fn account_status_value(code: &str) -> &'static str {
    match code {
        "L" => "Closed",
        "I" => "Transition to retirement income stream",
        _ => "Open",
    }
}

/// Age bands as the registry documents them, including the spacing. A column
/// whose values do not match its own documented domain makes the registry
/// wrong about the data.
fn age_range_value(age: i32) -> &'static str {
    match age {
        a if a < 18 => "Under 18",
        a if a < 25 => "18 - 24",
        a if a < 30 => "25 - 29",
        a if a < 35 => "30 - 34",
        a if a < 40 => "35 - 39",
        a if a < 45 => "40 - 44",
        a if a < 50 => "45 - 49",
        a if a < 55 => "50 - 54",
        a if a < 60 => "55 - 59",
        a if a < 65 => "60 - 64",
        a if a < 70 => "65 - 69",
        a if a < 75 => "70 - 74",
        _ => "75 and over",
    }
}

/// Project Superannuation Accounts Extract (SAE) from spine.
///
/// Person × account × FY. Each person may have 1-3 accounts.
/// @export
#[extendr]
fn project_sae__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    seed: i64,
    fy_start: i32,
    fy_end: i32,
) -> List {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let n_years = (fy_end - fy_start + 1) as usize;

    let est_rows = (n as f64 * SUPER_RATE * 1.5 * n_years as f64) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_fin_year: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_fund_id: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_phase: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_status: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_balance: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_brth_yr: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_brth_mth: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_gender: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_state: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_age_range: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_sg_amt: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_ss_amt: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_personal: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_smsf: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_insurance: Vec<String> = Vec::with_capacity(est_rows);

    // Pre-determine who has super and how many accounts
    struct PersonSuper {
        has_super: bool,
        n_accounts: u8,
        fund_ids: Vec<String>,
    }

    let mut person_super: Vec<PersonSuper> = Vec::with_capacity(n);
    for i in 0..n {
        let age = 2022 - birth_year[i];
        let employed = baseline_employed[i] == 1;
        let has = (employed || age > 25) && rng.gen::<f64>() < SUPER_RATE && age >= 18;

        let n_accts = if has {
            if rng.gen::<f64>() < 0.55 {
                1
            } else if rng.gen::<f64>() < 0.70 {
                2
            } else {
                3
            }
        } else {
            0
        };

        let mut fids = Vec::new();
        for _ in 0..n_accts {
            fids.push(format!("FND{:08X}", rng.gen::<u32>()));
        }

        person_super.push(PersonSuper {
            has_super: has,
            n_accounts: n_accts,
            fund_ids: fids,
        });
    }

    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        for i in 0..n {
            if !person_super[i].has_super {
                continue;
            }

            let person_aeuid = aeuid[i].to_string();
            let by = birth_year[i];
            let age = fy - by;
            let sx = sex[i];
            let st = state[i];
            let income = baseline_income[i];

            let sex_str = if sx == 1 { "M" } else { "F" };
            let age_range = age_range_value(age);

            for acct_idx in 0..person_super[i].n_accounts as usize {
                let fund_id = &person_super[i].fund_ids[acct_idx];

                // Phase: accumulation for working age, retirement for 60+
                let phase = if age >= 60 && rng.gen::<f64>() < 0.40 {
                    "Retirement"
                } else {
                    "Accumulation"
                };

                let sts_idx = weighted_sample(&mut rng, &STATUS_WEIGHTS);
                let status = STATUS_CODES[sts_idx];

                // Balance: age × ~$5K median, log-normal
                let base_bal = age as f64 * 5000.0;
                let balance = if status == "L" {
                    normal_sample(&mut rng, 2000.0, 3000.0).max(0.0).round()
                } else {
                    let mult = normal_sample(&mut rng, 1.0, 0.5).max(0.1);
                    (base_bal * mult).round()
                };

                // SG contribution: ~11% of salary (primary account only)
                let sg = if acct_idx == 0 && income > 0.0 && status == "A" {
                    (income * 0.11).round()
                } else {
                    0.0
                };

                // Salary sacrifice: ~10% of workers, ~5% of salary
                let ss = if acct_idx == 0 && rng.gen::<f64>() < 0.10 && income > 0.0 {
                    (income * 0.05).round()
                } else {
                    0.0
                };

                // Personal contributions
                let personal = if rng.gen::<f64>() < 0.15 {
                    normal_sample(&mut rng, 3000.0, 5000.0).max(0.0).round()
                } else {
                    0.0
                };

                let smsf = if rng.gen::<f64>() < 0.05 { "Y" } else { "N" };
                let insurance = if rng.gen::<f64>() < 0.60 { "Y" } else { "N" };

                out_aeuid.push(person_aeuid.clone());
                out_fin_year.push(fy_str.clone());
                out_fund_id.push(fund_id.clone());
                out_phase.push(phase.to_string());
                out_status.push(account_status_value(status).to_string());
                out_balance.push(balance);
                out_brth_yr.push(by);
                out_brth_mth.push(rng.gen_range(1..=12));
                out_gender.push(sex_str.to_string());
                out_state.push(st);
                out_age_range.push(age_range.to_string());
                out_sg_amt.push(sg);
                out_ss_amt.push(ss);
                out_personal.push(personal);
                out_smsf.push(smsf.to_string());
                out_insurance.push(insurance.to_string());
            }
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        FIN_YEAR = out_fin_year,
        FND_SID = out_fund_id,
        ACNT_PHS_CD = out_phase,
        MBR_ACNT_STS_CD = out_status,
        SF_MBRSHP_ACNT_BAL_AMT = out_balance,
        BRTH_DT_YEAR = out_brth_yr,
        BRTH_DT_MONTH = out_brth_mth,
        GENDER = out_gender,
        STATE = out_state,
        AGE_RANGE = out_age_range,
        EMPLOYERSGAMT = out_sg_amt,
        EMPLOYERSSAMT = out_ss_amt,
        PRSNL_CNTRBTN_TOTL_AMT = out_personal,
        SMSF_IND = out_smsf,
        SPNTN_MBRSHP_ACNT_INSRNC_IND = out_insurance
    )
}

/// Project SAE directly to per-FY parquet files.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_sae_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    seed: i64,
    fy_start: i32,
    fy_end: i32,
    out_dir: &str,
    product_prefix: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let aeuid_owned: Vec<String> = aeuid.iter().map(|s| s.to_string()).collect();

    // Pre-determine who has super and how many accounts (stable across years)
    struct PersonSuper {
        has_super: bool,
        n_accounts: u8,
        fund_ids: Vec<String>,
    }
    let mut person_super: Vec<PersonSuper> = Vec::with_capacity(n);
    for i in 0..n {
        let age = 2022 - birth_year[i];
        let employed = baseline_employed[i] == 1;
        let has = (employed || age > 25) && rng.gen::<f64>() < SUPER_RATE && age >= 18;
        let n_accts = if has {
            if rng.gen::<f64>() < 0.55 {
                1
            } else if rng.gen::<f64>() < 0.70 {
                2
            } else {
                3
            }
        } else {
            0
        };
        let mut fids = Vec::new();
        for _ in 0..n_accts {
            fids.push(format!("FND{:08X}", rng.gen::<u32>()));
        }
        person_super.push(PersonSuper {
            has_super: has,
            n_accounts: n_accts,
            fund_ids: fids,
        });
    }

    let mut total: usize = 0;
    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        let mut out_aeuid: Vec<String> = Vec::new();
        let mut out_fin_year: Vec<String> = Vec::new();
        let mut out_fund_id: Vec<String> = Vec::new();
        let mut out_phase: Vec<String> = Vec::new();
        let mut out_status: Vec<String> = Vec::new();
        let mut out_balance: Vec<f64> = Vec::new();
        let mut out_brth_yr: Vec<i32> = Vec::new();
        let mut out_brth_mth: Vec<i32> = Vec::new();
        let mut out_gender: Vec<String> = Vec::new();
        let mut out_state: Vec<i32> = Vec::new();
        let mut out_age_range: Vec<String> = Vec::new();
        let mut out_sg_amt: Vec<f64> = Vec::new();
        let mut out_ss_amt: Vec<f64> = Vec::new();
        let mut out_personal: Vec<f64> = Vec::new();
        let mut out_smsf: Vec<String> = Vec::new();
        let mut out_insurance: Vec<String> = Vec::new();

        for i in 0..n {
            if !person_super[i].has_super {
                continue;
            }
            let by = birth_year[i];
            let age = fy - by;
            let sx = sex[i];
            let st = state[i];
            let income = baseline_income[i];
            let sex_str = if sx == 1 { "M" } else { "F" };
            let age_range = age_range_value(age);
            for acct_idx in 0..person_super[i].n_accounts as usize {
                let fund_id = &person_super[i].fund_ids[acct_idx];
                let phase = if age >= 60 && rng.gen::<f64>() < 0.40 {
                    "Retirement"
                } else {
                    "Accumulation"
                };
                let sts_idx = weighted_sample(&mut rng, &STATUS_WEIGHTS);
                let status = STATUS_CODES[sts_idx];
                let base_bal = age as f64 * 5000.0;
                let balance = if status == "L" {
                    normal_sample(&mut rng, 2000.0, 3000.0).max(0.0).round()
                } else {
                    let mult = normal_sample(&mut rng, 1.0, 0.5).max(0.1);
                    (base_bal * mult).round()
                };
                let sg = if acct_idx == 0 && income > 0.0 && status == "A" {
                    (income * 0.11).round()
                } else {
                    0.0
                };
                let ss = if acct_idx == 0 && rng.gen::<f64>() < 0.10 && income > 0.0 {
                    (income * 0.05).round()
                } else {
                    0.0
                };
                let personal = if rng.gen::<f64>() < 0.15 {
                    normal_sample(&mut rng, 3000.0, 5000.0).max(0.0).round()
                } else {
                    0.0
                };
                let smsf = if rng.gen::<f64>() < 0.05 { "Y" } else { "N" };
                let insurance = if rng.gen::<f64>() < 0.60 { "Y" } else { "N" };

                out_aeuid.push(aeuid_owned[i].clone());
                out_fin_year.push(fy_str.clone());
                out_fund_id.push(fund_id.clone());
                out_phase.push(phase.to_string());
                out_status.push(account_status_value(status).to_string());
                out_balance.push(balance);
                out_brth_yr.push(by);
                out_brth_mth.push(rng.gen_range(1..=12));
                out_gender.push(sex_str.to_string());
                out_state.push(st);
                out_age_range.push(age_range.to_string());
                out_sg_amt.push(sg);
                out_ss_amt.push(ss);
                out_personal.push(personal);
                out_smsf.push(smsf.to_string());
                out_insurance.push(insurance.to_string());
            }
        }

        total += out_aeuid.len();

        let fname = format!(
            "{}{:02}{:02}.parquet",
            product_prefix,
            (fy - 1) % 100,
            fy % 100
        );
        let out_path = format!("{}/{}", out_dir, fname);

        let cols = vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(out_aeuid),
            },
            NamedCol {
                name: "FIN_YEAR",
                col: Col::Str(out_fin_year),
            },
            NamedCol {
                name: "FND_SID",
                col: Col::Str(out_fund_id),
            },
            NamedCol {
                name: "ACNT_PHS_CD",
                col: Col::Str(out_phase),
            },
            NamedCol {
                name: "MBR_ACNT_STS_CD",
                col: Col::Str(out_status),
            },
            NamedCol {
                name: "SF_MBRSHP_ACNT_BAL_AMT",
                col: Col::F64(out_balance),
            },
            NamedCol {
                name: "BRTH_DT_YEAR",
                col: Col::I32(out_brth_yr),
            },
            NamedCol {
                name: "BRTH_DT_MONTH",
                col: Col::I32(out_brth_mth),
            },
            NamedCol {
                name: "GENDER",
                col: Col::Str(out_gender),
            },
            NamedCol {
                name: "STATE",
                col: Col::I32(out_state),
            },
            NamedCol {
                name: "AGE_RANGE",
                col: Col::Str(out_age_range),
            },
            NamedCol {
                name: "EMPLOYERSGAMT",
                col: Col::F64(out_sg_amt),
            },
            NamedCol {
                name: "EMPLOYERSSAMT",
                col: Col::F64(out_ss_amt),
            },
            NamedCol {
                name: "PRSNL_CNTRBTN_TOTL_AMT",
                col: Col::F64(out_personal),
            },
            NamedCol {
                name: "SMSF_IND",
                col: Col::Str(out_smsf),
            },
            NamedCol {
                name: "SPNTN_MBRSHP_ACNT_INSRNC_IND",
                col: Col::Str(out_insurance),
            },
        ];
        write_columns_to_parquet(&out_path, cols)
            .unwrap_or_else(|e| panic!("sae parquet write: {}", e));
    }
    total as i32
}

extendr_module! {
    mod sae;
    fn project_sae__;
    fn project_sae_to_parquet__;
}

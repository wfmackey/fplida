use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::sampling::normal_sample;

// ~5% of tax lodgers have CGT events
const CGT_RATE: f64 = 0.05;

/// Project Capital Gains Tax (CGT) from spine.
///
/// Person × FY records for those with CGT events.
/// Net gain decomposed into real estate, shares, and other assets.
/// @export
#[extendr]
fn project_cgt__(
    aeuid: Strings,
    birth_year: &[i32],
    baseline_income: &[f64],
    seed: i64,
    fy_start: i32,
    fy_end: i32,
) -> List {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let n_years = (fy_end - fy_start + 1) as usize;

    let est_rows = (n as f64 * CGT_RATE * n_years as f64) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_income_year: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_entity_type: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_cg_net: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_cg_total: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_cg_re: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_cg_shares: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_cg_other: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_cl_total: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_discount: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_sb_conc: Vec<f64> = Vec::with_capacity(est_rows);

    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        for i in 0..n {
            let age = fy - birth_year[i];
            if age < 20 || age > 80 {
                continue;
            }

            // Higher income / older = more likely to have CGT event
            let p_cgt = if baseline_income[i] > 100000.0 {
                CGT_RATE * 2.0
            } else if age > 50 {
                CGT_RATE * 1.5
            } else {
                CGT_RATE
            };

            if rng.gen::<f64>() >= p_cgt {
                continue;
            }

            let person_aeuid = aeuid[i].to_string();

            // Total current year gains (log-normal, median ~$20K)
            let total_gain = (normal_sample(&mut rng, 10.0, 1.0) as f64).exp().round();
            let total_gain = total_gain.min(5_000_000.0);

            // Decompose into asset types
            let re_share = rng.gen_range(0.0..=0.60f64);
            let shares_share = rng.gen_range(0.0..=(1.0 - re_share).min(0.50));
            let other_share = 1.0 - re_share - shares_share;

            let cg_re = (total_gain * re_share).round();
            let cg_shares = (total_gain * shares_share).round();
            let cg_other = (total_gain * other_share).round();

            // ~30% also have losses
            let cl = if rng.gen::<f64>() < 0.30 {
                normal_sample(&mut rng, 5000.0, 8000.0).max(0.0).round()
            } else {
                0.0
            };

            // 50% CGT discount (held > 12 months, ~80% of gains)
            let discount = if rng.gen::<f64>() < 0.80 {
                ((total_gain - cl).max(0.0) * 0.50).round()
            } else {
                0.0
            };

            // Small business concessions (~5%)
            let sb = if rng.gen::<f64>() < 0.05 {
                normal_sample(&mut rng, 10000.0, 15000.0).max(0.0).round()
            } else {
                0.0
            };

            let net = (total_gain - cl - discount - sb).max(0.0).round();

            out_aeuid.push(person_aeuid);
            out_income_year.push(fy_str.clone());
            out_entity_type.push("I".to_string());
            out_cg_net.push(net);
            out_cg_total.push(total_gain);
            out_cg_re.push(cg_re);
            out_cg_shares.push(cg_shares);
            out_cg_other.push(cg_other);
            out_cl_total.push(cl);
            out_discount.push(discount);
            out_sb_conc.push(sb);
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        INCOME_YEAR = out_income_year,
        ENTITY_TYPE = out_entity_type,
        CG_NET_AMT = out_cg_net,
        CG_CY_TOTL_AMT = out_cg_total,
        CG_TOTL_REAL_EST_AMT = out_cg_re,
        CG_TOTL_SHARES_UNITS_AMT = out_cg_shares,
        CG_TOTL_OTH_AMT = out_cg_other,
        CL_CY_TOTL_AMT = out_cl_total,
        CGT_TOTL_DSCNT_APLD_AMT = out_discount,
        SB_CNCSNS_APLD_TOTL_AMT = out_sb_conc
    )
}

/// Project CGT directly to per-FY parquet files.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_cgt_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
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

    let mut total: usize = 0;
    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        let mut out_aeuid: Vec<String> = Vec::new();
        let mut out_income_year: Vec<String> = Vec::new();
        let mut out_entity_type: Vec<String> = Vec::new();
        let mut out_cg_net: Vec<f64> = Vec::new();
        let mut out_cg_total: Vec<f64> = Vec::new();
        let mut out_cg_re: Vec<f64> = Vec::new();
        let mut out_cg_shares: Vec<f64> = Vec::new();
        let mut out_cg_other: Vec<f64> = Vec::new();
        let mut out_cl_total: Vec<f64> = Vec::new();
        let mut out_discount: Vec<f64> = Vec::new();
        let mut out_sb_conc: Vec<f64> = Vec::new();

        for i in 0..n {
            let age = fy - birth_year[i];
            if age < 20 || age > 80 {
                continue;
            }

            let p_cgt = if baseline_income[i] > 100000.0 {
                CGT_RATE * 2.0
            } else if age > 50 {
                CGT_RATE * 1.5
            } else {
                CGT_RATE
            };
            if rng.gen::<f64>() >= p_cgt {
                continue;
            }

            let total_gain = (normal_sample(&mut rng, 10.0, 1.0) as f64)
                .exp()
                .round()
                .min(5_000_000.0);
            let re_share = rng.gen_range(0.0..=0.60f64);
            let shares_share = rng.gen_range(0.0..=(1.0 - re_share).min(0.50));
            let other_share = 1.0 - re_share - shares_share;
            let cg_re = (total_gain * re_share).round();
            let cg_shares = (total_gain * shares_share).round();
            let cg_other = (total_gain * other_share).round();
            let cl = if rng.gen::<f64>() < 0.30 {
                normal_sample(&mut rng, 5000.0, 8000.0).max(0.0).round()
            } else {
                0.0
            };
            let discount = if rng.gen::<f64>() < 0.80 {
                ((total_gain - cl).max(0.0) * 0.50).round()
            } else {
                0.0
            };
            let sb = if rng.gen::<f64>() < 0.05 {
                normal_sample(&mut rng, 10000.0, 15000.0).max(0.0).round()
            } else {
                0.0
            };
            let net = (total_gain - cl - discount - sb).max(0.0).round();

            out_aeuid.push(aeuid_owned[i].clone());
            out_income_year.push(fy_str.clone());
            out_entity_type.push("I".to_string());
            out_cg_net.push(net);
            out_cg_total.push(total_gain);
            out_cg_re.push(cg_re);
            out_cg_shares.push(cg_shares);
            out_cg_other.push(cg_other);
            out_cl_total.push(cl);
            out_discount.push(discount);
            out_sb_conc.push(sb);
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
                name: "INCOME_YEAR",
                col: Col::Str(out_income_year),
            },
            NamedCol {
                name: "ENTITY_TYPE",
                col: Col::Str(out_entity_type),
            },
            NamedCol {
                name: "CG_NET_AMT",
                col: Col::F64(out_cg_net),
            },
            NamedCol {
                name: "CG_CY_TOTL_AMT",
                col: Col::F64(out_cg_total),
            },
            NamedCol {
                name: "CG_TOTL_REAL_EST_AMT",
                col: Col::F64(out_cg_re),
            },
            NamedCol {
                name: "CG_TOTL_SHARES_UNITS_AMT",
                col: Col::F64(out_cg_shares),
            },
            NamedCol {
                name: "CG_TOTL_OTH_AMT",
                col: Col::F64(out_cg_other),
            },
            NamedCol {
                name: "CL_CY_TOTL_AMT",
                col: Col::F64(out_cl_total),
            },
            NamedCol {
                name: "CGT_TOTL_DSCNT_APLD_AMT",
                col: Col::F64(out_discount),
            },
            NamedCol {
                name: "SB_CNCSNS_APLD_TOTL_AMT",
                col: Col::F64(out_sb_conc),
            },
        ];
        write_columns_to_parquet(&out_path, cols)
            .unwrap_or_else(|e| panic!("cgt parquet write: {}", e));
    }
    total as i32
}

extendr_module! {
    mod cgt;
    fn project_cgt__;
    fn project_cgt_to_parquet__;
}

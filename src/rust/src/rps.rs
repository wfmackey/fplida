use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::nominal;
use crate::sampling::normal_sample;

// ~10% of adults own rental property; ~1.3 properties per investor
const INVESTOR_RATE: f64 = 0.10;

/// Both generators here name a financial year by the calendar year it ENDS in:
/// `fy` 2021 is the year ended 30 June 2021, which is what `FIN_YEAR` renders
/// as "2020-21". The nominal tables name the same year by the calendar year it
/// STARTS in, so every lookup takes the year before. Mixing the two is worth
/// about four per cent and nothing would flag it.
#[inline]
fn fy_start(fy_end_year: i32) -> i32 {
    fy_end_year - 1
}

/// The multiplier carrying an owner's anchor-year property amounts to `fy`.
///
/// Rent, rates, insurance, repairs and agent commission are prices, not wages,
/// so they follow the CPI. Holding them flat made a rental schedule from 2011
/// and one from 2023 name the same rent, which is the one thing a landlord can
/// say for certain is untrue.
///
/// The spread is per owner rather than per property. A landlord who bought into
/// an expensive market holds every one of their properties in it, so the two or
/// three properties on one schedule should move together rather than drift
/// apart at random.
///
/// The key comes from the AEUID and not from the loop index `i`, which counts
/// from zero within whichever slice of the spine a worker was handed. Keying on
/// `i` would give the same person a different deviation in a build with a
/// different slice count, and their rental figures would stop agreeing with
/// their own tax return.
#[inline]
fn property_factor(aeuid: &str, seed: i64, fy_end_year: i32) -> f64 {
    nominal::factor(
        nominal::Series::Price,
        nominal::Basis::Financial,
        nominal::PERSON,
        nominal::unit_key(aeuid),
        seed,
        fy_start(fy_end_year),
    )
}

/// Project Rental Property Schedule (RPS) from spine.
///
/// Person × property × FY records. Each property has income and expense items.
/// @export
#[extendr]
fn project_rps__(
    aeuid: Strings,
    birth_year: &[i32],
    baseline_income: &[f64],
    state: &[i32],
    seed: i64,
    fy_start: i32,
    fy_end: i32,
) -> List {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let n_years = (fy_end - fy_start + 1) as usize;

    let est_rows = (n as f64 * INVESTOR_RATE * 1.3 * n_years as f64) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_fin_year: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_ownership: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_gross_rent: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_net_rent: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_interest: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_insurance: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_repairs: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_council: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_water: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_land_tax: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_agent_fees: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_body_corp: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_depreciation: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_total_exp: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_weeks_rented: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_state: Vec<i32> = Vec::with_capacity(est_rows);

    // Pre-determine who is an investor
    struct Investor {
        is_investor: bool,
        n_properties: u8,
    }
    let mut investors: Vec<Investor> = Vec::with_capacity(n);
    for i in 0..n {
        let age = 2020 - birth_year[i];
        let income = baseline_income[i];
        let p = if age < 25 || age > 75 {
            0.02
        } else if income > 80000.0 {
            INVESTOR_RATE * 1.5
        } else {
            INVESTOR_RATE
        };
        let is_inv = rng.gen::<f64>() < p;
        let n_props = if is_inv {
            if rng.gen::<f64>() < 0.75 {
                1
            } else if rng.gen::<f64>() < 0.80 {
                2
            } else {
                3
            }
        } else {
            0
        };
        investors.push(Investor {
            is_investor: is_inv,
            n_properties: n_props,
        });
    }

    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        for i in 0..n {
            if !investors[i].is_investor {
                continue;
            }

            let person_aeuid = aeuid[i].to_string();
            let st = state[i];
            // Hoisted out of the property loop: one owner, one departure from
            // the headline, however many properties they hold.
            let price_f = property_factor(&person_aeuid, seed, fy);

            for _prop in 0..investors[i].n_properties {
                // Ownership percentage
                let ownership = if rng.gen::<f64>() < 0.60 {
                    100.0
                } else if rng.gen::<f64>() < 0.85 {
                    50.0
                } else {
                    rng.gen_range(25.0..=50.0f64).round()
                };

                // Gross rent: ~$25K/year median in anchor-year dollars. The
                // clamp comes before `price_f` throughout so that the floor is
                // indexed too: the cheapest tenancy on record still costs more
                // in 2023 than it did in 2011.
                let gross = (normal_sample(&mut rng, 25000.0, 10000.0).max(5000.0) * price_f)
                    .round();

                // Expenses
                let interest = (normal_sample(&mut rng, 15000.0, 8000.0).max(0.0) * price_f).round();
                let insurance_amt =
                    (normal_sample(&mut rng, 1500.0, 500.0).max(0.0) * price_f).round();
                let repairs = (normal_sample(&mut rng, 2000.0, 3000.0).max(0.0) * price_f).round();
                let council = (normal_sample(&mut rng, 2000.0, 800.0).max(500.0) * price_f).round();
                let water = (normal_sample(&mut rng, 800.0, 300.0).max(0.0) * price_f).round();
                let land_tax = if rng.gen::<f64>() < 0.30 {
                    (normal_sample(&mut rng, 2000.0, 2000.0).max(0.0) * price_f).round()
                } else {
                    0.0
                };
                let agent = (normal_sample(&mut rng, 2000.0, 1000.0).max(0.0) * price_f).round();
                let body_corp = if rng.gen::<f64>() < 0.40 {
                    (normal_sample(&mut rng, 4000.0, 2000.0).max(0.0) * price_f).round()
                } else {
                    0.0
                };
                let deprec = (normal_sample(&mut rng, 3000.0, 3000.0).max(0.0) * price_f).round();

                // The two totals are sums of the columns above and are left
                // alone: indexing them as well would double-count and break the
                // accounting identity a user checks the schedule against.
                let total_exp = interest
                    + insurance_amt
                    + repairs
                    + council
                    + water
                    + land_tax
                    + agent
                    + body_corp
                    + deprec;
                let net = gross - total_exp;

                let weeks = rng.gen_range(40..=52i32);

                out_aeuid.push(person_aeuid.clone());
                out_fin_year.push(fy_str.clone());
                out_ownership.push(ownership);
                out_gross_rent.push(gross);
                out_net_rent.push(net.round());
                out_interest.push(interest);
                out_insurance.push(insurance_amt);
                out_repairs.push(repairs);
                out_council.push(council);
                out_water.push(water);
                out_land_tax.push(land_tax);
                out_agent_fees.push(agent);
                out_body_corp.push(body_corp);
                out_depreciation.push(deprec);
                out_total_exp.push(total_exp.round());
                out_weeks_rented.push(weeks);
                out_state.push(st);
            }
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        FIN_YEAR = out_fin_year,
        OWNRSHP_PRCNT = out_ownership,
        RNTL_GRS_AMT = out_gross_rent,
        RNTL_NET_RNT_AMT = out_net_rent,
        LOAN_INTST_AMT = out_interest,
        INSRNC_AMT = out_insurance,
        RPRS_AND_MNTNCE_AMT = out_repairs,
        CNSL_RTS_AMT = out_council,
        WTR_CHRGS_AMT = out_water,
        LAND_TAX_AMT = out_land_tax,
        PRPTY_AGNT_FEES_OR_CMSN_AMT = out_agent_fees,
        BDY_CRPRT_FEES_AMT = out_body_corp,
        CAS_AMT = out_depreciation,
        EXPNSS_TOTL_AMT = out_total_exp,
        PRPTY_RNTD_WKS_THIS_YR_CNT = out_weeks_rented,
        STATE_CODE_2021 = out_state
    )
}

/// Project RPS directly to per-FY parquet files.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_rps_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    baseline_income: &[f64],
    state: &[i32],
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

    struct Investor {
        is_investor: bool,
        n_properties: u8,
    }
    let mut investors: Vec<Investor> = Vec::with_capacity(n);
    for i in 0..n {
        let age = 2020 - birth_year[i];
        let income = baseline_income[i];
        let p = if age < 25 || age > 75 {
            0.02
        } else if income > 80000.0 {
            INVESTOR_RATE * 1.5
        } else {
            INVESTOR_RATE
        };
        let is_inv = rng.gen::<f64>() < p;
        let n_props = if is_inv {
            if rng.gen::<f64>() < 0.75 {
                1
            } else if rng.gen::<f64>() < 0.80 {
                2
            } else {
                3
            }
        } else {
            0
        };
        investors.push(Investor {
            is_investor: is_inv,
            n_properties: n_props,
        });
    }

    let mut total: usize = 0;
    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        let mut out_aeuid: Vec<String> = Vec::new();
        let mut out_fin_year: Vec<String> = Vec::new();
        let mut out_ownership: Vec<f64> = Vec::new();
        let mut out_gross_rent: Vec<f64> = Vec::new();
        let mut out_net_rent: Vec<f64> = Vec::new();
        let mut out_interest: Vec<f64> = Vec::new();
        let mut out_insurance: Vec<f64> = Vec::new();
        let mut out_repairs: Vec<f64> = Vec::new();
        let mut out_council: Vec<f64> = Vec::new();
        let mut out_water: Vec<f64> = Vec::new();
        let mut out_land_tax: Vec<f64> = Vec::new();
        let mut out_agent_fees: Vec<f64> = Vec::new();
        let mut out_body_corp: Vec<f64> = Vec::new();
        let mut out_depreciation: Vec<f64> = Vec::new();
        let mut out_total_exp: Vec<f64> = Vec::new();
        let mut out_weeks_rented: Vec<i32> = Vec::new();
        let mut out_state: Vec<i32> = Vec::new();

        for i in 0..n {
            if !investors[i].is_investor {
                continue;
            }
            let st = state[i];
            // Same indexing as the list variant, and it has to stay that way:
            // the two are meant to be the same data in different containers.
            let price_f = property_factor(&aeuid_owned[i], seed, fy);
            for _prop in 0..investors[i].n_properties {
                let ownership = if rng.gen::<f64>() < 0.60 {
                    100.0
                } else if rng.gen::<f64>() < 0.85 {
                    50.0
                } else {
                    rng.gen_range(25.0..=50.0f64).round()
                };
                let gross = (normal_sample(&mut rng, 25000.0, 10000.0).max(5000.0) * price_f)
                    .round();
                let interest = (normal_sample(&mut rng, 15000.0, 8000.0).max(0.0) * price_f).round();
                let insurance_amt =
                    (normal_sample(&mut rng, 1500.0, 500.0).max(0.0) * price_f).round();
                let repairs = (normal_sample(&mut rng, 2000.0, 3000.0).max(0.0) * price_f).round();
                let council = (normal_sample(&mut rng, 2000.0, 800.0).max(500.0) * price_f).round();
                let water = (normal_sample(&mut rng, 800.0, 300.0).max(0.0) * price_f).round();
                let land_tax = if rng.gen::<f64>() < 0.30 {
                    (normal_sample(&mut rng, 2000.0, 2000.0).max(0.0) * price_f).round()
                } else {
                    0.0
                };
                let agent = (normal_sample(&mut rng, 2000.0, 1000.0).max(0.0) * price_f).round();
                let body_corp = if rng.gen::<f64>() < 0.40 {
                    (normal_sample(&mut rng, 4000.0, 2000.0).max(0.0) * price_f).round()
                } else {
                    0.0
                };
                let deprec = (normal_sample(&mut rng, 3000.0, 3000.0).max(0.0) * price_f).round();
                // Totals inherit from the columns above; see the list variant.
                let total_exp = interest
                    + insurance_amt
                    + repairs
                    + council
                    + water
                    + land_tax
                    + agent
                    + body_corp
                    + deprec;
                let net = gross - total_exp;
                let weeks = rng.gen_range(40..=52i32);

                out_aeuid.push(aeuid_owned[i].clone());
                out_fin_year.push(fy_str.clone());
                out_ownership.push(ownership);
                out_gross_rent.push(gross);
                out_net_rent.push(net.round());
                out_interest.push(interest);
                out_insurance.push(insurance_amt);
                out_repairs.push(repairs);
                out_council.push(council);
                out_water.push(water);
                out_land_tax.push(land_tax);
                out_agent_fees.push(agent);
                out_body_corp.push(body_corp);
                out_depreciation.push(deprec);
                out_total_exp.push(total_exp.round());
                out_weeks_rented.push(weeks);
                out_state.push(st);
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
                name: "OWNRSHP_PRCNT",
                col: Col::F64(out_ownership),
            },
            NamedCol {
                name: "RNTL_GRS_AMT",
                col: Col::F64(out_gross_rent),
            },
            NamedCol {
                name: "RNTL_NET_RNT_AMT",
                col: Col::F64(out_net_rent),
            },
            NamedCol {
                name: "LOAN_INTST_AMT",
                col: Col::F64(out_interest),
            },
            NamedCol {
                name: "INSRNC_AMT",
                col: Col::F64(out_insurance),
            },
            NamedCol {
                name: "RPRS_AND_MNTNCE_AMT",
                col: Col::F64(out_repairs),
            },
            NamedCol {
                name: "CNSL_RTS_AMT",
                col: Col::F64(out_council),
            },
            NamedCol {
                name: "WTR_CHRGS_AMT",
                col: Col::F64(out_water),
            },
            NamedCol {
                name: "LAND_TAX_AMT",
                col: Col::F64(out_land_tax),
            },
            NamedCol {
                name: "PRPTY_AGNT_FEES_OR_CMSN_AMT",
                col: Col::F64(out_agent_fees),
            },
            NamedCol {
                name: "BDY_CRPRT_FEES_AMT",
                col: Col::F64(out_body_corp),
            },
            NamedCol {
                name: "CAS_AMT",
                col: Col::F64(out_depreciation),
            },
            NamedCol {
                name: "EXPNSS_TOTL_AMT",
                col: Col::F64(out_total_exp),
            },
            NamedCol {
                name: "PRPTY_RNTD_WKS_THIS_YR_CNT",
                col: Col::I32(out_weeks_rented),
            },
            NamedCol {
                name: "STATE_CODE_2021",
                col: Col::I32(out_state),
            },
        ];
        write_columns_to_parquet(&out_path, cols)
            .unwrap_or_else(|e| panic!("rps parquet write: {}", e));
    }
    total as i32
}

extendr_module! {
    mod rps;
    fn project_rps__;
    fn project_rps_to_parquet__;
}

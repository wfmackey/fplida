use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::employment::person_year_wage_totals;
use crate::sampling::normal_sample;

/// Log-normal draw parametrised by its median and log-scale sigma (median > 0).
#[inline]
fn lognormal(rng: &mut StdRng, median: f64, sigma: f64) -> f64 {
    (median.ln() + normal_sample(rng, 0.0, sigma)).exp()
}

/// PIT_IE income components for one person-year.
struct IeComponents {
    wands: f64,
    invinc: f64,
    ownuninc: f64,
    supinc: f64,
    othinc: f64,
    totalinc: f64,
}

/// Draw the PIT_IE income components for one person-year. Component amounts are
/// right-skewed (log-normal), matching the shape of real derived-income
/// distributions, rather than the former truncated-Normal draws (e.g. INVINC
/// was `Normal(5000, 8000).max(0)`, a malformed half-normal). Shared by both
/// the list and the per-FY parquet generators so they cannot drift.
fn draw_ie_components(
    rng: &mut StdRng,
    fy: i32,
    age: i32,
    employed: bool,
    base_inc: f64,
) -> IeComponents {
    // Wages and salary: scaled from the spine baseline income (same basis as
    // PIT_PS) with FY growth and year-to-year noise.
    let wands = if employed && base_inc > 0.0 {
        let growth = 1.0 + 0.015 * (fy - 2015) as f64;
        let noise = normal_sample(rng, 1.0, 0.10);
        (base_inc * growth * noise).max(0.0).round()
    } else {
        0.0
    };

    // Investment income (interest/dividends/rent): right-skewed, more common and
    // larger for older persons.
    let p_inv = if age > 50 { 0.25 } else { 0.12 };
    let invinc = if rng.gen::<f64>() < p_inv {
        lognormal(rng, 2200.0, 1.2).round()
    } else {
        0.0
    };

    // Own unincorporated business income: right-skewed positive with an
    // occasional loss tail (business losses are real and ITR allows negatives).
    let ownuninc = if rng.gen::<f64>() < 0.08 {
        if rng.gen::<f64>() < 0.25 {
            -lognormal(rng, 9000.0, 0.9).round()
        } else {
            lognormal(rng, 22000.0, 0.9).round()
        }
    } else {
        0.0
    };

    // Superannuation income: common from age 60.
    let p_sup = if age >= 60 { 0.40 } else { 0.02 };
    let supinc = if rng.gen::<f64>() < p_sup {
        lognormal(rng, 20000.0, 0.6).round()
    } else {
        0.0
    };

    // Other income: small, infrequent.
    let othinc = if rng.gen::<f64>() < 0.05 {
        lognormal(rng, 1800.0, 0.9).round()
    } else {
        0.0
    };

    let totalinc = wands + invinc + ownuninc + supinc + othinc;
    IeComponents {
        wands,
        invinc,
        ownuninc,
        supinc,
        othinc,
        totalinc,
    }
}

/// Non-wage PIT_IE income components (investment, business, super, other), drawn
/// independently of the reconciled wages line — mirrors `draw_ie_components`
/// minus the wages draw. Returns (invinc, ownuninc, supinc, othinc).
fn draw_ie_nonwage(rng: &mut StdRng, age: i32) -> (f64, f64, f64, f64) {
    let p_inv = if age > 50 { 0.25 } else { 0.12 };
    let invinc = if rng.gen::<f64>() < p_inv {
        lognormal(rng, 2200.0, 1.2).round()
    } else {
        0.0
    };
    let ownuninc = if rng.gen::<f64>() < 0.08 {
        if rng.gen::<f64>() < 0.25 {
            -lognormal(rng, 9000.0, 0.9).round()
        } else {
            lognormal(rng, 22000.0, 0.9).round()
        }
    } else {
        0.0
    };
    let p_sup = if age >= 60 { 0.40 } else { 0.02 };
    let supinc = if rng.gen::<f64>() < p_sup {
        lognormal(rng, 20000.0, 0.6).round()
    } else {
        0.0
    };
    let othinc = if rng.gen::<f64>() < 0.05 {
        lognormal(rng, 1800.0, 0.9).round()
    } else {
        0.0
    };
    (invinc, ownuninc, supinc, othinc)
}

/// Project ABS Derived Income (PIT_IE) from spine.
///
/// One row per person per financial year. Income decomposed into
/// wages, investment, business, super, and other components.
/// @export
#[extendr]
fn project_pit_ie__(
    aeuid: Strings,
    birth_year: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    education: &[i32],
    seed: i64,
    fy_start: i32,
    fy_end: i32,
) -> List {
    let n = aeuid.len();
    let n_years = (fy_end - fy_start + 1) as usize;
    let est_rows = n * n_years;
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_fin_year: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_income_edited: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_wands: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_wands_earner: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_invinc: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_invinc_earner: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_ownuninc: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_ownuninc_earner: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_supinc: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_supinc_earner: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_othinc: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_othinc_earner: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_totalinc: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_totalinc_earner: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_nonlodger: Vec<String> = Vec::with_capacity(est_rows);

    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        for i in 0..n {
            let age = fy - birth_year[i];
            if age < 15 || age > 80 {
                continue;
            }

            let person_aeuid = aeuid[i].to_string();
            let employed = baseline_employed[i] == 1;
            let base_inc = baseline_income[i];

            let IeComponents {
                wands,
                invinc,
                ownuninc,
                supinc,
                othinc,
                totalinc,
            } = draw_ie_components(&mut rng, fy, age, employed, base_inc);

            // Skip persons with zero total income
            if totalinc.abs() < 1.0 && wands < 1.0 {
                continue;
            }

            let income_edited = totalinc + normal_sample(&mut rng, 0.0, 500.0);

            // Non-lodger: PS-only earners (employed but no ITR)
            let nonlodger = if employed && rng.gen::<f64>() < 0.15 {
                "PS"
            } else {
                "ITR"
            };

            let earner = |v: f64| -> &str {
                if v.abs() > 0.0 {
                    "E"
                } else {
                    "N"
                }
            };

            out_aeuid.push(person_aeuid);
            out_fin_year.push(fy_str.clone());
            out_income_edited.push(income_edited.round());
            out_wands.push(wands);
            out_wands_earner.push(earner(wands).to_string());
            out_invinc.push(invinc);
            out_invinc_earner.push(earner(invinc).to_string());
            out_ownuninc.push(ownuninc);
            out_ownuninc_earner.push(earner(ownuninc).to_string());
            out_supinc.push(supinc);
            out_supinc_earner.push(earner(supinc).to_string());
            out_othinc.push(othinc);
            out_othinc_earner.push(earner(othinc).to_string());
            out_totalinc.push(totalinc);
            out_totalinc_earner.push(earner(totalinc).to_string());
            out_nonlodger.push(nonlodger.to_string());
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        FIN_YEAR = out_fin_year,
        INCOME_EDITED = out_income_edited,
        WANDS = out_wands,
        WANDS_EARNER = out_wands_earner,
        INVINC = out_invinc,
        INVINC_EARNER = out_invinc_earner,
        OWNUNINC = out_ownuninc,
        OWNUNINC_EARNER = out_ownuninc_earner,
        SUPINC = out_supinc,
        SUPINC_EARNER = out_supinc_earner,
        OTHINC = out_othinc,
        OTHINC_EARNER = out_othinc_earner,
        TOTALINC = out_totalinc,
        TOTALINC_EARNER = out_totalinc_earner,
        NONLODGER = out_nonlodger
    )
}

/// Project PIT_IE directly to per-FY parquet files. Same logic as
/// `project_pit_ie__` but writes each year immediately, never
/// building a combined R list. Avoids R mem.maxVSize OOM at 30m.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_pit_ie_to_parquet__(
    id: Strings,
    aeuid: Strings,
    birth_year: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    baseline_hours: &[i32],
    anzsco_major: &[i32],
    anzsco_code: &[i32],
    industry: &[i32],
    archetype: &[i32],
    disability_onset_year: &[i32],
    disability_is_dc: &[i32],
    disability_severity: &[i32],
    disability_dose: &[f64],
    task_physical: &[f64],
    education: &[i32],
    seed: i64, // BASE seed (panel uses it; non-wage components use seed + 2100)
    fy_start: i32,
    fy_end: i32,
    out_dir: &str,
    product_fmt: &str, // e.g. "madipge-ato-d-pit-income-edited" — suffix is -%02d-%02d.parquet for fy<=2021, -fy%02d%02d.parquet otherwise
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let _ = education;

    let n = aeuid.len();
    // Non-wage components keep the legacy independent stream; the wages line is
    // the reconciled panel total (built with the BASE seed, like PAYG/STP).
    let mut rng = StdRng::seed_from_u64((seed + 2100) as u64);
    let aeuid_owned: Vec<String> = aeuid.iter().map(|s| s.to_string()).collect();

    // Reconciled per-(person, FY) wage gross from the shared wage ledger.
    let years_vec: Vec<i32> = (fy_start..=fy_end).collect();
    let wage_totals = person_year_wage_totals(
        id.iter().map(|s| s.to_string()).collect(),
        aeuid_owned.clone(),
        birth_year,
        baseline_employed,
        baseline_income,
        baseline_hours,
        anzsco_major,
        industry,
        seed as i32,
        &years_vec,
        disability_onset_year,
        disability_is_dc,
        disability_severity,
        disability_dose,
        anzsco_code,
        task_physical,
        archetype,
    );

    let mut total: usize = 0;
    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        let mut out_aeuid: Vec<String> = Vec::new();
        let mut out_fin_year: Vec<String> = Vec::new();
        let mut out_income_edited: Vec<f64> = Vec::new();
        let mut out_wands: Vec<f64> = Vec::new();
        let mut out_wands_earner: Vec<String> = Vec::new();
        let mut out_invinc: Vec<f64> = Vec::new();
        let mut out_invinc_earner: Vec<String> = Vec::new();
        let mut out_ownuninc: Vec<f64> = Vec::new();
        let mut out_ownuninc_earner: Vec<String> = Vec::new();
        let mut out_supinc: Vec<f64> = Vec::new();
        let mut out_supinc_earner: Vec<String> = Vec::new();
        let mut out_othinc: Vec<f64> = Vec::new();
        let mut out_othinc_earner: Vec<String> = Vec::new();
        let mut out_totalinc: Vec<f64> = Vec::new();
        let mut out_totalinc_earner: Vec<String> = Vec::new();
        let mut out_nonlodger: Vec<String> = Vec::new();

        for i in 0..n {
            let age = fy - birth_year[i];
            if age < 15 || age > 80 {
                continue;
            }

            // Reconciled wages from the shared panel ledger; non-wage drawn
            // independently (for every person, so the stream stays deterministic).
            let (invinc, ownuninc, supinc, othinc) = draw_ie_nonwage(&mut rng, age);
            // Exact sum of the (already cent-rounded) ledger entries, so WANDS
            // equals the sum of PAYG payment-summary gross to the cent.
            let wands = wage_totals
                .get(&(aeuid_owned[i].clone(), fy))
                .map(|v| v.0)
                .unwrap_or(0.0);
            let totalinc = wands + invinc + ownuninc + supinc + othinc;
            if totalinc.abs() < 1.0 && wands < 1.0 {
                continue;
            }

            let income_edited = totalinc + normal_sample(&mut rng, 0.0, 500.0);
            let nonlodger = if wands > 0.0 && rng.gen::<f64>() < 0.15 {
                "PS"
            } else {
                "ITR"
            };
            let earner = |v: f64| -> &str {
                if v.abs() > 0.0 {
                    "E"
                } else {
                    "N"
                }
            };

            out_aeuid.push(aeuid_owned[i].clone());
            out_fin_year.push(fy_str.clone());
            out_income_edited.push(income_edited.round());
            out_wands.push(wands);
            out_wands_earner.push(earner(wands).to_string());
            out_invinc.push(invinc);
            out_invinc_earner.push(earner(invinc).to_string());
            out_ownuninc.push(ownuninc);
            out_ownuninc_earner.push(earner(ownuninc).to_string());
            out_supinc.push(supinc);
            out_supinc_earner.push(earner(supinc).to_string());
            out_othinc.push(othinc);
            out_othinc_earner.push(earner(othinc).to_string());
            out_totalinc.push(totalinc);
            out_totalinc_earner.push(earner(totalinc).to_string());
            out_nonlodger.push(nonlodger.to_string());
        }

        total += out_aeuid.len();

        let fname = if fy <= 2021 {
            format!(
                "{}-{:02}-{:02}.parquet",
                product_fmt,
                (fy - 1) % 100,
                fy % 100
            )
        } else {
            format!(
                "{}-fy{:02}{:02}.parquet",
                product_fmt,
                (fy - 1) % 100,
                fy % 100
            )
        };
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
                name: "INCOME_EDITED",
                col: Col::F64(out_income_edited),
            },
            NamedCol {
                name: "WANDS",
                col: Col::F64(out_wands),
            },
            NamedCol {
                name: "WANDS_EARNER",
                col: Col::Str(out_wands_earner),
            },
            NamedCol {
                name: "INVINC",
                col: Col::F64(out_invinc),
            },
            NamedCol {
                name: "INVINC_EARNER",
                col: Col::Str(out_invinc_earner),
            },
            NamedCol {
                name: "OWNUNINC",
                col: Col::F64(out_ownuninc),
            },
            NamedCol {
                name: "OWNUNINC_EARNER",
                col: Col::Str(out_ownuninc_earner),
            },
            NamedCol {
                name: "SUPINC",
                col: Col::F64(out_supinc),
            },
            NamedCol {
                name: "SUPINC_EARNER",
                col: Col::Str(out_supinc_earner),
            },
            NamedCol {
                name: "OTHINC",
                col: Col::F64(out_othinc),
            },
            NamedCol {
                name: "OTHINC_EARNER",
                col: Col::Str(out_othinc_earner),
            },
            NamedCol {
                name: "TOTALINC",
                col: Col::F64(out_totalinc),
            },
            NamedCol {
                name: "TOTALINC_EARNER",
                col: Col::Str(out_totalinc_earner),
            },
            NamedCol {
                name: "NONLODGER",
                col: Col::Str(out_nonlodger),
            },
        ];
        write_columns_to_parquet(&out_path, cols)
            .unwrap_or_else(|e| panic!("pit_ie parquet write: {}", e));
    }

    total as i32
}

extendr_module! {
    mod pit_ie;
    fn project_pit_ie__;
    fn project_pit_ie_to_parquet__;
}

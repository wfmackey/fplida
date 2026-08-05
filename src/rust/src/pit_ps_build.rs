// PIT_PS sub-table construction in Rust.
//
// Ports the hot loop from R's .build_ps_table(). Takes the employment
// panel vectors (one row per person-year-employer) and computes the
// 14-column Payment Summary output: gross pay, tax withheld, SG,
// allowances, lump sums, union fees, workplace giving, etc.
//
// Random draws use a single StdRng seeded from `seed`. Deterministic
// within a slice worker's seed stream.

use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

#[inline]
pub(crate) fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

/// Compute simplified PAYG tax withholding.
#[inline]
pub(crate) fn compute_payg_tax(inc: f64) -> f64 {
    let inc = inc.max(0.0);
    let b2 = (inc - 18200.0).clamp(0.0, 45000.0 - 18200.0);
    let b3 = (inc - 45000.0).clamp(0.0, 120000.0 - 45000.0);
    let b4 = (inc - 120000.0).clamp(0.0, 180000.0 - 120000.0);
    let b5 = (inc - 180000.0).max(0.0);
    round2(b2 * 0.19 + b3 * 0.325 + b4 * 0.37 + b5 * 0.45)
}

/// Owned PS column output for one call of `build_ps_columns`.
pub struct PsColumns {
    pub aeuid: Vec<String>,
    pub year: Vec<i32>,
    pub employer: Vec<String>,
    pub gross: Vec<f64>,
    pub tax: Vec<f64>,
    pub fbt: Vec<f64>,
    pub sg: Vec<f64>,
    pub allow: Vec<f64>,
    pub lump_a: Vec<f64>,
    pub lump_b: Vec<f64>,
    pub lump_d: Vec<f64>,
    pub lump_e: Vec<f64>,
    pub union_fees: Vec<f64>,
    pub wg: Vec<f64>,
}

impl PsColumns {
    pub fn with_capacity(n: usize) -> Self {
        Self {
            aeuid: Vec::with_capacity(n),
            year: Vec::with_capacity(n),
            employer: Vec::with_capacity(n),
            gross: Vec::with_capacity(n),
            tax: Vec::with_capacity(n),
            fbt: Vec::with_capacity(n),
            sg: Vec::with_capacity(n),
            allow: Vec::with_capacity(n),
            lump_a: Vec::with_capacity(n),
            lump_b: Vec::with_capacity(n),
            lump_d: Vec::with_capacity(n),
            lump_e: Vec::with_capacity(n),
            union_fees: Vec::with_capacity(n),
            wg: Vec::with_capacity(n),
        }
    }
    pub fn len(&self) -> usize {
        self.aeuid.len()
    }
}

#[inline]
pub(crate) fn sg_rate_for_year(year: i32) -> f64 {
    match year {
        2015..=2020 => 0.095,
        2021 => 0.100,
        2022 => 0.105,
        2023 => 0.110,
        2024 => 0.115,
        _ => 0.10,
    }
}

/// Core PS column builder. Consumes the panel-row columns and produces
/// the 14 PS columns. Determinism is per-slice via the supplied `rng`.
pub fn build_ps_columns_core(
    aeuid_ato: Vec<String>,
    year: Vec<i32>,
    employer_id: Vec<String>,
    gross_annual: &[f64],
    anzsco_major: &[i32],
    rng: &mut StdRng,
) -> PsColumns {
    let n = aeuid_ato.len();
    let mut out = PsColumns::with_capacity(n);

    // Move owned inputs into per-row iteration; we consume them.
    for (i, ((aeuid_s, yr), emp_s)) in aeuid_ato
        .into_iter()
        .zip(year.into_iter())
        .zip(employer_id.into_iter())
        .enumerate()
    {
        let gross = gross_annual[i];
        let sg_r = sg_rate_for_year(yr);
        let tax = compute_payg_tax(gross);

        let fbt = if rng.gen::<f64>() < 0.08 {
            round2(gross * (0.02 + rng.gen::<f64>() * 0.06))
        } else {
            0.0
        };
        let allowances = if rng.gen::<f64>() < 0.15 {
            round2(500.0 + rng.gen::<f64>() * 4500.0)
        } else {
            0.0
        };
        let lump_a = if rng.gen::<f64>() < 0.03 {
            round2(200.0 + rng.gen::<f64>() * 2800.0)
        } else {
            0.0
        };
        let lump_b = if rng.gen::<f64>() < 0.005 {
            round2(500.0 + rng.gen::<f64>() * 9500.0)
        } else {
            0.0
        };
        let lump_d = if rng.gen::<f64>() < 0.003 {
            round2(200.0 + rng.gen::<f64>() * 4800.0)
        } else {
            0.0
        };
        let lump_e = if rng.gen::<f64>() < 0.002 {
            round2(500.0 + rng.gen::<f64>() * 7500.0)
        } else {
            0.0
        };

        let major = anzsco_major[i];
        let union_base_rate = if matches!(major, 3 | 4 | 7 | 8) {
            0.25
        } else {
            0.10
        };
        let union_fees = if rng.gen::<f64>() < union_base_rate {
            round2(200.0 + rng.gen::<f64>() * 1000.0)
        } else {
            0.0
        };
        let wg = if rng.gen::<f64>() < 0.05 {
            round2(50.0 + rng.gen::<f64>() * 450.0)
        } else {
            0.0
        };

        out.aeuid.push(aeuid_s);
        out.year.push(yr);
        out.employer.push(emp_s);
        out.gross.push(round2(gross));
        out.tax.push(tax);
        out.fbt.push(fbt);
        out.sg.push(round2(gross * sg_r));
        out.allow.push(allowances);
        out.lump_a.push(lump_a);
        out.lump_b.push(lump_b);
        out.lump_d.push(lump_d);
        out.lump_e.push(lump_e);
        out.union_fees.push(union_fees);
        out.wg.push(wg);
    }
    out
}

/// Build PS rows from the employment panel (extendr wrapper, kept for
/// legacy callers).
/// @export
#[extendr]
fn build_ps_table__(
    aeuid_ato: Strings,
    year: &[i32],
    employer_id: Strings,
    gross_annual: &[f64],
    anzsco_major: &[i32],
    seed: i64,
) -> List {
    let aeuid_v: Vec<String> = aeuid_ato.iter().map(|s| s.to_string()).collect();
    let year_v: Vec<i32> = year.to_vec();
    let emp_v: Vec<String> = employer_id.iter().map(|s| s.to_string()).collect();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let c = build_ps_columns_core(aeuid_v, year_v, emp_v, gross_annual, anzsco_major, &mut rng);
    list!(
        SYNTHETIC_AEUID = c.aeuid,
        FINANCIAL_YEAR = c.year,
        EMPLOYER_ABN = c.employer,
        GROSS_PAYMENTS = c.gross,
        TAX_WITHHELD = c.tax,
        REPORTABLE_FBT = c.fbt,
        SUPER_GUARANTEE = c.sg,
        ALLOWANCES = c.allow,
        LUMP_SUM_A = c.lump_a,
        LUMP_SUM_B = c.lump_b,
        LUMP_SUM_D = c.lump_d,
        LUMP_SUM_E = c.lump_e,
        UNION_FEES = c.union_fees,
        WORKPLACE_GIVING = c.wg
    )
}

extendr_module! {
    mod pit_ps_build;
    fn build_ps_table__;
}

use crate::disability;
use crate::sampling::normal_sample;
use crate::spine::anzsco_table::{ANZSCO_CODES, ARCHETYPE_RANGES};
use extendr_api::prelude::*;
use rand::prelude::*;
use rand::rngs::StdRng;

/// Deterministic employer identifier for a (global person number, employer
/// slot) pair. When a BLADE business pool is supplied the slot selects a real
/// business `bn` shared with STP (slot 0 = primary, `SECONDARY_SLOT` =
/// secondary); otherwise it falls back to the legacy 11-digit synthetic ABN.
fn make_employer_abn(person_number: i64, slot: i64, seed: i64, bn_pool: &[String]) -> String {
    crate::business_pool::employer_bn(bn_pool, person_number, slot, seed).unwrap_or_else(|| {
        let h = person_number
            .wrapping_mul(1_000_003)
            .wrapping_add(slot.wrapping_mul(999_983))
            .wrapping_add(seed)
            .rem_euclid(100_000_000_000);
        format!("{:011}", h)
    })
}

/// Age-band employer switch probability (Deutscher 2019 calibration).
fn switch_prob(age: i32) -> f64 {
    let rates: [f64; 11] = [
        0.28, 0.22, 0.16, 0.12, 0.10, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04,
    ];
    let band = (((age - 15).max(0)) / 5).min(10) as usize;
    rates[band]
}

/// Annual probability of employed -> not-employed.
fn exit_prob(age: i32) -> f64 {
    if age < 15 {
        1.0
    } else if age < 25 {
        0.10
    } else if age < 60 {
        0.04
    } else if age < 65 {
        0.08
    } else {
        0.15
    }
}

/// Annual probability of not-employed -> employed.
fn entry_prob(age: i32) -> f64 {
    if age < 15 {
        0.0
    } else if age < 20 {
        0.15
    } else if age < 60 {
        0.30
    } else if age < 65 {
        0.10
    } else if age < 70 {
        0.05
    } else {
        0.02
    }
}

/// Check if a person has disability (onset_year is not NA/i32::MIN).
#[inline]
fn has_disability(onset_year: i32) -> bool {
    onset_year != i32::MIN && onset_year > 0
}

/// Stable person-year draw for output-time disability employment effects.
///
/// The employment panel is anchored at 2021 so that the generated PAYG series
/// reconciles with the spine's baseline employment state. Applying disability
/// exits only through the forward transition walk misses retrospective onset
/// years. A deterministic output-time draw applies the cumulative event-time
/// employment gap symmetrically for pre- and post-2021 onset cohorts.
#[inline]
fn disability_employment_draw(person_number: i64, seed: i64, year: i32, salt: u64) -> f64 {
    let mut x = (person_number as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
        ^ (seed as u64).wrapping_mul(0xBF58_476D_1CE4_E5B9)
        ^ (year as u64).wrapping_mul(0x94D0_49BB_1331_11EB)
        ^ salt;
    x ^= x >> 30;
    x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x ^= x >> 27;
    x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^= x >> 31;
    ((x >> 11) as f64) * (1.0 / ((1u64 << 53) as f64))
}

/// Background occupation switching rate (~5% annual at 4-digit level).
const BACKGROUND_OCC_SWITCH_RATE: f64 = 0.05;

/// Per-(person, year) deterministic RNG for the employment-panel walk. Keying
/// every per-person draw to a stable hash of (person_number, seed, year, salt)
/// — instead of a single stream walked over persons×years — makes the panel
/// reproducible from the spine ALONE, independent of person order/set or year
/// range. This is what lets STP, PAYG and the PIT generators (which run in
/// separate slice processes over different person subsets) reconstruct the
/// IDENTICAL per-person wage/employer history and therefore reconcile.
#[inline]
fn panel_pp_rng(person_number: i64, seed: i64, year: i32, salt: u64) -> StdRng {
    let h = (person_number as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
        ^ (seed as u64).wrapping_mul(0xBF58_476D_1CE4_E5B9)
        ^ ((year as i64 as u64).wrapping_add(0x1000)).wrapping_mul(0x94D0_49BB_1331_11EB)
        ^ salt.wrapping_mul(0xD6E8_FEB8_6659_FD93);
    StdRng::seed_from_u64(h)
}

// ============================================================================
// Occupation switching: ANZSCO draw helpers
// ============================================================================

/// Target archetype weights for DC occupation switches.
/// DC workers move toward lower-physical-demand occupations.
/// Rows = source archetype (0-7), cols = target archetype (0-7).
const DC_SWITCH_TARGETS: [[f64; 8]; 8] = [
    // From Labourer (phys=0.75): → Office, Service, Health
    [0.05, 0.05, 0.20, 0.30, 0.10, 0.05, 0.05, 0.20],
    // From Skilled Trade (phys=0.70): → Technical, Office, Professional
    [0.05, 0.10, 0.10, 0.25, 0.20, 0.05, 0.15, 0.10],
    // From Health (phys=0.50): → Office, Professional
    [0.05, 0.05, 0.10, 0.30, 0.25, 0.10, 0.05, 0.10],
    // From Office (phys=0.15): → same (already low physical)
    [0.05, 0.05, 0.10, 0.30, 0.25, 0.10, 0.05, 0.10],
    // From Professional (phys=0.10): → same
    [0.05, 0.05, 0.05, 0.10, 0.40, 0.20, 0.10, 0.05],
    // From Manager (phys=0.10): → same
    [0.05, 0.05, 0.05, 0.10, 0.30, 0.30, 0.10, 0.05],
    // From Technical (phys=0.15): → Professional, Office
    [0.05, 0.05, 0.05, 0.20, 0.35, 0.15, 0.10, 0.05],
    // From Service (phys=0.20): → Office, Professional
    [0.05, 0.05, 0.10, 0.30, 0.20, 0.15, 0.05, 0.10],
];

/// Draw a new ANZSCO code from a given archetype range.
fn draw_anzsco_from_archetype(rng: &mut StdRng, archetype: usize) -> (i32, i32, i32) {
    let arch = archetype.min(7);
    let (start, end) = ARCHETYPE_RANGES[arch];
    let idx = rng.gen_range(start..end);
    let entry = &ANZSCO_CODES[idx];
    (
        entry.code as i32,
        entry.unit_code as i32,
        entry.major as i32,
    )
}

/// Draw a new occupation for background switching.
/// 80% same archetype, 20% random other archetype.
fn draw_background_switch(rng: &mut StdRng, current_archetype: usize) -> (i32, i32, i32, i32) {
    let target_arch = if rng.gen::<f64>() < 0.80 {
        current_archetype
    } else {
        rng.gen_range(0..8)
    };
    let (code, unit, major) = draw_anzsco_from_archetype(rng, target_arch);
    (code, unit, major, target_arch as i32)
}

/// Draw a new occupation for DC switching (toward lower physical demand).
fn draw_dc_switch(rng: &mut StdRng, current_archetype: usize) -> (i32, i32, i32, i32) {
    let arch = current_archetype.min(7);
    let weights = &DC_SWITCH_TARGETS[arch];
    let target_arch = crate::sampling::weighted_sample(rng, weights);
    let (code, unit, major) = draw_anzsco_from_archetype(rng, target_arch);
    (code, unit, major, target_arch as i32)
}

/// Output of `run_employment_panel` — owned column vectors.
pub struct EmploymentPanel {
    pub person_id: Vec<String>,
    pub person_idx: Vec<usize>,
    pub aeuid_ato: Vec<String>,
    pub year: Vec<i32>,
    pub employer_id: Vec<String>,
    /// Employer-spell index per row (0 = first employer, increments on each
    /// employer switch). Used to recover a switcher's prior employer.
    pub spell: Vec<i32>,
    pub primary_job: Vec<bool>,
    pub anzsco_major: Vec<i32>,
    pub anzsco_code: Vec<i32>,
    pub anzsco_unit: Vec<i32>,
    pub industry: Vec<i32>,
    pub hours_weekly: Vec<i32>,
    pub gross_annual: Vec<f64>,
    pub switched_employer: Vec<bool>,
    pub occ_switched: Vec<bool>,
    pub new_to_employment: Vec<bool>,
}

/// Core employment panel generator, callable from pure Rust. Owned-Vec
/// inputs for id/aeuid so it can be invoked without needing an `Strings`.
#[allow(clippy::too_many_arguments)]
pub fn run_employment_panel(
    id_vec: Vec<String>,
    aeuid_vec: Vec<String>,
    birth_year: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    baseline_hours: &[i32],
    anzsco_major: &[i32],
    industry: &[i32],
    seed: i32,
    years: &[i32],
    disability_onset_year: &[i32],
    disability_is_dc: &[i32],
    disability_severity: &[i32],
    disability_dose: &[f64],
    target_year: i32,
    anzsco_code: &[i32],
    _task_physical: &[f64],
    archetype: &[i32],
) -> EmploymentPanel {
    let n = birth_year.len();
    let n_years = years.len();
    // All per-person draws use panel_pp_rng(person, seed, year, salt) so the
    // panel is reproducible from the spine alone (see the walk loops below).
    let seed_i64 = seed as i64;
    // BLADE business-number pool (empty unless set from R before this runs).
    let bn_pool = crate::business_pool::snapshot();
    // Stable global person number per panel row, shared with STP so the two
    // products agree on each person's employers.
    let person_no: Vec<i64> = id_vec
        .iter()
        .map(|s| crate::business_pool::person_number(s))
        .collect();

    // Pre-compute person-level constants
    let age_2021: Vec<i32> = birth_year.iter().map(|&by| 2021 - by).collect();
    let employed: Vec<bool> = baseline_employed.iter().map(|&e| e == 1).collect();
    let log_income: Vec<f64> = baseline_income
        .iter()
        .map(|&inc| if inc > 0.0 { inc.ln() } else { f64::NAN })
        .collect();

    // Pre-compute disability flags — ALL severity levels now get employment effects.
    let dis_active: Vec<bool> = disability_onset_year
        .iter()
        .map(|&oy| has_disability(oy))
        .collect();
    let dis_dc: Vec<bool> = disability_is_dc.iter().map(|&v| v == 1).collect();

    // Estimate output capacity: ~80% employed × n_years × 1.07 multi-job
    let est_yr_count = if target_year == 0 { n_years } else { 1 };
    let est_rows = (n as f64 * est_yr_count as f64 * 0.80 * 1.07) as usize;

    let mut out_person_id: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_person_idx: Vec<usize> = Vec::with_capacity(est_rows);
    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_year: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_employer: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_spell: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_primary: Vec<bool> = Vec::with_capacity(est_rows);
    let mut out_anzsco_major: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_anzsco_code: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_anzsco_unit: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_industry: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_hours: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_gross: Vec<f64> = Vec::with_capacity(est_rows);
    let mut out_switched: Vec<bool> = Vec::with_capacity(est_rows);
    let mut out_occ_switched: Vec<bool> = Vec::with_capacity(est_rows);
    let mut out_new_entry: Vec<bool> = Vec::with_capacity(est_rows);

    // Current state vectors (mutable per-person state)
    // These track COUNTERFACTUAL log earnings (no disability effect)
    let mut cur_employed: Vec<bool> = employed.clone();
    let mut cur_spell: Vec<i32> = vec![0; n];
    let mut cur_log_earn: Vec<f64> = log_income.clone();
    let mut cur_hours: Vec<i32> = baseline_hours
        .iter()
        .map(|&h| if h == i32::MIN { 0 } else { h })
        .collect();
    // Time-varying occupation tracking
    let mut cur_anzsco_code: Vec<i32> = anzsco_code.to_vec();
    let mut cur_anzsco_unit: Vec<i32> = anzsco_code.iter().map(|&c| c / 100).collect();
    let mut cur_anzsco_major: Vec<i32> = anzsco_major.to_vec();
    let mut cur_archetype: Vec<i32> = archetype.to_vec();

    // NOTE: Employment state is a counterfactual trajectory anchored at 2021.
    // Observed disability exits are applied when rows are collected below, so
    // retrospective onset cohorts get the same event-time employment curve as
    // forward cohorts. The earnings disability shock is likewise applied to
    // counterfactual earnings via compute_observed_earnings at output time.

    // Sorted years for iteration
    let mut sorted_years: Vec<i32> = years.to_vec();
    sorted_years.sort();
    let anchor_idx = sorted_years.iter().position(|&y| y == 2021).unwrap_or(0);

    // Helper: compute observed log earnings (counterfactual + disability effect)
    let compute_observed_earnings = |i: usize, yr: i32, counterfactual_log_earn: f64| -> f64 {
        if !dis_active[i] {
            return counterfactual_log_earn;
        }
        let onset = disability_onset_year[i];
        let k = yr - onset;
        if k < 0 {
            return counterfactual_log_earn;
        }

        let effect =
            disability::earnings_effect(k, dis_dc[i], disability_severity[i], disability_dose[i])
                * disability::cohort_multiplier(onset);

        counterfactual_log_earn + effect // effect is negative
    };

    // Helper to collect one year's rows (with disability-adjusted earnings).
    macro_rules! collect_year_dis {
        ($yr:expr, $emp:expr, $spell:expr, $log_earn:expr, $hours:expr,
         $occ_code:expr, $occ_unit:expr, $occ_major:expr, $ind:expr,
         $switched:expr, $occ_sw:expr, $new_entry:expr) => {
            if target_year == 0 || $yr == target_year {
                for i in 0..n {
                    let mut observed_employed = $emp[i];
                    if observed_employed && dis_active[i] {
                        let onset = disability_onset_year[i];
                        let k = $yr - onset;
                        if k >= 0 {
                            let p_exit = (disability::employment_exit_effect(
                                k,
                                dis_dc[i],
                                disability_severity[i],
                            ) * disability::cohort_multiplier(onset))
                            .clamp(0.0, 0.95);
                            if disability_employment_draw(person_no[i], seed_i64, $yr, 430) < p_exit
                            {
                                observed_employed = false;
                            }
                        }
                    }

                    if observed_employed {
                        let observed = compute_observed_earnings(i, $yr, $log_earn[i]);
                        out_person_id.push(id_vec[i].clone());
                        out_person_idx.push(i + 1);
                        out_aeuid.push(aeuid_vec[i].clone());
                        out_year.push($yr);
                        out_employer.push(make_employer_abn(
                            person_no[i],
                            $spell[i] as i64,
                            seed_i64,
                            &bn_pool,
                        ));
                        out_spell.push($spell[i]);
                        out_primary.push(true);
                        out_anzsco_code.push($occ_code[i]);
                        out_anzsco_unit.push($occ_unit[i]);
                        out_anzsco_major.push($occ_major[i]);
                        out_industry.push($ind[i]);
                        out_hours.push($hours[i]);
                        out_gross.push(observed.exp().max(1.0).round());
                        out_switched.push($switched[i]);
                        out_occ_switched.push($occ_sw[i]);
                        out_new_entry.push($new_entry[i]);
                    }
                }
            }
        };
    }

    // ---- Record 2021 anchor ----
    let switched_2021 = vec![false; n];
    let occ_sw_2021 = vec![false; n];
    let new_entry_2021 = vec![false; n];
    collect_year_dis!(
        2021,
        cur_employed,
        cur_spell,
        cur_log_earn,
        cur_hours,
        cur_anzsco_code,
        cur_anzsco_unit,
        cur_anzsco_major,
        industry,
        switched_2021,
        occ_sw_2021,
        new_entry_2021
    );

    // Save anchor state for forward walk
    let anchor_employed = cur_employed.clone();
    let anchor_spell = cur_spell.clone();
    let anchor_log_earn = cur_log_earn.clone();
    let anchor_hours = cur_hours.clone();
    let anchor_anzsco_code = cur_anzsco_code.clone();
    let anchor_anzsco_unit = cur_anzsco_unit.clone();
    let anchor_anzsco_major = cur_anzsco_major.clone();
    let anchor_archetype = cur_archetype.clone();

    // ---- Walk backward (2020, 2019, ...) ----
    for yr_idx in (0..anchor_idx).rev() {
        let yr = sorted_years[yr_idx];
        let mut new_employed = vec![false; n];
        let mut switched = vec![false; n];
        let mut occ_sw = vec![false; n];
        let mut new_entry_flag = vec![false; n];
        let mut new_spell = cur_spell.clone();
        let mut new_log_earn = cur_log_earn.clone();
        let mut new_hours = cur_hours.clone();
        let mut new_anzsco_code = cur_anzsco_code.clone();
        let mut new_anzsco_unit = cur_anzsco_unit.clone();
        let mut new_anzsco_major = cur_anzsco_major.clone();
        let mut new_archetype = cur_archetype.clone();

        for i in 0..n {
            // Per-person deterministic streams (shadow the order-dependent
            // outer rng/rng_occ) so the panel reconstructs identically anywhere.
            let mut rng = panel_pp_rng(person_no[i], seed_i64, yr, 0xB1);
            let mut rng_occ = panel_pp_rng(person_no[i], seed_i64, yr, 0xB2);
            let age_yr = age_2021[i] - (2021 - yr);
            let p_stay = 1.0 - exit_prob(age_yr);
            let p_was = exit_prob(age_yr) * 0.5;

            // Backward walk: NO disability employment effects.
            // The backward walk generates COUNTERFACTUAL employment trajectories.
            // Disability earnings shocks are applied at output via compute_observed_earnings.
            // Disability employment exit is handled only in the forward walk.

            let u: f64 = rng.gen();
            if cur_employed[i] {
                new_employed[i] = u < p_stay;
            } else {
                new_employed[i] = u < p_was;
            }
            if age_yr < 15 {
                new_employed[i] = false;
            }

            // Pre-onset employment guarantee for disabled workers.
            // Disability is assigned only to baseline_employed workers, so they
            // MUST have been employed at all years before onset. This prevents
            // survivorship bias in pre-event coefficients.
            if dis_active[i] && yr < disability_onset_year[i] && employed[i] {
                new_employed[i] = true;
            }

            // Employer switching
            if new_employed[i] && cur_employed[i] {
                if rng.gen::<f64>() < switch_prob(age_yr) {
                    switched[i] = true;
                    new_spell[i] += 1;
                }
            }

            // Occupation switching — background only in backward walk.
            // Disabled workers' occupation is handled by post-processing
            // forward pass (after both walks). Skip switching for disabled
            // workers entirely to avoid reverse-time artifacts.
            if new_employed[i] && cur_employed[i] {
                let occ_rate = BACKGROUND_OCC_SWITCH_RATE;
                let occ_draw = rng_occ.gen::<f64>();
                // Skip switching for disabled workers — post-processing handles it
                if occ_draw < occ_rate && !dis_active[i] {
                    occ_sw[i] = true;
                    if !switched[i] {
                        switched[i] = true;
                        new_spell[i] += 1;
                    }
                    let arch = new_archetype[i].max(0).min(7) as usize;
                    let (code, unit, major, n_arch) = draw_background_switch(&mut rng_occ, arch);
                    new_anzsco_code[i] = code;
                    new_anzsco_unit[i] = unit;
                    new_anzsco_major[i] = major;
                    new_archetype[i] = n_arch;
                }
            }

            // For disabled workers: restore original occupation for ALL years
            // in backward walk. Post-processing will generate the correct
            // forward trajectory from onset.
            if dis_active[i] {
                new_anzsco_code[i] = anzsco_code[i];
                new_anzsco_unit[i] = anzsco_code[i] / 100;
                new_anzsco_major[i] = anzsco_major[i];
                new_archetype[i] = archetype[i];
            }

            // Earnings walk (backward) — counterfactual only
            let stayer_growth = normal_sample(&mut rng, 0.058, 0.10);
            let switcher_premium = normal_sample(&mut rng, 0.10, 0.05);

            if new_employed[i] && cur_employed[i] && !switched[i] {
                new_log_earn[i] = cur_log_earn[i] - stayer_growth;
            } else if switched[i] {
                new_log_earn[i] = cur_log_earn[i] - stayer_growth - switcher_premium;
            } else if new_employed[i] && !cur_employed[i] {
                new_entry_flag[i] = true;
                let yrs_from_anchor = (2021 - yr) as f64;
                if !log_income[i].is_nan() {
                    new_log_earn[i] = log_income[i] - 0.058 * yrs_from_anchor
                        + normal_sample(&mut rng, 0.0, 0.15);
                } else {
                    new_log_earn[i] = normal_sample(&mut rng, 10.8, 0.5);
                }
            }

            // Hours variation
            if new_employed[i] && rng.gen::<f64>() < 0.10 {
                new_hours[i] = if rng.gen::<f64>() < 0.65 { 38 } else { 20 };
            }
        }

        collect_year_dis!(
            yr,
            new_employed,
            new_spell,
            new_log_earn,
            new_hours,
            new_anzsco_code,
            new_anzsco_unit,
            new_anzsco_major,
            industry,
            switched,
            occ_sw,
            new_entry_flag
        );

        cur_employed = new_employed;
        cur_spell = new_spell;
        cur_log_earn = new_log_earn;
        cur_hours = new_hours;
        cur_anzsco_code = new_anzsco_code;
        cur_anzsco_unit = new_anzsco_unit;
        cur_anzsco_major = new_anzsco_major;
        cur_archetype = new_archetype;
    }

    // ---- Reset state to 2021 and walk forward ----
    cur_employed = anchor_employed;
    cur_spell = anchor_spell;
    cur_log_earn = anchor_log_earn;
    cur_hours = anchor_hours;
    cur_anzsco_code = anchor_anzsco_code;
    cur_anzsco_unit = anchor_anzsco_unit;
    cur_anzsco_major = anchor_anzsco_major;
    cur_archetype = anchor_archetype;

    for yr_idx in (anchor_idx + 1)..sorted_years.len() {
        let yr = sorted_years[yr_idx];
        let mut new_employed = vec![false; n];
        let mut switched = vec![false; n];
        let mut occ_sw = vec![false; n];
        let mut new_entry_flag = vec![false; n];
        let mut new_spell = cur_spell.clone();
        let mut new_log_earn = cur_log_earn.clone();
        let mut new_hours = cur_hours.clone();
        let mut new_anzsco_code = cur_anzsco_code.clone();
        let mut new_anzsco_unit = cur_anzsco_unit.clone();
        let mut new_anzsco_major = cur_anzsco_major.clone();
        let mut new_archetype = cur_archetype.clone();

        for i in 0..n {
            // Per-person deterministic streams (shadow the outer rng/rng_occ).
            let mut rng = panel_pp_rng(person_no[i], seed_i64, yr, 0xF1);
            let mut rng_occ = panel_pp_rng(person_no[i], seed_i64, yr, 0xF2);
            let age_yr = age_2021[i] + (yr - 2021);
            let p_stay = 1.0 - exit_prob(age_yr);
            let p_enter = entry_prob(age_yr);

            // Disability employment treatment is applied at output time so
            // both retrospective and prospective onset cohorts use the same
            // event-time curve. Keep this transition walk counterfactual.

            let u: f64 = rng.gen();
            if cur_employed[i] {
                new_employed[i] = u < p_stay;
            } else {
                new_employed[i] = u < p_enter;
            }
            if age_yr < 15 {
                new_employed[i] = false;
            }

            // Employer switching
            if new_employed[i] && cur_employed[i] {
                if rng.gen::<f64>() < switch_prob(age_yr) {
                    switched[i] = true;
                    new_spell[i] += 1;
                }
            }

            // Occupation switching (separate from employer switching)
            if new_employed[i] && cur_employed[i] {
                let mut occ_rate = BACKGROUND_OCC_SWITCH_RATE;
                if dis_active[i] {
                    let k = yr - disability_onset_year[i];
                    occ_rate += disability::occ_switch_boost(k, dis_dc[i]);
                    occ_rate = occ_rate.min(0.50);
                }
                if rng_occ.gen::<f64>() < occ_rate {
                    occ_sw[i] = true;
                    if !switched[i] {
                        switched[i] = true;
                        new_spell[i] += 1;
                    }
                    let arch = new_archetype[i].max(0).min(7) as usize;
                    let (code, unit, major, n_arch) = if dis_active[i] && dis_dc[i] {
                        let k = yr - disability_onset_year[i];
                        if k >= 0 {
                            draw_dc_switch(&mut rng_occ, arch)
                        } else {
                            draw_background_switch(&mut rng_occ, arch)
                        }
                    } else {
                        draw_background_switch(&mut rng_occ, arch)
                    };
                    new_anzsco_code[i] = code;
                    new_anzsco_unit[i] = unit;
                    new_anzsco_major[i] = major;
                    new_archetype[i] = n_arch;
                }
            }

            // Earnings walk (forward) — counterfactual only
            let stayer_growth = normal_sample(&mut rng, 0.058, 0.10);
            let switcher_premium = normal_sample(&mut rng, 0.10, 0.05);

            if new_employed[i] && cur_employed[i] && !switched[i] {
                new_log_earn[i] = cur_log_earn[i] + stayer_growth;
            } else if switched[i] {
                new_log_earn[i] = cur_log_earn[i] + stayer_growth + switcher_premium;
            } else if new_employed[i] && !cur_employed[i] {
                new_entry_flag[i] = true;
                new_spell[i] += 1;
                let yrs_from_anchor = (yr - 2021) as f64;
                if !log_income[i].is_nan() {
                    new_log_earn[i] = log_income[i]
                        + 0.058 * yrs_from_anchor
                        + normal_sample(&mut rng, 0.0, 0.15);
                } else {
                    new_log_earn[i] = normal_sample(&mut rng, 10.8, 0.5);
                }
            }

            // Hours variation
            if new_employed[i] && rng.gen::<f64>() < 0.10 {
                new_hours[i] = if rng.gen::<f64>() < 0.65 { 38 } else { 20 };
            }
        }

        collect_year_dis!(
            yr,
            new_employed,
            new_spell,
            new_log_earn,
            new_hours,
            new_anzsco_code,
            new_anzsco_unit,
            new_anzsco_major,
            industry,
            switched,
            occ_sw,
            new_entry_flag
        );

        cur_employed = new_employed;
        cur_spell = new_spell;
        cur_log_earn = new_log_earn;
        cur_hours = new_hours;
        cur_anzsco_code = new_anzsco_code;
        cur_anzsco_unit = new_anzsco_unit;
        cur_anzsco_major = new_anzsco_major;
        cur_archetype = new_archetype;
    }

    // ---- Post-processing: Fix disabled workers' occupation trajectories ----
    // The backward walk sets all disabled workers' occupation to baseline.
    // The forward walk applies DC/NC boost but only covers 2022+.
    // This pass generates a CONSISTENT forward trajectory from onset for
    // each disabled worker, overwriting occupation in the output vectors.
    {
        // Build per-person row index: person_idx (1-based) → Vec of (year, row_idx)
        let mut person_rows: Vec<Vec<(i32, usize)>> = vec![Vec::new(); n + 1];
        for j in 0..out_person_id.len() {
            let pidx = out_person_idx[j];
            person_rows[pidx].push((out_year[j], j));
        }

        for i in 0..n {
            if !dis_active[i] {
                continue;
            }
            // Per-person deterministic stream for the disability occupation fix.
            let mut rng_occ_fix = panel_pp_rng(person_no[i], seed_i64, 0, 0x0CC);
            let onset = disability_onset_year[i];
            let person_key = i + 1;

            let rows = &mut person_rows[person_key];
            if rows.is_empty() {
                continue;
            }

            // Sort by year
            rows.sort_by_key(|&(yr, _)| yr);

            // Start from baseline occupation at onset
            let mut cur_code = anzsco_code[i];
            let mut cur_unit = anzsco_code[i] / 100;
            let mut cur_major = anzsco_major[i];
            let mut cur_arch = archetype[i].max(0).min(7) as usize;
            let mut prev_yr = onset - 1;

            for &(yr, j) in rows.iter() {
                if yr < onset {
                    // Pre-onset: already baseline (set in backward walk), skip
                    continue;
                }

                // Apply switching once per year (not per row within same year)
                if yr != prev_yr {
                    let k = yr - onset;
                    let mut occ_rate = BACKGROUND_OCC_SWITCH_RATE;
                    occ_rate += disability::occ_switch_boost(k, dis_dc[i]);
                    occ_rate = occ_rate.min(0.50);

                    if rng_occ_fix.gen::<f64>() < occ_rate {
                        let (code, unit, major, n_arch) = if dis_dc[i] {
                            draw_dc_switch(&mut rng_occ_fix, cur_arch)
                        } else {
                            draw_background_switch(&mut rng_occ_fix, cur_arch)
                        };
                        cur_code = code;
                        cur_unit = unit;
                        cur_major = major;
                        cur_arch = n_arch as usize;
                    }
                    prev_yr = yr;
                }

                // Update output
                out_anzsco_code[j] = cur_code;
                out_anzsco_unit[j] = cur_unit;
                out_anzsco_major[j] = cur_major;
                out_occ_switched[j] = cur_code != anzsco_code[i];
            }
        }
    }

    // ---- Multi-job expansion: ~7% get a secondary job ----
    let n_primary = out_person_id.len();
    let mut sec_pid: Vec<String> = Vec::new();
    let mut sec_pidx: Vec<usize> = Vec::new();
    let mut sec_aeu: Vec<String> = Vec::new();
    let mut sec_yr: Vec<i32> = Vec::new();
    let mut sec_emp: Vec<String> = Vec::new();
    let mut sec_anz_code: Vec<i32> = Vec::new();
    let mut sec_anz_unit: Vec<i32> = Vec::new();
    let mut sec_anz_major: Vec<i32> = Vec::new();
    let mut sec_ind: Vec<i32> = Vec::new();
    let mut sec_hrs: Vec<i32> = Vec::new();
    let mut sec_grs: Vec<f64> = Vec::new();

    macro_rules! push_secondary {
        ($j:expr, $sec_share:expr) => {{
            sec_pid.push(out_person_id[$j].clone());
            sec_pidx.push(out_person_idx[$j]);
            sec_aeu.push(out_aeuid[$j].clone());
            sec_yr.push(out_year[$j]);
            let person_pn = crate::business_pool::person_number(&out_person_id[$j]);
            sec_emp.push(make_employer_abn(
                person_pn,
                crate::business_pool::SECONDARY_SLOT,
                seed_i64,
                &bn_pool,
            ));
            sec_anz_code.push(out_anzsco_code[$j]);
            sec_anz_unit.push(out_anzsco_unit[$j]);
            sec_anz_major.push(out_anzsco_major[$j]);
            sec_ind.push(out_industry[$j]);
            sec_hrs.push(out_hours[$j]);
            sec_grs.push((out_gross[$j] * $sec_share * 100.0).round() / 100.0);
        }};
    }

    // Per-(person, year) multi-job draw so streaming (per-month STP / per-year
    // PAYG) and all-year (PIT) callers agree on who holds a secondary job.
    for j in 0..n_primary {
        let pn = crate::business_pool::person_number(&out_person_id[j]);
        let mut rng_m = panel_pp_rng(pn, seed_i64, out_year[j], 0x5EC);
        if rng_m.gen::<f64>() < 0.07 {
            let sec_share: f64 = rng_m.gen_range(0.20..0.40);
            push_secondary!(j, sec_share);
        }
    }

    // Append secondary rows
    let n_sec = sec_pid.len();
    out_person_id.extend(sec_pid);
    out_person_idx.extend(sec_pidx);
    out_aeuid.extend(sec_aeu);
    out_year.extend(sec_yr);
    out_employer.extend(sec_emp);
    out_spell.extend(vec![0i32; n_sec]);
    out_primary.extend(vec![false; n_sec]);
    out_anzsco_code.extend(sec_anz_code);
    out_anzsco_unit.extend(sec_anz_unit);
    out_anzsco_major.extend(sec_anz_major);
    out_industry.extend(sec_ind);
    out_hours.extend(sec_hrs);
    out_gross.extend(sec_grs);
    out_switched.extend(vec![false; n_sec]);
    out_occ_switched.extend(vec![false; n_sec]);
    out_new_entry.extend(vec![false; n_sec]);

    EmploymentPanel {
        person_id: out_person_id,
        person_idx: out_person_idx,
        aeuid_ato: out_aeuid,
        year: out_year,
        employer_id: out_employer,
        spell: out_spell,
        primary_job: out_primary,
        anzsco_major: out_anzsco_major,
        anzsco_code: out_anzsco_code,
        anzsco_unit: out_anzsco_unit,
        industry: out_industry,
        hours_weekly: out_hours,
        gross_annual: out_gross,
        switched_employer: out_switched,
        occ_switched: out_occ_switched,
        new_to_employment: out_new_entry,
    }
}

// ============================================================================
// Shared wage ledger — the canonical per-(person, year, employer) salary +
// withholding record that STP, PAYG and PIT all derive from so they reconcile.
// ============================================================================

/// Salary/wages income classification for a ledger entry. `Regular` for normal
/// employment; the rest are the STP non-standard income types (contractor,
/// labour-hire, working-holiday-maker, voluntary agreement,
/// community-development).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IncomeType {
    Regular,
    Contractor,
    LabourHire,
    Whm,
    Va,
    CommunityDev,
}

/// One employer's wage record for a person in a financial year. STP realises
/// this as pay events (summing to `gross`/`withholding`); PAYG writes it as one
/// payment summary; PIT sums it (over employers) into the salary/wages line and
/// withholding credit. `year` is the FY-end year (panel year).
#[derive(Clone)]
pub struct WageEntry {
    pub person_idx: usize,
    pub aeuid_ato: String,
    pub person_id: String,
    pub year: i32,
    pub employer_abn: String,
    pub gross: f64,
    pub withholding: f64,
    pub primary: bool,
    pub spell: i32,
    pub income_type: IncomeType,
    /// First and last active month within the FY (1..=12), for STP pay timing.
    pub start_month: i32,
    pub end_month: i32,
    /// The employer relationship ended at `end_month` (drives STP cessation/ETP).
    pub ceased: bool,
}

/// The per-person mid-year employer-switch month (1..=11) for a (person, year).
/// Shared by `expand_wage_ledger` and the PAYG generator so the gross split
/// across the prior/new employer is identical and reconciles.
pub fn ledger_switch_month(person_number: i64, seed: i64, year: i32) -> i32 {
    panel_pp_rng(person_number, seed, year, 0x53C).gen_range(1..=11)
}

/// Classify a non-standard income type for a ledger entry. Deterministic in
/// (person, year). Only a minority of employer-spells are non-standard; the
/// rest stay `Regular`.
fn classify_income_type(person_number: i64, seed: i64, year: i32, primary: bool) -> IncomeType {
    let mut r = panel_pp_rng(person_number, seed, year, 0x17C);
    let u: f64 = r.gen();
    // Secondary/short jobs are likelier to be labour-hire / contractor / WHM.
    let (p_contract, p_labhire, p_whm, p_va, p_cdep) = if primary {
        (0.04, 0.05, 0.015, 0.01, 0.004)
    } else {
        (0.08, 0.12, 0.05, 0.02, 0.01)
    };
    let mut acc = p_contract;
    if u < acc {
        return IncomeType::Contractor;
    }
    acc += p_labhire;
    if u < acc {
        return IncomeType::LabourHire;
    }
    acc += p_whm;
    if u < acc {
        return IncomeType::Whm;
    }
    acc += p_va;
    if u < acc {
        return IncomeType::Va;
    }
    acc += p_cdep;
    if u < acc {
        return IncomeType::CommunityDev;
    }
    IncomeType::Regular
}

/// Expand the employment panel into the canonical per-employer wage ledger.
/// Mid-year employer switches become two entries (old + new employer, gross
/// split by a per-person switch month); secondary jobs are their own entries.
/// Withholding is `compute_payg_tax` on each entry's gross — one rule, so STP,
/// PAYG and PIT all agree. Deterministic in (panel, seed): every generator that
/// rebuilds the same panel rebuilds the same ledger.
pub fn expand_wage_ledger(panel: &EmploymentPanel, seed: i64) -> Vec<WageEntry> {
    use crate::pit_ps_build::{compute_payg_tax, round2};
    let bn_pool = crate::business_pool::snapshot();
    let n = panel.year.len();
    let mut out: Vec<WageEntry> = Vec::with_capacity(n + n / 12 + 1);

    for j in 0..n {
        let yr = panel.year[j];
        let gross = panel.gross_annual[j];
        let primary = panel.primary_job[j];
        let switched = panel.switched_employer[j];
        let new_entry = panel.new_to_employment[j];
        let pid = panel.person_id[j].clone();
        let aeu = panel.aeuid_ato[j].clone();
        let pidx = panel.person_idx[j];
        let pn = crate::business_pool::person_number(&pid);
        let itype = classify_income_type(pn, seed, yr, primary);

        if switched && primary {
            // Mid-year employer switch: split the annual gross across the prior
            // and current employer by a per-person switch month (reproducible
            // across generators, unlike the old per-year stream).
            let switch_month = ledger_switch_month(pn, seed, yr);
            let old_share = switch_month as f64 / 12.0;
            let new_share = 1.0 - old_share;
            let prior_slot = (panel.spell[j] as i64 - 1).max(0);
            let prior_abn = make_employer_abn(pn, prior_slot, seed, &bn_pool);
            let g_old = round2(gross * old_share);
            let g_new = round2(gross * new_share);
            out.push(WageEntry {
                person_idx: pidx,
                aeuid_ato: aeu.clone(),
                person_id: pid.clone(),
                year: yr,
                employer_abn: prior_abn,
                gross: g_old,
                withholding: compute_payg_tax(g_old),
                primary: true,
                spell: panel.spell[j] - 1,
                income_type: IncomeType::Regular,
                start_month: 1,
                end_month: switch_month,
                ceased: true,
            });
            out.push(WageEntry {
                person_idx: pidx,
                aeuid_ato: aeu,
                person_id: pid,
                year: yr,
                employer_abn: panel.employer_id[j].clone(),
                gross: g_new,
                withholding: compute_payg_tax(g_new),
                primary: true,
                spell: panel.spell[j],
                income_type: itype,
                start_month: switch_month + 1,
                end_month: 12,
                ceased: false,
            });
        } else {
            // Single employer for the year. A new entrant starts mid-year.
            let start_month = if new_entry {
                let mut r = panel_pp_rng(pn, seed, yr, 0x5AE);
                r.gen_range(1..=9)
            } else {
                1
            };
            out.push(WageEntry {
                person_idx: pidx,
                aeuid_ato: aeu,
                person_id: pid,
                year: yr,
                employer_abn: panel.employer_id[j].clone(),
                gross,
                withholding: compute_payg_tax(gross),
                primary,
                spell: panel.spell[j],
                income_type: itype,
                start_month,
                end_month: 12,
                ceased: false,
            });
        }
    }
    out
}

/// Per-(aeuid_ato, FY-end year) total wage gross + PAYG withholding, summed over
/// employers, from the canonical wage ledger. PIT_IE (WANDS) and PIT_ITR (gross
/// payments + withholding credit) look this up so their salary line reconciles
/// with the sum of PAYG payment summaries and STP pay events. The panel is built
/// with the BASE seed (the same one PAYG and STP use); callers keep their own
/// non-wage draws on a separate offset stream.
#[allow(clippy::too_many_arguments)]
pub fn person_year_wage_totals(
    id_vec: Vec<String>,
    aeuid_vec: Vec<String>,
    birth_year: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    baseline_hours: &[i32],
    anzsco_major: &[i32],
    industry: &[i32],
    seed: i32,
    years: &[i32],
    disability_onset_year: &[i32],
    disability_is_dc: &[i32],
    disability_severity: &[i32],
    disability_dose: &[f64],
    anzsco_code: &[i32],
    task_physical: &[f64],
    archetype: &[i32],
) -> std::collections::HashMap<(String, i32), (f64, f64)> {
    let panel = run_employment_panel(
        id_vec,
        aeuid_vec,
        birth_year,
        baseline_employed,
        baseline_income,
        baseline_hours,
        anzsco_major,
        industry,
        seed,
        years,
        disability_onset_year,
        disability_is_dc,
        disability_severity,
        disability_dose,
        0,
        anzsco_code,
        task_physical,
        archetype,
    );
    let ledger = expand_wage_ledger(&panel, seed as i64);
    let mut m: std::collections::HashMap<(String, i32), (f64, f64)> =
        std::collections::HashMap::with_capacity(ledger.len());
    for e in &ledger {
        let v = m.entry((e.aeuid_ato.clone(), e.year)).or_insert((0.0, 0.0));
        v.0 += e.gross;
        v.1 += e.withholding;
    }
    m
}

/// Generate the longitudinal employment panel from spine vectors.
///
/// Thin extendr adapter around `run_employment_panel`.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn generate_employment_panel__(
    id: Strings,
    aeuid_ato: Strings,
    birth_year: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    baseline_hours: &[i32],
    anzsco_major: &[i32],
    industry: &[i32],
    seed: i32,
    years: &[i32],
    disability_onset_year: &[i32],
    disability_is_dc: &[i32],
    disability_severity: &[i32],
    disability_dose: &[f64],
    target_year: i32,
    anzsco_code: &[i32],
    task_physical: &[f64],
    archetype: &[i32],
) -> List {
    let id_vec: Vec<String> = id.iter().map(|s| s.to_string()).collect();
    let aeuid_vec: Vec<String> = aeuid_ato.iter().map(|s| s.to_string()).collect();
    let p = run_employment_panel(
        id_vec,
        aeuid_vec,
        birth_year,
        baseline_employed,
        baseline_income,
        baseline_hours,
        anzsco_major,
        industry,
        seed,
        years,
        disability_onset_year,
        disability_is_dc,
        disability_severity,
        disability_dose,
        target_year,
        anzsco_code,
        task_physical,
        archetype,
    );
    list!(
        person_id = p.person_id,
        aeuid_ato = p.aeuid_ato,
        year = p.year,
        employer_id = p.employer_id,
        primary_job = p.primary_job,
        anzsco_code = p.anzsco_code,
        anzsco_unit = p.anzsco_unit,
        anzsco_major = p.anzsco_major,
        industry = p.industry,
        hours_weekly = p.hours_weekly,
        gross_annual = p.gross_annual,
        switched_employer = p.switched_employer,
        occ_switched = p.occ_switched,
        new_to_employment = p.new_to_employment,
    )
}

/// Per-(person, FY-end year) reconciled wage gross + PAYG withholding, summed
/// over employers from the shared wage ledger. Returned as long vectors
/// (aeuid_ato, fy, gross, withholding). STP anchors its pay events to `gross`
/// so a person's STP income tracks the same panel total as their PAYG payment
/// summaries and PIT salary line. Built with the BASE seed.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn person_fy_wages__(
    id: Strings,
    aeuid_ato: Strings,
    birth_year: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    baseline_hours: &[i32],
    anzsco_major: &[i32],
    industry: &[i32],
    seed: i32,
    years: &[i32],
    disability_onset_year: &[i32],
    disability_is_dc: &[i32],
    disability_severity: &[i32],
    disability_dose: &[f64],
    anzsco_code: &[i32],
    task_physical: &[f64],
    archetype: &[i32],
) -> List {
    let totals = person_year_wage_totals(
        id.iter().map(|s| s.to_string()).collect(),
        aeuid_ato.iter().map(|s| s.to_string()).collect(),
        birth_year,
        baseline_employed,
        baseline_income,
        baseline_hours,
        anzsco_major,
        industry,
        seed,
        years,
        disability_onset_year,
        disability_is_dc,
        disability_severity,
        disability_dose,
        anzsco_code,
        task_physical,
        archetype,
    );
    let mut out_aeuid: Vec<String> = Vec::with_capacity(totals.len());
    let mut out_fy: Vec<i32> = Vec::with_capacity(totals.len());
    let mut out_gross: Vec<f64> = Vec::with_capacity(totals.len());
    let mut out_whld: Vec<f64> = Vec::with_capacity(totals.len());
    for ((aeu, fy), (g, w)) in totals {
        out_aeuid.push(aeu);
        out_fy.push(fy);
        out_gross.push(g);
        out_whld.push(w);
    }
    list!(
        aeuid_ato = out_aeuid,
        fy = out_fy,
        gross = out_gross,
        withholding = out_whld
    )
}

extendr_module! {
    mod employment;
    fn generate_employment_panel__;
    fn person_fy_wages__;
}

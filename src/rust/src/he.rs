use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;

use crate::nominal;
use crate::sampling::{normal_sample, weighted_sample};

// ==========================================================================
// Constants (from inst/foundations/he.toml and R/generate_he.R)
// ==========================================================================

// Participation rate by education code (0..5)
const PARTICIPATION_RATE: [f64; 6] = [0.0, 0.02, 0.02, 0.05, 0.20, 1.0];

// Qualification shares for education==5
const QUAL_SHARES_EDU5: [f64; 6] = [0.65, 0.03, 0.08, 0.12, 0.03, 0.05];

// Qualification shares for education<5 (attempt distribution)
const QUAL_SHARES_ATTEMPT: [f64; 6] = [0.85, 0.005, 0.05, 0.05, 0.005, 0.005];

// FT duration in years by qualification index (0-based: Bachelor..PhD)
const FT_DURATION: [i32; 6] = [3, 1, 1, 2, 2, 4];

// Completion rate by qualification
const COMPLETION_RATE: [f64; 6] = [0.75, 0.85, 0.80, 0.80, 0.70, 0.65];

// FOE codes and shares
const FOE_CODES: [&str; 11] = [
    "0801", "0901", "0401", "0301", "0501", "0603", "0201", "0701", "1001", "0101", "0999",
];
const FOE_SHARES: [f64; 11] = [
    0.20, 0.12, 0.10, 0.10, 0.08, 0.08, 0.05, 0.05, 0.07, 0.02, 0.13,
];

// Archetype-to-FOE affinity (8 archetypes x 11 FOE codes)
const ARCHETYPE_FOE: [[f64; 11]; 8] = [
    [
        0.15, 0.15, 0.15, 0.05, 0.05, 0.05, 0.05, 0.10, 0.05, 0.02, 0.18,
    ], // Labourer
    [
        0.15, 0.05, 0.05, 0.30, 0.02, 0.02, 0.05, 0.03, 0.08, 0.05, 0.20,
    ], // SkilledTrade
    [
        0.05, 0.05, 0.05, 0.02, 0.25, 0.30, 0.02, 0.02, 0.08, 0.02, 0.14,
    ], // Health
    [
        0.30, 0.10, 0.10, 0.05, 0.05, 0.03, 0.10, 0.05, 0.03, 0.04, 0.15,
    ], // Office
    [
        0.10, 0.12, 0.05, 0.15, 0.08, 0.03, 0.10, 0.05, 0.15, 0.05, 0.12,
    ], // Professional
    [
        0.25, 0.12, 0.12, 0.08, 0.05, 0.03, 0.08, 0.05, 0.05, 0.02, 0.15,
    ], // Manager
    [
        0.08, 0.05, 0.03, 0.25, 0.03, 0.02, 0.15, 0.03, 0.18, 0.08, 0.10,
    ], // Technical
    [
        0.12, 0.18, 0.15, 0.03, 0.08, 0.08, 0.03, 0.10, 0.05, 0.02, 0.16,
    ], // Service
];

// Institutions: 59 total (43 Table A + 6 dual-sector + 10 other)
const N_INST: usize = 59;
const INST_STATE: [i32; N_INST] = [
    // 43 Table A, STE 2021 order:
    // NSW(12), VIC(10), QLD(8), SA(3), WA(5), TAS(1), NT(1), ACT(3)
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4,
    4, 5, 5, 5, 5, 5, 6, 7, 8, 8, 8, // 6 dual-sector
    2, 2, 2, 7, 3, 2, // 10 other
    1, 1, 2, 2, 3, 4, 5, 1, 3, 2,
];
const INST_TYPE: [i32; N_INST] = [
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
];

const FT_SHARE: f64 = 0.65;
const ATTEND_MODE_WEIGHTS: [f64; 3] = [0.55, 0.25, 0.20];

// Financial
const CSP_SHARE: f64 = 0.85;
const UPFRONT_SHARE: f64 = 0.10;
// The fee schedule, in 2021 dollars. Student contribution bands, HELP loan
// limits and the fees an institution may set are legislated and restated by the
// CPI each calendar year, so these are an anchor-year schedule that
// `fee_schedule_index` moves to the year a unit is actually charged in.
const ANNUAL_HELP: [f64; 6] = [8500.0, 8500.0, 10000.0, 12000.0, 0.0, 0.0];
const CSP_ANNUAL: f64 = 7500.0;
const FULL_FEE_ANNUAL: f64 = 22000.0;
const COURSE_TYPES: [i32; 6] = [1, 2, 3, 4, 5, 6];
// STE 2021 order: 1 NSW, 2 VIC, 3 QLD, 4 SA, 5 WA, 6 TAS, 7 NT, 8 ACT
const STATE_TO_PC: [&str; 8] = [
    "2000", "3000", "4000", "5000", "6000", "7000", "0800", "2600",
];

// Entry age offset probabilities
const ENTRY_OFFSETS: [i32; 3] = [17, 18, 19];
const ENTRY_OFFSET_PROBS: [f64; 3] = [0.15, 0.50, 0.35];

/// The fee schedule in force in a calendar year, relative to the 2021 anchor.
///
/// A charge is administered: every student in a band is billed the same number,
/// so there is no per-student departure from the headline. `year` is the
/// calendar year of the unit's census date, which is the year whose schedule
/// the charge is set under. It is not the year the student commenced: a
/// four-year degree would then carry its first-year schedule all the way to
/// completion and lose four years of indexation on the way.
#[inline]
fn fee_schedule_index(year: i32) -> f64 {
    nominal::administered_factor(nominal::Series::Price, nominal::Basis::Calendar, year)
}

// ==========================================================================
// Participant selection
// ==========================================================================

/// Select HE participants from spine and build enrolment spells.
///
/// Returns a named list of vectors containing spell-level data.
/// @export
#[extendr]
fn select_he_participants__(
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    education: &[i32],
    archetype: &[i32],
    country_of_birth: &[i32],
    indigenous: &[i32],
    aeuid: Strings,
    seed: i64,
    min_year: i32,
    max_year: i32,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64((seed + 800) as u64);

    // Step 1: Who participates in HE
    let mut he_idx: Vec<usize> = Vec::new();
    for i in 0..n {
        let edu = education[i].max(0).min(5) as usize;
        let p = PARTICIPATION_RATE[edu];
        let u: f64 = rng.gen();
        if education[i] == 5 || u < p {
            he_idx.push(i);
        }
    }

    if he_idx.is_empty() {
        return empty_spell_list();
    }

    let n_he = he_idx.len();

    // Step 2: Assign qualification (1-based in output, 0-based internally)
    let mut qual_idx = vec![0usize; n_he];
    for (j, &pi) in he_idx.iter().enumerate() {
        let shares = if education[pi] == 5 {
            &QUAL_SHARES_EDU5
        } else {
            &QUAL_SHARES_ATTEMPT
        };
        qual_idx[j] = weighted_sample(&mut rng, shares);
    }

    // Step 3: Commencement year
    let mut commence_year = vec![0i32; n_he];
    for (j, &pi) in he_idx.iter().enumerate() {
        // Standard entry: birth_year + 17/18/19
        let offset_idx = weighted_sample(&mut rng, &ENTRY_OFFSET_PROBS);
        let mut offset = ENTRY_OFFSETS[offset_idx];

        // Mature-age override (~30%)
        if rng.gen::<f64>() < 0.30 {
            let mature = normal_sample(&mut rng, 28.0, 6.0).round() as i32;
            offset = mature.max(22).min(50);
        }

        commence_year[j] = birth_year[pi] + offset;
    }

    // Step 4: Duration and completion
    let mut actual_duration = vec![0i32; n_he];
    let mut completed = vec![false; n_he];
    let mut is_ft = vec![false; n_he];
    let mut completion_year = vec![0i32; n_he];

    for j in 0..n_he {
        let qi = qual_idx[j];
        let ft_dur = FT_DURATION[qi];
        let ft = rng.gen::<f64>() < FT_SHARE;
        is_ft[j] = ft;

        let duration = if ft {
            ft_dur
        } else {
            (ft_dur as f64 * 1.8).ceil() as i32
        };

        let mut comp_rate = COMPLETION_RATE[qi];
        if education[he_idx[j]] != 5 {
            comp_rate *= 0.50;
        }

        let comp = rng.gen::<f64>() < comp_rate;
        completed[j] = comp;

        let withdrawn_frac = rng.gen_range(0.1..0.8);
        let withdrawn_dur = ((duration as f64 * withdrawn_frac).ceil() as i32).max(1);

        actual_duration[j] = if comp { duration } else { withdrawn_dur };
        completion_year[j] = commence_year[j] + actual_duration[j];
    }

    // Step 5: Filter to observation window
    let mut keep: Vec<usize> = Vec::new();
    for j in 0..n_he {
        let in_window =
            commence_year[j] <= max_year && (commence_year[j] + actual_duration[j] - 1) >= min_year;
        if in_window {
            keep.push(j);
        }
    }

    if keep.is_empty() {
        return empty_spell_list();
    }

    let n_keep = keep.len();

    // Step 6: Assign FOE (archetype-dependent)
    let mut foe = vec![String::new(); n_keep];
    for (k, &j) in keep.iter().enumerate() {
        let pi = he_idx[j];
        let arch = archetype[pi].max(0).min(7) as usize;
        let foe_idx = weighted_sample(&mut rng, &ARCHETYPE_FOE[arch]);
        foe[k] = FOE_CODES[foe_idx].to_string();
    }
    // Fallback for any empty FOE (shouldn't happen)
    for k in 0..n_keep {
        if foe[k].is_empty() {
            let foe_idx = weighted_sample(&mut rng, &FOE_SHARES);
            foe[k] = FOE_CODES[foe_idx].to_string();
        }
    }

    // Step 7: Assign institution (state-affinity)
    let mut inst_idx = vec![0usize; n_keep];
    for (k, &j) in keep.iter().enumerate() {
        let pi = he_idx[j];
        let person_state = state[pi];
        let same_state = rng.gen::<f64>() < 0.80;

        if same_state {
            // Find institutions in this state
            let candidates: Vec<usize> = (0..N_INST)
                .filter(|&i| INST_STATE[i] == person_state)
                .collect();
            if candidates.is_empty() {
                inst_idx[k] = rng.gen_range(0..N_INST);
            } else {
                inst_idx[k] = candidates[rng.gen_range(0..candidates.len())];
            }
        } else {
            inst_idx[k] = rng.gen_range(0..N_INST);
        }
    }

    // Step 8: Attendance mode
    let mut attend_mode = vec![0i32; n_keep];
    for k in 0..n_keep {
        attend_mode[k] = weighted_sample(&mut rng, &ATTEND_MODE_WEIGHTS) as i32 + 1;
    }

    // Step 9: Build output vectors
    let mut out_aeuid: Vec<String> = Vec::with_capacity(n_keep);
    let mut out_birth_year: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_sex: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_state: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_education: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_cob: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_indigenous: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_archetype: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_qual_idx: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_commence: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_duration: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_completed: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_comp_year: Vec<Rint> = Vec::with_capacity(n_keep);
    let mut out_is_ft: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_foe: Vec<String> = Vec::with_capacity(n_keep);
    let mut out_inst_code: Vec<String> = Vec::with_capacity(n_keep);
    let mut out_inst_type: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_inst_state: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_attend_mode: Vec<i32> = Vec::with_capacity(n_keep);
    let mut out_course_code: Vec<String> = Vec::with_capacity(n_keep);

    for (k, &j) in keep.iter().enumerate() {
        let pi = he_idx[j];
        out_aeuid.push(aeuid[pi].to_string());
        out_birth_year.push(birth_year[pi]);
        out_sex.push(sex[pi]);
        out_state.push(state[pi]);
        out_education.push(education[pi]);
        out_cob.push(country_of_birth[pi]);
        out_indigenous.push(indigenous[pi]);
        out_archetype.push(archetype[pi]);
        out_qual_idx.push(qual_idx[j] as i32 + 1); // 1-based for R
        out_commence.push(commence_year[j]);
        out_duration.push(actual_duration[j]);
        out_completed.push(if completed[j] { 1 } else { 0 });
        out_comp_year.push(if completed[j] {
            Rint::from(completion_year[j])
        } else {
            Rint::na()
        });
        out_is_ft.push(if is_ft[j] { 1 } else { 0 });
        out_foe.push(foe[k].clone());
        out_inst_code.push(format!("{:04}", inst_idx[k] + 1));
        out_inst_type.push(INST_TYPE[inst_idx[k]]);
        out_inst_state.push(INST_STATE[inst_idx[k]]);
        out_attend_mode.push(attend_mode[k]);
        out_course_code.push(format!("C{:04}{:05}", inst_idx[k] + 1, k + 1));
    }

    list!(
        aeuid = out_aeuid,
        birth_year = out_birth_year,
        sex = out_sex,
        state = out_state,
        education = out_education,
        country_of_birth = out_cob,
        indigenous = out_indigenous,
        archetype = out_archetype,
        qual_idx = out_qual_idx,
        commence_year = out_commence,
        actual_duration = out_duration,
        completed = out_completed,
        completion_year = out_comp_year,
        is_ft = out_is_ft,
        foe = out_foe,
        inst_code = out_inst_code,
        inst_type = out_inst_type,
        inst_state = out_inst_state,
        attend_mode = out_attend_mode,
        course_code = out_course_code
    )
}

// ==========================================================================
// Load projection (unit-of-study level) — the performance-critical function
// ==========================================================================

/// Project HE load records (unit-of-study level) from spell data.
///
/// This function replaces the R project_he_load() with an O(n) algorithm
/// (the R version had O(n²) for-loops computing cumulative unit positions).
///
/// @export
#[extendr]
fn project_he_load__(
    spell_aeuid: Strings,
    spell_commence_year: &[i32],
    spell_actual_duration: &[i32],
    spell_completed: &[i32],
    spell_is_ft: &[i32],
    spell_qual_idx: &[i32],
    spell_foe: Strings,
    spell_inst_code: Strings,
    spell_inst_state: &[i32],
    spell_country_of_birth: &[i32],
    spell_attend_mode: &[i32],
    spell_course_code: Strings,
    min_year: i32,
    max_year: i32,
    seed: i64,
) -> List {
    let n_spells = spell_commence_year.len();
    if n_spells == 0 {
        return empty_load_list();
    }

    let mut rng = StdRng::seed_from_u64((seed + 803) as u64);

    // Step 1: Compute active year ranges and filter
    let mut active_spells: Vec<usize> = Vec::new();
    let mut yr_starts: Vec<i32> = Vec::new();
    let mut yr_ends: Vec<i32> = Vec::new();
    let mut n_years_vec: Vec<i32> = Vec::new();

    for i in 0..n_spells {
        let yr_start = spell_commence_year[i].max(min_year);
        let yr_end = (spell_commence_year[i] + spell_actual_duration[i] - 1).min(max_year);
        let n_years = (yr_end - yr_start + 1).max(0);
        if n_years > 0 {
            active_spells.push(i);
            yr_starts.push(yr_start);
            yr_ends.push(yr_end);
            n_years_vec.push(n_years);
        }
    }

    let n_active = active_spells.len();
    if n_active == 0 {
        return empty_load_list();
    }

    // Step 2: Pre-compute financial fields (one draw per active spell)
    let mut is_csp = vec![false; n_active];
    let mut upfront_draw = vec![0.0f64; n_active];
    for a in 0..n_active {
        is_csp[a] = rng.gen::<f64>() < CSP_SHARE;
        upfront_draw[a] = rng.gen::<f64>();
    }

    let mut units_per_yr = vec![0i32; n_active];
    let mut student_status = vec![0i32; n_active];
    let mut help_per_unit = vec![0.0f64; n_active];
    let mut loan_fee_per_unit = vec![0.0f64; n_active];
    let mut unit_charge = vec![0.0f64; n_active];
    let mut upfront_per_unit = vec![0.0f64; n_active];
    let mut campus_pc: Vec<&str> = vec!["2000"; n_active];
    let mut cit_res = vec![0i32; n_active];

    for a in 0..n_active {
        let si = active_spells[a];
        let ft = spell_is_ft[si] != 0;
        let qi = (spell_qual_idx[si] - 1).max(0).min(5) as usize; // 0-based

        units_per_yr[a] = if ft { 8 } else { 4 };
        student_status[a] = if is_csp[a] { 10 } else { 11 };

        let annual_h = ANNUAL_HELP[qi];
        help_per_unit[a] = annual_h / units_per_yr[a] as f64;
        loan_fee_per_unit[a] = if is_csp[a] {
            0.0
        } else {
            help_per_unit[a] * 0.20
        };
        unit_charge[a] = if is_csp[a] {
            CSP_ANNUAL / units_per_yr[a] as f64
        } else {
            FULL_FEE_ANNUAL / units_per_yr[a] as f64
        };

        let pays_upfront = !is_csp[a] && upfront_draw[a] < UPFRONT_SHARE;
        upfront_per_unit[a] = if pays_upfront { unit_charge[a] } else { 0.0 };

        let ist = (spell_inst_state[si] - 1).max(0).min(7) as usize;
        campus_pc[a] = STATE_TO_PC[ist];

        cit_res[a] = if spell_country_of_birth[si] == 0 {
            1
        } else {
            2
        };
    }

    // Step 3: Count total spell-years and total units
    let total_sy: usize = n_years_vec.iter().map(|&n| n as usize).sum();
    let mut total_units: usize = 0;
    // Build spell-year to active-spell mapping
    let mut sy_active_idx: Vec<usize> = Vec::with_capacity(total_sy);
    let mut sy_year: Vec<i32> = Vec::with_capacity(total_sy);
    let mut sy_units: Vec<i32> = Vec::with_capacity(total_sy);

    for a in 0..n_active {
        let upy = units_per_yr[a];
        for yr_offset in 0..n_years_vec[a] {
            sy_active_idx.push(a);
            sy_year.push(yr_starts[a] + yr_offset);
            sy_units.push(upy);
            total_units += upy as usize;
        }
    }

    if total_units == 0 {
        return empty_load_list();
    }

    // Step 4: Expand to unit level — O(n) sequential
    let mut out_aeuid: Vec<String> = Vec::with_capacity(total_units);
    let mut out_year: Vec<i32> = Vec::with_capacity(total_units);
    let mut out_course: Vec<String> = Vec::with_capacity(total_units);
    let mut out_institution: Vec<String> = Vec::with_capacity(total_units);
    let mut out_unit_study: Vec<String> = Vec::with_capacity(total_units);
    let mut out_discipline: Vec<String> = Vec::with_capacity(total_units);
    let mut out_eftsl: Vec<f64> = Vec::with_capacity(total_units);
    let mut out_student_status: Vec<i32> = Vec::with_capacity(total_units);
    let mut out_unit_status: Vec<i32> = Vec::with_capacity(total_units);
    let mut out_mode_attend: Vec<i32> = Vec::with_capacity(total_units);
    let mut out_help_debt: Vec<f64> = Vec::with_capacity(total_units);
    let mut out_loan_fee: Vec<f64> = Vec::with_capacity(total_units);
    let mut out_total_charged: Vec<f64> = Vec::with_capacity(total_units);
    let mut out_amount_upfront: Vec<f64> = Vec::with_capacity(total_units);
    let mut out_campus_state: Vec<i32> = Vec::with_capacity(total_units);
    let mut out_campus_pc: Vec<String> = Vec::with_capacity(total_units);
    let mut out_cit_res: Vec<i32> = Vec::with_capacity(total_units);
    let mut out_census: Vec<String> = Vec::with_capacity(total_units);
    let mut out_summer: Vec<i32> = Vec::with_capacity(total_units);
    let mut out_industry: Vec<i32> = Vec::with_capacity(total_units);

    // Track cumulative unit counter per active spell (O(n) approach)
    let mut spell_unit_counter = vec![0i32; n_active];

    // For withdrawn final-year detection, pre-draw cutoffs
    // We need one draw per spell-year that is a withdrawn final year
    // For deterministic RNG ordering: draw for all spell-years, use only for finals
    let mut withdrawn_cutoffs: Vec<i32> = Vec::with_capacity(total_sy);
    for sy_i in 0..total_sy {
        let a = sy_active_idx[sy_i];
        let si = active_spells[a];
        let year = sy_year[sy_i];
        let is_final = year == yr_ends[a] && spell_completed[si] == 0;
        if is_final {
            let upy = units_per_yr[a];
            let cut = ((rng.gen::<f64>() * upy as f64).ceil() as i32 - 1).max(1);
            withdrawn_cutoffs.push(cut);
        } else {
            // Consume RNG for stability
            let _ = rng.gen::<f64>();
            withdrawn_cutoffs.push(units_per_yr[a]);
        }
    }

    for sy_i in 0..total_sy {
        let a = sy_active_idx[sy_i];
        let si = active_spells[a];
        let year = sy_year[sy_i];
        let upy = units_per_yr[a];
        let is_final = year == yr_ends[a] && spell_completed[si] == 0;
        let cutoff = withdrawn_cutoffs[sy_i];
        let base_counter = spell_unit_counter[a];

        let aeuid_str = spell_aeuid[si].to_string();
        let course_str = spell_course_code[si].to_string();
        let inst_str = spell_inst_code[si].to_string();
        let foe_str = spell_foe[si].to_string();
        let sem1_count = (upy + 1) / 2; // ceiling division

        // The per-spell money above is the anchor-year schedule, and the
        // schedule that applies is the one in force when the unit is charged.
        // A spell can run across five years of schedules, so the factor belongs
        // here at the spell-year, not up with the once-per-spell fields.
        let fee_index = fee_schedule_index(year);
        let help_debt = help_per_unit[a] * fee_index;
        let charged = unit_charge[a] * fee_index;
        // The loan fee is a fixed percentage of the HELP amount and the upfront
        // payment is the whole charge or nothing, so both take their parent's
        // factor rather than one of their own: a second factor would break the
        // percentage and leave a full upfront payment short of the charge.
        let loan_fee = loan_fee_per_unit[a] * fee_index;
        let upfront = upfront_per_unit[a] * fee_index;

        for u in 0..upy {
            let unit_within = u + 1; // 1-based
            let global_idx = base_counter + unit_within;

            // Unit status: 4 = active, 7 = withdrawn
            let status = if is_final && unit_within > cutoff {
                7
            } else {
                4
            };

            // Census date
            let is_sem1 = unit_within <= sem1_count;
            let census = if is_sem1 {
                format!("{}-03-31", year)
            } else {
                format!("{}-08-31", year)
            };

            // Unit code
            let unit_code = format!("U{:04}{:04}{:02}", inst_str, global_idx, year % 100);

            out_aeuid.push(aeuid_str.clone());
            out_year.push(year);
            out_course.push(course_str.clone());
            out_institution.push(inst_str.clone());
            out_unit_study.push(unit_code);
            out_discipline.push(foe_str.clone());
            out_eftsl.push(0.125);
            out_student_status.push(student_status[a]);
            out_unit_status.push(status);
            out_mode_attend.push(spell_attend_mode[si]);
            out_help_debt.push(help_debt);
            out_loan_fee.push(loan_fee);
            out_total_charged.push(charged);
            out_amount_upfront.push(upfront);
            out_campus_state.push(spell_inst_state[si]);
            out_campus_pc.push(campus_pc[a].to_string());
            out_cit_res.push(cit_res[a]);
            out_census.push(census);
            out_summer.push(0);
            out_industry.push(0);
        }

        spell_unit_counter[a] = base_counter + upy;
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR = out_year,
        COURSE = out_course,
        INSTITUTION = out_institution,
        UNIT_STUDY = out_unit_study,
        DISCIPLINE_CODE = out_discipline,
        EQUIVALENT_FT_STUDENT_LOAD = out_eftsl,
        STUDENT_STATUS = out_student_status,
        UNIT_STATUS = out_unit_status,
        MODE_ATTENDANCE = out_mode_attend,
        HELP_DEBT = out_help_debt,
        LOAN_FEE = out_loan_fee,
        TOTAL_AMOUNT_CHARGED = out_total_charged,
        AMOUNT_PAID_UPFRONT = out_amount_upfront,
        CAMPUS_STATE = out_campus_state,
        CAMPUS_POSTCODE = out_campus_pc,
        CITIZEN_RESIDENT = out_cit_res,
        UNIT_STUDY_CENSUS = out_census,
        SUMMER_SCHOOL_INDICATOR = out_summer,
        INDUSTRY = out_industry
    )
}

fn he_disability_code(disability_type: i32, support_indicator: i32) -> String {
    if disability_type == i32::MIN {
        return "20000000".to_string();
    }

    // E386 positions are: any disability, hearing, learning, mobility,
    // vision, medical, other, and whether support advice is requested.
    let category_position = match disability_type {
        2 => 1,
        7 | 15 => 2,
        4 | 5 | 8 | 9 | 10 | 12 => 3,
        1 => 4,
        6 | 14 => 5,
        _ => 6,
    };
    let mut characters = [b'0'; 8];
    characters[0] = b'1';
    characters[category_position] = b'1';
    characters[7] = if support_indicator == 1 { b'1' } else { b'2' };
    String::from_utf8(characters.to_vec()).expect("E386 code is ASCII")
}

/// Project HE enrolment records (student-year level).
/// @export
#[extendr]
fn project_he_enrol__(
    spell_aeuid: Strings,
    spell_commence_year: &[i32],
    spell_actual_duration: &[i32],
    spell_is_ft: &[i32],
    spell_course_code: Strings,
    spell_inst_code: Strings,
    spell_attend_mode: &[i32],
    spell_sex: &[i32],
    spell_country_of_birth: &[i32],
    spell_indigenous: &[i32],
    spell_disability_type: &[i32],
    spell_disability_support: &[i32],
    spell_education: &[i32],
    spell_birth_year: &[i32],
    min_year: i32,
    max_year: i32,
) -> List {
    let n_spells = spell_commence_year.len();
    if n_spells == 0 {
        return empty_enrol_list();
    }
    assert_eq!(spell_country_of_birth.len(), n_spells);
    assert_eq!(spell_indigenous.len(), n_spells);
    assert_eq!(spell_disability_type.len(), n_spells);
    assert_eq!(spell_disability_support.len(), n_spells);

    let mut total_rows = 0usize;
    for i in 0..n_spells {
        let yr_start = spell_commence_year[i].max(min_year);
        let yr_end = (spell_commence_year[i] + spell_actual_duration[i] - 1).min(max_year);
        let n_years = (yr_end - yr_start + 1).max(0);
        total_rows += n_years as usize;
    }
    if total_rows == 0 {
        return empty_enrol_list();
    }

    let mut out_aeuid: Vec<String> = Vec::with_capacity(total_rows);
    let mut out_year: Vec<i32> = Vec::with_capacity(total_rows);
    let mut out_course: Vec<String> = Vec::with_capacity(total_rows);
    let mut out_institution: Vec<String> = Vec::with_capacity(total_rows);
    let mut out_attendance_mode: Vec<i32> = Vec::with_capacity(total_rows);
    let mut out_attendance_type: Vec<i32> = Vec::with_capacity(total_rows);
    let mut out_com_indicator: Vec<i32> = Vec::with_capacity(total_rows);
    let mut out_gender: Vec<i32> = Vec::with_capacity(total_rows);
    let mut out_country_birth: Vec<String> = Vec::with_capacity(total_rows);
    let mut out_aborig_torres: Vec<i32> = Vec::with_capacity(total_rows);
    let mut out_disability: Vec<String> = Vec::with_capacity(total_rows);
    let mut out_highest_participation: Vec<i32> = Vec::with_capacity(total_rows);
    let mut out_year_left_school: Vec<i32> = Vec::with_capacity(total_rows);
    let mut out_reporting_year_period: Vec<String> = Vec::with_capacity(total_rows);
    let mut out_major_course: Vec<i32> = Vec::with_capacity(total_rows);

    for i in 0..n_spells {
        let yr_start = spell_commence_year[i].max(min_year);
        let yr_end = (spell_commence_year[i] + spell_actual_duration[i] - 1).min(max_year);
        if yr_end < yr_start {
            continue;
        }

        let aeuid = spell_aeuid[i].to_string();
        let course = spell_course_code[i].to_string();
        let inst = spell_inst_code[i].to_string();
        let attendance_type = if spell_is_ft[i] != 0 { 1 } else { 2 };
        let country_birth = if spell_country_of_birth[i] == i32::MIN {
            "9999".to_string()
        } else {
            format!("{:04}", spell_country_of_birth[i])
        };
        let aborig_torres = match spell_indigenous[i] {
            1 => 2,
            2 => 3,
            3 => 4,
            4 => 5,
            _ => 9,
        };
        let disability = he_disability_code(spell_disability_type[i], spell_disability_support[i]);
        let year_left_school = spell_birth_year[i] + 18;

        for yr in yr_start..=yr_end {
            out_aeuid.push(aeuid.clone());
            out_year.push(yr);
            out_course.push(course.clone());
            out_institution.push(inst.clone());
            out_attendance_mode.push(spell_attend_mode[i]);
            out_attendance_type.push(attendance_type);
            out_com_indicator.push(if yr == spell_commence_year[i] { 1 } else { 0 });
            out_gender.push(spell_sex[i]);
            out_country_birth.push(country_birth.clone());
            out_aborig_torres.push(aborig_torres);
            out_disability.push(disability.clone());
            out_highest_participation.push(spell_education[i]);
            out_year_left_school.push(year_left_school);
            out_reporting_year_period.push(format!("{}-1", yr));
            out_major_course.push(1);
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR = out_year,
        COURSE = out_course,
        INSTITUTION = out_institution,
        ATTENDANCE_MODE = out_attendance_mode,
        ATTENDANCE_TYPE = out_attendance_type,
        COM_INDICATOR = out_com_indicator,
        GENDER = out_gender,
        COUNTRY_BIRTH = out_country_birth,
        ABORIG_TORRES = out_aborig_torres,
        DISABILITY = out_disability,
        HIGHEST_PARTICIPATION = out_highest_participation,
        YEAR_LEFT_SCHOOL = out_year_left_school,
        REPORTING_YEAR_PERIOD = out_reporting_year_period,
        MAJOR_COURSE = out_major_course,
    )
}

/// Project HE course records (one row per spell).
/// @export
#[extendr]
fn project_he_course__(
    spell_aeuid: Strings,
    spell_commence_year: &[i32],
    spell_course_code: Strings,
    spell_qual_idx: &[i32],
    spell_foe: Strings,
    spell_inst_code: Strings,
    spell_is_ft: &[i32],
    spell_actual_duration: &[i32],
) -> List {
    let n = spell_commence_year.len();
    if n == 0 {
        return empty_course_list();
    }

    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut out_course: Vec<String> = Vec::with_capacity(n);
    let mut out_course_of_study_code: Vec<String> = Vec::with_capacity(n);
    let mut out_course_type: Vec<i32> = Vec::with_capacity(n);
    let mut out_foe: Vec<String> = Vec::with_capacity(n);
    let mut out_foe_supp: Vec<String> = Vec::with_capacity(n);
    let mut out_institution: Vec<String> = Vec::with_capacity(n);
    let mut out_special_course: Vec<i32> = Vec::with_capacity(n);
    let mut out_course_load: Vec<f64> = Vec::with_capacity(n);

    for i in 0..n {
        let qi = (spell_qual_idx[i] - 1).clamp(0, 5) as usize;
        let foe = spell_foe[i].to_string();
        out_aeuid.push(spell_aeuid[i].to_string());
        out_course.push(spell_course_code[i].to_string());
        out_course_of_study_code.push(format!("NCS{:06}", i + 1));
        out_course_type.push(COURSE_TYPES[qi]);
        out_foe.push(foe.clone());
        out_foe_supp.push(format!("{}00", foe));
        out_institution.push(spell_inst_code[i].to_string());
        out_special_course.push(0);
        out_course_load
            .push((if spell_is_ft[i] != 0 { 1.0 } else { 0.5 }) * spell_actual_duration[i] as f64);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR = spell_commence_year.to_vec(),
        COURSE = out_course,
        COURSE_OF_STUDY_CODE = out_course_of_study_code,
        COURSE_TYPE = out_course_type,
        FOE = out_foe,
        FOE_SUPP = out_foe_supp,
        INSTITUTION = out_institution,
        SPECIAL_COURSE = out_special_course,
        COURSE_LOAD = out_course_load,
    )
}

/// Project HE completion records.
/// @export
#[extendr]
fn project_he_completions__(
    spell_aeuid: Strings,
    spell_completed: &[i32],
    spell_completion_year: &[i32],
    spell_course_code: Strings,
    spell_qual_idx: &[i32],
    spell_foe: Strings,
    spell_inst_code: Strings,
    max_year: i32,
) -> List {
    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_year: Vec<i32> = Vec::new();
    let mut out_course_code: Vec<String> = Vec::new();
    let mut out_course_type: Vec<i32> = Vec::new();
    let mut out_foe: Vec<String> = Vec::new();
    let mut out_foe_supp: Vec<String> = Vec::new();
    let mut out_institution: Vec<String> = Vec::new();

    for i in 0..spell_completed.len() {
        if spell_completed[i] == 0
            || spell_completion_year[i] == i32::MIN
            || spell_completion_year[i] > max_year
        {
            continue;
        }
        let qi = (spell_qual_idx[i] - 1).clamp(0, 5) as usize;
        let foe = spell_foe[i].to_string();
        out_aeuid.push(spell_aeuid[i].to_string());
        out_year.push(spell_completion_year[i]);
        out_course_code.push(spell_course_code[i].to_string());
        out_course_type.push(COURSE_TYPES[qi]);
        out_foe.push(foe.clone());
        out_foe_supp.push(format!("{}00", foe));
        out_institution.push(spell_inst_code[i].to_string());
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR = out_year,
        COURSE_CODE = out_course_code,
        COURSE_TYPE = out_course_type,
        FOE = out_foe,
        FOE_SUPP = out_foe_supp,
        INSTITUTION = out_institution,
    )
}

/// Aggregate HE HELP records from load rows.
/// @export
#[extendr]
fn project_he_help__(
    load_aeuid: Strings,
    load_year: &[i32],
    load_institution: Strings,
    load_help_debt: &[f64],
    load_student_status: &[i32],
    load_loan_fee: &[f64],
) -> List {
    let mut seen: HashMap<String, usize> = HashMap::new();
    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_year: Vec<i32> = Vec::new();
    let mut out_institution: Vec<String> = Vec::new();
    let mut out_help_debt: Vec<f64> = Vec::new();
    let mut out_student_status: Vec<i32> = Vec::new();
    let mut out_loan_fee: Vec<f64> = Vec::new();

    for i in 0..load_year.len() {
        if load_help_debt[i] <= 0.0 {
            continue;
        }

        let aeuid = load_aeuid[i].to_string();
        let inst = load_institution[i].to_string();
        let key = format!("{}|{}|{}", aeuid, load_year[i], inst);

        if let Some(&out_idx) = seen.get(&key) {
            out_help_debt[out_idx] += load_help_debt[i];
            out_loan_fee[out_idx] += load_loan_fee[i];
        } else {
            let out_idx = out_aeuid.len();
            seen.insert(key, out_idx);
            out_aeuid.push(aeuid);
            out_year.push(load_year[i]);
            out_institution.push(inst);
            out_help_debt.push(load_help_debt[i]);
            out_student_status.push(load_student_status[i]);
            out_loan_fee.push(load_loan_fee[i]);
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR = out_year,
        INSTITUTION = out_institution,
        HELP_DEBT = out_help_debt,
        STUDENT_STATUS = out_student_status,
        LOAN_FEE = out_loan_fee,
    )
}

// ==========================================================================
// Empty list helpers
// ==========================================================================

fn empty_spell_list() -> List {
    let es: Vec<String> = Vec::new();
    let ei: Vec<i32> = Vec::new();
    let er: Vec<Rint> = Vec::new();

    list!(
        aeuid = es.clone(),
        birth_year = ei.clone(),
        sex = ei.clone(),
        state = ei.clone(),
        education = ei.clone(),
        country_of_birth = ei.clone(),
        indigenous = ei.clone(),
        archetype = ei.clone(),
        qual_idx = ei.clone(),
        commence_year = ei.clone(),
        actual_duration = ei.clone(),
        completed = ei.clone(),
        completion_year = er,
        is_ft = ei.clone(),
        foe = es.clone(),
        inst_code = es.clone(),
        inst_type = ei.clone(),
        inst_state = ei.clone(),
        attend_mode = ei.clone(),
        course_code = es
    )
}

fn empty_load_list() -> List {
    let es: Vec<String> = Vec::new();
    let ei: Vec<i32> = Vec::new();
    let ef: Vec<f64> = Vec::new();

    list!(
        SYNTHETIC_AEUID = es.clone(),
        YEAR = ei.clone(),
        COURSE = es.clone(),
        INSTITUTION = es.clone(),
        UNIT_STUDY = es.clone(),
        DISCIPLINE_CODE = es.clone(),
        EQUIVALENT_FT_STUDENT_LOAD = ef.clone(),
        STUDENT_STATUS = ei.clone(),
        UNIT_STATUS = ei.clone(),
        MODE_ATTENDANCE = ei.clone(),
        HELP_DEBT = ef.clone(),
        LOAN_FEE = ef.clone(),
        TOTAL_AMOUNT_CHARGED = ef.clone(),
        AMOUNT_PAID_UPFRONT = ef.clone(),
        CAMPUS_STATE = ei.clone(),
        CAMPUS_POSTCODE = es.clone(),
        CITIZEN_RESIDENT = ei.clone(),
        UNIT_STUDY_CENSUS = es.clone(),
        SUMMER_SCHOOL_INDICATOR = ei.clone(),
        INDUSTRY = ei
    )
}

fn empty_enrol_list() -> List {
    let es: Vec<String> = Vec::new();
    let ei: Vec<i32> = Vec::new();
    list!(
        SYNTHETIC_AEUID = es.clone(),
        YEAR = ei.clone(),
        COURSE = es.clone(),
        INSTITUTION = es.clone(),
        ATTENDANCE_MODE = ei.clone(),
        ATTENDANCE_TYPE = ei.clone(),
        COM_INDICATOR = ei.clone(),
        GENDER = ei.clone(),
        COUNTRY_BIRTH = es.clone(),
        ABORIG_TORRES = ei.clone(),
        DISABILITY = es.clone(),
        HIGHEST_PARTICIPATION = ei.clone(),
        YEAR_LEFT_SCHOOL = ei.clone(),
        REPORTING_YEAR_PERIOD = es.clone(),
        MAJOR_COURSE = ei,
    )
}

fn empty_course_list() -> List {
    let es: Vec<String> = Vec::new();
    let ei: Vec<i32> = Vec::new();
    let ef: Vec<f64> = Vec::new();
    list!(
        SYNTHETIC_AEUID = es.clone(),
        YEAR = ei.clone(),
        COURSE = es.clone(),
        COURSE_OF_STUDY_CODE = es.clone(),
        COURSE_TYPE = ei.clone(),
        FOE = es.clone(),
        FOE_SUPP = es.clone(),
        INSTITUTION = es.clone(),
        SPECIAL_COURSE = ei.clone(),
        COURSE_LOAD = ef,
    )
}

#[cfg(test)]
mod tests {
    use super::he_disability_code;

    #[test]
    fn legacy_disability_codes_preserve_status_and_type() {
        assert_eq!(he_disability_code(i32::MIN, i32::MIN), "20000000");
        assert_eq!(he_disability_code(2, 1), "11000001");
        assert_eq!(he_disability_code(7, 0), "10100002");
        assert_eq!(he_disability_code(1, 0), "10001002");
        assert_eq!(he_disability_code(17, 1), "10000011");
    }
}

extendr_module! {
    mod he;
    fn select_he_participants__;
    fn project_he_load__;
    fn project_he_enrol__;
    fn project_he_course__;
    fn project_he_completions__;
    fn project_he_help__;
}

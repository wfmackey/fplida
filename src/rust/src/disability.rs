use crate::sampling::weighted_sample;
use crate::service_profiles::{self, PROFILES};
use extendr_api::prelude::*;
use rand::prelude::*;
use rand::rngs::StdRng;

// ============================================================================
// SDAC disability type → 6D dimension mapping (kept for backward compat)
// ============================================================================

/// Indices into the 6D task score array:
/// 0=cognitive, 1=physical, 2=vision, 3=hearing, 4=manual_dexterity, 5=communication
const SDAC_TYPE_DIMENSIONS: &[(i32, &[usize])] = &[
    (1, &[2]),                 // Loss of sight → Vision
    (2, &[3]),                 // Loss of hearing → Hearing
    (3, &[5]),                 // Speech difficulties → Communication
    (4, &[1]),                 // Breathing difficulties → Physical
    (5, &[1, 4]),              // Chronic pain → Physical + Manual Dexterity
    (6, &[1, 0]),              // Blackouts/seizures → Physical + Cognitive
    (7, &[0]),                 // Difficulty learning → Cognitive
    (8, &[4, 1]),              // Incomplete arms/fingers → Manual Dexterity + Physical
    (9, &[4]),                 // Difficulty gripping → Manual Dexterity
    (10, &[1]),                // Incomplete feet/legs → Physical
    (11, &[5, 0]),             // Nervous/emotional → Communication + Cognitive
    (12, &[1]),                // Physical activity restriction → Physical
    (14, &[0, 5]),             // Mental illness → Cognitive + Communication
    (15, &[0]),                // Memory problems → Cognitive
    (16, &[5]),                // Social/behavioural → Communication
    (17, &[0, 1, 2, 3, 4, 5]), // Head injury/ABI → All
];

/// Severity distribution for broader disability/CHC population.
/// 1=profound/severe, 2=moderate, 3=mild, 4=condition_only
/// At 18% prevalence: ~1.8% profound, ~3.2% moderate, ~5.8% mild, ~7.2% condition.
/// ALL severity levels now receive employment/earnings effects (scaled by severity_multiplier).
pub const SEVERITY_WEIGHTS: &[f64] = &[0.10, 0.18, 0.32, 0.40];

// ============================================================================
// Severity multiplier — centered so weighted mean = 1.0
// ============================================================================

/// Severity multiplier for treatment effects.
/// Weighted mean across SEVERITY_WEIGHTS = 1.0, ensuring the average ATT
/// across all disabled workers matches the base theta curves.
///
/// Derivation: weights [0.10, 0.18, 0.32, 0.40] × mults [1.80, 1.40, 1.00, 0.62]
/// = 0.180 + 0.252 + 0.320 + 0.248 = 1.000
pub fn severity_multiplier(severity: i32) -> f64 {
    match severity {
        1 => 1.80, // profound/severe: 80% above average
        2 => 1.40, // moderate: 40% above average
        3 => 1.00, // mild: average
        4 => 0.62, // condition only: 38% below average
        _ => 1.0,
    }
}

// ============================================================================
// Treatment effect curves — calibrated to match spec targets (Section 2)
//
// These are the BASE treatment effects. The observed ATT in the analysis
// pipeline should approximately match these (averaged across severity and dose).
// ============================================================================

/// DC vs ND log-earnings treatment effect at event time k.
/// Spec targets: k=0 → -0.10, trough k=2 → -0.22, long-run k=10 → -0.12.
fn theta_dc_earnings(k: i32) -> f64 {
    if k < 0 {
        return 0.0;
    }
    match k {
        0 => -0.10,
        1 => -0.18,
        2 => -0.22,
        3 => -0.20,
        4 => -0.18,
        5 => -0.15,
        6 => -0.14,
        7 => -0.13,
        8 => -0.13,
        9 => -0.12,
        _ => -0.12,
    }
}

/// NC vs ND log-earnings treatment effect at event time k.
/// Spec targets: k=0 → -0.02, gradual erosion to k=10 → -0.09.
fn theta_nc_earnings(k: i32) -> f64 {
    if k < 0 {
        return 0.0;
    }
    match k {
        0 => -0.02,
        1 => -0.03,
        2 => -0.04,
        3 => -0.05,
        4 => -0.06,
        5 => -0.07,
        6 => -0.07,
        7 => -0.08,
        8 => -0.08,
        9 => -0.09,
        _ => -0.09,
    }
}

/// DC vs ND cumulative employment gap at event time k (Section 9.1).
/// At k=2: 15pp fewer DC workers employed than ND.
fn theta_dc_employment(k: i32) -> f64 {
    if k < 0 {
        return 0.0;
    }
    match k {
        0 => 0.06,
        1 => 0.12,
        2 => 0.15,
        3 => 0.14,
        4 => 0.13,
        5 => 0.12,
        6 => 0.11,
        7 => 0.11,
        8 => 0.10,
        9 => 0.10,
        _ => 0.10,
    }
}

/// NC vs ND cumulative employment gap at event time k.
fn theta_nc_employment(k: i32) -> f64 {
    if k < 0 {
        return 0.0;
    }
    match k {
        0 => 0.01,
        1 => 0.02,
        2 => 0.03,
        3 => 0.03,
        4 => 0.03,
        5 => 0.04,
        6 => 0.04,
        7 => 0.04,
        8 => 0.05,
        9 => 0.05,
        _ => 0.05,
    }
}

// ============================================================================
// Public API: earnings treatment effects
// ============================================================================

/// Log-earnings treatment effect for a disabled worker at event time k.
///
/// DC formula: base_DC(k) * (0.5 + s_std)
///   where s_std is the affected-dimension task score (dose).
///   DC workers have dose > dim_median (~0.35), so (0.5 + dose) ≈ 0.85--1.40.
///   This creates meaningful heterogeneity: higher task overlap → larger shock.
///
/// NC formula: base_NC(k)
///   No task-score or severity scaling — NC effects are uniform across workers.
///   NC path is mostly permanent (~80%) with slow erosion from non-task channels.
///
/// The DC path naturally embeds transitory (40%, 3yr half-life) + permanent (60%)
/// components via the shape of theta_dc_earnings(k): sharp drop then partial recovery.
pub fn earnings_effect(k: i32, is_dc: bool, _severity: i32, dose: f64) -> f64 {
    if k < 0 {
        return 0.0;
    }

    if is_dc {
        // DC: base_DC(k) * (0.5 + s_std)
        // dose is the affected-dimension task score (0-1), higher = more overlap
        theta_dc_earnings(k) * (0.5 + dose)
    } else {
        // NC: base_NC(k) — no scaling
        theta_nc_earnings(k)
    }
}

// ============================================================================
// Public API: employment exit/re-entry effects
// ============================================================================

/// Cumulative employment exit effect (for reference; used by downstream analysis).
pub fn employment_exit_effect(k: i32, is_dc: bool, severity: i32) -> f64 {
    if k < 0 {
        return 0.0;
    }

    let base = if is_dc {
        theta_dc_employment(k)
    } else {
        theta_nc_employment(k)
    };

    base * severity_multiplier(severity)
}

/// Annual ADDITIONAL exit probability for disabled workers at event time k.
/// These are flow rates used in the employment panel's year-by-year simulation.
/// Calibrated so cumulative effects approximate the theta_*_employment curves.
pub fn annual_exit_boost(k: i32, is_dc: bool, severity: i32) -> f64 {
    if k < 0 {
        return 0.0;
    }

    let base = if is_dc {
        match k {
            0 => 0.06,
            1 => 0.07,
            2 => 0.04,
            3 => 0.02,
            4..=5 => 0.01,
            _ => 0.005,
        }
    } else {
        match k {
            0..=4 => 0.01,
            _ => 0.005,
        }
    };

    base * severity_multiplier(severity)
}

/// Multiplier on re-entry probability for disabled workers.
/// < 1.0 means harder to re-enter employment after exit.
/// Combined with annual_exit_boost, produces the target cumulative employment gaps.
pub fn reentry_multiplier(k: i32, is_dc: bool) -> f64 {
    if k < 0 {
        return 1.0;
    }
    if is_dc {
        match k {
            0..=2 => 0.10,
            3..=5 => 0.30,
            6..=8 => 0.50,
            _ => 0.70,
        }
    } else {
        match k {
            0..=2 => 0.50,
            _ => 0.70,
        }
    }
}

// ============================================================================
// Public API: occupation switching
// ============================================================================

/// Additional occupation switching probability at event time k.
/// Stacks on top of background switching rate (~5%/year at 4-digit level).
///
/// Calibrated for cumulative targets:
/// DC at k=3: ~35% (background ~18% + boost ~17%)
/// NC at k=3: ~23% (background ~18% + boost ~5%)
/// ND at k=3: ~18% (background only)
pub fn occ_switch_boost(k: i32, is_dc: bool) -> f64 {
    if k < 0 {
        return 0.0;
    }
    if is_dc {
        match k {
            0 => 0.02,
            1 => 0.08,
            2 => 0.06,
            3 => 0.05,
            4 => 0.03,
            5 => 0.02,
            _ => 0.01,
        }
    } else {
        // NC: modest elevation above background
        match k {
            0..=1 => 0.01,
            2..=4 => 0.02,
            _ => 0.01,
        }
    }
}

/// Backward-compatible alias.
pub fn occ_switch_effect(k: i32, is_dc: bool) -> f64 {
    occ_switch_boost(k, is_dc)
}

// ============================================================================
// DC/NC classification
// ============================================================================

/// Dimension medians (approximate, from occupation distribution)
const DIM_MEDIANS: [f64; 6] = [0.45, 0.30, 0.48, 0.42, 0.42, 0.40];

/// Classify DC/NC using person type's primary dimension directly.
/// More precise than SDAC-based classification.
pub fn classify_dc_by_dim(dim_primary: usize, scores: &[f64; 6]) -> bool {
    if dim_primary >= 6 {
        return false;
    }
    scores[dim_primary] > DIM_MEDIANS[dim_primary]
}

/// Get dose using person type's primary dimension.
pub fn get_dose_by_dim(dim_primary: usize, scores: &[f64; 6]) -> f64 {
    if dim_primary >= 6 {
        return 0.0;
    }
    scores[dim_primary]
}

/// Legacy: classify DC using SDAC type (kept for backward compat).
pub fn classify_dc(disability_type: i32, scores: &[f64; 6]) -> bool {
    for &(sdac_id, dims) in SDAC_TYPE_DIMENSIONS {
        if sdac_id == disability_type {
            if dims.is_empty() {
                return false;
            }
            let primary_dim = dims[0];
            return scores[primary_dim] > DIM_MEDIANS[primary_dim];
        }
    }
    false
}

/// Legacy: get dose using SDAC type.
pub fn get_dose(disability_type: i32, scores: &[f64; 6]) -> f64 {
    for &(sdac_id, dims) in SDAC_TYPE_DIMENSIONS {
        if sdac_id == disability_type {
            if dims.is_empty() {
                return 0.0;
            }
            return scores[dims[0]];
        }
    }
    0.0
}

// ============================================================================
// Cohort-level treatment effect heterogeneity
// ============================================================================

/// Later cohorts (2017-2020) show ~10-15% smaller effects than earlier (2010-2013).
pub fn cohort_multiplier(onset_year: i32) -> f64 {
    let t = ((onset_year - 2010).max(0).min(10)) as f64 / 10.0;
    1.0 - 0.15 * t
}

// ============================================================================
// Person-type-first disability assignment
// ============================================================================

/// Assign disability using person types (1-27) from service_profiles.
/// Returns a list with 7 vectors:
/// - disability_onset_year (NA = no disability)
/// - disability_type (SDAC code derived from person type)
/// - disability_severity (1-4)
/// - is_dc (logical)
/// - disability_dose (0-1)
/// - person_type (1-27, NA = no disability)
/// - comorbidity_flags (bitmask, NA = no disability)
#[extendr]
fn assign_disability__(
    birth_year: &[i32],
    education: &[i32],
    baseline_employed: &[i32], // logical as 0/1
    task_cognitive: &[f64],
    task_physical: &[f64],
    task_vision: &[f64],
    task_hearing: &[f64],
    task_manual_dexterity: &[f64],
    task_communication: &[f64],
    seed: i32,
    onset_min_year: i32,
    onset_max_year: i32,
    disability_rate: f64,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(5));

    let n_onset_years = (onset_max_year - onset_min_year + 1) as usize;

    let mut out_onset_year: Vec<Rint> = Vec::with_capacity(n);
    let mut out_type: Vec<Rint> = Vec::with_capacity(n);
    let mut out_severity: Vec<Rint> = Vec::with_capacity(n);
    let mut out_is_dc: Vec<Rbool> = Vec::with_capacity(n);
    let mut out_dose: Vec<Rfloat> = Vec::with_capacity(n);
    let mut out_person_type: Vec<Rint> = Vec::with_capacity(n);
    let mut out_comorbidity: Vec<Rint> = Vec::with_capacity(n);

    for i in 0..n {
        let by = birth_year[i];
        let edu = education[i];
        let emp = baseline_employed[i] == 1;

        // Only working-age employed adults with occupations can get disability
        let age_2015 = 2015 - by; // midpoint of onset window
        let eligible = emp && edu > 0 && age_2015 >= 25 && age_2015 <= 60;

        if !eligible || rng.gen::<f64>() >= disability_rate {
            out_onset_year.push(Rint::na());
            out_type.push(Rint::na());
            out_severity.push(Rint::na());
            out_is_dc.push(Rbool::na());
            out_dose.push(Rfloat::na());
            out_person_type.push(Rint::na());
            out_comorbidity.push(Rint::na());
            continue;
        }

        // Assign onset year (uniform across cohorts)
        let onset_year = onset_min_year
            + (rng.gen::<f64>() * n_onset_years as f64).min((n_onset_years - 1) as f64) as i32;

        // Check working age at onset
        let age_at_onset = onset_year - by;
        if age_at_onset < 25 || age_at_onset > 60 {
            out_onset_year.push(Rint::na());
            out_type.push(Rint::na());
            out_severity.push(Rint::na());
            out_is_dc.push(Rbool::na());
            out_dose.push(Rfloat::na());
            out_person_type.push(Rint::na());
            out_comorbidity.push(Rint::na());
            continue;
        }

        // Draw PERSON TYPE (1-27) from prevalence weights
        let pt = service_profiles::draw_person_type(&mut rng);
        let profile = &PROFILES[(pt - 1) as usize];

        // Derive SDAC type from person type profile
        let sdac_type = profile.sdac_primary;

        // Draw severity
        let sev_idx = weighted_sample(&mut rng, SEVERITY_WEIGHTS);
        let severity = (sev_idx + 1) as i32; // 1-4

        // Classify DC/NC using person type's primary dimension
        let scores = [
            task_cognitive[i],
            task_physical[i],
            task_vision[i],
            task_hearing[i],
            task_manual_dexterity[i],
            task_communication[i],
        ];
        let is_dc = classify_dc_by_dim(profile.dim_primary, &scores);
        let dose = get_dose_by_dim(profile.dim_primary, &scores);

        // Draw comorbidity flags
        let comorb_flags =
            service_profiles::draw_comorbidity_flags(&mut rng, profile.comorbidity_eligible);

        out_onset_year.push(Rint::from(onset_year));
        out_type.push(Rint::from(sdac_type));
        out_severity.push(Rint::from(severity));
        out_is_dc.push(Rbool::from(is_dc));
        out_dose.push(Rfloat::from(dose));
        out_person_type.push(Rint::from(pt as i32));
        out_comorbidity.push(Rint::from(comorb_flags));
    }

    list!(
        disability_onset_year = out_onset_year,
        disability_type = out_type,
        disability_severity = out_severity,
        is_dc = out_is_dc,
        disability_dose = out_dose,
        person_type = out_person_type,
        comorbidity_flags = out_comorbidity,
    )
}

extendr_module! {
    mod disability;
    fn assign_disability__;
}

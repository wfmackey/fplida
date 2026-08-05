use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::sampling::{normal_sample, weighted_sample};

// SDAC: ~65K persons sampled. We sample ~2.5% of spine.
const SDAC_SAMPLE_RATE: f64 = 0.025;

// DISSTAT: Disability status (8 categories from ABS SDAC)
// 1=Profound, 2=Severe, 3=Moderate, 4=Mild, 5=Not limited but restricted,
// 6=Not limited or restricted, 7=Long-term condition only, 8=No condition/disability
// ABS SDAC 2022 public table: disability and profound/severe disability
// prevalence by age group and sex.
const SDAC_AGE_BANDS: [(i32, i32); 14] = [
    (0, 4),
    (5, 14),
    (15, 24),
    (25, 34),
    (35, 44),
    (45, 54),
    (55, 59),
    (60, 64),
    (65, 69),
    (70, 74),
    (75, 79),
    (80, 84),
    (85, 89),
    (90, i32::MAX),
];
const SDAC_DISABILITY_RATE_MALE: [f64; 14] = [
    0.072, 0.164, 0.144, 0.097, 0.103, 0.169, 0.243, 0.308, 0.415, 0.465, 0.554, 0.666, 0.768,
    0.863,
];
const SDAC_DISABILITY_RATE_FEMALE: [f64; 14] = [
    0.041, 0.107, 0.132, 0.112, 0.125, 0.184, 0.279, 0.347, 0.395, 0.438, 0.514, 0.685, 0.776,
    0.822,
];
const SDAC_PROFOUND_SEVERE_RATE_MALE: [f64; 14] = [
    0.044, 0.103, 0.058, 0.027, 0.030, 0.047, 0.071, 0.080, 0.106, 0.149, 0.195, 0.287, 0.396,
    0.647,
];
const SDAC_PROFOUND_SEVERE_RATE_FEMALE: [f64; 14] = [
    0.031, 0.056, 0.046, 0.036, 0.033, 0.043, 0.083, 0.095, 0.137, 0.131, 0.222, 0.318, 0.506,
    0.688,
];

// DISGP: Disability groups (1-6)
// 1=Sensory/speech, 2=Learning/understanding, 3=Physical, 4=Psychosocial,
// 5=Head injury/stroke/ABI, 6=Other
const DISGP_CODES: [i32; 6] = [1, 2, 3, 4, 5, 6];
const DISGP_WEIGHTS: [f64; 6] = [0.15, 0.10, 0.35, 0.20, 0.08, 0.12];

// Disability types (17 types from SDAC)
const DISTYPE_CODES: [i32; 17] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17];
// 1=Loss of sight, 2=Loss of hearing, 3=Speech difficulties,
// 4=Breathing difficulties, 5=Chronic pain, 6=Blackouts/seizures,
// 7=Difficulty learning, 8=Incomplete use of arms, 9=Difficulty gripping,
// 10=Incomplete use of legs, 11=Nervous/emotional, 12=Restriction in physical,
// 13=Disfigurement, 14=Mental illness, 15=Memory problems,
// 16=Social/behavioural, 17=Head injury/stroke/ABI
const DISTYPE_WEIGHTS: [f64; 17] = [
    0.04, 0.06, 0.03, 0.07, 0.15, 0.03, 0.05, 0.04, 0.05, 0.08, 0.10, 0.10, 0.02, 0.08, 0.04, 0.03,
    0.03,
];

// SDAC condition codes (simplified top-20)
const COND_CODES: [&str; 20] = [
    "220204", "220103", "140302", "140402", "130202", "190201", "180101", "050501", "220301",
    "180201", "150301", "150501", "130201", "190101", "160101", "170101", "110101", "140501",
    "150101", "120101",
];
// Back pain, Osteoarthritis, Depression, Anxiety, T2DM, Asthma,
// Hypertension, Chronic pain, Osteoporosis, Heart disease, Parkinson's, MS,
// T1DM, COPD, Vision, Hearing, Cancer, ASD, Epilepsy, Blood disorder
const COND_WEIGHTS: [f64; 20] = [
    0.12, 0.10, 0.09, 0.08, 0.08, 0.07, 0.07, 0.06, 0.05, 0.05, 0.02, 0.02, 0.02, 0.03, 0.03, 0.03,
    0.02, 0.02, 0.02, 0.02,
];

// Public SDAC 2022 anchors are only broad main-condition groups. These weights
// keep the synthetic detailed codes inside those broad shares: severe/profound
// skews more mental/developmental, moderate/mild skews more physical.
const COND_WEIGHTS_SEVERE: [f64; 20] = [
    0.070, 0.085, 0.110, 0.080, 0.040, 0.040, 0.040, 0.060, 0.025, 0.035, 0.020, 0.018, 0.012,
    0.022, 0.028, 0.032, 0.026, 0.130, 0.028, 0.024,
];
const COND_WEIGHTS_MODERATE_MILD: [f64; 20] = [
    0.154, 0.147, 0.050, 0.070, 0.055, 0.050, 0.050, 0.075, 0.045, 0.040, 0.018, 0.018, 0.016,
    0.026, 0.026, 0.030, 0.030, 0.075, 0.022, 0.018,
];
const COND_WEIGHTS_RESTRICTED: [f64; 20] = [
    0.125, 0.120, 0.075, 0.085, 0.055, 0.050, 0.055, 0.060, 0.040, 0.042, 0.018, 0.018, 0.016,
    0.026, 0.026, 0.030, 0.026, 0.090, 0.022, 0.026,
];

fn sdac_age_band_index(age: i32) -> usize {
    SDAC_AGE_BANDS
        .iter()
        .position(|(lo, hi)| age >= *lo && age <= *hi)
        .unwrap_or(SDAC_AGE_BANDS.len() - 1)
}

fn sdac_disability_rate(age: i32, sex: i32) -> f64 {
    let idx = sdac_age_band_index(age);
    if sex == 2 {
        SDAC_DISABILITY_RATE_FEMALE[idx]
    } else {
        SDAC_DISABILITY_RATE_MALE[idx]
    }
}

fn sdac_profound_severe_rate(age: i32, sex: i32) -> f64 {
    let idx = sdac_age_band_index(age);
    if sex == 2 {
        SDAC_PROFOUND_SEVERE_RATE_FEMALE[idx]
    } else {
        SDAC_PROFOUND_SEVERE_RATE_MALE[idx]
    }
}

/// Map the spine's `disability_severity` onto the SDAC DISSTAT ladder.
///
/// The two code frames are not the same scale, so this is not the identity.
/// The spine (see `disability.rs`) uses 1=profound/severe, 2=moderate,
/// 3=mild, 4=condition only. DISSTAT uses 1=Profound, 2=Severe, 3=Moderate,
/// 4=Mild, 5=Not limited but restricted, 6=Not limited or restricted,
/// 7=Long-term condition only, 8=No condition. Mapping across as an identity
/// overstates severity at every level, and puts "condition only" on Mild.
///
/// Spine level 1 combines profound and severe. The package has no published
/// profound-only rate to split it — only the combined
/// `SDAC_PROFOUND_SEVERE_RATE_*` tables — so an anchored record takes Severe
/// rather than inventing a split. The residual hazard still draws Profound
/// from the published combined rate, so the profound-plus-severe marginal,
/// which is the quantity the ABS actually publishes, is preserved.
///
/// `disability_severity` is NA (the R integer NA sentinel) for everyone the
/// spine did not give a disability, so only 1-4 anchors.
fn spine_disstat(disability_severity: i32) -> Option<i32> {
    match disability_severity {
        1 => Some(2), // profound/severe -> Severe
        2 => Some(3), // moderate        -> Moderate
        3 => Some(4), // mild            -> Mild
        4 => Some(7), // condition only  -> Long-term condition only
        _ => None,
    }
}

/// Anchor only where the spine disability has already begun by survey night.
///
/// A disability with a later onset year has not happened yet, so the person
/// belongs in the residual hazard draw rather than being coded disabled. A
/// missing onset year (the R integer NA sentinel) is treated as already
/// present, which matches how the spine records lifelong conditions.
fn anchored_disstat(
    disability_severity: i32,
    disability_onset_year: i32,
    survey_year: i32,
) -> Option<i32> {
    if disability_onset_year != i32::MIN && disability_onset_year > survey_year {
        return None;
    }
    spine_disstat(disability_severity)
}

/// Age/sex cell shares of the spine-anchored SDAC records.
///
/// The spine only gives disability to employed 25-60 year olds, so anchoring
/// alone cannot reproduce full-population SDAC prevalence. These shares let the
/// residual hazard be rebased cell by cell, keeping the published marginals.
struct AnchorShares {
    any: [[f64; 2]; SDAC_AGE_BANDS.len()],
    profound_severe: [[f64; 2]; SDAC_AGE_BANDS.len()],
}

fn sex_index(sex: i32) -> usize {
    if sex == 2 {
        1
    } else {
        0
    }
}

/// Deterministic pre-pass over the spine — no RNG is consumed here.
fn anchor_shares(
    birth_year: &[i32],
    sex: &[i32],
    disability_severity: &[i32],
    disability_onset_year: &[i32],
    survey_year: i32,
) -> AnchorShares {
    let mut total = [[0.0f64; 2]; SDAC_AGE_BANDS.len()];
    let mut any = [[0.0f64; 2]; SDAC_AGE_BANDS.len()];
    let mut profound_severe = [[0.0f64; 2]; SDAC_AGE_BANDS.len()];

    for i in 0..birth_year.len() {
        let age = survey_year - birth_year[i];
        if age < 0 {
            continue;
        }
        let b = sdac_age_band_index(age);
        let s = sex_index(sex[i]);
        total[b][s] += 1.0;
        if let Some(disstat) =
            anchored_disstat(disability_severity[i], disability_onset_year[i], survey_year)
        {
            // DISSTAT 7 is a long-term condition with no core-activity
            // limitation, which SDAC does not count as disability, so it must
            // not consume the published disability prevalence.
            if disstat <= 6 {
                any[b][s] += 1.0;
            }
            if disstat <= 2 {
                profound_severe[b][s] += 1.0;
            }
        }
    }

    for b in 0..SDAC_AGE_BANDS.len() {
        for s in 0..2 {
            if total[b][s] > 0.0 {
                any[b][s] /= total[b][s];
                profound_severe[b][s] /= total[b][s];
            }
        }
    }

    AnchorShares {
        any,
        profound_severe,
    }
}

/// Hazard rates for the persons the spine did not anchor, rebased so that the
/// anchored plus residual records still reproduce the published age/sex rate.
fn residual_rates(age: i32, sex: i32, shares: &AnchorShares) -> (f64, f64) {
    let b = sdac_age_band_index(age);
    let s = sex_index(sex);
    let anchored_any = shares.any[b][s];
    let anchored_ps = shares.profound_severe[b][s];
    if anchored_any >= 1.0 {
        return (0.0, 0.0);
    }
    let rate = ((sdac_disability_rate(age, sex) - anchored_any) / (1.0 - anchored_any))
        .clamp(0.0, 1.0);
    let ps = ((sdac_profound_severe_rate(age, sex) - anchored_ps) / (1.0 - anchored_any))
        .clamp(0.0, rate);
    (rate, ps)
}

/// Unanchored draw straight off the published age/sex rates. The generators
/// use the rebased residual rates instead; this is kept as the reference the
/// published-gradient test measures against.
#[cfg(test)]
fn draw_disstat(age: i32, sex: i32, rng: &mut StdRng) -> i32 {
    draw_disstat_at_rates(
        age,
        sdac_disability_rate(age, sex),
        sdac_profound_severe_rate(age, sex),
        rng,
    )
}

fn draw_disstat_at_rates(
    age: i32,
    disability_rate: f64,
    profound_severe_rate: f64,
    rng: &mut StdRng,
) -> i32 {
    if rng.gen::<f64>() >= disability_rate {
        if rng.gen::<f64>() < long_term_condition_only_rate(age) {
            return 7;
        }
        return 8;
    }

    let profound_severe_rate = profound_severe_rate.min(disability_rate);
    let severe_share = if disability_rate > 0.0 {
        profound_severe_rate / disability_rate
    } else {
        0.0
    };
    if rng.gen::<f64>() < severe_share {
        if rng.gen::<f64>() < 0.52 {
            1
        } else {
            2
        }
    } else {
        let non_severe_weights = [0.22, 0.43, 0.18, 0.17];
        match weighted_sample(rng, &non_severe_weights) {
            0 => 3,
            1 => 4,
            2 => 5,
            _ => 6,
        }
    }
}

fn long_term_condition_only_rate(age: i32) -> f64 {
    match age {
        0..=14 => 0.070,
        15..=34 => 0.105,
        35..=54 => 0.150,
        55..=64 => 0.200,
        65..=84 => 0.260,
        _ => 0.200,
    }
}

fn draw_numcond(disstat: i32, age: i32, rng: &mut StdRng) -> i32 {
    if disstat <= 4 {
        let upper = if age >= 65 {
            6
        } else if age >= 45 {
            5
        } else {
            4
        };
        rng.gen_range(1..=upper).min(9)
    } else if disstat <= 7 {
        let upper = if age >= 65 { 5 } else { 3 };
        rng.gen_range(1..=upper).min(9)
    } else {
        0
    }
}

fn draw_main_condition(disstat: i32, age: i32, person_type: i32, rng: &mut StdRng) -> String {
    let idx = if person_type >= 1 && rng.gen::<f64>() < 0.20 {
        profile_condition_index(person_type).unwrap_or_else(|| weighted_sample(rng, &COND_WEIGHTS))
    } else if disstat <= 2 {
        weighted_sample(rng, &COND_WEIGHTS_SEVERE)
    } else if disstat <= 4 {
        weighted_sample(rng, &COND_WEIGHTS_MODERATE_MILD)
    } else if disstat <= 7 {
        weighted_sample(rng, &COND_WEIGHTS_RESTRICTED)
    } else if age >= 65 && rng.gen::<f64>() < 0.35 {
        weighted_sample(rng, &COND_WEIGHTS_RESTRICTED)
    } else {
        weighted_sample(rng, &COND_WEIGHTS)
    };
    COND_CODES[idx].to_string()
}

/// Share of anchored records whose reported main disability group and type
/// match the spine person type's primary limitation. The rest nominate a
/// secondary limitation as the main one, as SDAC respondents do.
const SPINE_TYPE_ANCHOR_PROB: f64 = 0.80;

fn disgp_for_sdac_type(sdac_type: i32) -> i32 {
    match sdac_type {
        1 | 2 | 3 => 1,                    // sensory / speech
        7 | 15 => 2,                       // learning / understanding
        4 | 5 | 8 | 9 | 10 | 12 | 13 => 3, // physical
        11 | 14 | 16 => 4,                 // psychosocial
        17 => 5,                           // head injury / stroke / ABI
        _ => 6,                            // other
    }
}

fn draw_disgp(person_type: i32, rng: &mut StdRng) -> i32 {
    if let Some(profile) = service_profile_for_person_type(person_type) {
        if rng.gen::<f64>() < SPINE_TYPE_ANCHOR_PROB {
            return disgp_for_sdac_type(profile.sdac_primary);
        }
    }
    DISGP_CODES[weighted_sample(rng, &DISGP_WEIGHTS)]
}

fn draw_distype(person_type: i32, rng: &mut StdRng) -> i32 {
    if let Some(profile) = service_profile_for_person_type(person_type) {
        if rng.gen::<f64>() < SPINE_TYPE_ANCHOR_PROB {
            let t = profile.sdac_primary;
            if (1..=17).contains(&t) {
                return t;
            }
        }
    }
    DISTYPE_CODES[weighted_sample(rng, &DISTYPE_WEIGHTS)]
}

/// Core activity limitations: communication, mobility, self-care.
/// Communication, mobility and self-care limitations consistent with DISSTAT.
///
/// SDAC derives DISSTAT as the most severe of the three core-activity
/// limitations, so exactly one of them binds at the DISSTAT level and the
/// other two are equal or milder. Codes run 1=profound to 5=no limitation, so
/// "milder" is a larger number and the binding domain is the minimum.
///
/// Which domain binds is drawn. Fixing communication as the binding one left
/// COMMCALN perfectly collinear with DISSTAT, which real SDAC records are not.
fn draw_core_activity_limitations(disstat: i32, rng: &mut StdRng) -> (i32, i32, i32) {
    if disstat > 4 {
        return (5, 5, 5); // no limitation
    }
    let base = disstat; // 1=profound, 2=severe, 3=moderate, 4=mild
    let binding = rng.gen_range(0..3usize);
    let mut out = [0i32; 3];
    for (i, slot) in out.iter_mut().enumerate() {
        *slot = if i == binding {
            base
        } else {
            (base + rng.gen_range(0..=1)).min(5)
        };
    }
    (out[0], out[1], out[2])
}

fn profile_condition_index(person_type: i32) -> Option<usize> {
    let profile = service_profile_for_person_type(person_type)?;
    match profile.sdac_primary {
        1 => Some(14),      // loss of sight
        2 => Some(15),      // loss of hearing
        4 => Some(13),      // breathing difficulty / COPD
        5 | 12 => Some(0),  // pain / physical restriction
        6 => Some(18),      // blackouts or seizures
        7 | 16 => Some(17), // developmental/social
        8 | 9 | 10 => Some(0),
        11 | 14 => Some(2),
        15 => Some(3),
        17 => Some(9),
        _ => None,
    }
}

fn service_profile_for_person_type(
    person_type: i32,
) -> Option<&'static crate::service_profiles::PersonTypeProfile> {
    if person_type < 1 {
        return None;
    }
    crate::service_profiles::PROFILES.get((person_type - 1) as usize)
}

// Core activity limitation levels: 0=NA, 1=Profound, 2=Severe, 3=Moderate, 4=Mild, 5=No limitation
const CAL_CODES: [i32; 6] = [0, 1, 2, 3, 4, 5];

// Employment restriction severity: 0=NA, 1=Profound, 2=Severe, 3=Moderate, 4=Mild, 5=No restriction
const EMP_SEV_CODES: [i32; 6] = [0, 1, 2, 3, 4, 5];

// Self-assessed health: 1=Excellent, 2=Very good, 3=Good, 4=Fair, 5=Poor
const SAH_CODES: [i32; 5] = [1, 2, 3, 4, 5];
const SAH_WEIGHTS: [f64; 5] = [0.15, 0.30, 0.30, 0.15, 0.10];

// Carer status: 0=NA, 1=Primary+Other, 2=Primary only, 3=Secondary+Other,
// 4=Secondary only, 5=Carer not primary/secondary, 6=Not a carer
const CARER_CODES: [i32; 7] = [0, 1, 2, 3, 4, 5, 6];
const CARER_WEIGHTS: [f64; 7] = [0.0, 0.02, 0.04, 0.01, 0.02, 0.03, 0.88];

/// Project SDAC (Survey of Disability, Ageing and Carers) person-level from spine.
///
/// ~2.5% sample. Generates person-level records with disability status,
/// conditions, limitations, restrictions, employment, self-assessed health,
/// and carer status. Based on SDAC 2018/2022 data item list.
/// @export
#[extendr]
fn project_sdac__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    education: &[i32],
    baseline_employed: &[i32],
    disability_severity: &[i32],
    disability_onset_year: &[i32],
    person_type: &[i32],
    seed: i64,
    survey_year: i32,
) -> List {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let shares = anchor_shares(
        birth_year,
        sex,
        disability_severity,
        disability_onset_year,
        survey_year,
    );

    let est_n = (n as f64 * SDAC_SAMPLE_RATE) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_agep: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_sexp: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_ingp: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_state: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_disstat: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_wthrdis: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_disgp: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_distype: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_numcond: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_condmain: Vec<String> = Vec::with_capacity(est_n);
    let mut out_cal_comm: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_cal_mob: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_cal_self: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_empsevr: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_lfsp: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_sfhealth: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_k10score: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_carer: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_weight: Vec<f64> = Vec::with_capacity(est_n);

    for i in 0..n {
        if rng.gen::<f64>() >= SDAC_SAMPLE_RATE {
            continue;
        }

        let by = birth_year[i];
        let age = survey_year - by;
        if age < 0 {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();
        let sx = sex[i];
        let ind = indigenous[i];
        let st = state[i];
        let sev = disability_severity[i];
        let pt = person_type[i];

        // Anchor DISSTAT on the spine where the spine gave this person a
        // disability; draw from the rebased hazard for everyone else so the
        // published full-population prevalence is still reached.
        let disstat = match anchored_disstat(sev, disability_onset_year[i], survey_year) {
            Some(anchored) => anchored,
            None => {
                let (rate, ps_rate) = residual_rates(age, sx, &shares);
                draw_disstat_at_rates(age, rate, ps_rate, &mut rng)
            }
        };

        let has_disability = disstat <= 6;
        let wthrdis = if has_disability { 1 } else { 2 };

        // Disability group and type (only for disabled)
        let disgp = if has_disability {
            draw_disgp(pt, &mut rng)
        } else {
            7
        }; // Not applicable

        let distype = if has_disability {
            draw_distype(pt, &mut rng)
        } else {
            18
        }; // Not applicable

        // Number of conditions (0-9)
        let numcond = draw_numcond(disstat, age, &mut rng);

        // Main condition code
        let condmain = if numcond > 0 {
            draw_main_condition(disstat, age, pt, &mut rng)
        } else {
            "999999".to_string()
        };

        // Core activity limitations (communication, mobility, self-care)
        let (cal_c, cal_m, cal_s) = draw_core_activity_limitations(disstat, &mut rng);

        // Employment restriction severity
        let empsevr = if age < 15 || age > 65 {
            0
        } else if disstat <= 2 {
            rng.gen_range(1..=2)
        } else if disstat <= 4 {
            rng.gen_range(3..=4)
        } else if disstat <= 6 {
            rng.gen_range(4..=5)
        } else {
            5
        };

        // LFS: 1=FT, 2=PT, 3=Away, 4=Unemp FT, 5=Unemp PT, 6=NILF
        let lfsp = if age < 15 || age > 75 {
            6
        } else if baseline_employed[i] == 1 {
            if rng.gen::<f64>() < 0.70 {
                1
            } else {
                2
            }
        } else {
            if rng.gen::<f64>() < 0.30 {
                4
            } else {
                6
            }
        };

        // Self-assessed health
        let sah_idx = if disstat <= 2 {
            // Severe disability: skew towards fair/poor
            rng.gen_range(2..=4)
        } else {
            weighted_sample(&mut rng, &SAH_WEIGHTS)
        };
        let sfhealth = SAH_CODES[sah_idx.min(4)];

        // Kessler 10 score (10-50, higher = more distress)
        let k10 = if disstat <= 4 {
            (normal_sample(&mut rng, 25.0, 8.0) as i32).max(10).min(50)
        } else {
            (normal_sample(&mut rng, 15.0, 5.0) as i32).max(10).min(50)
        };

        // Carer status
        let cr_idx = weighted_sample(&mut rng, &CARER_WEIGHTS);
        let carer = CARER_CODES[cr_idx];

        // Survey weight: ~40 for 2.5% sample
        let weight = normal_sample(&mut rng, 40.0, 8.0).max(10.0);

        out_aeuid.push(person_aeuid);
        out_agep.push(age.min(85)); // top-coded at 85
        out_sexp.push(sx);
        out_ingp.push(ind);
        out_state.push(st);
        out_disstat.push(disstat);
        out_wthrdis.push(wthrdis);
        out_disgp.push(disgp);
        out_distype.push(distype);
        out_numcond.push(numcond);
        out_condmain.push(condmain);
        out_cal_comm.push(cal_c);
        out_cal_mob.push(cal_m);
        out_cal_self.push(cal_s);
        out_empsevr.push(empsevr);
        out_lfsp.push(lfsp);
        out_sfhealth.push(sfhealth);
        out_k10score.push(k10);
        out_carer.push(carer);
        out_weight.push((weight * 100.0).round() / 100.0);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        AGEP = out_agep,
        SEXP = out_sexp,
        INGP = out_ingp,
        STATE = out_state,
        DISSTAT = out_disstat,
        WTHRDIS = out_wthrdis,
        DISGP = out_disgp,
        DISTYPE = out_distype,
        NUMCOND = out_numcond,
        CONDMADL = out_condmain,
        COMMCALN = out_cal_comm,
        MOBCALN = out_cal_mob,
        SELFCALN = out_cal_self,
        EMPSEVR = out_empsevr,
        LFSP = out_lfsp,
        SFHEALTH = out_sfhealth,
        K10SCORE = out_k10score,
        CARER22 = out_carer,
        FINWTP = out_weight
    )
}

/// Project SDAC directly to parquet. Single file.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_sdac_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    education: &[i32],
    baseline_employed: &[i32],
    disability_severity: &[i32],
    disability_onset_year: &[i32],
    person_type: &[i32],
    seed: i64,
    survey_year: i32,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let _ = (country_of_birth, education);
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let shares = anchor_shares(
        birth_year,
        sex,
        disability_severity,
        disability_onset_year,
        survey_year,
    );

    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_agep: Vec<i32> = Vec::new();
    let mut out_sexp: Vec<i32> = Vec::new();
    let mut out_ingp: Vec<i32> = Vec::new();
    let mut out_state: Vec<i32> = Vec::new();
    let mut out_disstat: Vec<i32> = Vec::new();
    let mut out_wthrdis: Vec<i32> = Vec::new();
    let mut out_disgp: Vec<i32> = Vec::new();
    let mut out_distype: Vec<i32> = Vec::new();
    let mut out_numcond: Vec<i32> = Vec::new();
    let mut out_condmain: Vec<String> = Vec::new();
    let mut out_cal_comm: Vec<i32> = Vec::new();
    let mut out_cal_mob: Vec<i32> = Vec::new();
    let mut out_cal_self: Vec<i32> = Vec::new();
    let mut out_empsevr: Vec<i32> = Vec::new();
    let mut out_lfsp: Vec<i32> = Vec::new();
    let mut out_sfhealth: Vec<i32> = Vec::new();
    let mut out_k10score: Vec<i32> = Vec::new();
    let mut out_carer: Vec<i32> = Vec::new();
    let mut out_weight: Vec<f64> = Vec::new();

    for i in 0..n {
        if rng.gen::<f64>() >= SDAC_SAMPLE_RATE {
            continue;
        }
        let by = birth_year[i];
        let age = survey_year - by;
        if age < 0 {
            continue;
        }

        let sx = sex[i];
        let ind = indigenous[i];
        let st = state[i];
        let sev = disability_severity[i];
        let pt = person_type[i];

        let disstat = match anchored_disstat(sev, disability_onset_year[i], survey_year) {
            Some(anchored) => anchored,
            None => {
                let (rate, ps_rate) = residual_rates(age, sx, &shares);
                draw_disstat_at_rates(age, rate, ps_rate, &mut rng)
            }
        };
        let has_disability = disstat <= 6;
        let wthrdis = if has_disability { 1 } else { 2 };
        let disgp = if has_disability {
            draw_disgp(pt, &mut rng)
        } else {
            7
        };
        let distype = if has_disability {
            draw_distype(pt, &mut rng)
        } else {
            18
        };
        let numcond = draw_numcond(disstat, age, &mut rng);
        let condmain = if numcond > 0 {
            draw_main_condition(disstat, age, pt, &mut rng)
        } else {
            "999999".to_string()
        };
        let (cal_c, cal_m, cal_s) = draw_core_activity_limitations(disstat, &mut rng);
        let empsevr = if age < 15 || age > 65 {
            0
        } else if disstat <= 2 {
            rng.gen_range(1..=2)
        } else if disstat <= 4 {
            rng.gen_range(3..=4)
        } else if disstat <= 6 {
            rng.gen_range(4..=5)
        } else {
            5
        };
        let lfsp = if age < 15 || age > 75 {
            6
        } else if baseline_employed[i] == 1 {
            if rng.gen::<f64>() < 0.70 {
                1
            } else {
                2
            }
        } else {
            if rng.gen::<f64>() < 0.30 {
                4
            } else {
                6
            }
        };
        let sah_idx = if disstat <= 2 {
            rng.gen_range(2..=4)
        } else {
            weighted_sample(&mut rng, &SAH_WEIGHTS)
        };
        let sfhealth = SAH_CODES[sah_idx.min(4)];
        let k10 = if disstat <= 4 {
            (normal_sample(&mut rng, 25.0, 8.0) as i32).max(10).min(50)
        } else {
            (normal_sample(&mut rng, 15.0, 5.0) as i32).max(10).min(50)
        };
        let cr_idx = weighted_sample(&mut rng, &CARER_WEIGHTS);
        let carer = CARER_CODES[cr_idx];
        let weight = normal_sample(&mut rng, 40.0, 8.0).max(10.0);

        out_aeuid.push(aeuid[i].to_string());
        out_agep.push(age.min(85));
        out_sexp.push(sx);
        out_ingp.push(ind);
        out_state.push(st);
        out_disstat.push(disstat);
        out_wthrdis.push(wthrdis);
        out_disgp.push(disgp);
        out_distype.push(distype);
        out_numcond.push(numcond);
        out_condmain.push(condmain);
        out_cal_comm.push(cal_c);
        out_cal_mob.push(cal_m);
        out_cal_self.push(cal_s);
        out_empsevr.push(empsevr);
        out_lfsp.push(lfsp);
        out_sfhealth.push(sfhealth);
        out_k10score.push(k10);
        out_carer.push(carer);
        out_weight.push((weight * 100.0).round() / 100.0);
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "AGEP",
            col: Col::I32(out_agep),
        },
        NamedCol {
            name: "SEXP",
            col: Col::I32(out_sexp),
        },
        NamedCol {
            name: "INGP",
            col: Col::I32(out_ingp),
        },
        NamedCol {
            name: "STATE",
            col: Col::I32(out_state),
        },
        NamedCol {
            name: "DISSTAT",
            col: Col::I32(out_disstat),
        },
        NamedCol {
            name: "WTHRDIS",
            col: Col::I32(out_wthrdis),
        },
        NamedCol {
            name: "DISGP",
            col: Col::I32(out_disgp),
        },
        NamedCol {
            name: "DISTYPE",
            col: Col::I32(out_distype),
        },
        NamedCol {
            name: "NUMCOND",
            col: Col::I32(out_numcond),
        },
        NamedCol {
            name: "CONDMADL",
            col: Col::Str(out_condmain),
        },
        NamedCol {
            name: "COMMCALN",
            col: Col::I32(out_cal_comm),
        },
        NamedCol {
            name: "MOBCALN",
            col: Col::I32(out_cal_mob),
        },
        NamedCol {
            name: "SELFCALN",
            col: Col::I32(out_cal_self),
        },
        NamedCol {
            name: "EMPSEVR",
            col: Col::I32(out_empsevr),
        },
        NamedCol {
            name: "LFSP",
            col: Col::I32(out_lfsp),
        },
        NamedCol {
            name: "SFHEALTH",
            col: Col::I32(out_sfhealth),
        },
        NamedCol {
            name: "K10SCORE",
            col: Col::I32(out_k10score),
        },
        NamedCol {
            name: "CARER22",
            col: Col::I32(out_carer),
        },
        NamedCol {
            name: "FINWTP",
            col: Col::F64(out_weight),
        },
    ];
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("sdac parquet write: {}", e));
    total
}

extendr_module! {
    mod sdac;
    fn project_sdac__;
    fn project_sdac_to_parquet__;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sdac_public_rate_helpers_match_2022_table_points() {
        assert!((sdac_disability_rate(2, 1) - 0.072).abs() < 1e-12);
        assert!((sdac_disability_rate(2, 2) - 0.041).abs() < 1e-12);
        assert!((sdac_disability_rate(57, 1) - 0.243).abs() < 1e-12);
        assert!((sdac_disability_rate(92, 2) - 0.822).abs() < 1e-12);
        assert!((sdac_profound_severe_rate(9, 1) - 0.103).abs() < 1e-12);
        assert!((sdac_profound_severe_rate(87, 2) - 0.506).abs() < 1e-12);
    }

    #[test]
    fn sdac_disstat_draws_follow_public_age_gradient() {
        let mut young_rng = StdRng::seed_from_u64(100);
        let mut old_rng = StdRng::seed_from_u64(101);
        let n = 20_000;
        let young_disabled = (0..n)
            .filter(|_| draw_disstat(30, 1, &mut young_rng) <= 6)
            .count() as f64
            / n as f64;
        let old_disabled = (0..n)
            .filter(|_| draw_disstat(82, 1, &mut old_rng) <= 6)
            .count() as f64
            / n as f64;

        assert!((young_disabled - 0.097).abs() < 0.015);
        assert!((old_disabled - 0.666).abs() < 0.025);
        assert!(old_disabled > young_disabled * 5.0);
    }

    #[test]
    fn spine_severity_maps_onto_disstat_ladder() {
        // Not the identity: the spine scale and DISSTAT are different frames.
        assert_eq!(spine_disstat(1), Some(2)); // profound/severe -> Severe
        assert_eq!(spine_disstat(2), Some(3)); // moderate        -> Moderate
        assert_eq!(spine_disstat(3), Some(4)); // mild            -> Mild
        assert_eq!(spine_disstat(4), Some(7)); // condition only  -> condition only
        assert_eq!(spine_disstat(0), None);
        assert_eq!(spine_disstat(5), None);
        // R passes NA integers through as the NA sentinel.
        assert_eq!(spine_disstat(i32::MIN), None);
    }

    #[test]
    fn anchor_is_gated_on_onset_year() {
        // Onset after survey night: not yet disabled, so no anchor.
        assert_eq!(anchored_disstat(1, 2020, 2018), None);
        // Onset on or before survey night: anchored.
        assert_eq!(anchored_disstat(1, 2018, 2018), Some(2));
        assert_eq!(anchored_disstat(2, 2010, 2018), Some(3));
        // A missing onset year is treated as already present.
        assert_eq!(anchored_disstat(3, i32::MIN, 2018), Some(4));
    }

    #[test]
    fn residual_rates_offset_the_anchored_share() {
        let birth_year = vec![1980; 1000];
        let sex = vec![1; 1000];
        // 200 of 1000 anchored, 100 of them profound/severe.
        let mut severity = vec![i32::MIN; 1000];
        for (k, s) in severity.iter_mut().take(200).enumerate() {
            *s = if k < 100 { 1 } else { 3 };
        }
        let onset = vec![i32::MIN; birth_year.len()];
        let shares = anchor_shares(&birth_year, &sex, &severity, &onset, 2018);
        let b = sdac_age_band_index(38);
        assert!((shares.any[b][0] - 0.20).abs() < 1e-12);
        assert!((shares.profound_severe[b][0] - 0.10).abs() < 1e-12);

        // Published male 35-44 rates are 0.103 (any) and 0.030 (prof/severe);
        // both are already exceeded by the anchored share, so the residual is 0.
        let (rate, ps) = residual_rates(38, 1, &shares);
        assert!(rate.abs() < 1e-12);
        assert!(ps.abs() < 1e-12);

        // With no anchoring the residual falls back to the published rate.
        let empty = anchor_shares(&birth_year, &sex, &vec![i32::MIN; 1000], &onset, 2018);
        let (rate, ps) = residual_rates(38, 1, &empty);
        assert!((rate - 0.103).abs() < 1e-12);
        assert!((ps - 0.030).abs() < 1e-12);
    }

    #[test]
    fn core_activity_limitations_vary_below_disstat() {
        let mut rng = StdRng::seed_from_u64(202);
        let n = 20_000;
        let disstat = 2;
        // How often each domain is the binding (most severe) one.
        let mut binds = [0usize; 3];
        let mut differs = [0usize; 3];
        for _ in 0..n {
            let (comm, mob, slf) = draw_core_activity_limitations(disstat, &mut rng);
            let all = [comm, mob, slf];
            // SDAC derives DISSTAT as the most severe limitation, and lower
            // codes are more severe, so the minimum must equal DISSTAT.
            assert_eq!(*all.iter().min().unwrap(), disstat);
            for (i, v) in all.iter().enumerate() {
                assert!(*v >= disstat && *v <= disstat + 1);
                if *v == disstat {
                    binds[i] += 1;
                } else {
                    differs[i] += 1;
                }
            }
        }
        // No domain is always the limiting one, and none is ever ignored.
        for i in 0..3 {
            let share = differs[i] as f64 / n as f64;
            assert!(
                share > 0.20 && share < 0.50,
                "domain {i} differed from DISSTAT in {share} of draws"
            );
            assert!(binds[i] > 0);
        }

        // Mild disability cannot spill past the "no limitation" code.
        let (c, mob, slf) = draw_core_activity_limitations(4, &mut rng);
        assert!(c <= 5 && mob <= 5 && slf <= 5);

        // No limitation at all above the ladder.
        assert_eq!(draw_core_activity_limitations(7, &mut rng), (5, 5, 5));
    }

    #[test]
    fn disability_group_and_type_follow_the_spine_person_type() {
        let mut rng = StdRng::seed_from_u64(303);
        let n = 20_000;
        let person_type = 1;
        let expected = service_profile_for_person_type(person_type)
            .expect("person type 1 has a profile")
            .sdac_primary;
        let matches = (0..n)
            .filter(|_| draw_distype(person_type, &mut rng) == expected)
            .count() as f64
            / n as f64;
        assert!(matches > 0.75, "DISTYPE matched the spine in {matches}");

        let expected_group = disgp_for_sdac_type(expected);
        let group_matches = (0..n)
            .filter(|_| draw_disgp(person_type, &mut rng) == expected_group)
            .count() as f64
            / n as f64;
        assert!(group_matches > 0.75);

        // No person type: fall back to the published weights.
        let fallback = draw_distype(i32::MIN, &mut rng);
        assert!((1..=17).contains(&fallback));
        let fallback_group = draw_disgp(i32::MIN, &mut rng);
        assert!((1..=6).contains(&fallback_group));
    }
}

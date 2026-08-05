use rand::rngs::StdRng;
use rand::Rng;

use super::Person;
use crate::sampling::{normal_sample, weighted_sample};

// Occupation-specific log-earnings intercepts (FT, prime-age male baseline).
// Indexed by archetype: 0=Labourer, 1=SkilledTrade, 2=Health, 3=Office,
// 4=Professional, 5=Manager, 6=Technical, 7=Service
// Calibrated so median FT earnings match ABS occupation-level data.
const MU_OCC: [f64; 8] = [
    10.80, // Labourer     ~$49K
    11.10, // SkilledTrade ~$67K
    11.10, // Health       ~$67K
    11.00, // Office       ~$60K
    11.40, // Professional ~$90K
    11.60, // Manager      ~$110K
    11.20, // Technical    ~$73K
    10.70, // Service      ~$44K
];

// Age-earnings profile: beta1*(age-40) + beta2*(age-40)^2
// Peak around age 50, concave decline after
const AGE_EARN_BETA1: f64 = 0.02;
const AGE_EARN_BETA2: f64 = -0.0004;

// Female earnings gap (log points)
const FEMALE_PENALTY: f64 = -0.15;

// Age-conditional employment rates: [employed, unemployed, NILF]
// 15 age bands (15-19, 20-24, ..., 80-84, 85+)
// Same structure as census_2021.rs AGE_EMPLOYMENT
const AGE_EMPLOYMENT: [[f64; 3]; 15] = [
    [40.0, 15.0, 45.0], // 15-19
    [72.0, 8.0, 20.0],  // 20-24
    [80.0, 5.0, 15.0],  // 25-29
    [80.0, 4.0, 16.0],  // 30-34
    [80.0, 4.0, 16.0],  // 35-39
    [82.0, 3.0, 15.0],  // 40-44
    [82.0, 3.0, 15.0],  // 45-49
    [78.0, 3.0, 19.0],  // 50-54
    [72.0, 3.0, 25.0],  // 55-59
    [55.0, 3.0, 42.0],  // 60-64
    [30.0, 2.0, 68.0],  // 65-69
    [15.0, 1.0, 84.0],  // 70-74
    [5.0, 0.5, 94.5],   // 75-79
    [2.0, 0.2, 97.8],   // 80-84
    [1.0, 0.1, 98.9],   // 85+
];

fn age_to_employment_band(age: i32) -> usize {
    if age < 15 {
        return 0; // shouldn't be called for <15
    }
    let band = ((age - 15) / 5) as usize;
    band.min(14) // cap at 85+ band
}

pub fn assign(person: &mut Person, rng: &mut StdRng) {
    if person.education == 0 {
        return; // children — all defaults (false, 0, 0.0)
    }

    let age = 2021 - person.birth_year;
    let band = age_to_employment_band(age);
    let emp_weights = &AGE_EMPLOYMENT[band];
    let emp_idx = weighted_sample(rng, emp_weights);
    person.baseline_employed = emp_idx == 0;

    // Individual fixed effect and match quality (for all working-age, even if
    // not currently employed — these are latent person characteristics)
    person.individual_fe = normal_sample(rng, 0.0, 0.40) as f32;
    person.occ_match_quality = normal_sample(rng, 0.0, 0.10) as f32;

    // Hours (employed only)
    if person.baseline_employed {
        // 65% full-time (38h), 35% part-time (~20h)
        if rng.gen::<f64>() < 0.65 {
            person.baseline_hours = 38;
        } else {
            person.baseline_hours = 20;
        }
    }

    // Baseline income (actual dollars, 2021 anchor)
    // Only for employed persons; 0 otherwise.
    if person.baseline_employed {
        let age = 2021 - person.birth_year;
        let age_dev = (age - 40) as f64;
        let mu = MU_OCC[person.archetype as usize]
            + AGE_EARN_BETA1 * age_dev
            + AGE_EARN_BETA2 * age_dev * age_dev
            + if person.sex == 2 { FEMALE_PENALTY } else { 0.0 }
            + person.individual_fe as f64
            + person.occ_match_quality as f64;
        let hours_scale = person.baseline_hours as f64 / 38.0;
        person.baseline_income = (mu.exp() * hours_scale) as f32;
    }
}

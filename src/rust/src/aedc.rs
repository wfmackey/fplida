use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::sampling::normal_sample;

// AEDC: children in first year of school (~5 years old)
// ~300K children per cycle (triennial: 2009, 2012, 2015, 2018, 2021)

const NULL_I32: i32 = i32::MIN;

// Public AEDC documentation defines category percentile bands but does not
// publish the licensed Offord/McMaster item weights, score formula, or numeric
// 2009 score cut-offs. These z thresholds therefore calibrate a *synthetic*
// 2009 reference population to the public 10th/25th/50th percentile bands.
// They are fixed across cycles, as the public AEDC categories are.
const SYNTHETIC_P10_Z: f64 = -1.281_551_565_545;
const SYNTHETIC_P25_Z: f64 = -0.674_489_750_196;
const SYNTHETIC_P50_Z: f64 = 0.0;

// School types
const SCHOOL_TYPES: [&str; 3] = ["G", "C", "I"];
const SCHOOL_WEIGHTS: [f64; 3] = [0.65, 0.20, 0.15];

// Remoteness
const REMOTE_CODES: [i32; 5] = [1, 2, 3, 4, 5];
const REMOTE_WEIGHTS: [f64; 5] = [0.65, 0.20, 0.10, 0.03, 0.02];

fn remoteness_name(code: i32) -> &'static str {
    match code {
        1 => "Major Cities of Australia",
        2 => "Inner Regional Australia",
        3 => "Outer Regional Australia",
        4 => "Remote Australia",
        5 => "Very Remote Australia",
        _ => "",
    }
}

fn collection_cycle(cycle_year: i32) -> i32 {
    match cycle_year {
        2009 => 1,
        2012 => 2,
        2015 => 3,
        2018 => 4,
        2021 => 5,
        2024 => 6,
        _ => NULL_I32,
    }
}

fn not_applicable_code(cycle_year: i32) -> i32 {
    // The 2019 dictionary uses 999. The 2022 and 2025 dictionaries use 99.
    if cycle_year <= 2018 {
        999
    } else {
        99
    }
}

const PHYSICAL_QUESTION_ITEMS: [&str; 15] = [
    "A2", "A3", "A3A", "A3B", "A4", "A4A", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", "A13",
];
const COMMUNICATION_ITEMS: [&str; 8] = ["B1", "B2", "B3", "B4", "B5", "B6", "B7", "C24"];
const LANGUAGE_COGNITIVE_ITEMS: [&str; 26] = [
    "B8", "B9", "B10", "B11", "B12", "B13", "B14", "B15", "B16", "B17", "B18", "B19", "B20", "B21",
    "B22", "B23", "B24", "B25", "B26", "B27", "B28", "B29", "B30", "B31", "B32", "B33",
];
const SOCIAL_QUESTION_ITEMS: [&str; 25] = [
    "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C12A", "C13",
    "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C25",
];
const EMOTIONAL_ITEMS: [&str; 26] = [
    "C26", "C27", "C28", "C29", "C30", "C31", "C32", "C33", "C34", "C35", "C36", "C37", "C38",
    "C39", "C40", "C41", "C42", "C43", "C44", "C45", "C46", "C47", "C48", "C49", "C50", "C51",
];
const BACKGROUND_ITEMS: [&str; 35] = [
    "A1",
    "A1A",
    "A1B",
    "A1C",
    "A1D",
    "B34",
    "B35",
    "B36",
    "B37",
    "B38",
    "B39",
    "B40",
    "CLASSTYPEA_1",
    "CLASSTYPEA_2",
    "CLASSTYPEB",
    "CLASSTYPEC",
    "CLASSTYPEC_2",
    "TMSCH",
    "CANASSESS",
    "ESL",
    "LANG",
    "CANCOM",
    "E2Y",
    "E2AY",
    "E2BY",
    "E3AY",
    "E3BY",
    "E3CY",
    "E3DY",
    "E3EY",
    "E3FY",
    "E4",
    "E5",
    "E6",
    "E7",
];

#[derive(Clone, Copy)]
struct DomainTraits {
    physical: f64,
    social: f64,
    emotional: f64,
    language_cognitive: f64,
    communication: f64,
}

fn indigenous_development_adjustment(indigenous: i32) -> f64 {
    if (2..=4).contains(&indigenous) {
        -0.35
    } else {
        0.0
    }
}

fn domain_traits(
    rng: &mut StdRng,
    age_months: i32,
    gender: i32,
    indigenous: i32,
    lbote: i32,
    seifa_decile: i32,
    remoteness: i32,
) -> DomainTraits {
    let age_adjustment = (age_months as f64 - 60.0) * 0.030;
    let socioeconomic_adjustment = (seifa_decile as f64 - 5.5) * 0.035;
    let indigenous_adjustment = indigenous_development_adjustment(indigenous);
    let lbote_adjustment = if lbote == 1 { -0.15 } else { 0.0 };
    let remote_adjustment = if remoteness >= 4 { -0.10 } else { 0.0 };
    let general_mean = age_adjustment
        + socioeconomic_adjustment
        + indigenous_adjustment
        + lbote_adjustment
        + remote_adjustment;
    let general = normal_sample(rng, general_mean, 0.72);

    // A shared general-development factor creates realistic positive
    // correlation, while the residuals retain distinct domain variation.
    DomainTraits {
        physical: 0.72 * general + normal_sample(rng, 0.0, 0.68),
        social: 0.72 * general + normal_sample(rng, if gender == 1 { -0.08 } else { 0.04 }, 0.68),
        emotional: 0.72 * general
            + normal_sample(rng, if gender == 1 { -0.16 } else { 0.08 }, 0.68),
        language_cognitive: 0.72 * general + normal_sample(rng, 0.0, 0.68),
        communication: 0.72 * general + normal_sample(rng, 0.0, 0.68),
    }
}

fn synthetic_domain_score(trait_value: f64) -> f64 {
    // This bounded latent-trait index is intentionally a synthetic proxy. It
    // is not the proprietary AEDC domain-score calculation.
    let score = (6.8 + 1.55 * trait_value).clamp(0.0, 10.0);
    (score * 10_000.0).round() / 10_000.0
}

fn percentile_category(trait_value: f64) -> i32 {
    if trait_value <= SYNTHETIC_P10_Z {
        1
    } else if trait_value <= SYNTHETIC_P25_Z {
        2
    } else if trait_value <= SYNTHETIC_P50_Z {
        3
    } else {
        4
    }
}

#[inline]
fn item_difficulty(index: usize) -> f64 {
    ((index % 7) as f64 - 3.0) * 0.14
}

fn binary_development_item(rng: &mut StdRng, trait_value: f64, difficulty: f64) -> i32 {
    if rng.gen::<f64>() < 0.018 {
        88
    } else if trait_value + normal_sample(rng, 0.0, 0.82) >= difficulty {
        2
    } else {
        1
    }
}

fn ordinal_development_item(rng: &mut StdRng, trait_value: f64, difficulty: f64) -> i32 {
    if rng.gen::<f64>() < 0.018 {
        return 88;
    }
    let response = trait_value + normal_sample(rng, 0.0, 0.78) - difficulty;
    if response < -0.62 {
        1
    } else if response < 0.42 {
        2
    } else {
        3
    }
}

fn questionnaire_names() -> Vec<&'static str> {
    PHYSICAL_QUESTION_ITEMS
        .iter()
        .chain(COMMUNICATION_ITEMS.iter())
        .chain(LANGUAGE_COGNITIVE_ITEMS.iter())
        .chain(SOCIAL_QUESTION_ITEMS.iter())
        .chain(EMOTIONAL_ITEMS.iter())
        .chain(BACKGROUND_ITEMS.iter())
        .copied()
        .collect()
}

fn completed_item(value: i32) -> bool {
    value != NULL_I32 && value != 88 && value != 99 && value != 999
}

fn non_parental_care_item(rng: &mut StdRng, attendance_probability: f64) -> i32 {
    let draw = rng.gen::<f64>();
    if draw < 0.015 {
        88
    } else if draw < 0.015 + attendance_probability * 0.16 {
        1 // Yes, full time
    } else if draw < 0.015 + attendance_probability * 0.88 {
        2 // Yes, part time
    } else if draw < 0.015 + attendance_probability {
        3 // Yes, unsure whether full or part time
    } else {
        4 // No
    }
}

fn generate_questionnaire_row(
    rng: &mut StdRng,
    cycle_year: i32,
    state_code: i32,
    seifa_decile: i32,
    traits: DomainTraits,
    tmsch: i32,
    esl: i32,
    lang: i32,
) -> Vec<i32> {
    let mut values = Vec::with_capacity(
        PHYSICAL_QUESTION_ITEMS.len()
            + COMMUNICATION_ITEMS.len()
            + LANGUAGE_COGNITIVE_ITEMS.len()
            + SOCIAL_QUESTION_ITEMS.len()
            + EMOTIONAL_ITEMS.len()
            + BACKGROUND_ITEMS.len(),
    );
    let skip_instrument = tmsch == 1;

    // Physical wellbeing. A3A/A3B and A4A are collected details but are not
    // additional items in the public 12-item physical-domain allocation.
    if skip_instrument {
        values.extend(std::iter::repeat(NULL_I32).take(PHYSICAL_QUESTION_ITEMS.len()));
    } else {
        let a2 = binary_development_item(rng, traits.physical, -0.45);
        let a3 = binary_development_item(rng, traits.physical, -0.30);
        let detail_available = cycle_year >= 2015 || state_code != 1;
        let detail_asked = detail_available && (cycle_year < 2015 || a3 == 1);
        let (mut a3a, mut a3b) = if detail_asked {
            (
                binary_development_item(rng, traits.physical, -0.15),
                binary_development_item(rng, traits.physical, -0.05),
            )
        } else {
            (NULL_I32, NULL_I32)
        };
        if detail_asked && a3 == 1 && a3a != 1 && a3b != 1 {
            if rng.gen::<bool>() {
                a3a = 1;
            } else {
                a3b = 1;
            }
        }
        let a4 = binary_development_item(rng, traits.physical, -0.55);
        let breakfast_draw = rng.gen::<f64>();
        let breakfast_probability = 0.08 + (11 - seifa_decile) as f64 * 0.012;
        let a4a = if breakfast_draw < 0.018 {
            88
        } else if breakfast_draw < 0.10 {
            not_applicable_code(cycle_year)
        } else if breakfast_draw < 0.10 + breakfast_probability {
            1
        } else {
            0
        };
        values.extend([a2, a3, a3a, a3b, a4, a4a]);
        for index in 0..3 {
            values.push(binary_development_item(
                rng,
                traits.physical,
                item_difficulty(index),
            ));
        }
        for index in 0..6 {
            values.push(ordinal_development_item(
                rng,
                traits.physical,
                item_difficulty(index + 3),
            ));
        }
    }

    // Communication/general knowledge: B1:B7 use the rating scale and C24
    // uses the frequency scale. Both store higher codes for better responses.
    if skip_instrument {
        values.extend(std::iter::repeat(NULL_I32).take(COMMUNICATION_ITEMS.len()));
    } else {
        for index in 0..7 {
            let mut response =
                ordinal_development_item(rng, traits.communication, item_difficulty(index));
            if index == 0 && rng.gen::<f64>() < 0.008 {
                response = not_applicable_code(cycle_year); // B1 permits Not applicable.
            }
            values.push(response);
        }
        values.push(ordinal_development_item(
            rng,
            traits.communication,
            item_difficulty(7),
        ));
    }

    if skip_instrument {
        values.extend(
            std::iter::repeat(NULL_I32).take(LANGUAGE_COGNITIVE_ITEMS.len()),
        );
    } else {
        for index in 0..LANGUAGE_COGNITIVE_ITEMS.len() {
            values.push(binary_development_item(
                rng,
                traits.language_cognitive,
                item_difficulty(index),
            ));
        }
    }

    if skip_instrument {
        values.extend(std::iter::repeat(NULL_I32).take(SOCIAL_QUESTION_ITEMS.len()));
    } else {
        let mut social_values = Vec::with_capacity(SOCIAL_QUESTION_ITEMS.len());
        for index in 0..12 {
            social_values.push(ordinal_development_item(
                rng,
                traits.social,
                item_difficulty(index),
            ));
        }
        let c12a_available = (cycle_year >= 2015 || state_code != 1) && social_values[11] == 1;
        social_values.push(if c12a_available {
            ordinal_development_item(rng, traits.social, item_difficulty(12))
        } else {
            NULL_I32
        });
        for index in 12..24 {
            social_values.push(ordinal_development_item(
                rng,
                traits.social,
                item_difficulty(index + 1),
            ));
        }
        values.extend(social_values);
    }

    if skip_instrument {
        values.extend(std::iter::repeat(NULL_I32).take(EMOTIONAL_ITEMS.len()));
    } else {
        for index in 0..EMOTIONAL_ITEMS.len() {
            // C34:C51 are negatively worded. Their official stored codes are
            // reversed, so a higher-development response remains code 3.
            values.push(ordinal_development_item(
                rng,
                traits.emotional,
                item_difficulty(index),
            ));
        }
    }

    // Background and transition-to-school fields. Structural skips remain
    // Arrow nulls; 88 and 99 retain their distinct official meanings.
    if skip_instrument {
        values.extend(std::iter::repeat(NULL_I32).take(12)); // A1:A1D, B34:B40
    } else {
        let absence_draw = rng.gen::<f64>()
            + (6 - seifa_decile.min(6)) as f64 * 0.012
            + (-traits.physical).max(0.0) * 0.025;
        let a1 = if absence_draw < 0.58 {
            1
        } else if absence_draw < 0.84 {
            2
        } else if absence_draw < 0.95 {
            3
        } else {
            4
        };
        let mut reasons = [0, 0, 0, 0];
        if a1 > 1 || rng.gen::<f64>() < 0.35 {
            reasons[1] = i32::from(rng.gen::<f64>() < 0.72); // illness/injury
            reasons[0] = i32::from(rng.gen::<f64>() < 0.12); // family/cultural
            reasons[2] = i32::from(rng.gen::<f64>() < 0.18); // other explained
            reasons[3] = i32::from(rng.gen::<f64>() < 0.10); // unexplained
            if reasons.iter().all(|&reason| reason == 0) {
                reasons[1] = 1;
            }
        }
        values.extend([a1, reasons[0], reasons[1], reasons[2], reasons[3]]);
        let talent_traits = [
            traits.language_cognitive,
            traits.language_cognitive,
            traits.social,
            traits.emotional,
            traits.physical,
            traits.language_cognitive,
            (traits.social + traits.language_cognitive) / 2.0,
        ];
        for (index, &trait_value) in talent_traits.iter().enumerate() {
            values.push(binary_development_item(
                rng,
                trait_value,
                1.05 + item_difficulty(index) * 0.25,
            ));
        }
    }

    let multi_year = i32::from(rng.gen::<f64>() < 0.14);
    let multi_year_type = if multi_year == 1 {
        let draw = rng.gen::<f64>();
        if draw < 0.64 {
            1
        } else if draw < 0.86 {
            2
        } else {
            3
        }
    } else {
        NULL_I32
    };
    let repeating = i32::from(rng.gen::<f64>() < 0.025);
    let dual_placement = i32::from(rng.gen::<f64>() < 0.035);
    let dual_placement_type = if dual_placement == 1 {
        let draw = rng.gen::<f64>();
        if draw < 0.66 {
            1
        } else if draw < 0.80 {
            2
        } else {
            3
        }
    } else {
        NULL_I32
    };
    let can_assess = match tmsch {
        1 => 0,
        2 => 1,
        _ => NULL_I32,
    };
    let can_com = if lang == 1 {
        let draw = rng.gen::<f64>();
        if draw < 0.025 {
            88
        } else if draw < 0.10 {
            0
        } else {
            1
        }
    } else {
        NULL_I32
    };
    values.extend([
        multi_year,
        multi_year_type,
        repeating,
        dual_placement,
        dual_placement_type,
        tmsch,
        can_assess,
        esl,
        lang,
        can_com,
    ]);

    if skip_instrument {
        values.extend(std::iter::repeat(NULL_I32).take(13));
    } else {
        let e2_draw = rng.gen::<f64>();
        let e2y = if e2_draw < 0.80 {
            1
        } else if e2_draw < 0.94 {
            2
        } else {
            88
        };
        let (e2ay, e2by) = if e2y == 1 {
            let hours_draw = rng.gen::<f64>();
            let setting_draw = rng.gen::<f64>();
            (
                if hours_draw < 0.20 {
                    1
                } else if hours_draw < 0.32 {
                    2
                } else {
                    3
                },
                if setting_draw < 0.65 {
                    1
                } else if setting_draw < 0.93 {
                    2
                } else {
                    3
                },
            )
        } else {
            (NULL_I32, NULL_I32)
        };
        values.extend([e2y, e2ay, e2by]);
        values.extend([
            non_parental_care_item(rng, 0.38),
            non_parental_care_item(rng, 0.10),
            non_parental_care_item(rng, 0.42),
            non_parental_care_item(rng, 0.13),
            non_parental_care_item(rng, 0.07),
            non_parental_care_item(rng, 0.11),
        ]);
        let playgroup_draw = rng.gen::<f64>();
        values.push(if playgroup_draw < 0.025 {
            88
        } else if playgroup_draw < 0.46 {
            1
        } else {
            0
        });
        values.push(ordinal_development_item(rng, traits.social, -0.20));
        values.push(ordinal_development_item(
            rng,
            (traits.social + traits.language_cognitive) / 2.0,
            -0.10,
        ));
        values.push(ordinal_development_item(
            rng,
            traits.language_cognitive,
            -0.08,
        ));
    }

    debug_assert_eq!(values.len(), questionnaire_names().len());
    values
}

/// Project AEDC (Australian Early Development Census) from spine.
///
/// Generates core module records for children aged ~5 in each cycle year.
/// Five domain scores (0-10), vulnerability flags, demographics, geography.
/// @export
#[extendr]
fn project_aedc__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    seed: i64,
    cycle_year: i32,
) -> List {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    // Children who are ~5 in the cycle year (born cycle_year - 6 to cycle_year - 4)
    let est_n = n / 20; // rough estimate

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_gender: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_age_months: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_state: Vec<String> = Vec::with_capacity(est_n);
    let mut out_indigenous: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_lbote: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_school_type: Vec<String> = Vec::with_capacity(est_n);
    let mut out_remoteness: Vec<String> = Vec::with_capacity(est_n);
    let mut out_remoteness_code: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_seifa_decile: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_cycle: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_year: Vec<i32> = Vec::with_capacity(est_n);
    // Five domain scores
    let mut out_phys: Vec<f64> = Vec::with_capacity(est_n);
    let mut out_soc: Vec<f64> = Vec::with_capacity(est_n);
    let mut out_emot: Vec<f64> = Vec::with_capacity(est_n);
    let mut out_langcog: Vec<f64> = Vec::with_capacity(est_n);
    let mut out_comgen: Vec<f64> = Vec::with_capacity(est_n);
    // Vulnerability categories
    let mut out_phys_cat: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_soc_cat: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_emot_cat: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_langcog_cat: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_comgen_cat: Vec<i32> = Vec::with_capacity(est_n);
    // Summary flags
    let mut out_dv1: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_dv2: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_special_needs: Vec<i32> = Vec::with_capacity(est_n);

    for i in 0..n {
        let by = birth_year[i];
        let age_at_cycle = cycle_year - by;

        // Children entering school: age 4-6 at cycle year
        if age_at_cycle < 4 || age_at_cycle > 6 {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();
        let gender = if sex[i] == 1 { 1 } else { 2 };
        let age_m = age_at_cycle * 12 + rng.gen_range(0..=11);
        let ind = if indigenous[i] >= 2 && indigenous[i] <= 4 {
            1
        } else {
            0
        };
        let lbote = if country_of_birth[i] != 0 || rng.gen::<f64>() < 0.20 {
            1
        } else {
            0
        };

        let st_idx = crate::sampling::weighted_sample(&mut rng, &SCHOOL_WEIGHTS);
        let school_type = SCHOOL_TYPES[st_idx];

        let rm_idx = crate::sampling::weighted_sample(&mut rng, &REMOTE_WEIGHTS);
        let remoteness = REMOTE_CODES[rm_idx];

        let seifa = rng.gen_range(1..=10);

        let traits = domain_traits(&mut rng, age_m, gender, ind, lbote, seifa, remoteness);
        let sn = if rng.gen::<f64>() < 0.08 { 1 } else { 0 };
        let phys_r = if sn == 0 {
            synthetic_domain_score(traits.physical)
        } else {
            f64::NAN
        };
        let soc_r = if sn == 0 {
            synthetic_domain_score(traits.social)
        } else {
            f64::NAN
        };
        let emot_r = if sn == 0 {
            synthetic_domain_score(traits.emotional)
        } else {
            f64::NAN
        };
        let langcog_r = if sn == 0 {
            synthetic_domain_score(traits.language_cognitive)
        } else {
            f64::NAN
        };
        let comgen_r = if sn == 0 {
            synthetic_domain_score(traits.communication)
        } else {
            f64::NAN
        };
        let phys_c = if sn == 0 {
            percentile_category(traits.physical)
        } else {
            NULL_I32
        };
        let soc_c = if sn == 0 {
            percentile_category(traits.social)
        } else {
            NULL_I32
        };
        let emot_c = if sn == 0 {
            percentile_category(traits.emotional)
        } else {
            NULL_I32
        };
        let langcog_c = if sn == 0 {
            percentile_category(traits.language_cognitive)
        } else {
            NULL_I32
        };
        let comgen_c = if sn == 0 {
            percentile_category(traits.communication)
        } else {
            NULL_I32
        };
        let n_vuln = [phys_c, soc_c, emot_c, langcog_c, comgen_c]
            .iter()
            .filter(|&&category| category == 1)
            .count() as i32;
        let dv1 = if sn == 0 {
            i32::from(n_vuln >= 1)
        } else {
            NULL_I32
        };
        let dv2 = if sn == 0 {
            i32::from(n_vuln >= 2)
        } else {
            NULL_I32
        };

        out_aeuid.push(person_aeuid);
        out_gender.push(gender);
        out_age_months.push(age_m);
        let state_abbr = crate::codeframes::state_abbr(state[i]);
        out_state.push(
            if state_abbr.is_empty() {
                "XXX"
            } else {
                state_abbr
            }
            .to_string(),
        );
        out_indigenous.push(ind);
        out_lbote.push(lbote);
        out_school_type.push(school_type.to_string());
        out_remoteness.push(remoteness_name(remoteness).to_string());
        out_remoteness_code.push(remoteness);
        out_seifa_decile.push(seifa);
        out_cycle.push(collection_cycle(cycle_year));
        out_year.push(cycle_year);
        out_phys.push(phys_r);
        out_soc.push(soc_r);
        out_emot.push(emot_r);
        out_langcog.push(langcog_r);
        out_comgen.push(comgen_r);
        out_phys_cat.push(phys_c);
        out_soc_cat.push(soc_c);
        out_emot_cat.push(emot_c);
        out_langcog_cat.push(langcog_c);
        out_comgen_cat.push(comgen_c);
        out_dv1.push(dv1);
        out_dv2.push(dv2);
        out_special_needs.push(sn);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        GENDER = out_gender,
        AGEINMONTHS = out_age_months,
        STATE = out_state,
        ATSI = out_indigenous,
        LBOTE = out_lbote,
        SCHOOLTYPE = out_school_type,
        REMOTENESS = out_remoteness,
        REMOTENESSCODE = out_remoteness_code,
        SEIFADECILE = out_seifa_decile,
        CYCLE = out_cycle,
        YEAR = out_year,
        PHYS = out_phys,
        SOC = out_soc,
        EMOT = out_emot,
        LANGCOG = out_langcog,
        COMGEN = out_comgen,
        PHYSCATEGORY = out_phys_cat,
        SOCCATEGORY = out_soc_cat,
        EMOTCATEGORY = out_emot_cat,
        LANGCOGCATEGORY = out_langcog_cat,
        COMGENCATEGORY = out_comgen_cat,
        DV1 = out_dv1,
        DV2 = out_dv2,
        SPECIALNEEDS = out_special_needs
    )
}

/// Project AEDC directly to parquet. Single file.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_aedc_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    seed: i64,
    cycle_year: i32,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let item_names = questionnaire_names();
    let mut out_items: Vec<Vec<i32>> = item_names.iter().map(|_| Vec::new()).collect();
    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_gender: Vec<i32> = Vec::new();
    let mut out_age_months: Vec<i32> = Vec::new();
    let mut out_age_cut: Vec<i32> = Vec::new();
    let mut out_age_group: Vec<i32> = Vec::new();
    let mut out_state: Vec<String> = Vec::new();
    let mut out_indigenous: Vec<i32> = Vec::new();
    let mut out_indigenous_type: Vec<i32> = Vec::new();
    let mut out_lbote: Vec<i32> = Vec::new();
    let mut out_school_type: Vec<String> = Vec::new();
    let mut out_remoteness: Vec<String> = Vec::new();
    let mut out_remoteness_code: Vec<i32> = Vec::new();
    let mut out_seifa_decile: Vec<i32> = Vec::new();
    let mut out_seifa_category: Vec<i32> = Vec::new();
    let mut out_cycle: Vec<i32> = Vec::new();
    let mut out_year: Vec<i32> = Vec::new();
    let mut out_phys: Vec<f64> = Vec::new();
    let mut out_soc: Vec<f64> = Vec::new();
    let mut out_emot: Vec<f64> = Vec::new();
    let mut out_langcog: Vec<f64> = Vec::new();
    let mut out_comgen: Vec<f64> = Vec::new();
    let mut out_phys_cat: Vec<i32> = Vec::new();
    let mut out_soc_cat: Vec<i32> = Vec::new();
    let mut out_emot_cat: Vec<i32> = Vec::new();
    let mut out_langcog_cat: Vec<i32> = Vec::new();
    let mut out_comgen_cat: Vec<i32> = Vec::new();
    let mut out_phys_valid: Vec<i32> = Vec::new();
    let mut out_soc_valid: Vec<i32> = Vec::new();
    let mut out_emot_valid: Vec<i32> = Vec::new();
    let mut out_langcog_valid: Vec<i32> = Vec::new();
    let mut out_comgen_valid: Vec<i32> = Vec::new();
    let mut out_valid_domains: Vec<i32> = Vec::new();
    let mut out_valid_instrument: Vec<i32> = Vec::new();
    let mut out_low_total: Vec<i32> = Vec::new();
    let mut out_high_total: Vec<i32> = Vec::new();
    let mut out_dv1: Vec<i32> = Vec::new();
    let mut out_dv2: Vec<i32> = Vec::new();
    let mut out_ot5: Vec<i32> = Vec::new();
    let mut out_special_needs: Vec<i32> = Vec::new();

    for i in 0..n {
        let by = birth_year[i];
        let age_at_cycle = cycle_year - by;
        if age_at_cycle < 4 || age_at_cycle > 6 {
            continue;
        }

        let gender = if sex[i] == 1 { 1 } else { 2 };
        let age_m = age_at_cycle * 12 + rng.gen_range(0..=11);
        let ind = if indigenous[i] >= 2 && indigenous[i] <= 4 {
            1
        } else {
            0
        };
        let indigenous_type = match indigenous[i] {
            2 => 1,
            3 => 2,
            4 => 3,
            _ => 4,
        };
        let foreign_born = country_of_birth[i] != 0;
        let lang_probability = if foreign_born { 0.66 } else { 0.13 };
        let lang = i32::from(rng.gen::<f64>() < lang_probability);
        let esl_probability = if foreign_born {
            0.54
        } else if lang == 1 {
            0.28
        } else {
            0.035
        };
        let esl = i32::from(rng.gen::<f64>() < esl_probability);
        let lbote = i32::from(esl == 1 || lang == 1);
        let st_idx = crate::sampling::weighted_sample(&mut rng, &SCHOOL_WEIGHTS);
        let school_type = SCHOOL_TYPES[st_idx];
        let rm_idx = crate::sampling::weighted_sample(&mut rng, &REMOTE_WEIGHTS);
        let remoteness = REMOTE_CODES[rm_idx];
        let seifa = rng.gen_range(1..=10);
        let tmsch_draw = rng.gen::<f64>();
        let tmsch = if tmsch_draw < 0.012 {
            1
        } else if tmsch_draw < 0.035 {
            2
        } else {
            0
        };
        let traits = domain_traits(&mut rng, age_m, gender, ind, lbote, seifa, remoteness);
        let sn_probability = 0.065 + if gender == 1 { 0.022 } else { 0.0 };
        let sn = i32::from(rng.gen::<f64>() < sn_probability);
        let item_row = generate_questionnaire_row(
            &mut rng, cycle_year, state[i], seifa, traits, tmsch, esl, lang,
        );

        // Public validity requires at least 75% completed allocated items:
        // 9/12 physical, 18/24 social, 20/26 emotional, 20/26 language,
        // and 6/8 communication. Supplementary A3A/A3B/C12A/A4A do not
        // contribute to those denominators.
        let physical_completed = [0usize, 1, 4]
            .iter()
            .filter(|&&index| completed_item(item_row[index]))
            .count()
            + item_row[6..15]
                .iter()
                .filter(|&&value| completed_item(value))
                .count();
        let communication_completed = item_row[15..23]
            .iter()
            .filter(|&&value| completed_item(value))
            .count();
        let language_completed = item_row[23..49]
            .iter()
            .filter(|&&value| completed_item(value))
            .count();
        let social_completed = item_row[49..74]
            .iter()
            .enumerate()
            .filter(|(index, value)| *index != 12 && completed_item(**value))
            .count();
        let emotional_completed = item_row[74..100]
            .iter()
            .filter(|&&value| completed_item(value))
            .count();
        let assessable = sn == 0 && tmsch != 1;
        let phys_valid = assessable && physical_completed >= 9;
        let soc_valid = assessable && social_completed >= 18;
        let emot_valid = assessable && emotional_completed >= 20;
        let langcog_valid = assessable && language_completed >= 20;
        let comgen_valid = assessable && communication_completed >= 6;
        let valid_flags = [
            phys_valid,
            soc_valid,
            emot_valid,
            langcog_valid,
            comgen_valid,
        ];
        let valid_domains = valid_flags.iter().filter(|&&valid| valid).count() as i32;

        let trait_values = [
            traits.physical,
            traits.social,
            traits.emotional,
            traits.language_cognitive,
            traits.communication,
        ];
        let mut categories = [NULL_I32; 5];
        let mut scores = [f64::NAN; 5];
        for index in 0..5 {
            if valid_flags[index] {
                categories[index] = percentile_category(trait_values[index]);
                scores[index] = synthetic_domain_score(trait_values[index]);
            }
        }
        // Avoid inventing the unpublished partial-domain denominator rules:
        // public summary fields are derived only when all five domains exist.
        let (low_total, high_total, dv1, dv2, ot5) = if valid_domains == 5 {
            let low = categories.iter().filter(|&&category| category == 1).count() as i32;
            let high = categories.iter().filter(|&&category| category == 4).count() as i32;
            (
                low,
                high,
                i32::from(low >= 1),
                i32::from(low >= 2),
                i32::from(categories.iter().all(|&category| category >= 3)),
            )
        } else {
            (NULL_I32, NULL_I32, NULL_I32, NULL_I32, NULL_I32)
        };

        out_aeuid.push(aeuid[i].to_string());
        out_gender.push(gender);
        out_age_months.push(age_m);
        out_age_cut.push(if age_at_cycle < 5 {
            0
        } else if age_at_cycle == 5 {
            1
        } else {
            2
        });
        out_age_group.push(age_at_cycle);
        let state_abbr = crate::codeframes::state_abbr(state[i]);
        out_state.push(
            if state_abbr.is_empty() {
                "XXX"
            } else {
                state_abbr
            }
            .to_string(),
        );
        out_indigenous.push(ind);
        out_indigenous_type.push(indigenous_type);
        out_lbote.push(lbote);
        out_school_type.push(school_type.to_string());
        out_remoteness.push(remoteness_name(remoteness).to_string());
        out_remoteness_code.push(remoteness);
        out_seifa_decile.push(seifa);
        out_seifa_category.push((seifa + 1) / 2);
        out_cycle.push(collection_cycle(cycle_year));
        out_year.push(cycle_year);
        out_phys.push(scores[0]);
        out_soc.push(scores[1]);
        out_emot.push(scores[2]);
        out_langcog.push(scores[3]);
        out_comgen.push(scores[4]);
        out_phys_cat.push(categories[0]);
        out_soc_cat.push(categories[1]);
        out_emot_cat.push(categories[2]);
        out_langcog_cat.push(categories[3]);
        out_comgen_cat.push(categories[4]);
        out_phys_valid.push(i32::from(phys_valid));
        out_soc_valid.push(i32::from(soc_valid));
        out_emot_valid.push(i32::from(emot_valid));
        out_langcog_valid.push(i32::from(langcog_valid));
        out_comgen_valid.push(i32::from(comgen_valid));
        out_valid_domains.push(valid_domains);
        out_valid_instrument.push(i32::from(valid_domains >= 4));
        out_low_total.push(low_total);
        out_high_total.push(high_total);
        out_dv1.push(dv1);
        out_dv2.push(dv2);
        out_ot5.push(ot5);
        out_special_needs.push(sn);
        for (column, value) in out_items.iter_mut().zip(item_row) {
            column.push(value);
        }
    }

    let total = out_aeuid.len() as i32;
    let mut cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "GENDER",
            col: Col::I32(out_gender),
        },
        NamedCol {
            name: "AGEINMONTHS",
            col: Col::I32(out_age_months),
        },
        NamedCol {
            name: "AGECUT",
            col: Col::I32(out_age_cut),
        },
        NamedCol {
            name: "AGEGROUP3TO7",
            col: Col::I32(out_age_group),
        },
        NamedCol {
            name: "STATE",
            col: Col::Str(out_state.clone()),
        },
        NamedCol {
            name: "SCHOOLSTATE",
            col: Col::Str(out_state),
        },
        NamedCol {
            name: "ATSI",
            col: Col::I32(out_indigenous),
        },
        NamedCol {
            name: "ATSITYPE",
            col: Col::I32(out_indigenous_type),
        },
        NamedCol {
            name: "LBOTE",
            col: Col::I32(out_lbote),
        },
        NamedCol {
            name: "SCHOOLTYPE",
            col: Col::Str(out_school_type),
        },
        NamedCol {
            name: "REMOTENESS",
            col: Col::Str(out_remoteness),
        },
        NamedCol {
            name: "REMOTENESSCODE",
            col: Col::I32(out_remoteness_code),
        },
        NamedCol {
            name: "SEIFADECILE",
            col: Col::I32(out_seifa_decile),
        },
        NamedCol {
            name: "SEIFACATEGORY",
            col: Col::I32(out_seifa_category),
        },
        NamedCol {
            name: "CYCLE",
            col: Col::I32Opt(out_cycle),
        },
        NamedCol {
            name: "YEAR",
            col: Col::I32(out_year),
        },
        NamedCol {
            name: "PHYS",
            col: Col::F64(out_phys),
        },
        NamedCol {
            name: "SOC",
            col: Col::F64(out_soc),
        },
        NamedCol {
            name: "EMOT",
            col: Col::F64(out_emot),
        },
        NamedCol {
            name: "LANGCOG",
            col: Col::F64(out_langcog),
        },
        NamedCol {
            name: "COMGEN",
            col: Col::F64(out_comgen),
        },
        NamedCol {
            name: "PHYSCATEGORY",
            col: Col::I32Opt(out_phys_cat),
        },
        NamedCol {
            name: "SOCCATEGORY",
            col: Col::I32Opt(out_soc_cat),
        },
        NamedCol {
            name: "EMOTCATEGORY",
            col: Col::I32Opt(out_emot_cat),
        },
        NamedCol {
            name: "LANGCOGCATEGORY",
            col: Col::I32Opt(out_langcog_cat),
        },
        NamedCol {
            name: "COMGENCATEGORY",
            col: Col::I32Opt(out_comgen_cat),
        },
        NamedCol {
            name: "PHYSVALID",
            col: Col::I32(out_phys_valid),
        },
        NamedCol {
            name: "SOCVALID",
            col: Col::I32(out_soc_valid),
        },
        NamedCol {
            name: "EMOTVALID",
            col: Col::I32(out_emot_valid),
        },
        NamedCol {
            name: "LANGCOGVALID",
            col: Col::I32(out_langcog_valid),
        },
        NamedCol {
            name: "COMGENVALID",
            col: Col::I32(out_comgen_valid),
        },
        NamedCol {
            name: "VALIDDOMAINS",
            col: Col::I32(out_valid_domains),
        },
        NamedCol {
            name: "VALIDINSTRUMENT",
            col: Col::I32(out_valid_instrument),
        },
        NamedCol {
            name: "LOWTOTAL",
            col: Col::I32Opt(out_low_total),
        },
        NamedCol {
            name: "HIGHTOTAL",
            col: Col::I32Opt(out_high_total),
        },
        NamedCol {
            name: "DV1",
            col: Col::I32Opt(out_dv1),
        },
        NamedCol {
            name: "DV2",
            col: Col::I32Opt(out_dv2),
        },
        NamedCol {
            name: "OT5",
            col: Col::I32Opt(out_ot5),
        },
        NamedCol {
            name: "SPECIALNEEDS",
            col: Col::I32(out_special_needs),
        },
    ];
    for (name, values) in item_names.into_iter().zip(out_items) {
        cols.push(NamedCol {
            name,
            col: Col::I32Opt(values),
        });
    }
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("aedc parquet write: {}", e));
    total
}

#[cfg(test)]
mod tests {
    use super::indigenous_development_adjustment;

    #[test]
    fn development_adjustment_uses_the_spine_indigenous_domain() {
        assert_eq!(indigenous_development_adjustment(1), 0.0);
        for code in 2..=4 {
            assert_eq!(indigenous_development_adjustment(code), -0.35);
        }
        assert_eq!(indigenous_development_adjustment(9), 0.0);
    }
}

extendr_module! {
    mod aedc;
    fn project_aedc__;
    fn project_aedc_to_parquet__;
}

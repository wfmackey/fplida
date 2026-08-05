use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::sampling::{normal_sample, weighted_sample};

// Age-specific annual mortality rates (working-age focused)
// <20, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+
const MORTALITY: [f64; 8] = [
    0.0003, 0.0005, 0.0007, 0.0012, 0.0030, 0.0070, 0.0170, 0.0550,
];

fn age_band(age: i32) -> usize {
    match age {
        a if a < 20 => 0,
        a if a < 30 => 1,
        a if a < 40 => 2,
        a if a < 50 => 3,
        a if a < 60 => 4,
        a if a < 70 => 5,
        a if a < 80 => 6,
        _ => 7,
    }
}

// Top 20 Australian underlying causes of death (ICD-10)
// Source: ABS 3303.0, Causes of Death Australia
const UCOD_CODES: [&str; 20] = [
    "I25", "F03", "C34", "I50", "J44", "I63", "C18", "C50", "E11", "G30", "C61", "I21", "C25",
    "J18", "I10", "N18", "C16", "K74", "X71", "W19",
];
// I25=Ischaemic HD, F03=Dementia, C34=Lung cancer, I50=Heart failure,
// J44=COPD, I63=Stroke, C18=Colorectal, C50=Breast, E11=T2DM, G30=Alzheimer's,
// C61=Prostate, I21=AMI, C25=Pancreas, J18=Pneumonia, I10=Hypertension,
// N18=CKD, C16=Stomach, K74=Liver, X71=Suicide, W19=Falls

const UCOD_WEIGHTS: [f64; 20] = [
    0.12, 0.10, 0.08, 0.06, 0.06, 0.05, 0.04, 0.04, 0.04, 0.04, 0.03, 0.03, 0.03, 0.03, 0.03, 0.02,
    0.02, 0.02, 0.02, 0.02,
];

// Additional entity codes (comorbidities) — common conditions
const ENTITY_CODES: [&str; 15] = [
    "I10", "E11", "E78", "J44", "N18", "I25", "I50", "F03", "E66", "G47", "I48", "K21", "M81",
    "J96", "R09",
];
// I10=Hypertension, E11=DM, E78=Dyslipidaemia, J44=COPD, N18=CKD, I25=IHD,
// I50=HF, F03=Dementia, E66=Obesity, G47=Sleep disorders, I48=AF,
// K21=GORD, M81=Osteoporosis, J96=Resp failure, R09=Resp symptoms

// Death-certificate multiple-cause axes: up to 20 entity-axis codes (ENTITY1-20)
// and up to 20 record-axis condition codes (RACS1-20), matching the PLIDA
// cause_of_death product. RECORD_AXIS_COUNT is the number of populated RACS.
const ENTITY_COL_NAMES: [&str; 20] = [
    "ENTITY1", "ENTITY2", "ENTITY3", "ENTITY4", "ENTITY5", "ENTITY6", "ENTITY7", "ENTITY8",
    "ENTITY9", "ENTITY10", "ENTITY11", "ENTITY12", "ENTITY13", "ENTITY14", "ENTITY15", "ENTITY16",
    "ENTITY17", "ENTITY18", "ENTITY19", "ENTITY20",
];
const RACS_COL_NAMES: [&str; 20] = [
    "RACS1", "RACS2", "RACS3", "RACS4", "RACS5", "RACS6", "RACS7", "RACS8", "RACS9", "RACS10",
    "RACS11", "RACS12", "RACS13", "RACS14", "RACS15", "RACS16", "RACS17", "RACS18", "RACS19",
    "RACS20",
];

// Marital status: 1=never married, 2=married, 3=widowed, 4=divorced
const MARITAL_CODES: [i32; 4] = [1, 2, 3, 4];

// Place of death: 1=hospital, 2=home, 3=nursing home, 4=other
const PLACE_CODES: [i32; 4] = [1, 2, 3, 4];
const PLACE_WEIGHTS: [f64; 4] = [0.50, 0.25, 0.15, 0.10];

// Certifier: D=doctor, C=coroner
const CERTIFIER_CODES: [&str; 2] = ["D", "C"];
const CERTIFIER_WEIGHTS: [f64; 2] = [0.80, 0.20];

/// Select persons who die and generate death registration records.
///
/// For each spine person, we simulate mortality across years min_year..max_year.
/// Persons who die get a death registration with demographics, geography,
/// and ICD-10 cause of death codes.
/// @export
#[extendr]
fn project_deaths__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    seed: i64,
    min_year: i32,
    max_year: i32,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let n_years = (max_year - min_year + 1) as usize;

    // Estimate capacity: ~0.5% die per year over ~17 years ≈ ~8% of population
    let est_deaths = (n as f64 * 0.005 * n_years as f64) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_deaths);
    let mut out_death_year: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_death_month: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_death_day: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_death_age: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_sex: Vec<String> = Vec::with_capacity(est_deaths);
    let mut out_birth_place: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_marital_status: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_indigenous: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_reg_state: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_reference_year: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_certifier: Vec<String> = Vec::with_capacity(est_deaths);
    let mut out_place_of_death: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_remoteness: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_irsad: Vec<i32> = Vec::with_capacity(est_deaths);
    let mut out_irsd: Vec<i32> = Vec::with_capacity(est_deaths);
    // ICD-10 cause of death
    let mut out_ucod: Vec<String> = Vec::with_capacity(est_deaths);
    let mut out_entity_count: Vec<i32> = Vec::with_capacity(est_deaths);
    // Up to 5 entity codes per death (stored as pipe-separated string for simplicity)
    let mut out_entities: Vec<String> = Vec::with_capacity(est_deaths);

    for i in 0..n {
        let by = birth_year[i];
        let mut dead = false;

        for yr in min_year..=max_year {
            if dead {
                break;
            }
            let age = yr - by;
            if age < 0 {
                continue;
            }

            let band = age_band(age);
            let p_death = MORTALITY[band];

            // Males have ~1.3x mortality
            let p_adj = if sex[i] == 1 { p_death * 1.3 } else { p_death };

            if rng.gen::<f64>() < p_adj {
                dead = true;

                let person_aeuid = aeuid[i].to_string();
                let d_month = rng.gen_range(1..=12i32);
                let d_day = rng.gen_range(1..=28i32);

                let sex_str = if sex[i] == 1 { "M" } else { "F" };

                // Marital status: age-dependent
                let marital = if age < 25 {
                    1 // never married
                } else if age < 60 {
                    if rng.gen::<f64>() < 0.60 {
                        2
                    } else if rng.gen::<f64>() < 0.50 {
                        4
                    } else {
                        1
                    }
                } else {
                    if rng.gen::<f64>() < 0.40 {
                        2
                    } else if rng.gen::<f64>() < 0.50 {
                        3
                    } else {
                        4
                    }
                };

                let cert_idx = weighted_sample(&mut rng, &CERTIFIER_WEIGHTS);
                let certifier = CERTIFIER_CODES[cert_idx];

                let place_idx = weighted_sample(&mut rng, &PLACE_WEIGHTS);
                let place = PLACE_CODES[place_idx];

                // Remoteness
                let ra_draw: f64 = rng.gen();
                let ra = if ra_draw < 0.65 {
                    1
                } else if ra_draw < 0.85 {
                    2
                } else if ra_draw < 0.95 {
                    3
                } else if ra_draw < 0.98 {
                    4
                } else {
                    5
                };

                // UCOD: age-weighted selection
                let ucod_idx = weighted_sample(&mut rng, &UCOD_WEIGHTS);
                let ucod = UCOD_CODES[ucod_idx];

                // Entity codes: 1-5 additional conditions
                let n_entities = rng.gen_range(1..=5i32);
                let mut entities: Vec<String> = Vec::new();
                for _ in 0..n_entities {
                    let eidx = rng.gen_range(0..ENTITY_CODES.len());
                    entities.push(ENTITY_CODES[eidx].to_string());
                }
                let entities_str = entities.join("|");

                out_aeuid.push(person_aeuid);
                out_death_year.push(yr);
                out_death_month.push(d_month);
                out_death_day.push(d_day);
                out_death_age.push(age);
                out_sex.push(sex_str.to_string());
                out_birth_place.push(country_of_birth[i]);
                out_marital_status.push(marital);
                out_indigenous.push(indigenous[i]);
                out_reg_state.push(state[i]);
                out_reference_year.push(yr);
                out_certifier.push(certifier.to_string());
                out_place_of_death.push(place);
                out_remoteness.push(ra);
                out_irsad.push(rng.gen_range(1..=10));
                out_irsd.push(rng.gen_range(1..=10));
                out_ucod.push(ucod.to_string());
                out_entity_count.push(n_entities);
                out_entities.push(entities_str);
            }
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR_OF_DEATH = out_death_year,
        MONTH_OF_DEATH = out_death_month,
        DEATH_DAY = out_death_day,
        DEATH_AGE = out_death_age,
        SEX = out_sex,
        BIRTH_PLACE = out_birth_place,
        MARITAL_STATUS = out_marital_status,
        INDIGENOUS_STATUS = out_indigenous,
        REG_STATE = out_reg_state,
        REFERENCE_YEAR = out_reference_year,
        CERTIFIER = out_certifier,
        PLACE_OF_DEATH = out_place_of_death,
        REMOTENESS_AREA_2021 = out_remoteness,
        SEIFA_IRSAD_DEC_2021 = out_irsad,
        SEIFA_IRSD_DEC_2021 = out_irsd,
        UCOD = out_ucod,
        RECORD_AXIS_COUNT = out_entity_count,
        ENTITY_CODES = out_entities
    )
}

/// Project DEATHS directly to parquet, per-year files.
///
/// Writes one parquet per year in `min_year..=max_year` under the path:
///   {out_dir}/madipge-death-d-cause-of-death-{year}.parquet
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_deaths_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    seed: i64,
    min_year: i32,
    max_year: i32,
    out_dir: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let n_years = (max_year - min_year + 1) as usize;

    // Per-year buckets
    let mut bucket_aeuid: Vec<Vec<String>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_death_year: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_death_month: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_death_day: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_death_age: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_sex: Vec<Vec<String>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_birth_place: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_marital_status: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_indigenous: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_reg_state: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_reference_year: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_certifier: Vec<Vec<String>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_place_of_death: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_remoteness: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_irsad: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_irsd: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_ucod: Vec<Vec<String>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_entity_count: Vec<Vec<i32>> = (0..n_years).map(|_| Vec::new()).collect();
    // One entity-code list and one record-axis-code list per death; expanded to
    // ENTITY1-20 / RACS1-20 columns at write time.
    let mut bucket_entities: Vec<Vec<Vec<String>>> = (0..n_years).map(|_| Vec::new()).collect();
    let mut bucket_racs: Vec<Vec<Vec<String>>> = (0..n_years).map(|_| Vec::new()).collect();

    let mut total: i32 = 0;

    for i in 0..n {
        let yr = year_of_death[i];
        if yr == i32::MIN || yr < min_year || yr > max_year {
            continue;
        }
        let age = yr - birth_year[i];
        if age < 0 {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();
        let d_month = month_of_death[i].clamp(1, 12);
        let d_day = day_of_death[i].clamp(1, 28);
        let sex_str = if sex[i] == 1 { "M" } else { "F" };

        let marital = if age < 25 {
            1
        } else if age < 60 {
            if rng.gen::<f64>() < 0.60 {
                2
            } else if rng.gen::<f64>() < 0.50 {
                4
            } else {
                1
            }
        } else if rng.gen::<f64>() < 0.40 {
            2
        } else if rng.gen::<f64>() < 0.50 {
            3
        } else {
            4
        };

        let cert_idx = weighted_sample(&mut rng, &CERTIFIER_WEIGHTS);
        let certifier = CERTIFIER_CODES[cert_idx];

        let place_idx = weighted_sample(&mut rng, &PLACE_WEIGHTS);
        let place = PLACE_CODES[place_idx];

        let ra_draw: f64 = rng.gen();
        let ra = if ra_draw < 0.65 {
            1
        } else if ra_draw < 0.85 {
            2
        } else if ra_draw < 0.95 {
            3
        } else if ra_draw < 0.98 {
            4
        } else {
            5
        };

        let ucod_idx = weighted_sample(&mut rng, &UCOD_WEIGHTS);
        let ucod = UCOD_CODES[ucod_idx];

        let n_extra = rng.gen_range(0..=4i32);
        let mut entities: Vec<String> = vec![ucod.to_string()];
        for _ in 0..n_extra {
            let eidx = rng.gen_range(0..ENTITY_CODES.len());
            entities.push(ENTITY_CODES[eidx].to_string());
        }
        entities.truncate(20);
        let mut racs: Vec<String> = Vec::new();
        for e in &entities {
            if !racs.contains(e) {
                racs.push(e.clone());
            }
        }
        let racs_count = racs.len() as i32;

        let bi = (yr - min_year) as usize;
        bucket_aeuid[bi].push(person_aeuid);
        bucket_death_year[bi].push(yr);
        bucket_death_month[bi].push(d_month);
        bucket_death_day[bi].push(d_day);
        bucket_death_age[bi].push(age);
        bucket_sex[bi].push(sex_str.to_string());
        bucket_birth_place[bi].push(country_of_birth[i]);
        bucket_marital_status[bi].push(marital);
        bucket_indigenous[bi].push(indigenous[i]);
        bucket_reg_state[bi].push(state[i]);
        bucket_reference_year[bi].push(yr);
        bucket_certifier[bi].push(certifier.to_string());
        bucket_place_of_death[bi].push(place);
        bucket_remoteness[bi].push(ra);
        bucket_irsad[bi].push(rng.gen_range(1..=10));
        bucket_irsd[bi].push(rng.gen_range(1..=10));
        bucket_ucod[bi].push(ucod.to_string());
        bucket_entity_count[bi].push(racs_count);
        bucket_entities[bi].push(entities);
        bucket_racs[bi].push(racs);
        total += 1;
    }

    // Write one parquet per year
    for bi in 0..n_years {
        let yr = min_year + bi as i32;
        let mut cols = vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(std::mem::take(&mut bucket_aeuid[bi])),
            },
            NamedCol {
                name: "YEAR_OF_DEATH",
                col: Col::I32(std::mem::take(&mut bucket_death_year[bi])),
            },
            NamedCol {
                name: "MONTH_OF_DEATH",
                col: Col::I32(std::mem::take(&mut bucket_death_month[bi])),
            },
            NamedCol {
                name: "DEATH_DAY",
                col: Col::I32(std::mem::take(&mut bucket_death_day[bi])),
            },
            NamedCol {
                name: "DEATH_AGE",
                col: Col::I32(std::mem::take(&mut bucket_death_age[bi])),
            },
            NamedCol {
                name: "SEX",
                col: Col::Str(std::mem::take(&mut bucket_sex[bi])),
            },
            NamedCol {
                name: "BIRTH_PLACE",
                col: Col::I32(std::mem::take(&mut bucket_birth_place[bi])),
            },
            NamedCol {
                name: "MARITAL_STATUS",
                col: Col::I32(std::mem::take(&mut bucket_marital_status[bi])),
            },
            NamedCol {
                name: "INDIGENOUS_STATUS",
                col: Col::I32(std::mem::take(&mut bucket_indigenous[bi])),
            },
            NamedCol {
                name: "REG_STATE",
                col: Col::I32(std::mem::take(&mut bucket_reg_state[bi])),
            },
            NamedCol {
                name: "REFERENCE_YEAR",
                col: Col::I32(std::mem::take(&mut bucket_reference_year[bi])),
            },
            NamedCol {
                name: "CERTIFIER",
                col: Col::Str(std::mem::take(&mut bucket_certifier[bi])),
            },
            NamedCol {
                name: "PLACE_OF_DEATH",
                col: Col::I32(std::mem::take(&mut bucket_place_of_death[bi])),
            },
            NamedCol {
                name: "REMOTENESS_AREA_2021",
                col: Col::I32(std::mem::take(&mut bucket_remoteness[bi])),
            },
            NamedCol {
                name: "SEIFA_IRSAD_DEC_2021",
                col: Col::I32(std::mem::take(&mut bucket_irsad[bi])),
            },
            NamedCol {
                name: "SEIFA_IRSD_DEC_2021",
                col: Col::I32(std::mem::take(&mut bucket_irsd[bi])),
            },
            NamedCol {
                name: "UCOD",
                col: Col::Str(std::mem::take(&mut bucket_ucod[bi])),
            },
            NamedCol {
                name: "RECORD_AXIS_COUNT",
                col: Col::I32(std::mem::take(&mut bucket_entity_count[bi])),
            },
        ];
        // Expand ENTITY1-20 and RACS1-20 as separate nullable columns (None
        // where a death has fewer than k codes), matching the PLIDA layout.
        let ent_b = std::mem::take(&mut bucket_entities[bi]);
        let racs_b = std::mem::take(&mut bucket_racs[bi]);
        for k in 0..20usize {
            cols.push(NamedCol {
                name: ENTITY_COL_NAMES[k],
                col: Col::StrOpt(ent_b.iter().map(|e| e.get(k).cloned()).collect()),
            });
            cols.push(NamedCol {
                name: RACS_COL_NAMES[k],
                col: Col::StrOpt(racs_b.iter().map(|r| r.get(k).cloned()).collect()),
            });
        }
        let path = format!("{}/madipge-death-d-cause-of-death-{}.parquet", out_dir, yr);
        write_columns_to_parquet(&path, cols)
            .unwrap_or_else(|e| panic!("deaths parquet write year {}: {}", yr, e));
    }

    total
}

extendr_module! {
    mod deaths;
    fn project_deaths__;
    fn project_deaths_to_parquet__;
}

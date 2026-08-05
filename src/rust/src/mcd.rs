use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::mbs::days_since_epoch;

// Medicare participation: ~97% of population
const PART_RATE: f64 = 0.97;

// Entitlement types and weights
const ENT_TYPES: [&str; 3] = ["M", "DVA", "HCCA"];
const ENT_WEIGHTS: [f64; 3] = [0.95, 0.04, 0.01];

/// Project Medicare Consumer Directory from spine.
///
/// Produces demographics, address, and entitlement records for
/// persons enrolled in Medicare.
/// @export
#[extendr]
fn project_mcd__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    seed: i64,
) -> List {
    let n = birth_year.len();
    assert_eq!(year_of_arrival.len(), n);
    assert_eq!(year_of_death.len(), n);
    assert_eq!(month_of_death.len(), n);
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let est_n = (n as f64 * PART_RATE) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_year_of_birth: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_month_of_birth: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_sex: Vec<String> = Vec::with_capacity(est_n);
    let mut out_year_of_death: Vec<Rint> = Vec::with_capacity(est_n);
    let mut out_month_of_death: Vec<Rint> = Vec::with_capacity(est_n);
    let mut out_cnsmr_sts: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_state: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_adr_typ: Vec<String> = Vec::with_capacity(est_n);
    let mut out_ent_type: Vec<String> = Vec::with_capacity(est_n);

    for i in 0..n {
        if rng.gen::<f64>() >= PART_RATE {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let sx = sex[i];
        let st = state[i];
        let cob = country_of_birth[i];

        let mob = rng.gen_range(1..=12);
        let sex_str = if sx == 1 { "M" } else { "F" };

        // Consumer start: birth year for Australian-born people and the
        // spine arrival year for overseas-born people.
        let start_year = if cob == 0 {
            by
        } else if year_of_arrival[i] != i32::MIN {
            year_of_arrival[i]
        } else {
            by + 18
        };
        let consumer_start_year = start_year.max(2000);
        if year_of_death[i] != i32::MIN && consumer_start_year > year_of_death[i] {
            continue;
        }
        let cnsmr_sts = days_since_epoch(consumer_start_year, 1, 1);

        // Entitlement type
        let ent_idx = crate::sampling::weighted_sample(&mut rng, &ENT_WEIGHTS);
        let ent = ENT_TYPES[ent_idx];

        out_aeuid.push(person_aeuid);
        out_year_of_birth.push(by);
        out_month_of_birth.push(mob);
        out_sex.push(sex_str.to_string());
        out_year_of_death.push(if year_of_death[i] == i32::MIN {
            Rint::na()
        } else {
            Rint::from(year_of_death[i])
        });
        out_month_of_death.push(if month_of_death[i] == i32::MIN {
            Rint::na()
        } else {
            Rint::from(month_of_death[i])
        });
        out_cnsmr_sts.push(cnsmr_sts);
        out_state.push(st);
        out_adr_typ.push("R".to_string());
        out_ent_type.push(ent.to_string());
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR_OF_BIRTH = out_year_of_birth,
        MONTH_OF_BIRTH = out_month_of_birth,
        SEX = out_sex,
        YEAR_OF_DEATH = out_year_of_death,
        MONTH_OF_DEATH = out_month_of_death,
        CNSMR_STS = out_cnsmr_sts,
        STATE_ASGS_2021 = out_state,
        ADR_TYP = out_adr_typ,
        CNPGM_ETM_CDE = out_ent_type
    )
}

/// Project MCD directly to parquet. Single file.
/// @export
#[extendr]
fn project_mcd_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = birth_year.len();
    assert_eq!(year_of_arrival.len(), n);
    assert_eq!(year_of_death.len(), n);
    assert_eq!(month_of_death.len(), n);
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_year_of_birth: Vec<i32> = Vec::new();
    let mut out_month_of_birth: Vec<i32> = Vec::new();
    let mut out_sex: Vec<String> = Vec::new();
    let mut out_year_of_death: Vec<i32> = Vec::new();
    let mut out_month_of_death: Vec<i32> = Vec::new();
    let mut out_cnsmr_sts: Vec<i32> = Vec::new();
    let mut out_state: Vec<i32> = Vec::new();
    let mut out_adr_typ: Vec<String> = Vec::new();
    let mut out_ent_type: Vec<String> = Vec::new();

    for i in 0..n {
        if rng.gen::<f64>() >= PART_RATE {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let sx = sex[i];
        let st = state[i];
        let cob = country_of_birth[i];

        let mob = rng.gen_range(1..=12);
        let sex_str = if sx == 1 { "M" } else { "F" };

        let start_year = if cob == 0 {
            by
        } else if year_of_arrival[i] != i32::MIN {
            year_of_arrival[i]
        } else {
            by + 18
        };
        let consumer_start_year = start_year.max(2000);
        if year_of_death[i] != i32::MIN && consumer_start_year > year_of_death[i] {
            continue;
        }
        let cnsmr_sts = days_since_epoch(consumer_start_year, 1, 1);

        let ent_idx = crate::sampling::weighted_sample(&mut rng, &ENT_WEIGHTS);
        let ent = ENT_TYPES[ent_idx];

        out_aeuid.push(person_aeuid);
        out_year_of_birth.push(by);
        out_month_of_birth.push(mob);
        out_sex.push(sex_str.to_string());
        out_year_of_death.push(year_of_death[i]);
        out_month_of_death.push(month_of_death[i]);
        out_cnsmr_sts.push(cnsmr_sts);
        out_state.push(st);
        out_adr_typ.push("R".to_string());
        out_ent_type.push(ent.to_string());
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "YEAR_OF_BIRTH",
            col: Col::I32(out_year_of_birth),
        },
        NamedCol {
            name: "MONTH_OF_BIRTH",
            col: Col::I32(out_month_of_birth),
        },
        NamedCol {
            name: "SEX",
            col: Col::Str(out_sex),
        },
        NamedCol {
            name: "YEAR_OF_DEATH",
            col: Col::I32Opt(out_year_of_death),
        },
        NamedCol {
            name: "MONTH_OF_DEATH",
            col: Col::I32Opt(out_month_of_death),
        },
        NamedCol {
            name: "CNSMR_STS",
            col: Col::DateNN(out_cnsmr_sts),
        },
        NamedCol {
            name: "STATE_ASGS_2021",
            col: Col::I32(out_state),
        },
        NamedCol {
            name: "ADR_TYP",
            col: Col::Str(out_adr_typ),
        },
        NamedCol {
            name: "CNPGM_ETM_CDE",
            col: Col::Str(out_ent_type),
        },
    ];
    write_columns_to_parquet(out_path, cols).unwrap_or_else(|e| panic!("mcd parquet write: {}", e));
    total
}

extendr_module! {
    mod mcd;
    fn project_mcd__;
    fn project_mcd_to_parquet__;
}

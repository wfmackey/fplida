use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::mbs::days_since_epoch;
use crate::sampling::weighted_sample;

// English fluency codes: 1=very well, 2=well, 3=not well, 4=not at all
const ENG_CODES: [&str; 4] = ["1", "2", "3", "4"];
const ENG_WEIGHTS: [f64; 4] = [0.40, 0.30, 0.20, 0.10];

// Permanent visa subclasses for SDB
const PERM_SUBCLASS: [i32; 12] = [189, 190, 186, 820, 801, 100, 200, 201, 202, 204, 866, 143];
const PERM_WEIGHTS: [f64; 12] = [
    0.15, 0.12, 0.10, 0.10, 0.05, 0.05, 0.08, 0.05, 0.08, 0.02, 0.05, 0.05,
];

/// Project Settlements Database from spine (permanent visa holders only).
///
/// SDB is a subset of overseas-born persons who hold permanent visas.
/// ~60% of overseas-born are permanent residents.
/// @export
#[extendr]
fn project_sdb__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    education: &[i32],
    anzsco_code: Strings,
    seed: i64,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    // ~60% of overseas-born are permanent
    let perm_rate = 0.60;
    let est_n = (n as f64 * perm_rate) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_yob: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_mob: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_sex: Vec<String> = Vec::with_capacity(est_n);
    let mut out_cob: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_citizenship: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_arrival_date: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_grant_date: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_visa_subclass: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_principal_flag: Vec<String> = Vec::with_capacity(est_n);
    let mut out_marital_status: Vec<String> = Vec::with_capacity(est_n);
    let mut out_eng_fluency: Vec<String> = Vec::with_capacity(est_n);
    let mut out_occupation: Vec<String> = Vec::with_capacity(est_n);
    let mut out_yrs_educ_pri: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_yrs_educ_uni: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_last_on_day: Vec<i32> = Vec::with_capacity(est_n);

    for i in 0..n {
        // Filter: only permanent visa holders
        if rng.gen::<f64>() >= perm_rate {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let sx = sex[i];
        let yoa = year_of_arrival[i];
        let edu = education[i];

        let mob = rng.gen_range(1..=12);
        let sex_str = if sx == 1 { "M" } else { "F" };

        // Grant on or after arrival: the SDB date contract requires
        // DATE_OF_GRANT >= ARRIVAL_DATE (onshore settlement transition), with
        // most grants following within ~2 years of arrival.
        let grant_lag = rng.gen_range(0..=730);
        let arrival_dt = days_since_epoch(
            yoa,
            rng.gen_range(1..=12) as u32,
            rng.gen_range(1..=28) as u32,
        );
        let grant_dt = arrival_dt + grant_lag;

        // Visa subclass
        let vs_idx = weighted_sample(&mut rng, &PERM_WEIGHTS);
        let subclass = PERM_SUBCLASS[vs_idx];

        let principal = if rng.gen::<f64>() < 0.55 { "P" } else { "S" };

        let age = 2024 - by;
        let marital = if age < 25 {
            "Single"
        } else if rng.gen::<f64>() < 0.60 {
            "Married"
        } else {
            "Single"
        };

        let eng_idx = weighted_sample(&mut rng, &ENG_WEIGHTS);
        let eng = ENG_CODES[eng_idx];

        let occ_r = &anzsco_code[i];
        let occ = if occ_r.is_na() {
            "0000".to_string()
        } else {
            occ_r.to_string()
        };

        // Years of education from spine education level
        let yrs_pri = match edu {
            1 | 2 => 10, // year 10 or below
            3 => 12,     // year 12
            _ => 12,     // post-school
        };
        let yrs_uni = match edu {
            4 => 2, // diploma
            5 => 3, // bachelor
            6 => 5, // postgrad
            _ => 0,
        };

        out_aeuid.push(person_aeuid);
        out_yob.push(by);
        out_mob.push(mob);
        out_sex.push(sex_str.to_string());
        out_cob.push(country_of_birth[i]);
        out_citizenship.push(country_of_birth[i]);
        out_arrival_date.push(arrival_dt);
        out_grant_date.push(grant_dt);
        out_visa_subclass.push(subclass);
        out_principal_flag.push(principal.to_string());
        out_marital_status.push(marital.to_string());
        out_eng_fluency.push(eng.to_string());
        out_occupation.push(occ);
        out_yrs_educ_pri.push(yrs_pri);
        out_yrs_educ_uni.push(yrs_uni);
        out_last_on_day.push(1);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR_OF_BIRTH = out_yob,
        MONTH_OF_BIRTH = out_mob,
        SEX = out_sex,
        COUNTRY_OF_BIRTH = out_cob,
        CITIZENSHIP = out_citizenship,
        ARRIVAL_DATE = out_arrival_date,
        DATE_OF_GRANT = out_grant_date,
        VISA_SUB_CLASS = out_visa_subclass,
        PRINCIPAL_FLAG = out_principal_flag,
        MARITAL_STATUS = out_marital_status,
        ENG_FLUENCY_SPUK = out_eng_fluency,
        OCCUPATION = out_occupation,
        YRS_EDUC_PRI_HIGH = out_yrs_educ_pri,
        YRS_EDUC_UNIV = out_yrs_educ_uni,
        LAST_ON_DAY = out_last_on_day
    )
}

/// Project SDB directly to parquet. Single file.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_sdb_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    education: &[i32],
    anzsco_code: Strings,
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let _ = state;

    let perm_rate = 0.60;

    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_yob: Vec<i32> = Vec::new();
    let mut out_mob: Vec<i32> = Vec::new();
    let mut out_sex: Vec<String> = Vec::new();
    let mut out_cob: Vec<i32> = Vec::new();
    let mut out_citizenship: Vec<i32> = Vec::new();
    let mut out_arrival_date: Vec<i32> = Vec::new();
    let mut out_grant_date: Vec<i32> = Vec::new();
    let mut out_visa_subclass: Vec<i32> = Vec::new();
    let mut out_principal_flag: Vec<String> = Vec::new();
    let mut out_marital_status: Vec<String> = Vec::new();
    let mut out_eng_fluency: Vec<String> = Vec::new();
    let mut out_occupation: Vec<String> = Vec::new();
    let mut out_yrs_educ_pri: Vec<i32> = Vec::new();
    let mut out_yrs_educ_uni: Vec<i32> = Vec::new();
    let mut out_last_on_day: Vec<i32> = Vec::new();

    for i in 0..n {
        if rng.gen::<f64>() >= perm_rate {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let sx = sex[i];
        let yoa = year_of_arrival[i];
        let edu = education[i];

        let mob = rng.gen_range(1..=12);
        let sex_str = if sx == 1 { "M" } else { "F" };

        let grant_lag = rng.gen_range(0..=730);
        let arrival_dt = days_since_epoch(
            yoa,
            rng.gen_range(1..=12) as u32,
            rng.gen_range(1..=28) as u32,
        );
        // Use the same onshore-settlement date contract as the in-memory
        // projector: the permanent grant does not precede arrival.
        let grant_dt = arrival_dt + grant_lag;

        let vs_idx = weighted_sample(&mut rng, &PERM_WEIGHTS);
        let subclass = PERM_SUBCLASS[vs_idx];

        let principal = if rng.gen::<f64>() < 0.55 { "P" } else { "S" };

        let age = 2024 - by;
        let marital = if age < 25 {
            "Single"
        } else if rng.gen::<f64>() < 0.60 {
            "Married"
        } else {
            "Single"
        };

        let eng_idx = weighted_sample(&mut rng, &ENG_WEIGHTS);
        let eng = ENG_CODES[eng_idx];

        let occ_r = &anzsco_code[i];
        let occ = if occ_r.is_na() {
            "0000".to_string()
        } else {
            occ_r.to_string()
        };

        let yrs_pri = match edu {
            1 | 2 => 10,
            3 => 12,
            _ => 12,
        };
        let yrs_uni = match edu {
            4 => 2,
            5 => 3,
            6 => 5,
            _ => 0,
        };

        out_aeuid.push(person_aeuid);
        out_yob.push(by);
        out_mob.push(mob);
        out_sex.push(sex_str.to_string());
        out_cob.push(country_of_birth[i]);
        out_citizenship.push(country_of_birth[i]);
        out_arrival_date.push(arrival_dt);
        out_grant_date.push(grant_dt);
        out_visa_subclass.push(subclass);
        out_principal_flag.push(principal.to_string());
        out_marital_status.push(marital.to_string());
        out_eng_fluency.push(eng.to_string());
        out_occupation.push(occ);
        out_yrs_educ_pri.push(yrs_pri);
        out_yrs_educ_uni.push(yrs_uni);
        out_last_on_day.push(1);
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "YEAR_OF_BIRTH",
            col: Col::I32(out_yob),
        },
        NamedCol {
            name: "MONTH_OF_BIRTH",
            col: Col::I32(out_mob),
        },
        NamedCol {
            name: "SEX",
            col: Col::Str(out_sex),
        },
        NamedCol {
            name: "COUNTRY_OF_BIRTH",
            col: Col::I32(out_cob),
        },
        NamedCol {
            name: "CITIZENSHIP",
            col: Col::I32(out_citizenship),
        },
        NamedCol {
            name: "ARRIVAL_DATE",
            col: Col::DateNN(out_arrival_date),
        },
        NamedCol {
            name: "DATE_OF_GRANT",
            col: Col::DateNN(out_grant_date),
        },
        NamedCol {
            name: "VISA_SUB_CLASS",
            col: Col::I32(out_visa_subclass),
        },
        NamedCol {
            name: "PRINCIPAL_FLAG",
            col: Col::Str(out_principal_flag),
        },
        NamedCol {
            name: "MARITAL_STATUS",
            col: Col::Str(out_marital_status),
        },
        NamedCol {
            name: "ENG_FLUENCY_SPUK",
            col: Col::Str(out_eng_fluency),
        },
        NamedCol {
            name: "OCCUPATION",
            col: Col::Str(out_occupation),
        },
        NamedCol {
            name: "YRS_EDUC_PRI_HIGH",
            col: Col::I32(out_yrs_educ_pri),
        },
        NamedCol {
            name: "YRS_EDUC_UNIV",
            col: Col::I32(out_yrs_educ_uni),
        },
        NamedCol {
            name: "LAST_ON_DAY",
            col: Col::I32(out_last_on_day),
        },
    ];
    write_columns_to_parquet(out_path, cols).unwrap_or_else(|e| panic!("sdb parquet write: {}", e));
    total
}

extendr_module! {
    mod sdb;
    fn project_sdb__;
    fn project_sdb_to_parquet__;
}

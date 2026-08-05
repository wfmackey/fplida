use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::sampling::{normal_sample, weighted_sample};

// Indigenous codes (ABS Census): 1=Non-Indigenous, 2=Aboriginal, 3=TSI, 4=Both, 9=Not stated
// Plurality: 1 (singleton 97%), 2 (twin 2.8%), 3+ (0.2%)
const PLURALITY_VALS: [i32; 3] = [1, 2, 3];
const PLURALITY_WEIGHTS: [f64; 3] = [0.970, 0.028, 0.002];

// State codes 1-8
const STATE_LABELS: [&str; 8] = ["1", "2", "3", "4", "5", "6", "7", "8"];

// Common overseas parental birthplaces. Codes are country-level SACC 2016
// values and weights use the package's ABS Census-derived country frame.
const PARENT_OVERSEAS_BIRTHPLACE_CODES: [i32; 14] = [
    2102, 7103, 6101, 1201, 5204, 5105, 9225, 5203, 3104, 8104, 2304, 6102, 3207, 7106,
];
const PARENT_OVERSEAS_BIRTHPLACE_WEIGHTS: [f64; 14] = [
    927_490.0, 673_352.0, 549_618.0, 530_492.0, 293_892.0, 257_997.0, 189_207.0, 165_616.0,
    163_326.0, 101_309.0, 101_255.0, 100_148.0, 92_314.0, 89_633.0,
];

fn sample_parent_birthplace(rng: &mut StdRng) -> i32 {
    if rng.gen::<f64>() < 0.70 {
        1101
    } else {
        let index = weighted_sample(rng, &PARENT_OVERSEAS_BIRTHPLACE_WEIGHTS);
        PARENT_OVERSEAS_BIRTHPLACE_CODES[index]
    }
}

fn sample_parent_residence(rng: &mut StdRng, age_years: i32, birthplace: i32) -> (i32, i32) {
    let age_months = age_years.max(0).saturating_mul(12);
    let residence_months = if birthplace == 1101 {
        age_months
    } else {
        // Overseas-born parents typically arrived as children or young adults.
        // The clamp guarantees that Australian residence never exceeds age.
        let arrival_years = normal_sample(rng, 20.0, 10.0)
            .round()
            .max(0.0)
            .min(age_years.max(0) as f64) as i32;
        let arrival_months =
            (arrival_years.saturating_mul(12) + rng.gen_range(0..12)).min(age_months);
        age_months - arrival_months
    };
    (residence_months, residence_months / 12)
}

/// Project BIRTHS dataset from spine.
/// Only persons born in Australia (country_of_birth == 0 or 1101) appear.
/// @export
#[extendr]
fn project_births__(
    // Spine vectors (pre-filtered to Australian-born)
    aeuid: Strings,
    spine_id: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    seed: i64,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    // Output vectors
    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut out_birth_id: Vec<String> = Vec::with_capacity(n);
    let mut out_birth_year: Vec<i32> = Vec::with_capacity(n);
    let mut out_birth_month: Vec<i32> = Vec::with_capacity(n);
    let mut out_reg_year: Vec<i32> = Vec::with_capacity(n);
    let mut out_reg_state: Vec<i32> = Vec::with_capacity(n);
    let mut out_gender_child: Vec<String> = Vec::with_capacity(n);
    let mut out_birth_weight: Vec<i32> = Vec::with_capacity(n);
    let mut out_age_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_age_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_year_birth_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_month_birth_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_indigenous_child: Vec<i32> = Vec::with_capacity(n);
    let mut out_indigenous_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_indigenous_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_birth_place_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_birth_place_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_multi_birth: Vec<String> = Vec::with_capacity(n);
    let mut out_plurality: Vec<i32> = Vec::with_capacity(n);
    let mut out_first_time_mother: Vec<String> = Vec::with_capacity(n);
    let mut out_paternity: Vec<String> = Vec::with_capacity(n);
    let mut out_residence_months_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_residence_years_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_residence_months_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_residence_years_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_state_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_ra_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_irsad: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_irsd: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_ieo: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_ier: Vec<i32> = Vec::with_capacity(n);
    let mut out_person_flag: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        let sid = aeuid[i].to_string();
        let sp_id = spine_id[i].to_string();
        let by = birth_year[i];
        let sx = sex[i];
        let st = state[i];
        let ind = indigenous[i];

        // Birth ID: hash from spine_id
        let bid = format!(
            "B{:012X}",
            sp_id.as_bytes().iter().fold(0u64, |acc, &b| {
                acc.wrapping_mul(31).wrapping_add(b as u64)
            })
        );

        let bmonth = rng.gen_range(1..=12);

        // Registration: same year (95%) or next year (5%)
        let reg_year = if rng.gen::<f64>() < 0.95 { by } else { by + 1 };

        let gender = if sx == 1 { "M" } else { "F" };

        // Birth weight: Normal(3300, 500), clamp 500-5500
        let bw = normal_sample(&mut rng, 3300.0, 500.0)
            .round()
            .max(500.0)
            .min(5500.0) as i32;

        // Mother's age: Normal(30, 5), clamp 15-50
        let age_m = normal_sample(&mut rng, 30.0, 5.0)
            .round()
            .max(15.0)
            .min(50.0) as i32;
        // Father's age: mother + Normal(2, 3), clamp 16-65
        let age_f = (age_m as f64 + normal_sample(&mut rng, 2.0, 3.0))
            .round()
            .max(16.0)
            .min(65.0) as i32;

        let year_birth_m = by - age_m;
        let month_birth_m = rng.gen_range(1..=12);

        // Indigenous status for parents: mostly matches child
        let ind_mother = if rng.gen::<f64>() < 0.95 { ind } else { 1 }; // 5% discordant
        let ind_father = if rng.gen::<f64>() < 0.90 { ind } else { 1 }; // 10% discordant

        // Parent birthplaces use real SACC country codes.
        let bp_mother = sample_parent_birthplace(&mut rng);
        let bp_father = sample_parent_birthplace(&mut rng);

        // Plurality
        let p_idx = crate::sampling::weighted_sample(&mut rng, &PLURALITY_WEIGHTS);
        let plur = PLURALITY_VALS[p_idx];
        let multi = if plur > 1 { "Y" } else { "N" };

        // First-time mother: age-dependent
        let p_first = if age_m < 25 {
            0.60
        } else if age_m < 30 {
            0.45
        } else {
            0.30
        };
        let first_time = if rng.gen::<f64>() < p_first { "Y" } else { "N" };

        // Paternity acknowledged
        let paternity = if rng.gen::<f64>() < 0.95 { "1" } else { "0" };

        let (res_months_m, res_years_m) = sample_parent_residence(&mut rng, age_m, bp_mother);
        let (res_months_f, res_years_f) = sample_parent_residence(&mut rng, age_f, bp_father);

        // Remoteness: 1=Major Cities(65%), 2=Inner Regional(20%), 3=Outer Regional(10%),
        //             4=Remote(3%), 5=Very Remote(2%)
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

        // SEIFA deciles: uniform 1-10
        let irsad = rng.gen_range(1..=10);
        let irsd = rng.gen_range(1..=10);
        let ieo = rng.gen_range(1..=10);
        let ier = rng.gen_range(1..=10);

        out_aeuid.push(sid);
        out_birth_id.push(bid);
        out_birth_year.push(by);
        out_birth_month.push(bmonth);
        out_reg_year.push(reg_year);
        out_reg_state.push(st);
        out_gender_child.push(gender.to_string());
        out_birth_weight.push(bw);
        out_age_mother.push(age_m);
        out_age_father.push(age_f);
        out_year_birth_mother.push(year_birth_m);
        out_month_birth_mother.push(month_birth_m);
        out_indigenous_child.push(ind);
        out_indigenous_mother.push(ind_mother);
        out_indigenous_father.push(ind_father);
        out_birth_place_mother.push(bp_mother);
        out_birth_place_father.push(bp_father);
        out_multi_birth.push(multi.to_string());
        out_plurality.push(plur);
        out_first_time_mother.push(first_time.to_string());
        out_paternity.push(paternity.to_string());
        out_residence_months_mother.push(res_months_m);
        out_residence_years_mother.push(res_years_m);
        out_residence_months_father.push(res_months_f);
        out_residence_years_father.push(res_years_f);
        out_ures_state_mother.push(st);
        out_ures_ra_mother.push(ra);
        out_ures_irsad.push(irsad);
        out_ures_irsd.push(irsd);
        out_ures_ieo.push(ieo);
        out_ures_ier.push(ier);
        out_person_flag.push("Child".to_string());
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        BIRTH_ID = out_birth_id,
        BIRTH_YEAR = out_birth_year,
        BIRTH_MONTH = out_birth_month,
        REG_YEAR = out_reg_year,
        REG_STATE = out_reg_state,
        GENDER_CHILD = out_gender_child,
        BIRTH_WEIGHT = out_birth_weight,
        AGE_MOTHER = out_age_mother,
        AGE_FATHER = out_age_father,
        YEAR_BIRTH_MOTHER = out_year_birth_mother,
        MONTH_BIRTH_MOTHER = out_month_birth_mother,
        INDIGENOUS_CODE_CHILD = out_indigenous_child,
        INDIGENOUS_CODE_MOTHER = out_indigenous_mother,
        INDIGENOUS_CODE_FATHER = out_indigenous_father,
        BIRTH_PLACE_CODE_MOTHER = out_birth_place_mother,
        BIRTH_PLACE_CODE_FATHER = out_birth_place_father,
        MULTI_BIRTH_FLAG = out_multi_birth,
        PLURALITY = out_plurality,
        FIRST_TIME_MOTHER = out_first_time_mother,
        PATERNITY = out_paternity,
        RESIDENCE_MONTHS_MOTHER = out_residence_months_mother,
        RESIDENCE_YEARS_MOTHER = out_residence_years_mother,
        RESIDENCE_MONTHS_FATHER = out_residence_months_father,
        RESIDENCE_YEARS_FATHER = out_residence_years_father,
        URES_STATE_MOTHER = out_ures_state_mother,
        URES_RA_MOTHER = out_ures_ra_mother,
        URES_IRSAD_DECILE_MOTHER = out_ures_irsad,
        URES_IRSD_DECILE_MOTHER = out_ures_irsd,
        URES_IEO_DECILE_MOTHER = out_ures_ieo,
        URES_IER_DECILE_MOTHER = out_ures_ier,
        PERSON_FLAG = out_person_flag
    )
}

/// Project BIRTHS directly to parquet. Single file.
/// Persons must be pre-filtered to Australian-born.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_births_to_parquet__(
    aeuid: Strings,
    spine_id: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let _ = country_of_birth;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut out_birth_id: Vec<String> = Vec::with_capacity(n);
    let mut out_birth_year: Vec<i32> = Vec::with_capacity(n);
    let mut out_birth_month: Vec<i32> = Vec::with_capacity(n);
    let mut out_reg_year: Vec<i32> = Vec::with_capacity(n);
    let mut out_reg_state: Vec<i32> = Vec::with_capacity(n);
    let mut out_gender_child: Vec<String> = Vec::with_capacity(n);
    let mut out_birth_weight: Vec<i32> = Vec::with_capacity(n);
    let mut out_age_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_age_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_year_birth_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_month_birth_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_indigenous_child: Vec<i32> = Vec::with_capacity(n);
    let mut out_indigenous_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_indigenous_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_birth_place_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_birth_place_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_multi_birth: Vec<String> = Vec::with_capacity(n);
    let mut out_plurality: Vec<i32> = Vec::with_capacity(n);
    let mut out_first_time_mother: Vec<String> = Vec::with_capacity(n);
    let mut out_paternity: Vec<String> = Vec::with_capacity(n);
    let mut out_residence_months_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_residence_years_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_residence_months_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_residence_years_father: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_state_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_ra_mother: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_irsad: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_irsd: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_ieo: Vec<i32> = Vec::with_capacity(n);
    let mut out_ures_ier: Vec<i32> = Vec::with_capacity(n);
    let mut out_person_flag: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        let sid = aeuid[i].to_string();
        let sp_id = spine_id[i].to_string();
        let by = birth_year[i];
        let sx = sex[i];
        let st = state[i];
        let ind = indigenous[i];

        let bid = format!(
            "B{:012X}",
            sp_id.as_bytes().iter().fold(0u64, |acc, &b| {
                acc.wrapping_mul(31).wrapping_add(b as u64)
            })
        );

        let bmonth = rng.gen_range(1..=12);
        let reg_year = if rng.gen::<f64>() < 0.95 { by } else { by + 1 };
        let gender = if sx == 1 { "M" } else { "F" };

        let bw = normal_sample(&mut rng, 3300.0, 500.0)
            .round()
            .max(500.0)
            .min(5500.0) as i32;
        let age_m = normal_sample(&mut rng, 30.0, 5.0)
            .round()
            .max(15.0)
            .min(50.0) as i32;
        let age_f = (age_m as f64 + normal_sample(&mut rng, 2.0, 3.0))
            .round()
            .max(16.0)
            .min(65.0) as i32;

        let year_birth_m = by - age_m;
        let month_birth_m = rng.gen_range(1..=12);

        let ind_mother = if rng.gen::<f64>() < 0.95 { ind } else { 1 };
        let ind_father = if rng.gen::<f64>() < 0.90 { ind } else { 1 };

        let bp_mother = sample_parent_birthplace(&mut rng);
        let bp_father = sample_parent_birthplace(&mut rng);

        let p_idx = crate::sampling::weighted_sample(&mut rng, &PLURALITY_WEIGHTS);
        let plur = PLURALITY_VALS[p_idx];
        let multi = if plur > 1 { "Y" } else { "N" };

        let p_first = if age_m < 25 {
            0.60
        } else if age_m < 30 {
            0.45
        } else {
            0.30
        };
        let first_time = if rng.gen::<f64>() < p_first { "Y" } else { "N" };

        let paternity = if rng.gen::<f64>() < 0.95 { "1" } else { "0" };

        let (res_months_m, res_years_m) = sample_parent_residence(&mut rng, age_m, bp_mother);
        let (res_months_f, res_years_f) = sample_parent_residence(&mut rng, age_f, bp_father);

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

        let irsad = rng.gen_range(1..=10);
        let irsd = rng.gen_range(1..=10);
        let ieo = rng.gen_range(1..=10);
        let ier = rng.gen_range(1..=10);

        out_aeuid.push(sid);
        out_birth_id.push(bid);
        out_birth_year.push(by);
        out_birth_month.push(bmonth);
        out_reg_year.push(reg_year);
        out_reg_state.push(st);
        out_gender_child.push(gender.to_string());
        out_birth_weight.push(bw);
        out_age_mother.push(age_m);
        out_age_father.push(age_f);
        out_year_birth_mother.push(year_birth_m);
        out_month_birth_mother.push(month_birth_m);
        out_indigenous_child.push(ind);
        out_indigenous_mother.push(ind_mother);
        out_indigenous_father.push(ind_father);
        out_birth_place_mother.push(bp_mother);
        out_birth_place_father.push(bp_father);
        out_multi_birth.push(multi.to_string());
        out_plurality.push(plur);
        out_first_time_mother.push(first_time.to_string());
        out_paternity.push(paternity.to_string());
        out_residence_months_mother.push(res_months_m);
        out_residence_years_mother.push(res_years_m);
        out_residence_months_father.push(res_months_f);
        out_residence_years_father.push(res_years_f);
        out_ures_state_mother.push(st);
        out_ures_ra_mother.push(ra);
        out_ures_irsad.push(irsad);
        out_ures_irsd.push(irsd);
        out_ures_ieo.push(ieo);
        out_ures_ier.push(ier);
        out_person_flag.push("Child".to_string());
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "BIRTH_ID",
            col: Col::Str(out_birth_id),
        },
        NamedCol {
            name: "BIRTH_YEAR",
            col: Col::I32(out_birth_year),
        },
        NamedCol {
            name: "BIRTH_MONTH",
            col: Col::I32(out_birth_month),
        },
        NamedCol {
            name: "REG_YEAR",
            col: Col::I32(out_reg_year),
        },
        NamedCol {
            name: "REG_STATE",
            col: Col::I32(out_reg_state),
        },
        NamedCol {
            name: "GENDER_CHILD",
            col: Col::Str(out_gender_child),
        },
        NamedCol {
            name: "BIRTH_WEIGHT",
            col: Col::I32(out_birth_weight),
        },
        NamedCol {
            name: "AGE_MOTHER",
            col: Col::I32(out_age_mother),
        },
        NamedCol {
            name: "AGE_FATHER",
            col: Col::I32(out_age_father),
        },
        NamedCol {
            name: "YEAR_BIRTH_MOTHER",
            col: Col::I32(out_year_birth_mother),
        },
        NamedCol {
            name: "MONTH_BIRTH_MOTHER",
            col: Col::I32(out_month_birth_mother),
        },
        NamedCol {
            name: "INDIGENOUS_CODE_CHILD",
            col: Col::I32(out_indigenous_child),
        },
        NamedCol {
            name: "INDIGENOUS_CODE_MOTHER",
            col: Col::I32(out_indigenous_mother),
        },
        NamedCol {
            name: "INDIGENOUS_CODE_FATHER",
            col: Col::I32(out_indigenous_father),
        },
        NamedCol {
            name: "BIRTH_PLACE_CODE_MOTHER",
            col: Col::I32(out_birth_place_mother),
        },
        NamedCol {
            name: "BIRTH_PLACE_CODE_FATHER",
            col: Col::I32(out_birth_place_father),
        },
        NamedCol {
            name: "MULTI_BIRTH_FLAG",
            col: Col::Str(out_multi_birth),
        },
        NamedCol {
            name: "PLURALITY",
            col: Col::I32(out_plurality),
        },
        NamedCol {
            name: "FIRST_TIME_MOTHER",
            col: Col::Str(out_first_time_mother),
        },
        NamedCol {
            name: "PATERNITY",
            col: Col::Str(out_paternity),
        },
        NamedCol {
            name: "RESIDENCE_MONTHS_MOTHER",
            col: Col::I32(out_residence_months_mother),
        },
        NamedCol {
            name: "RESIDENCE_YEARS_MOTHER",
            col: Col::I32(out_residence_years_mother),
        },
        NamedCol {
            name: "RESIDENCE_MONTHS_FATHER",
            col: Col::I32(out_residence_months_father),
        },
        NamedCol {
            name: "RESIDENCE_YEARS_FATHER",
            col: Col::I32(out_residence_years_father),
        },
        NamedCol {
            name: "URES_STATE_MOTHER",
            col: Col::I32(out_ures_state_mother),
        },
        NamedCol {
            name: "URES_RA_MOTHER",
            col: Col::I32(out_ures_ra_mother),
        },
        NamedCol {
            name: "URES_IRSAD_DECILE_MOTHER",
            col: Col::I32(out_ures_irsad),
        },
        NamedCol {
            name: "URES_IRSD_DECILE_MOTHER",
            col: Col::I32(out_ures_irsd),
        },
        NamedCol {
            name: "URES_IEO_DECILE_MOTHER",
            col: Col::I32(out_ures_ieo),
        },
        NamedCol {
            name: "URES_IER_DECILE_MOTHER",
            col: Col::I32(out_ures_ier),
        },
        NamedCol {
            name: "PERSON_FLAG",
            col: Col::Str(out_person_flag),
        },
    ];
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("births parquet write: {}", e));
    total
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parent_birthplaces_use_real_sacc_values() {
        let mut rng = StdRng::seed_from_u64(1300);
        let values: Vec<i32> = (0..2_000)
            .map(|_| sample_parent_birthplace(&mut rng))
            .collect();
        assert!(values
            .iter()
            .all(|value| { *value == 1101 || PARENT_OVERSEAS_BIRTHPLACE_CODES.contains(value) }));
        assert!(values.iter().any(|value| *value != 1101));
        assert!(!values.contains(&0));
    }

    #[test]
    fn parent_residence_is_bounded_by_age_and_matches_years() {
        let mut rng = StdRng::seed_from_u64(1301);
        assert_eq!(sample_parent_residence(&mut rng, 30, 1101), (360, 30));
        for _ in 0..500 {
            let (months, years) = sample_parent_residence(&mut rng, 35, 7103);
            assert!((0..=420).contains(&months));
            assert_eq!(years, months / 12);
        }
    }
}

extendr_module! {
    mod births;
    fn project_births__;
    fn project_births_to_parquet__;
}

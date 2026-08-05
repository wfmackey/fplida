use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

/// Project Migrant and Traveller Demographics from spine (overseas-born only).
/// @export
#[extendr]
fn project_mt_demogs__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    seed: i64,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut out_dob_y: Vec<i32> = Vec::with_capacity(n);
    let mut out_dob_m: Vec<i32> = Vec::with_capacity(n);
    let mut out_sex: Vec<String> = Vec::with_capacity(n);
    let mut out_gender: Vec<String> = Vec::with_capacity(n);
    let mut out_birth_cntry: Vec<i32> = Vec::with_capacity(n);
    let mut out_marital_sts: Vec<String> = Vec::with_capacity(n);
    let mut out_adr_typ: Vec<String> = Vec::with_capacity(n);
    let mut out_state: Vec<i32> = Vec::with_capacity(n);
    let mut out_source: Vec<String> = Vec::with_capacity(n);
    let mut out_imputed_state: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let sx = sex[i];
        let yoa = year_of_arrival[i];

        let dob_m = rng.gen_range(1..=12);
        let sex_str = if sx == 1 { "M" } else { "F" };

        let age = 2024 - by;
        let marital = if age < 25 {
            "Single"
        } else if rng.gen::<f64>() < 0.55 {
            "Married"
        } else if rng.gen::<f64>() < 0.40 {
            "De facto"
        } else {
            "Single"
        };

        let source = if rng.gen::<f64>() < 0.80 {
            "VISA"
        } else {
            "SETTLE"
        };
        let imputed = if rng.gen::<f64>() < 0.95 { "N" } else { "Y" };

        out_aeuid.push(person_aeuid);
        out_dob_y.push(by);
        out_dob_m.push(dob_m);
        out_sex.push(sex_str.to_string());
        out_gender.push(sex_str.to_string());
        out_birth_cntry.push(country_of_birth[i]);
        out_marital_sts.push(marital.to_string());
        out_adr_typ.push(if rng.gen::<f64>() < 0.90 {
            "R".to_string()
        } else {
            "P".to_string()
        });
        out_state.push(state[i]);
        out_source.push(source.to_string());
        out_imputed_state.push(imputed.to_string());
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        DOB_Y = out_dob_y,
        DOB_M = out_dob_m,
        SEX = out_sex,
        GENDER = out_gender,
        BIRTH_CNTRY_CDE = out_birth_cntry,
        MARITAL_STS = out_marital_sts,
        ADR_TYP = out_adr_typ,
        STATE_ASGS_2021 = out_state,
        SOURCE = out_source,
        IMPUTED_STATE = out_imputed_state
    )
}

/// Project MT_DEMOGS directly to parquet. Single file.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_mt_demogs_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut out_dob_y: Vec<i32> = Vec::with_capacity(n);
    let mut out_dob_m: Vec<i32> = Vec::with_capacity(n);
    let mut out_sex: Vec<String> = Vec::with_capacity(n);
    let mut out_gender: Vec<String> = Vec::with_capacity(n);
    let mut out_birth_cntry: Vec<i32> = Vec::with_capacity(n);
    let mut out_marital_sts: Vec<String> = Vec::with_capacity(n);
    let mut out_adr_typ: Vec<String> = Vec::with_capacity(n);
    let mut out_state: Vec<i32> = Vec::with_capacity(n);
    let mut out_source: Vec<String> = Vec::with_capacity(n);
    let mut out_imputed_state: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let sx = sex[i];
        let _yoa = year_of_arrival[i];

        let dob_m = rng.gen_range(1..=12);
        let sex_str = if sx == 1 { "M" } else { "F" };

        let age = 2024 - by;
        let marital = if age < 25 {
            "Single"
        } else if rng.gen::<f64>() < 0.55 {
            "Married"
        } else if rng.gen::<f64>() < 0.40 {
            "De facto"
        } else {
            "Single"
        };

        let source = if rng.gen::<f64>() < 0.80 {
            "VISA"
        } else {
            "SETTLE"
        };
        let imputed = if rng.gen::<f64>() < 0.95 { "N" } else { "Y" };

        out_aeuid.push(person_aeuid);
        out_dob_y.push(by);
        out_dob_m.push(dob_m);
        out_sex.push(sex_str.to_string());
        out_gender.push(sex_str.to_string());
        out_birth_cntry.push(country_of_birth[i]);
        out_marital_sts.push(marital.to_string());
        out_adr_typ.push(if rng.gen::<f64>() < 0.90 {
            "R".to_string()
        } else {
            "P".to_string()
        });
        out_state.push(state[i]);
        out_source.push(source.to_string());
        out_imputed_state.push(imputed.to_string());
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "DOB_Y",
            col: Col::I32(out_dob_y),
        },
        NamedCol {
            name: "DOB_M",
            col: Col::I32(out_dob_m),
        },
        NamedCol {
            name: "SEX",
            col: Col::Str(out_sex),
        },
        NamedCol {
            name: "GENDER",
            col: Col::Str(out_gender),
        },
        NamedCol {
            name: "BIRTH_CNTRY_CDE",
            col: Col::I32(out_birth_cntry),
        },
        NamedCol {
            name: "MARITAL_STS",
            col: Col::Str(out_marital_sts),
        },
        NamedCol {
            name: "ADR_TYP",
            col: Col::Str(out_adr_typ),
        },
        NamedCol {
            name: "STATE_ASGS_2021",
            col: Col::I32(out_state),
        },
        NamedCol {
            name: "SOURCE",
            col: Col::Str(out_source),
        },
        NamedCol {
            name: "IMPUTED_STATE",
            col: Col::Str(out_imputed_state),
        },
    ];
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("mt_demogs parquet write: {}", e));
    total
}

extendr_module! {
    mod mt_demogs;
    fn project_mt_demogs__;
    fn project_mt_demogs_to_parquet__;
}

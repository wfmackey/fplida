use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::sampling::normal_sample;

// AMEP: ~30% of permanent migrants participate
const AMEP_RATE: f64 = 0.30;

// ASLPR English proficiency levels (0-5 scale)
// 0=Zero, 1=Elementary, 2=Survival, 3=Social, 4=Vocational, 5=Native-like

/// Project AMEP (Adult Migrant English Program) from spine.
///
/// Records for recent permanent migrants with low English proficiency.
/// Hours attended per year, English assessment scores.
/// @export
#[extendr]
fn project_amep__(
    aeuid: Strings,
    birth_year: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    seed: i64,
) -> List {
    let n = aeuid.len();
    assert_eq!(year_of_death.len(), n);
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let est_n = (n as f64 * AMEP_RATE) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_yob: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_hours_total: Vec<f64> = Vec::with_capacity(est_n);
    let mut out_aslpr_speak_init: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_aslpr_speak_latest: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_aslpr_listen_init: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_aslpr_listen_latest: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_aslpr_read_init: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_aslpr_read_latest: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_aslpr_write_init: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_aslpr_write_latest: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_enrol_year: Vec<i32> = Vec::with_capacity(est_n);

    for i in 0..n {
        // Only overseas-born, arrived within last 5 years
        let cob = country_of_birth[i];
        let yoa = year_of_arrival[i];
        if cob == 0 {
            continue;
        }
        if yoa < 2003 || yoa > 2019 {
            continue;
        }

        if rng.gen::<f64>() >= AMEP_RATE {
            continue;
        }
        let enrol_year = yoa + rng.gen_range(0..=1);
        if year_of_death[i] != i32::MIN && enrol_year > year_of_death[i] {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();

        // Hours attended: 100-510 (max entitlement 510 hours)
        let hours = normal_sample(&mut rng, 300.0, 120.0)
            .max(50.0)
            .min(510.0)
            .round();

        // ASLPR initial scores: 0-2 (low English)
        let speak_init = rng.gen_range(0..=2);
        let listen_init = rng.gen_range(0..=2);
        let read_init = rng.gen_range(0..=2);
        let write_init = rng.gen_range(0..=2);

        // Latest scores: improvement of 0-2 levels
        let speak_latest = (speak_init + rng.gen_range(0..=2)).min(5);
        let listen_latest = (listen_init + rng.gen_range(0..=2)).min(5);
        let read_latest = (read_init + rng.gen_range(0..=2)).min(5);
        let write_latest = (write_init + rng.gen_range(0..=2)).min(5);

        out_aeuid.push(person_aeuid);
        out_yob.push(birth_year[i]);
        out_hours_total.push(hours);
        out_aslpr_speak_init.push(speak_init);
        out_aslpr_speak_latest.push(speak_latest);
        out_aslpr_listen_init.push(listen_init);
        out_aslpr_listen_latest.push(listen_latest);
        out_aslpr_read_init.push(read_init);
        out_aslpr_read_latest.push(read_latest);
        out_aslpr_write_init.push(write_init);
        out_aslpr_write_latest.push(write_latest);
        out_enrol_year.push(enrol_year);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR_OF_BIRTH = out_yob,
        HOURS_TOTAL = out_hours_total,
        INITIAL_ASLPR_SPEAK = out_aslpr_speak_init,
        LATEST_ASLPR_SPEAK = out_aslpr_speak_latest,
        INITIAL_ASLPR_LISTEN = out_aslpr_listen_init,
        LATEST_ASLPR_LISTEN = out_aslpr_listen_latest,
        INITIAL_ASLPR_READ = out_aslpr_read_init,
        LATEST_ASLPR_READ = out_aslpr_read_latest,
        INITIAL_ASLPR_WRITE = out_aslpr_write_init,
        LATEST_ASLPR_WRITE = out_aslpr_write_latest,
        ENROLLED_FY = out_enrol_year
    )
}

/// Project AMEP directly to parquet. Single file.
/// @export
#[extendr]
fn project_amep_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = aeuid.len();
    assert_eq!(year_of_death.len(), n);
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_yob: Vec<i32> = Vec::new();
    let mut out_hours_total: Vec<f64> = Vec::new();
    let mut out_aslpr_speak_init: Vec<i32> = Vec::new();
    let mut out_aslpr_speak_latest: Vec<i32> = Vec::new();
    let mut out_aslpr_listen_init: Vec<i32> = Vec::new();
    let mut out_aslpr_listen_latest: Vec<i32> = Vec::new();
    let mut out_aslpr_read_init: Vec<i32> = Vec::new();
    let mut out_aslpr_read_latest: Vec<i32> = Vec::new();
    let mut out_aslpr_write_init: Vec<i32> = Vec::new();
    let mut out_aslpr_write_latest: Vec<i32> = Vec::new();
    let mut out_enrol_year: Vec<i32> = Vec::new();

    for i in 0..n {
        let cob = country_of_birth[i];
        let yoa = year_of_arrival[i];
        if cob == 0 {
            continue;
        }
        if yoa < 2003 || yoa > 2019 {
            continue;
        }
        if rng.gen::<f64>() >= AMEP_RATE {
            continue;
        }
        let enrol_year = yoa + rng.gen_range(0..=1);
        if year_of_death[i] != i32::MIN && enrol_year > year_of_death[i] {
            continue;
        }

        let hours = normal_sample(&mut rng, 300.0, 120.0)
            .max(50.0)
            .min(510.0)
            .round();
        let speak_init = rng.gen_range(0..=2);
        let listen_init = rng.gen_range(0..=2);
        let read_init = rng.gen_range(0..=2);
        let write_init = rng.gen_range(0..=2);
        let speak_latest = (speak_init + rng.gen_range(0..=2)).min(5);
        let listen_latest = (listen_init + rng.gen_range(0..=2)).min(5);
        let read_latest = (read_init + rng.gen_range(0..=2)).min(5);
        let write_latest = (write_init + rng.gen_range(0..=2)).min(5);
        out_aeuid.push(aeuid[i].to_string());
        out_yob.push(birth_year[i]);
        out_hours_total.push(hours);
        out_aslpr_speak_init.push(speak_init);
        out_aslpr_speak_latest.push(speak_latest);
        out_aslpr_listen_init.push(listen_init);
        out_aslpr_listen_latest.push(listen_latest);
        out_aslpr_read_init.push(read_init);
        out_aslpr_read_latest.push(read_latest);
        out_aslpr_write_init.push(write_init);
        out_aslpr_write_latest.push(write_latest);
        out_enrol_year.push(enrol_year);
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
            name: "HOURS_TOTAL",
            col: Col::F64(out_hours_total),
        },
        NamedCol {
            name: "INITIAL_ASLPR_SPEAK",
            col: Col::I32(out_aslpr_speak_init),
        },
        NamedCol {
            name: "LATEST_ASLPR_SPEAK",
            col: Col::I32(out_aslpr_speak_latest),
        },
        NamedCol {
            name: "INITIAL_ASLPR_LISTEN",
            col: Col::I32(out_aslpr_listen_init),
        },
        NamedCol {
            name: "LATEST_ASLPR_LISTEN",
            col: Col::I32(out_aslpr_listen_latest),
        },
        NamedCol {
            name: "INITIAL_ASLPR_READ",
            col: Col::I32(out_aslpr_read_init),
        },
        NamedCol {
            name: "LATEST_ASLPR_READ",
            col: Col::I32(out_aslpr_read_latest),
        },
        NamedCol {
            name: "INITIAL_ASLPR_WRITE",
            col: Col::I32(out_aslpr_write_init),
        },
        NamedCol {
            name: "LATEST_ASLPR_WRITE",
            col: Col::I32(out_aslpr_write_latest),
        },
        NamedCol {
            name: "ENROLLED_FY",
            col: Col::I32(out_enrol_year),
        },
    ];
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("amep parquet write: {}", e));
    total
}

extendr_module! {
    mod amep;
    fn project_amep__;
    fn project_amep_to_parquet__;
}

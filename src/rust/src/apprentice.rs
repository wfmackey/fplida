use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::mbs::{days_since_epoch, observable_days_in_year};
use crate::sampling::weighted_sample;

// ~5% of working-age population in apprenticeships/traineeships
const APPRENTICE_RATE: f64 = 0.05;

// Qualification levels
const QUAL_LEVELS: [i32; 4] = [2, 3, 4, 5]; // Cert II, III, IV, Diploma
const QUAL_WEIGHTS: [f64; 4] = [0.10, 0.55, 0.25, 0.10];

// Status
const STATUS_CODES: [&str; 4] = ["Completed", "In Training", "Cancelled", "Expired"];
const STATUS_WEIGHTS: [f64; 4] = [0.40, 0.25, 0.25, 0.10];

fn apprentice_period(
    rng: &mut StdRng,
    birth_year: i32,
    death_year: i32,
    death_month: i32,
    death_day: i32,
    sampled_status: &'static str,
) -> Option<(i32, i32, i32, &'static str)> {
    let first_start_year = (birth_year + 16).max(2006);
    let last_start_year = if death_year == i32::MIN {
        2023
    } else {
        death_year.min(2023)
    };
    if first_start_year > last_start_year {
        return None;
    }

    let start_year = rng.gen_range(first_start_year..=last_start_year);
    let year_start = days_since_epoch(start_year, 1, 1);
    let start_offset = observable_days_in_year(start_year, death_year, death_month, death_day)?;
    let start_date = year_start + rng.gen_range(0..=start_offset);
    let planned_days = rng.gen_range(365..=1460);
    let planned_end = start_date + planned_days;

    let death_date = if death_year == i32::MIN {
        None
    } else {
        observable_days_in_year(death_year, death_year, death_month, death_day)
            .map(|offset| days_since_epoch(death_year, 1, 1) + offset)
    };

    let has_recorded_end = matches!(sampled_status, "Completed" | "Cancelled");
    let death_ends_training = death_date.is_some_and(|date| {
        date >= start_date
            && (planned_end > date || (!has_recorded_end && date <= days_since_epoch(2023, 12, 31)))
    });
    if death_ends_training {
        let end_date = death_date.unwrap_or(start_date);
        return Some((start_date, end_date, end_date - start_date, "Cancelled"));
    }

    if has_recorded_end {
        Some((start_date, planned_end, planned_days, sampled_status))
    } else {
        Some((start_date, i32::MIN, planned_days, sampled_status))
    }
}

/// Project Apprentice and Trainee (A&T) from spine.
/// @export
#[extendr]
fn project_apprentice__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    education: &[i32],
    anzsco_code: Strings,
    industry: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    seed: i64,
) -> List {
    let n = aeuid.len();
    assert_eq!(year_of_death.len(), n);
    assert_eq!(month_of_death.len(), n);
    assert_eq!(day_of_death.len(), n);
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let est_n = (n as f64 * APPRENTICE_RATE) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_status: Vec<String> = Vec::with_capacity(est_n);
    let mut out_anzsco: Vec<String> = Vec::with_capacity(est_n);
    let mut out_qual_level: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_start_date: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_end_date: Vec<Rint> = Vec::with_capacity(est_n);
    let mut out_days: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_gender: Vec<String> = Vec::with_capacity(est_n);
    let mut out_dob_y: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_dob_m: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_indigenous: Vec<String> = Vec::with_capacity(est_n);
    let mut out_cob: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_school_based: Vec<String> = Vec::with_capacity(est_n);
    let mut out_state: Vec<i32> = Vec::with_capacity(est_n);

    for i in 0..n {
        let age = 2020 - birth_year[i];
        // Apprenticeships mainly for 16-30 year olds with lower education
        let p = if age >= 16 && age <= 25 && education[i] <= 3 {
            APPRENTICE_RATE * 3.0
        } else if age >= 16 && age <= 30 {
            APPRENTICE_RATE
        } else {
            APPRENTICE_RATE * 0.1
        };

        if rng.gen::<f64>() >= p {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();
        let gender = if sex[i] == 1 { "M" } else { "F" };
        let ind_str = if indigenous[i] >= 2 && indigenous[i] <= 4 {
            "Y"
        } else {
            "N"
        };

        let ql_idx = weighted_sample(&mut rng, &QUAL_WEIGHTS);
        let qual_level = QUAL_LEVELS[ql_idx];

        let st_idx = weighted_sample(&mut rng, &STATUS_WEIGHTS);
        let Some((start_dt, end_date, duration_days, status)) = apprentice_period(
            &mut rng,
            birth_year[i],
            year_of_death[i],
            month_of_death[i],
            day_of_death[i],
            STATUS_CODES[st_idx],
        ) else {
            continue;
        };
        let end_dt = if end_date == i32::MIN {
            Rint::na()
        } else {
            Rint::from(end_date)
        };

        let occ_r = &anzsco_code[i];
        let occ = if occ_r.is_na() {
            "0000".to_string()
        } else {
            occ_r.to_string()
        };

        let school_based = if age <= 18 && rng.gen::<f64>() < 0.15 {
            "Y"
        } else {
            "N"
        };

        out_aeuid.push(person_aeuid);
        out_status.push(status.to_string());
        out_anzsco.push(occ);
        out_qual_level.push(qual_level);
        out_start_date.push(start_dt);
        out_end_date.push(end_dt);
        out_days.push(duration_days);
        out_gender.push(gender.to_string());
        out_dob_y.push(birth_year[i]);
        out_dob_m.push(rng.gen_range(1..=12));
        out_indigenous.push(ind_str.to_string());
        out_cob.push(country_of_birth[i]);
        out_school_based.push(school_based.to_string());
        out_state.push(state[i]);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        STATUS = out_status,
        ANZSCO = out_anzsco,
        QUALIFICATION_LEVEL = out_qual_level,
        START_DATE = out_start_date,
        END_DATE = out_end_date,
        DAYS_IN_TRAINING = out_days,
        GENDER = out_gender,
        DOB_Y = out_dob_y,
        DOB_M = out_dob_m,
        INDIGENOUS_STATUS = out_indigenous,
        COUNTRY_OF_BIRTH = out_cob,
        SCHOOL_BASED = out_school_based,
        STATE_ASGS_2021 = out_state
    )
}

/// Project Apprentice directly to parquet. Single output file.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_apprentice_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    education: &[i32],
    anzsco_code: Strings,
    industry: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let _ = industry;
    let n = aeuid.len();
    assert_eq!(year_of_death.len(), n);
    assert_eq!(month_of_death.len(), n);
    assert_eq!(day_of_death.len(), n);
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_status: Vec<String> = Vec::new();
    let mut out_anzsco: Vec<String> = Vec::new();
    let mut out_qual_level: Vec<i32> = Vec::new();
    let mut out_start_date: Vec<i32> = Vec::new();
    let mut out_end_date: Vec<i32> = Vec::new(); // use i32::MIN for NA
    let mut out_days: Vec<i32> = Vec::new();
    let mut out_gender: Vec<String> = Vec::new();
    let mut out_dob_y: Vec<i32> = Vec::new();
    let mut out_dob_m: Vec<i32> = Vec::new();
    let mut out_indigenous: Vec<String> = Vec::new();
    let mut out_cob: Vec<i32> = Vec::new();
    let mut out_school_based: Vec<String> = Vec::new();
    let mut out_state: Vec<i32> = Vec::new();

    for i in 0..n {
        let age = 2020 - birth_year[i];
        let p = if age >= 16 && age <= 25 && education[i] <= 3 {
            APPRENTICE_RATE * 3.0
        } else if age >= 16 && age <= 30 {
            APPRENTICE_RATE
        } else {
            APPRENTICE_RATE * 0.1
        };
        if rng.gen::<f64>() >= p {
            continue;
        }

        let gender = if sex[i] == 1 { "M" } else { "F" };
        let ind_str = if indigenous[i] >= 2 && indigenous[i] <= 4 {
            "Y"
        } else {
            "N"
        };
        let ql_idx = weighted_sample(&mut rng, &QUAL_WEIGHTS);
        let qual_level = QUAL_LEVELS[ql_idx];
        let st_idx = weighted_sample(&mut rng, &STATUS_WEIGHTS);
        let Some((start_dt, end_dt, duration_days, status)) = apprentice_period(
            &mut rng,
            birth_year[i],
            year_of_death[i],
            month_of_death[i],
            day_of_death[i],
            STATUS_CODES[st_idx],
        ) else {
            continue;
        };
        let occ_r = &anzsco_code[i];
        let occ = if occ_r.is_na() {
            "0000".to_string()
        } else {
            occ_r.to_string()
        };
        let school_based = if age <= 18 && rng.gen::<f64>() < 0.15 {
            "Y"
        } else {
            "N"
        };

        out_aeuid.push(aeuid[i].to_string());
        out_status.push(status.to_string());
        out_anzsco.push(occ);
        out_qual_level.push(qual_level);
        out_start_date.push(start_dt);
        out_end_date.push(end_dt);
        out_days.push(duration_days);
        out_gender.push(gender.to_string());
        out_dob_y.push(birth_year[i]);
        out_dob_m.push(rng.gen_range(1..=12));
        out_indigenous.push(ind_str.to_string());
        out_cob.push(country_of_birth[i]);
        out_school_based.push(school_based.to_string());
        out_state.push(state[i]);
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "STATUS",
            col: Col::Str(out_status),
        },
        NamedCol {
            name: "ANZSCO",
            col: Col::Str(out_anzsco),
        },
        NamedCol {
            name: "QUALIFICATION_LEVEL",
            col: Col::I32(out_qual_level),
        },
        NamedCol {
            name: "START_DATE",
            col: Col::DateNN(out_start_date),
        },
        NamedCol {
            name: "END_DATE",
            col: Col::Date(out_end_date),
        },
        NamedCol {
            name: "DAYS_IN_TRAINING",
            col: Col::I32(out_days),
        },
        NamedCol {
            name: "GENDER",
            col: Col::Str(out_gender),
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
            name: "INDIGENOUS_STATUS",
            col: Col::Str(out_indigenous),
        },
        NamedCol {
            name: "COUNTRY_OF_BIRTH",
            col: Col::I32(out_cob),
        },
        NamedCol {
            name: "SCHOOL_BASED",
            col: Col::Str(out_school_based),
        },
        NamedCol {
            name: "STATE_ASGS_2021",
            col: Col::I32(out_state),
        },
    ];
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("apprentice parquet write: {}", e));
    total
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn apprentice_period_respects_age_and_death() {
        let mut rng = StdRng::seed_from_u64(2800);
        assert!(apprentice_period(&mut rng, 2005, 2018, 1, 1, "Completed").is_none());

        let death_date = days_since_epoch(2018, 1, 1);
        for _ in 0..200 {
            let (start, end, days, status) =
                apprentice_period(&mut rng, 2000, 2018, 1, 1, "In Training").unwrap();
            assert!(start >= days_since_epoch(2016, 1, 1));
            assert!(start <= death_date);
            assert!(end <= death_date);
            assert_eq!(days, end - start);
            assert_eq!(status, "Cancelled");
        }
    }
}

extendr_module! {
    mod apprentice;
    fn project_apprentice__;
    fn project_apprentice_to_parquet__;
}

use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::mbs::days_since_epoch;
use crate::sampling::weighted_sample;

// Visa subclass codes and weights (top Australian visa subclasses)
const VISA_SUBCLASS: [i32; 20] = [
    189, 190, 491, 186, 482, // Skilled
    820, 801, 309, 300, 100, // Family/Partner
    500, 485, 476, 417, 462, // Student/WHM
    200, 201, 202, 204, 866, // Humanitarian
];
const VISA_WEIGHTS: [f64; 20] = [
    0.10, 0.08, 0.04, 0.06, 0.08, 0.06, 0.03, 0.04, 0.03, 0.02, 0.15, 0.02, 0.01, 0.06, 0.03, 0.03,
    0.02, 0.02, 0.01, 0.02,
];

// Visa report group (finer): Skilled / Family / Student / Working Holiday /
// Humanitarian / Other Temporary. Emitted as VISA_REPORT_GROUP_DS.
fn visa_program(subclass: i32) -> &'static str {
    match subclass {
        189 | 190 | 491 | 186 | 482 | 485 | 476 => "Skilled",
        820 | 801 | 309 | 300 | 100 => "Family",
        500 => "Student",
        417 | 462 => "Working Holiday",
        200 | 201 | 202 | 204 | 866 => "Humanitarian",
        _ => "Other Temporary",
    }
}

// High-level visa program stream, emitted as VISA_PRGRM_DS (distinct from the
// finer report group above so the two columns are not collinear): permanent
// Skilled/Family are the Migration Program, refugee/protection are the
// Humanitarian Program, and everything else is the Temporary Visa Program.
fn visa_program_stream(subclass: i32) -> &'static str {
    match visa_program(subclass) {
        "Skilled" | "Family" => "Migration Program",
        "Humanitarian" => "Humanitarian Program",
        _ => "Temporary Visa Program",
    }
}

fn visa_subclass_desc(subclass: i32) -> &'static str {
    match subclass {
        189 => "Skilled Independent",
        190 => "Skilled Nominated",
        491 => "Skilled Work Regional",
        186 => "Employer Nomination Scheme",
        482 => "Temporary Skill Shortage",
        820 => "Partner (onshore)",
        801 => "Partner (permanent)",
        309 => "Partner (offshore provisional)",
        300 => "Prospective Marriage",
        100 => "Partner (offshore permanent)",
        500 => "Student",
        485 => "Temporary Graduate",
        476 => "Skilled Recognised Graduate",
        417 => "Working Holiday",
        462 => "Work and Holiday",
        200 => "Refugee",
        201 => "In-country Special Humanitarian",
        202 => "Global Special Humanitarian",
        204 => "Woman at Risk",
        866 => "Protection",
        _ => "Other",
    }
}

fn is_permanent(subclass: i32) -> bool {
    matches!(
        subclass,
        189 | 190 | 186 | 820 | 801 | 100 | 200 | 201 | 202 | 204 | 866
    )
}

/// Project VISA dataset from spine (overseas-born persons only).
/// @export
#[extendr]
fn project_visa__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    indigenous: &[i32],
    anzsco_code: Strings,
    baseline_income: &[f64],
    seed: i64,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut out_visa_subclass_cd: Vec<i32> = Vec::with_capacity(n);
    let mut out_visa_subclass_ds: Vec<String> = Vec::with_capacity(n);
    let mut out_visa_prgrm_ds: Vec<String> = Vec::with_capacity(n);
    let mut out_visa_report_group: Vec<String> = Vec::with_capacity(n);
    let mut out_va_lodged_dt: Vec<i32> = Vec::with_capacity(n);
    let mut out_tr_visa_grant_dt: Vec<i32> = Vec::with_capacity(n);
    let mut out_tr_stay_period_dd: Vec<i32> = Vec::with_capacity(n);
    let mut out_va_primary_fl: Vec<String> = Vec::with_capacity(n);
    let mut out_va_clnt_locn_cd: Vec<i32> = Vec::with_capacity(n);
    let mut out_tr_citz_cntry_cd: Vec<i32> = Vec::with_capacity(n);
    let mut out_tr_marital_status: Vec<String> = Vec::with_capacity(n);
    let mut out_nm_ocptn_cd: Vec<String> = Vec::with_capacity(n);
    let mut out_nm_salary_am: Vec<f64> = Vec::with_capacity(n);
    let mut out_ielts_ovrl: Vec<f64> = Vec::with_capacity(n);
    let mut out_last_on_day: Vec<i32> = Vec::with_capacity(n);

    for i in 0..n {
        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let yoa = year_of_arrival[i];

        // Draw visa subclass
        let vs_idx = weighted_sample(&mut rng, &VISA_WEIGHTS);
        let subclass = VISA_SUBCLASS[vs_idx];
        let desc = visa_subclass_desc(subclass);
        let program = visa_program(subclass);

        // Lodgement: 6-24 months before grant
        let lodge_lag = rng.gen_range(180..=730);
        let grant_dt = days_since_epoch(
            yoa,
            rng.gen_range(1..=12) as u32,
            rng.gen_range(1..=28) as u32,
        );
        let lodged_dt = grant_dt - lodge_lag;

        // Stay period: permanent = 0, temporary varies
        let stay_dd = if is_permanent(subclass) {
            0
        } else {
            match subclass {
                500 => rng.gen_range(365..=1825), // student: 1-5 years
                482 => rng.gen_range(730..=1460), // TSS: 2-4 years
                417 | 462 => 365,                 // WHM: 1 year
                _ => rng.gen_range(365..=1095),
            }
        };

        // Primary applicant
        let primary = if rng.gen::<f64>() < 0.60 { "Y" } else { "N" };

        // Client location at lodgement
        let locn = if rng.gen::<f64>() < 0.70 { 3 } else { 4 }; // 3=in AU, 4=outside

        // Marital status
        let age_at_grant = yoa - by;
        let marital = if age_at_grant < 25 {
            "Single"
        } else if rng.gen::<f64>() < 0.60 {
            "Married"
        } else if rng.gen::<f64>() < 0.50 {
            "De facto"
        } else {
            "Single"
        };

        // Occupation (skilled visa holders only)
        let occ = if matches!(subclass, 189 | 190 | 491 | 186 | 482 | 485 | 476) {
            let r = &anzsco_code[i];
            if r.is_na() {
                String::new()
            } else {
                r.to_string()
            }
        } else {
            String::new()
        };

        // Salary (skilled only)
        let salary = if matches!(subclass, 186 | 482) {
            baseline_income[i].max(53900.0) // TSMIT threshold
        } else {
            0.0
        };

        // IELTS (skilled only)
        let ielts = if matches!(subclass, 189 | 190 | 491 | 186 | 482) {
            let score = crate::sampling::normal_sample(&mut rng, 6.5, 1.0);
            (score.max(4.0).min(9.0) * 2.0).round() / 2.0 // round to 0.5
        } else {
            0.0
        };

        out_aeuid.push(person_aeuid);
        out_visa_subclass_cd.push(subclass);
        out_visa_subclass_ds.push(desc.to_string());
        out_visa_prgrm_ds.push(visa_program_stream(subclass).to_string());
        out_visa_report_group.push(program.to_string());
        out_va_lodged_dt.push(lodged_dt);
        out_tr_visa_grant_dt.push(grant_dt);
        out_tr_stay_period_dd.push(stay_dd);
        out_va_primary_fl.push(primary.to_string());
        out_va_clnt_locn_cd.push(locn);
        out_tr_citz_cntry_cd.push(country_of_birth[i]);
        out_tr_marital_status.push(marital.to_string());
        out_nm_ocptn_cd.push(occ);
        out_nm_salary_am.push(salary);
        out_ielts_ovrl.push(ielts);
        out_last_on_day.push(1);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        VISA_SUBCLASS_CD = out_visa_subclass_cd,
        VISA_SUBCLASS_DS = out_visa_subclass_ds,
        VISA_PRGRM_DS = out_visa_prgrm_ds,
        VISA_REPORT_GROUP_DS = out_visa_report_group,
        VA_LODGED_DT = out_va_lodged_dt,
        TR_VISA_GRANT_DT = out_tr_visa_grant_dt,
        TR_STAY_PERIOD_DD = out_tr_stay_period_dd,
        VA_PRIMARY_FL = out_va_primary_fl,
        VA_CLNT_LOCN_CD = out_va_clnt_locn_cd,
        TR_CITZ_CNTRY_CD = out_tr_citz_cntry_cd,
        TR_MARITAL_STATUS_DS = out_tr_marital_status,
        NM_OCPTN_CD = out_nm_ocptn_cd,
        NM_SALARY_AM = out_nm_salary_am,
        IELTS_OVRL = out_ielts_ovrl,
        LAST_ON_DAY = out_last_on_day
    )
}

/// Project VISA directly to parquet. Single file.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_visa_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    indigenous: &[i32],
    anzsco_code: Strings,
    baseline_income: &[f64],
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let _ = (state, indigenous); // silence unused warnings (kept for signature parity)

    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut out_visa_subclass_cd: Vec<i32> = Vec::with_capacity(n);
    let mut out_visa_subclass_ds: Vec<String> = Vec::with_capacity(n);
    let mut out_visa_prgrm_ds: Vec<String> = Vec::with_capacity(n);
    let mut out_visa_report_group: Vec<String> = Vec::with_capacity(n);
    let mut out_va_lodged_dt: Vec<i32> = Vec::with_capacity(n);
    let mut out_tr_visa_grant_dt: Vec<i32> = Vec::with_capacity(n);
    let mut out_tr_stay_period_dd: Vec<i32> = Vec::with_capacity(n);
    let mut out_va_primary_fl: Vec<String> = Vec::with_capacity(n);
    let mut out_va_clnt_locn_cd: Vec<i32> = Vec::with_capacity(n);
    let mut out_tr_citz_cntry_cd: Vec<i32> = Vec::with_capacity(n);
    let mut out_tr_marital_status: Vec<String> = Vec::with_capacity(n);
    let mut out_nm_ocptn_cd: Vec<String> = Vec::with_capacity(n);
    let mut out_nm_salary_am: Vec<f64> = Vec::with_capacity(n);
    let mut out_ielts_ovrl: Vec<f64> = Vec::with_capacity(n);
    let mut out_last_on_day: Vec<i32> = Vec::with_capacity(n);

    for i in 0..n {
        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let yoa = year_of_arrival[i];

        let vs_idx = weighted_sample(&mut rng, &VISA_WEIGHTS);
        let subclass = VISA_SUBCLASS[vs_idx];
        let desc = visa_subclass_desc(subclass);
        let program = visa_program(subclass);

        let lodge_lag = rng.gen_range(180..=730);
        let grant_dt = days_since_epoch(
            yoa,
            rng.gen_range(1..=12) as u32,
            rng.gen_range(1..=28) as u32,
        );
        let lodged_dt = grant_dt - lodge_lag;

        let stay_dd = if is_permanent(subclass) {
            0
        } else {
            match subclass {
                500 => rng.gen_range(365..=1825),
                482 => rng.gen_range(730..=1460),
                417 | 462 => 365,
                _ => rng.gen_range(365..=1095),
            }
        };

        let primary = if rng.gen::<f64>() < 0.60 { "Y" } else { "N" };
        let locn = if rng.gen::<f64>() < 0.70 { 3 } else { 4 };

        let age_at_grant = yoa - by;
        let marital = if age_at_grant < 25 {
            "Single"
        } else if rng.gen::<f64>() < 0.60 {
            "Married"
        } else if rng.gen::<f64>() < 0.50 {
            "De facto"
        } else {
            "Single"
        };

        let occ = if matches!(subclass, 189 | 190 | 491 | 186 | 482 | 485 | 476) {
            let r = &anzsco_code[i];
            if r.is_na() {
                String::new()
            } else {
                r.to_string()
            }
        } else {
            String::new()
        };

        let salary = if matches!(subclass, 186 | 482) {
            baseline_income[i].max(53900.0)
        } else {
            0.0
        };

        let ielts = if matches!(subclass, 189 | 190 | 491 | 186 | 482) {
            let score = crate::sampling::normal_sample(&mut rng, 6.5, 1.0);
            (score.max(4.0).min(9.0) * 2.0).round() / 2.0
        } else {
            0.0
        };

        out_aeuid.push(person_aeuid);
        out_visa_subclass_cd.push(subclass);
        out_visa_subclass_ds.push(desc.to_string());
        out_visa_prgrm_ds.push(visa_program_stream(subclass).to_string());
        out_visa_report_group.push(program.to_string());
        out_va_lodged_dt.push(lodged_dt);
        out_tr_visa_grant_dt.push(grant_dt);
        out_tr_stay_period_dd.push(stay_dd);
        out_va_primary_fl.push(primary.to_string());
        out_va_clnt_locn_cd.push(locn);
        out_tr_citz_cntry_cd.push(country_of_birth[i]);
        out_tr_marital_status.push(marital.to_string());
        out_nm_ocptn_cd.push(occ);
        out_nm_salary_am.push(salary);
        out_ielts_ovrl.push(ielts);
        out_last_on_day.push(1);
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "VISA_SUBCLASS_CD",
            col: Col::I32(out_visa_subclass_cd),
        },
        NamedCol {
            name: "VISA_SUBCLASS_DS",
            col: Col::Str(out_visa_subclass_ds),
        },
        NamedCol {
            name: "VISA_PRGRM_DS",
            col: Col::Str(out_visa_prgrm_ds),
        },
        NamedCol {
            name: "VISA_REPORT_GROUP_DS",
            col: Col::Str(out_visa_report_group),
        },
        NamedCol {
            name: "VA_LODGED_DT",
            col: Col::DateNN(out_va_lodged_dt),
        },
        NamedCol {
            name: "TR_VISA_GRANT_DT",
            col: Col::DateNN(out_tr_visa_grant_dt),
        },
        NamedCol {
            name: "TR_STAY_PERIOD_DD",
            col: Col::I32(out_tr_stay_period_dd),
        },
        NamedCol {
            name: "VA_PRIMARY_FL",
            col: Col::Str(out_va_primary_fl),
        },
        NamedCol {
            name: "VA_CLNT_LOCN_CD",
            col: Col::I32(out_va_clnt_locn_cd),
        },
        NamedCol {
            name: "TR_CITZ_CNTRY_CD",
            col: Col::I32(out_tr_citz_cntry_cd),
        },
        NamedCol {
            name: "TR_MARITAL_STATUS_DS",
            col: Col::Str(out_tr_marital_status),
        },
        NamedCol {
            name: "NM_OCPTN_CD",
            col: Col::Str(out_nm_ocptn_cd),
        },
        NamedCol {
            name: "NM_SALARY_AM",
            col: Col::F64(out_nm_salary_am),
        },
        NamedCol {
            name: "IELTS_OVRL",
            col: Col::F64(out_ielts_ovrl),
        },
        NamedCol {
            name: "LAST_ON_DAY",
            col: Col::I32(out_last_on_day),
        },
    ];
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("visa parquet write: {}", e));
    total
}

extendr_module! {
    mod visa;
    fn project_visa__;
    fn project_visa_to_parquet__;
}

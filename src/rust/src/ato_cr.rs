use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::mbs::days_since_epoch;

// ATO participation: ~90% of adults (16+)
const PART_RATE: f64 = 0.90;

/// Project ATO Client Register from spine.
///
/// Produces demographics and address records for persons who have
/// interacted with the ATO. Two products: demographics + address.
/// @export
#[extendr]
fn project_ato_cr__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    seed: i64,
    reference_year: i32,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let est_n = (n as f64 * PART_RATE) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_start_date: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_end_date: Vec<Rint> = Vec::with_capacity(est_n);
    let mut out_dob_monyyyy: Vec<String> = Vec::with_capacity(est_n);
    let mut out_sex: Vec<String> = Vec::with_capacity(est_n);
    let mut out_clnt_sts: Vec<String> = Vec::with_capacity(est_n);
    let mut out_diac_pid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_addr_sts: Vec<String> = Vec::with_capacity(est_n);
    let mut out_adr_typ: Vec<String> = Vec::with_capacity(est_n);
    let mut out_addr_reliable: Vec<String> = Vec::with_capacity(est_n);
    let mut out_state: Vec<i32> = Vec::with_capacity(est_n);

    let months = [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ];

    for i in 0..n {
        let age = reference_year - birth_year[i];
        // Only adults (16+) participate
        if age < 16 {
            continue;
        }
        if rng.gen::<f64>() >= PART_RATE {
            continue;
        }

        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let sx = sex[i];
        let st = state[i];
        let cob = country_of_birth[i];

        let birth_month = rng.gen_range(0..12usize);
        let dob_str = format!("{}{:04}", months[birth_month], by);

        // Start date: when they first interacted (age 16 or first employment)
        let start_yr = (by + 16).max(1999);
        let start_date = days_since_epoch(start_yr, 7, 1);

        let sex_str = if sx == 1 { "M" } else { "F" };

        // Client status: A=active (95%), I=inactive (5%)
        let clnt_sts = if rng.gen::<f64>() < 0.95 { "A" } else { "I" };

        // DIAC flag: Y if overseas-born
        let diac = if cob != 0 { "Y" } else { "N" };

        // Address reliability
        let reliable = if rng.gen::<f64>() < 0.95 { "R" } else { "U" };

        out_aeuid.push(person_aeuid);
        out_start_date.push(start_date);
        out_end_date.push(Rint::na()); // ongoing
        out_dob_monyyyy.push(dob_str);
        out_sex.push(sex_str.to_string());
        out_clnt_sts.push(clnt_sts.to_string());
        out_diac_pid.push(diac.to_string());
        out_addr_sts.push("A".to_string());
        out_adr_typ.push("R".to_string());
        out_addr_reliable.push(reliable.to_string());
        out_state.push(st);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        START_DATE = out_start_date,
        END_DATE = out_end_date,
        DOB_MONYYYY = out_dob_monyyyy,
        SEX = out_sex,
        CLNT_STS_CD = out_clnt_sts,
        DIAC_PID_IND = out_diac_pid,
        ADDR_STS_CD = out_addr_sts,
        ADR_TYP = out_adr_typ,
        ADDR_RELIABLE_STS = out_addr_reliable,
        STATE_ASGS_2021 = out_state
    )
}

/// Project ATO_CR directly to parquet. Single file.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_ato_cr_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    country_of_birth: &[i32],
    seed: i64,
    reference_year: i32,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_start_date: Vec<i32> = Vec::new();
    let mut out_end_date: Vec<i32> = Vec::new(); // i32::MIN = NA
    let mut out_dob_monyyyy: Vec<String> = Vec::new();
    let mut out_sex: Vec<String> = Vec::new();
    let mut out_clnt_sts: Vec<String> = Vec::new();
    let mut out_diac_pid: Vec<String> = Vec::new();
    let mut out_addr_sts: Vec<String> = Vec::new();
    let mut out_adr_typ: Vec<String> = Vec::new();
    let mut out_addr_reliable: Vec<String> = Vec::new();
    let mut out_state: Vec<i32> = Vec::new();

    let months = [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ];

    for i in 0..n {
        let age = reference_year - birth_year[i];
        if age < 16 {
            continue;
        }
        if rng.gen::<f64>() >= PART_RATE {
            continue;
        }
        let by = birth_year[i];
        let sx = sex[i];
        let st = state[i];
        let cob = country_of_birth[i];
        let birth_month = rng.gen_range(0..12usize);
        let dob_str = format!("{}{:04}", months[birth_month], by);
        let start_yr = (by + 16).max(1999);
        let start_date = days_since_epoch(start_yr, 7, 1);
        let sex_str = if sx == 1 { "M" } else { "F" };
        let clnt_sts = if rng.gen::<f64>() < 0.95 { "A" } else { "I" };
        let diac = if cob != 0 { "Y" } else { "N" };
        let reliable = if rng.gen::<f64>() < 0.95 { "R" } else { "U" };

        out_aeuid.push(aeuid[i].to_string());
        out_start_date.push(start_date);
        out_end_date.push(i32::MIN);
        out_dob_monyyyy.push(dob_str);
        out_sex.push(sex_str.to_string());
        out_clnt_sts.push(clnt_sts.to_string());
        out_diac_pid.push(diac.to_string());
        out_addr_sts.push("A".to_string());
        out_adr_typ.push("R".to_string());
        out_addr_reliable.push(reliable.to_string());
        out_state.push(st);
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
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
            name: "DOB_MONYYYY",
            col: Col::Str(out_dob_monyyyy),
        },
        NamedCol {
            name: "SEX",
            col: Col::Str(out_sex),
        },
        NamedCol {
            name: "CLNT_STS_CD",
            col: Col::Str(out_clnt_sts),
        },
        NamedCol {
            name: "DIAC_PID_IND",
            col: Col::Str(out_diac_pid),
        },
        NamedCol {
            name: "ADDR_STS_CD",
            col: Col::Str(out_addr_sts),
        },
        NamedCol {
            name: "ADR_TYP",
            col: Col::Str(out_adr_typ),
        },
        NamedCol {
            name: "ADDR_RELIABLE_STS",
            col: Col::Str(out_addr_reliable),
        },
        NamedCol {
            name: "STATE_ASGS_2021",
            col: Col::I32(out_state),
        },
    ];
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("ato_cr parquet write: {}", e));
    total
}

extendr_module! {
    mod ato_cr;
    fn project_ato_cr__;
    fn project_ato_cr_to_parquet__;
}

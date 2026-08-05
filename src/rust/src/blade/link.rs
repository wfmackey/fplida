//! BLADE person-business link builder (Stage 2 of the R->Rust port).
//!
//! Ports `.blade_employee_assignments`, `.blade_assignment_occupation_codes`
//! and `.make_blade_person_link`. Builds the employee block (primary +
//! secondary jobs) and the owner block, with occupation-health overweighting,
//! reusing the state-aware assigner from `business_spine`. RNG-free; i64 exact.

use extendr_api::prelude::*;

use super::business_spine::{assign_business_by_state, HEALTH_OCC_CODE, HEALTH_OCC_TITLE};
use super::helpers::{normalise_anzsco, round2};

/// Build the PLIDA-BLADE person link (26 columns, employee block then owner
/// block). `birth_year`/`sex` use i32::MIN for NA; aeuid/anzsco strings carry
/// NA where the spine value is missing.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn make_blade_person_link__(
    spine_state: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    spine_id: Strings,
    aeuid_ato: Strings,
    aeuid_abs: Strings,
    aeuid_dhda: Strings,
    anzsco_code: Strings,
    anzsco_title: Strings,
    birth_year: &[i32],
    sex: &[i32],
    bs_state: &[i32],
    bs_bn: Strings,
    bs_id: Strings,
    bs_bg_id: Strings,
    bs_health_flag: &[i32],
    seed: i32,
) -> List {
    let seed = seed as i64;
    let n = spine_state.len();
    let nb = bs_state.len();

    // String accessors that map R NA to None.
    let s_opt = |v: &Strings, i: usize| -> Option<String> {
        let x = &v[i];
        if x.is_na() || x.to_string() == "NA" {
            None
        } else {
            Some(x.to_string())
        }
    };
    let bn_v: Vec<String> = bs_bn.iter().map(|s| s.to_string()).collect();
    let bid_v: Vec<String> = bs_id.iter().map(|s| s.to_string()).collect();
    let bg_v: Vec<String> = bs_bg_id.iter().map(|s| s.to_string()).collect();

    // -- 26 output columns --
    let mut o_spine_id: Vec<Option<String>> = Vec::new();
    let mut o_aeuid: Vec<Option<String>> = Vec::new(); // SYNTHETIC_AEUID (== ATO)
    let mut o_abs: Vec<Option<String>> = Vec::new();
    let mut o_dhda: Vec<Option<String>> = Vec::new();
    let mut o_bn: Vec<String> = Vec::new();
    let mut o_id: Vec<String> = Vec::new();
    let mut o_bg: Vec<String> = Vec::new();
    let mut o_rel: Vec<String> = Vec::new();
    let mut o_src: Vec<String> = Vec::new();
    let mut o_jobno: Vec<i32> = Vec::new();
    let mut o_primary: Vec<i32> = Vec::new();
    let mut o_anzsco: Vec<Option<String>> = Vec::new();
    let mut o_anzsco_title: Vec<Option<String>> = Vec::new();
    let mut o_occ_health: Vec<i32> = Vec::new();
    let mut o_wage: Vec<Option<f64>> = Vec::new();
    let mut o_birth: Vec<i32> = Vec::new();
    let mut o_age: Vec<i32> = Vec::new();
    let mut o_sex: Vec<i32> = Vec::new();

    // ---- Employee block ----
    if nb > 0 {
        let employed_idx: Vec<usize> = (0..n)
            .filter(|&i| {
                baseline_employed[i] == 1
                    && !baseline_income[i].is_nan()
                    && baseline_income[i] > 0.0
            })
            .collect();
        if !employed_idx.is_empty() {
            let emp_state: Vec<i32> = employed_idx.iter().map(|&i| spine_state[i]).collect();
            let primary = assign_business_by_state(&emp_state, bs_state, seed, 31, 0.92, None);

            // Secondary jobs: ~12% of employees (or position 1 if none and >=8).
            let mut sec_pos: Vec<usize> = (0..employed_idx.len())
                .filter(|&p| (((p as i64 + 1) * 48271 + seed * 37).rem_euclid(100)) < 12)
                .collect();
            if sec_pos.is_empty() && employed_idx.len() >= 8 {
                sec_pos = vec![0];
            }

            // The employee_links frame = primary rows then secondary rows; build
            // (spine_row, business_index, job_number, primary, wage) for each.
            struct ELink {
                row: usize,
                bi: usize,
                job: i32,
                primary: i32,
                wage: f64,
            }
            let mut elinks: Vec<ELink> = employed_idx
                .iter()
                .enumerate()
                .map(|(k, &row)| ELink {
                    row,
                    bi: (primary[k] - 1) as usize,
                    job: 1,
                    primary: 1,
                    wage: round2(baseline_income[row]),
                })
                .collect();
            if !sec_pos.is_empty() {
                let sec_state: Vec<i32> = sec_pos
                    .iter()
                    .map(|&p| spine_state[employed_idx[p]])
                    .collect();
                let avoid: Vec<i32> = sec_pos.iter().map(|&p| primary[p]).collect();
                let assigned_sec =
                    assign_business_by_state(&sec_state, bs_state, seed, 73, 0.8, Some(&avoid));
                for (j, &p) in sec_pos.iter().enumerate() {
                    let row = employed_idx[p];
                    let wage =
                        round2(baseline_income[row] * (0.12 + (p as i64 % 9) as f64 / 100.0));
                    elinks.push(ELink {
                        row,
                        bi: (assigned_sec[j] - 1) as usize,
                        job: 2,
                        primary: 0,
                        wage,
                    });
                }
            }

            // Occupation codes with health overweighting (over the elinks order).
            let n_e = elinks.len();
            let mut forced_rank: i64 = 0;
            for (k, e) in elinks.iter().enumerate() {
                let mut occ = {
                    let raw = s_opt(&anzsco_code, e.row);
                    normalise_anzsco(raw.as_deref())
                };
                let mut title = s_opt(&anzsco_title, e.row)
                    .filter(|t| !t.is_empty())
                    .unwrap_or_else(|| "Occupation not stated".to_string());
                let health_business = bs_health_flag[e.bi] == 1;
                let draw = ((k as i64 + 1) * 1103515245 + seed * 2654435761).rem_euclid(100);
                let force = (health_business && draw < 72) || (!health_business && draw < 4);
                if force {
                    forced_rank += 1;
                    let r =
                        ((forced_rank + seed).rem_euclid(HEALTH_OCC_CODE.len() as i64)) as usize;
                    occ = HEALTH_OCC_CODE[r].to_string();
                    title = HEALTH_OCC_TITLE[r].to_string();
                }
                let by = birth_year[e.row];
                o_spine_id.push(spine_id_opt(&spine_id, e.row));
                o_aeuid.push(s_opt(&aeuid_ato, e.row));
                o_abs.push(s_opt(&aeuid_abs, e.row));
                o_dhda.push(s_opt(&aeuid_dhda, e.row));
                o_bn.push(bn_v[e.bi].clone());
                o_id.push(bid_v[e.bi].clone());
                o_bg.push(bg_v[e.bi].clone());
                o_rel.push(
                    if e.primary == 1 {
                        "employee"
                    } else {
                        "employee_secondary_job"
                    }
                    .to_string(),
                );
                o_src.push("STP".to_string());
                o_jobno.push(e.job);
                o_primary.push(e.primary);
                o_anzsco.push(Some(occ));
                o_anzsco_title.push(Some(title));
                o_occ_health.push(force as i32);
                o_wage.push(Some(e.wage));
                o_birth.push(by);
                o_age.push(if by == i32::MIN { i32::MIN } else { 2024 - by });
                o_sex.push(sex[e.row]);
            }
            let _ = n_e;
        }

        // ---- Owner block ----
        let age_of = |i: usize| -> i32 {
            let by = birth_year[i];
            if by == i32::MIN {
                40
            } else {
                2024 - by
            }
        };
        let adult_idx: Vec<usize> = (0..n)
            .filter(|&i| {
                let a = age_of(i);
                (20..=70).contains(&a)
            })
            .collect();
        if !adult_idx.is_empty() {
            let owner_idx: Vec<usize> = adult_idx
                .iter()
                .enumerate()
                .filter(|&(j, _)| {
                    (((j as i64 + 1) * 2654435761 + seed * 2200).rem_euclid(1_000_000)) < 80_000
                })
                .map(|(_, &i)| i)
                .collect();
            if !owner_idx.is_empty() {
                let own_state: Vec<i32> = owner_idx.iter().map(|&i| spine_state[i]).collect();
                let assigned = assign_business_by_state(&own_state, bs_state, seed, 101, 0.9, None);
                for (k, &row) in owner_idx.iter().enumerate() {
                    let bi = (assigned[k] - 1) as usize;
                    let by = birth_year[row];
                    o_spine_id.push(spine_id_opt(&spine_id, row));
                    o_aeuid.push(s_opt(&aeuid_ato, row));
                    o_abs.push(s_opt(&aeuid_abs, row));
                    o_dhda.push(s_opt(&aeuid_dhda, row));
                    o_bn.push(bn_v[bi].clone());
                    o_id.push(bid_v[bi].clone());
                    o_bg.push(bg_v[bi].clone());
                    o_rel.push("owner".to_string());
                    o_src.push("BUSOWN".to_string());
                    o_jobno.push(i32::MIN);
                    o_primary.push(i32::MIN);
                    o_anzsco.push(None);
                    o_anzsco_title.push(None);
                    o_occ_health.push(0);
                    o_wage.push(None);
                    o_birth.push(by);
                    o_age.push(age_of(row));
                    o_sex.push(sex[row]);
                }
            }
        }
    }

    let n_rows = o_spine_id.len();
    list!(
        spine_id = o_spine_id,
        SYNTHETIC_AEUID = o_aeuid.clone(),
        SYNTHETIC_AEUID_ATO = o_aeuid,
        SYNTHETIC_AEUID_ABS = o_abs.clone(),
        SYNTHETIC_AEUID_DHDA = o_dhda.clone(),
        synthetic_aeuid_abs = o_abs,
        synthetic_aeuid_dhda = o_dhda,
        bn = o_bn.clone(),
        BN = o_bn.clone(),
        ABN_HASH_TRUNC = o_bn,
        id = o_id,
        bg_id = o_bg,
        relationship_type = o_rel,
        source_dataset = o_src,
        job_number = o_jobno,
        primary_job = o_primary,
        FIN_YEAR = vec!["2023-24".to_string(); n_rows],
        tsid = vec!["24".to_string(); n_rows],
        ANZSCO_CODE = o_anzsco,
        ANZSCO_TITLE = o_anzsco_title,
        occupation_health_flag = o_occ_health,
        annual_wage = o_wage.clone(),
        PAYG_GROSS_WAGES = o_wage,
        birth_year = o_birth,
        age = o_age,
        sex = o_sex
    )
}

fn spine_id_opt(v: &Strings, i: usize) -> Option<String> {
    let x = &v[i];
    if x.is_na() {
        None
    } else {
        Some(x.to_string())
    }
}

extendr_module! {
    mod link;
    fn make_blade_person_link__;
}

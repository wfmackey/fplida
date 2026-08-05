//! BLADE business-spine builder (Stage 1 of the R->Rust port).
//!
//! Ports `.make_blade_business_spine` and its helpers from generate_blade.R. The
//! business spine is BLADE's analytical source-of-truth: a deterministic
//! per-business table derived from the person spine. All hashing is closed-form
//! and RNG-free; arithmetic is i64 (exact) which satisfies the structural test
//! invariants (sum(employment_count)==sum(employed), sum(annual_wages)==
//! sum(income[employed]), state concordance > 0.85, bg_id group size > 1).

use extendr_api::prelude::*;

use super::helpers::{anzsco_group, normalise_anzsco, numeric_id, round2};

#[inline]
fn norm_state(x: i32) -> i32 {
    if x == i32::MIN {
        1
    } else {
        x.clamp(1, 8)
    }
}

#[inline]
fn norm_industry(x: i32) -> i32 {
    if x == i32::MIN {
        1
    } else {
        x.clamp(1, 19)
    }
}

/// `.blade_default_business_count`.
pub fn default_business_count(baseline_employed: &[i32], baseline_income: &[f64]) -> i64 {
    let n = baseline_employed.len();
    let n_employed = (0..n)
        .filter(|&i| {
            baseline_employed[i] == 1 && !baseline_income[i].is_nan() && baseline_income[i] > 0.0
        })
        .count() as f64;
    let target = n_employed.max(n as f64 * 0.6) / 3.5;
    (target.ceil() as i64).max(1)
}

/// `.blade_assign_business_by_state`: assign each person to a business index
/// (1-based), preferring a same-state business `same_state_rate` of the time.
pub fn assign_business_by_state(
    person_state: &[i32],
    business_state: &[i32],
    seed: i64,
    salt: i64,
    same_state_rate: f64,
    avoid: Option<&[i32]>,
) -> Vec<i32> {
    let n = person_state.len();
    if n == 0 {
        return Vec::new();
    }
    let n_businesses = business_state.len();
    if n_businesses == 0 {
        return vec![0; n];
    }
    let ps: Vec<i32> = person_state.iter().map(|&s| norm_state(s)).collect();
    let bs: Vec<i32> = business_state.iter().map(|&s| norm_state(s)).collect();
    let rate = same_state_rate.clamp(0.0, 1.0);

    // Per-person draw in [0,1).
    let draw: Vec<f64> = (0..n)
        .map(|i| {
            (((i as i64 + 1) * 1103515245 + seed * 1009 + salt).rem_euclid(1000) as f64) / 1000.0
        })
        .collect();

    let mut assigned = vec![0i32; n];
    // Iterate states in ascending order to match R's sort(unique()).
    let mut states: Vec<i32> = ps.clone();
    states.sort_unstable();
    states.dedup();
    for st in states {
        let idx: Vec<usize> = (0..n).filter(|&i| ps[i] == st).collect();
        let state_pool: Vec<usize> = (0..n_businesses).filter(|&b| bs[b] == (st)).collect();
        let pool_len = state_pool.len();
        // Partition idx into same-state vs other, preserving order.
        let mut state_rank: i64 = 0;
        let mut other_rank: i64 = 0;
        for &i in &idx {
            let use_state = pool_len > 0 && draw[i] < rate;
            if use_state {
                state_rank += 1;
                let pick = ((state_rank * 2_654_435_761 + seed * 37 + salt + (st as i64) * 9176)
                    .rem_euclid(pool_len as i64)) as usize;
                assigned[i] = (state_pool[pick] + 1) as i32; // 1-based business index
            } else {
                other_rank += 1;
                let pick = ((other_rank * 2_246_822_519 + seed * 101 + salt + (st as i64) * 3571)
                    .rem_euclid(n_businesses as i64)) as usize;
                assigned[i] = (pick + 1) as i32;
            }
        }
    }
    // Optionally bump assignments that collide with `avoid` (used to keep a
    // secondary job at a different business from the primary).
    if let Some(av) = avoid {
        if av.len() == n && n_businesses > 1 {
            for i in 0..n {
                if assigned[i] == av[i] {
                    assigned[i] = (assigned[i].rem_euclid(n_businesses as i32)) + 1;
                }
            }
        }
    }
    assigned
}

/// `.blade_anzsic06`: 4-digit ANZSIC06 code for a business given its industry.
pub fn anzsic06_code(industry: i32, seq_business: i64, seed: i64) -> String {
    const BASE: [i64; 19] = [
        111, 600, 1111, 2611, 3011, 3211, 3911, 4511, 4610, 5801, 6221, 6711, 6910, 7211, 7510,
        8010, 8401, 8910, 9201,
    ];
    let ind = norm_industry(industry);
    if ind == 17 {
        const HEALTH: [&str; 10] = [
            "8401", "8511", "8520", "8531", "8532", "8533", "8534", "8591", "8601", "8790",
        ];
        let k = (seq_business + seed).rem_euclid(HEALTH.len() as i64) as usize;
        HEALTH[k].to_string()
    } else {
        let v = BASE[(ind - 1) as usize] + (seq_business * 7 + seed).rem_euclid(20);
        format!("{:04}", v)
    }
}

pub const HEALTH_OCC_CODE: [&str; 10] = [
    "253111", "253112", "253999", "254411", "254412", "254499", "251211", "252411", "423111",
    "411711",
];
pub const HEALTH_OCC_TITLE: [&str; 10] = [
    "General Practitioner",
    "Resident Medical Officer",
    "Medical Practitioners nec",
    "Nurse Practitioner",
    "Registered Nurse (Aged Care)",
    "Registered Nurses nec",
    "Medical Diagnostic Radiographer",
    "Occupational Therapist",
    "Aged or Disabled Carer",
    "Community Worker",
];

/// Group-max broadcast (R `ave(x, group, FUN = max)`).
fn ave_max(x: &[i32], group: &[i64]) -> Vec<bool> {
    use std::collections::HashMap;
    let mut gmax: HashMap<i64, i32> = HashMap::new();
    for (i, &g) in group.iter().enumerate() {
        let e = gmax.entry(g).or_insert(i32::MIN);
        if x[i] > *e {
            *e = x[i];
        }
    }
    group.iter().map(|g| gmax[g] > 0).collect()
}

fn opt_str(v: &Option<Vec<String>>, idx: usize, default: &str) -> String {
    match v {
        Some(col) => {
            let s = &col[idx];
            if s.is_empty() || s == "NA" {
                default.to_string()
            } else {
                s.clone()
            }
        }
        None => default.to_string(),
    }
}

/// Build the full business spine. Returns a named list (one column per name).
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn make_blade_business_spine__(
    spine_state: &[i32],
    spine_industry: Nullable<Integers>,
    spine_sector: Nullable<Integers>,
    baseline_employed: &[i32],
    baseline_income: &[f64],
    spine_id: Strings,
    aeuid_ato: Nullable<Strings>,
    anzsco_code: Nullable<Strings>,
    anzsco_title: Nullable<Strings>,
    seed: i32,
    n_businesses: Nullable<i32>,
) -> List {
    let seed = seed as i64;
    let n_persons = spine_state.len();
    if n_persons == 0 {
        panic!("Cannot create a BLADE business spine from an empty person spine.");
    }

    let industry_col: Option<Vec<i32>> = match spine_industry {
        Nullable::NotNull(x) => Some(x.iter().map(|v| Option::<i32>::from(v).unwrap_or(i32::MIN)).collect()),
        Nullable::Null => None,
    };
    let sector_col: Option<Vec<i32>> = match spine_sector {
        Nullable::NotNull(x) => Some(x.iter().map(|v| Option::<i32>::from(v).unwrap_or(i32::MIN)).collect()),
        Nullable::Null => None,
    };
    let aeuid_ato_col: Option<Vec<String>> = match aeuid_ato {
        Nullable::NotNull(x) => Some(x.iter().map(|s| s.to_string()).collect()),
        Nullable::Null => None,
    };
    let anzsco_code_col: Option<Vec<String>> = match anzsco_code {
        Nullable::NotNull(x) => Some(x.iter().map(|s| s.to_string()).collect()),
        Nullable::Null => None,
    };
    let anzsco_title_col: Option<Vec<String>> = match anzsco_title {
        Nullable::NotNull(x) => Some(x.iter().map(|s| s.to_string()).collect()),
        Nullable::Null => None,
    };
    let spine_id_v: Vec<Option<String>> = spine_id
        .iter()
        .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
        .collect();

    let nb = match n_businesses {
        Nullable::NotNull(x) => x as i64,
        Nullable::Null => default_business_count(baseline_employed, baseline_income),
    };
    assert!(nb > 0);
    let nb_us = nb as usize;

    // Per-business base attributes from a representative person.
    let seq: Vec<i64> = (1..=nb).collect();
    let bg_group: Vec<i64> = seq.iter().map(|&s| (s - 1) / 4 + 1).collect();
    let mut rep_idx: Vec<usize> = seq
        .iter()
        .map(|&s| ((s * 7919 + seed * 101).rem_euclid(n_persons as i64)) as usize)
        .collect();

    let mut state: Vec<i32> = rep_idx
        .iter()
        .map(|&i| norm_state(spine_state[i]))
        .collect();
    let mut industry: Vec<i32> = match &industry_col {
        Some(col) => rep_idx.iter().map(|&i| norm_industry(col[i])).collect(),
        None => seq
            .iter()
            .map(|&s| ((s + seed).rem_euclid(19) + 1) as i32)
            .collect(),
    };
    let mut sector: Vec<i32> = match &sector_col {
        Some(col) => rep_idx
            .iter()
            .map(|&i| if col[i] == i32::MIN { 1 } else { col[i] })
            .collect(),
        None => vec![1; nb_us],
    };

    // Employee assignment + headcount / wage reductions.
    let mut employment_count = vec![0i32; nb_us];
    let mut annual_wages = vec![0f64; nb_us];
    let employed_idx: Vec<usize> = (0..n_persons)
        .filter(|&i| {
            baseline_employed[i] == 1 && !baseline_income[i].is_nan() && baseline_income[i] > 0.0
        })
        .collect();
    if !employed_idx.is_empty() {
        let emp_state: Vec<i32> = employed_idx.iter().map(|&i| spine_state[i]).collect();
        let assigned = assign_business_by_state(&emp_state, &state, seed, 11, 0.92, None);
        // tabulate + rowsum
        let mut first_pos: Vec<Option<usize>> = vec![None; nb_us];
        for (k, &b) in assigned.iter().enumerate() {
            let bi = (b - 1) as usize;
            employment_count[bi] += 1;
            annual_wages[bi] += baseline_income[employed_idx[k]];
            if first_pos[bi].is_none() {
                first_pos[bi] = Some(k);
            }
        }
        // Employee-representative override.
        for bi in 0..nb_us {
            if let Some(k) = first_pos[bi] {
                let rep = employed_idx[k];
                state[bi] = norm_state(spine_state[rep]);
                if let Some(col) = &industry_col {
                    industry[bi] = norm_industry(col[rep]);
                }
                if let Some(col) = &sector_col {
                    sector[bi] = if col[rep] == i32::MIN { 1 } else { col[rep] };
                }
                rep_idx[bi] = rep;
            }
        }
    }

    // Health forcing (per bg_group).
    let mut force_health: Vec<bool> = bg_group
        .iter()
        .map(|&g| (g + seed).rem_euclid(9) == 0)
        .collect();
    if !force_health.iter().any(|&f| f) && nb_us >= 5 {
        let target_g = bg_group[(nb_us as f64 / 2.0).ceil() as usize - 1];
        for bi in 0..nb_us {
            if bg_group[bi] == target_g {
                force_health[bi] = true;
            }
        }
    }
    for bi in 0..nb_us {
        if force_health[bi] {
            industry[bi] = 17;
        }
    }
    let is17: Vec<i32> = industry.iter().map(|&i| (i == 17) as i32).collect();
    let health_group = ave_max(&is17, &bg_group);
    for bi in 0..nb_us {
        if health_group[bi] {
            industry[bi] = 17;
        }
    }

    let industry_division: Vec<String> = industry
        .iter()
        .map(|&i| ((b'A' + (i as u8) - 1) as char).to_string())
        .collect();
    let anzsic06: Vec<String> = (0..nb_us)
        .map(|bi| anzsic06_code(industry[bi], seq[bi], seed))
        .collect();

    // Birth/exit years.
    let mut birth_year: Vec<Option<i32>> = seq
        .iter()
        .map(|&s| Some(1992 + (s * 37 + seed).rem_euclid(33) as i32))
        .collect();
    for bi in 0..nb_us {
        let s = seq[bi];
        if (s + seed).rem_euclid(11) == 0 {
            birth_year[bi] = Some(1993);
        }
        if (s * 3 + seed).rem_euclid(13) == 0 {
            birth_year[bi] = Some(2001);
        }
        if (s * 7 + seed).rem_euclid(23) == 0 {
            birth_year[bi] = None;
        }
    }
    let exit_year: Vec<Option<i32>> = (0..nb_us)
        .map(|bi| {
            let s = seq[bi];
            let exit_draw = (s * 53 + seed).rem_euclid(100);
            let exit_birth = birth_year[bi].unwrap_or(2001);
            if exit_draw < 12 && exit_birth < 2020 {
                let span = (2025 - exit_birth).max(1) as i64;
                let v = exit_birth + 1 + (s * 17 + seed).rem_euclid(span) as i32;
                Some(v.min(2024))
            } else {
                None
            }
        })
        .collect();
    let alive_status: Vec<i32> = exit_year
        .iter()
        .map(|e| match e {
            None => 1,
            Some(y) if *y >= 2024 => 1,
            _ => 0,
        })
        .collect();

    // Turnover, profiling, legal form, SISCA, hcnt.
    let turnover: Vec<f64> = (0..nb_us)
        .map(|bi| {
            let non_emp = 30000.0 + (seq[bi] * 7919 + seed).rem_euclid(170000) as f64;
            let base = (annual_wages[bi] * (1.65 + industry[bi] as f64 / 25.0)).max(
                if employment_count[bi] > 0 {
                    annual_wages[bi] * 1.25
                } else {
                    non_emp
                },
            );
            round2(base)
        })
        .collect();

    let profiled_seed: Vec<i32> = (0..nb_us)
        .map(|bi| {
            (employment_count[bi] >= 8
                || turnover[bi] >= 1e6
                || (bg_group[bi] + seed).rem_euclid(5) == 0) as i32
        })
        .collect();
    let mut profiled = ave_max(&profiled_seed, &bg_group);
    // Groups of size < 2 cannot be profiled.
    let mut gcount: std::collections::HashMap<i64, i32> = std::collections::HashMap::new();
    for &g in &bg_group {
        *gcount.entry(g).or_insert(0) += 1;
    }
    for bi in 0..nb_us {
        if gcount[&bg_group[bi]] < 2 {
            profiled[bi] = false;
        }
    }

    let public_sector: Vec<bool> = (0..nb_us)
        .map(|bi| industry_division[bi] == "Q" && (seq[bi] + seed).rem_euclid(4) == 0)
        .collect();
    let legal_form: Vec<String> = (0..nb_us)
        .map(|bi| {
            let lf = if employment_count[bi] == 0 || (seq[bi] + seed).rem_euclid(7) == 0 {
                "Sole trader"
            } else {
                ["Company", "Partnership", "Trust"][((seq[bi] + seed).rem_euclid(3)) as usize]
            };
            if industry_division[bi] == "Q" && !public_sector[bi] {
                "Company".to_string()
            } else {
                lf.to_string()
            }
        })
        .collect();
    let tolo: Vec<i32> = legal_form
        .iter()
        .map(|lf| match lf.as_str() {
            "Company" => 1,
            "Sole trader" => 2,
            "Partnership" => 3,
            "Trust" => 4,
            _ => i32::MIN,
        })
        .collect();
    let x_sector: Vec<i32> = public_sector
        .iter()
        .map(|&p| if p { 2 } else { 1 })
        .collect();
    let sisca08: Vec<String> = (0..nb_us)
        .map(|bi| {
            if public_sector[bi] {
                "3000"
            } else if legal_form[bi] == "Sole trader" {
                "4000"
            } else if (seq[bi] + seed).rem_euclid(19) == 0 {
                "1001"
            } else {
                "1009"
            }
            .to_string()
        })
        .collect();
    let sisca_sector: Vec<String> = (0..nb_us)
        .map(|bi| {
            if public_sector[bi] {
                "Public"
            } else if legal_form[bi] == "Sole trader" {
                "Household business"
            } else {
                "Private"
            }
            .to_string()
        })
        .collect();
    let hcnt: Vec<i32> = (0..nb_us)
        .map(|bi| {
            let delta = if employment_count[bi] > 0 && (seq[bi] + seed).rem_euclid(5) == 0 {
                [-1, 1, 2][((seq[bi] + seed).rem_euclid(3)) as usize]
            } else {
                0
            };
            (employment_count[bi] + delta).max(0)
        })
        .collect();
    let fte: Vec<f64> = (0..nb_us)
        .map(|bi| {
            ((hcnt[bi] as f64 * (0.68 + (seq[bi] + seed).rem_euclid(23) as f64 / 100.0)).max(0.0)
                * 10.0)
                .round()
                / 10.0
        })
        .collect();

    // Representative ANZSCO (health override).
    let mut rep_anzsco_code: Vec<String> = rep_idx
        .iter()
        .map(|&i| normalise_anzsco(Some(&opt_str(&anzsco_code_col, i, "000000"))))
        .collect();
    let mut rep_anzsco_title: Vec<String> = rep_idx
        .iter()
        .map(|&i| opt_str(&anzsco_title_col, i, "Occupation not stated"))
        .collect();
    let force_rep_health: Vec<bool> = industry_division.iter().map(|d| d == "Q").collect();
    let mut hcount = 0i64;
    for bi in 0..nb_us {
        if force_rep_health[bi] {
            hcount += 1;
            let k = ((hcount + seed).rem_euclid(HEALTH_OCC_CODE.len() as i64)) as usize;
            rep_anzsco_code[bi] = HEALTH_OCC_CODE[k].to_string();
            rep_anzsco_title[bi] = HEALTH_OCC_TITLE[k].to_string();
        }
    }

    // Identifiers.
    let blade_business_id: Vec<String> =
        seq.iter().map(|&s| numeric_id("B", s as i128, 9)).collect();
    let bn: Vec<String> = seq
        .iter()
        .map(|&s| {
            numeric_id(
                "BN",
                ((s * 1_000_003 + seed * 9176).rem_euclid(100_000_000_000)) as i128,
                11,
            )
        })
        .collect();
    let id: Vec<String> = seq
        .iter()
        .map(|&s| {
            numeric_id(
                "E",
                ((s * 100_003 + seed).rem_euclid(1_000_000_000)) as i128,
                9,
            )
        })
        .collect();
    let bg_id: Vec<String> = (0..nb_us)
        .map(|bi| {
            if profiled[bi] {
                numeric_id(
                    "BG",
                    ((bg_group[bi] + seed * 13).rem_euclid(10_000_000_000)) as i128,
                    10,
                )
            } else {
                String::new()
            }
        })
        .collect();
    let cn: Vec<String> = seq
        .iter()
        .map(|&s| {
            numeric_id(
                "C",
                ((s * 300_007 + seed).rem_euclid(1_000_000_000)) as i128,
                9,
            )
        })
        .collect();
    let fnum: Vec<String> = seq
        .iter()
        .map(|&s| {
            numeric_id(
                "F",
                ((s * 700_001 + seed).rem_euclid(1_000_000_000)) as i128,
                9,
            )
        })
        .collect();
    let rep_spine_id: Vec<Option<String>> =
        rep_idx.iter().map(|&i| spine_id_v[i].clone()).collect();
    let rep_aeuid_ato: Vec<Option<String>> = rep_idx
        .iter()
        .map(|&i| aeuid_ato_col.as_ref().map(|c| c[i].clone()))
        .collect();

    let x_st_op: Vec<String> = (0..nb_us)
        .map(|bi| format!("{:09}", state[bi] as i64 * 10_000_000 + seq[bi]))
        .collect();
    let x_pcode: Vec<String> = (0..nb_us)
        .map(|bi| {
            format!(
                "{:04}",
                2000 + (state[bi] as i64 * 173 + seq[bi]).rem_euclid(7000)
            )
        })
        .collect();
    let x_npi: Vec<i32> = public_sector
        .iter()
        .map(|&p| if p { 1 } else { 2 })
        .collect();

    // Derived financials.
    let bit_total_income: Vec<f64> = turnover.iter().map(|&t| round2(t * 0.97)).collect();
    let bit_taxable_income: Vec<f64> = (0..nb_us)
        .map(|bi| round2((turnover[bi] - annual_wages[bi] * 1.08).max(0.0)))
        .collect();
    let gst_payable: Vec<f64> = (0..nb_us)
        .map(|bi| round2((turnover[bi] * 0.1 - annual_wages[bi] * 0.015).max(0.0)))
        .collect();
    let capex: Vec<f64> = (0..nb_us)
        .map(|bi| round2(turnover[bi] * (0.02 + industry[bi] as f64 / 1000.0)))
        .collect();
    let rd: Vec<f64> = (0..nb_us)
        .map(|bi| {
            round2(if matches!(industry[bi], 3 | 10 | 11 | 18) {
                turnover[bi] * 0.025
            } else {
                turnover[bi] * 0.004
            })
        })
        .collect();
    let export_v: Vec<f64> = (0..nb_us)
        .map(|bi| {
            round2(if matches!(industry[bi], 1 | 2 | 3 | 11) {
                turnover[bi] * 0.12
            } else {
                turnover[bi] * 0.015
            })
        })
        .collect();
    let import_v: Vec<f64> = (0..nb_us)
        .map(|bi| {
            round2(if matches!(industry[bi], 3 | 7 | 9) {
                turnover[bi] * 0.10
            } else {
                turnover[bi] * 0.02
            })
        })
        .collect();

    let annual_wages_r: Vec<f64> = annual_wages.iter().map(|&w| round2(w)).collect();
    let payg_gap: Vec<i32> = (0..nb_us)
        .map(|bi| hcnt[bi] - employment_count[bi])
        .collect();
    let mismatch: Vec<i32> = (0..nb_us)
        .map(|bi| (hcnt[bi] != employment_count[bi]) as i32)
        .collect();
    let private_public: Vec<String> = public_sector
        .iter()
        .map(|&p| if p { "public" } else { "private" }.to_string())
        .collect();
    let health_flag: Vec<i32> = industry_division
        .iter()
        .map(|d| (d == "Q") as i32)
        .collect();
    let public_health_flag: Vec<i32> = public_sector.iter().map(|&p| p as i32).collect();
    let rep_health_flag: Vec<i32> = force_rep_health.iter().map(|&f| f as i32).collect();
    let is_employing: Vec<i32> = employment_count.iter().map(|&c| (c > 0) as i32).collect();
    let is_profiled: Vec<i32> = profiled.iter().map(|&p| p as i32).collect();

    list!(
        blade_business_id = blade_business_id,
        bn = bn,
        id = id,
        bg_id = bg_id.clone(),
        cn = cn,
        fn_ = fnum,
        representative_spine_id = rep_spine_id,
        representative_aeuid_ato = rep_aeuid_ato,
        state = state.clone(),
        state_code = state.clone(),
        anzsic06 = anzsic06.clone(),
        industry_division = industry_division.clone(),
        d_div06 = industry_division.clone(),
        latest_div06 = industry_division.clone(),
        x_anzsic06 = anzsic06.clone(),
        x_anzsic93 = anzsic06.clone(),
        d_anzsic06 = anzsic06.clone(),
        cast_anzsic06 = anzsic06.clone(),
        latest_anzsic06 = anzsic06.clone(),
        x_sisca08 = sisca08.clone(),
        x_sisca06 = sisca08.clone(),
        x_sisca93 = sisca08.clone(),
        d_sisca08 = sisca08.clone(),
        cast_sisca08 = sisca08.clone(),
        latest_sisca08 = sisca08.clone(),
        sisca_sector = sisca_sector,
        x_sector = x_sector.clone(),
        cast_sector = x_sector.clone(),
        x_state = state.clone(),
        cast_state = state.clone(),
        x_st_op = x_st_op.clone(),
        cast_st_op = x_st_op,
        x_al_st = alive_status.clone(),
        x_pcode = x_pcode.clone(),
        cast_pcode = x_pcode,
        x_npi = x_npi.clone(),
        cast_npi = x_npi,
        industry = industry,
        sector = sector,
        business_birth_year = birth_year,
        business_exit_year = exit_year,
        alive_status = alive_status,
        employment_count = employment_count.clone(),
        payg_employee_count = employment_count.clone(),
        stp_employee_count = employment_count.clone(),
        annual_wages = annual_wages_r.clone(),
        hcnt = hcnt.clone(),
        fte = fte,
        payg_reported_hcnt = hcnt.clone(),
        payg_actual_hcnt = employment_count.clone(),
        linked_payg_rows = employment_count.clone(),
        linked_distinct_persons = employment_count.clone(),
        payg_link_hcnt_gap = payg_gap.clone(),
        payg_hcnt_delta = payg_gap,
        payg_headcount_mismatch = mismatch,
        d_total_payees = employment_count.clone(),
        turnover = turnover.clone(),
        bas_total_sales = turnover,
        bas_wages = annual_wages_r,
        bit_total_income = bit_total_income,
        bit_taxable_income = bit_taxable_income,
        gst_payable = gst_payable,
        capital_expenditure = capex,
        rd_expenditure = rd,
        export_value = export_v,
        import_value = import_v,
        legal_form = legal_form,
        x_tolo = tolo.clone(),
        cast_tolo = tolo.clone(),
        tolo = tolo,
        private_public = private_public,
        health_industry_flag = health_flag,
        public_health_flag = public_health_flag,
        representative_anzsco_code = rep_anzsco_code,
        representative_anzsco_title = rep_anzsco_title,
        representative_health_occupation_flag = rep_health_flag,
        is_employing = is_employing,
        is_profiled = is_profiled,
        source_person_n = employment_count
    )
}

extendr_module! {
    mod business_spine;
    fn make_blade_business_spine__;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assigner_concordance() {
        // 100 people, 20 businesses, states 1-3.
        let ps: Vec<i32> = (0..100).map(|i| (i % 3) + 1).collect();
        let bs: Vec<i32> = (0..20).map(|i| (i % 3) + 1).collect();
        let a = assign_business_by_state(&ps, &bs, 42, 11, 0.92, None);
        assert_eq!(a.len(), 100);
        assert!(a.iter().all(|&b| b >= 1 && b <= 20));
        // > 85% same-state
        let same = (0..100)
            .filter(|&i| bs[(a[i] - 1) as usize] == ps[i])
            .count();
        assert!(same as f64 / 100.0 > 0.85, "concordance {}", same);
    }

    #[test]
    fn anzsic_health() {
        assert_eq!(anzsic06_code(17, 1, 0).len(), 4);
        assert!(anzsic06_code(1, 1, 0).chars().all(|c| c.is_ascii_digit()));
    }
}

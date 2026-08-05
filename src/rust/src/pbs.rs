use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use rayon::prelude::*;

use crate::mbs::{
    days_since_epoch, observable_days_in_year, person_dob_days, person_year_window,
};
use crate::sampling::{normal_sample, weighted_sample};

// ==========================================================================
// Constants (from inst/foundations/pbs.toml)
// ==========================================================================

const AGE_BREAKS: [i32; 6] = [0, 15, 25, 45, 65, 75];
const AGE_RATES: [f64; 6] = [3.0, 4.0, 7.0, 12.0, 22.0, 30.0];
const FEMALE_MULT: f64 = 1.15;

const PART_CHILD: f64 = 0.60;
const PART_ADULT: f64 = 0.75;
const PART_ELDERLY: f64 = 0.95;

// ATC level 1 shares
const ATC1_CODES: [&str; 14] = [
    "A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R", "S", "V",
];
const ATC1_SHARES: [f64; 14] = [
    0.13, 0.05, 0.21, 0.02, 0.04, 0.03, 0.07, 0.04, 0.05, 0.20, 0.01, 0.08, 0.03, 0.01,
];

// Copayment thresholds (2024-25)
const GENERAL_COPAY: f64 = 25.00;
const CONCESSIONAL_COPAY: f64 = 7.70;

// Concessional share by age
const CONC_CHILD: f64 = 0.40;
const CONC_ADULT: f64 = 0.35;
const CONC_ELDERLY: f64 = 0.85;

// Prescriber type weights
const PRESCRIBER_TYPES: [&str; 2] = ["M", "N"];
const PRESCRIBER_WEIGHTS: [f64; 2] = [0.97, 0.03];

// Specialty
const SPECIALTY_CODES: [&str; 3] = ["GP", "SP", "OT"];
const SPECIALTY_WEIGHTS: [f64; 3] = [0.75, 0.20, 0.05];

// Pharmacy
const PHARMACY_TYPES: [&str; 2] = ["A", "F"];
const PHARMACY_WEIGHTS: [f64; 2] = [0.95, 0.05];

// Previous supply distribution
const PREV_SUPPLY_PROBS: [f64; 7] = [0.55, 0.15, 0.10, 0.08, 0.06, 0.04, 0.02];

const REG24_RATE: f64 = 0.01;
const CTG_RATE: f64 = 0.03;

// State postcode ranges (same as MBS)
const STATE_CODES: [i32; 8] = [1, 2, 3, 4, 5, 6, 7, 8];
const STATE_PC_LO: [i32; 8] = [2000, 3000, 4000, 5000, 6000, 7000, 800, 2600];
const STATE_PC_HI: [i32; 8] = [2999, 3999, 4999, 5799, 6797, 7999, 899, 2618];

// ==========================================================================
// PBS item table
// ==========================================================================

struct PbsItemTable {
    pbs_code: Vec<String>,
    atc_level1: Vec<String>,
    benefit_type: Vec<String>,
    claimed_price: Vec<f64>,
    pack_size: Vec<i32>,
    number_of_repeats: Vec<i32>,
    weight: Vec<f64>,
    // Pre-computed per ATC1 level
    atc1_indices: Vec<Vec<usize>>,
    atc1_weights: Vec<Vec<f64>>,
    atc1_map: Vec<(String, usize)>, // ATC1 code → index
}

impl PbsItemTable {
    fn from_vectors(
        pbs_code: &[String],
        atc_level1: &[String],
        benefit_type: &[String],
        claimed_price: &[f64],
        pack_size: &[i32],
        number_of_repeats: &[i32],
        weight: &[f64],
    ) -> Self {
        let n = pbs_code.len();

        // Build per-ATC1 indices
        let mut atc1_indices: Vec<Vec<usize>> = vec![Vec::new(); ATC1_CODES.len()];
        let mut atc1_raw_weights: Vec<Vec<f64>> = vec![Vec::new(); ATC1_CODES.len()];

        let atc1_map: Vec<(String, usize)> = ATC1_CODES
            .iter()
            .enumerate()
            .map(|(i, &code)| (code.to_string(), i))
            .collect();

        for row in 0..n {
            let atc1 = &atc_level1[row];
            for &(ref code, idx) in &atc1_map {
                if atc1 == code {
                    atc1_indices[idx].push(row);
                    atc1_raw_weights[idx].push(weight[row]);
                    break;
                }
            }
        }

        let atc1_weights: Vec<Vec<f64>> = atc1_raw_weights
            .iter()
            .map(|ws| {
                let total: f64 = ws.iter().sum();
                if total > 0.0 {
                    ws.iter().map(|w| w / total).collect()
                } else {
                    ws.clone()
                }
            })
            .collect();

        PbsItemTable {
            pbs_code: pbs_code.to_vec(),
            atc_level1: atc_level1.to_vec(),
            benefit_type: benefit_type.to_vec(),
            claimed_price: claimed_price.to_vec(),
            pack_size: pack_size.to_vec(),
            number_of_repeats: number_of_repeats.to_vec(),
            weight: weight.to_vec(),
            atc1_indices,
            atc1_weights,
            atc1_map,
        }
    }

}

// Helpers
// ==========================================================================

fn age_band_index(age: i32) -> usize {
    if age < 15 {
        0
    } else if age < 25 {
        1
    } else if age < 45 {
        2
    } else if age < 65 {
        3
    } else if age < 75 {
        4
    } else {
        5
    }
}

fn healthcare_usage_multiplier(
    year: i32,
    age: i32,
    baseline_income: f64,
    baseline_employed: i32,
    disability_onset_year: i32,
    disability_severity: i32,
    disability_is_dc: i32,
) -> f64 {
    let mut mult = 1.0;

    let has_disability = disability_onset_year != i32::MIN && disability_onset_year <= year;
    let has_severity = disability_severity != i32::MIN;
    let severe_disability = has_disability && has_severity && disability_severity <= 2;
    let other_disability = has_disability && has_severity && disability_severity > 2;

    if severe_disability {
        mult *= if disability_is_dc == 1 { 2.30 } else { 2.00 };
    } else if other_disability {
        mult *= 1.25;
    }

    let low_income =
        baseline_income.is_finite() && baseline_income >= 0.0 && baseline_income < 40_000.0;
    let weak_labour_attachment = baseline_employed != 1 || baseline_income < 25_000.0;
    if (16..65).contains(&age) && low_income && weak_labour_attachment {
        mult *= 1.10;
    }

    mult
}

fn poisson_sample(rng: &mut StdRng, lambda: f64) -> u32 {
    if lambda < 30.0 {
        let l = (-lambda).exp();
        let mut k: u32 = 0;
        let mut p: f64 = 1.0;
        loop {
            k += 1;
            p *= rng.gen::<f64>();
            if p < l {
                return k - 1;
            }
        }
    } else {
        let z = normal_sample(rng, lambda, lambda.sqrt());
        z.round().max(0.0) as u32
    }
}

#[inline]
fn random_positive_weight_row(rng: &mut StdRng, weights: &[f64], upper: usize) -> usize {
    let upper = upper.min(weights.len());
    for _ in 0..16 {
        let row = rng.gen_range(0..upper);
        if weights[row] > 0.0 {
            return row;
        }
    }
    weights
        .iter()
        .take(upper)
        .position(|w| *w > 0.0)
        .unwrap_or(0)
}

fn postcode_for_state(rng: &mut StdRng, state: i32) -> String {
    for i in 0..STATE_CODES.len() {
        if STATE_CODES[i] == state {
            let pc = rng.gen_range(STATE_PC_LO[i]..=STATE_PC_HI[i]);
            return format!("{:04}", pc);
        }
    }
    let pc = rng.gen_range(2000..=6999);
    format!("{:04}", pc)
}

// ==========================================================================
// Main generation: one year of PBS dispensings
// ==========================================================================

/// Output struct for PBS year generation (shared by list and parquet entry points).
struct PbsYearData {
    aeuid: Vec<String>,
    brth_mth: Vec<i32>,
    brth_yr: Vec<i32>,
    sex_cd: Vec<String>,
    pstcd: Vec<String>,
    ctgry: Vec<String>,
    itm: Vec<String>,
    drg_typ: Vec<String>,
    prscrb_dt: Vec<i32>,
    spply_dt: Vec<i32>,
    extrct_dt: Vec<i32>,
    prscrptn_cnt: Vec<i32>,
    rpt_ordr: Vec<i32>,
    prvs_spply: Vec<i32>,
    qty: Vec<i32>,
    srt_rpt: Vec<String>,
    ptnt_cntrbtn: Vec<f64>,
    bnft: Vec<f64>,
    ctg_bnft: Vec<f64>,
    ctg_cd: Vec<String>,
    prscrbr_id: Vec<String>,
    prscrbr_typ: Vec<String>,
    prscrbr_pc: Vec<String>,
    spec: Vec<String>,
    phrmcy_id: Vec<String>,
    phrmcy_pc: Vec<String>,
    phrmcy_typ: Vec<String>,
    rgltn24: Vec<String>,
    strmlnd: Vec<Option<String>>,
    undr_co: Vec<Option<String>>,
}

/// Internal generator — produces raw Vec columns without wrapping in List.
fn generate_pbs_year_impl(
    aeuid: &Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    sex: &[i32],
    state: &[i32],
    first_year: &[i32],
    last_year: &[i32],
    pbs_code: &Strings,
    atc_level1: &Strings,
    benefit_type: &Strings,
    claimed_price: &[f64],
    pack_size: &[i32],
    number_of_repeats: &[i32],
    item_weight: &[f64],
    prescriber_pool: &Strings,
    pharmacy_pool: &Strings,
    year: i32,
    seed: i64,
) -> Option<PbsYearData> {
    let n_participants = birth_year.len();

    let pbs_code_vec: Vec<String> = pbs_code.iter().map(|s| s.to_string()).collect();
    let atc1_vec: Vec<String> = atc_level1.iter().map(|s| s.to_string()).collect();
    let bt_vec: Vec<String> = benefit_type.iter().map(|s| s.to_string()).collect();

    let items = PbsItemTable::from_vectors(
        &pbs_code_vec,
        &atc1_vec,
        &bt_vec,
        claimed_price,
        pack_size,
        number_of_repeats,
        item_weight,
    );

    let prsc_vec: Vec<&str> = prescriber_pool.iter().map(|s| s.as_ref()).collect();
    let phar_vec: Vec<&str> = pharmacy_pool.iter().map(|s| s.as_ref()).collect();

    let aeuid_owned: Vec<String> = aeuid.iter().map(|s| s.to_string()).collect();

    let alive_idx: Vec<usize> = (0..n_participants)
        .filter(|&i| first_year[i] <= year && last_year[i] >= year)
        .collect();

    let n_alive = alive_idx.len();
    if n_alive == 0 {
        return None;
    }

    let mut avail_atc_indices: Vec<usize> = Vec::new();
    let mut avail_atc_shares: Vec<f64> = Vec::new();
    for (i, _code) in ATC1_CODES.iter().enumerate() {
        if !items.atc1_indices[i].is_empty() {
            avail_atc_indices.push(i);
            avail_atc_shares.push(ATC1_SHARES[i]);
        }
    }
    let atc_share_total: f64 = avail_atc_shares.iter().sum();
    let norm_atc_shares: Vec<f64> = avail_atc_shares
        .iter()
        .map(|s| s / atc_share_total)
        .collect();

    let n_threads = rayon::current_num_threads().max(1);
    let chunk_target = ((n_alive + n_threads - 1) / n_threads).max(1);
    let n_chunks = (n_alive + chunk_target - 1) / chunk_target;

    let alive_idx_ref: &[usize] = &alive_idx;
    let items_ref: &PbsItemTable = &items;
    let prsc_ref: &[&str] = &prsc_vec;
    let phar_ref: &[&str] = &phar_vec;
    let aeuid_ref: &[String] = &aeuid_owned;
    let birth_year_ref: &[i32] = birth_year;
    let month_of_birth_ref: &[i32] = month_of_birth;
    let sex_ref: &[i32] = sex;
    let state_ref: &[i32] = state;
    let atc_indices_ref: &[usize] = &avail_atc_indices;
    let atc_shares_ref: &[f64] = &norm_atc_shares;

    let chunk_results: Vec<PbsChunk> = (0..n_chunks)
        .into_par_iter()
        .map(|ci| {
            let start = ci * chunk_target;
            let end = ((ci + 1) * chunk_target).min(n_alive);
            let chunk_seed = (seed as u64).wrapping_add((ci as u64).wrapping_mul(2_654_435_761));
            let mut rng = StdRng::seed_from_u64(chunk_seed);
            let mut chunk = PbsChunk::default();

            for &pi in &alive_idx_ref[start..end] {
                let age = year - birth_year_ref[pi];
                let band = age_band_index(age);
                let mut rate = AGE_RATES[band];
                if sex_ref[pi] == 2 {
                    rate *= FEMALE_MULT;
                }
                // Only the post-birth part of the birth year is observable.
                let person_byr = birth_year_ref[pi];
                // Guard against a missing spine month (extendr renders R
                // `NA_integer_` as `i32::MIN`); January is the fallback.
                let person_brth_mth = month_of_birth_ref[pi].clamp(1, 12);
                let (window_start, window_days, obs_frac) =
                    person_year_window(year, person_byr, person_brth_mth);
                let dob_days = person_dob_days(person_byr, person_brth_mth);
                rate *= obs_frac;
                let count = poisson_sample(&mut rng, rate) as usize;
                if count == 0 {
                    continue;
                }

                let person_aeuid = &aeuid_ref[pi];
                let person_sex = sex_ref[pi];
                let person_state = state_ref[pi];

                for _ in 0..count {
                    generate_one_dispensing(
                        &mut rng,
                        items_ref,
                        prsc_ref,
                        phar_ref,
                        atc_indices_ref,
                        atc_shares_ref,
                        year,
                        window_start,
                        window_days,
                        dob_days,
                        person_aeuid,
                        person_byr,
                        person_brth_mth,
                        person_sex,
                        person_state,
                        &mut chunk,
                    );
                }
            }

            chunk
        })
        .collect();

    let total_disp: usize = chunk_results.iter().map(|c| c.aeuid.len()).sum();
    if total_disp == 0 {
        return None;
    }

    let mut d = PbsYearData {
        aeuid: Vec::with_capacity(total_disp),
        brth_mth: Vec::with_capacity(total_disp),
        brth_yr: Vec::with_capacity(total_disp),
        sex_cd: Vec::with_capacity(total_disp),
        pstcd: Vec::with_capacity(total_disp),
        ctgry: Vec::with_capacity(total_disp),
        itm: Vec::with_capacity(total_disp),
        drg_typ: Vec::with_capacity(total_disp),
        prscrb_dt: Vec::with_capacity(total_disp),
        spply_dt: Vec::with_capacity(total_disp),
        extrct_dt: Vec::with_capacity(total_disp),
        prscrptn_cnt: Vec::with_capacity(total_disp),
        rpt_ordr: Vec::with_capacity(total_disp),
        prvs_spply: Vec::with_capacity(total_disp),
        qty: Vec::with_capacity(total_disp),
        srt_rpt: Vec::with_capacity(total_disp),
        ptnt_cntrbtn: Vec::with_capacity(total_disp),
        bnft: Vec::with_capacity(total_disp),
        ctg_bnft: Vec::with_capacity(total_disp),
        ctg_cd: Vec::with_capacity(total_disp),
        prscrbr_id: Vec::with_capacity(total_disp),
        prscrbr_typ: Vec::with_capacity(total_disp),
        prscrbr_pc: Vec::with_capacity(total_disp),
        spec: Vec::with_capacity(total_disp),
        phrmcy_id: Vec::with_capacity(total_disp),
        phrmcy_pc: Vec::with_capacity(total_disp),
        phrmcy_typ: Vec::with_capacity(total_disp),
        rgltn24: Vec::with_capacity(total_disp),
        strmlnd: Vec::with_capacity(total_disp),
        undr_co: Vec::with_capacity(total_disp),
    };

    for mut c in chunk_results {
        d.aeuid.append(&mut c.aeuid);
        d.brth_mth.append(&mut c.brth_mth);
        d.brth_yr.append(&mut c.brth_yr);
        d.sex_cd.append(&mut c.sex_cd);
        d.pstcd.append(&mut c.pstcd);
        d.ctgry.append(&mut c.ctgry);
        d.itm.append(&mut c.itm);
        d.drg_typ.append(&mut c.drg_typ);
        d.prscrb_dt.append(&mut c.prscrb_dt);
        d.spply_dt.append(&mut c.spply_dt);
        d.extrct_dt.append(&mut c.extrct_dt);
        d.prscrptn_cnt.append(&mut c.prscrptn_cnt);
        d.rpt_ordr.append(&mut c.rpt_ordr);
        d.prvs_spply.append(&mut c.prvs_spply);
        d.qty.append(&mut c.qty);
        d.srt_rpt.append(&mut c.srt_rpt);
        d.ptnt_cntrbtn.append(&mut c.ptnt_cntrbtn);
        d.bnft.append(&mut c.bnft);
        d.ctg_bnft.append(&mut c.ctg_bnft);
        d.ctg_cd.append(&mut c.ctg_cd);
        d.prscrbr_id.append(&mut c.prscrbr_id);
        d.prscrbr_typ.append(&mut c.prscrbr_typ);
        d.prscrbr_pc.append(&mut c.prscrbr_pc);
        d.spec.append(&mut c.spec);
        d.phrmcy_id.append(&mut c.phrmcy_id);
        d.phrmcy_pc.append(&mut c.phrmcy_pc);
        d.phrmcy_typ.append(&mut c.phrmcy_typ);
        d.rgltn24.append(&mut c.rgltn24);
        d.strmlnd.append(&mut c.strmlnd);
        d.undr_co.append(&mut c.undr_co);
    }

    Some(d)
}

/// Generate PBS dispensings for a single year.
/// @export
#[extendr]
fn generate_pbs_year__(
    // Participant vectors
    aeuid: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    sex: &[i32],
    state: &[i32],
    first_year: &[i32],
    last_year: &[i32],
    // Item lookup vectors
    pbs_code: Strings,
    atc_code: Strings,
    atc_level1: Strings,
    benefit_type: Strings,
    claimed_price: &[f64],
    pack_size: &[i32],
    number_of_repeats: &[i32],
    item_weight: &[f64],
    // Provider/pharmacy pools
    prescriber_pool: Strings,
    pharmacy_pool: Strings,
    // Parameters
    year: i32,
    seed: i64,
) -> List {
    let _ = atc_code;
    let d = match generate_pbs_year_impl(
        &aeuid,
        birth_year,
        month_of_birth,
        sex,
        state,
        first_year,
        last_year,
        &pbs_code,
        &atc_level1,
        &benefit_type,
        claimed_price,
        pack_size,
        number_of_repeats,
        item_weight,
        &prescriber_pool,
        &pharmacy_pool,
        year,
        seed,
    ) {
        Some(d) => d,
        None => return empty_pbs_list(),
    };

    list!(
        SYNTHETIC_AEUID = d.aeuid,
        PTNT_BRTH_MTH = d.brth_mth,
        PTNT_BRTH_YR = d.brth_yr,
        PTNT_SEX_CD = d.sex_cd,
        PTNT_PSTCD = d.pstcd,
        PTNT_CTGRY_DRVD_CD = d.ctgry,
        ITM_CD = d.itm,
        DRG_TYP_CD = d.drg_typ,
        // Dates rendered as "ddmmmYY" strings (e.g. 31Jan20).
        PRSCRB_DT = d
            .prscrb_dt
            .iter()
            .map(|&x| crate::parquet_io::days_to_ddmmmyy(x).unwrap_or_default())
            .collect::<Vec<String>>(),
        SPPLY_DT = d
            .spply_dt
            .iter()
            .map(|&x| crate::parquet_io::days_to_ddmmmyy(x).unwrap_or_default())
            .collect::<Vec<String>>(),
        EXTRCT_DT = d
            .extrct_dt
            .iter()
            .map(|&x| crate::parquet_io::days_to_ddmmmyy(x).unwrap_or_default())
            .collect::<Vec<String>>(),
        PRSCRPTN_CNT = d.prscrptn_cnt,
        RPT_ORDR_NMBR = d.rpt_ordr,
        PRVS_SPPLY_NMBR = d.prvs_spply,
        PBS_RGLTN24_ADJST_QTY = d.qty,
        SRT_RPT_IND = d.srt_rpt,
        PTNT_CNTRBTN_AMT = d.ptnt_cntrbtn,
        BNFT_AMT = d.bnft,
        CTG_BNFT_AMT = d.ctg_bnft,
        CTG_CD = d.ctg_cd,
        PRSCRBR_ID_SCRAM = d.prscrbr_id,
        PRSCRBR_TYP_CD = d.prscrbr_typ,
        PRSCRBR_MJR_PSTCD = d.prscrbr_pc,
        MJR_SPCLTY_GRP_CD = d.spec,
        PHRMCY_ID_SCRAM = d.phrmcy_id,
        PHRMCY_PSTCD = d.phrmcy_pc,
        PHRMCY_APPRVL_TYP_CD = d.phrmcy_typ,
        RGLTN24_IND = d.rgltn24,
        STRMLND_ATHRTY_CD = d.strmlnd,
        UNDR_CPRSCRPTN_TYP_CD = d.undr_co
    )
}

/// Generate a PBS year and write directly to parquet, bypassing R.
/// @export
#[extendr]
fn generate_pbs_year_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    sex: &[i32],
    state: &[i32],
    first_year: &[i32],
    last_year: &[i32],
    pbs_code: Strings,
    atc_code: Strings,
    atc_level1: Strings,
    benefit_type: Strings,
    claimed_price: &[f64],
    pack_size: &[i32],
    number_of_repeats: &[i32],
    item_weight: &[f64],
    prescriber_pool: Strings,
    pharmacy_pool: Strings,
    year: i32,
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let _ = atc_code;

    let d = match generate_pbs_year_impl(
        &aeuid,
        birth_year,
        month_of_birth,
        sex,
        state,
        first_year,
        last_year,
        &pbs_code,
        &atc_level1,
        &benefit_type,
        claimed_price,
        pack_size,
        number_of_repeats,
        item_weight,
        &prescriber_pool,
        &pharmacy_pool,
        year,
        seed,
    ) {
        Some(d) => d,
        None => return 0,
    };

    let n_rows = d.aeuid.len() as i32;

    let columns: Vec<NamedCol> = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(d.aeuid),
        },
        NamedCol {
            name: "PTNT_BRTH_MTH",
            col: Col::I32(d.brth_mth),
        },
        NamedCol {
            name: "PTNT_BRTH_YR",
            col: Col::I32(d.brth_yr),
        },
        NamedCol {
            name: "PTNT_SEX_CD",
            col: Col::Str(d.sex_cd),
        },
        NamedCol {
            name: "PTNT_PSTCD",
            col: Col::Str(d.pstcd),
        },
        NamedCol {
            name: "PTNT_CTGRY_DRVD_CD",
            col: Col::Str(d.ctgry),
        },
        NamedCol {
            name: "ITM_CD",
            col: Col::Str(d.itm),
        },
        NamedCol {
            name: "DRG_TYP_CD",
            col: Col::Str(d.drg_typ),
        },
        NamedCol {
            name: "PRSCRB_DT",
            col: Col::DateStr(d.prscrb_dt),
        },
        NamedCol {
            name: "SPPLY_DT",
            col: Col::DateStr(d.spply_dt),
        },
        NamedCol {
            name: "EXTRCT_DT",
            col: Col::DateStr(d.extrct_dt),
        },
        NamedCol {
            name: "PRSCRPTN_CNT",
            col: Col::I32(d.prscrptn_cnt),
        },
        NamedCol {
            name: "RPT_ORDR_NMBR",
            col: Col::I32(d.rpt_ordr),
        },
        NamedCol {
            name: "PRVS_SPPLY_NMBR",
            col: Col::I32(d.prvs_spply),
        },
        NamedCol {
            name: "PBS_RGLTN24_ADJST_QTY",
            col: Col::I32(d.qty),
        },
        NamedCol {
            name: "SRT_RPT_IND",
            col: Col::Str(d.srt_rpt),
        },
        NamedCol {
            name: "PTNT_CNTRBTN_AMT",
            col: Col::F64(d.ptnt_cntrbtn),
        },
        NamedCol {
            name: "BNFT_AMT",
            col: Col::F64(d.bnft),
        },
        NamedCol {
            name: "CTG_BNFT_AMT",
            col: Col::F64(d.ctg_bnft),
        },
        NamedCol {
            name: "CTG_CD",
            col: Col::Str(d.ctg_cd),
        },
        NamedCol {
            name: "PRSCRBR_ID_SCRAM",
            col: Col::Str(d.prscrbr_id),
        },
        NamedCol {
            name: "PRSCRBR_TYP_CD",
            col: Col::Str(d.prscrbr_typ),
        },
        NamedCol {
            name: "PRSCRBR_MJR_PSTCD",
            col: Col::Str(d.prscrbr_pc),
        },
        NamedCol {
            name: "MJR_SPCLTY_GRP_CD",
            col: Col::Str(d.spec),
        },
        NamedCol {
            name: "PHRMCY_ID_SCRAM",
            col: Col::Str(d.phrmcy_id),
        },
        NamedCol {
            name: "PHRMCY_PSTCD",
            col: Col::Str(d.phrmcy_pc),
        },
        NamedCol {
            name: "PHRMCY_APPRVL_TYP_CD",
            col: Col::Str(d.phrmcy_typ),
        },
        NamedCol {
            name: "RGLTN24_IND",
            col: Col::Str(d.rgltn24),
        },
        NamedCol {
            name: "STRMLND_ATHRTY_CD",
            col: Col::StrOpt(d.strmlnd),
        },
        NamedCol {
            name: "UNDR_CPRSCRPTN_TYP_CD",
            col: Col::StrOpt(d.undr_co),
        },
    ];

    if let Err(e) = write_columns_to_parquet(out_path, columns) {
        panic!("PBS parquet write failed: {}", e);
    }

    n_rows
}

/// Select PBS participants from the spine.
/// @export
#[extendr]
fn select_pbs_participants__(birth_year: &[i32], min_year: i32, max_year: i32, seed: i64) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let mid_yr = ((min_year as f64 + max_year as f64) / 2.0).round() as i32;

    let mut indices: Vec<i32> = Vec::new();
    let mut first_years: Vec<i32> = Vec::new();
    let mut last_years: Vec<i32> = Vec::new();

    for i in 0..n {
        let age_mid = mid_yr - birth_year[i];
        let p_ever = if age_mid < 15 {
            PART_CHILD
        } else if age_mid < 65 {
            PART_ADULT
        } else {
            PART_ELDERLY
        };

        if rng.gen::<f64>() < p_ever && birth_year[i] <= max_year {
            indices.push((i + 1) as i32);
            first_years.push(birth_year[i].max(min_year));
            last_years.push(max_year);
        }
    }

    list!(
        index = indices,
        first_year = first_years,
        last_year = last_years
    )
}

fn empty_pbs_list() -> List {
    let empty_str: Vec<String> = Vec::new();
    let empty_i32: Vec<i32> = Vec::new();
    let empty_f64: Vec<f64> = Vec::new();
    let empty_opt_str: Vec<Option<String>> = Vec::new();

    list!(
        SYNTHETIC_AEUID = empty_str.clone(),
        PTNT_BRTH_MTH = empty_i32.clone(),
        PTNT_BRTH_YR = empty_i32.clone(),
        PTNT_SEX_CD = empty_str.clone(),
        PTNT_PSTCD = empty_str.clone(),
        PTNT_CTGRY_DRVD_CD = empty_str.clone(),
        ITM_CD = empty_str.clone(),
        DRG_TYP_CD = empty_str.clone(),
        PRSCRB_DT = empty_str.clone(),
        SPPLY_DT = empty_str.clone(),
        EXTRCT_DT = empty_str.clone(),
        PRSCRPTN_CNT = empty_i32.clone(),
        RPT_ORDR_NMBR = empty_i32.clone(),
        PRVS_SPPLY_NMBR = empty_i32.clone(),
        PBS_RGLTN24_ADJST_QTY = empty_i32.clone(),
        SRT_RPT_IND = empty_str.clone(),
        PTNT_CNTRBTN_AMT = empty_f64.clone(),
        BNFT_AMT = empty_f64.clone(),
        CTG_BNFT_AMT = empty_f64.clone(),
        CTG_CD = empty_str.clone(),
        PRSCRBR_ID_SCRAM = empty_str.clone(),
        PRSCRBR_TYP_CD = empty_str.clone(),
        PRSCRBR_MJR_PSTCD = empty_str.clone(),
        MJR_SPCLTY_GRP_CD = empty_str.clone(),
        PHRMCY_ID_SCRAM = empty_str.clone(),
        PHRMCY_PSTCD = empty_str.clone(),
        PHRMCY_APPRVL_TYP_CD = empty_str.clone(),
        RGLTN24_IND = empty_str.clone(),
        STRMLND_ATHRTY_CD = empty_opt_str.clone(),
        UNDR_CPRSCRPTN_TYP_CD = empty_opt_str
    )
}

// ==========================================================================
// Parallel chunk structures
// ==========================================================================

#[derive(Default)]
struct PbsChunk {
    aeuid: Vec<String>,
    brth_mth: Vec<i32>,
    brth_yr: Vec<i32>,
    sex_cd: Vec<String>,
    pstcd: Vec<String>,
    ctgry: Vec<String>,
    itm: Vec<String>,
    drg_typ: Vec<String>,
    prscrb_dt: Vec<i32>,
    spply_dt: Vec<i32>,
    extrct_dt: Vec<i32>,
    prscrptn_cnt: Vec<i32>,
    rpt_ordr: Vec<i32>,
    prvs_spply: Vec<i32>,
    qty: Vec<i32>,
    srt_rpt: Vec<String>,
    ptnt_cntrbtn: Vec<f64>,
    bnft: Vec<f64>,
    ctg_bnft: Vec<f64>,
    ctg_cd: Vec<String>,
    prscrbr_id: Vec<String>,
    prscrbr_typ: Vec<String>,
    prscrbr_pc: Vec<String>,
    spec: Vec<String>,
    phrmcy_id: Vec<String>,
    phrmcy_pc: Vec<String>,
    phrmcy_typ: Vec<String>,
    rgltn24: Vec<String>,
    strmlnd: Vec<Option<String>>,
    undr_co: Vec<Option<String>>,
}

struct PbsWriteChunk {
    person_idx: Vec<u32>,
    brth_mth: Vec<i32>,
    brth_yr: Vec<i32>,
    sex_cd: Vec<u8>,
    pstcd: Vec<i32>,
    ctgry: Vec<u8>,
    item_row: Vec<u32>,
    prscrb_dt: Vec<i32>,
    spply_dt: Vec<i32>,
    extrct_dt: Vec<i32>,
    prscrptn_cnt: Vec<i32>,
    rpt_ordr: Vec<i32>,
    prvs_spply: Vec<i32>,
    qty: Vec<i32>,
    srt_rpt: Vec<u8>,
    ptnt_cntrbtn: Vec<f64>,
    bnft: Vec<f64>,
    ctg_bnft: Vec<f64>,
    ctg_cd: Vec<u8>,
    prscrbr_idx: Vec<u32>,
    prscrbr_typ: Vec<u8>,
    prscrbr_pc: Vec<i32>,
    spec: Vec<u8>,
    phrmcy_idx: Vec<u32>,
    phrmcy_pc: Vec<i32>,
    phrmcy_typ: Vec<u8>,
    rgltn24: Vec<u8>,
    strmlnd: Vec<i32>,
    undr_co: Vec<i8>,
}

impl PbsWriteChunk {
    fn with_capacity(n: usize) -> Self {
        Self {
            person_idx: Vec::with_capacity(n),
            brth_mth: Vec::with_capacity(n),
            brth_yr: Vec::with_capacity(n),
            sex_cd: Vec::with_capacity(n),
            pstcd: Vec::with_capacity(n),
            ctgry: Vec::with_capacity(n),
            item_row: Vec::with_capacity(n),
            prscrb_dt: Vec::with_capacity(n),
            spply_dt: Vec::with_capacity(n),
            extrct_dt: Vec::with_capacity(n),
            prscrptn_cnt: Vec::with_capacity(n),
            rpt_ordr: Vec::with_capacity(n),
            prvs_spply: Vec::with_capacity(n),
            qty: Vec::with_capacity(n),
            srt_rpt: Vec::with_capacity(n),
            ptnt_cntrbtn: Vec::with_capacity(n),
            bnft: Vec::with_capacity(n),
            ctg_bnft: Vec::with_capacity(n),
            ctg_cd: Vec::with_capacity(n),
            prscrbr_idx: Vec::with_capacity(n),
            prscrbr_typ: Vec::with_capacity(n),
            prscrbr_pc: Vec::with_capacity(n),
            spec: Vec::with_capacity(n),
            phrmcy_idx: Vec::with_capacity(n),
            phrmcy_pc: Vec::with_capacity(n),
            phrmcy_typ: Vec::with_capacity(n),
            rgltn24: Vec::with_capacity(n),
            strmlnd: Vec::with_capacity(n),
            undr_co: Vec::with_capacity(n),
        }
    }

    fn len(&self) -> usize {
        self.spply_dt.len()
    }

    fn is_empty(&self) -> bool {
        self.spply_dt.is_empty()
    }
}

fn generate_one_dispensing(
    rng: &mut StdRng,
    items: &PbsItemTable,
    prsc_vec: &[&str],
    phar_vec: &[&str],
    avail_atc_indices: &[usize],
    norm_atc_shares: &[f64],
    year: i32,
    // Person-year window from `person_year_window()`: the first day the
    // person can be supplied on, and the span of days from it.
    window_start: i32,
    window_days: i32,
    // Date of birth in days since the epoch; lagged dates are held at it.
    dob_days: i32,
    person_aeuid: &str,
    person_byr: i32,
    person_brth_mth: i32,
    person_sex: i32,
    person_state: i32,
    out: &mut PbsChunk,
) {
    // Sample ATC1 category
    let atc_sel = weighted_sample(rng, norm_atc_shares);
    let atc1_idx = avail_atc_indices[atc_sel];

    // Sample item within ATC1
    let item_pool = &items.atc1_indices[atc1_idx];
    let item_w = &items.atc1_weights[atc1_idx];
    let item_row = if item_pool.is_empty() {
        random_positive_weight_row(rng, &items.weight, items.pbs_code.len())
    } else {
        let wi = weighted_sample(rng, item_w);
        item_pool[wi]
    };

    // Supply date
    let dt_offset = rng.gen_range(0..=window_days);
    let spply_dt = window_start + dt_offset;

    // Patient demographics. Birth month comes from the spine so it is the
    // same on every row of the same person, and matches every other product.
    let brth_mth = person_brth_mth;
    let sex_cd = match person_sex {
        1 => "M",
        2 => "F",
        _ => "X",
    };

    // Patient postcode
    let ptnt_pstcd = postcode_for_state(rng, person_state);

    // Patient category
    let age_at_disp = year - person_byr;
    let p_conc = if age_at_disp < 15 {
        CONC_CHILD
    } else if age_at_disp < 65 {
        CONC_ADULT
    } else {
        CONC_ELDERLY
    };
    let is_concessional = rng.gen::<f64>() < p_conc;
    let ptnt_ctgry = if is_concessional { "C" } else { "G" };

    // Item attributes
    let itm_cd = &items.pbs_code[item_row];
    let drg_typ = &items.benefit_type[item_row];
    let claimed_price_val = items.claimed_price[item_row];
    let pack_sz = items.pack_size[item_row];
    let n_repeats = items.number_of_repeats[item_row];

    // Copayment and benefit
    let copay_threshold = if is_concessional {
        CONCESSIONAL_COPAY
    } else {
        GENERAL_COPAY
    };
    let mut ptnt_cntrbtn = claimed_price_val.min(copay_threshold);
    ptnt_cntrbtn = (ptnt_cntrbtn * 100.0).round() / 100.0;
    let mut bnft_amt = ((claimed_price_val - ptnt_cntrbtn).max(0.0) * 100.0).round() / 100.0;

    let is_under_copay = claimed_price_val < copay_threshold;
    let undr_copay_cd = if is_under_copay {
        Some(if is_concessional { "2" } else { "1" })
    } else {
        None
    };

    // Closing the Gap
    let is_ctg = rng.gen::<f64>() < CTG_RATE;
    let ctg_cd = if is_ctg { "Y" } else { "N" };
    let mut ctg_bnft = 0.0;
    if is_ctg {
        ctg_bnft = ptnt_cntrbtn;
        bnft_amt = (claimed_price_val * 100.0).round() / 100.0;
        ptnt_cntrbtn = 0.0;
    }

    let prscrb_lag = normal_sample(rng, 3.0, 4.0).round().max(0.0).min(30.0) as i32;
    // A prescription cannot predate the person.
    let prscrb_dt = (spply_dt - prscrb_lag).max(dob_days);
    let extrct_lag = normal_sample(rng, 3.0, 2.0).round().max(1.0).min(14.0) as i32;
    let extrct_dt = spply_dt + extrct_lag;

    let prscrptn_cnt = 1 + n_repeats;
    let rpt_ordr = n_repeats;

    let prev_supply_idx = weighted_sample(rng, &PREV_SUPPLY_PROBS);
    let prev_supply = (prev_supply_idx as i32).min(rpt_ordr);
    let srt_rpt = if prev_supply > 0 { "Y" } else { "N" };

    let qty = if pack_sz > 0 { pack_sz } else { 30 };
    let rgltn24 = if rng.gen::<f64>() < REG24_RATE {
        "Y"
    } else {
        "N"
    };

    let strmlnd = if drg_typ == "S" {
        Some(format!("S{:04}", rng.gen_range(1..=9999)))
    } else {
        None
    };

    let prscrbr_idx = rng.gen_range(0..prsc_vec.len());
    let prscrbr_id = prsc_vec[prscrbr_idx].to_string();
    let prscrbr_typ_idx = weighted_sample(rng, &PRESCRIBER_WEIGHTS);
    let prscrbr_typ = PRESCRIBER_TYPES[prscrbr_typ_idx];
    let spec_idx = weighted_sample(rng, &SPECIALTY_WEIGHTS);
    let spec = SPECIALTY_CODES[spec_idx];
    let prscrbr_pc = postcode_for_state(rng, person_state);

    let phrmcy_idx = rng.gen_range(0..phar_vec.len());
    let phrmcy_id = phar_vec[phrmcy_idx].to_string();
    let phrmcy_typ_idx = weighted_sample(rng, &PHARMACY_WEIGHTS);
    let phrmcy_typ = PHARMACY_TYPES[phrmcy_typ_idx];
    let phrmcy_pc = postcode_for_state(rng, person_state);

    out.aeuid.push(person_aeuid.to_string());
    out.brth_mth.push(brth_mth);
    out.brth_yr.push(person_byr);
    out.sex_cd.push(sex_cd.to_string());
    out.pstcd.push(ptnt_pstcd);
    out.ctgry.push(ptnt_ctgry.to_string());
    out.itm.push(itm_cd.clone());
    out.drg_typ.push(drg_typ.clone());
    out.prscrb_dt.push(prscrb_dt);
    out.spply_dt.push(spply_dt);
    out.extrct_dt.push(extrct_dt);
    out.prscrptn_cnt.push(prscrptn_cnt);
    out.rpt_ordr.push(rpt_ordr);
    out.prvs_spply.push(prev_supply);
    out.qty.push(qty);
    out.srt_rpt.push(srt_rpt.to_string());
    out.ptnt_cntrbtn.push(ptnt_cntrbtn);
    out.bnft.push(bnft_amt);
    out.ctg_bnft.push(ctg_bnft);
    out.ctg_cd.push(ctg_cd.to_string());
    out.prscrbr_id.push(prscrbr_id);
    out.prscrbr_typ.push(prscrbr_typ.to_string());
    out.prscrbr_pc.push(prscrbr_pc);
    out.spec.push(spec.to_string());
    out.phrmcy_id.push(phrmcy_id);
    out.phrmcy_pc.push(phrmcy_pc);
    out.phrmcy_typ.push(phrmcy_typ.to_string());
    out.rgltn24.push(rgltn24.to_string());
    out.strmlnd.push(strmlnd);
    out.undr_co.push(undr_copay_cd.map(|s| s.to_string()));
}

#[inline]
fn postcode_int_for_state(rng: &mut StdRng, state: i32) -> i32 {
    for i in 0..STATE_CODES.len() {
        if STATE_CODES[i] == state {
            return rng.gen_range(STATE_PC_LO[i]..=STATE_PC_HI[i]);
        }
    }
    rng.gen_range(2000..=6999)
}

fn postcode_lookup(pool: &[String], postcode: i32) -> &str {
    if (0..pool.len() as i32).contains(&postcode) {
        pool[postcode as usize].as_str()
    } else {
        "0000"
    }
}

#[allow(clippy::too_many_arguments)]
fn generate_one_dispensing_compact(
    rng: &mut StdRng,
    items: &PbsItemTable,
    n_prescribers: usize,
    n_pharmacies: usize,
    avail_atc_indices: &[usize],
    norm_atc_shares: &[f64],
    year: i32,
    // Person-year window from `person_year_window()`: the first day the
    // person can be supplied on, and the span of days from it.
    window_start: i32,
    window_days: i32,
    // Date of birth in days since the epoch; lagged dates are held at it.
    dob_days: i32,
    person_idx: u32,
    person_byr: i32,
    person_brth_mth: i32,
    person_sex: i32,
    person_state: i32,
    out: &mut PbsWriteChunk,
    // Number of *baseline* item rows (excludes appended health markers); the
    // empty-category fallback samples within this bound so markers never leak
    // into baseline dispensings.
    n_base_items: usize,
    // When `Some`, emit this exact item row (a health marker) instead of
    // sampling. The ATC-sampling RNG draws are skipped in this branch.
    forced_item_row: Option<usize>,
) {
    let item_row = match forced_item_row {
        Some(r) => r,
        None => {
            let atc_sel = weighted_sample(rng, norm_atc_shares);
            let atc1_idx = avail_atc_indices[atc_sel];

            let item_pool = &items.atc1_indices[atc1_idx];
            let item_w = &items.atc1_weights[atc1_idx];
            if item_pool.is_empty() {
                random_positive_weight_row(rng, &items.weight, n_base_items)
            } else {
                let wi = weighted_sample(rng, item_w);
                item_pool[wi]
            }
        }
    };

    let dt_offset = rng.gen_range(0..=window_days);
    let spply_dt = window_start + dt_offset;

    // Birth month comes from the spine so it is the same on every row of the
    // same person, and matches every other product.
    let brth_mth = person_brth_mth;
    let sex_cd = match person_sex {
        1 => 0u8,
        2 => 1u8,
        _ => 2u8,
    };

    let ptnt_pstcd = postcode_int_for_state(rng, person_state);

    let age_at_disp = year - person_byr;
    let p_conc = if age_at_disp < 15 {
        CONC_CHILD
    } else if age_at_disp < 65 {
        CONC_ADULT
    } else {
        CONC_ELDERLY
    };
    let is_concessional = rng.gen::<f64>() < p_conc;
    let ptnt_ctgry = if is_concessional { 1u8 } else { 0u8 };

    let drg_typ = &items.benefit_type[item_row];
    let claimed_price_val = items.claimed_price[item_row];
    let pack_sz = items.pack_size[item_row];
    let n_repeats = items.number_of_repeats[item_row];

    let copay_threshold = if is_concessional {
        CONCESSIONAL_COPAY
    } else {
        GENERAL_COPAY
    };
    let mut ptnt_cntrbtn = claimed_price_val.min(copay_threshold);
    ptnt_cntrbtn = (ptnt_cntrbtn * 100.0).round() / 100.0;
    let mut bnft_amt = ((claimed_price_val - ptnt_cntrbtn).max(0.0) * 100.0).round() / 100.0;

    let is_under_copay = claimed_price_val < copay_threshold;
    let undr_copay_cd = if is_under_copay {
        if is_concessional {
            2i8
        } else {
            1i8
        }
    } else {
        0i8
    };

    let is_ctg = rng.gen::<f64>() < CTG_RATE;
    let mut ctg_bnft = 0.0;
    if is_ctg {
        ctg_bnft = ptnt_cntrbtn;
        bnft_amt = (claimed_price_val * 100.0).round() / 100.0;
        ptnt_cntrbtn = 0.0;
    }

    let prscrb_lag = normal_sample(rng, 3.0, 4.0).round().max(0.0).min(30.0) as i32;
    // A prescription cannot predate the person.
    let prscrb_dt = (spply_dt - prscrb_lag).max(dob_days);
    let extrct_lag = normal_sample(rng, 3.0, 2.0).round().max(1.0).min(14.0) as i32;
    let extrct_dt = spply_dt + extrct_lag;

    let prscrptn_cnt = 1 + n_repeats;
    let rpt_ordr = n_repeats;

    let prev_supply_idx = weighted_sample(rng, &PREV_SUPPLY_PROBS);
    let prev_supply = (prev_supply_idx as i32).min(rpt_ordr);
    let srt_rpt = if prev_supply > 0 { 1u8 } else { 0u8 };

    let qty = if pack_sz > 0 { pack_sz } else { 30 };
    let rgltn24 = if rng.gen::<f64>() < REG24_RATE {
        1u8
    } else {
        0u8
    };

    let strmlnd = if drg_typ == "S" {
        rng.gen_range(1..=9999)
    } else {
        -1
    };

    let prscrbr_idx = rng.gen_range(0..n_prescribers) as u32;
    let prscrbr_typ_idx = weighted_sample(rng, &PRESCRIBER_WEIGHTS) as u8;
    let spec_idx = weighted_sample(rng, &SPECIALTY_WEIGHTS) as u8;
    let prscrbr_pc = postcode_int_for_state(rng, person_state);

    let phrmcy_idx = rng.gen_range(0..n_pharmacies) as u32;
    let phrmcy_typ_idx = weighted_sample(rng, &PHARMACY_WEIGHTS) as u8;
    let phrmcy_pc = postcode_int_for_state(rng, person_state);

    out.person_idx.push(person_idx);
    out.brth_mth.push(brth_mth);
    out.brth_yr.push(person_byr);
    out.sex_cd.push(sex_cd);
    out.pstcd.push(ptnt_pstcd);
    out.ctgry.push(ptnt_ctgry);
    out.item_row.push(item_row as u32);
    out.prscrb_dt.push(prscrb_dt);
    out.spply_dt.push(spply_dt);
    out.extrct_dt.push(extrct_dt);
    out.prscrptn_cnt.push(prscrptn_cnt);
    out.rpt_ordr.push(rpt_ordr);
    out.prvs_spply.push(prev_supply);
    out.qty.push(qty);
    out.srt_rpt.push(srt_rpt);
    out.ptnt_cntrbtn.push(ptnt_cntrbtn);
    out.bnft.push(bnft_amt);
    out.ctg_bnft.push(ctg_bnft);
    out.ctg_cd.push(if is_ctg { 1 } else { 0 });
    out.prscrbr_idx.push(prscrbr_idx);
    out.prscrbr_typ.push(prscrbr_typ_idx);
    out.prscrbr_pc.push(prscrbr_pc);
    out.spec.push(spec_idx);
    out.phrmcy_idx.push(phrmcy_idx);
    out.phrmcy_pc.push(phrmcy_pc);
    out.phrmcy_typ.push(phrmcy_typ_idx);
    out.rgltn24.push(rgltn24);
    out.strmlnd.push(strmlnd);
    out.undr_co.push(undr_copay_cd);
}

// ==========================================================================
// Full consolidated PBS generator
// ==========================================================================

use arrow_array::{ArrayRef, Float64Array, Int32Array, RecordBatch, StringArray};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::arrow_writer::ArrowWriter;
use parquet::basic::Compression;
use parquet::file::properties::WriterProperties;
use std::fs::File;
use std::path::Path;
use std::sync::Arc;

/// Pre-computed context for a full PBS run.
struct PbsContext {
    items: PbsItemTable,
    prescriber_pool: Vec<String>,
    pharmacy_pool: Vec<String>,
    avail_atc_indices: Vec<usize>,
    norm_atc_shares: Vec<f64>,
    /// Number of item rows available to baseline sampling.
    n_base_items: usize,
}

fn pbs_output_schema() -> Arc<Schema> {
    Arc::new(Schema::new(vec![
        Field::new("SYNTHETIC_AEUID", DataType::Utf8, false),
        Field::new("PTNT_BRTH_MTH", DataType::Int32, false),
        Field::new("PTNT_BRTH_YR", DataType::Int32, false),
        Field::new("PTNT_SEX_CD", DataType::Utf8, false),
        Field::new("PTNT_PSTCD", DataType::Utf8, false),
        Field::new("PTNT_CTGRY_DRVD_CD", DataType::Utf8, false),
        Field::new("ITM_CD", DataType::Utf8, false),
        Field::new("DRG_TYP_CD", DataType::Utf8, false),
        // Dates rendered as "ddmmmYY" strings (e.g. 31Jan20).
        Field::new("PRSCRB_DT", DataType::Utf8, false),
        Field::new("SPPLY_DT", DataType::Utf8, false),
        Field::new("EXTRCT_DT", DataType::Utf8, false),
        Field::new("PRSCRPTN_CNT", DataType::Int32, false),
        Field::new("RPT_ORDR_NMBR", DataType::Int32, false),
        Field::new("PRVS_SPPLY_NMBR", DataType::Int32, false),
        Field::new("PBS_RGLTN24_ADJST_QTY", DataType::Int32, false),
        Field::new("SRT_RPT_IND", DataType::Utf8, false),
        Field::new("PTNT_CNTRBTN_AMT", DataType::Float64, false),
        Field::new("BNFT_AMT", DataType::Float64, false),
        Field::new("CTG_BNFT_AMT", DataType::Float64, false),
        Field::new("CTG_CD", DataType::Utf8, false),
        Field::new("PRSCRBR_ID_SCRAM", DataType::Utf8, false),
        Field::new("PRSCRBR_TYP_CD", DataType::Utf8, false),
        Field::new("PRSCRBR_MJR_PSTCD", DataType::Utf8, false),
        Field::new("MJR_SPCLTY_GRP_CD", DataType::Utf8, false),
        Field::new("PHRMCY_ID_SCRAM", DataType::Utf8, false),
        Field::new("PHRMCY_PSTCD", DataType::Utf8, false),
        Field::new("PHRMCY_APPRVL_TYP_CD", DataType::Utf8, false),
        Field::new("RGLTN24_IND", DataType::Utf8, false),
        Field::new("STRMLND_ATHRTY_CD", DataType::Utf8, true),
        Field::new("UNDR_CPRSCRPTN_TYP_CD", DataType::Utf8, true),
    ]))
}

/// Convert a PbsChunk into a RecordBatch for streaming write.
#[allow(dead_code)]
fn pbs_chunk_to_batch(chunk: PbsChunk, schema: Arc<Schema>) -> RecordBatch {
    let aeuid_arr: ArrayRef = Arc::new(StringArray::from(chunk.aeuid));
    let brth_mth_arr: ArrayRef = Arc::new(Int32Array::from(chunk.brth_mth));
    let brth_yr_arr: ArrayRef = Arc::new(Int32Array::from(chunk.brth_yr));
    let sex_cd_arr: ArrayRef = Arc::new(StringArray::from(chunk.sex_cd));
    let pstcd_arr: ArrayRef = Arc::new(StringArray::from(chunk.pstcd));
    let ctgry_arr: ArrayRef = Arc::new(StringArray::from(chunk.ctgry));
    let itm_arr: ArrayRef = Arc::new(StringArray::from(chunk.itm));
    let drg_typ_arr: ArrayRef = Arc::new(StringArray::from(chunk.drg_typ));
    let prscrb_dt_arr: ArrayRef = Arc::new(StringArray::from(
        chunk
            .prscrb_dt
            .iter()
            .map(|&d| crate::parquet_io::days_to_ddmmmyy(d))
            .collect::<Vec<_>>(),
    ));
    let spply_dt_arr: ArrayRef = Arc::new(StringArray::from(
        chunk
            .spply_dt
            .iter()
            .map(|&d| crate::parquet_io::days_to_ddmmmyy(d))
            .collect::<Vec<_>>(),
    ));
    let extrct_dt_arr: ArrayRef = Arc::new(StringArray::from(
        chunk
            .extrct_dt
            .iter()
            .map(|&d| crate::parquet_io::days_to_ddmmmyy(d))
            .collect::<Vec<_>>(),
    ));
    let prscrptn_cnt_arr: ArrayRef = Arc::new(Int32Array::from(chunk.prscrptn_cnt));
    let rpt_ordr_arr: ArrayRef = Arc::new(Int32Array::from(chunk.rpt_ordr));
    let prvs_spply_arr: ArrayRef = Arc::new(Int32Array::from(chunk.prvs_spply));
    let qty_arr: ArrayRef = Arc::new(Int32Array::from(chunk.qty));
    let srt_rpt_arr: ArrayRef = Arc::new(StringArray::from(chunk.srt_rpt));
    let ptnt_cntrbtn_arr: ArrayRef = Arc::new(Float64Array::from(chunk.ptnt_cntrbtn));
    let bnft_arr: ArrayRef = Arc::new(Float64Array::from(chunk.bnft));
    let ctg_bnft_arr: ArrayRef = Arc::new(Float64Array::from(chunk.ctg_bnft));
    let ctg_cd_arr: ArrayRef = Arc::new(StringArray::from(chunk.ctg_cd));
    let prscrbr_id_arr: ArrayRef = Arc::new(StringArray::from(chunk.prscrbr_id));
    let prscrbr_typ_arr: ArrayRef = Arc::new(StringArray::from(chunk.prscrbr_typ));
    let prscrbr_pc_arr: ArrayRef = Arc::new(StringArray::from(chunk.prscrbr_pc));
    let spec_arr: ArrayRef = Arc::new(StringArray::from(chunk.spec));
    let phrmcy_id_arr: ArrayRef = Arc::new(StringArray::from(chunk.phrmcy_id));
    let phrmcy_pc_arr: ArrayRef = Arc::new(StringArray::from(chunk.phrmcy_pc));
    let phrmcy_typ_arr: ArrayRef = Arc::new(StringArray::from(chunk.phrmcy_typ));
    let rgltn24_arr: ArrayRef = Arc::new(StringArray::from(chunk.rgltn24));
    let strmlnd_refs: Vec<Option<&str>> = chunk.strmlnd.iter().map(|o| o.as_deref()).collect();
    let strmlnd_arr: ArrayRef = Arc::new(StringArray::from(strmlnd_refs));
    let undr_co_refs: Vec<Option<&str>> = chunk.undr_co.iter().map(|o| o.as_deref()).collect();
    let undr_co_arr: ArrayRef = Arc::new(StringArray::from(undr_co_refs));

    RecordBatch::try_new(
        schema,
        vec![
            aeuid_arr,
            brth_mth_arr,
            brth_yr_arr,
            sex_cd_arr,
            pstcd_arr,
            ctgry_arr,
            itm_arr,
            drg_typ_arr,
            prscrb_dt_arr,
            spply_dt_arr,
            extrct_dt_arr,
            prscrptn_cnt_arr,
            rpt_ordr_arr,
            prvs_spply_arr,
            qty_arr,
            srt_rpt_arr,
            ptnt_cntrbtn_arr,
            bnft_arr,
            ctg_bnft_arr,
            ctg_cd_arr,
            prscrbr_id_arr,
            prscrbr_typ_arr,
            prscrbr_pc_arr,
            spec_arr,
            phrmcy_id_arr,
            phrmcy_pc_arr,
            phrmcy_typ_arr,
            rgltn24_arr,
            strmlnd_arr,
            undr_co_arr,
        ],
    )
    .expect("PBS record batch build")
}

const PBS_SEX_LABELS: [&str; 3] = ["M", "F", "X"];
const PBS_CATEGORY_LABELS: [&str; 2] = ["G", "C"];
const PBS_YN_LABELS: [&str; 2] = ["N", "Y"];

/// Convert a compact PBS write chunk into a RecordBatch. The hot
/// generation loop stores indices, flags, and integer postcodes; string
/// materialisation is delayed until this batch boundary.
fn pbs_write_chunk_to_batch(
    chunk: PbsWriteChunk,
    ctx: &PbsContext,
    aeuid: &[String],
    schema: Arc<Schema>,
) -> RecordBatch {
    let n = chunk.len();
    let items = &ctx.items;
    let postcode_strings: Vec<String> = (0..10_000).map(|pc| format!("{:04}", pc)).collect();
    let streamlined_strings: Vec<String> =
        (0..10_000).map(|code| format!("S{:04}", code)).collect();

    let mut out_aeuid: Vec<&str> = Vec::with_capacity(n);
    let mut out_sex: Vec<&str> = Vec::with_capacity(n);
    let mut out_pstcd: Vec<&str> = Vec::with_capacity(n);
    let mut out_ctgry: Vec<&str> = Vec::with_capacity(n);
    let mut out_itm: Vec<&str> = Vec::with_capacity(n);
    let mut out_drg_typ: Vec<&str> = Vec::with_capacity(n);
    let mut out_srt_rpt: Vec<&str> = Vec::with_capacity(n);
    let mut out_ctg_cd: Vec<&str> = Vec::with_capacity(n);
    let mut out_prscrbr_id: Vec<&str> = Vec::with_capacity(n);
    let mut out_prscrbr_typ: Vec<&str> = Vec::with_capacity(n);
    let mut out_prscrbr_pc: Vec<&str> = Vec::with_capacity(n);
    let mut out_spec: Vec<&str> = Vec::with_capacity(n);
    let mut out_phrmcy_id: Vec<&str> = Vec::with_capacity(n);
    let mut out_phrmcy_pc: Vec<&str> = Vec::with_capacity(n);
    let mut out_phrmcy_typ: Vec<&str> = Vec::with_capacity(n);
    let mut out_rgltn24: Vec<&str> = Vec::with_capacity(n);
    let mut out_strmlnd: Vec<Option<&str>> = Vec::with_capacity(n);

    for k in 0..n {
        let item_row = chunk.item_row[k] as usize;
        out_aeuid.push(aeuid[chunk.person_idx[k] as usize].as_str());
        out_sex.push(PBS_SEX_LABELS[chunk.sex_cd[k] as usize]);
        out_pstcd.push(postcode_lookup(&postcode_strings, chunk.pstcd[k]));
        out_ctgry.push(PBS_CATEGORY_LABELS[chunk.ctgry[k] as usize]);
        out_itm.push(items.pbs_code[item_row].as_str());
        out_drg_typ.push(items.benefit_type[item_row].as_str());
        out_srt_rpt.push(PBS_YN_LABELS[chunk.srt_rpt[k] as usize]);
        out_ctg_cd.push(PBS_YN_LABELS[chunk.ctg_cd[k] as usize]);
        out_prscrbr_id.push(ctx.prescriber_pool[chunk.prscrbr_idx[k] as usize].as_str());
        out_prscrbr_typ.push(PRESCRIBER_TYPES[chunk.prscrbr_typ[k] as usize]);
        out_prscrbr_pc.push(postcode_lookup(&postcode_strings, chunk.prscrbr_pc[k]));
        out_spec.push(SPECIALTY_CODES[chunk.spec[k] as usize]);
        out_phrmcy_id.push(ctx.pharmacy_pool[chunk.phrmcy_idx[k] as usize].as_str());
        out_phrmcy_pc.push(postcode_lookup(&postcode_strings, chunk.phrmcy_pc[k]));
        out_phrmcy_typ.push(PHARMACY_TYPES[chunk.phrmcy_typ[k] as usize]);
        out_rgltn24.push(PBS_YN_LABELS[chunk.rgltn24[k] as usize]);
        out_strmlnd.push(if chunk.strmlnd[k] < 0 {
            None
        } else {
            Some(postcode_lookup(&streamlined_strings, chunk.strmlnd[k]))
        });
    }

    let undr_co_refs: Vec<Option<&str>> = chunk
        .undr_co
        .iter()
        .map(|&x| match x {
            1 => Some("1"),
            2 => Some("2"),
            _ => None,
        })
        .collect();

    let aeuid_arr: ArrayRef = Arc::new(StringArray::from(out_aeuid));
    let brth_mth_arr: ArrayRef = Arc::new(Int32Array::from(chunk.brth_mth));
    let brth_yr_arr: ArrayRef = Arc::new(Int32Array::from(chunk.brth_yr));
    let sex_cd_arr: ArrayRef = Arc::new(StringArray::from(out_sex));
    let pstcd_arr: ArrayRef = Arc::new(StringArray::from(out_pstcd));
    let ctgry_arr: ArrayRef = Arc::new(StringArray::from(out_ctgry));
    let itm_arr: ArrayRef = Arc::new(StringArray::from(out_itm));
    let drg_typ_arr: ArrayRef = Arc::new(StringArray::from(out_drg_typ));
    let prscrb_dt_arr: ArrayRef = Arc::new(StringArray::from(
        chunk
            .prscrb_dt
            .iter()
            .map(|&d| crate::parquet_io::days_to_ddmmmyy(d))
            .collect::<Vec<_>>(),
    ));
    let spply_dt_arr: ArrayRef = Arc::new(StringArray::from(
        chunk
            .spply_dt
            .iter()
            .map(|&d| crate::parquet_io::days_to_ddmmmyy(d))
            .collect::<Vec<_>>(),
    ));
    let extrct_dt_arr: ArrayRef = Arc::new(StringArray::from(
        chunk
            .extrct_dt
            .iter()
            .map(|&d| crate::parquet_io::days_to_ddmmmyy(d))
            .collect::<Vec<_>>(),
    ));
    let prscrptn_cnt_arr: ArrayRef = Arc::new(Int32Array::from(chunk.prscrptn_cnt));
    let rpt_ordr_arr: ArrayRef = Arc::new(Int32Array::from(chunk.rpt_ordr));
    let prvs_spply_arr: ArrayRef = Arc::new(Int32Array::from(chunk.prvs_spply));
    let qty_arr: ArrayRef = Arc::new(Int32Array::from(chunk.qty));
    let srt_rpt_arr: ArrayRef = Arc::new(StringArray::from(out_srt_rpt));
    let ptnt_cntrbtn_arr: ArrayRef = Arc::new(Float64Array::from(chunk.ptnt_cntrbtn));
    let bnft_arr: ArrayRef = Arc::new(Float64Array::from(chunk.bnft));
    let ctg_bnft_arr: ArrayRef = Arc::new(Float64Array::from(chunk.ctg_bnft));
    let ctg_cd_arr: ArrayRef = Arc::new(StringArray::from(out_ctg_cd));
    let prscrbr_id_arr: ArrayRef = Arc::new(StringArray::from(out_prscrbr_id));
    let prscrbr_typ_arr: ArrayRef = Arc::new(StringArray::from(out_prscrbr_typ));
    let prscrbr_pc_arr: ArrayRef = Arc::new(StringArray::from(out_prscrbr_pc));
    let spec_arr: ArrayRef = Arc::new(StringArray::from(out_spec));
    let phrmcy_id_arr: ArrayRef = Arc::new(StringArray::from(out_phrmcy_id));
    let phrmcy_pc_arr: ArrayRef = Arc::new(StringArray::from(out_phrmcy_pc));
    let phrmcy_typ_arr: ArrayRef = Arc::new(StringArray::from(out_phrmcy_typ));
    let rgltn24_arr: ArrayRef = Arc::new(StringArray::from(out_rgltn24));
    let strmlnd_arr: ArrayRef = Arc::new(StringArray::from(out_strmlnd));
    let undr_co_arr: ArrayRef = Arc::new(StringArray::from(undr_co_refs));

    RecordBatch::try_new(
        schema,
        vec![
            aeuid_arr,
            brth_mth_arr,
            brth_yr_arr,
            sex_cd_arr,
            pstcd_arr,
            ctgry_arr,
            itm_arr,
            drg_typ_arr,
            prscrb_dt_arr,
            spply_dt_arr,
            extrct_dt_arr,
            prscrptn_cnt_arr,
            rpt_ordr_arr,
            prvs_spply_arr,
            qty_arr,
            srt_rpt_arr,
            ptnt_cntrbtn_arr,
            bnft_arr,
            ctg_bnft_arr,
            ctg_cd_arr,
            prscrbr_id_arr,
            prscrbr_typ_arr,
            prscrbr_pc_arr,
            spec_arr,
            phrmcy_id_arr,
            phrmcy_pc_arr,
            phrmcy_typ_arr,
            rgltn24_arr,
            strmlnd_arr,
            undr_co_arr,
        ],
    )
    .expect("PBS compact record batch build")
}

#[allow(clippy::too_many_arguments)]
fn write_pbs_year_file(
    year: i32,
    participant_rows: &[usize],
    part_first_year: &[i32],
    part_last_year: &[i32],
    aeuid_full: &[String],
    birth_year: &[i32],
    month_of_birth: &[i32],
    sex: &[i32],
    state: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    baseline_income: &[f64],
    baseline_employed: &[i32],
    disability_onset_year: &[i32],
    disability_severity: &[i32],
    disability_is_dc: &[i32],
    ctx: &PbsContext,
    year_seed: u64,
    out_path: &str,
    chunk_persons: usize,
) -> usize {
    let alive_idx: Vec<usize> = participant_rows
        .iter()
        .enumerate()
        .filter_map(|(i, &sp_idx)| {
            if part_first_year[i] <= year && part_last_year[i] >= year {
                Some(sp_idx)
            } else {
                None
            }
        })
        .collect();

    let schema = pbs_output_schema();
    let file = File::create(out_path).unwrap_or_else(|e| panic!("create {}: {}", out_path, e));
    // Cap row groups to 256k rows. With ~30 string columns at ~10 bytes
    // each, that's ~75 MB per column per row group — well under the
    // 2 GB i32 offset limit that crashes value_unchecked() on overflow.
    let props = WriterProperties::builder()
        .set_compression(Compression::SNAPPY)
        .set_max_row_group_size(256_000)
        .build();
    let mut writer =
        ArrowWriter::try_new(file, schema.clone(), Some(props)).expect("pbs arrow writer");

    if alive_idx.is_empty() {
        writer.close().expect("close empty writer");
        return 0;
    }

    let n_prescribers = ctx.prescriber_pool.len();
    let n_pharmacies = ctx.pharmacy_pool.len();

    let mut total = 0usize;
    let mut chunk_start = 0usize;
    let n_alive = alive_idx.len();

    while chunk_start < n_alive {
        let chunk_end = (chunk_start + chunk_persons).min(n_alive);
        let chunk_seed = year_seed.wrapping_add((chunk_start as u64).wrapping_mul(2_654_435_761));
        let mut rng = StdRng::seed_from_u64(chunk_seed);
        let cap = (chunk_end - chunk_start).saturating_mul(12);
        let mut chunk = PbsWriteChunk::with_capacity(cap);

        for &pi in &alive_idx[chunk_start..chunk_end] {
            let age = year - birth_year[pi];
            let band = age_band_index(age);
            let mut rate = AGE_RATES[band];
            if sex[pi] == 2 {
                rate *= FEMALE_MULT;
            }
            rate *= healthcare_usage_multiplier(
                year,
                age,
                baseline_income[pi],
                baseline_employed[pi],
                disability_onset_year[pi],
                disability_severity[pi],
                disability_is_dc[pi],
            );
            let person_byr = birth_year[pi];
            // Guard against a missing spine month (extendr renders R
            // `NA_integer_` as `i32::MIN`); January is the fallback.
            let person_brth_mth = month_of_birth[pi].clamp(1, 12);
            // Only the post-birth part of the birth year is observable.
            let (window_start, window_days, obs_frac) =
                person_year_window(year, person_byr, person_brth_mth);
            let dob_days = person_dob_days(person_byr, person_brth_mth);
            rate *= obs_frac;
            let person_sex = sex[pi];
            let person_state = state[pi];
            // Compose the death cutoff onto the birth window. Both bound the
            // same draw: the window opens at birth and closes at death.
            // `observable_days_in_year` returns an offset from 1 January, so
            // re-base it on `window_start`, which opens later in a birth year.
            let window_days = if year_of_death[pi] == year {
                let last = observable_days_in_year(
                    year,
                    year_of_death[pi],
                    month_of_death[pi],
                    day_of_death[pi],
                )
                .unwrap_or(0);
                let year_start = days_since_epoch(year, 1, 1);
                ((year_start + last) - window_start).clamp(0, window_days)
            } else {
                window_days
            };

            let count = poisson_sample(&mut rng, rate) as usize;
            for _ in 0..count {
                generate_one_dispensing_compact(
                    &mut rng,
                    &ctx.items,
                    n_prescribers,
                    n_pharmacies,
                    &ctx.avail_atc_indices,
                    &ctx.norm_atc_shares,
                    year,
                    window_start,
                    window_days,
                    dob_days,
                    pi as u32,
                    person_byr,
                    person_brth_mth,
                    person_sex,
                    person_state,
                    &mut chunk,
                    ctx.n_base_items,
                    None,
                );
            }

        }

        if !chunk.is_empty() {
            let n_this = chunk.len();
            let batch = pbs_write_chunk_to_batch(chunk, ctx, aeuid_full, schema.clone());
            writer.write(&batch).expect("pbs writer.write");
            total += n_this;
        }

        chunk_start = chunk_end;
    }

    writer.close().expect("pbs writer.close");
    total
}

/// Generate all PBS year products in a single Rust call.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn generate_pbs_full__(
    aeuid_dhda: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    sex: &[i32],
    state: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    baseline_income: &[f64],
    baseline_employed: &[i32],
    disability_onset_year: &[i32],
    disability_severity: &[i32],
    disability_is_dc: &[i32],
    pbs_code: Strings,
    atc_level1: Strings,
    benefit_type: Strings,
    claimed_price: &[f64],
    pack_size: &[i32],
    number_of_repeats: &[i32],
    item_weight: &[f64],
    out_dir: &str,
    year_start: i32,
    year_end: i32,
    seed: i64,
    chunk_persons: i32,
) -> List {
    let aeuid_full: Vec<String> = aeuid_dhda.iter().map(|s| s.to_string()).collect();
    let pbs_code_vec: Vec<String> = pbs_code.iter().map(|s| s.to_string()).collect();
    let atc1_vec: Vec<String> = atc_level1.iter().map(|s| s.to_string()).collect();
    let bt_vec: Vec<String> = benefit_type.iter().map(|s| s.to_string()).collect();

    let items = PbsItemTable::from_vectors(
        &pbs_code_vec,
        &atc1_vec,
        &bt_vec,
        claimed_price,
        pack_size,
        number_of_repeats,
        item_weight,
    );

    let n_base_items = items.pbs_code.len();

    let n_spine = birth_year.len();
    assert_eq!(year_of_death.len(), n_spine);
    assert_eq!(month_of_death.len(), n_spine);
    assert_eq!(day_of_death.len(), n_spine);

    // Participant selection — seed + 1100 matches select_pbs_participants__.
    let mut part_rng = StdRng::seed_from_u64((seed as u64).wrapping_add(1100));
    let mid_yr = ((year_start as f64 + year_end as f64) / 2.0).round() as i32;
    let mut part_idx: Vec<usize> = Vec::new();
    let mut part_first: Vec<i32> = Vec::new();
    let mut part_last: Vec<i32> = Vec::new();
    for i in 0..n_spine {
        let observed_through = if year_of_death[i] == i32::MIN {
            year_end
        } else {
            year_of_death[i].min(year_end)
        };
        if birth_year[i] > year_end || observed_through < year_start {
            continue;
        }
        let age_mid = mid_yr - birth_year[i];
        let p_ever = if age_mid < 15 {
            PART_CHILD
        } else if age_mid < 65 {
            PART_ADULT
        } else {
            PART_ELDERLY
        };
        let p_ever = (p_ever
            * healthcare_usage_multiplier(
                mid_yr,
                age_mid,
                baseline_income[i],
                baseline_employed[i],
                disability_onset_year[i],
                disability_severity[i],
                disability_is_dc[i],
            ))
        .min(0.995);
        if part_rng.gen::<f64>() < p_ever {
            part_idx.push(i);
            part_first.push(birth_year[i].max(year_start));
            part_last.push(observed_through);
        }
    }

    // Pools (seed + 1103 / 1104 match the R-side generator).
    let mut presc_rng = StdRng::seed_from_u64((seed as u64).wrapping_add(1103));
    let n_prescribers = (n_spine / 50).max(100);
    let prescriber_pool: Vec<String> = (0..n_prescribers)
        .map(|_| format!("PRES{:08X}", presc_rng.gen::<u32>() & 0x7FFF_FFFF))
        .collect();
    let mut phar_rng = StdRng::seed_from_u64((seed as u64).wrapping_add(1104));
    let n_pharmacies = (n_spine / 100).max(50);
    let pharmacy_pool: Vec<String> = (0..n_pharmacies)
        .map(|_| format!("PHAR{:08X}", phar_rng.gen::<u32>() & 0x7FFF_FFFF))
        .collect();

    // Pre-compute avail ATC indices and shares.
    let mut avail_atc_indices: Vec<usize> = Vec::new();
    let mut avail_atc_shares: Vec<f64> = Vec::new();
    for (i, _code) in ATC1_CODES.iter().enumerate() {
        if !items.atc1_indices[i].is_empty() {
            avail_atc_indices.push(i);
            avail_atc_shares.push(ATC1_SHARES[i]);
        }
    }
    let atc_share_total: f64 = avail_atc_shares.iter().sum();
    let norm_atc_shares: Vec<f64> = avail_atc_shares
        .iter()
        .map(|s| s / atc_share_total)
        .collect();

    let ctx = Arc::new(PbsContext {
        items,
        prescriber_pool,
        pharmacy_pool,
        avail_atc_indices,
        norm_atc_shares,
        n_base_items,
    });

    let aeuid_full = Arc::new(aeuid_full);
    let part_idx = Arc::new(part_idx);
    let part_first = Arc::new(part_first);
    let part_last = Arc::new(part_last);
    let birth_year_arc = Arc::new(birth_year.to_vec());
    let month_of_birth_arc = Arc::new(month_of_birth.to_vec());
    let sex_arc = Arc::new(sex.to_vec());
    let state_arc = Arc::new(state.to_vec());
    let year_of_death_arc = Arc::new(year_of_death.to_vec());
    let month_of_death_arc = Arc::new(month_of_death.to_vec());
    let day_of_death_arc = Arc::new(day_of_death.to_vec());
    let baseline_income_arc = Arc::new(baseline_income.to_vec());
    let baseline_employed_arc = Arc::new(baseline_employed.to_vec());
    let disability_onset_year_arc = Arc::new(disability_onset_year.to_vec());
    let disability_severity_arc = Arc::new(disability_severity.to_vec());
    let disability_is_dc_arc = Arc::new(disability_is_dc.to_vec());

    let years: Vec<i32> = (year_start..=year_end).collect();
    let chunk_persons = (chunk_persons as usize).max(1);

    let results: Vec<(i32, usize)> = years
        .par_iter()
        .map(|&yr| {
            let pc = format!("madipge-pbs-d-prescriptions-{}.parquet", yr);
            let path = Path::new(out_dir).join(&pc);
            let year_seed = (seed as u64)
                .wrapping_add(1101)
                .wrapping_add((yr - year_start) as u64);
            let n = write_pbs_year_file(
                yr,
                part_idx.as_slice(),
                part_first.as_slice(),
                part_last.as_slice(),
                aeuid_full.as_slice(),
                birth_year_arc.as_slice(),
                month_of_birth_arc.as_slice(),
                sex_arc.as_slice(),
                state_arc.as_slice(),
                year_of_death_arc.as_slice(),
                month_of_death_arc.as_slice(),
                day_of_death_arc.as_slice(),
                baseline_income_arc.as_slice(),
                baseline_employed_arc.as_slice(),
                disability_onset_year_arc.as_slice(),
                disability_severity_arc.as_slice(),
                disability_is_dc_arc.as_slice(),
                &ctx,
                year_seed,
                path.to_str().unwrap(),
                chunk_persons,
            );
            (yr, n)
        })
        .collect();

    let years_out: Vec<i32> = results.iter().map(|(y, _)| *y).collect();
    let counts_out: Vec<i32> = results.iter().map(|(_, n)| *n as i32).collect();
    list!(year = years_out, n_dispensings = counts_out)
}

extendr_module! {
    mod pbs;
    fn generate_pbs_year__;
    fn generate_pbs_year_parquet__;
    fn select_pbs_participants__;
    fn generate_pbs_full__;
}

use crate::codeframes;
use crate::sampling::{normal_sample, weighted_sample};
use extendr_api::prelude::*;
use rand::prelude::*;
use rand::rngs::StdRng;

// Participation rates by spine education code (0..5)
const PARTICIPATION_RATE: [f64; 6] = [0.00, 0.15, 0.30, 0.50, 0.25, 0.05];
const AGE_BOOST_YOUNG: f64 = 1.3;
const AGE_BOOST_MID: f64 = 1.0;
const AGE_PEN_OLD: f64 = 0.6;

// Qualification shares by education level
const QUAL_SHARES_BY_EDU: [[f64; 6]; 6] = [
    [0.08, 0.18, 0.35, 0.18, 0.15, 0.06],
    [0.20, 0.35, 0.30, 0.10, 0.04, 0.01],
    [0.10, 0.25, 0.35, 0.18, 0.08, 0.04],
    [0.05, 0.10, 0.40, 0.25, 0.14, 0.06],
    [0.02, 0.05, 0.15, 0.25, 0.38, 0.15],
    [0.02, 0.05, 0.15, 0.20, 0.40, 0.18],
];

const FOE_CODES: [&str; 10] = ["03", "04", "05", "06", "07", "08", "09", "10", "11", "12"];

// Archetype-to-FOE affinity matrix (8 archetypes × 10 FOEs)
const ARCHETYPE_FOE: [[f64; 10]; 8] = [
    [0.15, 0.10, 0.10, 0.02, 0.02, 0.10, 0.08, 0.03, 0.20, 0.20],
    [0.30, 0.25, 0.05, 0.02, 0.02, 0.05, 0.03, 0.03, 0.10, 0.15],
    [0.02, 0.02, 0.02, 0.55, 0.10, 0.05, 0.10, 0.02, 0.07, 0.05],
    [0.03, 0.03, 0.02, 0.05, 0.05, 0.40, 0.15, 0.05, 0.07, 0.15],
    [0.10, 0.05, 0.03, 0.08, 0.10, 0.25, 0.15, 0.08, 0.06, 0.10],
    [0.05, 0.05, 0.03, 0.05, 0.08, 0.35, 0.15, 0.04, 0.08, 0.12],
    [0.25, 0.10, 0.03, 0.05, 0.05, 0.15, 0.10, 0.07, 0.05, 0.15],
    [0.05, 0.03, 0.02, 0.08, 0.05, 0.15, 0.15, 0.07, 0.30, 0.10],
];

const FT_YEARS: [f64; 6] = [0.5, 0.5, 1.0, 1.0, 2.0, 2.0];
const PT_MULTIPLIER: f64 = 1.5;
const FT_SHARE: f64 = 0.45;
const COMPLETION_RATES: [f64; 6] = [0.40, 0.42, 0.47, 0.50, 0.55, 0.58];

const PROV_CODES: [i32; 6] = [21, 31, 61, 91, 95, 97];
const PROV_WEIGHTS: [f64; 6] = [0.30, 0.02, 0.08, 0.05, 0.50, 0.05];
const QUAL_CODES: [i32; 6] = [511, 514, 521, 524, 611, 613];
const REMOTE_CODES: [i32; 5] = [0, 1, 2, 3, 4];
const REMOTE_SHARES: [f64; 5] = [0.45, 0.25, 0.18, 0.08, 0.04];
const SEIFA_CODES: [i32; 5] = [1, 2, 3, 4, 5];
const SEIFA_SHARES: [f64; 5] = [0.22, 0.21, 0.20, 0.19, 0.18];
const LFS_CODES: [i32; 4] = [1, 2, 3, 4];
const LFS_SHARES: [f64; 4] = [0.45, 0.25, 0.15, 0.15];
const SCHOOL_CODES: [i32; 5] = [8, 9, 10, 11, 12];
const SCHOOL_SHARES: [f64; 5] = [0.02, 0.05, 0.15, 0.18, 0.60];
const EDU_TO_HED: [i32; 6] = [0, 1, 2, 4, 5, 6];

const DELIV_CODES: [i32; 5] = [10, 20, 30, 40, 90];
const DELIV_SHARES: [f64; 5] = [0.45, 0.15, 0.25, 0.10, 0.05];
const FUND_CODES: [i32; 6] = [11, 20, 30, 31, 80, 99];
const FUND_SHARES: [f64; 6] = [0.12, 0.40, 0.28, 0.03, 0.12, 0.05];
const OUTCOME_CODES: [i32; 8] = [20, 30, 40, 51, 60, 70, 81, 90];
const OUTCOME_SHARES: [f64; 8] = [0.55, 0.05, 0.10, 0.08, 0.05, 0.10, 0.04, 0.03];
const STUDY_REASON_CODES: [i32; 7] = [1, 2, 4, 7, 8, 11, 12];
const STUDY_REASON_SHARES: [f64; 7] = [0.40, 0.20, 0.10, 0.08, 0.07, 0.10, 0.05];
const SUBJECT_HOURS_MEAN: f64 = 40.0;
const SUBJECT_HOURS_SD: f64 = 20.0;
const SUBJECTS_FT: i32 = 8;
const SUBJECTS_PT: i32 = 4;

fn is_leap(y: i32) -> bool {
    y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)
}

fn days_in_year(y: i32) -> i32 {
    if is_leap(y) {
        366
    } else {
        365
    }
}

fn date_to_days(year: i32, month: i32, day: i32) -> i32 {
    let mut d = 0i32;
    if year >= 1970 {
        for y in 1970..year {
            d += days_in_year(y);
        }
    } else {
        for y in year..1970 {
            d -= days_in_year(y);
        }
    }
    let feb = if is_leap(year) { 29 } else { 28 };
    let mdays = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    for m in 0..((month - 1) as usize).min(11) {
        d += mdays[m];
    }
    d + day - 1
}

fn days_to_date_string(days: i32) -> String {
    let mut d = days;
    let mut year = 1970;
    if d >= 0 {
        loop {
            let dy = days_in_year(year);
            if d < dy {
                break;
            }
            d -= dy;
            year += 1;
        }
    } else {
        loop {
            year -= 1;
            d += days_in_year(year);
            if d >= 0 {
                break;
            }
        }
    }
    let feb = if is_leap(year) { 29 } else { 28 };
    let mdays = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    let mut month = 1;
    for &md in mdays.iter() {
        if d < md {
            break;
        }
        d -= md;
        month += 1;
    }
    format!("{:04}-{:02}-{:02}", year, month, d + 1)
}

const STANDARD_AGE_CODES: [i32; 5] = [16, 17, 18, 19, 20];
const STANDARD_AGE_WEIGHTS: [f64; 5] = [0.10, 0.20, 0.30, 0.25, 0.15];

fn state_postcode_range(state: i32) -> (i32, i32) {
    match state {
        1 => (2000, 2999),
        2 => (3000, 3999),
        3 => (4000, 4999),
        4 => (5000, 5799),
        5 => (6000, 6797),
        6 => (7000, 7999),
        7 => (800, 899),
        8 => (2600, 2618),
        _ => (2000, 6999),
    }
}

/// Select TVA participants from spine vectors.
/// @export
#[extendr]
fn select_tva_participants__(
    birth_year: &[i32],
    education: &[i32],
    archetype: &[i32],
    aeuid_ncver: Strings,
    anzsco_code: Strings,
    sex: &[i32],
    state: &[i32],
    seed: i32,
    min_year: i32,
    max_year: i32,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(1200));

    let mid_yr = ((min_year as f64 + max_year as f64) / 2.0).round() as i32;

    let aeuids: Vec<String> = aeuid_ncver.iter().map(|s| s.to_string()).collect();
    let anzscos: Vec<String> = anzsco_code.iter().map(|s| s.to_string()).collect();

    let mut out_idx: Vec<i32> = Vec::new();
    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_birth_year: Vec<i32> = Vec::new();
    let mut out_sex: Vec<i32> = Vec::new();
    let mut out_state: Vec<i32> = Vec::new();
    let mut out_education: Vec<i32> = Vec::new();
    let mut out_archetype: Vec<i32> = Vec::new();
    let mut out_anzsco: Vec<String> = Vec::new();
    let mut out_qual_idx: Vec<i32> = Vec::new();
    let mut out_foe: Vec<String> = Vec::new();
    let mut out_commence_year: Vec<i32> = Vec::new();
    let mut out_duration_yrs: Vec<f64> = Vec::new();
    let mut out_is_ft: Vec<bool> = Vec::new();
    let mut out_completed: Vec<bool> = Vec::new();
    let mut out_completion_year: Vec<Rint> = Vec::new();
    let mut out_prov_type: Vec<i32> = Vec::new();
    let mut out_rto_id: Vec<String> = Vec::new();

    // Generate RTO pool
    let n_rtos = std::cmp::max(200, n / 20);
    let rto_pool: Vec<String> = (0..n_rtos)
        .map(|_| format!("RTO{:05}", rng.gen_range(1..=99999i32)))
        .collect();

    for i in 0..n {
        let edu = education[i].min(5).max(0) as usize;
        let p_base = PARTICIPATION_RATE[edu];

        let age_mid = mid_yr - birth_year[i];
        let age_mod = if age_mid < 25 {
            AGE_BOOST_YOUNG
        } else if age_mid < 45 {
            AGE_BOOST_MID
        } else {
            AGE_PEN_OLD
        };
        let p_ever = (p_base * age_mod).min(1.0);

        if rng.gen::<f64>() >= p_ever {
            continue;
        }
        if birth_year[i] + 15 > max_year {
            continue;
        }

        let arch = archetype[i].min(7).max(0) as usize;

        // Qualification assignment
        let qual_idx = weighted_sample(&mut rng, &QUAL_SHARES_BY_EDU[edu]) as i32 + 1;

        // FOE assignment
        let foe_idx = weighted_sample(&mut rng, &ARCHETYPE_FOE[arch]);
        let foe = FOE_CODES[foe_idx].to_string();

        // Commencement year
        let standard_age = STANDARD_AGE_CODES[weighted_sample(&mut rng, &STANDARD_AGE_WEIGHTS)];
        let is_mature = rng.gen::<f64>() < 0.35;
        let mature_age = normal_sample(&mut rng, 30.0, 8.0)
            .round()
            .max(21.0)
            .min(55.0) as i32;
        let entry_age = if is_mature { mature_age } else { standard_age };
        let commence_year = (birth_year[i] + entry_age).max(min_year).min(max_year);

        // Duration
        let ft_yrs = FT_YEARS[(qual_idx - 1) as usize];
        let is_ft = rng.gen::<f64>() < FT_SHARE;
        let cal_yrs = if is_ft {
            ft_yrs
        } else {
            ft_yrs * PT_MULTIPLIER
        };
        let duration_yrs =
            ((cal_yrs + normal_sample(&mut rng, 0.0, 0.2)).max(0.25) * 10.0).round() / 10.0;

        // Completion
        let comp_rate = COMPLETION_RATES[(qual_idx - 1) as usize];
        let completed = rng.gen::<f64>() < comp_rate;
        let comp_year = commence_year + duration_yrs.ceil() as i32;

        // Provider
        let prov_idx = weighted_sample(&mut rng, &PROV_WEIGHTS);
        let prov_type = PROV_CODES[prov_idx];
        let rto_id = rto_pool[rng.gen_range(0..rto_pool.len())].clone();

        out_idx.push((i + 1) as i32); // R is 1-indexed
        out_aeuid.push(aeuids[i].clone());
        out_birth_year.push(birth_year[i]);
        out_sex.push(sex[i]);
        out_state.push(state[i]);
        out_education.push(education[i]);
        out_archetype.push(archetype[i]);
        out_anzsco.push(anzscos[i].clone());
        out_qual_idx.push(qual_idx);
        out_foe.push(foe);
        out_commence_year.push(commence_year);
        out_duration_yrs.push(duration_yrs);
        out_is_ft.push(is_ft);
        out_completed.push(completed);
        if completed {
            out_completion_year.push(Rint::from(comp_year));
        } else {
            out_completion_year.push(Rint::na());
        }
        out_prov_type.push(prov_type);
        out_rto_id.push(rto_id);
    }

    list!(
        spine_idx = out_idx,
        aeuid = out_aeuid,
        birth_year = out_birth_year,
        sex = out_sex,
        state = out_state,
        education = out_education,
        archetype = out_archetype,
        anzsco = out_anzsco,
        qual_idx = out_qual_idx,
        foe = out_foe,
        commence_year = out_commence_year,
        duration_yrs = out_duration_yrs,
        is_ft = out_is_ft,
        completed = out_completed,
        completion_year = out_completion_year,
        prov_type = out_prov_type,
        rto_id = out_rto_id,
    )
}

fn empty_tva_activity_list() -> List {
    let es: Vec<String> = Vec::new();
    let ei: Vec<i32> = Vec::new();

    list!(
        SYNTHETIC_AEUID = es.clone(),
        TVA_ROW_ID = es.clone(),
        COLLECTION_YR = ei.clone(),
        AGE = ei.clone(),
        YEAR_OF_BIRTH = ei.clone(),
        MONTH_OF_BIRTH = ei.clone(),
        GENDER_ID = ei.clone(),
        HIGHEST_SCHL_LVL_ST = ei.clone(),
        HIGHEST_ED_LVL_ST = ei.clone(),
        PRIOR_ED_ACHIEVE_FG = es.clone(),
        AT_SCHOOL_FG = es.clone(),
        LABOUR_FORCE_STATUS_ID = ei.clone(),
        ANZSCO_ID = es.clone(),
        PROGRAM_ID = es.clone(),
        PROGRAM_FOE_ID = es.clone(),
        PROGRAM_LOE_ID = ei.clone(),
        PROGRAM_RECOGNITION_ID = ei.clone(),
        PROGRAM_TRAINING_PACKAGE_ID = es.clone(),
        PROGRAM_TYPE_OF_TRAINING_ID = ei.clone(),
        PROGRAM_NOMINAL_HOURS = ei.clone(),
        PROGRAM_VET_FG = es.clone(),
        SUBJECT_ID = es.clone(),
        SUBJECT_FOE_ID = es.clone(),
        SUBJECT_NOMINAL_HOURS = ei.clone(),
        SUBJECT_VET_FG = es.clone(),
        SUBJECT_FG = es.clone(),
        APPRENTICESHIP_FG = es.clone(),
        VET_IN_SCHOOLS_FG = es.clone(),
        STUDENT_COMMENCING_FG = es.clone(),
        STUDY_REASON_ID = ei.clone(),
        ACTIVITY_START_DATE = es.clone(),
        ACTIVITY_END_DATE = es.clone(),
        DELIVERY_MODE_ID = ei.clone(),
        NATIONAL_FUNDING_SOURCE_ID = ei.clone(),
        NATIONAL_OUTCOME_ID = ei.clone(),
        CLIENT_POSTCODE_DERIVED = es.clone(),
        CLIENT_STATE_RESIDENCE_DERIVED = es.clone(),
        CLIENT_REMOTENESS_ID_DERIVED = ei.clone(),
        CLIENT_SEIFA_IRSD_QUINT_DRVD = ei.clone(),
        RTO_ID = es.clone(),
        TRAIN_ORG_TYPE_ID = ei.clone(),
        HEAD_OFFICE_STATE = es.clone(),
        HEAD_OFFICE_POSTCODE = es.clone(),
        STATE_OF_FUNDING_GF = es.clone(),
    )
}

/// Project TVA training activity across requested years.
/// @export
#[extendr]
fn project_tva_activity__(
    spell_aeuid: Strings,
    spell_birth_year: &[i32],
    spell_sex: &[i32],
    spell_state: &[i32],
    spell_education: &[i32],
    spell_anzsco: Strings,
    spell_qual_idx: &[i32],
    spell_foe: Strings,
    spell_commence_year: &[i32],
    spell_duration_yrs: &[f64],
    spell_is_ft: &[i32],
    spell_completion_year: &[i32],
    spell_rto_id: Strings,
    spell_prov_type: &[i32],
    years: &[i32],
    seed: i32,
) -> List {
    let n_spells = spell_aeuid.len();
    if n_spells == 0 || years.is_empty() {
        return empty_tva_activity_list();
    }

    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(1201));
    let aeuids: Vec<String> = spell_aeuid.iter().map(|s| s.to_string()).collect();
    let anzscos: Vec<String> = spell_anzsco.iter().map(|s| s.to_string()).collect();
    let foes: Vec<String> = spell_foe.iter().map(|s| s.to_string()).collect();
    let rto_ids: Vec<String> = spell_rto_id.iter().map(|s| s.to_string()).collect();

    let mut birth_month = Vec::with_capacity(n_spells);
    let mut school_lvl = Vec::with_capacity(n_spells);
    let mut lfs_id = Vec::with_capacity(n_spells);
    let mut study_reason = Vec::with_capacity(n_spells);
    let mut deliv_mode = Vec::with_capacity(n_spells);
    let mut fund_src = Vec::with_capacity(n_spells);
    let mut remote_id = Vec::with_capacity(n_spells);
    let mut seifa_q = Vec::with_capacity(n_spells);
    let mut client_pc = Vec::with_capacity(n_spells);
    let mut ho_pc = Vec::with_capacity(n_spells);
    let mut hed = Vec::with_capacity(n_spells);
    let mut prior_ed = Vec::with_capacity(n_spells);
    let mut is_apprentice = Vec::with_capacity(n_spells);
    let mut vet_schools = Vec::with_capacity(n_spells);
    let mut n_subj = Vec::with_capacity(n_spells);
    let mut prog_hours = Vec::with_capacity(n_spells);
    let mut prog_id = Vec::with_capacity(n_spells);
    let mut pkg_id = Vec::with_capacity(n_spells);
    let mut spell_end_year = Vec::with_capacity(n_spells);

    for i in 0..n_spells {
        let state = spell_state[i];
        let (pc_min, pc_max) = state_postcode_range(state);
        let client_postcode = format!("{:04}", rng.gen_range(pc_min..=pc_max));
        let qual_idx = spell_qual_idx[i].clamp(1, 6);
        let is_ft = spell_is_ft[i] != 0;
        let age_at_commence = spell_commence_year[i] - spell_birth_year[i];
        let deliv = DELIV_CODES[weighted_sample(&mut rng, &DELIV_SHARES)];
        let is_appr = (qual_idx == 3 || qual_idx == 4) && deliv == 30;

        birth_month.push(rng.gen_range(1..=12i32));
        school_lvl.push(SCHOOL_CODES[weighted_sample(&mut rng, &SCHOOL_SHARES)]);
        lfs_id.push(LFS_CODES[weighted_sample(&mut rng, &LFS_SHARES)]);
        study_reason.push(STUDY_REASON_CODES[weighted_sample(&mut rng, &STUDY_REASON_SHARES)]);
        deliv_mode.push(deliv);
        fund_src.push(FUND_CODES[weighted_sample(&mut rng, &FUND_SHARES)]);
        remote_id.push(REMOTE_CODES[weighted_sample(&mut rng, &REMOTE_SHARES)]);
        seifa_q.push(SEIFA_CODES[weighted_sample(&mut rng, &SEIFA_SHARES)]);
        client_pc.push(client_postcode.clone());
        ho_pc.push(format!("{:04}", rng.gen_range(pc_min..=pc_max)));
        hed.push(EDU_TO_HED[spell_education[i].clamp(0, 5) as usize]);
        prior_ed.push(if spell_education[i] >= 3 {
            "Y".to_string()
        } else {
            "N".to_string()
        });
        is_apprentice.push(is_appr);
        vet_schools.push(age_at_commence <= 18 && spell_prov_type[i] == 97);
        let subj_count = if is_ft { SUBJECTS_FT } else { SUBJECTS_PT };
        n_subj.push(subj_count);
        prog_hours.push(
            (subj_count as f64 * spell_duration_yrs[i].ceil() * SUBJECT_HOURS_MEAN).ceil() as i32,
        );
        prog_id.push(format!(
            "P{}{:03}{:04}",
            foes[i],
            qual_idx,
            (i + 1) % 10_000
        ));
        pkg_id.push(format!("PKG{}{}", foes[i], qual_idx));
        let fallback_end = spell_commence_year[i] + spell_duration_yrs[i].ceil() as i32;
        let completion_year = if spell_completion_year[i] == i32::MIN {
            fallback_end
        } else {
            spell_completion_year[i]
        };
        spell_end_year.push(completion_year);
    }

    let mut out_aeuid = Vec::new();
    let mut out_row_id = Vec::new();
    let mut out_collection_year = Vec::new();
    let mut out_age = Vec::new();
    let mut out_year_of_birth = Vec::new();
    let mut out_month_of_birth = Vec::new();
    let mut out_gender = Vec::new();
    let mut out_school_lvl = Vec::new();
    let mut out_hed = Vec::new();
    let mut out_prior_ed = Vec::new();
    let mut out_at_school = Vec::new();
    let mut out_lfs = Vec::new();
    let mut out_anzsco = Vec::new();
    let mut out_program_id = Vec::new();
    let mut out_program_foe = Vec::new();
    let mut out_program_loe = Vec::new();
    let mut out_program_recognition = Vec::new();
    let mut out_package_id = Vec::new();
    let mut out_program_type = Vec::new();
    let mut out_program_hours = Vec::new();
    let mut out_program_vet = Vec::new();
    let mut out_subject_id = Vec::new();
    let mut out_subject_foe = Vec::new();
    let mut out_subject_hours = Vec::new();
    let mut out_subject_vet = Vec::new();
    let mut out_subject_fg = Vec::new();
    let mut out_apprenticeship = Vec::new();
    let mut out_vet_schools = Vec::new();
    let mut out_commencing = Vec::new();
    let mut out_study_reason = Vec::new();
    let mut out_start_date = Vec::new();
    let mut out_end_date = Vec::new();
    let mut out_delivery_mode = Vec::new();
    let mut out_funding_source = Vec::new();
    let mut out_outcome = Vec::new();
    let mut out_client_pc = Vec::new();
    let mut out_client_state = Vec::new();
    let mut out_remote = Vec::new();
    let mut out_seifa = Vec::new();
    let mut out_rto_id = Vec::new();
    let mut out_train_org_type = Vec::new();
    let mut out_head_office_state = Vec::new();
    let mut out_head_office_pc = Vec::new();
    let mut out_state_of_funding = Vec::new();

    let mut row_id = 0i32;
    for &year in years {
        for i in 0..n_spells {
            if spell_commence_year[i] > year || spell_end_year[i] < year {
                continue;
            }
            let subject_count = n_subj[i].max(0) as usize;
            let is_commencing = year == spell_commence_year[i];
            for subj in 1..=subject_count {
                row_id += 1;
                let subject_hours = normal_sample(&mut rng, SUBJECT_HOURS_MEAN, SUBJECT_HOURS_SD)
                    .round()
                    .max(10.0) as i32;
                let start_month = rng.gen_range(1..=6i32);
                let start_day = rng.gen_range(1..=28i32);
                let start_days = date_to_days(year, start_month, start_day);
                let yr_end = date_to_days(year, 12, 31);
                let end_days =
                    (start_days + ((subject_hours as f64 / 5.0) * 7.0).round() as i32).min(yr_end);
                let at_school = vet_schools[i] && is_commencing;

                out_aeuid.push(aeuids[i].clone());
                out_row_id.push(format!("TVA{:010}", row_id));
                out_collection_year.push(year);
                out_age.push(year - spell_birth_year[i]);
                out_year_of_birth.push(spell_birth_year[i]);
                out_month_of_birth.push(birth_month[i]);
                out_gender.push(spell_sex[i]);
                out_school_lvl.push(school_lvl[i]);
                out_hed.push(hed[i]);
                out_prior_ed.push(prior_ed[i].clone());
                out_at_school.push(if at_school {
                    "Y".to_string()
                } else {
                    "N".to_string()
                });
                out_lfs.push(lfs_id[i]);
                out_anzsco.push(anzscos[i].clone());
                out_program_id.push(prog_id[i].clone());
                out_program_foe.push(foes[i].clone());
                out_program_loe.push(QUAL_CODES[spell_qual_idx[i].clamp(1, 6) as usize - 1]);
                out_program_recognition.push(11);
                out_package_id.push(pkg_id[i].clone());
                out_program_type.push(if is_apprentice[i] { 11 } else { 21 });
                out_program_hours.push(prog_hours[i]);
                out_program_vet.push("Y".to_string());
                out_subject_id.push(format!("S{}{:03}{:02}", foes[i], (i + 1) % 1000, subj));
                out_subject_foe.push(foes[i].clone());
                out_subject_hours.push(subject_hours);
                out_subject_vet.push("Y".to_string());
                out_subject_fg.push("Y".to_string());
                out_apprenticeship.push(if is_apprentice[i] {
                    "Y".to_string()
                } else {
                    "N".to_string()
                });
                out_vet_schools.push(if at_school {
                    "Y".to_string()
                } else {
                    "N".to_string()
                });
                out_commencing.push(if is_commencing {
                    "Y".to_string()
                } else {
                    "N".to_string()
                });
                out_study_reason.push(study_reason[i]);
                out_start_date.push(days_to_date_string(start_days));
                out_end_date.push(days_to_date_string(end_days));
                out_delivery_mode.push(deliv_mode[i]);
                out_funding_source.push(fund_src[i]);
                out_outcome.push(OUTCOME_CODES[weighted_sample(&mut rng, &OUTCOME_SHARES)]);
                out_client_pc.push(client_pc[i].clone());
                // AVETMISS two-character state identifier (01-08/09/99), not the
                // numeric 1-8 spine code.
                out_client_state.push(codeframes::state_avetmiss(spell_state[i]).to_string());
                out_remote.push(remote_id[i]);
                out_seifa.push(seifa_q[i]);
                out_rto_id.push(rto_ids[i].clone());
                out_train_org_type.push(spell_prov_type[i]);
                out_head_office_state.push(codeframes::state_avetmiss(spell_state[i]).to_string());
                out_head_office_pc.push(ho_pc[i].clone());
                out_state_of_funding.push(codeframes::state_avetmiss(spell_state[i]).to_string());
            }
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        TVA_ROW_ID = out_row_id,
        COLLECTION_YR = out_collection_year,
        AGE = out_age,
        YEAR_OF_BIRTH = out_year_of_birth,
        MONTH_OF_BIRTH = out_month_of_birth,
        GENDER_ID = out_gender,
        HIGHEST_SCHL_LVL_ST = out_school_lvl,
        HIGHEST_ED_LVL_ST = out_hed,
        PRIOR_ED_ACHIEVE_FG = out_prior_ed,
        AT_SCHOOL_FG = out_at_school,
        LABOUR_FORCE_STATUS_ID = out_lfs,
        ANZSCO_ID = out_anzsco,
        PROGRAM_ID = out_program_id,
        PROGRAM_FOE_ID = out_program_foe,
        PROGRAM_LOE_ID = out_program_loe,
        PROGRAM_RECOGNITION_ID = out_program_recognition,
        PROGRAM_TRAINING_PACKAGE_ID = out_package_id,
        PROGRAM_TYPE_OF_TRAINING_ID = out_program_type,
        PROGRAM_NOMINAL_HOURS = out_program_hours,
        PROGRAM_VET_FG = out_program_vet,
        SUBJECT_ID = out_subject_id,
        SUBJECT_FOE_ID = out_subject_foe,
        SUBJECT_NOMINAL_HOURS = out_subject_hours,
        SUBJECT_VET_FG = out_subject_vet,
        SUBJECT_FG = out_subject_fg,
        APPRENTICESHIP_FG = out_apprenticeship,
        VET_IN_SCHOOLS_FG = out_vet_schools,
        STUDENT_COMMENCING_FG = out_commencing,
        STUDY_REASON_ID = out_study_reason,
        ACTIVITY_START_DATE = out_start_date,
        ACTIVITY_END_DATE = out_end_date,
        DELIVERY_MODE_ID = out_delivery_mode,
        NATIONAL_FUNDING_SOURCE_ID = out_funding_source,
        NATIONAL_OUTCOME_ID = out_outcome,
        CLIENT_POSTCODE_DERIVED = out_client_pc,
        CLIENT_STATE_RESIDENCE_DERIVED = out_client_state,
        CLIENT_REMOTENESS_ID_DERIVED = out_remote,
        CLIENT_SEIFA_IRSD_QUINT_DRVD = out_seifa,
        RTO_ID = out_rto_id,
        TRAIN_ORG_TYPE_ID = out_train_org_type,
        HEAD_OFFICE_STATE = out_head_office_state,
        HEAD_OFFICE_POSTCODE = out_head_office_pc,
        STATE_OF_FUNDING_GF = out_state_of_funding,
    )
}

/// Project TVA completions.
/// @export
#[extendr]
fn project_tva_completions__(
    spell_aeuid: Strings,
    spell_birth_year: &[i32],
    spell_sex: &[i32],
    spell_state: &[i32],
    spell_education: &[i32],
    spell_anzsco: Strings,
    spell_qual_idx: &[i32],
    spell_foe: Strings,
    spell_completion_year: &[i32],
    spell_completed: &[i32],
    spell_rto_id: Strings,
    spell_prov_type: &[i32],
    seed: i32,
    min_year: i32,
    max_year: i32,
) -> List {
    let n_spells = spell_aeuid.len();
    if n_spells == 0 {
        return empty_tva_completions_list();
    }

    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(1202));

    let mut keep: Vec<usize> = Vec::new();
    for i in 0..n_spells {
        if spell_completed[i] != 0
            && spell_completion_year[i] != i32::MIN
            && spell_completion_year[i] >= min_year
            && spell_completion_year[i] <= max_year
        {
            keep.push(i);
        }
    }
    if keep.is_empty() {
        return empty_tva_completions_list();
    }

    let n_comp = keep.len();
    let mut out_aeuid: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_row_id: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_collection_year: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_age: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_birth_year: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_birth_month: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_gender: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_school_lvl: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_hed: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_prior_ed: Vec<&str> = Vec::with_capacity(n_comp);
    let out_at_school: Vec<&str> = vec!["N"; n_comp];
    let mut out_lfs: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_anzsco: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_program_id: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_program_foe: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_program_loe: Vec<i32> = Vec::with_capacity(n_comp);
    let out_program_recognition: Vec<i32> = vec![11; n_comp];
    let mut out_package_id: Vec<String> = Vec::with_capacity(n_comp);
    let out_program_type: Vec<i32> = vec![21; n_comp];
    let mut out_completed_date: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_completed_year: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_client_pc: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_client_state: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_remote: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_seifa: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_rto_id: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_train_org_type: Vec<i32> = Vec::with_capacity(n_comp);
    let mut out_head_office_state: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_head_office_pc: Vec<String> = Vec::with_capacity(n_comp);
    let mut out_state_of_funding: Vec<String> = Vec::with_capacity(n_comp);

    for (j, &i) in keep.iter().enumerate() {
        let comp_year = spell_completion_year[i];
        let state = spell_state[i];
        let (pc_min, pc_max) = state_postcode_range(state);
        let postcode = format!("{:04}", rng.gen_range(pc_min..=pc_max));
        let foe = spell_foe[i].to_string();
        let qual_idx = spell_qual_idx[i].clamp(1, 6) as usize;

        out_aeuid.push(spell_aeuid[i].to_string());
        out_row_id.push(format!("TVAC{:08}", j + 1));
        out_collection_year.push(comp_year);
        out_age.push(comp_year - spell_birth_year[i]);
        out_birth_year.push(spell_birth_year[i]);
        out_birth_month.push(rng.gen_range(1..=12i32));
        out_gender.push(spell_sex[i]);
        out_school_lvl.push(SCHOOL_CODES[weighted_sample(&mut rng, &SCHOOL_SHARES)]);
        out_hed.push(EDU_TO_HED[spell_education[i].clamp(0, 5) as usize]);
        out_prior_ed.push(if spell_education[i] >= 3 { "Y" } else { "N" });
        out_lfs.push(LFS_CODES[weighted_sample(&mut rng, &LFS_SHARES)]);
        out_anzsco.push(spell_anzsco[i].to_string());
        out_program_id.push(format!(
            "P{}{:03}{:04}",
            foe,
            spell_qual_idx[i],
            (j + 1) % 10_000
        ));
        out_program_foe.push(foe.clone());
        out_program_loe.push(QUAL_CODES[qual_idx - 1]);
        out_package_id.push(format!("PKG{}{}", foe, spell_qual_idx[i]));
        out_completed_date.push(format!(
            "{:04}-{:02}-{:02}",
            comp_year,
            rng.gen_range(1..=12i32),
            rng.gen_range(1..=28i32)
        ));
        out_completed_year.push(comp_year);
        out_client_pc.push(postcode.clone());
        // AVETMISS two-character state identifier (01-08/09/99).
        out_client_state.push(codeframes::state_avetmiss(state).to_string());
        out_remote.push(REMOTE_CODES[weighted_sample(&mut rng, &REMOTE_SHARES)]);
        out_seifa.push(SEIFA_CODES[weighted_sample(&mut rng, &SEIFA_SHARES)]);
        out_rto_id.push(spell_rto_id[i].to_string());
        out_train_org_type.push(spell_prov_type[i]);
        out_head_office_state.push(codeframes::state_avetmiss(state).to_string());
        out_head_office_pc.push(postcode);
        out_state_of_funding.push(codeframes::state_avetmiss(state).to_string());
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        TVA_ROW_ID = out_row_id,
        COLLECTION_YR = out_collection_year,
        AGE = out_age,
        YEAR_OF_BIRTH = out_birth_year,
        MONTH_OF_BIRTH = out_birth_month,
        GENDER_ID = out_gender,
        HIGHEST_SCHL_LVL_ST = out_school_lvl,
        HIGHEST_ED_LVL_ST = out_hed,
        PRIOR_ED_ACHIEVE_FG = out_prior_ed,
        AT_SCHOOL_FG = out_at_school,
        LABOUR_FORCE_STATUS_ID = out_lfs,
        ANZSCO_ID = out_anzsco,
        PROGRAM_ID = out_program_id,
        PROGRAM_FOE_ID = out_program_foe,
        PROGRAM_LOE_ID = out_program_loe,
        PROGRAM_RECOGNITION_ID = out_program_recognition,
        PROGRAM_TRAINING_PACKAGE_ID = out_package_id,
        PROGRAM_TYPE_OF_TRAINING_ID = out_program_type,
        DATE_PROGRAM_COMPLETED = out_completed_date,
        YR_PROGRAM_COMPLETED = out_completed_year,
        CLIENT_POSTCODE_DERIVED = out_client_pc,
        CLIENT_STATE_RESIDENCE_DERIVED = out_client_state,
        CLIENT_REMOTENESS_ID_DERIVED = out_remote,
        CLIENT_SEIFA_IRSD_QUINT_DRVD = out_seifa,
        RTO_ID = out_rto_id,
        TRAIN_ORG_TYPE_ID = out_train_org_type,
        HEAD_OFFICE_STATE = out_head_office_state,
        HEAD_OFFICE_POSTCODE = out_head_office_pc,
        STATE_OF_FUNDING_GF = out_state_of_funding,
    )
}

fn empty_tva_completions_list() -> List {
    let es: Vec<String> = Vec::new();
    let ei: Vec<i32> = Vec::new();

    list!(
        SYNTHETIC_AEUID = es.clone(),
        TVA_ROW_ID = es.clone(),
        COLLECTION_YR = ei.clone(),
        AGE = ei.clone(),
        YEAR_OF_BIRTH = ei.clone(),
        MONTH_OF_BIRTH = ei.clone(),
        GENDER_ID = ei.clone(),
        HIGHEST_SCHL_LVL_ST = ei.clone(),
        HIGHEST_ED_LVL_ST = ei.clone(),
        PRIOR_ED_ACHIEVE_FG = es.clone(),
        AT_SCHOOL_FG = es.clone(),
        LABOUR_FORCE_STATUS_ID = ei.clone(),
        ANZSCO_ID = es.clone(),
        PROGRAM_ID = es.clone(),
        PROGRAM_FOE_ID = es.clone(),
        PROGRAM_LOE_ID = ei.clone(),
        PROGRAM_RECOGNITION_ID = ei.clone(),
        PROGRAM_TRAINING_PACKAGE_ID = es.clone(),
        PROGRAM_TYPE_OF_TRAINING_ID = ei.clone(),
        DATE_PROGRAM_COMPLETED = es.clone(),
        YR_PROGRAM_COMPLETED = ei.clone(),
        CLIENT_POSTCODE_DERIVED = es.clone(),
        CLIENT_STATE_RESIDENCE_DERIVED = es.clone(),
        CLIENT_REMOTENESS_ID_DERIVED = ei.clone(),
        CLIENT_SEIFA_IRSD_QUINT_DRVD = ei.clone(),
        RTO_ID = es.clone(),
        TRAIN_ORG_TYPE_ID = ei.clone(),
        HEAD_OFFICE_STATE = es.clone(),
        HEAD_OFFICE_POSTCODE = es.clone(),
        STATE_OF_FUNDING_GF = es.clone(),
    )
}

extendr_module! {
    mod tva;
    fn select_tva_participants__;
    fn project_tva_activity__;
    fn project_tva_completions__;
}

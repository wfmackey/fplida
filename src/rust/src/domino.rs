use crate::nominal;
use crate::sampling::weighted_sample;
use extendr_api::prelude::*;
use rand::prelude::*;
use rand::rngs::StdRng;
use rand_distr::{Distribution, Gamma};

/// Reference year of the DOMINO extract: ages and death flags are as at this
/// year.
const DOMINO_REF_YEAR: i32 = 2024;

// Benefit duration parameters: (median_days, shape)
fn duration_params(ben_type: &str) -> (f64, f64) {
    match ben_type {
        "DSP" => (2190.0, 0.8),
        "NSA" => (240.0, 1.5),
        "JSP" => (240.0, 1.5),
        "YAL" => (540.0, 1.2),
        "PPP" => (1095.0, 1.5),
        "PPS" => (1460.0, 1.2),
        "AGE" => (5840.0, 0.5),
        "CAR" => (730.0, 1.0),
        "CDA" => (1825.0, 0.8),
        "FTB" => (2190.0, 1.0),
        "ABY" => (730.0, 1.2),
        "AUS" => (730.0, 1.2),
        _ => (365.0, 1.0),
    }
}

// Precomputed qgamma(0.5, shape) for the shapes we use
fn gamma_median_quantile(shape: f64) -> f64 {
    if (shape - 0.5).abs() < 0.01 {
        0.22749
    } else if (shape - 0.8).abs() < 0.01 {
        0.55378
    } else if (shape - 1.0).abs() < 0.01 {
        0.69315 // ln(2)
    } else if (shape - 1.2).abs() < 0.01 {
        0.85220
    } else if (shape - 1.5).abs() < 0.01 {
        1.14473
    } else {
        // Wilson-Hilferty approximation
        (shape * (1.0 - 1.0 / (9.0 * shape)).powi(3)).max(0.1)
    }
}

fn draw_spell_duration(rng: &mut StdRng, median_days: f64, shape: f64) -> i32 {
    let q50 = gamma_median_quantile(shape);
    let scale = median_days / q50;
    let gamma = Gamma::new(shape, scale).unwrap();
    let days = gamma.sample(rng).round() as i32;
    days.max(30)
}

const END_RSN_CODES: [&str; 12] = [
    "EMP", "INC", "ASS", "NFR", "APR", "ADE", "AGE", "OTH", "FSD", "NRQ", "CLR", "6WK",
];
const END_RSN_WEIGHTS: [f64; 12] = [
    0.25, 0.15, 0.05, 0.10, 0.10, 0.01, 0.05, 0.15, 0.05, 0.02, 0.02, 0.05,
];

// Date arithmetic helpers (days since 1970-01-01)
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

fn idx1_to0(idx1: i32, len: usize) -> usize {
    if len == 0 {
        0
    } else {
        idx1.saturating_sub(1).clamp(0, (len - 1) as i32) as usize
    }
}

fn year_start_date_string(year: i32) -> String {
    format!("{:04}-01-01", year)
}

fn year_end_date_string(year: i32) -> String {
    format!("{:04}-12-31", year)
}

const STATE_ABBR: [&str; 8] = ["NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"];
// Education level attained uses the custodian's alphabetic codes, not the
// 00-05 band invented here. The six spine education levels map onto the
// nearest published code so the relationship with the spine survives.
const EDU_ATTAINED_CODES: [&str; 6] = ["UNK", "Y11", "Y12", "C03", "DIP", "BAC"];
// The description column holds the label for the code beside it, using the
// custodian's wording.
const EDU_ATTAINED_DESC: [&str; 6] = [
    "Unknown",
    "Year 11",
    "Year 12",
    "Certificate 3",
    "Diploma",
    "Bachelor's Degree",
];

// Accommodation, home ownership and rent type all have published DSS code
// sets. The codes below are members of them; the weights remain synthetic.
const ACCOM_CODES: [&str; 5] = ["NAS", "SHR", "BAL", "LWP", "SHH"];
const ACCOM_WEIGHTS: [f64; 5] = [0.55, 0.20, 0.10, 0.10, 0.05];

// HOM/POH are homeowners, NHO is not, and NHO is what pays rent.
const HO_CODES: [&str; 5] = ["HOM", "POH", "NHO", "JNT", "OTH"];
const HO_PROBS_LOW: [f64; 5] = [0.10, 0.08, 0.70, 0.07, 0.05];
const HO_PROBS_HIGH: [f64; 5] = [0.38, 0.27, 0.25, 0.07, 0.03];

const MED_GROUPS: [&str; 22] = [
    "MUS", "PSY", "INT", "NER", "CIR", "RES", "SEN", "ABI", "CAN", "CHR", "CFS", "CGA", "EIS",
    "GIS", "IFD", "IHD", "AMP", "REP", "SDB", "URO", "VIS", "PFC",
];
const MED_SHARES: [f64; 22] = [
    0.280, 0.250, 0.050, 0.080, 0.040, 0.040, 0.030, 0.030, 0.030, 0.050, 0.010, 0.010, 0.020,
    0.020, 0.010, 0.010, 0.010, 0.010, 0.005, 0.005, 0.005, 0.005,
];
// Activity participation is a Yes/No/Not-required domain, not the A/E/N/P
// invented here before the DSS data element was found. The weights stay
// synthetic; the codes are the custodian's.
const ACTV_CODES: [&str; 3] = ["Y", "N", "NR"];
const ACTV_WEIGHTS: [f64; 3] = [0.45, 0.35, 0.20];

// Impairment is a rating from 0 to 95 in steps of 5, published in the
// Impairment Tables. The old 1/2/3 was a severity band that does not exist in
// the source. 20 points is the DSP qualification threshold, so the
// distribution is weighted around it rather than spread uniformly.
const IMPRMT_RATINGS: [i32; 20] = [
    0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95,
];
const IMPRMT_WEIGHTS: [f64; 20] = [
    0.02, 0.03, 0.05, 0.08, 0.14, 0.13, 0.12, 0.10, 0.08, 0.06, 0.05, 0.04, 0.03, 0.02, 0.015,
    0.01, 0.008, 0.005, 0.003, 0.002,
];
const CAPACITY_BINS: [i32; 5] = [0, 7, 14, 22, 30];
const CAPACITY_WEIGHTS: [f64; 5] = [0.45, 0.15, 0.15, 0.15, 0.10];

fn parse_date_string(value: &str) -> Option<i32> {
    if value.len() < 10 {
        return None;
    }
    let year = value[0..4].parse::<i32>().ok()?;
    let month = value[5..7].parse::<i32>().ok()?;
    let day = value[8..10].parse::<i32>().ok()?;
    Some(date_to_days(year, month, day))
}

fn parse_year_string(value: &str) -> Option<i32> {
    if value.len() < 4 {
        return None;
    }
    value[0..4].parse::<i32>().ok()
}

fn daily_rate_2024(ben_type: &str) -> f64 {
    match ben_type {
        "DSP" => 1020.40 / 14.0,
        "NSA" => 749.20 / 14.0,
        "JSP" => 749.20 / 14.0,
        "YAL" => 527.80 / 14.0,
        "PPP" => 660.80 / 14.0,
        "PPS" => 940.40 / 14.0,
        "AGE" => 1064.00 / 14.0,
        "CAR" => 1020.40 / 14.0,
        "CDA" => 144.40 / 14.0,
        "FTB" => 213.36 / 14.0,
        "ABY" => 527.80 / 14.0,
        "AUS" => 527.80 / 14.0,
        _ => 50.0,
    }
}

/// Which published series a payment rate follows.
///
/// Pensions — Age, Disability Support, Carer Payment and Parenting Payment
/// Single — take the higher of the CPI and the Pensioner and Beneficiary Living
/// Cost Index, and are then benchmarked to Male Total Average Weekly Earnings.
/// The MTAWE benchmark binds in most years, so the pension path follows wages.
/// Allowances and family payments are indexed to the CPI alone.
///
/// The difference compounds. Pensions have grown roughly a third faster than
/// allowances since 2006, and the gap between the Age Pension and JobSeeker is
/// one of the most-studied features of Australian income support. Deflating
/// every payment at a flat 2.5 per cent, as this generator used to, erased it.
fn rate_series(ben_type: &str) -> nominal::Series {
    match ben_type {
        "AGE" | "DSP" | "CAR" | "PPS" => nominal::Series::Transfer,
        _ => nominal::Series::Price,
    }
}

fn payment_module(ben_type: &str) -> Option<&'static str> {
    match ben_type {
        "BRV" | "BSW" | "CAR" | "CDA" | "DSP" | "DWS" | "MEP" | "MOB" | "WFD" | "WFR" => {
            Some("dis")
        }
        "JSA" | "JSP" | "MAA" | "MPA" | "NMA" | "NSA" | "NSS" | "PPP" | "PPS" | "PTA" => {
            Some("wrk")
        }
        "AEP" | "BBY" | "CCF" | "CCI" | "DAP" | "DOP" | "EIC" | "FPA" | "FTB" | "FTP" | "PPL" => {
            Some("fam")
        }
        "ABA" | "ABS" | "ABY" | "AUS" | "EPA" | "EPF" | "PES" | "TAP" | "YAL" | "YLA" | "YLS"
        | "YTA" => Some("std"),
        "AGE" | "BVA" | "PDB" | "PEN" | "SHC" | "WDA" | "WFA" | "WID" => Some("age"),
        _ => None,
    }
}

fn empty_pyh_list() -> List {
    let cs: Vec<String> = Vec::new();
    let ds: Vec<Option<String>> = Vec::new();
    let ns: Vec<f64> = Vec::new();
    list!(
        SYNTHETIC_AEUID = cs.clone(),
        BEN_TYPE = cs.clone(),
        PERIOD_START_DATE = cs.clone(),
        PERIOD_END_DATE = ds,
        CMPNT_ID = cs.clone(),
        CMPNT_TYPE = cs.clone(),
        CMPNT_DLY_AMT = ns,
        PYH_SOURCE = cs,
    )
}

fn empty_mcd_list() -> List {
    let cs: Vec<String> = Vec::new();
    let opt_cs: Vec<Option<String>> = Vec::new();
    let is: Vec<i32> = Vec::new();
    list!(
        SYNTHETIC_AEUID = cs.clone(),
        PERIOD_START_DATE = cs.clone(),
        PERIOD_END_DATE = cs.clone(),
        ASSMT_ID = cs.clone(),
        MED_PRMY_GRP = cs.clone(),
        MED_SCNDRY_GRP = opt_cs.clone(),
        MED_SCNDRY_ID = opt_cs.clone(),
        MED_SCNDRY_PERM = opt_cs.clone(),
        IMPRMT_CODE = is.clone(),
        IMPRMT_RATE = is.clone(),
        IMPRMT_VER_DATE = cs.clone(),
        CURR_CAPCTY_NUM = is.clone(),
        WITH_INTRVN_NUM = is.clone(),
        INCAP_START = cs.clone(),
        INCAP_END = opt_cs.clone(),
        INCAP_EXEMPT = cs.clone(),
        INCAP_WK_WRK_HRS = is.clone(),
        TEMP_LMT_CAP_START_DATE = opt_cs.clone(),
        TEMP_LMT_CAP_END_DATE = opt_cs.clone(),
        TEMP_REDN_CAP_HRS_NUM = Vec::<Rint>::new(),
        TEMP_REDN_CAP_END_DATE = opt_cs,
        MAN_CODE = cs.clone(),
        ACTV_PRTCPN_CODE = cs.clone(),
        AMR = cs.clone(),
        RFRL_RSN_CODE = cs.clone(),
        CHNL = cs,
    )
}

/// Select DOMINO participants from spine vectors.
/// @export
#[extendr]
fn select_domino_participants__(
    birth_year: &[i32],
    baseline_income: &[f64],
    sex: &[i32],
    education: &[i32],
    aeuid_dss: Strings,
    disability_onset_year: &[i32],
    disability_is_dc: &[i32],
    disability_severity: &[i32],
    seed: i32,
    min_year: i32,
    max_year: i32,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(900));
    let mid_yr = ((min_year as f64 + max_year as f64) / 2.0).round() as i32;
    let n_years = max_year - min_year + 1;
    let aeuids: Vec<String> = aeuid_dss.iter().map(|s| s.to_string()).collect();

    let mut idx: Vec<usize> = Vec::new();
    let mut phase2_dc: Vec<bool> = Vec::new();
    let mut phase2_nc: Vec<bool> = Vec::new();

    for i in 0..n {
        let income = baseline_income[i];
        let age_mid = mid_yr - birth_year[i];
        let mut p_contact: f64 = if income < 25_000.0 {
            0.08
        } else if income < 50_000.0 {
            0.03
        } else {
            0.005
        };
        if income < 15_000.0 {
            p_contact = (p_contact * 4.0).min(0.5);
        }
        let mut p_ever: f64 = 1.0 - (1.0_f64 - p_contact).powi(n_years);
        let is_age_pension = age_mid >= 65;
        if is_age_pension {
            p_ever = 0.70;
        }
        let is_young = (16..=24).contains(&age_mid);
        if is_young && !is_age_pension {
            p_ever = p_ever.max(0.20);
        }
        let is_parent_proxy = sex[i] == 2 && (25..=45).contains(&age_mid);
        if is_parent_proxy {
            p_ever = p_ever.max(0.25);
        }

        let onset = disability_onset_year.get(i).copied().unwrap_or(i32::MIN);
        let is_dc_raw = disability_is_dc.get(i).copied().unwrap_or(i32::MIN);
        let severity = disability_severity.get(i).copied().unwrap_or(i32::MIN);
        let has_dis = onset != i32::MIN;
        let dis_severe = has_dis && severity != i32::MIN && severity <= 2;
        let dis_dc = dis_severe && is_dc_raw == 1;
        let dis_nc = dis_severe && is_dc_raw == 0;
        if dis_dc {
            p_ever = p_ever.max(0.95);
        }
        if dis_nc {
            p_ever = p_ever.max(0.80);
        }

        if rng.gen::<f64>() >= p_ever {
            continue;
        }
        let age_at_start = min_year - birth_year[i];
        if age_at_start + n_years <= 16 {
            continue;
        }

        idx.push(i);
        phase2_dc.push(dis_dc);
        phase2_nc.push(dis_nc);
    }

    if idx.is_empty() {
        return list!(
            spine_idx = Vec::<i32>::new(),
            aeuid = Vec::<String>::new(),
            primary_ben = Vec::<String>::new(),
            first_year = Vec::<i32>::new(),
            last_year = Vec::<i32>::new(),
            phase2_dc = Vec::<bool>::new(),
            phase2_nc = Vec::<bool>::new(),
        );
    }

    let n_part = idx.len();
    let mut out_spine_idx: Vec<i32> = Vec::with_capacity(n_part);
    let mut out_aeuid: Vec<String> = Vec::with_capacity(n_part);
    let mut out_primary_ben: Vec<String> = Vec::with_capacity(n_part);
    let mut out_first_year: Vec<i32> = Vec::with_capacity(n_part);
    let mut out_last_year: Vec<i32> = Vec::with_capacity(n_part);

    for (pos, &i) in idx.iter().enumerate() {
        let income = baseline_income[i];
        let age_start = min_year - birth_year[i];
        let age_end = max_year - birth_year[i];
        let age_mid = mid_yr - birth_year[i];
        let is_parent_proxy = sex[i] == 2 && (25..=45).contains(&age_mid);

        let mut ben = String::new();
        let on_age = age_end >= 65 && age_start < 90;
        if on_age {
            ben = "AGE".to_string();
        }
        if ben.is_empty() && sex[i] == 2 && (25..=45).contains(&age_mid) && income < 50_000.0 {
            ben = if rng.gen::<f64>() < 0.55 {
                "PPP"
            } else {
                "PPS"
            }
            .to_string();
        }
        if ben.is_empty() && is_parent_proxy && income < 80_000.0 {
            ben = "FTB".to_string();
        }
        if ben.is_empty() && (16..=24).contains(&age_start) {
            ben = "YAL".to_string();
        }
        if ben.is_empty() && (18..=30).contains(&age_start) && education[i] >= 4 {
            ben = if rng.gen::<f64>() < 0.4 { "ABY" } else { "AUS" }.to_string();
        }
        if ben.is_empty() && age_start >= 18 && age_end < 65 && rng.gen::<f64>() < 0.03 {
            ben = "DSP".to_string();
        }
        if ben.is_empty() && age_start >= 18 && rng.gen::<f64>() < 0.02 {
            ben = if rng.gen::<f64>() < 0.4 { "CAR" } else { "CDA" }.to_string();
        }
        if ben.is_empty() && income < 40_000.0 {
            ben = "NSA".to_string();
        }
        if ben.is_empty() {
            ben = "NSA".to_string();
        }

        let mut first_year = (birth_year[i] + 16).max(min_year);
        if on_age {
            first_year = (birth_year[i] + 65).max(min_year);
        } else {
            let offset = weighted_sample(&mut rng, &[0.4, 0.3, 0.2, 0.1]) as i32;
            first_year = (first_year + offset).min(max_year);
        }
        first_year = first_year.max(min_year).min(max_year);

        let mut last_year = max_year;
        if rng.gen::<f64>() < 0.30 {
            last_year = (first_year + rng.gen_range(1..=8i32)).min(max_year);
        }

        let onset = disability_onset_year[i];
        let severity = disability_severity[i];
        let is_dc_raw = disability_is_dc[i];
        let has_dis = onset != i32::MIN;
        let dis_severe = has_dis && severity != i32::MIN && severity <= 2;
        let is_dc = dis_severe && is_dc_raw == 1;
        let is_nc = dis_severe && is_dc_raw == 0;
        let dc_rate = if severity == 1 {
            0.65
        } else if severity == 2 {
            0.48
        } else {
            0.0
        };
        let nc_rate = if severity == 1 {
            0.35
        } else if severity == 2 {
            0.22
        } else {
            0.0
        };
        let gets_dsp =
            (is_dc && rng.gen::<f64>() < dc_rate) || (is_nc && rng.gen::<f64>() < nc_rate);
        if gets_dsp {
            ben = "DSP".to_string();
            let lag = rng.gen_range(0..=2i32);
            first_year = (onset + lag).max(min_year).min(max_year);
            last_year = max_year;
        }

        out_spine_idx.push((i + 1) as i32);
        out_aeuid.push(aeuids[i].clone());
        out_primary_ben.push(ben);
        out_first_year.push(first_year);
        out_last_year.push(last_year);
        phase2_dc[pos] = is_dc;
        phase2_nc[pos] = is_nc;
    }

    list!(
        spine_idx = out_spine_idx,
        aeuid = out_aeuid,
        primary_ben = out_primary_ben,
        first_year = out_first_year,
        last_year = out_last_year,
        phase2_dc = phase2_dc,
        phase2_nc = phase2_nc,
    )
}

/// Generate DOMINO medical assessments from benefit spell vectors.
/// @export
#[extendr]
fn generate_medical_assessments__(
    det_ben_aeuid: Strings,
    det_ben_type_code: Strings,
    det_ben_period_start_date: Strings,
    seed: i32,
) -> List {
    let ben_types: Vec<String> = det_ben_type_code.iter().map(|s| s.to_string()).collect();
    let spell_starts: Vec<String> = det_ben_period_start_date
        .iter()
        .map(|s| s.to_string())
        .collect();
    let mut keep: Vec<usize> = Vec::new();
    for (i, ben_type) in ben_types.iter().enumerate() {
        if ben_type == "DSP" || ben_type == "CAR" {
            keep.push(i);
        }
    }
    if keep.is_empty() {
        return empty_mcd_list();
    }

    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(902));
    let mut out_aeuid: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_period_start: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_period_end: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_assmt_id: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_med_prmy: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_med_scndry: Vec<Option<String>> = Vec::with_capacity(keep.len());
    let mut out_med_scndry_id: Vec<Option<String>> = Vec::with_capacity(keep.len());
    let mut out_med_scndry_perm: Vec<Option<String>> = Vec::with_capacity(keep.len());
    let mut out_imprmt_code: Vec<i32> = Vec::with_capacity(keep.len());
    let mut out_imprmt_rate: Vec<i32> = Vec::with_capacity(keep.len());
    let mut out_imprmt_ver_date: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_curr_capcty: Vec<i32> = Vec::with_capacity(keep.len());
    let mut out_with_intrvn: Vec<i32> = Vec::with_capacity(keep.len());
    let mut out_incap_start: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_incap_end: Vec<Option<String>> = Vec::with_capacity(keep.len());
    let mut out_incap_exempt: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_incap_wk_hrs: Vec<i32> = Vec::with_capacity(keep.len());
    let mut out_temp_start: Vec<Option<String>> = Vec::with_capacity(keep.len());
    let mut out_temp_end: Vec<Option<String>> = Vec::with_capacity(keep.len());
    let mut out_temp_hours: Vec<Rint> = Vec::with_capacity(keep.len());
    let mut out_temp_redn_end: Vec<Option<String>> = Vec::with_capacity(keep.len());
    let mut out_man_code: Vec<Option<String>> = Vec::with_capacity(keep.len());
    let mut out_actv_prtcpn: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_amr: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_rfrl: Vec<String> = Vec::with_capacity(keep.len());
    let mut out_chnl: Vec<String> = Vec::with_capacity(keep.len());

    for (j, &i) in keep.iter().enumerate() {
        let spell_start_days =
            parse_date_string(&spell_starts[i]).unwrap_or_else(|| date_to_days(2005, 1, 1));
        let has_secondary = rng.gen::<f64>() < 0.30;
        let med_scndry = if has_secondary {
            Some(MED_GROUPS[weighted_sample(&mut rng, &MED_SHARES)].to_string())
        } else {
            None
        };
        let curr_capcty = (CAPACITY_BINS[weighted_sample(&mut rng, &CAPACITY_WEIGHTS)]
            + rng.gen_range(-2..=2i32))
        .clamp(0, 30);
        let period_start_days = spell_start_days + rng.gen_range(-180..=30i32);
        let period_end_days = period_start_days + rng.gen_range(90..=365i32);
        let incap_start_days = period_start_days + rng.gen_range(0..=30i32);
        let has_incap_end = rng.gen::<f64>() < 0.60;
        let incap_end_days = if has_incap_end {
            Some(incap_start_days + rng.gen_range(90..=730i32))
        } else {
            None
        };
        let has_temp = rng.gen::<f64>() < 0.20;
        let temp_start_days = if has_temp {
            Some(incap_start_days + rng.gen_range(0..=60i32))
        } else {
            None
        };
        let temp_end_days = temp_start_days.map(|start| start + rng.gen_range(90..=365i32));

        out_aeuid.push(det_ben_aeuid[i].to_string());
        out_period_start.push(days_to_date_string(period_start_days));
        out_period_end.push(days_to_date_string(period_end_days));
        out_assmt_id.push(format!("A{:08}", j + 1));
        // MED_PRMY_GRP: drawn on the published population shares.
        let med_prmy = MED_GROUPS[weighted_sample(&mut rng, &MED_SHARES)].to_string();
        out_med_prmy.push(med_prmy);
        out_med_scndry.push(med_scndry.clone());
        out_med_scndry_id.push(if has_secondary {
            Some(format!("S{:04}", rng.gen_range(1..=9999i32)))
        } else {
            None
        });
        out_med_scndry_perm.push(if has_secondary {
            Some(if rng.gen::<f64>() < 0.60 { "Y" } else { "N" }.to_string())
        } else {
            None
        });
        let imprmt = IMPRMT_RATINGS[weighted_sample(&mut rng, &IMPRMT_WEIGHTS)];
        out_imprmt_code.push(imprmt);
        // The rate reports the same rating, so it must agree with the code
        // rather than being drawn independently.
        out_imprmt_rate.push(imprmt);
        out_imprmt_ver_date.push(days_to_date_string(
            period_start_days + rng.gen_range(0..=14i32),
        ));
        out_curr_capcty.push(curr_capcty);
        out_with_intrvn.push((curr_capcty + rng.gen_range(0..=8i32)).min(40));
        out_incap_start.push(days_to_date_string(incap_start_days));
        out_incap_end.push(incap_end_days.map(days_to_date_string));
        out_incap_exempt.push(if rng.gen::<f64>() < 0.05 { "Y" } else { "N" }.to_string());
        out_incap_wk_hrs.push((curr_capcty - rng.gen_range(0..=5i32)).clamp(0, 30));
        out_temp_start.push(temp_start_days.map(days_to_date_string));
        out_temp_end.push(temp_end_days.map(days_to_date_string));
        out_temp_hours.push(if has_temp {
            Rint::from(rng.gen_range(8..=22i32))
        } else {
            Rint::na()
        });
        out_temp_redn_end.push(temp_end_days.map(days_to_date_string));
        // MAN_CODE names the ground on which a manifest grant was made — 14
        // published grounds such as permanent blindness or category 4 HIV. It
        // was being used as a Y/N flag, which has no code for "not manifest":
        // that case is absence, not a value.
        out_man_code.push(if ben_types[i] == "DSP" && rng.gen::<f64>() < 0.15 {
            Some(
                crate::codeframes::sample_researched("DOMINO", "MAN_CODE", &mut rng)
                    .unwrap_or("BLI")
                    .to_string(),
            )
        } else {
            None
        });
        out_actv_prtcpn.push(ACTV_CODES[weighted_sample(&mut rng, &ACTV_WEIGHTS)].to_string());
        out_amr.push(format!("R{:06}", rng.gen_range(1..=999_999i32)));
        // Referral reason and channel both have published code sets far wider
        // than the handful invented here, and neither publishes frequencies.
        out_rfrl.push(
            crate::codeframes::sample_researched("DOMINO", "RFRL_RSN_CODE", &mut rng)
                .unwrap_or("COCC")
                .to_string(),
        );
        out_chnl.push(
            crate::codeframes::sample_researched("DOMINO", "CHNL", &mut rng)
                .unwrap_or("CSO")
                .to_string(),
        );
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        PERIOD_START_DATE = out_period_start,
        PERIOD_END_DATE = out_period_end,
        ASSMT_ID = out_assmt_id,
        MED_PRMY_GRP = out_med_prmy,
        MED_SCNDRY_GRP = out_med_scndry,
        MED_SCNDRY_ID = out_med_scndry_id,
        MED_SCNDRY_PERM = out_med_scndry_perm,
        IMPRMT_CODE = out_imprmt_code,
        IMPRMT_RATE = out_imprmt_rate,
        IMPRMT_VER_DATE = out_imprmt_ver_date,
        CURR_CAPCTY_NUM = out_curr_capcty,
        WITH_INTRVN_NUM = out_with_intrvn,
        INCAP_START = out_incap_start,
        INCAP_END = out_incap_end,
        INCAP_EXEMPT = out_incap_exempt,
        INCAP_WK_WRK_HRS = out_incap_wk_hrs,
        TEMP_LMT_CAP_START_DATE = out_temp_start,
        TEMP_LMT_CAP_END_DATE = out_temp_end,
        TEMP_REDN_CAP_HRS_NUM = out_temp_hours,
        TEMP_REDN_CAP_END_DATE = out_temp_redn_end,
        MAN_CODE = out_man_code,
        ACTV_PRTCPN_CODE = out_actv_prtcpn,
        AMR = out_amr,
        RFRL_RSN_CODE = out_rfrl,
        CHNL = out_chnl,
    )
}

/// Generate DOMINO payment history tables from benefit spell vectors.
/// @export
#[extendr]
fn generate_payment_history__(
    det_ben_aeuid: Strings,
    det_ben_type_code: Strings,
    det_ben_period_start_date: Strings,
    det_ben_period_end_date: Strings,
    seed: i32,
) -> List {
    let ben_types: Vec<String> = det_ben_type_code.iter().map(|s| s.to_string()).collect();
    if ben_types.is_empty() {
        let empty = empty_pyh_list();
        return list!(
            pyh_combined_dis = empty.clone(),
            pyh_combined_wrk = empty.clone(),
            pyh_combined_fam = empty.clone(),
            pyh_combined_std = empty.clone(),
            pyh_combined_age = empty,
        );
    }

    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(903));
    let mut dis_aeuid = Vec::new();
    let mut dis_ben = Vec::new();
    let mut dis_start = Vec::new();
    let mut dis_end: Vec<Option<String>> = Vec::new();
    let mut dis_cmpnt = Vec::new();
    let mut dis_amt = Vec::new();

    let mut wrk_aeuid = Vec::new();
    let mut wrk_ben = Vec::new();
    let mut wrk_start = Vec::new();
    let mut wrk_end: Vec<Option<String>> = Vec::new();
    let mut wrk_cmpnt = Vec::new();
    let mut wrk_amt = Vec::new();

    let mut fam_aeuid = Vec::new();
    let mut fam_ben = Vec::new();
    let mut fam_start = Vec::new();
    let mut fam_end: Vec<Option<String>> = Vec::new();
    let mut fam_cmpnt = Vec::new();
    let mut fam_amt = Vec::new();

    let mut std_aeuid = Vec::new();
    let mut std_ben = Vec::new();
    let mut std_start = Vec::new();
    let mut std_end: Vec<Option<String>> = Vec::new();
    let mut std_cmpnt = Vec::new();
    let mut std_amt = Vec::new();

    let mut age_aeuid = Vec::new();
    let mut age_ben = Vec::new();
    let mut age_start = Vec::new();
    let mut age_end: Vec<Option<String>> = Vec::new();
    let mut age_cmpnt = Vec::new();
    let mut age_amt = Vec::new();

    for i in 0..ben_types.len() {
        let ben_type = &ben_types[i];
        let start = det_ben_period_start_date[i].to_string();
        let end = if det_ben_period_end_date[i].is_na() {
            None
        } else {
            Some(det_ben_period_end_date[i].to_string())
        };
        // The stored rates are 2024 rates, so the index is taken relative to
        // 2024 rather than to the nominal module's own anchor.
        //
        // The spread is one-sided. `daily_rate_2024` is the legislated MAXIMUM
        // basic rate, so what someone is actually paid can fall below it on the
        // income test or across a part period, but can never sit above it. The
        // symmetric ±5 per cent this used to draw put a fifth of payments above
        // the statutory maximum. Keep in step with `.generate_payment_history()`
        // in R/generate_domino.R.
        let year = parse_year_string(&start).unwrap_or(DOMINO_REF_YEAR);
        let series = rate_series(ben_type);
        let deflator = nominal::index(series, nominal::Basis::Calendar, year)
            / nominal::index(series, nominal::Basis::Calendar, DOMINO_REF_YEAR);
        let amount =
            ((daily_rate_2024(ben_type) * deflator * (1.0 - rng.gen_range(0.0f64..0.10f64)))
                * 100.0)
                .round()
                / 100.0;
        let cmpnt_id = format!("C{:08}", i + 1);
        let aeuid = det_ben_aeuid[i].to_string();

        match payment_module(ben_type) {
            Some("dis") => {
                dis_aeuid.push(aeuid);
                dis_ben.push(ben_type.clone());
                dis_start.push(start);
                dis_end.push(end);
                dis_cmpnt.push(cmpnt_id);
                dis_amt.push(amount);
            }
            Some("wrk") => {
                wrk_aeuid.push(aeuid);
                wrk_ben.push(ben_type.clone());
                wrk_start.push(start);
                wrk_end.push(end);
                wrk_cmpnt.push(cmpnt_id);
                wrk_amt.push(amount);
            }
            Some("fam") => {
                fam_aeuid.push(aeuid);
                fam_ben.push(ben_type.clone());
                fam_start.push(start);
                fam_end.push(end);
                fam_cmpnt.push(cmpnt_id);
                fam_amt.push(amount);
            }
            Some("std") => {
                std_aeuid.push(aeuid);
                std_ben.push(ben_type.clone());
                std_start.push(start);
                std_end.push(end);
                std_cmpnt.push(cmpnt_id);
                std_amt.push(amount);
            }
            Some("age") => {
                age_aeuid.push(aeuid);
                age_ben.push(ben_type.clone());
                age_start.push(start);
                age_end.push(end);
                age_cmpnt.push(cmpnt_id);
                age_amt.push(amount);
            }
            _ => {}
        }
    }

    let cmpnt_type = |n: usize| vec!["BASIC".to_string(); n];
    let pyh_source = |n: usize| vec!["CLK".to_string(); n];
    let table = |aeuid: Vec<String>,
                 ben: Vec<String>,
                 start: Vec<String>,
                 end: Vec<Option<String>>,
                 cmpnt: Vec<String>,
                 amt: Vec<f64>| {
        list!(
            SYNTHETIC_AEUID = aeuid,
            BEN_TYPE = ben,
            PERIOD_START_DATE = start,
            PERIOD_END_DATE = end,
            CMPNT_ID = cmpnt.clone(),
            CMPNT_TYPE = cmpnt_type(cmpnt.len()),
            CMPNT_DLY_AMT = amt,
            PYH_SOURCE = pyh_source(cmpnt.len()),
        )
    };

    list!(
        pyh_combined_dis = table(dis_aeuid, dis_ben, dis_start, dis_end, dis_cmpnt, dis_amt),
        pyh_combined_wrk = table(wrk_aeuid, wrk_ben, wrk_start, wrk_end, wrk_cmpnt, wrk_amt),
        pyh_combined_fam = table(fam_aeuid, fam_ben, fam_start, fam_end, fam_cmpnt, fam_amt),
        pyh_combined_std = table(std_aeuid, std_ben, std_start, std_end, std_cmpnt, std_amt),
        pyh_combined_age = table(age_aeuid, age_ben, age_start, age_end, age_cmpnt, age_amt),
    )
}

/// Generate DOMINO benefit spells from participant vectors.
/// @export
#[extendr]
fn generate_benefit_spells__(
    aeuid: Strings,
    primary_ben: Strings,
    first_year: &[i32],
    last_year: &[i32],
    death_day: &[i32],
    seed: i32,
    min_year: i32,
    max_year: i32,
) -> List {
    let n = first_year.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(901));

    let min_date = date_to_days(min_year, 1, 1);
    let max_date = date_to_days(max_year, 12, 31);
    let cutover = date_to_days(2020, 3, 20);

    let est_cap = n + n / 10; // some produce 2 spells
    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_cap);
    let mut out_ben_type: Vec<String> = Vec::with_capacity(est_cap);
    let mut out_status: Vec<String> = Vec::with_capacity(est_cap);
    let mut out_start: Vec<String> = Vec::with_capacity(est_cap);
    let mut out_end: Vec<Option<String>> = Vec::with_capacity(est_cap);
    let mut out_durn: Vec<i32> = Vec::with_capacity(est_cap);
    let mut out_end_rsn: Vec<Option<String>> = Vec::with_capacity(est_cap);
    let mut out_end_rsn_code: Vec<Option<String>> = Vec::with_capacity(est_cap);

    let aeuids: Vec<String> = aeuid.iter().map(|s| s.to_string()).collect();
    let ben_types: Vec<String> = primary_ben.iter().map(|s| s.to_string()).collect();

    for i in 0..n {
        let aid = &aeuids[i];
        let mut bt = ben_types[i].clone();
        let fy = first_year[i];
        let ly = last_year[i];

        // NSA -> JSP from 2020
        if bt == "NSA" && fy >= 2020 {
            bt = "JSP".to_string();
        }

        // Draw spell duration from gamma distribution
        let (median, shape) = duration_params(&bt);
        let dur_days = draw_spell_duration(&mut rng, median, shape);

        // Spell start: random day in first_year
        let start_month = rng.gen_range(1..=12i32);
        let start_day = rng.gen_range(1..=28i32);
        let spell_start = date_to_days(fy, start_month, start_day).max(min_date);
        let raw_spell_end = spell_start + dur_days;

        // Payments stop at death. `death_day` is days since epoch, or the R
        // integer NA sentinel for someone with no spine death date.
        let died = death_day[i] != i32::MIN;
        let person_max = if died {
            max_date.min(death_day[i])
        } else {
            max_date
        };
        // A spell that would begin after the person died never happens.
        if spell_start > person_max {
            continue;
        }

        // Determine status and end details
        let status: String;
        let mut end_rsn: Option<String>;
        let end_rsn_code: Option<String>;
        let spell_end_out: Option<i32>;
        let durn: i32;

        if raw_spell_end > person_max {
            if died && person_max < max_date {
                // Still open at death: close it there rather than leaving a
                // spell running past the person's death date.
                status = "CUR".to_string();
                end_rsn = Some("CAN".to_string());
                end_rsn_code = Some("OTH".to_string());
                spell_end_out = Some(person_max);
                durn = person_max - spell_start;
            } else {
                // Ongoing spell
                status = "CUR".to_string();
                end_rsn = None;
                end_rsn_code = None;
                spell_end_out = None;
                durn = person_max - spell_start;
            }
        } else {
            let ly_end = date_to_days(ly, 12, 31);
            if raw_spell_end > ly_end {
                // Ended after last observed year
                let end_month = rng.gen_range(1..=12i32);
                let end_day = rng.gen_range(1..=28i32);
                let mut adj_end = date_to_days(ly, end_month, end_day);
                if adj_end <= spell_start {
                    adj_end = spell_start + 30;
                }
                // Never past the person's death date.
                if adj_end > person_max {
                    adj_end = person_max;
                }
                let eidx = weighted_sample(&mut rng, &END_RSN_WEIGHTS);
                status = "CUR".to_string();
                end_rsn = Some("CAN".to_string());
                end_rsn_code = Some(END_RSN_CODES[eidx].to_string());
                spell_end_out = Some(adj_end);
                durn = adj_end - spell_start;
            } else {
                let eidx = weighted_sample(&mut rng, &END_RSN_WEIGHTS);
                status = "CUR".to_string();
                end_rsn = Some("CAN".to_string());
                end_rsn_code = Some(END_RSN_CODES[eidx].to_string());
                spell_end_out = Some(raw_spell_end);
                durn = dur_days;
            }
        }

        // Some spells are suspended
        let final_status = if end_rsn.is_some() && rng.gen::<f64>() < 0.08 {
            end_rsn = Some("SUS".to_string());
            "SUS".to_string()
        } else {
            status
        };

        // NSA -> JSP switchover: split spell at 2020-03-20
        if bt == "NSA" && spell_start < cutover && spell_end_out.map_or(true, |e| e > cutover) {
            // Spell 1: NSA until cutover
            out_aeuid.push(aid.clone());
            out_ben_type.push("NSA".to_string());
            out_status.push("CUR".to_string());
            out_start.push(days_to_date_string(spell_start));
            out_end.push(Some(days_to_date_string(cutover - 1)));
            out_durn.push(cutover - 1 - spell_start);
            out_end_rsn.push(Some("CAN".to_string()));
            out_end_rsn_code.push(Some("OTH".to_string()));

            // Spell 2: JSP from cutover
            out_aeuid.push(aid.clone());
            out_ben_type.push("JSP".to_string());
            out_status.push(final_status);
            out_start.push(days_to_date_string(cutover));
            out_end.push(spell_end_out.map(days_to_date_string));
            out_durn.push(
                spell_end_out
                    .map(|e| e - cutover)
                    .unwrap_or(max_date - cutover),
            );
            out_end_rsn.push(end_rsn);
            out_end_rsn_code.push(end_rsn_code);
        } else {
            out_aeuid.push(aid.clone());
            out_ben_type.push(bt);
            out_status.push(final_status);
            out_start.push(days_to_date_string(spell_start));
            out_end.push(spell_end_out.map(days_to_date_string));
            out_durn.push(durn);
            out_end_rsn.push(end_rsn);
            out_end_rsn_code.push(end_rsn_code);
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        BEN_TYPE_CODE = out_ben_type,
        BEN_STATUS = out_status,
        PERIOD_START_DATE = out_start,
        PERIOD_END_DATE = out_end,
        DURN_DAYS = out_durn,
        END_RSN = out_end_rsn,
        END_RSN_CODE = out_end_rsn_code,
    )
}

/// Project DOMINO static demographics.
/// @export
#[extendr]
fn project_domino_demogs__(
    participant_aeuid: Strings,
    participant_spine_idx: &[i32],
    spine_birth_year: &[i32],
    spine_month_of_birth: &[i32],
    spine_sex: &[i32],
    spine_country_of_birth: &[i32],
    spine_year_of_death: &[i32],
    spine_month_of_death: &[i32],
    seed: i32,
) -> List {
    let n = participant_aeuid.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(904));

    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut out_birth_year: Vec<i32> = Vec::with_capacity(n);
    let mut out_birth_month: Vec<i32> = Vec::with_capacity(n);
    let mut out_gender: Vec<&str> = Vec::with_capacity(n);
    let mut out_birth_ctry_code: Vec<i32> = Vec::with_capacity(n);
    let mut out_lang_code: Vec<&str> = Vec::with_capacity(n);
    let mut out_interpreter_ind: Vec<&str> = Vec::with_capacity(n);
    let mut out_age: Vec<i32> = Vec::with_capacity(n);
    let mut out_death_ind: Vec<&str> = Vec::with_capacity(n);
    let mut out_year_of_death: Vec<Rint> = Vec::with_capacity(n);
    let mut out_month_of_death: Vec<Rint> = Vec::with_capacity(n);
    let out_pen_blind_start_date: Vec<Option<String>> = vec![None; n];
    let out_object_type_code: Vec<&str> = vec!["PER"; n];

    for i in 0..n {
        let si = idx1_to0(participant_spine_idx[i], spine_birth_year.len());
        let birth_year = spine_birth_year[si];
        let sex = spine_sex[si];
        let age = DOMINO_REF_YEAR - birth_year;

        // Vitals come from the spine, never from a local draw, so DOMINO agrees
        // with CORE, deaths and every other product for the same person. A
        // person is flagged dead only once the spine death year has arrived by
        // the DOMINO reference year.
        let year_of_death = spine_year_of_death[si];
        let month_of_death = spine_month_of_death[si];
        let is_dead = year_of_death != i32::MIN && year_of_death <= DOMINO_REF_YEAR;

        out_aeuid.push(participant_aeuid[i].to_string());
        out_birth_year.push(birth_year);
        out_birth_month.push(spine_month_of_birth[si]);
        out_gender.push(if sex == 1 { "M" } else { "F" });
        out_birth_ctry_code.push(spine_country_of_birth[si]);
        out_lang_code.push(
            ["1201", "4202", "6513", "9200", "3503"]
                [weighted_sample(&mut rng, &[0.80, 0.05, 0.05, 0.05, 0.05])],
        );
        out_interpreter_ind.push(if rng.gen::<f64>() < 0.02 { "Y" } else { "N" });
        out_age.push(age);
        out_death_ind.push(if is_dead { "Y" } else { "N" });
        if is_dead {
            out_year_of_death.push(Rint::from(year_of_death));
            out_month_of_death.push(if month_of_death == i32::MIN {
                Rint::na()
            } else {
                Rint::from(month_of_death)
            });
        } else {
            out_year_of_death.push(Rint::na());
            out_month_of_death.push(Rint::na());
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR_OF_BIRTH = out_birth_year,
        MONTH_OF_BIRTH = out_birth_month,
        GENDER = out_gender,
        BIRTH_CTRY_CODE = out_birth_ctry_code,
        LANG_CODE = out_lang_code,
        INTERPRETER_IND = out_interpreter_ind,
        AGE = out_age,
        DEATH_IND = out_death_ind,
        YEAR_OF_DEATH = out_year_of_death,
        MONTH_OF_DEATH = out_month_of_death,
        PEN_BLIND_START_DATE = out_pen_blind_start_date,
        OBJECT_TYPE_CODE = out_object_type_code,
    )
}

/// Project DOMINO employment income spells.
/// @export
#[extendr]
fn project_domino_income__(
    participant_aeuid: Strings,
    participant_spine_idx: &[i32],
    participant_first_year: &[i32],
    participant_last_year: &[i32],
    spine_baseline_income: &[f64],
    seed: i32,
) -> List {
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(905));

    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_start: Vec<String> = Vec::new();
    let mut out_end: Vec<String> = Vec::new();
    let mut out_employer_id: Vec<String> = Vec::new();
    let mut out_emp_inc_cont: Vec<f64> = Vec::new();
    let mut out_emp_dly_inc_cont: Vec<f64> = Vec::new();
    let mut out_emp_inc_cont_hrs: Vec<i32> = Vec::new();
    let mut out_freq_code: Vec<&str> = Vec::new();
    let mut out_avg_ind: Vec<&str> = Vec::new();

    for i in 0..participant_aeuid.len() {
        let si = idx1_to0(participant_spine_idx[i], spine_baseline_income.len());
        let income = spine_baseline_income[si];
        if income <= 15000.0 {
            continue;
        }

        // Reported employment income is what the person was actually paid, so
        // it follows wages rather than prices, and it carries a person's own
        // deviation: two people on the same anchor income do not report the
        // same figure a decade later.
        //
        // `participant_first_year` and `participant_last_year` are calendar
        // years — `year_start_date_string` writes 1 January and
        // `year_end_date_string` 31 December — so the basis is calendar, with
        // none of the financial-year offset the tax products need. One record
        // covers the whole spell, and the amount attaches to the report that
        // opened it, so the start year is the year it is stated in. That also
        // matches the benefit-rate deflator above, which takes the spell start.
        //
        // The key is the AEUID, which the spine assigns once per person over
        // the whole population. `participant_spine_idx` counts within whichever
        // slice a worker was handed, and the build picks its slice count from
        // the machine's core count, so keying on it would make a person's
        // income depend on the machine that generated it.
        let aeuid = participant_aeuid[i].to_string();
        let paid = income
            * nominal::factor(
                nominal::Series::Wage,
                nominal::Basis::Calendar,
                nominal::PERSON,
                nominal::unit_key(&aeuid),
                seed as i64,
                participant_first_year[i],
            );

        // Only the daily amount is indexed. The fortnightly figure below is
        // fourteen times it, so indexing that as well would count the growth
        // twice and break the identity between the two columns.
        let daily_inc = (paid / 365.0 * 100.0).round() / 100.0;
        // Hours stay on the anchor income. The 2500 divisor is a fixed dollar
        // amount standing in for an hourly rate, so dividing an indexed income
        // by it would report a rise in nominal pay as a rise in hours worked,
        // and a full-time week would arrive by inflation alone.
        let hours = ((income / 2500.0).round() as i32).clamp(0, 40);

        out_aeuid.push(aeuid);
        out_start.push(year_start_date_string(participant_first_year[i]));
        out_end.push(year_end_date_string(participant_last_year[i]));
        out_employer_id.push(format!("E{:08}", rng.gen_range(1..=99_999_999i32)));
        out_emp_inc_cont.push((daily_inc * 14.0 * 100.0).round() / 100.0);
        out_emp_dly_inc_cont.push(daily_inc);
        out_emp_inc_cont_hrs.push(hours);
        // The custodian codes a fortnightly frequency as 2WE. FTN is not in
        // the published domain.
        out_freq_code.push("2WE");
        out_avg_ind.push(if rng.gen::<f64>() < 0.10 { "Y" } else { "N" });
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        PERIOD_START_DATE = out_start,
        PERIOD_END_DATE = out_end,
        EMPLYR_ID = out_employer_id,
        EMP_INC_CONT = out_emp_inc_cont,
        EMP_DLY_INC_CONT = out_emp_dly_inc_cont,
        EMP_INC_CONT_HRS = out_emp_inc_cont_hrs,
        FREQ_CODE = out_freq_code,
        AVG_IND = out_avg_ind,
    )
}

/// Project DOMINO location details.
/// @export
#[extendr]
fn project_domino_locations__(
    participant_aeuid: Strings,
    participant_spine_idx: &[i32],
    participant_first_year: &[i32],
    participant_last_year: &[i32],
    spine_state: &[i32],
    seed: i32,
) -> List {
    let n = participant_aeuid.len();
    // +908: locations previously shared +905 with the income projector, which
    // breaks the one-stream-per-generator invariant in `seeds.rs`.
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(908));

    let mut out_state_name: Vec<&str> = Vec::with_capacity(n);
    let mut out_postcode: Vec<String> = Vec::with_capacity(n);
    let mut out_rmt_ind: Vec<&str> = Vec::with_capacity(n);
    let mut out_meshblock: Vec<String> = Vec::with_capacity(n);
    let mut out_meshmatch: Vec<&str> = Vec::with_capacity(n);
    let mut out_sa1: Vec<String> = Vec::with_capacity(n);
    let mut out_sa2: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        let si = idx1_to0(participant_spine_idx[i], spine_state.len());
        let st = spine_state[si].clamp(1, 8);
        let st_idx = (st - 1) as usize;
        let postcode_num = match st {
            1 => rng.gen_range(2000..=2999i32),
            2 => rng.gen_range(3000..=3999i32),
            3 => rng.gen_range(4000..=4999i32),
            4 => rng.gen_range(5000..=5799i32),
            5 => rng.gen_range(6000..=6999i32),
            6 => rng.gen_range(7000..=7999i32),
            7 => rng.gen_range(800..=899i32),
            _ => rng.gen_range(2600..=2619i32),
        };

        out_state_name.push(STATE_ABBR[st_idx]);
        out_postcode.push(format!("{:04}", postcode_num));
        out_rmt_ind.push(if rng.gen::<f64>() < 0.15 { "Y" } else { "N" });
        out_meshblock.push(format!("{:011}", rng.gen_range(1..=99_999_999i32)));
        out_meshmatch.push(if rng.gen::<f64>() < 0.90 {
            "ADDR"
        } else {
            "COORDS"
        });
        out_sa1.push(format!("{}{:010}", st, rng.gen_range(1..=9_999_999i32)));
        out_sa2.push(format!("{}{:08}", st, rng.gen_range(1..=9_999_999i32)));
    }

    list!(
        SYNTHETIC_AEUID = participant_aeuid
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>(),
        PERIOD_START_DATE = participant_first_year
            .iter()
            .map(|&y| year_start_date_string(y))
            .collect::<Vec<_>>(),
        PERIOD_END_DATE = participant_last_year
            .iter()
            .map(|&y| year_end_date_string(y))
            .collect::<Vec<_>>(),
        // HOM is the published code for a home address; RES is not in the
        // domain.
        ADDR_TYPE_CODE = vec!["HOM"; n],
        ADDR_PRTY = vec![1i32; n],
        STATE = out_state_name,
        POSTCODE = out_postcode,
        CTRY_CODE = vec!["1101"; n],
        CMTY_CODE = vec![None::<String>; n],
        RMT_IND = out_rmt_ind,
        MESHBLOCK = out_meshblock,
        MESHMATCH = out_meshmatch,
        SA1_MAINCODE = out_sa1,
        SA2_MAINCODE = out_sa2,
    )
}

/// Project DOMINO education spells.
/// @export
#[extendr]
fn project_domino_education__(
    participant_aeuid: Strings,
    participant_spine_idx: &[i32],
    participant_first_year: &[i32],
    participant_last_year: &[i32],
    spine_education: &[i32],
    seed: i32,
) -> List {
    let n = participant_aeuid.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(906));

    let mut out_lvl_attained: Vec<&str> = Vec::with_capacity(n);
    let mut out_lvl_desc: Vec<&str> = Vec::with_capacity(n);
    let mut out_student_status: Vec<Option<&str>> = Vec::with_capacity(n);

    for i in 0..n {
        let si = idx1_to0(participant_spine_idx[i], spine_education.len());
        let edu = spine_education[si].clamp(0, 5) as usize;
        out_lvl_attained.push(EDU_ATTAINED_CODES[edu]);
        out_lvl_desc.push(EDU_ATTAINED_DESC[edu]);
        // FTS and PTS are published student statuses. Someone who is not a
        // student has no status rather than a code meaning "not a student":
        // NST is not in the published domain.
        let draw = rng.gen::<f64>();
        out_student_status.push(if draw < 0.05 {
            Some("FTS")
        } else if draw < 0.08 {
            Some("PTS")
        } else {
            None
        });
    }

    list!(
        SYNTHETIC_AEUID = participant_aeuid
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>(),
        PERIOD_START_DATE = participant_first_year
            .iter()
            .map(|&y| year_start_date_string(y))
            .collect::<Vec<_>>(),
        PERIOD_END_DATE = participant_last_year
            .iter()
            .map(|&y| year_end_date_string(y))
            .collect::<Vec<_>>(),
        LVL_ATTAINED = out_lvl_attained,
        LVL_ATTAINED_DESC = out_lvl_desc,
        STDNT_STS_CODE = out_student_status,
    )
}

/// Project DOMINO housing details.
/// @export
#[extendr]
fn project_domino_housing__(
    participant_aeuid: Strings,
    participant_spine_idx: &[i32],
    participant_first_year: &[i32],
    participant_last_year: &[i32],
    spine_baseline_income: &[f64],
    seed: i32,
) -> List {
    let n = participant_aeuid.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(907));

    let mut out_accom: Vec<&str> = Vec::with_capacity(n);
    let mut out_ho: Vec<&str> = Vec::with_capacity(n);
    let mut out_rent_type: Vec<&str> = Vec::with_capacity(n);
    let mut out_weekly_rent: Vec<f64> = Vec::with_capacity(n);

    for i in 0..n {
        let si = idx1_to0(participant_spine_idx[i], spine_baseline_income.len());
        let income = spine_baseline_income[si];
        let accom = ACCOM_CODES[weighted_sample(&mut rng, &ACCOM_WEIGHTS)];
        let ho = if income < 30000.0 {
            HO_CODES[weighted_sample(&mut rng, &HO_PROBS_LOW)]
        } else {
            HO_CODES[weighted_sample(&mut rng, &HO_PROBS_HIGH)]
        };
        // Only a non-homeowner pays rent. PRV and GOV are the published
        // private and government rent types; NRP means no rent paid.
        let rent_type = if ho == "NHO" {
            if rng.gen::<f64>() < 0.75 { "PRI" } else { "GOV" }
        } else {
            "NRP"
        };
        // Rent is a price, not a wage, and it is paid by the household rather
        // than legislated, so it takes a person's own deviation: two renters
        // who paid the same in the anchor year do not stay level for twenty
        // years. The 100-500 band is an anchor-year band, and holding it fixed
        // was the reason a 2006 record and a 2023 record named the same rent.
        //
        // Years here are calendar years, as in the income projector above: the
        // period runs 1 January to 31 December, and the record is stated at the
        // start of the spell. The key is the AEUID for the same reason as
        // there — `participant_spine_idx` is an index into one slice, and the
        // slice count comes from the machine's core count.
        let weekly_rent = if rent_type == "NRP" {
            0.0
        } else {
            let rent_factor = nominal::factor(
                nominal::Series::Price,
                nominal::Basis::Calendar,
                nominal::PERSON,
                nominal::unit_key(&participant_aeuid[i].to_string()),
                seed as i64,
                participant_first_year[i],
            );
            (rng.gen_range(100.0f64..500.0f64) * rent_factor).round()
        };

        out_accom.push(accom);
        out_ho.push(ho);
        out_rent_type.push(rent_type);
        out_weekly_rent.push(weekly_rent);
    }

    list!(
        SYNTHETIC_AEUID = participant_aeuid
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>(),
        PERIOD_START_DATE = participant_first_year
            .iter()
            .map(|&y| year_start_date_string(y))
            .collect::<Vec<_>>(),
        PERIOD_END_DATE = participant_last_year
            .iter()
            .map(|&y| year_end_date_string(y))
            .collect::<Vec<_>>(),
        HSE_ACCOM_CODE = out_accom,
        HSE_HO_CODE = out_ho,
        HSE_RENT_TYPE = out_rent_type,
        HSE_WK_RENT = out_weekly_rent,
    )
}

/// Project DOMINO Indigenous status.
/// @export
#[extendr]
fn project_domino_indigenous__(
    participant_aeuid: Strings,
    participant_spine_idx: &[i32],
    spine_indigenous: &[i32],
) -> List {
    let n = participant_aeuid.len();
    let mut out_indig_code: Vec<&str> = Vec::with_capacity(n);
    for &idx in participant_spine_idx {
        let si = idx1_to0(idx, spine_indigenous.len());
        out_indig_code.push(if (2..=4).contains(&spine_indigenous[si]) {
            "Y"
        } else {
            "N"
        });
    }

    list!(
        SYNTHETIC_AEUID = participant_aeuid
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>(),
        INDIG_CODE = out_indig_code,
    )
}

/// Project DOMINO principal carer relationships.
/// @export
#[extendr]
fn project_domino_principal_carer__(
    det_ben_aeuid: Strings,
    det_ben_type_code: Strings,
    det_ben_period_start_date: Strings,
    det_ben_period_end_date: Strings,
    seed: i32,
) -> List {
    // +909: principal carer previously shared +907 with the housing projector.
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(909));

    let ben_types: Vec<String> = det_ben_type_code.iter().map(|s| s.to_string()).collect();
    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_ben_type: Vec<String> = Vec::new();
    let mut out_start: Vec<String> = Vec::new();
    let mut out_end: Vec<Option<String>> = Vec::new();
    let mut out_rel_id: Vec<String> = Vec::new();

    for i in 0..ben_types.len() {
        let ben = &ben_types[i];
        if ben != "PPP" && ben != "PPS" && ben != "CAR" {
            continue;
        }
        out_aeuid.push(det_ben_aeuid[i].to_string());
        out_ben_type.push(ben.clone());
        out_start.push(det_ben_period_start_date[i].to_string());
        out_end.push(if det_ben_period_end_date[i].is_na() {
            None
        } else {
            Some(det_ben_period_end_date[i].to_string())
        });
        out_rel_id.push(format!("R{:012}", rng.gen_range(1..=999_999_999i32)));
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        BEN_TYPE_CODE = out_ben_type,
        PERIOD_START_DATE = out_start,
        PERIOD_END_DATE = out_end,
        EXT_DOM_REL_ID = out_rel_id,
    )
}

extendr_module! {
    mod domino;
    fn select_domino_participants__;
    fn generate_benefit_spells__;
    fn generate_medical_assessments__;
    fn generate_payment_history__;
    fn project_domino_demogs__;
    fn project_domino_income__;
    fn project_domino_locations__;
    fn project_domino_education__;
    fn project_domino_housing__;
    fn project_domino_indigenous__;
    fn project_domino_principal_carer__;
}

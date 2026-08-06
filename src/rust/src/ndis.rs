use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::mbs::days_since_epoch;
use crate::nominal;
use crate::sampling::{normal_sample, weighted_sample};
use crate::service_profiles::PROFILES;

// ============================================================================
// NDIS classification code frames
// ============================================================================
// Sourced from authoritative NDIS material (ndis.gov.au support budgets, the
// NDIS Quarterly Report disability groups, MMM) and adversarially verified.
// Current public domains and rules are linked from:
// https://dataresearch.ndis.gov.au/datasets/participant-datasets
// https://ndis.gov.au/providers/pricing-and-payments/pricing/pricing-arrangements
// The PLIDA NDIA variable names come from inst/plida_metadata/variables.csv.

/// NDIS primary disability group (NDISDSBLTYGRPNM), mapped from the spine
/// person_type to the NDIS quarterly-report group set.
fn ndis_disability_group(person_type: i32) -> &'static str {
    match person_type {
        2 | 3 | 4 | 21 | 27 => "Psychosocial disability",
        10 => "Multiple Sclerosis",
        11 => "Hearing Impairment",
        12 => "Visual Impairment",
        13 => "Spinal Cord Injury",
        14 => "Stroke",
        9 | 15 | 16 => "Other Neurological",
        23 => "Intellectual Disability",
        24 => "Autism",
        26 => "Other Sensory/Speech",
        1 | 5..=8 | 17..=20 | 22 | 25 => "Other Physical",
        _ => "Other",
    }
}

/// ICD-style disability name (ICDDSBLTYNM), from the spine person-type condition.
fn icd_disability_name(person_type: i32) -> &'static str {
    match person_type {
        1 => "Chronic back pain",
        2 => "Depression and anxiety",
        3 => "Schizophrenia",
        4 => "Bipolar affective disorder",
        5 => "Type 2 diabetes mellitus",
        6 => "Type 1 diabetes mellitus",
        7 => "Chronic obstructive pulmonary disease",
        8 => "Rheumatoid arthritis",
        9 => "Epilepsy",
        10 => "Multiple sclerosis",
        11 => "Sensorineural hearing loss",
        12 => "Visual impairment",
        13 => "Spinal cord injury",
        14 => "Stroke / acquired brain injury",
        15 => "Parkinson's disease",
        16 => "Dementia",
        17 => "Upper limb impairment",
        18 => "Lower limb impairment",
        19 => "Cardiovascular disease",
        20 => "Chronic kidney disease",
        21 => "Post-traumatic stress disorder",
        22 => "Carpal tunnel syndrome / RSI",
        23 => "Intellectual disability",
        24 => "Autism spectrum disorder",
        25 => "Fibromyalgia",
        26 => "Speech and language disorder",
        27 => "Social and behavioural disorder",
        _ => "Not stated",
    }
}

/// SVRTYSCR: normalised function/severity score in [0,1]; HIGHER = greater
/// impairment / lower function (per NDIS sources). Anchored on the spine
/// disability_severity (1=profound .. 4=condition-only) with jitter.
fn severity_score(rng: &mut StdRng, sev: i32) -> f64 {
    let centre = match sev {
        1 => 0.85,
        2 => 0.65,
        3 => 0.45,
        _ => 0.28,
    };
    (centre + normal_sample(rng, 0.0, 0.07)).clamp(0.0, 1.0)
}

/// NDIS support categories (SUPPCATNM) with their budget class (SUPPCLASS).
const SUPP_CATS: [(&str, &str); 16] = [
    ("Assistance with Daily Life", "Core"),
    ("Transport", "Core"),
    ("Consumables", "Core"),
    ("Assistance with Social & Community Participation", "Core"),
    ("Coordination of Supports", "Capacity Building"),
    ("Improved Living Arrangements", "Capacity Building"),
    (
        "Increased Social & Community Participation",
        "Capacity Building",
    ),
    ("Finding & Keeping a Job", "Capacity Building"),
    ("Improved Relationships", "Capacity Building"),
    ("Improved Health & Wellbeing", "Capacity Building"),
    ("Improved Learning", "Capacity Building"),
    ("Improved Life Choices", "Capacity Building"),
    ("Improved Daily Living Skills", "Capacity Building"),
    ("Assistive Technology", "Capital"),
    ("Home Modifications", "Capital"),
    ("Specialist Disability Accommodation", "Capital"),
];

/// Budget-management method (PLANMGTMTHDDESC).
const MGMT_METHODS: [&str; 5] = [
    "Agency Managed",
    "Plan Managed",
    "Self Managed Fully",
    "Self Managed Partly",
    "Not recorded",
];
const MGMT_METHOD_WEIGHTS: [f64; 5] = [0.42, 0.35, 0.14, 0.07, 0.02];

/// Payment service-booking type (MGTTYPDESC). These compact values are stated
/// in the PLIDA DIL description for the expanded payments product.
fn payment_management_type(plan_management: &str) -> &'static str {
    match plan_management {
        "Agency Managed" => "agency",
        "Plan Managed" => "plan",
        "Self Managed Fully" | "Self Managed Partly" => "self",
        _ => "agency",
    }
}

fn australian_financial_year_end(calendar_year: i32, month: u32) -> i32 {
    calendar_year + i32::from(month >= 7)
}

/// How much bigger a plan budget is in `plan_year` than at the 2021 anchor.
///
/// NDIS supports are overwhelmingly labour, and the price guide that caps what
/// a provider may charge for them is rebuilt each year off wage benchmarks
/// rather than the CPI, so a plan follows the wage series and not prices.
/// `plan_year` is a calendar year everywhere in this file — it comes off the
/// spine's disability onset year — so it goes to the nominal tables on the
/// calendar basis with no financial-year offset. (`FY_CLAIM` on the payments
/// product is a financial-year *end* derived from the same calendar year by
/// `australian_financial_year_end`; the two labels are a year apart in places
/// and must not be substituted for each other.)
///
/// Keyed on the AEUID so a participant's budgets move together across plans
/// and across the two generators below.
#[inline]
fn plan_budget_factor(aeuid: &str, seed: i64, plan_year: i32) -> f64 {
    nominal::factor(
        nominal::Series::Wage,
        nominal::Basis::Calendar,
        nominal::PERSON,
        nominal::unit_key(aeuid),
        seed,
        plan_year,
    )
}

/// The same movement without the per-participant part, for the budget cap.
///
/// A cap held at its 2021 dollar value while budgets grew past it would collect
/// an ever-growing share of plans on exactly one number, turning the top of the
/// distribution into a spike that no year of real plan data shows. The real
/// ceilings are restated with the price guide, so this one moves with the
/// headline — and only the headline, because a cap is the same number for every
/// participant who meets it.
#[inline]
fn plan_budget_cap(cap_at_anchor: f64, plan_year: i32) -> f64 {
    cap_at_anchor * nominal::index(nominal::Series::Wage, nominal::Basis::Calendar, plan_year)
}

/// Plan type (PLAN_TYPE).
const PLAN_TYPES: [&str; 3] = [
    "Initial plan",
    "Result of a scheduled review",
    "Result of an unscheduled review",
];

/// Payment claim type (CLAIMTYPDESC).
const CLAIM_TYPES: [&str; 3] = ["Standard", "Provider travel", "Cancellation"];
const CLAIM_TYPE_WEIGHTS: [f64; 3] = [0.88, 0.08, 0.04];

// Legacy 3-way support/management consts retained for the (unused) list-emitting
// `project_ndis_participants__` entry point.
const SUPPORT_CATS: [&str; 3] = ["Core", "Capacity Building", "Capital"];
const SUPPORT_WEIGHTS: [f64; 3] = [0.55, 0.35, 0.10];
const MGMT_TYPES: [&str; 3] = ["Agency", "Plan", "Self"];
const MGMT_WEIGHTS: [f64; 3] = [0.50, 0.30, 0.20];

/// Representative postcode for a state (POSTCD).
fn postcode_for_state(rng: &mut StdRng, state: i32) -> i32 {
    let (lo, hi) = match state {
        1 => (2000, 2999),
        2 => (3000, 3999),
        3 => (4000, 4999),
        4 => (5000, 5799),
        5 => (6000, 6797),
        6 => (7000, 7799),
        7 => (800, 899),
        8 => (2600, 2618),
        _ => (2000, 2999),
    };
    rng.gen_range(lo..=hi)
}

/// Modified Monash Model remoteness code (NDISMMMSCD), 1-7, skewed urban.
const MMM_WEIGHTS: [f64; 7] = [0.62, 0.14, 0.09, 0.06, 0.04, 0.03, 0.02];

/// Compact REMOTENESS_DESCRIPTION_MMM domain documented in the PLIDA DIL.
fn mmm_description(code: i32) -> &'static str {
    match code {
        1 => "City",
        2..=5 => "Rural",
        6 => "Remote",
        _ => "Very remote",
    }
}

/// Deterministic, checksum-valid synthetic Australian Business Number for a
/// provider bucket. The nine-digit payload identifies a reusable provider;
/// the leading two digits satisfy the published ABN modulus-89 check.
/// https://abr.business.gov.au/Help/AbnFormat
fn synthetic_provider_abn(provider_key: u64, seed: i64) -> String {
    const WEIGHTS: [i32; 11] = [10, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
    let seed_component = seed.unsigned_abs() % 1_000_000_000;
    let payload = provider_key
        .wrapping_mul(1_000_003)
        .wrapping_add(seed_component)
        % 1_000_000_000;
    let mut digits = [0_i32; 11];
    let mut value = payload;
    for index in (2..11).rev() {
        digits[index] = (value % 10) as i32;
        value /= 10;
    }
    let payload_sum: i32 = (2..11).map(|index| digits[index] * WEIGHTS[index]).sum();
    let required_prefix = (89 - payload_sum.rem_euclid(89)).rem_euclid(89);
    digits[0] = required_prefix / 10 + 1;
    digits[1] = required_prefix % 10;
    digits
        .iter()
        .map(|digit| char::from(b'0' + *digit as u8))
        .collect()
}

fn age_band(age: i32) -> &'static str {
    match age {
        a if a < 7 => "0 to 6",
        a if a < 15 => "7 to 14",
        a if a < 19 => "15 to 18",
        a if a < 25 => "19 to 24",
        a if a < 35 => "25 to 34",
        a if a < 45 => "35 to 44",
        a if a < 55 => "45 to 54",
        a if a < 65 => "55 to 64",
        _ => "65+",
    }
}

// ============================================================================
// Participant list entry point (unused by build_fplida; retained for the API)
// ============================================================================

/// Generate NDIS participants product from spine (legacy compact list form).
/// @export
#[extendr]
fn project_ndis_participants__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    disability_onset_year: &[i32],
    disability_severity: &[i32],
    person_type: &[i32],
    seed: i64,
) -> List {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let est_n = n / 5;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_yob: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_gender: Vec<String> = Vec::with_capacity(est_n);
    let mut out_state: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_indigenous: Vec<String> = Vec::with_capacity(est_n);
    let mut out_disability_group: Vec<String> = Vec::with_capacity(est_n);
    let mut out_plan_date: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_status: Vec<String> = Vec::with_capacity(est_n);
    let mut out_plan_budget: Vec<f64> = Vec::with_capacity(est_n);
    let mut out_support_cat: Vec<String> = Vec::with_capacity(est_n);
    let mut out_mgmt_type: Vec<String> = Vec::with_capacity(est_n);

    for i in 0..n {
        let sev = disability_severity[i];
        let pt = person_type[i];
        let onset = disability_onset_year[i];
        if sev == i32::MIN || pt == i32::MIN || pt <= 0 || pt > 27 {
            continue;
        }
        if onset == i32::MIN || onset <= 0 {
            continue;
        }
        let profile = &PROFILES[(pt - 1) as usize];
        let sev_idx = (sev - 1).max(0).min(3) as usize;
        if rng.gen::<f64>() >= profile.ndis_probs[sev_idx] {
            continue;
        }
        let plan_year = onset.max(2013) + rng.gen_range(0..=2);
        let plan_date = days_since_epoch(
            plan_year,
            rng.gen_range(1..=12) as u32,
            rng.gen_range(1..=28) as u32,
        );
        out_aeuid.push(aeuid[i].to_string());
        out_yob.push(birth_year[i]);
        out_gender.push(if sex[i] == 1 { "M" } else { "F" }.to_string());
        out_state.push(state[i]);
        out_indigenous.push(
            if indigenous[i] >= 2 && indigenous[i] <= 4 {
                "Yes"
            } else {
                "No"
            }
            .to_string(),
        );
        out_disability_group.push(ndis_disability_group(pt).to_string());
        out_plan_date.push(plan_date);
        out_status.push(
            if rng.gen::<f64>() < 0.90 {
                "Active"
            } else {
                "Exited"
            }
            .to_string(),
        );
        // The log-normal draw is the budget a plan of this kind would carry in
        // the 2021 anchor year; the factor moves it to the year the plan was
        // approved.
        out_plan_budget.push(
            ((normal_sample(&mut rng, 10.8, 0.8)).exp()
                * plan_budget_factor(aeuid[i].as_ref(), seed, plan_year))
            .min(plan_budget_cap(500000.0, plan_year))
            .round(),
        );
        out_support_cat.push(SUPPORT_CATS[weighted_sample(&mut rng, &SUPPORT_WEIGHTS)].to_string());
        out_mgmt_type.push(MGMT_TYPES[weighted_sample(&mut rng, &MGMT_WEIGHTS)].to_string());
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        YEAR_OF_BIRTH = out_yob,
        GENDER = out_gender,
        STATE_ASGS_2021 = out_state,
        ABORIGINAL_TSI = out_indigenous,
        DISABILITY_GROUP = out_disability_group,
        FIRST_PLAN_APPROVAL_DATE = out_plan_date,
        PARTICIPANT_STATUS = out_status,
        PLAN_BUDGET_AMT = out_plan_budget,
        SUPPORT_CATEGORY = out_support_cat,
        MANAGEMENT_TYPE = out_mgmt_type
    )
}

// ============================================================================
// Three-product NDIS generator: participants, plansupports, payments
// ============================================================================

/// Project NDIS to three linked parquet products from the spine:
/// participants (enriched with disability/geography/pathway detail),
/// plansupports (committed support-category budgets), and payments (claims).
/// Returns the participant count.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_ndis_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    disability_onset_year: &[i32],
    disability_severity: &[i32],
    person_type: &[i32],
    comorbidity_flags: &[i32],
    country_of_birth_sacc: &[i32],
    sa2: &[i32],
    seed: i64,
    out_participants: &str,
    out_plansupports: &str,
    out_payments: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let est_n = n / 5;

    // -- Participants columns --
    let mut p_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut p_yob: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_mob: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_gndr: Vec<String> = Vec::with_capacity(est_n);
    let mut p_atsi: Vec<String> = Vec::with_capacity(est_n);
    let mut p_cald: Vec<String> = Vec::with_capacity(est_n);
    let mut p_cob: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_dgrp: Vec<String> = Vec::with_capacity(est_n);
    let mut p_icd: Vec<String> = Vec::with_capacity(est_n);
    let mut p_svrty: Vec<f64> = Vec::with_capacity(est_n);
    let mut p_chc: Vec<String> = Vec::with_capacity(est_n);
    let mut p_csn: Vec<String> = Vec::with_capacity(est_n);
    let mut p_state: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_pc: Vec<String> = Vec::with_capacity(est_n);
    let mut p_sa2: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_sa3: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_sa4: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_mmm: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_mmm_desc: Vec<String> = Vec::with_capacity(est_n);
    let mut p_lang: Vec<String> = Vec::with_capacity(est_n);
    let mut p_plandt: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_actv: Vec<String> = Vec::with_capacity(est_n);
    let mut p_ever_eligible: Vec<String> = Vec::with_capacity(est_n);
    let mut p_ever_ineligible: Vec<String> = Vec::with_capacity(est_n);
    let mut p_exit: Vec<Option<String>> = Vec::with_capacity(est_n);
    let mut p_wait: Vec<i32> = Vec::with_capacity(est_n);
    let mut p_ageband: Vec<String> = Vec::with_capacity(est_n);

    // -- Plansupports columns --
    let mut s_aeuid: Vec<String> = Vec::new();
    let mut s_planid: Vec<String> = Vec::new();
    let mut s_seq: Vec<i32> = Vec::new();
    let mut s_fy: Vec<i32> = Vec::new();
    let mut s_catnm: Vec<String> = Vec::new();
    let mut s_class: Vec<String> = Vec::new();
    let mut s_budget: Vec<f64> = Vec::new();
    let mut s_fybudget: Vec<f64> = Vec::new();
    let mut s_mgmt: Vec<String> = Vec::new();
    let mut s_plantype: Vec<String> = Vec::new();
    let mut s_efct: Vec<i32> = Vec::new();
    let mut s_expry: Vec<i32> = Vec::new();
    let mut s_crnt: Vec<String> = Vec::new();
    let mut s_active: Vec<String> = Vec::new();
    let mut s_eversil: Vec<String> = Vec::new();
    let mut s_eversda: Vec<String> = Vec::new();
    let mut s_latest_sil: Vec<String> = Vec::new();
    let mut s_includes_sda: Vec<String> = Vec::new();

    // -- Payments columns --
    let mut y_aeuid: Vec<String> = Vec::new();
    let mut y_id: Vec<String> = Vec::new();
    let mut y_amt: Vec<f64> = Vec::new();
    let mut y_tax: Vec<f64> = Vec::new();
    let mut y_catnm: Vec<String> = Vec::new();
    let mut y_class: Vec<String> = Vec::new();
    let mut y_request_provider_bn: Vec<String> = Vec::new();
    let mut y_provider_bn: Vec<String> = Vec::new();
    let mut y_claimtyp: Vec<String> = Vec::new();
    let mut y_mgttyp: Vec<String> = Vec::new();
    let mut y_fy: Vec<i32> = Vec::new();
    let mut y_sts: Vec<String> = Vec::new();
    let mut y_crtdt: Vec<i32> = Vec::new();
    let mut y_qty: Vec<i32> = Vec::new();

    let mut pid: u64 = 0;
    let mut payid: u64 = 0;

    for i in 0..n {
        let sev = disability_severity[i];
        let pt = person_type[i];
        let onset = disability_onset_year[i];
        if sev == i32::MIN || pt == i32::MIN || pt <= 0 || pt > 27 {
            continue;
        }
        if onset == i32::MIN || onset <= 0 {
            continue;
        }
        let profile = &PROFILES[(pt - 1) as usize];
        let sev_idx = (sev - 1).max(0).min(3) as usize;
        if rng.gen::<f64>() >= profile.ndis_probs[sev_idx] {
            continue;
        }

        pid += 1;
        let plan_year = onset.max(2013) + rng.gen_range(0..=2);
        let plan_month = rng.gen_range(1..=12) as u32;
        let plan_day = rng.gen_range(1..=28) as u32;
        let plan_date = days_since_epoch(plan_year, plan_month, plan_day);
        let access_wait = rng.gen_range(14..=270); // days access -> first plan
        let active = rng.gen::<f64>() < 0.90;
        let exit_reason: Option<String> = if active {
            None
        } else {
            Some(
                match rng.gen_range(0..4) {
                    0 => "Deceased",
                    1 => "No longer meets disability requirements",
                    2 => "Moved overseas",
                    _ => "Other",
                }
                .to_string(),
            )
        };
        let ever_ineligible = matches!(
            exit_reason.as_deref(),
            Some("No longer meets disability requirements")
        );
        // NDIAAGEBND is explicitly defined in the DIL as age at
        // 31 December 2019, rather than age at generation time.
        let age_at_2019 = (2019 - birth_year[i]).max(0);
        let comorb = comorbidity_flags[i];
        // CHC: chronic-health flag if the condition or a comorbidity is chronic
        // (diabetes/cvd bits set, or a chronic person_type).
        let chc = matches!(pt, 1 | 5 | 6 | 7 | 8 | 19 | 20 | 25)
            || (comorb != i32::MIN && comorb & (2 | 4) != 0);
        let sacc = country_of_birth_sacc[i];
        let excluded_cald_country = matches!(
            sacc,
            1101 | 1201
                | 2100
                | 2102
                | 2103
                | 2104
                | 2105
                | 2106
                | 2107
                | 2201
                | 8102
                | 8104
                | 9225
        );
        let language = if sacc != 1101 && sacc != i32::MIN && rng.gen::<f64>() < 0.55 {
            ["Mandarin", "Arabic", "Vietnamese", "Punjabi"][rng.gen_range(0..4)]
        } else {
            "English"
        };
        // Current NDIS public reporting defines CALD from country of birth or
        // home language, excluding First Nations participants.
        let cald = !(indigenous[i] >= 2 && indigenous[i] <= 4)
            && (!excluded_cald_country || language != "English");
        let s2 = sa2[i];
        let mmm = (weighted_sample(&mut rng, &MMM_WEIGHTS) + 1) as i32;

        // The compact generator emits one current plan per participant. These
        // plan-level states therefore also define the current/latest flags.
        let ever_sil = sev == 1 && rng.gen::<f64>() < 0.30;
        let ever_sda = sev == 1 && rng.gen::<f64>() < 0.12;

        p_aeuid.push(aeuid[i].to_string());
        p_yob.push(birth_year[i]);
        p_mob.push(rng.gen_range(1..=12));
        p_gndr.push(if sex[i] == 1 { "Male" } else { "Female" }.to_string());
        p_atsi.push(
            if indigenous[i] >= 2 && indigenous[i] <= 4 {
                "Yes"
            } else {
                "No"
            }
            .to_string(),
        );
        p_cald.push(if cald { "Yes" } else { "No" }.to_string());
        p_cob.push(sacc);
        p_dgrp.push(ndis_disability_group(pt).to_string());
        p_icd.push(icd_disability_name(pt).to_string());
        p_svrty.push((severity_score(&mut rng, sev) * 1000.0).round() / 1000.0);
        p_chc.push(if chc { "Y" } else { "N" }.to_string());
        p_csn.push(if sev == 1 { "Y" } else { "N" }.to_string());
        p_state.push(state[i]);
        p_pc.push(format!("{:04}", postcode_for_state(&mut rng, state[i])));
        p_sa2.push(s2);
        p_sa3.push(if s2 > 0 { s2 / 10_000 } else { 0 });
        p_sa4.push(if s2 > 0 { s2 / 1_000_000 } else { 0 });
        p_mmm.push(mmm);
        p_mmm_desc.push(mmm_description(mmm).to_string());
        p_lang.push(language.to_string());
        p_plandt.push(plan_date);
        p_actv.push(if active { "Y" } else { "N" }.to_string());
        p_ever_eligible.push("Y".to_string());
        p_ever_ineligible.push(if ever_ineligible { "Y" } else { "N" }.to_string());
        p_exit.push(exit_reason);
        p_wait.push(access_wait);
        p_ageband.push(age_band(age_at_2019).to_string());

        // ---- Plan supports + payments for this participant ----
        let planid = format!("PLAN{:010}", pid);
        let mgmt = MGMT_METHODS[weighted_sample(&mut rng, &MGMT_METHOD_WEIGHTS)];
        let plan_type = PLAN_TYPES[0]; // first plan
        let plan_expiry = days_since_epoch(plan_year + 1, plan_month, plan_day);

        // More categories for higher severity.
        let n_extra = match sev {
            1 => rng.gen_range(3..=6),
            2 => rng.gen_range(2..=4),
            3 => rng.gen_range(1..=3),
            _ => rng.gen_range(0..=2),
        };
        // Always include the Core "Assistance with Daily Life" category, then a
        // random distinct selection of others.
        let mut cat_idx: Vec<usize> = vec![0];
        while cat_idx.len() < (1 + n_extra) {
            let c = rng.gen_range(0..SUPP_CATS.len());
            if !cat_idx.contains(&c) {
                cat_idx.push(c);
            }
        }

        for (seq, &c) in cat_idx.iter().enumerate() {
            let (catnm, class) = SUPP_CATS[c];
            // Committed budget: log-normal, larger for Core and higher severity.
            let base = match class {
                "Core" => 11.0,
                "Capacity Building" => 9.8,
                _ => 9.2,
            } + (4 - sev.clamp(1, 4)) as f64 * 0.12;
            // `base` is a 2021 log budget, so the draw is an anchor-year amount
            // and the factor carries it to the plan's approval year. Every
            // category of one plan shares the participant's factor, which keeps
            // the categories of a plan in proportion to each other.
            let budget = ((normal_sample(&mut rng, base, 0.7)).exp()
                * plan_budget_factor(aeuid[i].as_ref(), seed, plan_year))
            .min(plan_budget_cap(400000.0, plan_year))
            .round();

            s_aeuid.push(aeuid[i].to_string());
            s_planid.push(planid.clone());
            s_seq.push((seq + 1) as i32);
            s_fy.push(plan_year);
            s_catnm.push(catnm.to_string());
            s_class.push(class.to_string());
            s_budget.push(budget);
            s_fybudget.push(budget);
            s_mgmt.push(mgmt.to_string());
            s_plantype.push(plan_type.to_string());
            s_efct.push(plan_date);
            s_expry.push(plan_expiry);
            s_crnt.push(if active { "Y" } else { "N" }.to_string());
            s_active.push(if active { "Y" } else { "N" }.to_string());
            s_eversil.push(if ever_sil { "Y" } else { "N" }.to_string());
            s_eversda.push(if ever_sda { "Y" } else { "N" }.to_string());
            s_latest_sil.push(if ever_sil { "Y" } else { "N" }.to_string());
            s_includes_sda.push(if ever_sda { "Y" } else { "N" }.to_string());

            // Payments: a handful of claims drawing down this category's budget.
            // Reuse providers across participants within state/category buckets.
            let provider_key =
                (state[i].clamp(1, 8) as u64) * 100_000 + (c as u64) * 1_000 + pid % 20;
            let provider_bn = synthetic_provider_abn(provider_key, seed);
            let n_pay = rng.gen_range(1..=4);
            for _ in 0..n_pay {
                payid += 1;
                let frac = rng.gen_range(0.05..0.45);
                let amt = (budget * frac).round();
                if amt < 1.0 {
                    continue;
                }
                let tax = (amt * 0.0).round(); // NDIS supports are GST-free
                let claim = CLAIM_TYPES[weighted_sample(&mut rng, &CLAIM_TYPE_WEIGHTS)];
                let pay_month = rng.gen_range(1..=12) as u32;
                let pay_day = rng.gen_range(1..=28) as u32;
                let pay_dt = days_since_epoch(plan_year, pay_month, pay_day);

                y_aeuid.push(aeuid[i].to_string());
                y_id.push(format!("PMT{:012}", payid));
                y_amt.push(amt);
                y_tax.push(tax);
                y_catnm.push(catnm.to_string());
                y_class.push(class.to_string());
                y_request_provider_bn.push(provider_bn.clone());
                y_provider_bn.push(provider_bn.clone());
                y_claimtyp.push(claim.to_string());
                y_mgttyp.push(payment_management_type(mgmt).to_string());
                y_fy.push(australian_financial_year_end(plan_year, pay_month));
                y_sts.push(
                    if rng.gen::<f64>() < 0.97 {
                        "Paid"
                    } else {
                        "Rejected"
                    }
                    .to_string(),
                );
                y_crtdt.push(pay_dt);
                y_qty.push(rng.gen_range(1..=20));
            }
        }
    }

    let total = p_aeuid.len() as i32;

    let participants = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(p_aeuid),
        },
        NamedCol {
            name: "YEAR_OF_BIRTH",
            col: Col::I32(p_yob),
        },
        NamedCol {
            name: "MONTH_OF_BIRTH",
            col: Col::I32(p_mob),
        },
        NamedCol {
            name: "GNDRTYP",
            col: Col::Str(p_gndr),
        },
        NamedCol {
            name: "ATSISTS",
            col: Col::Str(p_atsi),
        },
        NamedCol {
            name: "CALDSTS",
            col: Col::Str(p_cald),
        },
        NamedCol {
            name: "CNTRYOFBRTHCD",
            col: Col::I32(p_cob),
        },
        NamedCol {
            name: "NDISDSBLTYGRPNM",
            col: Col::Str(p_dgrp),
        },
        NamedCol {
            name: "ICDDSBLTYNM",
            col: Col::Str(p_icd),
        },
        NamedCol {
            name: "SVRTYSCR",
            col: Col::F64(p_svrty),
        },
        NamedCol {
            name: "CHC_FLAG",
            col: Col::Str(p_chc),
        },
        NamedCol {
            name: "CSNIND",
            col: Col::Str(p_csn),
        },
        NamedCol {
            name: "RSDSINSTATECD",
            col: Col::I32(p_state),
        },
        NamedCol {
            name: "POSTCD",
            col: Col::Str(p_pc),
        },
        NamedCol {
            name: "SA2CD2021",
            col: Col::I32(p_sa2),
        },
        NamedCol {
            name: "SA3CD2021",
            col: Col::I32(p_sa3),
        },
        NamedCol {
            name: "SA4CD2021",
            col: Col::I32(p_sa4),
        },
        NamedCol {
            name: "NDISMMMSCD",
            col: Col::I32(p_mmm),
        },
        NamedCol {
            name: "REMOTENESS_DESCRIPTION_MMM",
            col: Col::Str(p_mmm_desc),
        },
        NamedCol {
            name: "LANGSPKNATHOMENM",
            col: Col::Str(p_lang),
        },
        NamedCol {
            name: "FRSTPLANAPRVLDT",
            col: Col::DateNN(p_plandt),
        },
        NamedCol {
            name: "ACTVPRTCPNTIND",
            col: Col::Str(p_actv),
        },
        NamedCol {
            name: "EVERELIGBLIND",
            col: Col::Str(p_ever_eligible),
        },
        NamedCol {
            name: "EVERINELIGBLIND",
            col: Col::Str(p_ever_ineligible),
        },
        NamedCol {
            name: "EXITRSNDESC",
            col: Col::StrOpt(p_exit),
        },
        NamedCol {
            name: "PLANWAITDAYS",
            col: Col::I32(p_wait),
        },
        NamedCol {
            name: "NDIAAGEBND",
            col: Col::Str(p_ageband),
        },
    ];
    write_columns_to_parquet(out_participants, participants)
        .unwrap_or_else(|e| panic!("ndis participants parquet write: {}", e));

    let plansupports = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(s_aeuid),
        },
        NamedCol {
            name: "PLANID",
            col: Col::Str(s_planid),
        },
        NamedCol {
            name: "APRVDPLANSEQNMBR",
            col: Col::I32(s_seq),
        },
        NamedCol {
            name: "FINANCIALYEAR",
            col: Col::I32(s_fy),
        },
        NamedCol {
            name: "SUPPCATNM",
            col: Col::Str(s_catnm),
        },
        NamedCol {
            name: "SUPPCLASS",
            col: Col::Str(s_class),
        },
        NamedCol {
            name: "CMTDSUPPBDGTAMT",
            col: Col::F64(s_budget),
        },
        NamedCol {
            name: "FYCMTDSUPPBDGTAMT",
            col: Col::F64(s_fybudget),
        },
        NamedCol {
            name: "PLANMGTMTHDDESC",
            col: Col::Str(s_mgmt),
        },
        NamedCol {
            name: "PLAN_TYPE",
            col: Col::Str(s_plantype),
        },
        NamedCol {
            name: "PLANEFCTVDT",
            col: Col::DateNN(s_efct),
        },
        NamedCol {
            name: "PLANEXPRYDT",
            col: Col::DateNN(s_expry),
        },
        NamedCol {
            name: "CRNTAPRVDPLANIND",
            col: Col::Str(s_crnt),
        },
        NamedCol {
            name: "ACTIVEPLANIND",
            col: Col::Str(s_active),
        },
        NamedCol {
            name: "EVERSILIND",
            col: Col::Str(s_eversil),
        },
        NamedCol {
            name: "EVERSDAIND",
            col: Col::Str(s_eversda),
        },
        NamedCol {
            name: "LTSTPLANSILIND",
            col: Col::Str(s_latest_sil),
        },
        NamedCol {
            name: "PLANINCLDSDAIND",
            col: Col::Str(s_includes_sda),
        },
    ];
    write_columns_to_parquet(out_plansupports, plansupports)
        .unwrap_or_else(|e| panic!("ndis plansupports parquet write: {}", e));

    let payments = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(y_aeuid),
        },
        NamedCol {
            name: "PYMTIDNTFR",
            col: Col::Str(y_id),
        },
        NamedCol {
            name: "PYMTAMT",
            col: Col::F64(y_amt),
        },
        NamedCol {
            name: "TAXAMT",
            col: Col::F64(y_tax),
        },
        NamedCol {
            name: "PYMTRQSTSUPPCATNM",
            col: Col::Str(y_catnm),
        },
        NamedCol {
            name: "SUPPCLASS",
            col: Col::Str(y_class),
        },
        NamedCol {
            name: "PMTRQSTPRVDR_BN",
            col: Col::Str(y_request_provider_bn),
        },
        NamedCol {
            name: "PRVDR_BN",
            col: Col::Str(y_provider_bn),
        },
        NamedCol {
            name: "CLAIMTYPDESC",
            col: Col::Str(y_claimtyp),
        },
        NamedCol {
            name: "MGTTYPDESC",
            col: Col::Str(y_mgttyp),
        },
        NamedCol {
            name: "FY_CLAIM",
            col: Col::I32(y_fy),
        },
        NamedCol {
            name: "PYMTSTS",
            col: Col::Str(y_sts),
        },
        NamedCol {
            name: "PYMTRQSTCRTDDT",
            col: Col::DateNN(y_crtdt),
        },
        NamedCol {
            name: "PYMTRQSTSUPPITEMQTY",
            col: Col::I32(y_qty),
        },
    ];
    write_columns_to_parquet(out_payments, payments)
        .unwrap_or_else(|e| panic!("ndis payments parquet write: {}", e));

    total
}

#[cfg(test)]
mod tests {
    use super::*;

    fn valid_abn(value: &str) -> bool {
        const WEIGHTS: [i32; 11] = [10, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
        if value.len() != 11 || !value.bytes().all(|byte| byte.is_ascii_digit()) {
            return false;
        }
        let mut digits: Vec<i32> = value.bytes().map(|byte| (byte - b'0') as i32).collect();
        digits[0] -= 1;
        digits
            .iter()
            .zip(WEIGHTS)
            .map(|(digit, weight)| digit * weight)
            .sum::<i32>()
            % 89
            == 0
    }

    #[test]
    fn provider_abns_are_deterministic_and_checksum_valid() {
        for provider_key in 0..500 {
            let first = synthetic_provider_abn(provider_key, 2704);
            let second = synthetic_provider_abn(provider_key, 2704);
            assert_eq!(first, second);
            assert!(valid_abn(&first));
        }
    }

    #[test]
    fn compact_ndis_domains_cover_documented_values() {
        assert_eq!(payment_management_type("Agency Managed"), "agency");
        assert_eq!(payment_management_type("Plan Managed"), "plan");
        assert_eq!(payment_management_type("Self Managed Fully"), "self");
        assert_eq!(payment_management_type("Self Managed Partly"), "self");
        assert_eq!(mmm_description(1), "City");
        assert_eq!(mmm_description(4), "Rural");
        assert_eq!(mmm_description(6), "Remote");
        assert_eq!(mmm_description(7), "Very remote");
        assert_eq!(australian_financial_year_end(2024, 6), 2024);
        assert_eq!(australian_financial_year_end(2024, 7), 2025);
    }
}

extendr_module! {
    mod ndis;
    fn project_ndis_participants__;
    fn project_ndis_to_parquet__;
}

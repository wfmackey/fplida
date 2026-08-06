use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::mbs::days_since_epoch;
use crate::nominal;
use crate::sampling::weighted_sample;

// DEX clients are mainly community / employment service recipients;
// ~10% of working-age population, skewed to unemployed / low income.
const DEX_RATE: f64 = 0.10;

// ============================================================================
// DSS Data Exchange reference code frames
// ============================================================================
// Values transcribed verbatim from the official DSS Data Exchange Protocols
// (v11, March 2024) + CHSP DEX Protocols, sourced and adversarially verified.
// PLIDA DEX variable names come from inst/plida_metadata/variables.csv.

/// EMPLOYMENTSTATUSCODE.
const EMPLOYMENT_STATUS: [&str; 9] = [
    "Paid work full-time",
    "Paid work part-time",
    "Unpaid work (includes volunteering)",
    "Not working and not looking for work",
    "Unemployed (not working but looking for work)",
    "Studying full-time",
    "Studying part-time",
    "Caring",
    "Parenting",
];

/// EDUCATIONLEVELCODE.
const EDUCATION_LEVEL: [&str; 9] = [
    "Pre-primary education",
    "Primary education",
    "Secondary education",
    "Certificate level",
    "Advanced diploma and diploma level",
    "Bachelor degree level",
    "Graduate diploma and graduate certificate level",
    "Postgraduate degree level",
    "Other education",
];

/// INCOMESOURCECODE.
const INCOME_SOURCE: [&str; 6] = [
    "Nil income",
    "Employee salary/wages",
    "Other income including superannuation and investments",
    "Self-employed (unincorporated business income)",
    "Government payments/pensions/allowances",
    "Not stated/Inadequately described",
];

/// HOMELESSCODE.
const HOMELESS: [&str; 3] = ["No", "At Risk", "Yes"];
const HOMELESS_WEIGHTS: [f64; 3] = [0.88, 0.08, 0.04];

/// ACCOMMODATIONTYPECODE.
const ACCOMMODATION: [&str; 6] = [
    "Private residence—client or family owned/purchasing",
    "Private residence—private rental",
    "Private residence—public rental",
    "Supported accommodation",
    "Crisis, emergency or transition",
    "Boarding house",
];
const ACCOMMODATION_WEIGHTS: [f64; 6] = [0.46, 0.30, 0.12, 0.06, 0.03, 0.03];

/// HOUSEHOLDCOMPOSITIONCODE.
const HOUSEHOLD_COMP: [&str; 6] = [
    "Single (person living alone)",
    "Sole parent with dependant(s)",
    "Couple",
    "Couple with dependant(s)",
    "Group (related adults)",
    "Group (unrelated adults)",
];
const HOUSEHOLD_COMP_WEIGHTS: [f64; 6] = [0.28, 0.14, 0.20, 0.24, 0.08, 0.06];

/// EXITREASONCODE.
const EXIT_REASON: [&str; 8] = [
    "Client needs have been met",
    "Client no longer requires assistance",
    "Client terminated the service",
    "Client has moved out of area",
    "Service unable to provide assistance",
    "Client now requires higher level of care",
    "Client no longer eligible",
    "Client died",
];
const EXIT_REASON_WEIGHTS: [f64; 8] = [0.34, 0.22, 0.14, 0.10, 0.07, 0.06, 0.05, 0.02];

/// SERVICESETTINGCODE.
const SERVICE_SETTING: [&str; 5] = [
    "Organisation outlet/office",
    "Clients residence",
    "Community venue",
    "Telephone",
    "Online service",
];
const SERVICE_SETTING_WEIGHTS: [f64; 5] = [0.45, 0.18, 0.17, 0.12, 0.08];

/// Service-type focus (DEX SERVICETYPEID surrogate / assistance theme).
const SERVICE_TYPES: [&str; 8] = [
    "Employment assistance",
    "Education and skills training",
    "Housing assistance",
    "Mental health",
    "Material wellbeing and basic necessities",
    "Family and relationship support",
    "Physical health",
    "Community engagement",
];
const SERVICE_TYPE_WEIGHTS: [f64; 8] = [0.30, 0.20, 0.10, 0.10, 0.10, 0.08, 0.07, 0.05];

/// DEX SCORE outcome domains (OUTCOMEDOMAIN).
const SCORE_DOMAINS: [&str; 8] = [
    "Material wellbeing",
    "Employment",
    "Housing",
    "Physical health",
    "Mental health, wellbeing and self-care",
    "Family functioning",
    "Community participation and networks",
    "Personal and family safety",
];

// -- legacy consts retained for the (unused) list entry point ----------------
const ASSIST_TYPES: [&str; 8] = [
    "Employment",
    "Training",
    "Housing",
    "Mental Health",
    "Material Wellbeing",
    "Family Relationships",
    "Health",
    "Education",
];
const ASSIST_WEIGHTS: [f64; 8] = [0.30, 0.20, 0.10, 0.10, 0.10, 0.08, 0.07, 0.05];
const EXIT_REASONS: [&str; 4] = [
    "Employment Obtained",
    "Completed Program",
    "No Longer Eligible",
    "Voluntary Withdrawal",
];
const EXIT_WEIGHTS: [f64; 4] = [0.35, 0.25, 0.25, 0.15];

fn gender_code(sex: i32) -> &'static str {
    match sex {
        1 => "Man or male",
        2 => "Woman or female",
        _ => "Not stated",
    }
}

fn atsi_code(indigenous: i32) -> &'static str {
    match indigenous {
        1 => "No",
        2 => "Aboriginal",
        3 => "Torres Strait Islander",
        4 => "Aboriginal and Torres Strait Islander",
        _ => "Not stated/inadequately described",
    }
}

/// EDUCATIONLEVELCODE from the spine education level (0-6).
fn education_code(education: i32) -> &'static str {
    match education {
        1 => EDUCATION_LEVEL[2], // below Year 12 -> Secondary
        2 => EDUCATION_LEVEL[2], // Year 12 -> Secondary
        3 => EDUCATION_LEVEL[3], // Cert III/IV -> Certificate level
        4 => EDUCATION_LEVEL[4], // Diploma -> Advanced diploma and diploma
        5 => EDUCATION_LEVEL[5], // Bachelor
        6 => EDUCATION_LEVEL[7], // Postgrad
        _ => EDUCATION_LEVEL[1], // <15 / not stated -> Primary
    }
}

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

// ============================================================================
// Legacy compact list entry point (unused by build_fplida; kept for the API)
// ============================================================================

/// Project DEX (Data Exchange) from spine — legacy single-table list form.
/// @export
#[extendr]
fn project_dex__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    education: &[i32],
    baseline_employed: &[i32],
    seed: i64,
) -> List {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let est_n = (n as f64 * DEX_RATE) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut out_birth_year: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_gender: Vec<String> = Vec::with_capacity(est_n);
    let mut out_assist: Vec<String> = Vec::with_capacity(est_n);
    let mut out_exit: Vec<String> = Vec::with_capacity(est_n);
    let mut out_cob: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_state: Vec<i32> = Vec::with_capacity(est_n);

    for i in 0..n {
        let age = 2020 - birth_year[i];
        if age < 16 || age > 65 {
            continue;
        }
        let employed = baseline_employed[i] == 1;
        let p = if !employed && education[i] <= 2 {
            DEX_RATE * 3.0
        } else if !employed {
            DEX_RATE * 2.0
        } else {
            DEX_RATE * 0.2
        };
        if rng.gen::<f64>() >= p {
            continue;
        }
        out_aeuid.push(aeuid[i].to_string());
        out_birth_year.push(birth_year[i]);
        out_gender.push(gender_code(sex[i]).to_string());
        out_assist.push(ASSIST_TYPES[weighted_sample(&mut rng, &ASSIST_WEIGHTS)].to_string());
        out_exit.push(EXIT_REASONS[weighted_sample(&mut rng, &EXIT_WEIGHTS)].to_string());
        out_cob.push(country_of_birth[i]);
        out_state.push(state[i]);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        BIRTH_YEAR = out_birth_year,
        GENDERCODE = out_gender,
        ASSISTANCE_TYPE = out_assist,
        EXIT_REASON = out_exit,
        BIRTHCOUNTRYCODE = out_cob,
        STATE_ASGS_2021 = out_state
    )
}

// ============================================================================
// Three-table normalised DEX generator
// ============================================================================

/// Project the DSS Data Exchange to three linked parquet tables from the spine:
/// special_client (client demographics/socioeconomic/geography), special_attendance
/// (service sessions + exit reasons), and special_client_assessment (SCORE outcome
/// domains). Returns the client count.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_dex_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    education: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    sa2: &[i32],
    disability_severity: &[i32],
    seed: i64,
    out_client: &str,
    out_attendance: &str,
    out_assessment: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let est_n = (n as f64 * DEX_RATE) as usize;

    // -- special_client --
    let mut c_aeuid: Vec<String> = Vec::with_capacity(est_n);
    let mut c_byr: Vec<i32> = Vec::with_capacity(est_n);
    let mut c_bmth: Vec<i32> = Vec::with_capacity(est_n);
    let mut c_gndr: Vec<String> = Vec::with_capacity(est_n);
    let mut c_atsi: Vec<String> = Vec::with_capacity(est_n);
    let mut c_cob: Vec<i32> = Vec::with_capacity(est_n);
    let mut c_lang: Vec<String> = Vec::with_capacity(est_n);
    let mut c_emp: Vec<String> = Vec::with_capacity(est_n);
    let mut c_edu: Vec<String> = Vec::with_capacity(est_n);
    let mut c_incamt: Vec<f64> = Vec::with_capacity(est_n);
    let mut c_incfreq: Vec<String> = Vec::with_capacity(est_n);
    let mut c_incsrc: Vec<String> = Vec::with_capacity(est_n);
    let mut c_homeless: Vec<String> = Vec::with_capacity(est_n);
    let mut c_hhcomp: Vec<String> = Vec::with_capacity(est_n);
    let mut c_accom: Vec<String> = Vec::with_capacity(est_n);
    let mut c_ndis: Vec<String> = Vec::with_capacity(est_n);
    let mut c_hascarer: Vec<String> = Vec::with_capacity(est_n);
    let mut c_iscarer: Vec<String> = Vec::with_capacity(est_n);
    let mut c_state: Vec<i32> = Vec::with_capacity(est_n);
    let mut c_pc: Vec<String> = Vec::with_capacity(est_n);
    let mut c_sa1: Vec<i32> = Vec::with_capacity(est_n);
    let mut c_sa2: Vec<i32> = Vec::with_capacity(est_n);
    let mut c_sa3: Vec<i32> = Vec::with_capacity(est_n);
    let mut c_sa4: Vec<i32> = Vec::with_capacity(est_n);
    let mut c_current: Vec<String> = Vec::with_capacity(est_n);

    // -- special_attendance --
    let mut a_aeuid: Vec<String> = Vec::new();
    let mut a_caseclient: Vec<String> = Vec::new();
    let mut a_attid: Vec<String> = Vec::new();
    let mut a_sessid: Vec<String> = Vec::new();
    let mut a_age: Vec<i32> = Vec::new();
    let mut a_date: Vec<i32> = Vec::new();
    let mut a_svctype: Vec<String> = Vec::new();
    let mut a_setting: Vec<String> = Vec::new();
    let mut a_exit: Vec<Option<String>> = Vec::new();
    let mut a_count: Vec<i32> = Vec::new();

    // -- special_client_assessment (SCORE) --
    let mut s_aeuid: Vec<String> = Vec::new();
    let mut s_asmtid: Vec<String> = Vec::new();
    let mut s_sessid: Vec<String> = Vec::new();
    let mut s_attid: Vec<String> = Vec::new();
    let mut s_domain: Vec<String> = Vec::new();
    let mut s_score: Vec<i32> = Vec::new();
    let mut s_asmttype: Vec<String> = Vec::new();
    let mut s_age: Vec<i32> = Vec::new();
    let mut s_date: Vec<i32> = Vec::new();

    let mut cid: u64 = 0;
    let mut sessid: u64 = 0;
    let mut attid: u64 = 0;
    let mut asmtid: u64 = 0;

    for i in 0..n {
        let age = 2020 - birth_year[i];
        if age < 16 || age > 65 {
            continue;
        }
        let employed = baseline_employed[i] == 1;
        let p = if !employed && education[i] <= 2 {
            DEX_RATE * 3.0
        } else if !employed {
            DEX_RATE * 2.0
        } else {
            DEX_RATE * 0.2
        };
        if rng.gen::<f64>() >= p {
            continue;
        }

        cid += 1;
        let sacc = country_of_birth[i];
        let cald = sacc != 1101 && sacc != i32::MIN;
        let s2 = sa2[i];
        let sev = disability_severity[i];

        // Employment status code.
        let emp = if employed {
            if rng.gen::<f64>() < 0.6 {
                EMPLOYMENT_STATUS[0]
            } else {
                EMPLOYMENT_STATUS[1]
            }
        } else if education[i] >= 5 && rng.gen::<f64>() < 0.3 {
            EMPLOYMENT_STATUS[5]
        } else {
            match rng.gen_range(0..10) {
                0..=5 => EMPLOYMENT_STATUS[4], // unemployed looking
                6..=7 => EMPLOYMENT_STATUS[3], // not looking
                8 => EMPLOYMENT_STATUS[8],     // parenting
                _ => EMPLOYMENT_STATUS[7],     // caring
            }
        };
        let inc_src = if employed {
            INCOME_SOURCE[1]
        } else {
            INCOME_SOURCE[4] // government payments
        };
        // A calendar-2021 fortnightly amount, because `baseline_income` is a
        // calendar-2021 annual one. It is only non-zero for the employed, so it
        // is a wage and follows the wage series; the year it belongs to is not
        // known until the client's attendances have been generated, so the
        // INCOMEAMOUNT row is pushed after that loop rather than here.
        let inc_fortnightly_2021 = (baseline_income[i] / 26.0).max(0.0);
        let ndis = match sev {
            1 | 2 => "NDIS eligible",
            3 => {
                if rng.gen::<f64>() < 0.4 {
                    "NDIS in-progress access request"
                } else {
                    "NDIS ineligible"
                }
            }
            _ => "NDIS ineligible",
        };

        c_aeuid.push(aeuid[i].to_string());
        c_byr.push(birth_year[i]);
        c_bmth.push(rng.gen_range(1..=12));
        c_gndr.push(gender_code(sex[i]).to_string());
        c_atsi.push(atsi_code(indigenous[i]).to_string());
        c_cob.push(sacc);
        c_lang.push(
            if cald && rng.gen::<f64>() < 0.5 {
                "Other"
            } else {
                "English"
            }
            .to_string(),
        );
        c_emp.push(emp.to_string());
        c_edu.push(education_code(education[i]).to_string());
        c_incfreq.push("Fortnightly".to_string());
        c_incsrc.push(inc_src.to_string());
        c_homeless.push(HOMELESS[weighted_sample(&mut rng, &HOMELESS_WEIGHTS)].to_string());
        c_hhcomp
            .push(HOUSEHOLD_COMP[weighted_sample(&mut rng, &HOUSEHOLD_COMP_WEIGHTS)].to_string());
        c_accom.push(ACCOMMODATION[weighted_sample(&mut rng, &ACCOMMODATION_WEIGHTS)].to_string());
        c_ndis.push(ndis.to_string());
        c_hascarer.push(if rng.gen::<f64>() < 0.18 { "Yes" } else { "No" }.to_string());
        c_iscarer.push(if rng.gen::<f64>() < 0.10 { "Yes" } else { "No" }.to_string());
        c_state.push(state[i]);
        c_pc.push(format!("{:04}", postcode_for_state(&mut rng, state[i])));
        // SA1 derived by extending SA2 with a within-SA2 suffix.
        c_sa1.push(if s2 > 0 {
            s2 * 100 + rng.gen_range(1..=20)
        } else {
            0
        });
        c_sa2.push(s2);
        c_sa3.push(if s2 > 0 { s2 / 10_000 } else { 0 });
        c_sa4.push(if s2 > 0 { s2 / 1_000_000 } else { 0 });
        c_current.push("Y".to_string());

        // ---- Sessions / attendances + SCORE assessments for this client ----
        let n_sessions = rng.gen_range(1..=8);
        let case_client = format!("CC{:010}", cid);
        // A client's details are re-collected at each contact, so the income on
        // the client record is the income last reported.
        let mut latest_session_yr = i32::MIN;
        for sess_i in 0..n_sessions {
            sessid += 1;
            attid += 1;
            let svc = SERVICE_TYPES[weighted_sample(&mut rng, &SERVICE_TYPE_WEIGHTS)];
            let session_yr = rng.gen_range(2015..=2024);
            latest_session_yr = latest_session_yr.max(session_yr);
            let session_dt = days_since_epoch(
                session_yr,
                rng.gen_range(1..=12) as u32,
                rng.gen_range(1..=28) as u32,
            );
            let is_last = sess_i == n_sessions - 1;
            // Exit reason recorded on the final attendance for ~70% of clients.
            let exit = if is_last && rng.gen::<f64>() < 0.70 {
                Some(EXIT_REASON[weighted_sample(&mut rng, &EXIT_REASON_WEIGHTS)].to_string())
            } else {
                None
            };

            a_aeuid.push(aeuid[i].to_string());
            a_caseclient.push(case_client.clone());
            a_attid.push(format!("ATT{:011}", attid));
            a_sessid.push(format!("SES{:011}", sessid));
            a_age.push(session_yr - birth_year[i]);
            a_date.push(session_dt);
            a_svctype.push(svc.to_string());
            a_setting.push(
                SERVICE_SETTING[weighted_sample(&mut rng, &SERVICE_SETTING_WEIGHTS)].to_string(),
            );
            a_exit.push(exit);
            a_count.push(1);

            // SCORE assessment: ~40% of sessions carry an outcome-domain score.
            if rng.gen::<f64>() < 0.40 {
                asmtid += 1;
                let dom = SCORE_DOMAINS[rng.gen_range(0..SCORE_DOMAINS.len())];
                // SCORE on a 1-5 scale (1 worst .. 5 best), centred mid.
                let score = rng.gen_range(1..=5);
                let asmt_type = match sess_i {
                    0 => "Initial",
                    _ if is_last => "Exit",
                    _ => "Review",
                };
                s_aeuid.push(aeuid[i].to_string());
                s_asmtid.push(format!("ASM{:011}", asmtid));
                s_sessid.push(format!("SES{:011}", sessid));
                s_attid.push(format!("ATT{:011}", attid));
                s_domain.push(dom.to_string());
                s_score.push(score);
                s_asmttype.push(asmt_type.to_string());
                s_age.push(session_yr - birth_year[i]);
                s_date.push(session_dt);
            }
        }

        // One row per client, in client order, so this sits with the other
        // `c_` columns despite being written after the attendance loop.
        // Attendance years are calendar years, and so is the wage series here.
        // Every client drawn here has at least one attendance, so the anchor
        // stands in only if that ever stops being true.
        let income_yr = if latest_session_yr == i32::MIN {
            nominal::ANCHOR
        } else {
            latest_session_yr
        };
        c_incamt.push(
            (inc_fortnightly_2021
                * nominal::factor(
                    nominal::Series::Wage,
                    nominal::Basis::Calendar,
                    nominal::PERSON,
                    nominal::unit_key(aeuid[i].as_ref()),
                    seed,
                    income_yr,
                ))
            .round(),
        );
    }

    let total = c_aeuid.len() as i32;

    let client = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(c_aeuid),
        },
        NamedCol {
            name: "BIRTHYEAR",
            col: Col::I32(c_byr),
        },
        NamedCol {
            name: "BIRTHMONTH",
            col: Col::I32(c_bmth),
        },
        NamedCol {
            name: "GENDERCODE",
            col: Col::Str(c_gndr),
        },
        NamedCol {
            name: "ATSICODE",
            col: Col::Str(c_atsi),
        },
        NamedCol {
            name: "BIRTHCOUNTRYCODE",
            col: Col::I32(c_cob),
        },
        NamedCol {
            name: "MAINLANGUAGECODE",
            col: Col::Str(c_lang),
        },
        NamedCol {
            name: "EMPLOYMENTSTATUSCODE",
            col: Col::Str(c_emp),
        },
        NamedCol {
            name: "EDUCATIONLEVELCODE",
            col: Col::Str(c_edu),
        },
        NamedCol {
            name: "INCOMEAMOUNT",
            col: Col::F64(c_incamt),
        },
        NamedCol {
            name: "INCOMEFREQUENCYCODE",
            col: Col::Str(c_incfreq),
        },
        NamedCol {
            name: "INCOMESOURCECODE",
            col: Col::Str(c_incsrc),
        },
        NamedCol {
            name: "HOMELESSCODE",
            col: Col::Str(c_homeless),
        },
        NamedCol {
            name: "HOUSEHOLDCOMPOSITIONCODE",
            col: Col::Str(c_hhcomp),
        },
        NamedCol {
            name: "ACCOMMODATIONTYPECODE",
            col: Col::Str(c_accom),
        },
        NamedCol {
            name: "NDISELIGIBILITYCODE",
            col: Col::Str(c_ndis),
        },
        NamedCol {
            name: "HASCARER",
            col: Col::Str(c_hascarer),
        },
        NamedCol {
            name: "ISCARER",
            col: Col::Str(c_iscarer),
        },
        NamedCol {
            name: "CLIENTSTATE",
            col: Col::I32(c_state),
        },
        NamedCol {
            name: "CLIENTPOSTCODE",
            col: Col::Str(c_pc),
        },
        NamedCol {
            name: "SA12021BOUNDARYCODE",
            col: Col::I32(c_sa1),
        },
        NamedCol {
            name: "SA22021BOUNDARYCODE",
            col: Col::I32(c_sa2),
        },
        NamedCol {
            name: "SA32021BOUNDARYCODE",
            col: Col::I32(c_sa3),
        },
        NamedCol {
            name: "SA42021BOUNDARYCODE",
            col: Col::I32(c_sa4),
        },
        NamedCol {
            name: "CURRENTRECORD",
            col: Col::Str(c_current),
        },
    ];
    write_columns_to_parquet(out_client, client)
        .unwrap_or_else(|e| panic!("dex special_client parquet write: {}", e));

    let attendance = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(a_aeuid),
        },
        NamedCol {
            name: "CASECLIENTID",
            col: Col::Str(a_caseclient),
        },
        NamedCol {
            name: "ATTENDANCEID",
            col: Col::Str(a_attid),
        },
        NamedCol {
            name: "SESSIONID",
            col: Col::Str(a_sessid),
        },
        NamedCol {
            name: "AGEATSESSION",
            col: Col::I32(a_age),
        },
        NamedCol {
            name: "CONDUCTEDDATE",
            col: Col::DateNN(a_date),
        },
        NamedCol {
            name: "SERVICETYPEID",
            col: Col::Str(a_svctype),
        },
        NamedCol {
            name: "SERVICESETTINGCODE",
            col: Col::Str(a_setting),
        },
        NamedCol {
            name: "EXITREASONCODE",
            col: Col::StrOpt(a_exit),
        },
        NamedCol {
            name: "ATTENDANCECOUNT",
            col: Col::I32(a_count),
        },
    ];
    write_columns_to_parquet(out_attendance, attendance)
        .unwrap_or_else(|e| panic!("dex special_attendance parquet write: {}", e));

    let assessment = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(s_aeuid),
        },
        NamedCol {
            name: "ASSESSMENTID",
            col: Col::Str(s_asmtid),
        },
        NamedCol {
            name: "SESSIONID",
            col: Col::Str(s_sessid),
        },
        NamedCol {
            name: "ATTENDANCEID",
            col: Col::Str(s_attid),
        },
        NamedCol {
            name: "OUTCOMEDOMAIN",
            col: Col::Str(s_domain),
        },
        NamedCol {
            name: "OUTCOMEDOMAINSCORE",
            col: Col::I32(s_score),
        },
        NamedCol {
            name: "ASSESSMENTTYPE",
            col: Col::Str(s_asmttype),
        },
        NamedCol {
            name: "AGEATSESSION",
            col: Col::I32(s_age),
        },
        NamedCol {
            name: "SESSIONCONDUCTEDDATE",
            col: Col::DateNN(s_date),
        },
    ];
    write_columns_to_parquet(out_assessment, assessment)
        .unwrap_or_else(|e| panic!("dex special_client_assessment parquet write: {}", e));

    total
}

extendr_module! {
    mod dex;
    fn project_dex__;
    fn project_dex_to_parquet__;
}

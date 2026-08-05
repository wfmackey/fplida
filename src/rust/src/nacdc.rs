use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

use crate::codeframes;
use crate::mbs::days_since_epoch;
use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
use crate::sampling::{normal_sample, weighted_sample};

// Aged-care participation rises sharply with age. These rates are synthetic
// calibration parameters; the value domains below come from the May 2026
// NACDC table specifications and the source classifications they reference.
const AGED_CARE_RATE_65: f64 = 0.08;
const AGED_CARE_RATE_80: f64 = 0.30;
const CARE_WEIGHTS: [f64; 4] = [0.45, 0.25, 0.25, 0.05];
const HCP_WEIGHTS: [f64; 4] = [0.25, 0.35, 0.25, 0.15];

const SA2_CODE_NAME_TSV: &str = include_str!("../../../inst/extdata/codeframes/sa2_2021.tsv");

static SA2_NAMES: LazyLock<HashMap<String, String>> = LazyLock::new(|| {
    SA2_CODE_NAME_TSV
        .lines()
        .skip(1)
        .filter_map(|line| {
            let mut fields = line.split('\t');
            let _year = fields.next()?;
            let code = fields.next()?;
            let name = fields.next()?;
            if code.is_empty() || name.is_empty() {
                None
            } else {
                Some((code.to_string(), name.to_string()))
            }
        })
        .collect()
});

fn service_sa2_name(code: &str) -> &'static str {
    SA2_NAMES.get(code).map(String::as_str).unwrap_or("")
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CareKind {
    Chsp,
    Hcp,
    Rac,
    Tcp,
}

impl CareKind {
    fn from_index(index: usize) -> Self {
        match index {
            0 => Self::Chsp,
            1 => Self::Hcp,
            2 => Self::Rac,
            _ => Self::Tcp,
        }
    }

    fn short(self) -> &'static str {
        match self {
            Self::Chsp => "CHSP",
            Self::Hcp => "HCP",
            Self::Rac => "RAC",
            Self::Tcp => "TCP",
        }
    }

    fn program_name(self) -> &'static str {
        match self {
            Self::Chsp => "Commonwealth Home Support Program",
            Self::Hcp => "Home Care Packages Program",
            Self::Rac => "Residential care",
            Self::Tcp => "Transition Care Program",
        }
    }

    fn service_approval_type(self) -> &'static str {
        match self {
            Self::Chsp => "Commonwealth Home Support Program (CHSP)",
            Self::Hcp => "Home Care",
            Self::Rac => "Residential",
            Self::Tcp => "Flexible",
        }
    }

    fn service_type(self) -> &'static str {
        match self {
            Self::Chsp => "Commonwealth Home Support Program",
            Self::Hcp => "Home Care",
            Self::Rac => "Residential",
            Self::Tcp => "Transition Care",
        }
    }
}

const MARITAL_CODES: [&str; 5] = ["D", "M", "P", "S", "W"];
const MARITAL_DESCRIPTIONS: [&str; 5] = [
    "Divorced",
    "Married (registered or de facto)",
    "Separated",
    "Never Married",
    "Widowed",
];

struct PreferredLanguage {
    ascl_2016_code: &'static str,
    acap_legacy_code: &'static str,
    description: &'static str,
}

// The common-language subset is sufficient for realistic synthetic data and
// preserves the two distinct official classifications used by NACDC tables:
// four-character ASCL 2016 codes in CHSP and the two-character ACAP adaptation
// in the common recipient table.
const PREFERRED_LANGUAGES: [PreferredLanguage; 11] = [
    PreferredLanguage {
        ascl_2016_code: "1201",
        acap_legacy_code: "02",
        description: "English",
    },
    PreferredLanguage {
        ascl_2016_code: "7104",
        acap_legacy_code: "72",
        description: "Mandarin",
    },
    PreferredLanguage {
        ascl_2016_code: "4202",
        acap_legacy_code: "38",
        description: "Arabic",
    },
    PreferredLanguage {
        ascl_2016_code: "6302",
        acap_legacy_code: "65",
        description: "Vietnamese",
    },
    PreferredLanguage {
        ascl_2016_code: "7101",
        acap_legacy_code: "67",
        description: "Cantonese",
    },
    PreferredLanguage {
        ascl_2016_code: "5207",
        acap_legacy_code: "48",
        description: "Punjabi",
    },
    PreferredLanguage {
        ascl_2016_code: "2201",
        acap_legacy_code: "12",
        description: "Greek",
    },
    PreferredLanguage {
        ascl_2016_code: "2401",
        acap_legacy_code: "13",
        description: "Italian",
    },
    PreferredLanguage {
        ascl_2016_code: "5203",
        acap_legacy_code: "46",
        description: "Hindi",
    },
    PreferredLanguage {
        ascl_2016_code: "2303",
        acap_legacy_code: "16",
        description: "Spanish",
    },
    PreferredLanguage {
        ascl_2016_code: "&&&&",
        acap_legacy_code: "96",
        description: "Not stated/Inadequately described",
    },
];
const LANGUAGE_WEIGHTS: [f64; 11] = [78.0, 3.1, 2.0, 1.8, 1.7, 1.3, 1.2, 1.2, 1.0, 0.9, 1.5];

const PROVIDER_ORGANISATION_TYPES: [&str; 10] = [
    "Charitable",
    "Community Based",
    "Local Government",
    "Private Incorporated Body",
    "Publicly Listed Company",
    "Religious",
    "Religious/Charitable",
    "State Government",
    "Territory Government",
    "Unknown",
];
const PROVIDER_ORGANISATION_WEIGHTS: [f64; 10] =
    [22.0, 18.0, 8.0, 20.0, 3.0, 10.0, 9.0, 5.0, 2.0, 3.0];

const CHSP_ACCOMMODATION_TYPES: [&str; 8] = [
    "Private residence - client or family owned/purchasing",
    "Private residence - private rental",
    "Private residence - public rental",
    "Independent living unit",
    "Boarding house",
    "Supported accommodation",
    "Institutional setting (i.e. residential aged care, hospital)",
    "Other",
];
const HCP_ACCOMMODATION_TYPES: [&str; 8] = [
    "Private residence - client owns or is purchasing",
    "Private residence - private rental",
    "Private residence - public rental or community housing",
    "Independent living within a retirement village",
    "Boarding house, hostel or similar accommodation",
    "Supported community accommodation",
    "Residential aged care service",
    "Other community",
];
const ACCOMMODATION_WEIGHTS: [f64; 8] = [65.0, 9.0, 8.0, 8.0, 2.0, 4.0, 2.0, 2.0];

const CHSP_LIVING_ARRANGEMENTS: [&str; 5] = [
    "Single (person living alone)",
    "Couple",
    "Group (related adults)",
    "Group (unrelated adults)",
    "Not stated/Inadequately described",
];
const HCP_LIVING_ARRANGEMENTS: [&str; 5] = [
    "Lives alone",
    "Lives with partner",
    "Lives with family",
    "Lives with friends",
    "Unknown",
];

const FIRST_CONTACT_SETTINGS: [&str; 5] = [
    "Hospital (acute care)",
    "Other inpatient setting",
    "Residential aged care service",
    "Other",
    "Not stated/inadequately described",
];
const FIRST_CONTACT_WEIGHTS: [f64; 5] = [28.0, 8.0, 7.0, 55.0, 2.0];

struct ChspService {
    description: &'static str,
    subtype: &'static str,
}

// Service descriptions are the CHSP_EPISODE values. Subtypes are exact
// current ACG service-subtype labels selected to be coherent with each type.
const CHSP_SERVICES: [ChspService; 17] = [
    ChspService {
        description: "Allied Health and Therapy Services",
        subtype: "Physiotherapy",
    },
    ChspService {
        description: "Assistance with Care and Housing",
        subtype: "Client Advocacy",
    },
    ChspService {
        description: "Centre-based Respite",
        subtype: "At Centre",
    },
    ChspService {
        description: "Cottage Respite",
        subtype: "Overnight Community Respite",
    },
    ChspService {
        description: "Domestic Assistance",
        subtype: "General House Cleaning",
    },
    ChspService {
        description: "Flexible Respite",
        subtype: "In Home/Community",
    },
    ChspService {
        description: "Goods, Equipment and Assistive Technology",
        subtype: "Support and mobility aids",
    },
    ChspService {
        description: "Home Maintenance",
        subtype: "Minor Home Maintenance and Repairs",
    },
    ChspService {
        description: "Home Modifications",
        subtype: "Car Modification",
    },
    ChspService {
        description: "Meals",
        subtype: "Food Preparation in the Home",
    },
    ChspService {
        description: "Nursing",
        subtype: "Continence Advisory Services",
    },
    ChspService {
        description: "Other Food Services",
        subtype: "Food Advice, Lessons, Training, Food Safety",
    },
    ChspService {
        description: "Personal Care",
        subtype: "Assistance with Self-Care",
    },
    ChspService {
        description: "Social Support Group",
        subtype: "Community Access - Group",
    },
    ChspService {
        description: "Social Support Individual",
        subtype: "Accompanied Activities e.g Shopping",
    },
    ChspService {
        description: "Specialised Support Services",
        subtype: "Dementia Advisory Services",
    },
    ChspService {
        description: "Transport",
        subtype: "Direct (driver is volunteer or worker)",
    },
];
const CHSP_SERVICE_WEIGHTS: [f64; 17] = [
    8.0, 2.0, 4.0, 2.0, 19.0, 4.0, 5.0, 7.0, 2.0, 12.0, 6.0, 2.0, 11.0, 5.0, 7.0, 2.0, 8.0,
];

#[derive(Debug)]
struct AgedCareRecord {
    aeuid: String,
    birth_month: i32,
    birth_year: i32,
    sex_code: String,
    sex_description: String,
    country_of_birth: i32,
    country_of_birth_description: String,
    indigenous_status: String,
    marital_code: String,
    marital_description: String,
    state_abbr: String,
    sa2_code: String,
    care: CareKind,
    hcp_level: Option<String>,
    entry_date: i32,
    exit_date: i32,
    functional_capacity_entry: i32,
    functional_capacity_exit: Option<i32>,
    preferred_language_index: usize,
    accommodation_index: usize,
    living_arrangements_index: usize,
    carer_existence: String,
    provider_id: String,
    provider_organisation_type: String,
    provider_purpose: String,
    provider_incorporated_body: String,
    service_id: String,
    service_status: String,
    chsp_service_index: usize,
    assistance_minutes: i32,
    first_contact_setting: String,
    exit_reason: Option<String>,
    leave_reason: String,
}

fn stable_hash(value: &str) -> u64 {
    // FNV-1a gives stable, platform-independent synthetic identifiers.
    let mut hash = 0xcbf29ce484222325_u64;
    for byte in value.as_bytes() {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

fn nacdc_indigenous_status(spine_code: i32) -> &'static str {
    // Spine: 1 neither, 2 Aboriginal, 3 Torres Strait Islander, 4 both.
    // NACDC: 1 Aboriginal, 2 Torres Strait Islander, 3 both, 4 neither.
    match spine_code {
        2 => "1",
        3 => "2",
        4 => "3",
        1 => "4",
        _ => "9",
    }
}

fn sample_marital_index(rng: &mut StdRng, age: i32, sex: i32) -> usize {
    let weights = if age >= 80 && sex == 2 {
        [8.0, 24.0, 3.0, 8.0, 57.0]
    } else if age >= 80 {
        [8.0, 45.0, 4.0, 10.0, 33.0]
    } else {
        [10.0, 58.0, 5.0, 14.0, 13.0]
    };
    weighted_sample(rng, &weights)
}

fn sample_living_index(rng: &mut StdRng, marital_code: &str) -> usize {
    let weights = if marital_code == "M" {
        [12.0, 70.0, 14.0, 2.0, 2.0]
    } else if marital_code == "W" {
        [64.0, 5.0, 24.0, 4.0, 3.0]
    } else {
        [49.0, 8.0, 31.0, 8.0, 4.0]
    };
    weighted_sample(rng, &weights)
}

fn provider_attributes(
    state: i32,
    care: CareKind,
    aeuid: &str,
) -> (String, String, String, String, String, String) {
    let person_hash = stable_hash(aeuid);
    let provider_bucket = person_hash % 12;
    let provider_key = format!("{}|{}|{}", state, care.short(), provider_bucket);
    let provider_hash = stable_hash(&provider_key);
    let provider_id = format!("PROV{:016X}", provider_hash);

    let service_bucket = (person_hash >> 8) % 3;
    let service_key = format!("{}|{}", provider_key, service_bucket);
    let service_hash = stable_hash(&service_key);
    let service_id = format!("SERV{:016X}", service_hash);

    let mut provider_rng = StdRng::seed_from_u64(provider_hash);
    let org_index = weighted_sample(&mut provider_rng, &PROVIDER_ORGANISATION_WEIGHTS);
    let organisation = PROVIDER_ORGANISATION_TYPES[org_index];
    let purpose = match organisation {
        "Local Government" | "State Government" | "Territory Government" => "Government",
        "Private Incorporated Body" | "Publicly Listed Company" => "For profit",
        _ => "Not for profit",
    };
    let incorporated = match organisation {
        "Unknown" | "Local Government" | "State Government" | "Territory Government" => "U",
        _ if provider_hash % 10 < 8 => "Y",
        _ => "N",
    };
    let status = match service_hash % 100 {
        0 => "Offline",
        1..=2 => "Suspended",
        3..=5 => "Inactive",
        _ => "Operational",
    };

    (
        provider_id,
        organisation.to_string(),
        purpose.to_string(),
        incorporated.to_string(),
        service_id,
        status.to_string(),
    )
}

fn sample_exit_reason(rng: &mut StdRng, care: CareKind) -> &'static str {
    match care {
        CareKind::Hcp => {
            const VALUES: [&str; 5] = [
                "Death",
                "Other",
                "Return to community",
                "To hospital",
                "To residential aged care",
            ];
            const WEIGHTS: [f64; 5] = [24.0, 10.0, 18.0, 16.0, 32.0];
            VALUES[weighted_sample(rng, &WEIGHTS)]
        }
        CareKind::Rac => {
            const VALUES: [&str; 5] = [
                "To other residential aged care",
                "To hospital",
                "Return to community",
                "Other",
                "Death",
            ];
            const WEIGHTS: [f64; 5] = [12.0, 21.0, 8.0, 4.0, 55.0];
            VALUES[weighted_sample(rng, &WEIGHTS)]
        }
        CareKind::Tcp => {
            const VALUES: [&str; 6] = [
                "To residential aged care",
                "To other transition care",
                "To hospital",
                "Return to community",
                "Other",
                "Death",
            ];
            const WEIGHTS: [f64; 6] = [18.0, 5.0, 16.0, 48.0, 9.0, 4.0];
            VALUES[weighted_sample(rng, &WEIGHTS)]
        }
        CareKind::Chsp => "Other",
    }
}

fn sample_leave_reason(rng: &mut StdRng, care: CareKind) -> &'static str {
    match care {
        CareKind::Hcp => {
            const VALUES: [&str; 4] = ["Hospital", "Respite", "Social", "Transition care"];
            const WEIGHTS: [f64; 4] = [44.0, 24.0, 27.0, 5.0];
            VALUES[weighted_sample(rng, &WEIGHTS)]
        }
        CareKind::Rac => {
            const VALUES: [&str; 5] = [
                "Emergency",
                "Transition care",
                "Social",
                "Pre-entry",
                "Hospital",
            ];
            const WEIGHTS: [f64; 5] = [6.0, 4.0, 25.0, 5.0, 60.0];
            VALUES[weighted_sample(rng, &WEIGHTS)]
        }
        _ => "Hospital",
    }
}

fn assistance_minutes(rng: &mut StdRng, service_index: usize) -> i32 {
    let values: &[i32] = match service_index {
        9 | 11 => &[15, 30, 45, 60],
        16 => &[30, 45, 60, 90],
        4 | 7 | 8 | 12 => &[45, 60, 90, 120, 180],
        2 | 3 | 5 => &[60, 90, 120, 180, 240],
        _ => &[30, 45, 60, 90, 120],
    };
    values[rng.gen_range(0..values.len())]
}

#[allow(clippy::too_many_arguments)]
fn generate_records(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    sa2: Option<&[i32]>,
    seed: i64,
    reference_year: i32,
) -> Vec<AgedCareRecord> {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let mut records = Vec::with_capacity(n / 10);
    let first_entry = days_since_epoch(2015, 1, 1);
    let reference_end = days_since_epoch(reference_year, 12, 31);

    for i in 0..n {
        let age = reference_year - birth_year[i];
        let is_indigenous = indigenous[i] >= 2 && indigenous[i] <= 4;
        let min_age = if is_indigenous { 50 } else { 65 };
        if age < min_age {
            continue;
        }
        let participation = if age >= 80 {
            AGED_CARE_RATE_80
        } else {
            AGED_CARE_RATE_65
        };
        if rng.gen::<f64>() >= participation {
            continue;
        }

        let care = CareKind::from_index(weighted_sample(&mut rng, &CARE_WEIGHTS));
        let hcp_level_number = if care == CareKind::Hcp {
            1 + weighted_sample(&mut rng, &HCP_WEIGHTS)
        } else {
            0
        };
        let hcp_level = if hcp_level_number > 0 {
            Some(format!("LEVEL {}", hcp_level_number))
        } else {
            None
        };

        let marital_index = sample_marital_index(&mut rng, age, sex[i]);
        let marital_code = MARITAL_CODES[marital_index];
        let living_index = sample_living_index(&mut rng, marital_code);
        let accommodation_index = weighted_sample(&mut rng, &ACCOMMODATION_WEIGHTS);
        let language_index = weighted_sample(&mut rng, &LANGUAGE_WEIGHTS);
        let chsp_service_index = weighted_sample(&mut rng, &CHSP_SERVICE_WEIGHTS);

        let latest_entry = (reference_end - 30).max(first_entry);
        let entry_date = rng.gen_range(first_entry..=latest_entry);
        let has_exit = rng.gen::<f64>() < 0.30;
        let exit_date = if has_exit {
            let max_duration = match care {
                CareKind::Tcp => 84,
                CareKind::Chsp => 730,
                CareKind::Hcp | CareKind::Rac => 1825,
            };
            let duration = rng.gen_range(7..=max_duration);
            (entry_date + duration).min(reference_end)
        } else {
            i32::MIN
        };

        let functional_mean = match care {
            CareKind::Chsp => 68.0,
            CareKind::Hcp => 52.0,
            CareKind::Rac => 34.0,
            CareKind::Tcp => 46.0,
        };
        let functional_entry = normal_sample(&mut rng, functional_mean, 16.0)
            .clamp(0.0, 100.0)
            .round() as i32;
        let functional_exit = if care == CareKind::Tcp && has_exit {
            Some((functional_entry + rng.gen_range(3..=22)).min(100))
        } else {
            None
        };

        let person_aeuid = aeuid[i].to_string();
        let (
            provider_id,
            provider_organisation_type,
            provider_purpose,
            provider_incorporated_body,
            service_id,
            service_status,
        ) = provider_attributes(state[i], care, &person_aeuid);

        let (sex_code, sex_description) = match sex[i] {
            1 => ("M", "Male"),
            2 => ("F", "Female"),
            _ => ("U", "Unknown"),
        };
        let country_description = codeframes::country_label(country_of_birth[i]);
        let country_description = if country_description.is_empty() {
            "Not stated"
        } else {
            country_description
        };
        let sa2_code = sa2
            .and_then(|values| values.get(i).copied())
            .filter(|value| *value > 0)
            .map(|value| format!("{:09}", value))
            .unwrap_or_default();
        let exit_reason = if has_exit && care != CareKind::Chsp {
            Some(sample_exit_reason(&mut rng, care).to_string())
        } else {
            None
        };

        records.push(AgedCareRecord {
            aeuid: person_aeuid,
            birth_month: rng.gen_range(1..=12),
            birth_year: birth_year[i],
            sex_code: sex_code.to_string(),
            sex_description: sex_description.to_string(),
            country_of_birth: country_of_birth[i],
            country_of_birth_description: country_description.to_string(),
            indigenous_status: nacdc_indigenous_status(indigenous[i]).to_string(),
            marital_code: marital_code.to_string(),
            marital_description: MARITAL_DESCRIPTIONS[marital_index].to_string(),
            state_abbr: codeframes::state_abbr(state[i]).to_string(),
            sa2_code,
            care,
            hcp_level,
            entry_date,
            exit_date,
            functional_capacity_entry: functional_entry,
            functional_capacity_exit: functional_exit,
            preferred_language_index: language_index,
            accommodation_index,
            living_arrangements_index: living_index,
            carer_existence: if rng.gen::<f64>() < 0.46 { "Y" } else { "N" }.to_string(),
            provider_id,
            provider_organisation_type,
            provider_purpose,
            provider_incorporated_body,
            service_id,
            service_status,
            chsp_service_index,
            assistance_minutes: assistance_minutes(&mut rng, chsp_service_index),
            first_contact_setting: FIRST_CONTACT_SETTINGS
                [weighted_sample(&mut rng, &FIRST_CONTACT_WEIGHTS)]
            .to_string(),
            exit_reason,
            leave_reason: sample_leave_reason(&mut rng, care).to_string(),
        });
    }

    records
}

fn write_aged_recipient(path: &str, records: &[AgedCareRecord]) -> std::result::Result<(), String> {
    let rows: Vec<&AgedCareRecord> = records.iter().collect();
    write_columns_to_parquet(
        path,
        vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(rows.iter().map(|r| r.aeuid.clone()).collect()),
            },
            NamedCol {
                name: "BIRTH_MONTH",
                col: Col::I32(rows.iter().map(|r| r.birth_month).collect()),
            },
            NamedCol {
                name: "BIRTH_YEAR",
                col: Col::I32(rows.iter().map(|r| r.birth_year).collect()),
            },
            NamedCol {
                name: "SEX",
                col: Col::Str(rows.iter().map(|r| r.sex_code.clone()).collect()),
            },
            NamedCol {
                name: "SEX_CODE",
                col: Col::Str(rows.iter().map(|r| r.sex_code.clone()).collect()),
            },
            NamedCol {
                name: "SEX_DESC",
                col: Col::Str(rows.iter().map(|r| r.sex_description.clone()).collect()),
            },
            NamedCol {
                name: "COUNTRY_OF_BIRTH",
                col: Col::I32(rows.iter().map(|r| r.country_of_birth).collect()),
            },
            NamedCol {
                name: "COUNTRY_OF_BIRTH_CODE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.country_of_birth.to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "COUNTRY_OF_BIRTH_DESC",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.country_of_birth_description.clone())
                        .collect(),
                ),
            },
            NamedCol {
                name: "INDIGENOUS_STATUS",
                col: Col::Str(rows.iter().map(|r| r.indigenous_status.clone()).collect()),
            },
            NamedCol {
                name: "MARITAL_STATUS",
                col: Col::Str(rows.iter().map(|r| r.marital_description.clone()).collect()),
            },
            NamedCol {
                name: "MARITAL_STATUS_CODE",
                col: Col::Str(rows.iter().map(|r| r.marital_code.clone()).collect()),
            },
            NamedCol {
                name: "MARITAL_STATUS_DESC",
                col: Col::Str(rows.iter().map(|r| r.marital_description.clone()).collect()),
            },
            NamedCol {
                name: "PREFERRED_LANGUAGE_CODE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| {
                            PREFERRED_LANGUAGES[r.preferred_language_index]
                                .acap_legacy_code
                                .to_string()
                        })
                        .collect(),
                ),
            },
            NamedCol {
                name: "PREFERRED_LANGUAGE_DESC",
                col: Col::Str(
                    rows.iter()
                        .map(|r| {
                            PREFERRED_LANGUAGES[r.preferred_language_index]
                                .description
                                .to_string()
                        })
                        .collect(),
                ),
            },
            NamedCol {
                name: "STATE",
                col: Col::Str(rows.iter().map(|r| r.state_abbr.clone()).collect()),
            },
            NamedCol {
                name: "RECIPIENT_STATE",
                col: Col::Str(rows.iter().map(|r| r.state_abbr.clone()).collect()),
            },
            NamedCol {
                name: "CARE_TYPE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.care.program_name().to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "HCP_LEVEL",
                col: Col::StrOpt(rows.iter().map(|r| r.hcp_level.clone()).collect()),
            },
            NamedCol {
                name: "ENTRY_DATE",
                col: Col::DateNN(rows.iter().map(|r| r.entry_date).collect()),
            },
            NamedCol {
                name: "EXIT_DATE",
                col: Col::Date(rows.iter().map(|r| r.exit_date).collect()),
            },
            NamedCol {
                name: "FUNCTIONAL_CAPACITY_SCORE",
                col: Col::F64(
                    rows.iter()
                        .map(|r| r.functional_capacity_entry as f64)
                        .collect(),
                ),
            },
        ],
    )
}

fn write_aged_service(path: &str, records: &[AgedCareRecord]) -> std::result::Result<(), String> {
    let mut seen = HashSet::new();
    let rows: Vec<&AgedCareRecord> = records
        .iter()
        .filter(|r| seen.insert(r.service_id.clone()))
        .collect();
    write_columns_to_parquet(
        path,
        vec![
            NamedCol {
                name: "PROVIDER_ID_ENCRYPTED",
                col: Col::Str(rows.iter().map(|r| r.provider_id.clone()).collect()),
            },
            NamedCol {
                name: "PROVIDER_INCORPORATED_BODY",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.provider_incorporated_body.clone())
                        .collect(),
                ),
            },
            NamedCol {
                name: "PROVIDER_ORGANISATION_TYPE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.provider_organisation_type.clone())
                        .collect(),
                ),
            },
            NamedCol {
                name: "PROVIDER_PURPOSE",
                col: Col::Str(rows.iter().map(|r| r.provider_purpose.clone()).collect()),
            },
            NamedCol {
                name: "PROVIDER_STATE",
                col: Col::Str(rows.iter().map(|r| r.state_abbr.clone()).collect()),
            },
            NamedCol {
                name: "SERVICE_APPROVAL_CARE_TYPE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.care.service_approval_type().to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "SERVICE_ID_ENCRYPTED",
                col: Col::Str(rows.iter().map(|r| r.service_id.clone()).collect()),
            },
            NamedCol {
                name: "SERVICE_SA2_CODE",
                col: Col::Str(rows.iter().map(|r| r.sa2_code.clone()).collect()),
            },
            NamedCol {
                name: "SERVICE_SA2_NAME",
                col: Col::Str(
                    rows.iter()
                        .map(|r| service_sa2_name(&r.sa2_code).to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "SERVICE_STATE",
                col: Col::Str(rows.iter().map(|r| r.state_abbr.clone()).collect()),
            },
            NamedCol {
                name: "SERVICE_STATUS",
                col: Col::Str(rows.iter().map(|r| r.service_status.clone()).collect()),
            },
            NamedCol {
                name: "SERVICE_TYPE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.care.service_type().to_string())
                        .collect(),
                ),
            },
        ],
    )
}

fn write_chsp(path: &str, records: &[AgedCareRecord]) -> std::result::Result<(), String> {
    let rows: Vec<&AgedCareRecord> = records
        .iter()
        .filter(|r| r.care == CareKind::Chsp)
        .collect();
    write_columns_to_parquet(
        path,
        vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(rows.iter().map(|r| r.aeuid.clone()).collect()),
            },
            NamedCol {
                name: "BIRTH_MONTH",
                col: Col::I32(rows.iter().map(|r| r.birth_month).collect()),
            },
            NamedCol {
                name: "BIRTH_YEAR",
                col: Col::I32(rows.iter().map(|r| r.birth_year).collect()),
            },
            NamedCol {
                name: "SEX_CODE",
                col: Col::Str(rows.iter().map(|r| r.sex_code.clone()).collect()),
            },
            NamedCol {
                name: "SEX_DESC",
                col: Col::Str(rows.iter().map(|r| r.sex_description.clone()).collect()),
            },
            NamedCol {
                name: "COUNTRY_OF_BIRTH_CODE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.country_of_birth.to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "COUNTRY_OF_BIRTH_DESC",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.country_of_birth_description.clone())
                        .collect(),
                ),
            },
            NamedCol {
                name: "RECIPIENT_STATE",
                col: Col::Str(rows.iter().map(|r| r.state_abbr.clone()).collect()),
            },
            NamedCol {
                name: "RECIPIENT_SA2_CODE_2016",
                col: Col::Str(rows.iter().map(|r| r.sa2_code.clone()).collect()),
            },
            NamedCol {
                name: "PREFERRED_LANGUAGE_CODE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| {
                            PREFERRED_LANGUAGES[r.preferred_language_index]
                                .ascl_2016_code
                                .to_string()
                        })
                        .collect(),
                ),
            },
            NamedCol {
                name: "PREFERRED_LANGUAGE_DESC",
                col: Col::Str(
                    rows.iter()
                        .map(|r| {
                            PREFERRED_LANGUAGES[r.preferred_language_index]
                                .description
                                .to_string()
                        })
                        .collect(),
                ),
            },
            NamedCol {
                name: "ACCOMMODATION_TYPE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| CHSP_ACCOMMODATION_TYPES[r.accommodation_index].to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "LIVING_ARRANGEMENTS",
                col: Col::Str(
                    rows.iter()
                        .map(|r| CHSP_LIVING_ARRANGEMENTS[r.living_arrangements_index].to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "CARER_EXISTENCE",
                col: Col::Str(rows.iter().map(|r| r.carer_existence.clone()).collect()),
            },
            NamedCol {
                name: "PROVIDER_ID_ENCRYPTED",
                col: Col::Str(rows.iter().map(|r| r.provider_id.clone()).collect()),
            },
            NamedCol {
                name: "PROVIDER_ORGANISATION_TYPE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.provider_organisation_type.clone())
                        .collect(),
                ),
            },
            NamedCol {
                name: "PROVIDER_PURPOSE",
                col: Col::Str(rows.iter().map(|r| r.provider_purpose.clone()).collect()),
            },
            NamedCol {
                name: "SERVICE_TYPE_DESC",
                col: Col::Str(
                    rows.iter()
                        .map(|r| CHSP_SERVICES[r.chsp_service_index].description.to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "SERVICE_SUBTYPE_DESC",
                col: Col::Str(
                    rows.iter()
                        .map(|r| CHSP_SERVICES[r.chsp_service_index].subtype.to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "ASSISTANCE_MINUTES_NBR",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.assistance_minutes.to_string())
                        .collect(),
                ),
            },
        ],
    )
}

fn write_hcp(path: &str, records: &[AgedCareRecord]) -> std::result::Result<(), String> {
    let rows: Vec<&AgedCareRecord> = records.iter().filter(|r| r.care == CareKind::Hcp).collect();
    write_columns_to_parquet(
        path,
        vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(rows.iter().map(|r| r.aeuid.clone()).collect()),
            },
            NamedCol {
                name: "CARE_TYPE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.care.program_name().to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "HCP_LEVEL",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.hcp_level.clone().unwrap_or_default())
                        .collect(),
                ),
            },
            NamedCol {
                name: "ACCOMMODATION_TYPE",
                col: Col::Str(
                    rows.iter()
                        .map(|r| HCP_ACCOMMODATION_TYPES[r.accommodation_index].to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "LIVING_ARRANGEMENTS",
                col: Col::Str(
                    rows.iter()
                        .map(|r| HCP_LIVING_ARRANGEMENTS[r.living_arrangements_index].to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "CARE_APPROVAL_LEVEL",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.hcp_level.clone().unwrap_or_default())
                        .collect(),
                ),
            },
            NamedCol {
                name: "CARE_APPROVAL_TYPE",
                col: Col::Str(rows.iter().map(|_| "Home Care".to_string()).collect()),
            },
            NamedCol {
                name: "FIRST_CONTACT_SETTING",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.first_contact_setting.clone())
                        .collect(),
                ),
            },
            NamedCol {
                name: "ENTRY_CARE_LEVEL",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.hcp_level.clone().unwrap_or_default())
                        .collect(),
                ),
            },
            NamedCol {
                name: "ENTRY_TYPE",
                col: Col::Str(rows.iter().map(|_| "HOME CARE".to_string()).collect()),
            },
            NamedCol {
                name: "ENTRY_DATE",
                col: Col::DateNN(rows.iter().map(|r| r.entry_date).collect()),
            },
            NamedCol {
                name: "EXIT_DATE",
                col: Col::Date(rows.iter().map(|r| r.exit_date).collect()),
            },
            NamedCol {
                name: "EXIT_REASON",
                col: Col::StrOpt(rows.iter().map(|r| r.exit_reason.clone()).collect()),
            },
            NamedCol {
                name: "LEAVE_REASON",
                col: Col::Str(rows.iter().map(|r| r.leave_reason.clone()).collect()),
            },
            NamedCol {
                name: "SERVICE_ID_ENCRYPTED",
                col: Col::Str(rows.iter().map(|r| r.service_id.clone()).collect()),
            },
        ],
    )
}

fn write_rac(path: &str, records: &[AgedCareRecord]) -> std::result::Result<(), String> {
    let rows: Vec<&AgedCareRecord> = records.iter().filter(|r| r.care == CareKind::Rac).collect();
    write_columns_to_parquet(
        path,
        vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(rows.iter().map(|r| r.aeuid.clone()).collect()),
            },
            NamedCol {
                name: "ENTRY_DATE",
                col: Col::DateNN(rows.iter().map(|r| r.entry_date).collect()),
            },
            NamedCol {
                name: "EXIT_DATE",
                col: Col::Date(rows.iter().map(|r| r.exit_date).collect()),
            },
            NamedCol {
                name: "EXIT_REASON",
                col: Col::StrOpt(rows.iter().map(|r| r.exit_reason.clone()).collect()),
            },
            NamedCol {
                name: "LEAVE_REASON",
                col: Col::Str(rows.iter().map(|r| r.leave_reason.clone()).collect()),
            },
            NamedCol {
                name: "SERVICE_ID_ENCRYPTED",
                col: Col::Str(rows.iter().map(|r| r.service_id.clone()).collect()),
            },
        ],
    )
}

fn write_tcp(path: &str, records: &[AgedCareRecord]) -> std::result::Result<(), String> {
    let rows: Vec<&AgedCareRecord> = records.iter().filter(|r| r.care == CareKind::Tcp).collect();
    write_columns_to_parquet(
        path,
        vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(rows.iter().map(|r| r.aeuid.clone()).collect()),
            },
            NamedCol {
                name: "ENTRY_DATE",
                col: Col::DateNN(rows.iter().map(|r| r.entry_date).collect()),
            },
            NamedCol {
                name: "EXIT_DATE",
                col: Col::Date(rows.iter().map(|r| r.exit_date).collect()),
            },
            NamedCol {
                name: "EXIT_REASON",
                col: Col::StrOpt(rows.iter().map(|r| r.exit_reason.clone()).collect()),
            },
            NamedCol {
                name: "FUNCTIONAL_CAPACITY_ENTRY",
                col: Col::Str(
                    rows.iter()
                        .map(|r| r.functional_capacity_entry.to_string())
                        .collect(),
                ),
            },
            NamedCol {
                name: "FUNCTIONAL_CAPACITY_EXIT",
                col: Col::StrOpt(
                    rows.iter()
                        .map(|r| r.functional_capacity_exit.map(|value| value.to_string()))
                        .collect(),
                ),
            },
            NamedCol {
                name: "SERVICE_ID_ENCRYPTED",
                col: Col::Str(rows.iter().map(|r| r.service_id.clone()).collect()),
            },
        ],
    )
}

/// Project NACDC (National Aged Care Data Clearinghouse) from spine.
///
/// Records are for people aged 65+ (50+ for Indigenous people) receiving
/// aged care. This in-memory interface returns the common recipient view.
/// @export
#[extendr]
fn project_nacdc__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    seed: i64,
    reference_year: i32,
) -> List {
    let records = generate_records(
        aeuid,
        birth_year,
        sex,
        state,
        indigenous,
        country_of_birth,
        None,
        seed,
        reference_year,
    );

    list!(
        SYNTHETIC_AEUID = records.iter().map(|r| r.aeuid.clone()).collect::<Vec<_>>(),
        BIRTH_MONTH = records.iter().map(|r| r.birth_month).collect::<Vec<_>>(),
        BIRTH_YEAR = records.iter().map(|r| r.birth_year).collect::<Vec<_>>(),
        SEX = records
            .iter()
            .map(|r| r.sex_code.clone())
            .collect::<Vec<_>>(),
        COUNTRY_OF_BIRTH = records
            .iter()
            .map(|r| r.country_of_birth)
            .collect::<Vec<_>>(),
        INDIGENOUS_STATUS = records
            .iter()
            .map(|r| r.indigenous_status.clone())
            .collect::<Vec<_>>(),
        MARITAL_STATUS = records
            .iter()
            .map(|r| r.marital_description.clone())
            .collect::<Vec<_>>(),
        STATE = records
            .iter()
            .map(|r| r.state_abbr.clone())
            .collect::<Vec<_>>(),
        CARE_TYPE = records
            .iter()
            .map(|r| r.care.program_name().to_string())
            .collect::<Vec<_>>(),
        HCP_LEVEL = records
            .iter()
            .map(|r| r
                .hcp_level
                .clone()
                .unwrap_or_else(|| "Not applicable".to_string()))
            .collect::<Vec<_>>(),
        ENTRY_DATE = records.iter().map(|r| r.entry_date).collect::<Vec<_>>(),
        EXIT_DATE = records
            .iter()
            .map(|r| Rint::from(r.exit_date))
            .collect::<Vec<_>>(),
        FUNCTIONAL_CAPACITY_SCORE = records
            .iter()
            .map(|r| r.functional_capacity_entry as f64)
            .collect::<Vec<_>>()
    )
}

/// Project NACDC directly to one native parquet source per DIL product.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_nacdc_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    sa2: &[i32],
    seed: i64,
    reference_year: i32,
    aged_recipient_out_path: &str,
    aged_service_out_path: &str,
    chsp_out_path: &str,
    hcp_out_path: &str,
    rac_out_path: &str,
    tcp_out_path: &str,
) -> i32 {
    let records = generate_records(
        aeuid,
        birth_year,
        sex,
        state,
        indigenous,
        country_of_birth,
        Some(sa2),
        seed,
        reference_year,
    );

    write_aged_recipient(aged_recipient_out_path, &records)
        .unwrap_or_else(|e| panic!("nacdc aged-recipient parquet write: {}", e));
    write_aged_service(aged_service_out_path, &records)
        .unwrap_or_else(|e| panic!("nacdc aged-service parquet write: {}", e));
    write_chsp(chsp_out_path, &records)
        .unwrap_or_else(|e| panic!("nacdc CHSP parquet write: {}", e));
    write_hcp(hcp_out_path, &records).unwrap_or_else(|e| panic!("nacdc HCP parquet write: {}", e));
    write_rac(rac_out_path, &records).unwrap_or_else(|e| panic!("nacdc RAC parquet write: {}", e));
    write_tcp(tcp_out_path, &records).unwrap_or_else(|e| panic!("nacdc TCP parquet write: {}", e));

    records.len() as i32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn official_sa2_code_names_are_complete() {
        assert_eq!(SA2_NAMES.len(), 2473);
        assert_eq!(service_sa2_name("101021007"), "Braidwood");
        assert_eq!(service_sa2_name("503021295"), "East Perth");
        assert_eq!(service_sa2_name("ZZZZZZZZZ"), "Outside Australia");
        assert_eq!(service_sa2_name("000000000"), "");
    }
}

extendr_module! {
    mod nacdc;
    fn project_nacdc__;
    fn project_nacdc_to_parquet__;
}

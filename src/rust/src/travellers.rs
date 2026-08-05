use crate::codeframes::{self, SACC_AUSTRALIA};
use crate::residence::{
    erp_status_for_quarter, pp_status_for_quarter, pp_weight_for_month, STATUS_NOT_RESIDENT,
    STATUS_RESIDENT_PRESENT,
};
use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::SeedableRng;

const SACC_NEW_ZEALAND: i32 = 1201;

// This is the existing 20-code domain and weighting used by the VISA
// generator. TRAVELLERS adds its documented 000, 001, and 444 fallbacks.
const REFERENCE_VISA_SUBCLASSES: [&str; 20] = [
    "189", "190", "491", "186", "482", "820", "801", "309", "300", "100", "500", "485", "476",
    "417", "462", "200", "201", "202", "204", "866",
];
const REFERENCE_VISA_WEIGHTS: [u32; 20] =
    [10, 8, 4, 6, 8, 6, 3, 4, 3, 2, 15, 2, 1, 6, 3, 3, 2, 2, 1, 2];

fn traveller_hash64(seed: i32, salt: u64) -> u64 {
    let mut x = (seed as i64 as u64) ^ salt.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    x ^= x >> 30;
    x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x ^= x >> 27;
    x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^ (x >> 31)
}

fn reference_visa_subclass(residence_seed: i32) -> &'static str {
    let total: u32 = REFERENCE_VISA_WEIGHTS.iter().sum();
    let draw = (traveller_hash64(residence_seed, 1) % total as u64) as u32;
    let mut cumulative = 0_u32;
    for (subclass, weight) in REFERENCE_VISA_SUBCLASSES
        .iter()
        .zip(REFERENCE_VISA_WEIGHTS.iter())
    {
        cumulative += weight;
        if draw < cumulative {
            return subclass;
        }
    }
    REFERENCE_VISA_SUBCLASSES[REFERENCE_VISA_SUBCLASSES.len() - 1]
}

fn citizenship_country(citizenship: i32, country_of_birth_sacc: i32, seed: i32) -> i32 {
    if citizenship == 1 {
        return SACC_AUSTRALIA;
    }
    if country_of_birth_sacc != SACC_AUSTRALIA
        && !codeframes::country_label(country_of_birth_sacc).is_empty()
    {
        return country_of_birth_sacc;
    }
    if citizenship != 2 && !codeframes::country_label(country_of_birth_sacc).is_empty() {
        return country_of_birth_sacc;
    }

    // The spine records citizenship status but not a second country. For the
    // small Australian-born, non-citizen group, draw a valid overseas SACC
    // country deterministically rather than contradicting that status.
    let mut rng = StdRng::seed_from_u64(traveller_hash64(seed, 2));
    codeframes::sample_overseas_country(&mut rng)
}

fn reference_movement_is_eligible(
    reference_year: i32,
    birth_year: i32,
    year_of_arrival: i32,
    year_of_death: i32,
) -> bool {
    (birth_year == i32::MIN || reference_year >= birth_year)
        && (year_of_arrival == i32::MIN || reference_year >= year_of_arrival)
        && (year_of_death == i32::MIN || reference_year <= year_of_death)
}

/// Project the non-wide TRAVELLERS fields that can be derived from the shared
/// spine and the existing VISA domain. The unpublished VISA stream codeframe
/// is deliberately not represented here.
#[extendr]
fn project_traveller_reference_values__(
    reference_year: i32,
    citizenship: &[i32],
    country_of_birth_sacc: &[i32],
    birth_year: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    residence_seed: &[i32],
) -> List {
    let n = citizenship.len();
    assert_eq!(country_of_birth_sacc.len(), n);
    assert_eq!(birth_year.len(), n);
    assert_eq!(year_of_arrival.len(), n);
    assert_eq!(year_of_death.len(), n);
    assert_eq!(residence_seed.len(), n);

    let mut countries = Vec::with_capacity(n);
    let mut subclasses = Vec::with_capacity(n);

    for i in 0..n {
        let country =
            citizenship_country(citizenship[i], country_of_birth_sacc[i], residence_seed[i]);
        let subclass = if citizenship[i] == 1 {
            "000"
        } else if country == SACC_NEW_ZEALAND {
            "444"
        } else if !reference_movement_is_eligible(
            reference_year,
            birth_year[i],
            year_of_arrival[i],
            year_of_death[i],
        ) {
            "001"
        } else {
            reference_visa_subclass(residence_seed[i])
        };

        countries.push(format!("{:04}", country));
        subclasses.push(subclass.to_string());
    }

    list!(
        COUNTRY_OF_CITIZENSHIP = countries,
        VISA_SUBCLASS = subclasses
    )
}

/// Project one metadata-shaped TRAVELLERS residence field for a small DIL
/// frame. This deliberately does not widen the main whole-population
/// TRAVELLERS products.
///
/// `kind` values:
/// 1 = monthly physically-present weight; 2 = monthly physically-present
/// status; 3 = quarterly ERP status; 4 = Census-night ERP status (Q3 proxy);
/// 5 = Census-night physically-present status (Q3 proxy).
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_traveller_dil_values__(
    kind: i32,
    year: i32,
    period: i32,
    birth_year: &[i32],
    month_of_birth: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    residence_seed: &[i32],
) -> List {
    let n = birth_year.len();
    assert_eq!(month_of_birth.len(), n);
    assert_eq!(country_of_birth.len(), n);
    assert_eq!(year_of_arrival.len(), n);
    assert_eq!(year_of_death.len(), n);
    assert_eq!(month_of_death.len(), n);
    assert_eq!(day_of_death.len(), n);
    assert_eq!(residence_seed.len(), n);

    let values: Vec<f64> = (0..n)
        .map(|i| match kind {
            1 => pp_weight_for_month(
                birth_year[i],
                month_of_birth[i],
                country_of_birth[i],
                year_of_arrival[i],
                year_of_death[i],
                month_of_death[i],
                day_of_death[i],
                residence_seed[i],
                year,
                period,
            ),
            2 => {
                let weight = pp_weight_for_month(
                    birth_year[i],
                    month_of_birth[i],
                    country_of_birth[i],
                    year_of_arrival[i],
                    year_of_death[i],
                    month_of_death[i],
                    day_of_death[i],
                    residence_seed[i],
                    year,
                    period,
                );
                if weight > 0.0 {
                    STATUS_RESIDENT_PRESENT as f64
                } else {
                    STATUS_NOT_RESIDENT as f64
                }
            }
            3 => erp_status_for_quarter(
                birth_year[i],
                month_of_birth[i],
                country_of_birth[i],
                year_of_arrival[i],
                year_of_death[i],
                month_of_death[i],
                day_of_death[i],
                residence_seed[i],
                year,
                period,
            ) as f64,
            4 => erp_status_for_quarter(
                birth_year[i],
                month_of_birth[i],
                country_of_birth[i],
                year_of_arrival[i],
                year_of_death[i],
                month_of_death[i],
                day_of_death[i],
                residence_seed[i],
                year,
                3,
            ) as f64,
            5 => pp_status_for_quarter(
                birth_year[i],
                month_of_birth[i],
                country_of_birth[i],
                year_of_arrival[i],
                year_of_death[i],
                month_of_death[i],
                day_of_death[i],
                residence_seed[i],
                year,
                3,
            ) as f64,
            _ => panic!("unknown TRAVELLERS DIL field kind: {}", kind),
        })
        .collect();

    list!(values = values)
}

/// Project TRAVELLERS dataset from spine.
///
/// Generates monthly/quarterly status indicators for each person tracking
/// whether they are resident/present in Australia. Three products:
/// - travellers (movement records)
/// - erp (Estimated Resident Population status)
/// - pp (Physically Present status)
///
/// For simplicity, we generate quarterly ERP status for 2006–2024.
/// Overseas-born persons have more complex patterns (travel, temporary absence).
/// Australian-born have simpler patterns (mostly resident-present).
/// @export
#[extendr]
fn project_travellers__(
    aeuid: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    residence_seed: &[i32],
    seed: i64,
    min_year: i32,
    max_year: i32,
) -> List {
    let n = birth_year.len();
    let _ = seed;
    let n_years = (max_year - min_year + 1) as usize;
    let n_quarters = n_years * 4;

    // Build quarter labels: "2006Q1", "2006Q2", etc.
    let mut quarter_labels: Vec<String> = Vec::with_capacity(n_quarters);
    for yr in min_year..=max_year {
        for q in 1..=4 {
            quarter_labels.push(format!("{}Q{}", yr, q));
        }
    }

    // Output: one row per person, with quarterly status columns
    // We'll return AEUID + quarterly ERP status as separate vectors
    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);

    // For ERP status: pack all quarters into a pipe-separated string per person
    let mut out_erp_status: Vec<String> = Vec::with_capacity(n);
    // For PP status: similar
    let mut out_pp_status: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        let person_aeuid = aeuid[i].to_string();
        let mut erp_quarters: Vec<i32> = Vec::with_capacity(n_quarters);
        let mut pp_quarters: Vec<i32> = Vec::with_capacity(n_quarters);

        for yr in min_year..=max_year {
            for q in 1..=4 {
                erp_quarters.push(erp_status_for_quarter(
                    birth_year[i],
                    month_of_birth[i],
                    country_of_birth[i],
                    year_of_arrival[i],
                    year_of_death[i],
                    month_of_death[i],
                    day_of_death[i],
                    residence_seed[i],
                    yr,
                    q,
                ));
                pp_quarters.push(pp_status_for_quarter(
                    birth_year[i],
                    month_of_birth[i],
                    country_of_birth[i],
                    year_of_arrival[i],
                    year_of_death[i],
                    month_of_death[i],
                    day_of_death[i],
                    residence_seed[i],
                    yr,
                    q,
                ));
            }
        }

        let erp_str: String = erp_quarters
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("|");
        let pp_str: String = pp_quarters
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("|");

        out_aeuid.push(person_aeuid);
        out_erp_status.push(erp_str);
        out_pp_status.push(pp_str);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        ERP_STATUS_QUARTERLY = out_erp_status,
        PP_STATUS_QUARTERLY = out_pp_status,
        QUARTER_LABELS = quarter_labels
    )
}

/// Project TRAVELLERS directly to parquet.
///
/// Writes three products to `out_dir` under the names:
///   {out_dir}/{combined_name}.parquet  (AEUID + ERP + PP)
///   {out_dir}/{erp_name}.parquet       (AEUID + ERP)
///   {out_dir}/{pp_name}.parquet        (AEUID + PP)
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_travellers_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    residence_seed: &[i32],
    seed: i64,
    min_year: i32,
    max_year: i32,
    out_dir: &str,
    combined_name: &str,
    erp_name: &str,
    pp_name: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = birth_year.len();
    let _ = seed;
    let n_years = (max_year - min_year + 1) as usize;
    let n_quarters = n_years * 4;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut out_erp_status: Vec<String> = Vec::with_capacity(n);
    let mut out_pp_status: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        let person_aeuid = aeuid[i].to_string();
        let mut erp_quarters: Vec<i32> = Vec::with_capacity(n_quarters);
        let mut pp_quarters: Vec<i32> = Vec::with_capacity(n_quarters);

        for yr in min_year..=max_year {
            for q in 1..=4 {
                erp_quarters.push(erp_status_for_quarter(
                    birth_year[i],
                    month_of_birth[i],
                    country_of_birth[i],
                    year_of_arrival[i],
                    year_of_death[i],
                    month_of_death[i],
                    day_of_death[i],
                    residence_seed[i],
                    yr,
                    q,
                ));
                pp_quarters.push(pp_status_for_quarter(
                    birth_year[i],
                    month_of_birth[i],
                    country_of_birth[i],
                    year_of_arrival[i],
                    year_of_death[i],
                    month_of_death[i],
                    day_of_death[i],
                    residence_seed[i],
                    yr,
                    q,
                ));
            }
        }

        let erp_str: String = erp_quarters
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("|");
        let pp_str: String = pp_quarters
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("|");

        out_aeuid.push(person_aeuid);
        out_erp_status.push(erp_str);
        out_pp_status.push(pp_str);
    }

    let total = out_aeuid.len() as i32;

    // Product 1: combined (AEUID + ERP + PP)
    {
        let cols = vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(out_aeuid.clone()),
            },
            NamedCol {
                name: "ERP_STATUS_QUARTERLY",
                col: Col::Str(out_erp_status.clone()),
            },
            NamedCol {
                name: "PP_STATUS_QUARTERLY",
                col: Col::Str(out_pp_status.clone()),
            },
        ];
        let path = format!("{}/{}.parquet", out_dir, combined_name);
        write_columns_to_parquet(&path, cols)
            .unwrap_or_else(|e| panic!("travellers combined write: {}", e));
    }
    // Product 2: ERP only
    {
        let cols = vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(out_aeuid.clone()),
            },
            NamedCol {
                name: "ERP_STATUS_QUARTERLY",
                col: Col::Str(out_erp_status),
            },
        ];
        let path = format!("{}/{}.parquet", out_dir, erp_name);
        write_columns_to_parquet(&path, cols)
            .unwrap_or_else(|e| panic!("travellers erp write: {}", e));
    }
    // Product 3: PP only
    {
        let cols = vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(out_aeuid),
            },
            NamedCol {
                name: "PP_STATUS_QUARTERLY",
                col: Col::Str(out_pp_status),
            },
        ];
        let path = format!("{}/{}.parquet", out_dir, pp_name);
        write_columns_to_parquet(&path, cols)
            .unwrap_or_else(|e| panic!("travellers pp write: {}", e));
    }

    total
}

extendr_module! {
    mod travellers;
    fn project_traveller_dil_values__;
    fn project_traveller_reference_values__;
    fn project_travellers__;
    fn project_travellers_to_parquet__;
}

use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

// ~8% of adults own a business; ~1.2 ABNs per owner on average
const BUSINESS_OWNER_RATE: f64 = 0.08;
const OWNER_PRIMARY_SLOT: i64 = 2_000_000_000;
const OWNER_SECONDARY_SLOT: i64 = 2_000_000_001;

fn owner_business_id(
    pool: &[String],
    aeuid: &str,
    slot: i64,
    seed: i64,
    rng: &mut StdRng,
) -> String {
    let person_number = crate::business_pool::person_number(aeuid);
    crate::business_pool::employer_bn(pool, person_number, slot, seed)
        .unwrap_or_else(|| format!("ABN{:012X}", rng.gen::<u64>() & 0xFFFF_FFFF_FFFF))
}

/// Project Business Ownership (BUSOWN) from spine.
///
/// Person × FY records. ~8% of adult population. Some have multiple ABNs.
/// @export
#[extendr]
fn project_busown__(
    aeuid: Strings,
    birth_year: &[i32],
    baseline_employed: &[i32],
    seed: i64,
    fy_start: i32,
    fy_end: i32,
) -> List {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let business_pool = crate::business_pool::snapshot();

    let est_rows = (n as f64 * BUSINESS_OWNER_RATE * 1.2 * (fy_end - fy_start + 1) as f64) as usize;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_fin_year: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_abn: Vec<String> = Vec::with_capacity(est_rows);

    // Pre-determine who is a business owner (stable across years)
    let mut is_owner: Vec<bool> = Vec::with_capacity(n);
    for i in 0..n {
        let age = 2020 - birth_year[i];
        let p = if age < 20 || age > 70 {
            0.01
        } else {
            BUSINESS_OWNER_RATE
        };
        is_owner.push(rng.gen::<f64>() < p);
    }

    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        for i in 0..n {
            if !is_owner[i] {
                continue;
            }

            let person_aeuid = aeuid[i].to_string();

            // Primary owner business. When BLADE exists this is a real `bn`;
            // otherwise keep the legacy standalone ABN hash fallback.
            let abn = owner_business_id(
                &business_pool,
                &person_aeuid,
                OWNER_PRIMARY_SLOT,
                seed,
                &mut rng,
            );
            out_aeuid.push(person_aeuid.clone());
            out_fin_year.push(fy_str.clone());
            out_abn.push(abn);

            // ~20% have a second ABN
            if rng.gen::<f64>() < 0.20 {
                let abn2 = owner_business_id(
                    &business_pool,
                    &person_aeuid,
                    OWNER_SECONDARY_SLOT,
                    seed,
                    &mut rng,
                );
                out_aeuid.push(person_aeuid);
                out_fin_year.push(fy_str.clone());
                out_abn.push(abn2);
            }
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        FIN_YEAR = out_fin_year,
        ABN_HASH_TRUNC = out_abn
    )
}

/// Project BUSOWN directly to per-FY parquet files.
///
/// Runs the same generation as `project_busown__` but writes each
/// year's rows immediately to a parquet file under `out_dir`,
/// never building a combined R list. Avoids the R vector-memory
/// OOM that kills the old as.data.frame path at 30m scale.
/// @export
#[extendr]
fn project_busown_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    baseline_employed: &[i32],
    seed: i64,
    fy_start: i32,
    fy_end: i32,
    out_dir: &str,
    product_prefix: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let _ = baseline_employed;

    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let business_pool = crate::business_pool::snapshot();

    // Pre-determine owners once (stable across years).
    let mut is_owner: Vec<bool> = Vec::with_capacity(n);
    for i in 0..n {
        let age = 2020 - birth_year[i];
        let p = if age < 20 || age > 70 {
            0.01
        } else {
            BUSINESS_OWNER_RATE
        };
        is_owner.push(rng.gen::<f64>() < p);
    }

    let aeuid_owned: Vec<String> = aeuid.iter().map(|s| s.to_string()).collect();

    let mut total_rows: usize = 0;
    for fy in fy_start..=fy_end {
        let fy_str = format!("{}-{:02}", fy - 1, fy % 100);

        let mut year_aeuid: Vec<String> = Vec::new();
        let mut year_fin: Vec<String> = Vec::new();
        let mut year_abn: Vec<String> = Vec::new();

        for i in 0..n {
            if !is_owner[i] {
                continue;
            }

            let abn = owner_business_id(
                &business_pool,
                &aeuid_owned[i],
                OWNER_PRIMARY_SLOT,
                seed,
                &mut rng,
            );
            year_aeuid.push(aeuid_owned[i].clone());
            year_fin.push(fy_str.clone());
            year_abn.push(abn);

            if rng.gen::<f64>() < 0.20 {
                let abn2 = owner_business_id(
                    &business_pool,
                    &aeuid_owned[i],
                    OWNER_SECONDARY_SLOT,
                    seed,
                    &mut rng,
                );
                year_aeuid.push(aeuid_owned[i].clone());
                year_fin.push(fy_str.clone());
                year_abn.push(abn2);
            }
        }

        total_rows += year_aeuid.len();

        let fname = format!(
            "{}{:02}{:02}.parquet",
            product_prefix,
            (fy - 1) % 100,
            fy % 100
        );
        let out_path = format!("{}/{}", out_dir, fname);

        let cols = vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(year_aeuid),
            },
            NamedCol {
                name: "FIN_YEAR",
                col: Col::Str(year_fin),
            },
            NamedCol {
                name: "ABN_HASH_TRUNC",
                col: Col::Str(year_abn),
            },
        ];
        write_columns_to_parquet(&out_path, cols)
            .unwrap_or_else(|e| panic!("busown parquet write: {}", e));
    }

    total_rows as i32
}

extendr_module! {
    mod busown;
    fn project_busown__;
    fn project_busown_to_parquet__;
}

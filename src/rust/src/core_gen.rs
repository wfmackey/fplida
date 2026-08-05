use extendr_api::prelude::*;
use rand::prelude::*;
use rand::rngs::StdRng;
use std::fs;

// Mortality rates by 8 age bands: <20, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+
const MORTALITY: [f64; 8] = [
    0.0003, 0.0005, 0.0007, 0.0012, 0.0030, 0.0070, 0.0170, 0.0550,
];

const OVERSEAS_CODES: [&str; 20] = [
    "2100", "7100", "6100", "5101", "5201", "5203", "5105", "6102", "7103", "6104", "3206", "3103",
    "2201", "7105", "3207", "5204", "7108", "6105", "3104", "2304",
];

const OVERSEAS_WEIGHTS: [f64; 20] = [
    927490.0, 673354.0, 549628.0, 530491.0, 293899.0, 257997.0, 189204.0, 165605.0, 163329.0,
    131907.0, 122507.0, 102087.0, 101309.0, 101256.0, 100158.0, 92925.0, 92305.0, 89636.0, 87343.0,
    87068.0,
];

fn age_band_8(age: i32) -> usize {
    match age {
        a if a < 20 => 0,
        a if a < 30 => 1,
        a if a < 40 => 2,
        a if a < 50 => 3,
        a if a < 60 => 4,
        a if a < 70 => 5,
        a if a < 80 => 6,
        _ => 7,
    }
}

/// Project CORE demographics from spine vectors.
/// @export
#[extendr]
fn project_core_demographics__(
    spine_id: Strings,
    birth_year: &[i32],
    sex: &[i32],
    country_of_birth_sacc: &[i32],
    seed: i32,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(700));

    let spine_ids: Vec<Option<String>> = spine_id
        .iter()
        .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
        .collect();
    let mut month_of_birth: Vec<i32> = Vec::with_capacity(n);
    let mut birth_ctry_code: Vec<String> = Vec::with_capacity(n);
    let mut core_gender: Vec<String> = Vec::with_capacity(n);
    let mut year_of_death: Vec<Rint> = Vec::with_capacity(n);
    let mut month_of_death: Vec<Rint> = Vec::with_capacity(n);
    let mut day_of_death: Vec<Rint> = Vec::with_capacity(n);

    for i in 0..n {
        let age = 2021 - birth_year[i];

        month_of_birth.push(rng.gen_range(1..=12));

        // BIRTH_CTRY_CODE is the spine SACC country code, so CORE, Census and
        // every other PLIDA dataset resolve the same person to the same country.
        birth_ctry_code.push(country_of_birth_sacc[i].to_string());

        core_gender.push(if sex[i] == 1 {
            "M".to_string()
        } else {
            "F".to_string()
        });

        // Year-by-year survival hazard over the 2006-2024 observation window:
        // step through each year applying that year's age-specific mortality
        // rate, so the death YEAR follows the compounding hazard (later years
        // more likely as the person ages) instead of a uniform draw, and total
        // mortality stays consistent with the per-year rates.
        let _ = age; // 2021-reference age retained for other columns
        let mut died_year: Option<i32> = None;
        for y in 2006..=2024 {
            let age_y = y - birth_year[i];
            if age_y < 0 {
                continue; // not yet born
            }
            let rate_y = MORTALITY[age_band_8(age_y)];
            if rng.gen::<f64>() < rate_y {
                died_year = Some(y);
                break;
            }
        }
        if let Some(y) = died_year {
            year_of_death.push(Rint::from(y));
            month_of_death.push(Rint::from(rng.gen_range(1..=12)));
            day_of_death.push(Rint::from(rng.gen_range(1..=28)));
        } else {
            year_of_death.push(Rint::na());
            month_of_death.push(Rint::na());
            day_of_death.push(Rint::na());
        }
    }

    list!(
        SPINE_ID = spine_ids,
        YEAR_OF_BIRTH = birth_year.to_vec(),
        MONTH_OF_BIRTH = month_of_birth,
        BIRTH_CTRY_CODE = birth_ctry_code,
        CORE_GENDER = core_gender,
        YEAR_OF_DEATH = year_of_death,
        MONTH_OF_DEATH = month_of_death,
        DAY_OF_DEATH = day_of_death,
    )
}

/// Project CORE relationships from spine vectors.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_core_locations__(
    spine_id: Strings,
    state: &[i32],
    sa2: &[i32],
    lookup_state: &[i32],
    lookup_mb_code: Strings,
    lookup_sa1_code: Strings,
    lookup_sa2_code: Strings,
    lookup_sa4_code: &[i32],
    seed: i32,
) -> List {
    let n = state.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(701));

    let spine_ids: Vec<Option<String>> = spine_id
        .iter()
        .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
        .collect();
    let lookup_mb: Vec<String> = lookup_mb_code.iter().map(|s| s.to_string()).collect();
    let lookup_sa1: Vec<String> = lookup_sa1_code.iter().map(|s| s.to_string()).collect();
    let lookup_sa2: Vec<String> = lookup_sa2_code.iter().map(|s| s.to_string()).collect();

    let mut by_state: Vec<Vec<usize>> = vec![Vec::new(); 9];
    for (i, &st) in lookup_state.iter().enumerate() {
        if (1..=8).contains(&st) {
            by_state[st as usize].push(i);
        }
    }

    // Mesh blocks indexed by their SA2, so a person's CORE address is drawn
    // from inside the SA2 the spine already assigned them. ASGS 2021 geography
    // then agrees between CORE and Census (and NDIS, DEX, NACDC), instead of
    // CORE re-drawing an unrelated mesh block from anywhere in the state.
    // Rows are pushed in lookup order and only ever read by key, so the map's
    // iteration order never reaches the RNG.
    let mut by_sa2: std::collections::HashMap<i32, Vec<usize>> =
        std::collections::HashMap::new();
    for (i, s) in lookup_sa2.iter().enumerate() {
        if let Ok(code) = s.parse::<i32>() {
            by_sa2.entry(code).or_default().push(i);
        }
    }

    let mut out_sa1: Vec<String> = Vec::with_capacity(n);
    let mut out_sa2: Vec<String> = Vec::with_capacity(n);
    let mut out_sa4: Vec<i32> = Vec::with_capacity(n);
    let mut out_mb: Vec<String> = Vec::with_capacity(n);
    let mut out_arid: Vec<String> = Vec::with_capacity(n);
    let out_adr_typ: Vec<&str> = vec!["R"; n];
    let out_source_flag: Vec<&str> = vec!["CENSUS"; n];
    let out_start_date: Vec<&str> = vec!["2006-01-01"; n];
    let out_end_date: Vec<Option<String>> = vec![None; n];

    let max_base = 0xFFFF_FFFF_FFFFu64.saturating_sub(n as u64);
    let arid_base = if max_base == 0 {
        0
    } else {
        rng.gen_range(0..=max_base)
    };

    let fallback = by_state
        .iter()
        .find(|rows| !rows.is_empty())
        .and_then(|rows| rows.first().copied())
        .unwrap_or(0);

    for i in 0..n {
        let st = state[i].clamp(1, 8) as usize;
        let state_pool = if by_state[st].is_empty() {
            &by_state[1]
        } else {
            &by_state[st]
        };
        // Restrict to the spine SA2; fall back to the state pool when the
        // spine SA2 is 0/NA or absent from the mesh block lookup.
        let sa2_pool = if sa2[i] > 0 { by_sa2.get(&sa2[i]) } else { None };
        let pool = match sa2_pool {
            Some(rows) if !rows.is_empty() => rows,
            _ => state_pool,
        };
        let lookup_idx = if pool.is_empty() {
            fallback
        } else {
            pool[rng.gen_range(0..pool.len())]
        };

        let sa1 = lookup_sa1[lookup_idx].clone();
        let sa2 = lookup_sa2[lookup_idx].clone();
        let mb = lookup_mb[lookup_idx].clone();

        out_sa1.push(sa1.clone());
        out_sa2.push(sa2);
        out_sa4.push(lookup_sa4_code[lookup_idx]);
        out_mb.push(mb);
        out_arid.push(format!("{:012X}", arid_base + i as u64));
    }

    list!(
        SPINE_ID = spine_ids,
        STATE = state.to_vec(),
        SA4_ASGS_2021 = out_sa4,
        SA2_ASGS_2021 = out_sa2,
        SA1_ASGS_2021 = out_sa1,
        MB_ASGS_2021 = out_mb,
        ARID = out_arid,
        ADR_TYP = out_adr_typ,
        SOURCE_FLAG = out_source_flag,
        START_DATE = out_start_date,
        END_DATE = out_end_date,
    )
}

/// Project CORE residence directly to a parquet part directory.
///
/// Writes one part per calendar month:
/// `{out_dir}/{product_name}/part-YYYYMM.parquet`.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_core_residence_to_parquet__(
    spine_id: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    residence_seed: &[i32],
    min_year: i32,
    max_year: i32,
    out_dir: &str,
    product_name: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    use crate::residence::{month_end_date32, pp_weight_for_month};

    let n = birth_year.len();
    let prod_dir = format!("{}/{}", out_dir, product_name);
    fs::create_dir_all(&prod_dir).unwrap_or_else(|e| panic!("create {}: {}", prod_dir, e));

    let spine_ids: Vec<Option<String>> = spine_id
        .iter()
        .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
        .collect();
    let mut total: i64 = 0;

    for year in min_year..=max_year {
        for month in 1..=12 {
            let period = month_end_date32(year, month);
            let mut pp_period: Vec<i32> = Vec::with_capacity(n);
            let mut pp_weight: Vec<f64> = Vec::with_capacity(n);

            for i in 0..n {
                pp_period.push(period);
                pp_weight.push(pp_weight_for_month(
                    birth_year[i],
                    month_of_birth[i],
                    country_of_birth[i],
                    year_of_arrival[i],
                    year_of_death[i],
                    month_of_death[i],
                    day_of_death[i],
                    residence_seed[i],
                    year,
                    month,
                ));
            }

            let path = format!("{}/part-{:04}{:02}.parquet", prod_dir, year, month);
            let cols = vec![
                NamedCol {
                    name: "spine_id",
                    col: Col::StrOpt(spine_ids.clone()),
                },
                NamedCol {
                    name: "pp_period",
                    col: Col::DateNN(pp_period),
                },
                NamedCol {
                    name: "pp_weight",
                    col: Col::F64(pp_weight),
                },
            ];
            write_columns_to_parquet(&path, cols)
                .unwrap_or_else(|e| panic!("core residence parquet write {}: {}", path, e));
            total += n as i64;
        }
    }

    total.min(i32::MAX as i64) as i32
}

/// Project CORE relationships from spine vectors.
/// @export
#[extendr]
fn project_core_relationships__(
    spine_id: Strings,
    birth_year: &[i32],
    household_id: &[i32],
    seed: i32,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(702));

    let spine_ids: Vec<Option<String>> = spine_id
        .iter()
        .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
        .collect();

    let age = |i: usize| 2021 - birth_year[i];

    // Group persons by their spine household id and derive relationships from
    // co-residence, so CORE relationships exactly match the spine households
    // (and any Census dwelling/family assembly built from the same id): the two
    // adults of a household are partners, and each child is linked to up to two
    // co-resident parenting adults. CORE runs centrally on the full population,
    // so every household is complete here. Household keys are sorted for
    // reproducibility.
    let mut by_hh: std::collections::HashMap<i32, Vec<usize>> = std::collections::HashMap::new();
    for i in 0..n {
        by_hh.entry(household_id[i]).or_default().push(i);
    }
    let mut hh_keys: Vec<i32> = by_hh.keys().copied().collect();
    hh_keys.sort_unstable();

    let mut sid_orig: Vec<Option<String>> = Vec::new();
    let mut sid_rel: Vec<Option<String>> = Vec::new();
    let mut pairids: Vec<String> = Vec::new();
    let mut categories: Vec<String> = Vec::new();
    let mut statuses: Vec<String> = Vec::new();
    let mut record_starts: Vec<String> = Vec::new();
    let mut sources: Vec<String> = Vec::new();
    let mut source_flags: Vec<String> = Vec::new();

    for hh in hh_keys {
        let members = &by_hh[&hh];
        let adults: Vec<usize> = members.iter().copied().filter(|&i| age(i) >= 18).collect();
        let children: Vec<usize> = members.iter().copied().filter(|&i| age(i) < 18).collect();

        // Partners: the two adults of a (couple) household.
        if adults.len() >= 2 {
            let a = adults[0];
            let b = adults[1];
            sid_orig.push(spine_ids[a].clone());
            sid_rel.push(spine_ids[b].clone());
            pairids.push(format!("PR{:010X}", rng.gen::<u32>()));
            categories.push("Partner".to_string());
            statuses.push(
                if rng.gen::<f64>() < 0.80 {
                    "Married"
                } else {
                    "De facto"
                }
                .to_string(),
            );
            let years_ago = rng.gen_range(1..=20i32);
            record_starts.push(format!("{}-01-01", 2021 - years_ago));
            sources.push("CENSUS".to_string());
            source_flags.push("CENSUS".to_string());
        }

        // Parent-child: link each child to up to two co-resident parents.
        for &child in &children {
            for &parent in adults.iter().take(2) {
                sid_orig.push(spine_ids[child].clone());
                sid_rel.push(spine_ids[parent].clone());
                pairids.push(format!("PC{:010X}", rng.gen::<u32>()));
                categories.push("Parent-Child".to_string());
                statuses.push(
                    if rng.gen::<f64>() < 0.90 {
                        "Biological"
                    } else {
                        "Step"
                    }
                    .to_string(),
                );
                record_starts.push(format!("{}-01-01", birth_year[child]));
                sources.push("CENSUS".to_string());
                source_flags.push("CENSUS".to_string());
            }
        }
    }

    let n_rels = sid_orig.len();
    let record_ends: Vec<Option<String>> = vec![None; n_rels];

    list!(
        SPINE_ID_ORIGINAL = sid_orig,
        SPINE_ID_MAIN_REL = sid_rel,
        PAIRID = pairids,
        COMBINED_CATEGORY = categories,
        COMBINED_STATUS = statuses,
        RECORD_START = record_starts,
        RECORD_END = record_ends,
        SOURCES = sources,
        SOURCE_FLAG = source_flags,
    )
}

extendr_module! {
    mod core_gen;
    fn project_core_demographics__;
    fn project_core_locations__;
    fn project_core_residence_to_parquet__;
    fn project_core_relationships__;
}

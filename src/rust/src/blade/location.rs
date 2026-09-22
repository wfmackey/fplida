//! BLADE tables 24 and 25, Business Locations (Stage 4 of the R->Rust port).
//!
//! Ports `.blade_business_location_frame` from `R/generate_blade.R`. The Mesh
//! Block row for each business is picked upstream by
//! `.blade_location_lookup_rows`, which is already Rust-backed and partitions
//! the pool by state, so every SA2 code here starts with the business's own
//! state digit. This module only turns that row into the published columns.

use std::collections::HashMap;

use extendr_api::prelude::*;

use super::helpers::{deidentified_id, id_number, r_mod};
use super::rows::{
    resolved, strings_to_vec, BladeColumn, BusinessRows, DomainSet, LocationRows, VariableSpec,
};
use super::tables::{value_for, ValueContext};

/// `.blade_business_location_frame` in Rust. Returns the frame as a named list
/// of columns; R stamps it into a data.frame.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn make_blade_location_frame__(
    variable_names: Strings,
    business: List,
    lookup_mb_code: Strings,
    lookup_sa1_code: Strings,
    lookup_sa2_code: Strings,
    table_number: i32,
    seed: i32,
    available_periods: Strings,
    reference_period: &str,
    pinned_period: &str,
) -> List {
    let seed = seed as i64;
    let rows = BusinessRows::from_list(&business);
    let n = rows.n;
    let period = resolved(
        &available_periods,
        reference_period,
        pinned_period,
    );
    let mb_code = strings_to_vec(&lookup_mb_code);
    let sa1_code = strings_to_vec(&lookup_sa1_code);
    let sa2_code = strings_to_vec(&lookup_sa2_code);

    let bn = rows.chr("bn").unwrap_or_default();
    let bn_values: Vec<String> = bn.iter().map(|v| v.clone().unwrap_or_default()).collect();
    let bn_refs: Vec<&str> = bn_values.iter().map(String::as_str).collect();
    let business_key = id_number(&bn_refs);
    let seq = |i: usize| (i as f64) + 1.0;

    // Most business addresses geocode to the property; the rest resolve only to
    // the street or the locality.
    let geocode_precision: Vec<i32> = (0..business_key.len())
        .map(|i| {
            let draw = r_mod(
                business_key[i] + (seed * 37) as f64 + seq(i) * 17.0,
                100.0,
            );
            if draw < 70.0 {
                1
            } else if draw < 95.0 {
                2
            } else {
                3
            }
        })
        .collect();
    // A blank address type is the registered business address. The rest are
    // the postal and tax-agent addresses the register also holds.
    let address_type: Vec<String> = (0..business_key.len())
        .map(|i| {
            let draw = r_mod(
                business_key[i] + (seed * 19) as f64 + seq(i) * 29.0,
                100.0,
            );
            if draw >= 98.0 {
                "POSTAL, ACCOUNTANT"
            } else if draw >= 92.0 {
                "ACCOUNTANT"
            } else if draw >= 82.0 {
                "POSTAL"
            } else {
                ""
            }
            .to_string()
        })
        .collect();
    // "A" plus twenty-three digits: the twenty-four character hashed address
    // register identifier.
    let hashed_arid: Vec<String> = (0..n)
        .map(|i| {
            // R's `paste` renders NA as the two-character string "NA".
            let value = format!(
                "{}|{}",
                bn.get(i)
                    .cloned()
                    .flatten()
                    .unwrap_or_else(|| "NA".to_string()),
                mb_code
                    .get(i)
                    .cloned()
                    .flatten()
                    .unwrap_or_else(|| "NA".to_string())
            );
            deidentified_id("A", &value, (i as i64) + 1, seed, 23)
        })
        .collect();

    let location = LocationRows {
        mb_code: mb_code.clone(),
        sa1_code,
        sa2_code: sa2_code.clone(),
    };
    let domains = DomainSet {
        variable_values: Vec::new(),
        named: HashMap::new(),
    };

    let mut names: Vec<String> = Vec::with_capacity(variable_names.len());
    let mut values: Vec<Robj> = Vec::with_capacity(variable_names.len());
    for name in variable_names.iter() {
        if name.is_na() {
            continue;
        }
        let name = name.to_string();
        let l = name.to_lowercase();
        let column = match l.as_str() {
            "bn" => BladeColumn::Chr(bn.clone()),
            "hashed_arid" => BladeColumn::strs(hashed_arid.clone()),
            "locations_version" => BladeColumn::rep_str("in166_v1", n),
            "busloc_version" => BladeColumn::rep_str("in271_v1", n),
            "tsid" => BladeColumn::rep_str(&period.tsid, n),
            "quarter" => {
                BladeColumn::rep_str(&format!("{}Q4", period.financial_year_code), n)
            }
            "mesh_block_21" => BladeColumn::Chr(mb_code.clone()),
            "sa2_code_21" => BladeColumn::Chr(sa2_code.clone()),
            "geocode_precision" => BladeColumn::ints(geocode_precision.clone()),
            "address_type" => BladeColumn::strs(address_type.clone()),
            _ => {
                let spec = VariableSpec::new(&name, "", "");
                let ctx = ValueContext {
                    rows: &rows,
                    table_number,
                    seed,
                    period: &period,
                    domains: &domains,
                    // The Mesh Block rows are already in hand and are exactly
                    // what R's lazy `.blade_location_lookup_rows` would rebuild
                    // for this slice and seed.
                    location: Some(&location),
                    bas_wage_level: 1.0,
                };
                value_for(&spec, &ctx)
            }
        };
        names.push(name);
        values.push(column.into_robj());
    }
    List::from_names_and_values(names, values).expect("BLADE location frame")
}

extendr_module! {
    mod location;
    fn make_blade_location_frame__;
}

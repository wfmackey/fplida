use extendr_api::prelude::*;

/// Project COMBINED demographics (Indigenous status) from spine.
///
/// Produces 4 columns: SPINE_ID, EVER_ABORIGINAL_PERSON,
/// EVER_TSI_PERSON, EVER_INDIGENOUS_PERSON.
///
/// Indigenous coding in spine (ABS Census): 1=Non-Indigenous,
/// 2=Aboriginal, 3=Torres Strait Islander, 4=Both, 9=Not stated.
/// @export
#[extendr]
fn project_combined_indigenous__(spine_id: Strings, indigenous: &[i32]) -> List {
    let n = spine_id.len();

    let mut out_spine_id: Vec<String> = Vec::with_capacity(n);
    let mut out_ever_aboriginal: Vec<i32> = Vec::with_capacity(n);
    let mut out_ever_tsi: Vec<i32> = Vec::with_capacity(n);
    let mut out_ever_indigenous: Vec<i32> = Vec::with_capacity(n);

    for i in 0..n {
        let sid = spine_id[i].to_string();
        let ind = indigenous[i];

        // Aboriginal: codes 2 (Aboriginal) or 4 (Both)
        let is_aboriginal = if ind == 2 || ind == 4 { 1 } else { 0 };
        // TSI: codes 3 (TSI) or 4 (Both)
        let is_tsi = if ind == 3 || ind == 4 { 1 } else { 0 };
        // Any Indigenous: codes 2, 3, or 4
        let is_indigenous = if ind >= 2 && ind <= 4 { 1 } else { 0 };

        out_spine_id.push(sid);
        out_ever_aboriginal.push(is_aboriginal);
        out_ever_tsi.push(is_tsi);
        out_ever_indigenous.push(is_indigenous);
    }

    list!(
        SPINE_ID = out_spine_id,
        EVER_ABORIGINAL_PERSON = out_ever_aboriginal,
        EVER_TSI_PERSON = out_ever_tsi,
        EVER_INDIGENOUS_PERSON = out_ever_indigenous
    )
}

/// Project COMBINED indigenous directly to parquet. Single file.
/// @export
#[extendr]
fn project_combined_indigenous_to_parquet__(
    spine_id: Strings,
    indigenous: &[i32],
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = spine_id.len();

    let mut out_spine_id: Vec<String> = Vec::with_capacity(n);
    let mut out_ever_aboriginal: Vec<i32> = Vec::with_capacity(n);
    let mut out_ever_tsi: Vec<i32> = Vec::with_capacity(n);
    let mut out_ever_indigenous: Vec<i32> = Vec::with_capacity(n);

    for i in 0..n {
        let sid = spine_id[i].to_string();
        let ind = indigenous[i];
        let is_aboriginal = if ind == 2 || ind == 4 { 1 } else { 0 };
        let is_tsi = if ind == 3 || ind == 4 { 1 } else { 0 };
        let is_indigenous = if ind >= 2 && ind <= 4 { 1 } else { 0 };
        out_spine_id.push(sid);
        out_ever_aboriginal.push(is_aboriginal);
        out_ever_tsi.push(is_tsi);
        out_ever_indigenous.push(is_indigenous);
    }

    let total = out_spine_id.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SPINE_ID",
            col: Col::Str(out_spine_id),
        },
        NamedCol {
            name: "EVER_ABORIGINAL_PERSON",
            col: Col::I32(out_ever_aboriginal),
        },
        NamedCol {
            name: "EVER_TSI_PERSON",
            col: Col::I32(out_ever_tsi),
        },
        NamedCol {
            name: "EVER_INDIGENOUS_PERSON",
            col: Col::I32(out_ever_indigenous),
        },
    ];
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("combined parquet write: {}", e));
    total
}

extendr_module! {
    mod combined;
    fn project_combined_indigenous__;
    fn project_combined_indigenous_to_parquet__;
}

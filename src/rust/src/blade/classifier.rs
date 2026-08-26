//! BLADE generic variable classifier (Stage 3 of the R->Rust port).
//!
//! Ports two ordered cascades from `R/generate_blade.R`:
//!
//! * `.blade_metadata_value_for` (with `.blade_admin_character_value` and
//!   `.blade_period_value` inside it), which reads each variable's published
//!   name, Item and Valid.Response and produces the column the data item list
//!   describes; and
//! * the name-based fallthrough at the tail of `.blade_value_for`, which
//!   catches every variable the metadata could not classify.
//!
//! Both cascades are first-match-wins, so branch order is the whole contract.
//! Each branch below carries the line it came from in `R/generate_blade.R` so
//! the two can be diffed. A NULL from the metadata cascade is not terminal --
//! the caller runs the fallthrough, which always produces a column. Without
//! that second pass, unfilled character columns leak the `_[0-9]{6}$`
//! placeholder shape the test suite forbids in every BLADE product.
//!
//! Generation is RNG-free: every value is a closed-form hash of the row's `bn`,
//! the seed and a per-name salt.

use std::cell::OnceCell;
use std::collections::HashMap;

use extendr_api::prelude::*;

use super::helpers::{
    all_digits, amount, any_of, australian_port_code, bcs_code, blade_draw, character_code,
    count_value, cycle_values, digits_before_digit_word, id_number, name_salt, norm_state,
    pick_codes, pick_values, related_id, round1, round2, seq2, stable_name_seed, token,
    valid_response_codes, word,
};
use super::periods::{PeriodContext, ResolvedPeriod};

/// R `month.abb`.
const MONTH_ABB: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];

const STATE_CODES: [&str; 8] = ["NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"];

const STATE_NAMES: [&str; 8] = [
    "NEW SOUTH WALES",
    "VICTORIA",
    "QUEENSLAND",
    "SOUTH AUSTRALIA",
    "WESTERN AUSTRALIA",
    "TASMANIA",
    "NORTHERN TERRITORY",
    "AUSTRALIAN CAPITAL TERRITORY",
];

/// The trade code fields whose local domain values are fixed-width strings.
/// Keeping them character preserves the leading zeroes the code frame needs.
const LOCAL_TRADE_FIELDS: [&str; 7] = [
    "commodity_code_ex",
    "sitc_item_code_ex",
    "bec_group_code_im",
    "commodity_code_im",
    "preference_code_im",
    "sitc_item_code_im",
    "treatment_code_im",
];

const APPOINTMENT_TYPES: [&str; 12] = [
    "Controller appointed (except receiver or managing controller)",
    "Court liquidation",
    "Creditors' voluntary liquidation",
    "Deed of company arrangement",
    "Managing controller",
    "Provisional liquidation",
    "Receiver",
    "Receiver and manager",
    "Restructuring",
    "Scheme administrator appointed",
    "Simplified liquidation",
    "Voluntary administration",
];

const MKT_DESCRIPTION: [&str; 21] = [
    "Active",
    "Export sales",
    "Foreign direct investment with export outcomes",
    "Growth in annual sales",
    "Interested",
    "International agreements, tender or project bid",
    "Outwards Investment Project",
    "Winning a Contract or Tender",
    "csbusinessmatch",
    "csexportguidance",
    "csintroductions",
    "csmarketexperience",
    "csmarketresearch",
    "csmarketselect",
    "cspracticalassist",
    "csprofilesupport",
    "cstroubleshoot",
    "expenditure",
    "ftip",
    "ggtwebinar",
    "landingpad",
];

/// `.blade_survey_table_numbers`: the ABS business surveys and their
/// requestable survey-weight and commodity tables.
#[inline]
fn is_survey_table(table_number: i32) -> bool {
    (8..=23).contains(&table_number) || (38..=46).contains(&table_number)
}

// ---------------------------------------------------------------------------
// Column type
// ---------------------------------------------------------------------------

/// One column of a BLADE frame. `None` is R `NULL`: the branch declined, which
/// in the metadata cascade means "keep going" and in the fallthrough means the
/// business spine does not carry the source column.
pub enum BladeColumn {
    Int(Vec<Option<i32>>),
    Dbl(Vec<Option<f64>>),
    Chr(Vec<Option<String>>),
    /// Days since 1970-01-01, returned to R with class "Date".
    Date(Vec<Option<f64>>),
    None,
}

impl BladeColumn {
    fn ints(values: Vec<i32>) -> BladeColumn {
        BladeColumn::Int(values.into_iter().map(Some).collect())
    }
    fn dbls(values: Vec<f64>) -> BladeColumn {
        BladeColumn::Dbl(values.into_iter().map(Some).collect())
    }
    fn strs(values: Vec<String>) -> BladeColumn {
        BladeColumn::Chr(values.into_iter().map(Some).collect())
    }
    fn na_chr(n: usize) -> BladeColumn {
        BladeColumn::Chr(vec![None; n])
    }
    fn rep_str(value: &str, n: usize) -> BladeColumn {
        BladeColumn::Chr(vec![Some(value.to_string()); n])
    }
    fn rep_int(value: i32, n: usize) -> BladeColumn {
        BladeColumn::Int(vec![Some(value); n])
    }
    fn rep_date(days: f64, n: usize) -> BladeColumn {
        BladeColumn::Date(vec![Some(days); n])
    }
    fn into_robj(self) -> Robj {
        match self {
            BladeColumn::Int(v) => v.into(),
            BladeColumn::Dbl(v) => v.into(),
            BladeColumn::Chr(v) => v.into(),
            BladeColumn::Date(v) => {
                let mut r: Robj = v.into();
                r.set_class(&["Date"]).expect("Date class");
                r
            }
            BladeColumn::None => ().into(),
        }
    }
}

// ---------------------------------------------------------------------------
// Inputs
// ---------------------------------------------------------------------------

fn as_str_vec(robj: &Robj) -> Option<Vec<Option<String>>> {
    let values: Strings = robj.clone().try_into().ok()?;
    Some(
        values
            .iter()
            .map(|x| {
                if x.is_na() {
                    None
                } else {
                    Some(x.to_string())
                }
            })
            .collect(),
    )
}

fn as_f64_vec(robj: &Robj) -> Option<Vec<f64>> {
    if let Some(slice) = robj.as_real_slice() {
        return Some(slice.to_vec());
    }
    robj.as_integer_slice().map(|slice| {
        slice
            .iter()
            .map(|&v| if v == i32::MIN { f64::NAN } else { v as f64 })
            .collect()
    })
}

fn as_i32_vec(robj: &Robj) -> Option<Vec<i32>> {
    if let Some(slice) = robj.as_integer_slice() {
        return Some(slice.to_vec());
    }
    robj.as_real_slice().map(|slice| {
        slice
            .iter()
            .map(|&v| if v.is_nan() { i32::MIN } else { v as i32 })
            .collect()
    })
}

/// The slice of the business spine a table is generated from. Columns are read
/// on demand: most variables touch one or two, and eagerly copying ninety
/// columns for every variable in a sixty-two table build would cost far more
/// than it saves.
pub struct BusinessRows {
    pub n: usize,
    cols: HashMap<String, Robj>,
    /// `id_number(bn)`, the handle every draw hangs off. R recomputes it inside
    /// each `.blade_draw`; here it is computed once per variable.
    bn_num: OnceCell<Vec<f64>>,
}

impl BusinessRows {
    fn from_list(list: &List) -> BusinessRows {
        let mut cols: HashMap<String, Robj> = HashMap::new();
        let mut n = 0usize;
        for (name, value) in list.iter() {
            n = n.max(value.len());
            cols.insert(name.to_string(), value);
        }
        BusinessRows {
            n,
            cols,
            bn_num: OnceCell::new(),
        }
    }

    fn chr(&self, name: &str) -> Option<Vec<Option<String>>> {
        self.cols.get(name).and_then(as_str_vec)
    }

    fn num(&self, name: &str) -> Option<Vec<f64>> {
        self.cols.get(name).and_then(as_f64_vec)
    }

    fn int(&self, name: &str) -> Option<Vec<i32>> {
        self.cols.get(name).and_then(as_i32_vec)
    }

    fn has(&self, name: &str) -> bool {
        self.cols.contains_key(name)
    }

    fn bn_num(&self) -> &[f64] {
        self.bn_num.get_or_init(|| {
            let bn = self.chr("bn").unwrap_or_default();
            let owned: Vec<String> = bn
                .into_iter()
                .map(|v| v.unwrap_or_default())
                .collect();
            let refs: Vec<&str> = owned.iter().map(String::as_str).collect();
            id_number(&refs)
        })
    }

    fn payees(&self) -> Option<Vec<i32>> {
        self.int("d_total_payees")
    }
}

/// The Mesh Block row picked for each business, already selected by the caller.
pub struct LocationRows {
    pub mb_code: Vec<Option<String>>,
    pub sa1_code: Vec<Option<String>>,
    pub sa2_code: Vec<Option<String>>,
}

/// The published domain values a variable can draw on.
pub struct DomainSet {
    /// `.blade_domain_values(variable_name = <this variable>)`.
    pub variable_values: Vec<String>,
    /// Domain name to values, for the named domains the admin cascade uses.
    pub named: HashMap<String, Vec<String>>,
}

impl DomainSet {
    fn domain(&self, name: &str) -> &[String] {
        self.named.get(name).map(Vec::as_slice).unwrap_or(&[])
    }
}

/// Everything derived from one variable's metadata, built once per column.
pub struct VariableSpec {
    pub name: String,
    pub lower: String,
    pub item_lower: String,
    pub valid_lower: String,
    /// The Valid.Response as published. The code-frame scanner reads this
    /// rather than the lowercased copy, because lowercasing is not
    /// length-preserving for every code point.
    pub valid_raw: String,
    /// `paste(lower, item_lower, valid_lower)`.
    pub context: String,
    /// `.blade_name_salt(name)`.
    pub salt: i64,
    /// `.stable_name_seed(name)`; a different hash, used by different branches.
    pub name_seed: i64,
}

impl VariableSpec {
    pub fn new(name: &str, item: &str, valid_response: &str) -> VariableSpec {
        let lower = name.to_lowercase();
        let item_lower = item.to_lowercase();
        let valid_lower = valid_response.to_lowercase();
        VariableSpec {
            context: format!("{} {} {}", lower, item_lower, valid_lower),
            salt: name_salt(name),
            name_seed: stable_name_seed(name),
            name: name.to_string(),
            valid_raw: valid_response.to_string(),
            lower,
            item_lower,
            valid_lower,
        }
    }
}

// ---------------------------------------------------------------------------
// .blade_period_value (branch M20's body)
// ---------------------------------------------------------------------------

fn period_value(spec: &VariableSpec, n: usize, seed: i64, period: &ResolvedPeriod) -> BladeColumn {
    let l = &spec.lower;
    // "periodjandec" contains "jandec", so the two calendar flags come first.
    if l.contains("jandec") {
        // R L1900
        return BladeColumn::ints(
            (0..n)
                .map(|i| i32::from(((i as i64 + 1) + seed).rem_euclid(5) == 0))
                .collect(),
        );
    }
    if l.contains("juljun") {
        // R L1901
        return BladeColumn::ints(
            (0..n)
                .map(|i| i32::from(((i as i64 + 1) + seed).rem_euclid(5) != 0))
                .collect(),
        );
    }
    if l.contains("periodoth") || l.contains("other_period") {
        // R L1902
        return BladeColumn::ints(
            (0..n)
                .map(|i| i32::from(((i as i64 + 1) + seed).rem_euclid(17) == 0))
                .collect(),
        );
    }
    if l.contains("periodbeg") || l.contains("period_start") || l.contains("start_period") {
        // R L1905
        return BladeColumn::rep_date(period.reference_date - 364.0, n);
    }
    if l.contains("periodend") || l.contains("period_end") || l.contains("end_period") {
        // R L1908
        return BladeColumn::rep_date(period.reference_date, n);
    }
    // R L1911
    BladeColumn::rep_str(&period.latest_period, n)
}

// ---------------------------------------------------------------------------
// .blade_character_response_values
// ---------------------------------------------------------------------------

/// R `\bY\s*[-=]\s*Yes\b` (and the N/No twin), case-insensitive.
fn has_yes_no(text_lower: &str, letter: u8, spelled: &str) -> bool {
    let is_w = |c: u8| c.is_ascii_alphanumeric() || c == b'_';
    let b = text_lower.as_bytes();
    for i in 0..b.len() {
        if b[i] != letter || (i > 0 && is_w(b[i - 1])) {
            continue;
        }
        let mut j = i + 1;
        while j < b.len() && b[j].is_ascii_whitespace() {
            j += 1;
        }
        if j >= b.len() || (b[j] != b'-' && b[j] != b'=') {
            continue;
        }
        j += 1;
        while j < b.len() && b[j].is_ascii_whitespace() {
            j += 1;
        }
        if text_lower[j..].starts_with(spelled) {
            let end = j + spelled.len();
            if end == b.len() || !is_w(b[end]) {
                return true;
            }
        }
    }
    false
}

/// `.blade_character_response_values`: the Y/N frame a Valid.Response spells
/// out. An absent half of the pair becomes NA, so the column keeps a
/// not-stated share rather than forcing every row to answer.
pub fn character_response_values(valid_response: &str) -> Vec<Option<String>> {
    if valid_response.is_empty() {
        return Vec::new();
    }
    let lower = valid_response.to_lowercase();
    let has_yes = has_yes_no(&lower, b'y', "yes");
    let has_no = has_yes_no(&lower, b'n', "no");
    match (has_yes, has_no) {
        (true, true) => vec![Some("Y".to_string()), Some("N".to_string())],
        (true, false) => vec![Some("Y".to_string()), None],
        (false, true) => vec![Some("N".to_string()), None],
        (false, false) => Vec::new(),
    }
}

// ---------------------------------------------------------------------------
// .blade_admin_character_value (branch M16)
// ---------------------------------------------------------------------------

fn owned(values: &[&str]) -> Vec<String> {
    values.iter().map(|s| s.to_string()).collect()
}

fn two_digit_range(from: i32, to: i32) -> Vec<String> {
    (from..=to).map(|i| format!("{:02}", i)).collect()
}

/// Returns `None` where R returns NULL, which lets the metadata cascade carry
/// on at branch M17.
#[allow(clippy::too_many_arguments)]
fn admin_character_value(
    spec: &VariableSpec,
    rows: &BusinessRows,
    table_number: i32,
    seed: i64,
    domains: &DomainSet,
) -> Option<BladeColumn> {
    let l = spec.lower.as_str();
    let n = rows.n;
    let key = rows.bn_num();
    let salt = spec.salt;
    let pick = |values: &[String], missing_rate: f64| -> Vec<Option<String>> {
        pick_values(values, key, seed, salt, missing_rate)
    };

    let variable_values = &domains.variable_values;

    // C1 (R L1644). Fixed-width local trade codes stay character so their
    // leading zeroes survive; C2 would coerce them to integers.
    if LOCAL_TRADE_FIELDS.contains(&l) && !variable_values.is_empty() {
        return Some(BladeColumn::Chr(pick(variable_values, 0.0)));
    }
    // C2 (R L1650)
    if !variable_values.is_empty() && table_number != 6 {
        let out = pick(variable_values, 0.0);
        let numeric_frame = variable_values.iter().all(|v| all_digits(v))
            && !any_of(
                &spec.valid_lower,
                &["character", "alphanumeric", "string"],
            );
        if numeric_frame {
            return Some(BladeColumn::Int(
                out.into_iter()
                    .map(|v| v.and_then(|s| s.parse::<i32>().ok()))
                    .collect(),
            ));
        }
        return Some(BladeColumn::Chr(out));
    }
    // C3 (R L1660)
    if table_number == 26 && l == "appointment_type" {
        return Some(BladeColumn::Chr(pick(&owned(&APPOINTMENT_TYPES), 0.0)));
    }

    let patent_classes = domains.domain("iplord_technology");
    let modal_class = |prefix: &str| l.starts_with(prefix) && l.ends_with("_modal_class");
    // C4-C7 (R L1672-1688)
    if table_number == 28 && modal_class("p_") {
        return Some(BladeColumn::Chr(pick(patent_classes, 0.05)));
    }
    if table_number == 28 && modal_class("tm_") {
        return Some(BladeColumn::Chr(pick(&two_digit_range(1, 45), 0.05)));
    }
    if table_number == 28 && modal_class("d_") {
        let mut values = two_digit_range(1, 32);
        values.push("99".to_string());
        return Some(BladeColumn::Chr(pick(&values, 0.05)));
    }
    if table_number == 28 && modal_class("pbr_") {
        return Some(BladeColumn::Chr(pick(
            &owned(&[
                "Brassica", "Gossypium", "Hordeum", "Lolium", "Rosa", "Solanum", "Triticum",
                "Vitis",
            ]),
            0.08,
        )));
    }

    // R L1691. The IP right type is drawn on its own table-keyed salt, not the
    // variable's, so every IP table agrees with itself about what each business
    // holds.
    let ip_type: Option<Vec<Option<String>>> = if (29..=34).contains(&table_number) {
        Some(pick_values(
            &owned(&["design", "patent", "trade_mark"]),
            key,
            seed,
            9109 + table_number as i64,
            0.0,
        ))
    } else {
        None
    };
    let ip_is = |types: &Option<Vec<Option<String>>>, i: usize, want: &str| -> bool {
        types
            .as_ref()
            .and_then(|v| v.get(i))
            .and_then(|v| v.as_deref())
            == Some(want)
    };

    // C8 (R L1696)
    if let Some(types) = &ip_type {
        if l == "ip_right_type" {
            return Some(BladeColumn::Chr(types.clone()));
        }
    }
    // C9 (R L1697)
    if table_number == 29 && l == "auexamsection" {
        let mut values: Vec<String> = (1..=5).map(|i| format!("CHEM{}", i)).collect();
        values.extend((1..=4).map(|i| format!("ELEC{}", i)));
        values.extend((1..=5).map(|i| format!("MECH{}", i)));
        return Some(BladeColumn::Chr(pick(&values, 0.0)));
    }
    // C10 (R L1701)
    if (28..=34).contains(&table_number)
        && matches!(
            l,
            "country_code"
                | "f_country_of_earliest_filing"
                | "f_earliest_country_of_grant"
                | "classifying_country_code"
                | "linked_application_country"
        )
    {
        return Some(BladeColumn::Chr(pick(
            domains.domain("iso_country_alpha2"),
            0.0,
        )));
    }
    // C11, C12 (R L1708, L1711)
    if table_number == 29 && l == "ip_right_sub_type" {
        return Some(BladeColumn::Chr(pick(
            domains.domain("ip_right_sub_type"),
            0.03,
        )));
    }
    if table_number == 29 && l == "status" {
        return Some(BladeColumn::Chr(pick(domains.domain("ip_status"), 0.0)));
    }
    // C13 (R L1714)
    if table_number == 31 && l == "classification" {
        let patent = owned(&["A01B", "A61K", "C07D", "G06F", "H04L"]);
        let trade_mark = two_digit_range(1, 45);
        // R: sprintf("%02d-%02d", rep(1:32, each = 2), 1:2)
        let design: Vec<String> = (1..=32)
            .flat_map(|a| (1..=2).map(move |b| format!("{:02}-{:02}", a, b)))
            .collect();
        let out: Vec<Option<String>> = (0..n)
            .map(|i| {
                let values = if ip_is(&ip_type, i, "patent") {
                    &patent
                } else if ip_is(&ip_type, i, "trade_mark") {
                    &trade_mark
                } else {
                    &design
                };
                // R draws on a one-row frame, so the row index inside the draw
                // is 1 for every row.
                let d = blade_draw(&[key[i]], seed, salt, values.len() as i64)[0];
                Some(values[d as usize].clone())
            })
            .collect();
        return Some(BladeColumn::Chr(out));
    }
    // C14 (R L1725). R's `ifelse` evaluates each `pick` over the whole frame
    // and then selects element-wise, so the draws must be full-length.
    if table_number == 31 && l == "classification_area" {
        let a = pick(patent_classes, 0.0);
        let b = pick(&two_digit_range(1, 45), 0.0);
        let mut d_values = two_digit_range(1, 32);
        d_values.push("99".to_string());
        let c = pick(&d_values, 0.0);
        let out: Vec<Option<String>> = (0..n)
            .map(|i| {
                if ip_is(&ip_type, i, "patent") {
                    a.get(i).cloned().flatten()
                } else if ip_is(&ip_type, i, "trade_mark") {
                    b.get(i).cloned().flatten()
                } else {
                    c.get(i).cloned().flatten()
                }
            })
            .collect();
        return Some(BladeColumn::Chr(out));
    }
    // C15-C17 (R L1730-1736)
    if table_number == 31 && l == "classification_importance" {
        return Some(BladeColumn::Chr(pick(&owned(&["primary", "secondary"]), 0.0)));
    }
    if table_number == 31 && l == "classification_inventiveness" {
        return Some(BladeColumn::Chr(pick(
            &owned(&["inventive", "non-inventive", "additional", "unknown"]),
            0.0,
        )));
    }
    if table_number == 31 && l == "classification_source" {
        return Some(BladeColumn::Chr(pick(
            &owned(&["human", "machine", "unknown"]),
            0.0,
        )));
    }
    // C18 (R L1739)
    if table_number == 31 && l == "classification_system" {
        let draw = blade_draw(key, seed, salt, 2);
        let out: Vec<Option<String>> = (0..n)
            .map(|i| {
                let value = if ip_is(&ip_type, i, "patent") {
                    if draw[i] == 0 {
                        "cpc_mark"
                    } else {
                        "ipc_mark"
                    }
                } else if ip_is(&ip_type, i, "trade_mark") {
                    "nice"
                } else {
                    "locarno"
                };
                Some(value.to_string())
            })
            .collect();
        return Some(BladeColumn::Chr(out));
    }
    // C19 (R L1744)
    if table_number == 31 && l == "coarse_classification_area" {
        let a = pick(
            &owned(&[
                "chemistry",
                "electrical_engineering",
                "instruments",
                "mechanical_engineering",
                "other_fields",
            ]),
            0.0,
        );
        let b = pick(&owned(&["goods", "services", "goods_and_services"]), 0.0);
        let out: Vec<Option<String>> = (0..n)
            .map(|i| {
                if ip_is(&ip_type, i, "patent") {
                    a.get(i).cloned().flatten()
                } else if ip_is(&ip_type, i, "trade_mark") {
                    b.get(i).cloned().flatten()
                } else {
                    Some("other_fields".to_string())
                }
            })
            .collect();
        return Some(BladeColumn::Chr(out));
    }
    // C20-C22 (R L1753-1759)
    if table_number == 32 && l == "event_category" {
        return Some(BladeColumn::Chr(pick(
            domains.domain("ip_event_category"),
            0.0,
        )));
    }
    if table_number == 32 && l == "event_type" {
        return Some(BladeColumn::Chr(pick(domains.domain("ip_event_type"), 0.0)));
    }
    if table_number == 33 && l == "link_type" {
        return Some(BladeColumn::Chr(pick(domains.domain("ip_link_type"), 0.0)));
    }
    // C23-C27 (R L1763-1775)
    if matches!(l, "country_of_final_dest_ex" | "country_of_origin_im") {
        return Some(BladeColumn::Chr(pick(
            domains.domain("trade_country_code"),
            0.0,
        )));
    }
    if matches!(l, "port_of_discharge_ex" | "port_of_loading_im") {
        return Some(BladeColumn::Chr(pick(
            domains.domain("trade_foreign_port"),
            0.0,
        )));
    }
    if l == "port_of_loading_ex" {
        return Some(BladeColumn::Chr(pick(
            domains.domain("trade_australian_port"),
            0.0,
        )));
    }
    if matches!(l, "invoice_currency_ex" | "invoice_currency_im") {
        return Some(BladeColumn::Chr(pick(domains.domain("trade_currency"), 0.0)));
    }
    if matches!(l, "unit_of_quantity_ex" | "unit_of_quantity_im") {
        return Some(BladeColumn::Chr(pick(domains.domain("trade_unit"), 0.0)));
    }
    // C28-C31 (R L1778-1791)
    if table_number == 48 && l == "form_id_im" {
        return Some(BladeColumn::Chr(pick(
            &owned(&["Nature 10", "Nature 20", "Nature 30"]),
            0.0,
        )));
    }
    if table_number == 35 && l == "entity_type" {
        return Some(BladeColumn::Chr(pick(&owned(&["C", "I", "S", "T"]), 0.0)));
    }
    if table_number == 52 && matches!(l, "australian_hq" | "diversified_corporate") {
        return Some(BladeColumn::Chr(pick(&owned(&["YES"]), 0.45)));
    }
    if table_number == 52 && l == "theme" {
        return Some(BladeColumn::rep_str("Agricultural and food biosensors", n));
    }
    // C32-C38 (R L1791-1814)
    if table_number == 53 && l == "rdentityasicregistrationtype" {
        let out = pick(&owned(&["1", "2", "3"]), 0.0);
        return Some(BladeColumn::Int(
            out.into_iter()
                .map(|v| v.and_then(|s| s.parse::<i32>().ok()))
                .collect(),
        ));
    }
    if table_number == 53
        && matches!(
            l,
            "rdti_incorporationcountry"
                | "rdti_residencecountryname"
                | "rdti_uhcincorporationcountry"
        )
    {
        return Some(BladeColumn::Chr(pick(&owned(&["AUSTRALIA", "FOREIGN"]), 0.0)));
    }
    if table_number == 53
        && matches!(
            l,
            "rdti_companyisheadofconsolidated" | "companyiscontrolledbytaxexempt"
        )
    {
        return Some(BladeColumn::Chr(pick(&owned(&["Yes", "No", "N/A"]), 0.0)));
    }
    if table_number == 53 && matches!(l, "indigenousowned" | "indigenouscontrolled") {
        return Some(BladeColumn::Chr(pick(
            &owned(&["Yes", "No", "N/A", "Prefer not to answer"]),
            0.0,
        )));
    }
    if table_number == 53 && l == "activitiesexcludedfrombeingcore" {
        return Some(BladeColumn::Chr(pick(&owned(&["1", "0", "N/A"]), 0.0)));
    }
    if table_number == 53 && l == "rdti_advancedoroverseasfinding" {
        return Some(BladeColumn::Chr(pick(&owned(&["1", "0", "UNKNOWN"]), 0.0)));
    }
    if table_number == 53 && l == "rdti_anzsrccode" {
        return Some(BladeColumn::Chr(pick(
            &owned(&[
                "0101", "0301", "0502", "0806", "1005", "1103", "1502", "1701", "2002", "2101",
                "3004", "3202",
            ]),
            0.0,
        )));
    }
    // C39-C43 (R L1818-1841)
    if table_number == 59 && l == "client_focus" {
        return Some(BladeColumn::Chr(pick(
            &owned(&["business", "consumer", "both"]),
            0.0,
        )));
    }
    if table_number == 61 && l == "segment" {
        return Some(BladeColumn::Chr(pick(
            &owned(&[
                "Born Global",
                "Expanding Exporter",
                "Global Leader",
                "Novice Exporter",
                "Stable Exporter",
            ]),
            0.0,
        )));
    }
    if table_number == 62 && l == "mktdescription" {
        return Some(BladeColumn::Chr(pick(&owned(&MKT_DESCRIPTION), 0.0)));
    }
    if table_number == 62 && l == "mktevent" {
        return Some(BladeColumn::Chr(pick(
            &owned(&[
                "Client Services",
                "EMDG",
                "Excelerate",
                "Go Global Toolkit",
                "Tailored Services",
                "Trade Outcome",
            ]),
            0.0,
        )));
    }
    if table_number == 62 && l == "mktselected" {
        return Some(BladeColumn::Chr(pick(domains.domain("max_market"), 0.0)));
    }
    // C44 (R L1844)
    None
}

// ---------------------------------------------------------------------------
// .blade_metadata_value_for
// ---------------------------------------------------------------------------

/// The ordered metadata cascade. `BladeColumn::None` means R NULL: the caller
/// must run `classify_fallthrough` next.
#[allow(clippy::too_many_arguments)]
pub fn classify_metadata(
    spec: &VariableSpec,
    rows: &BusinessRows,
    table_number: i32,
    seed: i64,
    period: &ResolvedPeriod,
    domains: &DomainSet,
    location: Option<&LocationRows>,
) -> BladeColumn {
    let l = spec.lower.as_str();
    let vl = spec.valid_lower.as_str();
    let ctx = spec.context.as_str();
    let n = rows.n;
    let salt = spec.salt;
    let key = rows.bn_num();
    // R computes `(seq + seed + salt) %% m` in doubles throughout this cascade.
    let row_hash = |i: usize, m: f64| -> f64 {
        super::helpers::r_mod((i as f64) + 1.0 + seed as f64 + salt as f64, m)
    };

    // M1 (R L1933)
    if l == "periodc" {
        return BladeColumn::strs(
            (0..n)
                .map(|i| {
                    let draw = ((i as i64) + 1 + seed).rem_euclid(85);
                    if draw == 0 {
                        "Other".to_string()
                    } else if draw % 5 == 0 {
                        "Jan-Dec".to_string()
                    } else {
                        "Jul-Jun".to_string()
                    }
                })
                .collect(),
        );
    }
    // M2 (R L1938)
    if l == "digtecoth_s" {
        return BladeColumn::Chr(cycle_values(
            &some_all(&[
                "Cloud computing",
                "Cyber security",
                "Data analytics",
                "Online collaboration",
            ]),
            n,
            seed,
            salt,
            Some(key),
        ));
    }
    // M3 (R L1945)
    if l == "sklegal_im" {
        return BladeColumn::ints(pick_codes(
            &[0, 1, 88_888_888, 999_999_999],
            n,
            seed,
            salt,
            true,
            Some(key),
        ));
    }
    // M4 (R L1950)
    if l == "nature_of_tariff_code_im" {
        let draw = blade_draw(key, seed, salt, 100);
        return BladeColumn::strs(
            draw.iter()
                .map(|&d| {
                    if d < 78 {
                        "N"
                    } else if d < 90 {
                        "R"
                    } else if d < 95 {
                        "G"
                    } else if d < 99 {
                        "Q"
                    } else {
                        "C"
                    }
                    .to_string()
                })
                .collect(),
        );
    }
    // M5 (R L1957)
    if l == "port_of_discharge_im" {
        let state = match rows.int("state") {
            Some(s) => s,
            None => return BladeColumn::Chr(Vec::new()),
        };
        return BladeColumn::strs(australian_port_code(&state, seed, salt));
    }
    // M6 (R L1960)
    if l == "typeofvariation" {
        return BladeColumn::Chr(cycle_values(
            &some_all(&["wages", "conditions", "wages/conditions"]),
            n,
            seed,
            salt,
            Some(key),
        ));
    }
    // M7 (R L1966)
    if l == "type" && table_number == 52 {
        return BladeColumn::rep_str("ADOPTER", n);
    }
    // M8 (R L1969)
    if l == "companyhasultimateholdingcompany" {
        let draw = blade_draw(key, seed, salt, 100);
        return BladeColumn::strs(
            draw.iter()
                .map(|&d| {
                    if d < 35 {
                        "Yes"
                    } else if d < 92 {
                        "No"
                    } else {
                        "N/A"
                    }
                    .to_string()
                })
                .collect(),
        );
    }
    // M9 (R L1973)
    if l == "applicationforheadorsubsidiary" {
        return BladeColumn::Chr(cycle_values(
            &some_all(&[
                "The head company",
                "Subsidiary members",
                "The head company and Subsidiary members",
            ]),
            n,
            seed,
            salt,
            Some(key),
        ));
    }
    // M10 (R L1980)
    if l == "impute_capex" {
        return BladeColumn::Chr(cycle_values(
            &some_all(&["Reported", "Impute partial", "Impute full", "Winsorised"]),
            n,
            seed,
            salt,
            Some(key),
        ));
    }
    // M11 (R L1986)
    if l == "d_ag_anzsic06_group" {
        let anzsic = match rows.chr("anzsic06") {
            Some(v) => v,
            None => return BladeColumn::Int(Vec::new()),
        };
        return BladeColumn::Int(
            anzsic
                .iter()
                .map(|v| {
                    v.as_deref()
                        .map(|s| s.chars().take(3).collect::<String>())
                        .and_then(|s| s.parse::<i32>().ok())
                })
                .collect(),
        );
    }
    // M12, M13 (R L1990, L1991)
    if l == "cast_anzsic93" || matches!(l, "cast_sisca06" | "cast_sisca93") {
        return BladeColumn::rep_int(9999, n);
    }
    // M14 (R L1994)
    if l == "cii_group" {
        return BladeColumn::Chr(cycle_values(
            &some_all(&["CII-Likely-MoM", "CII-Unlikely-MoM", "Non-CII"]),
            n,
            seed,
            salt,
            Some(key),
        ));
    }
    // M15 (R L2000). The NA is a documented outcome, so `cycle_values` -- which
    // drops nothing -- is the right helper here, not `pick_values`.
    if l == "sector_group" {
        return BladeColumn::Chr(cycle_values(
            &[Some("Consumer Goods and Services".to_string()), None],
            n,
            seed,
            salt,
            Some(key),
        ));
    }
    // M16 (R L2006)
    if let Some(column) = admin_character_value(spec, rows, table_number, seed, domains) {
        return column;
    }
    // M17 (R L2011)
    if l == "previous_id_capex" {
        return BladeColumn::strs(related_id("", key, seed, salt, 10));
    }
    // M18 (R L2014)
    if token(l, "date")
        || l.ends_with("_dt")
        || token(l, "start")
        || token(l, "end")
        || token(l, "birth")
        || l.contains("commenc")
    {
        return BladeColumn::Date(
            (0..n)
                .map(|i| Some(period.reference_date - row_hash(i, 365.0)))
                .collect(),
        );
    }
    // M19 (R L2020)
    if l == "year"
        || matches!(l, "financial_year" | "z_year")
        || l.ends_with("_fy")
        || l.ends_with("_yr_cd")
        || l.ends_with("_year")
    {
        let year = period.end_year;
        let ages_back = any_of(
            l,
            &[
                "first",
                "application",
                "gained",
                "round",
                "launch",
                "seed",
                "valuation",
                "sim_",
            ],
        );
        return BladeColumn::Int(
            (0..n)
                .map(|i| {
                    year.map(|y| {
                        if ages_back {
                            (y - row_hash(i, 20.0) as i32).max(1900)
                        } else {
                            y
                        }
                    })
                })
                .collect(),
        );
    }
    // M20 (R L2031)
    if any_of(
        l,
        &[
            "quarter",
            "periodbeg",
            "periodend",
            "periodjuljun",
            "periodjandec",
        ],
    ) {
        return period_value(spec, n, seed, period);
    }

    // R L2036
    let state: Vec<i32> = match rows.int("state") {
        Some(s) => s.iter().map(|v| norm_state(*v)).collect(),
        None => Vec::new(),
    };
    // M21-M23 (R L2037-2043)
    if matches!(l, "x_state" | "cast_state" | "d_ag_state" | "z_state") {
        if !rows.has("state") {
            return BladeColumn::Int(Vec::new());
        }
        return BladeColumn::ints(state);
    }
    if l == "state_code" {
        if !rows.has("state") {
            return BladeColumn::Chr(Vec::new());
        }
        return BladeColumn::strs(
            state
                .iter()
                .map(|s| STATE_CODES[(*s - 1) as usize].to_string())
                .collect(),
        );
    }
    if l.starts_with("state_of_") {
        if !rows.has("state") {
            return BladeColumn::Chr(Vec::new());
        }
        return BladeColumn::strs(
            state
                .iter()
                .map(|s| STATE_NAMES[(*s - 1) as usize].to_string())
                .collect(),
        );
    }

    // R L2051. The code frame in Valid.Response is authoritative wherever the
    // data item list spells one out.
    let codes = valid_response_codes(&spec.valid_raw);
    let sentinel_measure = vl.starts_with("numeric")
        && !codes.is_empty()
        && codes.iter().all(|c| {
            matches!(
                c,
                9999 | 111_111_111 | 222_222_222 | 7_777_777 | 88_888_888 | 999_999_999
            )
        });
    let example_only = codes.len() == 1 && any_of(vl, &["example", "e.g."]);
    let documented_single_code = codes.len() == 1 && seq2(vl, "everything else", "invalid");
    // M24 (R L2059). `sentinel_measure` deliberately does not gate the
    // two-or-more arm: a measure that documents only its missing codes still
    // has a real frame.
    if codes.len() >= 2 || (documented_single_code && !sentinel_measure && !example_only) {
        return BladeColumn::ints(pick_codes(
            &codes,
            n,
            seed,
            salt,
            any_of(vl, &["missing", "sequencing"]),
            Some(key),
        ));
    }
    // M25 (R L2068)
    if any_of(vl, &["date format", "dd/mm/yyyy"]) {
        return BladeColumn::Date(
            (0..n)
                .map(|i| Some(period.reference_date - row_hash(i, 365.0)))
                .collect(),
        );
    }
    // M26 (R L2073)
    if l.contains("month") || word(vl, "mmm") {
        return BladeColumn::strs(
            (0..n)
                .map(|i| MONTH_ABB[row_hash(i, 12.0) as usize].to_string())
                .collect(),
        );
    }
    // M27 (R L2078)
    if any_of(ctx, &["anzsic", "business industry codes"]) {
        return match rows.chr("anzsic06") {
            Some(v) => BladeColumn::Chr(v),
            None => BladeColumn::None,
        };
    }
    // M28 (R L2081)
    if ctx.contains("anzsco") {
        let width = if any_of(ctx, &["1 - digit", "1 digit"]) {
            1
        } else if any_of(ctx, &["2 - digit", "2 digit"]) {
            2
        } else if any_of(ctx, &["3 - digit", "3 digit"]) {
            3
        } else if any_of(ctx, &["4 - digit", "4 digit"]) {
            4
        } else {
            6
        };
        return match rows.chr("representative_anzsco_code") {
            Some(v) => BladeColumn::strs(
                v.iter()
                    .map(|c| super::helpers::anzsco_group(c.as_deref(), width))
                    .collect(),
            ),
            None => BladeColumn::Chr(Vec::new()),
        };
    }
    // M29 (R L2089)
    if any_of(ctx, &["asgs", "sa1", "sa2", "mesh block"]) {
        // G1 (R L2093)
        if l.contains("geocode") {
            return BladeColumn::ints(
                (0..n).map(|i| row_hash(i, 3.0) as i32 + 1).collect(),
            );
        }
        let loc = match location {
            Some(loc) => loc,
            // An empty business slice has no Mesh Blocks to carry, which is
            // not the caller forgetting to supply them.
            None if n == 0 => return BladeColumn::Chr(Vec::new()),
            None => throw_r_error(
                "BLADE geography variable reached the Rust classifier without \
                 Mesh Block rows.",
            ),
        };
        // G2 (R L2097)
        if seq2(ctx, "sa2", "name") || ctx.contains("full name of sa2") {
            return BladeColumn::Chr(
                loc.sa2_code
                    .iter()
                    .map(|c| Some(format!("SA2 {}", c.clone().unwrap_or_else(|| "NA".into()))))
                    .collect(),
            );
        }
        // G3-G5 (R L2100-2106)
        if l.contains("sa1") || ctx.contains("statistical area 1") {
            return BladeColumn::Chr(loc.sa1_code.clone());
        }
        if l.contains("sa2") || any_of(ctx, &["statistical area 2", "maincode of sa2"]) {
            return BladeColumn::Chr(loc.sa2_code.clone());
        }
        return BladeColumn::Chr(loc.mb_code.clone());
    }
    // M30 (R L2108)
    if ctx.contains("postcode") {
        return match rows.chr("x_pcode") {
            Some(v) => BladeColumn::Chr(v),
            None => BladeColumn::None,
        };
    }

    // M31 (R L2110)
    if any_of(
        vl,
        &[
            "numeric",
            "number",
            "$",
            "%000",
            "%'000",
            "(t)",
            "tonne",
            "kilogram",
            "kg",
            "million",
            "percentage",
            "%",
        ],
    ) || word(vl, "ha")
    {
        let count_like = any_of(
            l,
            &[
                "person",
                "employee",
                "job",
                "manager",
                "director",
                "vacanc",
                "location",
                "number",
                "count",
                "cnt",
                "headcount",
                "fte",
                "sequence",
                "duration",
                "days",
                "filter",
            ],
        ) || token(l, "n")
            || any_of(
                &spec.item_lower,
                &["total number", "number of", "sequence number", "count of"],
            );
        let amount_like = any_of(
            l,
            &[
                "income",
                "expenditure",
                "amount",
                "amt",
                "sales",
                "cost",
                "wage",
                "salary",
                "capital",
                "value",
                "val",
                "revenue",
                "tax",
                "fee",
            ],
        ) || any_of(vl, &["$", "million", "%000", "%'000", "monetary"]);
        // N1
        if count_like && !amount_like {
            return BladeColumn::ints(count_value(
                l,
                rows.payees().as_deref(),
                key,
                seed,
                salt,
            ));
        }
        let scale = if vl.contains("million") {
            1.0 / 1_000_000.0
        } else if vl.contains("000") {
            1.0 / 1_000.0
        } else {
            1.0
        };
        // N2
        if any_of(vl, &["percentage", "%"]) {
            return BladeColumn::dbls(
                blade_draw(key, seed, salt, 1000)
                    .iter()
                    .map(|&d| round1(d as f64 / 10.0))
                    .collect(),
            );
        }
        let turnover = rows.num("turnover");
        let wages = rows.num("annual_wages");
        // N3
        if word(vl, "ha") || vl.contains("hectare") {
            let turnover = match turnover {
                Some(t) => t,
                None => return BladeColumn::Dbl(Vec::new()),
            };
            let draw = blade_draw(key, seed, salt, 200);
            return BladeColumn::dbls(
                (0..n)
                    .map(|i| {
                        round2((turnover[i] / 100_000.0).max(1.0) * (0.5 + draw[i] as f64 / 100.0))
                    })
                    .collect(),
            );
        }
        // N4
        if any_of(vl, &["(t)", "tonne", "kilogram", "kg"]) {
            let turnover = match turnover {
                Some(t) => t,
                None => return BladeColumn::Dbl(Vec::new()),
            };
            let draw = blade_draw(key, seed, salt, 300);
            return BladeColumn::dbls(
                (0..n)
                    .map(|i| {
                        round2((turnover[i] / 50_000.0).max(1.0) * (1.0 + draw[i] as f64 / 100.0))
                    })
                    .collect(),
            );
        }
        // N5
        return match (turnover, wages) {
            (Some(t), Some(w)) => BladeColumn::dbls(amount(key, &t, &w, seed, salt, scale)),
            _ => BladeColumn::Dbl(Vec::new()),
        };
    }

    // M32 (R L2142)
    if any_of(vl, &["alphanumeric", "character", "categorical", "text"]) {
        // A1-A4
        if seq2(ctx, "10 digit", "abn") || ctx.contains("deidentified abn") {
            return column_or_none(rows.chr("bn"));
        }
        if any_of(ctx, &["unit_id", "deidentified unit"]) {
            return column_or_none(rows.chr("id"));
        }
        // R's "can" alternative is this loose on purpose: it also matches
        // "significant". Ported literally.
        if any_of(ctx, &["acn", "can"]) {
            return column_or_none(rows.chr("cn"));
        }
        if ctx.contains("sbid") {
            return column_or_none(rows.chr("fn"));
        }
        // A5
        if !is_survey_table(table_number) {
            return BladeColumn::na_chr(n);
        }
        // A6. R's `sub()` leaves the string alone when no "<n> digit" run is
        // there, and `as.integer()` then reads whatever the whole string is.
        let stated = digits_before_digit_word(vl)
            .or_else(|| vl.trim().parse::<f64>().ok().map(|v| v as i32));
        let width = match stated {
            Some(w) if (1..=12).contains(&w) => w as usize,
            _ => 3,
        };
        return BladeColumn::strs(character_code(n, seed, salt, width, "C"));
    }

    // M33 (R L2156)
    BladeColumn::None
}

// R reads a business column one of two ways, and the two behave differently
// when the column is not there. A bare `business_rows$col` is NULL, and the
// frame silently loses that variable. Anything wrapped in `as.integer()`,
// `round()` or `sprintf()` is a ZERO-LENGTH vector instead, and `as.data.frame`
// then refuses to build the frame at all. The second is the better failure --
// a missing business column should stop the build, not quietly drop a
// published variable -- so both are reproduced exactly where R has them.
fn column_or_none(values: Option<Vec<Option<String>>>) -> BladeColumn {
    match values {
        Some(v) => BladeColumn::Chr(v),
        None => BladeColumn::None,
    }
}

fn some_all(values: &[&str]) -> Vec<Option<String>> {
    values.iter().map(|s| Some(s.to_string())).collect()
}

// ---------------------------------------------------------------------------
// .blade_value_for's name-based fallthrough (R L2664-2780)
// ---------------------------------------------------------------------------

/// The last cascade. Unlike the metadata one this always produces a column,
/// except where the business spine simply does not carry the source column.
pub fn classify_fallthrough(
    spec: &VariableSpec,
    rows: &BusinessRows,
    table_number: i32,
    seed: i64,
    period: &ResolvedPeriod,
    location: Option<&LocationRows>,
) -> BladeColumn {
    let l = spec.lower.as_str();
    let n = rows.n;
    let key = rows.bn_num();
    let salt = spec.salt;

    // F1 (R L2664). No salt here, unlike M26.
    if l.contains("month") {
        return BladeColumn::strs(
            (0..n)
                .map(|i| {
                    MONTH_ABB[(((i as i64) + 1 + seed).rem_euclid(12)) as usize].to_string()
                })
                .collect(),
        );
    }
    // F2 (R L2668)
    if seq2(l, "financial", "year")
        || seq2(l, "income", "year")
        || l == "year"
        || l.ends_with("_yr")
        || l.contains("yr_")
    {
        return BladeColumn::Int(vec![period.end_year; n]);
    }
    // F3 (R L2671). Plain substrings here, unlike M18's underscore tokens.
    if any_of(l, &["date", "commenc", "start", "end", "birth"]) || l.ends_with("_dt") {
        return BladeColumn::Date(
            (0..n)
                .map(|i| {
                    let shift =
                        ((i as i64) + 1 + seed + table_number as i64).rem_euclid(365) as f64;
                    Some(period.reference_date - shift)
                })
                .collect(),
        );
    }
    // F4 (R L2675). The raw state, not the normalised one.
    if any_of(l, &["state", "ste", "main_state"]) {
        return match rows.int("state") {
            Some(s) => BladeColumn::Int(
                s.iter()
                    .map(|v| if *v == i32::MIN { None } else { Some(*v) })
                    .collect(),
            ),
            None => BladeColumn::Int(Vec::new()),
        };
    }
    // F5 (R L2678)
    if any_of(l, &["postcode", "post_code"]) {
        return match rows.int("state") {
            Some(s) => BladeColumn::Chr(
                (0..n)
                    .map(|i| {
                        if s[i] == i32::MIN {
                            None
                        } else {
                            let v = 2000
                                + ((s[i] as i64) * 173 + (i as i64) + 1).rem_euclid(7000);
                            Some(format!("{:04}", v))
                        }
                    })
                    .collect(),
            ),
            None => BladeColumn::Chr(Vec::new()),
        };
    }
    // F6-F8 (R L2683-2695)
    if l.contains("sa2") || l.contains("sa1") || l.contains("mesh") || l.contains("mb_") {
        let loc = match location {
            Some(loc) => loc,
            None if n == 0 => return BladeColumn::Chr(Vec::new()),
            None => throw_r_error(
                "BLADE geography variable reached the Rust fallthrough without \
                 Mesh Block rows.",
            ),
        };
        if l.contains("sa2") {
            return BladeColumn::Chr(loc.sa2_code.clone());
        }
        if l.contains("sa1") {
            return BladeColumn::Chr(loc.sa1_code.clone());
        }
        return BladeColumn::Chr(loc.mb_code.clone());
    }
    // F9, F10 (R L2701, L2704)
    if any_of(l, &["anzsic", "industry", "indus"]) {
        return column_or_none(rows.chr("anzsic06"));
    }
    if l.contains("sisca") {
        return column_or_none(rows.chr("x_sisca08"));
    }
    // F11 (R L2707)
    if l.contains("anzsco") {
        let code = match rows.chr("representative_anzsco_code") {
            Some(v) => v,
            // R reads the column two ways in this one branch: the digit-width
            // arms hand it to `.blade_anzsco_group`, which gives a zero-length
            // vector, and the last arm returns the bare column, which is NULL.
            None => {
                return if any_of(l, &["_1_", "_2_", "_3_", "_4_"]) {
                    BladeColumn::Chr(Vec::new())
                } else {
                    BladeColumn::None
                }
            }
        };
        let width = if l.contains("_1_") {
            Some(1)
        } else if l.contains("_2_") {
            Some(2)
        } else if l.contains("_3_") {
            Some(3)
        } else if l.contains("_4_") {
            Some(4)
        } else {
            None
        };
        return match width {
            Some(w) => BladeColumn::strs(
                code.iter()
                    .map(|c| super::helpers::anzsco_group(c.as_deref(), w))
                    .collect(),
            ),
            None => BladeColumn::Chr(code),
        };
    }
    // F12, F13 (R L2722, L2725)
    if any_of(l, &["d_div", "division"]) {
        return column_or_none(rows.chr("industry_division"));
    }
    if any_of(
        l,
        &[
            "employee",
            "employment",
            "employ",
            "headcount",
            "hdcnt",
            "fte",
            "jobs",
            "vacanc",
        ],
    ) {
        return match rows.int("employment_count") {
            Some(v) => BladeColumn::Int(
                v.iter()
                    .map(|x| if *x == i32::MIN { None } else { Some(*x) })
                    .collect(),
            ),
            None => BladeColumn::Int(Vec::new()),
        };
    }
    // F14 (R L2729). "ha" is a plain substring here, not a word.
    if any_of(
        l,
        &[
            "count",
            "cnt",
            "num",
            "number",
            "qty",
            "quantity",
            "volume",
            "weight",
            "tonne",
            "kg",
            "ha",
            "hectare",
            "area",
            "locs",
            "persons",
            "positions",
            "managers",
        ],
    ) {
        return BladeColumn::ints(count_value(l, rows.payees().as_deref(), key, seed, salt));
    }
    // F15 (R L2734)
    if any_of(
        l,
        &["wage", "salary", "payg", "payroll", "pyrl", "gross", "grs", "super"],
    ) {
        return match rows.num("annual_wages") {
            Some(w) => BladeColumn::dbls(w.iter().map(|x| round2(*x)).collect()),
            None => BladeColumn::Dbl(Vec::new()),
        };
    }
    // F16 (R L2737)
    if any_of(
        l,
        &[
            "sales", "turnover", "income", "revenue", "amount", "amt", "expense", "exp", "value",
            "val", "cost", "tax", "gst", "bas", "bit", "assets", "liab", "capital", "fob",
            "import", "export", "round",
        ],
    ) {
        return match (rows.num("turnover"), rows.num("annual_wages")) {
            (Some(t), Some(w)) => BladeColumn::dbls(amount(key, &t, &w, seed, salt, 1.0)),
            _ => BladeColumn::Dbl(Vec::new()),
        };
    }
    // F17 (R L2741)
    if any_of(
        l,
        &["flag", "indicator", "binary", "dummy", "active", "alive", "status"],
    ) || l.ends_with("ind")
    {
        return BladeColumn::ints(blade_draw(
            key,
            seed,
            salt + table_number as i64 * 1009,
            2,
        ));
    }
    // F18 (R L2748)
    if any_of(l, &["tolo", "legal", "type", "category", "class", "role"]) {
        if !is_survey_table(table_number) {
            return BladeColumn::na_chr(n);
        }
        return BladeColumn::strs(
            (0..n)
                .map(|i| format!("C{:02}", ((i as i64 + 1 + seed).rem_euclid(10)) + 1))
                .collect(),
        );
    }
    // F19 (R L2752)
    if any_of(
        l,
        &[
            "agreement",
            "project",
            "patent",
            "application",
            "party",
            "round",
            "transaction",
            "identifier",
            "number",
            "key",
        ],
    ) || l.ends_with("_id")
        || l.starts_with("s_")
    {
        return BladeColumn::strs(related_id("K", key, seed, salt, 9));
    }
    // F20 (R L2757)
    if any_of(l, &["code", "currency", "country"]) || l.ends_with("_cd") {
        if !is_survey_table(table_number) {
            return BladeColumn::na_chr(n);
        }
        return BladeColumn::strs(
            (0..n)
                .map(|i| format!("C{:03}", ((i as i64 + 1 + seed).rem_euclid(200)) + 1))
                .collect(),
        );
    }

    // F21 (R L2766). Tested before F22, and the survey set overlaps the
    // numeric set at tables 12-15, 18, 23 and 44-46; those reach the survey
    // code frame, never the dollar path.
    if is_survey_table(table_number) {
        return BladeColumn::ints(bcs_code(
            n,
            seed + spec.name_seed,
            l.starts_with("d_") || any_of(l, &["new", "innov", "gs", "change"]),
        ));
    }
    // F22 (R L2771)
    const NUMERIC_TABLES: [i32; 19] = [
        12, 13, 14, 15, 18, 23, 35, 36, 37, 44, 45, 46, 47, 48, 53, 57, 58, 61, 62,
    ];
    if NUMERIC_TABLES.contains(&table_number) {
        return match (rows.num("turnover"), rows.num("annual_wages")) {
            (Some(t), Some(w)) => BladeColumn::dbls(amount(key, &t, &w, seed, salt, 0.08)),
            _ => BladeColumn::Dbl(Vec::new()),
        };
    }
    // F23 (R L2775)
    const CHAR_TABLES: [i32; 13] = [28, 29, 30, 31, 32, 33, 34, 49, 50, 51, 54, 55, 56];
    if CHAR_TABLES.contains(&table_number) {
        return BladeColumn::strs(character_code(n, seed, salt, 6, "K"));
    }
    // F24 (R L2780)
    BladeColumn::na_chr(n)
}

// ---------------------------------------------------------------------------
// extendr entry points
// ---------------------------------------------------------------------------

fn strings_to_vec(values: &Strings) -> Vec<Option<String>> {
    values
        .iter()
        .map(|x| {
            if x.is_na() {
                None
            } else {
                Some(x.to_string())
            }
        })
        .collect()
}

fn strings_to_owned(values: &Strings) -> Vec<String> {
    values
        .iter()
        .filter(|x| !x.is_na())
        .map(|x| x.to_string())
        .collect()
}

fn location_from(mb: &Strings, sa1: &Strings, sa2: &Strings) -> Option<LocationRows> {
    if mb.is_empty() && sa1.is_empty() && sa2.is_empty() {
        return None;
    }
    Some(LocationRows {
        mb_code: strings_to_vec(mb),
        sa1_code: strings_to_vec(sa1),
        sa2_code: strings_to_vec(sa2),
    })
}

fn domain_set(variable_values: &Strings, domains: &List) -> DomainSet {
    let mut named: HashMap<String, Vec<String>> = HashMap::new();
    for (name, value) in domains.iter() {
        if let Ok(values) = Strings::try_from(value) {
            named.insert(name.to_string(), strings_to_owned(&values));
        }
    }
    DomainSet {
        variable_values: strings_to_owned(variable_values),
        named,
    }
}

fn resolved(
    available_periods: &Strings,
    reference_period: &str,
    t1_available_periods: &Strings,
    t1_reference_period: &str,
    table_number: i32,
) -> ResolvedPeriod {
    let ctx = PeriodContext {
        available_periods: strings_to_vec(available_periods),
        reference_period: reference_period.to_string(),
        t1_available_periods: strings_to_vec(t1_available_periods),
        t1_reference_period: t1_reference_period.to_string(),
    };
    ResolvedPeriod::resolve(&ctx, table_number)
}

/// `.blade_metadata_value_for` in Rust. Returns R NULL when no branch claims
/// the variable, which tells the caller to run the fallthrough.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn blade_metadata_value_for__(
    name: &str,
    table_number: i32,
    seed: i32,
    item: &str,
    valid_response: &str,
    business: List,
    location_mb: Strings,
    location_sa1: Strings,
    location_sa2: Strings,
    variable_values: Strings,
    domains: List,
    available_periods: Strings,
    reference_period: &str,
    t1_available_periods: Strings,
    t1_reference_period: &str,
) -> Robj {
    let spec = VariableSpec::new(name, item, valid_response);
    let rows = BusinessRows::from_list(&business);
    let period = resolved(
        &available_periods,
        reference_period,
        &t1_available_periods,
        t1_reference_period,
        table_number,
    );
    let domains = domain_set(&variable_values, &domains);
    let location = location_from(&location_mb, &location_sa1, &location_sa2);
    classify_metadata(
        &spec,
        &rows,
        table_number,
        seed as i64,
        &period,
        &domains,
        location.as_ref(),
    )
    .into_robj()
}

/// The tail of `.blade_value_for` in Rust.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn blade_fallthrough_value_for__(
    name: &str,
    table_number: i32,
    seed: i32,
    business: List,
    location_mb: Strings,
    location_sa1: Strings,
    location_sa2: Strings,
    available_periods: Strings,
    reference_period: &str,
    t1_available_periods: Strings,
    t1_reference_period: &str,
) -> Robj {
    let spec = VariableSpec::new(name, "", "");
    let rows = BusinessRows::from_list(&business);
    let period = resolved(
        &available_periods,
        reference_period,
        &t1_available_periods,
        t1_reference_period,
        table_number,
    );
    let location = location_from(&location_mb, &location_sa1, &location_sa2);
    classify_fallthrough(
        &spec,
        &rows,
        table_number,
        seed as i64,
        &period,
        location.as_ref(),
    )
    .into_robj()
}

/// `.blade_location_lookup_rows` in Rust: 1-based row numbers into the Mesh
/// Block lookup, one per business.
/// @export
#[extendr]
fn blade_location_lookup_rows__(
    bn: Strings,
    state: &[i32],
    seed: i32,
    lookup_state: &[i32],
) -> Robj {
    let owned: Vec<String> = bn
        .iter()
        .map(|x| if x.is_na() { String::new() } else { x.to_string() })
        .collect();
    let refs: Vec<&str> = owned.iter().map(String::as_str).collect();
    let bn_num = id_number(&refs);
    match super::helpers::location_lookup_indices(&bn_num, state, seed as i64, lookup_state) {
        Ok(idx) => idx
            .into_iter()
            .map(|i| (i as i32) + 1)
            .collect::<Vec<i32>>()
            .into(),
        Err(st) => throw_r_error(format!("No Mesh Block lookup rows for state {}", st)),
    }
}

extendr_module! {
    mod classifier;
    fn blade_metadata_value_for__;
    fn blade_fallthrough_value_for__;
    fn blade_location_lookup_rows__;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn character_response_frames() {
        assert_eq!(
            character_response_values("Y = Yes; N = No"),
            vec![Some("Y".to_string()), Some("N".to_string())]
        );
        assert_eq!(
            character_response_values("Y - Yes, everything else invalid"),
            vec![Some("Y".to_string()), None]
        );
        assert_eq!(
            character_response_values("N = No"),
            vec![Some("N".to_string()), None]
        );
        assert!(character_response_values("Numeric response ($)").is_empty());
        // "Yes" inside a longer word must not count.
        assert!(character_response_values("MY = Yesterday").is_empty());
    }

    #[test]
    fn survey_table_membership() {
        assert!(is_survey_table(8));
        assert!(is_survey_table(23));
        assert!(!is_survey_table(24));
        assert!(is_survey_table(38));
        assert!(is_survey_table(46));
        assert!(!is_survey_table(47));
    }
}

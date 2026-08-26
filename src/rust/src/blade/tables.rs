//! BLADE table-specific generators and the `.blade_value_for` master cascade
//! (Stage 4 of the R->Rust port).
//!
//! Six BLADE tables carry enough published structure to be generated on their
//! own terms rather than through the generic classifier:
//!
//! | table | generator | what it fixes |
//! |---|---|---|
//! | 1  | `frame_value_for`     | the GST role-code frame and grouped-GST ids |
//! | 4  | `bas_value_for`       | the BAS arithmetic: exports <= turnover, turnover >= other expenses |
//! | 6  | `bit_value_for`       | the c/i/p/t legal-form prefix masking |
//! | 7  | `stp_value_for`       | employer super == round(gross * 0.115, 2) |
//! | 8  | `bcs_value_for`       | the Business Characteristics Survey code frame |
//! | 27 | `birthdate_value_for` | the business birth financial-year label |
//!
//! `value_for` is the ordered cascade every BLADE variable goes through, in
//! `R/generate_blade.R`'s order: the business slice's own columns first, then
//! the identifiers, then the special generator for the table, then the generic
//! metadata classifier, then the name-based fallthrough. First match wins, and
//! moving a branch silently re-routes a column into a different type and range.
//!
//! Generation is RNG-free. Every draw is a closed-form
//! `(seq * const + seed + salt) %% mod` in f64, matching R's overflow-free
//! double arithmetic; see `helpers` for the determinism contract.

use extendr_api::prelude::*;

use super::classifier::{character_response_values, classify_fallthrough, classify_metadata};
use super::helpers::{
    all_digits, any_of, bcs_code, blade_draw, count_value, cycle_values, digits_run_before_digit_word,
    financial_name, financial_year_label, form_prefix, id_number, pick_codes, pick_values, pmax2,
    pmin2, r_mod, related_id, role_code, round1, round2, round_digits, seq2, token, token_suffixed,
    valid_response_codes,
};
use super::periods::ResolvedPeriod;
use super::rows::{
    column_or_none, domain_set, location_from, resolved, BladeColumn, BusinessRows, DomainSet,
    LocationRows, VariableSpec, MONTH_ABB,
};

// ---------------------------------------------------------------------------
// Applicability masks
//
// The BIT generator writes the same column two ways. Some branches build the
// full column and then blank the rows the variable does not apply to
// (`out[!applicable] <- NA`); others start from all-NA and fill only the rows
// it does apply to (`out[applicable] <- values[applicable]`). The two differ
// whenever `applicable` is shorter than the column -- which happens when the
// business slice carries no `legal_form`, because R's `active_prefix == prefix`
// is then `logical(0)` and selects nothing either way.
// ---------------------------------------------------------------------------

fn na_outside<T>(out: &mut [Option<T>], applicable: &[bool]) {
    for (i, ok) in applicable.iter().enumerate() {
        if !ok {
            if let Some(slot) = out.get_mut(i) {
                *slot = None;
            }
        }
    }
}

fn keep_applicable<T: Clone>(values: &[T], applicable: &[bool], n: usize) -> Vec<Option<T>> {
    let mut out: Vec<Option<T>> = (0..n).map(|_| None).collect();
    for (i, ok) in applicable.iter().enumerate() {
        if *ok {
            if let (Some(slot), Some(value)) = (out.get_mut(i), values.get(i)) {
                *slot = Some(value.clone());
            }
        }
    }
    out
}

fn owned(values: &[&str]) -> Vec<String> {
    values.iter().map(|s| s.to_string()).collect()
}

/// R `[<int vector>]` to a nullable integer column.
fn int_column(values: Option<Vec<i32>>) -> BladeColumn {
    match values {
        Some(v) => BladeColumn::Int(
            v.iter()
                .map(|x| if *x == i32::MIN { None } else { Some(*x) })
                .collect(),
        ),
        None => BladeColumn::Int(Vec::new()),
    }
}

/// R `^[cipt]_`.
fn legal_form_prefix(lower: &str) -> Option<char> {
    let b = lower.as_bytes();
    if b.len() >= 2 && b[1] == b'_' && matches!(b[0], b'c' | b'i' | b'p' | b't') {
        Some(b[0] as char)
    } else {
        None
    }
}

/// R `^[cipt]_<stem>$` for any of the given stems.
fn prefixed_total(lower: &str, stems: &[&str]) -> bool {
    match legal_form_prefix(lower) {
        Some(_) => stems.iter().any(|stem| &lower[2..] == *stem),
        None => false,
    }
}

// ---------------------------------------------------------------------------
// Table 1 -- .blade_frame_value_for
// ---------------------------------------------------------------------------

fn frame_value_for(spec: &VariableSpec, rows: &BusinessRows, seed: i64) -> BladeColumn {
    let l = spec.lower.as_str();
    let n = rows.n;

    // R L2361. The income-tax / GST role codes the business register carries.
    if matches!(l, "x_itip" | "x_itw" | "x_gstp") {
        return BladeColumn::strs(
            (1..=n)
                .map(|i| role_code(i as i64, seed, spec.name_seed).to_string())
                .collect(),
        );
    }
    // R L2364. Only the profiled businesses inside a GST group carry a group
    // registration; everyone else has a blank cell, which the test suite
    // requires to exist alongside the GST0000000 shape.
    if l == "x_gst_bn" {
        let mut out = vec![String::new(); n];
        // R's `NULL == 1L` is logical(0), so a slice with no `is_profiled` or
        // no `bg_id` groups nobody and every cell stays blank.
        let (profiled, bg_id) = match (rows.int("is_profiled"), rows.chr("bg_id")) {
            (Some(p), Some(b)) => (p, b),
            _ => return BladeColumn::strs(out),
        };
        let bg_values: Vec<String> = bg_id.into_iter().map(|v| v.unwrap_or_default()).collect();
        let bg_refs: Vec<&str> = bg_values.iter().map(String::as_str).collect();
        // The whole-column call back-fills every blank `bg_id` with 1,2,3,...
        let all_keys = id_number(&bg_refs);
        let grouped: Vec<bool> = (0..n)
            .map(|i| {
                profiled.get(i).copied() == Some(1)
                    && bg_values.get(i).map(|s| !s.is_empty()).unwrap_or(false)
                    && all_keys
                        .get(i)
                        .map(|k| r_mod(k + seed as f64, 3.0) == 0.0)
                        .unwrap_or(false)
            })
            .collect();
        // R calls `.blade_id_number` a SECOND time over the grouped subset
        // alone, where nothing is blank so nothing is back-filled. The two
        // calls disagree and both are load-bearing.
        let subset: Vec<&str> = (0..n).filter(|&i| grouped[i]).map(|i| bg_refs[i]).collect();
        let group_ids = id_number(&subset);
        let mut rank = 0usize;
        for i in 0..n {
            if grouped[i] {
                let value = r_mod(
                    group_ids[rank] + seed as f64 + spec.name_seed as f64,
                    10_000_000.0,
                ) as i64;
                out[i] = format!("GST{:07}", value);
                rank += 1;
            }
        }
        return BladeColumn::strs(out);
    }
    BladeColumn::None
}

// ---------------------------------------------------------------------------
// Table 4 -- .blade_bas_value_for
// ---------------------------------------------------------------------------

/// `bas_wage_level` is `nominal_index("wage", ...)` for the table's period,
/// computed in R because the Rust nominal module hashes differently. It scales
/// the flat dollar addition below, which is an anchor-year amount sitting on
/// wages that arrive already repriced.
fn bas_value_for(
    spec: &VariableSpec,
    rows: &BusinessRows,
    seed: i64,
    bas_wage_level: f64,
) -> BladeColumn {
    let l = spec.lower.as_str();
    let n = rows.n;
    let raw_turnover = rows.numeric_or_empty("turnover");
    let raw_wages = rows.numeric_or_empty("annual_wages");
    let turnover: Vec<f64> = raw_turnover.iter().map(|x| round2(*x)).collect();

    let seq = |i: usize| (i as i64) + 1;
    let wages: Vec<f64> = if raw_wages.is_empty() {
        Vec::new()
    } else {
        (0..raw_wages.len())
            .map(|i| {
                let w = raw_wages[i];
                if w > 0.0 {
                    round2(
                        w * (1.03 + ((seq(i) + seed).rem_euclid(11) as f64) / 100.0)
                            + (500.0 + ((seq(i) * 97 + seed).rem_euclid(2500) as f64))
                                * bas_wage_level,
                    )
                } else {
                    0.0
                }
            })
            .collect()
    };
    // The other-expenses factor tops out at 0.42 + 0.34, so turnover is always
    // at least other expenses -- one of the BAS inequalities the tests hold.
    let oexp: Vec<f64> = turnover
        .iter()
        .enumerate()
        .map(|(i, t)| round2(t * (0.42 + ((seq(i) * 13 + seed).rem_euclid(35) as f64) / 100.0)))
        .collect();
    let capex: Vec<f64> = rows
        .numeric_or_empty("capital_expenditure")
        .iter()
        .map(|x| round2(*x))
        .collect();
    // The export factor tops out at 0.01 + 0.17 and is capped at three
    // quarters of turnover, so exports never exceed turnover.
    let exports: Vec<f64> = turnover
        .iter()
        .enumerate()
        .map(|(i, t)| {
            round2(pmin2(
                t * (0.01 + ((seq(i) * 7 + seed).rem_euclid(18) as f64) / 100.0),
                t * 0.75,
            ))
        })
        .collect();

    match l {
        // Unreachable in the build -- the business slice carries its own
        // `turnover`, so the name-map branch of `value_for` wins. Kept because
        // a direct call with a slice that has no turnover column still routes
        // here, as the R does.
        "turnover" => BladeColumn::dbls(turnover),
        "exports_amt" => BladeColumn::dbls(exports),
        "other_gst_free_sales" => BladeColumn::dbls(
            (0..exports.len())
                .map(|i| round2(pmax2(exports[i], turnover[i] * 0.02)))
                .collect(),
        ),
        "capex" => BladeColumn::dbls(capex),
        "oexp" => BladeColumn::dbls(oexp),
        "tot_expenses" => {
            if oexp.is_empty() || capex.is_empty() {
                BladeColumn::Dbl(Vec::new())
            } else {
                BladeColumn::dbls((0..n).map(|i| round2(oexp[i] + capex[i])).collect())
            }
        }
        "wages" => BladeColumn::dbls(wages),
        // The raw wage, not the grossed-up BAS one.
        "p_wrk_amt" | "payg_tax_withheld" => {
            BladeColumn::dbls(raw_wages.iter().map(|w| round2(w * 0.22)).collect())
        }
        "a_it_w_amt" => BladeColumn::dbls(turnover.iter().map(|t| round2(t * 0.005)).collect()),
        "t_it_w_amt" => BladeColumn::dbls(turnover.iter().map(|t| round2(t * 0.003)).collect()),
        // Also unreachable: the spine's own `gst_payable` wins the name map.
        "gst_payable" => BladeColumn::dbls(
            (0..oexp.len())
                .map(|i| round2(pmax2(turnover[i] * 0.1 - oexp[i] * 0.1, 0.0)))
                .collect(),
        ),
        "credit_for_gst_paid" => BladeColumn::dbls(oexp.iter().map(|o| round2(o * 0.1)).collect()),
        "d_imprt_amt" => {
            let imports = rows.numeric_or_empty("import_value");
            if imports.is_empty() || turnover.is_empty() {
                BladeColumn::Dbl(Vec::new())
            } else {
                BladeColumn::dbls(
                    (0..n)
                        .map(|i| round2(pmin2(turnover[i] * 0.15, imports[i])))
                        .collect(),
                )
            }
        }
        // The only BAS field salted by name; everything else is index-only.
        "month_actioned" => {
            let draw = blade_draw(rows.bn_num(), seed, spec.name_seed, MONTH_ABB.len() as i64);
            BladeColumn::strs(
                draw.iter()
                    .map(|&d| MONTH_ABB[d as usize].to_string())
                    .collect(),
            )
        }
        _ => BladeColumn::None,
    }
}

// ---------------------------------------------------------------------------
// Table 6 -- .blade_bit_value_for
// ---------------------------------------------------------------------------

/// Business income tax. Every variable is prefixed by the legal form it
/// belongs to -- `c_` company, `i_` individual (sole trader), `p_` partnership,
/// `t_` trust -- and is NA on every business of a different form. That is the
/// c/i/p/t exclusivity the test suite checks through `c_totlwage`.
fn bit_value_for(
    spec: &VariableSpec,
    rows: &BusinessRows,
    seed: i64,
    period: &ResolvedPeriod,
    domains: &DomainSet,
) -> BladeColumn {
    let l = spec.lower.as_str();
    let n = rows.n;
    let salt = spec.salt;
    let key = rows.bn_num();
    let seq = |i: usize| (i as i64) + 1;

    let active_prefix: Vec<char> = match rows.chr("legal_form") {
        Some(v) => v
            .iter()
            .map(|s| form_prefix(s.as_deref().unwrap_or("")))
            .collect(),
        None => Vec::new(),
    };
    let prefix = legal_form_prefix(l);
    let applicable: Vec<bool> = match prefix {
        None => vec![true; n],
        Some(p) => active_prefix.iter().map(|c| *c == p).collect(),
    };

    let turnover = rows.numeric_or_empty("turnover");
    let raw_wages = rows.numeric_or_empty("annual_wages");
    let income: Vec<f64> = turnover
        .iter()
        .enumerate()
        .map(|(i, t)| round2(t * (0.92 + ((seq(i) * 5 + seed).rem_euclid(13) as f64) / 100.0)))
        .collect();
    let expenses: Vec<f64> = if turnover.is_empty() || raw_wages.is_empty() {
        Vec::new()
    } else {
        (0..n)
            .map(|i| {
                round2(pmin2(
                    income[i] * 0.96,
                    raw_wages[i] * 1.12
                        + turnover[i]
                            * (0.35 + ((seq(i) * 7 + seed).rem_euclid(25) as f64) / 100.0),
                ))
            })
            .collect()
    };
    let profit: Vec<f64> = (0..expenses.len())
        .map(|i| round2(pmax2(income[i] - expenses[i], 0.0)))
        .collect();
    let wages: Vec<f64> = raw_wages.iter().map(|w| round2(*w)).collect();

    // B1 (R L2431). The four legal-form indicator columns. They carry no
    // applicability mask of their own -- the comparison IS the mask.
    const BIT_FLAGS: [(&str, char); 4] = [
        ("bit_comp_yyyy", 'c'),
        ("bit_ind_yyyy", 'i'),
        ("bit_part_yyyy", 'p'),
        ("bit_trust_yyyy", 't'),
    ];
    if let Some((_, flag)) = BIT_FLAGS.iter().find(|(name, _)| *name == l) {
        return BladeColumn::Int(
            active_prefix
                .iter()
                .map(|c| if c == flag { Some(1) } else { None })
                .collect(),
        );
    }
    // B2 (R L2435)
    let character_values = character_response_values(&spec.valid_raw);
    if !character_values.is_empty() {
        let mut out = cycle_values(&character_values, n, seed, salt, Some(key));
        na_outside(&mut out, &applicable);
        return BladeColumn::Chr(out);
    }
    // B3 (R L2442). The published code frame is authoritative wherever the
    // data item list spells one out.
    let codes = valid_response_codes(&spec.valid_raw);
    if !codes.is_empty() {
        let mut out: Vec<Option<i32>> = pick_codes(&codes, n, seed, salt, true, Some(key))
            .into_iter()
            .map(Some)
            .collect();
        na_outside(&mut out, &applicable);
        return BladeColumn::Int(out);
    }
    // B4 (R L2451)
    let source_values = &domains.variable_values;
    if !source_values.is_empty() {
        let picked = pick_values(source_values, key, seed, salt, 0.0);
        let numeric_frame = source_values.iter().all(|v| all_digits(v))
            && !any_of(&spec.valid_lower, &["character", "alphanumeric", "string"]);
        if numeric_frame {
            let mut out: Vec<Option<i32>> = picked
                .into_iter()
                .map(|v| v.and_then(|s| s.parse::<i32>().ok()))
                .collect();
            na_outside(&mut out, &applicable);
            return BladeColumn::Int(out);
        }
        let mut out = picked;
        na_outside(&mut out, &applicable);
        return BladeColumn::Chr(out);
    }
    // B5 (R L2462). NFP is in the generator's frame but not in the published
    // Valid.Response, which lists only the five ATO market segments.
    if l.ends_with("busmarin") {
        let mut out = pick_values(
            &owned(&["GOV", "LGE", "SME", "INB", "MIC", "NFP"]),
            key,
            seed,
            salt,
            0.0,
        );
        na_outside(&mut out, &applicable);
        return BladeColumn::Chr(out);
    }
    // B6 (R L2470)
    if l == "c_coy_cntry_dcd" {
        let mut out = pick_values(
            &owned(&[
                "Australia",
                "New Zealand",
                "United Kingdom",
                "United States",
                "Singapore",
                "India",
                "China",
                "Japan",
            ]),
            key,
            seed,
            salt,
            0.0,
        );
        na_outside(&mut out, &applicable);
        return BladeColumn::Chr(out);
    }
    // B7 (R L2479). A foreign-currency rate, 0.5 to 3.0.
    if l == "c_fcrncyrt" {
        let draw = blade_draw(key, seed, salt, 25_001);
        let values: Vec<f64> = draw
            .iter()
            .map(|&d| round_digits(0.5 + d as f64 / 10_000.0, 5))
            .collect();
        return BladeColumn::Dbl(keep_applicable(&values, &applicable, n));
    }
    // B8 (R L2485). A fixed-width numeric string, e.g. the six-digit
    // asset-count fields. The zero padding is the published shape.
    if spec.valid_lower.contains("digit character") && l.ends_with("_num") {
        let width = digits_run_before_digit_word(&spec.valid_lower).unwrap_or(5).max(0) as usize;
        let values = count_value(l, rows.payees().as_deref(), key, seed, salt);
        let padded: Vec<String> = values
            .iter()
            .map(|v| format!("{:0w$}", v, w = width))
            .collect();
        return BladeColumn::Chr(keep_applicable(&padded, &applicable, n));
    }
    // B9 (R L2497). A percentage, one decimal place.
    if spec.valid_lower.contains("numeric response (%)") {
        let values: Vec<f64> = blade_draw(key, seed, salt, 1000)
            .iter()
            .map(|&d| round1(d as f64 / 10.0))
            .collect();
        return BladeColumn::Dbl(keep_applicable(&values, &applicable, n));
    }
    // B10 (R L2504). A four-digit year, within six years of the table period.
    if seq2(&spec.valid_lower, "4 digit character", "yyyy") {
        let years_ago = blade_draw(key, seed, salt, 6);
        let values: Vec<Option<i32>> = years_ago
            .iter()
            .map(|d| period.end_year.map(|y| y - d))
            .collect();
        let mut out: Vec<Option<i32>> = (0..n).map(|_| None).collect();
        for (i, ok) in applicable.iter().enumerate() {
            if *ok {
                if let (Some(slot), Some(value)) = (out.get_mut(i), values.get(i)) {
                    *slot = *value;
                }
            }
        }
        return BladeColumn::Int(out);
    }
    // B11, B12 (R L2511, L2518)
    if l.contains("anzsic") {
        let codes: Vec<Option<i32>> = rows
            .chr("anzsic06")
            .unwrap_or_default()
            .iter()
            .map(|v| v.as_deref().and_then(|s| s.parse::<i32>().ok()))
            .collect();
        let mut out: Vec<Option<i32>> = (0..n).map(|_| None).collect();
        for (i, ok) in applicable.iter().enumerate() {
            if *ok {
                if let (Some(slot), Some(value)) = (out.get_mut(i), codes.get(i)) {
                    *slot = *value;
                }
            }
        }
        return BladeColumn::Int(out);
    }
    if l.contains("poscode") || l.contains("postcode") {
        let codes: Vec<Option<i32>> = rows
            .chr("x_pcode")
            .unwrap_or_default()
            .iter()
            .map(|v| v.as_deref().and_then(|s| s.parse::<i32>().ok()))
            .collect();
        let mut out: Vec<Option<i32>> = (0..n).map(|_| None).collect();
        for (i, ok) in applicable.iter().enumerate() {
            if *ok {
                if let (Some(slot), Some(value)) = (out.get_mut(i), codes.get(i)) {
                    *slot = *value;
                }
            }
        }
        return BladeColumn::Int(out);
    }

    // The per-name spread factor, 0.25 to 0.90. It is salted by the variable
    // name, which is what stops sibling amount columns (gross interest, gross
    // dividends, total income) coming out identical. It is built to full
    // length so the branches below can index it: `bn` is the one business
    // column every draw needs, and a slice without it yields NA rather than a
    // short column that would index out of bounds.
    let value_factor: Vec<f64> = {
        let draw = blade_draw(key, seed, salt + 1543, 6501);
        (0..n)
            .map(|i| {
                draw.get(i)
                    .map(|d| 0.25 + *d as f64 / 10_000.0)
                    .unwrap_or(f64::NAN)
            })
            .collect()
    };

    // B13 (R L2527). The four `<form>_totlwage` columns carry the business's
    // whole wage bill; every other wage-shaped column is a component of it.
    if any_of(l, &["totlwage", "totlwg", "salwg", "salary", "wage", "labr"]) {
        let is_primary = prefixed_total(l, &["totlwage", "totlwg", "totlsalwg"]);
        let values: Vec<f64> = if is_primary {
            wages.clone()
        } else if wages.is_empty() || turnover.is_empty() {
            Vec::new()
        } else {
            (0..n)
                .map(|i| {
                    round2(pmax2(
                        wages[i] * value_factor[i],
                        turnover[i] * 0.005 * value_factor[i],
                    ))
                })
                .collect()
        };
        return BladeColumn::Dbl(keep_applicable(&values, &applicable, n));
    }
    // B14 (R L2537)
    if any_of(l, &["sales", "turnover", "tnovr"]) {
        let values: Vec<f64> = turnover
            .iter()
            .enumerate()
            .map(|(i, t)| round2(t * (0.72 + 0.28 * value_factor[i])))
            .collect();
        return BladeColumn::Dbl(keep_applicable(&values, &applicable, n));
    }
    // B15 (R L2544). `net` sits here on purpose: it catches the
    // net-exempt-income amounts, which are dollars, not the year their name
    // mentions.
    if any_of(l, &["taxinc", "taxable", "profit", "net", "loss"]) {
        let values: Vec<f64> = profit
            .iter()
            .enumerate()
            .map(|(i, p)| round2(p * (0.55 + 0.45 * value_factor[i])))
            .collect();
        return BladeColumn::Dbl(keep_applicable(&values, &applicable, n));
    }
    // B16 (R L2550)
    if any_of(
        l,
        &[
            "totlexps", "cost", "expense", "expn", "exps", "ddct", "ded", "depr", "rent", "super",
        ],
    ) {
        let is_primary = prefixed_total(l, &["totlexps"]);
        let values: Vec<f64> = if is_primary {
            expenses.clone()
        } else {
            expenses
                .iter()
                .enumerate()
                .map(|(i, e)| round2(e * value_factor[i]))
                .collect()
        };
        return BladeColumn::Dbl(keep_applicable(&values, &applicable, n));
    }
    // B17 (R L2559)
    if any_of(
        l,
        &[
            "totlinc", "income", "incm", "inclo", "inc", "revenue", "gross", "gros", "grss",
        ],
    ) {
        let is_primary = prefixed_total(l, &["totlinc", "totlincm"]);
        let values: Vec<f64> = if is_primary {
            income.clone()
        } else {
            income
                .iter()
                .enumerate()
                .map(|(i, v)| round2(v * value_factor[i]))
                .collect()
        };
        return BladeColumn::Dbl(keep_applicable(&values, &applicable, n));
    }
    // B18 (R L2568). Anything else that reads as money.
    if financial_name(l) {
        if turnover.is_empty() || wages.is_empty() {
            return BladeColumn::Dbl(keep_applicable::<f64>(&[], &applicable, n));
        }
        let values: Vec<f64> = (0..n)
            .map(|i| {
                round2(pmax2(
                    turnover[i] * (0.02 + value_factor[i] * 0.3),
                    wages[i] * (0.03 + value_factor[i] * 0.08),
                ))
            })
            .collect();
        return BladeColumn::Dbl(keep_applicable(&values, &applicable, n));
    }
    BladeColumn::None
}

// ---------------------------------------------------------------------------
// Table 7 -- .blade_stp_value_for
// ---------------------------------------------------------------------------

/// R `_ls_[a-e]$`: the unused annual-leave and long-service lump-sum
/// components, which are dollar amounts rather than flags.
fn lump_sum_component(lower: &str) -> bool {
    let b = lower.as_bytes();
    b.len() >= 5
        && matches!(b[b.len() - 1], b'a'..=b'e')
        && &b[b.len() - 5..b.len() - 1] == b"_ls_"
}

fn stp_value_for(
    spec: &VariableSpec,
    rows: &BusinessRows,
    seed: i64,
    period: &ResolvedPeriod,
) -> BladeColumn {
    let l = spec.lower.as_str();
    let n = rows.n;
    let wages: Vec<f64> = rows
        .numeric_or_empty("annual_wages")
        .iter()
        .map(|w| round2(*w))
        .collect();
    let seq = |i: usize| (i as i64) + 1;

    // S1 (R L2585). The payee count comes from the spine's own
    // `d_total_payees` in the build, because the name map wins first; this
    // branch serves the other payee-shaped names.
    if l == "d_total_payees" || any_of(l, &["payee", "employee", "headcount", "hcnt"]) {
        return int_column(rows.int("d_total_payees"));
    }
    // S2 (R L2589). The superannuation guarantee rate. Gross pay and employer
    // super come off the same `wages` vector, which is what makes
    // `super == round(gross * 0.115, 2)` hold to the cent.
    if l == "ed_sg_emplr_cntrbtn" || any_of(l, &["super", "spr", "sg_emplr", "emplr_cntrbtn"]) {
        return BladeColumn::dbls(wages.iter().map(|w| round2(w * 0.115)).collect());
    }
    // S3, S4 (R L2593, L2594)
    if any_of(l, &["tax", "withheld", "whld"]) {
        return BladeColumn::dbls(wages.iter().map(|w| round2(w * 0.22)).collect());
    }
    if any_of(l, &["allow", "alwnc"]) {
        return BladeColumn::dbls(wages.iter().map(|w| round2(w * 0.025)).collect());
    }
    // S5 (R L2599). Most employees have no lump sum; a minority have a payout.
    if any_of(l, &["lump", "bonus", "termination", "etp", "lng_srvc", "unsd"])
        || lump_sum_component(l)
    {
        return BladeColumn::dbls(
            (0..wages.len())
                .map(|i| {
                    if (seq(i) + seed).rem_euclid(9) == 0 {
                        round2(wages[i] * 0.04)
                    } else {
                        0.0
                    }
                })
                .collect(),
        );
    }
    // S6 (R L2603)
    if any_of(
        l,
        &[
            "gross", "grs", "wage", "salary", "pay", "pmt", "amount", "amt", "total", "totl",
            "sumry",
        ],
    ) {
        return BladeColumn::dbls(wages);
    }
    // S7 (R L2607)
    if any_of(l, &["date", "start", "end"]) {
        return BladeColumn::Date(
            (0..n)
                .map(|i| {
                    Some(period.reference_date - ((seq(i) + seed).rem_euclid(365) as f64))
                })
                .collect(),
        );
    }
    BladeColumn::None
}

// ---------------------------------------------------------------------------
// Table 8 -- .blade_bcs_value_for
// ---------------------------------------------------------------------------

/// The Business Characteristics Survey. Unlike the other five this generator
/// is TERMINAL: its last branch always produces the BCS code frame, so no
/// table 8 variable ever reaches the generic classifier or the fallthrough.
fn bcs_value_for(
    spec: &VariableSpec,
    rows: &BusinessRows,
    seed: i64,
    period: &ResolvedPeriod,
) -> BladeColumn {
    let l = spec.lower.as_str();
    let n = rows.n;
    let seq = |i: usize| (i as i64) + 1;
    let turnover: Vec<f64> = rows
        .numeric_or_empty("turnover")
        .iter()
        .map(|t| round2(*t))
        .collect();
    let wages: Vec<f64> = rows
        .numeric_or_empty("annual_wages")
        .iter()
        .map(|w| round2(*w))
        .collect();
    let emp_total: Vec<i32> = rows
        .int("d_total_payees")
        .map(|v| v.iter().map(|x| (*x).max(0)).collect())
        .unwrap_or_default();
    let legal_form = rows.chr("legal_form");

    match l {
        "period_bcs" | "period" => return BladeColumn::rep_str(&period.latest_period, n),
        "incsalgs_bcs" => {
            return BladeColumn::dbls(turnover.iter().map(|t| round2(t * 0.92)).collect())
        }
        "incothin_bcs" => {
            return BladeColumn::dbls(turnover.iter().map(|t| round2(t * 0.08)).collect())
        }
        "inctotal_bcs" => {
            return BladeColumn::dbls(
                turnover
                    .iter()
                    .map(|t| round2(round2(t * 0.92) + round2(t * 0.08)))
                    .collect(),
            )
        }
        "capextot_bcs" => {
            return BladeColumn::dbls(
                rows.numeric_or_empty("capital_expenditure")
                    .iter()
                    .map(|c| round2(*c))
                    .collect(),
            )
        }
        _ => {}
    }

    let expenses: Vec<f64> = turnover
        .iter()
        .enumerate()
        .map(|(i, t)| round2(t * (0.46 + ((seq(i) + seed).rem_euclid(24) as f64) / 100.0)))
        .collect();

    match l {
        "allexpto_bcs" => return BladeColumn::dbls(expenses),
        "exptotal_bcs" => {
            return if expenses.is_empty() || wages.is_empty() {
                BladeColumn::Dbl(Vec::new())
            } else {
                BladeColumn::dbls((0..n).map(|i| round2(expenses[i] + wages[i])).collect())
            }
        }
        // Every business operates at least one location.
        "locs_bcs" => {
            return BladeColumn::ints(
                (0..n)
                    .map(|i| (1 + (seq(i) + seed).rem_euclid(4) as i32).max(1))
                    .collect(),
            )
        }
        "emptotal_bcs" | "empoth_bcs" => return BladeColumn::ints(emp_total),
        "empft_bcs" => {
            return BladeColumn::ints(
                emp_total
                    .iter()
                    .enumerate()
                    .map(|(i, e)| {
                        let share = 0.45 + ((seq(i) + seed).rem_euclid(30) as f64) / 100.0;
                        (*e).min((*e as f64 * share).floor() as i32)
                    })
                    .collect(),
            )
        }
        "casuals_bcs" => {
            return BladeColumn::ints(
                emp_total
                    .iter()
                    .enumerate()
                    .map(|(i, e)| {
                        let share = 0.08 + ((seq(i) + seed).rem_euclid(20) as f64) / 100.0;
                        (*e).min((*e as f64 * share).ceil() as i32)
                    })
                    .collect(),
            )
        }
        // Working proprietors exist only in unincorporated businesses.
        "empprop_bcs" => {
            let forms = match &legal_form {
                Some(v) => v,
                None => return BladeColumn::Int(Vec::new()),
            };
            return BladeColumn::Int(
                forms
                    .iter()
                    .enumerate()
                    .map(|(i, f)| {
                        if matches!(f.as_deref(), Some("Sole trader") | Some("Partnership")) {
                            // R's `ifelse` recycles a zero-length `yes` arm into
                            // NA, so a slice with no payee count gives NA here
                            // rather than a shorter column.
                            emp_total.get(i).map(|e| (e + 1).min(4).max(1))
                        } else {
                            Some(0)
                        }
                    })
                    .collect(),
            );
        }
        // Working directors exist only in companies that employ.
        "empsaldr_bcs" => {
            let forms = match &legal_form {
                Some(v) => v,
                None => return BladeColumn::Int(Vec::new()),
            };
            if emp_total.is_empty() {
                return BladeColumn::Int(Vec::new());
            }
            return BladeColumn::ints(
                forms
                    .iter()
                    .enumerate()
                    .map(|(i, f)| {
                        if f.as_deref() == Some("Company") && emp_total[i] > 0 {
                            (1 + (seq(i) + seed).rem_euclid(3) as i32).min(3)
                        } else {
                            0
                        }
                    })
                    .collect(),
            );
        }
        "perscomm_bcs" | "persceas_bcs" => {
            return BladeColumn::ints(
                emp_total
                    .iter()
                    .enumerate()
                    .map(|(i, e)| {
                        (*e).min((seq(i) + seed + spec.name_seed).rem_euclid(6) as i32)
                    })
                    .collect(),
            )
        }
        // Years the business has been owned or operating, capped at forty.
        "busownyr_bcs" | "busopyr_bcs" => {
            let birth = rows.int("business_birth_year").unwrap_or_default();
            return BladeColumn::Int(
                birth
                    .iter()
                    .map(|y| {
                        if *y == i32::MIN {
                            return None;
                        }
                        period.end_year.map(|end| (end - y).min(40).max(0))
                    })
                    .collect(),
            );
        }
        "d_gsnewy" => return BladeColumn::ints(bcs_code(n, seed, true)),
        _ => {}
    }
    // C18 (R L2686)
    if any_of(l, &["date", "start", "end"]) {
        return BladeColumn::Date(
            (0..n)
                .map(|i| Some(period.reference_date - ((seq(i) + seed).rem_euclid(365) as f64)))
                .collect(),
        );
    }
    // C19 (R L2690)
    if any_of(
        l,
        &[
            "inc", "income", "sales", "capex", "expto", "exp", "cost", "wage", "salary", "pay",
            "amt", "amount", "turnover", "assets", "liab", "debt", "fund", "loan", "fee", "val",
        ],
    ) {
        if turnover.is_empty() || wages.is_empty() {
            return BladeColumn::Dbl(Vec::new());
        }
        return BladeColumn::dbls(
            (0..n)
                .map(|i| {
                    round2(pmax2(
                        turnover[i] * (0.02 + ((seq(i) * 3 + seed).rem_euclid(35) as f64) / 100.0),
                        wages[i] * 0.03,
                    ))
                })
                .collect(),
        );
    }
    // C20 (R L2698). Terminal: the survey response frame.
    BladeColumn::ints(bcs_code(
        n,
        seed,
        l.starts_with("d_") || any_of(l, &["new", "innov", "gs"]),
    ))
}

// ---------------------------------------------------------------------------
// Table 27 -- .blade_birthdate_value_for
// ---------------------------------------------------------------------------

/// The birth-year spikes at 1993 and 2001, and the not-stated share, are set
/// upstream in the business spine, not here. This only labels the year.
fn birthdate_value_for(spec: &VariableSpec, rows: &BusinessRows) -> BladeColumn {
    if spec.lower == "birth_date" {
        return match rows.int("business_birth_year") {
            Some(v) => BladeColumn::Chr(
                v.iter()
                    .map(|y| {
                        if *y == i32::MIN {
                            None
                        } else {
                            Some(financial_year_label(*y))
                        }
                    })
                    .collect(),
            ),
            None => BladeColumn::Chr(Vec::new()),
        };
    }
    BladeColumn::None
}

// ---------------------------------------------------------------------------
// .blade_special_value_for and .blade_value_for
// ---------------------------------------------------------------------------

/// Everything one variable's column needs beyond its own metadata.
pub struct ValueContext<'a> {
    pub rows: &'a BusinessRows,
    pub table_number: i32,
    pub seed: i64,
    pub period: &'a ResolvedPeriod,
    pub domains: &'a DomainSet,
    pub location: Option<&'a LocationRows>,
    /// `nominal_index("wage", ...)` for the table's period, used by BAS alone.
    pub bas_wage_level: f64,
}

pub fn special_value_for(spec: &VariableSpec, ctx: &ValueContext) -> BladeColumn {
    match ctx.table_number {
        1 => frame_value_for(spec, ctx.rows, ctx.seed),
        4 => bas_value_for(spec, ctx.rows, ctx.seed, ctx.bas_wage_level),
        6 => bit_value_for(spec, ctx.rows, ctx.seed, ctx.period, ctx.domains),
        7 => stp_value_for(spec, ctx.rows, ctx.seed, ctx.period),
        8 => bcs_value_for(spec, ctx.rows, ctx.seed, ctx.period),
        27 => birthdate_value_for(spec, ctx.rows),
        _ => BladeColumn::None,
    }
}

/// The ordered cascade every BLADE variable goes through (R L2734-2932).
/// First match wins; the order is the whole contract.
pub fn value_for(spec: &VariableSpec, ctx: &ValueContext) -> BladeColumn {
    let l = spec.lower.as_str();
    let rows = ctx.rows;
    let n = rows.n;

    // V1 (R L2741). The slice's own column, returned untouched. This is what
    // makes table 4's `gst_payable` and table 7's `d_total_payees` come from
    // the business spine rather than from their table generators.
    if let Some(column) = rows.raw_lower(l) {
        return BladeColumn::Raw(column);
    }
    // V2, V3 (R L2744, L2747)
    if l == "bn" {
        return column_or_none(rows.chr("bn"));
    }
    if matches!(l, "id" | "unit_id") || any_of(l, &["unitid", "unit_id"]) {
        return column_or_none(rows.chr("id"));
    }
    // V4 (R L2750). Length 1, as R leaves it -- the frame recycles it.
    if l.contains("version") {
        let code = ((ctx.table_number as i64) * 7 + ctx.seed).rem_euclid(300);
        return BladeColumn::Chr(vec![Some(format!("in{:03}_v1", code))]);
    }
    // V5, V6 (R L2754, L2757)
    if l == "tsid" || seq2(l, "time", "series") {
        return BladeColumn::rep_str(&ctx.period.tsid, n);
    }
    if l.contains("quarter") {
        return BladeColumn::rep_str(&format!("{}Q4", ctx.period.financial_year_code), n);
    }
    // V7 (R L2760)
    let special = special_value_for(spec, ctx);
    if !special.is_none() {
        return special;
    }
    // V8 (R L2764). Salted by `.stable_name_seed`, not `.blade_name_salt`.
    let character_values = character_response_values(&spec.valid_raw);
    if !character_values.is_empty() {
        return BladeColumn::Chr(cycle_values(
            &character_values,
            n,
            ctx.seed,
            spec.name_seed,
            Some(rows.bn_num()),
        ));
    }
    // V9-V11 (R L2769-2776). Related-identifier columns.
    if token(l, "bn") || token(l, "abn") {
        return BladeColumn::strs(related_id(
            "BN",
            rows.bn_num(),
            ctx.seed,
            spec.name_seed,
            11,
        ));
    }
    if token(l, "cn") || token(l, "acn") {
        return column_or_none(rows.chr("cn"));
    }
    if token(l, "fn")
        || token(l, "sbid")
        || token_suffixed(l, "employer", &["_id"], true)
        || token_suffixed(l, "emplr", &["_id"], true)
    {
        return column_or_none(rows.chr("fn"));
    }
    // V12 (R L2780). The generic metadata classifier.
    let metadata = classify_metadata(
        spec,
        rows,
        ctx.table_number,
        ctx.seed,
        ctx.period,
        ctx.domains,
        ctx.location,
    );
    if !metadata.is_none() {
        return metadata;
    }
    // V13-V36 (R L2815-2931). The name-based fallthrough. It always produces a
    // column, which is what keeps unclassified character variables clear of
    // the forbidden `_[0-9]{6}$` placeholder shape.
    classify_fallthrough(
        spec,
        rows,
        ctx.table_number,
        ctx.seed,
        ctx.period,
        ctx.location,
    )
}

// ---------------------------------------------------------------------------
// extendr entry point
// ---------------------------------------------------------------------------

/// `.blade_value_for` in Rust: the whole cascade for one variable.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn blade_value_for__(
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
    pinned_period: &str,
    bas_wage_level: f64,
) -> Robj {
    let spec = VariableSpec::new(name, item, valid_response);
    let rows = BusinessRows::from_list(&business);
    let period = resolved(
        &available_periods,
        reference_period,
        pinned_period,
    );
    let domains = domain_set(&variable_values, &domains);
    let location = location_from(&location_mb, &location_sa1, &location_sa2);
    let ctx = ValueContext {
        rows: &rows,
        table_number,
        seed: seed as i64,
        period: &period,
        domains: &domains,
        location: location.as_ref(),
        bas_wage_level,
    };
    value_for(&spec, &ctx).into_robj()
}

extendr_module! {
    mod tables;
    fn blade_value_for__;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn legal_form_prefixes() {
        assert_eq!(legal_form_prefix("c_totlwage"), Some('c'));
        assert_eq!(legal_form_prefix("t_asmt_calcn_cd"), Some('t'));
        assert_eq!(legal_form_prefix("bit_comp_yyyy"), None);
        assert_eq!(legal_form_prefix("c"), None);
        assert!(prefixed_total("c_totlwage", &["totlwage", "totlwg", "totlsalwg"]));
        assert!(prefixed_total("i_totlsalwg", &["totlwage", "totlwg", "totlsalwg"]));
        assert!(!prefixed_total("c_totlwages", &["totlwage"]));
        assert!(prefixed_total("p_totlexps", &["totlexps"]));
        assert!(prefixed_total("c_totlincm", &["totlinc", "totlincm"]));
        assert!(!prefixed_total("c_grosintr", &["totlinc", "totlincm"]));
    }

    #[test]
    fn lump_sum_suffix() {
        assert!(lump_sum_component("ed_anl_lng_srvc_unsd_ls_a"));
        assert!(lump_sum_component("ed_ls_e"));
        assert!(!lump_sum_component("ed_ls_f"));
        assert!(!lump_sum_component("ed_lsa"));
    }

    #[test]
    fn masks_follow_r_subsetting() {
        // `out[!applicable] <- NA` over a full column.
        let mut full = vec![Some(1), Some(2), Some(3)];
        na_outside(&mut full, &[true, false, true]);
        assert_eq!(full, vec![Some(1), None, Some(3)]);
        // A zero-length `applicable` selects nothing, so nothing is blanked.
        let mut kept = vec![Some(1), Some(2)];
        na_outside(&mut kept, &[]);
        assert_eq!(kept, vec![Some(1), Some(2)]);
        // `out <- rep(NA, n); out[applicable] <- values[applicable]`.
        assert_eq!(
            keep_applicable(&[10, 20, 30], &[false, true, false], 3),
            vec![None, Some(20), None]
        );
        assert_eq!(keep_applicable(&[10, 20], &[], 2), vec![None, None]);
    }
}

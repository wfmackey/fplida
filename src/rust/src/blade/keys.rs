//! BLADE key products (Stage 5 of the R->Rust port).
//!
//! Ports `.make_blade_id_bn_key` and `.make_blade_cn_bn_key`. Both key products
//! read the business spine and nothing else, so they need neither the classifier
//! nor the table generators.
//!
//! Every value that comes out of the CSV metadata -- the version literals and
//! the time-series ids -- is resolved in R and passed down, because
//! `getOption("fplida.blade_metadata_dir")` can point the whole metadata set
//! somewhere else mid-session. Nothing here is hard-coded from the shipped
//! files.
//!
//! RNG-free, like the rest of the module: every draw is a closed-form hash of
//! the business identifier, which already carries the build seed.

use extendr_api::prelude::*;

use super::helpers::{id_number, stable_name_seed};

/// The `match` value for the non-profiled population. The data item list
/// (keys.csv, Appendix A1) defines it as exactly that population, so it follows
/// from `is_profiled` rather than from a draw.
const MATCH_NON_PROFILED: &str = "NPP";

/// The six `match` values open to a profiled business, in the order the data
/// item list gives them: the single-unit case first, then the four ANZSIC06
/// match levels from finest to coarsest, then the enterprise-group residual.
const MATCH_PROFILED: [&str; 6] = ["One-TAU-BG", "Class", "Group", "SubDiv", "Div", "BG Match"];

/// Cumulative shares in percent over `MATCH_PROFILED`. A modelling choice: the
/// ABS publishes the seven-value frame but no distribution over it, and the
/// business spine gives every business a four-digit ANZSIC06 code, so there is
/// no ANZSIC depth to condition on either. The shape reasons from the
/// definitions -- most profiled enterprise groups hold a single type-of-activity
/// unit, so "One-TAU-BG" dominates; among groups that hold several, a match at
/// the finer ANZSIC level succeeds more often than one that has to fall back a
/// level, so Class > Group > SubDiv > Div; "BG Match" takes the units that
/// cannot be matched on industry at all.
const MATCH_PROFILED_CUMULATIVE: [i64; 6] = [55, 73, 83, 90, 94, 100];

/// Share, per thousand profiled businesses, whose ABN-to-TAU flags are recorded
/// as missing. The data item list gives both flags the frame "1 = Yes 0 = No
/// . = Missing"; the size of the missing share is a modelling choice, set small
/// so the flags stay usable.
const TAU_MISSING_PER_MILLE: i64 = 12;

/// Multiplier on the business identifier that picks the missing-flag rows.
/// The identifier steps by a constant from business to business, so a linear
/// hash of it walks its range evenly.
const TAU_MISSING_HASH_MULT: i64 = 91;

/// The `many_abn_to_one_tau` residues. Several ABNs rolling up to one
/// type-of-activity unit is a consolidation case, so it attaches to the
/// single-unit match rather than to the ANZSIC ladder.
const MANY_ABN_RESIDUES: [i64; 2] = [1, 2];

/// One business's key-product draw: the match category and the two ABN-to-TAU
/// flags, which the data item list ties to each other.
struct MatchDraw {
    match_value: &'static str,
    one_abn_to_many_tau: Rint,
    many_abn_to_one_tau: Rint,
}

/// Derive the match type and the two flags from the business identifier.
///
/// The flags follow from the match type rather than being drawn separately:
/// an ABN matched at an ANZSIC level is by definition an ABN whose activity
/// spans several type-of-activity units, and the enterprise-group residual is
/// by definition a unit whose ABN could not be told apart from its group's.
///
/// `name_hash` is `stable_name_seed` of the identifier string, not a linear
/// hash of its digits. Identifiers step by a constant, so a linear hash steps
/// by a constant too, and only about a fifth of businesses are profiled -- the
/// two periods beat against each other and skew the shares badly enough to
/// halve some of them. Hashing the digits through their positions breaks the
/// progression and holds every share within half a point of its target.
fn match_draw(idn: f64, name_hash: i64, profiled: bool) -> MatchDraw {
    if !profiled {
        return MatchDraw {
            match_value: MATCH_NON_PROFILED,
            one_abn_to_many_tau: Rint::from(0),
            many_abn_to_one_tau: Rint::from(0),
        };
    }

    let draw = name_hash.rem_euclid(100);
    let mut level = MATCH_PROFILED.len() - 1;
    for (i, &cut) in MATCH_PROFILED_CUMULATIVE.iter().enumerate() {
        if draw < cut {
            level = i;
            break;
        }
    }

    // f64 before the modulus, as everywhere else in the port: that is what R
    // does, and it never overflows.
    let missing = ((idn * TAU_MISSING_HASH_MULT as f64) % 1000.0) < TAU_MISSING_PER_MILLE as f64;
    if missing {
        return MatchDraw {
            match_value: MATCH_PROFILED[level],
            one_abn_to_many_tau: Rint::na(),
            many_abn_to_one_tau: Rint::na(),
        };
    }

    let (one, many) = match level {
        // One TAU in the group: the ABN cannot span several units, but several
        // ABNs can roll up to the group's single unit.
        0 => {
            let residue = (idn % 7.0) as i64;
            (0, MANY_ABN_RESIDUES.contains(&residue) as i32)
        }
        // Class, Group, SubDiv, Div: the ABN was matched to one of several
        // units by industry, which is what one-to-many means.
        1..=4 => (1, 0),
        // No industry match, group membership only.
        _ => (0, 1),
    };

    MatchDraw {
        match_value: MATCH_PROFILED[level],
        one_abn_to_many_tau: Rint::from(one),
        many_abn_to_one_tau: Rint::from(many),
    }
}

/// Build the BLADE ID-to-BN key: one block of `bn.len()` rows per time-series
/// id, in `tsids` order. `tsids` arrives already de-duplicated, so a metadata
/// edit that collapses the two ids gives one block rather than a repeated one.
/// @export
#[extendr]
fn make_blade_id_bn_key__(
    bn: Strings,
    id: Strings,
    bg_id: Strings,
    is_profiled: &[i32],
    key_version: &str,
    tsids: Strings,
) -> List {
    let bn: Vec<String> = bn.iter().map(|x| x.to_string()).collect();
    let id: Vec<String> = id.iter().map(|x| x.to_string()).collect();
    let bg_id: Vec<String> = bg_id.iter().map(|x| x.to_string()).collect();
    let tsids: Vec<String> = tsids.iter().map(|x| x.to_string()).collect();

    let n = bn.len();
    let n_tsid = tsids.len();
    let total = n * n_tsid;

    // R re-derives the identifier number once per block and once per flag,
    // four times over. Once is enough.
    let id_strs: Vec<&str> = id.iter().map(|s| s.as_str()).collect();
    let idn = id_number(&id_strs);

    let draws: Vec<MatchDraw> = (0..n)
        .map(|i| match_draw(idn[i], stable_name_seed(&id[i]), is_profiled[i] == 1))
        .collect();

    let mut out_bn: Vec<String> = Vec::with_capacity(total);
    let mut out_id: Vec<String> = Vec::with_capacity(total);
    let mut out_bg: Vec<String> = Vec::with_capacity(total);
    let mut out_tsid: Vec<String> = Vec::with_capacity(total);
    let mut out_one: Vec<Rint> = Vec::with_capacity(total);
    let mut out_many: Vec<Rint> = Vec::with_capacity(total);
    let mut out_match: Vec<String> = Vec::with_capacity(total);

    for tsid in &tsids {
        for i in 0..n {
            out_bn.push(bn[i].clone());
            out_id.push(id[i].clone());
            // Non-profiled businesses carry "" here, never NA: the metadata
            // reads a blank as "unit is not part of a BLADE Enterprise Group".
            out_bg.push(bg_id[i].clone());
            out_tsid.push(tsid.clone());
            out_one.push(draws[i].one_abn_to_many_tau);
            out_many.push(draws[i].many_abn_to_one_tau);
            out_match.push(draws[i].match_value.to_string());
        }
    }

    list!(
        bn = out_bn,
        id = out_id,
        bg_id = out_bg,
        key_version = vec![key_version.to_string(); total],
        tsid = out_tsid,
        one_abn_to_many_tau = out_one,
        many_abn_to_one_tau = out_many,
        // `match` is a Rust keyword; R renames this back.
        match_ = out_match
    )
}

/// Build the BLADE CN-to-BN key: one row per business, in spine order.
/// @export
#[extendr]
fn make_blade_cn_bn_key__(cn: Strings, bn: Strings, cn_bn_version: &str, tsid: &str) -> List {
    let cn: Vec<String> = cn.iter().map(|x| x.to_string()).collect();
    let bn: Vec<String> = bn.iter().map(|x| x.to_string()).collect();
    let n = cn.len();
    list!(
        cn = cn,
        bn = bn,
        cn_bn_version = vec![cn_bn_version.to_string(); n],
        tsid = vec![tsid.to_string(); n]
    )
}

extendr_module! {
    mod keys;
    fn make_blade_id_bn_key__;
    fn make_blade_cn_bn_key__;
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A business identifier as the spine builds it, for row `i`.
    fn spine_id(i: i64) -> String {
        super::super::helpers::numeric_id("E", ((i * 100_003 + 7) % 1_000_000_000) as i128, 9)
    }

    #[test]
    fn non_profiled_is_npp_with_both_flags_off() {
        let d = match_draw(1234.0, stable_name_seed(&spine_id(1)), false);
        assert_eq!(d.match_value, "NPP");
        assert_eq!(d.one_abn_to_many_tau, Rint::from(0));
        assert_eq!(d.many_abn_to_one_tau, Rint::from(0));
    }

    #[test]
    fn profiled_spans_the_published_frame() {
        let mut seen: Vec<&str> = (0..400)
            .map(|i| {
                let id = spine_id(i);
                match_draw(i as f64 * 100_003.0 + 7.0, stable_name_seed(&id), true).match_value
            })
            .collect();
        seen.sort_unstable();
        seen.dedup();
        for value in MATCH_PROFILED {
            assert!(seen.contains(&value), "{} missing", value);
        }
    }

    #[test]
    fn flags_agree_with_the_match_type() {
        for i in 0..500 {
            let id = spine_id(i);
            let d = match_draw(i as f64 * 100_003.0 + 3.0, stable_name_seed(&id), true);
            if d.one_abn_to_many_tau.is_na() {
                assert!(d.many_abn_to_one_tau.is_na());
                continue;
            }
            let one: Option<i32> = d.one_abn_to_many_tau.into();
            let many: Option<i32> = d.many_abn_to_one_tau.into();
            let (one, many) = (one.unwrap(), many.unwrap());
            match d.match_value {
                "One-TAU-BG" => assert_eq!(one, 0),
                "BG Match" => {
                    assert_eq!(one, 0);
                    assert_eq!(many, 1);
                }
                _ => {
                    assert_eq!(one, 1);
                    assert_eq!(many, 0);
                }
            }
        }
    }
}

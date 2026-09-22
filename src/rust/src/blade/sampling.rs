//! BLADE row samplers (Stage 4 of the R->Rust port).
//!
//! Ports `.select_blade_rows` and `.select_blade_frame_rows` from
//! `R/generate_blade.R`. Both pick a deterministic subset of rows for the
//! survey tables; the administrative tables ask for every row and short-circuit
//! before either key is built.
//!
//! The two keys differ by one term and the difference is deliberate:
//! `.select_blade_rows` is salted by the product name and table number so two
//! surveys drawn from the same business spine do not enrol the same
//! businesses, while `.select_blade_frame_rows` samples the employee link,
//! whose rows have no product or table identity of their own. Do not unify
//! them.

use extendr_api::prelude::*;

use super::helpers::{r_mod, stable_name_seed};

/// R's linear-congruential multiplier, as used by both keys.
const KEY_MULTIPLIER: f64 = 1_103_515_245.0;
/// The largest 32-bit signed prime, the modulus both keys reduce to.
const KEY_MODULUS: f64 = 2_147_483_647.0;

/// R `target <- max(1L, min(n, as.integer(min(ceiling(n * rate), max_rows))))`.
/// `max_rows` arrives as `Inf` for the administrative tables, so the
/// finiteness test has to be explicit.
fn target_rows(n: usize, sample_rate: f64, max_rows: f64) -> usize {
    let mut target = (n as f64 * sample_rate).ceil();
    if max_rows.is_finite() {
        target = target.min(max_rows.trunc());
    }
    if !target.is_finite() {
        return n;
    }
    (target.max(1.0) as usize).min(n)
}

/// R `sort(order(key)[seq_len(target)])`, returning 1-based row numbers.
///
/// `order()` picks the radix method for a plain double vector, which is
/// stable: equal keys keep their original ascending row order. `sort_by` is
/// stable too, so the two agree.
fn smallest_by_key(key: &[f64], target: usize) -> Vec<i32> {
    let mut idx: Vec<usize> = (0..key.len()).collect();
    idx.sort_by(|&a, &b| {
        key[a]
            .partial_cmp(&key[b])
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    idx.truncate(target);
    idx.sort_unstable();
    idx.into_iter().map(|i| (i as i32) + 1).collect()
}

/// `.select_blade_rows` in Rust: which business-spine rows a table takes.
/// @export
#[extendr]
fn blade_select_rows__(
    n: i32,
    table_number: i32,
    product_name: &str,
    seed: i32,
    sample_rate: f64,
    max_rows: f64,
) -> Robj {
    let n = n.max(0) as usize;
    if n == 0 {
        return Vec::<i32>::new().into();
    }
    let target = target_rows(n, sample_rate, max_rows);
    if target >= n {
        return (1..=(n as i32)).collect::<Vec<i32>>().into();
    }
    let salt = stable_name_seed(product_name) + (table_number as i64) * 1009;
    let key: Vec<f64> = (0..n)
        .map(|i| {
            r_mod(
                ((i as f64) + 1.0) * KEY_MULTIPLIER + (seed as f64) * 12345.0 + salt as f64,
                KEY_MODULUS,
            )
        })
        .collect();
    smallest_by_key(&key, target).into()
}

/// `.select_blade_frame_rows` in Rust: which rows of an already-built frame
/// survive. Unsalted, unlike `blade_select_rows__`.
/// @export
#[extendr]
fn blade_select_frame_rows__(n: i32, seed: i32, sample_rate: f64, max_rows: f64) -> Robj {
    let n = n.max(0) as usize;
    if n == 0 {
        return Vec::<i32>::new().into();
    }
    let target = target_rows(n, sample_rate, max_rows);
    if target >= n {
        return (1..=(n as i32)).collect::<Vec<i32>>().into();
    }
    let key: Vec<f64> = (0..n)
        .map(|i| {
            r_mod(
                ((i as f64) + 1.0) * KEY_MULTIPLIER + (seed as f64) * 12345.0,
                KEY_MODULUS,
            )
        })
        .collect();
    smallest_by_key(&key, target).into()
}

extendr_module! {
    mod sampling;
    fn blade_select_rows__;
    fn blade_select_frame_rows__;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn target_follows_r_truncation() {
        assert_eq!(target_rows(20, 0.5, 6.0), 6);
        assert_eq!(target_rows(20, 0.5, f64::INFINITY), 10);
        assert_eq!(target_rows(20, 1.0, f64::INFINITY), 20);
        // ceiling, then a floor of one row.
        assert_eq!(target_rows(3, 0.1, f64::INFINITY), 1);
        assert_eq!(target_rows(20, 0.5, 0.0), 1);
    }

    #[test]
    fn golden_values_match_the_r_helpers() {
        // Produced by running the live R helpers at seed 27 over 20 rows.
        let salt = stable_name_seed("blade-table-08-business-characteristics-survey-bcs") + 8 * 1009;
        assert_eq!(salt, 35_654);
        let key: Vec<f64> = (0..20)
            .map(|i| {
                r_mod(
                    ((i as f64) + 1.0) * KEY_MULTIPLIER + 27.0 * 12345.0 + salt as f64,
                    KEY_MODULUS,
                )
            })
            .collect();
        assert_eq!(smallest_by_key(&key, 6), vec![2, 4, 6, 8, 10, 12]);
        let unsalted: Vec<f64> = (0..20)
            .map(|i| {
                r_mod(
                    ((i as f64) + 1.0) * KEY_MULTIPLIER + 27.0 * 12345.0,
                    KEY_MODULUS,
                )
            })
            .collect();
        assert_eq!(smallest_by_key(&unsalted, 6), vec![2, 4, 6, 8, 10, 12]);
    }
}

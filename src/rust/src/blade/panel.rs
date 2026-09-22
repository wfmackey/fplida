//! Panel expansion for the BLADE tables whose grain is one row per business
//! per reference period.
//!
//! A table generated as a single-period snapshot has no time dimension at all:
//! every business appears once, carrying whatever the spine drew for it, with a
//! `tsid` that never varies. This module supplies the three per-period
//! decisions that turn such a table into a panel -- which businesses are on the
//! file for a given year, what their employment looks like that year, and which
//! business-years carry no PAYG data.
//!
//! Like the rest of the port these are RNG-free: every draw is a closed-form
//! `(seq * const + seed + salt) %% mod` on the business identifier, so a
//! business's whole trajectory is reproducible from its `bn` alone and a rebuilt
//! run gives byte-identical output.

use extendr_api::prelude::*;

use super::helpers::{id_number, r_mod, round1};
use super::rows::strings_to_vec;

/// The calendar year the business spine draws employment and money at; it is
/// `.NOMINAL_ANCHOR` in `R/nominal.R`. A financial year is named here by the
/// calendar year it STARTS in, which is `end_year - 1`, so the trajectory below
/// measures its distance from the anchor the same way `.blade_nominal_period()`
/// does.
pub const EMPLOYMENT_ANCHOR_YEAR: i32 = 2021;

/// A business's own employment trend, per year, as a proportion. Drawn once per
/// business and held for its whole life, so employment walks rather than being
/// redrawn each period.
///
/// A modelling choice. Nothing in the BLADE delivery states a growth
/// distribution, and the range is set so that a business two decades from the
/// anchor is between roughly a third and three times its anchor size -- wide
/// enough for the panel to carry real size variation, narrow enough that a
/// twenty-four year panel does not produce employment counts the spine's
/// turnover could not support.
const TREND_MIN: f64 = -0.05;
const TREND_SPAN: f64 = 0.11;
const TREND_MOD: f64 = 1000.0;
const TREND_MULT: f64 = 7919.0;
const TREND_SALT: f64 = 5101.0;

/// The year-to-year departure from that trend, plus or minus this proportion.
/// Employment is lumpy at the level of a single business -- one hire moves a
/// three-person firm by a third -- but the wobble is deliberately small next to
/// the trend so that a run of years still reads as one business rather than as
/// independent draws.
///
/// The draw is linear in the year, as every draw in this port is, so the
/// departure carries a component common to all businesses in a year: a shared
/// cycle on top of each business's own trend and level. That is a fair enough
/// picture of aggregate employment, and rounding at small headcounts breaks the
/// lockstep, but it is a property of the shape rather than an accident of it.
const WOBBLE_SPAN: f64 = 0.06;
const WOBBLE_MOD: f64 = 200.0;
const WOBBLE_MULT: f64 = 6151.0;
const WOBBLE_SALT: f64 = 5107.0;
const WOBBLE_YEAR_MULT: f64 = 104_729.0;

/// Business-years in a thousand for which PAYG holds no data.
///
/// `Valid.Response` for `fte` and `hcnt` documents `. = No PAYG data`, so the
/// case has to occur. A business is on the PAYG file because it is registered
/// to withhold, which is not the same as having withheld: a registration kept
/// open through a year with no employees, a year of a labour-hire arrangement,
/// or a return not lodged by the extract date all leave the row present and the
/// two employment items empty. Forty in a thousand is a modelling choice -- the
/// ABS publishes no such share -- kept small so the panel stays usable while the
/// documented empty case is exercised on every build.
const NO_PAYG_PER_MILLE: f64 = 40.0;
const NO_PAYG_MOD: f64 = 1000.0;
const NO_PAYG_MULT: f64 = 3607.0;
const NO_PAYG_SALT: f64 = 5113.0;
const NO_PAYG_YEAR_MULT: f64 = 97_711.0;

/// Which businesses belong on a panel table's file for the financial year
/// ending `end_year`.
///
/// A financial year ending in Y runs from 1 July Y-1 to 30 June Y. The business
/// spine records a birth and an exit as calendar years with no month, so the
/// first year a business is certainly trading throughout is the one ending
/// `birth_year + 1`, and the last is the one ending in its exit year. A missing
/// birth year is a business whose start the register never recorded, so it
/// carries no start constraint; a missing exit year is a business still
/// operating.
///
/// NA arrives from R as `i32::MIN`, which compares as a very large negative
/// year, so both are tested for explicitly rather than by sign.
pub fn active_in_year(birth_year: &[i32], exit_year: &[i32], end_year: i32) -> Vec<bool> {
    let n = birth_year.len().max(exit_year.len());
    (0..n)
        .map(|i| {
            let birth = birth_year.get(i).copied().unwrap_or(i32::MIN);
            let exit = exit_year.get(i).copied().unwrap_or(i32::MIN);
            let started = birth == i32::MIN || end_year >= birth + 1;
            let still_open = exit == i32::MIN || end_year <= exit;
            started && still_open
        })
        .collect()
}

/// The multiplier on a business's anchor-year employment for the financial year
/// ending `end_year`: its own compounding trend, plus a bounded departure that
/// changes from year to year.
fn employment_scale(key: f64, seed: i64, end_year: i32) -> f64 {
    let seed = seed as f64;
    let trend = TREND_MIN
        + TREND_SPAN * (r_mod(key * TREND_MULT + seed + TREND_SALT, TREND_MOD) / TREND_MOD);
    // A financial year is named by the calendar year it starts in, matching
    // `.blade_nominal_period()`, so the whole panel moves money and employment
    // away from the anchor in step.
    let k = f64::from(end_year - 1 - EMPLOYMENT_ANCHOR_YEAR);
    let wobble_draw = r_mod(
        key * WOBBLE_MULT + f64::from(end_year) * WOBBLE_YEAR_MULT + seed + WOBBLE_SALT,
        WOBBLE_MOD,
    );
    let wobble = WOBBLE_SPAN * (wobble_draw / WOBBLE_MOD - 0.5) * 2.0;
    ((1.0 + trend).powf(k) * (1.0 + wobble)).max(0.0)
}

/// One business-year's headcount, employee count and full-time equivalent.
pub struct PeriodEmployment {
    pub employment_count: Vec<i32>,
    pub hcnt: Vec<i32>,
    pub fte: Vec<f64>,
}

/// Move a business slice's employment items to the financial year ending
/// `end_year`.
///
/// `fte` is not drawn: it holds the business's own anchor-year ratio of
/// full-time equivalents to heads, so the two items move together and a reader
/// who divides one by the other gets a stable part-time share rather than
/// noise. An employing business keeps at least one head in every year it is on
/// the file; a non-employing one stays at zero.
pub fn period_employment(
    bn: &[Option<String>],
    employment_count: &[i32],
    hcnt: &[i32],
    fte: &[f64],
    seed: i64,
    end_year: i32,
) -> PeriodEmployment {
    let owned: Vec<String> = bn.iter().map(|v| v.clone().unwrap_or_default()).collect();
    let refs: Vec<&str> = owned.iter().map(String::as_str).collect();
    let keys = id_number(&refs);
    let n = keys.len();

    let mut out_employment = Vec::with_capacity(n);
    let mut out_hcnt = Vec::with_capacity(n);
    let mut out_fte = Vec::with_capacity(n);

    for i in 0..n {
        let scale = employment_scale(keys[i], seed, end_year);
        let base_hcnt = hcnt.get(i).copied().unwrap_or(i32::MIN);
        let base_employment = employment_count.get(i).copied().unwrap_or(i32::MIN);
        let base_fte = fte.get(i).copied().unwrap_or(f64::NAN);

        let moved = |base: i32| -> i32 {
            if base == i32::MIN {
                return i32::MIN;
            }
            if base <= 0 {
                return 0;
            }
            let scaled = (f64::from(base) * scale).round();
            (scaled as i32).max(1)
        };
        let year_hcnt = moved(base_hcnt);
        let year_employment = moved(base_employment);

        // The anchor ratio, not a fresh draw. A business with no heads has no
        // ratio to keep, so its full-time equivalent stays at zero too, and a
        // business whose headcount the spine never recorded keeps both empty.
        let year_fte = if year_hcnt == i32::MIN || base_fte.is_nan() {
            f64::NAN
        } else if year_hcnt == 0 || base_hcnt <= 0 {
            0.0
        } else {
            round1(f64::from(year_hcnt) * (base_fte / f64::from(base_hcnt)))
        };

        out_employment.push(year_employment);
        out_hcnt.push(year_hcnt);
        out_fte.push(year_fte);
    }

    PeriodEmployment {
        employment_count: out_employment,
        hcnt: out_hcnt,
        fte: out_fte,
    }
}

/// The business-years whose PAYG employment items are empty.
pub fn no_payg_data(bn: &[Option<String>], seed: i64, end_year: i32) -> Vec<bool> {
    let owned: Vec<String> = bn.iter().map(|v| v.clone().unwrap_or_default()).collect();
    let refs: Vec<&str> = owned.iter().map(String::as_str).collect();
    id_number(&refs)
        .into_iter()
        .map(|key| {
            let draw = r_mod(
                key * NO_PAYG_MULT
                    + f64::from(end_year) * NO_PAYG_YEAR_MULT
                    + seed as f64
                    + NO_PAYG_SALT,
                NO_PAYG_MOD,
            );
            draw < NO_PAYG_PER_MILLE
        })
        .collect()
}

/// Which rows of a business slice belong on a panel table's file for one year.
/// @export
#[extendr]
fn blade_panel_active__(birth_year: &[i32], exit_year: &[i32], end_year: i32) -> Vec<i32> {
    active_in_year(birth_year, exit_year, end_year)
        .into_iter()
        .map(i32::from)
        .collect()
}

/// One year of a business slice's employment items.
/// @export
#[extendr]
fn blade_panel_employment__(
    bn: Strings,
    employment_count: &[i32],
    hcnt: &[i32],
    fte: &[f64],
    seed: i32,
    end_year: i32,
) -> List {
    let out = period_employment(
        &strings_to_vec(&bn),
        employment_count,
        hcnt,
        fte,
        seed as i64,
        end_year,
    );
    list!(
        employment_count = out.employment_count,
        hcnt = out.hcnt,
        fte = out.fte
    )
}

/// The business-years PAYG holds no data for.
/// @export
#[extendr]
fn blade_panel_no_payg_data__(bn: Strings, seed: i32, end_year: i32) -> Vec<i32> {
    no_payg_data(&strings_to_vec(&bn), seed as i64, end_year)
        .into_iter()
        .map(i32::from)
        .collect()
}

extendr_module! {
    mod panel;
    fn blade_panel_active__;
    fn blade_panel_employment__;
    fn blade_panel_no_payg_data__;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bns(n: usize) -> Vec<Option<String>> {
        (1..=n)
            .map(|i| Some(format!("BN{:011}", i * 3851)))
            .collect()
    }

    #[test]
    fn a_business_is_absent_before_it_starts_and_after_it_ends() {
        let birth = [2010, 2010, i32::MIN, 1995];
        let exit = [i32::MIN, 2015, 2008, i32::MIN];
        assert_eq!(
            active_in_year(&birth, &exit, 2010),
            vec![false, false, false, true]
        );
        assert_eq!(
            active_in_year(&birth, &exit, 2008),
            vec![false, false, true, true]
        );
        assert_eq!(
            active_in_year(&birth, &exit, 2011),
            vec![true, true, false, true]
        );
        assert_eq!(
            active_in_year(&birth, &exit, 2016),
            vec![true, false, false, true]
        );
    }

    #[test]
    fn employment_moves_but_keeps_the_full_time_ratio() {
        let bn = bns(60);
        let employment: Vec<i32> = (0..60).map(|i| 2 + (i % 40)).collect();
        let hcnt: Vec<i32> = employment.clone();
        let fte: Vec<f64> = hcnt.iter().map(|h| f64::from(*h) * 0.8).collect();

        // At the anchor the trend has compounded over no years at all, so only
        // the bounded year departure separates the panel from the spine.
        let anchor = period_employment(&bn, &employment, &hcnt, &fte, 42, 2022);
        for i in 20..60 {
            let step = (f64::from(anchor.hcnt[i]) / f64::from(hcnt[i]) - 1.0).abs();
            assert!(step <= WOBBLE_SPAN + 0.02, "step {} at {}", step, i);
        }

        let later = period_employment(&bn, &employment, &hcnt, &fte, 42, 2010);
        assert!(later.hcnt.iter().zip(&hcnt).any(|(a, b)| a != b));
        for i in 0..60 {
            assert!(later.hcnt[i] >= 1);
            let ratio = later.fte[i] / f64::from(later.hcnt[i]);
            assert!((ratio - 0.8).abs() < 0.06, "ratio {} at {}", ratio, i);
        }
    }

    #[test]
    fn the_trajectory_is_smooth_rather_than_redrawn() {
        let bn = bns(200);
        let employment: Vec<i32> = (0..200).map(|i| 20 + (i % 30)).collect();
        let fte: Vec<f64> = employment.iter().map(|h| f64::from(*h) * 0.75).collect();
        let a = period_employment(&bn, &employment, &employment, &fte, 7, 2012);
        let b = period_employment(&bn, &employment, &employment, &fte, 7, 2013);
        // A year apart, no business should move by more than about a fifth.
        for i in 0..200 {
            let step = (f64::from(b.hcnt[i]) / f64::from(a.hcnt[i]) - 1.0).abs();
            assert!(step < 0.22, "step {} at {}", step, i);
        }
    }

    #[test]
    fn the_no_data_share_is_small_and_non_zero() {
        let bn = bns(5000);
        let flagged: usize = no_payg_data(&bn, 42, 2015).into_iter().filter(|x| *x).count();
        let share = flagged as f64 / 5000.0;
        assert!(share > 0.01 && share < 0.09, "share {}", share);
        // Different years flag different businesses.
        let other = no_payg_data(&bn, 42, 2016);
        assert!(no_payg_data(&bn, 42, 2015) != other);
    }

    #[test]
    fn missing_employment_stays_missing() {
        let bn = bns(3);
        let employment = [i32::MIN, 0, 5];
        let hcnt = [i32::MIN, 0, 5];
        let fte = [f64::NAN, 0.0, 4.0];
        let out = period_employment(&bn, &employment, &hcnt, &fte, 42, 2005);
        assert_eq!(out.hcnt[0], i32::MIN);
        assert!(out.fte[0].is_nan());
        assert_eq!(out.hcnt[1], 0);
        assert_eq!(out.fte[1], 0.0);
        assert!(out.hcnt[2] >= 1);
    }
}

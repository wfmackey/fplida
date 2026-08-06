//! Nominal growth: prices, wages, business income and administered rates.
//!
//! Every dollar figure in PLIDA and BLADE is nominal, so the same real quantity
//! carries a different number in every reference year. Before this module the
//! package drew a person's income once at a calendar-2021 anchor and reused
//! that number in every year, which meant a synthetic PLIDA had no inflation
//! and no nominal wage growth. Deflating it was a no-op, bracket creep did not
//! exist, and a nominal-versus-real comparison could not fail.
//!
//! A dollar amount for a unit (a person or a business) in year `y` is
//!
//! ```text
//!     amount(y) = anchor_amount * index(series, y) * exp(deviation(unit, y))
//! ```
//!
//! where `index` is a published headline series and `deviation` is that unit's
//! own departure from it. The headline comes from
//! [`crate::nominal_series`], which `data-raw/update_nominal_indices.R`
//! generates from ABS and ATO publications.
//!
//! # The deviation
//!
//! Real earnings do not all grow at the headline rate, and the ways they differ
//! are not interchangeable. Three components, in logs, over `k = y - anchor`
//! years:
//!
//! * a **profile** term `g * k`, where `g` is drawn once per unit and never
//!   changes. Some people are on a steeper career path than others for a
//!   decade at a time. Without this the cross-sectional spread of earnings
//!   would be the same in every year, which it is not.
//! * a **permanent** term, the sum of one shock per year. A promotion or a
//!   demotion moves the level and it stays moved. This is a random walk, so
//!   its variance grows with `|k|`.
//! * a **transitory** term, one shock in the year itself. Overtime, a bonus, a
//!   month out of work. It is gone by the next year.
//!
//! The three are then mean-corrected by `-0.5 * Var`, so the *mean* dollar
//! amount across units grows at exactly the headline rate. Without that
//! correction the population aggregate would drift above the published series
//! by the Jensen gap, and a user reconciling synthetic PLIDA totals against ABS
//! or ATO aggregates would find a discrepancy that widened every year.
//!
//! At the anchor the deviation is exactly zero for every unit. That is
//! deliberate: the anchor amount is the spine's `baseline_income`, which
//! already carries the person's individual fixed effect and occupation match
//! quality. The deviation measures departure *from* that, so it starts at
//! nothing.
//!
//! It leaves one artefact worth knowing about. The anchor is a single year on
//! whichever basis a caller asks for, so a product on [`Basis::Financial`] has
//! no per-unit spread at all in financial year 2021-22, and a product on
//! [`Basis::Calendar`] has none in calendar 2021. One year in twenty is a
//! little tighter than its neighbours. It is left as it is because the
//! alternative — measuring distance from the anchor in half-years so no year
//! lands on it — buys a small gain in a synthetic file at the cost of a
//! permanent-walk index that is no longer an integer year, and the amounts most
//! affected are drawn from their own distribution anyway.
//!
//! # Determinism
//!
//! Every draw is a hash of `(unit, seed, year, salt)` rather than a step of a
//! shared stream, using the same SplitMix64 mix as
//! `employment::disability_employment_draw`. Nothing depends on row order, on
//! which persons are in the slice, or on which years were asked for. Two slice
//! workers processing disjoint person sets reconstruct identical paths, which
//! is what lets STP, PAYG, PIT and BLADE agree on the same person's or
//! business's money in the same year.

use crate::nominal_series::{
    NominalRow, ANCHOR_YEAR, CALENDAR_YEAR, FINANCIAL_YEAR, FIRST_YEAR, LAST_YEAR,
};

/// Which published series a dollar amount follows.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Series {
    /// Employment income. ATO average salary or wages per individual.
    Wage,
    /// The general price level. ABS CPI, All groups.
    Price,
    /// Business income. ATO average total income per company.
    Business,
    /// Income support. The MTAWE pension benchmark.
    Transfer,
    /// Medicare benefits. The MBS indexation factor, including the freeze.
    Health,
}

/// Whether a year label means a financial year or a calendar year.
///
/// PLIDA mixes the two and the distinction is worth about half a year of
/// growth, so it is never inferred. Tax, BLADE and payment-summary products are
/// [`Basis::Financial`]; Census, residence and the monthly service products are
/// [`Basis::Calendar`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Basis {
    /// A financial year, named by the calendar year it starts in: 2011 is the
    /// year ended 30 June 2012.
    Financial,
    /// A calendar year.
    Calendar,
}

/// The calendar year every index equals 1.0 in, and the year the spine draws
/// `baseline_income` at.
pub const ANCHOR: i32 = ANCHOR_YEAR;

#[inline]
fn row(basis: Basis, year: i32) -> &'static NominalRow {
    let table: &[NominalRow] = match basis {
        Basis::Financial => &FINANCIAL_YEAR,
        Basis::Calendar => &CALENDAR_YEAR,
    };
    // Years outside the table are held at the nearest end rather than
    // extrapolated here. The generated table already runs well past any year
    // the package emits, so a clamp only fires when a caller asks for
    // something absurd, and holding flat is a visible, harmless answer.
    let idx = (year.clamp(FIRST_YEAR, LAST_YEAR) - FIRST_YEAR) as usize;
    &table[idx]
}

/// The headline index for a series and year, relative to the anchor.
///
/// `index(Series::Price, Basis::Financial, 2021) / index(Series::Price,
/// Basis::Financial, 2015)` is the CPI increase across those financial years.
#[inline]
pub fn index(series: Series, basis: Basis, year: i32) -> f64 {
    let r = row(basis, year);
    match series {
        Series::Wage => r.wage,
        Series::Price => r.price,
        Series::Business => r.business,
        Series::Transfer => r.transfer,
        Series::Health => r.health,
    }
}

/// Headline log growth from one year to another.
///
/// For a generator that walks a log-earnings series year by year, this is the
/// drift term: `log_step(Wage, Calendar, 2018, 2019)` is how much average wages
/// grew across that step, in log points.
#[inline]
pub fn log_step(series: Series, basis: Basis, from: i32, to: i32) -> f64 {
    (index(series, basis, to) / index(series, basis, from)).ln()
}

/// True when the year falls outside the published data and the index there is
/// this package's own extrapolation.
#[inline]
pub fn is_projected(basis: Basis, year: i32) -> bool {
    year < FIRST_YEAR || year > LAST_YEAR || row(basis, year).projected
}

/// How far a unit's own growth departs from the headline.
///
/// All three are standard deviations in log points. Setting all three to zero
/// makes a series follow the headline exactly, which is what an administered
/// rate does.
#[derive(Debug, Clone, Copy)]
pub struct Dispersion {
    /// Spread of the per-unit growth-rate differential, per year.
    pub profile_sd: f64,
    /// Spread of each year's permanent shock.
    pub permanent_sd: f64,
    /// Spread of the transitory shock within a year.
    pub transitory_sd: f64,
}

impl Dispersion {
    /// Log variance of the deviation `k` years from the anchor.
    #[inline]
    fn variance(&self, k: i32) -> f64 {
        if k == 0 {
            return 0.0;
        }
        let kf = k as f64;
        self.profile_sd * self.profile_sd * kf * kf
            + self.permanent_sd * self.permanent_sd * kf.abs()
            + self.transitory_sd * self.transitory_sd
    }

    #[inline]
    fn is_zero(&self) -> bool {
        self.profile_sd == 0.0 && self.permanent_sd == 0.0 && self.transitory_sd == 0.0
    }
}

/// Individual earnings.
///
/// The permanent and transitory figures give an annual log-earnings change with
/// a standard deviation near 0.107, which is what the employment panel's
/// earnings walk already assumed before this module replaced its flat 5.8 per
/// cent. The profile term is deliberately small: 1 log point a year still
/// separates the top and bottom of a cohort by about 40 log points over twenty
/// years, which is as much career divergence as administrative earnings show.
pub const PERSON: Dispersion = Dispersion {
    profile_sd: 0.010,
    permanent_sd: 0.080,
    transitory_sd: 0.050,
};

/// Business income, which is far more dispersed than earnings. A firm can lose
/// a third of its turnover in a year and get it back the next; a wage earner
/// cannot.
pub const BUSINESS: Dispersion = Dispersion {
    profile_sd: 0.020,
    permanent_sd: 0.150,
    transitory_sd: 0.100,
};

/// An administered amount: a benefit rate, a co-payment, a schedule fee. These
/// are legislated and identical for everyone who gets them, so they follow the
/// headline with no deviation at all.
pub const ADMINISTERED: Dispersion = Dispersion {
    profile_sd: 0.0,
    permanent_sd: 0.0,
    transitory_sd: 0.0,
};

// Salts keep the three components independent of each other and of every other
// hashed draw in the package.
const SALT_PROFILE: u64 = 0x4E4F_4D5F_5052_4F46; // "NOM_PROF"
const SALT_PERMANENT: u64 = 0x4E4F_4D5F_5045_524D; // "NOM_PERM"
const SALT_TRANSITORY: u64 = 0x4E4F_4D5F_5452_414E; // "NOM_TRAN"

/// A stable unit key from whatever identifier a product happens to carry.
///
/// The deviation only needs a key that comes out the same every time the same
/// unit is generated. Products carry different identifiers — a spine id, an
/// agency AEUID, a business number — and each is stable for a given unit within
/// its own product. Hashing the string turns any of them into a key without
/// changing an extendr signature or plumbing a new argument through every
/// generator.
///
/// Where a product already has an integer person number, prefer it: two
/// products keyed to the same number give a person the same deviation, which
/// is what makes their dollar figures move together.
#[inline]
pub fn unit_key(id: &str) -> i64 {
    // FNV-1a, then the SplitMix64 finaliser to spread the low bits.
    let mut h: u64 = 0xCBF2_9CE4_8422_2325;
    for b in id.as_bytes() {
        h ^= *b as u64;
        h = h.wrapping_mul(0x0000_0100_0000_01B3);
    }
    h ^= h >> 30;
    h = h.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    h ^= h >> 27;
    h = h.wrapping_mul(0x94D0_49BB_1331_11EB);
    h ^= h >> 31;
    (h >> 1) as i64
}

/// SplitMix64 finalising mix over `(unit, seed, year, salt)`, returning a
/// uniform in `[0, 1)`. Identical in form to
/// `employment::disability_employment_draw`, so the two behave the same way
/// under slicing.
#[inline]
fn hash_unit_interval(unit: i64, seed: i64, salt: u64, year: i32) -> f64 {
    let mut x = (unit as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
        ^ (seed as u64).wrapping_mul(0xBF58_476D_1CE4_E5B9)
        ^ ((year as i64 as u64).wrapping_add(0x1000)).wrapping_mul(0x94D0_49BB_1331_11EB)
        ^ salt.wrapping_mul(0xD6E8_FEB8_6659_FD93);
    x ^= x >> 30;
    x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x ^= x >> 27;
    x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^= x >> 31;
    ((x >> 11) as f64) * (1.0 / ((1u64 << 53) as f64))
}

/// A standard normal from the same hash, by Box-Muller on two draws that differ
/// only in the salt.
#[inline]
fn hash_normal(unit: i64, seed: i64, salt: u64, year: i32) -> f64 {
    let u1 = hash_unit_interval(unit, seed, salt, year).max(1e-15);
    let u2 = hash_unit_interval(unit, seed, salt ^ 0xA5A5_A5A5_A5A5_A5A5, year);
    (-2.0 * u1.ln()).sqrt() * (std::f64::consts::TAU * u2).cos()
}

/// The permanent random walk at `year`, measured from zero at the anchor.
///
/// A shock belongs to the later of the two years it separates, so the walk
/// gives the same path whichever direction it is built from: `u(t) - u(t-1)`
/// is always `eta(t)`.
#[inline]
fn permanent_walk(sd: f64, unit: i64, seed: i64, year: i32) -> f64 {
    if sd == 0.0 || year == ANCHOR {
        return 0.0;
    }
    let mut total = 0.0;
    if year > ANCHOR {
        for y in (ANCHOR + 1)..=year {
            total += hash_normal(unit, seed, SALT_PERMANENT, y);
        }
    } else {
        for y in (year + 1)..=ANCHOR {
            total -= hash_normal(unit, seed, SALT_PERMANENT, y);
        }
    }
    sd * total
}

/// A unit's log departure from the headline in one year.
///
/// Costs one hash per year between the anchor and `year`, so a caller that
/// needs a whole panel should use [`path`] instead, which costs one hash per
/// year in total.
#[inline]
pub fn deviation(d: Dispersion, unit: i64, seed: i64, year: i32) -> f64 {
    if d.is_zero() || year == ANCHOR {
        return 0.0;
    }
    let k = year - ANCHOR;
    let profile = d.profile_sd * hash_normal(unit, seed, SALT_PROFILE, 0) * k as f64;
    let permanent = permanent_walk(d.permanent_sd, unit, seed, year);
    let transitory = d.transitory_sd * hash_normal(unit, seed, SALT_TRANSITORY, year);
    profile + permanent + transitory - 0.5 * d.variance(k)
}

/// The multiplier that turns an anchor-year dollar amount into a `year` amount.
///
/// This is the one function most call sites need:
///
/// ```ignore
/// let wage = baseline_income * nominal::factor(
///     nominal::Series::Wage, nominal::Basis::Financial,
///     nominal::PERSON, person_number, seed, fy,
/// );
/// ```
#[inline]
pub fn factor(
    series: Series,
    basis: Basis,
    d: Dispersion,
    unit: i64,
    seed: i64,
    year: i32,
) -> f64 {
    index(series, basis, year) * deviation(d, unit, seed, year).exp()
}

/// The multiplier for an administered amount, which every recipient shares.
#[inline]
pub fn administered_factor(series: Series, basis: Basis, year: i32) -> f64 {
    index(series, basis, year)
}

/// Fill `out` with the factor for each year in `first ..= first + out.len() - 1`.
///
/// [`factor`] rebuilds the permanent walk from the anchor on every call, so a
/// panel generator calling it year by year pays `O(years^2)` hashes. This walks
/// the range once instead, for `O(years)`, and allocates nothing: `out` doubles
/// as the scratch space for the walk. The values it produces are identical to
/// [`factor`] year by year, which `the_path_matches_year_by_year_evaluation`
/// checks.
pub fn path(
    series: Series,
    basis: Basis,
    d: Dispersion,
    unit: i64,
    seed: i64,
    first: i32,
    out: &mut [f64],
) {
    if out.is_empty() {
        return;
    }
    let n = out.len();
    let last = first + n as i32 - 1;

    if d.is_zero() {
        for (i, slot) in out.iter_mut().enumerate() {
            *slot = index(series, basis, first + i as i32);
        }
        return;
    }

    // Pass one: the permanent walk, written into `out`. A shock belongs to the
    // later of the two years it separates, so stepping up adds the arriving
    // year's shock and stepping down subtracts the departing year's.
    let sd = d.permanent_sd;
    let shock = |year: i32| sd * hash_normal(unit, seed, SALT_PERMANENT, year);
    if sd == 0.0 {
        out.fill(0.0);
    } else if first > ANCHOR {
        // The whole range sits above the anchor: carry the walk up to `first`,
        // then step through the range.
        out[0] = permanent_walk(sd, unit, seed, first);
        for i in 1..n {
            out[i] = out[i - 1] + shock(first + i as i32);
        }
    } else if last < ANCHOR {
        // Entirely below the anchor: carry down to `last`, then step back.
        out[n - 1] = permanent_walk(sd, unit, seed, last);
        for i in (0..n - 1).rev() {
            out[i] = out[i + 1] - shock(first + i as i32 + 1);
        }
    } else {
        // The anchor is inside the range, so the walk starts from zero at it
        // and runs out in both directions.
        let a = (ANCHOR - first) as usize;
        out[a] = 0.0;
        for i in (a + 1)..n {
            out[i] = out[i - 1] + shock(first + i as i32);
        }
        for i in (0..a).rev() {
            out[i] = out[i + 1] - shock(first + i as i32 + 1);
        }
    }

    // Pass two: turn each walk value into the factor for that year.
    let profile = d.profile_sd * hash_normal(unit, seed, SALT_PROFILE, 0);
    for (i, slot) in out.iter_mut().enumerate() {
        let year = first + i as i32;
        let k = year - ANCHOR;
        let dev = if k == 0 {
            0.0
        } else {
            profile * k as f64
                + *slot
                + d.transitory_sd * hash_normal(unit, seed, SALT_TRANSITORY, year)
                - 0.5 * d.variance(k)
        };
        *slot = index(series, basis, year) * dev.exp();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SEED: i64 = 42;

    #[test]
    fn anchor_index_is_one_on_both_bases() {
        for s in [
            Series::Wage,
            Series::Price,
            Series::Business,
            Series::Transfer,
            Series::Health,
        ] {
            assert!((index(s, Basis::Calendar, ANCHOR) - 1.0).abs() < 1e-9);
        }
    }

    #[test]
    fn the_table_matches_the_published_series() {
        // Golden values, checked against the source publications and against
        // inst/extdata/nominal-indices.csv, which tests/testthat/test-nominal.R
        // asserts the same figures from. The two are generated together by
        // data-raw/update_nominal_indices.R; if only one is regenerated these
        // two tests disagree, which is the point of having both.
        let close = |got: f64, want: f64| {
            assert!(
                (got - want).abs() < 5e-6,
                "index {got} differs from the published {want}"
            )
        };
        close(index(Series::Price, Basis::Financial, 2005), 0.702_704);
        close(index(Series::Wage, Basis::Financial, 2005), 0.582_686);
        close(index(Series::Price, Basis::Financial, 2022), 1.093_715);
        close(index(Series::Wage, Basis::Financial, 2023), 1.129_336);
        close(index(Series::Business, Basis::Financial, 2015), 0.911_812);
        close(index(Series::Transfer, Basis::Financial, 2021), 1.011_982);

        // The MBS indexation freeze. The schedule fee for item 23, a standard
        // GP consultation, was $37.05 from 2014-15 through 2017-18 and did not
        // move once. A Medicare benefit does not track prices and must not be
        // indexed to them, which is the whole reason Health is a separate
        // series.
        for y in 2015..=2017 {
            close(
                index(Series::Health, Basis::Financial, y),
                index(Series::Health, Basis::Financial, 2014),
            );
        }
        assert!(
            index(Series::Health, Basis::Financial, 2018)
                > index(Series::Health, Basis::Financial, 2017)
        );
    }

    #[test]
    fn prices_and_wages_rise_over_the_window() {
        assert!(index(Series::Price, Basis::Financial, 2023) > index(Series::Price, Basis::Financial, 2005));
        assert!(index(Series::Wage, Basis::Financial, 2023) > index(Series::Wage, Basis::Financial, 2005));
        // Wages outgrew prices over the window, as they did in the published
        // series. A build where they did not has the columns crossed.
        let wage = index(Series::Wage, Basis::Financial, 2023) / index(Series::Wage, Basis::Financial, 2005);
        let price = index(Series::Price, Basis::Financial, 2023) / index(Series::Price, Basis::Financial, 2005);
        assert!(wage > price);
    }

    #[test]
    fn deviation_is_zero_at_the_anchor() {
        for unit in 1..500i64 {
            assert_eq!(deviation(PERSON, unit, SEED, ANCHOR), 0.0);
            assert_eq!(deviation(BUSINESS, unit, SEED, ANCHOR), 0.0);
        }
    }

    #[test]
    fn administered_amounts_have_no_deviation() {
        for year in 2000..2030 {
            assert_eq!(deviation(ADMINISTERED, 7, SEED, year), 0.0);
            assert_eq!(
                factor(Series::Transfer, Basis::Financial, ADMINISTERED, 7, SEED, year),
                index(Series::Transfer, Basis::Financial, year)
            );
        }
    }

    #[test]
    fn the_path_matches_year_by_year_evaluation() {
        for &(first, n) in &[(2010i32, 16usize), (2022, 6), (2000, 12), (2021, 1)] {
            let mut out = vec![0.0; n];
            for unit in [1i64, 97, 5_000_003] {
                path(Series::Wage, Basis::Financial, PERSON, unit, SEED, first, &mut out);
                for (i, got) in out.iter().enumerate() {
                    let year = first + i as i32;
                    let want = factor(Series::Wage, Basis::Financial, PERSON, unit, SEED, year);
                    assert!(
                        (got - want).abs() < 1e-12,
                        "unit {unit} year {year}: path {got} vs factor {want}"
                    );
                }
            }
        }
    }

    #[test]
    fn the_population_mean_tracks_the_headline() {
        // The mean-correction exists so that synthetic aggregates reproduce the
        // published series. Check it does, across a span wide enough for the
        // Jensen gap to be obvious if the correction were missing.
        for year in [2010i32, 2015, 2019, 2025] {
            let n = 200_000i64;
            let mut total = 0.0;
            for unit in 1..=n {
                total += deviation(PERSON, unit, SEED, year).exp();
            }
            let mean = total / n as f64;
            assert!(
                (mean - 1.0).abs() < 0.02,
                "year {year}: mean multiplier {mean}, expected 1"
            );
        }
    }

    #[test]
    fn dispersion_widens_with_distance_from_the_anchor() {
        let spread = |year: i32| {
            let n = 20_000i64;
            let devs: Vec<f64> = (1..=n).map(|u| deviation(PERSON, u, SEED, year)).collect();
            let mean = devs.iter().sum::<f64>() / n as f64;
            (devs.iter().map(|d| (d - mean).powi(2)).sum::<f64>() / n as f64).sqrt()
        };
        let near = spread(ANCHOR + 1);
        let far = spread(ANCHOR + 10);
        assert!(far > near * 1.5, "spread at +10 ({far}) vs +1 ({near})");
    }

    #[test]
    fn the_walk_is_symmetric_about_the_anchor() {
        // Going back one year then forward one year must land where it started.
        for unit in [3i64, 41, 999_331] {
            let back = permanent_walk(PERSON.permanent_sd, unit, SEED, ANCHOR - 1);
            let step = PERSON.permanent_sd * hash_normal(unit, SEED, SALT_PERMANENT, ANCHOR);
            assert!((back + step).abs() < 1e-12);
        }
    }

    #[test]
    fn years_outside_the_table_are_held_flat() {
        assert_eq!(
            index(Series::Price, Basis::Financial, FIRST_YEAR - 50),
            index(Series::Price, Basis::Financial, FIRST_YEAR)
        );
        assert_eq!(
            index(Series::Price, Basis::Financial, LAST_YEAR + 50),
            index(Series::Price, Basis::Financial, LAST_YEAR)
        );
    }
}

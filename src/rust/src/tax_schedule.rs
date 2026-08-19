//! The personal income tax schedule, by financial year.
//!
//! Rates, thresholds and the low income tax offset are legislated and change,
//! and the window fplida generates spans three distinct schedules plus the
//! 2024-25 restructure. Applying one year's schedule to all of them means an
//! effective tax rate computed over the extract is flat by construction: the
//! largest change in the window, the Stage 3 cuts, is invisible, and a
//! difference-in-differences on a tax reform has no reform to find.
//!
//! `fy_end` throughout is the year a financial year ends, so 2021 is 2020-21.
//!
//! Sources: ATO, Individual income tax rates,
//! <https://www.ato.gov.au/tax-rates-and-codes/tax-rates-australian-residents>;
//! ATO, Low income tax offset,
//! <https://www.ato.gov.au/individuals-and-families/income-deductions-offsets-and-records/tax-offsets/low-and-middle-income-earner-tax-offsets>;
//! ATO, Medicare levy reduction,
//! <https://www.ato.gov.au/individuals-and-families/medicare-and-private-health-insurance/medicare-levy/medicare-levy-reduction>.

/// A bracket: income at or above `threshold` is taxed at `rate` on the excess
/// over it, up to the next threshold.
struct Bracket {
    threshold: f64,
    rate: f64,
}

/// Resident rates for a financial year.
fn resident_brackets(fy_end: i32) -> [Bracket; 4] {
    if fy_end >= 2025 {
        // Stage 3: the 19% rate falls to 16%, the 32.5% to 30%, and the top
        // two thresholds move to 135,000 and 190,000.
        [
            Bracket { threshold: 18_200.0, rate: 0.16 },
            Bracket { threshold: 45_000.0, rate: 0.30 },
            Bracket { threshold: 135_000.0, rate: 0.37 },
            Bracket { threshold: 190_000.0, rate: 0.45 },
        ]
    } else if fy_end >= 2021 {
        [
            Bracket { threshold: 18_200.0, rate: 0.19 },
            Bracket { threshold: 45_000.0, rate: 0.325 },
            Bracket { threshold: 120_000.0, rate: 0.37 },
            Bracket { threshold: 180_000.0, rate: 0.45 },
        ]
    } else if fy_end >= 2017 {
        // The third threshold moved from 80,000 to 87,000 for 2016-17.
        [
            Bracket { threshold: 18_200.0, rate: 0.19 },
            Bracket { threshold: 37_000.0, rate: 0.325 },
            Bracket { threshold: 87_000.0, rate: 0.37 },
            Bracket { threshold: 180_000.0, rate: 0.45 },
        ]
    } else if fy_end >= 2013 {
        // The tax-free threshold rose to 18,200 for 2012-13.
        [
            Bracket { threshold: 18_200.0, rate: 0.19 },
            Bracket { threshold: 37_000.0, rate: 0.325 },
            Bracket { threshold: 80_000.0, rate: 0.37 },
            Bracket { threshold: 180_000.0, rate: 0.45 },
        ]
    } else {
        [
            Bracket { threshold: 6_000.0, rate: 0.15 },
            Bracket { threshold: 37_000.0, rate: 0.30 },
            Bracket { threshold: 80_000.0, rate: 0.37 },
            Bracket { threshold: 180_000.0, rate: 0.45 },
        ]
    }
}

#[inline]
fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

/// Tax on a resident's taxable income for a financial year, before offsets
/// and the Medicare levy.
pub fn resident_tax(income: f64, fy_end: i32) -> f64 {
    let income = income.max(0.0);
    let brackets = resident_brackets(fy_end);
    let mut tax = 0.0;
    for (i, bracket) in brackets.iter().enumerate() {
        let upper = brackets.get(i + 1).map_or(f64::INFINITY, |b| b.threshold);
        let taxed = (income - bracket.threshold).clamp(0.0, upper - bracket.threshold);
        tax += taxed * bracket.rate;
    }
    round2(tax)
}

/// Tax on a foreign resident's taxable income. A foreign resident has no
/// tax-free threshold and pays the second-bracket rate from the first dollar,
/// which is the single largest difference between the two schedules and the
/// reason a residency flag has to reach the calculation at all.
pub fn foreign_resident_tax(income: f64, fy_end: i32) -> f64 {
    let income = income.max(0.0);
    let brackets = resident_brackets(fy_end);
    // The first taxable bracket's rate applies from zero.
    let entry_rate = brackets[1].rate;
    let first_upper = brackets[2].threshold;
    let mut tax = income.min(first_upper) * entry_rate;
    for (i, bracket) in brackets.iter().enumerate().skip(2) {
        let upper = brackets.get(i + 1).map_or(f64::INFINITY, |b| b.threshold);
        let taxed = (income - bracket.threshold).clamp(0.0, upper - bracket.threshold);
        tax += taxed * bracket.rate;
    }
    round2(tax)
}

/// The low income tax offset for a financial year.
pub fn low_income_tax_offset(taxable_income: f64, fy_end: i32) -> f64 {
    let ti = taxable_income.max(0.0);
    let offset = if fy_end >= 2021 {
        // $700, reducing by 5 cents per dollar from $37,500 to $45,000, then
        // by 1.5 cents per dollar to $66,667. A single taper misses the
        // second rate and zeroes the offset far too early.
        if ti <= 37_500.0 {
            700.0
        } else if ti <= 45_000.0 {
            700.0 - (ti - 37_500.0) * 0.05
        } else {
            (325.0 - (ti - 45_000.0) * 0.015).max(0.0)
        }
    } else {
        // $445, reducing by 1.5 cents per dollar from $37,000.
        if ti <= 37_000.0 {
            445.0
        } else {
            (445.0 - (ti - 37_000.0) * 0.015).max(0.0)
        }
    };
    round2(offset)
}

/// The Medicare levy for a financial year. Below the lower threshold no levy
/// is payable; between the thresholds it shades in at 10 cents per dollar;
/// above the upper it is 2% of taxable income.
pub fn medicare_levy(taxable_income: f64, fy_end: i32) -> f64 {
    let ti = taxable_income.max(0.0);
    // The thresholds are indexed each year.
    let (lower, upper) = if fy_end >= 2024 {
        (26_000.0, 32_500.0)
    } else if fy_end >= 2021 {
        (23_365.0, 29_207.0)
    } else if fy_end >= 2017 {
        (21_655.0, 27_069.0)
    } else {
        (20_542.0, 24_167.0)
    };
    let levy = if ti > upper {
        ti * 0.02
    } else if ti > lower {
        (ti - lower) * 0.10
    } else {
        0.0
    };
    round2(levy)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_schedule_changes_between_years() {
        // The Stage 3 cuts are the largest change in the window, so a person
        // on $100,000 pays materially less in 2024-25 than in 2023-24.
        let before = resident_tax(100_000.0, 2024);
        let after = resident_tax(100_000.0, 2025);
        assert!(after < before, "{after} should be below {before}");
        assert!(before - after > 1_000.0);
    }

    #[test]
    fn the_tax_free_threshold_is_free() {
        for fy in [2013, 2020, 2024, 2025] {
            assert_eq!(resident_tax(18_200.0, fy), 0.0);
        }
        // And before 2012-13 it was $6,000.
        assert_eq!(resident_tax(6_000.0, 2012), 0.0);
        assert!(resident_tax(18_200.0, 2012) > 0.0);
    }

    #[test]
    fn a_foreign_resident_has_no_tax_free_threshold() {
        assert!(foreign_resident_tax(10_000.0, 2024) > 0.0);
        assert_eq!(resident_tax(10_000.0, 2024), 0.0);
        assert!(foreign_resident_tax(50_000.0, 2024) > resident_tax(50_000.0, 2024));
    }

    #[test]
    fn the_offset_tapers_in_two_stages() {
        assert_eq!(low_income_tax_offset(37_500.0, 2024), 700.0);
        assert_eq!(low_income_tax_offset(45_000.0, 2024), 325.0);
        assert_eq!(low_income_tax_offset(66_667.0, 2024), 0.0);
        // The earlier schedule is a smaller offset on a single taper.
        assert_eq!(low_income_tax_offset(37_000.0, 2019), 445.0);
    }

    #[test]
    fn the_levy_shades_in() {
        assert_eq!(medicare_levy(20_000.0, 2023), 0.0);
        assert!(medicare_levy(26_000.0, 2023) > 0.0);
        assert_eq!(medicare_levy(50_000.0, 2023), 1_000.0);
    }
}

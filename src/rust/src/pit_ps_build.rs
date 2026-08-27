// Amounts a payment summary reports that more than one product needs.
//
// The column builder that once lived here wrote fourteen column names, of
// which only `SYNTHETIC_AEUID` is a variable the PLIDA data item list
// publishes. It is gone: `pit_ps_full` and `pit_ps_tables` build the
// registry's own schema, year by year. What remains are the three rules the
// other ATO and payroll products share with it.

#[inline]
pub(crate) fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

/// Withholding on a year's gross, under the schedule in force that year.
/// One rule, so STP, PAYG and PIT all agree, and all three move when the
/// schedule does.
#[inline]
pub(crate) fn compute_payg_tax(inc: f64, fy_end: i32) -> f64 {
    crate::tax_schedule::resident_tax(inc, fy_end)
}

#[inline]
pub(crate) fn sg_rate_for_year(year: i32) -> f64 {
    match year {
        2015..=2020 => 0.095,
        2021 => 0.100,
        2022 => 0.105,
        2023 => 0.110,
        2024 => 0.115,
        _ => 0.10,
    }
}

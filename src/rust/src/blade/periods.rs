//! BLADE reference-period derivation (Stage 3 of the R->Rust port).
//!
//! Ports the period chain in `R/generate_blade.R`: `.blade_split_periods`,
//! `.blade_period_end_year`, `.blade_latest_period_from_values`,
//! `.blade_latest_period_from_reference`, `.blade_latest_period`,
//! `.blade_tsid_from_period`, `.blade_tsid`, `.blade_financial_year_code`,
//! `.blade_end_year`, `.blade_nominal_period` and `.blade_reference_date`.
//!
//! Every BLADE table names its own reference period in the data item list, and
//! the whole table (dates, year columns, tsid, quarter) hangs off that one
//! string, so the chain is resolved once per table and reused for every
//! variable. This module exports nothing to R directly; the classifier calls
//! it. It therefore carries no `extendr_module!` block -- a second block whose
//! name did not match the file would collide at `R_init_fplida`.

/// R's `[[:space:]]` in the C locale: the six ASCII whitespace characters.
/// Rust's `is_ascii_whitespace` omits the vertical tab, so spell the set out.
#[inline]
fn is_r_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c')
}

/// R `gsub("[[:space:]]+", "", x)` followed by `trimws()` (a no-op afterwards).
fn strip_space(s: &str) -> String {
    s.chars().filter(|c| !is_r_space(*c)).collect()
}

/// R `trimws(x)`.
fn trim_r(s: &str) -> &str {
    s.trim_matches(is_r_space)
}

/// R `grepl("^[0-9]{4}-[0-9]{2}$", s)` on a whitespace-stripped string.
fn is_financial_year(s: &str) -> bool {
    let b = s.as_bytes();
    b.len() == 7
        && b[..4].iter().all(u8::is_ascii_digit)
        && b[4] == b'-'
        && b[5..].iter().all(u8::is_ascii_digit)
}

/// R `grepl("^[0-9]{4}$", s)`.
fn is_calendar_year(s: &str) -> bool {
    let b = s.as_bytes();
    b.len() == 4 && b.iter().all(u8::is_ascii_digit)
}

/// `.blade_split_periods`: drop NA, split each entry on `;`, trim, keep the
/// non-empty pieces in order.
pub fn split_periods(entries: &[Option<String>]) -> Vec<String> {
    let mut out = Vec::new();
    for entry in entries.iter().flatten() {
        for piece in entry.split(';') {
            let piece = trim_r(piece);
            if !piece.is_empty() {
                out.push(piece.to_string());
            }
        }
    }
    out
}

/// `.blade_period_end_year`: the year a period ENDS in.
///
/// `2025-26` is a financial year written with a two-digit end, so the century
/// comes from the start year. `1999-00` therefore ends in 2000, not 1900, which
/// is what the `+ 100` correction is for.
pub fn period_end_year(period: &str) -> Option<i32> {
    let s = strip_space(period);
    if s.is_empty() {
        return None;
    }
    if is_financial_year(&s) {
        let start_year: i32 = s[0..4].parse().ok()?;
        let end_two: i32 = s[5..7].parse().ok()?;
        let century = start_year - start_year.rem_euclid(100);
        let mut end_year = century + end_two;
        if end_year < start_year {
            end_year += 100;
        }
        return Some(end_year);
    }
    if is_calendar_year(&s) {
        return s.parse::<i32>().ok();
    }
    // R `gregexpr("[0-9]{4}")`: non-overlapping four-digit runs, left to right,
    // so "20231231" yields 2023 then 1231 and the max is taken.
    let b = s.as_bytes();
    let mut best: Option<i32> = None;
    let mut i = 0usize;
    while i + 4 <= b.len() {
        if b[i..i + 4].iter().all(u8::is_ascii_digit) {
            if let Ok(v) = s[i..i + 4].parse::<i32>() {
                best = Some(best.map_or(v, |m: i32| m.max(v)));
            }
            i += 4;
        } else {
            i += 1;
        }
    }
    best
}

/// `.blade_latest_period_from_values`: the entry with the highest end year.
/// R uses `which.max`, so the FIRST maximum wins.
pub fn latest_period_from_values(
    entries: &[Option<String>],
    default: Option<&str>,
) -> Option<String> {
    let periods = split_periods(entries);
    if periods.is_empty() {
        return default.map(str::to_string);
    }
    let mut best: Option<(i32, usize)> = None;
    for (i, p) in periods.iter().enumerate() {
        if let Some(y) = period_end_year(p) {
            match best {
                Some((by, _)) if by >= y => {}
                _ => best = Some((y, i)),
            }
        }
    }
    match best {
        Some((_, i)) => Some(periods[i].clone()),
        None => default.map(str::to_string),
    }
}

/// R `strsplit(x, "[[:space:]]+to[[:space:]]+")`: the text after the last
/// non-overlapping match.
fn after_last_to(s: &str) -> &str {
    let b = s.as_bytes();
    let mut last_end: Option<usize> = None;
    let mut i = 0usize;
    while i < b.len() {
        if is_r_space(b[i] as char) {
            let mut j = i;
            while j < b.len() && is_r_space(b[j] as char) {
                j += 1;
            }
            if j + 2 <= b.len()
                && &b[j..j + 2] == b"to"
                && j + 2 < b.len()
                && is_r_space(b[j + 2] as char)
            {
                let mut k = j + 2;
                while k < b.len() && is_r_space(b[k] as char) {
                    k += 1;
                }
                last_end = Some(k);
                i = k;
                continue;
            }
            i = j;
        } else {
            i += 1;
        }
    }
    match last_end {
        Some(k) => &s[k..],
        None => s,
    }
}

/// `.blade_latest_period_from_reference`: take the end of a "2001-02 to
/// 2025-26" range. An unparseable or empty reference falls back to the
/// generator's default period.
pub fn latest_period_from_reference(reference: &str) -> String {
    const DEFAULT: &str = "2023-24";
    if trim_r(reference).is_empty() {
        return DEFAULT.to_string();
    }
    let period = strip_space(after_last_to(trim_r(reference)));
    if is_financial_year(&period) || is_calendar_year(&period) {
        period
    } else {
        DEFAULT.to_string()
    }
}

/// `.blade_latest_period` for one table: the variables' `Available.Periods`
/// win, then the table's `Reference.Period`, then the generator default.
pub fn latest_period_using(available: &[Option<String>], reference: &str) -> String {
    match latest_period_from_values(available, None) {
        Some(p) => p,
        None => latest_period_from_reference(reference),
    }
}

/// `.blade_tsid_from_period`: the two-digit end year.
pub fn tsid_from_period(period: &str) -> String {
    match period_end_year(period) {
        // R `sprintf("%02d", NA_integer_)` renders the string "NA"; keep that
        // rather than inventing a year, and the letters keep the cell clear of
        // the forbidden `_[0-9]{6}$` placeholder shape either way.
        None => "NA".to_string(),
        Some(y) => format!("{:02}", y.rem_euclid(100)),
    }
}

/// `.blade_financial_year_code`: "2025-26" -> "2526".
pub fn financial_year_code(period: &str) -> String {
    let p = strip_space(period);
    if is_financial_year(&p) {
        format!("{}{}", &p[2..4], &p[5..7])
    } else {
        match period_end_year(&p) {
            Some(y) => y.to_string(),
            None => "NA".to_string(),
        }
    }
}

/// Days since 1970-01-01 (Howard Hinnant's civil-date algorithm, proleptic
/// Gregorian). R stores a `Date` as exactly this day count.
pub fn days_from_civil(year: i32, m: u32, d: u32) -> f64 {
    let y: i64 = (year - i32::from(m <= 2)) as i64;
    let era: i64 = if y >= 0 { y } else { y - 399 } / 400;
    let yoe: i64 = y - era * 400;
    let mp: i64 = if m > 2 { (m - 3) as i64 } else { (m + 9) as i64 };
    let doy: i64 = (153 * mp + 2) / 5 + (d as i64) - 1;
    let doe: i64 = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    (era * 146_097 + doe - 719_468) as f64
}

/// `.blade_reference_date`: 30 June of the end year for a financial-year
/// period, 31 December for a calendar-year one.
pub fn reference_date(period: &str) -> f64 {
    let p = strip_space(period);
    let year = period_end_year(&p).unwrap_or(2024);
    if is_financial_year(&p) {
        days_from_civil(year, 6, 30)
    } else {
        days_from_civil(year, 12, 31)
    }
}

/// The metadata a table needs to resolve its period, gathered by R from
/// `variables.csv` and `tables.csv`.
pub struct PeriodContext {
    /// This table's `Available.Periods` column, one entry per variable row.
    pub available_periods: Vec<Option<String>>,
    /// This table's `Reference.Period`; "" when NA or the table is absent.
    pub reference_period: String,
    /// Table 1's two fields, consulted only for the table-5 tsid redirect.
    pub t1_available_periods: Vec<Option<String>>,
    pub t1_reference_period: String,
}

/// Everything the classifier needs from the period chain, resolved once per
/// table so no variable recomputes it.
pub struct ResolvedPeriod {
    pub latest_period: String,
    pub tsid: String,
    pub end_year: Option<i32>,
    pub financial_year_code: String,
    /// Days since 1970-01-01.
    pub reference_date: f64,
}

impl ResolvedPeriod {
    pub fn resolve(ctx: &PeriodContext, table_number: i32) -> ResolvedPeriod {
        let latest = latest_period_using(&ctx.available_periods, &ctx.reference_period);
        // Table 5 (PAYG) deliberately borrows table 1's period so its `tsid`
        // agrees with the business-register table it is joined to.
        let tsid = if table_number == 5 {
            tsid_from_period(&latest_period_using(
                &ctx.t1_available_periods,
                &ctx.t1_reference_period,
            ))
        } else {
            tsid_from_period(&latest)
        };
        ResolvedPeriod {
            tsid,
            end_year: period_end_year(&latest),
            financial_year_code: financial_year_code(&latest),
            reference_date: reference_date(&latest),
            latest_period: latest,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn owned(values: &[&str]) -> Vec<Option<String>> {
        values.iter().map(|s| Some(s.to_string())).collect()
    }

    #[test]
    fn end_year_handles_century_rollover() {
        assert_eq!(period_end_year("2025-26"), Some(2026));
        assert_eq!(period_end_year("1999-00"), Some(2000));
        assert_eq!(period_end_year("2001-02"), Some(2002));
        assert_eq!(period_end_year("2024"), Some(2024));
        assert_eq!(period_end_year("20231231"), Some(2023));
        assert_eq!(period_end_year(" 2025 - 26 "), Some(2026));
        assert_eq!(period_end_year(""), None);
        assert_eq!(period_end_year("not a period"), None);
    }

    #[test]
    fn split_and_latest() {
        let entries = owned(&["2001-02;2002-03;2025-26", "2003-04"]);
        assert_eq!(split_periods(&entries).len(), 4);
        assert_eq!(
            latest_period_from_values(&entries, None),
            Some("2025-26".to_string())
        );
        assert_eq!(latest_period_from_values(&[], None), None);
        assert_eq!(
            latest_period_from_values(&[None], Some("2023-24")),
            Some("2023-24".to_string())
        );
        // which.max: the FIRST maximum wins.
        let tied = owned(&["2025-26;2025-26"]);
        assert_eq!(
            latest_period_from_values(&tied, None),
            Some("2025-26".to_string())
        );
    }

    #[test]
    fn reference_period_takes_the_range_end() {
        assert_eq!(latest_period_from_reference("2001-02 to 2025-26"), "2025-26");
        assert_eq!(latest_period_from_reference("2024"), "2024");
        assert_eq!(latest_period_from_reference(""), "2023-24");
        assert_eq!(latest_period_from_reference("rolling"), "2023-24");
    }

    #[test]
    fn table_one_periods_give_tsid_26() {
        let ctx = PeriodContext {
            available_periods: owned(&["2001-02;2025-26"]),
            reference_period: "2001-02 to 2025-26".to_string(),
            t1_available_periods: owned(&["2001-02;2025-26"]),
            t1_reference_period: "2001-02 to 2025-26".to_string(),
        };
        let r = ResolvedPeriod::resolve(&ctx, 1);
        assert_eq!(r.latest_period, "2025-26");
        assert_eq!(r.tsid, "26");
        assert_eq!(r.end_year, Some(2026));
        assert_eq!(r.financial_year_code, "2526");
        assert_eq!(r.reference_date, 20634.0);
    }

    #[test]
    fn table_five_borrows_table_one() {
        let ctx = PeriodContext {
            available_periods: owned(&["2019-20"]),
            reference_period: String::new(),
            t1_available_periods: owned(&["2025-26"]),
            t1_reference_period: String::new(),
        };
        let r = ResolvedPeriod::resolve(&ctx, 5);
        assert_eq!(r.latest_period, "2019-20");
        assert_eq!(r.tsid, "26");
    }

    #[test]
    fn civil_days_match_the_calendar() {
        assert_eq!(days_from_civil(1970, 1, 1), 0.0);
        assert_eq!(days_from_civil(2026, 6, 30), 20634.0);
        assert_eq!(days_from_civil(2024, 12, 31), 20088.0);
    }

    #[test]
    fn reference_date_picks_june_or_december() {
        assert_eq!(reference_date("2025-26"), days_from_civil(2026, 6, 30));
        assert_eq!(reference_date("2024"), days_from_civil(2024, 12, 31));
        assert_eq!(reference_date("rubbish"), days_from_civil(2024, 12, 31));
    }
}

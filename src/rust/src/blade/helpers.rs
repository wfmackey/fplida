//! BLADE deterministic leaf helpers (Stage 0 of the R->Rust port).
//!
//! These are pure, RNG-free functions that reproduce the `.blade_*` /
//! `.stable_name_seed` helpers in `R/generate_blade.R` exactly. BLADE generation
//! is deterministic by construction: every value comes from a closed-form
//! `(seq * const + seed + salt) %% mod` hash with 1-based indices, never a PRNG.
//! All arithmetic is done in i64/i128 to avoid the i32 overflow that R sidesteps
//! via double-precision integer multiplication.

/// Round to 2 decimal places (R `round(x, 2)`).
#[inline]
pub fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

/// `.stable_name_seed(value)`: a deterministic per-name salt. R computes
/// `sum(utf8ToInt(value) * seq_along) %% 100000`. `utf8ToInt` yields Unicode
/// code points, so iterate over `chars()` (not bytes).
pub fn stable_name_seed(s: &str) -> i64 {
    s.chars()
        .enumerate()
        .map(|(i, c)| (c as i64) * ((i as i64) + 1))
        .sum::<i64>()
        .rem_euclid(100_000)
}

/// `.blade_numeric_id(prefix, value, width)`: a 1-char prefix followed by the
/// value zero-padded to `width` digits. Uses i128 so wide ids (e.g. the 23-digit
/// hashed ARID) do not overflow.
pub fn numeric_id(prefix: &str, value: i128, width: usize) -> String {
    format!("{}{:0w$}", prefix, value, w = width)
}

/// `.blade_id_number(values)`: strip non-digits and parse to f64; positions that
/// have no digits are filled 1,2,3,... in order. Used as a per-row hash handle.
pub fn id_number(values: &[&str]) -> Vec<f64> {
    let mut na_counter: i64 = 0;
    values
        .iter()
        .map(|v| {
            let digits: String = v.chars().filter(|c| c.is_ascii_digit()).collect();
            match digits.parse::<f64>() {
                Ok(x) => x,
                Err(_) => {
                    na_counter += 1;
                    na_counter as f64
                }
            }
        })
        .collect()
}

/// `.blade_financial_year_label(start_year)`: e.g. 2011 -> "2011-12".
pub fn financial_year_label(start_year: i32) -> String {
    format!("{:04}-{:02}", start_year, (start_year + 1).rem_euclid(100))
}

/// `.blade_deidentified_id(prefix, value, i, seed, width)`: a stable hashed id.
/// `i` is the 1-based row index.
pub fn deidentified_id(prefix: &str, value: &str, i: i64, seed: i64, width: usize) -> String {
    let h = stable_name_seed(&format!("{}|{}|{}", value, i, seed));
    let modulus = 10i128.pow(width as u32);
    let num = ((h as i128) * 1_000_003 + (i as i128) * 9176 + (seed as i128)).rem_euclid(modulus);
    numeric_id(prefix, num, width)
}

/// `.normalise_blade_anzsco(x)`: NA / "" / "0" -> "000000", else the numeric value
/// rounded and zero-padded to 6 digits.
pub fn normalise_anzsco(x: Option<&str>) -> String {
    match x {
        None => "000000".to_string(),
        Some(s) if s.is_empty() || s == "0" => "000000".to_string(),
        Some(s) => match s.trim().parse::<f64>() {
            Ok(v) => format!("{:06}", v.round() as i64),
            Err(_) => "000000".to_string(),
        },
    }
}

/// `.blade_anzsco_group(code, digits)`: first `digits` chars of the normalised
/// 6-digit ANZSCO code.
pub fn anzsco_group(code: Option<&str>, digits: usize) -> String {
    normalise_anzsco(code).chars().take(digits).collect()
}

/// `.blade_amount(business_rows, seed, salt, scale)`: a turnover-/wage-anchored
/// dollar amount. `seq` is the 1-based row index.
pub fn amount(turnover: f64, annual_wages: f64, seq: i64, seed: i64, salt: i64, scale: f64) -> f64 {
    let draw = ((seq * 37 + seed + salt).rem_euclid(100) as f64) / 100.0;
    round2((turnover * scale * (0.6 + draw)).max(annual_wages * 0.1))
}

/// `.blade_pick_codes(codes, n, seed, salt, include_missing)`: pick one
/// substantive code per row, occasionally substituting a "missing" sentinel
/// (7777777 / 88888888 / 999999999).
pub fn pick_codes(
    codes: &[i32],
    n: usize,
    seed: i64,
    salt: i64,
    include_missing: bool,
) -> Vec<i32> {
    // unique, preserving order
    let mut seen = std::collections::HashSet::new();
    let uniq: Vec<i32> = codes.iter().copied().filter(|c| seen.insert(*c)).collect();
    if uniq.is_empty() {
        return vec![0; n];
    }
    let is_missing = |c: i32| matches!(c, 7_777_777 | 88_888_888 | 999_999_999);
    let missing: Vec<i32> = uniq.iter().copied().filter(|c| is_missing(*c)).collect();
    let mut substantive: Vec<i32> = uniq.iter().copied().filter(|c| !is_missing(*c)).collect();
    if substantive.is_empty() {
        substantive = uniq.clone();
    }
    let mut out: Vec<i32> = (0..n)
        .map(|i| {
            let seq = (i as i64) + 1;
            substantive[(seq + seed + salt).rem_euclid(substantive.len() as i64) as usize]
        })
        .collect();
    if include_missing && !missing.is_empty() {
        let mut rank: i64 = 0;
        for (i, slot) in out.iter_mut().enumerate() {
            let seq = (i as i64) + 1;
            if (seq * 41 + seed + salt).rem_euclid(100) >= 92 {
                rank += 1;
                *slot = missing[(rank + seed + salt).rem_euclid(missing.len() as i64) as usize];
            }
        }
    }
    out
}

/// `.blade_valid_response_codes(valid_response)`: extract numeric codes that are
/// a run of 1-9 digits, not preceded by a digit, and followed (after optional
/// whitespace) by `=` or `-`. Hand-rolled because the Rust `regex` crate has no
/// look-behind. Returns unique codes in order of appearance.
pub fn valid_response_codes(text: &str) -> Vec<i32> {
    if text.is_empty() {
        return Vec::new();
    }
    let chars: Vec<char> = text.chars().collect();
    let mut out: Vec<i32> = Vec::new();
    let mut seen = std::collections::HashSet::new();
    let mut i = 0usize;
    while i < chars.len() {
        if chars[i].is_ascii_digit() {
            let preceded = i > 0 && chars[i - 1].is_ascii_digit();
            let start = i;
            while i < chars.len() && chars[i].is_ascii_digit() {
                i += 1;
            }
            let run_len = i - start;
            let mut j = i;
            while j < chars.len() && chars[j].is_whitespace() {
                j += 1;
            }
            let followed = j < chars.len() && (chars[j] == '=' || chars[j] == '-');
            if !preceded && (1..=9).contains(&run_len) && followed {
                if let Ok(v) = chars[start..i].iter().collect::<String>().parse::<i32>() {
                    if seen.insert(v) {
                        out.push(v);
                    }
                }
            }
        } else {
            i += 1;
        }
    }
    out
}

/// `.blade_character_code(n, seed, salt, width)`: a "C"-prefixed code; the letter
/// prefix guarantees the output never matches the `_[0-9]{6}$` placeholder regex
/// the tests forbid.
pub fn character_code(seq: i64, seed: i64, salt: i64, width: usize) -> String {
    let m = 10i64.pow(width as u32);
    format!("C{:0w$}", (seq * 17 + seed + salt).rem_euclid(m), w = width)
}

/// `.blade_count_value(name, business_rows, seed, salt)`: a headcount-style count.
pub fn count_value(name_lower: &str, d_total_payees: i32, seq: i64, seed: i64, salt: i64) -> i32 {
    let base = d_total_payees.max(0) as i64;
    let v = if name_lower.contains("loc")
        || name_lower.contains("site")
        || name_lower.contains("premis")
    {
        (1 + (seq + seed + salt).rem_euclid(6)).max(1)
    } else if name_lower.contains("manager")
        || name_lower.contains("director")
        || name_lower.contains("proprietor")
        || name_lower.contains("partner")
    {
        base.max(1).min(5)
    } else {
        (base + (seq + seed + salt).rem_euclid(4) - 1).max(0)
    };
    v as i32
}

/// `.blade_code_name(lower)`: is this variable a coded/categorical field?
/// (`_cd$|code|typ|status|sts|ind$|flag|rt$|rng|marin|cntry|sgmt`).
pub fn code_name(lower: &str) -> bool {
    lower.ends_with("_cd")
        || lower.contains("code")
        || lower.contains("typ")
        || lower.contains("status")
        || lower.contains("sts")
        || lower.ends_with("ind")
        || lower.contains("flag")
        || lower.ends_with("rt")
        || lower.contains("rng")
        || lower.contains("marin")
        || lower.contains("cntry")
        || lower.contains("sgmt")
}

const FINANCIAL_PATTERNS: [&str; 44] = [
    "sales", "turnover", "income", "incm", "inclo", "inc", "revenue", "gross", "gros", "grss",
    "amount", "amt", "expense", "expn", "exps", "cost", "ddct", "ded", "depr", "rent", "super",
    "wage", "salary", "salwg", "labr", "asset", "asst", "liab", "debt", "stock", "credit",
    "debtor", "tax", "profit", "loss", "gain", "cgt", "frank", "loan", "pay", "fees", "val",
    "tofa", "tnovr",
];

/// `.blade_financial_name(lower)`: is this variable a financial/amount field?
pub fn financial_name(lower: &str) -> bool {
    FINANCIAL_PATTERNS.iter().any(|p| lower.contains(p))
}

/// `.blade_form_prefix(legal_form)`: Company->c, Sole trader->i, Partnership->p,
/// Trust->t (default c). Drives the BIT c/i/p/t wage exclusivity.
pub fn form_prefix(legal_form: &str) -> char {
    match legal_form {
        "Sole trader" => 'i',
        "Partnership" => 'p',
        "Trust" => 't',
        _ => 'c',
    }
}

/// `.blade_role_code(n, seed, salt)`: the GST role code frame n/0/1/L for
/// x_itip / x_itw / x_gstp.
pub fn role_code(seq: i64, seed: i64, salt: i64) -> &'static str {
    let draw = (seq * 37 + seed + salt).rem_euclid(100);
    if draw < 10 {
        "n"
    } else if draw < 18 {
        "0"
    } else if draw < 92 {
        "1"
    } else {
        "L"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stable_name_seed_matches_r() {
        // R: sum(utf8ToInt("bn") * 1:2) %% 100000 = (98*1 + 110*2) = 318
        assert_eq!(stable_name_seed("bn"), 318);
        // "id": (105*1 + 100*2) = 305
        assert_eq!(stable_name_seed("id"), 305);
        // empty -> 0
        assert_eq!(stable_name_seed(""), 0);
    }

    #[test]
    fn numeric_id_pads() {
        assert_eq!(numeric_id("BN", 651499, 11), "BN00000651499");
        assert_eq!(numeric_id("E", 1, 9), "E000000001"); // prefix + 9 = nchar 10
        assert_eq!(numeric_id("BG", 927, 10), "BG0000000927");
    }

    #[test]
    fn normalise_and_group() {
        assert_eq!(normalise_anzsco(None), "000000");
        assert_eq!(normalise_anzsco(Some("0")), "000000");
        assert_eq!(normalise_anzsco(Some("253111")), "253111");
        assert_eq!(anzsco_group(Some("253111"), 2), "25");
        assert_eq!(anzsco_group(Some("253111"), 4), "2531");
    }

    #[test]
    fn valid_response_codes_parsing() {
        assert_eq!(valid_response_codes("1 = Weekly"), vec![1]);
        assert_eq!(
            valid_response_codes("0 = No; 1 = Yes; 7777777 = Missing"),
            vec![0, 1, 7_777_777]
        );
        assert_eq!(
            valid_response_codes("Numeric response ($)"),
            Vec::<i32>::new()
        );
        // 10-digit run (>9) followed by '=' must NOT match.
        assert_eq!(valid_response_codes("1234567890 = x"), Vec::<i32>::new());
    }

    #[test]
    fn pick_codes_no_codes_zero() {
        assert_eq!(pick_codes(&[], 3, 1, 0, true), vec![0, 0, 0]);
        // substantive-only: every row is one of the codes
        let out = pick_codes(&[1, 2, 3], 10, 5, 7, false);
        assert!(out.iter().all(|c| [1, 2, 3].contains(c)));
    }

    #[test]
    fn role_and_form() {
        assert!(["n", "0", "1", "L"].contains(&role_code(1, 0, 0)));
        assert_eq!(form_prefix("Company"), 'c');
        assert_eq!(form_prefix("Sole trader"), 'i');
        assert_eq!(form_prefix("Trust"), 't');
    }

    #[test]
    fn code_and_financial_names() {
        assert!(code_name("c_agrgtd_tnovr_rng_cd"));
        assert!(code_name("payg_headcount_mismatch_ind"));
        assert!(!code_name("turnover"));
        assert!(financial_name("c_totlwage"));
        assert!(financial_name("turnover"));
        // "pay" is itself a financial token (so payfreq_eeh would match too), use
        // a name with no financial substring.
        assert!(!financial_name("agecat_eeh"));
    }
}

//! BLADE deterministic leaf helpers (Stage 0 of the R->Rust port).
//!
//! These are pure, RNG-free functions that reproduce the `.blade_*` /
//! `.stable_name_seed` helpers in `R/generate_blade.R` exactly. BLADE generation
//! is deterministic by construction: every value comes from a closed-form
//! `(seq * const + seed + salt) %% mod` hash with 1-based indices, never a PRNG.
//! All arithmetic is done in i64/i128 to avoid the i32 overflow that R sidesteps
//! via double-precision integer multiplication.

/// R's `round(x, digits)` for `digits > 0`, as rewritten in R 4.0.0: of the two
/// representable numbers either side, take the nearer, and on a tie take the
/// even one. Rust's `f64::round` rounds halves away from zero instead, which
/// puts a cent between the two languages on roughly three per cent of dollar
/// amounts -- enough to break a derived-equality column downstream.
fn r_round(x: f64, digits: i32) -> f64 {
    if !x.is_finite() {
        return x;
    }
    let sign = if x < 0.0 { -1.0 } else { 1.0 };
    let value = x.abs();
    let scale = 10f64.powi(digits);
    let floor10 = (value * scale).floor();
    let lower = floor10 / scale;
    let upper = (floor10 + 1.0) / scale;
    let down = value - lower;
    let up = upper - value;
    let out = if down < up {
        lower
    } else if up < down {
        upper
    } else if floor10 % 2.0 == 0.0 {
        lower
    } else {
        upper
    };
    sign * out
}

/// R `round(x, 2)`.
#[inline]
pub fn round2(x: f64) -> f64 {
    r_round(x, 2)
}

/// R `round(x, 1)`.
#[inline]
pub fn round1(x: f64) -> f64 {
    r_round(x, 1)
}

/// R `round(x, digits)` for any positive `digits`.
#[inline]
pub fn round_digits(x: f64, digits: i32) -> f64 {
    r_round(x, digits)
}

/// R `pmin` on two scalars: NA wins over any value, which `f64::min` does not.
#[inline]
pub fn pmin2(a: f64, b: f64) -> f64 {
    if a.is_nan() || b.is_nan() {
        f64::NAN
    } else {
        a.min(b)
    }
}

/// R `pmax` on two scalars. Same NA rule as `pmin2`.
#[inline]
pub fn pmax2(a: f64, b: f64) -> f64 {
    if a.is_nan() || b.is_nan() {
        f64::NAN
    } else {
        a.max(b)
    }
}

/// R's `%%` on doubles, which is `myfmod()` in `arithmetic.c` and NOT C `fmod`.
/// The two agree until `|x1|` passes 2^53, which `blade_draw` reaches at around
/// three hundred thousand businesses, so every BLADE hash whose left operand is
/// a double has to go through this.
#[inline]
pub fn r_mod(x1: f64, x2: f64) -> f64 {
    if x2 == 0.0 {
        return f64::NAN;
    }
    let q = x1 / x2;
    let tmp = x1 - q.floor() * x2;
    let q2 = (tmp / x2).floor();
    tmp - q2 * x2
}

/// `.normalise_blade_state(x)`: NA -> 1, otherwise clamped to the eight
/// state and territory codes.
#[inline]
pub fn norm_state(x: i32) -> i32 {
    if x == i32::MIN {
        1
    } else {
        x.clamp(1, 8)
    }
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

/// `.blade_draw(business_rows, seed, salt, modulus)`: the per-row hash every
/// BLADE categorical and dollar draw is built on. `bn_num` is the raw
/// `id_number(bn)` output. Returns 0-based draws in `[0, modulus)`.
///
/// The second mixing pass breaks the short cycles a single linear congruence
/// produces for small domains (binary, month, twelve-level fields). The order
/// of the additions is load-bearing: floating-point addition is not
/// associative and R evaluates left to right as written.
pub fn blade_draw(bn_num: &[f64], seed: i64, salt: i64, modulus: i64) -> Vec<i32> {
    const M: f64 = 2_147_483_647.0;
    let n = bn_num.len();
    if n == 0 || modulus <= 0 {
        return Vec::new();
    }
    let coefficient = 104_729.0 + salt.rem_euclid(1009) as f64;
    let row_coefficient = 13_007.0 + salt.rem_euclid(97) as f64;
    let scrambler = 48_271.0 + salt.rem_euclid(997) as f64;
    (0..n)
        .map(|i| {
            let seq = (i + 1) as f64;
            let key = r_mod(bn_num[i], M);
            let mixed = r_mod(
                key * coefficient + seq * row_coefficient + (seed as f64) * 8191.0
                    + (salt as f64) * 65537.0,
                M,
            );
            let mixed = r_mod(
                mixed * scrambler + seq * seq * 104_729.0 + (salt as f64) * 8191.0,
                M,
            );
            r_mod(mixed, modulus as f64) as i32
        })
        .collect()
}

/// `.blade_amount(business_rows, seed, salt, scale)`: a turnover-anchored
/// dollar amount with a wages floor. `scale` applies to BOTH arms.
pub fn amount(
    bn_num: &[f64],
    turnover: &[f64],
    annual_wages: &[f64],
    seed: i64,
    salt: i64,
    scale: f64,
) -> Vec<f64> {
    let draws = blade_draw(bn_num, seed, salt, 10_000);
    (0..bn_num.len())
        .map(|i| {
            let draw = draws[i] as f64 / 10_000.0;
            round2((turnover[i] * scale * (0.6 + draw)).max(annual_wages[i] * 0.1 * scale))
        })
        .collect()
}

/// The three BLADE special-missing sentinels: not stated, not applicable and
/// multiple responses.
#[inline]
fn is_missing_code(c: i32) -> bool {
    matches!(c, 7_777_777 | 88_888_888 | 999_999_999)
}

/// `.blade_pick_codes(codes, n, seed, salt, include_missing, business_rows)`:
/// pick one substantive code per row, occasionally substituting a special
/// missing sentinel. `key = Some(bn_num)` reproduces the business-row path;
/// `None` the vectorised fallback.
pub fn pick_codes(
    codes: &[i32],
    n: usize,
    seed: i64,
    salt: i64,
    include_missing: bool,
    key: Option<&[f64]>,
) -> Vec<i32> {
    let mut seen = std::collections::HashSet::new();
    let uniq: Vec<i32> = codes.iter().copied().filter(|c| seen.insert(*c)).collect();
    if uniq.is_empty() {
        return vec![0; n];
    }
    let missing: Vec<i32> = uniq.iter().copied().filter(|c| is_missing_code(*c)).collect();
    let mut substantive: Vec<i32> = uniq.iter().copied().filter(|c| !is_missing_code(*c)).collect();
    if substantive.is_empty() {
        substantive.clone_from(&uniq);
    }
    let sub_len = substantive.len();
    let sub_draw: Vec<i32> = match key {
        Some(k) => blade_draw(k, seed, salt, sub_len as i64),
        None => (0..n)
            .map(|i| {
                let seq = (i + 1) as f64;
                r_mod(
                    seq * (17 + salt.rem_euclid(89)) as f64 + seed as f64 + salt as f64,
                    sub_len as f64,
                ) as i32
            })
            .collect(),
    };
    // Every call site passes `n == bn_num.len()`; index defensively so a
    // mismatched caller degrades to the first code rather than panicking.
    let at = |v: &[i32], i: usize| v.get(i).copied().unwrap_or(0).max(0) as usize;
    let mut out: Vec<i32> = (0..n)
        .map(|i| substantive[at(&sub_draw, i).min(sub_len - 1)])
        .collect();
    if include_missing && !missing.is_empty() {
        let miss_draw: Vec<i32> = match key {
            Some(k) => blade_draw(k, seed, salt + 7919, 100),
            None => (0..n)
                .map(|i| {
                    let seq = (i + 1) as f64;
                    r_mod(
                        seq * (41 + salt.rem_euclid(73)) as f64 + seed as f64 + salt as f64,
                        100.0,
                    ) as i32
                })
                .collect(),
        };
        // R indexes `missing_codes` by the rank of the selected row within the
        // selected subset, not by the global row number.
        let mut rank: i64 = 0;
        for i in 0..n {
            if miss_draw.get(i).copied().unwrap_or(0) >= 92 {
                rank += 1;
                let j = r_mod(
                    rank as f64 + seed as f64 + salt as f64,
                    missing.len() as f64,
                ) as usize;
                out[i] = missing[j];
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

/// `.blade_character_code(n, seed, salt, width, prefix)`: a letter-prefixed
/// code. The letter is what keeps the output clear of the `_[0-9]{6}$`
/// placeholder shape the test suite forbids anywhere in a BLADE product.
pub fn character_code(n: usize, seed: i64, salt: i64, width: usize, prefix: &str) -> Vec<String> {
    let m = 10f64.powi(width as i32);
    (0..n)
        .map(|i| {
            let v = r_mod(((i + 1) as f64) * 17.0 + seed as f64 + salt as f64, m);
            format!("{}{:0w$}", prefix, v as i64, w = width)
        })
        .collect()
}

/// `.blade_count_value(name, business_rows, seed, salt)`: a headcount-style
/// count. Branch order is load-bearing -- "location" contains "loc", so the
/// site branch has to be tested first.
pub fn count_value(
    name_lower: &str,
    d_total_payees: Option<&[i32]>,
    bn_num: &[f64],
    seed: i64,
    salt: i64,
) -> Vec<i32> {
    let draws = blade_draw(bn_num, seed, salt, 1000);
    let site = name_lower.contains("loc")
        || name_lower.contains("site")
        || name_lower.contains("premis");
    let role = name_lower.contains("manager")
        || name_lower.contains("director")
        || name_lower.contains("proprietor")
        || name_lower.contains("partner");
    // Only the site branch ignores the payee base. R's `pmax(0L, NULL)` is a
    // zero-length vector, so the other two branches collapse to nothing when
    // the business frame carries no `d_total_payees`.
    if d_total_payees.is_none() && !site {
        return Vec::new();
    }
    let payees = d_total_payees.unwrap_or(&[]);
    (0..bn_num.len())
        .map(|i| {
            let base = payees.get(i).copied().unwrap_or(0).max(0);
            let d = draws[i];
            if site {
                (1 + d % 6).max(1)
            } else if role {
                (base - d % 3).max(1).min(5)
            } else {
                (base + d % 4 - 1).max(0)
            }
        })
        .collect()
}

/// `.blade_name_salt(value)`: a base-131 rolling hash of the lowercased name.
/// Distinct from `stable_name_seed`; both are used, sometimes in adjacent
/// branches, and swapping them changes every draw.
pub fn name_salt(value: &str) -> i64 {
    let mut hash: i64 = 17;
    for (i, c) in value.to_lowercase().chars().enumerate() {
        hash = (hash * 131 + (c as i64) + ((i as i64) + 1) * 17).rem_euclid(2_147_483_647);
    }
    hash
}

/// `.blade_related_id(prefix, business_rows, seed, salt, width)`: a stable
/// foreign-key-shaped identifier derived from the row's `bn`.
pub fn related_id(prefix: &str, bn_num: &[f64], seed: i64, salt: i64, width: usize) -> Vec<String> {
    let m = 10f64.powi(width as i32);
    (0..bn_num.len())
        .map(|i| {
            let head = r_mod(bn_num[i] + seed as f64 + salt as f64, 1e9) as i64;
            let base = ((i as i64) + 1) + head;
            numeric_id(prefix, r_mod(base as f64, m) as i128, width)
        })
        .collect()
}

/// `.blade_cycle_values(values, n, seed, salt, business_rows)`: deal the given
/// values out across the rows. Unlike `pick_values` this drops nothing, so an
/// NA in `values` stays a legitimate outcome (`sector_group` relies on it).
pub fn cycle_values(
    values: &[Option<String>],
    n: usize,
    seed: i64,
    salt: i64,
    key: Option<&[f64]>,
) -> Vec<Option<String>> {
    if values.is_empty() || n == 0 {
        return Vec::new();
    }
    let len = values.len();
    let draw: Vec<i32> = match key {
        Some(k) => blade_draw(k, seed, salt, len as i64),
        None => (0..n)
            .map(|i| {
                let seq = (i + 1) as f64;
                r_mod(
                    seq * (17 + salt.rem_euclid(89)) as f64 + seed as f64 + salt as f64,
                    len as f64,
                ) as i32
            })
            .collect(),
    };
    (0..n)
        .map(|i| {
            let j = draw.get(i).copied().unwrap_or(0).max(0) as usize;
            values[j.min(len - 1)].clone()
        })
        .collect()
}

/// `.blade_pick_values(values, business_rows, seed, salt, missing_rate)`: pick
/// one of a published domain's values per row, blanking `missing_rate` of them.
/// Drops NA and empty values first, which is the difference from
/// `cycle_values`.
pub fn pick_values(
    values: &[String],
    bn_num: &[f64],
    seed: i64,
    salt: i64,
    missing_rate: f64,
) -> Vec<Option<String>> {
    let mut seen = std::collections::HashSet::new();
    let uniq: Vec<&String> = values
        .iter()
        .filter(|v| !v.is_empty())
        .filter(|v| seen.insert(v.as_str()))
        .collect();
    let n = bn_num.len();
    if uniq.is_empty() || n == 0 {
        return Vec::new();
    }
    let draw = blade_draw(bn_num, seed, salt, uniq.len() as i64);
    let mut out: Vec<Option<String>> = (0..n)
        .map(|i| Some(uniq[draw[i] as usize].clone()))
        .collect();
    if missing_rate > 0.0 {
        let miss = blade_draw(bn_num, seed, salt + 48611, 1000);
        let threshold = (1000.0 * missing_rate).round() as i32;
        for i in 0..n {
            if miss[i] < threshold {
                out[i] = None;
            }
        }
    }
    out
}

/// `.blade_bcs_code(n, seed, include_multi)`: the Business Characteristics
/// Survey response frame -- 0/1 plus the not-applicable and not-stated
/// sentinels, and the multiple-response sentinel where the item allows one.
pub fn bcs_code(n: usize, seed: i64, include_multi: bool) -> Vec<i32> {
    (0..n)
        .map(|i| {
            // R computes `seed * 97L` in i32 and would overflow to NA above a
            // seed of about 22 million; i64 here diverges only in that
            // unreachable case.
            let draw = r_mod(
                ((i + 1) as f64) * 1_103_515_245.0 + (seed * 97) as f64,
                100.0,
            ) as i32;
            if include_multi && (78..84).contains(&draw) {
                7_777_777
            } else if draw < 50 {
                0
            } else if draw < 78 {
                1
            } else if draw < 89 {
                88_888_888
            } else {
                999_999_999
            }
        })
        .collect()
}

/// Operative Australian port codes from Appendix 7 of the April 2026 BLADE
/// data item list, pooled by state. Several high-volume ports are kept per
/// state rather than inventing a generic categorical code.
const PORT_POOLS: [&[&str]; 8] = [
    &["101", "102", "103", "104", "106", "107", "112", "118"],
    &["201", "202", "203", "204", "212", "298"],
    &["301", "303", "304", "305", "306", "307", "309", "311"],
    &["401", "403", "408", "409", "411", "413", "417", "418"],
    &["501", "502", "504", "505", "508", "510", "512", "513"],
    &["601", "602", "603", "605", "610", "611", "698"],
    &["701", "703", "704", "780", "798"],
    &["801", "898"],
];

/// `.blade_australian_port_code(state, seed, salt)`.
pub fn australian_port_code(state: &[i32], seed: i64, salt: i64) -> Vec<String> {
    (0..state.len())
        .map(|i| {
            let pool = PORT_POOLS[(norm_state(state[i]) - 1) as usize];
            // R does this in i32 and would overflow to NA at salts near 2^31;
            // i64 here diverges only in that unreachable case.
            let j = ((i as i64) + 1 + seed + salt).rem_euclid(pool.len() as i64) as usize;
            pool[j].to_string()
        })
        .collect()
}

/// `.blade_location_lookup_rows(business_rows, seed)`: pick one Mesh Block per
/// business from the rows of the lookup that sit in the business's own state,
/// so the geography columns agree with the state column. Returns 0-based
/// indices into the lookup, or `Err(state)` when a state has no lookup rows.
pub fn location_lookup_indices(
    bn_num: &[f64],
    state: &[i32],
    seed: i64,
    lookup_state: &[i32],
) -> Result<Vec<usize>, i32> {
    let n = bn_num.len();
    let mut out = vec![0usize; n];
    if n == 0 {
        return Ok(out);
    }
    let states: Vec<i32> = state.iter().map(|s| norm_state(*s)).collect();
    let mut distinct: Vec<i32> = states.clone();
    distinct.sort_unstable();
    distinct.dedup();
    for st in distinct {
        let idx: Vec<usize> = (0..n).filter(|&i| states[i] == st).collect();
        let pool: Vec<usize> = (0..lookup_state.len())
            .filter(|&i| lookup_state[i] == st)
            .collect();
        if pool.is_empty() {
            return Err(st);
        }
        for (rank, &i) in idx.iter().enumerate() {
            // `rank` is 1-based WITHIN the state group, not the global row.
            let pick = r_mod(
                bn_num[i] + ((rank as f64) + 1.0) * 2_654_435_761.0
                    + (seed * 1009) as f64
                    + (st as i64 * 9176) as f64,
                pool.len() as f64,
            ) as usize;
            out[i] = pool[pick.min(pool.len() - 1)];
        }
    }
    Ok(out)
}

// ---------------------------------------------------------------------------
// String predicates.
//
// The two BLADE cascades are written in R as alternations of literals with a
// handful of anchors. Spelling them out here keeps the crate free of a regex
// dependency and makes each R pattern's Rust equivalent readable at the branch
// that uses it.
// ---------------------------------------------------------------------------

/// Any of the literal substrings is present.
pub fn any_of(s: &str, patterns: &[&str]) -> bool {
    patterns.iter().any(|p| s.contains(p))
}

/// R `(^|_)tok($|_)`: the token stands alone between underscores.
pub fn token(s: &str, tok: &str) -> bool {
    let b = s.as_bytes();
    let t = tok.as_bytes();
    if t.is_empty() || t.len() > b.len() {
        return false;
    }
    (0..=(b.len() - t.len())).any(|i| {
        &b[i..i + t.len()] == t
            && (i == 0 || b[i - 1] == b'_')
            && (i + t.len() == b.len() || b[i + t.len()] == b'_')
    })
}

/// R `(^|_)tok(suffix|$)`; `allow_end` covers the `$` alternative.
pub fn token_suffixed(s: &str, tok: &str, suffixes: &[&str], allow_end: bool) -> bool {
    let b = s.as_bytes();
    let t = tok.as_bytes();
    if t.is_empty() || t.len() > b.len() {
        return false;
    }
    (0..=(b.len() - t.len())).any(|i| {
        if &b[i..i + t.len()] != t || !(i == 0 || b[i - 1] == b'_') {
            return false;
        }
        let rest = &s[i + t.len()..];
        (allow_end && rest.is_empty()) || suffixes.iter().any(|suf| rest.starts_with(suf))
    })
}

/// R `\btok\b` with `\w` = `[A-Za-z0-9_]`.
pub fn word(s: &str, tok: &str) -> bool {
    let is_w = |c: u8| c.is_ascii_alphanumeric() || c == b'_';
    let b = s.as_bytes();
    let t = tok.as_bytes();
    if t.is_empty() || t.len() > b.len() {
        return false;
    }
    (0..=(b.len() - t.len())).any(|i| {
        &b[i..i + t.len()] == t
            && (i == 0 || !is_w(b[i - 1]))
            && (i + t.len() == b.len() || !is_w(b[i + t.len()]))
    })
}

/// R `a.*b`: some occurrence of `b` starts at or after the end of some `a`.
pub fn seq2(s: &str, a: &str, b: &str) -> bool {
    match s.find(a) {
        Some(i) => s[i + a.len()..].contains(b),
        None => false,
    }
}

/// R `^[0-9]+$`.
pub fn all_digits(s: &str) -> bool {
    !s.is_empty() && s.bytes().all(|c| c.is_ascii_digit())
}

/// R `sub(".*?([0-9]{1,2}) digit.*", "\\1", text)` then `as.integer`.
///
/// The lazy `.*?` means the leftmost start position wins, and `{1,2}` is greedy
/// there, so "123 digit" yields 23 rather than 1 or 123.
pub fn digits_before_digit_word(text: &str) -> Option<i32> {
    let b = text.as_bytes();
    for start in 0..b.len() {
        for len in [2usize, 1] {
            if start + len <= b.len()
                && b[start..start + len].iter().all(u8::is_ascii_digit)
                && text[start + len..].starts_with(" digit")
            {
                return text[start..start + len].parse::<i32>().ok();
            }
        }
    }
    None
}

/// R `sub(".*?([0-9]+) digit.*", "\\1", text)` then `as.integer`.
///
/// The difference from `digits_before_digit_word` is the unbounded `+`: this
/// takes the whole digit run, so "100 digit" yields 100. The leftmost run that
/// is followed by " digit" wins, and a run that is not cannot succeed at any
/// start inside itself, so no backtracking is needed.
pub fn digits_run_before_digit_word(text: &str) -> Option<i32> {
    let b = text.as_bytes();
    let mut i = 0usize;
    while i < b.len() {
        if b[i].is_ascii_digit() {
            let start = i;
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
            if text[i..].starts_with(" digit") {
                return text[start..i].parse::<i32>().ok();
            }
        } else {
            i += 1;
        }
    }
    None
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
        assert_eq!(pick_codes(&[], 3, 1, 0, true, None), vec![0, 0, 0]);
        // substantive-only: every row is one of the codes
        let out = pick_codes(&[1, 2, 3], 10, 5, 7, false, None);
        assert!(out.iter().all(|c| [1, 2, 3].contains(c)));
        // the sentinels are never dealt unless include_missing is set
        let bn: Vec<f64> = (1..=40).map(|i| i as f64 * 1_000_003.0).collect();
        let kept = pick_codes(&[0, 1, 88_888_888], 40, 3, 11, false, Some(&bn));
        assert!(kept.iter().all(|c| [0, 1].contains(c)));
        let with_missing = pick_codes(&[0, 1, 88_888_888], 40, 3, 11, true, Some(&bn));
        assert!(with_missing.iter().all(|c| [0, 1, 88_888_888].contains(c)));
    }

    #[test]
    fn round_matches_r_ties_to_even() {
        // R: round(c(0.145, 0.155, 2.675, 359.525, 1.005, 1234.565, -0.145, 0), 2)
        assert_eq!(round2(0.145), 0.14);
        assert_eq!(round2(0.155), 0.16);
        assert_eq!(round2(2.675), 2.67);
        assert_eq!(round2(359.525), 359.52);
        assert_eq!(round2(1.005), 1.0);
        assert_eq!(round2(1234.565), 1234.57);
        assert_eq!(round2(-0.145), -0.14);
        assert_eq!(round2(0.0), 0.0);
        // R: round(c(0.25, 0.35, 2.45, 2.55), 1)
        assert_eq!(round1(0.25), 0.2);
        assert_eq!(round1(0.35), 0.3);
        assert_eq!(round1(2.45), 2.5);
        assert_eq!(round1(2.55), 2.5);
    }

    #[test]
    fn r_mod_matches_r_doubles() {
        // R: 7 %% 3 == 1; -7 %% 3 == 2; 8.8e17 %% 2147483647 == 1352036352
        assert_eq!(r_mod(7.0, 3.0), 1.0);
        assert_eq!(r_mod(-7.0, 3.0), 2.0);
        let big = 104_729.0 * 3_000_000.0 * 3_000_000.0;
        let m = 2_147_483_647.0;
        assert_eq!(r_mod(big, m), big - (big / m).floor() * m);
    }

    #[test]
    fn draws_stay_inside_the_modulus() {
        let bn: Vec<f64> = (1..=50).map(|i| 65_100_000_000.0 + i as f64).collect();
        let d = blade_draw(&bn, 42, 913, 100);
        assert_eq!(d.len(), 50);
        assert!(d.iter().all(|v| (0..100).contains(v)));
        // Both branches of the site/role split respect their documented bounds.
        let payees = vec![6i32; 50];
        let locs = count_value("total_locations", Some(&payees), &bn, 42, 913);
        assert!(locs.iter().all(|v| (1..=6).contains(v)));
        let mgrs = count_value("managers", Some(&payees), &bn, 42, 913);
        assert!(mgrs.iter().all(|v| (1..=5).contains(v)));
        // R's `pmax(0L, NULL)` collapses every branch that reads the payee base.
        assert_eq!(count_value("total_locations", None, &bn, 42, 913).len(), 50);
        assert!(count_value("managers", None, &bn, 42, 913).is_empty());
        assert!(count_value("vacancies", None, &bn, 42, 913).is_empty());
    }

    #[test]
    fn character_codes_carry_a_letter() {
        let out = character_code(5, 42, 913, 6, "K");
        assert_eq!(out.len(), 5);
        assert!(out.iter().all(|s| s.starts_with('K') && s.len() == 7));
    }

    #[test]
    fn name_salt_matches_r() {
        // R: hash <- 17; for (i in 1:2) hash <- (hash*131 + utf8ToInt("bn")[i]
        //    + i*17) %% 2147483647   ->  17*131+98+17 = 2342; 2342*131+110+34
        assert_eq!(name_salt("bn"), 2342 * 131 + 110 + 34);
        assert_eq!(name_salt(""), 17);
    }

    #[test]
    fn string_predicates() {
        assert!(token("investors_swf_n", "n"));
        assert!(!token("nature", "n"));
        assert!(token("date_of_birth", "date"));
        assert!(word("area (ha)", "ha"));
        assert!(!word("shark", "ha"));
        assert!(seq2("everything else is invalid", "everything else", "invalid"));
        assert!(!seq2("invalid, everything else", "everything else", "invalid"));
        assert!(token_suffixed("employer_id", "employer", &["_id"], true));
        assert!(token_suffixed("x_employer", "employer", &["_id"], true));
        assert!(all_digits("0101"));
        assert!(!all_digits("01a"));
        assert_eq!(digits_before_digit_word("123 digit"), Some(23));
        assert_eq!(digits_before_digit_word("6 digit code"), Some(6));
        assert_eq!(digits_before_digit_word("numeric ($)"), None);
    }

    #[test]
    fn bcs_and_ports_stay_in_frame() {
        let codes = bcs_code(200, 42, true);
        assert!(codes
            .iter()
            .all(|c| [0, 1, 7_777_777, 88_888_888, 999_999_999].contains(c)));
        let states: Vec<i32> = (1..=8).collect();
        let ports = australian_port_code(&states, 42, 913);
        for (i, p) in ports.iter().enumerate() {
            assert!(PORT_POOLS[i].contains(&p.as_str()));
        }
    }

    #[test]
    fn location_indices_respect_state() {
        let bn: Vec<f64> = (1..=10).map(|i| 65_100_000_000.0 + i as f64).collect();
        let state: Vec<i32> = (1..=10).map(|i| ((i - 1) % 8) + 1).collect();
        let lookup_state: Vec<i32> = (0..80).map(|i| (i % 8) + 1).collect();
        let idx = location_lookup_indices(&bn, &state, 42, &lookup_state).unwrap();
        for (i, &j) in idx.iter().enumerate() {
            assert_eq!(lookup_state[j], state[i]);
        }
        assert!(location_lookup_indices(&bn, &state, 42, &[1, 1, 1]).is_err());
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

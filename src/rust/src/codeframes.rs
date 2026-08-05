//! Canonical shared code frames.
//!
//! A single source of truth for the classification code sets that must
//! be consistent across every PLIDA/BLADE dataset: state/territory, ASGS
//! geography, SACC country of birth, ANZSIC industry, AVETMISS state, and
//! visa subclass. Historically each generator carried its own (often
//! incompatible) version of these — STATE was emitted four different ways
//! and country of birth collapsed to a 0/1 flag under column names
//! reserved for SACC codes, corrupting cross-dataset joins.
//!
//! Design goals:
//!   * Authoritative codes, synthetic distributions. The *codes* emitted
//!     are genuine classification members; the sampling *weights* are
//!     approximate and explicitly synthetic.
//!   * Zero per-row allocation in the hot path. Reference tables are
//!     parsed once into `LazyLock` statics; per-person use is an O(1) or
//!     O(log k) lookup over borrowed data with no `String` churn.
//!   * Embedded at compile time via `include_str!` so the codes ship with
//!     the binary and are available to the pure-Rust spine builder without
//!     threading lookup tables through every entry point. The source TSVs
//!     also live in `inst/extdata/codeframes/` for provenance and R use.
//!
//! Source provenance (see `inst/extdata/codeframes/`):
//!   * `state.tsv`        — ABS states/territories + AVETMISS 2-char codes
//!   * `sa2_lookup.tsv`   — ASGS 2021 SA2->SA3->SA4->state, derived from
//!                          `inst/extdata/mb_lookup.csv.gz` (meshblock count
//!                          as a size proxy weight)
//!   * `anzsic2006.tsv`   — ANZSIC 2006 division/class (via `strayr`)
//!   * `sacc_country.tsv` — SACC 2016 country of birth (ABS Census 2021)
//!   * `visa_subclass.tsv`— Department of Home Affairs visa subclasses

use rand::distributions::WeightedIndex;
use rand::prelude::Distribution;
use rand::Rng;
use std::sync::LazyLock;

// ============================================================================
// State / territory
// ============================================================================

pub struct StateInfo {
    pub code: u8,
    pub name: &'static str,
    pub abbr: &'static str,
    /// AVETMISS two-character state identifier (NCVER / VET collections).
    pub avetmiss: &'static str,
}

/// ABS state/territory order (STE 2021): 1=NSW .. 8=ACT, 9=Other Territories.
/// This ordering is the canonical spine `state` code and matches the Census
/// foundation, AVETMISS `01`..`08`, and the ASGS geography lookup.
pub const STATES: [StateInfo; 9] = [
    StateInfo {
        code: 1,
        name: "New South Wales",
        abbr: "NSW",
        avetmiss: "01",
    },
    StateInfo {
        code: 2,
        name: "Victoria",
        abbr: "VIC",
        avetmiss: "02",
    },
    StateInfo {
        code: 3,
        name: "Queensland",
        abbr: "QLD",
        avetmiss: "03",
    },
    StateInfo {
        code: 4,
        name: "South Australia",
        abbr: "SA",
        avetmiss: "04",
    },
    StateInfo {
        code: 5,
        name: "Western Australia",
        abbr: "WA",
        avetmiss: "05",
    },
    StateInfo {
        code: 6,
        name: "Tasmania",
        abbr: "TAS",
        avetmiss: "06",
    },
    StateInfo {
        code: 7,
        name: "Northern Territory",
        abbr: "NT",
        avetmiss: "07",
    },
    StateInfo {
        code: 8,
        name: "Australian Capital Territory",
        abbr: "ACT",
        avetmiss: "08",
    },
    StateInfo {
        code: 9,
        name: "Other Territories",
        abbr: "OT",
        avetmiss: "09",
    },
];

#[inline]
fn state_info(code: i32) -> Option<&'static StateInfo> {
    if code >= 1 && code <= 9 {
        Some(&STATES[(code - 1) as usize])
    } else {
        None
    }
}

/// State abbreviation (NSW, VIC, ...) or "" if the code is out of range.
#[inline]
pub fn state_abbr(code: i32) -> &'static str {
    state_info(code).map(|s| s.abbr).unwrap_or("")
}

/// Full state name, or "" if the code is out of range.
#[inline]
pub fn state_name(code: i32) -> &'static str {
    state_info(code).map(|s| s.name).unwrap_or("")
}

/// AVETMISS two-character state code; "99" (not stated) if out of range.
#[inline]
pub fn state_avetmiss(code: i32) -> &'static str {
    state_info(code).map(|s| s.avetmiss).unwrap_or("99")
}

// ============================================================================
// ASGS 2021 geography
// ============================================================================

const SA2_TSV: &str = include_str!("../../../inst/extdata/codeframes/sa2_lookup.tsv");

/// A statistical area, with its nesting up the ASGS hierarchy.
#[derive(Clone, Copy)]
pub struct Sa2 {
    pub sa2: u32,
    pub sa3: u32,
    pub sa4: u16,
    pub state: u8,
}

struct GeoIndex {
    /// SA2 records bucketed by state code (index = state code; 0 unused).
    by_state: Vec<Vec<Sa2>>,
    /// A population-proxy weighted index per state for O(log k) sampling.
    dist: Vec<Option<WeightedIndex<f64>>>,
}

static GEO: LazyLock<GeoIndex> = LazyLock::new(|| {
    let mut by_state: Vec<Vec<Sa2>> = vec![Vec::new(); 10];
    let mut weights: Vec<Vec<f64>> = vec![Vec::new(); 10];
    for line in SA2_TSV.lines().skip(1) {
        if line.is_empty() {
            continue;
        }
        // sa2_code \t sa3_code \t sa4_code \t state \t mb_count
        let mut f = line.split('\t');
        let sa2 = f.next().and_then(|s| s.parse::<u32>().ok());
        let sa3 = f.next().and_then(|s| s.parse::<u32>().ok());
        let sa4 = f.next().and_then(|s| s.parse::<u16>().ok());
        let state = f.next().and_then(|s| s.parse::<u8>().ok());
        let w = f.next().and_then(|s| s.parse::<f64>().ok()).unwrap_or(1.0);
        if let (Some(sa2), Some(sa3), Some(sa4), Some(state)) = (sa2, sa3, sa4, state) {
            if (state as usize) < 10 {
                by_state[state as usize].push(Sa2 {
                    sa2,
                    sa3,
                    sa4,
                    state,
                });
                weights[state as usize].push(w.max(1.0));
            }
        }
    }
    let dist: Vec<Option<WeightedIndex<f64>>> = weights
        .iter()
        .map(|w| {
            if w.is_empty() {
                None
            } else {
                WeightedIndex::new(w).ok()
            }
        })
        .collect();
    GeoIndex { by_state, dist }
});

/// Sample an SA2 (with its SA3/SA4 nesting) for a person in `state`,
/// weighted by meshblock count. Returns `None` if the state has no
/// geography (e.g. an out-of-range state code).
#[inline]
pub fn sample_sa2(state: u8, rng: &mut impl Rng) -> Option<Sa2> {
    let s = state as usize;
    if s >= GEO.by_state.len() {
        return None;
    }
    let dist = GEO.dist[s].as_ref()?;
    let idx = dist.sample(rng);
    GEO.by_state[s].get(idx).copied()
}

/// Total number of SA2s loaded (across all states). Used in tests.
pub fn sa2_count() -> usize {
    GEO.by_state.iter().map(|v| v.len()).sum()
}

// ============================================================================
// SACC 2016 country of birth
// ============================================================================

const SACC_TSV: &str = include_str!("../../../inst/extdata/codeframes/sacc_country.tsv");

/// SACC code for Australia (born in Australia).
pub const SACC_AUSTRALIA: i32 = 1101;
/// Census "not stated" supplementary code for country of birth (4-char).
pub const SACC_NOT_STATED: &str = "&&&&";
/// Census "overseas visitor" supplementary code (4-char).
pub const SACC_OVERSEAS_VISITOR: &str = "VVVV";

struct CountryTable {
    /// Overseas country codes (excludes Australia).
    overseas_codes: Vec<i32>,
    overseas_dist: Option<WeightedIndex<f64>>,
    /// All codes (incl. Australia) -> label, for label resolution.
    labels: Vec<(i32, &'static str)>,
}

static SACC: LazyLock<CountryTable> = LazyLock::new(|| {
    let mut overseas_codes = Vec::new();
    let mut overseas_w = Vec::new();
    let mut labels = Vec::new();
    for line in SACC_TSV.lines().skip(1) {
        if line.is_empty() {
            continue;
        }
        // code \t label \t weight
        let mut f = line.split('\t');
        let code = f.next().and_then(|s| s.parse::<i32>().ok());
        let label = f.next().unwrap_or("");
        let w = f.next().and_then(|s| s.parse::<f64>().ok()).unwrap_or(1.0);
        if let Some(code) = code {
            labels.push((code, label));
            if code != SACC_AUSTRALIA {
                overseas_codes.push(code);
                overseas_w.push(w.max(1.0));
            }
        }
    }
    let overseas_dist = if overseas_w.is_empty() {
        None
    } else {
        WeightedIndex::new(&overseas_w).ok()
    };
    CountryTable {
        overseas_codes,
        overseas_dist,
        labels,
    }
});

/// Intern a &str from the embedded (already-'static) TSV. `include_str!`
/// yields a `&'static str`, and `lines()`/`split()` borrow from it, so the
/// slices are themselves 'static — this is a no-op transmute of lifetime
/// that is sound because the backing data is a compile-time constant.
#[inline]
fn leak_static(s: &str) -> &'static str {
    // SAFETY: `s` borrows from `SACC_TSV`, a `&'static str` produced by
    // `include_str!`; the referent lives for the whole program.
    unsafe { std::mem::transmute::<&str, &'static str>(s) }
}

/// Sample a SACC country code for an overseas-born person, weighted by the
/// (synthetic, Census-anchored) country-of-birth distribution.
#[inline]
pub fn sample_overseas_country(rng: &mut impl Rng) -> i32 {
    match SACC.overseas_dist.as_ref() {
        Some(d) => SACC.overseas_codes[d.sample(rng)],
        None => SACC_AUSTRALIA,
    }
}

/// Resolve a SACC code to its label, or "" if unknown.
pub fn country_label(code: i32) -> &'static str {
    SACC.labels
        .iter()
        .find(|(c, _)| *c == code)
        .map(|(_, l)| *l)
        .unwrap_or("")
}

/// Number of distinct SACC codes loaded (incl. Australia). Used in tests.
pub fn sacc_count() -> usize {
    SACC.labels.len()
}

// ============================================================================
// Visa subclass (Department of Home Affairs)
// ============================================================================

const VISA_TSV: &str = include_str!("../../../inst/extdata/codeframes/visa_subclass.tsv");

pub struct VisaSubclass {
    pub code: &'static str,
    pub label: &'static str,
    pub stream: &'static str,
}

static VISA: LazyLock<Vec<VisaSubclass>> = LazyLock::new(|| {
    let mut v = Vec::new();
    for line in VISA_TSV.lines().skip(1) {
        if line.is_empty() {
            continue;
        }
        // code \t label \t stream
        let mut f = line.split('\t');
        let code = f.next().unwrap_or("");
        let label = f.next().unwrap_or("");
        let stream = f.next().unwrap_or("");
        if !code.is_empty() {
            v.push(VisaSubclass {
                code: leak_static(code),
                label: leak_static(label),
                stream: leak_static(stream),
            });
        }
    }
    v
});

/// All visa subclasses.
pub fn visa_subclasses() -> &'static [VisaSubclass] {
    &VISA
}

/// Sample a visa subclass uniformly (distribution is synthetic; callers that
/// need a stream-conditioned draw should filter `visa_subclasses()`).
pub fn sample_visa_subclass(rng: &mut impl Rng) -> &'static VisaSubclass {
    let v = visa_subclasses();
    &v[rng.gen_range(0..v.len())]
}

// ============================================================================
// Labeled string-code frames (ASCRG religion, ASCL language)
// ============================================================================
//
// Both are `code<TAB>label<TAB>weight` tables whose codes are strings (so the
// supplementary `&&` not-stated code coexists with numeric codes). Parsed once
// into a borrowed code list + a weighted sampler.

struct LabeledFrame {
    codes: Vec<&'static str>,
    dist: Option<WeightedIndex<f64>>,
}

fn parse_labeled_frame(tsv: &'static str) -> LabeledFrame {
    let mut codes = Vec::new();
    let mut weights = Vec::new();
    for line in tsv.lines().skip(1) {
        if line.is_empty() {
            continue;
        }
        let mut f = line.split('\t');
        let code = f.next().unwrap_or("");
        let _label = f.next();
        let w = f.next().and_then(|s| s.parse::<f64>().ok()).unwrap_or(1.0);
        if !code.is_empty() {
            codes.push(code);
            weights.push(w.max(1.0));
        }
    }
    let dist = if weights.is_empty() {
        None
    } else {
        WeightedIndex::new(&weights).ok()
    };
    LabeledFrame { codes, dist }
}

const RELIGION_TSV: &str = include_str!("../../../inst/extdata/codeframes/ascrg_religion.tsv");
static RELIGION: LazyLock<LabeledFrame> = LazyLock::new(|| parse_labeled_frame(RELIGION_TSV));

/// Sample an ASCRG religion code (Census RELP), weighted by the real 2021
/// Census narrow-group distribution. Returns the code string (`&&` for not
/// stated). Falls back to `&&` if the table failed to load.
#[inline]
pub fn sample_religion(rng: &mut impl Rng) -> &'static str {
    match RELIGION.dist.as_ref() {
        Some(d) => RELIGION.codes[d.sample(rng)],
        None => "&&",
    }
}

/// Number of religion codes loaded. Used in tests.
pub fn religion_count() -> usize {
    RELIGION.codes.len()
}

const LANGUAGE_TSV: &str = include_str!("../../../inst/extdata/codeframes/ascl_language.tsv");
static LANGUAGE: LazyLock<LabeledFrame> = LazyLock::new(|| parse_labeled_frame(LANGUAGE_TSV));

/// ASCL language code for English (Census LANP).
pub const LANP_ENGLISH: &str = "1201";
/// Census "not stated" supplementary code for language used at home (4-char).
pub const LANP_NOT_STATED: &str = "&&&&";

/// Sample an ASCL language-used-at-home code (Census LANP), weighted by the
/// real 2021 Census distribution. Returns the code string.
#[inline]
pub fn sample_language(rng: &mut impl Rng) -> &'static str {
    match LANGUAGE.dist.as_ref() {
        Some(d) => LANGUAGE.codes[d.sample(rng)],
        None => LANP_ENGLISH,
    }
}

/// Number of language codes loaded. Used in tests.
pub fn language_count() -> usize {
    LANGUAGE.codes.len()
}

// ============================================================================
// ANZSCO not-applicable sentinel
// ============================================================================

/// Canonical "no occupation" / not-applicable ANZSCO code used on the spine
/// and in projections (a person not employed or with no occupation). Kept as
/// a single constant so generators stop inventing their own NA sentinels.
pub const ANZSCO_NA: i32 = 0;

#[cfg(test)]
mod tests {
    use super::*;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    #[test]
    fn states_round_trip() {
        assert_eq!(state_abbr(1), "NSW");
        assert_eq!(state_avetmiss(8), "08");
        assert_eq!(state_avetmiss(99), "99");
        assert_eq!(state_abbr(0), "");
    }

    #[test]
    fn geography_loads_and_samples() {
        assert!(sa2_count() > 2000, "expected the full SA2 table");
        let mut rng = StdRng::seed_from_u64(1);
        let s = sample_sa2(1, &mut rng).expect("NSW has geography");
        assert_eq!(s.state, 1);
        // SA2 nests under its SA4 and SA3.
        assert_eq!(s.sa2 / 1_000_000, s.sa4 as u32);
        assert_eq!(s.sa2 / 10_000, s.sa3);
    }

    #[test]
    fn sacc_samples_overseas() {
        assert!(sacc_count() >= 5);
        let mut rng = StdRng::seed_from_u64(2);
        for _ in 0..100 {
            let c = sample_overseas_country(&mut rng);
            assert_ne!(c, SACC_AUSTRALIA);
            assert!(c >= 1000 && c <= 9999, "SACC code must be 4 digits");
        }
    }
}

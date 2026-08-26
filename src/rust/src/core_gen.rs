use extendr_api::prelude::*;
use rand::prelude::*;
use rand::rngs::StdRng;
use std::fs;

// Mortality rates by 8 age bands: <20, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+
const MORTALITY: [f64; 8] = [
    0.0003, 0.0005, 0.0007, 0.0012, 0.0030, 0.0070, 0.0170, 0.0550,
];

const OVERSEAS_CODES: [&str; 20] = [
    "2100", "7100", "6100", "5101", "5201", "5203", "5105", "6102", "7103", "6104", "3206", "3103",
    "2201", "7105", "3207", "5204", "7108", "6105", "3104", "2304",
];

const OVERSEAS_WEIGHTS: [f64; 20] = [
    927490.0, 673354.0, 549628.0, 530491.0, 293899.0, 257997.0, 189204.0, 165605.0, 163329.0,
    131907.0, 122507.0, 102087.0, 101309.0, 101256.0, 100158.0, 92925.0, 92305.0, 89636.0, 87343.0,
    87068.0,
];

fn age_band_8(age: i32) -> usize {
    match age {
        a if a < 20 => 0,
        a if a < 30 => 1,
        a if a < 40 => 2,
        a if a < 50 => 3,
        a if a < 60 => 4,
        a if a < 70 => 5,
        a if a < 80 => 6,
        _ => 7,
    }
}

/// Project CORE demographics from spine vectors.
/// @export
#[extendr]
fn project_core_demographics__(
    spine_id: Strings,
    birth_year: &[i32],
    sex: &[i32],
    country_of_birth_sacc: &[i32],
    seed: i32,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(700));

    let spine_ids: Vec<Option<String>> = spine_id
        .iter()
        .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
        .collect();
    let mut month_of_birth: Vec<i32> = Vec::with_capacity(n);
    let mut birth_ctry_code: Vec<String> = Vec::with_capacity(n);
    let mut core_gender: Vec<String> = Vec::with_capacity(n);
    let mut year_of_death: Vec<Rint> = Vec::with_capacity(n);
    let mut month_of_death: Vec<Rint> = Vec::with_capacity(n);
    let mut day_of_death: Vec<Rint> = Vec::with_capacity(n);

    for i in 0..n {
        let age = 2021 - birth_year[i];

        month_of_birth.push(rng.gen_range(1..=12));

        // BIRTH_CTRY_CODE is the spine SACC country code, so CORE, Census and
        // every other PLIDA dataset resolve the same person to the same country.
        birth_ctry_code.push(country_of_birth_sacc[i].to_string());

        core_gender.push(if sex[i] == 1 {
            "M".to_string()
        } else {
            "F".to_string()
        });

        // Year-by-year survival hazard over the 2006-2024 observation window:
        // step through each year applying that year's age-specific mortality
        // rate, so the death YEAR follows the compounding hazard (later years
        // more likely as the person ages) instead of a uniform draw, and total
        // mortality stays consistent with the per-year rates.
        let _ = age; // 2021-reference age retained for other columns
        let mut died_year: Option<i32> = None;
        for y in 2006..=2024 {
            let age_y = y - birth_year[i];
            if age_y < 0 {
                continue; // not yet born
            }
            let rate_y = MORTALITY[age_band_8(age_y)];
            if rng.gen::<f64>() < rate_y {
                died_year = Some(y);
                break;
            }
        }
        if let Some(y) = died_year {
            year_of_death.push(Rint::from(y));
            month_of_death.push(Rint::from(rng.gen_range(1..=12)));
            day_of_death.push(Rint::from(rng.gen_range(1..=28)));
        } else {
            year_of_death.push(Rint::na());
            month_of_death.push(Rint::na());
            day_of_death.push(Rint::na());
        }
    }

    list!(
        SPINE_ID = spine_ids,
        YEAR_OF_BIRTH = birth_year.to_vec(),
        MONTH_OF_BIRTH = month_of_birth,
        BIRTH_CTRY_CODE = birth_ctry_code,
        CORE_GENDER = core_gender,
        YEAR_OF_DEATH = year_of_death,
        MONTH_OF_DEATH = month_of_death,
        DAY_OF_DEATH = day_of_death,
    )
}

/// Project CORE relationships from spine vectors.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_core_locations__(
    spine_id: Strings,
    state: &[i32],
    sa2: &[i32],
    dwelling_id: &[i32],
    lookup_state: &[i32],
    lookup_mb_code: Strings,
    lookup_sa1_code: Strings,
    lookup_sa2_code: Strings,
    lookup_sa4_code: &[i32],
    seed: i32,
) -> List {
    let n = state.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(701));

    let spine_ids: Vec<Option<String>> = spine_id
        .iter()
        .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
        .collect();
    let lookup_mb: Vec<String> = lookup_mb_code.iter().map(|s| s.to_string()).collect();
    let lookup_sa1: Vec<String> = lookup_sa1_code.iter().map(|s| s.to_string()).collect();
    let lookup_sa2: Vec<String> = lookup_sa2_code.iter().map(|s| s.to_string()).collect();

    let mut by_state: Vec<Vec<usize>> = vec![Vec::new(); 9];
    for (i, &st) in lookup_state.iter().enumerate() {
        if (1..=8).contains(&st) {
            by_state[st as usize].push(i);
        }
    }

    // Mesh blocks indexed by their SA2, so a person's CORE address is drawn
    // from inside the SA2 the spine already assigned them. ASGS 2021 geography
    // then agrees between CORE and Census (and NDIS, DEX, NACDC), instead of
    // CORE re-drawing an unrelated mesh block from anywhere in the state.
    // Rows are pushed in lookup order and only ever read by key, so the map's
    // iteration order never reaches the RNG.
    let mut by_sa2: std::collections::HashMap<i32, Vec<usize>> =
        std::collections::HashMap::new();
    for (i, s) in lookup_sa2.iter().enumerate() {
        if let Ok(code) = s.parse::<i32>() {
            by_sa2.entry(code).or_default().push(i);
        }
    }

    let mut out_sa1: Vec<String> = Vec::with_capacity(n);
    let mut out_sa2: Vec<String> = Vec::with_capacity(n);
    let mut out_sa4: Vec<i32> = Vec::with_capacity(n);
    let mut out_mb: Vec<String> = Vec::with_capacity(n);
    let mut out_arid: Vec<String> = Vec::with_capacity(n);
    let out_adr_typ: Vec<&str> = vec!["R"; n];
    let out_source_flag: Vec<&str> = vec!["CENSUS"; n];
    let out_start_date: Vec<&str> = vec!["2006-01-01"; n];
    let out_end_date: Vec<Option<String>> = vec![None; n];

    // The ARID used to be this random base plus a row counter. It is now
    // derived from the person, but the draw stays exactly as it was: dropping
    // it would shift every later draw and silently move every person's mesh
    // block for a given seed, which is a far larger change than the one
    // intended here.
    let max_base = 0xFFFF_FFFF_FFFFu64.saturating_sub(n as u64);
    let _stream_position = if max_base == 0 {
        0
    } else {
        rng.gen_range(0..=max_base)
    };

    let fallback = by_state
        .iter()
        .find(|rows| !rows.is_empty())
        .and_then(|rows| rows.first().copied())
        .unwrap_or(0);

    for i in 0..n {
        let st = state[i].clamp(1, 8) as usize;
        let state_pool = if by_state[st].is_empty() {
            &by_state[1]
        } else {
            &by_state[st]
        };
        // Restrict to the spine SA2; fall back to the state pool when the
        // spine SA2 is 0/NA or absent from the mesh block lookup.
        let sa2_pool = if sa2[i] > 0 { by_sa2.get(&sa2[i]) } else { None };
        let pool = match sa2_pool {
            Some(rows) if !rows.is_empty() => rows,
            _ => state_pool,
        };
        let dwelling = dwelling_id.get(i).copied().unwrap_or(0);
        // The address below the SA2 is a pure function of the dwelling, not a
        // draw. Every resident of one dwelling therefore lands on the same mesh
        // block and the same SA1, and — more importantly — the R side computes
        // the identical value from the spine columns alone, with no shared
        // state and no central pass. `.dil_asgs_2021_value()` indexes the same
        // pool the same way; keep the two in step.
        let lookup_idx = if pool.is_empty() {
            fallback
        } else if dwelling > 0 {
            pool[(dwelling as usize) % pool.len()]
        } else {
            pool[rng.gen_range(0..pool.len())]
        };

        let sa1 = lookup_sa1[lookup_idx].clone();
        let sa2 = lookup_sa2[lookup_idx].clone();
        let mb = lookup_mb[lookup_idx].clone();

        out_sa1.push(sa1.clone());
        out_sa2.push(sa2);
        out_sa4.push(lookup_sa4_code[lookup_idx]);
        out_mb.push(mb);
        // An ARID stands for an address, so it is derived from the dwelling and
        // the seed rather than drawn: one household's address in the ATO,
        // Centrelink and Medicare products has to carry the same value. The R
        // helper `.address_key_hex()` computes this identically, and the two
        // must stay in step.
        // Both sides reduce the person modulo 2^32 first. R does this
        // arithmetic in doubles, which are exact only below 2^53, and
        // 2^32 * 1000003 leaves room.
        // A spine id reads SP0000000109, so the digits have to be pulled out
        // of the string; `.person_number()` on the R side does the same.
        let person = spine_ids[i]
            .as_deref()
            .and_then(|s| {
                let digits: String = s.chars().filter(char::is_ascii_digit).collect();
                digits.parse::<u64>().ok()
            })
            .unwrap_or(i as u64 + 1);
        // The dwelling, not the person: an ARID stands for an address, and the
        // people who live at one address share it. A row with no dwelling keeps
        // its own key rather than joining everyone else who has none.
        let subject = if dwelling > 0 {
            dwelling as u64
        } else {
            person
        } % (1u64 << 32);
        let key = subject
            .wrapping_mul(1_000_003)
            .wrapping_add((seed.unsigned_abs() as u64).wrapping_mul(104_729))
            % (1u64 << 47);
        out_arid.push(format!("{:012X}", key));
    }

    list!(
        SPINE_ID = spine_ids,
        STATE = state.to_vec(),
        SA4_ASGS_2021 = out_sa4,
        SA2_ASGS_2021 = out_sa2,
        SA1_ASGS_2021 = out_sa1,
        MB_ASGS_2021 = out_mb,
        ARID = out_arid,
        ADR_TYP = out_adr_typ,
        SOURCE_FLAG = out_source_flag,
        START_DATE = out_start_date,
        END_DATE = out_end_date,
    )
}

/// Project CORE residence directly to a parquet part directory.
///
/// Writes one part per calendar month:
/// `{out_dir}/{product_name}/part-YYYYMM.parquet`.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_core_residence_to_parquet__(
    spine_id: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    country_of_birth: &[i32],
    year_of_arrival: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    residence_seed: &[i32],
    min_year: i32,
    max_year: i32,
    out_dir: &str,
    product_name: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    use crate::residence::{month_end_date32, pp_weight_for_month};

    let n = birth_year.len();
    let prod_dir = format!("{}/{}", out_dir, product_name);
    fs::create_dir_all(&prod_dir).unwrap_or_else(|e| panic!("create {}: {}", prod_dir, e));

    let spine_ids: Vec<Option<String>> = spine_id
        .iter()
        .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
        .collect();
    let mut total: i64 = 0;

    for year in min_year..=max_year {
        for month in 1..=12 {
            let period = month_end_date32(year, month);
            let mut pp_period: Vec<i32> = Vec::with_capacity(n);
            let mut pp_weight: Vec<f64> = Vec::with_capacity(n);

            for i in 0..n {
                pp_period.push(period);
                pp_weight.push(pp_weight_for_month(
                    birth_year[i],
                    month_of_birth[i],
                    country_of_birth[i],
                    year_of_arrival[i],
                    year_of_death[i],
                    month_of_death[i],
                    day_of_death[i],
                    residence_seed[i],
                    year,
                    month,
                ));
            }

            let path = format!("{}/part-{:04}{:02}.parquet", prod_dir, year, month);
            let cols = vec![
                NamedCol {
                    name: "spine_id",
                    col: Col::StrOpt(spine_ids.clone()),
                },
                NamedCol {
                    name: "pp_period",
                    col: Col::DateNN(pp_period),
                },
                NamedCol {
                    name: "pp_weight",
                    col: Col::F64(pp_weight),
                },
            ];
            write_columns_to_parquet(&path, cols)
                .unwrap_or_else(|e| panic!("core residence parquet write {}: {}", path, e));
            total += n as i64;
        }
    }

    total.min(i32::MAX as i64) as i32
}

// -- Core Relationships ------------------------------------------------------
//
// Relationships and residence are one story. A couple that separates leaves one
// address and opens another, and a household that never separates still reports
// its move one person at a time. Both come out of the same household pass, so
// the relationship record and the address history cannot contradict each other.
//
// Every parameter below has an R twin in `R/generate_core.R` carrying the same
// value and the reasoning behind it. Keep the two in step.

/// The year household composition is read at. The spine builds its households
/// around the 2021 Census, so every age test here is an age at that Census.
const CORE_REFERENCE_YEAR: i32 = 2021;

/// 2021 Census night, the date `.census_present_on_night()` also uses.
const CORE_CENSUS_NIGHT: Ymd = Ymd { y: 2021, m: 8, d: 10 };

/// The last year a relationship can end in. The generated window runs to 2025.
const CORE_LAST_YEAR: i32 = 2025;

/// Core Locations opens its history here, so a move before this date has no
/// spell to close.
const CORE_LOCATIONS_START: Ymd = Ymd { y: 2006, m: 1, d: 1 };

/// A couple is two adults close in age. Beyond this gap they read as a parent
/// and an adult child. `.CENSUS_COUPLE_MAX_AGE_GAP` holds the same value, and
/// the two rules must agree or CORE and the Census name different couples.
const CORE_COUPLE_MAX_AGE_GAP: i32 = 18;

/// Share of couples in a registered marriage rather than a de facto one.
/// `.CENSUS_REGISTERED_SHARE` holds the same value, and the draw below is the
/// same per-dwelling draw, so CORE COMBINED_STATUS agrees with Census RLHP
/// about the same couple.
const CORE_REGISTERED_SHARE: f64 = 0.80;

/// A partnership starts one to twenty years before the reference year.
const CORE_PARTNERSHIP_MAX_YEARS: i32 = 20;

/// Share of partner pairs whose two members are at different dwellings. A
/// population in which every couple is co-resident lets a pipeline that reads
/// co-residence off the address run clean here and find nothing in the lab.
/// The share is a modelling choice, not a published rate.
const CORE_LIVING_APART_SHARE: f64 = 0.08;

/// Share of children with no parent link at all. The old blanket 90% link rate
/// left the unlinked child unrepresentative of anything; this is the share that
/// gives a consumer a bad path to exercise. A modelling choice.
const CORE_CHILD_UNLINKED: f64 = 0.06;

/// Share of children with fewer than two co-resident candidate parents who
/// carry a link to an adult at another dwelling -- roughly one child in six.
/// A modelling choice, not a published rate.
const CORE_CHILD_NON_RESIDENT_PARENT: f64 = 0.16;

/// Minimum years between a child and a recorded parent. Without it a
/// twenty-two-year-old housemate is recorded as the parent of a ten-year-old.
const CORE_PARENT_MIN_AGE_GAP: i32 = 16;

/// The age window a non-resident parent is drawn from, in years above the
/// child. A modelling choice: below the lower bound the adult is too young to
/// be a parent, above the upper bound they read as a grandparent.
const CORE_NON_RESIDENT_MIN_AGE_GAP: i32 = 18;
const CORE_NON_RESIDENT_MAX_AGE_GAP: i32 = 50;

/// Share of parent-child links recorded as a step relationship.
const CORE_PARENT_STEP_SHARE: f64 = 0.10;

/// Share of parent-child links sourced from the births register rather than the
/// Census. A modelling choice; BIRTHS is in the SOURCES code frame and a
/// single-valued SOURCES column lets a consumer ignore it.
const CORE_PARENT_BIRTHS_SHARE: f64 = 0.35;

/// Annual hazard that a live partner pair separates. Over a ten-year mean
/// exposure this ends about a fifth of pairs. A modelling choice.
const CORE_SEPARATION_HAZARD: f64 = 0.020;

/// Share of separations whose RECORD_END carries neither amendment flag -- the
/// unobserved separation, where the end date came from neither a single-status
/// start nor a death. A modelling choice.
const CORE_SEPARATION_UNOBSERVED: f64 = 0.35;

/// Share of partner pairs recorded twice, once from each of two sources.
/// A modelling choice.
const CORE_MULTI_SOURCE_SHARE: f64 = 0.22;

/// Of those, the share whose administrative spell comes from the ATO rather
/// than DOMINO. Both are in the SOURCES code frame.
const CORE_MULTI_SOURCE_ATO_SHARE: f64 = 0.25;

/// How far before Census night the administrative spell of a twice-recorded
/// pair starts, in years.
const CORE_MULTI_SOURCE_MIN_YEARS: i32 = 2;
const CORE_MULTI_SOURCE_MAX_YEARS: i32 = 6;

/// Age band in which living apart together is a real arrangement rather than a
/// young adult who has not partnered yet or a widowed pensioner.
const CORE_LIVING_APART_MIN_AGE: i32 = 25;
const CORE_LIVING_APART_MAX_AGE: i32 = 70;

// Salts for the keyed draws below. They are deliberately different: two
// purposes sharing a salt would make one a function of the other, so a pair's
// separation year would be decided by its start year.
const SALT_PAIRID: u64 = 13;
const SALT_PARTNERSHIP_START: u64 = 29;
const SALT_PARTNER_SEPARATION: u64 = 41;
const SALT_MULTI_SOURCE: u64 = 59;
const SALT_MULTI_SEPARATION: u64 = 67;
const SALT_CHILD_UNLINKED: u64 = 83;
const SALT_CHILD_NON_RESIDENT: u64 = 97;
const SALT_PARENT_LINK: u64 = 109;
const SALT_LEAVER: u64 = 127;

/// A calendar date, ordered by year then month then day.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Ymd {
    y: i32,
    m: i32,
    d: i32,
}

impl Ymd {
    fn text(&self) -> String {
        format!("{:04}-{:02}-{:02}", self.y, self.m, self.d)
    }
}

/// The Rust twin of `.stable_name_seed()`.
fn stable_name_seed(value: &str) -> u64 {
    value
        .chars()
        .enumerate()
        .map(|(i, c)| (c as u64) * (i as u64 + 1))
        .sum::<u64>()
        % 100_000
}

/// The Rust twin of `.mobility_draw_for()`.
///
/// Used here only for the per-dwelling registered-marriage draw, which the
/// Census reads too. Both sides must produce the same number for the same
/// dwelling, so this follows R's arithmetic exactly: the key is reduced modulo
/// the prime first, and the remainder is Euclidean, as R's `%%` is.
fn mobility_draw_for(key: u64, seed: i32, purpose: &str) -> f64 {
    const MODULUS: i64 = 1_000_003;
    let salt = stable_name_seed(purpose) as i64;
    // The salt chooses the multiplier rather than being added to the result:
    // adding it only rotates the sequence, leaving two purposes perfectly
    // rank-correlated.
    let multiplier = 2 + salt % (MODULUS - 3);
    let value = (key % MODULUS as u64) as i64 * multiplier + salt * 7919 + seed as i64 * 9176;
    value.rem_euclid(MODULUS) as f64 / MODULUS as f64
}

fn mix64(mut x: u64) -> u64 {
    x ^= x >> 30;
    x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x ^= x >> 27;
    x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^= x >> 31;
    x
}

/// A deterministic draw stream keyed on the people it belongs to.
///
/// The household loop runs in a fixed order, but the living-apart pairing and
/// the non-resident parent link do not sit inside it, and a sequential stream
/// there would make one household's draws depend on how many households came
/// before it. Keying every draw on the pair or the person instead means a
/// change upstream moves the households it touches and nothing else.
#[derive(Clone, Copy)]
struct KeyedStream(u64);

impl KeyedStream {
    fn new(a: u64, b: u64, seed: i32, salt: u64) -> Self {
        let x = a.wrapping_mul(0x9E37_79B9_7F4A_7C15)
            ^ b.wrapping_mul(0xC2B2_AE3D_27D4_EB4F)
            ^ (seed as i64 as u64).wrapping_mul(0xD6E8_FEB8_6659_FD93)
            ^ salt.wrapping_mul(0xA076_1D64_78BD_642F);
        KeyedStream(mix64(x))
    }

    fn next_u64(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        mix64(self.0)
    }

    fn unit(&mut self) -> f64 {
        (self.next_u64() >> 11) as f64 / (1u64 << 53) as f64
    }

    fn between(&mut self, lo: i32, hi: i32) -> i32 {
        lo + (self.next_u64() % ((hi - lo + 1) as u64)) as i32
    }
}

/// PAIRID as a function of the unordered pair.
///
/// A pair recorded from two sources is one pair, so both rows have to carry one
/// identifier; a drawn value cannot. The old `u32` draw also collided about a
/// thousand times over three million pairs. Forty-eight bits puts that below
/// one in ten thousand.
fn core_pairid(prefix: &str, a: u64, b: u64, seed: i32) -> String {
    let (lo, hi) = if a <= b { (a, b) } else { (b, a) };
    let mut stream = KeyedStream::new(lo, hi, seed, SALT_PAIRID);
    format!("{}{:012X}", prefix, stream.next_u64() & 0xFFFF_FFFF_FFFF)
}

/// How a partner pair ends, and which amendment flag says so.
///
/// Returns the end date, SINGLE_AMENDED and DEATH_AMENDED. A death at or before
/// the separation ends the relationship and sets DEATH_AMENDED; otherwise a
/// separation sets SINGLE_AMENDED, except for the share that carries no flag at
/// all. A death before the record starts is not this relationship's ending.
fn core_resolve_end(
    start: Ymd,
    deaths: (Option<Ymd>, Option<Ymd>),
    key_lo: u64,
    key_hi: u64,
    seed: i32,
    salt: u64,
) -> (Option<Ymd>, i32, i32) {
    let mut stream = KeyedStream::new(key_lo, key_hi, seed, salt);

    // Year by year from the record start, the same shape as the mortality loop
    // above. The first year the hazard fires is the separation. The loop opens
    // the year after the start so a relationship cannot end in the month it
    // began, which would let RECORD_END precede RECORD_START on a record whose
    // start carries a month.
    let mut separation: Option<Ymd> = None;
    for year in (start.y + 1)..=CORE_LAST_YEAR {
        if stream.unit() < CORE_SEPARATION_HAZARD {
            separation = Some(Ymd {
                y: year,
                m: stream.between(1, 12),
                d: stream.between(1, 28),
            });
            break;
        }
    }

    let first_death = [deaths.0, deaths.1]
        .into_iter()
        .flatten()
        .filter(|d| *d >= start)
        .min();

    let death_first = match (first_death, separation) {
        (Some(d), Some(s)) => d <= s,
        (Some(_), None) => true,
        _ => false,
    };

    if death_first {
        (first_death, 0, 1)
    } else if let Some(s) = separation {
        if stream.unit() < CORE_SEPARATION_UNOBSERVED {
            (Some(s), 0, 0)
        } else {
            (Some(s), 1, 0)
        }
    } else {
        (None, 0, 0)
    }
}

/// The relationship rows, one push per emitted record.
struct CoreRelRows {
    orig: Vec<Option<String>>,
    rel: Vec<Option<String>>,
    pairid: Vec<String>,
    category: Vec<String>,
    status: Vec<String>,
    start: Vec<String>,
    end: Vec<Option<String>>,
    single_amended: Vec<Rint>,
    death_amended: Vec<Rint>,
    sources: Vec<String>,
    source_flag: Vec<String>,
}

impl CoreRelRows {
    fn new() -> Self {
        CoreRelRows {
            orig: Vec::new(),
            rel: Vec::new(),
            pairid: Vec::new(),
            category: Vec::new(),
            status: Vec::new(),
            start: Vec::new(),
            end: Vec::new(),
            single_amended: Vec::new(),
            death_amended: Vec::new(),
            sources: Vec::new(),
            source_flag: Vec::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn push(
        &mut self,
        orig: Option<String>,
        rel: Option<String>,
        pairid: String,
        category: &str,
        status: &str,
        start: Ymd,
        end: Option<Ymd>,
        single_amended: Rint,
        death_amended: Rint,
        source: &str,
    ) {
        self.orig.push(orig);
        self.rel.push(rel);
        self.pairid.push(pairid);
        self.category.push(category.to_string());
        self.status.push(status.to_string());
        self.start.push(start.text());
        self.end.push(end.map(|d| d.text()));
        self.single_amended.push(single_amended);
        self.death_amended.push(death_amended);
        self.sources.push(source.to_string());
        // SOURCE_FLAG names the source that contributed the record, which for
        // a single-source row is the source itself.
        self.source_flag.push(source.to_string());
    }
}

/// Project CORE relationships, and the residential events they imply, from
/// spine vectors.
///
/// Returns two frames: `relationships`, the flat partner and parent-child
/// record, and `moves`, one row per person who left a shared dwelling when
/// their relationship ended. Core Locations reads the second so a separated
/// couple's address history actually parts.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_core_relationships__(
    spine_id: Strings,
    birth_year: &[i32],
    state: &[i32],
    household_id: &[i32],
    dwelling_id: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    seed: i32,
) -> List {
    let n = birth_year.len();

    let spine_ids: Vec<Option<String>> = spine_id
        .iter()
        .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
        .collect();

    // A spine id reads SP0000000109, so the number has to come out of the
    // string. `.person_number()` on the R side does the same.
    let person_no: Vec<u64> = (0..n)
        .map(|i| {
            spine_ids[i]
                .as_deref()
                .and_then(|s| {
                    let digits: String = s.chars().filter(char::is_ascii_digit).collect();
                    digits.parse::<u64>().ok()
                })
                .unwrap_or(i as u64 + 1)
        })
        .collect();

    // `i32::MIN` is the NA sentinel arriving through `&[i32]`. An unknown birth
    // year reads as age 40, which is what `census_household_roles()` does, so
    // the two agree about who is an adult.
    let age = |i: usize| -> i32 {
        match birth_year[i] {
            i32::MIN => 40,
            by => CORE_REFERENCE_YEAR - by,
        }
    };
    let born = |i: usize| -> i32 { CORE_REFERENCE_YEAR - age(i) };

    let field = |v: &[i32], i: usize| -> i32 { v.get(i).copied().unwrap_or(i32::MIN) };
    let death_of = |i: usize| -> Option<Ymd> {
        let y = field(year_of_death, i);
        if y == i32::MIN {
            return None;
        }
        // An unknown month or day resolves late in the year. That matches
        // `.census_present_on_night()`, which drops a person only when the
        // parts that are known already establish the death came first.
        let m = match field(month_of_death, i) {
            i32::MIN => 12,
            m => m,
        };
        let d = match field(day_of_death, i) {
            i32::MIN => 28,
            d => d,
        };
        Some(Ymd { y, m, d })
    };

    // `.dil_dwelling_key()`: the dwelling, or the person where there is none.
    let dwelling_key = |i: usize| -> u64 {
        match dwelling_id.get(i).copied().unwrap_or(0) {
            d if d > 0 => d as u64,
            _ => person_no[i],
        }
    };

    // Group persons by household. CORE runs centrally on the full population,
    // so every household is complete here -- including the households of people
    // who died before Census night, whom the cached Census roles exclude and
    // whose records are exactly the ones that must carry DEATH_AMENDED.
    //
    // A person with no household is skipped rather than joined to everyone else
    // who has none: a hand-built spine with a zero household on many rows would
    // otherwise read as one enormous household.
    let mut by_hh: std::collections::HashMap<i32, Vec<usize>> = std::collections::HashMap::new();
    for i in 0..n {
        let hh = household_id.get(i).copied().unwrap_or(0);
        if hh <= 0 {
            continue;
        }
        by_hh.entry(hh).or_default().push(i);
    }
    let mut hh_keys: Vec<i32> = by_hh.keys().copied().collect();
    hh_keys.sort_unstable();

    // The pairs, the parent-child links, and the two pools the second passes
    // need. Index vectors rather than copies of the persons: at ten million
    // rows the difference is hundreds of megabytes.
    let mut pairs: Vec<(usize, usize)> = Vec::new();
    let mut links: Vec<(usize, usize)> = Vec::new();
    let mut short_linked: Vec<usize> = Vec::new();
    let mut lone_references: Vec<usize> = Vec::new();

    for hh in &hh_keys {
        let members = &by_hh[hh];
        let adults: Vec<usize> = members.iter().copied().filter(|&i| age(i) >= 18).collect();

        // The couple, by exactly the rule `census_household_roles()` uses: the
        // oldest adult is the reference person, and their partner is the other
        // adult closest to them in age, taken only when the gap is small enough
        // to read as a couple rather than as a parent and an adult child.
        // Members arrive in increasing spine row order, so a first-past-the-post
        // comparison reproduces R's `which.max`/`which.min` tie-break.
        let mut reference: Option<usize> = None;
        for &a in &adults {
            match reference {
                Some(r) if age(a) <= age(r) => {}
                _ => reference = Some(a),
            }
        }
        let mut partner: Option<usize> = None;
        if let Some(r) = reference {
            let mut closest = i32::MAX;
            for &a in &adults {
                if a == r {
                    continue;
                }
                let gap = (age(a) - age(r)).abs();
                if gap < closest {
                    closest = gap;
                    partner = Some(a);
                }
            }
            if closest > CORE_COUPLE_MAX_AGE_GAP {
                partner = None;
            }
        }

        if let (Some(r), Some(p)) = (reference, partner) {
            pairs.push((r, p));
        } else if let Some(r) = reference {
            // A lone adult who is nobody's partner is the person who might be
            // in a couple that lives apart.
            if adults.len() == 1
                && (CORE_LIVING_APART_MIN_AGE..=CORE_LIVING_APART_MAX_AGE).contains(&age(r))
            {
                lone_references.push(r);
            }
        }

        for &child in members.iter().filter(|&&i| age(i) < 18) {
            // A share of children carry no parent link at all, so a consumer
            // has an unlinked child to handle.
            let mut unlinked = KeyedStream::new(person_no[child], 0, seed, SALT_CHILD_UNLINKED);
            if unlinked.unit() < CORE_CHILD_UNLINKED {
                continue;
            }
            let mut found = 0usize;
            for adult in [reference, partner].into_iter().flatten() {
                if age(adult) - age(child) >= CORE_PARENT_MIN_AGE_GAP {
                    links.push((child, adult));
                    found += 1;
                }
            }
            if found < 2 {
                short_linked.push(child);
            }
        }
    }

    // -- Couples who live apart ------------------------------------------
    //
    // Selected from a fixed total order rather than a draw, so a change
    // upstream cannot shift which references pair with which.
    lone_references.sort_by_key(|&i| (field(state, i), born(i), i));
    let n_within = pairs.len();
    let n_apart =
        ((n_within as f64) * CORE_LIVING_APART_SHARE / (1.0 - CORE_LIVING_APART_SHARE)).round();
    let take = ((2.0 * n_apart) as usize).min(lone_references.len());
    let mut k = 0usize;
    while k + 1 < take {
        let a = lone_references[k];
        let b = lone_references[k + 1];
        // A couple lives in one state even when it lives at two addresses, so
        // a pair that straddles the sort's state boundary is skipped.
        if field(state, a) == field(state, b) {
            pairs.push((a, b));
        }
        k += 2;
    }

    // -- The parent at another dwelling ----------------------------------
    //
    // Adults by state, ordered by age then row index, so the age window a child
    // needs is a contiguous slice and the pick inside it is a pure function of
    // the child.
    if !short_linked.is_empty() {
        let mut adults_by_state: Vec<Vec<usize>> = vec![Vec::new(); 10];
        for i in 0..n {
            if age(i) >= 18 {
                let s = match field(state, i) {
                    s if (1..=8).contains(&s) => s as usize,
                    _ => 0,
                };
                adults_by_state[s].push(i);
            }
        }
        for pool in adults_by_state.iter_mut() {
            pool.sort_by_key(|&i| (age(i), i));
        }

        for &child in &short_linked {
            let mut stream = KeyedStream::new(person_no[child], 0, seed, SALT_CHILD_NON_RESIDENT);
            if stream.unit() >= CORE_CHILD_NON_RESIDENT_PARENT {
                continue;
            }
            let s = match field(state, child) {
                s if (1..=8).contains(&s) => s as usize,
                _ => 0,
            };
            let pool = &adults_by_state[s];
            let child_age = age(child);
            let lo = pool.partition_point(|&i| age(i) < child_age + CORE_NON_RESIDENT_MIN_AGE_GAP);
            let hi = pool.partition_point(|&i| age(i) <= child_age + CORE_NON_RESIDENT_MAX_AGE_GAP);
            if hi <= lo {
                continue;
            }
            let width = hi - lo;
            let mut at = lo + (stream.next_u64() % width as u64) as usize;
            // Step past the child's own household: a non-resident parent who
            // lives with the child is not a non-resident parent.
            let mut tries = 0;
            while household_id.get(pool[at]).copied().unwrap_or(0)
                == household_id.get(child).copied().unwrap_or(0)
                && tries < 8
            {
                at = lo + (at + 1 - lo) % width;
                tries += 1;
            }
            if household_id.get(pool[at]).copied().unwrap_or(0)
                != household_id.get(child).copied().unwrap_or(0)
            {
                links.push((child, pool[at]));
            }
        }
    }

    // -- Emit ------------------------------------------------------------
    let mut rows = CoreRelRows::new();
    let mut move_ids: Vec<Option<String>> = Vec::new();
    let mut move_dates: Vec<String> = Vec::new();

    for &(a, b) in &pairs {
        let (key_lo, key_hi) = if person_no[a] <= person_no[b] {
            (person_no[a], person_no[b])
        } else {
            (person_no[b], person_no[a])
        };
        let pairid = core_pairid("PR", person_no[a], person_no[b], seed);

        // The same per-dwelling draw the Census reads, so CORE COMBINED_STATUS
        // and Census RLHP agree about the same couple.
        let registered =
            mobility_draw_for(dwelling_key(a), seed, "registered marriage") < CORE_REGISTERED_SHARE;
        let status = if registered { "Married" } else { "De facto" };

        let mut stream = KeyedStream::new(key_lo, key_hi, seed, SALT_PARTNERSHIP_START);
        let years_ago = stream.between(1, CORE_PARTNERSHIP_MAX_YEARS);
        // A relationship cannot start before the younger member turned 18.
        let start_year =
            (CORE_REFERENCE_YEAR - years_ago).max(born(a).max(born(b)) + 18);
        let start = Ymd {
            y: start_year,
            m: 1,
            d: 1,
        };
        let deaths = (death_of(a), death_of(b));
        let (end, single, death) = core_resolve_end(
            start,
            deaths,
            key_lo,
            key_hi,
            seed,
            SALT_PARTNER_SEPARATION,
        );

        let mut multi = KeyedStream::new(key_lo, key_hi, seed, SALT_MULTI_SOURCE);
        let twice = multi.unit() < CORE_MULTI_SOURCE_SHARE;

        // The span the address history follows. For a pair recorded twice it is
        // the administrative spell, which is the row that carries a span at
        // all; the Census row is a point observation on one night.
        let mut span_end = end;
        let mut span_death = death;

        if !twice {
            rows.push(
                spine_ids[a].clone(),
                spine_ids[b].clone(),
                pairid,
                "Partner",
                status,
                start,
                end,
                Rint::from(single),
                Rint::from(death),
                "CENSUS",
            );
        } else {
            // The Census point record: the couple as one night saw them.
            let alive = |i: usize| match death_of(i) {
                None => true,
                Some(d) => d >= CORE_CENSUS_NIGHT,
            };
            let covers = start <= CORE_CENSUS_NIGHT
                && match end {
                    None => true,
                    Some(e) => e >= CORE_CENSUS_NIGHT,
                };
            if alive(a) && alive(b) && covers {
                rows.push(
                    spine_ids[a].clone(),
                    spine_ids[b].clone(),
                    pairid.clone(),
                    "Partner",
                    status,
                    CORE_CENSUS_NIGHT,
                    Some(CORE_CENSUS_NIGHT),
                    Rint::from(0),
                    Rint::from(0),
                    "CENSUS",
                );
            }

            // And the administrative spell, mirrored, with a span of its own.
            // The mirrored orientation is deliberate: a consumer folding the
            // pair has to reach the same pair from either row.
            let back = multi.between(CORE_MULTI_SOURCE_MIN_YEARS, CORE_MULTI_SOURCE_MAX_YEARS);
            let admin_start = Ymd {
                y: CORE_CENSUS_NIGHT.y - back,
                m: CORE_CENSUS_NIGHT.m,
                d: CORE_CENSUS_NIGHT.d,
            };
            let (admin_end, admin_single, admin_death) = core_resolve_end(
                admin_start,
                deaths,
                key_lo,
                key_hi,
                seed,
                SALT_MULTI_SEPARATION,
            );
            let source = if multi.unit() < CORE_MULTI_SOURCE_ATO_SHARE {
                "ATO"
            } else {
                "DOMINO"
            };
            rows.push(
                spine_ids[b].clone(),
                spine_ids[a].clone(),
                pairid,
                "Partner",
                status,
                admin_start,
                admin_end,
                Rint::from(admin_single),
                Rint::from(admin_death),
                source,
            );
            span_end = admin_end;
            span_death = admin_death;
        }

        // A co-resident couple that separates cannot both stay. One of them
        // closes the shared address and opens another, which is the event a
        // co-residence rule needs to see. A death does not move anybody.
        let co_resident = household_id.get(a).copied().unwrap_or(0)
            == household_id.get(b).copied().unwrap_or(-1);
        if co_resident && span_death == 0 {
            if let Some(when) = span_end {
                if when > CORE_LOCATIONS_START {
                    let mut which = KeyedStream::new(key_lo, key_hi, seed, SALT_LEAVER);
                    let leaver = if which.next_u64() & 1 == 0 { a } else { b };
                    move_ids.push(spine_ids[leaver].clone());
                    move_dates.push(when.text());
                }
            }
        }
    }

    for &(child, parent) in &links {
        let pairid = core_pairid("PC", person_no[child], person_no[parent], seed);
        let mut stream = KeyedStream::new(person_no[child], person_no[parent], seed, SALT_PARENT_LINK);
        let status = if stream.unit() < CORE_PARENT_STEP_SHARE {
            "Step"
        } else {
            "Biological"
        };
        let start = Ymd {
            y: born(child),
            m: 1,
            d: 1,
        };
        // A parent-child link ends only with a death. The registry declares the
        // amendment flags on the partner tables alone, so both are missing here.
        let end = [death_of(child), death_of(parent)]
            .into_iter()
            .flatten()
            .filter(|d| *d >= start)
            .min();
        let source = if stream.unit() < CORE_PARENT_BIRTHS_SHARE {
            "BIRTHS"
        } else {
            "CENSUS"
        };
        rows.push(
            spine_ids[child].clone(),
            spine_ids[parent].clone(),
            pairid,
            "Parent-Child",
            status,
            start,
            end,
            Rint::na(),
            Rint::na(),
            source,
        );
    }

    list!(
        relationships = list!(
            SPINE_ID_ORIGINAL = rows.orig,
            SPINE_ID_MAIN_REL = rows.rel,
            PAIRID = rows.pairid,
            COMBINED_CATEGORY = rows.category,
            COMBINED_STATUS = rows.status,
            RECORD_START = rows.start,
            RECORD_END = rows.end,
            SINGLE_AMENDED = rows.single_amended,
            DEATH_AMENDED = rows.death_amended,
            SOURCES = rows.sources,
            SOURCE_FLAG = rows.source_flag,
        ),
        moves = list!(SPINE_ID = move_ids, LEAVE_DATE = move_dates,),
    )
}

extendr_module! {
    mod core_gen;
    fn project_core_demographics__;
    fn project_core_locations__;
    fn project_core_residence_to_parquet__;
    fn project_core_relationships__;
}

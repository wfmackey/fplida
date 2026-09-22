//! Business ownership (BUSOWN): the person-to-business concordance.
//!
//! BUSOWN is drawn business-first. A business is minted, given a legal form
//! and an ownership spell, and only then given its owners: one for a sole
//! trader, two or more for a partnership. Drawing person-first -- minting an
//! identifier per owner -- cannot produce shared ownership at all, and gives
//! every business exactly one owner and no ownership spell.
//!
//! Partnership owners are drawn from a household. Spousal and family
//! partnerships are the common Australian form, so co-residence is the
//! natural source of co-owners, and it is the only person-to-person relation
//! the spine carries.
//!
//! Because a household must be seen whole, this generator runs centrally on
//! the full spine rather than per slice. Slices are contiguous row ranges and
//! households are scattered across them, so a sliced BUSOWN would split most
//! multi-person households and lose their partnerships.

use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;

/// Age range over which a person can hold a business.
const OWNER_MIN_AGE: i32 = 18;
const OWNER_MAX_AGE: i32 = 75;

/// Per-adult probability of holding a sole-trader business.
const P_SOLE_TRADER: f64 = 0.040;

/// Probability that a sole trader holds a second business.
const P_SECOND_SOLE_TRADER: f64 = 0.20;

/// Per-adult rate of partnership businesses. Set so partnerships are about a
/// quarter of the businesses BUSOWN covers, matching the ABS counts of sole
/// proprietors against partnerships.
const P_PARTNERSHIP: f64 = 0.0143;

/// Share of partnerships anchored in a household. Spousal and family
/// partnerships are the common Australian form, but not the only one, so the
/// rest are drawn from unrelated adults.
const P_FAMILY_PARTNERSHIP: f64 = 0.65;

/// Partner counts and their shares. ATO partnership returns are dominated by
/// two-partner businesses.
const PARTNER_COUNTS: [usize; 3] = [2, 3, 4];
const PARTNER_SHARES: [f64; 3] = [0.82, 0.12, 0.06];

/// Share of businesses that already existed when the window opens.
const P_PREDATES_WINDOW: f64 = 0.55;

/// Annual probability that a business ceases.
const ANNUAL_EXIT_HAZARD: f64 = 0.12;

/// Legal form of a business. BUSOWN covers only the two forms that have
/// person-level beneficiaries; companies and trusts are out of scope.
#[derive(Clone, Copy, PartialEq, Eq)]
enum LegalForm {
    SoleTrader,
    Partnership,
}

/// One person's ownership of one business, over one spell of financial years.
struct Ownership {
    person: usize,
    bn: String,
    form: LegalForm,
    start_fy: i32,
    end_fy: i32,
}

fn weighted_pick(rng: &mut StdRng, shares: &[f64]) -> usize {
    let total: f64 = shares.iter().sum();
    let mut u = rng.gen::<f64>() * total;
    for (i, &w) in shares.iter().enumerate() {
        if u < w {
            return i;
        }
        u -= w;
    }
    shares.len() - 1
}

/// Draw the financial years a business trades over.
fn draw_spell(rng: &mut StdRng, fy_start: i32, fy_end: i32) -> (i32, i32) {
    let start = if rng.gen::<f64>() < P_PREDATES_WINDOW {
        fy_start
    } else {
        rng.gen_range(fy_start..=fy_end)
    };
    let mut end = start;
    while end < fy_end && rng.gen::<f64>() >= ANNUAL_EXIT_HAZARD {
        end += 1;
    }
    (start, end)
}

/// Take the next distinct business number from the BLADE pool.
///
/// Businesses are enumerated before their owners, so each one can claim its
/// own pool entry rather than hashing into the pool and colliding. Falls back
/// to a synthetic identifier only when no BLADE stage has run.
///
/// The fallback mints in the same `BN` space and by the same formula as the
/// business spine, so a BLADE-less run still produces identifiers that
/// `abn_hash_trunc` can hash and that look like the ones a full run would give.
/// The earlier fallback minted an `ABN`-prefixed value that matched nothing
/// anywhere and that no era of the delivery uses.
fn next_bn(pool: &[String], counter: &mut usize, seed: i64) -> String {
    if pool.is_empty() {
        let s = *counter as i64 + 1;
        *counter += 1;
        return crate::blade::helpers::numeric_id(
            "BN",
            ((s * 1_000_003 + seed * 9176).rem_euclid(100_000_000_000)) as i128,
            11,
        );
    }
    let bn = pool[*counter % pool.len()].clone();
    *counter += 1;
    bn
}

fn fin_year_label(fy: i32) -> String {
    format!("{}-{:02}", fy - 1, fy.rem_euclid(100))
}

fn extract_ref_label(fy: i32) -> String {
    format!("FY{:04}-{:02}", fy - 1, fy.rem_euclid(100))
}

/// Build every ownership record for the window.
fn build_ownerships(
    aeuid: &[String],
    birth_year: &[i32],
    household_id: &[String],
    pool: &[String],
    seed: i64,
    fy_start: i32,
    fy_end: i32,
) -> Vec<Ownership> {
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let mid_fy = (fy_start + fy_end) / 2;

    // Adults, judged at the middle of the window so a person is eligible over
    // it as a whole rather than at one arbitrary end.
    let eligible: Vec<bool> = (0..n)
        .map(|i| {
            let age = mid_fy - birth_year[i];
            (OWNER_MIN_AGE..=OWNER_MAX_AGE).contains(&age)
        })
        .collect();

    // Households in first-appearance order, so iteration is deterministic
    // without sorting identifiers.
    let mut household_order: Vec<usize> = Vec::new();
    let mut household_index: HashMap<&str, usize> = HashMap::new();
    let mut household_adults: Vec<Vec<usize>> = Vec::new();
    for i in 0..n {
        if !eligible[i] {
            continue;
        }
        let key = household_id[i].as_str();
        match household_index.get(key) {
            Some(&h) => household_adults[h].push(i),
            None => {
                let h = household_adults.len();
                household_index.insert(key, h);
                household_adults.push(vec![i]);
                household_order.push(h);
            }
        }
    }

    let mut ownerships: Vec<Ownership> = Vec::new();
    let mut bn_counter: usize = 0;
    let mut in_partnership: Vec<bool> = vec![false; n];

    // A partner who is not a household member is drawn from this shuffled
    // roll of adults, so nobody is picked into two partnerships.
    let mut unrelated: Vec<usize> = (0..n).filter(|&i| eligible[i]).collect();
    let n_eligible = unrelated.len();
    for k in (1..unrelated.len()).rev() {
        unrelated.swap(k, rng.gen_range(0..=k));
    }
    let mut unrelated_cursor: usize = 0;
    let mut take_unrelated = |rng: &mut StdRng,
                              cursor: &mut usize,
                              used: &[bool]|
     -> Option<usize> {
        let _ = rng;
        while *cursor < unrelated.len() {
            let person = unrelated[*cursor];
            *cursor += 1;
            if !used[person] {
                return Some(person);
            }
        }
        None
    };

    // Households able to anchor a family partnership, bucketed by how many
    // partners they can supply. A three-partner family partnership should
    // prefer a household with three adults over one with two.
    let max_partners = *PARTNER_COUNTS.iter().max().unwrap();
    let family_households: Vec<Vec<usize>> = (0..=max_partners)
        .map(|size| {
            household_order
                .iter()
                .copied()
                .filter(|&h| household_adults[h].len() >= size.max(2))
                .collect::<Vec<usize>>()
        })
        .collect();

    // Partnerships first: a business, then the people who hold it.
    let n_partnerships = (n_eligible as f64 * P_PARTNERSHIP).round() as usize;
    for _ in 0..n_partnerships {
        let wanted = PARTNER_COUNTS[weighted_pick(&mut rng, &PARTNER_SHARES)];
        let mut partners: Vec<usize> = Vec::with_capacity(wanted);

        // Prefer a household that can supply every partner; fall back to any
        // household with two adults when none is large enough.
        let bucket = if !family_households[wanted].is_empty() {
            &family_households[wanted]
        } else {
            &family_households[2]
        };
        let anchor_in_household =
            !bucket.is_empty() && rng.gen::<f64>() < P_FAMILY_PARTNERSHIP;
        if anchor_in_household {
            let h = bucket[rng.gen_range(0..bucket.len())];
            let mut candidates = household_adults[h].clone();
            for k in (1..candidates.len()).rev() {
                candidates.swap(k, rng.gen_range(0..=k));
            }
            for &person in candidates.iter() {
                if partners.len() == wanted {
                    break;
                }
                if !in_partnership[person] {
                    partners.push(person);
                }
            }
        }

        // A household rarely holds three or more adults, so a larger
        // partnership is topped up with partners from outside it.
        while partners.len() < wanted {
            match take_unrelated(&mut rng, &mut unrelated_cursor, &in_partnership) {
                Some(person) if !partners.contains(&person) => partners.push(person),
                Some(_) => continue,
                None => break,
            }
        }
        if partners.len() < 2 {
            continue;
        }

        let bn = next_bn(pool, &mut bn_counter, seed);
        let (start_fy, end_fy) = draw_spell(&mut rng, fy_start, fy_end);
        for &person in &partners {
            in_partnership[person] = true;
            ownerships.push(Ownership {
                person,
                bn: bn.clone(),
                form: LegalForm::Partnership,
                start_fy,
                end_fy,
            });
        }
    }

    // Sole traders: one business, one owner, by construction.
    for i in 0..n {
        if !eligible[i] || in_partnership[i] {
            continue;
        }
        if rng.gen::<f64>() >= P_SOLE_TRADER {
            continue;
        }
        let bn = next_bn(pool, &mut bn_counter, seed);
        let (start_fy, end_fy) = draw_spell(&mut rng, fy_start, fy_end);
        ownerships.push(Ownership {
            person: i,
            bn,
            form: LegalForm::SoleTrader,
            start_fy,
            end_fy,
        });

        if rng.gen::<f64>() < P_SECOND_SOLE_TRADER {
            let bn2 = next_bn(pool, &mut bn_counter, seed);
            let (s2, e2) = draw_spell(&mut rng, fy_start, fy_end);
            ownerships.push(Ownership {
                person: i,
                bn: bn2,
                form: LegalForm::SoleTrader,
                start_fy: s2,
                end_fy: e2,
            });
        }
    }

    ownerships
}

/// Project BUSOWN directly to per-table parquet files.
///
/// The caller supplies one entry per output file: its stem, its legal form
/// (0 sole trader, 1 partnership), the financial year it reports, the length
/// of the extract window in months, whether it carries `EXTRACT_REF`, and
/// which identifier it keys its businesses on. Table naming and the choice of
/// identifier therefore stay with the registry on the R side.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_busown_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    household_id: Strings,
    seed: i64,
    fy_start: i32,
    fy_end: i32,
    out_dir: &str,
    file_stem: Strings,
    file_form: &[i32],
    file_fy: &[i32],
    file_months: &[i32],
    file_extract_ref: &[i32],
    file_key_var: Strings,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};

    let aeuid_owned: Vec<String> = aeuid.iter().map(|s| s.to_string()).collect();
    let household_owned: Vec<String> = household_id.iter().map(|s| s.to_string()).collect();
    let pool = crate::business_pool::snapshot();

    let ownerships = build_ownerships(
        &aeuid_owned,
        birth_year,
        &household_owned,
        &pool,
        seed,
        fy_start,
        fy_end,
    );

    let mut total_rows: usize = 0;
    for f in 0..file_stem.len() {
        let form = if file_form[f] == 1 {
            LegalForm::Partnership
        } else {
            LegalForm::SoleTrader
        };
        let fy = file_fy[f];
        // A 16-month extract also catches ownerships commencing in the four
        // months after the financial year it reports.
        let lookahead = if file_months[f] > 12 { 1 } else { 0 };

        // Which era this file belongs to. A business holds one `bn` for its
        // whole spell and `abn_hash_trunc` is a pure function of it, so the
        // same business carries the same identifier in every file of its era
        // without any extra state, and the correspondence key joins the two.
        let keyed_on_bn = file_key_var[f].to_string() == "BN";
        let key_name: &'static str = if keyed_on_bn { "BN" } else { "ABN_HASH_TRUNC" };

        let mut year_aeuid: Vec<String> = Vec::new();
        let mut year_fin: Vec<String> = Vec::new();
        let mut year_abn: Vec<String> = Vec::new();

        for o in &ownerships {
            if o.form != form {
                continue;
            }
            if o.start_fy > fy + lookahead || o.end_fy < fy {
                continue;
            }
            year_aeuid.push(aeuid_owned[o.person].clone());
            year_fin.push(fin_year_label(fy));
            year_abn.push(if keyed_on_bn {
                o.bn.clone()
            } else {
                crate::blade::helpers::abn_hash_trunc(&o.bn)
            });
        }

        total_rows += year_aeuid.len();
        let n_rows = year_aeuid.len();

        let mut cols = vec![
            NamedCol {
                name: "SYNTHETIC_AEUID",
                col: Col::Str(year_aeuid),
            },
            NamedCol {
                name: "FIN_YEAR",
                col: Col::Str(year_fin),
            },
            NamedCol {
                name: key_name,
                col: Col::Str(year_abn),
            },
        ];
        if file_extract_ref[f] == 1 {
            cols.push(NamedCol {
                name: "EXTRACT_REF",
                col: Col::Str(vec![extract_ref_label(fy); n_rows]),
            });
        }

        let out_path = format!("{}/{}.parquet", out_dir, file_stem[f]);
        write_columns_to_parquet(&out_path, cols)
            .unwrap_or_else(|e| panic!("busown parquet write: {}", e));
    }

    total_rows as i32
}

extendr_module! {
    mod busown;
    fn project_busown_to_parquet__;
}

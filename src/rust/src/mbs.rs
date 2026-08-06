use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use rayon::prelude::*;

use crate::sampling::{normal_sample, weighted_sample};

// ==========================================================================
// Constants (from inst/foundations/mbs.toml)
// ==========================================================================

// Mean MBS services per person per year by age band.
// Source: AIHW Medicare Statistics 2022-23, Table 2.1
const AGE_BREAKS: [i32; 6] = [0, 15, 25, 45, 65, 75];
const AGE_RATES: [f64; 6] = [6.0, 8.0, 12.0, 16.0, 24.0, 32.0];

const FEMALE_MULT: f64 = 1.10;

// Participation probability by broad age group
const PART_CHILD: f64 = 0.92;
const PART_ADULT: f64 = 0.97;
const PART_ELDERLY: f64 = 0.99;

// Category-level claim shares
const CAT_CODES: [i32; 8] = [1, 2, 3, 4, 5, 6, 7, 8];
const CAT_SHARES: [f64; 8] = [0.38, 0.03, 0.08, 0.02, 0.08, 0.30, 0.05, 0.06];

// Billing type: D = bulk-billed, P = patient-billed
const BULK_BILLED_SHARE: f64 = 0.82;
const MARKUP_MEAN: f64 = 1.35;
const MARKUP_SD: f64 = 0.15;

// In-hospital rate
const INHOSPITAL_RATE: f64 = 0.08;

// NUMSERV distribution
const NUMSERV_VALS: [i32; 4] = [1, 2, 3, 4];
const NUMSERV_PROBS: [f64; 4] = [0.95, 0.03, 0.01, 0.01];

// Broad Type of Service by MBS category.
//
// The Department of Health publishes BTOS as a four-digit hierarchy of 18
// classes, listed in the AIHW METEOR data set specification that the PLIDA
// item descriptions are taken from. The single letters here before were
// invented shorthand and are not members of that domain.
//
// The mapping preserves exactly what the letters meant, so the column keeps
// its relationship with MBS category and only its vocabulary changes:
// U -> other, A -> non-referred attendance, O -> operations, D -> dental,
// I -> diagnostic imaging, P -> pathology, R -> radiotherapy.
const BTOS_LABELS: [&str; 8] = [
    "1100", "0101", "0700", "1200", "0600", "0502", "1000", "1100",
];

// State → postcode range mapping
const STATE_PC_LO: [i32; 8] = [2000, 3000, 4000, 5000, 6000, 7000, 800, 2600];
const STATE_PC_HI: [i32; 8] = [2999, 3999, 4999, 5799, 6797, 7999, 899, 2618];
const STATE_CODES: [i32; 8] = [1, 2, 3, 4, 5, 6, 7, 8];

// Provider specialties by category
struct SpecialtyPool {
    codes: &'static [&'static str],
    weights: &'static [f64],
}

const SPEC_CAT1: SpecialtyPool = SpecialtyPool {
    codes: &[
        "GP", "DER", "CAR", "GAS", "NEU", "ONC", "OPH", "ORT", "PSY", "RHE", "URO",
    ],
    weights: &[
        0.65, 0.03, 0.04, 0.03, 0.03, 0.02, 0.04, 0.04, 0.05, 0.03, 0.04,
    ],
};
const SPEC_CAT3: SpecialtyPool = SpecialtyPool {
    codes: &[
        "ORT", "OPH", "GEN", "GYN", "ENT", "URO", "PLS", "CAR", "NEU", "GAS",
    ],
    weights: &[0.15, 0.12, 0.15, 0.10, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08],
};
const SPEC_CAT5: SpecialtyPool = SpecialtyPool {
    codes: &["RAD", "NUC"],
    weights: &[0.90, 0.10],
};
const SPEC_CAT6: SpecialtyPool = SpecialtyPool {
    codes: &["PAT"],
    weights: &[1.0],
};
const SPEC_CAT7: SpecialtyPool = SpecialtyPool {
    codes: &["PSK", "PHY", "OCC", "SOC", "AUD", "DIE", "POD"],
    weights: &[0.30, 0.20, 0.10, 0.10, 0.10, 0.10, 0.10],
};

// ==========================================================================
// Item lookup - parsed from R-passed vectors
// ==========================================================================

struct MbsItemTable {
    item_num: Vec<i32>,
    category: Vec<i32>,
    group: Vec<String>,
    sub_heading: Vec<String>,
    benefit_type: Vec<String>,
    schedule_fee: Vec<f64>,
    benefit_75: Vec<f64>,
    benefit_85: Vec<f64>,
    benefit_100: Vec<f64>,
    weight: Vec<f64>,
    // Pre-computed: indices per category, normalised weights per category
    cat_indices: Vec<Vec<usize>>, // cat_indices[cat_idx] = vec of row indices
    cat_weights: Vec<Vec<f64>>,   // cat_weights[cat_idx] = normalised weights
    cat_map: [usize; 9],          // category code → index into cat_indices (1-8)
}

impl MbsItemTable {
    fn from_vectors(
        item_num: &[i32],
        category: &[i32],
        group: &[String],
        sub_heading: &[String],
        benefit_type: &[String],
        schedule_fee: &[f64],
        benefit_75: &[f64],
        benefit_85: &[f64],
        benefit_100: &[f64],
        weight: &[f64],
    ) -> Self {
        let n = item_num.len();

        // Build per-category index lists
        let mut cat_indices: Vec<Vec<usize>> = vec![Vec::new(); 8];
        let mut cat_raw_weights: Vec<Vec<f64>> = vec![Vec::new(); 8];
        let mut cat_map = [0usize; 9]; // map category code to index

        for (i, &cat) in CAT_CODES.iter().enumerate() {
            cat_map[cat as usize] = i;
        }

        for row in 0..n {
            let cat = category[row];
            if cat >= 1 && cat <= 8 {
                let idx = cat_map[cat as usize];
                cat_indices[idx].push(row);
                cat_raw_weights[idx].push(weight[row]);
            }
        }

        // Normalise weights
        let cat_weights: Vec<Vec<f64>> = cat_raw_weights
            .iter()
            .map(|ws| {
                let total: f64 = ws.iter().sum();
                if total > 0.0 {
                    ws.iter().map(|w| w / total).collect()
                } else {
                    ws.clone()
                }
            })
            .collect();

        MbsItemTable {
            item_num: item_num.to_vec(),
            category: category.to_vec(),
            group: group.to_vec(),
            sub_heading: sub_heading.to_vec(),
            benefit_type: benefit_type.to_vec(),
            schedule_fee: schedule_fee.to_vec(),
            benefit_75: benefit_75.to_vec(),
            benefit_85: benefit_85.to_vec(),
            benefit_100: benefit_100.to_vec(),
            weight: weight.to_vec(),
            cat_indices,
            cat_weights,
            cat_map,
        }
    }

}

// ==========================================================================
// Helper functions
// ==========================================================================

fn age_band_index(age: i32) -> usize {
    if age < 15 {
        0
    } else if age < 25 {
        1
    } else if age < 45 {
        2
    } else if age < 65 {
        3
    } else if age < 75 {
        4
    } else {
        5
    }
}

fn healthcare_usage_multiplier(
    year: i32,
    age: i32,
    baseline_income: f64,
    baseline_employed: i32,
    disability_onset_year: i32,
    disability_severity: i32,
    disability_is_dc: i32,
) -> f64 {
    let mut mult = 1.0;

    let has_disability = disability_onset_year != i32::MIN && disability_onset_year <= year;
    let has_severity = disability_severity != i32::MIN;
    let severe_disability = has_disability && has_severity && disability_severity <= 2;
    let other_disability = has_disability && has_severity && disability_severity > 2;

    if severe_disability {
        mult *= if disability_is_dc == 1 { 2.30 } else { 2.00 };
    } else if other_disability {
        mult *= 1.25;
    }

    let low_income =
        baseline_income.is_finite() && baseline_income >= 0.0 && baseline_income < 40_000.0;
    let weak_labour_attachment = baseline_employed != 1 || baseline_income < 25_000.0;
    if (16..65).contains(&age) && low_income && weak_labour_attachment {
        mult *= 1.10;
    }

    mult
}

fn poisson_sample(rng: &mut StdRng, lambda: f64) -> u32 {
    // Knuth's algorithm for small lambda; for large lambda use normal approx
    if lambda < 30.0 {
        let l = (-lambda).exp();
        let mut k: u32 = 0;
        let mut p: f64 = 1.0;
        loop {
            k += 1;
            p *= rng.gen::<f64>();
            if p < l {
                return k - 1;
            }
        }
    } else {
        // Normal approximation for large lambda
        let z = normal_sample(rng, lambda, lambda.sqrt());
        z.round().max(0.0) as u32
    }
}

#[inline]
fn random_positive_weight_row(rng: &mut StdRng, weights: &[f64], upper: usize) -> usize {
    let upper = upper.min(weights.len());
    for _ in 0..16 {
        let row = rng.gen_range(0..upper);
        if weights[row] > 0.0 {
            return row;
        }
    }
    weights
        .iter()
        .take(upper)
        .position(|w| *w > 0.0)
        .unwrap_or(0)
}

fn postcode_for_state(rng: &mut StdRng, state: i32) -> String {
    for i in 0..STATE_CODES.len() {
        if STATE_CODES[i] == state {
            let pc = rng.gen_range(STATE_PC_LO[i]..=STATE_PC_HI[i]);
            return format!("{:04}", pc);
        }
    }
    // Fallback
    let pc = rng.gen_range(2000..=6999);
    format!("{:04}", pc)
}

fn specialty_for_cat(rng: &mut StdRng, cat: i32) -> &'static str {
    let pool = match cat {
        1 => &SPEC_CAT1,
        3 => &SPEC_CAT3,
        5 => &SPEC_CAT5,
        6 => &SPEC_CAT6,
        7 => &SPEC_CAT7,
        _ => return "GP",
    };
    let idx = weighted_sample(rng, pool.weights);
    pool.codes[idx]
}

// ==========================================================================
// Main generation function: one year of MBS claims
// ==========================================================================

/// Generate MBS claims for a single year.
///
/// Takes spine-level vectors (already filtered to participants) and item
/// lookup vectors. Returns a named list of column vectors.
///
/// @export
#[extendr]
fn generate_mbs_year__(
    // Participant vectors
    aeuid: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    sex: &[i32],
    state: &[i32],
    first_year: &[i32],
    last_year: &[i32],
    // Item lookup vectors
    item_num: &[i32],
    item_category: &[i32],
    item_group: Strings,
    item_sub_heading: Strings,
    item_benefit_type: Strings,
    item_schedule_fee: &[f64],
    item_benefit_75: &[f64],
    item_benefit_85: &[f64],
    item_benefit_100: &[f64],
    item_weight: &[f64],
    // Provider/practice pools
    provider_pool: Strings,
    ref_pool: Strings,
    prac_pool: Strings,
    // Parameters
    year: i32,
    seed: i64,
) -> List {
    let n_participants = birth_year.len();

    // Parse String vectors into owned Strings for the item table
    let group_vec: Vec<String> = item_group.iter().map(|s| s.to_string()).collect();
    let sub_heading_vec: Vec<String> = item_sub_heading.iter().map(|s| s.to_string()).collect();
    let benefit_type_vec: Vec<String> = item_benefit_type.iter().map(|s| s.to_string()).collect();

    let items = MbsItemTable::from_vectors(
        item_num,
        item_category,
        &group_vec,
        &sub_heading_vec,
        &benefit_type_vec,
        item_schedule_fee,
        item_benefit_75,
        item_benefit_85,
        item_benefit_100,
        item_weight,
    );

    // Provider pools to Vec<String>
    let prov_vec: Vec<&str> = provider_pool.iter().map(|s| s.as_ref()).collect();
    let rref_vec: Vec<&str> = ref_pool.iter().map(|s| s.as_ref()).collect();
    let rprac_vec: Vec<&str> = prac_pool.iter().map(|s| s.as_ref()).collect();

    // Convert aeuid Strings to owned Vec<String> so it's Send/Sync
    // (extendr Strings holds a raw pointer to an R SEXP, not thread-safe).
    // This has to be sequential — extendr's Strings is not Send.
    let aeuid_owned: Vec<String> = aeuid.iter().map(|s| s.to_string()).collect();

    // Filter to alive participants this year
    let alive_idx: Vec<usize> = (0..n_participants)
        .filter(|&i| first_year[i] <= year && last_year[i] >= year)
        .collect();

    let n_alive = alive_idx.len();
    if n_alive == 0 {
        return empty_mbs_list();
    }

    // Capture references to borrowable slices (Send/Sync)
    let birth_year_ref: &[i32] = birth_year;
    let month_of_birth_ref: &[i32] = month_of_birth;
    let sex_ref: &[i32] = sex;
    let state_ref: &[i32] = state;
    let aeuid_ref: &[String] = &aeuid_owned;

    // ---- Parallel chunk-based generation ----
    // Split persons into chunks (roughly one per CPU thread).
    // Each chunk gets its own deterministically-seeded RNG and produces
    // its own output vectors; we concatenate them at the end.
    let n_threads = rayon::current_num_threads().max(1);
    let chunk_target = ((n_alive + n_threads - 1) / n_threads).max(1);
    let n_chunks = (n_alive + chunk_target - 1) / chunk_target;

    let alive_idx_ref: &[usize] = &alive_idx;
    let items_ref: &MbsItemTable = &items;
    let n_prov = prov_vec.len();
    let n_rref = rref_vec.len();
    let n_rprac = rprac_vec.len();

    // Pre-compute item-row → item_num_str and cat → btos_str (sequential, small)
    let item_num_strs: Vec<String> = items.item_num.iter().map(|n| n.to_string()).collect();

    // Estimate claims per chunk: n_chunk × ~15 claims/person
    let chunk_cap = (chunk_target as f64 * 16.0) as usize;

    let chunk_results: Vec<MbsChunk> = (0..n_chunks)
        .into_par_iter()
        .map(|ci| {
            let start = ci * chunk_target;
            let end = ((ci + 1) * chunk_target).min(n_alive);
            let chunk_seed = (seed as u64).wrapping_add((ci as u64).wrapping_mul(2_654_435_761));
            let mut rng = StdRng::seed_from_u64(chunk_seed);
            let mut chunk = MbsChunk::with_capacity(chunk_cap);

            for &pi in &alive_idx_ref[start..end] {
                let age = year - birth_year_ref[pi];
                let band = age_band_index(age);
                let mut rate = AGE_RATES[band];
                if sex_ref[pi] == 2 {
                    rate *= FEMALE_MULT;
                }
                // Only the post-birth part of the birth year is observable.
                let (window_start, window_days, obs_frac) =
                    person_year_window(year, birth_year_ref[pi], month_of_birth_ref[pi]);
                let dob_days = person_dob_days(birth_year_ref[pi], month_of_birth_ref[pi]);
                rate *= obs_frac;
                let count = poisson_sample(&mut rng, rate) as usize;
                if count == 0 {
                    continue;
                }

                let person_state = state_ref[pi];
                let person_idx = pi as u32;

                for _ in 0..count {
                    generate_one_claim(
                        &mut rng,
                        items_ref,
                        n_prov,
                        n_rref,
                        n_rprac,
                        window_start,
                        window_days,
                        dob_days,
                        person_idx,
                        person_state,
                        &mut chunk,
                        items_ref.item_num.len(),
                        None,
                    );
                }
            }

            chunk
        })
        .collect();

    // Compute total
    let total_claims: usize = chunk_results.iter().map(|c| c.dos.len()).sum();
    if total_claims == 0 {
        return empty_mbs_list();
    }

    // ---- Materialise strings sequentially ----
    // This converts primitive indices to Strings. Allocations happen on a
    // single thread, avoiding allocator contention that plagued the parallel
    // hot loop.
    let mut out_aeuid: Vec<String> = Vec::with_capacity(total_claims);
    // DOS/DOP rendered as "ddmmmYY" strings (e.g. 31Jan20) for consistency
    // with the parquet output paths.
    let mut out_dos: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_dop: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_item: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_aggritem: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_mbscat: Vec<i32> = Vec::with_capacity(total_claims);
    let mut out_mbsgroup: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_mbssubgroup: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_btos: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_spr_rsp: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_feecharged: Vec<f64> = Vec::with_capacity(total_claims);
    let mut out_benpaid: Vec<f64> = Vec::with_capacity(total_claims);
    let mut out_schedfee: Vec<f64> = Vec::with_capacity(total_claims);
    let mut out_numserv: Vec<i32> = Vec::with_capacity(total_claims);
    let mut out_inhospital: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_billtypecd: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_enrpc: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_sprpc: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_scram_spr: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_scram_rpr: Vec<Option<String>> = Vec::with_capacity(total_claims);
    let mut out_sprprac: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_rprprac: Vec<Option<String>> = Vec::with_capacity(total_claims);
    let mut out_rpdate: Vec<Option<String>> = Vec::with_capacity(total_claims);
    let mut out_rprpc: Vec<Option<String>> = Vec::with_capacity(total_claims);

    for c in &chunk_results {
        let n = c.dos.len();
        for k in 0..n {
            let pi = c.person_idx[k] as usize;
            let item_row = c.item_row[k] as usize;
            let cat_idx = c.cat_idx[k] as usize;
            let cat_code = CAT_CODES[cat_idx];

            out_aeuid.push(aeuid_ref[pi].clone());
            out_dos.push(crate::parquet_io::days_to_ddmmmyy(c.dos[k]).unwrap_or_default());
            out_dop.push(crate::parquet_io::days_to_ddmmmyy(c.dop[k]).unwrap_or_default());
            out_item.push(item_num_strs[item_row].clone());
            out_aggritem.push(item_num_strs[item_row].clone());
            out_mbscat.push(cat_code);
            out_mbsgroup.push(items_ref.group[item_row].clone());
            out_mbssubgroup.push(items_ref.sub_heading[item_row].clone());
            out_btos.push(BTOS_LABELS[cat_idx].to_string());
            out_spr_rsp.push(SPR_RSP_POOL[c.spr_rsp_code[k] as usize].to_string());
            out_feecharged.push(c.feecharged[k]);
            out_benpaid.push(c.benpaid[k]);
            out_schedfee.push(c.schedfee[k]);
            out_numserv.push(c.numserv[k]);
            out_inhospital.push(if c.inhospital_flag[k] == 1 {
                "Y".to_string()
            } else {
                "N".to_string()
            });
            out_billtypecd.push(if c.billtype_flag[k] == 0 {
                "D".to_string()
            } else {
                "P".to_string()
            });
            out_enrpc.push(format!("{:04}", c.enrpc[k]));
            out_sprpc.push(format!("{:04}", c.sprpc[k]));
            out_scram_spr.push(prov_vec[c.scram_spr_idx[k] as usize].to_string());

            let rpr = c.scram_rpr_idx[k];
            out_scram_rpr.push(if rpr < 0 {
                None
            } else {
                Some(rref_vec[rpr as usize].to_string())
            });

            out_sprprac.push(rprac_vec[c.sprprac_idx[k] as usize].to_string());

            let rprp = c.rprprac_idx[k];
            out_rprprac.push(if rprp < 0 {
                None
            } else {
                Some(rprac_vec[rprp as usize].to_string())
            });

            // RPDATE rendered as "ddmmmYY" string (NA-preserving).
            let rpd = c.rpdate[k];
            out_rpdate.push(if rpd == i32::MIN {
                None
            } else {
                crate::parquet_io::days_to_ddmmmyy(rpd)
            });

            let rppc = c.rprpc[k];
            out_rprpc.push(if rppc < 0 {
                None
            } else {
                Some(format!("{:04}", rppc))
            });
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        DOS = out_dos,
        DOP = out_dop,
        ITEM = out_item,
        AGGRITEM = out_aggritem,
        MBSCAT = out_mbscat,
        MBSGROUP = out_mbsgroup,
        MBSSUBGROUP = out_mbssubgroup,
        BTOS = out_btos,
        SPR_RSP = out_spr_rsp,
        FEECHARGED = out_feecharged,
        BENPAID = out_benpaid,
        SCHEDFEE = out_schedfee,
        NUMSERV = out_numserv,
        INHOSPITAL = out_inhospital,
        BILLTYPECD = out_billtypecd,
        ENRPC = out_enrpc,
        SPRPC = out_sprpc,
        SCRAM_SPR = out_scram_spr,
        SCRAM_RPR = out_scram_rpr,
        SPRPRAC = out_sprprac,
        RPRPRAC = out_rprprac,
        RPDATE = out_rpdate,
        RPRPC = out_rprpc
    )
}

/// Generate an MBS year and write it directly to parquet.
///
/// Bypasses R's arrow::write_parquet (~4s/year for 14M rows at 1M spine).
/// Returns the row count as an integer.
///
/// @export
#[extendr]
fn generate_mbs_year_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    sex: &[i32],
    state: &[i32],
    first_year: &[i32],
    last_year: &[i32],
    item_num: &[i32],
    item_category: &[i32],
    item_group: Strings,
    item_sub_heading: Strings,
    item_benefit_type: Strings,
    item_schedule_fee: &[f64],
    item_benefit_75: &[f64],
    item_benefit_85: &[f64],
    item_benefit_100: &[f64],
    item_weight: &[f64],
    provider_pool: Strings,
    ref_pool: Strings,
    prac_pool: Strings,
    year: i32,
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};

    let n_participants = birth_year.len();

    let group_vec: Vec<String> = item_group.iter().map(|s| s.to_string()).collect();
    let sub_heading_vec: Vec<String> = item_sub_heading.iter().map(|s| s.to_string()).collect();
    let benefit_type_vec: Vec<String> = item_benefit_type.iter().map(|s| s.to_string()).collect();

    let items = MbsItemTable::from_vectors(
        item_num,
        item_category,
        &group_vec,
        &sub_heading_vec,
        &benefit_type_vec,
        item_schedule_fee,
        item_benefit_75,
        item_benefit_85,
        item_benefit_100,
        item_weight,
    );

    let prov_vec: Vec<&str> = provider_pool.iter().map(|s| s.as_ref()).collect();
    let rref_vec: Vec<&str> = ref_pool.iter().map(|s| s.as_ref()).collect();
    let rprac_vec: Vec<&str> = prac_pool.iter().map(|s| s.as_ref()).collect();

    let aeuid_owned: Vec<String> = aeuid.iter().map(|s| s.to_string()).collect();

    let alive_idx: Vec<usize> = (0..n_participants)
        .filter(|&i| first_year[i] <= year && last_year[i] >= year)
        .collect();

    let n_alive = alive_idx.len();
    if n_alive == 0 {
        // Write an empty file? Just return 0 and let R handle it.
        return 0;
    }

    let birth_year_ref: &[i32] = birth_year;
    let month_of_birth_ref: &[i32] = month_of_birth;
    let sex_ref: &[i32] = sex;
    let state_ref: &[i32] = state;
    let aeuid_ref: &[String] = &aeuid_owned;

    let n_threads = rayon::current_num_threads().max(1);
    let chunk_target = ((n_alive + n_threads - 1) / n_threads).max(1);
    let n_chunks = (n_alive + chunk_target - 1) / chunk_target;

    let alive_idx_ref: &[usize] = &alive_idx;
    let items_ref: &MbsItemTable = &items;
    let n_prov = prov_vec.len();
    let n_rref = rref_vec.len();
    let n_rprac = rprac_vec.len();

    let item_num_strs: Vec<String> = items.item_num.iter().map(|n| n.to_string()).collect();

    let chunk_cap = (chunk_target as f64 * 16.0) as usize;

    let chunk_results: Vec<MbsChunk> = (0..n_chunks)
        .into_par_iter()
        .map(|ci| {
            let start = ci * chunk_target;
            let end = ((ci + 1) * chunk_target).min(n_alive);
            let chunk_seed = (seed as u64).wrapping_add((ci as u64).wrapping_mul(2_654_435_761));
            let mut rng = StdRng::seed_from_u64(chunk_seed);
            let mut chunk = MbsChunk::with_capacity(chunk_cap);

            for &pi in &alive_idx_ref[start..end] {
                let age = year - birth_year_ref[pi];
                let band = age_band_index(age);
                let mut rate = AGE_RATES[band];
                if sex_ref[pi] == 2 {
                    rate *= FEMALE_MULT;
                }
                // Only the post-birth part of the birth year is observable.
                let (window_start, window_days, obs_frac) =
                    person_year_window(year, birth_year_ref[pi], month_of_birth_ref[pi]);
                let dob_days = person_dob_days(birth_year_ref[pi], month_of_birth_ref[pi]);
                rate *= obs_frac;
                let count = poisson_sample(&mut rng, rate) as usize;
                if count == 0 {
                    continue;
                }

                let person_state = state_ref[pi];
                let person_idx = pi as u32;

                for _ in 0..count {
                    generate_one_claim(
                        &mut rng,
                        items_ref,
                        n_prov,
                        n_rref,
                        n_rprac,
                        window_start,
                        window_days,
                        dob_days,
                        person_idx,
                        person_state,
                        &mut chunk,
                        items_ref.item_num.len(),
                        None,
                    );
                }
            }

            chunk
        })
        .collect();

    let total_claims: usize = chunk_results.iter().map(|c| c.dos.len()).sum();
    if total_claims == 0 {
        return 0;
    }

    // Materialise into Vec columns (sequential)
    let mut out_aeuid: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_dos: Vec<i32> = Vec::with_capacity(total_claims);
    let mut out_dop: Vec<i32> = Vec::with_capacity(total_claims);
    let mut out_item: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_aggritem: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_mbscat: Vec<i32> = Vec::with_capacity(total_claims);
    let mut out_mbsgroup: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_mbssubgroup: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_btos: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_spr_rsp: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_feecharged: Vec<f64> = Vec::with_capacity(total_claims);
    let mut out_benpaid: Vec<f64> = Vec::with_capacity(total_claims);
    let mut out_schedfee: Vec<f64> = Vec::with_capacity(total_claims);
    let mut out_numserv: Vec<i32> = Vec::with_capacity(total_claims);
    let mut out_inhospital: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_billtypecd: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_enrpc: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_sprpc: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_scram_spr: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_scram_rpr: Vec<Option<String>> = Vec::with_capacity(total_claims);
    let mut out_sprprac: Vec<String> = Vec::with_capacity(total_claims);
    let mut out_rprprac: Vec<Option<String>> = Vec::with_capacity(total_claims);
    let mut out_rpdate: Vec<i32> = Vec::with_capacity(total_claims);
    let mut out_rprpc: Vec<Option<String>> = Vec::with_capacity(total_claims);

    for c in &chunk_results {
        let n = c.dos.len();
        for k in 0..n {
            let pi = c.person_idx[k] as usize;
            let item_row = c.item_row[k] as usize;
            let cat_idx = c.cat_idx[k] as usize;
            let cat_code = CAT_CODES[cat_idx];

            out_aeuid.push(aeuid_ref[pi].clone());
            out_dos.push(c.dos[k]);
            out_dop.push(c.dop[k]);
            out_item.push(item_num_strs[item_row].clone());
            out_aggritem.push(item_num_strs[item_row].clone());
            out_mbscat.push(cat_code);
            out_mbsgroup.push(items_ref.group[item_row].clone());
            out_mbssubgroup.push(items_ref.sub_heading[item_row].clone());
            out_btos.push(BTOS_LABELS[cat_idx].to_string());
            out_spr_rsp.push(SPR_RSP_POOL[c.spr_rsp_code[k] as usize].to_string());
            out_feecharged.push(c.feecharged[k]);
            out_benpaid.push(c.benpaid[k]);
            out_schedfee.push(c.schedfee[k]);
            out_numserv.push(c.numserv[k]);
            out_inhospital.push(if c.inhospital_flag[k] == 1 {
                "Y".to_string()
            } else {
                "N".to_string()
            });
            out_billtypecd.push(if c.billtype_flag[k] == 0 {
                "D".to_string()
            } else {
                "P".to_string()
            });
            out_enrpc.push(format!("{:04}", c.enrpc[k]));
            out_sprpc.push(format!("{:04}", c.sprpc[k]));
            out_scram_spr.push(prov_vec[c.scram_spr_idx[k] as usize].to_string());

            let rpr = c.scram_rpr_idx[k];
            out_scram_rpr.push(if rpr < 0 {
                None
            } else {
                Some(rref_vec[rpr as usize].to_string())
            });

            out_sprprac.push(rprac_vec[c.sprprac_idx[k] as usize].to_string());

            let rprp = c.rprprac_idx[k];
            out_rprprac.push(if rprp < 0 {
                None
            } else {
                Some(rprac_vec[rprp as usize].to_string())
            });

            out_rpdate.push(c.rpdate[k]);

            let rppc = c.rprpc[k];
            out_rprpc.push(if rppc < 0 {
                None
            } else {
                Some(format!("{:04}", rppc))
            });
        }
    }

    // Assemble columns
    let columns: Vec<NamedCol> = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "DOS",
            col: Col::DateStr(out_dos),
        },
        NamedCol {
            name: "DOP",
            col: Col::DateStr(out_dop),
        },
        NamedCol {
            name: "ITEM",
            col: Col::Str(out_item),
        },
        NamedCol {
            name: "AGGRITEM",
            col: Col::Str(out_aggritem),
        },
        NamedCol {
            name: "MBSCAT",
            col: Col::I32(out_mbscat),
        },
        NamedCol {
            name: "MBSGROUP",
            col: Col::Str(out_mbsgroup),
        },
        NamedCol {
            name: "MBSSUBGROUP",
            col: Col::Str(out_mbssubgroup),
        },
        NamedCol {
            name: "BTOS",
            col: Col::Str(out_btos),
        },
        NamedCol {
            name: "SPR_RSP",
            col: Col::Str(out_spr_rsp),
        },
        NamedCol {
            name: "FEECHARGED",
            col: Col::F64(out_feecharged),
        },
        NamedCol {
            name: "BENPAID",
            col: Col::F64(out_benpaid),
        },
        NamedCol {
            name: "SCHEDFEE",
            col: Col::F64(out_schedfee),
        },
        NamedCol {
            name: "NUMSERV",
            col: Col::I32(out_numserv),
        },
        NamedCol {
            name: "INHOSPITAL",
            col: Col::Str(out_inhospital),
        },
        NamedCol {
            name: "BILLTYPECD",
            col: Col::Str(out_billtypecd),
        },
        NamedCol {
            name: "ENRPC",
            col: Col::Str(out_enrpc),
        },
        NamedCol {
            name: "SPRPC",
            col: Col::Str(out_sprpc),
        },
        NamedCol {
            name: "SCRAM_SPR",
            col: Col::Str(out_scram_spr),
        },
        NamedCol {
            name: "SCRAM_RPR",
            col: Col::StrOpt(out_scram_rpr),
        },
        NamedCol {
            name: "SPRPRAC",
            col: Col::Str(out_sprprac),
        },
        NamedCol {
            name: "RPRPRAC",
            col: Col::StrOpt(out_rprprac),
        },
        NamedCol {
            name: "RPDATE",
            col: Col::DateStr(out_rpdate),
        },
        NamedCol {
            name: "RPRPC",
            col: Col::StrOpt(out_rprpc),
        },
    ];

    if let Err(e) = write_columns_to_parquet(out_path, columns) {
        panic!("Parquet write failed: {}", e);
    }

    total_claims as i32
}

/// Select MBS participants from the spine.
/// Returns indices (1-based for R) of persons who participate.
/// @export
#[extendr]
fn select_mbs_participants__(birth_year: &[i32], min_year: i32, max_year: i32, seed: i64) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let mid_yr = ((min_year as f64 + max_year as f64) / 2.0).round() as i32;

    let mut indices: Vec<i32> = Vec::new();
    let mut first_years: Vec<i32> = Vec::new();
    let mut last_years: Vec<i32> = Vec::new();

    for i in 0..n {
        let age_mid = mid_yr - birth_year[i];
        let p_ever = if age_mid < 15 {
            PART_CHILD
        } else if age_mid < 65 {
            PART_ADULT
        } else {
            PART_ELDERLY
        };

        if rng.gen::<f64>() < p_ever && birth_year[i] <= max_year {
            indices.push((i + 1) as i32); // 1-based for R
            first_years.push(birth_year[i].max(min_year));
            last_years.push(max_year);
        }
    }

    list!(
        index = indices,
        first_year = first_years,
        last_year = last_years
    )
}

// ==========================================================================
// Helpers
// ==========================================================================

/// Days since Unix epoch (1970-01-01) for a given date.
pub fn days_since_epoch(year: i32, month: u32, day: u32) -> i32 {
    // Algorithm from Howard Hinnant's date library
    let y = if month <= 2 { year - 1 } else { year } as i64;
    let m = if month <= 2 { month + 9 } else { month - 3 } as i64;
    let d = day as i64;
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = (y - era * 400) as i64;
    let doy = (153 * m + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    (era * 146097 + doe - 719468) as i32
}

/// Return the last observable day offset within `year`, inclusive.
///
/// R integer `NA` arrives through extendr as `i32::MIN`. A missing death
/// date therefore leaves the full year observable. When only a death year is
/// available, the end of that year is the best supported cutoff.
pub(crate) fn observable_days_in_year(
    year: i32,
    death_year: i32,
    death_month: i32,
    death_day: i32,
) -> Option<i32> {
    let year_start = days_since_epoch(year, 1, 1);
    let year_end = days_since_epoch(year, 12, 31);

    if death_year == i32::MIN || death_year > year {
        return Some(year_end - year_start);
    }
    if death_year < year {
        return None;
    }

    let month = if death_month == i32::MIN {
        12
    } else {
        death_month.clamp(1, 12)
    } as u32;
    let month_end = if month == 12 {
        days_since_epoch(year + 1, 1, 1) - 1
    } else {
        days_since_epoch(year, month + 1, 1) - 1
    };
    let max_day = month_end - days_since_epoch(year, month, 1) + 1;
    let day = if death_day == i32::MIN {
        max_day
    } else {
        death_day.clamp(1, max_day)
    } as u32;

    Some(days_since_epoch(year, month, day) - year_start)
}

/// Date of birth in days since the epoch, dated at the first of the
/// spine's `month_of_birth` (the spine carries no day of birth). A
/// missing month falls back to January.
#[inline]
pub fn person_dob_days(birth_year: i32, month_of_birth: i32) -> i32 {
    days_since_epoch(birth_year, month_of_birth.clamp(1, 12) as u32, 1)
}

/// Observable window inside `year` for one person.
///
/// A person cannot generate a claim before they are born, so in the birth
/// year the window opens on the first of the spine's `month_of_birth`
/// rather than 1 January. Every other year is the whole calendar year.
///
/// Returns `(window_start, window_days, observable_fraction)`. Dates are
/// drawn as `window_start + rng.gen_range(0..=window_days)`, and the
/// Poisson rate is scaled by `observable_fraction` so a person born in
/// (say) October only accrues a quarter of a year's claims in that year.
///
/// A missing `month_of_birth` (extendr renders R `NA_integer_` as
/// `i32::MIN`) falls back to January, i.e. the whole year.
#[inline]
pub fn person_year_window(year: i32, birth_year: i32, month_of_birth: i32) -> (i32, i32, f64) {
    let year_start = days_since_epoch(year, 1, 1);
    let year_end = days_since_epoch(year, 12, 31);
    let days_in_year = year_end - year_start;
    if year != birth_year {
        return (year_start, days_in_year, 1.0);
    }
    let window_start = person_dob_days(birth_year, month_of_birth);
    let window_days = year_end - window_start;
    let frac = (window_days + 1) as f64 / (days_in_year + 1) as f64;
    (window_start, window_days, frac)
}

fn empty_mbs_list() -> List {
    let empty_str: Vec<String> = Vec::new();
    let empty_opt_str: Vec<Option<String>> = Vec::new();
    let empty_i32: Vec<i32> = Vec::new();
    let empty_f64: Vec<f64> = Vec::new();

    list!(
        SYNTHETIC_AEUID = empty_str.clone(),
        DOS = empty_str.clone(),
        DOP = empty_str.clone(),
        ITEM = empty_str.clone(),
        AGGRITEM = empty_str.clone(),
        MBSCAT = empty_i32.clone(),
        MBSGROUP = empty_str.clone(),
        MBSSUBGROUP = empty_str.clone(),
        BTOS = empty_str.clone(),
        SPR_RSP = empty_str.clone(),
        FEECHARGED = empty_f64.clone(),
        BENPAID = empty_f64.clone(),
        SCHEDFEE = empty_f64.clone(),
        NUMSERV = empty_i32.clone(),
        INHOSPITAL = empty_str.clone(),
        BILLTYPECD = empty_str.clone(),
        ENRPC = empty_str.clone(),
        SPRPC = empty_str.clone(),
        SCRAM_SPR = empty_str.clone(),
        SCRAM_RPR = empty_opt_str.clone(),
        SPRPRAC = empty_str.clone(),
        RPRPRAC = empty_opt_str.clone(),
        RPDATE = empty_opt_str.clone(),
        RPRPC = empty_opt_str
    )
}

// ==========================================================================
// Parallel chunk structures
// ==========================================================================

/// Per-thread output buffer for MBS claim generation.
///
/// Stores primitive values only — no String allocations during the hot
/// parallel loop. Strings are materialised sequentially at merge time.
#[derive(Default)]
struct MbsChunk {
    person_idx: Vec<u32>, // index into aeuid_owned (per-person aeuid lookup)
    dos: Vec<i32>,
    dop: Vec<i32>,
    item_row: Vec<u32>, // index into items.item_num / items.group / items.sub_heading
    cat_idx: Vec<u8>,   // 0-7, index into CAT_CODES and BTOS_LABELS
    feecharged: Vec<f64>,
    benpaid: Vec<f64>,
    schedfee: Vec<f64>,
    numserv: Vec<i32>,
    inhospital_flag: Vec<u8>, // 0=N, 1=Y
    billtype_flag: Vec<u8>,   // 0=D (bulk), 1=P (patient)
    spr_rsp_code: Vec<u8>,    // index into SPR_RSP_POOL
    enrpc: Vec<i32>,          // postcode as integer
    sprpc: Vec<i32>,
    scram_spr_idx: Vec<u32>, // index into prov_vec
    scram_rpr_idx: Vec<i32>, // -1 if None, else index into rref_vec
    sprprac_idx: Vec<u32>,   // index into rprac_vec
    rprprac_idx: Vec<i32>,   // -1 if None, else index into rprac_vec
    rpdate: Vec<i32>,        // i32::MIN if None
    rprpc: Vec<i32>,         // -1 if None, else postcode
}

impl MbsChunk {
    fn with_capacity(n: usize) -> Self {
        Self {
            person_idx: Vec::with_capacity(n),
            dos: Vec::with_capacity(n),
            dop: Vec::with_capacity(n),
            item_row: Vec::with_capacity(n),
            cat_idx: Vec::with_capacity(n),
            feecharged: Vec::with_capacity(n),
            benpaid: Vec::with_capacity(n),
            schedfee: Vec::with_capacity(n),
            numserv: Vec::with_capacity(n),
            inhospital_flag: Vec::with_capacity(n),
            billtype_flag: Vec::with_capacity(n),
            spr_rsp_code: Vec::with_capacity(n),
            enrpc: Vec::with_capacity(n),
            sprpc: Vec::with_capacity(n),
            scram_spr_idx: Vec::with_capacity(n),
            scram_rpr_idx: Vec::with_capacity(n),
            sprprac_idx: Vec::with_capacity(n),
            rprprac_idx: Vec::with_capacity(n),
            rpdate: Vec::with_capacity(n),
            rprpc: Vec::with_capacity(n),
        }
    }
}

// Specialty pool: flatten all SPEC_CATx into a single indexed pool
// (static, shared across threads). Index 255 = "GP" fallback.
const SPR_RSP_POOL: [&'static str; 36] = [
    "GP", // 0: default
    "DER", "CAR", "GAS", "NEU", "ONC", "OPH", "ORT", "PSY", "RHE", "URO", // 1-10
    "GEN", "GYN", "ENT", "PLS", // 11-14
    "RAD", "NUC", // 15-16
    "PAT", // 17
    "PSK", "PHY", "OCC", "SOC", "AUD", "DIE", "POD", // 18-24
    // Unused slots reserved for extension
    "UNK", "UNK", "UNK", "UNK", "UNK", "UNK", "UNK", "UNK", "UNK", "UNK", "UNK",
];

fn specialty_to_code(sp: &str) -> u8 {
    match sp {
        "GP" => 0,
        "DER" => 1,
        "CAR" => 2,
        "GAS" => 3,
        "NEU" => 4,
        "ONC" => 5,
        "OPH" => 6,
        "ORT" => 7,
        "PSY" => 8,
        "RHE" => 9,
        "URO" => 10,
        "GEN" => 11,
        "GYN" => 12,
        "ENT" => 13,
        "PLS" => 14,
        "RAD" => 15,
        "NUC" => 16,
        "PAT" => 17,
        "PSK" => 18,
        "PHY" => 19,
        "OCC" => 20,
        "SOC" => 21,
        "AUD" => 22,
        "DIE" => 23,
        "POD" => 24,
        _ => 0,
    }
}

/// Generate a single MBS claim into `out` (primitives only).
#[inline]
fn generate_one_claim(
    rng: &mut StdRng,
    items: &MbsItemTable,
    n_prov: usize,
    n_rref: usize,
    n_rprac: usize,
    // Person-year window from `person_year_window()`: the first day the
    // person can be claimed for, and the span of days from it.
    window_start: i32,
    window_days: i32,
    // Date of birth in days since the epoch. Lagged dates (referrals) are
    // held at it so nothing predates the person.
    dob_days: i32,
    person_idx: u32,
    person_state: i32,
    out: &mut MbsChunk,
    // Number of *baseline* item rows (excludes appended health markers); the
    // empty-category fallback samples within this bound so markers never leak
    // into baseline claims.
    n_base_items: usize,
    // When `Some((item_row, cat_idx))`, emit this exact marker item instead of
    // sampling. The category/item-sampling RNG draws are skipped.
    forced: Option<(usize, usize)>,
) {
    // Category + item: sampled for baseline, fixed for a health marker.
    let (cat_idx, item_row) = match forced {
        Some((ir, ci)) => (ci, ir),
        None => {
            let cat_idx = weighted_sample(rng, &CAT_SHARES);
            let item_pool = &items.cat_indices[cat_idx];
            let item_weights = &items.cat_weights[cat_idx];
            let item_row = if item_pool.is_empty() {
                random_positive_weight_row(rng, &items.weight, n_base_items)
            } else {
                let wi = weighted_sample(rng, item_weights);
                item_pool[wi]
            };
            (cat_idx, item_row)
        }
    };
    let cat_code = CAT_CODES[cat_idx];

    // Date of service
    let dos_offset = rng.gen_range(0..=window_days);
    let dos = window_start + dos_offset;

    // Billing type
    let is_bulk = rng.gen::<f64>() < BULK_BILLED_SHARE;

    // Schedule fee and fee charged
    let sched_fee = items.schedule_fee[item_row];
    let fee_charged = if is_bulk {
        sched_fee
    } else {
        let markup = normal_sample(rng, MARKUP_MEAN, MARKUP_SD).max(1.0);
        (sched_fee * markup * 100.0).round() / 100.0
    };

    // Benefit paid
    let bt = items.benefit_type[item_row].as_bytes();
    let ben75 = items.benefit_75[item_row];
    let ben85 = items.benefit_85[item_row];
    let ben100 = items.benefit_100[item_row];
    let benpaid = match bt.first().copied() {
        Some(b'E') => ben100,
        Some(b'A') => ben75,
        Some(b'B') => ben85,
        Some(b'C') => {
            if is_bulk {
                ben85
            } else {
                ben75
            }
        }
        Some(b'D') => ben75,
        _ => ben75,
    };
    let benpaid = (benpaid * 100.0).round().max(0.0) / 100.0;

    // In-hospital
    let inhospital_flag = if rng.gen::<f64>() < INHOSPITAL_RATE {
        1u8
    } else {
        0u8
    };

    // NUMSERV
    let numserv_idx = weighted_sample(rng, &NUMSERV_PROBS);
    let numserv = NUMSERV_VALS[numserv_idx];

    // DOP
    let lag = normal_sample(rng, 5.0, 4.0).round().max(0.0).min(30.0) as i32;
    let dop = dos + lag;

    // Provider specialty (code, not string)
    let spr_rsp_name = specialty_for_cat(rng, cat_code);
    let spr_rsp_code = specialty_to_code(spr_rsp_name);

    // Provider/practice/ref indices
    let spr_idx = rng.gen_range(0..n_prov) as u32;

    let needs_referral = cat_code != 1 && cat_code != 6;

    let scram_rpr_idx: i32 = if needs_referral {
        rng.gen_range(0..n_rref) as i32
    } else {
        let _ = rng.gen_range(0..n_rref);
        -1
    };

    let spr_prac_idx = rng.gen_range(0..n_rprac) as u32;

    let rprprac_idx: i32 = if needs_referral {
        rng.gen_range(0..n_rprac) as i32
    } else {
        let _ = rng.gen_range(0..n_rprac);
        -1
    };

    let rp_lag = rng.gen_range(1..=90);
    let rpdate: i32 = if needs_referral {
        // A referral cannot predate the person.
        (dos - rp_lag).max(dob_days)
    } else {
        i32::MIN
    };

    // Postcodes as integers
    let enrpc = postcode_int_for_state(rng, person_state);
    let sprpc = postcode_int_for_state(rng, person_state);
    let rprpc: i32 = if needs_referral {
        postcode_int_for_state(rng, person_state)
    } else {
        let _ = postcode_int_for_state(rng, person_state);
        -1
    };

    out.person_idx.push(person_idx);
    out.dos.push(dos);
    out.dop.push(dop);
    out.item_row.push(item_row as u32);
    out.cat_idx.push(cat_idx as u8);
    out.feecharged.push(fee_charged);
    out.benpaid.push(benpaid);
    out.schedfee.push(sched_fee);
    out.numserv.push(numserv);
    out.inhospital_flag.push(inhospital_flag);
    out.billtype_flag.push(if is_bulk { 0 } else { 1 });
    out.spr_rsp_code.push(spr_rsp_code);
    out.enrpc.push(enrpc);
    out.sprpc.push(sprpc);
    out.scram_spr_idx.push(spr_idx);
    out.scram_rpr_idx.push(scram_rpr_idx);
    out.sprprac_idx.push(spr_prac_idx);
    out.rprprac_idx.push(rprprac_idx);
    out.rpdate.push(rpdate);
    out.rprpc.push(rprpc);
}

/// Postcode as integer (no String allocation).
#[inline]
fn postcode_int_for_state(rng: &mut StdRng, state: i32) -> i32 {
    for i in 0..STATE_CODES.len() {
        if STATE_CODES[i] == state {
            return rng.gen_range(STATE_PC_LO[i]..=STATE_PC_HI[i]);
        }
    }
    rng.gen_range(2000..=6999)
}

fn postcode_lookup(pool: &[String], postcode: i32) -> &str {
    if (0..pool.len() as i32).contains(&postcode) {
        pool[postcode as usize].as_str()
    } else {
        "0000"
    }
}

// ==========================================================================
// Full consolidated MBS generator
// ==========================================================================
//
// Single-call orchestrator that replaces the per-year per-chunk
// R-driven loop. Benefits:
//   - one R->Rust transition instead of ~80 * n_years
//   - participant selection + provider pool generation run in Rust
//   - year-level parallelism via rayon::par_iter
//   - streaming ArrowWriter flushes per-chunk RecordBatches so peak
//     memory per year is bounded to one chunk's worth
//   - single output parquet file per year (no part files to merge)

use arrow_array::{ArrayRef, Float64Array, Int32Array, RecordBatch, StringArray};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::arrow_writer::ArrowWriter;
use parquet::basic::Compression;
use parquet::file::properties::WriterProperties;
use std::fs::File;
use std::path::Path;
use std::sync::Arc;

/// Pre-computed read-only context for a full MBS run.
struct MbsContext {
    items: MbsItemTable,
    item_num_strs: Vec<String>,
    provider_pool: Vec<String>,
    ref_pool: Vec<String>,
    prac_pool: Vec<String>,
    /// Number of item rows available to baseline sampling.
    n_base_items: usize,
}

/// Canonical MBS output schema (24 columns).
fn mbs_output_schema() -> Arc<Schema> {
    Arc::new(Schema::new(vec![
        Field::new("SYNTHETIC_AEUID", DataType::Utf8, false),
        // Dates rendered as "ddmmmYY" strings (e.g. 31Jan20).
        Field::new("DOS", DataType::Utf8, false),
        Field::new("DOP", DataType::Utf8, false),
        Field::new("ITEM", DataType::Utf8, false),
        Field::new("AGGRITEM", DataType::Utf8, false),
        Field::new("MBSCAT", DataType::Int32, false),
        Field::new("MBSGROUP", DataType::Utf8, false),
        Field::new("MBSSUBGROUP", DataType::Utf8, false),
        Field::new("BTOS", DataType::Utf8, false),
        Field::new("SPR_RSP", DataType::Utf8, false),
        Field::new("FEECHARGED", DataType::Float64, false),
        Field::new("BENPAID", DataType::Float64, false),
        Field::new("SCHEDFEE", DataType::Float64, false),
        Field::new("NUMSERV", DataType::Int32, false),
        Field::new("INHOSPITAL", DataType::Utf8, false),
        Field::new("BILLTYPECD", DataType::Utf8, false),
        Field::new("ENRPC", DataType::Utf8, false),
        Field::new("SPRPC", DataType::Utf8, false),
        Field::new("SCRAM_SPR", DataType::Utf8, false),
        Field::new("SCRAM_RPR", DataType::Utf8, true),
        Field::new("SPRPRAC", DataType::Utf8, false),
        Field::new("RPRPRAC", DataType::Utf8, true),
        Field::new("RPDATE", DataType::Utf8, true),
        Field::new("RPRPC", DataType::Utf8, true),
    ]))
}

/// Materialise an `MbsChunk` (primitive columns) into an Arrow
/// `RecordBatch` (typed columns) ready for a streaming parquet write.
fn mbs_chunk_to_batch(
    c: MbsChunk,
    ctx: &MbsContext,
    aeuid: &[String],
    schema: Arc<Schema>,
) -> RecordBatch {
    let n = c.dos.len();
    let items = &ctx.items;
    let postcode_strings: Vec<String> = (0..10_000).map(|pc| format!("{:04}", pc)).collect();

    let mut out_aeuid: Vec<&str> = Vec::with_capacity(n);
    let mut out_item: Vec<&str> = Vec::with_capacity(n);
    let mut out_mbscat: Vec<i32> = Vec::with_capacity(n);
    let mut out_mbsgroup: Vec<&str> = Vec::with_capacity(n);
    let mut out_mbssubgroup: Vec<&str> = Vec::with_capacity(n);
    let mut out_btos: Vec<&str> = Vec::with_capacity(n);
    let mut out_spr_rsp: Vec<&str> = Vec::with_capacity(n);
    let mut out_inhospital: Vec<&str> = Vec::with_capacity(n);
    let mut out_billtypecd: Vec<&str> = Vec::with_capacity(n);
    let mut out_enrpc: Vec<&str> = Vec::with_capacity(n);
    let mut out_sprpc: Vec<&str> = Vec::with_capacity(n);
    let mut out_scram_spr: Vec<&str> = Vec::with_capacity(n);
    let mut out_scram_rpr: Vec<Option<&str>> = Vec::with_capacity(n);
    let mut out_sprprac: Vec<&str> = Vec::with_capacity(n);
    let mut out_rprprac: Vec<Option<&str>> = Vec::with_capacity(n);
    let mut out_rprpc: Vec<Option<&str>> = Vec::with_capacity(n);
    let mut out_rpdate: Vec<Option<i32>> = Vec::with_capacity(n);

    for k in 0..n {
        let pi = c.person_idx[k] as usize;
        let item_row = c.item_row[k] as usize;
        let cat_idx = c.cat_idx[k] as usize;
        out_aeuid.push(aeuid[pi].as_str());
        out_item.push(ctx.item_num_strs[item_row].as_str());
        out_mbscat.push(CAT_CODES[cat_idx]);
        out_mbsgroup.push(items.group[item_row].as_str());
        out_mbssubgroup.push(items.sub_heading[item_row].as_str());
        out_btos.push(BTOS_LABELS[cat_idx]);
        out_spr_rsp.push(SPR_RSP_POOL[c.spr_rsp_code[k] as usize]);
        out_inhospital.push(if c.inhospital_flag[k] == 1 { "Y" } else { "N" });
        out_billtypecd.push(if c.billtype_flag[k] == 0 { "D" } else { "P" });
        out_enrpc.push(postcode_lookup(&postcode_strings, c.enrpc[k]));
        out_sprpc.push(postcode_lookup(&postcode_strings, c.sprpc[k]));
        out_scram_spr.push(ctx.provider_pool[c.scram_spr_idx[k] as usize].as_str());
        let rpr = c.scram_rpr_idx[k];
        out_scram_rpr.push(if rpr < 0 {
            None
        } else {
            Some(ctx.ref_pool[rpr as usize].as_str())
        });
        out_sprprac.push(ctx.prac_pool[c.sprprac_idx[k] as usize].as_str());
        let rprp = c.rprprac_idx[k];
        out_rprprac.push(if rprp < 0 {
            None
        } else {
            Some(ctx.prac_pool[rprp as usize].as_str())
        });
        let rppc = c.rprpc[k];
        out_rprpc.push(if rppc < 0 {
            None
        } else {
            Some(postcode_lookup(&postcode_strings, rppc))
        });
        let rpd = c.rpdate[k];
        out_rpdate.push(if rpd == i32::MIN { None } else { Some(rpd) });
    }

    let aeuid_arr: ArrayRef = Arc::new(StringArray::from(out_aeuid));
    let dos_arr: ArrayRef = Arc::new(StringArray::from(
        c.dos
            .iter()
            .map(|&d| crate::parquet_io::days_to_ddmmmyy(d))
            .collect::<Vec<_>>(),
    ));
    let dop_arr: ArrayRef = Arc::new(StringArray::from(
        c.dop
            .iter()
            .map(|&d| crate::parquet_io::days_to_ddmmmyy(d))
            .collect::<Vec<_>>(),
    ));
    let item_arr: ArrayRef = Arc::new(StringArray::from(out_item));
    let aggritem_arr: ArrayRef = item_arr.clone();
    let mbscat_arr: ArrayRef = Arc::new(Int32Array::from(out_mbscat));
    let mbsgroup_arr: ArrayRef = Arc::new(StringArray::from(out_mbsgroup));
    let mbssubgroup_arr: ArrayRef = Arc::new(StringArray::from(out_mbssubgroup));
    let btos_arr: ArrayRef = Arc::new(StringArray::from(out_btos));
    let spr_rsp_arr: ArrayRef = Arc::new(StringArray::from(out_spr_rsp));
    let feecharged_arr: ArrayRef = Arc::new(Float64Array::from(c.feecharged));
    let benpaid_arr: ArrayRef = Arc::new(Float64Array::from(c.benpaid));
    let schedfee_arr: ArrayRef = Arc::new(Float64Array::from(c.schedfee));
    let numserv_arr: ArrayRef = Arc::new(Int32Array::from(c.numserv));
    let inhospital_arr: ArrayRef = Arc::new(StringArray::from(out_inhospital));
    let billtypecd_arr: ArrayRef = Arc::new(StringArray::from(out_billtypecd));
    let enrpc_arr: ArrayRef = Arc::new(StringArray::from(out_enrpc));
    let sprpc_arr: ArrayRef = Arc::new(StringArray::from(out_sprpc));
    let scram_spr_arr: ArrayRef = Arc::new(StringArray::from(out_scram_spr));
    let scram_rpr_arr: ArrayRef = Arc::new(StringArray::from(out_scram_rpr));
    let sprprac_arr: ArrayRef = Arc::new(StringArray::from(out_sprprac));
    let rprprac_arr: ArrayRef = Arc::new(StringArray::from(out_rprprac));
    let rpdate_arr: ArrayRef = Arc::new(StringArray::from(
        out_rpdate
            .iter()
            .map(|&d| d.and_then(crate::parquet_io::days_to_ddmmmyy))
            .collect::<Vec<_>>(),
    ));
    let rprpc_arr: ArrayRef = Arc::new(StringArray::from(out_rprpc));

    RecordBatch::try_new(
        schema,
        vec![
            aeuid_arr,
            dos_arr,
            dop_arr,
            item_arr,
            aggritem_arr,
            mbscat_arr,
            mbsgroup_arr,
            mbssubgroup_arr,
            btos_arr,
            spr_rsp_arr,
            feecharged_arr,
            benpaid_arr,
            schedfee_arr,
            numserv_arr,
            inhospital_arr,
            billtypecd_arr,
            enrpc_arr,
            sprpc_arr,
            scram_spr_arr,
            scram_rpr_arr,
            sprprac_arr,
            rprprac_arr,
            rpdate_arr,
            rprpc_arr,
        ],
    )
    .expect("MBS record batch build")
}

/// Generate one year's MBS claims to a single parquet file using a
/// streaming ArrowWriter. Memory peak is bounded to one chunk.
#[allow(clippy::too_many_arguments)]
fn write_mbs_year_file(
    year: i32,
    participant_rows: &[usize],
    part_first_year: &[i32],
    part_last_year: &[i32],
    aeuid_full: &[String],
    birth_year: &[i32],
    month_of_birth: &[i32],
    sex: &[i32],
    state: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    baseline_income: &[f64],
    baseline_employed: &[i32],
    disability_onset_year: &[i32],
    disability_severity: &[i32],
    disability_is_dc: &[i32],
    ctx: &MbsContext,
    year_seed: u64,
    out_path: &str,
    chunk_persons: usize,
) -> usize {
    // Filter participants alive in this year.
    let alive_idx: Vec<usize> = participant_rows
        .iter()
        .enumerate()
        .filter_map(|(i, &spine_idx)| {
            if part_first_year[i] <= year && part_last_year[i] >= year {
                Some(spine_idx)
            } else {
                None
            }
        })
        .collect();

    let n_alive = alive_idx.len();
    if n_alive == 0 {
        // Write an empty file so downstream readers see it.
        let schema = mbs_output_schema();
        let file = File::create(out_path).expect("create empty mbs file");
        let props = WriterProperties::builder()
            .set_compression(Compression::SNAPPY)
            .build();
        let mut writer =
            ArrowWriter::try_new(file, schema.clone(), Some(props)).expect("mbs writer");
        writer.close().expect("close empty writer");
        return 0;
    }

    let n_prov = ctx.provider_pool.len();
    let n_rref = ctx.ref_pool.len();
    let n_rprac = ctx.prac_pool.len();

    let schema = mbs_output_schema();
    let file = File::create(out_path).unwrap_or_else(|e| panic!("create {}: {}", out_path, e));
    // Cap row groups to 256k rows to keep i32 offsets safe.
    let props = WriterProperties::builder()
        .set_compression(Compression::SNAPPY)
        .set_max_row_group_size(256_000)
        .build();
    let mut writer =
        ArrowWriter::try_new(file, schema.clone(), Some(props)).expect("mbs arrow writer");

    let mut total_claims = 0usize;
    let mut chunk_start = 0usize;

    while chunk_start < n_alive {
        let chunk_end = (chunk_start + chunk_persons).min(n_alive);
        let chunk_seed = year_seed.wrapping_add((chunk_start as u64).wrapping_mul(2_654_435_761));
        let mut rng = StdRng::seed_from_u64(chunk_seed);

        // Rough claim cap per chunk (16 claims/person avg)
        let cap = (chunk_end - chunk_start).saturating_mul(16);
        let mut chunk = MbsChunk::with_capacity(cap);

        for &pi in &alive_idx[chunk_start..chunk_end] {
            let age = year - birth_year[pi];
            let band = age_band_index(age);
            let mut rate = AGE_RATES[band];
            if sex[pi] == 2 {
                rate *= FEMALE_MULT;
            }
            rate *= healthcare_usage_multiplier(
                year,
                age,
                baseline_income[pi],
                baseline_employed[pi],
                disability_onset_year[pi],
                disability_severity[pi],
                disability_is_dc[pi],
            );
            // Only the post-birth part of the birth year is observable.
            let (window_start, window_days, obs_frac) =
                person_year_window(year, birth_year[pi], month_of_birth[pi]);
            let dob_days = person_dob_days(birth_year[pi], month_of_birth[pi]);
            rate *= obs_frac;
            let person_state = state[pi];
            let person_idx = pi as u32;
            // Compose the death cutoff onto the birth window. Both bound the
            // same draw: the window opens at birth and closes at death.
            // `observable_days_in_year` returns an offset from 1 January, so
            // re-base it on `window_start`, which opens later in a birth year.
            let window_days = if year_of_death[pi] == year {
                let last = observable_days_in_year(
                    year,
                    year_of_death[pi],
                    month_of_death[pi],
                    day_of_death[pi],
                )
                .unwrap_or(0);
                let year_start = days_since_epoch(year, 1, 1);
                ((year_start + last) - window_start).clamp(0, window_days)
            } else {
                window_days
            };

            let count = poisson_sample(&mut rng, rate) as usize;
            for _ in 0..count {
                generate_one_claim(
                    &mut rng,
                    &ctx.items,
                    n_prov,
                    n_rref,
                    n_rprac,
                    window_start,
                    window_days,
                    dob_days,
                    person_idx,
                    person_state,
                    &mut chunk,
                    ctx.n_base_items,
                    None,
                );
            }

        }

        if !chunk.dos.is_empty() {
            let n_this = chunk.dos.len();
            let batch = mbs_chunk_to_batch(chunk, ctx, aeuid_full, schema.clone());
            writer.write(&batch).expect("mbs writer.write");
            total_claims += n_this;
        }

        chunk_start = chunk_end;
    }

    writer.close().expect("mbs writer.close");
    total_claims
}

/// Generate all MBS year products in a single Rust call.
///
/// Takes the full spine (all columns that MBS needs, full-population
/// length) plus the item lookup and configuration. Selects participants,
/// builds provider/practice/ref pools, and then runs the per-year
/// generation in parallel across years via rayon::par_iter. Each year
/// writes a single parquet file via a streaming ArrowWriter.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn generate_mbs_full__(
    // Full spine columns (length = n_spine)
    aeuid_dhda: Strings,
    birth_year: &[i32],
    month_of_birth: &[i32],
    sex: &[i32],
    state: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    baseline_income: &[f64],
    baseline_employed: &[i32],
    disability_onset_year: &[i32],
    disability_severity: &[i32],
    disability_is_dc: &[i32],
    // Item lookup
    item_num: &[i32],
    item_category: &[i32],
    item_group: Strings,
    item_sub_heading: Strings,
    item_benefit_type: Strings,
    item_schedule_fee: &[f64],
    item_benefit_75: &[f64],
    item_benefit_85: &[f64],
    item_benefit_100: &[f64],
    item_weight: &[f64],
    // Configuration
    out_dir: &str,
    year_start: i32,
    year_end: i32,
    seed: i64,
    chunk_persons: i32,
) -> List {
    // 1. Convert R Strings to owned Rust Vec<String> once.
    let aeuid_full: Vec<String> = aeuid_dhda.iter().map(|s| s.to_string()).collect();
    let group_vec: Vec<String> = item_group.iter().map(|s| s.to_string()).collect();
    let sub_heading_vec: Vec<String> = item_sub_heading.iter().map(|s| s.to_string()).collect();
    let benefit_type_vec: Vec<String> = item_benefit_type.iter().map(|s| s.to_string()).collect();

    let items = MbsItemTable::from_vectors(
        item_num,
        item_category,
        &group_vec,
        &sub_heading_vec,
        &benefit_type_vec,
        item_schedule_fee,
        item_benefit_75,
        item_benefit_85,
        item_benefit_100,
        item_weight,
    );

    let n_base_items = items.item_num.len();
    let item_num_strs: Vec<String> = items.item_num.iter().map(|n| n.to_string()).collect();

    // 2. Select MBS participants in Rust (seed + 1000 matches the
    // existing select_mbs_participants__ contract).
    let n_spine = birth_year.len();
    assert_eq!(year_of_death.len(), n_spine);
    assert_eq!(month_of_death.len(), n_spine);
    assert_eq!(day_of_death.len(), n_spine);
    let mut part_rng = StdRng::seed_from_u64((seed as u64).wrapping_add(1000));
    let mid_yr = ((year_start as f64 + year_end as f64) / 2.0).round() as i32;
    let mut part_idx: Vec<usize> = Vec::new();
    let mut part_first: Vec<i32> = Vec::new();
    let mut part_last: Vec<i32> = Vec::new();
    for i in 0..n_spine {
        let observed_through = if year_of_death[i] == i32::MIN {
            year_end
        } else {
            year_of_death[i].min(year_end)
        };
        if birth_year[i] > year_end || observed_through < year_start {
            continue;
        }
        let age_mid = mid_yr - birth_year[i];
        let p_ever = if age_mid < 15 {
            PART_CHILD
        } else if age_mid < 65 {
            PART_ADULT
        } else {
            PART_ELDERLY
        };
        let p_ever = (p_ever
            * healthcare_usage_multiplier(
                mid_yr,
                age_mid,
                baseline_income[i],
                baseline_employed[i],
                disability_onset_year[i],
                disability_severity[i],
                disability_is_dc[i],
            ))
        .min(0.995);
        if part_rng.gen::<f64>() < p_ever {
            part_idx.push(i);
            part_first.push(birth_year[i].max(year_start));
            part_last.push(observed_through);
        }
    }

    // 3. Build provider/practice/ref pools in Rust. Match the seed
    // convention used by the old R-side pools (seed + 1003).
    let mut pool_rng = StdRng::seed_from_u64((seed as u64).wrapping_add(1003));
    let n_providers = ((n_spine / 50).max(100)) as usize;
    let provider_pool: Vec<String> = (0..n_providers)
        .map(|_| format!("SPR{:08X}", pool_rng.gen::<u32>() & 0x7FFF_FFFF))
        .collect();
    let ref_pool: Vec<String> = (0..n_providers)
        .map(|_| format!("RPR{:08X}", pool_rng.gen::<u32>() & 0x7FFF_FFFF))
        .collect();
    let n_practices = ((n_spine / 100).max(50)) as usize;
    let prac_pool: Vec<String> = (0..n_practices)
        .map(|_| format!("P{:06}", pool_rng.gen_range(0..999_999u32)))
        .collect();

    let ctx = Arc::new(MbsContext {
        items,
        item_num_strs,
        provider_pool,
        ref_pool,
        prac_pool,
        n_base_items,
    });

    // 4. Year-level parallel generation. Each year writes a single
    // streaming parquet file; memory per year is bounded to one chunk.
    let years: Vec<i32> = (year_start..=year_end).collect();
    let aeuid_full = Arc::new(aeuid_full);
    let part_idx = Arc::new(part_idx);
    let part_first = Arc::new(part_first);
    let part_last = Arc::new(part_last);

    let birth_year_vec: Vec<i32> = birth_year.to_vec();
    let sex_vec: Vec<i32> = sex.to_vec();
    let state_vec: Vec<i32> = state.to_vec();
    let birth_year_arc = Arc::new(birth_year_vec);
    let month_of_birth_arc = Arc::new(month_of_birth.to_vec());
    let sex_arc = Arc::new(sex_vec);
    let state_arc = Arc::new(state_vec);
    let year_of_death_arc = Arc::new(year_of_death.to_vec());
    let month_of_death_arc = Arc::new(month_of_death.to_vec());
    let day_of_death_arc = Arc::new(day_of_death.to_vec());
    let baseline_income_arc = Arc::new(baseline_income.to_vec());
    let baseline_employed_arc = Arc::new(baseline_employed.to_vec());
    let disability_onset_year_arc = Arc::new(disability_onset_year.to_vec());
    let disability_severity_arc = Arc::new(disability_severity.to_vec());
    let disability_is_dc_arc = Arc::new(disability_is_dc.to_vec());

    let chunk_persons = (chunk_persons as usize).max(1);

    let results: Vec<(i32, usize)> = years
        .par_iter()
        .map(|&yr| {
            let pc = format!("madipge-mbs-d-claims-{}.parquet", yr);
            let path = Path::new(out_dir).join(&pc);
            let year_seed = (seed as u64)
                .wrapping_add(1001)
                .wrapping_add((yr - year_start) as u64);
            let n = write_mbs_year_file(
                yr,
                part_idx.as_slice(),
                part_first.as_slice(),
                part_last.as_slice(),
                aeuid_full.as_slice(),
                birth_year_arc.as_slice(),
                month_of_birth_arc.as_slice(),
                sex_arc.as_slice(),
                state_arc.as_slice(),
                year_of_death_arc.as_slice(),
                month_of_death_arc.as_slice(),
                day_of_death_arc.as_slice(),
                baseline_income_arc.as_slice(),
                baseline_employed_arc.as_slice(),
                disability_onset_year_arc.as_slice(),
                disability_severity_arc.as_slice(),
                disability_is_dc_arc.as_slice(),
                &ctx,
                year_seed,
                path.to_str().unwrap(),
                chunk_persons,
            );
            (yr, n)
        })
        .collect();

    // 5. Return (year, n_claims) pairs as an R list.
    let years_out: Vec<i32> = results.iter().map(|(y, _)| *y).collect();
    let counts_out: Vec<i32> = results.iter().map(|(_, n)| *n as i32).collect();
    list!(year = years_out, n_claims = counts_out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn observable_year_window_ends_on_death_date() {
        assert_eq!(observable_days_in_year(2020, 2019, 12, 31), None);
        assert_eq!(observable_days_in_year(2020, 2021, 1, 1), Some(365));
        assert_eq!(
            observable_days_in_year(2020, i32::MIN, i32::MIN, i32::MIN),
            Some(365)
        );
        assert_eq!(observable_days_in_year(2020, 2020, 1, 1), Some(0));
        assert_eq!(
            observable_days_in_year(2020, 2020, 2, 29),
            Some(days_since_epoch(2020, 2, 29) - days_since_epoch(2020, 1, 1))
        );
    }
}

extendr_module! {
    mod mbs;
    fn generate_mbs_year__;
    fn generate_mbs_year_parquet__;
    fn select_mbs_participants__;
    fn generate_mbs_full__;
}

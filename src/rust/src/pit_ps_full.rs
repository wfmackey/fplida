//! Full PIT_PS pipeline in Rust: employment panel, per-year switcher
//! expansion, PS table build, sort, and parquet write — all parallel
//! across years. Replaces the legacy R-orchestrated path in
//! `generate_pit_ps.R`.

use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use rayon::prelude::*;
use std::path::Path;
use std::sync::Arc;

use crate::employment::{run_employment_panel, EmploymentPanel};
use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
use crate::pit_ps_build::{compute_payg_tax, round2, sg_rate_for_year};
use crate::spine::anzsco_table::ANZSCO_CODES;
use std::sync::LazyLock;

// The ATO salary and wage occupation code list, embedded at compile time so
// the reporting-noise draws stay inside a real ATO code frame rather than
// wandering into ANZSCO codes the ATO does not publish. Source:
// data.gov.au "ATO Salary and Wage Occupation Codes", CC BY 2.5 AU.
// Regenerate with data-raw/update_ato_occupation_codes.R.
const ATO_OCCUPATION_TSV: &str =
    include_str!("../../../inst/extdata/codeframes/ato-occupation-codes.tsv");

/// Valid ATO occupation codes, ascending. Parsed once.
static ATO_OCCUPATION_CODES: LazyLock<Vec<i32>> = LazyLock::new(|| {
    let mut v: Vec<i32> = ATO_OCCUPATION_TSV
        .lines()
        .skip(1) // header
        .filter_map(|line| line.split('\t').next())
        .filter_map(|f| f.trim().trim_matches('"').parse::<i32>().ok())
        .collect();
    v.sort_unstable();
    v.dedup();
    v
});

// Retain most true employment-panel occupations, but add enough ATO-side
// reporting noise that exact ATO/Census occupation agreement is about 80%
// after existing job mobility, Census ANZSCO remapping, ATO nfd values, and
// ATO-specific salary/wage occupation special codes are accounted for.
const ATO_OCCUPATION_REPORT_KEEP_RATE: f64 = 0.97;
const ATO_OCCUPATION_NFD_RATE_MANAGER: f64 = 0.10;
const ATO_OCCUPATION_NFD_RATE_OTHER: f64 = 0.035;
const ATO_OCCUPATION_NOT_LISTED_RATE: f64 = 0.002;
/// "Occupation not listed" on the ATO salary and wage code list.
const ATO_OCCUPATION_NOT_LISTED: i32 = 999000;

// ATO salary and wage occupation special codes from the 2025-26 code list.
// These are ATO return codes, not Census OCCP/ANZSCO codes.
const ATO_CONSULTANT_CODES: [i32; 31] = [
    913301, 922101, 922201, 922301, 922302, 922303, 922401, 922501, 922502, 922503, 923202, 923301,
    923302, 923303, 923304, 923305, 923306, 923307, 923401, 925001, 925101, 925401, 926101, 944202,
    945102, 945104, 955201, 959902, 961101, 961202, 962102,
];

const ATO_APPRENTICE_TRAINEE_CODES: [i32; 66] = [
    931101, 931102, 931301, 931302, 932101, 932102, 932201, 932301, 932302, 932303, 932401, 933101,
    933102, 933201, 933202, 933301, 933302, 933401, 934101, 934201, 934202, 934203, 935101, 935102,
    935103, 935104, 936102, 936103, 936104, 936201, 936202, 936203, 936204, 939101, 939201, 939301,
    939302, 939303, 939401, 939901, 939902, 939903, 939904, 939905, 941001, 942001, 943101, 944201,
    945101, 945103, 945201, 945203, 951001, 953001, 955001, 959901, 971201, 973001, 974101, 981101,
    983101, 984101, 984102, 984103, 989901, 989902,
];

/// Prior-employer identifier for a switcher's old-employer row: the business
/// the person worked at in the previous employer spell (slot `spell - 1`),
/// using the same shared scheme as the primary employer so it also resolves
/// to BLADE. Falls back to a legacy 11-digit synthetic ABN when no pool.
fn make_employer_abn_prior(person_number: i64, prior_slot: i64, seed: i64) -> String {
    let pool = crate::business_pool::snapshot();
    crate::business_pool::employer_bn(&pool, person_number, prior_slot, seed).unwrap_or_else(|| {
        let h = person_number
            .wrapping_mul(1_000_003)
            .wrapping_add(prior_slot.wrapping_mul(999_983))
            .wrapping_add(seed)
            .rem_euclid(100_000_000_000);
        format!("{:011}", h)
    })
}

fn draw_alternative_anzsco_code(rng: &mut StdRng, current_code: i32, major: i32) -> i32 {
    let major_group = occupation_major_group(current_code, major);
    if !(1..=8).contains(&major_group) {
        return current_code;
    }

    // Draw from the ATO code list, not ANZSCO: a taxpayer reporting a different
    // occupation still picks a code the ATO publishes.
    let candidates: Vec<i32> = ATO_OCCUPATION_CODES
        .iter()
        .copied()
        .filter(|code| code / 100_000 == major_group && *code != current_code)
        .collect();
    if candidates.is_empty() {
        return current_code;
    }
    candidates[rng.gen_range(0..candidates.len())]
}

/// An "not elsewhere classified" ATO code within a major group. These end in
/// 99 by the ATO's own convention. Falls back to 999000 when the group has
/// none.
fn draw_ato_nec_code(rng: &mut StdRng, major_group: i32) -> i32 {
    let candidates: Vec<i32> = ATO_OCCUPATION_CODES
        .iter()
        .copied()
        .filter(|code| code / 100_000 == major_group && code % 100 == 99)
        .collect();
    if candidates.is_empty() {
        return ATO_OCCUPATION_NOT_LISTED;
    }
    candidates[rng.gen_range(0..candidates.len())]
}

fn draw_code_from(rng: &mut StdRng, codes: &[i32]) -> i32 {
    codes[rng.gen_range(0..codes.len())]
}

fn occupation_major_group(current_code: i32, major: i32) -> i32 {
    if (1..=8).contains(&major) {
        major
    } else {
        current_code / 100000
    }
}

fn occupation_special_code_rate(age: i32, major_group: i32) -> f64 {
    let age_rate: f64 = match age {
        0..=24 => 0.055,
        25..=29 => 0.035,
        30..=34 => 0.015,
        _ => 0.004,
    };
    let occupation_rate: f64 = match major_group {
        1 | 2 => 0.012,
        3 => 0.006,
        _ => 0.0,
    };
    (age_rate + occupation_rate).min(0.08)
}

fn draw_ato_special_occupation_code(rng: &mut StdRng, age: i32, major_group: i32) -> i32 {
    if rng.gen::<f64>() < ATO_OCCUPATION_NOT_LISTED_RATE {
        return 999000;
    }

    let apprentice_weight = match age {
        0..=24 => 0.85,
        25..=29 => 0.55,
        30..=34 => 0.25,
        _ => 0.08,
    };
    let consultant_weight = match major_group {
        1 | 2 => 0.85,
        3 => 0.50,
        _ => 0.20,
    };

    if rng.gen::<f64>() < apprentice_weight {
        draw_code_from(rng, &ATO_APPRENTICE_TRAINEE_CODES)
    } else if rng.gen::<f64>() < consultant_weight {
        draw_code_from(rng, &ATO_CONSULTANT_CODES)
    } else if rng.gen::<f64>() < 0.5 {
        draw_code_from(rng, &ATO_APPRENTICE_TRAINEE_CODES)
    } else {
        draw_code_from(rng, &ATO_CONSULTANT_CODES)
    }
}

fn occupation_nfd_rate(current_code: i32, major: i32) -> f64 {
    match occupation_major_group(current_code, major) {
        1 => ATO_OCCUPATION_NFD_RATE_MANAGER,
        2..=8 => ATO_OCCUPATION_NFD_RATE_OTHER,
        _ => 0.0,
    }
}

fn reported_ato_occupation_code(rng: &mut StdRng, current_code: i32, major: i32, age: i32) -> i32 {
    if current_code <= 0 {
        return current_code;
    }

    let major_group = occupation_major_group(current_code, major);
    if rng.gen::<f64>() < occupation_special_code_rate(age, major_group) {
        return draw_ato_special_occupation_code(rng, age, major_group);
    }

    if (1..=8).contains(&major_group) && rng.gen::<f64>() < occupation_nfd_rate(current_code, major)
    {
        // The ATO list has no major-group "not further defined" code. A
        // taxpayer who cannot find their exact occupation picks the "nec"
        // code for their group, or 999000 "Occupation not listed".
        return draw_ato_nec_code(rng, major_group);
    }

    if rng.gen::<f64>() < ATO_OCCUPATION_REPORT_KEEP_RATE {
        current_code
    } else {
        draw_alternative_anzsco_code(rng, current_code, major)
    }
}

/// Write an occupation panel parquet for a year (one row per primary
/// person-year, deduplicated on (aeuid, year)).
fn write_occ_panel_year(
    path: &str,
    aeuid: Vec<String>,
    year: Vec<i32>,
    anzsco_code: Vec<i32>,
    anzsco_unit: Vec<i32>,
) -> std::result::Result<(), String> {
    let cols = vec![
        NamedCol {
            name: "aeuid_ato",
            col: Col::Str(aeuid),
        },
        NamedCol {
            name: "year",
            col: Col::I32(year),
        },
        NamedCol {
            name: "anzsco_code",
            col: Col::I32(anzsco_code),
        },
        NamedCol {
            name: "anzsco_unit",
            col: Col::I32(anzsco_unit),
        },
    ];
    write_columns_to_parquet(path, cols)
}

/// Full PIT_PS pipeline: generate employment panel, then per-year
/// parallel switcher expansion, PS build, sort, and parquet write.
///
/// `product_names` is a parallel Strings vector aligned with `years`;
/// each entry is the canonical PLIDA product filename stem for that FY.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
pub fn generate_pit_ps_full_to_parquet__(
    id: Strings,
    aeuid_ato: Strings,
    birth_year: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    baseline_hours: &[i32],
    anzsco_major: &[i32],
    industry: &[i32],
    anzsco_code: &[i32],
    task_physical: &[f64],
    archetype: &[i32],
    disability_onset_year: &[i32],
    disability_is_dc: &[i32],
    disability_severity: &[i32],
    disability_dose: &[f64],
    years: &[i32],
    seed: i32,
    out_dir: &str,
    occ_out_dir: &str,
    product_names: Strings,
) -> List {
    // Ensure output directories exist.
    std::fs::create_dir_all(out_dir).ok();
    std::fs::create_dir_all(occ_out_dir).ok();

    let id_vec: Vec<String> = id.iter().map(|s| s.to_string()).collect();
    let aeuid_vec: Vec<String> = aeuid_ato.iter().map(|s| s.to_string()).collect();
    // Keep a copy of spine aeuid for prior-ABN lookup (indexed by person_idx from panel).
    let _ = &aeuid_vec;

    // 1. Generate full multi-year employment panel (Rust).
    let panel: EmploymentPanel = run_employment_panel(
        id_vec.clone(),
        aeuid_vec,
        birth_year,
        baseline_employed,
        baseline_income,
        baseline_hours,
        anzsco_major,
        industry,
        seed,
        years,
        disability_onset_year,
        disability_is_dc,
        disability_severity,
        disability_dose,
        0, // target_year = 0 → all years
        anzsco_code,
        task_physical,
        archetype,
    );

    // 2. Partition panel row indices by year for parallel workers.
    //    Also aggregate product-name mapping.
    let years_v: Vec<i32> = years.to_vec();
    let product_names_v: Vec<String> = product_names.iter().map(|s| s.to_string()).collect();
    assert_eq!(
        years_v.len(),
        product_names_v.len(),
        "years and product_names length mismatch"
    );

    let panel = Arc::new(panel);
    let out_dir_str = out_dir.to_string();
    let occ_out_dir_str = occ_out_dir.to_string();
    let seed_i64 = seed as i64;

    // Build per-year row indices.
    let mut year_rows: std::collections::HashMap<i32, Vec<usize>> =
        std::collections::HashMap::new();
    for (j, &yr) in panel.year.iter().enumerate() {
        year_rows.entry(yr).or_default().push(j);
    }
    let year_rows = Arc::new(year_rows);

    // 3. Year-level parallel build. Workers operate on row indices into
    //    the shared panel — no String cloning until the final parquet
    //    write, to avoid allocator contention at 30m+ scale.
    //
    //    Per-row representation for switcher-expanded rows:
    //      panel_idx  : usize    — original row index into the shared panel
    //      gross      : f64      — original or share-adjusted gross pay
    //      prior_abn  : Option<String> — Some for prior-employer row of a
    //                                    switcher pair (allocated once, ~5%);
    //                                    None means "use panel.employer_id[idx]"
    let results: Vec<(i32, usize)> = years_v
        .par_iter()
        .zip(product_names_v.par_iter())
        .map(|(&yr, pname)| {
            let empty: Vec<usize> = Vec::new();
            let rows_for_year: &Vec<usize> = year_rows.get(&yr).unwrap_or(&empty);
            let n_yr = rows_for_year.len();

            // --- Switcher expansion: produce (panel_idx, gross, prior_abn) tuples ---
            // Switch month is per-person (shared with expand_wage_ledger) so the
            // employer gross split is identical to what STP/PIT reconstruct.
            let cap = n_yr + (n_yr / 20).max(1);
            let mut ex_idx: Vec<usize> = Vec::with_capacity(cap);
            let mut ex_gross: Vec<f64> = Vec::with_capacity(cap);
            let mut ex_prior: Vec<Option<String>> = Vec::with_capacity(cap);

            for &j in rows_for_year.iter() {
                let primary = panel.primary_job[j];
                let switched = panel.switched_employer[j];
                let gross = panel.gross_annual[j];

                if switched && primary {
                    let prior_pn = crate::business_pool::person_number(&panel.person_id[j]);
                    let switch_month =
                        crate::employment::ledger_switch_month(prior_pn, seed_i64, yr) as f64;
                    let old_share = switch_month / 12.0;
                    let new_share = 1.0 - old_share;
                    let prior_slot = (panel.spell[j] as i64 - 1).max(0);
                    let prior_abn = make_employer_abn_prior(prior_pn, prior_slot, seed_i64);
                    // Old-employer row (prior ABN, old share)
                    ex_idx.push(j);
                    ex_gross.push(round2(gross * old_share));
                    ex_prior.push(Some(prior_abn));
                    // New-employer row (current ABN, new share)
                    ex_idx.push(j);
                    ex_gross.push(round2(gross * new_share));
                    ex_prior.push(None);
                } else {
                    ex_idx.push(j);
                    ex_gross.push(gross);
                    ex_prior.push(None);
                }
            }

            let n_rows = ex_idx.len();

            // --- Sort by SYNTHETIC_AEUID via permutation over the shared panel ---
            let mut order: Vec<usize> = (0..n_rows).collect();
            order.sort_by(|&a, &b| panel.aeuid_ato[ex_idx[a]].cmp(&panel.aeuid_ato[ex_idx[b]]));

            // --- Build output columns in sorted order, materialising Strings
            //     exactly once per row per String column. ---
            let mut rng_ps = StdRng::seed_from_u64(seed as u64);

            let mut s_aeuid: Vec<String> = Vec::with_capacity(n_rows);
            let mut s_year: Vec<i32> = Vec::with_capacity(n_rows);
            let mut s_employer: Vec<String> = Vec::with_capacity(n_rows);
            let mut s_gross: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_tax: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_fbt: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_sg: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_allow: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_lump_a: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_lump_b: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_lump_d: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_lump_e: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_union: Vec<f64> = Vec::with_capacity(n_rows);
            let mut s_wg: Vec<f64> = Vec::with_capacity(n_rows);

            // NOTE: We draw RNG values in the SORTED (output) order, not the
            // original panel order. This differs subtly from the previous code,
            // which drew in panel order. It does not change distributional
            // properties (draws are iid) and this path is the only consumer.

            // Fold RNG draws into the sort-order loop to avoid a second pass.
            // The build_ps_columns_core loop is inlined here to share the
            // panel index access.
            let sg_r = sg_rate_for_year(yr);
            for &k in &order {
                let panel_idx = ex_idx[k];
                let gross = ex_gross[k];
                let major = panel.anzsco_major[panel_idx];

                let tax = compute_payg_tax(gross);

                let fbt = if rng_ps.gen::<f64>() < 0.08 {
                    round2(gross * (0.02 + rng_ps.gen::<f64>() * 0.06))
                } else {
                    0.0
                };
                let allowances = if rng_ps.gen::<f64>() < 0.15 {
                    round2(500.0 + rng_ps.gen::<f64>() * 4500.0)
                } else {
                    0.0
                };
                let lump_a = if rng_ps.gen::<f64>() < 0.03 {
                    round2(200.0 + rng_ps.gen::<f64>() * 2800.0)
                } else {
                    0.0
                };
                let lump_b = if rng_ps.gen::<f64>() < 0.005 {
                    round2(500.0 + rng_ps.gen::<f64>() * 9500.0)
                } else {
                    0.0
                };
                let lump_d = if rng_ps.gen::<f64>() < 0.003 {
                    round2(200.0 + rng_ps.gen::<f64>() * 4800.0)
                } else {
                    0.0
                };
                let lump_e = if rng_ps.gen::<f64>() < 0.002 {
                    round2(500.0 + rng_ps.gen::<f64>() * 7500.0)
                } else {
                    0.0
                };
                let union_base = if matches!(major, 3 | 4 | 7 | 8) {
                    0.25
                } else {
                    0.10
                };
                let union_fees = if rng_ps.gen::<f64>() < union_base {
                    round2(200.0 + rng_ps.gen::<f64>() * 1000.0)
                } else {
                    0.0
                };
                let wg = if rng_ps.gen::<f64>() < 0.05 {
                    round2(50.0 + rng_ps.gen::<f64>() * 450.0)
                } else {
                    0.0
                };

                // Materialise Strings exactly once per row.
                s_aeuid.push(panel.aeuid_ato[panel_idx].clone());
                s_year.push(yr);
                match ex_prior[k].take() {
                    Some(prior) => s_employer.push(prior),
                    None => s_employer.push(panel.employer_id[panel_idx].clone()),
                }
                s_gross.push(round2(gross));
                s_tax.push(tax);
                s_fbt.push(fbt);
                s_sg.push(round2(gross * sg_r));
                s_allow.push(allowances);
                s_lump_a.push(lump_a);
                s_lump_b.push(lump_b);
                s_lump_d.push(lump_d);
                s_lump_e.push(lump_e);
                s_union.push(union_fees);
                s_wg.push(wg);
            }

            let n_ps = s_aeuid.len();

            // --- Write PS parquet ---
            let ps_path = Path::new(&out_dir_str).join(format!("{}.parquet", pname));
            let cols = vec![
                NamedCol {
                    name: "SYNTHETIC_AEUID",
                    col: Col::Str(s_aeuid),
                },
                NamedCol {
                    name: "FINANCIAL_YEAR",
                    col: Col::I32(s_year),
                },
                NamedCol {
                    name: "EMPLOYER_ABN",
                    col: Col::Str(s_employer),
                },
                NamedCol {
                    name: "GROSS_PAYMENTS",
                    col: Col::F64(s_gross),
                },
                NamedCol {
                    name: "TAX_WITHHELD",
                    col: Col::F64(s_tax),
                },
                NamedCol {
                    name: "REPORTABLE_FBT",
                    col: Col::F64(s_fbt),
                },
                NamedCol {
                    name: "SUPER_GUARANTEE",
                    col: Col::F64(s_sg),
                },
                NamedCol {
                    name: "ALLOWANCES",
                    col: Col::F64(s_allow),
                },
                NamedCol {
                    name: "LUMP_SUM_A",
                    col: Col::F64(s_lump_a),
                },
                NamedCol {
                    name: "LUMP_SUM_B",
                    col: Col::F64(s_lump_b),
                },
                NamedCol {
                    name: "LUMP_SUM_D",
                    col: Col::F64(s_lump_d),
                },
                NamedCol {
                    name: "LUMP_SUM_E",
                    col: Col::F64(s_lump_e),
                },
                NamedCol {
                    name: "UNION_FEES",
                    col: Col::F64(s_union),
                },
                NamedCol {
                    name: "WORKPLACE_GIVING",
                    col: Col::F64(s_wg),
                },
            ];
            write_columns_to_parquet(ps_path.to_str().unwrap(), cols)
                .unwrap_or_else(|e| panic!("PIT_PS parquet write ({}): {}", yr, e));

            // --- Occupation panel for this year (primary rows only, dedup) ---
            // Walk the year's panel rows directly — no need for yr_aeuid copy.
            {
                let mut seen: std::collections::HashSet<(&str, i32)> =
                    std::collections::HashSet::with_capacity(n_yr);
                let mut rng_occ =
                    StdRng::seed_from_u64((seed as u64).wrapping_add(450).wrapping_add(yr as u64));
                let mut op_aeuid: Vec<String> = Vec::new();
                let mut op_year: Vec<i32> = Vec::new();
                let mut op_code: Vec<i32> = Vec::new();
                let mut op_unit: Vec<i32> = Vec::new();
                for &j in rows_for_year.iter() {
                    if !panel.primary_job[j] {
                        continue;
                    }
                    let key_str: &str = panel.aeuid_ato[j].as_str();
                    let key_yr = panel.year[j];
                    if seen.insert((key_str, key_yr)) {
                        let person_idx0 = panel.person_idx[j].saturating_sub(1);
                        let age = birth_year
                            .get(person_idx0)
                            .map(|&by| key_yr - by)
                            .unwrap_or(40);
                        let reported_code = reported_ato_occupation_code(
                            &mut rng_occ,
                            panel.anzsco_code[j],
                            panel.anzsco_major[j],
                            age,
                        );
                        op_aeuid.push(panel.aeuid_ato[j].clone());
                        op_year.push(key_yr);
                        op_code.push(reported_code);
                        op_unit.push(if reported_code > 0 {
                            reported_code / 100
                        } else {
                            0
                        });
                    }
                }
                let occ_path = Path::new(&occ_out_dir_str).join(format!("occ_{}.parquet", yr));
                write_occ_panel_year(
                    occ_path.to_str().unwrap(),
                    op_aeuid,
                    op_year,
                    op_code,
                    op_unit,
                )
                .unwrap_or_else(|e| panic!("occ panel write ({}): {}", yr, e));
            }

            (yr, n_ps)
        })
        .collect();

    let years_out: Vec<i32> = results.iter().map(|(y, _)| *y).collect();
    let counts_out: Vec<i32> = results.iter().map(|(_, n)| *n as i32).collect();
    list!(year = years_out, n_rows = counts_out)
}

extendr_module! {
    mod pit_ps_full;
    fn generate_pit_ps_full_to_parquet__;
}

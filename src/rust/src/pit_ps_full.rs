//! Full PIT_PS pipeline in Rust: employment panel, per-year switcher
//! expansion, PS record build, sort, projection onto the tables the PLIDA
//! data item list declares for the year, and parquet write — all parallel
//! across years. Replaces the legacy R-orchestrated path in
//! `generate_pit_ps.R`.
//!
//! The table plan comes down from R, which reads it from
//! `inst/plida_metadata/variables.csv`. Nothing about the published schema is
//! written here: the year a table reports, the variables it carries, the
//! extract window, and which of `BN` and `ABN_HASH_TRUNC` names the employer
//! are all the registry's answers.

use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use rayon::prelude::*;
use std::path::Path;
use std::sync::Arc;

use crate::employment::{run_employment_panel, EmploymentPanel};
use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
use crate::pit_ps_build::{compute_payg_tax, round2, sg_rate_for_year};
use crate::pit_ps_tables::{
    build_column, rows_for_table, rows_one_per_person, PersonFields, YearRecords, PS_TYPE_BPSI,
    PS_TYPE_ETP, PS_TYPE_FEI, PS_TYPE_INB, PS_TYPE_SIS,
};
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

/// The employer identifier as a business number. A build with a BLADE stage
/// draws employers straight from the business spine and they already carry
/// the `BN` prefix; a standalone build mints an unprefixed number, which the
/// `BN` column must not publish as it stands. Prefixing is safe for the
/// bridge either way: `abn_hash_trunc` reads the digits and ignores the
/// prefix, so both spellings hash to the same value.
fn as_business_number(id: &str) -> String {
    if id.starts_with("BN") {
        return id.to_string();
    }
    let digits: String = id.chars().filter(|c| c.is_ascii_digit()).collect();
    let value: i128 = digits.parse::<i128>().unwrap_or(0);
    crate::blade::helpers::numeric_id("BN", value, 11)
}

/// Days since 1970-01-01 for a civil date. Howard Hinnant's
/// days-from-civil algorithm, the inverse of `parquet_io::days_to_ddmmmyy`.
fn days_from_civil(year: i32, month: i32, day: i32) -> i32 {
    let y = if month <= 2 { year - 1 } else { year } as i64;
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400; // [0, 399]
    let mp = if month > 2 { month - 3 } else { month + 9 } as i64; // [0, 11]
    let doy = (153 * mp + 2) / 5 + day as i64 - 1; // [0, 365]
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy; // [0, 146096]
    (era * 146_097 + doe - 719_468) as i32
}

/// First day of the month `offset` months after 1 July of `fy_start`.
fn month_start_after_july(fy_start: i32, offset: i32) -> i32 {
    let month0 = 6 + offset; // 0-based month index from January
    days_from_civil(fy_start + month0 / 12, month0 % 12 + 1, 1)
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

// How often each of the five PAYG payment summary forms is the one a record
// carries. The ATO publishes the forms but not how the delivery divides
// between them, so every rate here is a modelling choice; what the shape
// rests on is that these are employees, so the individual non-business
// summary is nearly all of it.
//
// An employment termination payment summary reports a discrete payment rather
// than a year of pay, and a person leaving an employer mid-year is who
// receives one, so the rate is far higher on the old-employer row of a
// switcher pair than elsewhere.
const ETP_RATE_ON_TERMINATION: f64 = 0.15;
const ETP_RATE_OTHERWISE: f64 = 0.004;
// A superannuation income stream summary belongs to someone drawing a
// pension, so it only applies from preservation age on. Sixty is the
// preservation age for everyone born from 1 July 1964, which is most of the
// people still working in the years these tables cover.
const SIS_MIN_AGE: i32 = 60;
const SIS_RATE: f64 = 0.02;
const BPSI_RATE: f64 = 0.02;
const FEI_RATE: f64 = 0.004;
// Payment types on the individual non-business summary: salary or wages,
// working holiday maker, closely held payees. Modelling choices again, set so
// salary and wages carries almost all of it.
const INB_WHM_RATE: f64 = 0.02;
const INB_CLOSELY_HELD_RATE: f64 = 0.01;
// The share of the gross carved out as a tax-free component on the summaries
// that report one. A modelling choice: the statutory tax-free amounts depend
// on service length and preservation age, neither of which the panel carries.
const TAX_FREE_SHARE_MAX: f64 = 0.4;
// Of the payment summaries carrying a lump sum A, the share paid on a
// termination for a reason other than redundancy, invalidity or early
// retirement. A modelling choice; the delivery publishes no split.
const LSPA_OTHER_TERMINATION_RATE: f64 = 0.2;
// Payment summaries lodged after a six-month extract was cut. A modelling
// choice; the delivery does not publish lodgement timing.
const LATE_LODGEMENT_RATE: f64 = 0.10;
// One payment summary in twenty amends an earlier one, matching the rate the
// other ATO products use for `AMDT_CD`.
const AMENDMENT_RATE: f64 = 0.05;
// Address repair outcomes, in the order `REPAIR_RESULT_LABEL` gives them:
// no repair needed, a repair made, a repair attempted and nothing changed.
// The ABS describes the step but publishes no outcome rates, so these are a
// modelling choice.
const REPAIR_NONE_RATE: f64 = 0.82;
const REPAIR_MADE_RATE: f64 = 0.13;

/// Full PIT_PS pipeline: generate the employment panel, then per-year
/// parallel switcher expansion, record build, sort, and one parquet per
/// table the data item list declares for the year.
///
/// The `tbl_*` arguments are the table plan, one entry per output table: the
/// file name stem the package gives a product's table, the table's own name,
/// the financial year it reports, its extract window in months, the business
/// identifier column it keys on, whether it is a geography-only table, and
/// its declared variables flattened into `tbl_variables` with
/// `tbl_var_offsets` marking where each table's block starts.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
pub fn generate_pit_ps_full_to_parquet__(
    id: Strings,
    aeuid_ato: Strings,
    birth_year: &[i32],
    birth_month: &[i32],
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
    geo_sa1: Strings,
    geo_mb: Strings,
    geo_lga: Strings,
    geo_arid: Strings,
    geo_sa2: &[i32],
    geo_sa3: &[i32],
    geo_sa4: &[i32],
    geo_ste: &[i32],
    years: &[i32],
    seed: i32,
    out_dir: &str,
    occ_out_dir: &str,
    tbl_stem: Strings,
    tbl_table: Strings,
    tbl_year: &[i32],
    tbl_months: &[i32],
    tbl_key_var: Strings,
    tbl_geo: &[i32],
    tbl_variables: Strings,
    tbl_var_offsets: &[i32],
) -> List {
    // Ensure output directories exist.
    std::fs::create_dir_all(out_dir).ok();
    std::fs::create_dir_all(occ_out_dir).ok();

    let id_vec: Vec<String> = id.iter().map(|s| s.to_string()).collect();
    let aeuid_vec: Vec<String> = aeuid_ato.iter().map(|s| s.to_string()).collect();
    // Keep a copy of spine aeuid for prior-ABN lookup (indexed by person_idx from panel).
    let _ = &aeuid_vec;

    let sa1_vec: Vec<String> = geo_sa1.iter().map(|s| s.to_string()).collect();
    let mb_vec: Vec<String> = geo_mb.iter().map(|s| s.to_string()).collect();
    let lga_vec: Vec<String> = geo_lga.iter().map(|s| s.to_string()).collect();
    let arid_vec: Vec<String> = geo_arid.iter().map(|s| s.to_string()).collect();
    let people = PersonFields {
        birth_year,
        birth_month,
        sa1: &sa1_vec,
        mb: &mb_vec,
        lga: &lga_vec,
        arid: &arid_vec,
        sa2: geo_sa2,
        sa3: geo_sa3,
        sa4: geo_sa4,
        ste: geo_ste,
    };

    // Unpack the table plan into one entry per output table.
    let plan_stem: Vec<String> = tbl_stem.iter().map(|s| s.to_string()).collect();
    let plan_table: Vec<String> = tbl_table.iter().map(|s| s.to_string()).collect();
    let plan_key: Vec<String> = tbl_key_var.iter().map(|s| s.to_string()).collect();
    let plan_variable: Vec<String> = tbl_variables.iter().map(|s| s.to_string()).collect();
    let n_tables = plan_table.len();
    assert_eq!(
        tbl_var_offsets.len(),
        n_tables + 1,
        "PIT_PS table plan: one variable offset per table plus a final bound"
    );
    let plan_vars: Vec<&[String]> = (0..n_tables)
        .map(|t| {
            let lo = tbl_var_offsets[t] as usize;
            let hi = tbl_var_offsets[t + 1] as usize;
            &plan_variable[lo..hi]
        })
        .collect();

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

    // 2. Partition panel row indices by year for parallel workers, and group
    //    the table plan by the financial year each table reports.
    let years_v: Vec<i32> = years.to_vec();
    let tables_by_year: Vec<Vec<usize>> = years_v
        .iter()
        .map(|&yr| {
            (0..n_tables)
                .filter(|&t| tbl_year[t] == yr)
                .collect::<Vec<usize>>()
        })
        .collect();

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
    //    the shared panel — no String cloning until the record build, to
    //    avoid allocator contention at 30m+ scale.
    //
    //    Per-row representation for switcher-expanded rows:
    //      panel_idx  : usize    — original row index into the shared panel
    //      gross      : f64      — original or share-adjusted gross pay
    //      prior_abn  : Option<String> — Some for prior-employer row of a
    //                                    switcher pair (allocated once, ~5%);
    //                                    None means "use panel.employer_id[idx]"
    //      split_day  : i32      — the day the two employer spells meet, so
    //                              each row's reported period is the part of
    //                              the year it covers
    let results: Vec<(i32, usize)> = years_v
        .par_iter()
        .zip(tables_by_year.par_iter())
        .map(|(&yr, table_indices)| {
            let empty: Vec<usize> = Vec::new();
            let rows_for_year: &Vec<usize> = year_rows.get(&yr).unwrap_or(&empty);
            let n_yr = rows_for_year.len();

            let fy_start_day = days_from_civil(yr - 1, 7, 1);
            let fy_end_day = days_from_civil(yr, 6, 30);

            // --- Switcher expansion ---
            // Switch month is per-person (shared with expand_wage_ledger) so the
            // employer gross split is identical to what STP/PIT reconstruct.
            let cap = n_yr + (n_yr / 20).max(1);
            let mut ex_idx: Vec<usize> = Vec::with_capacity(cap);
            let mut ex_gross: Vec<f64> = Vec::with_capacity(cap);
            let mut ex_prior: Vec<Option<String>> = Vec::with_capacity(cap);
            let mut ex_start: Vec<i32> = Vec::with_capacity(cap);
            let mut ex_end: Vec<i32> = Vec::with_capacity(cap);
            let mut ex_terminated: Vec<bool> = Vec::with_capacity(cap);

            for &j in rows_for_year.iter() {
                let primary = panel.primary_job[j];
                let switched = panel.switched_employer[j];
                let gross = panel.gross_annual[j];

                if switched && primary {
                    let prior_pn = crate::business_pool::person_number(&panel.person_id[j]);
                    let switch_month =
                        crate::employment::ledger_switch_month(prior_pn, seed_i64, yr);
                    let old_share = switch_month as f64 / 12.0;
                    let new_share = 1.0 - old_share;
                    let split_day = month_start_after_july(yr - 1, switch_month);
                    let prior_slot = (panel.spell[j] as i64 - 1).max(0);
                    let prior_abn = make_employer_abn_prior(prior_pn, prior_slot, seed_i64);
                    // Old-employer row (prior ABN, old share): the part of the
                    // year up to the day before the new spell starts.
                    ex_idx.push(j);
                    ex_gross.push(round2(gross * old_share));
                    ex_prior.push(Some(prior_abn));
                    ex_start.push(fy_start_day);
                    ex_end.push(split_day - 1);
                    ex_terminated.push(true);
                    // New-employer row (current ABN, new share)
                    ex_idx.push(j);
                    ex_gross.push(round2(gross * new_share));
                    ex_prior.push(None);
                    ex_start.push(split_day);
                    ex_end.push(fy_end_day);
                    ex_terminated.push(false);
                } else {
                    ex_idx.push(j);
                    ex_gross.push(gross);
                    ex_prior.push(None);
                    ex_start.push(fy_start_day);
                    ex_end.push(fy_end_day);
                    ex_terminated.push(false);
                }
            }

            let n_rows = ex_idx.len();

            // --- Sort by SYNTHETIC_AEUID via permutation over the shared panel ---
            let mut order: Vec<usize> = (0..n_rows).collect();
            order.sort_by(|&a, &b| panel.aeuid_ato[ex_idx[a]].cmp(&panel.aeuid_ato[ex_idx[b]]));

            // --- Build the year's records in sorted order, materialising
            //     Strings exactly once per row. Every table of this year is a
            //     view of these records, so a person's pay, dates and codes
            //     are the same value in each. ---
            let mut rng_ps =
                StdRng::seed_from_u64((seed as u64).wrapping_add(7919u64.wrapping_mul(yr as u64)));

            let mut rec = YearRecords {
                aeuid: Vec::with_capacity(n_rows),
                bn: Vec::with_capacity(n_rows),
                person: Vec::with_capacity(n_rows),
                gross: Vec::with_capacity(n_rows),
                tax: Vec::with_capacity(n_rows),
                fbt: Vec::with_capacity(n_rows),
                employer_super: Vec::with_capacity(n_rows),
                allowances: Vec::with_capacity(n_rows),
                lump_a: Vec::with_capacity(n_rows),
                lump_b: Vec::with_capacity(n_rows),
                lump_d: Vec::with_capacity(n_rows),
                lump_e: Vec::with_capacity(n_rows),
                tax_free: Vec::with_capacity(n_rows),
                taxable: Vec::with_capacity(n_rows),
                exempt_foreign: Vec::with_capacity(n_rows),
                ps_type: Vec::with_capacity(n_rows),
                inb_type: Vec::with_capacity(n_rows),
                lspa_type: Vec::with_capacity(n_rows),
                amended: Vec::with_capacity(n_rows),
                period_start: Vec::with_capacity(n_rows),
                period_end: Vec::with_capacity(n_rows),
                payment_date: Vec::with_capacity(n_rows),
                repair_result: Vec::with_capacity(n_rows),
                late: Vec::with_capacity(n_rows),
            };

            // NOTE: RNG values are drawn in the SORTED (output) order rather
            // than panel order. The draws are iid, so this changes no
            // distributional property, and it lets one pass fill every column.
            let sg_r = sg_rate_for_year(yr);
            for &k in &order {
                let panel_idx = ex_idx[k];
                let gross = round2(ex_gross[k]);
                let person = panel.person_idx[panel_idx].saturating_sub(1);
                let age = birth_year
                    .get(person)
                    .map(|&by| yr - by)
                    .unwrap_or(40);

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
                // Which PAYG payment summary form this record is. A person
                // leaving an employer is who gets an employment termination
                // payment summary, so the type follows the termination.
                let u_type: f64 = rng_ps.gen();
                let etp_rate = if ex_terminated[k] {
                    ETP_RATE_ON_TERMINATION
                } else {
                    ETP_RATE_OTHERWISE
                };
                let ps_type = if u_type < etp_rate {
                    PS_TYPE_ETP
                } else if age >= SIS_MIN_AGE && u_type < etp_rate + SIS_RATE {
                    PS_TYPE_SIS
                } else if u_type < etp_rate + SIS_RATE + BPSI_RATE {
                    PS_TYPE_BPSI
                } else if u_type < etp_rate + SIS_RATE + BPSI_RATE + FEI_RATE {
                    PS_TYPE_FEI
                } else {
                    PS_TYPE_INB
                };

                // The tax-free and taxable components sum to the payment on
                // the summary, so they are carved out of the gross rather
                // than drawn beside it. A salary summary is fully taxable.
                let tax_free = if matches!(ps_type, PS_TYPE_ETP | PS_TYPE_SIS) {
                    round2(gross * rng_ps.gen::<f64>() * TAX_FREE_SHARE_MAX)
                } else {
                    0.0
                };
                let taxable = round2(gross - tax_free);

                // Foreign employment income is exempted from the gross under
                // the foreign service rules and reported beside it, so only
                // the foreign employment summary carries an amount.
                let exempt_foreign = if ps_type == PS_TYPE_FEI {
                    round2(gross * (0.3 + rng_ps.gen::<f64>() * 0.7))
                } else {
                    0.0
                };

                let inb_type = if ps_type == PS_TYPE_INB {
                    let u: f64 = rng_ps.gen();
                    if u < INB_WHM_RATE {
                        2
                    } else if u < INB_WHM_RATE + INB_CLOSELY_HELD_RATE {
                        3
                    } else {
                        1
                    }
                } else {
                    0
                };

                // Lump sum A carries a type wherever there is a lump sum A
                // and nothing where there is not. R is a genuine redundancy,
                // invalidity or early retirement payment and T is any other
                // termination; redundancy is the commoner reason a lump sum A
                // is paid at all.
                let lspa_type = if lump_a > 0.0 {
                    if rng_ps.gen::<f64>() < LSPA_OTHER_TERMINATION_RATE {
                        2
                    } else {
                        1
                    }
                } else {
                    0
                };

                // A discrete payment carries a payment date; a summary
                // reporting a whole year of pay reports a period instead.
                let payment_date = if matches!(ps_type, PS_TYPE_ETP | PS_TYPE_SIS) {
                    ex_end[k] + rng_ps.gen_range(0..=14)
                } else {
                    i32::MIN
                };

                let amended = rng_ps.gen::<f64>() < AMENDMENT_RATE;
                let late = rng_ps.gen::<f64>() < LATE_LODGEMENT_RATE;
                let u_repair: f64 = rng_ps.gen();
                let repair_result = if u_repair < REPAIR_NONE_RATE {
                    0
                } else if u_repair < REPAIR_NONE_RATE + REPAIR_MADE_RATE {
                    1
                } else {
                    2
                };

                rec.aeuid.push(panel.aeuid_ato[panel_idx].clone());
                rec.bn.push(match ex_prior[k].take() {
                    Some(prior) => as_business_number(&prior),
                    None => as_business_number(&panel.employer_id[panel_idx]),
                });
                rec.person.push(person);
                rec.gross.push(gross);
                rec.tax.push(compute_payg_tax(gross, yr));
                rec.fbt.push(fbt);
                rec.employer_super.push(round2(gross * sg_r));
                rec.allowances.push(allowances);
                rec.lump_a.push(lump_a);
                rec.lump_b.push(lump_b);
                rec.lump_d.push(lump_d);
                rec.lump_e.push(lump_e);
                rec.tax_free.push(tax_free);
                rec.taxable.push(taxable);
                rec.exempt_foreign.push(exempt_foreign);
                rec.ps_type.push(ps_type);
                rec.inb_type.push(inb_type);
                rec.lspa_type.push(lspa_type);
                rec.amended.push(amended);
                rec.period_start.push(ex_start[k]);
                rec.period_end.push(ex_end[k]);
                rec.payment_date.push(payment_date);
                rec.repair_result.push(repair_result);
                rec.late.push(late);
            }

            let n_ps = rec.aeuid.len();

            // --- One parquet per table the registry declares for this year ---
            let extract_rows: std::collections::HashMap<i32, Vec<usize>> = table_indices
                .iter()
                .map(|&t| tbl_months[t])
                .collect::<std::collections::HashSet<i32>>()
                .into_iter()
                .map(|months| (months, rows_for_table(&rec, months)))
                .collect();

            for &t in table_indices.iter() {
                // The registry decides which of `BN` and `ABN_HASH_TRUNC`
                // names the employer, and `build_column` writes whichever it
                // declares. A plan naming a key the table does not declare
                // stops here rather than writing a table with no employer on
                // it.
                assert!(
                    plan_key[t].is_empty() || plan_vars[t].iter().any(|v| v == &plan_key[t]),
                    "PIT_PS table {} keys on {} but does not declare it",
                    plan_table[t],
                    plan_key[t]
                );
                let rows = &extract_rows[&tbl_months[t]];
                // A geography table names the person's address once, not once
                // per payment summary.
                let table_rows: Vec<usize> = if tbl_geo[t] == 1 {
                    rows_one_per_person(&rec, rows)
                } else {
                    rows.clone()
                };
                let cols: Vec<NamedCol> = plan_vars[t]
                    .iter()
                    .map(|name| build_column(name, &rec, &people, &table_rows, yr))
                    .collect();
                let path =
                    Path::new(&out_dir_str).join(format!("{}.parquet", plan_stem[t]));
                write_columns_to_parquet(path.to_str().unwrap(), cols).unwrap_or_else(|e| {
                    panic!("PIT_PS parquet write ({}): {}", plan_table[t], e)
                });
            }

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

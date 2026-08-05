// PIT_ITR sub-table construction in Rust.
//
// Ports the hot loop previously in R's .build_itr_tables(). Takes the
// filer vectors (one row per person-year lodged return), a spine
// lookup, and an optional time-varying occupation panel, and emits
// the four PLIDA sub-tables as column vectors:
//   - context     (SYNTHETIC_AEUID, INCM_YR, ANZSCO 4d/6d, industry, residence)
//   - inc_loss    (gross, business, interest, taxable income)
//   - ded_exp_off (work/car/cloth/travel/other deductions + gifts)
//   - whld_debt   (gross tax, LITO, medicare, net tax, balance)
//
// All random draws use a single StdRng seeded from `seed + 601`. The
// bit-level output does NOT match the R version (R uses scattered
// runif/rnorm calls without a consistent seed stream), but statistical
// properties are preserved and each slice worker is deterministic in
// its own (seed + slice_id * 100000) stream.
//
// The R wrapper does aggregation of PS → filers, filing probability,
// and disk I/O. This module just does the per-filer construction and
// tax math.

use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;

// Archetype median for work deductions (matches the R vector).
const ARCHETYPE_MEDIAN: [f64; 8] = [
    1500.0, 2500.0, 2000.0, 2500.0, 4000.0, 3500.0, 3000.0, 1800.0,
];

/// Compute PAYG tax withholding (vectorised).
#[inline]
pub(crate) fn compute_payg_tax(inc: f64) -> f64 {
    let inc = inc.max(0.0);
    let b2 = (inc - 18200.0).clamp(0.0, 45000.0 - 18200.0);
    let b3 = (inc - 45000.0).clamp(0.0, 120000.0 - 45000.0);
    let b4 = (inc - 120000.0).clamp(0.0, 180000.0 - 120000.0);
    let b5 = (inc - 180000.0).max(0.0);
    round2(b2 * 0.19 + b3 * 0.325 + b4 * 0.37 + b5 * 0.45)
}

#[inline]
pub(crate) fn compute_lito(ti: f64) -> f64 {
    let ti = ti.max(0.0);
    let lito = if ti <= 37500.0 {
        700.0
    } else {
        (700.0 - (ti - 37500.0) * 0.05).max(0.0)
    };
    round2(lito)
}

#[inline]
pub(crate) fn compute_medicare_levy(ti: f64) -> f64 {
    let ti = ti.max(0.0);
    let ml = if ti > 29207.0 {
        ti * 0.02
    } else if ti > 23365.0 {
        (ti - 23365.0) * 0.10
    } else {
        0.0
    };
    round2(ml)
}

#[inline]
pub(crate) fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

/// Owned column output of `build_itr_columns_core`.
pub struct ItrColumns {
    pub aeuid: Vec<String>,
    pub fy: Vec<i32>,
    pub anzsco_4d: Vec<String>,
    pub anzsco_6d: Vec<String>,
    pub industry_code: Vec<String>,
    pub clnt_res: Vec<String>,
    pub grs_pmt: Vec<f64>,
    pub bus_income: Vec<f64>,
    pub intst_income: Vec<f64>,
    pub taxable_income: Vec<f64>,
    pub total_work_ded: Vec<f64>,
    pub car_ded: Vec<f64>,
    pub cloth_ded: Vec<f64>,
    pub travel_ded: Vec<f64>,
    pub other_w_ded: Vec<f64>,
    pub gifts: Vec<f64>,
    pub other_ded: Vec<f64>,
    pub gross_tax: Vec<f64>,
    pub lito: Vec<f64>,
    pub medicare: Vec<f64>,
    pub net_tax: Vec<f64>,
    pub balance: Vec<f64>,
}

/// Pure-Rust core ITR table builder. Takes pre-built spine and
/// occupation panel lookups as arguments so multiple year-workers
/// can share them without rebuilding.
#[allow(clippy::too_many_arguments)]
pub fn build_itr_columns_core(
    filers_aeuid: Vec<String>,
    filers_fy: Vec<i32>,
    filers_total_gross: Vec<f64>,
    filers_total_tax: Vec<f64>,
    spine_anzsco: &[i32],
    spine_industry: &[i32],
    spine_archetype: &[i32],
    _spine_birth_yr: &[i32],
    spine_idx: &HashMap<String, usize>,
    occ_lookup: &HashMap<(String, i32), i32>,
    has_occ_panel: bool,
    rng: &mut StdRng,
) -> ItrColumns {
    use crate::sampling::normal_sample;

    let nf = filers_aeuid.len();
    let mut out = ItrColumns {
        aeuid: Vec::with_capacity(nf),
        fy: Vec::with_capacity(nf),
        anzsco_4d: Vec::with_capacity(nf),
        anzsco_6d: Vec::with_capacity(nf),
        industry_code: Vec::with_capacity(nf),
        clnt_res: Vec::with_capacity(nf),
        grs_pmt: Vec::with_capacity(nf),
        bus_income: Vec::with_capacity(nf),
        intst_income: Vec::with_capacity(nf),
        taxable_income: Vec::with_capacity(nf),
        total_work_ded: Vec::with_capacity(nf),
        car_ded: Vec::with_capacity(nf),
        cloth_ded: Vec::with_capacity(nf),
        travel_ded: Vec::with_capacity(nf),
        other_w_ded: Vec::with_capacity(nf),
        gifts: Vec::with_capacity(nf),
        other_ded: Vec::with_capacity(nf),
        gross_tax: Vec::with_capacity(nf),
        lito: Vec::with_capacity(nf),
        medicare: Vec::with_capacity(nf),
        net_tax: Vec::with_capacity(nf),
        balance: Vec::with_capacity(nf),
    };

    for i in 0..nf {
        let aeuid_s = &filers_aeuid[i];
        let fy = filers_fy[i];
        let gross = filers_total_gross[i];
        let tax_withheld = filers_total_tax[i];

        let sidx = spine_idx.get(aeuid_s).copied();

        let anzsco_int: Option<i32> = if has_occ_panel {
            if let Some(&code) = occ_lookup.get(&(aeuid_s.clone(), fy)) {
                Some(code)
            } else {
                sidx.map(|idx| spine_anzsco[idx])
            }
        } else {
            sidx.map(|idx| spine_anzsco[idx])
        };
        let anzsco_6d_s = match anzsco_int {
            Some(c) => format!("{:06}", c),
            None => "000000".to_string(),
        };
        let anzsco_4d_s = anzsco_6d_s[..4].to_string();

        let industry_s = sidx
            .map(|idx| format!("{:02}", spine_industry[idx]))
            .unwrap_or_else(|| "00".to_string());
        let archetype_i = sidx.map(|idx| spine_archetype[idx]).unwrap_or(0);

        let u_disc: f64 = rng.gen();
        let disc_factor = if u_disc < 0.85 {
            1.0
        } else if u_disc < 0.97 {
            0.97 + rng.gen::<f64>() * 0.06
        } else {
            0.90 + rng.gen::<f64>() * 0.20
        };
        let salary_wages = round2(gross * disc_factor);

        let has_bus = rng.gen::<f64>() < 0.18;
        let bus_i = if has_bus {
            round2(normal_sample(rng, 9.6, 1.2).exp())
        } else {
            0.0
        };

        let has_intst = rng.gen::<f64>() < 0.35;
        let intst_i = if has_intst {
            round2(normal_sample(rng, 7.6, 1.5).exp())
        } else {
            0.0
        };

        let has_wk_ded = rng.gen::<f64>() < 0.75;
        let arch_idx = (archetype_i as usize).min(7);
        let total_wk_ded = if has_wk_ded {
            let med = ARCHETYPE_MEDIAN[arch_idx];
            round2(normal_sample(rng, med.ln(), 0.80).exp())
        } else {
            0.0
        };

        let (mut car, mut cloth, mut travel, mut other_w) = (0.0, 0.0, 0.0, 0.0);
        if has_wk_ded && total_wk_ded > 0.0 {
            if rng.gen::<f64>() < 0.25 {
                car = round2(total_wk_ded * (0.20 + rng.gen::<f64>() * 0.40));
            }
            if rng.gen::<f64>() < 0.30 {
                cloth = round2(total_wk_ded * (0.05 + rng.gen::<f64>() * 0.15));
            }
            if rng.gen::<f64>() < 0.10 {
                travel = round2(total_wk_ded * (0.10 + rng.gen::<f64>() * 0.30));
            }
            let sub = car + cloth + travel;
            other_w = round2((total_wk_ded - sub).max(0.0));
        }

        let gifts = if rng.gen::<f64>() < 0.30 {
            round2(normal_sample(rng, 5.5, 1.2).exp())
        } else {
            0.0
        };
        let other_d = if rng.gen::<f64>() < 0.20 {
            round2(normal_sample(rng, 6.2, 1.0).exp())
        } else {
            0.0
        };

        let total_deductions = total_wk_ded + gifts + other_d;
        let total_income = salary_wages + bus_i + intst_i;
        let ti = ((total_income - total_deductions).max(-50000.0) * 100.0).round() / 100.0;

        let gross_tax = compute_payg_tax(ti.max(0.0));
        let lito = compute_lito(ti.max(0.0));
        let medicare = compute_medicare_levy(ti);
        let net_tax = round2((gross_tax - lito).max(0.0) + medicare);
        let balance = round2(net_tax - tax_withheld);

        out.aeuid.push(aeuid_s.clone());
        out.fy.push(fy);
        out.anzsco_4d.push(anzsco_4d_s);
        out.anzsco_6d.push(anzsco_6d_s);
        out.industry_code.push(industry_s);
        out.clnt_res.push("Y".to_string());
        out.grs_pmt.push(salary_wages);
        out.bus_income.push(bus_i);
        out.intst_income.push(intst_i);
        out.taxable_income.push(ti);
        out.total_work_ded.push(total_wk_ded);
        out.car_ded.push(car);
        out.cloth_ded.push(cloth);
        out.travel_ded.push(travel);
        out.other_w_ded.push(other_w);
        out.gifts.push(gifts);
        out.other_ded.push(other_d);
        out.gross_tax.push(gross_tax);
        out.lito.push(lito);
        out.medicare.push(medicare);
        out.net_tax.push(net_tax);
        out.balance.push(balance);
    }
    let _ = filers_total_gross;
    let _ = filers_total_tax;
    out
}

/// Build spine aeuid→idx hashmap (shared across year workers).
pub fn build_spine_idx(spine_aeuid: &[String]) -> HashMap<String, usize> {
    let mut m = HashMap::with_capacity(spine_aeuid.len());
    for (i, s) in spine_aeuid.iter().enumerate() {
        m.insert(s.clone(), i);
    }
    m
}

/// Build ITR sub-tables for a vector of filers.
///
/// Inputs (parallel vectors of length `n_filers`):
/// - `filers_aeuid`, `filers_fy`, `filers_total_gross`, `filers_total_tax`
///
/// Spine (parallel vectors of length `n_spine`):
/// - `spine_aeuid`, `spine_anzsco`, `spine_industry`, `spine_archetype`,
///   `spine_birth_yr`
///
/// Optional time-varying occupation panel (parallel vectors):
/// - `occ_panel_aeuid`, `occ_panel_year`, `occ_panel_anzsco`
///   (pass empty vectors to disable the time-varying lookup)
///
/// Returns a single named list with 4 sublists, one per sub-table.
/// Each sublist is a named list of columns suitable for
/// `data.frame()` or direct `arrow::arrow_table()` construction in R.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn build_itr_tables__(
    filers_aeuid: Strings,
    filers_fy: &[i32],
    filers_total_gross: &[f64],
    filers_total_tax: &[f64],
    spine_aeuid: Strings,
    spine_anzsco: &[i32],
    spine_industry: &[i32],
    spine_archetype: &[i32],
    spine_birth_yr: &[i32],
    occ_panel_aeuid: Strings,
    occ_panel_year: &[i32],
    occ_panel_anzsco: &[i32],
    seed: i64,
) -> List {
    let n_spine = spine_aeuid.len();
    let spine_aeuid_vec: Vec<String> = spine_aeuid
        .iter()
        .map(|s| {
            if s.is_na() {
                String::new()
            } else {
                s.to_string()
            }
        })
        .collect();
    let mut spine_idx = HashMap::with_capacity(n_spine);
    for (i, s) in spine_aeuid_vec.iter().enumerate() {
        if !s.is_empty() {
            spine_idx.insert(s.clone(), i);
        }
    }
    let has_occ_panel = occ_panel_aeuid.len() > 0;
    let mut occ_lookup: HashMap<(String, i32), i32> = HashMap::new();
    if has_occ_panel {
        occ_lookup.reserve(occ_panel_aeuid.len());
        for (i, s) in occ_panel_aeuid.iter().enumerate() {
            if !s.is_na() {
                occ_lookup.insert((s.to_string(), occ_panel_year[i]), occ_panel_anzsco[i]);
            }
        }
    }
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(601));

    let filers_aeuid_v: Vec<String> = filers_aeuid.iter().map(|s| s.to_string()).collect();
    let filers_fy_v: Vec<i32> = filers_fy.to_vec();
    let filers_gross_v: Vec<f64> = filers_total_gross.to_vec();
    let filers_tax_v: Vec<f64> = filers_total_tax.to_vec();

    let out = build_itr_columns_core(
        filers_aeuid_v,
        filers_fy_v,
        filers_gross_v,
        filers_tax_v,
        spine_anzsco,
        spine_industry,
        spine_archetype,
        spine_birth_yr,
        &spine_idx,
        &occ_lookup,
        has_occ_panel,
        &mut rng,
    );

    list!(
        SYNTHETIC_AEUID = out.aeuid,
        INCM_YR = out.fy,
        OCPTN_GRP_CD = out.anzsco_4d,
        SUB_OCPTN_GRP_CD = out.anzsco_6d,
        MN_BUS_TAX_OFC_ANZSIC_CD = out.industry_code,
        CLNT_RSDNT_IND = out.clnt_res,
        GRS_PMT_TOTL_CALCD_AMT = out.grs_pmt,
        BUS_INCM_TOTL_AMT = out.bus_income,
        GRS_INTST_AMT = out.intst_income,
        TI_OR_LSS_AMT = out.taxable_income,
        DECLD_TOTL_WRK_DEDN_AMT = out.total_work_ded,
        WRK_RLTD_CAR_EXPNSS_AMT = out.car_ded,
        WRK_RLTD_CLTHG_EXPNSS_AMT = out.cloth_ded,
        WRK_RLTD_TRVL_EXPNSS_AMT = out.travel_ded,
        WRK_RLTD_OTHR_EXPNSS_AMT = out.other_w_ded,
        GIFTS_OR_DNTNS_AMT = out.gifts,
        OTHR_DDCTNS_TOTL_AMT = out.other_ded,
        GRS_TAX_AMT = out.gross_tax,
        LOW_INCM_ERNR_TAX_OFST_AMT = out.lito,
        BSC_ML_CALCD_AMT = out.medicare,
        NET_TAX_AMT = out.net_tax,
        BAL_PYBLE_RFNDBL_AMT = out.balance
    )
}

// =======================================================================
// Full PIT_ITR pipeline: parallel across years, reading PS parquet files,
// aggregating, filing-probability filter, building ITR sub-tables, and
// writing 4 sub-table parquets per year.
// =======================================================================

use crate::parquet_io::write_named_arrays_to_parquet;
use arrow_array::{ArrayRef, Float64Array, Int32Array, StringArray};
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
use rayon::prelude::*;
use std::path::Path;
use std::sync::Arc;

fn filing_probability(income: f64, tax: f64) -> f64 {
    if income > 18200.0 && income <= 45000.0 {
        0.92
    } else if income <= 18200.0 && tax > 0.0 {
        0.85
    } else if income <= 18200.0 && tax <= 0.0 {
        0.25
    } else {
        0.98
    }
}

/// Read one PS parquet year file and aggregate by AEUID.
fn load_and_aggregate_ps(path: &str) -> HashMap<String, (f64, f64)> {
    let file =
        std::fs::File::open(path).unwrap_or_else(|e| panic!("open PS parquet {}: {}", path, e));
    let builder = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap_or_else(|e| panic!("parquet reader {}: {}", path, e));
    let reader = builder
        .with_batch_size(65_536)
        .build()
        .unwrap_or_else(|e| panic!("parquet build {}: {}", path, e));

    let mut agg: HashMap<String, (f64, f64)> = HashMap::new();
    for batch_res in reader {
        let batch = batch_res.expect("read batch");
        let schema = batch.schema();
        let aeuid_idx = schema
            .index_of("SYNTHETIC_AEUID")
            .expect("SYNTHETIC_AEUID col");
        let gross_idx = schema
            .index_of("GROSS_PAYMENTS")
            .expect("GROSS_PAYMENTS col");
        let tax_idx = schema.index_of("TAX_WITHHELD").expect("TAX_WITHHELD col");
        let aeuid_arr = batch
            .column(aeuid_idx)
            .as_any()
            .downcast_ref::<arrow_array::StringArray>()
            .expect("aeuid string array");
        let gross_arr = batch
            .column(gross_idx)
            .as_any()
            .downcast_ref::<arrow_array::Float64Array>()
            .expect("gross f64 array");
        let tax_arr = batch
            .column(tax_idx)
            .as_any()
            .downcast_ref::<arrow_array::Float64Array>()
            .expect("tax f64 array");
        for i in 0..batch.num_rows() {
            let key = aeuid_arr.value(i).to_string();
            let g = gross_arr.value(i);
            let t = tax_arr.value(i);
            let e = agg.entry(key).or_insert((0.0, 0.0));
            e.0 += g;
            e.1 += t;
        }
    }
    agg
}

/// Load a parquet file containing the occupation panel and extend the
/// caller-supplied HashMap.
fn load_occ_panel_file(path: &str, out: &mut HashMap<(String, i32), i32>) {
    let file =
        std::fs::File::open(path).unwrap_or_else(|e| panic!("open occ panel {}: {}", path, e));
    let builder = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap_or_else(|e| panic!("parquet reader occ {}: {}", path, e));
    let reader = builder
        .with_batch_size(65_536)
        .build()
        .unwrap_or_else(|e| panic!("occ reader build {}: {}", path, e));
    for batch_res in reader {
        let batch = batch_res.expect("read occ batch");
        let schema = batch.schema();
        let a_idx = schema.index_of("aeuid_ato").expect("aeuid_ato");
        let y_idx = schema.index_of("year").expect("year");
        let c_idx = schema.index_of("anzsco_code").expect("anzsco_code");
        let a = batch
            .column(a_idx)
            .as_any()
            .downcast_ref::<arrow_array::StringArray>()
            .expect("aeuid str");
        let y = batch
            .column(y_idx)
            .as_any()
            .downcast_ref::<arrow_array::Int32Array>()
            .expect("year i32");
        let c = batch
            .column(c_idx)
            .as_any()
            .downcast_ref::<arrow_array::Int32Array>()
            .expect("anzsco i32");
        for i in 0..batch.num_rows() {
            out.insert((a.value(i).to_string(), y.value(i)), c.value(i));
        }
    }
}

/// Full PIT_ITR pipeline.
///
/// Reads PS parquet files (one per year), aggregates, applies filing
/// probability, builds ITR sub-tables (parallel across years), and writes
/// 4 parquet sub-tables per year to `out_dir`.
///
/// `ps_file_paths[i]` ↔ `ps_years[i]` — PS files matched to their FY.
/// `occ_panel_paths` are parquet files for the occupation panel.
/// `years` is the requested FY list. `product_name_by_yr_type` is
/// flattened: length 4 * n_years, order is per-year × (context, inc-loss,
/// ded-exp-off, whld-debt).
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
pub fn generate_pit_itr_full_to_parquet__(
    spine_aeuid: Strings,
    spine_anzsco: &[i32],
    spine_industry: &[i32],
    spine_archetype: &[i32],
    spine_birth_yr: &[i32],
    ps_file_paths: Strings,
    ps_years: &[i32],
    occ_panel_paths: Strings,
    years: &[i32],
    product_name_by_yr_type: Strings,
    out_dir: &str,
    seed: i32,
) -> List {
    std::fs::create_dir_all(out_dir).ok();

    // 1. Build spine hashmap (shared).
    let spine_aeuid_v: Vec<String> = spine_aeuid
        .iter()
        .map(|s| {
            if s.is_na() {
                String::new()
            } else {
                s.to_string()
            }
        })
        .collect();
    let spine_idx_map = build_spine_idx(&spine_aeuid_v);
    let spine_idx = Arc::new(spine_idx_map);

    // 2. Load occupation panel from parquet files once.
    let mut occ_lookup: HashMap<(String, i32), i32> = HashMap::new();
    for p in occ_panel_paths.iter() {
        if p.is_na() {
            continue;
        }
        let path = p.to_string();
        if !path.is_empty() && Path::new(&path).exists() {
            load_occ_panel_file(&path, &mut occ_lookup);
        }
    }
    let has_occ_panel = !occ_lookup.is_empty();
    let occ_lookup = Arc::new(occ_lookup);

    // 3. Map year → PS file path.
    let mut ps_path_by_year: HashMap<i32, String> = HashMap::new();
    for (i, p) in ps_file_paths.iter().enumerate() {
        ps_path_by_year.insert(ps_years[i], p.to_string());
    }

    // 4. Convert product name list into year-indexed groups of 4.
    let pnames: Vec<String> = product_name_by_yr_type
        .iter()
        .map(|s| s.to_string())
        .collect();
    let years_v: Vec<i32> = years.to_vec();
    assert_eq!(
        pnames.len(),
        4 * years_v.len(),
        "product_name_by_yr_type length must be 4 * n_years"
    );

    let spine_anzsco_vec: Vec<i32> = spine_anzsco.to_vec();
    let spine_industry_vec: Vec<i32> = spine_industry.to_vec();
    let spine_archetype_vec: Vec<i32> = spine_archetype.to_vec();
    let spine_birth_vec: Vec<i32> = spine_birth_yr.to_vec();
    let sa_arc = Arc::new(spine_anzsco_vec);
    let si_arc = Arc::new(spine_industry_vec);
    let sarch_arc = Arc::new(spine_archetype_vec);
    let sby_arc = Arc::new(spine_birth_vec);
    let out_dir_s = out_dir.to_string();
    let ps_path_by_year = Arc::new(ps_path_by_year);

    // 5. Parallel per-year workers.
    let results: Vec<(i32, usize)> = (0..years_v.len())
        .collect::<Vec<_>>()
        .par_iter()
        .map(|&yi| {
            let yr = years_v[yi];
            let ps_path = match ps_path_by_year.get(&yr) {
                Some(p) => p.clone(),
                None => return (yr, 0usize),
            };
            if !Path::new(&ps_path).exists() {
                return (yr, 0);
            }

            // a. Aggregate PS rows.
            let agg = load_and_aggregate_ps(&ps_path);

            // b. Filing probability filter.
            let mut rng =
                StdRng::seed_from_u64((seed as u64).wrapping_add(600).wrapping_add(yr as u64));
            let mut filers_aeuid: Vec<String> = Vec::with_capacity(agg.len());
            let mut filers_fy: Vec<i32> = Vec::with_capacity(agg.len());
            let mut filers_gross: Vec<f64> = Vec::with_capacity(agg.len());
            let mut filers_tax: Vec<f64> = Vec::with_capacity(agg.len());
            // Deterministic iteration: sort by key.
            let mut keys: Vec<&String> = agg.keys().collect();
            keys.sort();
            for k in keys {
                let (g, t) = agg[k];
                let p = filing_probability(g, t);
                if rng.gen::<f64>() < p {
                    filers_aeuid.push(k.clone());
                    filers_fy.push(yr);
                    filers_gross.push(g);
                    filers_tax.push(t);
                }
            }

            if filers_aeuid.is_empty() {
                return (yr, 0);
            }

            // c. Build ITR columns.
            let mut rng_itr = StdRng::seed_from_u64((seed as u64).wrapping_add(601));
            let n_filers = filers_aeuid.len();
            let out = build_itr_columns_core(
                filers_aeuid,
                filers_fy,
                filers_gross,
                filers_tax,
                sa_arc.as_slice(),
                si_arc.as_slice(),
                sarch_arc.as_slice(),
                sby_arc.as_slice(),
                spine_idx.as_ref(),
                occ_lookup.as_ref(),
                has_occ_panel,
                &mut rng_itr,
            );

            // d. Sort order by AEUID (FY is constant).
            let mut order: Vec<usize> = (0..n_filers).collect();
            order.sort_by(|&a, &b| out.aeuid[a].cmp(&out.aeuid[b]));

            // e. Build shared ArrayRefs once. Each String is materialised
            //    exactly once into the StringArray's Arrow buffer; the
            //    resulting ArrayRef (Arc<dyn Array>) is cheaply cloned
            //    across the 4 sub-table writes.
            //
            //    This replaces the previous 4× permute + 4× StringArray
            //    construction pattern, which at 30m produced 4×22m =
            //    ~88M extra String clones per year worker and was the
            //    dominant allocator-contention bottleneck.
            let aeuid_arr: ArrayRef = {
                let sa =
                    StringArray::from_iter_values(order.iter().map(|&k| out.aeuid[k].as_str()));
                Arc::new(sa)
            };
            let fy_arr: ArrayRef = {
                let ia = Int32Array::from_iter_values(order.iter().map(|&k| out.fy[k]));
                Arc::new(ia)
            };

            // Helpers to build string / float arrays in sorted order from
            // owned source Vecs. These consume the source so each String
            // moves (no clone) into the Arrow buffer.
            let build_str_arr = |src: Vec<String>| -> ArrayRef {
                let sa = StringArray::from_iter_values(order.iter().map(|&k| src[k].as_str()));
                Arc::new(sa)
            };
            let build_f64_arr = |src: &[f64]| -> ArrayRef {
                Arc::new(Float64Array::from_iter_values(
                    order.iter().map(|&k| src[k]),
                ))
            };

            // Sub-table-unique columns — built once each.
            let anzsco_4d_arr = build_str_arr(out.anzsco_4d);
            let anzsco_6d_arr = build_str_arr(out.anzsco_6d);
            let industry_arr = build_str_arr(out.industry_code);
            let clnt_arr = build_str_arr(out.clnt_res);
            let grs_pmt_arr = build_f64_arr(&out.grs_pmt);
            let bus_arr = build_f64_arr(&out.bus_income);
            let intst_arr = build_f64_arr(&out.intst_income);
            let ti_arr = build_f64_arr(&out.taxable_income);
            let twd_arr = build_f64_arr(&out.total_work_ded);
            let car_arr = build_f64_arr(&out.car_ded);
            let cloth_arr = build_f64_arr(&out.cloth_ded);
            let travel_arr = build_f64_arr(&out.travel_ded);
            let other_w_arr = build_f64_arr(&out.other_w_ded);
            let gifts_arr = build_f64_arr(&out.gifts);
            let other_arr = build_f64_arr(&out.other_ded);
            let gross_tax_arr = build_f64_arr(&out.gross_tax);
            let lito_arr = build_f64_arr(&out.lito);
            let medicare_arr = build_f64_arr(&out.medicare);
            let net_tax_arr = build_f64_arr(&out.net_tax);
            let balance_arr = build_f64_arr(&out.balance);

            // f. Resolve 4 product names for this year.
            let p_context = pnames[yi * 4 + 0].clone();
            let p_inc_loss = pnames[yi * 4 + 1].clone();
            let p_ded_exp_off = pnames[yi * 4 + 2].clone();
            let p_whld_debt = pnames[yi * 4 + 3].clone();

            // g. Write 4 sub-table parquets. ArrayRef clones are refcount
            //    bumps, not data copies.
            let path_ctx = Path::new(&out_dir_s).join(format!("{}.parquet", p_context));
            write_named_arrays_to_parquet(
                path_ctx.to_str().unwrap(),
                vec![
                    ("SYNTHETIC_AEUID", aeuid_arr.clone()),
                    ("INCM_YR", fy_arr.clone()),
                    ("OCPTN_GRP_CD", anzsco_4d_arr),
                    ("SUB_OCPTN_GRP_CD", anzsco_6d_arr),
                    ("MN_BUS_TAX_OFC_ANZSIC_CD", industry_arr),
                    ("CLNT_RSDNT_IND", clnt_arr),
                ],
            )
            .unwrap_or_else(|e| panic!("ITR context write ({}): {}", yr, e));

            let path_il = Path::new(&out_dir_s).join(format!("{}.parquet", p_inc_loss));
            write_named_arrays_to_parquet(
                path_il.to_str().unwrap(),
                vec![
                    ("SYNTHETIC_AEUID", aeuid_arr.clone()),
                    ("INCM_YR", fy_arr.clone()),
                    ("GRS_PMT_TOTL_CALCD_AMT", grs_pmt_arr),
                    ("BUS_INCM_TOTL_AMT", bus_arr),
                    ("GRS_INTST_AMT", intst_arr),
                    ("TI_OR_LSS_AMT", ti_arr),
                ],
            )
            .unwrap_or_else(|e| panic!("ITR inc_loss write ({}): {}", yr, e));

            let path_de = Path::new(&out_dir_s).join(format!("{}.parquet", p_ded_exp_off));
            write_named_arrays_to_parquet(
                path_de.to_str().unwrap(),
                vec![
                    ("SYNTHETIC_AEUID", aeuid_arr.clone()),
                    ("INCM_YR", fy_arr.clone()),
                    ("DECLD_TOTL_WRK_DEDN_AMT", twd_arr),
                    ("WRK_RLTD_CAR_EXPNSS_AMT", car_arr),
                    ("WRK_RLTD_CLTHG_EXPNSS_AMT", cloth_arr),
                    ("WRK_RLTD_TRVL_EXPNSS_AMT", travel_arr),
                    ("WRK_RLTD_OTHR_EXPNSS_AMT", other_w_arr),
                    ("GIFTS_OR_DNTNS_AMT", gifts_arr),
                    ("OTHR_DDCTNS_TOTL_AMT", other_arr),
                ],
            )
            .unwrap_or_else(|e| panic!("ITR ded_exp_off write ({}): {}", yr, e));

            let path_wd = Path::new(&out_dir_s).join(format!("{}.parquet", p_whld_debt));
            write_named_arrays_to_parquet(
                path_wd.to_str().unwrap(),
                vec![
                    ("SYNTHETIC_AEUID", aeuid_arr),
                    ("INCM_YR", fy_arr),
                    ("GRS_TAX_AMT", gross_tax_arr),
                    ("LOW_INCM_ERNR_TAX_OFST_AMT", lito_arr),
                    ("BSC_ML_CALCD_AMT", medicare_arr),
                    ("NET_TAX_AMT", net_tax_arr),
                    ("BAL_PYBLE_RFNDBL_AMT", balance_arr),
                ],
            )
            .unwrap_or_else(|e| panic!("ITR whld_debt write ({}): {}", yr, e));

            (yr, n_filers)
        })
        .collect();

    let years_out: Vec<i32> = results.iter().map(|(y, _)| *y).collect();
    let counts_out: Vec<i32> = results.iter().map(|(_, n)| *n as i32).collect();
    list!(year = years_out, n_filers = counts_out)
}

extendr_module! {
    mod pit_itr_build;
    fn build_itr_tables__;
    fn generate_pit_itr_full_to_parquet__;
}

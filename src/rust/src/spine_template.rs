// Spine template: build once, sample many.
//
// At large N, generating a fresh spine costs both CPU (per-person
// Rust logic × N) and I/O (writing a 41-column parquet × N rows).
// At 25M the total can be 12–35 min depending on the R→Arrow path.
//
// The spine is deterministic per seed and its distributions are
// stable — so generating all 25M unique persons from scratch buys
// us only marginal extra diversity beyond a well-sized template.
//
// This module implements a two-stage approach:
//
//   1. Build a ~100k-row "spine template" parquet once per seed.
//      Cached on disk at a user-provided path. Stores all 36 base
//      spine columns (no agency AEUIDs).
//
//   2. On every `generate_spine()` call, read the template (small,
//      fast), sample N indices with replacement, take template rows
//      positionally into output arrays, overwrite the per-row
//      unique columns (`id`, `spine_id`) with fresh sequential
//      values, generate the per-agency AEUID columns in Rust,
//      and write the output parquet directly.
//
// The whole pipeline stays in Rust from read to write, bypassing
// the R arrow::arrow_table conversion that is O(big) and the main
// R-side bottleneck at 25M. At 25M the sample+write should be on
// the order of 15–60 seconds.
//
// Trade-off: exactly 100k distinct joint-attribute combinations
// appear in the output, each appearing ~N/100k times. This is
// irrelevant for the DiD analyses the spine exists to support.

use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::fs::File;
use std::sync::Arc;

// extendr_api::prelude shadows std's Result with a 1-arg alias.
// Use a local alias for 2-arg results.
type StdResult<T, E> = std::result::Result<T, E>;

use arrow_array::{
    Array, ArrayRef, BooleanArray, Float64Array, Int32Array, RecordBatch, StringArray,
};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
use parquet::arrow::arrow_writer::ArrowWriter;
use parquet::basic::Compression;
use parquet::file::properties::WriterProperties;

use crate::spine::anzsco_table::{ANZSCO_CODES, ANZSCO_TITLES};
use crate::spine::{assign_household_ids, build_persons, Person};

// -- Person -> Arrow arrays ---------------------------------------------------

/// Convert a Vec<Person> to a list of (name, ArrayRef) pairs in the
/// canonical spine column order. This mirrors `spine::to_r_list` but
/// produces Arrow arrays directly instead of R SEXP column vectors.
fn persons_to_arrays(persons: &[Person]) -> Vec<(String, ArrayRef)> {
    let n = persons.len();

    // Pre-allocate per-column Vec buffers.
    let mut id: Vec<String> = Vec::with_capacity(n);
    let mut spine_id: Vec<Option<String>> = Vec::with_capacity(n);

    let mut birth_year: Vec<i32> = Vec::with_capacity(n);
    let mut sex: Vec<i32> = Vec::with_capacity(n);
    let mut state: Vec<i32> = Vec::with_capacity(n);
    let mut indigenous: Vec<i32> = Vec::with_capacity(n);
    let mut country_of_birth: Vec<i32> = Vec::with_capacity(n);
    let mut country_of_birth_sacc: Vec<i32> = Vec::with_capacity(n);
    let mut year_of_arrival: Vec<Option<i32>> = Vec::with_capacity(n);
    let mut citizenship: Vec<i32> = Vec::with_capacity(n);
    let mut month_of_birth: Vec<i32> = Vec::with_capacity(n);
    let mut year_of_death: Vec<Option<i32>> = Vec::with_capacity(n);
    let mut month_of_death: Vec<Option<i32>> = Vec::with_capacity(n);
    let mut day_of_death: Vec<Option<i32>> = Vec::with_capacity(n);
    let mut residence_seed: Vec<i32> = Vec::with_capacity(n);
    let mut sa2_code: Vec<i32> = Vec::with_capacity(n);
    let mut sa3_code: Vec<i32> = Vec::with_capacity(n);
    let mut sa4_code: Vec<i32> = Vec::with_capacity(n);
    let mut education: Vec<i32> = Vec::with_capacity(n);
    let mut archetype: Vec<i32> = Vec::with_capacity(n);

    let mut anzsco_code: Vec<i32> = Vec::with_capacity(n);
    let mut anzsco_unit: Vec<i32> = Vec::with_capacity(n);
    let mut anzsco_major: Vec<i32> = Vec::with_capacity(n);
    let mut anzsco_title: Vec<Option<String>> = Vec::with_capacity(n);
    let mut skill_level: Vec<i32> = Vec::with_capacity(n);

    let mut task_cognitive: Vec<f64> = Vec::with_capacity(n);
    let mut task_physical: Vec<f64> = Vec::with_capacity(n);
    let mut task_vision: Vec<f64> = Vec::with_capacity(n);
    let mut task_hearing: Vec<f64> = Vec::with_capacity(n);
    let mut task_manual_dexterity: Vec<f64> = Vec::with_capacity(n);
    let mut task_communication: Vec<f64> = Vec::with_capacity(n);

    let mut industry: Vec<i32> = Vec::with_capacity(n);
    let mut sector: Vec<i32> = Vec::with_capacity(n);
    let mut individual_fe: Vec<f64> = Vec::with_capacity(n);
    let mut occ_match_quality: Vec<f64> = Vec::with_capacity(n);
    let mut baseline_employed: Vec<bool> = Vec::with_capacity(n);
    let mut baseline_hours: Vec<i32> = Vec::with_capacity(n);
    let mut baseline_income: Vec<f64> = Vec::with_capacity(n);

    let mut dis_onset_year: Vec<Option<i32>> = Vec::with_capacity(n);
    let mut dis_type: Vec<Option<i32>> = Vec::with_capacity(n);
    let mut dis_severity: Vec<Option<i32>> = Vec::with_capacity(n);
    let mut dis_is_dc: Vec<Option<bool>> = Vec::with_capacity(n);
    let mut dis_dose: Vec<Option<f64>> = Vec::with_capacity(n);
    let mut dis_person_type: Vec<Option<i32>> = Vec::with_capacity(n);
    let mut dis_comorbidity: Vec<Option<i32>> = Vec::with_capacity(n);

    for p in persons {
        id.push(format!("P{:010}", p.id + 1));
        spine_id.push(if p.has_spine_link {
            Some(format!("SP{:010}", p.id + 1))
        } else {
            None
        });
        birth_year.push(p.birth_year);
        sex.push(p.sex as i32);
        state.push(p.state as i32);
        indigenous.push(p.indigenous as i32);
        country_of_birth.push(p.country_of_birth as i32);
        country_of_birth_sacc.push(p.country_of_birth_sacc);
        year_of_arrival.push(p.year_of_arrival);
        citizenship.push(p.citizenship as i32);
        month_of_birth.push(p.month_of_birth);
        year_of_death.push(p.year_of_death);
        month_of_death.push(p.month_of_death);
        day_of_death.push(p.day_of_death);
        residence_seed.push(p.residence_seed);
        sa2_code.push(p.sa2 as i32);
        sa3_code.push((p.sa2 / 10_000) as i32);
        sa4_code.push((p.sa2 / 1_000_000) as i32);
        education.push(p.education as i32);
        archetype.push(p.archetype as i32);

        if p.education > 0 && (p.anzsco_index as usize) < ANZSCO_CODES.len() {
            let entry = &ANZSCO_CODES[p.anzsco_index as usize];
            anzsco_code.push(entry.code as i32);
            anzsco_unit.push(entry.unit_code as i32);
            anzsco_major.push(entry.major as i32);
            anzsco_title.push(Some(ANZSCO_TITLES[p.anzsco_index as usize].to_string()));
            skill_level.push(entry.skill_level as i32);
        } else {
            anzsco_code.push(0);
            anzsco_unit.push(0);
            anzsco_major.push(0);
            anzsco_title.push(None);
            skill_level.push(0);
        }

        task_cognitive.push(p.task_cognitive as f64);
        task_physical.push(p.task_physical as f64);
        task_vision.push(p.task_vision as f64);
        task_hearing.push(p.task_hearing as f64);
        task_manual_dexterity.push(p.task_manual_dexterity as f64);
        task_communication.push(p.task_communication as f64);

        industry.push(p.industry as i32);
        sector.push(p.sector as i32);
        individual_fe.push(p.individual_fe as f64);
        occ_match_quality.push(p.occ_match_quality as f64);
        baseline_employed.push(p.baseline_employed);
        baseline_hours.push(p.baseline_hours as i32);
        baseline_income.push(p.baseline_income as f64);

        dis_onset_year.push(p.disability_onset_year);
        dis_type.push(p.disability_type);
        dis_severity.push(p.disability_severity);
        dis_is_dc.push(p.is_dc);
        dis_dose.push(p.disability_dose);
        dis_person_type.push(p.person_type.map(|v| v as i32));
        dis_comorbidity.push(p.comorbidity_flags);
    }

    let nullable_strs = |v: Vec<Option<String>>| -> ArrayRef {
        let refs: Vec<Option<&str>> = v.iter().map(|s| s.as_deref()).collect();
        Arc::new(StringArray::from(refs))
    };

    vec![
        ("id".into(), Arc::new(StringArray::from(id)) as ArrayRef),
        ("spine_id".into(), nullable_strs(spine_id)),
        ("birth_year".into(), Arc::new(Int32Array::from(birth_year))),
        ("sex".into(), Arc::new(Int32Array::from(sex))),
        ("state".into(), Arc::new(Int32Array::from(state))),
        ("indigenous".into(), Arc::new(Int32Array::from(indigenous))),
        (
            "country_of_birth".into(),
            Arc::new(Int32Array::from(country_of_birth)),
        ),
        (
            "country_of_birth_sacc".into(),
            Arc::new(Int32Array::from(country_of_birth_sacc)),
        ),
        (
            "year_of_arrival".into(),
            Arc::new(Int32Array::from(year_of_arrival)),
        ),
        (
            "citizenship".into(),
            Arc::new(Int32Array::from(citizenship)),
        ),
        (
            "month_of_birth".into(),
            Arc::new(Int32Array::from(month_of_birth)),
        ),
        (
            "year_of_death".into(),
            Arc::new(Int32Array::from(year_of_death)),
        ),
        (
            "month_of_death".into(),
            Arc::new(Int32Array::from(month_of_death)),
        ),
        (
            "day_of_death".into(),
            Arc::new(Int32Array::from(day_of_death)),
        ),
        (
            "residence_seed".into(),
            Arc::new(Int32Array::from(residence_seed)),
        ),
        ("sa2_code".into(), Arc::new(Int32Array::from(sa2_code))),
        ("sa3_code".into(), Arc::new(Int32Array::from(sa3_code))),
        ("sa4_code".into(), Arc::new(Int32Array::from(sa4_code))),
        ("education".into(), Arc::new(Int32Array::from(education))),
        ("archetype".into(), Arc::new(Int32Array::from(archetype))),
        (
            "anzsco_code".into(),
            Arc::new(Int32Array::from(anzsco_code)),
        ),
        (
            "anzsco_unit".into(),
            Arc::new(Int32Array::from(anzsco_unit)),
        ),
        (
            "anzsco_major".into(),
            Arc::new(Int32Array::from(anzsco_major)),
        ),
        ("anzsco_title".into(), nullable_strs(anzsco_title)),
        (
            "skill_level".into(),
            Arc::new(Int32Array::from(skill_level)),
        ),
        (
            "task_cognitive".into(),
            Arc::new(Float64Array::from(task_cognitive)),
        ),
        (
            "task_physical".into(),
            Arc::new(Float64Array::from(task_physical)),
        ),
        (
            "task_vision".into(),
            Arc::new(Float64Array::from(task_vision)),
        ),
        (
            "task_hearing".into(),
            Arc::new(Float64Array::from(task_hearing)),
        ),
        (
            "task_manual_dexterity".into(),
            Arc::new(Float64Array::from(task_manual_dexterity)),
        ),
        (
            "task_communication".into(),
            Arc::new(Float64Array::from(task_communication)),
        ),
        ("industry".into(), Arc::new(Int32Array::from(industry))),
        ("sector".into(), Arc::new(Int32Array::from(sector))),
        (
            "individual_fe".into(),
            Arc::new(Float64Array::from(individual_fe)),
        ),
        (
            "occ_match_quality".into(),
            Arc::new(Float64Array::from(occ_match_quality)),
        ),
        (
            "baseline_employed".into(),
            Arc::new(BooleanArray::from(baseline_employed)),
        ),
        (
            "baseline_hours".into(),
            Arc::new(Int32Array::from(baseline_hours)),
        ),
        (
            "baseline_income".into(),
            Arc::new(Float64Array::from(baseline_income)),
        ),
        (
            "disability_onset_year".into(),
            Arc::new(Int32Array::from(dis_onset_year)),
        ),
        (
            "disability_type".into(),
            Arc::new(Int32Array::from(dis_type)),
        ),
        (
            "disability_severity".into(),
            Arc::new(Int32Array::from(dis_severity)),
        ),
        ("is_dc".into(), Arc::new(BooleanArray::from(dis_is_dc))),
        (
            "disability_dose".into(),
            Arc::new(Float64Array::from(dis_dose)),
        ),
        (
            "person_type".into(),
            Arc::new(Int32Array::from(dis_person_type)),
        ),
        (
            "comorbidity_flags".into(),
            Arc::new(Int32Array::from(dis_comorbidity)),
        ),
    ]
}

// -- Parquet write/read helpers ----------------------------------------------

fn write_arrays_to_parquet(arrays: Vec<(String, ArrayRef)>, path: &str) -> StdResult<(), String> {
    let fields: Vec<Field> = arrays
        .iter()
        .map(|(name, a)| {
            Field::new(
                name.as_str(),
                a.data_type().clone(),
                a.is_nullable() || a.null_count() > 0,
            )
        })
        .collect();
    let schema = Arc::new(Schema::new(fields));
    let cols: Vec<ArrayRef> = arrays.into_iter().map(|(_, a)| a).collect();
    let batch =
        RecordBatch::try_new(schema.clone(), cols).map_err(|e| format!("record batch: {}", e))?;

    let file = File::create(path).map_err(|e| format!("create {}: {}", path, e))?;
    let props = WriterProperties::builder()
        .set_compression(Compression::SNAPPY)
        .build();
    let mut writer =
        ArrowWriter::try_new(file, schema, Some(props)).map_err(|e| format!("writer: {}", e))?;
    writer.write(&batch).map_err(|e| format!("write: {}", e))?;
    writer.close().map_err(|e| format!("close: {}", e))?;
    Ok(())
}

/// Read a parquet file that we wrote ourselves (expected to contain
/// a single RecordBatch).
fn read_single_batch_parquet(path: &str) -> StdResult<RecordBatch, String> {
    let file = File::open(path).map_err(|e| format!("open {}: {}", path, e))?;
    let builder = ParquetRecordBatchReaderBuilder::try_new(file)
        .map_err(|e| format!("reader build: {}", e))?;
    let reader = builder.build().map_err(|e| format!("reader: {}", e))?;
    let batches: Vec<RecordBatch> = reader
        .collect::<StdResult<_, _>>()
        .map_err(|e| format!("read batches: {}", e))?;
    if batches.is_empty() {
        return Err("template parquet has no batches".to_string());
    }
    if batches.len() == 1 {
        return Ok(batches.into_iter().next().unwrap());
    }
    // Multiple batches — concatenate manually (templates should be
    // small enough that this isn't expensive).
    let schema = batches[0].schema();
    let n_cols = schema.fields().len();
    let mut out_cols: Vec<ArrayRef> = Vec::with_capacity(n_cols);
    for col_idx in 0..n_cols {
        let dt = schema.field(col_idx).data_type().clone();
        out_cols.push(concat_arrays(&batches, col_idx, &dt)?);
    }
    RecordBatch::try_new(schema, out_cols).map_err(|e| format!("concat batch: {}", e))
}

fn concat_arrays(
    batches: &[RecordBatch],
    col_idx: usize,
    dt: &DataType,
) -> StdResult<ArrayRef, String> {
    // Simple per-type manual concat. Only handles types we use.
    match dt {
        DataType::Int32 => {
            let mut out: Vec<Option<i32>> = Vec::new();
            for b in batches {
                let a = b
                    .column(col_idx)
                    .as_any()
                    .downcast_ref::<Int32Array>()
                    .ok_or("expected Int32Array")?;
                for i in 0..a.len() {
                    out.push(if a.is_null(i) { None } else { Some(a.value(i)) });
                }
            }
            Ok(Arc::new(Int32Array::from(out)))
        }
        DataType::Float64 => {
            let mut out: Vec<Option<f64>> = Vec::new();
            for b in batches {
                let a = b
                    .column(col_idx)
                    .as_any()
                    .downcast_ref::<Float64Array>()
                    .ok_or("expected Float64Array")?;
                for i in 0..a.len() {
                    out.push(if a.is_null(i) { None } else { Some(a.value(i)) });
                }
            }
            Ok(Arc::new(Float64Array::from(out)))
        }
        DataType::Boolean => {
            let mut out: Vec<Option<bool>> = Vec::new();
            for b in batches {
                let a = b
                    .column(col_idx)
                    .as_any()
                    .downcast_ref::<BooleanArray>()
                    .ok_or("expected BooleanArray")?;
                for i in 0..a.len() {
                    out.push(if a.is_null(i) { None } else { Some(a.value(i)) });
                }
            }
            Ok(Arc::new(BooleanArray::from(out)))
        }
        DataType::Utf8 => {
            let mut out: Vec<Option<String>> = Vec::new();
            for b in batches {
                let a = b
                    .column(col_idx)
                    .as_any()
                    .downcast_ref::<StringArray>()
                    .ok_or("expected StringArray")?;
                for i in 0..a.len() {
                    out.push(if a.is_null(i) {
                        None
                    } else {
                        Some(a.value(i).to_string())
                    });
                }
            }
            let refs: Vec<Option<&str>> = out.iter().map(|s| s.as_deref()).collect();
            Ok(Arc::new(StringArray::from(refs)))
        }
        _ => Err(format!("unsupported concat type: {:?}", dt)),
    }
}

// -- Per-type take (positional gather) ---------------------------------------

fn take_i32(src: &Int32Array, idx: &[u32]) -> Int32Array {
    if src.null_count() == 0 {
        let v: Vec<i32> = idx.iter().map(|&i| src.value(i as usize)).collect();
        Int32Array::from(v)
    } else {
        let v: Vec<Option<i32>> = idx
            .iter()
            .map(|&i| {
                let i = i as usize;
                if src.is_null(i) {
                    None
                } else {
                    Some(src.value(i))
                }
            })
            .collect();
        Int32Array::from(v)
    }
}

fn take_f64(src: &Float64Array, idx: &[u32]) -> Float64Array {
    if src.null_count() == 0 {
        let v: Vec<f64> = idx.iter().map(|&i| src.value(i as usize)).collect();
        Float64Array::from(v)
    } else {
        let v: Vec<Option<f64>> = idx
            .iter()
            .map(|&i| {
                let i = i as usize;
                if src.is_null(i) {
                    None
                } else {
                    Some(src.value(i))
                }
            })
            .collect();
        Float64Array::from(v)
    }
}

fn take_bool(src: &BooleanArray, idx: &[u32]) -> BooleanArray {
    if src.null_count() == 0 {
        let v: Vec<bool> = idx.iter().map(|&i| src.value(i as usize)).collect();
        BooleanArray::from(v)
    } else {
        let v: Vec<Option<bool>> = idx
            .iter()
            .map(|&i| {
                let i = i as usize;
                if src.is_null(i) {
                    None
                } else {
                    Some(src.value(i))
                }
            })
            .collect();
        BooleanArray::from(v)
    }
}

fn take_string(src: &StringArray, idx: &[u32]) -> StringArray {
    if src.null_count() == 0 {
        let v: Vec<&str> = idx.iter().map(|&i| src.value(i as usize)).collect();
        StringArray::from(v)
    } else {
        let v: Vec<Option<&str>> = idx
            .iter()
            .map(|&i| {
                let i = i as usize;
                if src.is_null(i) {
                    None
                } else {
                    Some(src.value(i))
                }
            })
            .collect();
        StringArray::from(v)
    }
}

fn take_dispatch(src: &ArrayRef, idx: &[u32]) -> ArrayRef {
    match src.data_type() {
        DataType::Int32 => Arc::new(take_i32(
            src.as_any().downcast_ref::<Int32Array>().unwrap(),
            idx,
        )),
        DataType::Float64 => Arc::new(take_f64(
            src.as_any().downcast_ref::<Float64Array>().unwrap(),
            idx,
        )),
        DataType::Boolean => Arc::new(take_bool(
            src.as_any().downcast_ref::<BooleanArray>().unwrap(),
            idx,
        )),
        DataType::Utf8 => Arc::new(take_string(
            src.as_any().downcast_ref::<StringArray>().unwrap(),
            idx,
        )),
        dt => panic!("take_dispatch: unsupported type {:?}", dt),
    }
}

// -- AEUID generation ---------------------------------------------------------

const AEUID_SEQ_LIMIT: usize = 0x1000_0000;

fn aeuid_prefix(agency: &str, base_seed: i32) -> u64 {
    let agency_hash: i64 = agency
        .to_uppercase()
        .chars()
        .enumerate()
        .map(|(i, c)| (c as i64) * ((i as i64) + 1) * 4099)
        .sum();
    ((base_seed as i64) * 1009 + agency_hash + 0xA53C7).rem_euclid(0x10_0000) as u64
}

/// Deterministic collision-free per-agency AEUID generator.
///
/// AEUIDs remain 12-character uppercase hex strings. The first 5 hex digits
/// are an agency/seed prefix, and the final 7 hex digits are the person-row
/// sequence number.
fn generate_aeuids_rust(n: usize, agency: &str, base_seed: i32) -> Vec<String> {
    if n >= AEUID_SEQ_LIMIT {
        panic!("AEUID generator supports n < {}", AEUID_SEQ_LIMIT);
    }
    let prefix = aeuid_prefix(agency, base_seed);
    (0..n)
        .map(|i| format!("{:05X}{:07X}", prefix, (i as u64) + 1))
        .collect()
}

// -- Public extendr entry points ---------------------------------------------

/// Build the spine template parquet.
/// Runs the full spine generator at `n_template` persons and writes
/// the result directly to a parquet file. This is the one-time cost
/// amortised over many subsequent `generate_spine_from_template__`
/// calls.
/// @export
#[extendr]
fn build_spine_template_parquet__(n_template: i32, seed: i32, out_path: &str) -> i32 {
    let persons = build_persons(n_template as usize, seed);
    let arrays = persons_to_arrays(&persons);
    if let Err(e) = write_arrays_to_parquet(arrays, out_path) {
        panic!("write template: {}", e);
    }
    n_template
}

/// Generate a spine of size n from a cached template parquet.
///
/// - Reads `template_path` (expected single batch).
/// - Samples n indices uniformly with replacement from the template.
/// - Takes each column positionally.
/// - Overwrites `id` and `spine_id` with fresh sequential values.
/// - Generates per-agency AEUID columns in Rust.
/// - Writes the output parquet.
///
/// The sampling RNG is seeded from `seed + 900`. AEUID RNGs are
/// seeded per agency from `seed + salt * 1000` to match the R
/// `generate_aeuids()` contract.
/// @export
#[extendr]
fn generate_spine_from_template_parquet__(
    template_path: &str,
    n: i32,
    seed: i32,
    out_path: &str,
    agency_codes: Strings,
) -> i32 {
    let n = n as usize;

    let template = match read_single_batch_parquet(template_path) {
        Ok(b) => b,
        Err(e) => panic!("read template: {}", e),
    };
    let n_template = template.num_rows();
    if n_template == 0 {
        panic!("template has zero rows");
    }

    // Sample indices with replacement.
    let mut rng_sample = StdRng::seed_from_u64((seed as i64 + 900) as u64);
    let indices: Vec<u32> = (0..n)
        .map(|_| rng_sample.gen_range(0..n_template as u32))
        .collect();

    // Take each column from template.
    let schema = template.schema();
    let n_cols = schema.fields().len();

    let mut out_fields: Vec<Field> = Vec::with_capacity(n_cols + agency_codes.len());
    let mut out_arrays: Vec<ArrayRef> = Vec::with_capacity(n_cols + agency_codes.len());

    for col_idx in 0..n_cols {
        let field = schema.field(col_idx);
        let name = field.name().as_str();
        let src = template.column(col_idx);

        // Overwrite unique-per-row columns with fresh sequential values.
        let out_arr: ArrayRef = match name {
            "id" => {
                let v: Vec<String> = (1..=n).map(|i| format!("P{:010}", i)).collect();
                Arc::new(StringArray::from(v))
            }
            "spine_id" => {
                let v: Vec<String> = (1..=n).map(|i| format!("SP{:010}", i)).collect();
                Arc::new(StringArray::from(v))
            }
            "residence_seed" => {
                let mut rng = StdRng::seed_from_u64(
                    (seed as u64).wrapping_add(crate::seeds::spine::RESIDENCE),
                );
                let v: Vec<i32> = (0..n).map(|_| rng.gen_range(1..=i32::MAX)).collect();
                Arc::new(Int32Array::from(v))
            }
            _ => take_dispatch(src, &indices),
        };

        out_fields.push(Field::new(
            name,
            out_arr.data_type().clone(),
            out_arr.null_count() > 0 || field.is_nullable(),
        ));
        out_arrays.push(out_arr);
    }

    // Assign household ids on the full sampled population (households cannot
    // be carried in the with-replacement template), so the Census and CORE can
    // derive consistent dwelling/family membership. Runs centrally before
    // slicing, so a household never splits across build slices.
    if let Some(bi) = out_fields.iter().position(|f| f.name() == "birth_year") {
        if let Some(arr) = out_arrays[bi].as_any().downcast_ref::<Int32Array>() {
            let by: Vec<i32> = (0..arr.len()).map(|i| arr.value(i)).collect();
            let household_ids = assign_household_ids(&by, seed);
            out_fields.push(Field::new("household_id", DataType::Int32, false));
            out_arrays.push(Arc::new(Int32Array::from(household_ids)));
        }
    }

    // Generate per-agency AEUID columns directly in Rust.
    for agency in agency_codes.iter() {
        let agency_s = agency.to_string();
        let aeuids = generate_aeuids_rust(n, &agency_s, seed);
        let col_name = format!("aeuid_{}", agency_s.to_lowercase());
        out_fields.push(Field::new(&col_name, DataType::Utf8, false));
        out_arrays.push(Arc::new(StringArray::from(aeuids)));
    }

    let out_schema = Arc::new(Schema::new(out_fields));
    let out_batch = match RecordBatch::try_new(out_schema.clone(), out_arrays) {
        Ok(b) => b,
        Err(e) => panic!("output record batch: {}", e),
    };

    let file = match File::create(out_path) {
        Ok(f) => f,
        Err(e) => panic!("create {}: {}", out_path, e),
    };
    let props = WriterProperties::builder()
        .set_compression(Compression::SNAPPY)
        .build();
    let mut writer = match ArrowWriter::try_new(file, out_schema, Some(props)) {
        Ok(w) => w,
        Err(e) => panic!("arrow writer: {}", e),
    };
    if let Err(e) = writer.write(&out_batch) {
        panic!("write batch: {}", e);
    }
    if let Err(e) = writer.close() {
        panic!("close writer: {}", e);
    }

    n as i32
}

extendr_module! {
    mod spine_template;
    fn build_spine_template_parquet__;
    fn generate_spine_from_template_parquet__;
}

//! Direct parquet writing from Rust.
//!
//! Bypasses R's arrow package (~4s/year for 14M-row MBS outputs) by writing
//! parquet files directly from native Rust Vec columns.

use std::fs::File;
use std::sync::Arc;

use arrow_array::{
    Array, ArrayRef, BooleanArray, Float64Array, Int32Array, RecordBatch, StringArray,
};
use arrow_schema::{DataType, Field, Schema};
use parquet::arrow::arrow_writer::ArrowWriter;
use parquet::basic::Compression;
use parquet::file::properties::WriterProperties;

/// Column payload for building an Arrow RecordBatch.
pub enum Col {
    Bool(Vec<bool>),
    I32(Vec<i32>),
    F64(Vec<f64>),
    Str(Vec<String>),
    /// Optional string (nullable)
    StrOpt(Vec<Option<String>>),
    /// Nullable i32 (i32::MIN = null)
    I32Opt(Vec<i32>),
    /// Date32 (days since epoch, nullable with i32::MIN = null)
    Date(Vec<i32>),
    /// Date32 (days since epoch, non-nullable)
    DateNN(Vec<i32>),
    /// Date rendered as a "ddmmmYY" string (e.g. "31Jan20") from days since
    /// epoch; i32::MIN = null. Used by the health-claim products (STP/MBS/PBS).
    DateStr(Vec<i32>),
}

const MONTH_ABBR: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];

/// Convert a Date32 value (days since 1970-01-01) to a "ddmmmYY" string, e.g.
/// 18292 -> "31Jan20". Returns None for the i32::MIN null sentinel. Uses
/// Howard Hinnant's civil-from-days algorithm (no chrono dependency).
pub fn days_to_ddmmmyy(days: i32) -> Option<String> {
    if days == i32::MIN {
        return None;
    }
    let z = days as i64 + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097; // [0, 146096]
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365; // [0, 399]
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100); // [0, 365]
    let mp = (5 * doy + 2) / 153; // [0, 11]
    let d = doy - (153 * mp + 2) / 5 + 1; // [1, 31]
    let m = if mp < 10 { mp + 3 } else { mp - 9 }; // [1, 12]
    let year = if m <= 2 { y + 1 } else { y };
    Some(format!(
        "{:02}{}{:02}",
        d,
        MONTH_ABBR[(m - 1) as usize],
        year.rem_euclid(100)
    ))
}

impl Col {
    pub fn len(&self) -> usize {
        match self {
            Col::Bool(v) => v.len(),
            Col::I32(v) | Col::I32Opt(v) => v.len(),
            Col::F64(v) => v.len(),
            Col::Str(v) => v.len(),
            Col::StrOpt(v) => v.len(),
            Col::Date(v) | Col::DateNN(v) | Col::DateStr(v) => v.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[allow(dead_code)]
    fn to_array(self) -> ArrayRef {
        match self {
            Col::Bool(v) => Arc::new(BooleanArray::from(v)),
            Col::I32(v) => Arc::new(Int32Array::from(v)),
            Col::F64(v) => Arc::new(Float64Array::from(v)),
            Col::Str(v) => {
                let refs: Vec<&str> = v.iter().map(|s| s.as_str()).collect();
                Arc::new(StringArray::from(refs))
            }
            Col::StrOpt(v) => {
                let refs: Vec<Option<&str>> = v.iter().map(|o| o.as_deref()).collect();
                Arc::new(StringArray::from(refs))
            }
            Col::I32Opt(v) => {
                let opts: Vec<Option<i32>> = v
                    .into_iter()
                    .map(|x| if x == i32::MIN { None } else { Some(x) })
                    .collect();
                Arc::new(Int32Array::from(opts))
            }
            Col::Date(v) => {
                let opts: Vec<Option<i32>> = v
                    .into_iter()
                    .map(|x| if x == i32::MIN { None } else { Some(x) })
                    .collect();
                Arc::new(arrow_array::Date32Array::from(opts))
            }
            Col::DateNN(v) => Arc::new(arrow_array::Date32Array::from(v)),
            Col::DateStr(v) => {
                let strs: Vec<Option<String>> = v.into_iter().map(days_to_ddmmmyy).collect();
                let refs: Vec<Option<&str>> = strs.iter().map(|o| o.as_deref()).collect();
                Arc::new(StringArray::from(refs))
            }
        }
    }

    fn slice_to_array(&self, offset: usize, len: usize) -> ArrayRef {
        let end = offset + len;
        match self {
            Col::Bool(v) => Arc::new(BooleanArray::from(v[offset..end].to_vec())),
            Col::I32(v) => Arc::new(Int32Array::from_iter_values(v[offset..end].iter().copied())),
            Col::F64(v) => Arc::new(Float64Array::from_iter_values(
                v[offset..end].iter().copied(),
            )),
            Col::Str(v) => {
                let refs: Vec<&str> = v[offset..end].iter().map(|s| s.as_str()).collect();
                Arc::new(StringArray::from(refs))
            }
            Col::StrOpt(v) => {
                let refs: Vec<Option<&str>> = v[offset..end].iter().map(|o| o.as_deref()).collect();
                Arc::new(StringArray::from(refs))
            }
            Col::I32Opt(v) => {
                let opts: Vec<Option<i32>> = v[offset..end]
                    .iter()
                    .map(|&x| if x == i32::MIN { None } else { Some(x) })
                    .collect();
                Arc::new(Int32Array::from(opts))
            }
            Col::Date(v) => {
                let opts: Vec<Option<i32>> = v[offset..end]
                    .iter()
                    .map(|&x| if x == i32::MIN { None } else { Some(x) })
                    .collect();
                Arc::new(arrow_array::Date32Array::from(opts))
            }
            Col::DateNN(v) => Arc::new(arrow_array::Date32Array::from_iter_values(
                v[offset..end].iter().copied(),
            )),
            Col::DateStr(v) => {
                let strs: Vec<Option<String>> =
                    v[offset..end].iter().map(|&x| days_to_ddmmmyy(x)).collect();
                let refs: Vec<Option<&str>> = strs.iter().map(|o| o.as_deref()).collect();
                Arc::new(StringArray::from(refs))
            }
        }
    }

    /// Split the column into chunks of at most `chunk_rows` rows each,
    /// producing one ArrayRef per chunk. Consumes the column.
    pub fn into_chunks(self, chunk_rows: usize) -> Vec<ArrayRef> {
        let n = self.len();
        if n == 0 {
            return Vec::new();
        }
        let n_chunks = n.div_ceil(chunk_rows);
        let mut out: Vec<ArrayRef> = Vec::with_capacity(n_chunks);
        match self {
            Col::I32(mut v) => {
                // Drain in chunks so each chunk becomes an owned Vec.
                let mut drained = 0usize;
                while drained < v.len() {
                    let take = (v.len() - drained).min(chunk_rows);
                    let chunk: Vec<i32> = v.drain(..take).collect();
                    drained += 0; // no-op; v is shrinking
                    let _ = drained;
                    out.push(Arc::new(Int32Array::from(chunk)));
                }
            }
            Col::Bool(mut v) => {
                while !v.is_empty() {
                    let take = v.len().min(chunk_rows);
                    let chunk: Vec<bool> = v.drain(..take).collect();
                    out.push(Arc::new(BooleanArray::from(chunk)));
                }
            }
            Col::F64(mut v) => {
                while !v.is_empty() {
                    let take = v.len().min(chunk_rows);
                    let chunk: Vec<f64> = v.drain(..take).collect();
                    out.push(Arc::new(Float64Array::from(chunk)));
                }
            }
            Col::Str(mut v) => {
                while !v.is_empty() {
                    let take = v.len().min(chunk_rows);
                    let chunk: Vec<String> = v.drain(..take).collect();
                    let refs: Vec<&str> = chunk.iter().map(|s| s.as_str()).collect();
                    out.push(Arc::new(StringArray::from(refs)));
                }
            }
            Col::StrOpt(mut v) => {
                while !v.is_empty() {
                    let take = v.len().min(chunk_rows);
                    let chunk: Vec<Option<String>> = v.drain(..take).collect();
                    let refs: Vec<Option<&str>> = chunk.iter().map(|o| o.as_deref()).collect();
                    out.push(Arc::new(StringArray::from(refs)));
                }
            }
            Col::I32Opt(mut v) => {
                while !v.is_empty() {
                    let take = v.len().min(chunk_rows);
                    let chunk: Vec<i32> = v.drain(..take).collect();
                    let opts: Vec<Option<i32>> = chunk
                        .into_iter()
                        .map(|x| if x == i32::MIN { None } else { Some(x) })
                        .collect();
                    out.push(Arc::new(Int32Array::from(opts)));
                }
            }
            Col::Date(mut v) => {
                while !v.is_empty() {
                    let take = v.len().min(chunk_rows);
                    let chunk: Vec<i32> = v.drain(..take).collect();
                    let opts: Vec<Option<i32>> = chunk
                        .into_iter()
                        .map(|x| if x == i32::MIN { None } else { Some(x) })
                        .collect();
                    out.push(Arc::new(arrow_array::Date32Array::from(opts)));
                }
            }
            Col::DateNN(mut v) => {
                while !v.is_empty() {
                    let take = v.len().min(chunk_rows);
                    let chunk: Vec<i32> = v.drain(..take).collect();
                    out.push(Arc::new(arrow_array::Date32Array::from(chunk)));
                }
            }
            Col::DateStr(mut v) => {
                while !v.is_empty() {
                    let take = v.len().min(chunk_rows);
                    let chunk: Vec<i32> = v.drain(..take).collect();
                    let strs: Vec<Option<String>> =
                        chunk.into_iter().map(days_to_ddmmmyy).collect();
                    let refs: Vec<Option<&str>> = strs.iter().map(|o| o.as_deref()).collect();
                    out.push(Arc::new(StringArray::from(refs)));
                }
            }
        }
        out
    }

    fn arrow_type(&self) -> DataType {
        match self {
            Col::Bool(_) => DataType::Boolean,
            Col::I32(_) | Col::I32Opt(_) => DataType::Int32,
            Col::F64(_) => DataType::Float64,
            Col::Str(_) | Col::StrOpt(_) | Col::DateStr(_) => DataType::Utf8,
            Col::Date(_) | Col::DateNN(_) => DataType::Date32,
        }
    }

    fn nullable(&self) -> bool {
        matches!(
            self,
            Col::StrOpt(_) | Col::I32Opt(_) | Col::Date(_) | Col::DateStr(_)
        )
    }
}

/// Named column.
pub struct NamedCol {
    pub name: &'static str,
    pub col: Col,
}

/// Max rows per chunk when building StringArrays. Each StringArray uses i32
/// offsets with a 2 GB total string byte cap; chunking bounds per-chunk
/// cumulative bytes well below that limit.
const CHUNK_ROWS: usize = 1_000_000;

/// Write a set of named columns to a parquet file.
///
/// At large N (20m+) a single i32-offset StringArray can exceed the 2 GB
/// cumulative-byte limit, panicking inside `arrow-array::byte_array`. To
/// avoid that, this helper splits the input into fixed-size row chunks
/// and writes them as separate RecordBatches to the same ArrowWriter.
/// The parquet writer also caps row groups at 256k rows so the written
/// file stays compatible with readers that use i32 offsets per row group.
pub fn write_columns_to_parquet(path: &str, columns: Vec<NamedCol>) -> Result<(), String> {
    // Schema is built from column names + types (borrowed from first pass).
    let fields: Vec<Field> = columns
        .iter()
        .map(|nc| Field::new(nc.name, nc.col.arrow_type(), nc.col.nullable()))
        .collect();
    let schema = Arc::new(Schema::new(fields));

    // Row count — all columns must be equal length.
    let n_rows = columns.first().map(|nc| nc.col.len()).unwrap_or(0);
    for nc in &columns {
        if nc.col.len() != n_rows {
            return Err(format!(
                "column `{}` has {} rows, expected {}",
                nc.name,
                nc.col.len(),
                n_rows
            ));
        }
    }

    let file = File::create(path).map_err(|e| format!("create file: {}", e))?;
    let props = WriterProperties::builder()
        .set_compression(Compression::SNAPPY)
        .set_max_row_group_size(256_000)
        .build();
    let mut writer = ArrowWriter::try_new(file, schema.clone(), Some(props))
        .map_err(|e| format!("writer build: {}", e))?;

    if n_rows == 0 {
        writer.close().map_err(|e| format!("close writer: {}", e))?;
        return Ok(());
    }

    let n_chunks = if n_rows == 0 {
        0
    } else {
        n_rows.div_ceil(CHUNK_ROWS)
    };
    for chunk_i in 0..n_chunks {
        let offset = chunk_i * CHUNK_ROWS;
        let len = (n_rows - offset).min(CHUNK_ROWS);
        let arrays: Vec<ArrayRef> = columns
            .iter()
            .map(|nc| nc.col.slice_to_array(offset, len))
            .collect();
        let batch = RecordBatch::try_new(schema.clone(), arrays)
            .map_err(|e| format!("RecordBatch build (chunk {}): {}", chunk_i, e))?;
        writer
            .write(&batch)
            .map_err(|e| format!("write batch (chunk {}): {}", chunk_i, e))?;
    }
    writer.close().map_err(|e| format!("close writer: {}", e))?;
    Ok(())
}

/// Write a set of pre-built Arrow ArrayRefs to a parquet file.
///
/// This variant is used when the same ArrayRef is shared across multiple
/// parquet writes (e.g. PIT_ITR writes the same AEUID and FY columns to 4
/// sub-table files). ArrayRefs are `Arc<dyn Array>`, so cloning one across
/// writes is just a refcount bump — no data copy.
///
/// Splits per-row-group at 256k rows via writer properties for downstream
/// reader compatibility, and slices the arrays into CHUNK_ROWS batches to
/// stay within i32-offset limits when writing.
pub fn write_named_arrays_to_parquet(
    path: &str,
    named: Vec<(&'static str, ArrayRef)>,
) -> Result<(), String> {
    if named.is_empty() {
        return Err("write_named_arrays_to_parquet called with 0 columns".into());
    }
    let n_rows = named[0].1.len();
    for (name, arr) in &named {
        if arr.len() != n_rows {
            return Err(format!(
                "column `{}` has {} rows, expected {}",
                name,
                arr.len(),
                n_rows
            ));
        }
    }
    let fields: Vec<Field> = named
        .iter()
        .map(|(name, arr)| Field::new(*name, arr.data_type().clone(), arr.is_nullable()))
        .collect();
    let schema = Arc::new(Schema::new(fields));
    let arrays: Vec<ArrayRef> = named.into_iter().map(|(_, a)| a).collect();

    let file = File::create(path).map_err(|e| format!("create file: {}", e))?;
    let props = WriterProperties::builder()
        .set_compression(Compression::SNAPPY)
        .set_max_row_group_size(256_000)
        .build();
    let mut writer = ArrowWriter::try_new(file, schema.clone(), Some(props))
        .map_err(|e| format!("writer build: {}", e))?;

    if n_rows == 0 {
        writer.close().map_err(|e| format!("close writer: {}", e))?;
        return Ok(());
    }

    // Slice the arrays into chunks that fit within StringArray's 2 GB
    // cumulative-bytes cap. ArrayRef::slice() is a cheap zero-copy view.
    let n_chunks = n_rows.div_ceil(CHUNK_ROWS);
    for chunk_i in 0..n_chunks {
        let offset = chunk_i * CHUNK_ROWS;
        let len = (n_rows - offset).min(CHUNK_ROWS);
        let sliced: Vec<ArrayRef> = arrays.iter().map(|a| a.slice(offset, len)).collect();
        let batch = RecordBatch::try_new(schema.clone(), sliced)
            .map_err(|e| format!("RecordBatch build (chunk {}): {}", chunk_i, e))?;
        writer
            .write(&batch)
            .map_err(|e| format!("write batch (chunk {}): {}", chunk_i, e))?;
    }
    writer.close().map_err(|e| format!("close writer: {}", e))?;
    Ok(())
}

#[cfg(test)]
mod date_tests {
    use super::days_to_ddmmmyy;

    #[test]
    fn ddmmmyy_format() {
        assert_eq!(days_to_ddmmmyy(0).as_deref(), Some("01Jan70")); // epoch
        assert_eq!(days_to_ddmmmyy(365).as_deref(), Some("01Jan71"));
        // 2020-01-31 is 18292 days after 1970-01-01.
        assert_eq!(days_to_ddmmmyy(18292).as_deref(), Some("31Jan20"));
        // 2000-02-29 (leap day) is 11016 days.
        assert_eq!(days_to_ddmmmyy(11016).as_deref(), Some("29Feb00"));
        assert_eq!(days_to_ddmmmyy(i32::MIN), None);
    }
}

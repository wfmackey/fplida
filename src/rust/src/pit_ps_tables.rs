//! Projection of a financial year's payment-summary records onto the tables
//! the PLIDA data item list declares for that year.
//!
//! The registry is the contract: `generate_pit_ps()` reads
//! `inst/plida_metadata/variables.csv` and hands the plan down, and every
//! column written here is one the registry names. A variable this module does
//! not know fails the build rather than being written under a guessed name or
//! silently dropped.

use crate::parquet_io::{Col, NamedCol};

/// The PAYG payment summary forms the ATO publishes. `PMT_SMRY_TYP` has an
/// open domain -- the delivery publishes no code list -- so these short
/// mnemonics are a modelling choice, one per form the ATO does publish
/// (individual non-business, business and personal services income,
/// employment termination payment, superannuation income stream, foreign
/// employment). Source: ATO, PAYG payment summary forms and guidelines.
pub const PS_TYPE_INB: u8 = 0;
pub const PS_TYPE_BPSI: u8 = 1;
pub const PS_TYPE_ETP: u8 = 2;
pub const PS_TYPE_SIS: u8 = 3;
pub const PS_TYPE_FEI: u8 = 4;

const PS_TYPE_CODE: [&str; 5] = ["INB", "BPSI", "ETP", "SIS", "FEI"];

/// Payment type on a PAYG payment summary - individual non-business.
/// `INB_INCM_TYP` also has an open domain, so these follow the payment types
/// that form distinguishes: salary or wages, working holiday maker, and
/// closely held payees. Source: ATO, PAYG payment summary - individual
/// non-business.
const INB_TYPE_CODE: [&str; 3] = ["S", "H", "P"];

/// Lump sum A type. The two codes are the registry's own value list: R for a
/// payment on termination for genuine redundancy, invalidity or an early
/// retirement scheme, T for termination for any other reason. Source: ATO,
/// Single Touch Payroll Phase 2 employer reporting guidelines.
const LSPA_TYPE_CODE: [&str; 2] = ["R", "T"];

/// Address repair outcome. The three values are the registry's own value
/// list; the ABS does not publish codes for them. Source: ABS Statistical
/// Spatial Framework, 'Geocoding unit record data using address and
/// location'.
const REPAIR_RESULT_LABEL: [&str; 3] =
    ["No repair required", "Repaired", "Not repaired"];

/// One financial year of payment-summary records, before any table takes a
/// view of them. One entry per person-employer-year row, in output order.
pub struct YearRecords {
    pub aeuid: Vec<String>,
    pub bn: Vec<String>,
    pub person: Vec<usize>,
    pub gross: Vec<f64>,
    pub tax: Vec<f64>,
    pub fbt: Vec<f64>,
    pub employer_super: Vec<f64>,
    pub allowances: Vec<f64>,
    pub lump_a: Vec<f64>,
    pub lump_b: Vec<f64>,
    pub lump_d: Vec<f64>,
    pub lump_e: Vec<f64>,
    pub tax_free: Vec<f64>,
    pub taxable: Vec<f64>,
    pub exempt_foreign: Vec<f64>,
    pub ps_type: Vec<u8>,
    /// 0 = not an individual non-business summary, otherwise 1-based into
    /// `INB_TYPE_CODE`.
    pub inb_type: Vec<u8>,
    /// 0 = no lump sum A on this summary, otherwise 1-based into
    /// `LSPA_TYPE_CODE`.
    pub lspa_type: Vec<u8>,
    pub amended: Vec<bool>,
    pub period_start: Vec<i32>,
    pub period_end: Vec<i32>,
    /// Days since epoch, `i32::MIN` where the form reports a period rather
    /// than one discrete payment.
    pub payment_date: Vec<i32>,
    pub repair_result: Vec<u8>,
    /// Lodged after the six-month extract was cut.
    pub late: Vec<bool>,
}

/// Per-person fields the payment-summary tables carry but the employment
/// panel does not: age inputs and the address the ATO holds.
pub struct PersonFields<'a> {
    pub birth_year: &'a [i32],
    pub birth_month: &'a [i32],
    pub sa1: &'a [String],
    pub mb: &'a [String],
    pub lga: &'a [String],
    pub arid: &'a [String],
    pub sa2: &'a [i32],
    pub sa3: &'a [i32],
    pub sa4: &'a [i32],
    pub ste: &'a [i32],
}

/// Age in completed years at 1 July of `year`, from the birth year and month.
/// A person born in January to June has had their birthday by 1 July; one
/// born from July on has not.
pub fn age_at_july(birth_year: i32, birth_month: i32, year: i32) -> i32 {
    let age = if birth_month <= 6 {
        year - birth_year
    } else {
        year - birth_year - 1
    };
    age.clamp(0, 115)
}

/// Age in completed years at 30 June of `year`. Every 1 July to 30 June
/// window contains exactly one birthday, so this is always one more than the
/// age at the 1 July that opened it.
pub fn age_at_june(birth_year: i32, birth_month: i32, year: i32) -> i32 {
    age_at_july(birth_year, birth_month, year - 1) + 1
}

fn optional_string(value: &str) -> Option<String> {
    if value.is_empty() {
        None
    } else {
        Some(value.to_string())
    }
}

fn optional_int(value: i32) -> i32 {
    if value <= 0 {
        i32::MIN
    } else {
        value
    }
}

fn fin_year_label(fy: i32) -> String {
    format!("{}-{:02}", fy - 1, fy.rem_euclid(100))
}

/// The ATO extract reference, spelled as the other ATO products spell it
/// (`fplida:::.dil_tax_extract_reference`, and BUSOWN's `extract_ref_label`).
fn extract_ref_label(fy: i32) -> String {
    format!("FY{:04}-{:02}", fy - 1, fy.rem_euclid(100))
}

/// The version of the ABS address coding index the extract was coded with.
/// The domain is open -- no list of index identifiers is published -- so one
/// identifier per extract year is a modelling choice, constant within a
/// table, which is what "one version of the coding index" means.
fn coding_index_label(fy: i32) -> String {
    format!("AR{:04}", fy)
}

/// Build one declared column for a table, over the record indices it keeps.
///
/// Returns the static column name alongside the values so the name written to
/// the parquet schema is a literal in this file rather than a string handed
/// in from R.
pub fn build_column(
    name: &str,
    records: &YearRecords,
    people: &PersonFields,
    rows: &[usize],
    fy: i32,
) -> NamedCol {
    let n = rows.len();
    macro_rules! f64_col {
        ($field:ident) => {{
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(records.$field[i]);
            }
            Col::F64(v)
        }};
    }
    macro_rules! person_str {
        ($field:ident) => {{
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(
                    people
                        .$field
                        .get(records.person[i])
                        .and_then(|s| optional_string(s)),
                );
            }
            Col::StrOpt(v)
        }};
    }
    macro_rules! person_int {
        ($field:ident) => {{
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(
                    people
                        .$field
                        .get(records.person[i])
                        .copied()
                        .map(optional_int)
                        .unwrap_or(i32::MIN),
                );
            }
            Col::I32Opt(v)
        }};
    }
    macro_rules! age_col {
        ($fn:ident, $year:expr) => {{
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                let p = records.person[i];
                let year = people.birth_year.get(p).copied().unwrap_or(0);
                let month = people.birth_month.get(p).copied().unwrap_or(6);
                v.push($fn(year, month, $year));
            }
            Col::I32(v)
        }};
    }

    let (column_name, col): (&'static str, Col) = match name {
        "SYNTHETIC_AEUID" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(records.aeuid[i].clone());
            }
            ("SYNTHETIC_AEUID", Col::Str(v))
        }
        // The unprefixed hashing of the employer's ABN that the tables
        // delivered to 2021-22 key on. `crate::blade::helpers::abn_hash_trunc`
        // is the same function `fplida:::.abn_hash_trunc()` calls and the same
        // one that fills `blade-key-abn-hash-trunc-to-bn-key`, so every value
        // written here bridges to a BLADE business.
        "ABN_HASH_TRUNC" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(crate::blade::helpers::abn_hash_trunc(&records.bn[i]));
            }
            ("ABN_HASH_TRUNC", Col::Str(v))
        }
        "BN" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(records.bn[i].clone());
            }
            ("BN", Col::Str(v))
        }
        "GRS_AMT" => ("GRS_AMT", f64_col!(gross)),
        "TAX_WHELD_AMT" => ("TAX_WHELD_AMT", f64_col!(tax)),
        "RPRTBL_FBT_AMT" => ("RPRTBL_FBT_AMT", f64_col!(fbt)),
        "RPRTBL_EMPLYR_SUPER_CNTRBN_AMT" => (
            "RPRTBL_EMPLYR_SUPER_CNTRBN_AMT",
            f64_col!(employer_super),
        ),
        "TOTL_ALWNC_AMT" => ("TOTL_ALWNC_AMT", f64_col!(allowances)),
        "LSPA_AMT" => ("LSPA_AMT", f64_col!(lump_a)),
        "LSPB_AMT" => ("LSPB_AMT", f64_col!(lump_b)),
        "LSPD_AMT" => ("LSPD_AMT", f64_col!(lump_d)),
        "LSPE_AMT" => ("LSPE_AMT", f64_col!(lump_e)),
        "TAX_FREE_AMT" => ("TAX_FREE_AMT", f64_col!(tax_free)),
        "TOTL_TXBL_AMT" => ("TOTL_TXBL_AMT", f64_col!(taxable)),
        "EXMPT_FORGN_EMPLT_INCM_AMT" => (
            "EXMPT_FORGN_EMPLT_INCM_AMT",
            f64_col!(exempt_foreign),
        ),
        "INCM_YR" => ("INCM_YR", Col::I32(vec![fy; n])),
        "FIN_YEAR" => ("FIN_YEAR", Col::Str(vec![fin_year_label(fy); n])),
        "EXTRACT_REF" => (
            "EXTRACT_REF",
            Col::Str(vec![extract_ref_label(fy); n]),
        ),
        "CONST" => ("CONST", Col::Str(vec![coding_index_label(fy); n])),
        "PERD_STRT_DT" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(records.period_start[i]);
            }
            ("PERD_STRT_DT", Col::DateNN(v))
        }
        "PERD_END_DT" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(records.period_end[i]);
            }
            ("PERD_END_DT", Col::DateNN(v))
        }
        "PMT_DT" | "PERD_PMT_DT" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(records.payment_date[i]);
            }
            // Two spellings of one field: `PERD_PMT_DT` from 2010-11 to
            // 2017-18, `PMT_DT` in 2009-10 and again from 2018-19.
            let static_name = if name == "PMT_DT" {
                "PMT_DT"
            } else {
                "PERD_PMT_DT"
            };
            (static_name, Col::Date(v))
        }
        "PMT_SMRY_TYP" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(PS_TYPE_CODE[records.ps_type[i] as usize].to_string());
            }
            ("PMT_SMRY_TYP", Col::Str(v))
        }
        "INB_INCM_TYP" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                let code = records.inb_type[i];
                v.push(if code == 0 {
                    None
                } else {
                    Some(INB_TYPE_CODE[(code - 1) as usize].to_string())
                });
            }
            ("INB_INCM_TYP", Col::StrOpt(v))
        }
        "LSPA_TYP" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                let code = records.lspa_type[i];
                v.push(if code == 0 {
                    None
                } else {
                    Some(LSPA_TYPE_CODE[(code - 1) as usize].to_string())
                });
            }
            ("LSPA_TYP", Col::StrOpt(v))
        }
        // A for an amended payee record, O for an original one not previously
        // reported. Source: ATO, Correct a mistake, PAYG withholding payment
        // summaries. The same one-in-twenty rate the other ATO products use.
        "AMDT_CD" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(if records.amended[i] { "A" } else { "O" }.to_string());
            }
            ("AMDT_CD", Col::Str(v))
        }
        "REPAIR_RESULT" => {
            let mut v = Vec::with_capacity(n);
            for &i in rows {
                v.push(
                    REPAIR_RESULT_LABEL[records.repair_result[i] as usize]
                        .to_string(),
                );
            }
            ("REPAIR_RESULT", Col::Str(v))
        }
        "AGE_FY_START" => ("AGE_FY_START", age_col!(age_at_july, fy - 1)),
        "AGE2122_START" => ("AGE2122_START", age_col!(age_at_july, 2021)),
        "AGE2122_END" => ("AGE2122_END", age_col!(age_at_june, 2022)),
        "AGE2223_START" => ("AGE2223_START", age_col!(age_at_july, 2022)),
        "AGE2223_END" => ("AGE2223_END", age_col!(age_at_june, 2023)),
        "ARID_HASH_TRUNC" => ("ARID_HASH_TRUNC", person_str!(arid)),
        "SA1" => ("SA1", person_str!(sa1)),
        "MB" => ("MB", person_str!(mb)),
        "LGA" => ("LGA", person_str!(lga)),
        "SA2" => ("SA2", person_int!(sa2)),
        "SA3" => ("SA3", person_int!(sa3)),
        "SA4" => ("SA4", person_int!(sa4)),
        "STE" => ("STE", person_int!(ste)),
        other => panic!(
            "PIT_PS: the data item list declares variable `{}`, which the \
             generator does not produce. Add it rather than writing the table \
             without it.",
            other
        ),
    };
    NamedCol {
        name: column_name,
        col,
    }
}

/// The record indices a table keeps.
///
/// An extract cut six months after 30 June has not seen the payment summaries
/// lodged after it; the twelve- and sixteen-month extracts have. The share
/// missing from the early cut is a modelling choice.
pub fn rows_for_table(records: &YearRecords, months: i32) -> Vec<usize> {
    if months >= 12 {
        (0..records.aeuid.len()).collect()
    } else {
        (0..records.aeuid.len())
            .filter(|&i| !records.late[i])
            .collect()
    }
}

/// One row per person present in the year's payment summaries, in the order
/// they first appear.
pub fn rows_one_per_person(records: &YearRecords, rows: &[usize]) -> Vec<usize> {
    let mut seen: std::collections::HashSet<&str> =
        std::collections::HashSet::with_capacity(rows.len());
    let mut out = Vec::with_capacity(rows.len());
    for &i in rows {
        if seen.insert(records.aeuid[i].as_str()) {
            out.push(i);
        }
    }
    out
}

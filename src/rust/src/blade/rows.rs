//! Shared BLADE frame types: the business slice a table is generated from, the
//! column a generator returns, and the per-variable metadata bundle.
//!
//! Every BLADE cascade -- the six table-specific generators, the generic
//! metadata classifier and the name-based fallthrough -- reads the same slice
//! and returns the same column type, so those types live here rather than in
//! any one cascade's file.

use std::cell::OnceCell;
use std::collections::HashMap;

use extendr_api::prelude::*;

use super::helpers::id_number;
use super::periods::{PeriodContext, ResolvedPeriod};

/// R `month.abb`.
pub const MONTH_ABB: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];

/// `.blade_survey_table_numbers`: the ABS business surveys and their
/// requestable survey-weight and commodity tables.
#[inline]
pub fn is_survey_table(table_number: i32) -> bool {
    (8..=23).contains(&table_number) || (38..=46).contains(&table_number)
}

// ---------------------------------------------------------------------------
// Column type
// ---------------------------------------------------------------------------

/// One column of a BLADE frame. `None` is R `NULL`: the branch declined, which
/// in a first-match-wins cascade means "keep going" and at the end of the
/// chain means the business spine does not carry the source column.
pub enum BladeColumn {
    Int(Vec<Option<i32>>),
    Dbl(Vec<Option<f64>>),
    Chr(Vec<Option<String>>),
    /// Days since 1970-01-01, returned to R with class "Date".
    Date(Vec<Option<f64>>),
    /// A business-slice column handed back untouched, for R's `match()` on the
    /// slice's own names.
    Raw(Robj),
    None,
}

impl BladeColumn {
    pub fn ints(values: Vec<i32>) -> BladeColumn {
        BladeColumn::Int(values.into_iter().map(Some).collect())
    }
    pub fn dbls(values: Vec<f64>) -> BladeColumn {
        BladeColumn::Dbl(values.into_iter().map(Some).collect())
    }
    pub fn strs(values: Vec<String>) -> BladeColumn {
        BladeColumn::Chr(values.into_iter().map(Some).collect())
    }
    pub fn na_chr(n: usize) -> BladeColumn {
        BladeColumn::Chr(vec![None; n])
    }
    pub fn rep_str(value: &str, n: usize) -> BladeColumn {
        BladeColumn::Chr(vec![Some(value.to_string()); n])
    }
    pub fn rep_int(value: i32, n: usize) -> BladeColumn {
        BladeColumn::Int(vec![Some(value); n])
    }
    pub fn rep_date(days: f64, n: usize) -> BladeColumn {
        BladeColumn::Date(vec![Some(days); n])
    }
    pub fn is_none(&self) -> bool {
        matches!(self, BladeColumn::None)
    }
    pub fn into_robj(self) -> Robj {
        match self {
            BladeColumn::Int(v) => v.into(),
            BladeColumn::Dbl(v) => v.into(),
            BladeColumn::Chr(v) => v.into(),
            BladeColumn::Date(v) => {
                let mut r: Robj = v.into();
                r.set_class(&["Date"]).expect("Date class");
                r
            }
            BladeColumn::Raw(r) => r,
            BladeColumn::None => ().into(),
        }
    }
}

// ---------------------------------------------------------------------------
// Inputs
// ---------------------------------------------------------------------------

pub fn as_str_vec(robj: &Robj) -> Option<Vec<Option<String>>> {
    let values: Strings = robj.clone().try_into().ok()?;
    Some(
        values
            .iter()
            .map(|x| {
                if x.is_na() {
                    None
                } else {
                    Some(x.to_string())
                }
            })
            .collect(),
    )
}

pub fn as_f64_vec(robj: &Robj) -> Option<Vec<f64>> {
    if let Some(slice) = robj.as_real_slice() {
        return Some(slice.to_vec());
    }
    robj.as_integer_slice().map(|slice| {
        slice
            .iter()
            .map(|&v| if v == i32::MIN { f64::NAN } else { v as f64 })
            .collect()
    })
}

pub fn as_i32_vec(robj: &Robj) -> Option<Vec<i32>> {
    if let Some(slice) = robj.as_integer_slice() {
        return Some(slice.to_vec());
    }
    robj.as_real_slice().map(|slice| {
        slice
            .iter()
            .map(|&v| if v.is_nan() { i32::MIN } else { v as i32 })
            .collect()
    })
}

/// The slice of the business spine a table is generated from. Columns are read
/// on demand: most variables touch one or two, and eagerly copying ninety
/// columns for every variable in a sixty-two table build would cost far more
/// than it saves.
pub struct BusinessRows {
    pub n: usize,
    cols: HashMap<String, Robj>,
    /// Lowercased column name to the name as the slice spells it, first
    /// occurrence winning, which is what R's `match(lower, tolower(names(x)))`
    /// resolves to.
    lower_names: HashMap<String, String>,
    /// `id_number(bn)`, the handle every draw hangs off. R recomputes it inside
    /// each `.blade_draw`; here it is computed once per variable.
    bn_num: OnceCell<Vec<f64>>,
}

impl BusinessRows {
    pub fn from_list(list: &List) -> BusinessRows {
        let mut cols: HashMap<String, Robj> = HashMap::new();
        let mut lower_names: HashMap<String, String> = HashMap::new();
        let mut n = 0usize;
        for (name, value) in list.iter() {
            n = n.max(value.len());
            lower_names
                .entry(name.to_lowercase())
                .or_insert_with(|| name.to_string());
            cols.insert(name.to_string(), value);
        }
        BusinessRows {
            n,
            cols,
            lower_names,
            bn_num: OnceCell::new(),
        }
    }

    pub fn chr(&self, name: &str) -> Option<Vec<Option<String>>> {
        self.cols.get(name).and_then(as_str_vec)
    }

    pub fn num(&self, name: &str) -> Option<Vec<f64>> {
        self.cols.get(name).and_then(as_f64_vec)
    }

    pub fn int(&self, name: &str) -> Option<Vec<i32>> {
        self.cols.get(name).and_then(as_i32_vec)
    }

    pub fn has(&self, name: &str) -> bool {
        self.cols.contains_key(name)
    }

    /// The column R's `business_rows[[match(lower, tolower(names(.)))]]`
    /// returns, untouched.
    pub fn raw_lower(&self, lower: &str) -> Option<Robj> {
        self.lower_names
            .get(lower)
            .and_then(|name| self.cols.get(name))
            .cloned()
    }

    pub fn bn_num(&self) -> &[f64] {
        self.bn_num.get_or_init(|| {
            let bn = self.chr("bn").unwrap_or_default();
            let owned: Vec<String> = bn.into_iter().map(|v| v.unwrap_or_default()).collect();
            let refs: Vec<&str> = owned.iter().map(String::as_str).collect();
            id_number(&refs)
        })
    }

    pub fn payees(&self) -> Option<Vec<i32>> {
        self.int("d_total_payees")
    }

    /// A numeric business column as R's `as.numeric(business_rows$col)` sees
    /// it: the values when the column is there, and a ZERO-LENGTH vector when
    /// it is not. R propagates that zero length through every arithmetic
    /// expression built on it, and the direct-call unit tests supply slices
    /// with thirteen columns rather than the spine's ninety, so the empty case
    /// is a live path, not a defensive one.
    pub fn numeric_or_empty(&self, name: &str) -> Vec<f64> {
        self.num(name).unwrap_or_default()
    }
}

/// The Mesh Block row picked for each business, already selected by the caller.
pub struct LocationRows {
    pub mb_code: Vec<Option<String>>,
    pub sa1_code: Vec<Option<String>>,
    pub sa2_code: Vec<Option<String>>,
}

/// The published domain values a variable can draw on.
pub struct DomainSet {
    /// `.blade_domain_values(variable_name = <this variable>)`.
    pub variable_values: Vec<String>,
    /// Domain name to values, for the named domains the admin cascade uses.
    pub named: HashMap<String, Vec<String>>,
}

impl DomainSet {
    pub fn domain(&self, name: &str) -> &[String] {
        self.named.get(name).map(Vec::as_slice).unwrap_or(&[])
    }
}

/// Everything derived from one variable's metadata, built once per column.
pub struct VariableSpec {
    pub name: String,
    pub lower: String,
    pub item_lower: String,
    pub valid_lower: String,
    /// The Valid.Response as published. The code-frame scanner reads this
    /// rather than the lowercased copy, because lowercasing is not
    /// length-preserving for every code point.
    pub valid_raw: String,
    /// `paste(lower, item_lower, valid_lower)`.
    pub context: String,
    /// `.blade_name_salt(name)`.
    pub salt: i64,
    /// `.stable_name_seed(name)`; a different hash, used by different branches.
    pub name_seed: i64,
}

impl VariableSpec {
    pub fn new(name: &str, item: &str, valid_response: &str) -> VariableSpec {
        let lower = name.to_lowercase();
        let item_lower = item.to_lowercase();
        let valid_lower = valid_response.to_lowercase();
        VariableSpec {
            context: format!("{} {} {}", lower, item_lower, valid_lower),
            salt: super::helpers::name_salt(name),
            name_seed: super::helpers::stable_name_seed(name),
            name: name.to_string(),
            valid_raw: valid_response.to_string(),
            lower,
            item_lower,
            valid_lower,
        }
    }
}

// R reads a business column one of two ways, and the two behave differently
// when the column is not there. A bare `business_rows$col` is NULL, and the
// frame silently loses that variable. Anything wrapped in `as.integer()`,
// `round()` or `sprintf()` is a ZERO-LENGTH vector instead, and `as.data.frame`
// then refuses to build the frame at all. The second is the better failure --
// a missing business column should stop the build, not quietly drop a
// published variable -- so both are reproduced exactly where R has them.
pub fn column_or_none(values: Option<Vec<Option<String>>>) -> BladeColumn {
    match values {
        Some(v) => BladeColumn::Chr(v),
        None => BladeColumn::None,
    }
}

pub fn some_all(values: &[&str]) -> Vec<Option<String>> {
    values.iter().map(|s| Some(s.to_string())).collect()
}

// ---------------------------------------------------------------------------
// extendr argument marshalling
// ---------------------------------------------------------------------------

pub fn strings_to_vec(values: &Strings) -> Vec<Option<String>> {
    values
        .iter()
        .map(|x| {
            if x.is_na() {
                None
            } else {
                Some(x.to_string())
            }
        })
        .collect()
}

pub fn strings_to_owned(values: &Strings) -> Vec<String> {
    values
        .iter()
        .filter(|x| !x.is_na())
        .map(|x| x.to_string())
        .collect()
}

pub fn location_from(mb: &Strings, sa1: &Strings, sa2: &Strings) -> Option<LocationRows> {
    if mb.is_empty() && sa1.is_empty() && sa2.is_empty() {
        return None;
    }
    Some(LocationRows {
        mb_code: strings_to_vec(mb),
        sa1_code: strings_to_vec(sa1),
        sa2_code: strings_to_vec(sa2),
    })
}

pub fn domain_set(variable_values: &Strings, domains: &List) -> DomainSet {
    let mut named: HashMap<String, Vec<String>> = HashMap::new();
    for (name, value) in domains.iter() {
        if let Ok(values) = Strings::try_from(value) {
            named.insert(name.to_string(), strings_to_owned(&values));
        }
    }
    DomainSet {
        variable_values: strings_to_owned(variable_values),
        named,
    }
}

pub fn resolved(
    available_periods: &Strings,
    reference_period: &str,
    t1_available_periods: &Strings,
    t1_reference_period: &str,
    table_number: i32,
) -> ResolvedPeriod {
    let ctx = PeriodContext {
        available_periods: strings_to_vec(available_periods),
        reference_period: reference_period.to_string(),
        t1_available_periods: strings_to_vec(t1_available_periods),
        t1_reference_period: t1_reference_period.to_string(),
    };
    ResolvedPeriod::resolve(&ctx, table_number)
}

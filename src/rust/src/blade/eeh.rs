//! BLADE table 17, Employee Earnings and Hours (Stage 4 of the R->Rust port).
//!
//! Ports `.make_blade_eeh_frame` from `R/generate_blade.R`. EEH is the one
//! BLADE table generated at EMPLOYEE level: one row per employee link row, not
//! per business, so the business columns arrive already re-indexed to link
//! order by the caller.
//!
//! The annual wage arrives from R already moved to the table's reference
//! period. That nominal factor stays in R deliberately: it hashes with
//! `.nominal_hash_normal`, which is a different function from the Rust nominal
//! module's, so recomputing it here would move every employee's earnings.

use std::collections::HashMap;

use extendr_api::prelude::*;

use super::helpers::{anzsco_group, deidentified_id, normalise_anzsco, round2};
use super::rows::{resolved, strings_to_vec, BladeColumn, BusinessRows, DomainSet, VariableSpec};
use super::tables::{value_for, ValueContext};

/// EEH age-category breakpoints, as UPPER bounds on age in completed years.
///
/// The April 2026 data item list publishes only the first label of the frame,
/// "1 = Under 18 years", so the under-18 floor is the published boundary. The
/// bands above it are a modelling choice -- ten-year bands to 64, then a
/// 65-and-over band. No source in this repository states the rest of the
/// frame, so nothing here should be read as the published one.
const AGE_CATEGORY_UPPER_BOUNDS: [i32; 6] = [17, 24, 34, 44, 54, 64];

/// R `grepl("anzsco[0-9]+_<k>_eeh", lower)` for k in 1..4.
fn anzsco_group_width(lower: &str) -> Option<usize> {
    let start = lower.find("anzsco")? + "anzsco".len();
    let rest = &lower[start..];
    let digits = rest
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(rest.len());
    if digits == 0 {
        return None;
    }
    let tail = &rest[digits..];
    (1..=4usize).find(|k| tail.starts_with(&format!("_{}_eeh", k)))
}

fn integers_to_vec(values: &[i32]) -> Vec<Option<i32>> {
    values
        .iter()
        .map(|v| if *v == i32::MIN { None } else { Some(*v) })
        .collect()
}

/// R's `paste` renders NA as the two-character string "NA", and the hashed
/// employee id is built with `paste`. A link column the caller did not supply
/// reads the same way rather than indexing past the end of an empty vector.
fn pasted(values: &[Option<String>], i: usize) -> &str {
    match values.get(i) {
        Some(Some(v)) => v.as_str(),
        _ => "NA",
    }
}

fn at(values: &[Option<String>], i: usize) -> Option<String> {
    values.get(i).cloned().flatten()
}

/// `.make_blade_eeh_frame` in Rust. Returns the frame as a named list of
/// columns; R stamps it into a data.frame.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn make_blade_eeh_frame__(
    variable_names: Strings,
    link_id: Strings,
    link_bg_id: Strings,
    link_bn: Strings,
    link_aeuid: Strings,
    link_job_number: Strings,
    link_anzsco: Strings,
    eeh_wage: &[f64],
    link_primary_job: &[i32],
    link_birth_year: &[i32],
    link_age: &[i32],
    link_sex: &[i32],
    business_state: &[i32],
    business: List,
    table_number: i32,
    seed: i32,
    available_periods: Strings,
    reference_period: &str,
    pinned_period: &str,
) -> List {
    let seed = seed as i64;
    let id = strings_to_vec(&link_id);
    let bg_id = strings_to_vec(&link_bg_id);
    let bn = strings_to_vec(&link_bn);
    let aeuid = strings_to_vec(&link_aeuid);
    let job_number = strings_to_vec(&link_job_number);
    let anzsco_raw = strings_to_vec(&link_anzsco);
    let n = id.len();
    let period = resolved(
        &available_periods,
        reference_period,
        pinned_period,
    );

    // R replaces a missing annual wage with zero before the nominal factor.
    let wage: Vec<f64> = eeh_wage
        .iter()
        .map(|w| if w.is_nan() { 0.0 } else { *w })
        .collect();
    let weekly: Vec<f64> = wage.iter().map(|w| round2(w / 52.0)).collect();
    let hourly: Vec<f64> = weekly.iter().map(|w| round2(w / 38.0)).collect();
    let anzsco: Vec<String> = anzsco_raw
        .iter()
        .map(|code| normalise_anzsco(code.as_deref()))
        .collect();
    let birth_year = integers_to_vec(link_birth_year);
    let link_age = integers_to_vec(link_age);
    let sex = integers_to_vec(link_sex);
    let state = integers_to_vec(business_state);
    // The employee's age at the table's reference year, from the spine birth
    // year where it is known and from the link's own age column otherwise.
    let age: Vec<Option<i32>> = (0..n)
        .map(|i| match birth_year.get(i).copied().flatten() {
            Some(b) => period.end_year.map(|y| y - b),
            None => link_age.get(i).copied().flatten(),
        })
        .collect();
    // A missing primary-job flag reads as the primary job, which is what a
    // single-job employee is.
    let primary: Vec<i32> = integers_to_vec(link_primary_job)
        .into_iter()
        .map(|v| v.unwrap_or(1))
        .collect();
    // "P" plus fourteen digits: fifteen characters, and never equal to a spine
    // person id, whose own shape is ten wide.
    let eid: Vec<String> = (0..n)
        .map(|i| {
            let value = format!(
                "{}|{}|{}",
                pasted(&aeuid, i),
                pasted(&bn, i),
                pasted(&job_number, i)
            );
            deidentified_id("P", &value, (i as i64) + 1, seed, 14)
        })
        .collect();

    let rows = BusinessRows::from_list(&business);
    let domains = DomainSet {
        variable_values: Vec::new(),
        named: HashMap::new(),
    };
    let seq = |i: usize| (i as i64) + 1;

    let mut names: Vec<String> = Vec::with_capacity(variable_names.len());
    let mut values: Vec<Robj> = Vec::with_capacity(variable_names.len());
    for name in variable_names.iter() {
        if name.is_na() {
            continue;
        }
        let name = name.to_string();
        let l = name.to_lowercase();
        let column = match l.as_str() {
            "id" => BladeColumn::Chr(id.clone()),
            "eeh_version" => BladeColumn::rep_str("in217_v1", n),
            "tsid" => BladeColumn::rep_str(&period.tsid, n),
            // A business inside an enterprise group reports under the group.
            "s_groupid_eeh" => BladeColumn::Chr(
                (0..n)
                    .map(|i| match at(&bg_id, i) {
                        Some(g) if !g.is_empty() => Some(g),
                        _ => at(&id, i),
                    })
                    .collect(),
            ),
            "eid_eeh" => BladeColumn::strs(eid.clone()),
            "swte_eeh" | "sawote_eeh" => BladeColumn::dbls(weekly.clone()),
            // Overtime earnings accrue on secondary jobs only.
            "wovte_eeh" => BladeColumn::dbls(
                (0..n)
                    .map(|i| round2(weekly[i] * (1 - primary[i]) as f64 * 0.15))
                    .collect(),
            ),
            "wass_eeh" => BladeColumn::dbls((0..n).map(|i| round2(weekly[i] * 0.03)).collect()),
            "sahte_eeh" | "sahote_eeh" => BladeColumn::dbls(hourly.clone()),
            "hovte_eeh" => BladeColumn::dbls(
                (0..n)
                    .map(|i| round2(hourly[i] * 1.5 * (1 - primary[i]) as f64))
                    .collect(),
            ),
            "totwhpf_eeh" => BladeColumn::dbls(
                primary
                    .iter()
                    .map(|p| if *p == 1 { 38.0 } else { 8.0 })
                    .collect(),
            ),
            "ordwhpf_eeh" => BladeColumn::dbls(
                primary
                    .iter()
                    .map(|p| if *p == 1 { 36.0 } else { 8.0 })
                    .collect(),
            ),
            "ovtwhpf_eeh" => BladeColumn::dbls(
                primary
                    .iter()
                    .map(|p| if *p == 1 { 2.0 } else { 0.0 })
                    .collect(),
            ),
            "agecat_eeh" => BladeColumn::Int(
                age.iter()
                    .map(|a| {
                        a.map(|years| {
                            AGE_CATEGORY_UPPER_BOUNDS
                                .iter()
                                .position(|upper| years <= *upper)
                                .unwrap_or(AGE_CATEGORY_UPPER_BOUNDS.len())
                                as i32
                                + 1
                        })
                    })
                    .collect(),
            ),
            "age_eeh" => BladeColumn::Int(age.clone()),
            "casload_eeh" => {
                BladeColumn::ints(primary.iter().map(|p| i32::from(*p == 0)).collect())
            }
            "ftpt_eeh" | "typeemp_eeh" => {
                BladeColumn::ints(primary.iter().map(|p| if *p == 1 { 1 } else { 2 }).collect())
            }
            "manage_eeh" => BladeColumn::ints(
                anzsco
                    .iter()
                    .map(|code| i32::from(code.starts_with('1')))
                    .collect(),
            ),
            "mannohrs_eeh" => {
                BladeColumn::ints(primary.iter().map(|p| i32::from(*p == 1)).collect())
            }
            "mosp_eeh" => BladeColumn::ints(
                (0..n)
                    .map(|i| ((seq(i) + seed).rem_euclid(6) as i32) + 1)
                    .collect(),
            ),
            // Pay frequency and rate of pay are numeric code frames in the data
            // item list ("1 = Weekly", "1 = Adult rate"), weighted here to the
            // dominant code.
            "payfreq_eeh" => BladeColumn::ints(
                (0..n)
                    .map(|i| {
                        let draw = (seq(i) * 31 + seed).rem_euclid(100);
                        if draw < 62 {
                            1
                        } else if draw < 92 {
                            2
                        } else if draw < 96 {
                            3
                        } else {
                            4
                        }
                    })
                    .collect(),
            ),
            "rop_eeh" => BladeColumn::ints(
                (0..n)
                    .map(|i| {
                        let draw = (seq(i) * 17 + seed).rem_euclid(100);
                        if draw < 82 {
                            1
                        } else if draw < 94 {
                            2
                        } else {
                            3
                        }
                    })
                    .collect(),
            ),
            "aj2012_eeh" | "stateops_eeh" => BladeColumn::rep_int(1, n),
            "sex_eeh" => BladeColumn::Int(sex.clone()),
            "empstate_eeh" | "state_eeh" => BladeColumn::Int(state.clone()),
            _ => match anzsco_group_width(&l) {
                Some(width) => BladeColumn::strs(
                    anzsco
                        .iter()
                        .map(|code| anzsco_group(Some(code), width))
                        .collect(),
                ),
                // Defensive: every published table 17 variable is matched
                // above, but a metadata change must not leave a column
                // unfilled -- that is how a placeholder shape leaks into a
                // product.
                None => {
                    let spec = VariableSpec::new(&name, "", "");
                    let ctx = ValueContext {
                        rows: &rows,
                        table_number,
                        seed,
                        period: &period,
                        domains: &domains,
                        location: None,
                        bas_wage_level: 1.0,
                    };
                    value_for(&spec, &ctx)
                }
            },
        };
        names.push(name);
        values.push(column.into_robj());
    }
    List::from_names_and_values(names, values).expect("EEH frame")
}

extendr_module! {
    mod eeh;
    fn make_blade_eeh_frame__;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn anzsco_group_names() {
        assert_eq!(anzsco_group_width("anzsco06_1_eeh"), Some(1));
        assert_eq!(anzsco_group_width("anzsco22_4_eeh"), Some(4));
        assert_eq!(anzsco_group_width("anzsco13_3_eeh"), Some(3));
        assert_eq!(anzsco_group_width("manage_eeh"), None);
        assert_eq!(anzsco_group_width("anzsco_1_eeh"), None);
    }

    #[test]
    fn age_categories_start_under_eighteen() {
        let band = |years: i32| {
            AGE_CATEGORY_UPPER_BOUNDS
                .iter()
                .position(|upper| years <= *upper)
                .unwrap_or(AGE_CATEGORY_UPPER_BOUNDS.len()) as i32
                + 1
        };
        assert_eq!(band(16), 1);
        assert_eq!(band(17), 1);
        assert_eq!(band(18), 2);
        assert_eq!(band(24), 2);
        assert_eq!(band(25), 3);
        assert_eq!(band(64), 6);
        assert_eq!(band(65), 7);
    }
}

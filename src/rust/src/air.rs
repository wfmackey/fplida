use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::mbs::{days_since_epoch, observable_days_in_year};

// NIP schedule: children get ~10-15 vaccination encounters by age 5
// Adults: ~2-3 (flu, COVID boosters)
// Antigen codes (simplified NIP schedule)
const CHILD_ANTIGENS: [&str; 12] = [
    "DTPA",
    "IPV",
    "HEP_B",
    "HIB",
    "PCV13",
    "ROTA",
    "MMR",
    "VAR",
    "MENACWY",
    "HPV",
    "DTPA_BOOST",
    "FLU_CHILD",
];
const ADULT_ANTIGENS: [&str; 4] = ["FLU", "COVID", "PNEU", "ZOSTER"];

fn child_schedule_date(birth_year: i32, birth_month: i32, age_months: i32, day: u32) -> i32 {
    let month_index = birth_month.clamp(1, 12) - 1 + age_months.max(0);
    let year = birth_year + month_index.div_euclid(12);
    let month = month_index.rem_euclid(12) as u32 + 1;
    days_since_epoch(year, month, day)
}

fn event_is_observable(
    event_date: i32,
    reference_year: i32,
    death_year: i32,
    death_month: i32,
    death_day: i32,
) -> bool {
    if event_date > days_since_epoch(reference_year, 12, 31) {
        return false;
    }
    if death_year == i32::MIN {
        return true;
    }
    let death_offset =
        observable_days_in_year(death_year, death_year, death_month, death_day).unwrap_or(0);
    event_date <= days_since_epoch(death_year, 1, 1) + death_offset
}

/// A deterministic hash for synthetic AIR identifiers. This is deliberately
/// simple and stable across platforms; it is not used for security.
fn stable_air_hash(seed: i64, parts: &[&str]) -> u64 {
    let mut hash = 14_695_981_039_346_656_037_u64 ^ seed.unsigned_abs();
    for part in parts {
        for byte in part.as_bytes() {
            hash ^= u64::from(*byte);
            hash = hash.wrapping_mul(1_099_511_628_211);
        }
        hash ^= 0xff;
        hash = hash.wrapping_mul(1_099_511_628_211);
    }
    hash
}

fn provider_group(antigen: &str) -> &'static str {
    match antigen {
        "FLU" | "FLU_CHILD" => "FLU",
        "COVID" => "COVID",
        "PNEU" | "ZOSTER" => "ADULT",
        _ => "CHILD",
    }
}

struct AirCanonicalColumns {
    spine_id: Vec<String>,
    vaccine_seq: Vec<String>,
    claim_id: Vec<String>,
    claim_seq: Vec<String>,
    encounter_seq: Vec<String>,
    provider_id: Vec<String>,
    practice_id: Vec<String>,
    batch_id: Vec<String>,
    received_date: Vec<i32>,
    processed_date: Vec<i32>,
    registered_state: Vec<String>,
}

impl AirCanonicalColumns {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            spine_id: Vec::with_capacity(capacity),
            vaccine_seq: Vec::with_capacity(capacity),
            claim_id: Vec::with_capacity(capacity),
            claim_seq: Vec::with_capacity(capacity),
            encounter_seq: Vec::with_capacity(capacity),
            provider_id: Vec::with_capacity(capacity),
            practice_id: Vec::with_capacity(capacity),
            batch_id: Vec::with_capacity(capacity),
            received_date: Vec::with_capacity(capacity),
            processed_date: Vec::with_capacity(capacity),
            registered_state: Vec::with_capacity(capacity),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn push(
        &mut self,
        spine_id: &str,
        aeuid: &str,
        antigen: &str,
        sequence: i32,
        encounter_date: i32,
        state: i32,
        seed: i64,
    ) {
        let state_code = state.clamp(1, 8);
        let state_text = state_code.to_string();
        let sequence_text = sequence.to_string();
        let encounter_text = encounter_date.to_string();
        let group = provider_group(antigen);
        let provider_bucket = stable_air_hash(seed, &[aeuid, group, &state_text]) % 64 + 1;
        let provider_id = format!("AIRP{}{:04}", state_code, provider_bucket);
        let practice_id = format!("AIRL{}{:04}", state_code, provider_bucket);
        let claim_hash = stable_air_hash(seed, &[aeuid, antigen, &sequence_text, &encounter_text]);
        let batch_period = encounter_date.div_euclid(30).to_string();
        let batch_hash = stable_air_hash(0, &[antigen, &batch_period]);
        let received_delay = (claim_hash % 5) as i32;
        let processing_delay = ((claim_hash >> 8) % 3) as i32;

        self.spine_id.push(spine_id.to_string());
        self.vaccine_seq.push(sequence_text.clone());
        self.claim_id.push(format!("AIRCLM{:016X}", claim_hash));
        self.claim_seq.push("1".to_string());
        self.encounter_seq.push(sequence_text);
        self.provider_id.push(provider_id);
        self.practice_id.push(practice_id);
        self.batch_id
            .push(format!("AIRB{:012X}", batch_hash & 0xFFFF_FFFF_FFFF));
        self.received_date.push(encounter_date + received_delay);
        self.processed_date
            .push(encounter_date + received_delay + processing_delay);
        self.registered_state.push(state_text);
    }
}

/// Project AIR (Australian Immunisation Register) from spine.
///
/// Vaccination event records. Children get NIP schedule (~12 events),
/// adults get periodic boosters (~2 events).
/// @export
#[extendr]
fn project_air__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    seed: i64,
    reference_year: i32,
) -> List {
    let n = aeuid.len();
    assert_eq!(year_of_death.len(), n);
    assert_eq!(month_of_death.len(), n);
    assert_eq!(day_of_death.len(), n);
    let mut rng = StdRng::seed_from_u64(seed as u64);

    // ~10 events per child + ~2 per adult = ~4 average per person
    let est_rows = n * 4;

    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_dob_month: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_dob_year: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_sex: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_indigenous: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_encounter_date: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_vaccine_seq: Vec<i32> = Vec::with_capacity(est_rows);
    let mut out_antigen: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_status: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_schedule: Vec<String> = Vec::with_capacity(est_rows);
    let mut out_state: Vec<i32> = Vec::with_capacity(est_rows);

    for i in 0..n {
        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let age = reference_year - by;
        let sex_str = if sex[i] == 1 { "M" } else { "F" };
        let ind_str = if indigenous[i] >= 2 && indigenous[i] <= 4 {
            "Y"
        } else {
            "N"
        };
        let dob_m = rng.gen_range(1..=12);

        let mut seq = 0i32;

        // Child vaccinations (NIP schedule)
        if age >= 0 && age <= 18 {
            // Number of vaccinations depends on age
            let n_child = if age < 2 {
                6
            } else if age < 5 {
                10
            } else {
                12
            };
            let n_vacc = n_child.min(CHILD_ANTIGENS.len());

            for v in 0..n_vacc {
                // Schedule age in months for each antigen
                let age_months = match v {
                    0..=2 => rng.gen_range(1..=4),   // 2-4 months
                    3..=5 => rng.gen_range(4..=8),   // 4-8 months
                    6..=7 => rng.gen_range(12..=18), // 12-18 months
                    8..=9 => rng.gen_range(48..=60), // 4-5 years
                    _ => rng.gen_range(132..=180),   // 11-15 years
                };
                let enc_date =
                    child_schedule_date(by, dob_m, age_months, rng.gen_range(1..=28) as u32);
                if !event_is_observable(
                    enc_date,
                    reference_year,
                    year_of_death[i],
                    month_of_death[i],
                    day_of_death[i],
                ) {
                    continue;
                }
                seq += 1;

                out_aeuid.push(person_aeuid.clone());
                out_dob_month.push(dob_m);
                out_dob_year.push(by);
                out_sex.push(sex_str.to_string());
                out_indigenous.push(ind_str.to_string());
                out_encounter_date.push(enc_date);
                out_vaccine_seq.push(seq);
                out_antigen.push(CHILD_ANTIGENS[v].to_string());
                out_status.push("Valid".to_string());
                out_schedule.push("NIP".to_string());
                out_state.push(state[i]);
            }
        }

        // Adult vaccinations (flu, COVID)
        if age >= 18 {
            // Flu: ~40% get annual flu shot
            if rng.gen::<f64>() < 0.40 {
                for yr in (reference_year - 2)..=reference_year {
                    let enc_date = days_since_epoch(
                        yr,
                        rng.gen_range(3..=5) as u32,
                        rng.gen_range(1..=28) as u32,
                    );
                    if !event_is_observable(
                        enc_date,
                        reference_year,
                        year_of_death[i],
                        month_of_death[i],
                        day_of_death[i],
                    ) {
                        continue;
                    }
                    seq += 1;
                    out_aeuid.push(person_aeuid.clone());
                    out_dob_month.push(dob_m);
                    out_dob_year.push(by);
                    out_sex.push(sex_str.to_string());
                    out_indigenous.push(ind_str.to_string());
                    out_encounter_date.push(enc_date);
                    out_vaccine_seq.push(seq);
                    out_antigen.push("FLU".to_string());
                    out_status.push("Valid".to_string());
                    out_schedule.push("NIP".to_string());
                    out_state.push(state[i]);
                }
            }

            // COVID: ~85% got at least 2 doses (2021-2023)
            if rng.gen::<f64>() < 0.85 {
                for dose in 1..=rng.gen_range(2..=4i32) {
                    let yr = 2021 + (dose - 1).min(2);
                    let enc_date = days_since_epoch(
                        yr,
                        rng.gen_range(1..=12) as u32,
                        rng.gen_range(1..=28) as u32,
                    );
                    if !event_is_observable(
                        enc_date,
                        reference_year,
                        year_of_death[i],
                        month_of_death[i],
                        day_of_death[i],
                    ) {
                        continue;
                    }
                    seq += 1;
                    out_aeuid.push(person_aeuid.clone());
                    out_dob_month.push(dob_m);
                    out_dob_year.push(by);
                    out_sex.push(sex_str.to_string());
                    out_indigenous.push(ind_str.to_string());
                    out_encounter_date.push(enc_date);
                    out_vaccine_seq.push(seq);
                    out_antigen.push("COVID".to_string());
                    out_status.push("Valid".to_string());
                    out_schedule.push("NIP".to_string());
                    out_state.push(state[i]);
                }
            }
        }
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        DOB_MONTH = out_dob_month,
        DOB_YEAR = out_dob_year,
        SEXCDE = out_sex,
        ABORIND = out_indigenous,
        ENCOUNTER_DATE = out_encounter_date,
        VACCINE_SEQUENCE = out_vaccine_seq,
        ANTIGEN_CODE = out_antigen,
        EPISODE_STATUS = out_status,
        SCHEDULE_CLASSIFICATION = out_schedule,
        STATE_ASGS_2021 = out_state
    )
}

/// Project AIR directly to parquet. Single file.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_air_to_parquet__(
    aeuid: Strings,
    spine_id: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    year_of_death: &[i32],
    month_of_death: &[i32],
    day_of_death: &[i32],
    seed: i64,
    reference_year: i32,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let n = aeuid.len();
    assert_eq!(spine_id.len(), n);
    assert_eq!(year_of_death.len(), n);
    assert_eq!(month_of_death.len(), n);
    assert_eq!(day_of_death.len(), n);
    let mut rng = StdRng::seed_from_u64(seed as u64);
    let mut canonical = AirCanonicalColumns::with_capacity(n * 4);

    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_dob_month: Vec<i32> = Vec::new();
    let mut out_dob_year: Vec<i32> = Vec::new();
    let mut out_sex: Vec<String> = Vec::new();
    let mut out_indigenous: Vec<String> = Vec::new();
    let mut out_encounter_date: Vec<i32> = Vec::new();
    let mut out_vaccine_seq: Vec<i32> = Vec::new();
    let mut out_antigen: Vec<String> = Vec::new();
    let mut out_status: Vec<String> = Vec::new();
    let mut out_schedule: Vec<String> = Vec::new();
    let mut out_state: Vec<i32> = Vec::new();

    for i in 0..n {
        let person_aeuid = aeuid[i].to_string();
        let by = birth_year[i];
        let age = reference_year - by;
        let sex_str = if sex[i] == 1 { "M" } else { "F" };
        let ind_str = if indigenous[i] >= 2 && indigenous[i] <= 4 {
            "Y"
        } else {
            "N"
        };
        let dob_m = rng.gen_range(1..=12);
        let mut seq = 0i32;

        if age >= 0 && age <= 18 {
            let n_child = if age < 2 {
                6
            } else if age < 5 {
                10
            } else {
                12
            };
            let n_vacc = n_child.min(CHILD_ANTIGENS.len());
            for v in 0..n_vacc {
                let age_months = match v {
                    0..=2 => rng.gen_range(1..=4),
                    3..=5 => rng.gen_range(4..=8),
                    6..=7 => rng.gen_range(12..=18),
                    8..=9 => rng.gen_range(48..=60),
                    _ => rng.gen_range(132..=180),
                };
                let enc_date =
                    child_schedule_date(by, dob_m, age_months, rng.gen_range(1..=28) as u32);
                if !event_is_observable(
                    enc_date,
                    reference_year,
                    year_of_death[i],
                    month_of_death[i],
                    day_of_death[i],
                ) {
                    continue;
                }
                seq += 1;
                out_aeuid.push(person_aeuid.clone());
                out_dob_month.push(dob_m);
                out_dob_year.push(by);
                out_sex.push(sex_str.to_string());
                out_indigenous.push(ind_str.to_string());
                out_encounter_date.push(enc_date);
                out_vaccine_seq.push(seq);
                out_antigen.push(CHILD_ANTIGENS[v].to_string());
                out_status.push("Valid".to_string());
                out_schedule.push("NIP".to_string());
                out_state.push(state[i]);
                canonical.push(
                    &spine_id[i],
                    &person_aeuid,
                    CHILD_ANTIGENS[v],
                    seq,
                    enc_date,
                    state[i],
                    seed,
                );
            }
        }

        if age >= 18 {
            if rng.gen::<f64>() < 0.40 {
                for yr in (reference_year - 2)..=reference_year {
                    let enc_date = days_since_epoch(
                        yr,
                        rng.gen_range(3..=5) as u32,
                        rng.gen_range(1..=28) as u32,
                    );
                    if !event_is_observable(
                        enc_date,
                        reference_year,
                        year_of_death[i],
                        month_of_death[i],
                        day_of_death[i],
                    ) {
                        continue;
                    }
                    seq += 1;
                    out_aeuid.push(person_aeuid.clone());
                    out_dob_month.push(dob_m);
                    out_dob_year.push(by);
                    out_sex.push(sex_str.to_string());
                    out_indigenous.push(ind_str.to_string());
                    out_encounter_date.push(enc_date);
                    out_vaccine_seq.push(seq);
                    out_antigen.push("FLU".to_string());
                    out_status.push("Valid".to_string());
                    out_schedule.push("NIP".to_string());
                    out_state.push(state[i]);
                    canonical.push(
                        &spine_id[i],
                        &person_aeuid,
                        "FLU",
                        seq,
                        enc_date,
                        state[i],
                        seed,
                    );
                }
            }
            if rng.gen::<f64>() < 0.85 {
                for dose in 1..=rng.gen_range(2..=4i32) {
                    let yr = 2021 + (dose - 1).min(2);
                    let enc_date = days_since_epoch(
                        yr,
                        rng.gen_range(1..=12) as u32,
                        rng.gen_range(1..=28) as u32,
                    );
                    if !event_is_observable(
                        enc_date,
                        reference_year,
                        year_of_death[i],
                        month_of_death[i],
                        day_of_death[i],
                    ) {
                        continue;
                    }
                    seq += 1;
                    out_aeuid.push(person_aeuid.clone());
                    out_dob_month.push(dob_m);
                    out_dob_year.push(by);
                    out_sex.push(sex_str.to_string());
                    out_indigenous.push(ind_str.to_string());
                    out_encounter_date.push(enc_date);
                    out_vaccine_seq.push(seq);
                    out_antigen.push("COVID".to_string());
                    out_status.push("Valid".to_string());
                    out_schedule.push("NIP".to_string());
                    out_state.push(state[i]);
                    canonical.push(
                        &spine_id[i],
                        &person_aeuid,
                        "COVID",
                        seq,
                        enc_date,
                        state[i],
                        seed,
                    );
                }
            }
        }
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "DOB_MONTH",
            col: Col::I32(out_dob_month),
        },
        NamedCol {
            name: "DOB_YEAR",
            col: Col::I32(out_dob_year),
        },
        NamedCol {
            name: "SEXCDE",
            col: Col::Str(out_sex),
        },
        NamedCol {
            name: "ABORIND",
            col: Col::Str(out_indigenous),
        },
        NamedCol {
            name: "ENCOUNTER_DATE",
            col: Col::DateNN(out_encounter_date.clone()),
        },
        NamedCol {
            name: "ENCDATE",
            col: Col::DateNN(out_encounter_date),
        },
        NamedCol {
            name: "RCPTDATE",
            col: Col::DateNN(canonical.received_date),
        },
        NamedCol {
            name: "EPSPROC",
            col: Col::DateNN(canonical.processed_date),
        },
        NamedCol {
            name: "VACCINE_SEQUENCE",
            col: Col::I32(out_vaccine_seq),
        },
        NamedCol {
            name: "VACCSEQ",
            col: Col::Str(canonical.vaccine_seq),
        },
        NamedCol {
            name: "ANTIGEN_CODE",
            col: Col::Str(out_antigen.clone()),
        },
        NamedCol {
            name: "ANTGNCDE",
            col: Col::Str(out_antigen),
        },
        NamedCol {
            name: "EPISODE_STATUS",
            col: Col::Str(out_status.clone()),
        },
        NamedCol {
            name: "EPSSTAT",
            col: Col::Str(out_status),
        },
        NamedCol {
            name: "SCHEDULE_CLASSIFICATION",
            col: Col::Str(out_schedule.clone()),
        },
        NamedCol {
            name: "SCHEDCDE",
            col: Col::Str(out_schedule),
        },
        NamedCol {
            name: "STATE_ASGS_2021",
            col: Col::I32(out_state),
        },
        NamedCol {
            name: "REGSTECD",
            col: Col::Str(canonical.registered_state),
        },
        NamedCol {
            name: "CLAIMID",
            col: Col::Str(canonical.claim_id),
        },
        NamedCol {
            name: "CLAIMSEQ",
            col: Col::Str(canonical.claim_seq),
        },
        NamedCol {
            name: "ENCSEQ",
            col: Col::Str(canonical.encounter_seq),
        },
        NamedCol {
            name: "BATCHNUM",
            col: Col::Str(canonical.batch_id),
        },
        NamedCol {
            name: "CPROVNUM",
            col: Col::Str(canonical.provider_id.clone()),
        },
        NamedCol {
            name: "PROVNUM",
            col: Col::Str(canonical.provider_id.clone()),
        },
        NamedCol {
            name: "CIMMPROV",
            col: Col::Str(canonical.provider_id.clone()),
        },
        NamedCol {
            name: "IMMPROVN",
            col: Col::Str(canonical.provider_id.clone()),
        },
        NamedCol {
            name: "RPLCD_ID",
            col: Col::Str(canonical.provider_id),
        },
        NamedCol {
            name: "PRACLOCN",
            col: Col::Str(canonical.practice_id.clone()),
        },
        NamedCol {
            name: "IMMPLOCN",
            col: Col::Str(canonical.practice_id),
        },
        NamedCol {
            name: "SPINE_V4_ID",
            col: Col::Str(canonical.spine_id.clone()),
        },
        NamedCol {
            name: "SPINE_V5_ID",
            col: Col::Str(canonical.spine_id.clone()),
        },
        NamedCol {
            name: "SPINE_V6_ID",
            col: Col::Str(canonical.spine_id.clone()),
        },
        NamedCol {
            name: "SPINE_V7_ID",
            col: Col::Str(canonical.spine_id.clone()),
        },
        NamedCol {
            name: "SPINE_V8_ID",
            col: Col::Str(canonical.spine_id.clone()),
        },
        NamedCol {
            name: "SPINE_V9_ID",
            col: Col::Str(canonical.spine_id),
        },
    ];
    write_columns_to_parquet(out_path, cols).unwrap_or_else(|e| panic!("air parquet write: {}", e));
    total
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_event_values_are_stable_and_coherent() {
        let mut first = AirCanonicalColumns::with_capacity(1);
        let mut second = AirCanonicalColumns::with_capacity(1);
        first.push("SP0001", "AE0001", "COVID", 3, 19_000, 2, 71);
        second.push("SP0001", "AE0001", "COVID", 3, 19_000, 2, 71);

        assert_eq!(first.claim_id, second.claim_id);
        assert_eq!(first.provider_id, second.provider_id);
        assert_eq!(first.practice_id, second.practice_id);
        assert_eq!(first.batch_id, second.batch_id);
        assert_eq!(first.vaccine_seq[0], "3");
        assert_eq!(first.encounter_seq[0], "3");
        assert_eq!(first.claim_seq[0], "1");
        assert!(first.provider_id[0].starts_with("AIRP2"));
        assert!(first.practice_id[0].starts_with("AIRL2"));
        assert!(first.received_date[0] >= 19_000);
        assert!(first.processed_date[0] >= first.received_date[0]);
        assert_eq!(first.registered_state[0], "2");
    }

    #[test]
    fn schedule_dates_roll_across_years_and_stop_at_death() {
        assert_eq!(
            child_schedule_date(2020, 11, 4, 15),
            days_since_epoch(2021, 3, 15)
        );
        let before_death = days_since_epoch(2022, 5, 9);
        let after_death = days_since_epoch(2022, 5, 11);
        assert!(event_is_observable(before_death, 2024, 2022, 5, 10));
        assert!(!event_is_observable(after_death, 2024, 2022, 5, 10));
        assert!(!event_is_observable(
            days_since_epoch(2025, 1, 1),
            2024,
            i32::MIN,
            i32::MIN,
            i32::MIN,
        ));
    }
}

extendr_module! {
    mod air;
    fn project_air__;
    fn project_air_to_parquet__;
}

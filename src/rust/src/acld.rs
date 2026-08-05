use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::sampling::normal_sample;

// ACLD: 5% panel of Census respondents, wide format with wave suffixes
// Linkage rates: ~77% 2011→2016, ~83% 2016→2021

const SAMPLE_RATE: f64 = 0.05;
const LINK_RATE_11_16: f64 = 0.77;
const LINK_RATE_16_21: f64 = 0.83;

// The official ACLD detailed-microdata workbooks use variable-width
// "Unlinked record" codes, rather than one generic sentinel.
const UNLINKED_2D: i32 = 99;
const UNLINKED_3D: i32 = 999;

/// Project ACLD (Australian Census Longitudinal Dataset) from spine.
///
/// 5% sample, wide format: person-level Census variables for each wave
/// (2011, 2016, 2021) with wave suffixes. Unlinked waves use each field's
/// official ACLD code (99 for two-digit fields and 999 for three-digit fields).
/// @export
#[extendr]
fn project_acld__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    education: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    seed: i64,
) -> List {
    let _ = (state, country_of_birth);
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let est_n = (n as f64 * SAMPLE_RATE) as usize;

    // Person-level output (one row per person in sample)
    let mut out_aeuid: Vec<String> = Vec::with_capacity(est_n);

    // 2011 wave variables
    let mut out_agep_11: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_sexp_11: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_ingp_11: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_mstp_11: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_lfsp_11: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_hscp_11: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_incp_11: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_assnp_11: Vec<i32> = Vec::with_capacity(est_n);

    // 2016 wave variables
    let mut out_agep_16: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_sexp_16: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_ingp_16: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_mstp_16: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_lfsp_16: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_hscp_16: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_incp_16: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_assnp_16: Vec<i32> = Vec::with_capacity(est_n);

    // 2021 wave variables
    let mut out_agep_21: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_sexp_21: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_ingp_21: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_mstp_21: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_lfsp_21: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_hscp_21: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_incp_21: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_assnp_21: Vec<i32> = Vec::with_capacity(est_n);

    // Link flags and weights
    let mut out_linked_16: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_linked_21: Vec<i32> = Vec::with_capacity(est_n);
    let mut out_weight: Vec<f64> = Vec::with_capacity(est_n);

    for i in 0..n {
        // 5% sample
        if rng.gen::<f64>() >= SAMPLE_RATE {
            continue;
        }

        let by = birth_year[i];
        let sx = sex[i];
        let ind = indigenous[i];
        let edu = education[i];
        let employed = baseline_employed[i] == 1;

        let person_aeuid = aeuid[i].to_string();

        // Linkage: does this person link across waves?
        let linked_to_16 = rng.gen::<f64>() < LINK_RATE_11_16;
        let linked_to_21 = linked_to_16 && rng.gen::<f64>() < LINK_RATE_16_21;

        // Helper: generate wave-specific values
        // Age
        let age_11 = 2011 - by;
        let age_16 = if linked_to_16 { 2016 - by } else { UNLINKED_3D };
        let age_21 = if linked_to_21 { 2021 - by } else { UNLINKED_3D };

        // Sex (stable)
        let sex_16 = if linked_to_16 { sx } else { UNLINKED_2D };
        let sex_21 = if linked_to_21 { sx } else { UNLINKED_2D };

        // Indigenous (stable)
        let ind_16 = if linked_to_16 { ind } else { UNLINKED_2D };
        let ind_21 = if linked_to_21 { ind } else { UNLINKED_2D };

        // Marital status (age-dependent, can change)
        let marital_for_age = |age: i32, rng: &mut StdRng| -> i32 {
            if age < 18 {
                1
            }
            // never married
            else if age < 30 {
                if rng.gen::<f64>() < 0.60 {
                    1
                } else {
                    5
                }
            } else if age < 60 {
                if rng.gen::<f64>() < 0.55 {
                    5
                } else if rng.gen::<f64>() < 0.30 {
                    3
                } else {
                    1
                }
            } else {
                if rng.gen::<f64>() < 0.45 {
                    5
                } else if rng.gen::<f64>() < 0.40 {
                    2
                } else {
                    3
                }
            }
        };
        let mstp_11 = marital_for_age(age_11, &mut rng);
        let mstp_16 = if linked_to_16 {
            marital_for_age(age_16, &mut rng)
        } else {
            UNLINKED_2D
        };
        let mstp_21 = if linked_to_21 {
            marital_for_age(age_21, &mut rng)
        } else {
            UNLINKED_2D
        };

        // LFS (can change across waves)
        let lfs_for_person = |employed: bool, age: i32, rng: &mut StdRng| -> i32 {
            if age < 15 || age > 75 {
                6
            }
            // NILF
            else if employed {
                if rng.gen::<f64>() < 0.70 {
                    1
                } else {
                    2
                }
            }
            // FT/PT
            else {
                if rng.gen::<f64>() < 0.30 {
                    4
                } else {
                    6
                }
            } // Unemp/NILF
        };
        let lfsp_11 = lfs_for_person(employed, age_11, &mut rng);
        let lfsp_16 = if linked_to_16 {
            lfs_for_person(employed, age_16, &mut rng)
        } else {
            UNLINKED_2D
        };
        let lfsp_21 = if linked_to_21 {
            lfs_for_person(employed, age_21, &mut rng)
        } else {
            UNLINKED_2D
        };

        // Highest school year (stable or improving)
        let school_from_edu = |edu: i32| -> i32 {
            match edu {
                1 => 5, // year 8 or below
                2 => 3, // year 10
                3 => 1, // year 12
                _ => 1, // post-school implies year 12
            }
        };
        let hscp_11 = school_from_edu(edu);
        let hscp_16 = if linked_to_16 { hscp_11 } else { UNLINKED_2D };
        let hscp_21 = if linked_to_21 { hscp_11 } else { UNLINKED_2D };

        // Income quintile (changes with employment/age)
        let inc_quintile = |income: f64, rng: &mut StdRng| -> i32 {
            if income <= 0.0 {
                1
            } else if income < 30000.0 {
                rng.gen_range(1..=2)
            } else if income < 60000.0 {
                rng.gen_range(2..=3)
            } else if income < 100000.0 {
                rng.gen_range(3..=4)
            } else {
                rng.gen_range(4..=5)
            }
        };
        let base_inc = baseline_income[i];
        let incp_11 = inc_quintile(base_inc * 0.85, &mut rng);
        let incp_16 = if linked_to_16 {
            inc_quintile(base_inc * 0.95, &mut rng)
        } else {
            UNLINKED_3D
        };
        let incp_21 = if linked_to_21 {
            inc_quintile(base_inc, &mut rng)
        } else {
            UNLINKED_3D
        };

        // Core activity need for assistance (2 = no need for most)
        let assnp_11 = if rng.gen::<f64>() < 0.05 { 1 } else { 2 };
        let assnp_16 = if linked_to_16 {
            if rng.gen::<f64>() < 0.06 {
                1
            } else {
                2
            }
        } else {
            UNLINKED_2D
        };
        let assnp_21 = if linked_to_21 {
            if rng.gen::<f64>() < 0.07 {
                1
            } else {
                2
            }
        } else {
            UNLINKED_2D
        };

        // Weight: ~20 for the 5% sample
        let weight = normal_sample(&mut rng, 20.0, 3.0).max(5.0);

        out_aeuid.push(person_aeuid);
        out_agep_11.push(age_11);
        out_sexp_11.push(sx);
        out_ingp_11.push(ind);
        out_mstp_11.push(mstp_11);
        out_lfsp_11.push(lfsp_11);
        out_hscp_11.push(hscp_11);
        out_incp_11.push(incp_11);
        out_assnp_11.push(assnp_11);
        out_agep_16.push(age_16);
        out_sexp_16.push(sex_16);
        out_ingp_16.push(ind_16);
        out_mstp_16.push(mstp_16);
        out_lfsp_16.push(lfsp_16);
        out_hscp_16.push(hscp_16);
        out_incp_16.push(incp_16);
        out_assnp_16.push(assnp_16);
        out_agep_21.push(age_21);
        out_sexp_21.push(sex_21);
        out_ingp_21.push(ind_21);
        out_mstp_21.push(mstp_21);
        out_lfsp_21.push(lfsp_21);
        out_hscp_21.push(hscp_21);
        out_incp_21.push(incp_21);
        out_assnp_21.push(assnp_21);
        out_linked_16.push(if linked_to_16 { 1 } else { 2 });
        out_linked_21.push(if linked_to_21 { 1 } else { 2 });
        out_weight.push((weight * 100.0).round() / 100.0);
    }

    list!(
        SYNTHETIC_AEUID = out_aeuid,
        AGEP_11 = out_agep_11,
        SEXP_11 = out_sexp_11,
        INGP_11 = out_ingp_11,
        MSTP_11 = out_mstp_11,
        LFSP_11 = out_lfsp_11,
        HSCP_11 = out_hscp_11,
        INCP_11 = out_incp_11,
        ASSNP_11 = out_assnp_11,
        AGEP_16 = out_agep_16,
        SEXP_16 = out_sexp_16,
        INGP_16 = out_ingp_16,
        MSTP_16 = out_mstp_16,
        LFSP_16 = out_lfsp_16,
        HSCP_16 = out_hscp_16,
        INCP_16 = out_incp_16,
        ASSNP_16 = out_assnp_16,
        AGEP_21 = out_agep_21,
        SEXP_21 = out_sexp_21,
        INGP_21 = out_ingp_21,
        MSTP_21 = out_mstp_21,
        LFSP_21 = out_lfsp_21,
        HSCP_21 = out_hscp_21,
        INCP_21 = out_incp_21,
        ASSNP_21 = out_assnp_21,
        LINKFLAG_16 = out_linked_16,
        LINKFLAG_21 = out_linked_21,
        WEIGHT4 = out_weight
    )
}

/// Project ACLD directly to parquet. Single file.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_acld_to_parquet__(
    aeuid: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth: &[i32],
    education: &[i32],
    baseline_employed: &[i32],
    baseline_income: &[f64],
    seed: i64,
    out_path: &str,
) -> i32 {
    use crate::parquet_io::{write_columns_to_parquet, Col, NamedCol};
    let _ = (state, country_of_birth);
    let n = aeuid.len();
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut out_aeuid: Vec<String> = Vec::new();
    let mut out_agep_11: Vec<i32> = Vec::new();
    let mut out_sexp_11: Vec<i32> = Vec::new();
    let mut out_ingp_11: Vec<i32> = Vec::new();
    let mut out_mstp_11: Vec<i32> = Vec::new();
    let mut out_lfsp_11: Vec<i32> = Vec::new();
    let mut out_hscp_11: Vec<i32> = Vec::new();
    let mut out_incp_11: Vec<i32> = Vec::new();
    let mut out_assnp_11: Vec<i32> = Vec::new();
    let mut out_agep_16: Vec<i32> = Vec::new();
    let mut out_sexp_16: Vec<i32> = Vec::new();
    let mut out_ingp_16: Vec<i32> = Vec::new();
    let mut out_mstp_16: Vec<i32> = Vec::new();
    let mut out_lfsp_16: Vec<i32> = Vec::new();
    let mut out_hscp_16: Vec<i32> = Vec::new();
    let mut out_incp_16: Vec<i32> = Vec::new();
    let mut out_assnp_16: Vec<i32> = Vec::new();
    let mut out_agep_21: Vec<i32> = Vec::new();
    let mut out_sexp_21: Vec<i32> = Vec::new();
    let mut out_ingp_21: Vec<i32> = Vec::new();
    let mut out_mstp_21: Vec<i32> = Vec::new();
    let mut out_lfsp_21: Vec<i32> = Vec::new();
    let mut out_hscp_21: Vec<i32> = Vec::new();
    let mut out_incp_21: Vec<i32> = Vec::new();
    let mut out_assnp_21: Vec<i32> = Vec::new();
    let mut out_linked_16: Vec<i32> = Vec::new();
    let mut out_linked_21: Vec<i32> = Vec::new();
    let mut out_weight: Vec<f64> = Vec::new();

    for i in 0..n {
        if rng.gen::<f64>() >= SAMPLE_RATE {
            continue;
        }
        let by = birth_year[i];
        let sx = sex[i];
        let ind = indigenous[i];
        let edu = education[i];
        let employed = baseline_employed[i] == 1;

        let linked_to_16 = rng.gen::<f64>() < LINK_RATE_11_16;
        let linked_to_21 = linked_to_16 && rng.gen::<f64>() < LINK_RATE_16_21;
        let age_11 = 2011 - by;
        let age_16 = if linked_to_16 { 2016 - by } else { UNLINKED_3D };
        let age_21 = if linked_to_21 { 2021 - by } else { UNLINKED_3D };
        let sex_16 = if linked_to_16 { sx } else { UNLINKED_2D };
        let sex_21 = if linked_to_21 { sx } else { UNLINKED_2D };
        let ind_16 = if linked_to_16 { ind } else { UNLINKED_2D };
        let ind_21 = if linked_to_21 { ind } else { UNLINKED_2D };
        let marital_for_age = |age: i32, rng: &mut StdRng| -> i32 {
            if age < 18 {
                1
            } else if age < 30 {
                if rng.gen::<f64>() < 0.60 {
                    1
                } else {
                    5
                }
            } else if age < 60 {
                if rng.gen::<f64>() < 0.55 {
                    5
                } else if rng.gen::<f64>() < 0.30 {
                    3
                } else {
                    1
                }
            } else {
                if rng.gen::<f64>() < 0.45 {
                    5
                } else if rng.gen::<f64>() < 0.40 {
                    2
                } else {
                    3
                }
            }
        };
        let mstp_11 = marital_for_age(age_11, &mut rng);
        let mstp_16 = if linked_to_16 {
            marital_for_age(age_16, &mut rng)
        } else {
            UNLINKED_2D
        };
        let mstp_21 = if linked_to_21 {
            marital_for_age(age_21, &mut rng)
        } else {
            UNLINKED_2D
        };
        let lfs_for_person = |employed: bool, age: i32, rng: &mut StdRng| -> i32 {
            if age < 15 || age > 75 {
                6
            } else if employed {
                if rng.gen::<f64>() < 0.70 {
                    1
                } else {
                    2
                }
            } else {
                if rng.gen::<f64>() < 0.30 {
                    4
                } else {
                    6
                }
            }
        };
        let lfsp_11 = lfs_for_person(employed, age_11, &mut rng);
        let lfsp_16 = if linked_to_16 {
            lfs_for_person(employed, age_16, &mut rng)
        } else {
            UNLINKED_2D
        };
        let lfsp_21 = if linked_to_21 {
            lfs_for_person(employed, age_21, &mut rng)
        } else {
            UNLINKED_2D
        };
        let school_from_edu = |edu: i32| -> i32 {
            match edu {
                1 => 5,
                2 => 3,
                3 => 1,
                _ => 1,
            }
        };
        let hscp_11 = school_from_edu(edu);
        let hscp_16 = if linked_to_16 { hscp_11 } else { UNLINKED_2D };
        let hscp_21 = if linked_to_21 { hscp_11 } else { UNLINKED_2D };
        let inc_quintile = |income: f64, rng: &mut StdRng| -> i32 {
            if income <= 0.0 {
                1
            } else if income < 30000.0 {
                rng.gen_range(1..=2)
            } else if income < 60000.0 {
                rng.gen_range(2..=3)
            } else if income < 100000.0 {
                rng.gen_range(3..=4)
            } else {
                rng.gen_range(4..=5)
            }
        };
        let base_inc = baseline_income[i];
        let incp_11 = inc_quintile(base_inc * 0.85, &mut rng);
        let incp_16 = if linked_to_16 {
            inc_quintile(base_inc * 0.95, &mut rng)
        } else {
            UNLINKED_3D
        };
        let incp_21 = if linked_to_21 {
            inc_quintile(base_inc, &mut rng)
        } else {
            UNLINKED_3D
        };
        let assnp_11 = if rng.gen::<f64>() < 0.05 { 1 } else { 2 };
        let assnp_16 = if linked_to_16 {
            if rng.gen::<f64>() < 0.06 {
                1
            } else {
                2
            }
        } else {
            UNLINKED_2D
        };
        let assnp_21 = if linked_to_21 {
            if rng.gen::<f64>() < 0.07 {
                1
            } else {
                2
            }
        } else {
            UNLINKED_2D
        };
        let weight = normal_sample(&mut rng, 20.0, 3.0).max(5.0);

        out_aeuid.push(aeuid[i].to_string());
        out_agep_11.push(age_11);
        out_sexp_11.push(sx);
        out_ingp_11.push(ind);
        out_mstp_11.push(mstp_11);
        out_lfsp_11.push(lfsp_11);
        out_hscp_11.push(hscp_11);
        out_incp_11.push(incp_11);
        out_assnp_11.push(assnp_11);
        out_agep_16.push(age_16);
        out_sexp_16.push(sex_16);
        out_ingp_16.push(ind_16);
        out_mstp_16.push(mstp_16);
        out_lfsp_16.push(lfsp_16);
        out_hscp_16.push(hscp_16);
        out_incp_16.push(incp_16);
        out_assnp_16.push(assnp_16);
        out_agep_21.push(age_21);
        out_sexp_21.push(sex_21);
        out_ingp_21.push(ind_21);
        out_mstp_21.push(mstp_21);
        out_lfsp_21.push(lfsp_21);
        out_hscp_21.push(hscp_21);
        out_incp_21.push(incp_21);
        out_assnp_21.push(assnp_21);
        out_linked_16.push(if linked_to_16 { 1 } else { 2 });
        out_linked_21.push(if linked_to_21 { 1 } else { 2 });
        out_weight.push((weight * 100.0).round() / 100.0);
    }

    let total = out_aeuid.len() as i32;
    let cols = vec![
        NamedCol {
            name: "SYNTHETIC_AEUID",
            col: Col::Str(out_aeuid),
        },
        NamedCol {
            name: "AGEP_11",
            col: Col::I32(out_agep_11),
        },
        NamedCol {
            name: "SEXP_11",
            col: Col::I32(out_sexp_11),
        },
        NamedCol {
            name: "INGP_11",
            col: Col::I32(out_ingp_11),
        },
        NamedCol {
            name: "MSTP_11",
            col: Col::I32(out_mstp_11),
        },
        NamedCol {
            name: "LFSP_11",
            col: Col::I32(out_lfsp_11),
        },
        NamedCol {
            name: "HSCP_11",
            col: Col::I32(out_hscp_11),
        },
        NamedCol {
            name: "INCP_11",
            col: Col::I32(out_incp_11),
        },
        NamedCol {
            name: "ASSNP_11",
            col: Col::I32(out_assnp_11),
        },
        NamedCol {
            name: "AGEP_16",
            col: Col::I32(out_agep_16),
        },
        NamedCol {
            name: "SEXP_16",
            col: Col::I32(out_sexp_16),
        },
        NamedCol {
            name: "INGP_16",
            col: Col::I32(out_ingp_16),
        },
        NamedCol {
            name: "MSTP_16",
            col: Col::I32(out_mstp_16),
        },
        NamedCol {
            name: "LFSP_16",
            col: Col::I32(out_lfsp_16),
        },
        NamedCol {
            name: "HSCP_16",
            col: Col::I32(out_hscp_16),
        },
        NamedCol {
            name: "INCP_16",
            col: Col::I32(out_incp_16),
        },
        NamedCol {
            name: "ASSNP_16",
            col: Col::I32(out_assnp_16),
        },
        NamedCol {
            name: "AGEP_21",
            col: Col::I32(out_agep_21),
        },
        NamedCol {
            name: "SEXP_21",
            col: Col::I32(out_sexp_21),
        },
        NamedCol {
            name: "INGP_21",
            col: Col::I32(out_ingp_21),
        },
        NamedCol {
            name: "MSTP_21",
            col: Col::I32(out_mstp_21),
        },
        NamedCol {
            name: "LFSP_21",
            col: Col::I32(out_lfsp_21),
        },
        NamedCol {
            name: "HSCP_21",
            col: Col::I32(out_hscp_21),
        },
        NamedCol {
            name: "INCP_21",
            col: Col::I32(out_incp_21),
        },
        NamedCol {
            name: "ASSNP_21",
            col: Col::I32(out_assnp_21),
        },
        NamedCol {
            name: "LINKFLAG_16",
            col: Col::I32(out_linked_16),
        },
        NamedCol {
            name: "LINKFLAG_21",
            col: Col::I32(out_linked_21),
        },
        NamedCol {
            name: "WEIGHT4",
            col: Col::F64(out_weight),
        },
    ];
    write_columns_to_parquet(out_path, cols)
        .unwrap_or_else(|e| panic!("acld parquet write: {}", e));
    total
}

extendr_module! {
    mod acld;
    fn project_acld__;
    fn project_acld_to_parquet__;
}

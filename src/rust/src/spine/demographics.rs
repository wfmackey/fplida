use rand::rngs::StdRng;
use rand::Rng;

use super::Person;
use crate::codeframes;
use crate::sampling::{uniform_int, weighted_sample};

// Age 5-year bands (same weights as census_2021.rs)
const AGE_BAND_WEIGHTS: [f64; 18] = [
    1_463_817.0,
    1_586_138.0,
    1_588_051.0,
    1_457_812.0,
    1_579_539.0,
    1_771_676.0,
    1_853_085.0,
    1_838_822.0,
    1_648_843.0,
    1_635_963.0,
    1_610_944.0,
    1_541_911.0,
    1_468_097.0,
    1_298_460.0,
    1_160_768.0,
    821_920.0,
    554_598.0,
    542_342.0,
];
const AGE_BAND_LOWER: [i32; 18] = [
    0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,
];
const AGE_BAND_UPPER: [i32; 18] = [
    4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 99,
];

// Sex: Male 49.3%, Female 50.7%
const SEX_WEIGHTS: [f64; 2] = [49.3, 50.7];

// State (ABS codes 1-8; excluding Other Territories for simplicity)
const STATE_WEIGHTS: [f64; 8] = [
    8_072_163.0, // 1 NSW
    6_503_491.0, // 2 VIC
    5_156_138.0, // 3 QLD
    1_781_516.0, // 4 SA
    2_660_026.0, // 5 WA
    557_571.0,   // 6 TAS
    232_605.0,   // 7 NT
    454_499.0,   // 8 ACT
];

// Indigenous status: Non-Indigenous, Aboriginal, TSI, Both
const INDIGENOUS_WEIGHTS: [f64; 4] = [92.8, 3.2, 0.13, 0.14];

// Country of birth: 0=Australia (~67%), 1=Overseas (~33%)
const COB_WEIGHTS: [f64; 2] = [67.0, 33.0];

// Citizenship: 1=Australian (78%), 2=Not (22%)
const CITIZENSHIP_WEIGHTS: [f64; 2] = [78.0, 22.0];

const MORTALITY: [f64; 8] = [
    0.0003, 0.0005, 0.0007, 0.0012, 0.0030, 0.0070, 0.0170, 0.0550,
];

pub fn assign(person: &mut Person, rng: &mut StdRng) {
    // Age → birth year
    let age_idx = weighted_sample(rng, &AGE_BAND_WEIGHTS);
    let age = uniform_int(rng, AGE_BAND_LOWER[age_idx], AGE_BAND_UPPER[age_idx]);
    person.birth_year = 2021 - age;

    // Sex
    person.sex = (weighted_sample(rng, &SEX_WEIGHTS) + 1) as u8; // 1=M, 2=F

    // State
    person.state = (weighted_sample(rng, &STATE_WEIGHTS) + 1) as u8; // 1-8

    // Indigenous
    person.indigenous = (weighted_sample(rng, &INDIGENOUS_WEIGHTS) + 1) as u8; // 1-4

    // Country of birth
    let cob_idx = weighted_sample(rng, &COB_WEIGHTS);
    person.country_of_birth = cob_idx as u8; // 0=Aus, 1=Overseas

    // Year of arrival (overseas-born only)
    if cob_idx == 1 {
        let earliest = person.birth_year.max(1950);
        if earliest < 2021 {
            person.year_of_arrival = Some(uniform_int(rng, earliest, 2021));
        } else {
            person.year_of_arrival = Some(2021);
        }
    }

    // Citizenship
    person.citizenship = (weighted_sample(rng, &CITIZENSHIP_WEIGHTS) + 1) as u8;
    // 1-2
}

/// Assign the SACC 2016 country-of-birth code, consistent with the binary
/// `country_of_birth` flag set in [`assign`]. Drawn from a separate RNG
/// (`seeds::spine::COUNTRY_SACC`) so the existing demographic columns are
/// bit-identical. Australia is SACC 1101; the overseas-born draw a real
/// overseas country code from the canonical [`codeframes`] table.
pub fn assign_country_sacc(person: &mut Person, rng: &mut StdRng) {
    person.country_of_birth_sacc = if person.country_of_birth == 0 {
        codeframes::SACC_AUSTRALIA
    } else {
        codeframes::sample_overseas_country(rng)
    };
}

pub fn assign_vitals(person: &mut Person, rng: &mut StdRng) {
    person.month_of_birth = rng.gen_range(1..=12);
    person.year_of_death = None;
    person.month_of_death = None;
    person.day_of_death = None;

    for year in 2006..=2025 {
        let age = year - person.birth_year;
        if age < 0 {
            continue;
        }
        let mut rate = MORTALITY[age_band_8(age)];
        if person.sex == 1 {
            rate *= 1.3;
        }
        if rng.gen::<f64>() < rate {
            person.year_of_death = Some(year);
            person.month_of_death = Some(rng.gen_range(1..=12));
            person.day_of_death = Some(rng.gen_range(1..=28));
            break;
        }
    }
}

fn age_band_8(age: i32) -> usize {
    match age {
        a if a < 20 => 0,
        a if a < 30 => 1,
        a if a < 40 => 2,
        a if a < 50 => 3,
        a if a < 60 => 4,
        a if a < 70 => 5,
        a if a < 80 => 6,
        _ => 7,
    }
}

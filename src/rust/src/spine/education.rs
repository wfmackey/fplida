use rand::rngs::StdRng;

use super::Person;
use crate::sampling::weighted_sample;

// Education levels:
//   0 = Not applicable (under 15)
//   1 = Below Year 12
//   2 = Year 12
//   3 = Certificate III-IV
//   4 = Diploma / Advanced Diploma
//   5 = Bachelor degree or above

// Cohort-conditioned weights: [3 cohort bands] x [5 education levels]
// Older cohorts skew lower; younger cohorts skew higher.
const COHORT_EDUCATION_WEIGHTS: [[f64; 5]; 3] = [
    // Born before 1960 (age 61+ in 2021): more Below Yr12, less Bachelor+
    [25.0, 20.0, 25.0, 10.0, 20.0],
    // Born 1960-1984 (age 37-61 in 2021): close to population average
    [15.0, 20.0, 25.0, 10.0, 30.0],
    // Born 1985+ (age <37 in 2021): more Bachelor+, less Below Yr12
    [8.0, 18.0, 22.0, 12.0, 40.0],
];

fn cohort_band(birth_year: i32) -> usize {
    if birth_year < 1960 {
        0
    } else if birth_year < 1985 {
        1
    } else {
        2
    }
}

pub fn assign(person: &mut Person, rng: &mut StdRng) {
    let age = 2021 - person.birth_year;
    if age < 15 {
        person.education = 0;
        return;
    }

    let band = cohort_band(person.birth_year);
    let weights = &COHORT_EDUCATION_WEIGHTS[band];
    person.education = (weighted_sample(rng, weights) + 1) as u8; // 1-5
}

//! Tax and immigration residency status for the person spine.
//!
//! The ATO's own definition, carried in the registry against PIT_ITR
//! `CLNT_RSDNT_IND`, is a presence test rather than a citizenship or visa
//! test: it counts an overseas student on a course longer than six months as
//! a resident. So a foreign resident is not a migrant and not a non-citizen —
//! it is a person who does not live here but has Australian-source income.
//! That is why this is derived from birthplace and arrival year rather than
//! read off `citizenship`, which is drawn independently of birthplace and
//! therefore says nothing about where a person lives.

use rand::rngs::StdRng;
use rand::Rng;

use super::Person;

/// Australian resident: citizen or permanent resident, living here.
pub const RESIDENT: u8 = 1;
/// Temporary resident, present in Australia (student, 482, working holiday).
pub const TEMPORARY: u8 = 2;
/// Foreign resident: lives overseas, has Australian-source income only.
pub const FOREIGN: u8 = 3;

// Every share below is a modelling choice. No source in this repository
// states a foreign-resident share; the ATO publishes one in Taxation
// Statistics ("Individuals — selected items by residency status") but that
// table is not in the tree, so nothing here is cited to it. The shares are
// set to land the whole-of-spine foreign-resident figure near 1.3 per cent,
// which is the order of magnitude a presence-based residency test implies —
// expatriates with rental income, short-stay workers, offshore investors —
// and emphatically not the 33 per cent overseas-born or 22 per cent
// non-citizen share, either of which would put the wrong third of the filing
// population on a schedule with no tax-free threshold.

/// Australian-born people living overseas with Australian-source income.
const EXPAT_RATE_AUS_BORN: f64 = 0.010;
/// Arrived within four years: most temporary visas are still running.
const P_TEMP_RECENT: f64 = 0.45;
const P_FOREIGN_RECENT: f64 = 0.05;
/// Arrived five to nine years ago: most have converted to permanent.
const P_TEMP_MID: f64 = 0.12;
const P_FOREIGN_MID: f64 = 0.02;
/// Arrived ten or more years ago: settled.
const P_TEMP_SETTLED: f64 = 0.02;
const P_FOREIGN_SETTLED: f64 = 0.01;

/// The spine reference year, matching `demographics::assign`
/// (`person.birth_year = 2021 - age`).
const REFERENCE_YEAR: i32 = 2021;

/// Years since arrival assumed for an overseas-born person with no recorded
/// arrival year. `demographics::assign` always records one, so this only
/// guards a hand-built `Person`; long-settled is the safe reading.
const ASSUMED_YEARS_HERE: i32 = 30;

/// Age below which everyone is a resident. A child does not lodge as a
/// foreign resident in any material number and is not the person a residency
/// rule is about.
const ADULT_AGE: i32 = 18;

/// Assign tax/immigration residency.
///
/// Drawn from its own sub-RNG (`seeds::spine::RESIDENCY`) so every existing
/// spine column stays bit-identical. Depends on `birth_year`,
/// `country_of_birth` and `year_of_arrival`, so it must run after
/// `demographics::assign`.
pub fn assign(person: &mut Person, rng: &mut StdRng) {
    // Drawn before the branch so the stream does not depend on which branch a
    // person takes, and adding a later condition cannot shift anyone else's
    // draw.
    let u = rng.gen::<f64>();

    if REFERENCE_YEAR - person.birth_year < ADULT_AGE {
        person.residency_status = RESIDENT;
        return;
    }

    if person.country_of_birth == 0 {
        // Australian-born. Not a temporary resident by construction; a small
        // share are expatriates with Australian-source income.
        person.residency_status = if u < EXPAT_RATE_AUS_BORN {
            FOREIGN
        } else {
            RESIDENT
        };
        return;
    }

    let years_here = person
        .year_of_arrival
        .map(|y| REFERENCE_YEAR - y)
        .unwrap_or(ASSUMED_YEARS_HERE);
    let (p_temp, p_foreign) = if years_here <= 4 {
        (P_TEMP_RECENT, P_FOREIGN_RECENT)
    } else if years_here <= 9 {
        (P_TEMP_MID, P_FOREIGN_MID)
    } else {
        (P_TEMP_SETTLED, P_FOREIGN_SETTLED)
    };

    person.residency_status = if u < p_temp {
        TEMPORARY
    } else if u < p_temp + p_foreign {
        FOREIGN
    } else {
        RESIDENT
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::spine::build_persons;

    #[test]
    fn the_shares_are_what_the_spec_claims() {
        let persons = build_persons(50_000, 42);
        let adults: Vec<&Person> = persons
            .iter()
            .filter(|p| REFERENCE_YEAR - p.birth_year >= ADULT_AGE)
            .collect();
        let n = adults.len() as f64;
        let foreign =
            adults.iter().filter(|p| p.residency_status == FOREIGN).count() as f64 / n;
        let temp =
            adults.iter().filter(|p| p.residency_status == TEMPORARY).count() as f64 / n;
        assert!((0.008..0.025).contains(&foreign), "foreign share {foreign}");
        assert!((0.020..0.060).contains(&temp), "temporary share {temp}");
    }

    #[test]
    fn an_australian_born_person_is_never_a_temporary_resident() {
        for p in build_persons(20_000, 7) {
            if p.country_of_birth == 0 {
                assert_ne!(p.residency_status, TEMPORARY);
            }
            assert!((1..=3).contains(&p.residency_status));
        }
    }

    #[test]
    fn a_child_is_always_a_resident() {
        for p in build_persons(20_000, 11) {
            if REFERENCE_YEAR - p.birth_year < ADULT_AGE {
                assert_eq!(p.residency_status, RESIDENT);
            }
        }
    }
}

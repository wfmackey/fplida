use rand::rngs::StdRng;

use super::Person;

/// Assign base spine linkage status.
///
/// The base synthetic spine carries the canonical person identifiers for the
/// whole generated population. Agency-specific linkage failure is applied when
/// writing each agency spine, so different agencies can have different realised
/// linkage rates.
pub fn assign(person: &mut Person, _rng: &mut StdRng) {
    person.has_spine_link = true;
}

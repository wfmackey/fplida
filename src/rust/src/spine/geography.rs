use rand::rngs::StdRng;

use super::Person;
use crate::codeframes;

/// Assign ASGS 2021 geography: an SA2 of usual residence drawn within the
/// person's `state` (population-proxy weighted). SA3/SA4 nest inside the SA2
/// and are derived on emit. `sa2 == 0` denotes no geography (e.g. an
/// out-of-range state code), in which case downstream emitters write null.
pub fn assign(person: &mut Person, rng: &mut StdRng) {
    person.sa2 = codeframes::sample_sa2(person.state, rng)
        .map(|s| s.sa2)
        .unwrap_or(0);
}

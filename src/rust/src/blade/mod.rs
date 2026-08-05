//! BLADE business-data module (R->Rust port).
//!
//! Staged port of `R/generate_blade.R`. Stage 0 (here) ports the deterministic
//! leaf helpers in `helpers`. Later stages add the business-spine builder,
//! person-business link, the metadata-driven value classifier, the
//! table-specific generators, and the key products. The port is metadata- and
//! formula-driven and RNG-free; see `helpers` for the determinism contract.

pub mod business_spine;
pub mod helpers;
pub mod link;

use extendr_api::prelude::*;

/// Validation entry point: exposes the ported `stable_name_seed` so an R test
/// can confirm the Rust hash matches `fplida:::.stable_name_seed`.
/// @export
#[extendr]
fn blade_stable_name_seed__(value: &str) -> i32 {
    helpers::stable_name_seed(value) as i32
}

extendr_module! {
    mod blade;
    fn blade_stable_name_seed__;
    use business_spine;
    use link;
}

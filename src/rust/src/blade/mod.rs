//! BLADE business-data module (R->Rust port).
//!
//! Staged port of `R/generate_blade.R`. Stage 0 (here) ports the deterministic
//! leaf helpers in `helpers`. Later stages add the business-spine builder,
//! person-business link, the metadata-driven value classifier, the
//! table-specific generators, and the key products. The port is metadata- and
//! formula-driven and RNG-free; see `helpers` for the determinism contract.

pub mod business_spine;
pub mod classifier;
pub mod eeh;
pub mod helpers;
pub mod keys;
pub mod link;
pub mod location;
pub mod panel;
pub mod periods;
pub mod rows;
pub mod sampling;
pub mod tables;

use extendr_api::prelude::*;

/// Validation entry point: exposes the ported `stable_name_seed` so an R test
/// can confirm the Rust hash matches `fplida:::.stable_name_seed`.
/// @export
#[extendr]
fn blade_stable_name_seed__(value: &str) -> i32 {
    helpers::stable_name_seed(value) as i32
}

/// Validation entry point: exposes the ported `abn_hash_trunc` so an R test can
/// confirm the Rust hash matches `fplida:::.abn_hash_trunc`. The busown writer,
/// the person-business link and the correspondence key all key on this value,
/// so the two implementations agreeing is what keeps the eras joinable.
/// @export
#[extendr]
fn blade_abn_hash_trunc__(bn: &str) -> String {
    helpers::abn_hash_trunc(bn)
}

extendr_module! {
    mod blade;
    fn blade_stable_name_seed__;
    fn blade_abn_hash_trunc__;
    use business_spine;
    use classifier;
    use eeh;
    use keys;
    use link;
    use location;
    use panel;
    use sampling;
    use tables;
}

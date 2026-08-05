use extendr_api::prelude::*;

// There is deliberately no #[global_allocator] here.
//
// mimalloc was used to cut allocator contention across rayon workers in the
// hot MBS/PBS loops, worth roughly 15-20 per cent on that path. It had to go.
// The R arrow package bundles its own mimalloc and keeps all of its symbols
// local; a Rust staticlib exports them, so fplida published 425 mi_* symbols
// and the two copies collided over process-wide allocator state. On macOS that
// crashed R inside arrow::write_parquet. arrow is an unconditional dependency,
// so the two are always loaded together.
//
// Do not reintroduce a global allocator without checking `nm -gU` on the built
// shared object for symbols that could interpose on another R package.

pub mod acld;
pub mod aedc;
pub mod air;
pub mod amep;
pub mod apprentice;
pub mod ato_cr;
pub mod births;
pub mod blade;
pub mod business_pool;
pub mod busown;
mod census_2021;
pub mod cgt;
pub mod codeframes;
pub mod combined;
pub mod core_gen;
pub mod deaths;
pub mod dex;
pub mod disability;
pub mod domino;
pub mod employment;
pub mod he;
pub mod mbs;
pub mod mcd;
pub mod mt_demogs;
pub mod nacdc;
pub mod ndis;
pub mod parquet_io;
pub mod pbs;
pub mod pit_ie;
pub mod pit_itr_build;
pub mod pit_ps_build;
pub mod pit_ps_full;
pub mod residence;
pub mod rps;
pub mod sae;
mod sampling;
pub mod sdac;
pub mod sdb;
pub mod seeds;
pub mod service_profiles;
mod spine;
pub mod spine_template;
pub mod stp;
pub mod travellers;
pub mod tva;
pub mod visa;

/// Return the package version string.
/// @export
#[extendr]
fn fplida_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

extendr_module! {
    mod fplida;
    fn fplida_version;
    use census_2021;
    use disability;
    use spine;
    use mbs;
    use pbs;
    use he;
    use core_gen;
    use tva;
    use domino;
    use employment;
    use combined;
    use births;
    use blade;
    use deaths;
    use mcd;
    use ato_cr;
    use visa;
    use mt_demogs;
    use sdb;
    use travellers;
    use pit_ie;
    use busown;
    use business_pool;
    use sae;
    use cgt;
    use rps;
    use stp;
    use ndis;
    use apprentice;
    use dex;
    use air;
    use amep;
    use nacdc;
    use aedc;
    use acld;
    use sdac;
    use pit_itr_build;
    use pit_ps_build;
    use pit_ps_full;
    use spine_template;
}

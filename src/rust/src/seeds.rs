//! Central seed-offset registry.
//!
//! Every generator derives its RNG from a single base `seed` plus a fixed
//! offset, so that (a) runs are reproducible and (b) two generators never
//! share a stream and silently correlate. Historically these offsets were an
//! implicit convention scattered across ~58 `wrapping_add`/`set.seed(seed + k)`
//! sites in both R and Rust. This module makes the Rust-side offsets explicit
//! and is the place to *allocate a new offset* when adding a generator or a
//! spine attribute.
//!
//! Conventions (kept in lockstep with the R wrappers):
//!   * Spine sub-RNGs: `base_seed + 0..=11` (see [`spine`]). Each spine
//!     attribute group draws from its own sub-RNG, so adding a new attribute
//!     at a fresh offset leaves every existing column bit-identical.
//!   * Per-generator base offsets (R `set.seed(seed + k)` and Rust
//!     `seed_from_u64(seed + k)` agree):
//!       census +100/+200, employment +300..+500, pit_itr +600/+601,
//!       core +700..+702, he +800..+803, domino +900..+907,
//!       mbs +1000..+1004, pbs +1100..+1104, tva +1200..+1202.
//!   * Per-slice spacing: the build controller spaces slice workers by
//!     `slice_id * 100_000` (see `build_fplida()`), so per-generator offsets
//!     must stay well below 100_000.
//!   * Per-chunk spacing inside a generator uses a large multiplicative
//!     stride (e.g. `ci * 2_654_435_761`) to decorrelate chunks.
//!
//! Existing sites are migrated to reference these constants opportunistically
//! as each generator is touched; new code should use them from the start.

/// Spine sub-RNG offsets, added to `base_seed` in `spine::build_persons`.
/// Offsets 0..=5 are the original attribute groups; 6..=11 were added for the
/// cross-cutting fidelity upgrade. Each is an independent stream.
pub mod spine {
    /// Demographics: sex, age, state, indigenous, country-of-birth flag.
    pub const DEMOGRAPHICS: u64 = 0;
    /// Education level / archetype affinity.
    pub const EDUCATION: u64 = 1;
    /// Occupation (ANZSCO) and task scores.
    pub const OCCUPATION: u64 = 2;
    /// Income: employment, hours, baseline income, fixed effects.
    pub const INCOME: u64 = 3;
    /// Cross-dataset linkage flag.
    pub const LINKAGE: u64 = 4;
    /// Disability / chronic-health-condition assignment.
    pub const DISABILITY: u64 = 5;
    /// ASGS geography (SA2 within state). Added 2026 upgrade.
    pub const GEOGRAPHY: u64 = 6;
    /// SACC country-of-birth code for the overseas-born. Added 2026 upgrade.
    pub const COUNTRY_SACC: u64 = 7;
    /// Household / family grouping. Added 2026 upgrade.
    pub const HOUSEHOLD: u64 = 8;
    /// Latent health / condition propensities beyond `person_type`. Added 2026.
    pub const HEALTH: u64 = 9;
    /// Month of birth and death date. Added for PLIDA core-scope products.
    pub const VITALS: u64 = 10;
    /// Per-person physical-presence seed. Added for PLIDA core residence.
    pub const RESIDENCE: u64 = 11;
}

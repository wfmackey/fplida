//! Process-global pool of BLADE business numbers (`bn`).
//!
//! BLADE is the business source of truth and is generated before the
//! employer-linked worker products (PIT_PS payment summaries, STP). Setting
//! this pool from the written business spine lets those products draw their
//! employer identifiers from real BLADE `bn` values, so they resolve to BLADE
//! businesses via `_system/plida-blade-link`. When the pool is empty (e.g. a
//! standalone product build with no BLADE stage), generators fall back to
//! their legacy self-contained identifier scheme.
//!
//! The pool is set once (single-threaded) from R before a product's Rust
//! generator runs, then read concurrently by rayon workers. Reads take a
//! cheap `Arc` snapshot, so the hot path never holds the lock.

use extendr_api::prelude::*;
use std::sync::{Arc, OnceLock, RwLock};

fn store() -> &'static RwLock<Arc<Vec<String>>> {
    static STORE: OnceLock<RwLock<Arc<Vec<String>>>> = OnceLock::new();
    STORE.get_or_init(|| RwLock::new(Arc::new(Vec::new())))
}

/// Replace the global business-number pool.
pub fn set_pool(bns: Vec<String>) {
    *store().write().expect("business pool lock poisoned") = Arc::new(bns);
}

/// Cheap snapshot of the current pool (clones the `Arc`, not the strings).
pub fn snapshot() -> Arc<Vec<String>> {
    Arc::clone(&store().read().expect("business pool lock poisoned"))
}

/// Map a hash to a pooled `bn`, or `None` when the pool is empty.
///
/// The hash is whatever deterministic value a generator already computes for
/// an employer, so the mapping preserves that generator's switching and
/// reuse behaviour while constraining identifiers to real businesses.
pub fn bn_for_hash(pool: &[String], hash: i64) -> Option<String> {
    if pool.is_empty() {
        None
    } else {
        Some(pool[(hash.unsigned_abs() as usize) % pool.len()].clone())
    }
}

/// Parse the trailing digits of a person/spine identifier into a stable
/// global person number (e.g. "P0000010006" or "SP0000010006" -> 10006).
/// Both PIT_PS (person `id`) and STP (spine `id`) parse to the same number,
/// which lets the two products agree on a person's employers.
pub fn person_number(id: &str) -> i64 {
    let mut out: i64 = 0;
    let mut seen = false;
    for b in id.bytes() {
        if b.is_ascii_digit() {
            seen = true;
            out = out.saturating_mul(10).saturating_add((b - b'0') as i64);
        }
    }
    if seen {
        out
    } else {
        0
    }
}

/// Employer slot reserved for a person's secondary job, kept clear of the
/// primary employer-spell slots (0, 1, 2, ...).
pub const SECONDARY_SLOT: i64 = 1_000_000_000;

/// Deterministic BLADE `bn` for a (global person number, employer slot)
/// pair. PIT_PS and STP both call this, so a person's primary employer
/// (slot 0) and secondary employer (`SECONDARY_SLOT`) resolve to the same
/// business in both products. Returns `None` when the pool is empty so the
/// caller can fall back to its legacy identifier scheme.
pub fn employer_bn(pool: &[String], person_number: i64, slot: i64, seed: i64) -> Option<String> {
    let h = person_number
        .wrapping_mul(1_000_003)
        .wrapping_add(slot.wrapping_mul(999_983))
        .wrapping_add(seed)
        .rem_euclid(100_000_000_000);
    bn_for_hash(pool, h)
}

/// Set the global business-number pool from R. Pass the BLADE business
/// spine `bn` vector. Passing an empty vector clears the pool, restoring
/// legacy identifier behaviour.
#[extendr]
fn set_business_pool__(bns: Strings) {
    set_pool(bns.iter().map(|s| s.to_string()).collect());
}

/// Number of business numbers currently in the pool (diagnostics).
#[extendr]
fn business_pool_size__() -> i32 {
    snapshot().len() as i32
}

extendr_module! {
    mod business_pool;
    fn set_business_pool__;
    fn business_pool_size__;
}

# Plan: reconcile STP / PAYG / PIT salary-and-wages income

Goal (user-chosen scope: **per-employer + labour churn**): for every person-financial-year, and for every
(person, employer, FY), the salary/wages income and PAYG withholding tie out across the three administrative
sources, and the labour module's multi-job / separation churn flows coherently into all three:

```
sum(STP pay-event gross for person-employer-FY)  ==  PAYG payment-summary GROSS_PAYMENTS for that person-employer-FY
sum over employers                               ==  PIT salary line (PIT_IE WANDS, PIT_ITR GRS_PMT_TOTL_CALCD_AMT)
sum(STP TAX_WHELD)  ==  PAYG TAX_WITHHELD  ==  PIT withholding credit  (per employer, and summed per person-FY)
```
Non-wage PIT income (INVINC, OWNUNINC, SUPINC, OTHINC, business/investment) stays independent.

## Current state (code-grounded)

- `employment.rs::run_employment_panel` already produces a per-(person, year, job) panel: `gross_annual`,
  `employer_id`, `spell` (employer-switch index), `primary_job`, plus ~7% secondary jobs. Anchored at 2021,
  log-earnings random walk outward.
- **PAYG (`pit_ps_full.rs`) already consumes the panel**: GROSS_PAYMENTS = panel `gross_annual`, split across the
  old/new employer for mid-year switchers by a random switch-month; `compute_payg_tax(gross)` for TAX_WITHHELD.
  Employer ABN via `business_pool::employer_bn(person_number, slot, seed)` — **shared with STP** (slot 0 primary,
  SECONDARY_SLOT secondary).
- **STP (`stp.rs`) is the outlier**: per-pay gross = `baseline_income/365.25 * gross_days * variability`
  (`stp.rs:637,921`), an independent draw; its own job history and churn, separate from the panel.
  STP covers **2020–2025** (monthly products), `generate_stp.R:131`.
- **PIT_IE WANDS** (`pit_ie.rs:37`) and **PIT_ITR gross/withholding** (`pit_itr_build.rs`) are independent draws
  from `baseline_income`.
- Coverage: PAYG/PIT default 2010–2024; STP 2020–2025. Reconcile in the **overlap (2020–2024)**; PAYG↔PIT alone
  before STP existed (realistic — STP phased in ~2018–19).

## Foundational prerequisite (do first)

**Make the employment panel per-person deterministic.** Today `run_employment_panel` draws the earnings walk and
switches from single shared streams (`seed+300`, `seed+360`) iterated over persons×years, so per-person values
depend on person order/set. STP and the PIT generators run in separate slice processes over (potentially) different
person subsets/orders, so they cannot reconstruct the identical panel. Re-key every per-person draw to a stable
hash of `(person_number, seed, year, salt)` — the same trick `disability_employment_draw` (employment.rs:78) and
the health module already use — so any generator reconstructs an identical per-person panel from the spine alone.
This is a behaviour change to the panel (and thus to PAYG default output) but is required for reconciliation.

## Canonical structure: a shared wage ledger

Add `employment.rs::expand_wage_ledger(panel, seed) -> Vec<WageEntry>` where
```
WageEntry { aeuid, person_number, year, employer_abn, gross, withholding,
            income_type,        // Regular | Contractor | LabourHire | WHM | VA | CommunityDev
            primary, spell, start_month, end_month, pay_freq }
```
This generalises pit_ps_full's existing switcher-split into one shared, deterministic function:
- one entry per employer-spell active in the year (a mid-year switch ⇒ two entries: old spell + new spell, gross
  split by switch month — exactly today's PAYG logic, lifted out);
- secondary-job entries;
- `withholding = compute_payg_tax(gross)` once per entry (single shared rule, move the duplicated copies in
  `pit_ps_build.rs` / `pit_itr_build.rs` into `employment.rs`);
- `income_type` classification (the nonstandard STP income types) lives here, not in STP.

Churn enrichment moves **into the panel + expansion** (more employer switches, gaps, multi-job, and the
nonstandard income types), so the churn produces more ledger entries ⇒ more STP separations **and** more PAYG
payment summaries **and** the correct summed PIT line — all automatically.

## Per-generator changes

1. **PAYG (`pit_ps_full.rs`)**: replace the inline switch-split with `expand_wage_ledger`; one payment summary per
   entry. Gross/withholding behaviour preserved (modulo the per-person-determinism change); adds income_type.
2. **PIT_IE (`pit_ie.rs`)**: run the panel + `expand_wage_ledger`; `WANDS = Σ entry.gross` per (person, FY);
   wages withholding available for the ITR. Keep INVINC/OWNUNINC/SUPINC/OTHINC draws unchanged. (Needs the panel's
   spine columns added to `generate_pit_ie.R` + an identical spine filter.)
3. **PIT_ITR (`pit_itr_build.rs`)**: `GRS_PMT_TOTL_CALCD_AMT = Σ entry.gross`; `tax_withheld credit = Σ
   entry.withholding`; the ITR still computes actual tax on total income ⇒ realistic balance payable/refund
   (per-employer PAYG over/under-withholding reconciled at the return, as in reality).
4. **STP (`stp.rs`)** — the big one: for each ledger entry, realise pay events across `[start_month, end_month]` at
   `pay_freq`, distributing `gross`/`withholding` across pays with the **last pay absorbing rounding** so the sum is
   exact. Cessation date = spell end; ETP at separations. `income_type` ⇒ CNTRCTR_BN / INCM_LABR_HIR_ARNGMT_GRS_AMT
   / INCM_WHM_GRS_AMT / INCM_VA_GRS_AMT / INCM_CMNTY_DEV_EMPLT_PRJCT_AMT. Remove `stp_pay_event_gross` and the
   separate `stp_job_history` labour churn (now panel-driven). Preserve the DIL schema + the monthly/extended
   product split.
5. **R wrappers**: ensure `generate_pit_ps` / `generate_pit_itr` / `generate_pit_ie` and
   `generate_stp` all load the **same** spine columns and apply the **same** `filter_ato_records`, so the
   reconstructed panel is identical. Update `slice_worker.R` + `build_fplida.R`.

## Build order (each stage independently verifiable)

- **S0** Per-person-deterministic panel (prerequisite). Re-run PAYG; confirm unchanged-shape output.
- **S1** `expand_wage_ledger` + shared `compute_payg_tax`; refactor PAYG onto it (behaviour-preserving). 
- **S2** PIT_IE + PIT_ITR onto the panel/ledger ⇒ **PAYG↔PIT reconcile** per person-FY and per employer. Verify.
- **S3** STP onto the ledger ⇒ **STP↔PAYG↔PIT reconcile** in 2020–2024. Verify exact per-employer tie-out.
- **S4** Move churn into the panel; confirm churn ⇒ multiple payment summaries + summed PIT line +
  STP separations, all consistent.
- **S5** `scripts/check_income_reconciliation.R` (regression test); update testthat (`test-generate_stp`,
  `test-generate_pit*`, `test-employment_panel`) + cargo tests for the new default output.

## Risks / notes

- **Breaking change** to default `stp` / `pit_ps` / `pit_itr` / `pit_ie` output (intended — they now tie out).
- **Determinism** is the central risk: every generator must reconstruct the identical panel ⇒ S0 is mandatory and
  must be verified (build PIT twice in separate processes, assert identical gross).
- **Per-slice parallelism**: each slice runs the panel on its slice's persons; per-person determinism makes this safe.
- **Performance**: the panel is now computed in 4 generators instead of 1 (~4× panel cost; per-person O(n·years), so
  acceptable, but worth measuring at 1m with the timing harness).
- **Exact tie-out** needs integer-cent care: distribute annual gross/withholding across pays, last pay absorbs the
  remainder; PAYG/PIT round once.
- STP only covers 2020–2025, so pre-2020 reconciliation is PAYG↔PIT only (correct).

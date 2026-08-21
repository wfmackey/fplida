# fplida — remaining work (to-do)

Status as of 2026-08-21, after the generator-defect sweep landed on `main`
(#7). Full package green: 7,344 passing expectations, 0 failures, 0 errors,
7 skips. `R CMD check` is `Status: OK`, and the R-CMD-check workflow passes on
macOS, Ubuntu and Windows. Plans: this file, `dev/implementation-plan.md`
(per-domain gap analysis), `dev/blade-port-plan.md` (BLADE port spec).

Build/test: `export PATH="$HOME/.cargo/bin:$PATH" && R CMD INSTALL .`; tests via
`testthat::test_file(...)`. NAMESPACE + `R/extendr-wrappers.R` are hand-maintained
(rextendr not installed) — add new `#[extendr]` fns to both manually.

Closed entries have been removed rather than ticked. `git log` holds the record;
this file is meant to say what is left.

---

## A. BLADE R→Rust port — finish stages 3-5

Stages 0-2 done (`src/rust/src/blade/{helpers,business_spine,link}.rs`; suite
150/0). The remaining stages port the per-row value generation. Tests are
STRUCTURAL not bit-exact (column presence, regex/`^BN[0-9]{11}$`, type, range,
code-frame membership, linkage/aggregate consistency, and the global no-
placeholder rule: no string matching `_[0-9]{6}$`). Port is formula-driven /
RNG-free, i64/i128. **Gotcha:** each file's `extendr_module! { mod NAME; }` NAME
must equal the Rust module (file) name or `R_init_NAME_extendr` symbols collide;
`fn` is a keyword → emit the list field as `fn_` and rename to `fn` in R.

- [ ] **Stage 3 — generic classifier** (`blade/classifier.rs` + `blade/periods.rs`).
  Port `.blade_metadata_value_for` (the ordered version/period/date/month/year/
  anzsic/anzsco/geography/postcode/state/coded-response/numeric/alphanumeric
  cascade) AND the name-based fallthrough in `.blade_value_for` (R 1860-1971).
  Port the period chain (`split_periods`, `period_end_year` with YYYY-YY century
  math, `latest_period[_from_values]`, `tsid`/`tsid_from_period` incl. table-5→
  table-1, `end_year`, `reference_date`, `financial_year_code`). Build a
  `VariableSpec{name,lower,item_lower,valid_lower,context,salt}` once per column;
  `classify()` returns `Option<BladeColumn{Int/Dbl/Chr/Date/None}>`. Wire via a
  Rust `make_blade_frame__`-equivalent that the R `.make_blade_frame` calls for
  the ~50 GENERIC (non-special) tables; keep special tables on R until Stage 4.
- [ ] **Stage 4 — table-specific generators** (`blade/tables.rs`, `eeh.rs`,
  `location.rs`, `sampling.rs`). Port the `.blade_special_value_for` dispatch and
  the six generators: Table 1 (`.blade_frame_value_for`/`.blade_role_code`/
  `x_gst_bn`), 4 BAS (`exports_amt ≤ turnover`, `turnover ≥ oexp`), 6 BIT
  (c/i/p/t legal-form prefix masking → `c_totlwage>0 ⇒ i/p/t==0`), 7 STP
  (`ed_sg_emplr_cntrbtn == round(ed_pmt_sumry_totl_grs_pmt*0.115,2)`; lump-sum +
  EEH fixes already in the R version — keep them), 8 BCS (`.blade_bcs_code`
  frame `{0,1,7777777,88888888,999999999}`), 27 birthdate (1993/2001 spikes +
  ~NA). Port `.make_blade_eeh_frame` (Table 17 employee-level; `eid_eeh` 15-char,
  health ANZSCO prefixes 25/41/42), `.blade_business_location_frame` (24/25,
  mesh-block lookup from `mb_lookup.csv.gz` — see `codeframes.rs` geography),
  `.blade_location_lookup_rows`, and `.select_blade_rows`/`.select_blade_frame_rows`
  (order()-equivalent stable rank). Wire the full `.blade_value_for` cascade
  (name_map passthrough → abn/id/bg/version/tsid/quarter → special → generic →
  fallthrough). Parallelise tables with rayon.
- [ ] **Stage 5 — key products + orchestration** (`blade/keys.rs`). Port
  `.make_blade_id_bn_key` (2N rows over tsid union) and `.make_blade_cn_bn_key`,
  `.select_key_columns`, `.blade_table_variable_names` column drops. Optionally a
  single `generate_blade__` entry returning a list-of-products. Keep
  build_fplida BLADE central-stage integration intact (worker_results==0).
- [ ] Optional: port `.add_blade_link_reconciliation` (currently R; simple
  O(n_link) HashMap reduction) once the link is fully Rust.

---

## B. Fidelity items still open

Everything else in the two backlogs was closed by #7. What is below either
needs data the repository does not have, or a shape decision only the owner
can make.

- [ ] **A spine has no residency flag.** `tax_schedule.rs` carries a
  `foreign_resident_tax()` branch with no tax-free threshold, and it is
  unit-tested, but nothing on the spine ever selects it, so every generated
  return is taxed as a resident. HE `COUNTRY_BIRTH` enrichment and the DOMINO
  residency rules want the same flag. One weighted draw on the spine — sized
  against the ABS temporary-visa population — would light up all three.
- [ ] **COMBINED indigenous has no code 9.** The generated
  `EVER_INDIGENOUS_PERSON` is 0/1 from the spine's own indigenous status
  (`combined.rs:29`), so the "not stated" category the real product carries
  never appears. Needs a not-stated weight on the spine's indigenous draw,
  which changes every downstream product that reads it — hence not done in
  passing.
- [ ] **A&T (DEWR apprentice) multi-table rebuild.** DEFERRED: no public
  apprentice codebook to source a code frame from. Revisit if DEWR or NCVER
  publish one.
- [ ] **AMEP and TRAVELLERS shape.** Both now emit every variable the data
  item list publishes, so nothing is missing. Open questions are structural:
  whether AMEP's client and english schemas should be two products rather
  than one completed table, and whether TRAVELLERS should carry its wide
  per-period columns (~300) or stay compact. Owner decisions, not defects.
- [ ] **STP `PYRL_FNCL_YR` — confirm the source type for pay and ETP.** The
  generator side is settled: jobs, pay-event and ETP tables all emit an
  integer ending year (verified 2026-08-21 across
  `stp_standard_jobs`, `stp_extended_jobs`, `stp_standard_pay_events`,
  `stp_extended_pay_events`, `stp_extended_etp`). What is unconfirmed is the
  real PLIDA type: the in-lab check on 2026-08-04 covered `stp_jobs` only.
  Confirm the other two in-lab, then the labour build's register parse can
  drop its deviation comment.

---

## C. Measurement / housekeeping

- [ ] **Regenerate `generated-variable-crosswalk.csv`.** It still describes the
  61-table, 1,585-column sample the schema register used to be, while the
  register now covers 596 tables and 32,056 columns. Because
  `build-variable-code-evidence-registers.R` merges the two on
  guide/dataset/table/variable, re-running it today fills the crosswalk
  columns for almost no rows — so the five per-guide evidence registers
  cannot be rebuilt until the crosswalk catches up. Most of it is
  mechanical: 825 rows match `inst/blade_metadata/variables.csv` on the
  variable name and 692 match `inst/plida_metadata/variables.csv` on
  dataset+variable. The remaining 68 are hand-written concept matches
  (`basis` beginning `manual_`) and must be carried over, not regenerated.
  There is no script for this yet; write one, in `data-raw/`, beside the
  schema-register builder.
- [ ] **Decide where LFS belongs.** `GUIDE_OF_DATASET` in
  `data-raw/build_generated_schema_register.R` has no entry for it, so its 385
  columns now fall to a guide of their own, `schema-register-lfs.csv`. The
  other surveys (NHS, NSMHW, PEX, SDAC) sit under `core-combined`. Either add
  LFS there or give it a real guide document.
- [ ] Re-run the schema register after each domain lands and check that
  metadata coverage does not fall. Last run 2026-08-21: 596 tables, 32,056
  columns, 99.0% matched to metadata — against 32,984 columns at the same
  coverage before. The 928-column fall is the AEDC sibling tables shedding
  their byte-copied 175-column record (-3,096) and the DEATHS vintage keying
  dropping PLACE_OF_DEATH and the 2021 geography reissues from tables that
  should not have carried them (-7), against +2,175 columns of new Home
  Affairs, DEX, DOMINO and MCD coverage.

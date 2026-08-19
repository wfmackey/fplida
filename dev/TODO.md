# fplida — remaining work (to-do)

Status as of the 0.3.0 variable-fidelity release + BLADE port stages 0-2.
Full package green: 6,687 expectations, 0 failures, 0 errors. The STP
statistical test that used to fail (`test-dil-2026.R:181`) now passes. Plans:
this file, `dev/implementation-plan.md` (per-domain gap analysis),
`dev/blade-port-plan.md` (BLADE port spec).

Build/test: `export PATH="$HOME/.cargo/bin:$PATH" && R CMD INSTALL .`; tests via
`testthat::test_file(...)`. NAMESPACE + `R/extendr-wrappers.R` are hand-maintained
(rextendr not installed) — add new `#[extendr]` fns to both manually.

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

## B. STP / MBS / PBS date formatting → ddmmmYY  ✅ DONE (commit 68f68f7)

Health-claim and payroll date columns now render as `ddmmmYY` strings (e.g.
`31Jan20` — 2-digit year per the user's clarification, superseding the earlier
`DDmmmYYYY` note) instead of Date32.

- [x] `parquet_io.rs`: `Col::DateStr(Vec<i32>)` + `days_to_ddmmmyy()` (Howard
  Hinnant civil-from-days, no chrono) + `MONTH_ABBR`; arrow_type Utf8,
  nullable true. Unit-tested.
- [x] **MBS** `DOS`/`DOP`/`RPDATE` across all three paths (list!, Col parquet,
  streaming chunked writer). RPDATE NA-preserving. `empty_mbs_list` updated.
- [x] **PBS** `PRSCRB_DT`/`SPPLY_DT`/`EXTRCT_DT` across list!, Col, and both
  streaming chunk-to-batch writers. `empty_pbs_list` updated.
- [x] **STP** six date columns via `Col::DateStr`. `test-dil-2026.R` parses
  with `format="%d%b%y"`. Verified ddmmmYY in both return-data and on-disk
  paths; full suite 1275/1 (pre-existing CV test only).

---

## C. Other deferred fidelity items (from dev/implementation-plan.md)

Fourteen entries from the root `TODO.md` landed after this file was last
written, and several overlap the items below: the higher education columns,
the household `dwelling_id` geography every product now reads, BUSOWN's legal
forms, the Census ASCED attainment items (HEAP at three digits, QALLP, QALFP),
and the registry's move to `fplida.info`. Items marked done below were closed
by that work.

- [x] **STP `PYRL_FNCL_YR` type**: DONE. Now an INTEGER ending year (2023) in
  the jobs, pay and ETP frames, in both the R and Rust generators, in the DIL
  completion path (`complete_dil_structures.R`) and the lightweight path
  (`generate_dil_lightweight.R`), and in the registry. `.stp_fy_label()` is
  replaced by `.stp_fy_year()`; `.stp_fy_suffix()` keeps the two-part label for
  table names. Downstream `scripts/check_income_reconciliation.R` simplified to
  `as.integer(pyrl_fncl_yr)`. STILL OPEN: only `stp_jobs` was verified in-lab
  (2026-08-04); confirm the pay and ETP tables carry the integer too. The
  labour build's register parse can now drop its deviation comment.
- [ ] **Vital Events**: DEATHS `death_registrations_{year}` product split (the
  14-var demographic table, separate from cause_of_death) + year-vintaged
  PLACE_OF_DEATH/SEIFA; MCD 3-table model (demogs/address/entitlements per
  vintage). BIRTHS 2006 window + DEATHS ENTITY/RACS already done.
- [ ] **Census central household assembly**: derive DWELLING_ID/FAMILY_ID +
  RLHP/FPIP/SPIP from spine `household_id` via a CENTRAL dwelling/family table
  stage in build_fplida (households span build slices — per-slice generation
  would duplicate inconsistent dwellings). The `household_id` enabler is done;
  this is the orchestration change. CORE already consumes `household_id`, and
  BUSOWN now shows the pattern: it moved to a central stage for exactly this
  reason. Households can now hold three or more adults, so RLHP has adult
  children and housemates to describe rather than only couples and children.
- [ ] **Home Affairs (larger items)**: AMEP client(44)/english(31) distinct-
  schema split (currently a verbatim copy with corrected names); VISA ~54
  missing official variables (VA_CASE_ID, subclass-500 COE/IELTS fields);
  MT_DEMOGS ASGS geography + address-spell START/END_DATE + STATE_ASGS_2022;
  TRAVELLERS wide per-period columns + monthly PP status (owner-gated: wide
  ~300 cols vs compact).
- [ ] **PIT exact reconciliation**: PIT_IE WANDS == PS gross at person-year
  (thread the PS aggregate + ITR-filer set through build_fplida); year-keyed
  LITO/bracket tax schedule in pit_itr_build.rs; non-resident branch.
- [ ] **Health (P0 leftovers)**: AIR PNEU/ZOSTER age-gated blocks + parametrise
  the year window from the spine min/max (currently hardcoded). MBS BTOS sampler
  and PBS Safety Net were judged NOT bugs (BTOS derived; high PBS tail = real
  high-cost drugs).
- [ ] **Education**: AEDC sibling products (domain/indigenous/language/
  specialneeds) + per-domain cut scores. The HE enrol/load columns are DONE:
  the enrol table emits all 25 registry variables (EDUCATION_PARENT1/2,
  TERT_ENT_SCORE, YEAR_ARRIVAL from the spine, NEW_ADMISSION,
  SEPARATION_STATUS_CODE, CREDIT_OFFERED/CREDIT_VALUE_USED, SCHOLARSHIP_TYPE,
  LANGUAGE_HOME) and the load table all 22 (COURSE_DATE,
  CAMPUS_GLOBAL_REGION). REPORTING_YEAR_PERIOD no longer ends in -1 on every
  row.
- [ ] **CORE/SDAC/DOMINO leftovers**: SDAC DISGP=7/DISTYPE=18 → unambiguous NA
  sentinel; COMBINED indigenous code-9 (needs a spine indigenous weight change);
  CORE locations SA3/LGA + multi-spell; HE/DOMINO residency-from-flag (HE
  COUNTRY_BIRTH enrichment); DOMINO income/een subtables (21 of 35 products).
- [ ] **VET**: A&T (DEWR apprentice) multi-table rebuild — DEFERRED pending a
  public apprentice codebook (no sourceable code frame yet).
- [ ] **NDIS / DEX**: NDIS carers/providers/outcomes products; DEX remaining
  reference/lookup tables (organisation/outlet/program/ref_*). (Core products
  done.)

---

## D. Measurement / housekeeping

- [x] **Base-spine export is opt-in.** `build_fplida()` now removes
  `_system/base-spine.parquet` by default. Set `export_base_file = TRUE` only
  for diagnostic builds that need the internal base spine. CSV builds also
  omit `base-spine-v6/base-spine-v6.csv` unless the option is true.
- [x] **Schema-register builder** (Phase 0): DONE.
  `data-raw/build_generated_schema_register.R` builds a fixed-seed sample with
  every product, reads every parquet, records per-column type, missingness,
  distinct count and domain, joins the PLIDA and BLADE metadata, regenerates
  `fplida.info/inst/internal-docs/generated-schema-register.csv` and its
  per-guide splits, and reports the change in column count and coverage
  against the committed version. The register went from a 61-table sample of
  1,585 columns to all 532 generated tables and 32,984 columns, of which 314
  (1.0%) have no metadata behind them -- that list is the actionable one.
  `FPLIDA_REGISTER_RUN_DIR` reassembles from an existing build.
- [ ] Re-run the variable-code-evidence registers after each domain lands;
  recompute `observed_in_generated_register` coverage (must be non-decreasing).
- [x] `test-dil-2026.R:181` STP CV>0.6 — RESOLVED, no code change needed. It
  passes now: full suite 6,687 expectations, 0 failures, 0 errors. The recorded
  failure dates from the WIP baseline `a6a5765` and was fixed by intervening
  work, so the suite is green end to end.

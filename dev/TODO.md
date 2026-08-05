# fplida — remaining work (to-do)

Status as of the 0.3.0 variable-fidelity release + BLADE port stages 0-2.
Full package green except one **pre-existing** STP statistical test
(`test-dil-2026.R:181`, fails identically at the WIP baseline `a6a5765` — not a
regression). Plans: this file, `dev/implementation-plan.md` (per-domain gap
analysis), `dev/blade-port-plan.md` (BLADE port spec).

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

- [ ] **STP `PYRL_FNCL_YR` type**: fplida emits a VARCHAR label ("2022-23")
  at every site (`generate_stp.R` 751, 815, 1039, 1136 + empty stubs 595,
  1051, 1156; column list 934); the real PLIDA extract holds an INTEGER
  ending year (2023) — verified in-lab against real `stp_jobs` 2026-08-04.
  Switch generation to the integer form across jobs, pay and ETP frames,
  and confirm the pay/ETP-side type in-lab (only jobs verified so far).
  Downstream: the labour build's register parse
  (`right(pyrl_fncl_yr, 2)`) then simplifies to a plain
  `as.integer(pyrl_fncl_yr)` and its fplida-vs-real deviation comment
  can be removed.
- [ ] **Vital Events**: DEATHS `death_registrations_{year}` product split (the
  14-var demographic table, separate from cause_of_death) + year-vintaged
  PLACE_OF_DEATH/SEIFA; MCD 3-table model (demogs/address/entitlements per
  vintage). BIRTHS 2006 window + DEATHS ENTITY/RACS already done.
- [ ] **Census central household assembly**: derive DWELLING_ID/FAMILY_ID +
  RLHP/FPIP/SPIP from spine `household_id` via a CENTRAL dwelling/family table
  stage in build_fplida (households span build slices — per-slice generation
  would duplicate inconsistent dwellings). The `household_id` enabler is done;
  this is the orchestration change. CORE already consumes `household_id`.
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
  specialneeds) + per-domain cut scores; HE missing enrol/load columns
  (parent/score/year-arrival, CAMPUS_GLOBAL_REGION/COURSE_DATE).
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
- [ ] **Schema-register builder** (Phase 0): a committed, fixed-seed script that
  builds a small dataset, reads every parquet, records per-column type/
  missingness/distinct/domain, joins PLIDA+BLADE metadata, regenerates
  `inst/internal-docs/generated-schema-register.csv`, and diffs coverage. The
  headline progress metric; needed before claiming further coverage gains.
- [ ] Re-run the variable-code-evidence registers after each domain lands;
  recompute `observed_in_generated_register` coverage (must be non-decreasing).
- [ ] Investigate the pre-existing `test-dil-2026.R:181` STP CV>0.6 borderline
  assertion (fails at baseline; not introduced by this work).

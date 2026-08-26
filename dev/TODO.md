# fplida — remaining work (to-do)

Status as of the 0.3.0 variable-fidelity release + BLADE port stages 0-2.
Full package green: 6,687 expectations, 0 failures, 0 errors. The STP
statistical test that used to fail (`test-dil-2026.R:181`) now passes. Plans:
this file, `dev/implementation-plan.md` (per-domain gap analysis),
`dev/blade-port-plan.md` (BLADE port spec).

Build/test: `export PATH="$HOME/.cargo/bin:$PATH" && R CMD INSTALL fplida.info &&
R CMD INSTALL .` (the in-repo `fplida.info` is an Imports dependency and must go
first, or the build stops on it); tests via
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

- [x] **Stage 3 — generic classifier** (`blade/classifier.rs` + `blade/periods.rs`)
  — DONE. `.blade_metadata_value_for` (with `.blade_admin_character_value` and
  `.blade_period_value` inside it) and the name-based fallthrough at the tail of
  `.blade_value_for` are both in Rust, as is the period chain
  (`split_periods`, `period_end_year` with the YYYY-YY century rollover,
  `latest_period`, `tsid` including the table-5→table-1 redirect, `end_year`,
  `reference_date`, `financial_year_code`) and `.blade_location_lookup_rows`.
  `VariableSpec` is built once per column and the per-row `grepl` is gone: every
  R pattern is a hand-rolled predicate in `helpers.rs`, so the crate still needs
  no `regex`. Wiring is per variable (`blade_metadata_value_for__`,
  `blade_fallthrough_value_for__`), not a whole-frame call, because R still owns
  the first half of the `.blade_value_for` cascade; Stage 4 folds both into
  `make_blade_frame__` and the argument marshalling disappears. The R
  implementations stay behind `exists("<fn>__", mode = "function")` so a failing
  stage can be A/B compared. All 5,246 variables across all 62 tables were run
  both ways, twice (once with a business frame carrying every column, once with
  the leaner unit-test fixture): 5,238 columns are bit-identical, and the other
  8 are the year and period-boundary variables where R returns a length-1 vector
  that `as.data.frame` recycles and Rust returns the column already recycled.
  Note the A/B mask has to blank the function in `package:fplida` as well as the
  namespace, or `exists()` finds it on the search path and both arms run Rust.
  - Also fixed here: `helpers::round2` was rounding halves away from zero where
    R's `round(x, 2)` takes the nearer representable number and breaks ties to
    even. Over that same 5,204-column run, 116 columns carried a disagreement
    and 128 of their 4,640 cells differed by a cent, in the business spine and
    the link as well as the tables. `r_round` now reproduces R exactly (checked
    against R on 401,006 values).
  - Where a business column is absent, R returns NULL from a bare
    `business_rows$col` but a ZERO-LENGTH vector from anything wrapped in
    `as.integer()`/`round()`/`sprintf()`, which makes `as.data.frame` refuse the
    frame. Both are reproduced exactly: a missing business column should stop
    the build, not quietly drop a published variable.
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
  mesh-block lookup from `mb_lookup.csv.gz` — see `codeframes.rs` geography;
  `.blade_location_lookup_rows` itself landed in Stage 3), and
  `.select_blade_rows`/`.select_blade_frame_rows`
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
- [x] **Vital Events**: DONE. DEATHS now writes
  `death_registrations_{2007..2012}` with its fourteen demographic variables,
  separate from `cause_of_death`, and names its geography by vintage:
  SEIFA_IRSD_DEC and REMOTENESS_AREA up to 2020, the _2021 reissues from
  2021, and PLACE_OF_DEATH in the 2019 tables only. MCD writes the
  three-table model -- demogs, address and entitlements -- for each of the
  five extract vintages the data item list names, with the June 2022 one
  carrying both ASGS editions of its address table because it spans the
  reissue. Address, programme and concession spells each have a start and an
  open or closed end. BIRTHS 2006 window + DEATHS ENTITY/RACS already done.
- [x] **Census central household assembly**: DONE. The identifiers come from
  the spine's own `dwelling_id` so every slice agrees, the dwelling and family
  tables are collapsed after the merge, and RLHP, FPIP and SPIP are derived
  once centrally over the population the Census sees. Original entry: derive
  DWELLING_ID/FAMILY_ID +
  RLHP/FPIP/SPIP from spine `household_id` via a CENTRAL dwelling/family table
  stage in build_fplida (households span build slices — per-slice generation
  would duplicate inconsistent dwellings). The `household_id` enabler is done;
  this is the orchestration change. CORE already consumes `household_id`, and
  BUSOWN now shows the pattern: it moved to a central stage for exactly this
  reason. Households can now hold three or more adults, so RLHP has adult
  children and housemates to describe rather than only couples and children.
- [x] **Home Affairs**: DONE for variable coverage. Every variable the data
  item list publishes is now emitted across all four datasets -- AMEP 417,
  VISA 69, MT_DEMOGS 27, TRAVELLERS 588 -- where AMEP was missing 24 of the
  25 on its address table and VISA 37 of 48 on its application table. The
  bespoke generators keep every value they already produced; only the gaps
  are filled, from the registry's own value rules. STILL OPEN: whether AMEP's
  client and english schemas should be separate products rather than one
  completed table, and whether TRAVELLERS should carry its wide per-period
  columns (~300) or stay compact -- both owner-gated shape decisions rather
  than missing data.
- [x] **PIT exact reconciliation**: DONE. PIT_IE WANDS already equalled the
  payment summary gross at person-year after the shared-panel refactor --
  13,130 of 13,130 exact, maximum difference $0.00 -- and the tax schedule is
  now year-keyed. `tax_schedule.rs` carries the resident brackets for every
  schedule in the window (the $6,000 threshold before 2012-13, the 80,000 and
  87,000 third thresholds, the 2020-21 restructure and the 2024-25 Stage 3
  cuts), the low income tax offset on its correct two-stage taper, indexed
  Medicare levy thresholds, and a foreign-resident branch with no tax-free
  threshold. The offset moves from $445 to $700 exactly at 2020-21 in the
  generated returns. STILL OPEN: wiring the foreign-resident branch to a
  residency flag on the spine -- the schedule is there, nothing sets it yet.
- [x] **Health (P0 leftovers)**: DONE. AIR emits PNEU and ZOSTER gated by the
  ages the National Immunisation Program funds, and its observation window
  comes from the build's years rather than a fixed offset from 2024. MBS BTOS
  sampler and PBS Safety Net were judged NOT bugs (BTOS derived; high PBS tail
  = real high-cost drugs).
- [x] **Education**: DONE. The AEDC siblings each carry their own variable
  list -- indigenous 17, language 14, special needs 77 -- instead of being a
  byte copy of the 175-column core record, and vulnerability is cut at the
  10th and 25th percentiles of the 2009 national baseline, so the baseline
  cycle sits at 10.1% vulnerable per domain and later cycles are free to move
  (7.8% to 11.0%). The HE enrol/load columns are DONE:
  the enrol table emits all 25 registry variables (EDUCATION_PARENT1/2,
  TERT_ENT_SCORE, YEAR_ARRIVAL from the spine, NEW_ADMISSION,
  SEPARATION_STATUS_CODE, CREDIT_OFFERED/CREDIT_VALUE_USED, SCHOLARSHIP_TYPE,
  LANGUAGE_HOME) and the load table all 22 (COURSE_DATE,
  CAMPUS_GLOBAL_REGION). REPORTING_YEAR_PERIOD no longer ends in -1 on every
  row.
- [ ] **CORE/SDAC/DOMINO leftovers**: SDAC DISGP/DISTYPE sentinel DONE (both
  are missing rather than 7 and 18). CORE locations SA3/LGA DONE (SA3 nests in
  the SA2, LGA comes from the code frame keyed on the dwelling). DOMINO
  subtables DONE: all 35 published products are written, where nine were, and
  a subtable covers the recipients the base record holds. STILL OPEN: COMBINED
  indigenous code-9 (needs a spine indigenous weight change); HE/DOMINO
  residency-from-flag (HE COUNTRY_BIRTH enrichment). CORE locations multi-spell
  DONE: a person who moved has a closed spell at the address they left and an
  open one where they live now, with its own ARID, and the spells abut.
- [ ] **VET**: A&T (DEWR apprentice) multi-table rebuild — DEFERRED pending a
  public apprentice codebook (no sourceable code frame yet).
- [x] **NDIS / DEX**: DONE. All six NDIS products are written -- carers,
  providers and outcomes join participants, payments and plan supports -- and
  all fifteen DEX tables. The reference and lookup tables are catalogues
  rather than per-client records: 60 organisations and 25 programmes against
  290 clients, with no client identifier on them. The three bespoke DEX
  tables also emitted a subset of their registry columns and are now topped
  up, so a join on OUTLETID or ACTIVITYID finds the column.

---

- [ ] **Core Relationships and Core Locations: co-residence and relationship
  history** (2026-08-21, from the labour build's family stage). The generator
  now keys ARID on the dwelling, so people in one dwelling share an address --
  but the relationship pairs ignore the dwelling. `generate_core.R` pairs
  partners as random adults in selection order and gives each child one random
  parent from the 25-55 pool, so related people are co-resident only by
  accident; every pair is one CENSUS record with RECORD_END NA; and the
  amendment flags the lab carries (SINGLE_AMENDED, DEATH_AMENDED) do not exist.
  The installed 10m extract predates the dwelling-keyed ARID: 10m spells, 10m
  distinct ARIDs, none shared. Any household or family construction keyed on
  co-residence therefore runs green and produces nothing -- in the labour
  build every person is `alone`, every recorded pair `separated`, and the
  co-residence rules of
  `thesis_notes/01-admin-labour-data/doc/notes/family-household-construction.qmd`
  (B4-B7, C2, D1) never fire, which is the failure mode that passes locally
  and breaks in the lab. Needed: (1) draw partner pairs and parent-child links
  from the household structure `census_households.R` already builds -- the
  dwelling's couple and its children -- keeping a minority of couples living
  apart and of children with a non-resident parent; (2) two parent links per
  child, with a share of separated parents at different dwellings; (3)
  relationship endings: RECORD_END for separations and deaths carrying
  SINGLE_AMENDED / DEATH_AMENDED, plus some pairs that end with no flag (the
  unobserved separation); (4) the multi-source repeat -- the same pair as a
  Census point record (start = end = Census night) beside a DOMINO spell with a
  different span; (5) moves with a per-person reporting lag, so the two members
  of a couple change address records months apart (`residential_mobility.R`
  moves the household as one and copies a stale address to the whole
  household, so the lag never varies within a couple); (6) regenerate the 10m
  extract at `~/offline/datalab10m` afterwards. Acceptance: the share of
  partner pairs sharing a dwelling, of children sharing a dwelling with a
  linked parent, of pairs with an end date, and of children with two parent
  links must all be non-zero, and the labour build's
  `_checks/probe-07-scenarios.R` outcomes must appear in the real-extract
  `check_07-family.R` counts (couples, dependants, siblings, lone parents).

---

- [ ] **BLADE/PLIDA business keys: the abn_hash_trunc era and the id-to-bn
  correspondence** (2026-08-26, from the labour build's business stage). In
  the real data, products from 2022 on store ABN-level information under `bn`
  (a hashed ABN with the "BN" prefix); products to 2021 use `abn_hash_trunc`
  (a different, unprefixed hashing of the ABN), and the delivery includes a
  two-variable correspondence table (`abn_hash_trunc`, `bn`) to bridge them.
  fplida does not model this: `blade-key-id-to-bn-key` carries a placeholder
  `id` (E-prefixed) plus version columns instead of `abn_hash_trunc`, the
  hashes on `ato-d-business-owners` match nothing in the key or in BLADE, and
  every vintage of business_owners is keyed the same way. Needed: (1) emit the
  correspondence with exactly (`abn_hash_trunc`, `bn`), one row per ABN,
  hashes consistent with the ATO-side products; (2) key business_owners
  vintages to FY2021 on `abn_hash_trunc` only and FY2022+ on `bn` only, the
  same business carrying consistent ids across vintages; (3) those `bn`
  values must exist in the BLADE tables so register/BAS joins land.
  Acceptance: the labour build's 08-business builds the busown products
  locally through the bridge (its stand-down message no longer fires) and
  `check_08-business.R`'s ownership half runs with a non-zero bridge match.

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

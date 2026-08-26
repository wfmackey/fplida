# fplida — remaining work (to-do)

Status as of the 0.3.0 variable-fidelity release, the spine residency and
not-stated Indigenous work, the CORE household pass, and the BLADE R-to-Rust
port, all five stages of it.

Full package green: 8,632 expectations, 0 failures, 0 errors, measured
2026-08-27 with stages 0-3 in and again with stage 4 in, and
`test-generate_blade.R` holds at 329/0/0 at every stage. The port is
behaviour-preserving, which is what the structural tests are for.

Plans: this file, `dev/implementation-plan.md` (per-domain gap analysis),
`dev/blade-port-plan.md` (BLADE port spec, with its stale claims corrected at
the end).

Build/test: `export PATH="$HOME/.cargo/bin:$PATH" && R CMD INSTALL fplida.info &&
R CMD INSTALL .` (the in-repo `fplida.info` is an Imports dependency and must go
first, or the build stops on it). Run tests with `testthat::test_local()`, NOT
`test_file()` plus `library(fplida)`: several files call internal functions
unqualified, and those only resolve inside the package namespace, so
`test_file()` reports "could not find function" errors that are not real.
The whole suite takes about 23 minutes on an unloaded ten-core machine.
NAMESPACE + `R/extendr-wrappers.R` are hand-maintained (rextendr not
installed) — add new `#[extendr]` fns to both manually.

---

## A. BLADE R→Rust port — done

All five stages done (`src/rust/src/blade/{helpers,periods,rows,business_spine,
link,classifier,tables,eeh,location,sampling,keys}.rs`; `test-generate_blade.R`
329/0/0). What stays in R is the metadata and IO boundary the plan always
reserved for it: the CSV readers, `.resolve_blade_tables`, the per-table
dispatch loop, `.select_key_columns`, `.make_blade_frame`'s orchestration and
the parquet writing. Tests are
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
- [x] **Stage 4 — table-specific generators** (`blade/tables.rs`, `eeh.rs`,
  `location.rs`, `sampling.rs`) — DONE. `.blade_special_value_for` and all six
  generators are in Rust: Table 1 (role codes, `x_gst_bn`), 4 BAS, 6 BIT with
  the c/i/p/t prefix masking, 7 STP, 8 BCS, 27 birthdate. So are
  `.make_blade_eeh_frame`, `.blade_business_location_frame` and the two row
  samplers, and the full `.blade_value_for` cascade is now one Rust call per
  variable (`blade_value_for__`) rather than R walking the first half of it.
  The shared frame types moved out of `classifier.rs` into `blade/rows.rs`.
  Every R implementation stays behind `exists("<fn>__", mode = "function")`.
  Verified by building all 62 tables plus the two key products both ways on the
  same spine and seed: 2,319 columns across 16 tables (1, 3-8, 17, 24, 25, 27,
  29, 49, 53, 56, 59) are identical in class and value, and
  `test-generate_blade.R` holds at 329/0/0.
  - Two things stayed in R deliberately. `.make_blade_frame` and
    `.blade_enforce_admin_relationships` are still the R orchestrator, because
    the work item scoped Stage 4 to the generators and the admin post-pass is a
    short vectorised sweep over the finished frame; folding both into a single
    `make_blade_frame__` belongs with Stage 5's dispatch loop. The BAS wage
    index and the EEH nominal wage factor also stay in R: both come from
    `nominal.R`, whose hash is a different function from `crate::nominal`'s, so
    recomputing either in Rust would move the amounts.
  - The Mesh Block lookup is passed down as three character vectors rather than
    pushed into a process-global Rust store. R already holds the picked rows by
    the time the location frame is built, so a second copy of the 368,149-row
    table in the crate would buy nothing.
  - Also closed here: the EEH `agecat_eeh` frame started at 24 where the data
    item list publishes "1 = Under 18 years". It now has seven bands with the
    first ending at 17; the bands above it are marked as a modelling choice in
    both implementations.
  - Left open: table 8's generator is terminal by design, so its 882 variables
    never reach the generic classifier and the per-variable BCS code frames in
    the implementation plan's gap register stay unclosed for that table.
  - Not done: rayon. Every table is a separate R call and the per-variable work
    is already a few microseconds; parallelism belongs with the Stage 5 loop
    that owns all 62 tables at once.
- [x] **Stage 5 — key products + orchestration** (`blade/keys.rs`) — DONE.
  `.make_blade_id_bn_key` (one block per time-series id, over the union of the
  key's own tsid and table 8's) and `.make_blade_cn_bn_key` are in Rust, as is
  `.add_blade_link_reconciliation` (in `blade/link.rs`, the optional item
  below). Both version literals and both tsids stay in R and travel down as
  arguments, because `fplida.blade_metadata_dir` can redirect the CSVs they
  come from. All three R implementations stay behind
  `exists("<fn>__", mode = "function")`. Verified both ways on the same spine
  and seed at 686 businesses: the id key (8 columns, 1,372 rows), the cn key
  (4 columns, 686 rows) and the reconciled spine (79 columns) agree in class
  and in value, cell for cell. `test-generate_blade.R` holds at 329/0/0.
  - The tsid union is `{key_tsid, tsid(8)}` = `{"25", "20"}` with the shipped
    metadata, not `{"25","21"}` as the port plan said. R does the `unique()`,
    so a metadata edit that collapses the two gives one block, not a repeat.
  - Also closed here: the id key's `match` field used two of the seven values
    the data item list publishes. It now spans the whole frame -- "NPP" for the
    non-profiled population by definition, and the six profiled values drawn
    from a hash of the business identifier -- and the two ABN-to-TAU flags
    follow from the match type rather than being drawn separately, so an
    ANZSIC-level match implies one ABN to many units and the enterprise-group
    residual implies many ABNs to one. The shares over the six profiled values
    are a modelling choice; the ABS publishes the frame but no distribution.
    A small share carries the frame's "." missing code on both flags.
  - The match hash reads the identifier string through `stable_name_seed`, not
    the digits as a number. Identifiers step by a constant and only about a
    fifth of businesses are profiled, so a linear hash beats against that
    period and skewed the shares badly -- SubDiv came out at 0.6 per cent
    against a 7 per cent target. Through the string every share lands within
    half a point.
  - `.blade_table_variable_names` was left alone. The "table-specific column
    drops" the port plan asked for do not exist in this repo and never have
    (`git show 1688be4`), and the tests require the opposite: table 1 must
    carry `id`/`x_sisca06`/`x_anzsic93`, table 4 `month_actioned`, table 6
    `cn`/`fn`. `.select_key_columns` also stayed in R -- it enforces the
    keys.csv column order and is the only guard that a builder still emits
    every variable the metadata lists.
  - Not done, deliberately: a single `generate_blade__` entry returning a
    list-of-products. It would hold all 62 products alive at once (table 6
    alone is 700 columns by 171,429 businesses at n=1M) where the loop builds
    one, writes it and drops it; the loop's per-product metadata and the
    `return_data` path both need an R data.frame per product anyway; and the
    useful unit of parallelism is variables within a table, not tables.
  - `.blade_tables`, `.blade_variables` and `.blade_key_variables` now share a
    path-and-timestamp cache. variables.csv is 5,246 rows and a 62-table build
    read it hundreds of times.

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
  `as.integer(pyrl_fncl_yr)`. The pay and ETP tables are now confirmed too
  (2026-08-26): a 2,000-person FY2022-FY2024 sample wrote 57 product
  directories and 182 canonical DIL structures, and every one of them types
  `PYRL_FNCL_YR` as parquet `int32`. `test-generate_stp.R` now holds each
  table to the financial year its own name implies, so a family that reverted
  to a two-part label could not pass. The canonical DIL fallback was a year
  short for July to December -- it took the period's ending year, and a
  monthly table names its calendar year -- and is fixed. The labour build's
  deviation comment at `01-stp-1-build-history.R:124` is stale, but its
  expression is still numerically right on the new value, so replacing it is a
  simplification rather than a fix.
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
  are filled, from the registry's own value rules. STILL OPEN, and left open
  deliberately: whether AMEP's client and english schemas should be separate
  products rather than one completed table, and whether TRAVELLERS should
  carry its wide per-period columns (~300) or stay compact. Both are shape
  decisions about published products, so they are the owner's to make rather
  than the generator's. The registry's own answer, for whoever makes it: AMEP
  is declared as seven tables in seven products, with
  `amep_cltpro_dob_visa_treated` and `amep_englishproficiency` already
  separate; TRAVELLERS is declared as 35 tables and 588 variables, one table
  per year from 2006.
- [x] **PIT exact reconciliation**: DONE. PIT_IE WANDS already equalled the
  payment summary gross at person-year after the shared-panel refactor --
  13,130 of 13,130 exact, maximum difference $0.00 -- and the tax schedule is
  now year-keyed. `tax_schedule.rs` carries the resident brackets for every
  schedule in the window (the $6,000 threshold before 2012-13, the 80,000 and
  87,000 third thresholds, the 2020-21 restructure and the 2024-25 Stage 3
  cuts), the low income tax offset on its correct two-stage taper, indexed
  Medicare levy thresholds, and a foreign-resident branch with no tax-free
  threshold. The offset moves from $445 to $700 exactly at 2020-21 in the
  generated returns. The foreign-resident branch is now wired: the spine
  carries `residency_status` and PIT_ITR puts code 3 on the foreign schedule
  with no LITO and no Medicare levy, and stamps `CLNT_RSDNT_IND = "N"`.
  STILL OPEN: the working holiday maker schedule (15% from the first dollar),
  which is a third schedule `tax_schedule.rs` does not carry.
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
- [x] **CORE/SDAC/DOMINO leftovers**: SDAC DISGP/DISTYPE sentinel DONE (both
  are missing rather than 7 and 18). CORE locations SA3/LGA DONE (SA3 nests in
  the SA2, LGA comes from the code frame keyed on the dwelling). DOMINO
  subtables DONE: all 35 published products are written, where nine were, and
  a subtable covers the recipients the base record holds. COMBINED indigenous
  code-9 DONE: the spine draws a latent Indigenous status and then decides
  separately whether the person stated it, at 4%, so code 9 is reachable and
  each downstream product translates it into its own not-stated code.
  HE/DOMINO residency-from-flag DONE: `residency_status` decides whether an HE
  student is domestic or overseas and gates DOMINO eligibility, with a
  four-year newly-arrived waiting period. CORE locations multi-spell
  DONE: a person who moved has a closed spell at the address they left and an
  open one where they live now, with its own ARID, and the spells abut.
- [ ] **Spine citizenship is drawn independently of birthplace**:
  `demographics.rs` draws `citizenship` from `CITIZENSHIP_WEIGHTS` with no
  reference to `cob_idx`, drawn 14 lines earlier, so at n=200,000 14.7% of the
  spine is Australian-born and coded a non-citizen and 25.8% is overseas-born
  and coded a citizen. `travellers.rs` already works around it and calls the
  group "small", which it is not. The fix is to condition the draw on
  `country_of_birth`, which changes the `citizenship` column for everyone and
  moves Census `CITP` and TRAVELLERS with it — so it needs its own change, not
  a rider on another. Until then `residency_status` and `citizenship` can
  contradict each other on a person.
- [ ] **Working holiday maker tax schedule**: 417 and 462 visa holders pay 15%
  from the first dollar to $45,000. `tax_schedule.rs` carries two schedules,
  resident and foreign; this is a third, and it applies to a slice of
  `residency_status` code 2 rather than to the whole code.
- [ ] **VET**: A&T (DEWR apprentice) multi-table rebuild — DEFERRED pending a
  public apprentice codebook (no sourceable code frame yet). Rechecked
  2026-08-27: the evidence status is still
  `apprentice_public_codebook_not_found_local_metadata_only`, so nothing has
  changed. Separately, and NOT deferred, `dev/implementation-plan.md` carries
  one sourced TVA defect this file has never listed: the three AVETMISS state
  fields (`CLIENT_STATE_RESIDENCE_DERIVED`, `HEAD_OFFICE_STATE`,
  `STATE_OF_FUNDING_GF`, both tables) are emitted as bare integers 1-8 where
  AVETMISS defines two-character codes "01" to "08" plus "09" and "99". That
  is a format fix in `tva.rs` with a published source behind it.
- [x] **NDIS / DEX**: DONE. All six NDIS products are written -- carers,
  providers and outcomes join participants, payments and plan supports -- and
  all fifteen DEX tables. The reference and lookup tables are catalogues
  rather than per-client records: 60 organisations and 25 programmes against
  290 clients, with no client identifier on them. The three bespoke DEX
  tables also emitted a subset of their registry columns and are now topped
  up, so a join on OUTLETID or ACTIVITYID finds the column.

---

- [ ] **Core Relationships and Core Locations: co-residence and relationship
  history** (2026-08-21, from the labour build's family stage). Items (1) to
  (5) are DONE; item (6), the 10m rebuild, is STILL OPEN. CORE Relationships
  and CORE Locations are now one household pass. The dwelling's couple is the
  oldest adult and the other adult closest in age within 18 years -- exactly
  the rule `census_household_roles()` uses -- and the married/de facto draw is
  the same per-dwelling draw, so CORE COMBINED_STATUS and Census RLHP agree
  about the same couple. A child's parents are the reference person and their
  partner, subject to a 16-year age gap, with 6% of children unlinked and 16%
  of children short of two co-resident candidates carrying a link to an adult
  at another dwelling; 8% of couples live apart. Pairs end: an annual
  separation hazard of 0.020 and the members' death dates resolve RECORD_END,
  with SINGLE_AMENDED and DEATH_AMENDED saying which and 35% of separations
  carrying neither. Both flags are NA on every Parent-Child row, because the
  registry declares them on `core_partner_*` only. 22% of pairs are recorded
  twice, as a Census point record (start = end = 2021-08-10) and a mirrored
  ATO or DOMINO spell, so PAIRID is now a function of the unordered pair
  rather than a 32-bit draw that collided about a thousand times at 10m. A
  separation moves one member out: their Core Locations history closes at the
  shared ARID and opens at a new one. A household that did not separate still
  moves as one, but a per-person reporting lag (mean 3 months, capped at 9)
  now sits on the move date, so two members of a couple switch address records
  months apart. Measured at n=40,000: 92.0% of partner rows share a dwelling,
  97.9% of parent-child links do, 41.0% of partner rows have an end date,
  87.1% of linked children have two parent links, and 97.0% of separated
  co-resident couples are at different addresses afterwards. STILL OPEN:
  regenerate the extracts at `~/offline/datalab10m` and `~/offline/datalab1m`,
  which predate both the dwelling-keyed ARID and this change (10m spells, 10m
  distinct ARIDs, none shared), then confirm the labour build's
  `_checks/probe-07-scenarios.R` outcomes appear in the real-extract
  `check_07-family.R` counts (couples, dependants, siblings, lone parents).
  Note for the labour build: the amendment flags ship as INTEGER 0/1/NULL, as
  the registry code frame declares, so `07-family.R`'s
  `coalesce(single_amended, FALSE)` needs to become
  `coalesce(single_amended == 1, FALSE)` -- DuckDB will not mix INTEGER and
  BOOLEAN in `coalesce`.

---

- [x] **BLADE/PLIDA business keys: the abn_hash_trunc era and the id-to-bn
  correspondence** (2026-08-26, from the labour build's business stage). DONE,
  with two corrections to the entry as written. `blade-key-id-to-bn-key` is
  not a placeholder: it is Appendix A1 of the BLADE data item list, and its
  `id` is the deidentified unit_id, so it is untouched and the correspondence
  is a new product, `blade-key-abn-hash-trunc-to-bn-key`, with exactly
  (`abn_hash_trunc`, `bn`) and one row per business. And the crossover is not
  a financial-year threshold: `inst/plida_metadata/variables.csv` already
  named the identifier per table, and 2021-22 is mixed -- the 12-month
  extracts carry `ABN_HASH_TRUNC`, the 16-month re-extracts carry `BN`. The
  generator now reads that column from the registry rather than assuming a
  year. `abn_hash_trunc` is a bijection on 48 bits of the same `bn`, mirrored
  in R and Rust, and the business pool is unchanged, so every published `bn`
  still resolves to a BLADE business and a register or BAS join lands. On a
  3,000-person build the correspondence resolves 42 of 42 post-crossover
  businesses, 39 of which also appear in a pre-crossover file, and the raw
  `BN` matches none of them. STILL OPEN: the labour build's `08-business` and
  `check_08-business.R` cannot be run from this repository.
- [ ] **PIT_PS publishes its employer under the wrong name.** PIT_PS writes
  `EMPLOYER_ABN` (`src/rust/src/pit_ps_build.rs`, `src/rust/src/pit_ps_full.rs`)
  while `inst/plida_metadata/variables.csv` declares `ABN_HASH_TRUNC` for every
  `ato_pay_sum_*` table, and the value is a raw `bn` drawn from the BLADE pool
  rather than the hash that name implies. Same class of defect as the busown
  one above, same fix: take the column name from the registry and hash the
  value when the registry asks for `ABN_HASH_TRUNC`. The same question applies
  to `R/generate_dil_lightweight.R`, whose fallback writes a `BN`-prefixed
  12-hexadecimal value into any column named `ABN_HASH_TRUNC`, which is
  neither era's identifier.

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
- [x] **Re-run the registers after each domain lands.** DONE for the schema
  register (2026-08-27): rebuilt against the new generators, 32,058 columns,
  99.0% matched to metadata, and `observed_in_generated_register` recomputes to
  71.15% against the committed 48.50% -- a rise of 22.65 points, so the
  non-decreasing gate is met and no guide falls. The rule behind that flag,
  which no script records: a row is TRUE when `toupper(dataset)` (reading
  `BLADE_KEYS` as `BLADE`) plus `toupper(variable)` appears in the
  generated-variable crosswalk.
- [ ] **The generated-variable crosswalk has no builder**, and the evidence
  registers cannot be refreshed without it. The five buildable evidence CSVs
  join `schema-register-<guide>.csv` to `generated-variable-crosswalk.csv`, but
  the crosswalk is still on its 1,585-row baseline and nothing in the tree
  rebuilds it, so re-running
  `fplida.info/inst/internal-docs/build-variable-code-evidence-registers.R`
  today would replace curated evidence rows with thousands whose
  `crosswalk_status` is empty. Do not run it until the crosswalk is
  regenerated. The `census` and `dhda-health` evidence CSVs have no builder at
  all and are hand-made -- do not clobber them either.
- [ ] **LFS has a schema-register split and no guide.** The split writer's
  fallback now gives an unmapped dataset its own guide, so LFS gets
  `schema-register-lfs.csv` (385 columns) where before it reached no split at
  all. There is no `lfs-dataset-explainer.qmd` to go with it. LFS is mentioned
  in the census explainer, so folding it into `census` is defensible, as is
  `core-combined` or a guide of its own -- but it is a documentation decision
  rather than a code one.
- [x] `test-dil-2026.R:181` STP CV>0.6 — RESOLVED, no code change needed. It
  passes now: full suite 6,687 expectations, 0 failures, 0 errors. The recorded
  failure dates from the WIP baseline `a6a5765` and was fixed by intervening
  work, so the suite is green end to end.

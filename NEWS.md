# fplida 0.3.0

A research-informed variable-fidelity upgrade. Shared code frames now improve
agreement across related PLIDA and BLADE products. The package records source
evidence and unresolved value domains separately.

## Cross-product correctness fixes

- MBS and PBS no longer emit claims dated before the person was born.
  Participation began at 1 January of the birth year and the claim-date window
  was the whole year: the truncation handled death but never birth. The window
  now opens at the spine's `month_of_birth` and the Poisson rate is scaled by
  the observable part of the year, so birth-year volumes fall away as the birth
  month gets later. Measured on a 5,000-person spine over 2006-2025: 2,649 of
  1,319,142 MBS claims and 860 of 804,415 PBS supplies preceded birth, about 46
  per cent of each dataset's birth-year rows. Both are now zero.
- PBS takes the patient's birth month from the spine. It was redrawn with
  `rng.gen_range(1..=12)` on every prescription row, so it varied within the
  same person and matched the spine on about one row in twelve. It now matches
  on every row.
- DOMINO reads its vitals from the shared spine. It derived its own death
  probability and emitted a random month of birth and a random year of death,
  so 92 per cent of rows disagreed with the spine on month of birth and 848
  people with a spine death were coded as alive. All three now agree exactly.
- DOMINO benefit spells stop at death. A spell could start, or continue, years
  after the person died — 364 spells started after death and 47 spanned it, at
  a maximum of 16.2 years past the death date. Both are now zero.
- CORE geography agrees with the spine. `project_core_locations__` drew a mesh
  block uniformly from the person's whole state, so CORE and Census agreed on
  the same person's SA2 only by chance, 0.67 per cent of the time. CORE now
  draws from inside the spine SA2 and agreement is complete, with the
  SA1/SA2/SA4 nesting preserved.
- Higher education campus postcodes use the correct state map. State 4 (SA) was
  given the Perth prefix and state 5 (WA) the Adelaide prefix, and NT and ACT
  were likewise swapped, so 20.7 per cent of rows carried a postcode outside
  the range for their own state. The institution counts and the dual-sector
  entry carried the same inversion. All are corrected.
- SDAC anchors disability status on the spine. Both entry points redrew
  disability from an independent hazard, so 85 per cent of respondents carrying
  a spine disability were coded as having none. DISSTAT is now taken from the
  spine where the spine has a value, with the hazard covering the residual
  population so the published prevalence still holds. The spine and SDAC scales
  are different code frames, so the map is not the identity: spine
  profound/severe becomes DISSTAT Severe, moderate becomes Moderate, mild
  becomes Mild, and condition-only becomes long-term condition only. A
  disability whose onset is after survey night is no longer anchored.
- SDAC core-activity limitations vary. `base.min(base + rng.gen_range(0..=1))`
  always returned `base`, so communication, mobility and self-care were
  perfectly collinear with DISSTAT. Which of the three is the binding
  limitation is now drawn, with the most severe still equal to DISSTAT.

- ATO occupation fields now hold real ATO salary and wage occupation codes
  instead of ANZSCO. The occupation field on an individual income tax return is
  not ANZSCO: taxpayers pick a six-digit code from the ATO's own published list,
  and of the 1,167 codes for 2025-26, 951 are also valid ANZSCO 2019 codes and
  216 are not. Each person's ANZSCO occupation from the spine is now mapped onto
  an ATO code through a bundled crosswalk, resolving up the code hierarchy where
  there is no exact counterpart, so the occupation stays consistent across
  products while the code system matches the source. The reporting-noise model
  also draws from the ATO list rather than ANZSCO, and the "cannot find my
  occupation" case now emits the group's `nec` code or `999000`, because the ATO
  list has no major-group "not further defined" code. Every emitted code is now
  a published ATO code, up from 81.5 per cent. Source: ATO Salary and Wage
  Occupation Codes on data.gov.au, CC BY 2.5 AU. Regenerate with
  `data-raw/update_ato_occupation_codes.R`.

## Build and platform fixes

- The Rust backend no longer registers mimalloc as its global allocator. The
  `arrow` package bundles its own mimalloc and keeps every one of its symbols
  local; a Rust static library exports them, so `fplida` published 425 `mi_*`
  symbols and the two copies collided over process-wide allocator state. On
  macOS that crashed R inside `arrow::write_parquet()`. Removing it also
  clears the compiled-code check, which now reports no warning and no note.
  Generation of MBS and PBS is about 15 to 20 per cent slower as a result.
- The `zstd` parquet feature is removed. Every writer used
  `Compression::SNAPPY`, so it was never reached, but it pulled in `zstd-sys`,
  whose C objects produced a macOS link warning and the `abort` and `exit`
  references that R's compiled-code check reports.
- `extendr-api` moves from 0.8 to 0.9, which no longer calls the retired
  non-API entry point `R_NamespaceRegistry`. Output is unchanged.
- Parquet files are no longer left memory-mapped after reading. Windows
  refuses to rewrite a mapped file, so the build failed with `os error 1224`
  as soon as it tried to replace its own person spine.

- The macOS-only `-framework Security` linker flag is removed from
  `src/Makevars`. R uses that file on every Unix-alike, so the flag was also
  passed to the Linux compiler, where it is not recognised. The crate has no
  Security framework dependency.
- The Rust profile no longer sets `panic = "abort"`, and the `abort()`
  override in `src/entrypoint.c` is removed. A panic in the Rust backend now
  arrives as an ordinary R error that `tryCatch()` can handle, including a
  panic raised on a worker thread. It previously crashed the R session.
- A build no longer reports files as merged when the move failed. Slice output
  lives under `tempdir()` while the run directory follows `set_data_path()`, so
  the two are often on different filesystems, where `file.rename()` fails.
  Moves now fall back to copy-then-delete and raise an error if both fail.
- `resolve_output_dir()` checks that the output directory can be created and
  written before generation starts. An unwritable location now gives a clear
  message instead of failing inside the Rust writers.
- The L-drive reference-table stage is removed, along with the
  `fplida.export_l_drive` option. It wrote ASGS geography and classification
  tables to imitate the DataLab L: drive, needed two extra packages, and
  downloaded boundary files over the network during a build.
- `strayr` and `sf` are no longer dependencies, and the package no longer
  carries a `Remotes:` field. It now installs entirely from CRAN packages.
- The valid ANZSCO 2019 occupation codes are bundled in
  `inst/extdata/codeframes/`. Census validates `OCCP` against them. They were
  previously read from `strayr` at run time, so Census output silently
  depended on whether that optional package was installed: without it the
  validity check was skipped, the per-person random stream shifted, and
  almost every Census row changed. Output is now the same either way, and
  matches what an installation with `strayr` produced.
- `arrow` moves from Suggests to Imports. Every build writes parquet
  internally, including a CSV build, so it was never optional. A reader who
  followed the README and installed only the hard dependencies previously
  failed on their first build.
- `convert_parquet_dir_to_csv()` takes `threads = NULL` by default and resolves
  the thread count in the function body. On a host where `detectCores()`
  returns `NA` the conversion previously failed with `PRAGMA threads = NA`.
- The declared minimum Rust version of 1.81 is now honoured. Seven uses of
  `std::iter::repeat_n`, stable only from 1.82, are replaced.
- `.gitattributes` pins `eol=lf`, so a Windows checkout no longer produces a
  CR-in-source note during `R CMD check`.

## Release preparation

- Public package metadata, installation instructions, contribution guidance,
  continuous integration, and a pkgdown site are now included.
- The bundled PLIDA metadata contains 48 dataset rows, 43 unique dataset
  codes, 559 products, and 67,398 variable rows. The bundled BLADE metadata
  contains 62 tables and 5,246 variable rows.
- The release audit gives priority to administrative data and Census. Value
  validation remains incomplete for SDAC, NHS, NSMHW, PEX, LFS, and the 25
  BLADE survey tables.
- A complete build now writes one canonical `product--table` file for every
  PLIDA DIL structure. The full surface contains 2,140 structures across 559
  products. Each canonical file has at most `complete_dil_rows` synthetic
  rows; the default cap is 100 rows. Richer bespoke generator outputs remain
  separate and retain their normal row counts.
- `complete_dil_schema` now defaults to `TRUE` for a full `products = "all"`
  build with no exclusions and to `FALSE` for a partial build. Set it to
  `TRUE` explicitly when a partial build also needs canonical DIL companions.
- The administrative/Census audit now fails when a canonical variable is
  entirely missing. It writes `unresolved-admin-variables.csv` for remediation.
- The fixed-seed 4 August 2026 audit observed all 2,140 PLIDA structures and
  all 62 BLADE tables. It found no missing official variables, placeholders,
  generic category codes, or official-domain errors. 55,586 of 56,994
  administrative and Census occurrences carry a value, a rate of 97.5 per
  cent. 1,408 occurrences remain entirely missing and 278 carry a
  reference-period warning. Both sets are listed in the audit report and
  remain open work.
- Canonical table completion preserves unsupported code frames as typed
  missing values. It does not insert generic category codes or fabricated
  labels. Canonical schema coverage does not establish statistical fidelity
  or validate synthetic values.
- `export_base_file` now defaults to `FALSE` for Parquet and CSV builds. Set it
  to `TRUE` to retain `_system/base-spine.parquet`; a CSV build then also
  exports `base-spine-v6/base-spine-v6.csv`. Product files and agency spine
  files remain unchanged.

## Cross-cutting code-frame foundation

- New `codeframes.rs` is the single source of truth for the classifications
  that must agree across datasets: SACC 2016 country of birth (255 codes,
  weighted by real ABS Census 2021 counts), ASGS 2021 geography (SA2->SA3->
  SA4->state from the meshblock lookup), state/AVETMISS codes, ASCL language,
  ASCRG religion, and visa subclasses. Reference tables are embedded via
  `include_str!` and parsed once into `LazyLock` statics; per-row use is an
  O(1)/O(log k) lookup with no allocation.
- New `seeds.rs` centralises the seed-offset registry.

## Spine

- New columns: `country_of_birth_sacc` (real SACC code; the authoritative
  cross-dataset country identity), `sa2_code`/`sa3_code`/`sa4_code` (ASGS
  geography consistent with state), and `household_id` (central household
  assignment: age-matched couples, single/sole-parent households, children
  attached to parenting-age households). Each is added via an independent
  sub-RNG, so every pre-existing column is bit-identical. The spine is now 54
  columns and the template version is bumped to v2.

## Cross-dataset consistency

- Country of birth is consistent across all ten country-bearing datasets
  (Census BPLP, CORE, DEX, NACDC, A&T, VISA, SDB, MT_DEMOGS, DEATHS, DOMINO):
  a person resolves to the same SACC country everywhere, replacing the prior
  0/1 born-overseas flag emitted under SACC-coded column names.
- CORE relationships are derived from the spine `household_id`, so partners
  and parent-child links are guaranteed co-resident.

## Per-dataset fidelity

- Census: BPLP from the spine SACC code; RELP and LANP rebuilt from the full
  ASCRG religion and ASCL language classifications; ASGS geography columns.
- NDIS has bespoke generators for three products — participants (enriched with
  NDISDSBLTYGRPNM/ICDDSBLTYNM/SVRTYSCR/CHC_FLAG disability detail),
  plansupports and payments. They use DIL column names and provisional NDIS
  code frames. Exact code-frame provenance remains unresolved.
- DEX has bespoke generators for three normalised tables: special_client,
  special_attendance, and special_client_assessment. They use DIL field names
  and provisional Data Exchange Protocols v11 code frames. The remaining DEX
  tables and exact code mappings remain unresolved.
- DEATHS cause_of_death now emits proper ENTITY1-20 + RACS1-20 multiple-cause
  columns with consistent RECORD_AXIS_COUNT (was one pipe-joined string).
- BIRTHS restricted to the 2006+ registration window.
- PIT_IE income components are log-normal; Home Affairs SDB grant-after-
  arrival, AMEP official names, VISA program decollinearised; TVA AVETMISS
  two-character state codes; AEDC 2024 cycle.

# fplida 0.2.0

This release updates fplida to the 19 March 2026 PLIDA data item list
and moves STP to the new DIL raw table shape.

## Employer-to-BLADE linkage fix

- Payment-summary (`PIT_PS`) `EMPLOYER_ABN` and Single Touch Payroll
  (`STP`) `BN` values now resolve to real BLADE businesses. Previously
  these products minted standalone employer identifiers from a private
  hash space, so they did not match any `bn` in the BLADE business spine
  or the `_system/plida-blade-link` crosswalk (0% linkage).
- A process-global business-number pool (`set_business_pool__`,
  `business_pool_size__`, Rust module `business_pool`) is populated from
  the BLADE business spine before the employer-linked products run. The
  PIT_PS and STP generators draw employer identifiers from this pool, so
  every employer is a real BLADE `bn`. When the pool is unset (a build
  with no BLADE stage, or a standalone product call), generators fall
  back to their previous self-contained identifier scheme.
- Sliced builds carry a thin `_system/business-bn-pool.parquet` into each
  slice so workers draw from the same business universe as the central
  BLADE stage.
- PIT_PS and STP now agree on each person's employers. Both products map a
  person's primary job to employer slot 0 and secondary job to a reserved
  slot via a shared `business_pool::employer_bn(person_number, slot, seed)`,
  keyed on the global person number parsed from the spine id. A person's
  primary employer is therefore the same BLADE business in PAYG and STP
  (100% agreement among persons present in both, up from 0%). PAYG retains
  employer switching: a switcher's prior-employer row is the business from
  the previous employer spell.

## DIL metadata

- Metadata refreshed from `PLIDA data item list - 19MAR2026.xlsx`.
  The package metadata covers 48 dataset rows, 559 products, and
  67,398 variable rows from the DIL workbook.
- Added APSC linkage support and schema-complete DIL-backed generators
  for `apsed`, `ato_mcs`, `ers`, `jk`, `jm`, `lfs`, `nhs`, `nsmhw`,
  `pex`, and `smsf`.
- Forty-four build targets are now supported: `spine`, `census`,
  `pit_ps`, `pit_itr`, `core`, `he`, `domino`, `mbs`, `pbs`, `tva`,
  `combined`, `births`, `deaths`, `mcd`, `ato_cr`, `visa`,
  `mt_demogs`, `sdb`, `travellers`, `pit_ie`, `busown`, `sae`, `cgt`,
  `rps`, `stp`, `ndis`, `apprentice`, `dex`, `air`, `amep`, `nacdc`,
  `aedc`, `acld`, `sdac`, `ers`, `jk`, `jm`, `nhs`, `nsmhw`, `pex`,
  `apsed`, `ato_mcs`, `lfs`, and `smsf`.

## STP

- STP now writes the 2026 DIL raw shape: monthly standard and extended
  pay-event tables, financial-year standard and extended jobs tables,
  and financial-year extended ETP tables. The generator keeps the
  existing person/employer employment behaviour but maps it into the
  new raw tables.

## Build behaviour

- `build_fplida()` now defaults to `years = 2015:2025`, and MBS/PBS
  now default to calendar years `2006:2025`.
- Slice workers now surface per-product errors from child processes
  instead of silently continuing after failed product builds.
- CSV exports now default to a messier PLIDA-like layout: every data
  product is emitted as a top-level folder, agency grouping folders are
  omitted except for BLADE under `abs-blade/` and STP under
  `ato-stp/stp-standard/` or `ato-stp/stp-extended/`, agency spines are
  emitted as top-level `*-spine-v6` folders, and selected PIT/PAYG, MBS,
  and PBS year products have intentionally inconsistent column names.

# fplida 0.1.0

First release.

## Core

- `build_fplida()` — parallel slice-based build orchestrator. Generates
  a synthetic person spine once, then runs each PLIDA product generator
  in parallel across disjoint person slices via `parallel::parLapply()`.
  The merged output directory has one folder per agency-dataset, with
  each product written as a partition of `part-NNN.parquet` files
  readable via `arrow::open_dataset()`.
- Thirty-four build targets supported: `spine`, `census`, `pit_ps`,
  `pit_itr`, `core`, `he`, `domino`, `mbs`, `pbs`, `tva`, `combined`,
  `births`, `deaths`, `mcd`, `ato_cr`, `visa`, `mt_demogs`, `sdb`,
  `travellers`, `pit_ie`, `busown`, `sae`, `cgt`, `rps`, `stp`, `ndis`,
  `apprentice`, `dex`, `air`, `amep`, `nacdc`, `aedc`, `acld`, `sdac`.

## CSV export

- `convert_parquet_dir_to_csv()` — walks a parquet run directory and
  rewrites every output as CSV via a persistent DuckDB connection.
  Benchmarked at ~6× `arrow::write_csv_arrow` and ~85×
  `arrow::read_parquet + data.table::fwrite` on 30 million rows.
- `build_fplida(export_format = "csv")` — builds to parquet internally
  (all Rust fast paths are parquet-only) and invokes
  `convert_parquet_dir_to_csv()` at the end. `keep_parquet = FALSE`
  deletes the parquet run directory after conversion.

## Performance

- All generators are implemented in Rust via `extendr`. Per-generator
  hot loops and parquet writes happen entirely in Rust with no R-side
  data.frame materialisation.
- MBS, PBS, PIT_PS, and PIT_ITR use `rayon::par_iter` across years
  inside their consolidated `*_full_to_parquet__` entry points.
- PIT_PS and PIT_ITR workers use shared `Arc<ArrayRef>` columns across
  sub-table parquet writes to avoid allocator contention at 30 million
  row scale.
- Spine generation uses a template cache: a ~100k-person template is
  built once per seed, then sampled with replacement to produce any
  target N in seconds.
- Benchmark on a 10-core Apple silicon machine, before the 2026 DIL
  expansion: 30 million persons, all 34 original products, 2 hours
  11 minutes end-to-end for parquet output, plus 14 minutes for CSV
  conversion (707 GB of CSV).

---
name: fplida
description: Answer questions about Australia's PLIDA (formerly MADIP) and BLADE microdata — which datasets, products, tables and variables exist, what their values and codes mean, and which years they cover — and generate synthetic data with that structure. Use whenever a task touches PLIDA, MADIP, BLADE, the ABS DataLab or Australian linked administrative data, including questions about specific datasets (Census, MBS, PBS, DOMINO, PIT, STP, NDIS and others), finding the right variable for an analysis, planning a data linkage project, or writing and testing DataLab code before DataLab access.
---

# fplida

`fplida` is an R package with a Rust backend. It does two things.

It carries the published ABS metadata for PLIDA and BLADE: 44 datasets, 623
products, 2,204 tables and 72,656 variable occurrences, with descriptions,
value domains, sources and reference periods. Most questions about what PLIDA
or BLADE contains can be answered from this alone — no data generated, no
DataLab access, and (through the website or the raw registry files) no R
either.

It also generates synthetic data with that structure. Table names, file layout,
variable names and value codes follow the Data Item Lists, so code written
against `fplida` output has a good chance of running unchanged inside the
DataLab.

PLIDA is the asset formerly named MADIP, and the product names still carry the
old prefix (`madipge-...`). Treat a MADIP question as a PLIDA question.

The package is not affiliated with the ABS. It contains no confidential data
and reproduces no real person or business.

## Choose a route

- **What is in a dataset; what a variable means; what values it takes; which
  years it covers** — read the bundled registries. In R, `dataset_info()` and
  `variable_info()` answer instantly with nothing generated. Without R, use
  the website or the raw registry files. See "Look up metadata".
- **Find the right variable for an analysis** — `variable_info(topic = ...)`,
  or search the website.
- **Write or test DataLab code** — generate a small build (`n = 10000`) and
  develop against it. See "Generate data".
- **Practise linking datasets** — generate two products and join them through
  the agency spines. See "Join across datasets".
- **Estimate anything about the real population** — stop. See "Read this
  first".

## Read this first

The synthetic values are not validated. The author states plainly that this is
an AI-implemented project that has not been quality assured. Treat the
structure as reliable and the numbers as illustrative.

Specifically, do not use this data to:

- estimate anything about the real Australian population;
- benchmark a method's statistical performance; or
- draw a substantive conclusion of any kind.

Do use it to:

- write and debug data-processing code before you have DataLab access;
- work out which dataset, table and variable you need;
- test that a pipeline handles the real schema, missing-value codes and joins; and
- prototype an analysis so you arrive at the DataLab with working code.

Known limitations, as at August 2026:

- `build_fplida()` picks its worker count from the host CPU count by default,
  so the same seed gives different data on machines with different core counts.
  Pass `k_slices` explicitly if you need reproducibility across machines.
- DOMINO `BEN_STATUS` is `"CUR"` on nearly every spell (a small share are
  `"SUS"`), including spells that have closed and carry an end date and an end
  reason. Do not read it as a point-in-time status.
- Values are not calibrated. Marginals for the anchored products follow
  published ABS rates where the package has a source for them, but most
  distributions are approximations and none has been validated.

Some cross-product agreements do hold, after fixes in 0.3.0: MBS and PBS emit
no claims dated before birth; PBS takes the patient's birth month from the
spine; DOMINO reads its death dates and birth months from the spine and stops
paying benefits at death; CORE and Census agree on where a person lives;
higher education campus postcodes use the correct state map; and SDAC anchors
disability status on the spine while preserving the published prevalence.

Note on ATO occupation codes, which is a difference rather than a defect: the
PIT occupation fields hold real
[ATO salary and wage occupation codes](https://www.ato.gov.au/forms-and-instructions/salary-and-wage-occupation-codes),
not ANZSCO, because that is what the source holds. Each person's ANZSCO
occupation from the spine is mapped onto an ATO code through a bundled
crosswalk, so the occupation stays consistent across products while the code
system matches the source. Census `OCCP` remains ANZSCO. Do not join the two
on code equality.

If a cross-dataset agreement matters to your task, verify it on the generated
data rather than assuming it.

## Install

Needs R 4.1.0+ and Rust 1.81+. There is no binary release; it compiles from
source, and the first install takes a few minutes.

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# Windows only, because R links against Rtools rather than MSVC:
rustup target add x86_64-pc-windows-gnu
```

```r
install.packages("remotes")
remotes::install_github("wfmackey/fplida")
```

`arrow` installs with the package. CSV output additionally needs `DBI` and
`duckdb`; without them, parquet builds still work.

## Look up metadata in R

`dataset_info()` and `variable_info()` read bundled registries. They need no
generated data and run instantly. Reach for these first — most questions about
"what is in PLIDA" are answered here.

```r
library(fplida)

dataset_info("MBS")                              # one dataset
dataset_info(asset = "BLADE")                    # filter by asset; BLADE is one dataset
variable_info(asset = "BLADE")                   # every BLADE variable (5,258)
variable_info("PIT_ITR", topic = "income")       # filter by topic
variable_info("CENSUS", value_support_status = "supported")
```

`variable_info()` returns one row per variable occurrence with 38 columns. The
useful ones are `dataset`, `product`, `table`, `variable`,
`official_description`, `variable_type`, `reference_period`, `valid_values`,
`value_source`, `value_source_url`, `value_support_status` and `topic_tags`.

`valid_values` is a JSON array, but it is the literal `[]` for most rows —
only 3,574 of the 72,656 occurrences carry a value list. Where present,
entries are either `"code: label"` strings or bare codes with no label.

Filters: `dataset`, `asset` (`"PLIDA"` or `"BLADE"`), `topic`,
`collection_type` (`"administrative"` or `"survey"`), `record_type`
(`"variable"` or `"linking_key"`), `value_support_status`.

Topics: `aged_care`, `agriculture`, `births_deaths`, `business`,
`data_quality`, `date_time`, `demographic`, `digital_technology`,
`disability_caring`, `education_training`, `employment`, `energy_environment`,
`family_household`, `finance_accounting`, `geography`, `health`, `housing`,
`id`, `income`, `industry`, `innovation_research`, `intellectual_property`,
`legal_insolvency`, `migration_citizenship`, `payroll`,
`program_service_delivery`, `social_security`, `superannuation`,
`survey_design`, `taxation`, `trade`, `travel`.

`value_support_status` has three values, and `supported` does not mean what the
name suggests. It is the residual category: every administrative occurrence not
specifically flagged, 55,861 of 57,021. Only 6.4% of `supported` rows actually
carry a code list; the other 93.6% have `valid_values` set to `[]` and
`value_domain` "not specified". `unsupported` (1,160) means the registry has
flagged a known value-mapping problem, so the column is written as typed
missing rather than given invented codes. `not_applicable` (15,635) means a
survey variable, outside the value scope.

So do not filter on `value_support_status == "supported"` to find variables
with known value domains — filter on `valid_values != "[]"` instead. A
`supported` status also says nothing about the realism of the generated column,
and does not guarantee the column is populated in the canonical companion
files.

## Look up metadata without R

Everything the registries hold is also published, so a metadata question can
be answered with a web fetch when R or the package is unavailable.

The website has a page per dataset listing every product, table and variable,
with descriptions, types, value domains, values and sources:

- Index of all datasets: <https://wfmackey.github.io/fplida/articles/datasets.html>
- One dataset: `https://wfmackey.github.io/fplida/articles/dataset-<slug>.html`,
  where the slug is the dataset code lowercased with runs of non-alphanumeric
  characters replaced by hyphens. `MBS` → `dataset-mbs`, `PIT_PS` →
  `dataset-pit-ps`, `A&T` → `dataset-a-t`, `MT_DEMOGS` → `dataset-mt-demogs`.

The registries themselves can be downloaded from the repository and queried
with any CSV tool:

- <https://raw.githubusercontent.com/wfmackey/fplida/main/inst/dataset-info.csv>
  — one row per dataset (24 KB).
- <https://raw.githubusercontent.com/wfmackey/fplida/main/inst/variable-info.csv.gz>
  — one row per variable occurrence, 38 columns, gzipped CSV (2.8 MB). The
  same table `variable_info()` returns.

## Generate data

Set an output directory first. Note that `set_data_path()` also writes
`FPLIDA_DATA_PATH` into your `~/.Renviron`, so it affects every later R session
and every other project, not just this one. Give it a durable path — a
`tempdir()` path is lost when the session ends, leaving a stale entry behind.

```r
set_data_path("~/fplida-data")
```

Then either build many products at once:

```r
result <- build_fplida(
  n        = 10000,
  seed     = 42,
  products = c("census", "pit_ps", "mbs"),
  k_slices = 4          # set explicitly for cross-machine reproducibility
)
result$canonical_run_dir
```

or generate one dataset at a time. Every `generate_*()` needs a spine, so
build that first:

```r
spine <- generate_spine(n = 10000, seed = 42)
generate_census(spine = spine, seed = 42)
generate_mbs(spine = spine, seed = 42, years = 2015:2020)
```

`build_fplida()` covers 2015–2025 by default; pass `years` for a longer or
shorter window. Longitudinal datasets such as MBS reach back to 2006.

### Product tokens

`build_fplida(products = ...)` takes these 45 tokens, which are not always the
dataset code — note `apprentice` for the `A&T` dataset:

```
spine census pit_ps pit_itr core blade he domino mbs pbs tva combined births
deaths mcd ato_cr visa mt_demogs sdb travellers pit_ie busown sae cgt rps stp
ndis apprentice dex air amep nacdc aedc acld sdac ers jk jm nhs nsmhw pex
apsed ato_mcs lfs smsf
```

Three rules are applied silently: `spine` is always added; `pit_itr` pulls in
`pit_ps`; `blade` pulls in `core`. For everything-except builds, keep
`products = "all"` and pass `exclude_products` instead.

### Scale

`build_fplida()` defaults to `n = 1000000`, which takes a long time and writes
many gigabytes. Start at 10,000 and increase once you know the cost on your
machine. A full `products = "all"` build also writes a canonical
`product--table` file for every one of the 2,140 PLIDA structures, capped at
`complete_dil_rows` rows (100 by default).

## Read the output

Parquet by default. The run directory holds one directory per dataset, named
`<agency>-<dataset>`. Inside each is that agency's spine file plus one
directory per product, each holding `part-NNN.parquet` files.

The only loose `.parquet` in a dataset directory is the agency spine, so
filtering for `.parquet` there gets you the spine, not the data. Point
`open_dataset()` at the product directory instead.

```r
library(arrow)
run <- result$canonical_run_dir
list.files(run)                             # dataset dirs: abs-census, dhda-mbs, ...
list.files(file.path(run, "abs-census"))    # abs-spine.parquet + one dir per product

census <- as.data.frame(
  open_dataset(file.path(run, "abs-census", "madipge-cen21-d-person-2021"))
)
```

For CSV output, pass `export_format = "csv"` to `build_fplida()`. That needs
`DBI` and `duckdb`. CSV builds default to `messy_files = TRUE` and
`messy_names = TRUE`, which reproduce file-layout and variable-naming quirks
found in the DataLab — turn both off if you want tidy output.

## Join across datasets

Most products are projections from one shared synthetic person spine, so the
same person has the same demographics, geography and wage history everywhere.
But no product carries a global person ID. Each dataset directory has an
agency spine — `abs-spine.parquet`, `ato-spine.parquet` and so on — with
exactly two columns: `spine_id` and `SYNTHETIC_AEUID`, that agency's
identifier. Product tables carry `SYNTHETIC_AEUID`. Joining across agencies
means going through both spines, the same way you would in the DataLab:

```r
library(arrow); library(dplyr)
run <- result$canonical_run_dir

# Products are directories; pick the year you want.
mbs <- open_dataset(file.path(run, "dhda-mbs", "madipge-mbs-d-claims-2018")) |>
  collect()
pit <- open_dataset(list.dirs(file.path(run, "ato-pit_ps"),
                              recursive = FALSE)[1]) |>
  collect()

# Drop unlinked records before joining: spine_id is NA for a slice of every
# agency spine, and dplyr joins match NA to NA, cross-joining the unlinked.
mbs_ids <- read_parquet(file.path(run, "dhda-mbs", "dhda-spine.parquet")) |>
  filter(!is.na(spine_id)) |> rename(aeuid_dhda = SYNTHETIC_AEUID)
ato_ids <- read_parquet(file.path(run, "ato-pit_ps", "ato-spine.parquet")) |>
  filter(!is.na(spine_id)) |> rename(aeuid_ato = SYNTHETIC_AEUID)

crosswalk <- inner_join(mbs_ids, ato_ids, by = "spine_id")

linked <- mbs |>
  inner_join(crosswalk, by = c("SYNTHETIC_AEUID" = "aeuid_dhda")) |>
  inner_join(pit,       by = c("aeuid_ato" = "SYNTHETIC_AEUID"))
```

Two things to expect, both deliberate:

- The `NA` `spine_id` rows are linkage failures by design, as in the real
  asset. Inner joins drop those people silently; decide how the analysis
  should treat unlinked records rather than letting the join decide.
- Administrative products observe different populations. MBS observes
  Medicare-subsidised services; NDIS observes scheme participants. Do not
  treat a program population as an unconditional denominator.

BLADE is built from its own business spine, plus a synthetic
person-to-business link file (with employee, secondary-job and owner
relationships) for practising PLIDA-to-BLADE joins. That link file is an
`fplida` feature, not a published BLADE product.

## Reproducibility

A fixed seed gives repeatable output for the same package version, options
and toolchain, but only if you also pin `k_slices`. A package update can change
generated output.

## Getting more detail

- Every dataset and variable: <https://wfmackey.github.io/fplida/>
- Function reference: `?build_fplida`, `?variable_info`, `?dataset_info`
- Source and issues: <https://github.com/wfmackey/fplida>

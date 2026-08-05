---
name: fplida
description: Answer questions about Australia's PLIDA and BLADE microdata — which datasets, products, tables and variables exist, what values they take, and which years they cover — and generate synthetic data with that structure. Use when asked what a PLIDA or BLADE dataset or variable contains, when finding the right variable for an analysis, or when writing and testing DataLab code outside the DataLab.
---

# fplida

`fplida` is an R package with a Rust backend. It does two things.

It carries the published ABS metadata for PLIDA and BLADE: 44 datasets, 623
products, 2,204 tables and 72,656 variable occurrences, with descriptions,
value domains, sources and reference periods. Most questions about what PLIDA
or BLADE contains can be answered from this alone, with no data generated and
no DataLab access. That is usually the fastest thing you can do with this
package — see "Look up metadata" below.

It also generates synthetic data with that structure. Table names, file layout,
variable names and value codes follow the Data Item Lists, so code written
against `fplida` output has a good chance of running unchanged inside the
DataLab.

The package is not affiliated with the ABS. It contains no confidential data
and reproduces no real person or business.

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
- DOMINO `BEN_STATUS` is `"CUR"` on every spell, including spells that have
  closed and carry an end date and an end reason. Do not read it as a
  point-in-time status.
- Values are not calibrated. Marginals for the anchored products follow
  published ABS rates where the package has a source for them, but most
  distributions are approximations and none has been validated.

The cross-product consistency defects that were listed here have been fixed:
MBS and PBS no longer emit claims dated before birth; PBS takes the patient's
birth month from the spine; DOMINO reads its death dates and birth months from
the spine and stops paying benefits at death; CORE and Census agree on where a
person lives; higher education campus postcodes use the correct state map; and
SDAC anchors disability status on the spine while preserving the published
prevalence.

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

## Look up metadata without generating anything

`dataset_info()` and `variable_info()` read bundled registries. They need no
generated data and run instantly. Reach for these first — most questions about
"what is in PLIDA" are answered here.

```r
library(fplida)

dataset_info("MBS")                              # one dataset
dataset_info(asset = "BLADE")                    # filter by asset; BLADE is one dataset
variable_info(asset = "BLADE")                   # every BLADE variable (5,258)
variable_info("PIT_ITR", topic = "income")       # filter by topic
variable_info("CENSUS", value_support_status = "sourced")
```

`variable_info()` returns one row per variable occurrence with 38 columns. The
useful ones are `dataset`, `product`, `table`, `variable`,
`official_description`, `variable_type`, `reference_period`, `valid_values`,
`value_source`, `value_source_url`, `value_support_status` and `topic_tags`.

`valid_values` is a JSON array, but it is the literal `[]` for most rows —
9,101 of the 72,656 occurrences carry a value list, of which 3,955 are sourced
and 5,146 guessed. Where present, entries are either `"code: label"` strings or
bare codes with no label.

An empty `valid_values` does not mean the domain is unknown. Some published
classifications are too large to carry — mesh blocks run to 358,010 codes — so
the registry records the domain, its source and its size, and leaves the list
empty. Read `value_definition` to tell the two cases apart.

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

`value_support_status` records where a variable's values came from, and has
four values.

- `sourced` (24,285) — the codes come from a published classification or code
  list, and `value_source` names it. This is the only status where the source
  confirms the mapping.
- `guessed` (35,626) — the codes are inferred from the variable's name and
  description. A variable ending `_STATE` gets the Australian state codes on
  that basis alone. The column holds plausible values of the right shape; the
  source does not confirm them. Useful for exercising a pipeline, not for
  interpreting a code.
- `unsupported` (7) — research looked and found nothing defensible, so the
  column is written as typed missing rather than given invented codes. Two AEDC
  school-administration fields and one ATO code box reach this bar.
- `not_applicable` (12,738) — a survey variable no rule matched, outside the
  value scope.

If you need a value domain you can rely on, filter on `sourced`. If you need a
column that is merely populated, `sourced` or `guessed` will both do. The
status says nothing about the realism of the generated column, and does not
guarantee the column is populated in the canonical companion files.

Note this was renamed: `supported` became `sourced` and `guessed` was added.
Older code filtering on `"supported"` returns nothing.

The same information is browsable at
<https://wfmackey.github.io/fplida/>.

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
`pit_ps`; `blade` pulls in `core`.

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

## How the data hangs together

Most products are projections from one shared synthetic person spine holding
demographics, geography, education, work, income, disability, household and
linkage fields. Because products share it, they are intended to agree with each
other — the same person has the same wage history in PAYG, STP and derived
income products. BLADE is built from a separate business spine, plus a
synthetic person-to-business link file for practising joins.

Each dataset also gets an agency spine (`abs-spine.parquet`,
`ato-spine.parquet`, and so on) mapping the person to that agency's
synthetic identifier, which is how you join across datasets — the same way you
would in the DataLab.

See the caveats above: the intent is cross-product agreement, but several
generators currently break it.

## Reproducibility

A fixed seed gives repeatable output for the same package version, options and
toolchain, but only if you also pin `k_slices`. A package update can change
generated output.

## Getting more detail

- Every dataset and variable: <https://wfmackey.github.io/fplida/>
- Function reference: `?build_fplida`, `?variable_info`, `?dataset_info`
- Source and issues: <https://github.com/wfmackey/fplida>

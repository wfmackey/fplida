# fplida

[![R-CMD-check](https://github.com/wfmackey/fplida/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wfmackey/fplida/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/wfmackey/fplida/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/wfmackey/fplida/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

`fplida` generates synthetic data with the (broad) structure of Australia's Person Level Integrated Data Asset (PLIDA), and of the Business Longitudinal Analysis Data Environment (BLADE).

Write and test your Datalab code before you get to the Datalab. The tables, the file layout, the variable names and the value codes match the published Data Item Lists, so code that runs here has a good chance of running inside the Datalab unchanged.

Documentation, including a page for every dataset, variable and (almost) value, is at [wfmackey.github.io/fplida](https://wfmackey.github.io/fplida/).

As this package contains a lot of information about PLIDA and BLADE data, this repo also comes with a generic AI agent 'fplida-skill.md' that you can download or point your AI to if you're using one.

#### Why this package exists
I made this package for four reasons:

1. Teaching people how to actually use administrative data within Datalab is difficult, and having an out-of-Lab set of data to demonstrate data processing concepts on is useful.
2. Outside of Datalab, much of the documentation on what variables in specific datasets actually represent and what their values actually mean exist entirely on agency websites or deep in PDFs somewhere. This can make it hard to plan a project. This package offers an attempt to collate a lot of that public knowledge in a single place. This part is still a work in progress.
3. It is increasingly useful to get AI to spitball and then implement research ideas, and a useful way to do this is to let it have a go on a synthetic database to identify issues or best approaches or just to tell you that it was a silly impractical idea to begin with.
4. I wanted to see how Claude Code (Opus 4.8 then 5) would go on a relatively large coding project in an efficient-but-complex language I do not understand (Rust). Despite it doing a pretty good job (imo), this introduces obvious quality risks; see _Warnings_.

#### Warnings

1. That last point above is important for any users: this is a fully AI-implemented project and, beyond using a subset of the outputs for teaching, I have not thoroughly or even lightly quality assured this synthetic data.
2. This project is in no way affiliated with the [Australian Bureau of Statistics](https://www.abs.gov.au/). It does not use any [Datalab](https://www.abs.gov.au/statistics/microdata-tablebuilder/datalab)-sourced data to form its person spine. It relies only on publicly available [Census DataPacks](https://www.abs.gov.au/census/find-census-data/datapacks) to inform spine and dataset generation behaviour; and it relies on publicly available [PLIDA](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/person-level-integrated-data-asset-plida) and [BLADE](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/business-longitudinal-analysis-data-environment-blade) metadata. The data do not reproduce any real person or business.
3. The datasets generated are internally consistent at a very high level, but do not think of this as fully simulated model of the Australian population. There will be many things that don't make sense.

#### Contribute


## Installation

`fplida` has an R front end and a Rust back end, so you need both toolchains.
There is no binary release; the package compiles from source.

You need R 4.1.0 or later, and Rust 1.81 or later. Install Rust with
[`rustup`](https://rustup.rs/), which is the supported route on every
platform:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

On Windows, also add the GNU target, because R links against the Rtools
toolchain rather than the MSVC one:

```bash
rustup target add x86_64-pc-windows-gnu
```

Then install the package, with either `pak` or `remotes`:

```r
# pak
install.packages("pak")
pak::pak("wfmackey/fplida")
```

```r
# remotes
install.packages("remotes")
remotes::install_github("wfmackey/fplida")
```

Parquet is the internal format for every build, so `arrow` installs with the
package. CSV output additionally needs `DBI` and `duckdb`:

```r
install.packages(c("DBI", "duckdb"))
```

The first install compiles the Rust crate, which takes a few minutes. Later
installs reuse the compiled artefacts.

## What the package produces

### PLIDA

The bundled PLIDA metadata comes from the 19 March 2026 Data Item List. It
covers:

- 43 dataset codes;
- 559 products;
- 2,140 product-table structures; and
- 67,398 variable occurrences, over 13,656 distinct dataset-and-variable pairs.

The datasets span tax, superannuation, payroll and business ownership; Census,
CORE and combined demographics; Medicare, medicines, immunisation and Medicare
registration; income support, disability and social services; education,
apprenticeships and vocational training; migration, visas, settlement and
travel; aged care and early childhood development; and birth and death
registrations.

Five PLIDA datasets are surveys rather than administrative collections:
`SDAC`, `NHS`, `NSMHW`, `PEX` and `LFS`. Their structures are generated, but
the registry does not assess their values.

### BLADE

The bundled BLADE metadata comes from the April 2026 Data Item List. It
covers 62 tables and 5,246 variable occurrences, plus 12 linking keys.

BLADE is built from its own business spine. The build also writes a synthetic
person-to-business relationship file so you can practise linking BLADE to
PLIDA. That relationship file is an `fplida` feature, not a published BLADE
product.

Twenty-five of the 62 BLADE tables are survey sources. Like the PLIDA surveys,
their values are not assessed.

### Value coverage

Across both assets the registry holds 72,656 variable occurrences, which
reduce to 18,540 distinct dataset-and-variable pairs. Each occurrence records
where its values came from:

- `sourced` (24,285) — the codes come from a published classification or code
  list. `value_source` names it. This is the only status where the mapping is
  confirmed by the source.
- `guessed` (35,626) — the codes are inferred from the variable's name and
  description, not from a published list. A variable whose name ends `_STATE`
  is given the Australian state and territory codes on that basis alone. The
  generated column holds plausible values of the right shape, which is more
  useful than an empty column, but the source does not confirm them. Treat
  them as a placeholder, not a mapping.
- `unsupported` (7) — research looked and found nothing defensible, so the
  column is written as typed missing rather than given invented codes. Two
  AEDC school-administration fields and one ATO code box reach this bar.
- `not_applicable` (12,738) — a survey variable the rules could not infer,
  outside the value scope.

Guessing and research together raised the number of occurrences carrying an
actual value list from 3,574 to 9,101. Where a variable has a list, generation
draws from it rather than from the generator's own name heuristics, so a
`_STATE` column holds 1 to 9 rather than an arbitrary categorical.

An empty list does not always mean an unknown domain. Some published
classifications are too large to carry — mesh blocks run to 358,010 codes — and
some could only be sampled, which is not the same as knowing the domain. In
both cases the registry records the domain, its source and its size, and
`value_definition` says which case applies.

The status describes the registry, not the realism of the generated column.
Some distributions are approximations. Check `variable_info()` before you rely
on a product for a realistic exercise.

## Using the package

### Find out what exists

`dataset_info()` and `variable_info()` read the bundled registries. Neither
needs generated data, so they work the moment the package is installed.

```r
library(fplida)

dataset_info("MBS")
#>   dataset            MBS
#>   dataset_name       Medicare Benefits Schedule
#>   supplier           DHDA
#>   reference_period   2006 to current
#>   update_frequency   Quarterly
#>   information_url    https://www.mbsonline.gov.au/
```

`variable_info()` returns one row per variable occurrence across 38 columns:

```r
variable_info("CENSUS")
#>   dataset                CENSUS
#>   product                madip-ge-030101d-census2016-census2016person-2016
#>   table                  census_2016_person
#>   variable               SEXP
#>   official_description   Sex
#>   variable_type          categorical
#>   reference_period       2011, 2016, 2021
#>   valid_values           ["1: Male","2: Female"]
#>   value_source           ABS Census data item list
```

Filter with any combination of `dataset`, `asset` (`"PLIDA"` or `"BLADE"`),
`topic`, `collection_type` (`"administrative"` or `"survey"`), `record_type`
(`"variable"` or `"linking_key"`) and `value_support_status`:

```r
variable_info("PIT_ITR", topic = "income")       # income variables in one dataset
variable_info(asset = "BLADE")                   # all 5,258 BLADE variables
variable_info(topic = "disability_caring")       # across every dataset
variable_info(record_type = "linking_key")       # the 12 BLADE linking keys
```

The 32 topic tags are `aged_care`, `agriculture`, `births_deaths`, `business`,
`data_quality`, `date_time`, `demographic`, `digital_technology`,
`disability_caring`, `education_training`, `employment`, `energy_environment`,
`family_household`, `finance_accounting`, `geography`, `health`, `housing`,
`id`, `income`, `industry`, `innovation_research`, `intellectual_property`,
`legal_insolvency`, `migration_citizenship`, `payroll`,
`program_service_delivery`, `social_security`, `superannuation`,
`survey_design`, `taxation`, `trade` and `travel`.

Most variables have no code list, and `variable_info()` packs the ones that do
into a JSON string in `valid_values`. Use `variable_values()` instead: it
returns one row per code, once per variable rather than once per occurrence.

```r
variable_values("mbs", "billtypecd")
#>   dataset   variable code                       label
#> 1     MBS BILLTYPECD    D Direct billed (bulk billed)
#> 2     MBS BILLTYPECD    P             Patient billed
```

Names are matched without regard to case, so there is no need to shout. It
takes the same `dataset` filter as `variable_info()`, plus
`value_support_status` to exclude codes that were inferred from a variable's
name rather than published:

```r
# Which DOMINO variables have a published code list at all.
unique(variable_values("domino", value_support_status = "sourced")$variable)
```

### Generate data

Set an output directory first. Use a durable path if you want the data to
survive the session.

```r
set_data_path("~/fplida-data")
```

Generate a few products:

```r
result <- build_fplida(
  n = 10000,
  seed = 42,
  products = c("census", "pit_ps", "mbs")
)

result$canonical_run_dir
```

Generate everything. There are 45 build targets: the shared person spine, the
43 PLIDA datasets, and the BLADE module.

```r
result <- build_fplida(n = 100000, seed = 42)
```

Start small. The default is `n = 1000000`, and a full build at that size takes
a long time and writes many gigabytes. Raise `n` once you know the runtime and
disk cost on your machine.

Ask for CSV instead of parquet:

```r
result <- build_fplida(
  n = 10000,
  seed = 42,
  products = c("census", "pit_ps"),
  export_format = "csv"
)
```

Each PLIDA dataset also has its own generator, such as `generate_census()` or
`generate_mbs()`, if you want one dataset without the rest. They all need a
spine, so call `generate_spine()` first.

### Read the output

Output is parquet by default, one directory per product:

```r
library(arrow)

path <- file.path(result$canonical_run_dir, "abs-census")
census <- read_parquet(list.files(path, "\\.parquet$", full.names = TRUE)[1])
```

## Output layout

Each product has its own directory. CSV output uses a Datalab-like layout, so
a product path can look like this:

```text
madipge-ato-d-context-fy1011/madipge-ato-d-context-fy1011.csv
```

BLADE products sit below `abs-blade/`. STP products sit below
`ato-stp/stp-standard/` and `ato-stp/stp-extended/`.

The canonical Data Item List companion files use a `product--table` file name,
so every table in a multi-table product stays distinct.

CSV builds default to `messy_files = TRUE` and `messy_names = TRUE`. These
reproduce selected file-layout and variable-naming quirks that you meet in the
Datalab.

## How the data is generated

Most products are projections from one shared synthetic person spine. The
spine holds demographics, geography, education, work, income, disability,
household and linkage fields. Because the products share it, they agree with
each other: the same person has the same employment and wage history in PAYG,
in STP and in the derived income products.

A full `products = "all"` build also writes one canonical `product--table`
file for every PLIDA structure, capped at `complete_dil_rows` rows (100 by
default). Partial builds skip these unless you set
`complete_dil_schema = TRUE`.

Administrative products observe different populations. MBS observes
Medicare-subsidised services; NDIS observes scheme participants. Do not treat
a program population as an unconditional denominator.

## Reproducibility and privacy

A fixed seed gives repeatable records for the same package version, options
and toolchain. A package update can change the output.

The package does not reproduce any real person or business. Do not use
synthetic output to make claims about the Australian population.

`fplida` is an independent package. The Australian Bureau of Statistics has
not endorsed, reviewed or approved it, and it is not connected with the ABS or
any other source agency.

## Metadata and evidence

- [`inst/dataset-info.csv`](https://github.com/wfmackey/fplida/blob/main/inst/dataset-info.csv)
  — one row per PLIDA dataset and for BLADE;
- [`inst/variable-info.csv.gz`](https://github.com/wfmackey/fplida/blob/main/inst/variable-info.csv.gz)
  — variable descriptions, sources, value support and topic tags;
- [`inst/plida_metadata`](https://github.com/wfmackey/fplida/tree/main/inst/plida_metadata)
  — the PLIDA Data Item List;
- [`inst/blade_metadata`](https://github.com/wfmackey/fplida/tree/main/inst/blade_metadata)
  — the BLADE Data Item List;
- [`inst/extdata`](https://github.com/wfmackey/fplida/tree/main/inst/extdata)
  — public lookups and code frames; and
- [`inst/foundations`](https://github.com/wfmackey/fplida/tree/main/inst/foundations)
  — calibration settings and their sources.

These registries describe `fplida`. They are not official ABS metadata.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md) for the development and validation
process. Report a defect through the
[GitHub issue tracker](https://github.com/wfmackey/fplida/issues).

## Licence and citation

`fplida` is available under the MIT Licence. See [`LICENSE.md`](LICENSE.md).

The included ABS Data Item List material is © Commonwealth of Australia
(Australian Bureau of Statistics). The ABS makes it available under the
[Creative Commons Attribution 4.0 International Licence](https://creativecommons.org/licenses/by/4.0/),
subject to the [ABS copyright conditions](https://www.abs.gov.au/website-privacy-copyright-and-disclaimer).
`fplida` reformats, subsets and extends that material. The MIT Licence applies
to the `fplida` code, not to third-party material. See
[`inst/NOTICE.md`](https://github.com/wfmackey/fplida/blob/main/inst/NOTICE.md)
for the bundled-material notice.

Use the repository citation panel, or run:

```r
citation("fplida")
```

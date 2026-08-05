# Contributing to fplida

Thank you for your interest in `fplida`. A contribution must preserve the
distinction between official metadata and synthetic implementation choices.

## Before you start

Open an issue before you make a large change. State the affected dataset,
product, table, and variable names.

The project currently gives priority to administrative data and Census. Value
validation for `SDAC`, `NHS`, `NSMHW`, `PEX`, and `LFS` is not complete.
Value validation is also deferred for the 25 BLADE tables classified as
survey sources in the bundled DIL.

## Development setup

Install a current R release and Rust 1.81 or later. Install the R package from
a local checkout:

```bash
export PATH="$HOME/.cargo/bin:$PATH"
R CMD INSTALL .
```

Install the suggested R packages before you run all tests:

```r
install.packages(c("arrow", "DBI", "duckdb", "sf", "testthat"))
```

## Metadata and generated values

Use the applicable source for each type of change:

- Use `inst/plida_metadata` for the official PLIDA structure.
- Use `inst/blade_metadata` for the official BLADE structure.
- Use `inst/extdata` for public code lists and lookups.
- Use `inst/foundations` for sourced calibration settings.
- Use `inst/internal-docs` for value evidence and audit status.

Do not present a generated value list as an official code list. Add a source
URL and evidence status when you introduce or change a value domain.

Do not add confidential or unit-record PLIDA or BLADE data. Use only public
metadata, public aggregate sources, and synthetic output.

## Validation

Run these commands before you submit a pull request:

```r
roxygen2::roxygenise()
testthat::test_local()
```

Then build and check the source package:

```bash
R CMD build . --no-build-vignettes
R CMD check fplida_*.tar.gz --no-manual
```

If a check cannot run, state the reason in the pull request. Include the exact
warning, error, or skipped test.

## Pull requests

Keep a pull request focused on one dataset or one release concern. Describe:

- the source evidence;
- the changed products, tables, and variables;
- the tests that you ran; and
- any value domain that remains approximate or unverified.

Do not commit generated build directories, source tarballs, or check output.

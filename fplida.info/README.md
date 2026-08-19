# fplida.info

What PLIDA and BLADE contain: the datasets, every variable, what its values
can be, and where that was published.

```r
# install.packages("pak")
pak::pak("wfmackey/fplida/fplida.info")

library(fplida.info)

dataset_info("BUSOWN")
variable_info("STP", topic = "earnings")
variable_values("CENSUS", "HEAP")
get_values("sa2")
```

The package needs base R, `utils` and (for one value-domain reader)
`jsonlite`. It has no Rust toolchain, no `arrow` and no `dplyr`, and it
generates nothing. Someone planning a project can ask whether PLIDA carries
the column they need without building a compiler first.

To generate synthetic PLIDA and BLADE microdata, install
[fplida](https://github.com/wfmackey/fplida), which imports this package and
re-exports every function above.

The package contains public metadata only. It contains no confidential PLIDA
or BLADE data.

## How the split works

The registry — `inst/variable-info.csv.gz`, `inst/dataset-info.csv`, the code
frames under `inst/extdata/`, and the internal documentation — lives here, and
`fplida` imports it. Five questions had to be settled to get there.

**Does `fplida` depend on `fplida.info`, or do both carry the registry?**
`fplida` depends on `fplida.info`. One copy, one build, and no way for the two
to drift. It costs a release coupling: a compiled package now depends on a
data one. Carrying the registry twice would cost 76 MB of duplication and the
certainty that the copies would eventually disagree, which is worse.

**Where does the registry build live?** `fplida`'s `data-raw/` still owns it.
The research findings and `update_variable_info.R` are the source of truth and
belong in one place, and that place is where the variable-description work
happens. The build writes its artefacts into `fplida.info/inst/`, and this
package ships them.

**What happens to the pkgdown site and the dataset articles?** They stay on
`fplida`, which documents both halves. The site's reference index covers the
generation API and the registry readers together, because a reader arriving
from a search for a variable name should not have to work out which of two
packages they are in.

**How large is acceptable on CRAN?** CRAN is not the target. The registry is
18 MB compressed before the code frames and 76 MB with them, against CRAN's
5 MB limit, and no amount of trimming closes that. The package is installed
from GitHub. Nothing here needs the CRAN release cadence.

**Does the skill file follow the documentation?** `fplida-skill.md` stays with
`fplida`. It describes generating data as well as looking things up, so it
belongs with the package that can do both.

## One caveat about building from source

`fplida`'s Rust crate embeds several code frames at compile time with
`include_str!`, reaching into `fplida.info/inst/extdata/codeframes/`. Building
`fplida` from source therefore needs this package's source tree beside it,
which the repository provides. Installing either package from a built tarball
is unaffected.

# TODO

Work that is understood but not yet done. Each entry says why it matters, not
just what to change.

## Finish the variable descriptions: wave 4

6,505 of the 7,578 administrative dataset-and-variable pairs carry a written
description, covering 45,637 of 57,021 occurrences. 38,368 quote a published
source and cite it; 7,269 are written from several. Waves 1, 2 and 3 are done
and pass the audit.

[data-raw/value-research/README.md](data-raw/value-research/README.md) is the
guide — how to research a variable, write it up, decide between the `official`
and `AI` tags, check the generator still agrees, and rebuild. Run
[data-raw/audit_variable_descriptions.R](data-raw/audit_variable_descriptions.R)
before calling a dataset done; it exits non-zero on failure.

### What is left (1,073 pairs)

| Dataset | Pairs | Authority |
|---|---|---|
| NDIS | 195 | NDIS price guide, NDIA data and insights |
| CORE | 178 | PLIDA Core module documentation, ABS Life Course Dataset methodology |
| A&T | 116 | DEWR Australian Apprenticeships, NCVER apprentice and trainee collection |
| TVA | 91 | NCVER AVETMISS data element definitions |
| DEATHS | 77 | ABS Causes of Death methodology, WHO ICD-10 |
| AIR | 58 | Australian Immunisation Register, National Immunisation Program schedule |
| SAE | 54 | ATO superannuation reporting guidance, the MCS specification |
| BIRTHS | 50 | ABS Births methodology |
| HE | 49 | Department of Education higher education statistics, HEIMS dictionary |
| JK | 48 | ATO JobKeeper guidance |
| APSED | 35 | APS Employment Database |
| JM | 30 | DEWR employment services |
| ERS | 29 | ATO early release of superannuation guidance |
| MCD | 29 | Services Australia Medicare enrolment |
| SDB | 29 | Home Affairs settlement database |
| COMBINED | 4 | ABS PLIDA combined demographics |
| TRAVELLERS | 1 | ABS Overseas Migration methodology |

The chunk inputs are already built, split into 641 variables with no finding
at all and 431 that have codes but no prose. The second kind needs a
description-only patch merged into the entry that already exists, because a
second finding on the same key is a duplicate the build rejects.

### What running it costs

Roughly 20 research agents and 20 auditors, and about 5 million subagent
tokens judging by wave 3. Run one wave at a time: launching wave 3 and wave 4
together exhausted the session budget and killed all 40 agents two minutes in.

### Then clear the last of the curation CSV

`data-raw/variable-info-description-curation.csv` is down from 57 rows to 34.
Each remaining row sets a one-line description and nothing else, so those
variables show a sentence with no value domain and the generic definition
printed twice. They belong in findings files like everything else.

### Track it

```r
info <- read.csv(gzfile("inst/variable-info.csv.gz"), stringsAsFactors = FALSE)
admin <- info[info$collection_type == "administrative", ]
done <- admin$description_provenance %in% c("ai", "official")
length(unique(paste(admin$dataset, toupper(admin$variable))[done]))
```

## Split out an information-only package, fplida.info

Everything the registry knows about PLIDA and BLADE — what the datasets are,
what every variable means, what its values can be and where that was published
— is useful on its own, to a far wider group than the people generating
synthetic microdata. Someone planning a project wants to know whether PLIDA
carries the column they need. They do not want to install a Rust toolchain to
find out.

Today the whole thing is one package. `fplida` imports arrow and dplyr, needs
Rust 1.81 or later, and the first install compiles a crate for several minutes.
None of that is required to answer a question about a variable.

The documentation half is already separable. It is the registry files —
`inst/variable-info.csv.gz`, `inst/dataset-info.csv`, the code frames under
`inst/extdata/codeframes/` and the internal docs — read by `variable_info()`,
`dataset_info()` and `variable_values()`, which between them need nothing
beyond base R, `utils` and `jsonlite`.

Open questions, none of them settled:

- Does `fplida` depend on `fplida.info`, or do both carry the registry? A
  dependency keeps one copy and one build, but couples the release of a
  compiled package to a data one.
- Where does the registry build live? `data-raw/update_variable_info.R` and the
  research findings are the source of truth and should stay in one place, which
  probably means the data package owns them and the generator consumes the
  built artefact.
- What happens to the pkgdown site and the dataset articles? They are built
  from the registry, so they belong with it, but the site currently documents
  the generation API as well.
- How large is acceptable on CRAN, if CRAN is even the target? The registry is
  18 MB compressed before the code frames.
- Does the skill file `fplida-skill.md` follow the documentation or stay with
  the package that generates data?

## Hand over the code lists that are too large to print

3,539 occurrences across 99 variables carry a value definition reading "The
source publishes this value domain of 5,911 values. It is too large to list
here; see the value source." That is honest and it is a dead end. The package
holds many of those tables already — `inst/extdata/codeframes/` ships the SA2,
LGA, mesh block, ANZSIC, ANZSCO, country and Census code frames — so a reader
is being sent to an external website for something sitting in the library they
have loaded.

Two parts.

First, an accessor. `get_mbs_item_numbers()` is the clearer name and reads
better at the console; `get_values("mbs-items")` scales better across the 129
oversized domains covering 2.5 million values, and is what the second part
needs to print. Do the generic one and add named wrappers for the tables people
reach for most. It should return a tibble, and it must fail loudly rather than
silently empty when a table is not shipped, because not every oversized domain
has one — MBS item numbers are the case that prompted this and they are not in
the package today.

Second, print the call. The value definition should end with the call that
fetches the list, so the page answers the question rather than deferring it:
"The source publishes this value domain of 5,911 values. Get it with
`get_values(\"mbs-items\")`." The registry already knows which domains are
oversized — `enumerated` is `FALSE` and `full_list_size` is recorded in
`inst/internal-docs/resolved-value-domains.csv` — so the text can be generated
rather than written per variable. Only emit the call where the table actually
ships.

## Give the base spine a dwelling

The spine has a `household_id`, but it does not place a household anywhere:
households routinely span several SA2s, and in a 1,529-household sample 754 of
them spanned more than one state. Nothing in the spine says "these people live
at this address".

That gap is why the address register identifier is keyed on the person. `ARID`
and `ARID_HASH_TRUNC` stand for an address in the real data, so co-residents
share one value, and the AIFS and ABS household work depends on exactly that.
The synthetic data cannot reproduce it, so a researcher testing a cohabitation
pipeline against fplida gets one person per address and no signal.

The change: assign each household a dwelling on the spine, with the household's
geography derived from the dwelling rather than drawn per person. Every
residential identifier then falls out of it — `ARID` in Core Locations, the
`ARID_HASH_TRUNC` columns in the ATO, Centrelink, Medicare, births, migrant and
apprenticeship products, and the mesh block, SA1, SA2 and SA4 codes that sit
beside them.

Add noise rather than a clean mapping. Real address histories lag: a person who
has moved keeps their old address on a file they have not touched, agencies
disagree with one another about where somebody lives, and a share of records
carry no identifier at all — ABS found 4.3% to 11.2% of children with an address
history between 2006 and 2021 were missing an `ARID`. A dwelling model that
makes every product agree perfectly would be as misleading as the current one,
in the opposite direction.

See `.dil_address_key()` in [R/complete_dil_structures.R](R/complete_dil_structures.R)
for the key space this would feed, and the `## The address register identifier`
entry in [NEWS.md](NEWS.md) for what is already in place.

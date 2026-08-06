# Documenting a variable properly

How to take a variable page from saying nothing to saying something, using the
address register identifier as the worked example. Every step here was done for
`ARID`; the mechanism is general.

## What "saying nothing" looks like

`ARID_HASH_TRUNC` on the ATO payment summary used to render:

| Field | Text |
|---|---|
| Official description | Hashed arid truncated |
| Value domain | not specified |
| Value definition | The source metadata does not publish a finite value list. |
| Limitation | The source metadata does not publish a finite value list. |

Three things are wrong. The description restates the variable name. The value
definition and the limitation are the same generic sentence printed twice. And
nothing on the page says what an ARID *is*, which is the only thing a reader
came for.

The generic sentences are not a bug. They are the honest fallback when the only
question the registry could answer was "is there a code list?". The fix is to
let research answer a second question — "what is this variable?" — and to write
the answer down where the fallback used to be.

## Find the variables worth doing

A page says nothing when the value definition repeats the limitation *and* the
description is just the custodian's wording. Over administrative occurrences
that is 1,373 variables covering 5,072 occurrences:

```r
info <- read.csv(gzfile("inst/variable-info.csv.gz"), stringsAsFactors = FALSE)
admin <- info[info$collection_type == "administrative", ]
empty <- (admin$value_definition == admin$limitation |
            grepl("does not publish a finite value list",
                  admin$value_definition, fixed = TRUE)) &
  admin$variable_description == admin$official_description
sort(table(paste(admin$dataset[empty], toupper(admin$variable[empty]))),
     decreasing = TRUE)[1:30]
```

Two things make a variable worth the effort, and they are not the same thing.

Occurrence count says how many pages improve. By that measure the work sits in
CGT (1,371 occurrences over 101 variables), TRAVELLERS (500 over 328), PIT_ITR
(460 over 41), STP (434 over 4) and AEDC (242 over 48).

Reach across datasets says how much a reader learns. A column that appears in
one dataset teaches one thing; a column in six teaches how the asset fits
together. `ARID` was worth doing because it was the same identifier in fourteen
datasets meaning something different in each. Twenty-eight such columns remain:

```r
spread <- tapply(admin$dataset[empty], toupper(admin$variable[empty]),
                 function(x) length(unique(x)))
sort(spread[spread > 1], decreasing = TRUE)
```

Counts below are within the empty set, so `ADR_TYP` shows three datasets rather
than the five it appears in: the other two already say something.

| Column | Datasets | Occurrences | What it is |
|---|---|---|---|
| `ABN_HASH_TRUNC` | 6 | 60 | The employer or business. A&T, BUSOWN, DEX, JK, PIT_ITR and PIT_PS, and the obvious next job |
| `EXTRACT_REF` | 5 | 36 | Which extract the row came from — vintage, not content |
| `ADR_TYP` | 3 | 13 | Residential against postal, which decides what an address means |
| `VISA_SUBCLASS` | 2 | 33 | Home Affairs subclasses, with documented dump codes (000, 001, 444) and no list attached |
| `INB_INCM_TYP` | 2 | 20 | Individual non-business income type |

Do the wide, shallow columns first. They are cheap per page improved, and each
one teaches something about the asset rather than about one table.

## The six steps

### 1. Read the variable in context

The most useful evidence is usually already in the registry: the columns that
sit beside the one you are documenting. `ARID` sits with `START_DATE`,
`END_DATE`, `MB_ASGS_2021` and `SA1_ASGS_2021` in Core Locations, and with
`OUTLETID`, `OUTLETPOSTCODE` and `OUTLETSTATE` in DEX. The first is a person's
address history; the second is a service outlet. Nothing external told me that.

```r
info <- read.csv(gzfile("inst/variable-info.csv.gz"), stringsAsFactors = FALSE)
neighbours <- function(dataset, table) {
  r <- info[info$dataset == dataset & info$table == table, ]
  r[, c("variable", "official_description")]
}
neighbours("DEX", "special_outlet")
```

Also list every table the variable appears in, because the answer often differs
between them. `ARID` in `ar_addr` is the Address Register reference table; in
`plida_core_loc_v8` it is a person's location episode.

### 2. Find the authority, and read it

The custodian's own documentation first, then the classification's publisher.
For the assets in this registry that has meant:

- ABS methodology pages and information guides for anything ABS-defined
- AIHW METEOR for MBS and PBS — the PLIDA descriptions are lifted verbatim from
  METEOR value domains, so a search on the description text usually lands on
  the exact data element
- The DSS Aristotle metadata registry for DOMINO and the Data Exchange
- ATO guidance and the relevant schedule or return form for tax variables
- Geoscape and the ABS Address Register for anything address-shaped

A source is only usable if it is fetchable. A claim citing a login-walled PDF
or a document you were told about but did not read is a `guessed`, not a
`sourced`.

### 3. Write the finding

One JSON object per dataset-and-variable pair, in a file under
`data-raw/value-research/`. Group by topic, not by dataset — `findings3-arid.json`
holds all fourteen ARID entries.

```json
{
  "dataset": "PIT_PS",
  "variable": "ARID_HASH_TRUNC",
  "status": "sourced",
  "value_domain": "ABS Address Register identifier (hashed and truncated)",
  "values": [],
  "values_are_sample": false,
  "value_source": "ABS Address Register Information Guide",
  "value_source_url": "https://www.abs.gov.au/statistics/research/abs-address-register-information-guide",
  "variable_description": "The address the ATO held for the payee when the payment summary was reported. The MB, LGA, SA1, SA2, SA3, SA4 and STE columns on the same row are that one address geocoded ...",
  "value_definition": "A hash of the address register identifier, truncated. The ABS Address Register is ...",
  "limitation": "The identifier stands for an address, not for a person or a household ...",
  "confidence": "high",
  "rationale": "Why this source answers this variable.",
  "evidence_quote": "the sentence in the source that carries the claim",
  "supersedes_rationale": "What the entry said before, and why that was not enough."
}
```

Required: `dataset`, `variable`, `status`. A `sourced` status also requires
`value_source` and an `http(s)` `value_source_url`. A finding with no `values`
requires a `value_domain`.

Optional, and the point of this guide: `variable_description`,
`value_definition`, `limitation`, `description_source`,
`description_source_url`. Supply any of them and that text replaces the generic
sentence; leave them out and the fallback stands. `description_source` defaults
to `value_source`, so most findings need only the description itself.

### 4. Write it so the panel does not repeat itself

The panel renders, in order: official description, detailed description, value
domain, value definition, reference period, valid values, value source,
limitation, appears in. Each field has a job, and the fields fail when two of
them do the same job.

`variable_description` — what this column holds *in this dataset*. Dataset
specific, always. It is the only field that should differ between the fourteen
ARID entries, and the reason RPS says "the rental property's address, not the
landlord's" while DEX says "the service outlet's address, not a client's". Put
the answer in the first sentence: the table cell shows that sentence alone.

`value_definition` — what the values look like and where the classification
comes from. Generic across the family; identical wording in all fourteen ARID
entries is correct, because it is the same fact.

`limitation` — what the column cannot tell you. Not a restatement of the
definition. Good limitations name a confusion a reader would otherwise fall
into: an address is not a household, a Medicare address belongs to the
enrolment so a child carries a parent's, an overseas address has no identifier
at all.

Where a figure is available, use it: "ABS found that 4.3% to 11.2% of children
with an address history between 2006 and 2021 were missing an ARID" is worth
more than "coverage is incomplete".

### 5. Ask whether generation now contradicts the registry

This is the step that is easy to skip and expensive to skip. Research that
reaches only the documentation leaves the package asserting something its own
output denies.

Three questions:

- Does the generator emit values outside the domain now documented? For a code
  list this is caught by the test in `tests/testthat/test-value-research.R`,
  which generates data and compares it against every resolution.
- Does the documented meaning imply a constraint the generator ignores? ARID
  means an address, so it has to agree wherever the same address appears. It
  did not: Core Locations minted a row counter, the Core address tables minted
  an `AR`-prefixed identifier, and every other product minted a per-dataset
  hash. One person's address could not be joined to itself.
- Is there a Rust generator with its own hardcoded copy? Codes reach Rust
  through `inst/extdata/codeframes/researched-value-codes.tsv`, which
  `update_resolved_value_domains.R` writes and `src/rust/src/codeframes.rs`
  embeds. Semantics do not: `core_gen.rs` had to be edited by hand to match
  `.address_key_hex()`, and a comment on each side says to keep them in step.

Where a constraint cannot be honoured, say so rather than pretending. The spine
has no dwelling, so the synthetic ARID is per person and co-residence is not
reproduced. That is recorded in the code, in `NEWS.md` and in `TODO.md`, which
is a better outcome than a key that quietly means the wrong thing.

### 6. Rebuild, test, look

In this order. The second step takes about three minutes.

```bash
Rscript data-raw/update_resolved_value_domains.R
```

```bash
Rscript data-raw/update_variable_info.R
```

```bash
Rscript data-raw/build_variable_articles.R
```

```bash
Rscript -e 'testthat::test_local()'
```

Then read the page rather than trusting the diff:

```bash
Rscript -e 'pkgdown::build_site(preview = FALSE, install = FALSE, new_process = FALSE)'
```

`preview_start` serves `docs/` on port 4321 from `.claude/launch.json`.

Add a test for anything a future edit could silently undo.
`tests/testthat/test-arid.R` pins that every ARID occurrence is sourced, that
the definition never equals the limitation, that the descriptions are not one
sentence copied fourteen times, and that a person's address key agrees across
products.

## What will stop you

The build refuses these, by design:

| Refusal | Meaning |
|---|---|
| Duplicate dataset/variable in the research findings | An earlier findings file already covers this pair. Delete the old entry; the new one supersedes it. |
| A sourced resolution carries no usable source URL | `sourced` without a fetchable citation launders an inference into a fact. Downgrade to `guessed` or find the page. |
| A curated description carries no source URL | A description that overrides the custodian's wording has to say where it came from. |
| A resolved variable carries neither values nor a value domain | Name the domain even when it is open. "This is a timestamp" beats silence. |
| A value source cites a path inside the package | `value_source` names the publisher, not where this repo keeps a copy. |
| An unsupported resolution carries values or a source | `unsupported` is the honest outcome and should look like one. |

Two silent behaviours worth knowing. A `guessed` resolution carrying no values
will not demote a variable that is already `sourced` — a pass looking for value
domains that concludes "this is an opaque identifier" has learned nothing about
provenance. And resolutions are applied after the exception determinations but
before the name-based guesses, so a researched domain always beats one inferred
from the variable name.

## The tags on the page

Each field carries `metadata` or `AI`, derived from the recorded source rather
than asserted: a source naming the data item list and nothing else is the
custodian's own wording, and one that joins the data item list to something
else is a description somebody composed. See `from_dil()` in
`data-raw/build_variable_articles.R`.

This means a finding's `value_source` and `description_source` decide what the
page claims about its own provenance. Naming the real publisher is what makes
the tag right.

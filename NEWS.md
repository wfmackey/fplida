# fplida (development version)

## Generated dollars now have a year

Every dollar figure in PLIDA and BLADE is nominal. A wage in the 2016-17 Single
Touch Payroll file and a wage in the 2023-24 file are not the same unit, because
prices rose about 17 per cent between them and average wages rose about 25 per
cent. Until now fplida drew a person's income once at a 2021 anchor and reused
that number in every reference year, and drew a business's turnover once and
reused it in every BLADE table. The generated data had no inflation and no
nominal income growth at all.

That made a whole class of method test impossible to fail. Deflating a synthetic
PLIDA was a no-op. Bracket creep did not exist. A regression of a nominal
outcome on a real one could not go wrong, because there was nothing there to get
wrong. Anyone using the package to check a real-terms conversion before running
it in the DataLab got a clean answer from data that could not have told them
otherwise.

### The headline series come from published sources

`data-raw/update_nominal_indices.R` builds five series and writes them to
`inst/extdata/nominal-indices.csv` and to `src/rust/src/nominal_series.rs` as
compiled-in constants:

- **wage** — ATO Taxation statistics, Individuals Table 1A: total salary or
  wages divided by the number of individuals reporting it. The closest published
  analogue of what the synthetic data actually contains, so PLIDA's own wage
  aggregates reproduce it. It runs about half a point a year above the Wage
  Price Index because it carries compositional change and job mobility, which
  the synthetic panel also has.
- **price** — ABS Consumer Price Index, All groups, via the ABS SDMX API.
- **business** — ATO Taxation statistics, Company Table 1A: total income divided
  by the number of companies. Unlike the other four it falls in some years, as
  company income does.
- **transfer** — Male Total Average Weekly Earnings, the Age Pension benchmark.
- **health** — the Medicare Benefits Schedule indexation factor, from MBS Online
  where the Department publishes one and from the schedule fee for item 23
  where it does not.

Each is given on both a financial-year and a calendar-year basis, because PLIDA
mixes the two, and both are normalised to the same base so a person's tax return
and their Census record line up. Years the published data does not reach are
flagged `projected` rather than passed off as sourced: ATO taxation statistics
lag by about two years, so the most recent years are always this package's own
extrapolation. Every figure in the two administered series carries its source
URL in `data-raw/nominal-sources/administered-indexation.csv`.

`nominal_index()` and `nominal_indices()` expose the series, so output can be
deflated with the exact index that inflated it.

### Growth is a headline plus a unit's own departure from it

A unit's amount in year *y* is its anchor amount times the headline index times
`exp` of its own deviation, and that deviation has three parts: a growth profile
drawn once per unit and never changed, a permanent random walk, and a transitory
shock in the year itself. Without the profile term the spread of earnings would
be identical in every year; without the permanent term every deviation would
wash out by the next year and the long-run distribution would be far too tight.

The three are mean-corrected by half their variance, so population aggregates
reproduce the published series rather than drifting above them by the Jensen
gap. Legislated amounts — benefit rates, MBS schedule fees, PBS co-payments,
HELP fee bands — take the headline with no deviation at all, because everyone
who receives them receives the same number.

Every draw is a hash of `(unit, seed, year, salt)` rather than a step of a
shared stream, so nothing depends on row order or on how many workers the build
was sliced across. Two slice workers holding disjoint sets of people reconstruct
identical paths, which is what keeps Single Touch Payroll, PAYG payment
summaries and income tax returns agreeing on the same person's wage.

### Two invented constants are gone

The employment panel's earnings walk used a flat 5.8 per cent a year in both
directions from the 2021 anchor. That figure was invented and about two points
above what Australian wages did. It is now the published growth across each
step, with the half-variance correction changing sign with the direction so the
mean lands on the index whether the walk runs forwards or backwards. The spread
around the drift is unchanged, so what separates one career from another is the
same as before.

DOMINO deflated every benefit at a flat 2.5 per cent a year back from 2024.
Pensions — Age, Disability Support, Carer and Parenting Payment Single — now
follow the MTAWE benchmark, and allowances and family payments follow the CPI.
That is the difference that has pensions growing about a third faster than
allowances since 2006, and it was previously absent.

### What moved, product by product

Wages flow from one place. The employment panel is the earnings backbone, so
Single Touch Payroll, PAYG payment summaries and income tax returns inherit its
growth without needing their own indexation. Beyond that: DOMINO benefit
components and reported employment income; rental property schedules, where
rent, rates, insurance, repairs and agent fees follow the CPI; superannuation
balances and contributions; NDIS plan budgets, including the plan cap, which
would otherwise have collected an ever-growing pile of plans on one fixed
number; higher education charges and HELP debt; Data Exchange reported income;
Medicare schedule fees and benefits; PBS dispensed prices; and every BLADE
financial item.

BLADE is the largest single change. Its 62 tables are one cross-section each,
carrying reference periods from 2015-16 to 2025-26, and every one of them used
to report the same business the same turnover. Turnover now moves on the
business series and the wage bill on the wage series, so the labour share of
turnover differs between one table's period and another's. The wage bill takes
the headline only, with no per-business shock, so it stays the sum of its
employees' wages.

Two things are now looked up rather than indexed, because indexing them would
be wrong. The PBS general co-payment was CUT from \$42.50 to \$30.00 on 1
January 2023 and again to \$25.00 on 1 January 2026, and no price index can
produce a cut, so the legislated rates from 2000 to 2026 are carried in full.
And DOMINO's variation around a benefit rate is now one-sided: a rate is a
legislated maximum, so what someone is actually paid can fall below it on the
income test but can never sit above it.

### Consequences worth knowing about

PAYG withholding is still computed on fixed tax brackets. Now that wages grow
and the brackets do not, the generated data has bracket creep in it, which is
the correct behaviour and is new.

The Medicare series is flat from 2014-15 to 2017-18. The schedule fee for a
standard GP consultation was \$37.05 for four straight years while prices rose
about five per cent, so a Medicare benefit fell in real terms. Indexing Medicare
amounts to the CPI would have hidden that, which is why `health` is a separate
series from `price`.

The spine's `baseline_income` is unchanged and remains a 2021 amount. Generators
that gate on it — the income thresholds in DOMINO, for instance — are therefore
unaffected, and the change is confined to emitted dollar values.

Three limitations are worth stating rather than burying. PBS dispensed prices
follow the health series and so rise, whereas real prices for high-volume
generics fall in nominal terms as price disclosure bites, so the sign is wrong
on that part of PBS expenditure. A superannuation balance is indexed on wages,
which credits fund earnings with tracking wage growth and no more, and so
understates a long-held balance; holding it flat in nominal terms, which is what
happened before, was wrong by considerably more and in the other direction. And
the Medicare series before 2006-07 is an assumption of 2 per cent a year, the
average of the years that are sourced, because the Department publishes no
indexation factor and no schedule fee that far back.
## Variables can now be explained, not just classified

Every field on a variable page answered the question "is there a code list?".
For a variable whose answer is no, that produced a page saying nothing: the
description restated the variable name, and the value definition and the
limitation were the same generic sentence printed twice. `ARID_HASH_TRUNC` on
the ATO payment summary read "Hashed arid truncated", followed by "The source
metadata does not publish a finite value list." twice, which does not tell a
reader what an ARID is.

A research finding may now carry written prose — a description, a value
definition and a limitation — alongside the codes it resolves. Where it does,
that text replaces the generic sentences; where it does not, the fallbacks
stand. A written description must cite a fetchable source, the same bar a
`sourced` code list has to clear.

The variable table now shows the first sentence and the panel below it the
whole description, and the panel drops a row that would only repeat what is
already on screen. Each label sits above its content rather than beside it, and
"Appears in" is one point per occurrence instead of a run of slashes.

Every field also says where its text came from, through two new registry
columns, `description_provenance` and `value_provenance`. Each takes
`metadata` for the custodian's own wording out of the data item list,
`official` for a published source quoted and cited, or `ai` for prose written
from several sources. The page shows the label beside the field and the full
sentence on hover.

`official` is not derived from a citation, because naming a source does not
make a paragraph written around it a quote. Research declares it, and the build
refuses the claim without a fetchable URL. The one exception is a code list
carried across verbatim from a cited publisher: those codes and labels are the
source's own text, which covers 6,431 administrative occurrences.

## The address register identifier, explained and joinable

`ARID` and `ARID_HASH_TRUNC` are documented across all fourteen datasets that
carry them, each saying whose address it is: the rental property rather than
the landlord in RPS, the service outlet rather than a client in DEX, the
provider's registered office in NDIS, the business location in BLADE, the
person's residence in the ATO, Centrelink, Medicare and Core Locations
products. All 82 occurrences move to `sourced`, citing the ABS Address
Register Information Guide and the ABS Life Course Dataset methodology.

Generation follows. An ARID stands for an address, so the same address has to
carry the same value wherever it appears; it was previously minted three
different ways — a row counter in Core Locations, an `AR`-prefixed identifier
in the Core address tables, and an `H`-prefixed hash keyed on the dataset in
every other product — so a person's address could not be joined to itself.
There is now one address key, computed identically in R and in Rust, keyed on
the person and the seed alone. Addresses belonging to an organisation or a
property are drawn from a disjoint half of the key space, so a join between a
provider table and a location table cannot match by accident.

The synthetic spine has no dwelling of its own, so one person gets one address
and co-residence is not reproduced, even though the real identifier carries it.

## Value support status: a breaking rename, and a new `guessed` category

`value_support_status` now takes four values rather than three. This is a
breaking change: code filtering on `"supported"` must use `"sourced"`.

- `supported` is renamed `sourced`. The old name suggested the registry had the
  variable's values, which was not what it recorded. Of the 55,849 occurrences
  it covered, only 3,571 carried an actual value list; the remaining 93.6 per
  cent had `valid_values` of `[]` and a `value_domain` of "not specified". A
  green "Supported" badge sat directly above "Value domain: not specified" on
  every one of those variable pages. `sourced` now means what it says: the
  codes come from a published classification, named in `value_source`.
- `guessed` is new. It marks a value domain inferred from the variable's name
  and description rather than from a published list — a variable whose name
  ends `_STATE` is given the Australian state and territory codes on that basis
  alone. The generated column holds plausible values of the right shape, which
  is more useful than an empty column, and the status says plainly that the
  source does not confirm them.
- `unsupported` is unchanged in meaning but is now a genuine residual: the
  value domain is neither published nor inferable.
- `not_applicable` is unchanged. Survey variables stay outside the value scope.

The registry now prefers a labelled guess to an empty column. Read `sourced` as
a mapping you can rely on and `guessed` as a placeholder of the right shape.

Measured over the shipped registry: 23,758 occurrences are `sourced`, 35,000
`guessed`, 1,160 `unsupported` and 12,738 `not_applicable`. Guessing raised the
number of occurrences carrying an actual value list from 3,574 to 8,402. No
`sourced` domain and no `unsupported` determination was overwritten: guessing
runs last and fills only genuine blanks.

Survey occurrences may now be `guessed` where a rule matches. Survey
instruments turn out to be the most regular thing to infer from, because a
whole module shares one answer scale.

## Generation draws from the registry

The canonical-structure generator inferred a column's values from its name,
independently of the variable registry. A column the registry documented
precisely — the nine Australian state codes, say — was still filled by a
generic categorical heuristic.

Generation now consults the registry. Where a dataset-and-variable pair carries
a value list, whether `sourced` or `guessed`, the column is drawn from that
list; the name heuristics remain the fallback.

The registry is consulted last, not first, and that ordering is load-bearing.
The rules above it encode semantics the registry cannot know — that the sex of
a female parent is 2, that a change flag is 0/1 — and a documented domain must
never preempt them.

## Researched value domains for the variables the first review could not resolve

`unsupported` covered 1,160 occurrences across 278 variables. Every one had
been reviewed, and the review recorded its reasoning: "no exact public
codeframe", "no defensible crosswalk", "would conceal an unresolved mapping".
That reasoning answered the only question available at the time, when a value
domain was either an exact published codeframe or nothing.

`guessed` changes the question. A field whose exact mapping is unpublished can
still have a defensible shape. Re-researching all 278 against that lower bar
resolved all but three, and turned up published codeframes the first pass had
missed:

- The Department of Social Services runs a public metadata registry that binds
  each DOMINO column to a data element with its permissible values. 59 of the
  60 DOMINO variables resolve there. It also corrected values already being
  generated: `IMPRMT_CODE` is an impairment rating from 0 to 95 in steps of 5,
  not the 1-3 code being emitted; `CODE_VALUE` holds 23 DSS medical condition
  groups, not ICD-10; `END_RSN_CODE` has 675 published values against the 12 in
  use.
- The BLADE Data Item List points its trade variables at external appendices,
  which are the ABS international merchandise trade workbook. All 11 BLADE
  variables resolve, to AHECC, the Customs Tariff, SITC Rev.4, BEC Rev.4, and
  the IP Australia data dictionary.
- The AEDC publishes a data dictionary, a legacy dictionary and a reference
  tables workbook. 96 AEDC variables resolve, including the domain and
  sub-domain scores, the vulnerability flags and the publishability reason
  codeframes.
- The ABS geo service carries the 2011 and 2016 ASGS vintages the earlier
  review recorded as unavailable, which resolves the ACLD geography fields.
  `IEO_UR_21` needed no research at all: the codeframe was already attached to
  the same variable in a sibling product and had been missed on one occurrence.

Three variables remain `unsupported`, and the status now means what it says —
research looked and found nothing defensible. They are two AEDC
school-administration units with no published national list, and an ATO code
box that does not exist on the only return year its field covers.

Every `sourced` claim was independently checked against its cited source before
being accepted. 29 were downgraded to `guessed` because the source established
the shape without confirming the mapping — the AEDC language fields, whose
Indigenous and non-Indigenous split is not printed anywhere; `INCM_TYP_CD`,
identified against a reporting standard that postdates the field by four years;
the NCVER provider types, whose categories are published while the codes behind
them are not.

Measured over the shipped registry: 24,285 occurrences are `sourced`, 35,626
`guessed`, 7 `unsupported` and 12,738 `not_applicable`. Occurrences carrying an
actual value list rose from 8,402 to 9,101, of which 3,955 are sourced and
5,146 guessed.

A partial list is not carried at all. Where research could only sample a
classification — eight New South Wales electorates out of several hundred
nationally — the registry records the domain, its source and its published
size, and leaves `valid_values` empty. Generating from the sample would have
put every child in New South Wales.

## A second research round: MBS, PBS and the other high-volume collections

The first round covered variables that generated nothing. This one covered
variables that already produce data but carry no documented value domain —
23,485 administrative occurrences, concentrated in MBS, PBS, NDIS, CORE,
DEATHS, PIT_ITR and TVA.

The structural find is that the PLIDA item descriptions for MBS and PBS are
lifted verbatim from AIHW METEOR, and METEOR holds Department of Health data
set specifications mapping data elements to the exact PLIDA variable names.
That makes METEOR, not MBS Online, the authority for most of those two
datasets. It resolved the MBS category hierarchy, Broad Type of Service,
billing type and in-hospital indicator, and nearly every PBS field including
patient category, drug type, pharmacy approval type and prescriber type.

Applied: 683 variables across 16,824 occurrences. `sourced` rises from 23,758
to 25,930 and occurrences carrying a value list from 8,402 to 11,912.

Every sourced claim was verified against its cited source. 173 were confirmed,
four downgraded and two refuted — both refutations being a real classification
attached to the wrong field, where the domain assigned duplicated a sibling
column that already carried it.

### Research adds; it does not subtract

A pass looking for value domains that concludes "this is an opaque identifier"
has learned nothing about provenance. Such a finding no longer demotes a
variable the evidence process established as `sourced`, and no longer
overwrites its domain text — a `sourced` row carrying a domain marked
"(inferred)" contradicts itself.

### A researched list is not carried when it contradicts the data

Where a researched value list disagrees with what the generator already emits,
the list is withheld and only the domain, source and size are recorded. Some
findings described a column differently from the way it is filled: a range
written as a value, or a code set documented for a `..._NM` column that holds
names. Asserting those would have made the registry contradict 33 working
columns. `registry-generator-conflicts.csv` records them, and is rebuilt by
generating data and comparing, so it can be refreshed when either side moves.

`BTOS` was the exception worth fixing rather than withholding: the generator
wrote invented single letters, and the published four-digit classes map onto
exactly the same semantics, so the column keeps its meaning and only its
vocabulary changes.

## The researched codes reach the Rust generators

Documenting a domain does not change the data. The Rust generators write the
per-dataset products and cannot read the registry, so each kept its own
hardcoded arrays — and those had drifted from the source.

The research findings now build `inst/extdata/codeframes/researched-value-codes.tsv`,
which is embedded into the binary by `include_str!` alongside the state, SACC
and ANZSIC code frames it already ships. Both the registry and the generators
derive from the same findings, so they cannot drift apart again.

Measured on generated DOMINO and SAE data, 15 columns were emitting codes that
are not in their published domain. All 15 now agree with it:

- `FREQ_CODE` wrote `FTN` for a fortnightly income frequency the custodian
  codes as `2WE`.
- `IMPRMT_CODE` wrote a 1-3 severity band. Impairment is published as a rating
  from 0 to 95 in steps of 5, and `IMPRMT_RATE` reports the same rating — it
  was drawn independently, so the two disagreed on every row. They now agree.
- `ACTV_PRTCPN_CODE` wrote A/E/N/P against a published Yes/No/Not-required.
- `ADDR_TYPE_CODE`, `CHNL`, `RFRL_RSN_CODE`, `HSE_ACCOM_CODE`, `HSE_HO_CODE`,
  `HSE_RENT_TYPE`, `LVL_ATTAINED` and `STDNT_STS_CODE` all drew from invented
  sets where a published one exists.
- SAE `ACNT_PHS_CD` and `MBR_ACNT_STS_CD` wrote single letters. The ATO's
  member-attribute specification enumerates these in full, so `A` and `L` were
  not members of the domain. The internal shorthand is kept for the balance and
  contribution rules; only what reaches the column changed.
- SAE `AGE_RANGE` used `Under 25` to `70+` where the documented bands run
  `Under 18` to `75 and over`.

Two fields stopped inventing a code for absence. `MAN_CODE` names the ground
for a manifest grant — permanent blindness, category 4 HIV — and was being used
as a Y/N flag; there is no published code meaning "not a manifest grant", so
that case is now missing rather than `N`. `STDNT_STS_CODE` likewise wrote `NST`
for someone who is not a student.

A test generates data and asserts no column emits a value outside its
documented domain. Nothing was comparing the two before, which is how a
fortnightly frequency spelled `FTN` survived.

## Generation fixes found while wiring the registry in

- The registry was consulted only on one of the six paths that write typed
  missing. The broadest of the other five caught any name holding STATUS, TYPE,
  CODE, FLAG or IND, so a variable with a published code list still generated an
  empty column. 593 of the 3,222 documented pairs were emptied this way, 454 of
  them `sourced`. All six paths now consult the registry before writing typed
  missing.
- Documented area codes are drawn against the person's state rather than
  uniformly. ASGS codes carry their state in the leading digit, so a flat draw
  handed a Queenslander a Victorian mesh block, contradicting the guarantee
  that a person's geography agrees across products. Where a documented list
  cannot be anchored — because it is a partial list covering one state — the
  column stays typed missing rather than break the guarantee.
- A `..._NAME` column beside a `..._CODE` column receives the label, not the
  code. Both were being given the same `code: label` list, and the code was
  emitted into both.
- AEDC fields the first review could not resolve were short-circuited to typed
  missing before the registry was reached, so none of the 96 newly documented
  domains would have appeared in generated data. Those fields now defer to the
  registry, and still write typed missing where it documents nothing.
- A code appears once in its value list. A codeframe spanning several vintages
  carries one row per code per year and the place name drifts between them, so
  deduplicating the rendered `code: label` string kept all three spellings of
  LGA 11500 — "Campbelltown (C)", "Campbelltown (C) (NSW)" and "Campbelltown
  (NSW)". Generation strips the label and samples uniformly, so that area came
  up three times as often as a single-vintage one. 571 of the 1,197 entries in
  the DOMINO `LGA` list were duplicates; the codeframe now deduplicates on the
  code and keeps the most recent vintage's name.
- The variable articles are generated from the source registry rather than the
  installed package. The script exists to be run straight after the registry is
  rebuilt, which was exactly when it silently wrote articles from the previous
  registry.

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

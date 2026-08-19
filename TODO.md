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

## Let a business have more than one owner

Every business in the generated BUSOWN is owned by exactly one person, and no
business survives from one year to the next. On a ten-million-person extract all
6,853,837 business-years have one owner and every business identifier appears
exactly once in the whole file. The generator mints a fresh identifier per person
per year, so there is no shared ownership and no ownership spell.

The consequence is worse than a missing feature. Partnership structure cannot be
prototyped at all, and a rule that counts co-owners runs green and does nothing —
which is the failure mode that survives a local test and breaks on real data.
Anyone building a self-employment or business-ownership measure against fplida is
exactly the audience for this file, and it is the one thing it will not let them
check.

The cause is a fallback firing every time.
[owner_business_id()](src/rust/src/busown.rs) asks
`business_pool::employer_bn()` for a BLADE business number and, when the pool is
empty, falls back to `format!("ABN{:012X}", rng.gen())` — a fresh random value
per call. Every identifier in the extract carries that `ABN` prefix and none
intersects the `BN` space that BLADE, STP and the payment summaries use, so the
pool was empty for the whole run. Populating it fixes half the problem on its
own: `employer_bn()` is deterministic in the person and the seed, so the same
person would hold the same business across years, and ownership spells would
appear.

Shared ownership needs a real change, not a pool. Draw the business first and the
owners second: give a business a legal form, give a partnership two or more
owners drawn together, and let a sole trader have one. Households are the obvious
place to source co-owners, since spousal and family partnerships are the common
Australian form, and [the base spine has no dwelling](#give-the-base-spine-a-dwelling)
is the entry that would make that possible.

Note what this blocks downstream. BUSOWN's real tables are split by legal form —
`ato_sole_trader*` and `ato_partnership*`, one pair per financial year — and the
package writes a single flat table with no form at all, so the split that carries
the sole-trader/partner distinction in the real asset does not exist here either.
The two are one job: there is no point giving a partnership several owners while
nothing marks it as a partnership.

## Give the dwelling its noise

The spine now has a dwelling. A household is formed within a state, given one
of its members' SA2s, and given a `dwelling_id`; `ARID`, `ARID_HASH_TRUNC` and
the Core Locations mesh block, SA1, SA2 and SA4 are all keyed on it, so
co-residents share an address. What is not done is the other half of the entry:
the model is now perfectly clean, and a dwelling model where every product
agrees on every address is as misleading as the old one, in the opposite
direction.

Three kinds of noise, each with a published figure behind it.

Records with no address at all. The ABS could tie 91% of the 25.7 million
people on its 2021 administrative population snapshot to a dwelling; the
remaining 9%, 2.3 million people, could be coded to an area but not to an
address. For children specifically, 4.3% to 11.2% with an address history
between 2006 and 2021 were missing an `ARID`, and 0.8% to 1.6% a year did not
link to Core Locations at all. Sources: [ABS, Administrative data snapshot of
housing methodology, 30 June
2021](https://www.abs.gov.au/methodologies/administrative-data-snapshot-population-and-housing-experimental-housing-);
[ABS, Creating a child mobility indicator using the Life Course
Dataset](https://www.abs.gov.au/statistics/detailed-methodology-information/information-papers/creating-child-mobility-).

Agencies disagreeing with one another. This is the one with a hard number to
hit: on 2016 Census night, PLIDA disagreed with the Census for 22.48% of
records at SA1, 18.07% at SA2, 9.50% at SA4 and 2.18% at state level, and
allowing one to three extra months barely moves it (SA1 falls only to 21.31%).
Mismatch is not random — it is highest for children, young adults, recent
migrants, international students and First Nations Australians, and lowest for
homeowners and people with dependent children. Source: [Bernard, Wu, Wilson,
Argent, Zajac and Kimpton (2024), Demographic Research 51(22), Table 5 and
Table 6, pp. 700-701](https://www.demographic-research.org/volumes/vol51/22/51-22.pdf).

Addresses that lag the move. The ABS assumes three months between a person
moving and their Medicare address being updated, and says so: "This assumes
that on average the time between a person moving and registering a change of
address is three months, which has proven to be the best lag time for
estimating the population." The Productivity Commission puts it more bluntly:
"Reported addresses in some datasets may be significantly out of date."
Sources: [ABS, Regional internal migration estimates, provisional
methodology](https://www.abs.gov.au/methodologies/regional-internal-migration-estimates-provisional-methodology/mar-2021);
[Productivity Commission (2024), A-PLIDA-nalysis, p.
26](https://assets.pc.gov.au/research/completed/plida/plida.pdf).

A lag needs something to lag behind, and the spine has no move history: one
household, one dwelling, for the whole 2005-2024 window. The Census variables
`PUR1P` and `PUR5P` — place of usual residence one and five years ago — are
currently set to the person's current SA2, so everyone in the synthetic
population has lived at the same address for five years. The real figures are
15.0% moving in the year before the 2021 Census and 40.7% over five years, with
87.0% of movers staying inside their own state. That makes `PUR1P`/`PUR5P` both
the obvious first use of a move history and the obvious test of one. Source:
[ABS, Population movement in
Australia](https://www.abs.gov.au/articles/population-movement-australia).

Note what the noise breaks on purpose. `tests/testthat/test-arid.R` asserts one
ARID per person across products, which is exactly what a disagreement rate
contradicts. It has to become a co-residence guarantee plus a bounded
disagreement rate, rather than being deleted.

## Make every product read the spine's geography

Three generators draw a geography of their own instead of reading the spine's,
so the dwelling stops at their door.

`.stp_location_lookup_rows()` in [R/generate_stp.R](R/generate_stp.R) draws a
mesh block from anywhere in the person's state, and reseeds on the year and the
month, so a person's `SA2_ASGS_2021` in the payroll data changes every month
and never matches their spine SA2. `.dil_location_lookup_rows()` in
[R/generate_dil_lightweight.R](R/generate_dil_lightweight.R) does the same for
every lightweight DIL product. DOMINO's address product
([src/rust/src/domino.rs](src/rust/src/domino.rs)) invents mesh block, SA1 and
SA2 digits that are not real ASGS codes at all.

The first two predate the dwelling and are wrong on their own terms — a
person's address should not depend on which month's table you read it from.
The third writes codes that no code frame contains.

## Give a household its LGA, PHN and Indigenous Region

The state-anchored area draw exists in five copies, all keyed on the person:
`.dil_pick_area_value()` in
[R/dil_geography_values.R](R/dil_geography_values.R) and its three callers,
`.dil_lga_value()` in the same file, and `.acld_pick_by_state()` in
[R/acld_codeframes.R](R/acld_codeframes.R), which inline the same logic
independently. Co-residents therefore get different local government areas,
different Primary Health Networks and different Indigenous Regions, which is
impossible: all three are geographic catchments, and the household is now in
one place.

The single lever is `.dil_numeric_key()` in
[R/complete_dil_structures.R](R/complete_dil_structures.R), the per-person key
underneath every one of them. Keying the area path on `dwelling_id` instead of
the person makes all five agree at once. Fixing only the shared function would
miss `.dil_lga_value()` and `.acld_pick_by_state()`, and LGA is the code most
likely to be used as a household control.

## Cut the COES earnings ranges from the amount they band

COES reports weekly and hourly pay twice: once as an actual dollar amount and
once as a range code. The generator builds the amounts —
`WKPAYMJB`, `WKPAYSJB`, `WKPAYAJB`, `HRLYRMJB`, `HRLYRSJB` — from hours times
an hourly rate in `.coes_weekly_earnings()` and `.coes_hourly_earnings()` in
[R/generate_lfs.R](R/generate_lfs.R). The range codes that describe the same
pay do not read from them.

`WKPYMJBR`, `WKPYMJNR`, `WKPYSJBR`, `WKPYSJNR`, `HOURLYMJ` and `HOURLYSJ`
appear nowhere in the file, so they fall through to `.coes_from_codes()`,
which indexes the code list by the person key. A respondent can report $1,850
in `WKPAYMJB` and "Under $200" in `WKPYMJBR` on the same row. Anyone reading
the range gets a distribution unrelated to the amounts — and the range is what
the published Characteristics of Employment release reports, so it is the
natural item to reach for.

The two all-jobs ranges are derived, in `.coes_earnings_range()`, but against
invented breaks: five codes broad and nine narrow, where the code frames in
[inst/extdata/llfs/coes_values.csv](inst/extdata/llfs/coes_values.csv) carry
13 and 71. Broad codes 06 to 13 and narrow codes 10 to 71 are never emitted,
so the top two-thirds of the published distribution is empty by construction.

One helper cutting an amount against the real edges fixes all eight items:
$200 steps to $2,000 then $500 steps to $3,000 for the broad ranges, $40 steps
to $2,000 then $50 steps to $3,000 for the narrow, an open top band last, and
the non-amount codes (-2 payment in kind, -3 drew no wage, 00 not an employee)
following the paired `WKPAY*A` flag rather than being drawn independently. The
edges have to be typed rather than read: the narrow frames in `coes_values.csv`
elide their middle with a placeholder row, so only the ends are there to check
against.

## Take the STP birth month from the person, not from their id

`BIRTH_YEAR_MONTH_ABS` is the only demographic STP carries, and on the real
asset it is the payer's report of the payee's date of birth — the same person's
date of birth that CORE Demographics holds. In the generated data the year is
that person's and the month is a hash of their identifier:

```
birth_year_month.push(format!("{:04}{:02}", birth_year[i],
    ((spine_num as i64 + seed).rem_euclid(12) + 1)));
```

in [src/rust/src/stp.rs](src/rust/src/stp.rs), and the same expression in
[R/generate_stp.R](R/generate_stp.R). On the ten-million-person extract, across
4,476,439 person-values, the STP year matches the CORE birth year 100 per cent
of the time and the month matches 8.33 per cent — one in twelve, which is
chance.

The failure is quiet in the way that matters. Anyone deriving age from STP gets
an age that is right to the year and wrong by up to eleven months, and any check
that sets the payroll date of birth against the spine's reads as a catastrophic
linkage or parsing failure when the code is correct. Both are exactly what a
person prototyping a payroll-only age measure would build, since STP's birth
month is the only route to an age for a person the spine never places.

The fix is to pass the spine's birth month through instead of re-deriving it,
the same way the year already is. Where a deliberate discrepancy is wanted — an
employer reporting a wrong date of birth is real — it should be a small,
declared share of people rather than eleven twelfths of them.

## Drop the person id from the course-of-study table

`hes_madip_student_course` is reference data. The PLIDA registry lists nine
variables for it — COURSE, COURSE_LOAD, COURSE_OF_STUDY_CODE, COURSE_TYPE, FOE,
FOE_SUPP, INSTITUTION, SPECIAL_COURSE, YEAR — and describes a course of study,
not a student's enrolment in one. The generator writes a tenth,
`SYNTHETIC_AEUID`, which the real table does not have.

The invented column changes the table's grain and its meaning. On a
ten-million-person extract it produces 1,373,504 rows with 1,373,504 distinct
people, so every person appears exactly once and every course belongs to one
person — a course of study is a property of the provider, and here it is a
property of an individual. The real table is keyed on course, institution and
year.

The cost is a join that cannot survive the trip to the DataLab. Anyone
prototyping against fplida naturally joins the course attributes on the person
id, because it is sitting there, and that join has no counterpart in the real
data: the correct key is (COURSE, INSTITUTION, YEAR) with no person at all.
It fails loudly on arrival rather than silently, which is the better failure,
but only after the pipeline is written the wrong way round.

Removing the column also makes the table honest about its size: 1.37m rows for
what should be a course catalogue, one row per course per institution per year.

## Emit a real unit-of-study status code

`UNIT_STATUS` on the higher education load table takes two values, 4 and 7.
The element it stands for is TCSI E355, whose domain is 1 withdrew without
academic penalty, 2 failed, 3 successfully completed all the requirements,
4 to be commenced later or still in progress, 5 recognition of prior learning
(VET only), and 6 withdrew due to medical reasons. 7 is not in the current
version and is not in any earlier one either — the 2009 version ran 1 to 4 and
5 was added in 2011.

So the generator emits one code that means "still in progress" for an active
unit, which is at least defensible, and one that means nothing at all for a
withdrawn one. The consequence is the usual quiet kind: a consumer who codes
withdrawal correctly against the published element gets zero withdrawals here
and no error, while one who writes `== "7"` against the extract gets a number
that will be zero the moment it meets real data. Both are wrong and neither
finds out locally.

Worth fixing beyond the withdrawal code, because the element carries the unit
outcome: 2 and 3 distinguish a failed unit from a passed one, which is the
only unit-level attainment signal in the load table. Generating 3 for units
in completed years, 2 for a share of them, 1 for withdrawals and 4 for units
still in progress would make unit outcomes testable at all.

## Give STP the spine's birth month, not just its year

`BIRTH_YEAR_MONTH_ABS` on the pay events agrees with the spine on the year and
not on the month. Across 4,476,439 people who appear in both, the year matches
on every one and the month matches on 372,851 — 8.33 per cent, which is 1/12 to
two decimal places, so the month is drawn independently of the person's actual
birth month.

The consequence is quiet, because it only bites where the field is used as a
fallback. A pipeline that fills a missing spine birth month from payroll is
doing the right thing on real data, where both derive from the same person; here
it assigns a month at random and computes an age from it. Nothing errors, and
the disagreement is invisible unless someone thinks to compare the two — which
they will not, because the spine's demographics in fplida are 100 per cent
complete, so the fallback never fires and the bad path is never exercised at
all. Two independent defects hiding each other.

The fix is to draw the payroll birth month from the same person attribute the
spine uses, rather than redrawing the month. The year already comes from there,
so the plumbing exists.

While in that field: consider whether the generator should leave a realistic
share of spine birth months missing. At 100 per cent coverage no consumer can
test a demographics fallback of any kind, and every PLIDA-based pipeline has
one.

## Scale BLADE with the extract

BLADE holds 10,000 businesses no matter how large the extract is. On the
ten-million-person extract, STP carries 1,435,821 distinct employers and 8,389
of BLADE's 10,000 appear among them, so 0.58 per cent of employers can be given
an industry, a sector or a state. The identifier spaces agree — both are the
`BN` space, and almost every BLADE business is a real STP employer — so this is
a population-size problem, not a key problem.

What it costs is every employer-side measure. Industry, sector, GST status and
business state are the standard controls in labour work, and against fplida
they are missing for 99.4 per cent of job-months. A pipeline that joins them
runs clean, returns almost entirely NULL, and cannot be told apart from one
with a broken join — the same failure mode as the BUSOWN and VET completion
entries, and for the same reason: the join succeeds on a handful of rows.

The person side already scales. `n_people` drives the spine, and STP employers
are minted per employment spell, which is why the employer count tracks the
extract while BLADE does not. BLADE's business count should follow the same
lever, with the employer pool drawn from it rather than beside it. Note that
[the BUSOWN entry](#let-a-business-have-more-than-one-owner) turns on the same
pool: `business_pool::employer_bn()` was empty there for a related reason, and
fixing the pool once serves both.

## Make a VET completion name the program it completes

`TVA_ACTIVITY` and `TVA_COMPLETIONS` do not share a program identifier. On the
ten-million-person extract, activity holds 1,821,262 distinct person-program
pairs and completions 789,065, and 103 of them intersect. A completion
therefore cannot be attached to the program it completes.

The consequence is the failure mode that survives a local test. Nothing errors:
the join runs, returns almost nothing, and every downstream measure that needs
a completion — completion rates, time to completion, dropout, the end of a
study spell — comes back empty or, worse, silently reclassified. A pipeline
that dates the end of a program from its completion and otherwise infers a
departure will conclude that essentially every VET student in Australia drops
out.

The cause is one index. Both generators mint the identifier with the same
format string in [tva.rs](src/rust/src/tva.rs): the activity side builds
`prog_id` per spell as `P{foe}{qual_idx:03}{(i + 1) % 10_000:04}`, where `i` is
the spell, and pushes that id onto every subject row. The completions side
rebuilds the same string as `P{foe}{qual_idx:03}{(j + 1) % 10_000:04}`, where
`j` is the position in the kept-completions vector. The two agree only when
every spell completes, which is why 103 collide by chance. Pushing the spell's
own `prog_id` — it is already in scope — makes the tables join.

Two neighbours are worth checking at the same time, because the same audit
covers them. `PROGRAM_FOE_ID` and `PROGRAM_LOE_ID` are rebuilt on the
completions side from the spell as well, so they should agree once the id does.
And a completed program should not also produce activity rows implying it
continued past its completion date.

## Emit HEAP at three digits, as the Census does

`census_person.HEAP` on the ten-million-person extract holds the one-digit
values 1 through 8 — the broad rollup of the classification. The real 2021
Census variable is three digits, and at three digits it is ASCED level of
education verbatim across the qualification range (111 Higher Doctorate, 312
Bachelor Pass, 421 Diploma, 511/514 the certificates III and IV), departing
only where the Census renumbers to carry its own ordering: the Certificate
I & II detail sits at 720/721/724 against ASCED's 520/521/524, and the
below-Year-10 school years at 811/812 against ASCED's 622/623.

The one-digit form breaks the property that makes HEAP useful beside the
administrative completion tables: a consumer reading HEAP as ASCED — which is
the correct reading of the real variable — gets nothing from fplida, because
"3" is not an ASCED code. (The labour build has since moved its Census
attainment read to QALLP/QALFP — see the next entry — but any HEAP consumer
hits this.)

The fix is to draw the three-digit code directly. The generator already picks
a broad band; picking a detailed code within it (312 for band 3, 421 or 411
for band 4, 511/514 for band 5, 611/613/621 for band 6, 721/724 for band 7,
811/812 for band 8, with the nfd codes as rare draws) matches the published
category list, and the one-digit rollup remains recoverable as the first
character. `HSCP` alongside it is already one digit and correct — that
variable's published form is one digit — so the fix is HEAP alone.

## Date a higher education completion in its final study year

Every course completion on the ten-million-person extract is dated the year
after the course's last load row: joining distinct (person, course, year)
pairs from `hied_completions` against `hied_load` gives 875,265 of 875,265
completions with load in the prior year and none with load in the completion
year itself. TCSI reports a completion for the collection year in which the
course was completed, which is normally the year of its final teaching
period, so the shifted year means any consumer that dates a completion by
its final study months finds nothing and falls back to a year-end guess —
the labour build's completion_month lands on 1 December for every higher
education completion, and its he_complete events stack there.

The fix is to draw the completion year from the spell's final load year
rather than incrementing it.

## Generate QALLP and QALFP on the census person file

The 2021 person table generates `HEAP` and `HSCP` but not `QALLP` (non-school
qualification: level of education) or `QALFP` (non-school qualification:
field of study), the two items HEAP is derived from. Both are on the real
person record and both are natively ASCED — QALLP the level codes, QALFP the
field codes — which makes them the correct Census source for any attainment
measure built on the ASCED scale: no correspondence, no school years mixed
in, and a field of study beside the level. The labour build's attainment
ladder reads exactly these two and shims them to NULL when absent, so its
Census branch contributes nothing on fplida and first fires on real data.

Generation is cheap given HEAP already exists: a HEAP in the qualification
range implies a QALLP in the same ASCED band, a school-year HEAP implies no
non-school qualification, and QALFP draws from the field distribution
conditional on level.

## Give the higher education load table its second date

`hes_madip_student_load` has 22 variables in the registry and the generator
emits 20, dropping `CAMPUS_GLOBAL_REGION` and `COURSE_DATE`. The second is the
one that costs something. It is a month and year, typed as a date, covering
2005 to 2021 like the rest of the table. The registry description calls it the
month and year the student commenced the current course of study for the first
time; in the load file it dates the subject rather than the whole course. That
reading needs confirming against the data dictionary, and if it holds the
description in `variable-info` is wrong as well as the column being absent.

Without it, higher education cannot be dated below the calendar year.
`UNIT_STUDY_CENSUS` is a point inside a teaching period rather than a span, and
the completions table carries a year alone, so a study episode built from
fplida can only ever be a year flag. TVA, in the same extract, carries
`ACTIVITY_START_DATE` and `ACTIVITY_END_DATE` and collapses to spells. Anyone
building a monthly study measure therefore watches higher education stop at the
year, and cannot tell from here whether that is the source's limit or the
generator's. It is the generator's.

The change is small, because the spell already knows the answer.
[project_he_load()](src/rust/src/he.rs) is handed `spell_commence_year` and
emits every unit row from the spell it belongs to, and the census date a few
lines below is already chosen by semester — `{year}-03-31` for the first half
of a year's units, `{year}-08-31` for the rest. A date drawn by the same rule
and pushed as a twenty-first column is the whole job, at unit grain rather than
spell grain if the subject reading is right.

Two neighbours belong with it. The enrol table generates 15 of its 25 registry
variables, and the missing ten include `SEPARATION_STATUS_CODE`, which is how a
course exit without a completion is recorded, along with `NEW_ADMISSION`,
`CREDIT_OFFERED` and `TERT_ENT_SCORE`. And `REPORTING_YEAR_PERIOD`, which
carries the year and the period within it, only ever ends in `-1`: all
3,907,294 enrol rows on the ten-million-person extract are period 1, so the
sub-annual dimension of the higher education year is never exercised.

Note how clean the teaching calendar is once someone does use the census dates
to infer periods. `UNIT_STUDY_CENSUS` takes exactly two values a year, 31 March
and 31 August; every one of the 23,269,512 unit rows carries an
`EQUIVALENT_FT_STUDENT_LOAD` of exactly 0.125, so a year's load is the unit
count over eight; and `SUMMER_SCHOOL_INDICATOR` is 0 on every row. A semester
rule fitted against this succeeds perfectly and says nothing about how it will
behave against trimester and block-model institutions, which is the failure
mode that survives a local test.


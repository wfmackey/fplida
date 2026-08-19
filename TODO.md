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
info <- read.csv(gzfile("fplida.info/inst/variable-info.csv.gz"), stringsAsFactors = FALSE)
admin <- info[info$collection_type == "administrative", ]
done <- admin$description_provenance %in% c("ai", "official")
length(unique(paste(admin$dataset, toupper(admin$variable))[done]))
```

## Give some households more than two adults

No household in the spine holds more than two adults. Households of size 3 to
6 exist, but every member beyond the second is a child by construction, so
`table(adults per household)` returns only 1 and 2 on any extract.

Found while giving BUSOWN partnerships their co-owners. A family partnership
can therefore never have three partners: the generator asks the household for
up to four adults, gets two, and tops the rest up from unrelated people. The
same cap means no consumer can prototype a measure over adult children,
multi-generational households or share houses, and any rule that counts adults
in a dwelling runs against a distribution that stops at two.

The fix belongs in household formation on the spine, which currently seeds a
household with one or two adults and fills the rest with children. Letting a
share of households take a third or fourth adult -- an adult child, a parent,
a housemate -- would give the distribution its tail. The ABS reports about 4%
of Australian households as multi-family or group households, and adult
children living at home are far more common than that.

Note what turns on it. [The BUSOWN
partnerships](#let-a-business-have-more-than-one-owner) currently reach outside
the household for every partner beyond the second, and the co-residence share
of three-partner partnerships is zero rather than merely low.

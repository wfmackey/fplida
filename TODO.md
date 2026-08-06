# TODO

Work that is understood but not yet done. Each entry says why it matters, not
just what to change.

## Write a detailed description for every administrative variable

The target is all 7,578 administrative dataset-and-variable pairs across 39
datasets, covering 57,021 occurrences. 98 pairs are done. `ARID` is the worked
example; [data-raw/value-research/README.md](data-raw/value-research/README.md)
is the guide — how to research a variable, write it up, decide between the
`official` and `AI` tags, check the generator still agrees, and rebuild.

Organise by where the answers live, because that decides the method and the
tag. A dataset with a published dictionary can be quoted directly and tagged
`official`; one without needs prose written across several sources and tagged
`AI`.

### Wave 1 — datasets with a dictionary to quote (4,179 pairs)

The cheapest and the highest quality, because the work is transcription with
citation rather than composition.

| Dataset | Pairs | Authority |
|---|---|---|
| BLADE | 2,153 | BLADE Data Item List, ABS business statistics methodology, the trade appendices |
| ACLD | 841 | ABS Census Dictionary, ACLD detailed microdata workbook |
| AEDC | 433 | AEDC Data Dictionary and reference tables workbook |
| CENSUS | 261 | ABS Census Dictionary |
| DOMINO | 211 | DSS Aristotle metadata registry |
| DEX | 149 | DSS Aristotle, Data Exchange Protocols |
| NACDC | 75 | AIHW NACDC Data Dictionary |
| PBS | 30 | AIHW METEOR |
| MBS | 26 | AIHW METEOR |

### Wave 2 — ATO products (1,187 pairs)

Nearly every variable is a label on a return, a schedule or a statement, and
the ATO publishes instructions for each label. Slower per variable than a
dictionary lookup, but the answers exist.

PIT_ITR 601, SMSF 192, CGT 146, ATO_MCS 62, STP 56, RPS 42, PIT_PS 41, ATO_CR
24, PIT_IE 18, BUSOWN 5.

### Wave 3 — migration and settlement (1,101 pairs)

TRAVELLERS 588, AMEP 417, VISA 69, MT_DEMOGS 27. Home Affairs publishes visa
subclasses and movement concepts; the AMEP panel columns are largely
structural and can be described as a family rather than one at a time.

### Wave 4 — everything else (1,111 pairs)

NDIS 205, CORE 179, A&T 132, TVA 91, DEATHS 77, AIR 58, BIRTHS 55, SAE 55, HE
50, JK 49, APSED 36, MCD 31, JM 30, SDB 30, ERS 29, COMBINED 4.

### Cut across the waves first

Before starting any dataset, do the columns that appear in several. They are
cheap, they teach a reader how the asset fits together, and getting them wrong
once is wrong everywhere.

1. `ABN_HASH_TRUNC` — 60 occurrences across A&T, BUSOWN, DEX, JK, PIT_ITR and
   PIT_PS. Directly analogous to `ARID`: the employer in payment summaries, the
   business in BUSOWN, the funded organisation in DEX. Same generator question
   too, since a business address and a business identifier have to agree.
2. The other 27 cross-dataset columns, 438 occurrences. `EXTRACT_REF`,
   `ADR_TYP`, `VISA_SUBCLASS`, `INB_INCM_TYP` and the CGT schedule labels.

### Track it

```r
info <- read.csv(gzfile("inst/variable-info.csv.gz"), stringsAsFactors = FALSE)
admin <- info[info$collection_type == "administrative", ]
done <- admin$variable_description != admin$official_description
length(unique(paste(admin$dataset, toupper(admin$variable))[done]))
```

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

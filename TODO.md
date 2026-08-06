# TODO

Work that is understood but not yet done. Each entry says why it matters, not
just what to change.

## Document the variables that still say nothing

1,373 administrative variables, covering 5,072 occurrences, render a page that
tells a reader nothing: the value definition repeats the limitation, and the
description restates the variable name. `ARID` was one of them and is now done.
[data-raw/value-research/README.md](data-raw/value-research/README.md) is the
guide — how to find them, research them, write them up, check the generator
still agrees, and rebuild.

Do them in this order. The reasoning is in the guide: a column that spans
several datasets teaches a reader how the asset fits together, and is cheaper
per page improved than a column that appears once.

1. `ABN_HASH_TRUNC` — 60 occurrences across A&T, BUSOWN, DEX, JK, PIT_ITR and
   PIT_PS. Directly analogous to `ARID`: one identifier standing for the
   employer in payment summaries, the business in BUSOWN, the funded
   organisation in DEX. Same generator question too, since a business address
   and a business identifier have to agree with each other.
2. The rest of the cross-dataset columns — 28 in all, 438 occurrences.
   `EXTRACT_REF`, `ADR_TYP`, `VISA_SUBCLASS`, `INB_INCM_TYP` and the CGT
   schedule labels.
3. CGT — 1,371 occurrences over 101 variables, the largest single block. The
   variables are capital gains schedule labels, so the ATO's own schedule
   instructions document nearly all of them, one label at a time.
4. STP — 434 occurrences over only 4 variables, the best ratio in the registry.
5. PIT_ITR (460 over 41), AEDC (242 over 48), PIT_IE (229 over 16) and SAE
   (226 over 27).

TRAVELLERS is 500 occurrences but 328 variables, which is close to one page
each. Leave it until the cheaper blocks are done.

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

# TODO

Work that is understood but not yet done. Each entry says why it matters, not
just what to change.

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

# Schema fidelity audit, 2026-08-27

Every generated variable in the package was compared against the PLIDA variable
registry at `inst/plida_metadata/variables.csv`. The comparison uses the
`metadata_match` column already carried by the schema registers in
`fplida.info/inst/internal-docs/schema-registers/`, so it needs no build.

Of 32,060 generated variable rows, 31,745 match a registry variable on dataset
and name. The remaining 315 do not. Thirty-nine of those are the internal keys
`spine_id` and `abn_hash_trunc`, which are deliberate: they link synthetic
records across products and are not meant to be published PLIDA variables. That
leaves 276 rows, 47 distinct names, across 12 datasets, where the generator
publishes a column the registry does not declare.

## Where the invented names are

| Dataset | Rows | Distinct names |
|---|---|---|
| PIT_PS | 208 | 13 |
| CGT | 23 | 1 |
| NACDC | 10 | 8 |
| SDAC | 10 | 10 |
| A&T | 6 | 6 |
| AIR | 5 | 5 |
| ACLD | 4 | 1 |
| TRAVELLERS | 4 | 2 |
| AMEP | 2 | 1 |
| DOMINO | 2 | 2 |
| COMBINED | 1 | 1 |
| CORE | 1 | 1 |

PIT_PS accounts for three-quarters of the total. It emits the same 13 invented
names in every one of its 16 year tables: `ALLOWANCES`, `EMPLOYER_ABN`,
`FINANCIAL_YEAR`, `GROSS_PAYMENTS`, `LUMP_SUM_A`, `LUMP_SUM_B`, `LUMP_SUM_D`,
`LUMP_SUM_E`, `REPORTABLE_FBT`, `SUPER_GUARANTEE`, `TAX_WITHHELD`, `UNION_FEES`
and `WORKPLACE_GIVING`. The registry instead declares a schema that grows from
4 variables in 2001-02 to 36 in 2022-23, and changes its employer key from
`ABN_HASH_TRUNC` to `BN` partway through. This is being fixed on `fix-pitps`.

The registry is not thin for any of the affected datasets, so none of these are
gaps in the metadata. SDAC declares 2,810 distinct variable names and the
generator publishes ten that are not among them; CGT declares 2,187 and the
generator adds `INCOME_YEAR`; ACLD declares 1,401 and the generator adds
`WEIGHT4`.

The remaining invented names, by dataset:

- CGT: `INCOME_YEAR`, in all 23 year tables.
- NACDC: `CARE_TYPE`, `COUNTRY_OF_BIRTH`, `FUNCTIONAL_CAPACITY_SCORE`,
  `HCP_LEVEL`, `INDIGENOUS_STATUS`, `MARITAL_STATUS`, `SEX`, `STATE`.
- SDAC: `COMMCALN`, `DISGP`, `DISSTAT`, `DISTYPE`, `INGP`, `LFSP`, `MOBCALN`,
  `SELFCALN`, `SEXP`, `STATE`.
- A&T: `ANZSCO`, `COUNTRY_OF_BIRTH`, `DAYS_IN_TRAINING`, `INDIGENOUS_STATUS`,
  `QUALIFICATION_LEVEL`, `SCHOOL_BASED`.
- AIR: `ANTIGEN_CODE`, `ENCOUNTER_DATE`, `EPISODE_STATUS`,
  `SCHEDULE_CLASSIFICATION`, `VACCINE_SEQUENCE`.
- TRAVELLERS: `ERP_STATUS_QUARTERLY`, `PP_STATUS_QUARTERLY`.
- ACLD: `WEIGHT4`. AMEP: `HOURS_TOTAL`. DOMINO: `MONTH_OF_DEATH`,
  `YEAR_OF_DEATH`.

## The two that are probably the reverse

COMBINED and CORE each publish `SYNTHETIC_AEUID`, which is a real PLIDA
variable and is declared for other datasets. These two look like gaps in the
registry rather than invented names, and should be checked before anything is
renamed.

## Years as well as names

The same registers show PIT_PS writing `madipge-ato-d-pay-sum-fy2324` and
`-fy2425`. Neither product exists: the registry's PIT_PS reference period ends
at 2022-23. This is the year-validity defect being fixed on `fix-years`, and it
is not confined to PIT_PS — `build_fplida()` passes one global year vector to
every generator with nothing checking it against each dataset's declared
coverage.

Parsing the declared reference period for every dataset and comparing it against
the years encoded in generated table names finds six violations across the 148
tables whose names carry a financial-year pair: PIT_PS writes 2023-24 and
2024-25 against a period ending 2022-23, and PIT_ITR writes 2024-25 against a
period ending 2023-24. Six is a lower bound, not a total. The check only sees
tables whose names encode a financial-year pair, so products named by calendar
year are not covered by it.

## What this does not cover

The comparison is on variable NAME only. A column that carries the right name
but the wrong code frame or the wrong value range still counts as matched here.
Value-level fidelity is tracked separately in the admin value registers.

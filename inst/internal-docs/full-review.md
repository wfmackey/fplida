# Internal Dataset Explainers Full Review

Date: 2026-06-16

Scope: all `*dataset-explainer.qmd` files under `inst/internal-docs`, plus the
new `generated-schema-register.csv`.

## Summary assessment

The current guides are useful as first-pass orientation notes, but they are not
yet acceptable as source-backed data dictionaries. The main problem is that
many claims about category domains, distributions, and product coverage are
based on local generator behaviour or a single smoke sample, while the prose
often reads as if those claims describe the source data.

I created `generated-schema-register.csv` as the first corrective evidence
base. It records 1,585 generated variable rows with type, missingness, distinct
counts, sampled domains, numeric/date distributions, and joins to local PLIDA
or BLADE metadata where possible. This is necessary but not sufficient:
sampled generated values still need to be separated from official source
definitions.

I also split that register into one exact per-guide CSV under
`schema-registers/`, and every dataset explainer now links to its per-guide
register and reports generated variable-row counts plus direct PLIDA metadata
matches. That solves the immediate "where is every generated variable listed?"
problem for current `fplida` output, but it does not solve full official DIL
coverage or source-data distributions.

I added two companion audit files: `generated-variable-crosswalk.csv` has one
crosswalk/evidence-status row for each of the 1,585 generated variables, and
`local-metadata-variable-inventory.csv` records 49,987 PLIDA/BLADE metadata
rows for the dataset families covered by these guides. Those files make the
remaining generated-name and full-DIL coverage gaps explicit.

I also generated per-guide variable evidence appendices under
`variable-evidence-appendices/`. Each appendix renders the current schema
register and crosswalk into a table with one row per generated variable,
including observed generated values/distributions, local metadata/crosswalk
status, and unresolved-source caveats. These appendices make the schema tables
visible from the guides while preserving the warning that generated values are
implementation evidence, not official calibration facts.

I added `source-gap-register.csv` and `source-gap-register.qmd` as the
guide-level audit layer for source support and not-found evidence gaps. It
contains 38 product-area rows extracted from the dataset guides' source-backed
coverage tables.

I also added `census-variable-code-evidence.csv` and
`census-variable-code-evidence.qmd`. These give Census-specific variable-level
code evidence for all 57 generated Census variables, including ABS Dictionary
URLs where public variable pages exist and explicit local-only statuses for
identifiers and PLIDA/ASGS-style fields.

I then added five more source-audited variable-code evidence registers:
`home-affairs-variable-code-evidence.csv` (58 rows),
`dss-variable-code-evidence.csv` (144 rows),
`education-variable-code-evidence.csv` (81 rows),
`vet-apprentice-variable-code-evidence.csv` (88 rows), and
`ndis-variable-code-evidence.csv` (11 rows), each with a rendered `.qmd`
companion. These registers are deliberately stricter than the generated schema
appendices: they say when a public source supports only the broad concept, when
generated category labels are local, and when no public code table was found.

## Priority findings

### P0: Full source dictionaries still are not worked out for every variable

Earlier drafts summarised variable groups rather than listing every generated
variable. That failed the current requirement. The clearest example is BLADE:
the guide listed selected table groups, but the generated register contains
825 variables for selected BLADE tables. The guides now link to per-guide
register CSVs for every observed generated variable, but their prose tables
should still be treated as summaries rather than complete official data
dictionaries.

Remaining fix: attach official variable-level codebooks/distribution sources
where public or DIL documentation exists. The rendered appendices now expose
every generated variable, but they are still generated-output evidence rather
than official data dictionaries.

### P0: Some claims report sampled/generated values as if they were source data

Examples include the MBS/PBS smoke-output quantiles in
`dhda-health-dataset-explainer.qmd`, AEDC local category cutoffs and weights in
`education-dataset-explainer.qmd`, BIRTHS/DEATHS foundation targets in
`vital-events-dataset-explainer.qmd`, and apprenticeship status/qualification
weights in `vet-apprentice-dataset-explainer.qmd`.

These may be valid implementation facts, but they are not official source-data
facts unless tied to an official data dictionary or statistical publication.

Required fix: change every such row to identify the source as either generated
sample output, local Rust/test code, local foundation assumption, or official
source. Do not mix these in one evidence label.

### P0: Full PLIDA metadata coverage is being conflated with compact generator output

The local metadata inventory is much larger than the generated compact outputs:
PBS has 6,780 metadata variable rows, MBS 6,408, CORE 3,871, NDIS 2,647, AEDC
2,253, TVA 2,007, and DEATHS 1,711. The generated register covers the current
`fplida` output, not the full DIL surface.

Required fix: each guide needs separate sections for:

1. PLIDA/BLADE metadata inventory;
2. current `fplida` generated output;
3. official/public dictionary coverage;
4. unresolved gaps.

### P1: Metadata matching reveals concrete naming/coverage gaps

The generated crosswalk now covers 1,585 generated rows. It classifies:

- 825 rows as direct BLADE metadata matches.
- 692 rows as direct PLIDA `dataset + variable` matches.
- 58 rows as manual same-dataset PLIDA concept crosswalks.
- 4 rows as generated compact fields backed by multiple same-dataset metadata
  columns.
- 2 rows as generated date/component fields derived from same-dataset metadata.

At the crosswalk-status layer, only four rows are still explicitly marked as
not source-equivalent:

- `NACDC/INDIGENOUS_STATUS`: public NACDC May 2026 table specifications include
  Indigenous-status concepts, but the generated field is a local `Y`/`N`
  collapse and is not source-equivalent to the multi-category source codes.
- `PIT_PS/SUPER_GUARANTEE`: related local metadata exists for reportable
  employer superannuation contributions, but the generated field is ordinary
  SG-style local logic and is not source-equivalent.
- `PIT_PS/UNION_FEES`: an ATO deduction concept exists, but no same-dataset
  PIT_PS local metadata candidate was found, so the generated field is not
  source-equivalent.
- `PIT_PS/WORKPLACE_GIVING`: ATO workplace-giving and local STP metadata
  concepts exist, but no same-dataset PIT_PS local metadata candidate was
  found, so the generated field is not source-equivalent.

The DHDA/Health variable-code evidence register adds a second, stricter
value-domain check. It identifies additional NACDC compact fields whose
generated values are concept matches but not source-equivalent public code
values: `COUNTRY_OF_BIRTH`, `INDIGENOUS_STATUS`, `MARITAL_STATUS`,
`CARE_TYPE`, and `HCP_LEVEL`; `STATE` also needs its concept crosswalk changed
from `RECIPIENT_STATE` abbreviations to an ASGS `STE_CODE`-style field before
source-equivalence is claimed.

Required fix: keep both layers visible: crosswalk matches are not enough when
the generated value domain does not match the public source schema. Continue
replacing local generator distributions with official or local-metadata-backed
wording where stronger evidence exists.

The same stricter value-domain distinction is now applied to Home Affairs,
DSS, Education, VET/Apprentice, and NDIS via their variable-code evidence
registers. Notable blockers include Home Affairs visa/traveller public
codebooks not found, DOMINO public variable-level Aristotle code pages not
found, HE TCSI mapping not exhausted, AEDC generated cutoffs not verified
against official scoring tables, apprentice compact statuses/qualification
levels not source-equivalent, and NDIS generated labels needing data-rule
mapping.

### P1: PIT_ITR/PIT_IE generated distributions are now observed, but PIT_PS crosswalks remain weak

The updated register uses a targeted non-empty FY2021 PIT run under
`/tmp/fplida-pit-schema/fplida_12k`. It observed 5,990 `PIT_PS` rows, 5,223
`PIT_ITR` filer rows in each of the four ITR sub-tables, and 6,055 `PIT_IE`
rows. `PIT_ITR` and `PIT_IE` are now represented in the generated register.

Remaining fix: keep `SUPER_GUARANTEE`, `UNION_FEES`, and `WORKPLACE_GIVING`
explicitly labelled as not source-equivalent unless a same-dataset PIT source
variable is found.

### P1: Official source anchors are attached, but many exact code labels remain open

The research pass found usable official anchors:

- Home Affairs SDB data dictionary and caveats.
- AIR reporting rule/data-element source.
- PBS API data dictionary, PBS fees/co-payment notes, and PBS explanatory
  pages.
- MBS Online and MBS classification specifications.
- DSS DOMINO registry and DEX protocols/SCORE documentation.
- Department of Education HE student data page and AEDC data dictionaries.
- NCVER AVETMISS data element definitions and VET Provider Collection
  specifications.
- NDIS participant dataset/data-rules pages and management options.
- ABS Census dictionary/variables index.
- ABS Births, Deaths, Causes of Death methodologies.
- ABS Core Scoping/Life Course Dataset paper.
- ABS BLADE overview/microdata page plus local BLADE metadata.

Required fix: attach exact source-table or codebook citations to the remaining
category labels and distribution targets. Where no source was found, keep the
guide text explicitly marked as not found rather than filling the gap from
local generator output.

## File-level status

| File | Status | Main issue |
|---|---|---|
| `home-affairs-dataset-explainer.qmd` | Incomplete | The 58-row Home Affairs variable-code register separates SDB dictionary concept support from generated local domains. AMEP and Traveller compact fields now have local metadata crosswalks, but visa/traveller/AMEP public codebooks and generated distributions are still not found/source-backed. |
| `dhda-health-dataset-explainer.qmd` | Incomplete | The 88-row Health variable-code register now separates source-supported concepts from generated distributions. AIR compact fields and some MBS/PBS compact labels still lack public code tables; MCD has no public generated-variable-level codebook found; NACDC `COUNTRY_OF_BIRTH`, `INDIGENOUS_STATUS`, `MARITAL_STATUS`, `CARE_TYPE`, and `HCP_LEVEL` are concept-only/local compact values, not source-equivalent public code values. |
| `dss-dataset-explainer.qmd` | Incomplete | The 144-row DSS variable-code register separates DOMINO/DEX concept support from local generated domains. DOMINO public variable-level code pages were not found; DEX SCORE is source-supported at the 1-5 concept level, but generated assistance, demographic, and exit labels need exact protocol mapping. |
| `education-dataset-explainer.qmd` | Incomplete | The 81-row Education variable-code register adds exact TCSI anchors for HE attendance type/mode and AEDC dictionary concept support. Most HE TCSI mappings are not exhausted, and AEDC generated vulnerability cutoffs remain local until checked against official scoring/reference tables. |
| `vet-apprentice-dataset-explainer.qmd` | Incomplete | The 88-row VET/Apprentice variable-code register separates AVETMISS element support from generated subsets. TVA state code formatting needs tightening, and A&T compact statuses, qualification levels, and selected indicators are not source-equivalent public code values. |
| `ndis-dataset-explainer.qmd` | Incomplete | The 11-row NDIS variable-code register separates public NDIS concept support from local generated labels. Management type has public concept support, but generated `Self`/`Plan`/`Agency` labels, disability/support categories, participant status, and plan-budget distributions need exact data-rule/source mapping. |
| `vital-events-dataset-explainer.qmd` | Incomplete | ABS methodology supports scope and `ENTITY_CODES` now maps to local `ENTITY1..ENTITY20`; foundation distribution targets still need exact ABS/AIHW table citations or removal. |
| `core-combined-dataset-explainer.qmd` | Incomplete | ABS PLIDA/Life Course sources now support the broad core-location/core-relationship concepts, but generated location/relationship category labels and source-marker values still need exact public or DIL code evidence. |
| `blade-dataset-explainer.qmd` | Incomplete | The guide now links to the 825-row register and all generated BLADE rows match local BLADE metadata; ABS BLADE pages support scope/source families, but public source-agency codebooks are still needed where local metadata is blank or sparse. |
| `census-dataset-explainer.qmd` | Partly supported | Census dictionary and supplementary-code sources are attached, stale HSCP/code-set claims were corrected against the current generator, and the 57-row Census code-evidence register maps generated variables to ABS URLs or local-only evidence; remaining work is frozen full code tables for large classifications and official distribution evidence. |
| `pit-dataset-explainer.qmd` | Partly supported | PIT_PS, PIT_ITR, and PIT_IE generated schemas are now observed from a non-empty FY2021 run; source-coverage decisions separate ATO/ABS concepts from compact generator logic, and `SUPER_GUARANTEE`, `UNION_FEES`, and `WORKPLACE_GIVING` remain explicitly not source-equivalent PIT_PS variables. |
| `stp-dataset-explainer.qmd` | Partly supported | Representative STP generated tables are now in the register; current ATO STP reporting guidance has been added, but local DIL names still come from PLIDA metadata and not every payroll event field has a one-to-one public ATO citation. |

## Source anchors checked in this pass

- Home Affairs Settlement Database data dictionary:
  https://immi.homeaffairs.gov.au/settlement-services-subsite/files/settlement-database-data-dictionary.pdf
- Home Affairs settlement reports:
  https://immi.homeaffairs.gov.au/settling-in-australia/settlement-reports
- Home Affairs Settlement reporting caveats:
  https://immi.homeaffairs.gov.au/settling-in-australia/settlement-reports/settlement-reporting-caveats
- AMEP client profile paper:
  https://immi.homeaffairs.gov.au/amep-subsite/Files/paper-a-profile-amep-clients.pdf
- Australian Immunisation Register reporting:
  https://www.health.gov.au/topics/immunisation/immunisation-information-for-health-professionals/using-the-australian-immunisation-register
- PBS API data dictionary:
  https://data.pbs.gov.au/download/api/files/2025-PBS-API-V3-Data-Dictionary-v3.6.5.pdf
- PBS fees, patient contributions, and Safety Net thresholds:
  https://www.pbs.gov.au/info/healthpro/explanatory-notes/front/fee
- MBS Online:
  https://www.mbsonline.gov.au/
- MBS classification specifications:
  https://www.mbsonline.gov.au/internet/mbsonline/publishing.nsf/Content/81D3D41DCEAE03F0CA2581C60013304B/%24File/MBS%20Classification%20specifications%20effective%20November%202014.pdf
- NACDC data dictionary:
  https://www.gen-agedcaredata.gov.au/resources/publications/2020/september/national-aged-care-data-clearinghouse-data-dictionary
- NACDC user guide and table specifications:
  https://www.gen-agedcaredata.gov.au/resources/publications/2026/may/national-aged-care-data-clearinghouse-user-guide
- NACDC May 2026 table specifications workbook:
  https://www.gen-agedcaredata.gov.au/getmedia/37f96968-da83-453b-8ccb-003404df9d45/NACDC-table-specifications-May-2026
- ATO union fees and professional association fees:
  https://www.ato.gov.au/individuals-and-families/income-deductions-offsets-and-records/deductions-you-can-claim/memberships-accreditations-fees-and-commissions/union-fees-subscriptions-to-associations-and-bargaining-agents-fees
- ATO workplace giving programs for employees:
  https://www.ato.gov.au/individuals-and-families/jobs-and-employment-types/working-as-an-employee/workplace-giving-programs-for-employees
- ATO reportable employer super contributions:
  https://www.ato.gov.au/businesses-and-organisations/super-for-employers/setting-up-super-for-your-business/identify-reportable-employer-super-contributions
- DSS metadata for research datasets:
  https://www.dss.gov.au/doing-business-us/corporate-policies/metadata-research-datasets
- DSS DOMINO metadata:
  https://dss.aristotlecloud.io/item/1942/dataset/domino-external-analytical-version
- DEX policy:
  https://dex.dss.gov.au/policy
- DEX protocols:
  https://dex.dss.gov.au/sites/default/files/documents/2023-03/1931-data-exchange-protocols.pdf
- CHSP DEX protocols:
  https://dex.dss.gov.au/sites/default/files/documents/2025-09/2646-2536-chsp-dex-protocols.pdf
- Higher education student data:
  https://www.education.gov.au/higher-education-statistics/student-data
- TCSI attendance type element:
  https://www.tcsisupport.gov.au/element/330
- TCSI mode of attendance element:
  https://www.tcsisupport.gov.au/element/329/7.00
- Department of Education AEDC page:
  https://www.education.gov.au/early-childhood/about/data-and-reports/australian-early-development-census
- AEDC 2025 Data Dictionary:
  https://www.aedc.gov.au/resources/detail/aedc-2025-data-dictionary
- NCVER Total VET students and courses:
  https://www.ncver.edu.au/research-and-statistics/collections/students-and-courses-collection/total-vet-students-and-courses
- NCVER AVETMISS overview:
  https://www.ncver.edu.au/rto-hub/what-is-avetmiss
- AVETMISS data element definitions:
  https://www.ncver.edu.au/__data/assets/pdf_file/0022/62383/AVETMISS-Data-element-definitions-2_3-Nov-2022.pdf
- DEWR Australian Apprenticeships:
  https://www.dewr.gov.au/australian-apprenticeships
- NDIS datasets:
  https://dataresearch.ndis.gov.au/datasets
- NDIS participant datasets:
  https://dataresearch.ndis.gov.au/datasets/participant-datasets
- NDIS management options:
  https://www.ndis.gov.au/participants/using-your-funding/plan-implementation-meeting/guide-your-management-options
- ABS Census dictionary:
  https://www.abs.gov.au/census/guide-census-data/census-dictionary/latest-release
- ABS Census supplementary codes:
  https://www.abs.gov.au/statistics/detailed-methodology-information/information-papers/understanding-supplementary-codes-census-variables
- ABS Births methodology:
  https://www.abs.gov.au/methodologies/births-australia-methodology/2024
- ABS Causes of Death methodology:
  https://www.abs.gov.au/methodologies/causes-death-australia-methodology/2024
- ABS Life Course Dataset scoping paper:
  https://www.abs.gov.au/statistics/detailed-methodology-information/information-papers/scoping-population-life-course-dataset
- ABS Life Course Data Initiative data developments:
  https://www.abs.gov.au/about/key-priorities/life-course-data-initiative/data-developments
- ABS Life Course Dataset household structures:
  https://www.abs.gov.au/statistics/detailed-methodology-information/information-papers/creating-household-structures-using-life-course-dataset
- ABS BLADE overview:
  https://www.abs.gov.au/statistics/data-integration/integrated-data/business-longitudinal-analysis-data-environment-blade
- ABS BLADE microdata page:
  https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/business-longitudinal-analysis-data-environment-blade
- ATO STP rules of reporting:
  https://www.ato.gov.au/businesses-and-organisations/hiring-and-paying-your-workers/single-touch-payroll/in-detail/single-touch-payroll-employer-reporting-guidelines/rules-of-reporting-through-stp
- ATO STP Phase 2 disaggregation of gross:
  https://www.ato.gov.au/businesses-and-organisations/hiring-and-paying-your-workers/single-touch-payroll/in-detail/single-touch-payroll-phase-2-employer-reporting-guidelines/reporting-the-amounts-you-have-paid/disaggregation-of-gross

## Required next work

1. Keep the four non-proven crosswalk rows in
   `generated-variable-crosswalk.csv` explicitly labelled as
   not source-equivalent: `NACDC/INDIGENOUS_STATUS`,
   `PIT_PS/SUPER_GUARANTEE`, `PIT_PS/UNION_FEES`, and
   `PIT_PS/WORKPLACE_GIVING`. Also keep the stricter
   `dhda-health-variable-code-evidence.csv` value-domain flags visible for
   compact NACDC fields whose generated values do not match public table-spec
   values.
2. Replace remaining smoke/foundation claims in guide prose with register-backed wording
   or official-source wording.
3. Expand official variable-level codebook citations where public sources exist;
   where they do not, keep the guide text explicitly unresolved.
4. Use the seven variable-code evidence registers now in place
   (`census`, `dhda-health`, `home-affairs`, `dss`, `education`,
   `vet-apprentice`, and `ndis`) as the controlling source-status layer for
   future fplida build tests and code-domain changes.

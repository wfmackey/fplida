# Administrative and Census release audit

Structure and value coverage are complete enough to publish. 1,408
administrative and Census occurrences remain entirely missing and 278 carry a
reference-period warning. Both sets are open work, listed below by dataset.

This document reports two runs with seed `20260803`:

- the baseline run of 3 August 2026, which the remediation work was measured
  against; and
- the current run of 4 August 2026.

Both covered all administrative PLIDA products, Census, and all BLADE tables.
Both deferred survey-value checks as agreed.

## Scope

The audit checked:

- 43 PLIDA datasets;
- 559 PLIDA products;
- 2,140 PLIDA product-table structures;
- 62 BLADE tables;
- 56,994 administrative and Census variable occurrences in canonical tables;
  and
- 2,990 physical output files, including richer auxiliary outputs.

The audit deferred values for `LFS`, `NHS`, `NSMHW`, `PEX`, and `SDAC`. It
also deferred values for 25 BLADE survey tables. Census, ACLD, and AEDC
remained in scope.

## Method

The fixed-seed build used 500 persons and the years 2006 to 2025. Each
canonical PLIDA table contained no more than 100 rows. The audit compared each
canonical table with the bundled Data Item List.

The scan checked:

- product and table coverage;
- official variable coverage;
- explicit placeholder values;
- dataset-counter placeholders;
- generic `Cnn` category codes;
- values outside published domains;
- entirely missing administrative variables; and
- suspicious fixed years and dates.

The command was:

```bash
Rscript scripts/audit_admin_schema_values.R \
  --build \
  --build-root /tmp/fplida-admin-build \
  --n 500 \
  --seed 20260803 \
  --years 2006:2025 \
  --report-dir /tmp/fplida-admin-report \
  --report-only
```

The `--report-only` option writes findings without returning a failed process.
Remove that option for the strict release gate.

## Results

| Check | 3 August baseline | 4 August current |
|---|---:|---:|
| PLIDA products observed | 559 of 559 | 559 of 559 |
| PLIDA structures observed | 2,140 of 2,140 | 2,140 of 2,140 |
| PLIDA structures with complete schemas | 2,140 of 2,140 | 2,140 of 2,140 |
| BLADE tables observed | 62 of 62 | 62 of 62 |
| BLADE tables with complete schemas | 62 of 62 | 62 of 62 |
| Missing official variables | 0 | 0 |
| Unmatched canonical variables | 0 | 0 |
| Explicit placeholder errors | 0 | 0 |
| Generic category-code errors | 0 | 0 |
| Official-domain errors | 0 | 0 |
| Administrative and Census occurrences with a value | 40,532 of 56,994 | 55,586 of 56,994 |
| Entirely missing administrative and Census occurrences | 16,462 | 1,408 |
| Suspicious-value warnings | 275 | 278 |
| Unallowed release-gate failures | 16,462 | 1,408 |

The observed-value rate moved from 71.12 per cent to 97.53 per cent. The unit
is one official variable in one canonical administrative or Census table. It is
not a count of unique variable names. The denominator did not change, so the
two columns compare directly.

No BLADE administrative variable was entirely missing in either run. Every
missing occurrence was in a PLIDA administrative or Census table.

The absence of placeholder and domain errors does not establish statistical
fidelity. Unsupported code frames remain typed missing. Approximate generators
also need source evidence before a release claim can describe them as
realistic.

## Largest value gaps

Remaining gaps as at 4 August 2026, across 20 datasets. The baseline column is
the 3 August figure, so the two read as a remediation record.

| Dataset | 3 August baseline | 4 August current |
|---|---:|---:|
| MBS | 3,377 | 0 |
| PBS | 2,664 | 0 |
| AEDC | 1,910 | 519 |
| NDIS | 1,099 | 7 |
| CGT | 1,062 | 41 |
| CORE | 885 | 0 |
| PIT_ITR | 825 | 178 |
| ACLD | 766 | 39 |
| TRAVELLERS | 722 | 32 |
| TVA | 626 | 95 |
| CENSUS | 413 | 0 |
| NACDC | 275 | 35 |
| DEATHS | 232 | 0 |
| STP | 218 | 61 |
| DOMINO | 193 | 127 |
| RPS | — | 86 |
| SAE | — | 74 |
| A&T | — | 60 |
| AIR | — | 17 |
| ATO_MCS | — | 15 |

MBS, PBS, CORE, CENSUS and DEATHS are now fully populated. AEDC carries the
largest remaining gap at 519 occurrences, then PIT_ITR at 178 and DOMINO at
127. Those datasets still need approved public domains or explicit source
translations.

## Warnings

Counts are from the 4 August 2026 run.

| Warning | Count |
|---|---:|
| All parseable dates fell in 2024 | 186 |
| A financial year was fixed | 64 |
| A calendar year was fixed | 28 |

Warnings are review prompts. They are not release-gate errors. Each warning
needs a check against the product reference period. They are open work and do
not block publication.

## Registers

These files record the 3 August 2026 baseline. They are a fixed reference point,
not a live snapshot, and several tests assert exact counts against them:

- `admin-value-audit-summary.csv` contains the machine-readable summary;
- `admin-value-gap-register.csv` identifies every entirely missing occurrence
  in the baseline run;
- `admin-value-warning-register.csv` identifies every baseline warning; and
- `source-gap-register.csv` records broader evidence gaps.

Re-run `scripts/audit_admin_schema_values.R` for the current position. Do not
overwrite the baseline registers; the remediation tests read them.

## Open work

These items remain open. None blocks publication.

1. Resolve the 1,408 remaining administrative and Census gaps with sourced
   values, a safe derivation, or a documented applicability rule. AEDC,
   PIT_ITR and DOMINO carry the largest shares.
2. Check each of the 278 warnings against its product reference period.
3. Run the strict audit without `--report-only` once the gaps close, and
   obtain zero unallowed failures.
4. Keep survey-value validation explicitly deferred.

## Package validation

The macOS source-package check completed with zero errors and zero warnings.
The test suite passed 4,713 assertions with no failures and no skips, with
every suggested package installed.

The check reported one compiled-code note. Rust's static standard-library
object references the system `stdout` and `stderr` symbols. Package code does
not call `print!`, `println!`, `eprint!`, or `eprintln!`.

Linux and Windows are covered by the GitHub workflows. They have not yet run,
because the local commits have not reached the remote repository. Confirm both
before tagging a release.

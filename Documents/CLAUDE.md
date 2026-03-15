# Ontario Hospital Name Crosswalk — CLAUDE.md
*Last Updated: March 2026*

This file is the primary reference for Claude Code and for resuming work after any break.
Read it before writing any code or modifying any file.

---

## Project Purpose

Creates and maintains a crosswalk linking Ontario hospitals across four data sources,
using the MOH FAC (3-digit facility identifier) as the primary key. Enables cross-dataset
analysis such as comparing MOH budget data with Ontario Sunshine List salary expenditures.

---

## Architecture — Two-Table Flat Store

The canonical store is two RDS/CSV files. Everything else is either a source input or a
derived output. No script other than the designated writer for each table should modify
these files directly.

### `outputs/fac_master.rds` / `fac_master.csv`
One row per FAC. Written by script 01; enriched with `sactype_value` by script 06.

```
fac               character   3-digit FAC (primary key)
canonical_name    character   MOH official name (uppercase)
moh_type          character   MOH facility type classification
sactype_value     integer     Statistics Canada SAC type code 1–7 (NA for some chronic/private)
status            character   "active" | "merged_into" | "closed"
merged_into_fac   character   FAC of successor (if status = merged_into)
effective_date    character   Date of status change (if known)
```

### `outputs/fac_aliases.rds` / `fac_aliases.csv`
One row per alias observation. Appended by scripts 02–06.

```
fac                   character   Foreign key to fac_master
source_system         character   "sunshine" | "moh" | "cihi" | "statscan"
name_raw              character   Exact string as observed in source
name_norm             character   Normalized for matching (uppercase, stripped)
unsafe                logical     TRUE = collision risk, do not use for matching
unsafe_reasons        character   Pipe-delimited reason codes
year_observed         integer     Source year (sunshine only; NA otherwise)
fac_before_correction character   Pre-correction FAC if apply_fac_corrections was run
```

---

## Script Inventory

Scripts live in `code/`. Run them from the project root. Each script does one thing:
input → transform → output, with a printed summary at the end.

```
code/
  01_build_fac_master.R             MOH FAC list -> fac_master
  02_build_sunshine_aliases.R       Salary feed -> fac_aliases (source = sunshine)
  03_detect_unsafe_aliases.R        Collision + pattern flags -> unsafe on fac_aliases
  04_append_moh_aliases.R           MOH canonical names -> fac_aliases (source = moh)
  05_append_cihi_aliases.R          CIHI override map -> fac_aliases (source = cihi)
  06_append_sactype_aliases.R       SACtype fuzzy+override -> fac_aliases (source = statscan)
                                    Also writes sactype_value onto fac_master
  07_validate_alias_table.R         QA checks — report and continue (never stops pipeline)
  08_build_derived_tables.R         Materialise all derived outputs

  utils/
    normalize.R                     normalize_name(), standardize_colnames()
    fuzzy_match.R                   Jaro-Winkler helpers (if extracted)

  apply_fac_corrections.R           One-time manual FAC remapping (keep as-is)
  fix_07_validation_issues.R        One-time fixes from 2026-03-15 QA run (keep for audit)
```

### Standard run order (full rebuild)

```r
source("code/01_build_fac_master.R")
source("code/02_build_sunshine_aliases.R")
source("code/03_detect_unsafe_aliases.R")
source("code/04_append_moh_aliases.R")
source("code/05_append_cihi_aliases.R")
source("code/06_append_sactype_aliases.R")
source("code/07_validate_alias_table.R")
source("code/08_build_derived_tables.R")
```

Scripts 01–06 each write to `outputs/fac_master.rds` and/or `outputs/fac_aliases.rds`.
Script 07 reads both and writes validation reports only — it never modifies the core tables.
Script 08 reads both and writes to `outputs/derived/` only.

---

## Source Files

```
sources/
  MOHFAC.csv / MOHFAC.rds               MOH facility list (produced by importMOHFAC.R)
  salary_alias_feed_2024.csv            Sunshine List alias feed (script 02 input)
  CIHI.csv                              Raw CIHI names
  cihi_fac_overrides.csv                Manual CIHI->FAC mappings
  odhf_v1.1.csv                         Statistics Canada ODHF hospital file
  2021_csd.csv                          Statistics Canada CSD/SACtype lookup
  sactype_prepared.csv                  Prepared SACtype source (one-time; script 06 input)
  sactype_fac_overrides.csv             Manual SACtype->FAC mappings (fac as chr, value as int)
  FAC_Corrections.csv                   Manual FAC corrections
```

---

## Derived Outputs

All written by script 08 to `outputs/derived/`. Never edit these by hand — regenerate
from `fac_master` + `fac_aliases`.

```
outputs/derived/
  name_universe_safe.csv        Safe aliases only, all sources — primary lookup table
  fac_aliases_all.csv           All aliases including unsafe (audit use)
  fac_to_source_coverage.csv    One row per FAC, boolean columns per source_system
  sactype_fac_rollup.csv        SACtype value + label + provenance per FAC
  cihi_fac_rollup.csv           CIHI reporting names per FAC + coverage flag

outputs/validation/
  validation_report_YYYY-MM-DD.csv    Script 07 summary (one file per run)
  alias_collisions.csv                Safe aliases appearing under >1 FAC
  edge_cases.csv                      Hotel Dieu / St Joseph / network name audit
  source_coverage.csv                 Alias counts per source_system

outputs/
  sactype_auto_matched.csv        Auto-matched ODHF rows (script 06 diagnostic)
  sactype_unmatched.csv           Unmatched ODHF rows (script 06 diagnostic)
  sactype_fac_classification.csv  FAC-level SACtype with provenance (script 06)
```

---

## Override File Governance

Every source that lacks a FAC uses the same pattern:
auto-match what you can → export override template → fill in FAC and reason → re-run script.

Override files live in `sources/`. They are the manual review mechanism.
Never apply corrections by editing `fac_master` or `fac_aliases` directly.

Key constraint: `sactype_fac_overrides.csv` must have `fac` as character and
`sactype_value` as integer (1–7). Blank `override_reason` causes script 06 to stop.

---

## SACtype Codes (Statistics Canada)

| Code | Label |
|------|-------|
| 1 | CMA — Census Metropolitan Area (large urban) |
| 2 | CA — Census Agglomeration (medium urban) |
| 3 | Strong Metropolitan Influence Zone |
| 4 | Moderate Metropolitan Influence Zone |
| 5 | Weak Metropolitan Influence Zone |
| 6 | No Metropolitan Influence Zone |
| 7 | Remote |
| NA | Intentional — chronic/palliative/private facilities |

Eight FACs intentionally carry NA SACtype (605, 680, 827, 850, 854, 908, 927, 971).

---

## Key Numbers (as of March 2026)

| Metric | Value |
|--------|-------|
| Total FACs in master | 148 |
| FACs with SACtype | 140 |
| Total aliases | 742 |
| Safe aliases | 683 |
| Unsafe aliases | 59 |
| FACs with sunshine coverage | 137 |
| FACs with CIHI coverage | 115 (105 safe) |
| FACs with statscan coverage | 140 |
| FACs with all four sources | 112 |

---

## Design Principles

**Flat table is the source of truth.** `fac_master` and `fac_aliases` replace the YAML
store archived in `archive/canonical_hospitals_20260313/`.

**Each script does one thing.** No script sources another mid-execution.

**Override files are the manual review mechanism.** Do not edit core tables by hand.

**Derived tables are always regenerated.** If something looks wrong in a derived table,
fix the source table, then re-run script 08.

**Phase H (YAML-era derived tables) is retired.** Do not run it. Use script 08 instead.

---

## Scope

Ontario hospitals only — 148 FACs in the current master. No expansion to other provinces
or health system entities without explicit discussion.

Strategic Plans/Websites integration is a future phase not yet implemented. When added,
it will follow the same pattern as SACtype: prepare script + append script +
`source_system = "strategic_plan"` in `fac_aliases`.

---

## R Packages Required

```r
library(dplyr)
library(readr)
library(stringr)
library(stringdist)   # Jaro-Winkler fuzzy matching
library(tidyr)
library(tibble)
```

---

## Repository Layout

```
OntarioHospitalNamesCrosswalk/
├── code/               All R scripts (versioned)
│   └── utils/          Shared helper functions
├── sources/            Input data files (NOT in Git)
├── outputs/            fac_master, fac_aliases, derived tables, validation (NOT in Git)
│   └── derived/
│   └── validation/
├── archive/            Retired YAML store and legacy scripts (NOT in Git)
├── docs/               Documentation markdown files
└── Documents/          Guidance documents
```

`.gitignore` excludes all data files. Only code, docs, and config are versioned.

# Ontario Hospital Crosswalk — Build Checklist
*Updated March 2026 — reflects flat-table architecture (scripts 01–08)*

---

## Preconditions

- [ ] Project root set to `E:\Public\OntarioHospitalNamesCrosswalk`
- [ ] Source files present in `sources/` (not in Git)
- [ ] R packages available: dplyr, readr, stringr, stringdist, tidyr, tibble
- [ ] `code/utils/normalize.R` present and sourced correctly by all scripts

---

## Script 01 — Build fac_master

- [ ] `sources/MOHFAC.csv` is current
- [ ] Run `code/01_build_fac_master.R`
- [ ] Console confirms: row count matches MOHFAC source, no duplicate FACs,
      no missing canonical_name or moh_type
- [ ] `outputs/fac_master.rds` and `outputs/fac_master.csv` written

---

## Script 02 — Sunshine Aliases

- [ ] `sources/salary_alias_feed_YYYY.csv` is present
- [ ] `year_observed` value in script matches the data year
- [ ] Run `code/02_build_sunshine_aliases.R`
- [ ] Console confirms: alias count, FAC coverage, year distribution
- [ ] `outputs/fac_aliases.rds` written (sunshine source rows only at this point)

---

## Script 03 — Detect Unsafe Aliases

- [ ] Run `code/03_detect_unsafe_aliases.R`
- [ ] Console confirms: unsafe count, collision count, pattern-match count
- [ ] Review unsafe flagging logic if collision count seems unexpectedly high or low
- [ ] `outputs/fac_aliases.rds` updated with unsafe flags

---

## Script 04 — MOH Aliases

- [ ] Run `code/04_append_moh_aliases.R`
- [ ] Console confirms: 148 MOH aliases appended (one per FAC), all safe
- [ ] `outputs/fac_aliases.rds` updated

---

## Script 05 — CIHI Aliases

- [ ] `sources/cihi_fac_overrides.csv` is current
- [ ] Run `code/05_append_cihi_aliases.R`
- [ ] Console confirms: CIHI FAC coverage count, no CIHI aliases for FACs
      absent from fac_master
- [ ] `outputs/fac_aliases.rds` updated

---

## Script 06 — SACtype Aliases + fac_master Enrichment

- [ ] `sources/sactype_prepared.csv` exists (produced by prepare_sactype_source.R)
- [ ] `sources/sactype_fac_overrides.csv` is current:
      - [ ] `fac` column is character type
      - [ ] `sactype_value` column is integer (1–7), no text labels
      - [ ] No blank `override_reason` values
      - [ ] No FACs that are absent from fac_master
- [ ] Run `code/06_append_sactype_aliases.R`
- [ ] Console confirms:
      - [ ] FACs with SACtype: 140 (or documented explanation for any change)
      - [ ] NAs: 8 (intentional — see list below)
      - [ ] Inconsistent multi-site FACs: ≤9 (all documented as legitimate)
- [ ] SACtype distribution sanity check: majority of FACs should be codes 1–3 (urban),
      meaningful presence of codes 5–7 (rural/remote)
- [ ] `outputs/fac_master.rds` updated with sactype_value
- [ ] `outputs/fac_aliases.rds` updated with statscan aliases

**Intentional NA SACtype FACs:**
605, 680, 827, 850, 854, 908, 927, 971 (chronic/palliative/private — out of scope)

---

## Script 07 — Validation

- [ ] Run `code/07_validate_alias_table.R`
- [ ] All four checks pass:
      - [ ] `facs_with_no_alias = 0` — OK
      - [ ] `facs_with_no_safe_alias = 0` — OK
      - [ ] `safe_alias_collisions = 0` — OK
      - [ ] `orphan_alias_facs = 0` — OK
- [ ] `facs_in_master == facs_in_aliases` (both should be 148)
- [ ] Review `outputs/validation/edge_cases.csv` — Hotel Dieu and St Joseph entries
      should all be marked unsafe or be unambiguous single-FAC matches
- [ ] Validation report written to `outputs/validation/validation_report_YYYY-MM-DD.csv`

**If any FAIL is present:** resolve before proceeding to script 08.
Common fixes: update override file, re-run script 06, mark colliding aliases unsafe.

---

## Script 08 — Derived Tables

- [ ] Run `code/08_build_derived_tables.R`
- [ ] Console confirms all five derived tables written to `outputs/derived/`
- [ ] `name_universe_safe.csv` row count matches `aliases_safe` from script 07
- [ ] `fac_to_source_coverage.csv` has 148 rows
- [ ] `sactype_fac_rollup.csv` has 148 rows
- [ ] `cihi_fac_rollup.csv` has 148 rows
- [ ] Source coverage summary: moh = 148, sunshine ~137, cihi ~115, statscan ~140

---

## Post-Build

- [ ] Commit to GitHub with message describing what changed and why
- [ ] If significant decisions were made, write a session summary to `docs/`
- [ ] Update `CLAUDE.md` key numbers table if counts have changed materially

---

## Quick Diagnostic Snippets

**Check fac_master current state:**
```r
fac_master <- readRDS("outputs/fac_master.rds")
cat("FACs:", nrow(fac_master), "\n")
table(fac_master$sactype_value, useNA = "ifany")
```

**Check fac_aliases current state:**
```r
aliases <- readRDS("outputs/fac_aliases.rds")
cat("Aliases:", nrow(aliases), "| Unsafe:", sum(aliases$unsafe), "\n")
table(aliases$source_system)
```

**Check for orphan aliases:**
```r
fac_master <- readRDS("outputs/fac_master.rds")
aliases    <- readRDS("outputs/fac_aliases.rds")
aliases |> dplyr::filter(!fac %in% fac_master$fac) |> dplyr::distinct(fac, source_system)
```

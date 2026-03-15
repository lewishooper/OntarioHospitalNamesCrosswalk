# Annual Update Procedure
*Ontario Hospital Name Crosswalk*
*Last Updated: March 2026*

---

This document describes how to incorporate new data for each source system into the
crosswalk. The update cycle follows the Ontario Sunshine List release, which publishes
in late March each year.

---

## When to Run Updates

| Trigger | Action |
|---------|--------|
| New Sunshine List released (~March) | Run sections 1 and 2 |
| New MOH FAC file issued | Run section 3 |
| New CIHI data available | Run section 4 |
| New Statistics Canada ODHF release | Run section 5 |

Sections are independent. Run only the sections relevant to what has changed.
Always run script 07 and script 08 after any update to validate and refresh derived tables.

---

## Section 1 — New Sunshine List Year

**Scripts affected:** 02, 03

**Steps:**

1. Obtain the new salary survey file and place it in `sources/` following the existing
   naming convention. Do not overwrite the previous year's file.

2. Update the `SALARY_PATH` and `year_observed` value at the top of
   `code/02_build_sunshine_aliases.R` to point to the new file and the correct year.

3. Run script 02. It will append new aliases with `source_system = "sunshine"` and the
   new `year_observed`. Aliases from prior years are preserved with their original year.

4. Run script 03 to re-evaluate unsafe flags across the full alias set. New Sunshine names
   may introduce new collisions or resolve old ones.

5. Run scripts 07 and 08 to validate and refresh derived tables.

**What to watch for:**
- New hospital names in the Sunshine List that don't match any existing alias. These will
  appear in the script 07 validation report as FACs with reduced sunshine coverage if the
  name changed substantially.
- New FACs in the Sunshine List that don't exist in `fac_master`. These appear as orphan
  aliases in script 07 and require a FAC correction entry in `sources/FAC_Corrections.csv`.

---

## Section 2 — FAC Corrections for New Sunshine Aliases

If script 07 reports orphan aliases from the sunshine source after a new year's data is
loaded, the salary data likely contains a FAC that has been merged or renumbered.

1. Check the orphan FAC against the current MOH FAC file.
2. If the FAC is a known predecessor to an active FAC, add a row to
   `sources/FAC_Corrections.csv` mapping the old FAC to the current one.
3. Re-run `code/apply_fac_corrections.R`, then re-run scripts 02–03 and 07–08.

---

## Section 3 — New MOH FAC File

**Scripts affected:** 01, and potentially all downstream scripts

This is the highest-impact update. A new MOH FAC file may add FACs (new hospitals,
de-mergers), deprecate FACs (closures, mergers), or change canonical names or types.

**Steps:**

1. Place the new MOH FAC file in `sources/`. Update `importMOHFAC.R` to point to it and
   re-run that script to produce the new `MOHFAC.csv` / `MOHFAC.rds`.

2. Run script 01 to rebuild `fac_master` from the new source.

3. Compare the new `fac_master` row count to the previous version:
   ```r
   old <- readr::read_csv("outputs/fac_master_previous.csv")
   new <- readRDS("outputs/fac_master.rds")
   cat("Old FAC count:", nrow(old), "\n")
   cat("New FAC count:", nrow(new), "\n")
   dplyr::anti_join(new, old, by = "fac")  # Added FACs
   dplyr::anti_join(old, new, by = "fac")  # Removed FACs
   ```

4. For any removed FACs, update their `status` field in `fac_master` to `"merged_into"`
   or `"closed"` and populate `merged_into_fac` and `effective_date` where known. This
   requires a manual edit to script 01 or a small correction script.

5. Re-run scripts 04 (MOH aliases), then 06 (SACtype, to pick up any new FACs), then
   07 and 08.

**What to watch for:**
- FACs in `fac_aliases` that no longer exist in the new `fac_master` will appear as
  orphan aliases in script 07.
- New FACs with no sunshine or CIHI coverage are expected until the next year's data
  is available.

---

## Section 4 — New CIHI Data

**Scripts affected:** 05

CIHI data is curated manually via `sources/cihi_fac_overrides.csv`. There is no
automated CIHI ingestion pipeline — the override file is the source of truth.

**Steps:**

1. Obtain the new CIHI file and identify any new or changed hospital names.

2. For each new or changed CIHI name, determine the correct FAC by looking up against
   `name_universe_safe.csv` or directly in `fac_master`.

3. Add or update rows in `sources/cihi_fac_overrides.csv`. Each row requires:
   - `cihi_name_raw` — exact CIHI string
   - `fac` — mapped FAC
   - `override_reason` — brief note on why this mapping was made

4. Re-run script 05, then scripts 07 and 08.

**What to watch for:**
- CIHI sometimes changes the reported name for a hospital without changing the underlying
  institution. Check the edge cases report (`outputs/validation/edge_cases.csv`) after
  re-running script 07 to confirm Hotel Dieu and St Joseph entries are still correctly
  flagged.

---

## Section 5 — New Statistics Canada ODHF Release

**Scripts affected:** prepare_sactype_source.R (one-time prep), 06

Statistics Canada releases updated ODHF files periodically. SACtype codes for a given
municipality change rarely, but the hospital name strings in ODHF may differ from the
previous release.

**Steps:**

1. Place the new ODHF file in `sources/` and update `prepare_sactype_source.R` to point
   to it. Re-run that script to produce a new `sources/sactype_prepared.csv`.

2. Re-run script 06. The auto-match pass will attempt to match the new ODHF names against
   the current `fac_aliases` name universe. Check the console output for the pass counts
   and unmatched rows.

3. Review `outputs/sactype_unmatched.csv`. For any unmatched rows that represent real
   Ontario hospitals, add override rows to `sources/sactype_fac_overrides.csv`.

4. Re-run script 06, then scripts 07 and 08.

**What to watch for:**
- ODHF name strings sometimes differ substantially from MOH/Sunshine names, which is
  why the override file exists. A new ODHF release may require additional overrides even
  if the underlying hospitals haven't changed.
- SACtype values themselves (1–7) are stable and unlikely to change unless a municipality's
  population classification changes in a Census year.

---

## After Any Update — Final Checklist

```
[ ] script 07 reports zero FAILs
[ ] fac_master row count is as expected
[ ] facs_in_master == facs_in_aliases in script 07 summary
[ ] safe_alias_collisions == 0
[ ] orphan_alias_facs == 0
[ ] script 08 has run and outputs/derived/ files are current
[ ] changes committed to GitHub with a clear message
[ ] session summary updated if significant decisions were made
```

---

## Key File Locations Quick Reference

```
sources/sactype_fac_overrides.csv     fac as character, sactype_value as integer 1–7
sources/cihi_fac_overrides.csv        CIHI name -> FAC manual mappings
sources/FAC_Corrections.csv           Old FAC -> current FAC remapping
outputs/fac_master.rds                148 FACs, primary table
outputs/fac_aliases.rds               742 aliases across 4 source systems
outputs/derived/name_universe_safe.csv  Primary lookup table for downstream matching
outputs/validation/                   Script 07 reports (one per run date)
archive/canonical_hospitals_20260313/ Retired YAML store (do not modify)
```

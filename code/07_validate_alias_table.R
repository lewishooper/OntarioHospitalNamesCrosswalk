# 07_validate_alias_table.R
# Phase 5: QA validation of fac_master and fac_aliases flat tables
# Project: OntarioHospitalNamesCrosswalk
#
# Checks:
#   1. Every FAC in fac_master has at least one alias (any source)
#   2. Every FAC in fac_master has at least one SAFE alias
#   3. No safe alias_norm appears under more than one FAC (collision check)
#   4. No alias FAC is absent from fac_master (orphan check)
#   5. Source coverage: how many FACs have coverage per source_system
#   6. Edge case spot-check: Hotel Dieu, St Joseph, network/corporate names
#
# Report-and-continue: all failures are logged; script never stops on a check failure.
#
# Inputs:  outputs/fac_master.rds
#          outputs/fac_aliases.rds
# Outputs: outputs/validation/validation_report_YYYY-MM-DD.csv
#          outputs/validation/alias_collisions.csv
#          outputs/validation/edge_cases.csv
#          outputs/validation/source_coverage.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

source(file.path("code", "utils", "normalize.R"))

# ----------------------------
# CONFIG
# ----------------------------
FAC_MASTER_PATH <- file.path("outputs", "fac_master.rds")
ALIASES_PATH    <- file.path("outputs", "fac_aliases.rds")

today           <- format(Sys.Date(), "%Y-%m-%d")
OUT_DIR         <- file.path("outputs", "validation")
OUT_REPORT      <- file.path(OUT_DIR, paste0("validation_report_", today, ".csv"))
OUT_COLLISIONS  <- file.path(OUT_DIR, "alias_collisions.csv")
OUT_EDGE        <- file.path(OUT_DIR, "edge_cases.csv")
OUT_COVERAGE    <- file.path(OUT_DIR, "source_coverage.csv")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# LOAD
# ----------------------------
fac_master <- readRDS(FAC_MASTER_PATH)
aliases    <- readRDS(ALIASES_PATH)

# Ensure name_norm is populated (re-normalize defensively)
aliases <- aliases |>
  mutate(name_norm = if_else(
    is.na(name_norm) | name_norm == "",
    normalize_name(name_raw),
    name_norm
  ))

# ----------------------------
# CHECK 1: Every FAC has at least one alias (any source)
# ----------------------------
fac_any_alias <- aliases |>
  distinct(fac) |>
  pull(fac)

no_alias_at_all <- fac_master |>
  filter(!fac %in% fac_any_alias) |>
  select(fac, canonical_name, moh_type)

# ----------------------------
# CHECK 2: Every FAC has at least one SAFE alias
# ----------------------------
fac_safe_alias <- aliases |>
  filter(!unsafe) |>
  distinct(fac) |>
  pull(fac)

no_safe_alias <- fac_master |>
  filter(!fac %in% fac_safe_alias) |>
  select(fac, canonical_name, moh_type)

# ----------------------------
# CHECK 3: Safe alias collision — same name_norm under more than one FAC
# ----------------------------
safe_aliases <- aliases |>
  filter(!unsafe, !is.na(name_norm), name_norm != "")

alias_multi_fac <- safe_aliases |>
  distinct(fac, name_norm) |>
  group_by(name_norm) |>
  summarise(
    n_facs = n_distinct(fac),
    facs   = paste(sort(unique(fac)), collapse = "|"),
    .groups = "drop"
  ) |>
  filter(n_facs > 1)

collisions_detail <- safe_aliases |>
  semi_join(alias_multi_fac, by = "name_norm") |>
  select(name_norm, fac, source_system, name_raw, unsafe) |>
  arrange(name_norm, fac)

# ----------------------------
# CHECK 4: Orphan aliases — FAC in fac_aliases not in fac_master
# ----------------------------
orphan_aliases <- aliases |>
  filter(!fac %in% fac_master$fac) |>
  distinct(fac, source_system, name_raw) |>
  arrange(fac)

# ----------------------------
# CHECK 5: Source coverage per FAC
# ----------------------------
source_coverage <- fac_master |>
  select(fac, canonical_name) |>
  left_join(
    aliases |>
      distinct(fac, source_system) |>
      mutate(present = TRUE) |>
      pivot_wider(names_from = source_system, values_from = present,
                  values_fill = FALSE),
    by = "fac"
  ) |>
  arrange(fac)

# Summary row counts per source
coverage_summary <- aliases |>
  group_by(source_system) |>
  summarise(
    n_aliases  = n(),
    n_facs     = n_distinct(fac),
    n_safe     = sum(!unsafe),
    n_unsafe   = sum(unsafe),
    .groups = "drop"
  )

# ----------------------------
# CHECK 6: Edge case spot-check
# ----------------------------
edge <- aliases |>
  mutate(
    edge_class = case_when(
      str_detect(name_norm, "HOTEL DIEU|HOTEL-DIEU")     ~ "hotel_dieu",
      str_detect(name_norm, "ST JOSEPH|SAINT JOSEPH")    ~ "st_joseph",
      str_detect(name_norm,
        "NETWORK|ASSOCIATION|FOUNDATION|CORPORATION|GROUP|HOSPITALLERS|RELIGIEUSES"
      )                                                  ~ "network_or_corporate",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(edge_class)) |>
  select(edge_class, fac, source_system, name_raw, name_norm, unsafe) |>
  arrange(edge_class, fac, name_raw)

# ----------------------------
# WRITE OUTPUTS
# ----------------------------
readr::write_csv(collisions_detail, OUT_COLLISIONS)
readr::write_csv(edge,              OUT_EDGE)
readr::write_csv(source_coverage,   OUT_COVERAGE)

summary_tbl <- tibble::tibble(
  metric = c(
    "facs_in_master",
    "facs_in_aliases",
    "aliases_total",
    "aliases_safe",
    "aliases_unsafe",
    "facs_with_no_alias",
    "facs_with_no_safe_alias",
    "safe_alias_collisions",
    "orphan_alias_facs",
    "edge_cases_flagged"
  ),
  value = c(
    nrow(fac_master),
    n_distinct(aliases$fac),
    nrow(aliases),
    sum(!aliases$unsafe),
    sum(aliases$unsafe),
    nrow(no_alias_at_all),
    nrow(no_safe_alias),
    nrow(alias_multi_fac),
    nrow(orphan_aliases),
    nrow(edge)
  ),
  status = c(
    "info",
    "info",
    "info",
    "info",
    "info",
    if_else(nrow(no_alias_at_all)    == 0, "OK", "FAIL"),
    if_else(nrow(no_safe_alias)      == 0, "OK", "WARN"),
    if_else(nrow(alias_multi_fac)    == 0, "OK", "FAIL"),
    if_else(nrow(orphan_aliases)     == 0, "OK", "FAIL"),
    "info"
  )
)

readr::write_csv(summary_tbl, OUT_REPORT)

# ----------------------------
# CONSOLE SUMMARY
# ----------------------------
cat("\n==============================\n")
cat("SCRIPT 07: ALIAS TABLE VALIDATION\n")
cat("==============================\n")
print(summary_tbl, n = nrow(summary_tbl))

cat("\nSource coverage by system:\n")
print(coverage_summary)

if (nrow(no_alias_at_all) > 0) {
  cat("\n[FAIL] FACs with NO aliases at all:\n")
  print(no_alias_at_all)
} else {
  cat("\n[OK] All FACs have at least one alias.\n")
}

if (nrow(no_safe_alias) > 0) {
  cat("\n[WARN] FACs with no SAFE alias:\n")
  print(no_safe_alias)
} else {
  cat("\n[OK] All FACs have at least one safe alias.\n")
}

if (nrow(alias_multi_fac) > 0) {
  cat("\n[FAIL] Safe alias collisions (same name_norm under >1 FAC):\n")
  cat("See: ", OUT_COLLISIONS, "\n", sep = "")
  print(alias_multi_fac)
} else {
  cat("\n[OK] No safe alias collisions.\n")
}

if (nrow(orphan_aliases) > 0) {
  cat("\n[FAIL] Alias FACs not found in fac_master:\n")
  print(orphan_aliases)
} else {
  cat("\n[OK] No orphan aliases.\n")
}

cat("\nEdge cases exported to: ", OUT_EDGE, "\n", sep = "")
cat("Source coverage table:  ", OUT_COVERAGE, "\n", sep = "")
cat("Full report:            ", OUT_REPORT, "\n", sep = "")
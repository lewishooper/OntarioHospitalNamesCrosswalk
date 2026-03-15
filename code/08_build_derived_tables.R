# 08_build_derived_tables.R
# Phase 5: Build derived tables from fac_master and fac_aliases
# Project: OntarioHospitalNamesCrosswalk
#
# Inputs:  outputs/fac_master.rds
#          outputs/fac_aliases.rds
# Outputs: outputs/derived/
#            name_universe_safe.csv
#            fac_aliases_all.csv
#            fac_to_source_coverage.csv
#            sactype_fac_rollup.csv
#            cihi_fac_rollup.csv

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
OUT_DIR         <- file.path("outputs", "derived")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# LOAD
# ----------------------------
fac_master <- readRDS(FAC_MASTER_PATH)
aliases    <- readRDS(ALIASES_PATH)

# ----------------------------
# DERIVED TABLE 1: name_universe_safe
# Primary lookup table — safe aliases only, all sources
# ----------------------------
name_universe_safe <- aliases |>
  filter(!unsafe) |>
  select(fac, source_system, name_raw, name_norm) |>
  left_join(
    fac_master |> select(fac, canonical_name, moh_type, sactype_value),
    by = "fac"
  ) |>
  arrange(fac, source_system, name_norm)

# ----------------------------
# DERIVED TABLE 2: fac_aliases_all
# All aliases including unsafe — for audit use
# ----------------------------
fac_aliases_all <- aliases |>
  left_join(
    fac_master |> select(fac, canonical_name, moh_type),
    by = "fac"
  ) |>
  arrange(fac, source_system, name_raw)

# ----------------------------
# DERIVED TABLE 3: fac_to_source_coverage
# One row per FAC, boolean columns for each source_system
# ----------------------------
fac_to_source_coverage <- fac_master |>
  select(fac, canonical_name, moh_type, sactype_value) |>
  left_join(
    aliases |>
      distinct(fac, source_system) |>
      mutate(present = TRUE) |>
      pivot_wider(
        names_from  = source_system,
        values_from = present,
        values_fill = FALSE
      ),
    by = "fac"
  ) |>
  # Ensure all four source columns exist even if a source has no data
  mutate(
    sunshine = if ("sunshine" %in% names(pick(everything()))) sunshine else FALSE,
    moh      = if ("moh"      %in% names(pick(everything()))) moh      else FALSE,
    cihi     = if ("cihi"     %in% names(pick(everything()))) cihi     else FALSE,
    statscan = if ("statscan" %in% names(pick(everything()))) statscan else FALSE
  ) |>
  mutate(n_sources = sunshine + moh + cihi + statscan) |>
  arrange(fac)

# ----------------------------
# DERIVED TABLE 4: sactype_fac_rollup
# SACtype value per FAC with match provenance
# ----------------------------
sactype_fac_rollup <- fac_master |>
  select(fac, canonical_name, moh_type, sactype_value) |>
  left_join(
    aliases |>
      filter(source_system == "statscan") |>
      group_by(fac) |>
      summarise(
        n_statscan_aliases = n(),
        statscan_names     = paste(sort(unique(name_raw)), collapse = " || "),
        .groups = "drop"
      ),
    by = "fac"
  ) |>
  mutate(
    sactype_label = case_when(
      sactype_value == 1 ~ "CMA (large urban)",
      sactype_value == 2 ~ "CA (medium urban)",
      sactype_value == 3 ~ "Strong MIZ",
      sactype_value == 4 ~ "Moderate MIZ",
      sactype_value == 5 ~ "Weak MIZ",
      sactype_value == 6 ~ "No MIZ",
      sactype_value == 7 ~ "Remote",
      TRUE               ~ NA_character_
    ),
    sactype_source = case_when(
      !is.na(sactype_value) & !is.na(n_statscan_aliases) ~ "statscan_matched",
      !is.na(sactype_value) &  is.na(n_statscan_aliases) ~ "override_only",
      TRUE                                               ~ "unmatched"
    )
  ) |>
  arrange(fac)

# ----------------------------
# DERIVED TABLE 5: cihi_fac_rollup
# CIHI reporting names per FAC
# ----------------------------
cihi_fac_rollup <- fac_master |>
  select(fac, canonical_name, moh_type) |>
  left_join(
    aliases |>
      filter(source_system == "cihi") |>
      group_by(fac) |>
      summarise(
        n_cihi_aliases    = n(),
        n_cihi_safe       = sum(!unsafe),
        cihi_names        = paste(sort(unique(name_raw)), collapse = " | "),
        .groups = "drop"
      ),
    by = "fac"
  ) |>
  mutate(
    cihi_coverage = case_when(
      is.na(n_cihi_aliases) ~ "no_cihi",
      n_cihi_safe == 0      ~ "cihi_unsafe_only",
      TRUE                  ~ "cihi_covered"
    )
  ) |>
  arrange(fac)

# ----------------------------
# WRITE OUTPUTS
# ----------------------------
write_csv(name_universe_safe,    file.path(OUT_DIR, "name_universe_safe.csv"))
write_csv(fac_aliases_all,       file.path(OUT_DIR, "fac_aliases_all.csv"))
write_csv(fac_to_source_coverage,file.path(OUT_DIR, "fac_to_source_coverage.csv"))
write_csv(sactype_fac_rollup,    file.path(OUT_DIR, "sactype_fac_rollup.csv"))
write_csv(cihi_fac_rollup,       file.path(OUT_DIR, "cihi_fac_rollup.csv"))

# ----------------------------
# CONSOLE SUMMARY
# ----------------------------
cat("\n==============================\n")
cat("SCRIPT 08: DERIVED TABLES\n")
cat("==============================\n")
cat("Output directory:", OUT_DIR, "\n\n")

cat("name_universe_safe:     ", nrow(name_universe_safe),
    "rows (", n_distinct(name_universe_safe$fac), "FACs )\n")
cat("fac_aliases_all:        ", nrow(fac_aliases_all),
    "rows\n")
cat("fac_to_source_coverage: ", nrow(fac_to_source_coverage),
    "rows\n")
cat("sactype_fac_rollup:     ", nrow(sactype_fac_rollup),
    "rows\n")
cat("cihi_fac_rollup:        ", nrow(cihi_fac_rollup),
    "rows\n")

cat("\nSource coverage summary:\n")
fac_to_source_coverage |>
  summarise(
    sunshine = sum(sunshine),
    moh      = sum(moh),
    cihi     = sum(cihi),
    statscan = sum(statscan),
    all_four = sum(n_sources == 4),
    three    = sum(n_sources == 3),
    two      = sum(n_sources == 2),
    one      = sum(n_sources == 1)
  ) |>
  print()

cat("\nSACtype coverage:\n")
sactype_fac_rollup |>
  count(sactype_label, sort = FALSE) |>
  print(n = 20)

cat("\nCIHI coverage:\n")
cihi_fac_rollup |>
  count(cihi_coverage) |>
  print()
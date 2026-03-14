# 05_append_cihi_aliases.R
# Phase 3 (cont): Append CIHI names as aliases to fac_aliases
# Project: OntarioHospitalNamesCrosswalk
#
# Input:  outputs/phaseG_cihi_fac_map_final.csv
#         outputs/fac_master.rds
#         outputs/fac_aliases.rds
# Output: outputs/fac_aliases.rds (updated)
#         outputs/fac_aliases.csv (updated)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source(file.path("code", "utils", "normalize.R"))

# ----------------------------
# CONFIG
# ----------------------------
CIHI_MAP_PATH   <- file.path("outputs", "phaseG_cihi_fac_map_final.csv")
FAC_MASTER_PATH <- file.path("outputs", "fac_master.rds")
ALIASES_PATH    <- file.path("outputs", "fac_aliases.rds")
OUT_RDS         <- file.path("outputs", "fac_aliases.rds")
OUT_CSV         <- file.path("outputs", "fac_aliases.csv")

SOURCE_SYSTEM <- "cihi"

# ----------------------------
# LOAD
# ----------------------------
if (!file.exists(CIHI_MAP_PATH))   stop("CIHI map not found: ", CIHI_MAP_PATH)
if (!file.exists(FAC_MASTER_PATH)) stop("fac_master not found — run 01 first.")
if (!file.exists(ALIASES_PATH))    stop("fac_aliases not found — run 02/03/04 first.")

fac_master <- readRDS(FAC_MASTER_PATH)
aliases    <- readRDS(ALIASES_PATH)

cihi_map <- readr::read_csv(CIHI_MAP_PATH, show_col_types = FALSE) |>
  standardize_colnames()

# ----------------------------
# BUILD CIHI ALIAS ROWS
# ----------------------------
# cihi_map has: fac, cihi_name_raw, cihi_type, match_status, etc.
cihi_aliases <- cihi_map |>
  filter(!is.na(fac), !is.na(cihi_name_raw)) |>
  transmute(
    fac                  = as.character(fac),
    source_system        = SOURCE_SYSTEM,
    name_raw             = as.character(cihi_name_raw),
    name_norm            = normalize_name(cihi_name_raw),
    unsafe               = FALSE,   # CIHI names are curated
    unsafe_reasons       = NA_character_,
    year_observed        = NA_integer_,
    fac_before_correction = NA_character_
  ) |>
  filter(!is.na(name_norm)) |>
  distinct(fac, name_raw, .keep_all = TRUE)

# ----------------------------
# QA: CIHI FACs not in master
# ----------------------------
not_in_master <- setdiff(cihi_aliases$fac, fac_master$fac)

# ----------------------------
# APPEND (idempotent — remove existing CIHI aliases first)
# ----------------------------
existing_cihi <- aliases |> filter(source_system == "cihi")

if (nrow(existing_cihi) > 0) {
  cat("[INFO] Removing", nrow(existing_cihi),
      "existing CIHI aliases before re-appending.\n")
  aliases <- aliases |> filter(source_system != "cihi")
}

aliases_updated <- bind_rows(aliases, cihi_aliases) |>
  arrange(fac, source_system, name_raw)

# ----------------------------
# VALIDATION
# ----------------------------
facs_with_cihi <- n_distinct(cihi_aliases$fac)
facs_in_master <- nrow(fac_master)

cat("\n==============================\n")
cat("PHASE 5: CIHI ALIAS APPEND\n")
cat("==============================\n")
cat("CIHI map rows loaded:     ", nrow(cihi_map), "\n")
cat("CIHI aliases built:       ", nrow(cihi_aliases), "\n")
cat("FACs with CIHI coverage:  ", facs_with_cihi, "of", facs_in_master, "\n")
cat("CIHI FACs not in master:  ", length(not_in_master), "\n")

if (length(not_in_master) > 0) {
  cat("\n[WARN] CIHI FACs not in fac_master:\n")
  print(not_in_master)
}

cat("\nTotal aliases now:        ", nrow(aliases_updated), "\n")
cat("\nSource breakdown:\n")
print(table(aliases_updated$source_system))

# Migration baseline check
BASELINE_TOTAL <- 363L
if (nrow(aliases_updated) < BASELINE_TOTAL) {
  cat("\n[WARN] Total alias count", nrow(aliases_updated),
      "is below migration baseline of", BASELINE_TOTAL,
      "— investigate.\n")
} else {
  cat("\n[OK] Total alias count", nrow(aliases_updated),
      "at or above migration baseline (", BASELINE_TOTAL, ").\n")
}

# ----------------------------
# WRITE
# ----------------------------
saveRDS(aliases_updated, OUT_RDS)
readr::write_csv(aliases_updated, OUT_CSV)

cat("\nWrote:\n")
cat(" - ", OUT_RDS, "\n", sep = "")
cat(" - ", OUT_CSV, "\n", sep = "")
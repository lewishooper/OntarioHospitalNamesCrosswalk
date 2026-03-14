# 02_build_sunshine_aliases.R
# Phase 2: Build sunshine aliases from 2024 salary feed
# Project: OntarioHospitalNamesCrosswalk
#
# Input:  outputs/salary_alias_feed_2024.csv
#         sources/FAC_Corrections.csv (optional)
#         outputs/fac_master.rds
# Output: outputs/fac_aliases.rds
#         outputs/fac_aliases.csv
#
# NOTE: This script creates fac_aliases fresh. Run before 03_detect_unsafe_aliases.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source(file.path("code", "utils", "normalize.R"))
source(file.path("code", "apply_fac_corrections.R"))

# ----------------------------
# CONFIG
# ----------------------------
ALIAS_FEED_PATH <- file.path("outputs", "salary_alias_feed_2024.csv")
FAC_MASTER_PATH <- file.path("outputs", "fac_master.rds")
OUT_RDS         <- file.path("outputs", "fac_aliases.rds")
OUT_CSV         <- file.path("outputs", "fac_aliases.csv")

YEAR_OBSERVED   <- 2024L
SOURCE_SYSTEM   <- "sunshine"

# ----------------------------
# LOAD
# ----------------------------
if (!file.exists(ALIAS_FEED_PATH)) stop("Alias feed not found: ", ALIAS_FEED_PATH)
if (!file.exists(FAC_MASTER_PATH)) stop("fac_master not found — run 01 first.")

fac_master  <- readRDS(FAC_MASTER_PATH)
alias_feed  <- readr::read_csv(ALIAS_FEED_PATH, show_col_types = FALSE) |>
  standardize_colnames()

# ----------------------------
# CLEAN + APPLY FAC CORRECTIONS
# ----------------------------
alias_clean <- alias_feed |>
  transmute(
    fac      = clean_fac(fac),
    name_raw = clean_text(name_raw)
  ) |>
  filter(!is.na(fac), !is.na(name_raw)) |>
  distinct(fac, name_raw)

alias_clean <- apply_fac_corrections(alias_clean, fac_col = "fac")

# ----------------------------
# BUILD ALIAS ROWS
# ----------------------------
sunshine_aliases <- alias_clean |>
  transmute(
    fac                  = fac,
    source_system        = SOURCE_SYSTEM,
    name_raw             = name_raw,
    name_norm            = normalize_name(name_raw),
    unsafe               = FALSE,
    unsafe_reasons       = NA_character_,
    year_observed        = YEAR_OBSERVED,
    fac_before_correction = if ("fac_before_correction" %in% names(alias_clean))
      fac_before_correction
    else NA_character_
  ) |>
  filter(!is.na(name_norm)) |>
  distinct(fac, name_raw, .keep_all = TRUE) |>
  arrange(fac, name_raw)

# ----------------------------
# VALIDATION
# ----------------------------
facs_in_master   <- fac_master$fac
facs_in_sunshine <- unique(sunshine_aliases$fac)

not_in_master <- setdiff(facs_in_sunshine, facs_in_master)
not_in_sunshine <- setdiff(facs_in_master, facs_in_sunshine)

cat("\n==============================\n")
cat("PHASE 2: SUNSHINE ALIASES\n")
cat("==============================\n")
cat("Alias rows:              ", nrow(sunshine_aliases), "\n")
cat("Distinct FACs in aliases:", n_distinct(sunshine_aliases$fac), "\n")
cat("FACs in master:          ", length(facs_in_master), "\n")
cat("Sunshine FACs not in master:", length(not_in_master), "\n")
cat("Master FACs not in sunshine:", length(not_in_sunshine), "\n")

if (length(not_in_master) > 0) {
  cat("\n[WARN] Sunshine FACs not in fac_master (should be zero after corrections):\n")
  print(not_in_master)
}

cat("\nAliases per FAC:\n")
aliases_per_fac <- sunshine_aliases |> count(fac, name = "n_aliases")
cat(" Min:    ", min(aliases_per_fac$n_aliases), "\n")
cat(" Median: ", median(aliases_per_fac$n_aliases), "\n")
cat(" Max:    ", max(aliases_per_fac$n_aliases), "\n")

# Migration baseline check
BASELINE_SUNSHINE <- 246L
if (nrow(sunshine_aliases) != BASELINE_SUNSHINE) {
  cat("\n[WARN] Sunshine alias count", nrow(sunshine_aliases),
      "differs from migration baseline of", BASELINE_SUNSHINE,
      "— investigate if gap is large.\n")
} else {
  cat("\n[OK] Sunshine alias count matches migration baseline (", BASELINE_SUNSHINE, ").\n")
}

# ----------------------------
# WRITE
# ----------------------------
saveRDS(sunshine_aliases, OUT_RDS)
readr::write_csv(sunshine_aliases, OUT_CSV)

cat("\nWrote:\n")
cat(" - ", OUT_RDS, "\n", sep = "")
cat(" - ", OUT_CSV, "\n", sep = "")
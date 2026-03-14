# 04_append_moh_aliases.R
# Phase 3 (cont): Append MOH canonical names as aliases to fac_aliases
# Project: OntarioHospitalNamesCrosswalk
#
# Input:  outputs/fac_master.rds
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
FAC_MASTER_PATH <- file.path("outputs", "fac_master.rds")
ALIASES_PATH    <- file.path("outputs", "fac_aliases.rds")
OUT_RDS         <- file.path("outputs", "fac_aliases.rds")
OUT_CSV         <- file.path("outputs", "fac_aliases.csv")

SOURCE_SYSTEM <- "moh"

# ----------------------------
# LOAD
# ----------------------------
if (!file.exists(FAC_MASTER_PATH)) stop("fac_master not found — run 01 first.")
if (!file.exists(ALIASES_PATH))    stop("fac_aliases not found — run 02/03 first.")

fac_master <- readRDS(FAC_MASTER_PATH)
aliases    <- readRDS(ALIASES_PATH)

# ----------------------------
# BUILD MOH ALIAS ROWS
# ----------------------------
moh_aliases <- fac_master |>
  filter(!is.na(canonical_name)) |>
  transmute(
    fac                  = fac,
    source_system        = SOURCE_SYSTEM,
    name_raw             = canonical_name,
    name_norm            = normalize_name(canonical_name),
    unsafe               = FALSE,
    unsafe_reasons       = NA_character_,
    year_observed        = NA_integer_,
    fac_before_correction = NA_character_
  ) |>
  filter(!is.na(name_norm))

# ----------------------------
# APPEND (skip any already present)
# ----------------------------
existing_moh <- aliases |> filter(source_system == "moh")

if (nrow(existing_moh) > 0) {
  cat("[INFO] Removing", nrow(existing_moh),
      "existing MOH aliases before re-appending.\n")
  aliases <- aliases |> filter(source_system != "moh")
}

aliases_updated <- bind_rows(aliases, moh_aliases) |>
  arrange(fac, source_system, name_raw)

# ----------------------------
# VALIDATION
# ----------------------------
n_moh_added    <- nrow(moh_aliases)
n_no_canonical <- sum(is.na(fac_master$canonical_name))

cat("\n==============================\n")
cat("PHASE 4: MOH ALIAS APPEND\n")
cat("==============================\n")
cat("MOH aliases added:        ", n_moh_added, "\n")
cat("FACs with no canonical:   ", n_no_canonical, "\n")
cat("Total aliases now:        ", nrow(aliases_updated), "\n")
cat("\nSource breakdown:\n")
print(table(aliases_updated$source_system))

# ----------------------------
# WRITE
# ----------------------------
saveRDS(aliases_updated, OUT_RDS)
readr::write_csv(aliases_updated, OUT_CSV)

cat("\nWrote:\n")
cat(" - ", OUT_RDS, "\n", sep = "")
cat(" - ", OUT_CSV, "\n", sep = "")
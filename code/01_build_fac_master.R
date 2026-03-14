# 01_build_fac_master.R
# Phase 1: Build fac_master from MOH FAC source
# Project: OntarioHospitalNamesCrosswalk
#
# Input:  sources/MOHFAC.csv
# Output: outputs/fac_master.rds
#         outputs/fac_master.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source(file.path("code", "utils", "normalize.R"))

# ----------------------------
# CONFIG
# ----------------------------
MOHFAC_PATH <- file.path("sources", "MOHFAC.csv")
OUT_RDS     <- file.path("outputs", "fac_master.rds")
OUT_CSV     <- file.path("outputs", "fac_master.csv")

# ----------------------------
# LOAD + CLEAN
# ----------------------------
if (!file.exists(MOHFAC_PATH)) stop("MOHFAC not found: ", MOHFAC_PATH)

mohfac <- readr::read_csv(MOHFAC_PATH, show_col_types = FALSE) |>
  standardize_colnames()

# Expecting: fac, mohname, type (as produced by importMOHFAC.R)
required <- c("fac", "mohname", "type")
missing  <- setdiff(required, names(mohfac))
if (length(missing) > 0) {
  stop("MOHFAC.csv missing required columns: ", paste(missing, collapse = ", "),
       "\nAvailable: ", paste(names(mohfac), collapse = ", "))
}

fac_master <- mohfac |>
  transmute(
    fac            = clean_fac(fac),
    canonical_name = clean_text(mohname),
    moh_type       = clean_text(type),
    sactype_value  = NA_character_,   # populated later by script 06
    status         = "active",
    merged_into_fac = NA_character_,
    effective_date  = NA_character_
  ) |>
  filter(!is.na(fac), fac != "") |>
  distinct(fac, .keep_all = TRUE) |>
  arrange(fac)

# ----------------------------
# VALIDATION
# ----------------------------
n_fac        <- nrow(fac_master)
n_distinct   <- n_distinct(fac_master$fac)
n_dup        <- n_fac - n_distinct
n_no_name    <- sum(is.na(fac_master$canonical_name))
n_no_type    <- sum(is.na(fac_master$moh_type))

cat("\n==============================\n")
cat("PHASE 1: FAC MASTER BUILD\n")
cat("==============================\n")
cat("Input rows:              ", nrow(mohfac), "\n")
cat("Output rows (fac_master):", n_fac, "\n")
cat("Distinct FACs:           ", n_distinct, "\n")
cat("Duplicate FACs removed:  ", n_dup, "\n")
cat("Missing canonical_name:  ", n_no_name, "\n")
cat("Missing moh_type:        ", n_no_type, "\n")

# Migration baseline check
BASELINE_FACS <- 148L
if (n_fac != BASELINE_FACS) {
  cat("\n[WARN] FAC count", n_fac, "does not match migration baseline of",
      BASELINE_FACS, "— investigate before proceeding.\n")
} else {
  cat("\n[OK] FAC count matches migration baseline (", BASELINE_FACS, ").\n")
}

if (n_dup > 0) {
  cat("\n[WARN] Duplicate FACs found after dedup — review MOHFAC source.\n")
}
if (n_no_name > 0) {
  cat("\n[WARN] FACs with no canonical_name:\n")
  print(fac_master |> filter(is.na(canonical_name)) |> select(fac, moh_type))
}

# ----------------------------
# WRITE
# ----------------------------
dir.create("outputs", showWarnings = FALSE)
saveRDS(fac_master, OUT_RDS)
readr::write_csv(fac_master, OUT_CSV)

cat("\nWrote:\n")
cat(" - ", OUT_RDS, "\n", sep = "")
cat(" - ", OUT_CSV, "\n", sep = "")
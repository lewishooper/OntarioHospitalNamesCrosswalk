# prepare_sactype_source.R
# One-time preparation of SACType source data from Statistics Canada ODHF
# Project: OntarioHospitalNamesCrosswalk
#
# Inputs:  ODHF v1.1 CSV
#          2021 CSD SACtype CSV
# Output:  sources/sactype_prepared.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source(file.path("code", "utils", "normalize.R"))

# ----------------------------
# CONFIG
# ----------------------------
ODHF_PATH <- "E:/Public/HospitalNames/source/ODHF_v1.1/ODHF_v1.1/odhf_v1.1.csv"
CSD_PATH  <- "E:/Public/HospitalNames/source/2021_92-150-X_eng/csd.csv"
OUT_CSV   <- file.path("sources", "sactype_prepared.csv")

# Remove the sactype_label function entirely, and in the transmute block



# ----------------------------
# LOAD
# ----------------------------
odhf <- read.csv(ODHF_PATH, fileEncoding = "windows-1252")

csd <- read.csv(CSD_PATH, fileEncoding = "ISO-8859-1") |>
  select(CSDuid, SACtype) |>
  distinct(CSDuid, .keep_all = TRUE)

# ----------------------------
# FILTER + JOIN
# ----------------------------
ont_hosp <- odhf |>
  filter(province == "on", odhf_facility_type == "Hospitals") |>
  left_join(csd, by = "CSDuid") |>
  transmute(
    facility_name    = clean_text(facility_name),
    facility_name_norm = normalize_name(facility_name),
    city             = clean_text(city),
    sactype_value    = as.integer(SACtype),
    CSDuid           = CSDuid,
    CSDname          = clean_text(CSDname)
  ) |>
  filter(!is.na(facility_name)) |>
  arrange(facility_name)

# ----------------------------
# SUMMARY
# ----------------------------
cat("\n==============================\n")
cat("SACTYPE SOURCE PREPARATION\n")
cat("==============================\n")
cat("Total Ontario hospital rows:  ", nrow(ont_hosp), "\n")
cat("With SACtype assigned:        ",
    sum(!is.na(ont_hosp$sactype_value)), "\n")
cat("Without SACtype (no CSDuid):  ",
    sum(is.na(ont_hosp$sactype_value)), "\n")
cat("\nSACtype distribution:\n")
print(table(ont_hosp$sactype_value, useNA = "ifany"))
cat("\nSACtype code distribution:\n")
print(table(ont_hosp$sactype_code, useNA = "ifany"))

# ----------------------------
# WRITE
# ----------------------------
readr::write_csv(ont_hosp, OUT_CSV)
cat("\nWrote: ", OUT_CSV, "\n", sep = "")
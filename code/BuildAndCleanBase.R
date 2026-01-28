# Phase B: Build Salary Alias Feed (2024) with Cleanup + Validation
# Project: OntarioHospitalNamesCrosswalk
# Output: data/derived/salary_alias_feed_2024.csv (+ .rds optional)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

# ----------------------------
# CONFIG
# ----------------------------
INPUT_PATH  <- file.path("E:/Public", "SalaryHospitalManualMatchNewAndFinal2.csv")  # <- adjust if needed
OUTPUT_CSV  <- file.path("E:/Public/OntarioHospitalNamesCrosswalk/outputs", "salary_alias_feed_2024.csv")
OUTPUT_RDS  <- file.path("E:/Public/OntarioHospitalNamesCrosswalk/outputs", "salary_alias_feed_2024.rds")

SOURCE_SYSTEM <- "sunshine"
YEAR_OBSERVED <- 2024

# ----------------------------
# HELPERS
# ----------------------------

# Normalize column names to a consistent scheme
standardize_colnames <- function(df) {
  names(df) <- names(df) |>
    str_trim() |>
    str_replace_all("\\s+", "_") |>
    str_replace_all("[^A-Za-z0-9_]", "") |>
    tolower()
  df
}

# Clean strings but preserve raw-ish content (no aggressive rewriting)
clean_name_raw <- function(x) {
  x <- enc2utf8(x)                         # ensure UTF-8 encoding
  x <- str_replace_all(x, "\u00A0", " ")    # NBSP to space
  x <- str_squish(x)                       # trim + collapse whitespace
  x
}

# FAC cleanup: keep digits; allow leading zeros if any (store as character)
clean_fac <- function(x) {
  x <- enc2utf8(x)
  x <- str_squish(x)
  x <- str_replace_all(x, "[^0-9]", "")     # keep digits only
  x <- na_if(x, "")
  x
}

# Validation summary printer
print_validation_summary <- function(df_raw, df_clean, issues) {
  cat("\n==============================\n")
  cat("PHASE B VALIDATION SUMMARY\n")
  cat("==============================\n")
  
  cat("\nInput rows:", nrow(df_raw), "\n")
  cat("Rows after cleanup/filter:", nrow(df_clean), "\n")
  cat("Distinct FACs:", n_distinct(df_clean$fac), "\n")
  cat("Distinct names (post-clean):", n_distinct(df_clean$name_raw), "\n")
  cat("Distinct (FAC, name_raw):", nrow(df_clean), "\n")
  
  cat("\n-- Issues detected --\n")
  cat("Missing/blank FAC:", issues$missing_fac, "\n")
  cat("Missing/blank SalaryHospital:", issues$missing_name, "\n")
  cat("Non-numeric FAC (after cleaning):", issues$non_numeric_fac, "\n")
  cat("Names reduced to empty after trimming:", issues$emptied_names, "\n")
  
  cat("\n-- UTF-8 check (heuristic) --\n")
  # enc2utf8 does not guarantee "correctness", but ensures internal representation
  # We’ll just confirm we applied it and the column is character.
  cat("name_raw is character:", is.character(df_clean$name_raw), "\n")
  cat("fac is character:", is.character(df_clean$fac), "\n")
}

# ----------------------------
# LOAD
# ----------------------------
stopifnot(file.exists(INPUT_PATH))

df0 <- readr::read_csv(INPUT_PATH, show_col_types = FALSE) %>%
  as.data.frame()%>%
  filter(FAC>=592) %>% # drops some irrelelvant data used elsewhere
  select(FAC,SalaryHospital)

df0 <- standardize_colnames(df0)

# Find required columns (case-insensitive via standardized names)
# Expect: fac, salaryhospital (or close)
if (!("fac" %in% names(df0))) {
  stop("Required column 'FAC' not found (after standardizing column names).")
}
if (!("salaryhospital" %in% names(df0))) {
  stop("Required column 'SalaryHospital' not found (after standardizing column names).")
}

df_raw <- df0

# ----------------------------
# CLEANUP + VALIDATION COUNTS
# ----------------------------
# Pre-clean issue counts
issues <- list(
  missing_fac = sum(is.na(df_raw$fac) | str_squish(as.character(df_raw$fac)) == ""),
  missing_name = sum(is.na(df_raw$salaryhospital) | str_squish(as.character(df_raw$salaryhospital)) == ""),
  non_numeric_fac = NA_integer_,
  emptied_names = NA_integer_
)

df_clean <- df_raw %>%
  transmute(
    fac = clean_fac(as.character(fac)),
    name_raw = clean_name_raw(as.character(salaryhospital))
  )

# Post-clean issue counts
issues$non_numeric_fac <- sum(!is.na(df_raw$fac) & clean_fac(as.character(df_raw$fac)) %in% c(NA, ""))
issues$emptied_names <- sum(!is.na(df_raw$salaryhospital) & clean_name_raw(as.character(df_raw$salaryhospital)) %in% c(NA, ""))

# Filter out missing
df_clean <- df_clean %>%
  filter(!is.na(fac), fac != "", !is.na(name_raw), name_raw != "")

# De-duplicate (FAC, name_raw)
alias_feed <- df_clean %>%
  distinct(fac, name_raw) %>%
  mutate(
    source_system = SOURCE_SYSTEM,
    year_observed = YEAR_OBSERVED,
    .before = 1
  ) %>%
  arrange(fac, name_raw)

print_validation_summary(df_raw = df_raw, df_clean = alias_feed, issues = issues)

# ----------------------------
# OUTPUT
# ----------------------------
dir.create(dirname(OUTPUT_CSV), recursive = TRUE, showWarnings = FALSE)

readr::write_csv(alias_feed, OUTPUT_CSV)
saveRDS(alias_feed, OUTPUT_RDS)

cat("\nWrote:\n - ", OUTPUT_CSV, "\n - ", OUTPUT_RDS, "\n", sep = "")

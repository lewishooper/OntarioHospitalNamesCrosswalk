# Phase F: MOH enrichment of Salary Alias Feed (2024) + Audit Reports
# Project: OntarioHospitalNamesCrosswalk

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tools)
})

# ----------------------------
# CONFIG
# ----------------------------
# Inputs
ALIAS_FEED_PATH <- file.path("outputs", "salary_alias_feed_2024.csv")
# Provide MOHFAC as CSV. If you have xlsx, convert to CSV or tell me and we’ll swap readr->readxl.
MOHFAC_PATH     <- file.path("sources", "MOHFAC.csv")

# outputs
OUT_ENRICHED    <- file.path("outputs", "salary_alias_feed_2024_moh_enriched.csv")
OUT_AUDIT       <- file.path("outputs", "audit_moh_enrichment_2024.csv")
OUT_MOH_CLEAN   <- file.path("outputs", "mohfac_cleaned.csv")  # optional helpful artifact

# Column expectations (after standardization)
# MOHFAC must contain FAC, MOHName, Type
EXPECTED_MOH_COLS <- c("fac", "mohname", "type")

# ----------------------------
# HELPERS
# ----------------------------
standardize_colnames <- function(df) {
  names(df) <- names(df) |>
    str_trim() |>
    str_replace_all("\\s+", "_") |>
    str_replace_all("[^A-Za-z0-9_]", "") |>
    tolower()
  df
}

clean_fac <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- str_squish(x)
  x <- str_replace_all(x, "[^0-9]", "")
  x <- na_if(x, "")
  x
}

clean_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_squish(x)
  x <- na_if(x, "")
  x
}

stop_with_column_hint <- function(df, needed_cols, context = "MOHFAC") {
  missing <- setdiff(needed_cols, names(df))
  if (length(missing) > 0) {
    stop(
      paste0(
        context, " is missing required columns after standardization: ",
        paste(missing, collapse = ", "),
        "\nAvailable columns: ",
        paste(names(df), collapse = ", "),
        "\nIf your source uses different headers, rename them to: FAC, MOHName, Type"
      )
    )
  }
}

write_audit <- function(audit_tbl) {
  dir.create("outputs", showWarnings = FALSE)
  readr::write_csv(audit_tbl, OUT_AUDIT)
}

# ----------------------------
# LOAD INPUTS
# ----------------------------
dir.create("outputs", showWarnings = FALSE)

if (!file.exists(ALIAS_FEED_PATH)) {
  stop("Alias feed not found. Run Phase B first: ", ALIAS_FEED_PATH)
}
if (!file.exists(MOHFAC_PATH)) {
  stop("MOHFAC not found at: ", MOHFAC_PATH, "\nExpected a CSV named MOHFAC.csv in sources/.")
}

alias_feed <- readr::read_csv(ALIAS_FEED_PATH, show_col_types = FALSE) |>
  as.data.frame() |>
  standardize_colnames()

mohfac_raw <- readr::read_csv(MOHFAC_PATH, show_col_types = FALSE) |>
  as.data.frame() |>
  standardize_colnames()

# Validate alias feed required columns
if (!all(c("fac", "name_raw") %in% names(alias_feed))) {
  stop(
    "Alias feed must contain columns: fac, name_raw.\n",
    "Available columns: ", paste(names(alias_feed), collapse = ", ")
  )
}

# Validate MOHFAC required columns
stop_with_column_hint(mohfac_raw, EXPECTED_MOH_COLS, context = "MOHFAC")

# ----------------------------
# CLEANUP
# ----------------------------
#source(file.path("code", "apply_fac_corrections.R"))
#alias_feed_clean <- apply_fac_corrections(alias_feed_clean, fac_col = "fac")
alias_feed_clean <- alias_feed |>
  transmute(
    source_system = if ("source_system" %in% names(alias_feed)) source_system else "sunshine",
    year_observed  = if ("year_observed" %in% names(alias_feed)) year_observed else NA_integer_,
    fac            = clean_fac(fac),
    name_raw       = clean_text(name_raw)
  ) |>
  filter(!is.na(fac), !is.na(name_raw)) |>
  distinct(source_system, year_observed, fac, name_raw)
source(file.path("code", "apply_fac_corrections.R"))
alias_feed_clean <- apply_fac_corrections(alias_feed_clean, fac_col = "fac")
mohfac_clean <- mohfac_raw |>
  transmute(
    fac      = clean_fac(fac),
    moh_name = clean_text(mohname),
    moh_type = clean_text(type)
  )

# Basic missing checks
alias_missing_fac  <- sum(is.na(alias_feed_clean$fac))
alias_missing_name <- sum(is.na(alias_feed_clean$name_raw))

moh_missing_fac    <- sum(is.na(mohfac_clean$fac))
moh_missing_name   <- sum(is.na(mohfac_clean$moh_name))
moh_missing_type   <- sum(is.na(mohfac_clean$moh_type))

# Duplicate FAC check in MOHFAC
moh_dups <- mohfac_clean |>
  filter(!is.na(fac)) |>
  count(fac, name = "n") |>
  filter(n > 1)

# If duplicates exist, keep them for audit but don’t silently choose one.
# We will still attempt enrichment by reducing duplicates ONLY if identical rows.
mohfac_dedup <- mohfac_clean |>
  distinct(fac, moh_name, moh_type)

moh_dups_after_dedup <- mohfac_dedup |>
  count(fac, name = "n") |>
  filter(n > 1)

# ----------------------------
# ENRICH (LEFT JOIN from alias feed to MOHFAC)
# ----------------------------
enriched <- alias_feed_clean |>
  left_join(mohfac_dedup, by = "fac") |>
  arrange(fac, source_system, name_raw)

# Write cleaned MOHFAC for transparency
readr::write_csv(mohfac_dedup, OUT_MOH_CLEAN)

# ----------------------------
# AUDIT REPORTS
# ----------------------------

# Coverage: salary FACs missing in MOHFAC
salary_facs <- enriched |>
  distinct(fac)

moh_facs <- mohfac_dedup |>
  filter(!is.na(fac)) |>
  distinct(fac)

missing_in_moh <- salary_facs |>
  anti_join(moh_facs, by = "fac") |>
  mutate(issue = "salary_fac_missing_in_mohfac")

# MOHFAC FACs not in salary feed (not an error; useful completeness info)
moh_not_in_salary <- moh_facs |>
  anti_join(salary_facs, by = "fac") |>
  mutate(issue = "mohfac_fac_not_in_salary_alias_feed")

# Enriched rows missing moh_name
enriched_missing_moh_name <- enriched |>
  filter(is.na(moh_name)) |>
  distinct(fac) |>
  mutate(issue = "moh_name_missing_after_join")

# Enriched rows missing moh_type (note: could be legit if Type missing)
enriched_missing_moh_type <- enriched |>
  filter(is.na(moh_type)) |>
  distinct(fac) |>
  mutate(issue = "moh_type_missing_after_join")

# MOHFAC duplicates
dup_report_raw <- moh_dups |>
  mutate(issue = "mohfac_duplicate_fac_raw") |>
  select(issue, fac, n)

dup_report_dedup <- moh_dups_after_dedup |>
  mutate(issue = "mohfac_duplicate_fac_after_dedup") |>
  select(issue, fac, n)

audit_tbl <- bind_rows(
  missing_in_moh |> mutate(n = NA_integer_) |> select(issue, fac, n),
  moh_not_in_salary |> mutate(n = NA_integer_) |> select(issue, fac, n),
  enriched_missing_moh_name |> mutate(n = NA_integer_) |> select(issue, fac, n),
  enriched_missing_moh_type |> mutate(n = NA_integer_) |> select(issue, fac, n),
  dup_report_raw,
  dup_report_dedup
) |>
  arrange(issue, fac)

write_audit(audit_tbl)

# Write enriched feed
readr::write_csv(enriched, OUT_ENRICHED)

# ----------------------------
# CONSOLE SUMMARY
# ----------------------------
cat("\n==============================\n")
cat("PHASE F: MOH ENRICHMENT SUMMARY\n")
cat("==============================\n")

cat("\nAlias feed rows (clean, distinct):", nrow(alias_feed_clean), "\n")
cat("Distinct salary FACs:", n_distinct(alias_feed_clean$fac), "\n")

cat("\nMOHFAC rows (clean, distinct FAC-name-type):", nrow(mohfac_dedup), "\n")
cat("Distinct MOHFAC FACs:", n_distinct(mohfac_dedup$fac), "\n")

cat("\nMissing values (post-clean):\n")
cat(" - Alias feed missing FAC:", alias_missing_fac, "\n")
cat(" - Alias feed missing name_raw:", alias_missing_name, "\n")
cat(" - MOHFAC missing FAC:", moh_missing_fac, "\n")
cat(" - MOHFAC missing MOHName:", moh_missing_name, "\n")
cat(" - MOHFAC missing Type:", moh_missing_type, "\n")

cat("\nJoin coverage:\n")
cat(" - Salary FACs missing in MOHFAC:", nrow(missing_in_moh), "\n")
cat(" - MOHFAC FACs not in salary alias feed:", nrow(moh_not_in_salary), "\n")
cat(" - FACs with missing moh_name after join:", nrow(enriched_missing_moh_name), "\n")
cat(" - FACs with missing moh_type after join:", nrow(enriched_missing_moh_type), "\n")

cat("\nMOHFAC duplicate FACs:\n")
cat(" - Raw duplicates:", nrow(moh_dups), "\n")
cat(" - Duplicates after de-dup:", nrow(moh_dups_after_dedup), "\n")

cat("\nWrote outputs:\n")
cat(" - Enriched alias feed: ", OUT_ENRICHED, "\n", sep = "")
cat(" - Audit report:       ", OUT_AUDIT, "\n", sep = "")
cat(" - Cleaned MOHFAC:      ", OUT_MOH_CLEAN, "\n", sep = "")


### Correctons Block
changed <- alias_feed_clean %>%
  filter(!is.na(fac_before_correction), fac_before_correction != fac) %>%
  distinct(fac_before_correction, fac) %>%
  arrange(fac_before_correction)

if (nrow(changed) > 0) {
  write_csv(changed, file.path("Outputs", "fac_corrections_applied_2024.csv"))
  cat("\nFAC corrections applied (see Outputs/fac_corrections_applied_2024.csv): ", nrow(changed), "\n")
} else {
  cat("\nNo FAC corrections applied.\n")
}


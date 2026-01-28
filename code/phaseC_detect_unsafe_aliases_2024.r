# Phase C: Detect Unsafe Aliases (fully automated)
# Project: OntarioHospitalNamesCrosswalk
#
# Input:  outputs/salary_alias_feed_2024_moh_enriched.csv
# Output: outputs/salary_alias_feed_2024_moh_enriched_unsafe.csv
#         outputs/unsafe_aliases_2024.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# ----------------------------
# CONFIG
# ----------------------------
IN_PATH  <- file.path("outputs", "salary_alias_feed_2024_moh_enriched.csv")

OUT_ALL  <- file.path("outputs", "salary_alias_feed_2024_moh_enriched_unsafe.csv")
OUT_BAD  <- file.path("outputs", "unsafe_aliases_2024.csv")

# Pattern lists (tune later; intentionally conservative)
PAT_CORPORATE <- c(
  "HEALTH NETWORK",
  "HEALTHCARE GROUP",
  "HEALTHCARE NETWORK",
  "NETWORK",
  "CORPORATION",
  "INCORPORATED",
  "FOUNDATION",
  "SOCIETY",
  "ASSOCIATION",
  "RELIGIOUS HOSPITALLERS",
  "HOSPITALLERS",
  "RELIGIEUSES",
  "RELIGIEUSES HOSPITALIERES",
  "CATHOLIC",
  "DIOCESE",
  "ORDER OF"
)

PAT_RELIGIOUS_GENERIC <- c(
  "HOTEL DIEU",
  "ST JOSEPH",
  "SAINT JOSEPH",
  "ST MARY",
  "SAINT MARY",
  "ST MICHAEL",
  "SAINT MICHAEL"
)

GENERIC_ONLY_NAMES <- c(
  "HOSPITAL",
  "GENERAL HOSPITAL",
  "MEMORIAL HOSPITAL",
  "DISTRICT HOSPITAL",
  "REGIONAL HEALTH CENTRE",
  "REGIONAL HEALTH CENTER",
  "HEALTH CENTRE",
  "HEALTH CENTER",
  "HOSPITAL CENTRE",
  "HOSPITAL CENTER"
)

PAT_GENERIC_FACILITY <- c(
  "GENERAL HOSPITAL",
  "MEMORIAL HOSPITAL"
)

# Minimum “distinctive token” count heuristic (after removing generic tokens)
MIN_DISTINCT_TOKENS <- 2

# ----------------------------
# HELPERS
# ----------------------------
normalize_name <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- str_replace_all(x, "\u00A0", " ")
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- str_squish(x)
  x <- na_if(x, "")
  x
}

strip_generic_tokens <- function(x) {
  # Remove tokens that add little identity; keep geography/unique words
  x <- str_replace_all(
    x,
    "\\b(HOSPITAL|HOPITAL|CENTRE|CENTER|HEALTH|SANTE|REGIONAL|REGIONALE|DISTRICT|CLINIC|CLINIQUE|THE)\\b",
    " "
  )
  x <- str_squish(x)
  x
}

count_tokens <- function(x) {
  if (is.na(x) || x == "") return(0L)
  length(str_split(x, "\\s+")[[1]])
}

# Vectorized: does each element contain ANY of the patterns?
has_any_pattern_vec <- function(x, patterns) {
  x <- ifelse(is.na(x), "", x)
  if (length(patterns) == 0) return(rep(FALSE, length(x)))
  re <- paste0("(", paste(patterns, collapse = "|"), ")")
  str_detect(x, re)
}

# ----------------------------
# LOAD
# ----------------------------
if (!file.exists(IN_PATH)) {
  stop("Input not found: ", IN_PATH, "\nRun Phase F first.")
}

df <- readr::read_csv(IN_PATH, show_col_types = FALSE)

if (!all(c("fac", "name_raw") %in% names(df))) {
  stop("Expected columns not found in input. Need at least: fac, name_raw")
}

# ----------------------------
# NORMALIZE + BASIC FEATURES
# ----------------------------
df2 <- df %>%
  mutate(
    fac = as.character(fac),
    name_raw = as.character(name_raw),
    name_norm = normalize_name(name_raw),
    name_norm_stripped = strip_generic_tokens(name_norm),
    token_count_stripped = vapply(name_norm_stripped, count_tokens, integer(1))
  )

# ----------------------------
# RULE A: Collisions across FACs (exact normalized string)
# ----------------------------
collisions <- df2 %>%
  filter(!is.na(name_norm)) %>%
  distinct(fac, name_norm) %>%
  count(name_norm, name = "n_fac") %>%
  filter(n_fac > 1)

df2 <- df2 %>%
  left_join(collisions, by = "name_norm") %>%
  mutate(flag_collision = !is.na(n_fac) & n_fac > 1)

# ----------------------------
# RULE B: Corporate / sponsor / network entities
# ----------------------------
df2 <- df2 %>%
  mutate(
    flag_corporate = has_any_pattern_vec(name_norm, PAT_CORPORATE)
  )

# ----------------------------
# RULE C: Religious/generic names (high collision risk)
# ----------------------------
df2 <- df2 %>%
  mutate(
    flag_religious_generic = has_any_pattern_vec(name_norm, PAT_RELIGIOUS_GENERIC)
  )

# ----------------------------
# RULE D: Generic facility types w/ low distinctiveness
# ----------------------------
df2 <- df2 %>%
  mutate(
    flag_generic_facility =
      has_any_pattern_vec(name_norm, PAT_GENERIC_FACILITY) &
      token_count_stripped < MIN_DISTINCT_TOKENS
  )

# ----------------------------
# RULE E: Too-short / not distinctive (conservative)
# ----------------------------
df2 <- df2 %>%
  mutate(
    flag_generic_only = !is.na(name_norm) & name_norm %in% GENERIC_ONLY_NAMES
  )

# ----------------------------
# Combine unsafe + reasons
# ----------------------------
df3 <- df2 %>%
  mutate(
    unsafe =
      flag_collision |
      flag_corporate |
      flag_religious_generic |
      flag_generic_facility |
      flag_generic_only,
    unsafe_reasons = paste0(
      if_else(flag_collision, "collision_across_fac|", ""),
      if_else(flag_corporate, "corporate_or_network_entity|", ""),
      if_else(flag_religious_generic, "religious_generic_name|", ""),
      if_else(flag_generic_facility, "generic_facility_unqualified|", ""),
      if_else(flag_generic_only, "generic_only_name|", "")
    ),
    unsafe_reasons = str_replace(unsafe_reasons, "\\|$", ""),
    unsafe_reasons = na_if(unsafe_reasons, "")
  ) %>%
  select(-name_norm_stripped, -token_count_stripped)

# ----------------------------
# WRITE OUTPUTS
# ----------------------------
dir.create("outputs", showWarnings = FALSE)

readr::write_csv(df3, OUT_ALL)

unsafe_only <- df3 %>%
  filter(unsafe) %>%
  distinct(fac, name_raw, name_norm, unsafe_reasons)

readr::write_csv(unsafe_only, OUT_BAD)

# ----------------------------
# CONSOLE SUMMARY
# ----------------------------
cat("\n==============================\n")
cat("PHASE C: UNSAFE ALIAS SUMMARY\n")
cat("==============================\n")
cat("\nInput rows:", nrow(df), "\n")
cat("Unsafe rows:", sum(df3$unsafe, na.rm = TRUE), "\n")
cat("Unsafe %:", round(100 * mean(df3$unsafe, na.rm = TRUE), 1), "%\n")

cat("\nTop reasons:\n")
df3 %>%
  filter(unsafe, !is.na(unsafe_reasons)) %>%
  count(unsafe_reasons, sort = TRUE) %>%
  head(15) %>%
  print(n = 15)

cat("\nWrote outputs:\n")
cat(" - ", OUT_ALL, "\n", sep = "")
cat(" - ", OUT_BAD, "\n", sep = "")


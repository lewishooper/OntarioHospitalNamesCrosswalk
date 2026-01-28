# Phase E: QA / Validation for canonical hospital YAML files
# Project: OntarioHospitalNamesCrosswalk

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(yaml)
  library(tidyr)
})

# ----------------------------
# CONFIG
# ----------------------------
YAML_DIR <- file.path("code", "canonical_hospitals")

OUT_SUMMARY <- file.path("outputs", "phaseE_yaml_validation_summary.csv")
OUT_ALIAS_COLLISIONS <- file.path("outputs", "phaseE_alias_collisions.csv")
OUT_EDGE_CASES <- file.path("outputs", "phaseE_edge_cases.csv")

# ----------------------------
# HELPERS
# ----------------------------
normalize_alias <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- str_replace_all(x, "\u00A0", " ")
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- str_squish(x)
  x <- na_if(x, "")
  x
}

safe_length <- function(x) {
  if (is.null(x)) 0L else length(x)
}

read_one_yaml <- function(path) {
  obj <- yaml::read_yaml(path)
  
  fac <- obj$fac %||% NA_character_
  canonical_name <- obj$canonical_name %||% NA_character_
  moh_type <- obj$moh_type %||% NA_character_
  
  # Sunshine aliases expected
  sunshine <- obj$aliases$sunshine %||% NULL
  
  # normalize sunshine aliases into a tibble of (name, unsafe)
  alias_tbl <- tibble::tibble()
  
  if (!is.null(sunshine) && length(sunshine) > 0) {
    alias_tbl <- purrr::map_dfr(sunshine, function(item) {
      # item might be list(name=..., unsafe=TRUE) OR list(name=...) OR string (defensive)
      if (is.character(item) && length(item) == 1) {
        tibble::tibble(name_raw = item, unsafe = FALSE)
      } else {
        tibble::tibble(
          name_raw = item$name %||% NA_character_,
          unsafe = isTRUE(item$unsafe)
        )
      }
    }) %>%
      filter(!is.na(name_raw), name_raw != "")
  }
  
  # prior facs (optional)
  prior_facs <- obj$prior_facs %||% character(0)
  prior_facs <- as.character(prior_facs)
  
  list(
    fac = as.character(fac),
    canonical_name = as.character(canonical_name),
    moh_type = as.character(moh_type),
    prior_facs = prior_facs,
    alias_tbl = alias_tbl
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Extract FAC from filename prefix "123_*.yaml"
fac_from_filename <- function(path) {
  bn <- basename(path)
  m <- str_match(bn, "^([0-9]+)_.*\\.ya?ml$")
  if (is.na(m[1,2])) NA_character_ else m[1,2]
}

# ----------------------------
# LOAD YAML FILES
# ----------------------------
dir.create("outputs", showWarnings = FALSE)

files <- list.files(YAML_DIR, pattern = "\\.ya?ml$", full.names = TRUE)

if (length(files) == 0) stop("No YAML files found in: ", YAML_DIR)

yaml_parsed <- purrr::map(files, read_one_yaml)

meta <- tibble::tibble(
  path = files,
  fac_filename = vapply(files, fac_from_filename, character(1)),
  fac_yaml = vapply(yaml_parsed, `[[`, character(1), "fac"),
  canonical_name = vapply(yaml_parsed, `[[`, character(1), "canonical_name"),
  moh_type = vapply(yaml_parsed, `[[`, character(1), "moh_type"),
  n_aliases = vapply(yaml_parsed, function(x) nrow(x$alias_tbl), integer(1)),
  has_prior_facs = vapply(yaml_parsed, function(x) length(x$prior_facs) > 0, logical(1))
)

# ----------------------------
# CHECK 1: One YAML file per FAC (and filename matches content)
# ----------------------------
dup_fac_files <- meta %>%
  filter(!is.na(fac_yaml), fac_yaml != "") %>%
  count(fac_yaml, name = "n_files") %>%
  filter(n_files > 1)

fac_mismatch <- meta %>%
  filter(!is.na(fac_filename), !is.na(fac_yaml), fac_filename != fac_yaml)

missing_fac_in_yaml <- meta %>%
  filter(is.na(fac_yaml) | fac_yaml == "")

# ----------------------------
# CHECK 2: Every FAC has >= 1 alias
# ----------------------------
missing_aliases <- meta %>%
  filter(n_aliases < 1)

# ----------------------------
# Build alias table across all FACs for collision checks
# ----------------------------
alias_all <- purrr::imap_dfr(yaml_parsed, function(x, i) {
  tibble::tibble(
    fac = x$fac,
    path = files[[i]],
    canonical_name = x$canonical_name
  ) %>%
    tidyr::crossing(x$alias_tbl) %>%
    mutate(
      alias_norm = normalize_alias(name_raw)
    ) %>%
    filter(!is.na(alias_norm), alias_norm != "")
})

# ----------------------------
# CHECK 3: No alias appears under multiple FACs unless marked unsafe
# Conservative rule: if alias_norm occurs under >1 FAC, then ALL occurrences must be unsafe
# ----------------------------
alias_multi_fac <- alias_all %>%
  distinct(fac, alias_norm, unsafe) %>%
  group_by(alias_norm) %>%
  summarise(
    n_fac = n_distinct(fac),
    any_safe = any(!unsafe),
    facs = paste(sort(unique(fac)), collapse = "|"),
    .groups = "drop"
  ) %>%
  filter(n_fac > 1)

collisions_bad <- alias_multi_fac %>%
  filter(any_safe)

# A more detailed collision report (rows for review)
collisions_detail <- alias_all %>%
  semi_join(collisions_bad, by = "alias_norm") %>%
  arrange(alias_norm, fac)

# ----------------------------
# CHECK 4: Edge case spot-check exports
# ----------------------------
edge <- alias_all %>%
  mutate(
    edge_class = case_when(
      str_detect(alias_norm, "HOTEL DIEU") ~ "hotel_dieu",
      str_detect(alias_norm, "ST JOSEPH|SAINT JOSEPH") ~ "st_joseph",
      str_detect(alias_norm, "NETWORK|ASSOCIATION|FOUNDATION|CORPORATION|GROUP|HOSPITALLERS|RELIGIEUSES") ~ "network_or_corporate",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(edge_class)) %>%
  select(edge_class, fac, canonical_name, name_raw, unsafe, path) %>%
  arrange(edge_class, fac, name_raw)

# ----------------------------
# WRITE OUTPUTS
# ----------------------------
summary_tbl <- tibble::tibble(
  metric = c(
    "n_yaml_files",
    "n_unique_fac_yaml",
    "n_fac_duplicate_files",
    "n_fac_filename_mismatch",
    "n_missing_fac_in_yaml",
    "n_fac_with_no_aliases",
    "n_alias_norm_collisions_multi_fac",
    "n_bad_collisions_any_safe",
    "n_edge_cases"
  ),
  value = c(
    nrow(meta),
    n_distinct(meta$fac_yaml),
    nrow(dup_fac_files),
    nrow(fac_mismatch),
    nrow(missing_fac_in_yaml),
    nrow(missing_aliases),
    nrow(alias_multi_fac),
    nrow(collisions_bad),
    nrow(edge)
  )
)

readr::write_csv(summary_tbl, OUT_SUMMARY)

# collision outputs (only if present)
readr::write_csv(collisions_detail, OUT_ALIAS_COLLISIONS)

# edge case listing
readr::write_csv(edge, OUT_EDGE_CASES)

# ----------------------------
# CONSOLE SUMMARY
# ----------------------------
cat("\n==============================\n")
cat("PHASE E: YAML QA / VALIDATION\n")
cat("==============================\n")

print(summary_tbl)

if (nrow(dup_fac_files) > 0) {
  cat("\n[FAIL] Duplicate FACs across YAML files:\n")
  print(dup_fac_files)
}

if (nrow(fac_mismatch) > 0) {
  cat("\n[FAIL] Filename FAC != YAML FAC:\n")
  print(fac_mismatch %>% select(path, fac_filename, fac_yaml))
}

if (nrow(missing_aliases) > 0) {
  cat("\n[FAIL] FACs with no aliases:\n")
  print(missing_aliases %>% select(path, fac_yaml, canonical_name))
}

if (nrow(collisions_bad) > 0) {
  cat("\n[FAIL] Alias collisions across multiple FACs where at least one occurrence is SAFE.\n")
  cat("See: ", OUT_ALIAS_COLLISIONS, "\n", sep = "")
  print(collisions_bad)
} else {
  cat("\n[OK] No unsafe-rule violations for alias collisions (safe aliases are unique across FACs).\n")
}

cat("\nEdge cases exported to: ", OUT_EDGE_CASES, "\n", sep = "")
cat("Summary exported to:   ", OUT_SUMMARY, "\n", sep = "")

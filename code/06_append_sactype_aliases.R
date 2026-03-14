# 06_append_sactype_aliases.R
# Phase 4: Match SACType source to FAC master and append aliases
# Project: OntarioHospitalNamesCrosswalk
#
# Pass 1: Exact normalized match on full name
# Pass 2: Exact normalized match on parent name (strip site suffix)
# Pass 3: Exact normalized match on site suffix (right side of separator)
# Pass 4: Jaro-Winkler fuzzy match on parent name
# Remaining: Export override template for manual review
#
# Inputs:  sources/sactype_prepared.csv
#          outputs/fac_aliases.rds
#          outputs/fac_master.rds
#          sources/sactype_fac_overrides.csv (optional)
# Outputs: outputs/fac_aliases.rds (updated)
#          outputs/fac_aliases.csv (updated)
#          outputs/fac_master.rds (updated with sactype_value)
#          outputs/fac_master.csv (updated)
#          outputs/sactype_auto_matched.csv
#          outputs/sactype_unmatched.csv
#          outputs/sactype_override_template.csv
#          outputs/sactype_fac_classification.csv
#          outputs/validation/sactype_mapping_summary.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringdist)
  library(tidyr)
})

source(file.path("code", "utils", "normalize.R"))

# ----------------------------
# CONFIG
# ----------------------------
SACTYPE_PATH    <- file.path("sources", "sactype_prepared.csv")
ALIASES_PATH    <- file.path("outputs", "fac_aliases.rds")
FAC_MASTER_PATH <- file.path("outputs", "fac_master.rds")
OVERRIDES_PATH  <- file.path("sources", "sactype_fac_overrides.csv")

OUT_ALIASES_RDS <- file.path("outputs", "fac_aliases.rds")
OUT_ALIASES_CSV <- file.path("outputs", "fac_aliases.csv")
OUT_MASTER_RDS  <- file.path("outputs", "fac_master.rds")
OUT_MASTER_CSV  <- file.path("outputs", "fac_master.csv")
OUT_MATCHED     <- file.path("outputs", "sactype_auto_matched.csv")
OUT_UNMATCHED   <- file.path("outputs", "sactype_unmatched.csv")
OUT_TEMPLATE    <- file.path("outputs", "sactype_override_template.csv")
OUT_CLASS       <- file.path("outputs", "sactype_fac_classification.csv")
OUT_SUMMARY     <- file.path("outputs", "validation", "sactype_mapping_summary.csv")

FUZZY_THRESHOLD <- 0.88  # Jaro-Winkler minimum for auto-accept

# ----------------------------
# HELPERS
# ----------------------------
# Extract parent name (left of separator) and site name (right of separator)
split_site_name <- function(x) {
  sep_pattern <- "\\s[—–]\\s|\\s-\\s"
  has_sep <- str_detect(x, sep_pattern)
  parent <- if_else(has_sep,
                    str_trim(str_replace(x, paste0("(", sep_pattern, ").*$"), "")),
                    x)
  site   <- if_else(has_sep,
                    str_trim(str_replace(x, paste0("^.*", sep_pattern), "")),
                    NA_character_)
  list(parent = parent, site = site)
}

# ----------------------------
# LOAD
# ----------------------------
sactype  <- readr::read_csv(SACTYPE_PATH, show_col_types = FALSE)
aliases  <- readRDS(ALIASES_PATH)
fac_master <- readRDS(FAC_MASTER_PATH)

# Build safe name universe from fac_aliases (all sources, safe only)
name_universe <- aliases |>
  filter(!(source_system == "sunshine" & unsafe)) |>
  select(fac, source_system, name_raw, name_norm) |>
  filter(!is.na(name_norm), name_norm != "")

# ----------------------------
# PREPARE SACTYPE ROWS
# ----------------------------
split_result <- split_site_name(sactype$facility_name)

sactype2 <- sactype |>
  mutate(
    odhf_row_id   = row_number(),
    parent_name   = split_result$parent,
    site_name     = split_result$site,
    parent_norm   = normalize_name(parent_name),
    site_norm     = normalize_name(site_name)
  )

# ----------------------------
# PASS 1: Exact match on full normalized name
# ----------------------------
pass1 <- sactype2 |>
  inner_join(name_universe |> select(fac, name_norm),
             by = c("facility_name_norm" = "name_norm")) |>
  distinct(odhf_row_id, .keep_all = TRUE) |>
  mutate(match_pass = "pass1_full_exact")

remaining <- sactype2 |>
  anti_join(pass1, by = "odhf_row_id")

cat("Pass 1 (full exact):  ", nrow(pass1), "matched,",
    nrow(remaining), "remaining\n")

# ----------------------------
# PASS 2: Exact match on parent name (left of separator)
# ----------------------------
pass2 <- remaining |>
  filter(!is.na(parent_norm)) |>
  inner_join(name_universe |> select(fac, name_norm),
             by = c("parent_norm" = "name_norm")) |>
  distinct(odhf_row_id, .keep_all = TRUE) |>
  mutate(match_pass = "pass2_parent_exact")

remaining <- remaining |>
  anti_join(pass2, by = "odhf_row_id")

cat("Pass 2 (parent exact):", nrow(pass2), "matched,",
    nrow(remaining), "remaining\n")

# ----------------------------
# PASS 3: Exact match on site name (right of separator)
# ----------------------------
pass3 <- remaining |>
  filter(!is.na(site_norm)) |>
  inner_join(name_universe |> select(fac, name_norm),
             by = c("site_norm" = "name_norm")) |>
  distinct(odhf_row_id, .keep_all = TRUE) |>
  mutate(match_pass = "pass3_site_exact")

remaining <- remaining |>
  anti_join(pass3, by = "odhf_row_id")

cat("Pass 3 (site exact):  ", nrow(pass3), "matched,",
    nrow(remaining), "remaining\n")

# ----------------------------
# PASS 4: Jaro-Winkler fuzzy on parent name
# ----------------------------
universe_norms <- unique(name_universe$name_norm)

fuzzy_results <- remaining |>
  filter(!is.na(parent_norm)) |>
  rowwise() |>
  mutate(
    jw_scores    = list(stringdist::stringsim(
      parent_norm, universe_norms, method = "jw")),
    best_score   = max(unlist(jw_scores)),
    best_match   = universe_norms[which.max(unlist(jw_scores))]
  ) |>
  ungroup()

pass4 <- fuzzy_results |>
  filter(best_score >= FUZZY_THRESHOLD) |>
  left_join(name_universe |>
              select(fac, name_norm) |>
              distinct(name_norm, .keep_all = TRUE),
            by = c("best_match" = "name_norm")) |>
  distinct(odhf_row_id, .keep_all = TRUE) |>
  mutate(match_pass = paste0("pass4_fuzzy_jw_", round(best_score, 3)))

remaining <- fuzzy_results |>
  anti_join(pass4, by = "odhf_row_id") |>
  bind_rows(remaining |> filter(is.na(parent_norm)))

cat("Pass 4 (fuzzy JW):    ", nrow(pass4), "matched,",
    nrow(remaining), "remaining\n")

# ----------------------------
# COMBINE AUTO MATCHES
# ----------------------------
auto_matched <- bind_rows(
  pass1 |> select(odhf_row_id, facility_name, fac, sactype_value, match_pass),
  pass2 |> select(odhf_row_id, facility_name, fac, sactype_value, match_pass),
  pass3 |> select(odhf_row_id, facility_name, fac, sactype_value, match_pass),
  pass4 |> select(odhf_row_id, facility_name, fac, sactype_value, match_pass)
) |>
  mutate(fac = as.character(fac)) |>  
  arrange(fac, facility_name)

# ----------------------------
# APPLY OVERRIDES (if present)
# ----------------------------
if (file.exists(OVERRIDES_PATH)) {
  overrides <- readr::read_csv(OVERRIDES_PATH, show_col_types = FALSE) |>
    standardize_colnames()
  
  required <- c("sactype_name_clean", "fac", "sactype_value", "override_reason")
  missing  <- setdiff(required, names(overrides))
  if (length(missing) > 0) stop("Overrides missing columns: ",
                                paste(missing, collapse = ", "))
  
  blank <- overrides |>
    filter(is.na(override_reason) | str_trim(override_reason) == "")
  if (nrow(blank) > 0) stop("Overrides has blank override_reason for: ",
                            paste(blank$sactype_name_clean, collapse = ", "))
  
  overrides_clean <- overrides |>
    transmute(
      facility_name   = str_squish(sactype_name_clean),
      fac             = as.character(str_replace_all(str_squish(fac), "[^0-9]", "")),
      sactype_value   = as.integer(str_squish(sactype_value)),  # WAS str_squish only
      override_reason = str_squish(override_reason),
      match_pass      = "override"
    )
  
  # Remove any auto matches for rows that have an override
  auto_matched <- auto_matched |>
    anti_join(overrides_clean |> select(facility_name), by = "facility_name")
  
  remaining <- remaining |>
    anti_join(overrides_clean |> select(facility_name), by = "facility_name")
  
  auto_matched <- bind_rows(auto_matched, overrides_clean)
  cat("Overrides applied:    ", nrow(overrides_clean), "rows\n")
} else {
  cat("No overrides file found — proceeding with auto matches only.\n")
}

# ----------------------------
# SACTYPE CONSISTENCY CHECK
# ----------------------------
# For FACs matched via multiple ODHF rows, check SACtype consistency
fac_sactype_check <- auto_matched |>
  group_by(fac) |>
  summarise(
    n_odhf_rows     = n(),
    n_distinct_sac  = n_distinct(sactype_value, na.rm = TRUE),
    sactype_values  = paste(sort(unique(sactype_value)), collapse = "|"),
    .groups = "drop"
  )

inconsistent <- fac_sactype_check |> filter(n_distinct_sac > 1)
if (nrow(inconsistent) > 0) {
  cat("\n[WARN] FACs with inconsistent SACtype across sites:\n")
  print(inconsistent)
}

# Resolve to one SACtype per FAC (majority vote; urban wins ties)
# Replace the fac_sactype_final block with this:
fac_sactype_final <- auto_matched |>
  group_by(fac) |>
  summarise(
    sactype_value  = {
      vals <- sactype_value[!is.na(sactype_value)]
      if (length(vals) == 0) NA_integer_
      else as.integer(names(sort(table(vals), decreasing = TRUE))[1])  # ADD as.integer()
      
    },
    n_odhf_rows  = n(),
    match_passes = paste(sort(unique(match_pass)), collapse = "|"),
    odhf_names   = paste(sort(unique(facility_name)), collapse = " || "),
    .groups = "drop"
  ) |>
  mutate(fac = as.character(fac))
# ----------------------------
# EXPORT OVERRIDE TEMPLATE for unmatched
# ----------------------------
override_template <- remaining |>
  transmute(
    sactype_name_clean = facility_name,
    fac                = "",
    sactype_value      = "",
    override_reason    = "",
    notes              = paste0("city: ", city,
                                " | norm: ", facility_name_norm)
  )

# ----------------------------
# APPEND SACTYPE ALIASES TO fac_aliases
# ----------------------------
existing_sac <- aliases |> filter(source_system == "statscan")
if (nrow(existing_sac) > 0) {
  cat("[INFO] Removing", nrow(existing_sac),
      "existing statscan aliases before re-appending.\n")
  aliases <- aliases |> filter(source_system != "statscan")
}

sactype_aliases <- auto_matched |>
  transmute(
    fac                  = as.character(fac),
    source_system        = "statscan",
    name_raw             = facility_name,
    name_norm            = normalize_name(facility_name),
    unsafe               = FALSE,
    unsafe_reasons       = NA_character_,
    year_observed        = NA_integer_,
    fac_before_correction = NA_character_
  ) |>
  filter(!is.na(name_norm)) |>
  distinct(fac, name_raw, .keep_all = TRUE)

aliases_updated <- bind_rows(aliases, sactype_aliases) |>
  arrange(fac, source_system, name_raw)

# ----------------------------
# UPDATE fac_master WITH sactype_value
# ----------------------------
fac_master_updated <- fac_master |>
  select(-sactype_value) |>
  left_join(fac_sactype_final |> select(fac, sactype_value),
            by = "fac")

# ----------------------------
# WRITE ALL OUTPUTS
# ----------------------------
dir.create(file.path("outputs", "validation"), showWarnings = FALSE)

saveRDS(aliases_updated,    OUT_ALIASES_RDS)
readr::write_csv(aliases_updated,    OUT_ALIASES_CSV)
saveRDS(fac_master_updated, OUT_MASTER_RDS)
readr::write_csv(fac_master_updated, OUT_MASTER_CSV)
readr::write_csv(auto_matched,       OUT_MATCHED)
readr::write_csv(remaining |>
                   select(odhf_row_id, facility_name, city,
                          facility_name_norm, parent_norm, sactype_value),
                 OUT_UNMATCHED)
readr::write_csv(override_template,  OUT_TEMPLATE)
readr::write_csv(fac_sactype_final,  OUT_CLASS)

summary_tbl <- tibble::tibble(
  metric = c(
    "odhf_rows_total",
    "pass1_full_exact",
    "pass2_parent_exact",
    "pass3_site_exact",
    "pass4_fuzzy_jw",
    "overrides_applied",
    "total_auto_matched",
    "unmatched",
    "facs_with_sactype",
    "facs_total",
    "facs_inconsistent_sactype"
  ),
  value = c(
    nrow(sactype2),
    nrow(pass1),
    nrow(pass2),
    nrow(pass3),
    nrow(pass4),
    if (file.exists(OVERRIDES_PATH)) nrow(overrides_clean) else 0L,
    nrow(auto_matched),
    nrow(remaining),
    nrow(fac_sactype_final),
    nrow(fac_master),
    nrow(inconsistent)
  )
)

readr::write_csv(summary_tbl, OUT_SUMMARY)

# ----------------------------
# CONSOLE SUMMARY
# ----------------------------
cat("\n==============================\n")
cat("PHASE 4: SACTYPE MAPPING\n")
cat("==============================\n")
print(summary_tbl, n = nrow(summary_tbl))

cat("\nSACtype distribution (matched FACs):\n")
print(table(fac_master_updated$sactype_value, useNA = "ifany"))

cat("\nFinal alias source breakdown:\n")
print(table(aliases_updated$source_system))
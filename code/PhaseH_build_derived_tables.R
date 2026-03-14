# Phase H — Derived Tables (Read-only)
# Project: OntarioHospitalNamesCrosswalk
#
# Inputs:
# - FAC YAMLs: code/canonical_hospitals/*.yaml
# - Reporting Entities YAMLs (optional): code/reporting_entities/*.yaml
# - CIHI->FAC final map: outputs/phaseG_cihi_fac_map_final.csv
#
# Outputs (all to outputs/derived/):
# - fac_master.csv
# - fac_aliases_all.csv
# - fac_aliases_safe.csv
# - cihi_fac_map.csv
# - cihi_reporting_entities.csv (if entity YAMLs exist)
# - cihi_entity_members.csv (if entity YAMLs exist)
# - fac_to_cihi_rollup.csv
# - name_universe_safe.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(yaml)
  library(tidyr)
  library(tibble)
  library(fs)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------
# CONFIG
# ----------------------------
FAC_YAML_DIR    <- file.path("code", "canonical_hospitals")
ENTITY_YAML_DIR <- file.path("code", "reporting_entities")  # optional
CIHI_MAP_FINAL  <- file.path("outputs", "phaseG_cihi_fac_map_final.csv")

OUT_DIR <- file.path("outputs", "derived")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

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

read_yaml_drop_path <- function(path) {
  obj <- yaml::read_yaml(path)
  # Defensively drop any file-provenance keys (Phase H is read-only, but keep tables clean)
  if (".__path__" %in% names(obj)) obj$`.__path__` <- NULL
  if ("__path__" %in% names(obj))  obj$`__path__` <- NULL
  obj$yaml_path <- path
  obj
}

# Aliases can be a list of strings or list of {name, unsafe, evidence, ...}
as_alias_rows <- function(alias_list, fac, source_system, canonical_name = NA_character_) {
  if (is.null(alias_list) || length(alias_list) == 0) return(tibble())
  
  purrr::map_dfr(alias_list, function(a) {
    
    # If stored as a bare string:
    if (is.character(a) && length(a) == 1) {
      nm <- a
      unsafe <- FALSE
      evidence <- NA_character_
      normalized <- normalize_name(nm)
      extra <- list()
    } else {
      nm <- as.character(a$name %||% NA_character_)
      unsafe <- isTRUE(a$unsafe)
      evidence <- as.character(a$evidence %||% a$notes %||% NA_character_)
      normalized <- as.character(a$normalized %||% normalize_name(nm))
      extra <- a
    }
    
    if (is.na(nm) || !nzchar(nm)) return(tibble())
    
    tibble(
      fac = as.character(fac),
      source_system = source_system,
      alias_name = nm,
      alias_norm = normalized,
      unsafe = unsafe,
      evidence = evidence,
      canonical_name = canonical_name
    )
  }) %>%
    filter(!is.na(alias_norm), alias_norm != "")
}

# ----------------------------
# STEP 1: LOAD FAC YAMLs
# ----------------------------
if (!dir.exists(FAC_YAML_DIR)) stop("FAC_YAML_DIR not found: ", FAC_YAML_DIR)

fac_files <- list.files(FAC_YAML_DIR, pattern = "\\.ya?ml$", full.names = TRUE)
if (length(fac_files) == 0) stop("No FAC YAML files found in: ", FAC_YAML_DIR)

fac_objs <- purrr::map(fac_files, read_yaml_drop_path)

# fac_master (1 row per FAC)
fac_master <- purrr::map_dfr(fac_objs, function(x) {
  fac <- as.character(x$fac %||% NA_character_)
  canonical <- as.character(x$canonical_name %||% NA_character_)
  moh_type <- as.character(x$moh_type %||% NA_character_)
  
  sunshine_aliases <- x$aliases$sunshine %||% list()
  cihi_aliases     <- x$aliases$cihi %||% list()
  
  # Count unsafe in sunshine only (your Phase C concept); CIHI aliases are authoritative for CIHI use
  n_aliases_total <- length(sunshine_aliases) + length(cihi_aliases)
  n_unsafe_sunshine <- sum(purrr::map_lgl(sunshine_aliases, ~ isTRUE(.x$unsafe)))
  
  tibble(
    fac = fac,
    canonical_name = canonical,
    moh_type = moh_type,
    yaml_path = x$yaml_path,
    n_aliases_total = n_aliases_total,
    n_aliases_sunshine = length(sunshine_aliases),
    n_aliases_cihi = length(cihi_aliases),
    n_unsafe_sunshine = n_unsafe_sunshine
  )
}) %>%
  filter(!is.na(fac), fac != "") %>%
  distinct(fac, .keep_all = TRUE)

# aliases table (all)
fac_aliases_all <- purrr::map_dfr(fac_objs, function(x) {
  fac <- as.character(x$fac %||% NA_character_)
  canonical <- as.character(x$canonical_name %||% NA_character_)
  
  bind_rows(
    as_alias_rows(x$aliases$sunshine %||% list(), fac, "sunshine", canonical),
    as_alias_rows(x$aliases$cihi %||% list(), fac, "cihi", canonical)
  )
}) %>%
  distinct(fac, source_system, alias_norm, .keep_all = TRUE)

# safe-only universe (for future matching)
fac_aliases_safe <- fac_aliases_all %>%
  filter(!(source_system == "sunshine" & unsafe)) %>%
  distinct(fac, alias_norm, .keep_all = TRUE)

# A convenient "name universe safe" table
name_universe_safe <- fac_aliases_safe %>%
  select(fac, source_system, alias_name, alias_norm, canonical_name) %>%
  left_join(fac_master %>% select(fac, moh_type), by = "fac") %>%
  arrange(fac, source_system, alias_norm)

# ----------------------------
# STEP 2: LOAD CIHI MAP FINAL (Phase G output)
# ----------------------------
if (!file.exists(CIHI_MAP_FINAL)) stop("CIHI_MAP_FINAL not found: ", CIHI_MAP_FINAL)

cihi_fac_map <- readr::read_csv(CIHI_MAP_FINAL, show_col_types = FALSE) %>%
  mutate(
    fac = as.character(fac),
    cihi_name_raw = as.character(cihi_name_raw),
    cihi_name_norm = normalize_name(cihi_name_raw)
  )

# ----------------------------
# STEP 3: LOAD REPORTING ENTITIES (optional)
# ----------------------------
has_entities <- dir.exists(ENTITY_YAML_DIR) &&
  length(list.files(ENTITY_YAML_DIR, pattern="\\.ya?ml$", full.names = TRUE)) > 0

cihi_reporting_entities <- tibble()
cihi_entity_members <- tibble()

if (has_entities) {
  entity_files <- list.files(ENTITY_YAML_DIR, pattern="\\.ya?ml$", full.names = TRUE)
  entity_objs <- purrr::map(entity_files, read_yaml_drop_path)
  
  cihi_reporting_entities <- purrr::map_dfr(entity_objs, function(x) {
    tibble(
      entity_id = as.character(x$entity_id %||% NA_character_),
      reporting_name = as.character(x$reporting_name %||% x$display_name %||% NA_character_),
      source_system = as.character(x$source_system %||% "cihi"),
      reporting_type = as.character(x$reporting_type %||% x$entity_type %||% NA_character_),
      n_member_facs = length(x$fac_members %||% x$member_facs %||% list()),
      notes = as.character(x$notes %||% NA_character_),
      yaml_path = x$yaml_path
    )
  }) %>% filter(!is.na(entity_id), entity_id != "")
  
  # Support either fac_members: [{fac, role},...] OR member_facs: ["684","824"]
  cihi_entity_members <- purrr::map_dfr(entity_objs, function(x) {
    entity_id <- as.character(x$entity_id %||% NA_character_)
    
    if (!is.null(x$fac_members) && length(x$fac_members) > 0) {
      purrr::map_dfr(x$fac_members, function(m) {
        tibble(
          entity_id = entity_id,
          fac = as.character(m$fac %||% NA_character_),
          role = as.character(m$role %||% "member")
        )
      })
    } else if (!is.null(x$member_facs) && length(x$member_facs) > 0) {
      tibble(
        entity_id = entity_id,
        fac = as.character(unlist(x$member_facs)),
        role = "member"
      )
    } else {
      tibble()
    }
  }) %>%
    filter(!is.na(entity_id), entity_id != "", !is.na(fac), fac != "")
}

# ----------------------------
# STEP 4: FAC -> CIHI ROLLUP
# ----------------------------
# A FAC can appear under multiple CIHI reporting names in rare cases; this captures it explicitly.
fac_to_cihi_rollup <- cihi_fac_map %>%
  group_by(fac) %>%
  summarise(
    n_cihi_rows = n_distinct(cihi_row_id %||% cihi_name_raw),
    cihi_reporting_names = paste(sort(unique(cihi_name_raw)), collapse = " | "),
    rollup_type = case_when(
      n_cihi_rows == 1 ~ "single",
      n_cihi_rows > 1 ~ "multi",
      TRUE ~ "unknown"
    ),
    .groups = "drop"
  ) %>%
  arrange(fac)

# ----------------------------
# WRITE OUTPUTS
# ----------------------------
write_csv(fac_master,           file.path(OUT_DIR, "fac_master.csv"))
write_csv(fac_aliases_all,      file.path(OUT_DIR, "fac_aliases_all.csv"))
write_csv(fac_aliases_safe,     file.path(OUT_DIR, "fac_aliases_safe.csv"))
write_csv(name_universe_safe,   file.path(OUT_DIR, "name_universe_safe.csv"))
write_csv(cihi_fac_map,         file.path(OUT_DIR, "cihi_fac_map.csv"))
write_csv(fac_to_cihi_rollup,   file.path(OUT_DIR, "fac_to_cihi_rollup.csv"))

if (has_entities) {
  write_csv(cihi_reporting_entities, file.path(OUT_DIR, "cihi_reporting_entities.csv"))
  write_csv(cihi_entity_members,    file.path(OUT_DIR, "cihi_entity_members.csv"))
}

cat("\n==============================\n")
cat("PHASE H: DERIVED TABLES COMPLETE\n")
cat("==============================\n")
cat("Wrote to: ", OUT_DIR, "\n", sep = "")
cat("FACs: ", nrow(fac_master), "\n", sep = "")
cat("Aliases (all): ", nrow(fac_aliases_all), "\n", sep = "")
cat("Aliases (safe): ", nrow(fac_aliases_safe), "\n", sep = "")
cat("CIHI map rows: ", nrow(cihi_fac_map), "\n", sep = "")
if (has_entities) cat("Reporting entities: ", nrow(cihi_reporting_entities), "\n", sep = "")

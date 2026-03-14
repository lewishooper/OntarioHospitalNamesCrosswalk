# Phase G2 — CIHI Alias Enrichment (FAC YAML Update)
# Project: OntarioHospitalNamesCrosswalk
#
# Purpose:
# - Take outputs/phaseG_cihi_fac_map_final.csv (authoritative CIHI→FAC mapping)
# - Write CIHI raw reporting names into FAC-centric YAML files as aliases$cihi
#
# Design:
# - FAC YAML remains the "truth store"
# - CIHI raw name is the reporting identifier
# - normalized is auxiliary (search/matching aid)
# - Script is idempotent (safe to re-run)
#
# Outputs:
# - outputs/phaseG2_cihi_alias_update_report.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(yaml)
  library(tibble)
  library(fs)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------
# CONFIG
# ----------------------------
YAML_DIR   <- file.path("code", "canonical_hospitals")
MAP_FINAL  <- file.path("outputs", "phaseG_cihi_fac_map_final.csv")

# Optionally record the CIHI year that this alias set came from
# (set to NA if you don't want it written)
CIHI_SOURCE_YEAR <- 2025

# If TRUE, will NOT write YAML changes (produces report only)
DRY_RUN <- FALSE

# Backups
BACKUP_ROOT <- file.path("outputs", "backups")
dir.create(BACKUP_ROOT, showWarnings = FALSE, recursive = TRUE)

OUT_REPORT <- file.path("outputs", "phaseG2_cihi_alias_update_report.csv")

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

read_yaml_safe <- function(path) {
  obj <- yaml::read_yaml(path)
  obj$.__path__ <- path
  obj
}
### 
## code to stop ".__path__: code/canonical_hospitals..." being added to yaml
obj$`__path__` <- NULL

write_yaml_pretty <- function(obj, path) {
  # Keep yaml stable and readable
  # yaml::as.yaml is generally deterministic if you keep list ordering stable
  txt <- yaml::as.yaml(
    obj,
    indent = 2,
    line.sep = "\n"
  )
  writeLines(txt, path, useBytes = TRUE)
}

# Find YAML file by parsing all and mapping FAC -> path
load_fac_yamls <- function(dir_path) {
  files <- list.files(dir_path, pattern = "\\.ya?ml$", full.names = TRUE)
  if (length(files) == 0) stop("No YAML files found in: ", dir_path)
  
  objs <- purrr::map(files, read_yaml_safe)
  
  fac_index <- purrr::map_dfr(objs, function(x) {
    tibble(
      fac = as.character(x$fac %||% NA_character_),
      yaml_path = x$.__path__
    )
  }) %>%
    filter(!is.na(fac), fac != "") %>%
    distinct(fac, .keep_all = TRUE)
  
  if (nrow(fac_index) == 0) stop("No FAC values found inside YAML files in: ", dir_path)
  
  list(objs = objs, index = fac_index)
}

# Ensure aliases list exists
ensure_alias_lists <- function(obj) {
  obj$aliases <- obj$aliases %||% list()
  obj$aliases$cihi <- obj$aliases$cihi %||% list()
  obj
}

# Create CIHI alias entry
make_cihi_alias_entry <- function(cihi_name_raw, cihi_name_norm, cihi_type = NA_character_,
                                  match_status = NA_character_,
                                  override_reason = NA_character_,
                                  notes = NA_character_,
                                  source_year = NA_integer_) {
  entry <- list(
    name = cihi_name_raw,
    normalized = cihi_name_norm
  )
  
  if (!is.na(source_year)) entry$source_year <- source_year
  if (!is.na(cihi_type) && nzchar(cihi_type)) entry$cihi_type <- cihi_type
  if (!is.na(match_status) && nzchar(match_status)) entry$match_status <- match_status
  if (!is.na(override_reason) && nzchar(override_reason)) entry$override_reason <- override_reason
  if (!is.na(notes) && nzchar(notes)) entry$notes <- notes
  
  entry
}

# Detect if an identical CIHI alias already exists (match by raw name)
cihi_alias_exists <- function(obj, cihi_name_raw) {
  obj <- ensure_alias_lists(obj)
  existing <- obj$aliases$cihi %||% list()
  
  # existing might be list of lists; handle defensive
  existing_names <- purrr::map_chr(existing, function(x) {
    if (is.character(x)) x else as.character(x$name %||% NA_character_)
  })
  
  any(!is.na(existing_names) & existing_names == cihi_name_raw)
}

# ----------------------------
# LOAD INPUTS
# ----------------------------
if (!dir.exists(YAML_DIR)) stop("YAML_DIR not found: ", YAML_DIR)
if (!file.exists(MAP_FINAL)) stop("CIHI->FAC final map not found: ", MAP_FINAL)

map_final <- readr::read_csv(MAP_FINAL, show_col_types = FALSE) %>%
  as.data.frame() %>%
  tibble::as_tibble()

# Required columns (robustly)
needed <- c("fac", "cihi_name_raw")
missing <- setdiff(needed, names(map_final))
if (length(missing) > 0) {
  stop("phaseG_cihi_fac_map_final.csv is missing required columns: ", paste(missing, collapse = ", "),
       "\nAvailable: ", paste(names(map_final), collapse = ", "))
}

# Optional columns
get_col <- function(df, nm, default = NA_character_) {
  if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
}

map2 <- map_final %>%
  transmute(
    fac = as.character(fac),
    cihi_name_raw = as.character(cihi_name_raw),
    cihi_type = as.character(get_col(map_final, "cihi_type")),
    match_status = as.character(get_col(map_final, "match_status")),
    override_reason = as.character(get_col(map_final, "override_reason")),
    notes = as.character(get_col(map_final, "notes"))
  ) %>%
  mutate(
    cihi_name_raw = str_squish(cihi_name_raw),
    cihi_name_norm = normalize_name(cihi_name_raw)
  ) %>%
  filter(!is.na(fac), fac != "", !is.na(cihi_name_raw), cihi_name_raw != "")

# Deduplicate exact FAC+CIHI raw name pairs
map2 <- map2 %>% distinct(fac, cihi_name_raw, .keep_all = TRUE)

# ----------------------------
# LOAD YAMLs + BACKUP
# ----------------------------
loaded <- load_fac_yamls(YAML_DIR)
fac_index <- loaded$index
yaml_objs <- loaded$objs

# Backup YAML directory (copy entire folder)
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_dir <- file.path(BACKUP_ROOT, paste0("canonical_hospitals_backup_", stamp))
dir_create(backup_dir)

if (!DRY_RUN) {
  # Copy all YAML files as a safety net
  file_copy(list.files(YAML_DIR, full.names = TRUE), backup_dir, overwrite = TRUE)
}

# ----------------------------
# APPLY UPDATES
# ----------------------------
# Build path lookup
fac_to_path <- fac_index %>% deframe()

# Track changes
changes <- list()

# We'll re-read/write each YAML by path (avoids needing to keep parallel obj lists aligned)
unique_facs <- sort(unique(map2$fac))

for (fac_i in unique_facs) {
  
  if (!fac_i %in% names(fac_to_path)) {
    # FAC from CIHI map not present in canonical YAML set
    changes[[length(changes) + 1]] <- tibble(
      fac = fac_i,
      yaml_path = NA_character_,
      cihi_name_raw = NA_character_,
      action = "fac_not_found_in_yaml",
      detail = "FAC appears in CIHI map but no YAML exists in canonical_hospitals."
    )
    next
  }
  
  path_i <- fac_to_path[[fac_i]]
  obj <- read_yaml_safe(path_i) %>% ensure_alias_lists()
  
  rows_i <- map2 %>% filter(fac == fac_i)
  
  for (k in seq_len(nrow(rows_i))) {
    r <- rows_i[k, ]
    
    already <- cihi_alias_exists(obj, r$cihi_name_raw)
    
    if (already) {
      changes[[length(changes) + 1]] <- tibble(
        fac = fac_i,
        yaml_path = path_i,
        cihi_name_raw = r$cihi_name_raw,
        action = "skipped_existing",
        detail = "CIHI alias already present (matched by raw name)."
      )
      next
    }
    
    entry <- make_cihi_alias_entry(
      cihi_name_raw = r$cihi_name_raw,
      cihi_name_norm = r$cihi_name_norm,
      cihi_type = r$cihi_type,
      match_status = r$match_status,
      override_reason = r$override_reason,
      notes = r$notes,
      source_year = if (is.na(CIHI_SOURCE_YEAR)) NA_integer_ else as.integer(CIHI_SOURCE_YEAR)
    )
    
    # Append
    obj$aliases$cihi <- append(obj$aliases$cihi, list(entry))
    
    changes[[length(changes) + 1]] <- tibble(
      fac = fac_i,
      yaml_path = path_i,
      cihi_name_raw = r$cihi_name_raw,
      action = "added",
      detail = paste0("Added CIHI alias; normalized='", r$cihi_name_norm, "'")
    )
  }
  
  # Write back
  if (!DRY_RUN) {
    write_yaml_pretty(obj, path_i)
  }
}

report <- bind_rows(changes) %>% arrange(action, fac, cihi_name_raw)

# ----------------------------
# OUTPUT REPORT + SUMMARY
# ----------------------------
dir.create("outputs", showWarnings = FALSE)
write_csv(report, OUT_REPORT)

summary_tbl <- report %>%
  count(action, name = "n") %>%
  arrange(desc(n))

cat("\n==============================\n")
cat("PHASE G2: CIHI ALIAS ENRICHMENT\n")
cat("==============================\n")
cat("YAML_DIR:   ", YAML_DIR, "\n", sep = "")
cat("MAP_FINAL:  ", MAP_FINAL, "\n", sep = "")
cat("DRY_RUN:    ", DRY_RUN, "\n", sep = "")
if (!DRY_RUN) cat("Backup:     ", backup_dir, "\n", sep = "")
cat("Report:     ", OUT_REPORT, "\n", sep = "")
cat("\nActions summary:\n")
print(summary_tbl, n = nrow(summary_tbl))

# Optional guardrail: how many FACs got at least one CIHI alias added
fac_added <- report %>% filter(action == "added") %>% distinct(fac) %>% nrow()
cat("\nFACs with >=1 CIHI alias added:", fac_added, "\n")

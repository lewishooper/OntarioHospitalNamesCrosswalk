# Phase D: Generate Canonical Hospital YAML files (one per FAC)
# Project: OntarioHospitalNamesCrosswalk

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(yaml)
  library(purrr)
})

# ----------------------------
# CONFIG
# ----------------------------
IN_PATH <- file.path("outputs", "salary_alias_feed_2024_moh_enriched_unsafe.csv")

OUT_DIR <- file.path("code", "canonical_hospitals")
OUT_INDEX <- file.path("outputs", "canonical_hospitals_index.csv") # light index for quick review

YEAR_OBSERVED <- 2024
SOURCE_SYSTEM <- "sunshine"

# ----------------------------
# HELPERS
# ----------------------------
slugify <- function(x, max_len = 60) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  ifelse(nchar(x) > max_len, substr(x, 1, max_len), x)
}

# YAML-friendly alias list: strings or {name, unsafe}
alias_item <- function(name_raw, unsafe) {
  if (isTRUE(unsafe)) {
    list(name = name_raw, unsafe = TRUE)
  } else {
    # keep as list(name=...) for consistent schema (simpler downstream)
    list(name = name_raw)
  }
}

write_yaml_file <- function(path, obj) {
  # Ensure 2-space indent and stable formatting
  yml <- yaml::as.yaml(
    obj,
    indent.mapping.sequence = TRUE,
    indent = 2,
    line.sep = "\n"
  )
  writeLines(yml, con = path, useBytes = TRUE)
}

# ----------------------------
# LOAD
# ----------------------------
if (!file.exists(IN_PATH)) {
  stop("Input not found: ", IN_PATH, "\nRun Phase C first.")
}

df <- read_csv(IN_PATH, show_col_types = FALSE) %>%
  mutate(
    fac = as.character(fac),
    fac_before_correction = if ("fac_before_correction" %in% names(.)) as.character(fac_before_correction) else NA_character_,
    moh_name = if ("moh_name" %in% names(.)) as.character(moh_name) else NA_character_,
    moh_type = if ("moh_type" %in% names(.)) as.character(moh_type) else NA_character_,
    name_raw = as.character(name_raw),
    unsafe = if ("unsafe" %in% names(.)) as.logical(unsafe) else FALSE
  )

# Basic validations
stopifnot(all(c("fac", "name_raw") %in% names(df)))

# ----------------------------
# BUILD YAML OBJECTS PER FAC
# ----------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# Aggregate per FAC
by_fac <- df %>%
  filter(!is.na(fac), fac != "", !is.na(name_raw), name_raw != "") %>%
  group_by(fac) %>%
  summarise(
    moh_name = dplyr::first(na.omit(moh_name)),
    moh_type = dplyr::first(na.omit(moh_type)),
    
    # Keep all aliases, distinct on exact string
    aliases_tbl = list(dplyr::distinct(dplyr::pick(name_raw, unsafe))),
    
    # Prior FACs: only keep those that differ from the corrected FAC for this group
    fac_sources = list({
      this_fac <- dplyr::first(fac)
      dplyr::pick(fac_before_correction) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(fac_before_correction),
                      fac_before_correction != "",
                      fac_before_correction != this_fac)
    }),
    
    .groups = "drop"
  ) %>%
  mutate(
    moh_name = ifelse(is.na(moh_name) | moh_name == "", NA_character_, moh_name),
    moh_type = ifelse(is.na(moh_type) | moh_type == "", NA_character_, moh_type)
  )

# ----------------------------
# WRITE YAML FILES
# ----------------------------
index_rows <- list()

for (i in seq_len(nrow(by_fac))) {
  fac_i <- by_fac$fac[[i]]
  moh_name_i <- by_fac$moh_name[[i]]
  moh_type_i <- by_fac$moh_type[[i]]
  
  aliases_tbl <- by_fac$aliases_tbl[[i]]
  aliases_list <- purrr::pmap(
    list(aliases_tbl$name_raw, aliases_tbl$unsafe),
    ~ alias_item(..1, ..2)
  )
  
  prior_facs <- by_fac$fac_sources[[i]]
  prior_fac_list <- if (nrow(prior_facs) > 0) as.character(prior_facs$fac_before_correction) else character(0)
  
  # Build the YAML object
  yobj <- list(
    fac = fac_i,
    canonical_name = moh_name_i,
    moh_type = moh_type_i,
    aliases = list(
      sunshine = aliases_list
    )
  )
  
  # Only include prior_facs if there were corrections; keeps YAML clean
  if (length(prior_fac_list) > 0) {
    yobj$prior_facs <- prior_fac_list
  }
  
  # File naming: FAC + slug of MOH name if available else first alias
  base_name <- if (!is.na(moh_name_i) && moh_name_i != "") moh_name_i else aliases_tbl$name_raw[[1]]
  slug <- slugify(base_name)
  
  out_file <- file.path(OUT_DIR, paste0(fac_i, "_", slug, ".yaml"))
  write_yaml_file(out_file, yobj)
  
  index_rows[[length(index_rows) + 1]] <- tibble::tibble(
    fac = fac_i,
    canonical_name = moh_name_i,
    moh_type = moh_type_i,
    yaml_file = out_file,
    n_aliases = nrow(aliases_tbl),
    n_unsafe = sum(aliases_tbl$unsafe, na.rm = TRUE),
    has_prior_facs = length(prior_fac_list) > 0
  )
}

index_df <- dplyr::bind_rows(index_rows) %>%
  arrange(fac)

write_csv(index_df, OUT_INDEX)

cat("\n==============================\n")
cat("PHASE D: YAML GENERATION SUMMARY\n")
cat("==============================\n")
cat("YAML directory: ", OUT_DIR, "\n", sep = "")
cat("Files written:  ", nrow(index_df), "\n", sep = "")
cat("Index written:  ", OUT_INDEX, "\n", sep = "")
cat("Total aliases:  ", sum(index_df$n_aliases), "\n", sep = "")
cat("Total unsafe:   ", sum(index_df$n_unsafe), "\n", sep = "")

# Phase G (Revised): CIHI Reporting Identifier -> FAC Mapping
# CIHI raw/clean name is the identifier; normalization is aid only.
#
# Outputs:
# - outputs/phaseG_cihi_rows_prepared.csv
# - outputs/phaseG_cihi_fac_map_final.csv
# - outputs/phaseG_cihi_unmapped_intentional.csv
# - outputs/phaseG_cihi_mapping_summary.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(yaml)
  library(tibble)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------
# CONFIG
# ----------------------------
YAML_DIR <- file.path("code", "canonical_hospitals")
CIHI_PATH <- file.path("Sources", "CIHI.csv")

# Overrides keyed on CIHI cleaned raw name
OVERRIDES_PATH <- file.path("Sources", "cihi_fac_overrides.csv")
OVERRIDE_KEY_COL <- "cihi_name_raw_clean"  # or "cihi_name_raw" if you prefer

# Outputs
OUT_PREP   <- file.path("outputs", "phaseG_cihi_rows_prepared.csv")
OUT_FINAL  <- file.path("outputs", "phaseG_cihi_fac_map_final.csv")
OUT_UNMAP  <- file.path("outputs", "phaseG_cihi_unmapped_intentional.csv")
OUT_SUM    <- file.path("outputs", "phaseG_cihi_mapping_summary.csv")

dir.create("outputs", showWarnings = FALSE)

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

normalize_name <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- str_replace_all(x, "\u00A0", " ")
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- str_squish(x)
  x <- na_if(x, "")
  x
}

clean_cihi_name <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_squish(x)
  
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }
  
  # Keep RHS of "A — B" style labels (CIHI sometimes repeats)
  has_dash <- str_detect(x, "\\s[—–-]\\s")
  x <- ifelse(has_dash, str_trim(str_replace(x, "^.*\\s[—–-]\\s", "")), x)
  x <- str_squish(x)
  
  # Remove bracket qualifiers
  x <- str_remove_all(x, "\\s*\\[[^\\]]*\\]\\s*")
  x <- str_squish(x)
  
  # Remove trailing province marker at end
  x <- str_remove(x, regex("\\s*\\((ont\\.?|on\\.?|ontario)\\)\\s*$", ignore_case = TRUE))
  x <- str_squish(x)
  
  # Remove trailing legal suffix at end
  x <- str_remove(x, regex("\\s+(corporation|corp\\.?|incorporated|inc\\.?)\\s*$", ignore_case = TRUE))
  x <- str_squish(x)
  
  x <- na_if(x, "")
  x
}

read_one_fac_yaml <- function(path) {
  obj <- yaml::read_yaml(path)
  fac <- as.character(obj$fac %||% NA_character_)
  canonical_name <- as.character(obj$canonical_name %||% NA_character_)
  list(fac = fac, canonical_name = canonical_name, path = path)
}

load_overrides <- function(path, key_col = "cihi_name_raw_clean") {
  if (!file.exists(path)) stop("Overrides not found: ", path)
  
  ov <- readr::read_csv(path, show_col_types = FALSE) %>% standardize_colnames()
  
  if (!key_col %in% names(ov)) stop("Overrides missing key column: ", key_col)
  required <- c(key_col, "fac_set", "override_reason")
  missing <- setdiff(required, names(ov))
  if (length(missing) > 0) stop("Overrides missing columns: ", paste(missing, collapse = ", "))
  
  bad <- ov %>% filter(is.na(.data[[key_col]]) | str_trim(.data[[key_col]]) == "")
  if (nrow(bad) > 0) stop("Overrides has blank key values in: ", key_col)
  
  badr <- ov %>% filter(is.na(override_reason) | str_trim(override_reason) == "")
  if (nrow(badr) > 0) stop("Overrides has blank override_reason for: ", paste(badr[[key_col]], collapse = ", "))
  
  ov %>%
    transmute(
      override_key = str_squish(as.character(.data[[key_col]])),
      fac_set = str_replace_all(str_squish(as.character(fac_set)), "\\s+", ""),
      override_reason = str_squish(as.character(override_reason)),
      notes = if ("notes" %in% names(ov)) str_squish(as.character(notes)) else NA_character_
    ) %>%
    mutate(
      fac = str_split(fac_set, "\\|")
    ) %>%
    tidyr::unnest(fac) %>%
    mutate(
      fac = str_replace_all(str_squish(as.character(fac)), "[^0-9]", ""),
      fac = na_if(fac, ""),
      is_multi_fac = str_detect(fac_set, "\\|")
    ) %>%
    filter(!is.na(fac))
}

# ----------------------------
# STEP 0: FAC universe (for QA)
# ----------------------------
yaml_files <- list.files(YAML_DIR, pattern = "\\.ya?ml$", full.names = TRUE)
if (length(yaml_files) == 0) stop("No FAC YAMLs found in: ", YAML_DIR)

fac_universe <- purrr::map_dfr(yaml_files, read_one_fac_yaml) %>%
  distinct(fac)

# ----------------------------
# STEP 1: Load CIHI and prepare identifiers
# ----------------------------
if (!file.exists(CIHI_PATH)) stop("CIHI file not found: ", CIHI_PATH)

cihi_raw <- readr::read_csv(CIHI_PATH, show_col_types = FALSE) %>% as.data.frame()
cihi <- cihi_raw %>% standardize_colnames()

# Auto-detect columns
name_col <- intersect(c("hospitalname","hospital_name","hospital","name","organization_name"), names(cihi))
if (length(name_col) == 0) stop("Cannot find CIHI name column in CIHI.csv")
name_col <- name_col[[1]]

type_col <- intersect(c("cihi_type","type","facility_type","peer_group"), names(cihi))
type_col <- if (length(type_col) == 0) NA_character_ else type_col[[1]]

cihi2 <- cihi %>%
  transmute(
    cihi_row_id = row_number(),
    cihi_name_raw = as.character(.data[[name_col]]),
    cihi_type = if (!is.na(type_col)) as.character(.data[[type_col]]) else NA_character_
  ) %>%
  mutate(
    cihi_name_raw_clean = clean_cihi_name(cihi_name_raw),
    cihi_name_norm = normalize_name(cihi_name_raw_clean)
  ) %>%
  filter(!is.na(cihi_name_raw), str_trim(cihi_name_raw) != "") %>%
  filter(!is.na(cihi_name_raw_clean), str_trim(cihi_name_raw_clean) != "")

write_csv(cihi2, OUT_PREP)

# ----------------------------
# STEP 2: Apply overrides by CIHI reporting identifier (raw_clean by default)
# ----------------------------
overrides <- load_overrides(OVERRIDES_PATH, key_col = OVERRIDE_KEY_COL)

# QA: overrides FACs must exist
bad_fac <- overrides %>%
  anti_join(fac_universe, by = "fac") %>%
  distinct(fac)

if (nrow(bad_fac) > 0) {
  stop("Overrides contain FACs not in FAC YAML universe: ", paste(bad_fac$fac, collapse = ", "))
}

# Build join key in CIHI to match overrides
cihi_keyed <- cihi2 %>%
  mutate(
    override_key = if (OVERRIDE_KEY_COL == "cihi_name_raw") cihi_name_raw else cihi_name_raw_clean,
    override_key = str_squish(override_key)
  )

mapped <- cihi_keyed %>%
  inner_join(overrides, by = "override_key") %>%
  transmute(
    cihi_row_id,
    cihi_name_raw,
    cihi_name_raw_clean,
    cihi_name_norm,
    cihi_type,
    fac,
    fac_set,
    override_reason,
    notes,
    match_status = "override_reporting_id",
    is_multi_fac
  ) %>%
  distinct(cihi_row_id, fac, .keep_all = TRUE)

# ----------------------------
# STEP 3: Identify any remaining unmapped CIHI rows
# (No longer called "unmatched" — they are either missing overrides or out-of-scope)
# ----------------------------
unmapped <- cihi2 %>%
  anti_join(mapped %>% distinct(cihi_row_id), by = "cihi_row_id") %>%
  transmute(
    cihi_row_id,
    cihi_name_raw,
    cihi_name_raw_clean,
    cihi_name_norm,
    cihi_type,
    status = "unmapped_intentional",
    notes = "No override provided for this CIHI reporting name in this extract."
  )

# ----------------------------
# STEP 4: Outputs + summary
# ----------------------------
write_csv(mapped, OUT_FINAL)
write_csv(unmapped, OUT_UNMAP)

summary_tbl <- tibble(
  metric = c(
    "cihi_rows_loaded",
    "cihi_rows_prepared",
    "override_rows_output",
    "unique_cihi_rows_mapped",
    "unmapped_intentional_rows"
  ),
  value = c(
    nrow(cihi_raw),
    nrow(cihi2),
    nrow(mapped),
    n_distinct(mapped$cihi_row_id),
    nrow(unmapped)
  )
)

write_csv(summary_tbl, OUT_SUM)

cat("\n==============================\n")
cat("PHASE G (REVISED): CIHI Reporting Identifier -> FAC\n")
cat("==============================\n")
print(summary_tbl)

cat("\nWrote outputs:\n")
cat(" - ", OUT_PREP, "\n", sep="")
cat(" - ", OUT_FINAL, "\n", sep="")
cat(" - ", OUT_UNMAP, "\n", sep="")
cat(" - ", OUT_SUM, "\n", sep="")

# Phase G (Option B): CIHI -> FAC mapping
# - CIHI raw name is reporting identifier
# - Normalize is matching aid only
# - Auto exact match first (safe names only)
# - Export unmatched + overrides template
# - If overrides present, apply overrides (authoritative) and finalize
getwd()
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
OVERRIDE_KEY_COL <- "cihi_name_raw_clean"

# Outputs (run artifacts)
OUT_PREP      <- file.path("outputs", "phaseG_cihi_rows_prepared.csv")
OUT_CANON     <- file.path("outputs", "phaseG_fac_name_universe_safe.csv")
OUT_EXACT     <- file.path("outputs", "phaseG_cihi_fac_auto_exact.csv")
OUT_AMBIG     <- file.path("outputs", "phaseG_cihi_fac_auto_ambiguous.csv")
OUT_UNMATCHED <- file.path("outputs", "phaseG_cihi_fac_unmatched.csv")
OUT_OVTEMPLATE<- file.path("outputs", "phaseG_cihi_fac_overrides_template.csv")

# Final outputs (after overrides)
OUT_OVAPPLIED <- file.path("outputs", "phaseG_cihi_fac_overrides_applied.csv")
OUT_FINAL     <- file.path("outputs", "phaseG_cihi_fac_map_final.csv")
OUT_UNMAPPED  <- file.path("outputs", "phaseG_cihi_fac_unmapped_intentional.csv")
OUT_SUMMARY   <- file.path("outputs", "phaseG_cihi_fac_mapping_summary.csv")

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
  
  # Keep RHS of "A — B" style labels
  has_dash <- str_detect(x, "\\s[—–-]\\s")
  x <- ifelse(has_dash, str_trim(str_replace(x, "^.*\\s[—–-]\\s", "")), x)
  x <- str_squish(x)
  
  # Remove bracket qualifiers
  x <- str_remove_all(x, "\\s*\\[[^\\]]*\\]\\s*")
  x <- str_squish(x)
  
  # Remove trailing province markers at end only
  x <- str_remove(x, regex("\\s*\\((ont\\.?|on\\.?|ontario)\\)\\s*$", ignore_case = TRUE))
  x <- str_squish(x)
  
  # Remove trailing legal suffix at end only
  x <- str_remove(x, regex("\\s+(corporation|corp\\.?|incorporated|inc\\.?)\\s*$", ignore_case = TRUE))
  x <- str_squish(x)
  
  x <- na_if(x, "")
  x
}

read_one_yaml <- function(path) {
  obj <- yaml::read_yaml(path)
  fac <- as.character(obj$fac %||% NA_character_)
  canonical_name <- as.character(obj$canonical_name %||% NA_character_)
  sunshine <- obj$aliases$sunshine %||% NULL
  
  alias_tbl <- tibble()
  if (!is.null(sunshine) && length(sunshine) > 0) {
    alias_tbl <- purrr::map_dfr(sunshine, function(item) {
      if (is.character(item) && length(item) == 1) {
        tibble(alias_raw = item, unsafe = FALSE)
      } else {
        tibble(
          alias_raw = item$name %||% NA_character_,
          unsafe = isTRUE(item$unsafe)
        )
      }
    }) %>% filter(!is.na(alias_raw), str_trim(alias_raw) != "")
  }
  
  list(fac = fac, canonical_name = canonical_name, alias_tbl = alias_tbl)
}

load_overrides <- function(path, key_col = "cihi_name_raw_clean") {
  if (!file.exists(path)) return(NULL)
  
  ov <- readr::read_csv(path, show_col_types = FALSE) %>% standardize_colnames()
  
  required <- c(key_col, "fac_set", "override_reason")
  missing <- setdiff(required, names(ov))
  if (length(missing) > 0) stop("Overrides missing columns: ", paste(missing, collapse = ", "))
  
  # Enforce explicit override_reason (your governance choice)
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
    filter(!is.na(override_key), override_key != "", !is.na(fac))
}

# ----------------------------
# STEP 1: Build FAC safe name universe from YAMLs
# ----------------------------
yaml_files <- list.files(YAML_DIR, pattern = "\\.ya?ml$", full.names = TRUE)
if (length(yaml_files) == 0) stop("No YAML files found in: ", YAML_DIR)

parsed <- purrr::map(yaml_files, read_one_yaml)

canon_tbl <- purrr::imap_dfr(parsed, function(x, i) {
  fac_i <- x$fac
  path_i <- yaml_files[[i]]
  
  canonical_rows <- tibble(
    fac = fac_i,
    source = "moh_canonical",
    name_raw = x$canonical_name,
    name_norm = normalize_name(x$canonical_name),
    unsafe = FALSE,
    yaml_path = path_i
  ) %>% filter(!is.na(name_norm), name_norm != "")
  
  alias_rows <- x$alias_tbl %>%
    mutate(
      fac = fac_i,
      source = "sunshine_alias",
      name_raw = alias_raw,
      name_norm = normalize_name(alias_raw),
      yaml_path = path_i
    ) %>%
    filter(!is.na(name_norm), name_norm != "") %>%
    filter(!unsafe) %>%
    transmute(fac, source, name_raw, name_norm, unsafe, yaml_path)
  
  bind_rows(canonical_rows, alias_rows)
}) %>%
  distinct(fac, name_norm, .keep_all = TRUE)

write_csv(canon_tbl, OUT_CANON)

# ----------------------------
# STEP 2: Load CIHI and prepare identifiers
# ----------------------------
if (!file.exists(CIHI_PATH)) stop("CIHI file not found: ", CIHI_PATH)

cihi_raw <- readr::read_csv(CIHI_PATH, show_col_types = FALSE) %>% as.data.frame()
cihi <- cihi_raw %>% standardize_colnames()

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
  filter(!is.na(cihi_name_raw_clean), str_trim(cihi_name_raw_clean) != "") %>%
  filter(!is.na(cihi_name_norm), cihi_name_norm != "")

write_csv(cihi2, OUT_PREP)

# ----------------------------
# STEP 3: Automated exact normalized match (safe only)
# ----------------------------
auto_matches <- cihi2 %>%
  inner_join(
    canon_tbl %>% select(fac, source, name_raw, name_norm),
    by = c("cihi_name_norm" = "name_norm")
  ) %>%
  rename(
    matched_name_raw = name_raw,
    matched_source = source
  )

auto_summary <- auto_matches %>%
  group_by(cihi_row_id, cihi_name_raw, cihi_name_raw_clean, cihi_type, cihi_name_norm) %>%
  summarise(
    n_fac = n_distinct(fac),
    facs = paste(sort(unique(fac)), collapse = "|"),
    matched_sources = paste(sort(unique(matched_source)), collapse = "|"),
    matched_names = paste(sort(unique(matched_name_raw)), collapse = " || "),
    .groups = "drop"
  )

exact <- auto_summary %>%
  filter(n_fac == 1) %>%
  transmute(
    cihi_row_id,
    cihi_name_raw,
    cihi_name_raw_clean,
    cihi_name_norm,
    cihi_type,
    fac_set = facs,
    match_status = "auto_exact_norm",
    matched_sources,
    matched_names
  )

ambig <- auto_summary %>%
  filter(n_fac > 1) %>%
  transmute(
    cihi_row_id,
    cihi_name_raw,
    cihi_name_raw_clean,
    cihi_name_norm,
    cihi_type,
    fac_set = facs,
    n_fac,
    match_status = "auto_ambiguous_norm",
    matched_sources,
    matched_names
  )

unmatched <- cihi2 %>%
  anti_join(auto_summary %>% select(cihi_row_id), by = "cihi_row_id") %>%
  transmute(
    cihi_row_id,
    cihi_name_raw,
    cihi_name_raw_clean,
    cihi_name_norm,
    cihi_type,
    match_status = "unmatched"
  )

write_csv(exact, OUT_EXACT)
write_csv(ambig, OUT_AMBIG)
write_csv(unmatched, OUT_UNMATCHED)

# Overrides template from unmatched (you will edit this into Sources/cihi_fac_overrides.csv)
override_template <- unmatched %>%
  transmute(
    cihi_name_raw_clean = cihi_name_raw_clean,
    fac_set = "",
    override_reason = "",
    notes = paste0("CIHI raw: ", cihi_name_raw)
  )

write_csv(override_template, OUT_OVTEMPLATE)

# ----------------------------
# STEP 4: Apply overrides (if present) and finalize
# ----------------------------
# ----------------------------
# STEP 4: Apply overrides (if present) and finalize
# ----------------------------
# If overrides are missing, use an empty tibble with the expected schema
ov <- load_overrides(OVERRIDES_PATH, key_col = OVERRIDE_KEY_COL)

if (is.null(ov)) {
  ov <- tibble::tibble(
    override_key     = character(),  # REQUIRED for the join
    fac_set          = character(),
    override_reason  = character(),
    notes            = character()
  )
}

cat("\n[DEBUG] STEP 4 reached\n")
cat("[DEBUG] OVERRIDES_PATH: ", OVERRIDES_PATH, "\n", sep = "")
cat("[DEBUG] file.exists(OVERRIDES_PATH): ", file.exists(OVERRIDES_PATH), "\n", sep = "")
cat("[DEBUG] ov is NULL: ", is.null(ov), "\n", sep = "")
flush.console()

if (is.null(ov)) {
  
  # 1) Remove message(); use cat() only (lower-risk)
  cat("[DEBUG] In NULL overrides branch\n")
  flush.console()
  
  # 2) Avoid tibble() initially; use base data.frame
  summary_tbl <- data.frame(
    metric = c("cihi_rows_prepared","auto_exact","auto_ambiguous","unmatched","overrides_applied","final_mapped_rows"),
    value  = c(nrow(cihi2), nrow(exact), nrow(ambig), nrow(unmatched), 0L, 0L),
    stringsAsFactors = FALSE
  )
  
  cat("[DEBUG] summary_tbl created, nrow=", nrow(summary_tbl), "\n", sep = "")
  flush.console()
  
  # 3) Validate output path is sane and writable before write_csv
  cat("[DEBUG] OUT_SUMMARY: ", OUT_SUMMARY, "\n", sep = "")
  cat("[DEBUG] OUT_SUMMARY dir exists: ", dir.exists(dirname(OUT_SUMMARY)), "\n", sep = "")
  flush.console()
  
  # Write using base write.csv first (lower-risk) to isolate readr issues
  utils::write.csv(summary_tbl, OUT_SUMMARY, row.names = FALSE)
  
  cat("[DEBUG] wrote OUT_SUMMARY via write.csv\n")
  flush.console()
  
  # IMPORTANT: stop cleanly (do not quit the session)
  cat("[DEBUG] Exiting script early because overrides missing.\n")

  cat("[INFO] Overrides not found. Pass 1 complete.\n")
  cat("[INFO] Template written to: ", OUT_OVTEMPLATE, "\n", sep = "")
  
  
}


# Key CIHI rows by clean reporting identifier
cihi_keyed <- cihi2 %>%
  mutate(override_key = str_squish(cihi_name_raw_clean))
if (nrow(ov) == 0) {
  ov_applied <- tibble::tibble(
    cihi_row_id = numeric(),
    cihi_name_raw = character(),
    cihi_name_raw_clean = character(),
    cihi_name_norm = character(),
    cihi_type = character(),
    fac = character(),
    fac_set = character(),
    override_reason = character(),
    notes = character(),
    match_status = character(),
    is_multi_fac = logical()
  )
} else {
  ov_applied <- cihi_keyed %>%
    inner_join(ov, by = "override_key") %>%
    transmute(
      cihi_row_id,
      cihi_name_raw,
      cihi_name_raw_clean,
      cihi_name_norm,
      cihi_type,
      fac = fac,
      fac_set,
      override_reason,
      notes,
      match_status = "override_reporting_id",
      is_multi_fac
    ) %>%
    distinct(cihi_row_id, fac, .keep_all = TRUE)
}


write_csv(ov_applied, OUT_OVAPPLIED)

# Combine: overrides take precedence over auto exact
# (If an override exists for a CIHI row, we keep override mapping and drop auto mapping for that row)
auto_exact_facrows <- exact %>%
  mutate(fac = str_split(fac_set, "\\|")) %>%
  unnest(fac) %>%
  transmute(
    cihi_row_id,
    cihi_name_raw,
    cihi_name_raw_clean,
    cihi_name_norm,
    cihi_type,
    fac,
    fac_set,
    match_status,
    override_reason = NA_character_,
    notes = NA_character_
  )

final_map <- bind_rows(
  ov_applied %>% select(names(auto_exact_facrows), is_multi_fac = is_multi_fac),
  auto_exact_facrows %>% mutate(is_multi_fac = FALSE)
) %>%
  # drop auto rows where override exists
  anti_join(ov_applied %>% distinct(cihi_row_id), by = "cihi_row_id") %>%
  bind_rows(ov_applied %>% select(names(auto_exact_facrows), is_multi_fac = is_multi_fac)) %>%
  distinct(cihi_row_id, fac, .keep_all = TRUE) %>%
  arrange(cihi_row_id, fac)

# Remaining unmapped after overrides + auto exact
unmapped_after <- cihi2 %>%
  anti_join(final_map %>% distinct(cihi_row_id), by = "cihi_row_id") %>%
  transmute(
    cihi_row_id,
    cihi_name_raw,
    cihi_name_raw_clean,
    cihi_name_norm,
    cihi_type,
    status = "unmapped_intentional",
    notes = "No override; no auto exact match (safe)."
  )

write_csv(final_map, OUT_FINAL)
write_csv(unmapped_after, OUT_UNMAPPED)

summary_tbl <- tibble(
  metric = c(
    "cihi_rows_prepared",
    "auto_exact_rows",
    "auto_ambiguous_rows",
    "unmatched_before_overrides",
    "override_rows_output",
    "unique_cihi_rows_mapped_final",
    "unmapped_after_overrides"
  ),
  value = c(
    nrow(cihi2),
    nrow(exact),
    nrow(ambig),
    nrow(unmatched),
    nrow(ov_applied),
    n_distinct(final_map$cihi_row_id),
    nrow(unmapped_after)
  )
)

write_csv(summary_tbl, OUT_SUMMARY)

cat("\n==============================\n")
cat("PHASE G (Option B): CIHI -> FAC (AUTO + OVERRIDES)\n")
cat("==============================\n")
print(summary_tbl)

cat("\nWrote outputs:\n")
cat(" - ", OUT_PREP, "\n", sep="")
cat(" - ", OUT_CANON, "\n", sep="")
cat(" - ", OUT_EXACT, "\n", sep="")
cat(" - ", OUT_AMBIG, "\n", sep="")
cat(" - ", OUT_UNMATCHED, "\n", sep="")
cat(" - ", OUT_OVTEMPLATE, "\n", sep="")
cat(" - ", OUT_OVAPPLIED, "\n", sep="")
cat(" - ", OUT_FINAL, "\n", sep="")
cat(" - ", OUT_UNMAPPED, "\n", sep="")
cat(" - ", OUT_SUMMARY, "\n", sep="")

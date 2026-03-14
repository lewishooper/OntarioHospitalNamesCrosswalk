# 03_detect_unsafe_aliases.R
# Phase 3: Detect and flag unsafe aliases in fac_aliases (sunshine source)
# Project: OntarioHospitalNamesCrosswalk
#
# Input:  outputs/fac_aliases.rds
# Output: outputs/fac_aliases.rds (updated with unsafe flags)
#         outputs/fac_aliases.csv (updated)
#         outputs/validation/unsafe_aliases_2024.csv (audit report)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source(file.path("code", "utils", "normalize.R"))

# ----------------------------
# CONFIG
# ----------------------------
IN_PATH  <- file.path("outputs", "fac_aliases.rds")
OUT_RDS  <- file.path("outputs", "fac_aliases.rds")
OUT_CSV  <- file.path("outputs", "fac_aliases.csv")
OUT_BAD  <- file.path("outputs", "validation", "unsafe_aliases_2024.csv")

PAT_CORPORATE <- c(
  "HEALTH NETWORK", "HEALTHCARE GROUP", "HEALTHCARE NETWORK",
  "NETWORK", "CORPORATION", "INCORPORATED", "FOUNDATION",
  "SOCIETY", "ASSOCIATION", "RELIGIOUS HOSPITALLERS",
  "HOSPITALLERS", "RELIGIEUSES", "RELIGIEUSES HOSPITALIERES",
  "CATHOLIC", "DIOCESE", "ORDER OF"
)

PAT_RELIGIOUS_GENERIC <- c(
  "HOTEL DIEU", "ST JOSEPH", "SAINT JOSEPH",
  "ST MARY", "SAINT MARY", "ST MICHAEL", "SAINT MICHAEL"
)

GENERIC_ONLY_NAMES <- c(
  "HOSPITAL", "GENERAL HOSPITAL", "MEMORIAL HOSPITAL",
  "DISTRICT HOSPITAL", "REGIONAL HEALTH CENTRE",
  "REGIONAL HEALTH CENTER", "HEALTH CENTRE", "HEALTH CENTER",
  "HOSPITAL CENTRE", "HOSPITAL CENTER"
)

PAT_GENERIC_FACILITY <- c("GENERAL HOSPITAL", "MEMORIAL HOSPITAL")

MIN_DISTINCT_TOKENS <- 2L

# ----------------------------
# HELPERS
# ----------------------------
strip_generic_tokens <- function(x) {
  x <- str_replace_all(
    x,
    "\\b(HOSPITAL|HOPITAL|CENTRE|CENTER|HEALTH|SANTE|REGIONAL|REGIONALE|
     DISTRICT|CLINIC|CLINIQUE|THE)\\b", " "
  )
  str_squish(x)
}

count_tokens <- function(x) {
  if (is.na(x) || x == "") return(0L)
  length(str_split(x, "\\s+")[[1]])
}

has_any_pattern_vec <- function(x, patterns) {
  x <- ifelse(is.na(x), "", x)
  if (length(patterns) == 0) return(rep(FALSE, length(x)))
  str_detect(x, paste0("(", paste(patterns, collapse = "|"), ")"))
}

# ----------------------------
# LOAD
# ----------------------------
if (!file.exists(IN_PATH)) stop("fac_aliases not found — run 02 first.")

aliases <- readRDS(IN_PATH)

# Only apply unsafe detection to sunshine aliases
sunshine <- aliases |> filter(source_system == "sunshine")
other    <- aliases |> filter(source_system != "sunshine")

# ----------------------------
# UNSAFE DETECTION (sunshine only)
# ----------------------------
df <- sunshine |>
  mutate(
    name_norm_stripped  = strip_generic_tokens(name_norm),
    token_count_stripped = vapply(name_norm_stripped, count_tokens, integer(1))
  )

# Rule A: collision across FACs
collisions <- df |>
  filter(!is.na(name_norm)) |>
  distinct(fac, name_norm) |>
  count(name_norm, name = "n_fac") |>
  filter(n_fac > 1)

df <- df |>
  left_join(collisions, by = "name_norm") |>
  mutate(
    flag_collision         = !is.na(n_fac) & n_fac > 1,
    flag_corporate         = has_any_pattern_vec(name_norm, PAT_CORPORATE),
    flag_religious_generic = has_any_pattern_vec(name_norm, PAT_RELIGIOUS_GENERIC),
    flag_generic_facility  = has_any_pattern_vec(name_norm, PAT_GENERIC_FACILITY) &
      token_count_stripped < MIN_DISTINCT_TOKENS,
    flag_generic_only      = !is.na(name_norm) & name_norm %in% GENERIC_ONLY_NAMES
  ) |>
  mutate(
    unsafe = flag_collision | flag_corporate | flag_religious_generic |
      flag_generic_facility | flag_generic_only,
    unsafe_reasons = paste0(
      if_else(flag_collision,         "collision_across_fac|",           ""),
      if_else(flag_corporate,         "corporate_or_network_entity|",    ""),
      if_else(flag_religious_generic, "religious_generic_name|",         ""),
      if_else(flag_generic_facility,  "generic_facility_unqualified|",   ""),
      if_else(flag_generic_only,      "generic_only_name|",              "")
    ),
    unsafe_reasons = str_replace(unsafe_reasons, "\\|$", ""),
    unsafe_reasons = na_if(unsafe_reasons, "")
  ) |>
  select(-name_norm_stripped, -token_count_stripped,
         -n_fac, -starts_with("flag_"))

# ----------------------------
# RECOMBINE + WRITE
# ----------------------------
aliases_updated <- bind_rows(df, other) |>
  arrange(fac, source_system, name_raw)

dir.create(file.path("outputs", "validation"), showWarnings = FALSE)

saveRDS(aliases_updated, OUT_RDS)
readr::write_csv(aliases_updated, OUT_CSV)

unsafe_only <- aliases_updated |>
  filter(unsafe) |>
  select(fac, source_system, name_raw, name_norm, unsafe_reasons)

readr::write_csv(unsafe_only, OUT_BAD)

# ----------------------------
# SUMMARY
# ----------------------------
cat("\n==============================\n")
cat("PHASE 3: UNSAFE ALIAS DETECTION\n")
cat("==============================\n")
cat("Total aliases:       ", nrow(aliases_updated), "\n")
cat("Sunshine aliases:    ", nrow(df), "\n")
cat("Unsafe:              ", sum(aliases_updated$unsafe, na.rm = TRUE), "\n")
cat("Unsafe %:            ",
    round(100 * mean(df$unsafe, na.rm = TRUE), 1), "%\n")
cat("\nTop reasons:\n")
aliases_updated |>
  filter(unsafe, !is.na(unsafe_reasons)) |>
  count(unsafe_reasons, sort = TRUE) |>
  print(n = 20)

# Migration baseline check
BASELINE_UNSAFE <- 363L - 324L  # 39 unsafe in baseline
cat("\nBaseline unsafe count: ", BASELINE_UNSAFE, "\n")
cat("Current unsafe count:  ", sum(aliases_updated$unsafe, na.rm = TRUE), "\n")
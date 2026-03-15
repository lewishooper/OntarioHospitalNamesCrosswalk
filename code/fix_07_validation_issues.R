# fix_07_validation_issues.R
# One-time corrections identified by 07_validate_alias_table.R on 2026-03-15
#
# Fixes:
#   1. Remap orphan FAC 737 -> 974 in sactype_fac_overrides.csv
#   2. Delete orphan FAC 898 from sactype_fac_overrides.csv (no valid FAC exists)
#   3. Mark unsafe: wrong-hospital aliases from Category A collisions
#   4. Mark unsafe: ambiguous network/alliance names from Category B collisions

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# ----------------------------
# FIX 1 & 2 — Overrides file orphans
# ----------------------------
overrides <- read_csv(
  "sources/sactype_fac_overrides.csv",
  col_types = cols(fac = col_character()),
  show_col_types = FALSE
)

cat("Overrides before fix:", nrow(overrides), "rows\n")

overrides_fixed <- overrides |>
  mutate(fac = if_else(fac == "737", "974", fac)) |>  # remap McLaren -> North Bay
  filter(fac != "898")                                  # delete St Joseph's Toronto (no valid FAC)

cat("Overrides after fix: ", nrow(overrides_fixed), "rows\n")
cat("FAC 737 remapped to 974:",
    "974" %in% overrides_fixed$fac[overrides_fixed$sactype_name_clean ==
                                     "North Bay General Hospital - Mclaren Site"], "\n")
cat("FAC 898 removed:",
    !"898" %in% overrides_fixed$fac, "\n")

write_csv(overrides_fixed, "sources/sactype_fac_overrides.csv")

# ----------------------------
# FIX 3 & 4 — Mark unsafe aliases in fac_aliases
# ----------------------------
aliases <- readRDS("outputs/fac_aliases.rds")

cat("\nAliases before fix:", nrow(aliases), "\n")
cat("Unsafe before:     ", sum(aliases$unsafe), "\n")

# Category A: specific wrong-hospital matches (fac + name_norm combination)
category_a <- tribble(
  ~fac,  ~name_norm,
  "648", "HALIBURTON HIGHLANDS HEALTH SERVICES",   # Dunnville matched by mistake
  "813", "HURON PERTH HEALTHCARE ALLIANCE",         # Stratford General matched by mistake
  "957", "QUEENSWAY CARLETON HOSPITAL"              # Belleville Quinte matched by mistake
)

# Category B: ambiguous network names (all FACs, all sources)
category_b_norms <- c(
  "ALEXANDRA HOSPITAL ONT",
  "H PITAL DE MATTAWA HOSPITAL ONT",
  "HUMBER RIVER HEALTH ONT",
  "MICS GROUP OF HEALTH SERVICES ONT",
  "MIDDLESEX HOSPITAL ALLIANCE ONT",
  "NORTH WELLINGTON HEALTH CARE CORPORATION ONT",
  "SOUTH HURON HOSPITAL ONT"
)

aliases_fixed <- aliases |>
  mutate(unsafe = case_when(
    # Category A: wrong FAC + name combination
    paste(fac, name_norm) %in% paste(category_a$fac, category_a$name_norm) ~ TRUE,
    # Category B: ambiguous network names regardless of FAC
    name_norm %in% category_b_norms ~ TRUE,
    TRUE ~ unsafe
  ),
  unsafe_reasons = case_when(
    paste(fac, name_norm) %in% paste(category_a$fac, category_a$name_norm) ~
      "wrong_hospital_match",
    name_norm %in% category_b_norms ~
      "ambiguous_multi_fac_network_name",
    TRUE ~ unsafe_reasons
  ))

cat("Unsafe after:      ", sum(aliases_fixed$unsafe), "\n")
cat("Aliases total:     ", nrow(aliases_fixed), "(unchanged)\n")

# Spot check Category A
cat("\n--- Category A spot check ---\n")
aliases_fixed |>
  filter(fac %in% c("648", "813", "957"),
         name_norm %in% category_a$name_norm) |>
  select(fac, source_system, name_norm, unsafe, unsafe_reasons) |>
  print()

# Spot check Category B
cat("\n--- Category B spot check (first 5) ---\n")
aliases_fixed |>
  filter(name_norm %in% category_b_norms) |>
  select(fac, source_system, name_norm, unsafe, unsafe_reasons) |>
  head(5) |>
  print()

# ----------------------------
# WRITE
# ----------------------------
saveRDS(aliases_fixed, "outputs/fac_aliases.rds")
write_csv(aliases_fixed, "outputs/fac_aliases.csv")

cat("\nDone. Now re-run script 06 to rebuild statscan aliases with the corrected\n")
cat("overrides file, then re-run script 07 to confirm all FAILs clear.\n")
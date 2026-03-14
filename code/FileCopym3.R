# Step M2 — Copy baseline files
file.copy(
  file.path("outputs", "derived", "fac_aliases_all.csv"),
  file.path("outputs", "migration_baseline_aliases.csv")
)
file.copy(
  file.path("outputs", "derived", "fac_master.csv"),
  file.path("outputs", "migration_baseline_fac_master.csv")
)

# Step M3 — Archive YAML store
archive_dir <- file.path(
  "archive",
  paste0("canonical_hospitals_", format(Sys.Date(), "%Y%m%d"))
)
dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

yaml_files <- list.files(
  file.path("code", "canonical_hospitals"),
  pattern = "\\.ya?ml$",
  full.names = TRUE
)
file.copy(yaml_files, archive_dir)
file.remove(yaml_files)

cat("Archived", length(yaml_files), "YAML files to:", archive_dir, "\n")

# Step M4 — Confirm baseline numbers
baseline_aliases <- readr::read_csv(
  file.path("outputs", "migration_baseline_aliases.csv"),
  show_col_types = FALSE
)
baseline_master  <- readr::read_csv(
  file.path("outputs", "migration_baseline_fac_master.csv"),
  show_col_types = FALSE
)

cat("Baseline FACs:          ", nrow(baseline_master), "\n")
cat("Baseline aliases (all): ", nrow(baseline_aliases), "\n")
cat("Baseline aliases (safe):",
    sum(!baseline_aliases$unsafe, na.rm = TRUE), "\n")
cat("Source breakdown:\n")
print(table(baseline_aliases$source_system))
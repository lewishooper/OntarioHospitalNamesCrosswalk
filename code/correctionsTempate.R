# Create FAC correction template
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

out_path <- file.path("Sources", "FAC_Corrections.csv")
dir.create("Sources", showWarnings = FALSE)

template <- tibble::tibble(
  fac_original  = character(),
  fac_corrected = character(),
  reason        = character(),
  evidence      = character(),
  updated_at    = character()
)

write_csv(template, out_path)
cat("Wrote template: ", out_path, "\n", sep = "")
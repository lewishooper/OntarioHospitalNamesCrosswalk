# utils/override_loader.R
# Shared override file loader for OntarioHospitalNamesCrosswalk
# Source normalize.R before sourcing this file.

load_overrides <- function(path, key_col = "cihi_name_raw_clean") {
  if (!file.exists(path)) return(NULL)
  
  ov <- readr::read_csv(path, show_col_types = FALSE) |>
    standardize_colnames()
  
  required <- c(key_col, "fac_set", "override_reason")
  missing  <- setdiff(required, names(ov))
  if (length(missing) > 0) {
    stop("Override file missing required columns: ",
         paste(missing, collapse = ", "))
  }
  
  blank_reason <- ov |>
    dplyr::filter(is.na(override_reason) |
                    stringr::str_trim(override_reason) == "")
  if (nrow(blank_reason) > 0) {
    stop("Override file has blank override_reason for: ",
         paste(blank_reason[[key_col]], collapse = ", "))
  }
  
  ov |>
    dplyr::transmute(
      override_key    = stringr::str_squish(as.character(.data[[key_col]])),
      fac_set         = stringr::str_replace_all(
        stringr::str_squish(as.character(fac_set)), "\\s+", ""),
      override_reason = stringr::str_squish(as.character(override_reason)),
      notes           = if ("notes" %in% names(ov))
        stringr::str_squish(as.character(notes))
      else NA_character_
    ) |>
    dplyr::mutate(fac = stringr::str_split(fac_set, "\\|")) |>
    tidyr::unnest(fac) |>
    dplyr::mutate(
      fac          = stringr::str_replace_all(
        stringr::str_squish(as.character(fac)), "[^0-9]", ""),
      fac          = dplyr::na_if(fac, ""),
      is_multi_fac = stringr::str_detect(fac_set, "\\|")
    ) |>
    dplyr::filter(!is.na(override_key), override_key != "", !is.na(fac))
}
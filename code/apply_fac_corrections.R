apply_fac_corrections <- function(df, fac_col = "fac",
                                  corrections_path = file.path("Sources", "FAC_Corrections.csv")) {
  
  if (!fac_col %in% names(df)) {
    stop("FAC column not found in df: ", fac_col)
  }
  
  # Always normalize FAC first
  df[[fac_col]] <- clean_fac(df[[fac_col]])
  
  if (!file.exists(corrections_path)) {
    return(df)
  }
  
  corr <- readr::read_csv(corrections_path, show_col_types = FALSE) |>
    as.data.frame() |>
    standardize_colnames()
  
  required <- c("fac_original", "fac_corrected", "reason", "updated_at")
  missing <- setdiff(required, names(corr))
  if (length(missing) > 0) {
    stop("FAC_Corrections.csv missing required columns: ",
         paste(missing, collapse = ", "))
  }
  
  corr <- corr |>
    transmute(
      fac_original  = clean_fac(fac_original),
      fac_corrected = clean_fac(fac_corrected),
      reason        = as.character(reason),
      evidence      = if ("evidence" %in% names(corr)) as.character(evidence) else NA_character_,
      updated_at    = as.character(updated_at)
    ) |>
    filter(
      !is.na(fac_original),
      !is.na(fac_corrected),
      fac_original != "",
      fac_corrected != ""
    ) |>
    distinct(fac_original, .keep_all = TRUE)
  
  out <- df |>
    left_join(corr, by = setNames("fac_original", fac_col)) |>
    mutate(
      fac_before_correction = .data[[fac_col]],
      !!fac_col := if_else(!is.na(fac_corrected),
                           fac_corrected,
                           .data[[fac_col]])
    ) |>
    select(-fac_corrected)
  
  out
}

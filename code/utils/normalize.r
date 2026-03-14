# utils/normalize.R
# Shared normalization helpers for OntarioHospitalNamesCrosswalk
# Source this at the top of any script that needs these functions.
# NOTE: Accented characters (e.g. ô, é) are stripped by the [^A-Z0-9] regex,
# not transliterated. "Hôpital" normalizes to "H PITAL", not "HOPITAL".
# This is consistent with all existing phase scripts (Phases C, E, G, H).
# For sources with heavy French content (SACType), consider adding:
#   stringi::stri_trans_general(x, "Latin-ASCII")
# before the toupper() call, and update all downstream baselines accordingly.


normalize_name <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- toupper(x)
  x <- stringr::str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- stringr::str_squish(x)
  x <- dplyr::na_if(x, "")
  x
}

clean_fac <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(x, "[^0-9]", "")
  x <- dplyr::na_if(x, "")
  x
}

clean_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_squish(x)
  x <- dplyr::na_if(x, "")
  x
}

standardize_colnames <- function(df) {
  names(df) <- names(df) |>
    stringr::str_trim() |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^A-Za-z0-9_]", "") |>
    tolower()
  df
}
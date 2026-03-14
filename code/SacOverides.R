library(tibble)
library(readr)

overrides <- tribble(
  ~sactype_name_clean,                                          ~fac,  ~sactype_value, ~override_reason,                          ~notes,
  # Fix bad fuzzy matches — these should NOT be FAC 957 or 978
  "Quinte Health Care - North Hastings District Hospital",      "769", "rural",        "correct_fac_manual",                      "Quinte HC is FAC 769; wrongly matched to 957 via fuzzy",
  "Quinte Health Care - Prince Edward County Memorial Hospital","769", "rural",        "correct_fac_manual",                      "Quinte HC is FAC 769; wrongly matched to 957 via fuzzy",
  "Anson General Hospital",                                     "685", "rural",        "correct_fac_manual",                      "Anson General is FAC 685; wrongly matched to 978 via fuzzy",
  # French hospitals missed due to accent stripping
  "HÔPITAL MONTFORT HOSPITAL",                                  "753", "urban",        "french_name_accent_stripping",             "Ottawa; normalize_name strips accents",
  "HÔPITAL GENERAL DE HAWKESBURY AND DISTRICT GENERAL HOSPITAL INC.", "800", "urban", "french_name_accent_stripping",             "Hawkesbury",
  "HÔPITAL ÉLISABETH BRUYÈRE HOSPITAL",                        "932", "urban",        "french_name_accent_stripping",             "Ottawa",
  # Other clear matches from unmatched list
  "Belleville General Hospital",                                "615", "urban",        "manual_review",                           "Belleville; in fac_master as Belleville General",
  "Credit Valley Hospital",                                     "648", "urban",        "manual_review",                           "Mississauga",
  "Georgetown Hospital",                                        "663", "urban",        "manual_review",                           "Georgetown",
  "Hotel Dieu Hospital of Cornwall",                            "644", "urban",        "manual_review",                           "Cornwall Hotel Dieu FAC 644",
  "Hotel Dieu Shaver Health & Rehabilitation Centre",           "790", "urban",        "manual_review",                           "St Catharines Hotel Dieu FAC 790",
  "Huronia District Hospital",                                  "726", "urban",        "manual_review",                           "Midland Georgian Bay",
  "Lady Minto Hospital",                                        "638", "rural",        "manual_review",                           "Cochrane Lady Minto FAC 638",
  "Lakeridge Health Ajax Pickering Hospital",                   "952", "urban",        "manual_review",                           "Lakeridge Ajax site",
  "Mackenzie Richmond Hill Hospital",                           "701", "urban",        "manual_review",                           "Richmond Hill Mackenzie Health FAC 701",
  "Mattawa General Hospital Inc.",                              "724", "rural",        "manual_review",                           "Mattawa General FAC 724",
  "Ongwanada Hospital",                                         "757", "urban",        "manual_review",                           "Kingston",
  "Royal Ottawa Mental Health Centre",                          "761", "urban",        "manual_review",                           "Ottawa",
  "Shouldice Hospital Ltd.",                                    "855", "urban",        "manual_review",                           "Thornhill",
  "South Muskoka Memorial Hospital",                            "869", "rural",        "manual_review",                           "Bracebridge",
  "St. Mary's General Hospital",                                "699", "urban",        "manual_review",                           "Kitchener St Marys FAC 699",
  "the West Nipissing General Hospital",                        "881", "rural",        "manual_review",                           "Sturgeon Falls West Nipissing FAC 881",
  "Weeneebayko General Hospital",                               "896", "rural",        "manual_review",                           "Moosonee",
  "Homewood Health Centre",                                     "601", "urban",        "manual_review",                           "Guelph Homewood FAC 601",
  "Notre-Dame Hospital - Hearst",                               "681", "rural",        "manual_review",                           "Hearst Notre Dame FAC 681",
  "Bingham Memorial Hospital",                                  "723", "rural",        "manual_review",                           "Matheson Bingham Memorial FAC 723",
  "North Bay General Hospital - Mclaren Site",                  "737", "urban",        "manual_review",                           "North Bay site",
  "St. Joseph's Health Centre - Toronto",                       "898", "urban",        "manual_review",                           "Toronto St Josephs",
  "St. Joseph's General Hospital - Elliot Lake",                "650", "urban",        "manual_review",                           "Elliot Lake St Josephs FAC 650",
  "St. Joseph's Hospital - London",                             "717", "urban",        "manual_review",                           "London St Josephs",
  "St. Joseph's Care Group - Lakehead Psychiatric Hospital",    "781", "urban",        "manual_review",                           "Thunder Bay St Josephs FAC 781"
)

dir.create("sources", showWarnings = FALSE)
write_csv(overrides, file.path("sources", "sactype_fac_overrides.csv"))
cat("Wrote", nrow(overrides), "override rows to sources/sactype_fac_overrides.csv\n")
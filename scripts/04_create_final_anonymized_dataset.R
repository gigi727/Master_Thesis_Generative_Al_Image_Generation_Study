#####################################################################
###04_create_final_anonymized_dataset.R                          ###
### Erstellt drei direkt identifikatorfreie Analyse-Datensätze:    ###
### 1) Pre-Survey cleaned/anonymized, unge-matcht                  ###
### 2) Main-Survey cleaned/anonymized, unge-matcht                 ###
### 3) finaler gematchter Analyse-Datensatz mit VIVIQ + Coding     ###
#####################################################################

library(dplyr)
library(readr)
library(stringr)
library(here)
library(tibble)

# =========================================================
# 1) Projektpfade und Vorgänger-Skripte                   ===
# =========================================================

project_root <- here::here()

helper_script_candidates <- c(
  file.path(project_root, "scripts", "00_project_helpers_unified.R"),
  file.path(project_root, "00_project_helpers_unified.R")
)
helper_script_path <- helper_script_candidates[file.exists(helper_script_candidates)][1]

if (length(helper_script_path) == 0 || is.na(helper_script_path)) {
  stop(
    paste0(
      "The central helper script could not be found. Expected one of these locations:\n",
      paste(helper_script_candidates, collapse = "\n")
    ),
    call. = FALSE
  )
}

source(helper_script_path, local = .GlobalEnv)

source(file.path(project_root, "scripts", "01_masterarbeit_data_cleaning_workflow_csv_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "02_viviq_scoring_matched_sample_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "03_coding_promts.qc.R"), local = .GlobalEnv)

# =========================================================
# 2) Benötigte interne Objekte prüfen                     ===
# =========================================================

required_internal_objects <- c(
  "pre_clean_full",
  "main_clean_full",
  "final_analysis_dataset_full_viviq_prompt_coding"
)

missing_internal_objects <- required_internal_objects[
  !vapply(required_internal_objects, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)
]

if (length(missing_internal_objects) > 0) {
  stop(
    paste0(
      "The following required objects were not created by scripts 01-03:\n",
      paste(missing_internal_objects, collapse = "\n")
    ),
    call. = FALSE
  )
}

# =========================================================
# 3) Anonymisierungs-Hilfsfunktionen                      ===
# =========================================================

direct_identifier_exact <- c(
  "matched_email",

  "Pre_Survey_Q32",
  "Main_Survey_Q2",

  "Pre_Survey_email_raw",
  "Pre_Survey_email_clean",
  "Pre_Survey_email_valid_format",
  "Main_Survey_email_raw",
  "Main_Survey_email_clean",
  "Main_Survey_email_valid_format",

  "Pre_Survey_ResponseId",
  "Main_Survey_ResponseId",
  "Case_Response_ID",

  "Pre_Survey_IPAddress",
  "Main_Survey_IPAddress",

  "Pre_Survey_Recipient Last Name",
  "Main_Survey_Recipient Last Name",
  "Pre_Survey_Recipient First Name",
  "Main_Survey_Recipient First Name",
  "Pre_Survey_Recipient Email",
  "Main_Survey_Recipient Email",

  "Pre_Survey_ExternalReference",
  "Main_Survey_ExternalReference",

  "Pre_Survey_Location Latitude",
  "Main_Survey_Location Latitude",
  "Pre_Survey_Location Longitude",
  "Main_Survey_Location Longitude"
)

direct_identifier_patterns <- c(
  "email",
  "e-mail",
  "IPAddress",
  "IP Address",
  "Recipient.*Last",
  "Recipient.*First",
  "Recipient.*Email",
  "ExternalReference",
  "Location.*Latitude",
  "Location.*Longitude",
  "ResponseId",
  "Case_Response_ID"
)

identifier_regex <- paste(direct_identifier_patterns, collapse = "|")

remove_direct_identifiers <- function(df) {
  df %>%
    select(-any_of(direct_identifier_exact)) %>%
    select(-matches(identifier_regex, ignore.case = TRUE))
}

check_no_direct_identifiers <- function(df, dataset_label) {
  remaining_suspicious_columns <- names(df)[
    str_detect(names(df), regex(identifier_regex, ignore_case = TRUE))
  ]

  if (length(remaining_suspicious_columns) > 0) {
    stop(
      paste0(
        "Potential direct identifiers still remain in ", dataset_label, ":\n",
        paste(remaining_suspicious_columns, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

add_row_based_anonymous_id <- function(df, id_name, prefix, order_candidates = character()) {
  df_tmp <- df %>% mutate(.internal_original_row = dplyr::row_number())
  order_var <- order_candidates[order_candidates %in% names(df_tmp)][1]

  if (!is.na(order_var) && length(order_var) == 1) {
    id_map <- df_tmp %>%
      arrange(.data[[order_var]], .internal_original_row) %>%
      transmute(
        .internal_original_row,
        "{id_name}" := sprintf(paste0(prefix, "%05d"), dplyr::row_number())
      )
  } else {
    id_map <- df_tmp %>%
      transmute(
        .internal_original_row,
        "{id_name}" := sprintf(paste0(prefix, "%05d"), dplyr::row_number())
      )
  }

  df_tmp %>%
    left_join(id_map, by = ".internal_original_row") %>%
    select(-.internal_original_row) %>%
    relocate(all_of(id_name), .before = 1)
}

# =========================================================
# 4) Pre-Survey anonymisiert, unge-matcht                 ===
# =========================================================

pre_survey_anonymized <- pre_clean_full %>%
  add_row_based_anonymous_id(
    id_name = "pre_participant_id",
    prefix = "PRE",
    order_candidates = c("Pre_Survey_ResponseId", "Pre_Survey_StartDate", "Pre_Survey_RecordedDate")
  ) %>%
  remove_direct_identifiers()

check_no_direct_identifiers(pre_survey_anonymized, "pre_survey_anonymized")

# =========================================================
# 5) Main-Survey anonymisiert, unge-matcht                ===
# =========================================================

main_survey_anonymized <- main_clean_full %>%
  add_row_based_anonymous_id(
    id_name = "main_participant_id",
    prefix = "MAIN",
    order_candidates = c("Main_Survey_ResponseId", "Main_Survey_StartDate", "Main_Survey_RecordedDate")
  ) %>%
  remove_direct_identifiers()

check_no_direct_identifiers(main_survey_anonymized, "main_survey_anonymized")

# =========================================================
# 6) Finaler gematchter Analyse-Datensatz anonymisiert    ===
# =========================================================

final_internal_raw <- final_analysis_dataset_full_viviq_prompt_coding

if (!"matched_email" %in% names(final_internal_raw)) {
  stop(
    "Column 'matched_email' is missing. It is required temporarily to create participant_id.",
    call. = FALSE
  )
}

if (any(is.na(final_internal_raw$matched_email))) {
  stop(
    "Some rows have missing matched_email. Cannot create stable participant_id safely.",
    call. = FALSE
  )
}

id_map_final <- final_internal_raw %>%
  distinct(matched_email) %>%
  arrange(matched_email) %>%
  mutate(participant_id = sprintf("P%05d", row_number()))

final_analysis_dataset_anonymized <- final_internal_raw %>%
  left_join(id_map_final, by = "matched_email") %>%
  relocate(participant_id, .before = 1) %>%
  remove_direct_identifiers()

check_no_direct_identifiers(final_analysis_dataset_anonymized, "final_analysis_dataset_anonymized")

if (!"participant_id" %in% names(final_analysis_dataset_anonymized)) {
  stop("participant_id is missing from the final matched dataset.", call. = FALSE)
}

# =========================================================
# 7) Public feature lookups                              ===
# =========================================================

pre_feature_lookup_public <- if (exists("pre_feature_lookup", envir = .GlobalEnv, inherits = FALSE)) {
  pre_feature_lookup
} else {
  derive_feature_lookup_from_dataset(pre_survey_anonymized, "Pre_Survey")
}

main_feature_lookup_public <- if (exists("main_feature_lookup", envir = .GlobalEnv, inherits = FALSE)) {
  main_feature_lookup
} else {
  derive_feature_lookup_from_dataset(main_survey_anonymized, "Main_Survey")
}

# Keine direkten Identifier in den public lookups erzwingen.
pre_feature_lookup_public <- pre_feature_lookup_public %>%
  filter(!str_detect(variable_name, regex(identifier_regex, ignore_case = TRUE)))

main_feature_lookup_public <- main_feature_lookup_public %>%
  filter(!str_detect(variable_name, regex(identifier_regex, ignore_case = TRUE)))

# =========================================================
# 8) Dokumentation des Exports                           ===
# =========================================================

export_summary <- tibble(
  object = c(
    "pre_survey_anonymized",
    "main_survey_anonymized",
    "final_analysis_dataset_anonymized"
  ),
  n_rows = c(
    nrow(pre_survey_anonymized),
    nrow(main_survey_anonymized),
    nrow(final_analysis_dataset_anonymized)
  ),
  n_columns = c(
    ncol(pre_survey_anonymized),
    ncol(main_survey_anonymized),
    ncol(final_analysis_dataset_anonymized)
  ),
  id_variable = c(
    "pre_participant_id",
    "main_participant_id",
    "participant_id"
  ),
  role = c(
    "Cleaned Pre-Survey dataset before matching",
    "Cleaned Main-Survey dataset before matching",
    "Matched final analysis dataset with VIVIQ and prompt coding"
  ),
  direct_identifiers_removed = TRUE,
  dates_retained = TRUE,
  free_text_retained = TRUE,
  age_gender_retained = TRUE
)

# =========================================================
# 9) Export                                              ===
# =========================================================

out_final_dir <- file.path(project_root, "data_final")
dir.create(out_final_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(pre_survey_anonymized, file.path(out_final_dir, "pre_survey_anonymized.csv"))
saveRDS(pre_survey_anonymized, file.path(out_final_dir, "pre_survey_anonymized.rds"))

readr::write_csv(main_survey_anonymized, file.path(out_final_dir, "main_survey_anonymized.csv"))
saveRDS(main_survey_anonymized, file.path(out_final_dir, "main_survey_anonymized.rds"))

readr::write_csv(final_analysis_dataset_anonymized, file.path(out_final_dir, "final_analysis_dataset_anonymized.csv"))
saveRDS(final_analysis_dataset_anonymized, file.path(out_final_dir, "final_analysis_dataset_anonymized.rds"))

readr::write_csv(pre_feature_lookup_public, file.path(out_final_dir, "pre_feature_lookup_public.csv"))
saveRDS(pre_feature_lookup_public, file.path(out_final_dir, "pre_feature_lookup_public.rds"))

readr::write_csv(main_feature_lookup_public, file.path(out_final_dir, "main_feature_lookup_public.csv"))
saveRDS(main_feature_lookup_public, file.path(out_final_dir, "main_feature_lookup_public.rds"))

readr::write_csv(export_summary, file.path(out_final_dir, "anonymized_dataset_export_summary.csv"))

message("Anonymized datasets created successfully:")
message("- ", file.path(out_final_dir, "pre_survey_anonymized.rds"))
message("- ", file.path(out_final_dir, "main_survey_anonymized.rds"))
message("- ", file.path(out_final_dir, "final_analysis_dataset_anonymized.rds"))

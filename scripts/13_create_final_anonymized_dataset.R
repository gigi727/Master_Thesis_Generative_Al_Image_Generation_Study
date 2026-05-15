#####################################################################
### 13_create_final_anonymized_dataset.R
### Erstellt den finalen bereinigten, direkt identifikatorfreien
### Analyse- und Publikationsdatensatz mit Prompt-Coding
#####################################################################

library(dplyr)
library(readr)
library(stringr)
library(here)
library(tibble)

# =========================================================
# 1) Vorherige Pipeline ausführen
# =========================================================

project_root <- here::here()

source(file.path(project_root, "scripts", "00_project_helpers_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "01_masterarbeit_data_cleaning_workflow_csv_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "02_viviq_scoring_matched_sample_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "03_coding_promts.qc.R"), local = .GlobalEnv)

# =========================================================
# 2) Prüfen, ob finaler Prompt-Coding-Datensatz existiert
# =========================================================

if (!exists("final_analysis_dataset_full_viviq_prompt_coding", envir = .GlobalEnv)) {
  stop(
    "Object 'final_analysis_dataset_full_viviq_prompt_coding' was not created. Please check scripts 01, 02 and 03.",
    call. = FALSE
  )
}

final_internal_raw <- final_analysis_dataset_full_viviq_prompt_coding

# =========================================================
# 3) Prüfen, ob Matching-ID vorhanden ist
# =========================================================

if (!"matched_email" %in% names(final_internal_raw)) {
  stop(
    "Column 'matched_email' is missing. It is required only temporarily to create participant_id.",
    call. = FALSE
  )
}

if (any(is.na(final_internal_raw$matched_email))) {
  stop(
    "Some rows have missing matched_email. Cannot create stable participant_id safely.",
    call. = FALSE
  )
}

if (nrow(final_internal_raw) != dplyr::n_distinct(final_internal_raw$matched_email)) {
  warning(
    "There are repeated matched_email values. This is acceptable only if the final dataset intentionally has multiple rows per participant."
  )
}

# =========================================================
# 4) Neue nicht-sprechende participant_id erzeugen
# =========================================================

id_map <- final_internal_raw %>%
  distinct(matched_email) %>%
  arrange(matched_email) %>%
  mutate(
    participant_id = sprintf("P%05d", row_number())
  )

final_with_id <- final_internal_raw %>%
  left_join(id_map, by = "matched_email") %>%
  relocate(participant_id, .before = 1)

# =========================================================
# 5) Direkte Identifikatoren definieren
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
  "Case_ID",

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
  "Case_Response_ID",
  "Case_ID"
)

# =========================================================
# 6) Finalen anonymisierten Datensatz erstellen
# =========================================================

final_analysis_dataset_anonymized <- final_with_id %>%
  select(-any_of(direct_identifier_exact)) %>%
  select(
    -matches(
      paste(direct_identifier_patterns, collapse = "|"),
      ignore.case = TRUE
    )
  )

# =========================================================
# 7) Sicherheitschecks
# =========================================================

remaining_suspicious_columns <- names(final_analysis_dataset_anonymized)[
  str_detect(
    names(final_analysis_dataset_anonymized),
    regex(
      "email|e-mail|IPAddress|IP Address|Recipient|ExternalReference|Location.*Latitude|Location.*Longitude|ResponseId|Case_Response_ID|Case_ID",
      ignore_case = TRUE
    )
  )
]

if (length(remaining_suspicious_columns) > 0) {
  stop(
    paste0(
      "Potential direct identifiers still remain:\n",
      paste(remaining_suspicious_columns, collapse = "\n")
    ),
    call. = FALSE
  )
}

# Zusätzlicher wertbasierter Check: Die manuell codierte Prompt-Datei
# kann historische IP-Werte in Case_ID enthalten. Deshalb wird nicht nur
# nach Spaltennamen, sondern auch nach IP-ähnlichen Zellwerten gesucht.
ip_like_pattern <- "^\\s*([0-9]{1,3}\\.){3}[0-9]{1,3}\\s*$"

character_columns <- names(final_analysis_dataset_anonymized)[
  vapply(final_analysis_dataset_anonymized, is.character, logical(1))
]

remaining_ip_like_columns <- character_columns[
  vapply(
    character_columns,
    function(col) {
      any(
        stringr::str_detect(
          as.character(final_analysis_dataset_anonymized[[col]]),
          stringr::regex(ip_like_pattern)
        ),
        na.rm = TRUE
      )
    },
    logical(1)
  )
]

if (length(remaining_ip_like_columns) > 0) {
  stop(
    paste0(
      "Potential IP-like values still remain in these columns:\n",
      paste(remaining_ip_like_columns, collapse = "\n")
    ),
    call. = FALSE
  )
}

if (!"participant_id" %in% names(final_analysis_dataset_anonymized)) {
  stop("participant_id is missing from the final dataset.", call. = FALSE)
}

if ("matched_email" %in% names(final_analysis_dataset_anonymized)) {
  stop("matched_email is still present and must be removed.", call. = FALSE)
}

# =========================================================
# 8) Dokumentation des Exports
# =========================================================

export_summary <- tibble(
  object = "final_analysis_dataset_anonymized",
  n_rows = nrow(final_analysis_dataset_anonymized),
  n_columns = ncol(final_analysis_dataset_anonymized),
  n_participants = n_distinct(final_analysis_dataset_anonymized$participant_id),
  contains_prompt_coding = all(c("R1_Text", "R2_Text", "R3_Text") %in% names(final_analysis_dataset_anonymized)),
  contains_dates = any(str_detect(names(final_analysis_dataset_anonymized), regex("Date|Start|End|Recorded", ignore_case = TRUE))),
  contains_age = any(str_detect(names(final_analysis_dataset_anonymized), regex("age|alter", ignore_case = TRUE))),
  contains_gender = any(str_detect(names(final_analysis_dataset_anonymized), regex("gender|geschlecht|sex", ignore_case = TRUE)))
)

# =========================================================
# 9) Export
# =========================================================

out_final_dir <- file.path(project_root, "data_final")
dir.create(out_final_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  final_analysis_dataset_anonymized,
  file.path(out_final_dir, "final_analysis_dataset_anonymized.csv")
)

saveRDS(
  final_analysis_dataset_anonymized,
  file.path(out_final_dir, "final_analysis_dataset_anonymized.rds")
)

readr::write_csv(
  export_summary,
  file.path(out_final_dir, "final_analysis_dataset_anonymized_export_summary.csv")
)

message("Final anonymized dataset created successfully:")
message(file.path(out_final_dir, "final_analysis_dataset_anonymized.csv"))
message(file.path(out_final_dir, "final_analysis_dataset_anonymized.rds"))

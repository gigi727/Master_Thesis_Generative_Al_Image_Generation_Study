#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.

#####################################################################
###         Variablenübersicht mit Normierung und Typen           ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript erstellt eine vollständige Übersicht über alle Variablen
# des finalen anonymisierten Datensatzes. Für jede Variable werden Datensatz,
# Variablenname, Fragetext, ein beobachteter Beispielwert,
# Antworttyp sowie eine vorhandene Normierung dokumentiert.
# Das Skript baut direkt auf dem finalen anonymisierten Datensatz auf
# und verwendet die dort enthaltenen präfixierten Variablennamen.

# =========================================================
# 0) Pakete                                              ===
#
# In diesem Abschnitt werden die für das Erstellen,
# Dokumentieren und Exportieren der Variablenübersicht
# benötigten Pakete installiert und geladen.
# =========================================================

#install.packages(c("tidyverse", "writexl", "here", “gt“), dependencies = TRUE)

library(tidyverse)
library(writexl)
library(here)
library(gt)

#####################################################################
###                Pfade und Ordnerstruktur definieren            ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden die Pfade zum zentralen Cleaning-Skript
# sowie die Ausgabeordner für Tabellen und Dokumentation definiert.
# Die Ergebnisse werden in einem eigenen Unterordner gespeichert,
# damit das Variableninventar separat von den Analyseergebnissen
# dokumentiert werden kann.

# =========================================================
# 1) Pfade und Ausgabeordner                             ===
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

out_inventory_dir <- file.path(project_root, "data_output", "variable_inventory")
out_tables_dir <- file.path(out_inventory_dir, "tables")
out_document_dir <- file.path(out_inventory_dir, "documentation")
out_gt_dir <- file.path(out_inventory_dir, "gt_tables")
out_gt_html_dir <- file.path(out_gt_dir, "html")
out_gt_rtf_dir <- file.path(out_gt_dir, "rtf")
out_gt_doc_dir <- file.path(out_gt_dir, "documentation")

purrr::walk(
  c(out_inventory_dir, out_tables_dir, out_document_dir, out_gt_dir, out_gt_html_dir, out_gt_rtf_dir, out_gt_doc_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)


#####################################################################
###         Finalen anonymisierten Datensatz laden                 ###
#####################################################################

### BESCHREIBUNG ###

# Ab dieser Version verwendet das Skript ausschließlich den finalen
# anonymisierten Datensatz aus data_final/. Die Variablenübersicht wird
# aus den dort enthaltenen Pre- und Main-Survey-Variablen rekonstruiert.
# Es werden keine Rohdaten und keine Outputs aus 01-03 geladen.


# =========================================================
# Final-Dataset-Only Bootstrap                            ===
# =========================================================

load_final_analysis_dataset_only <- function(project_root) {
  if (exists("final_analysis_dataset", envir = .GlobalEnv, inherits = FALSE)) {
    ds <- get("final_analysis_dataset", envir = .GlobalEnv)
  } else if (exists("final_analysis_dataset_anonymized", envir = .GlobalEnv, inherits = FALSE)) {
    ds <- get("final_analysis_dataset_anonymized", envir = .GlobalEnv)
  } else {
    rds_candidates <- c(
      file.path(project_root, "data_final", "final_analysis_dataset_anonymized.rds"),
      file.path(project_root, "final_analysis_dataset_anonymized.rds")
    )
    csv_candidates <- c(
      file.path(project_root, "data_final", "final_analysis_dataset_anonymized.csv"),
      file.path(project_root, "final_analysis_dataset_anonymized.csv")
    )

    rds_path <- rds_candidates[file.exists(rds_candidates)][1]
    csv_path <- csv_candidates[file.exists(csv_candidates)][1]

    if (!is.na(rds_path)) {
      ds <- readRDS(rds_path)
      message("Confirmation: Loaded final anonymized dataset: ", rds_path)
    } else if (!is.na(csv_path)) {
      ds <- readr::read_csv(csv_path, show_col_types = FALSE)
      message("Confirmation: Loaded final anonymized dataset: ", csv_path)
    } else {
      stop(
        paste0(
          "The final anonymized dataset could not be found. Expected one of these files:\n",
          paste(c(rds_candidates, csv_candidates), collapse = "\n"),
          "\nRun 13_create_final_anonymized_dataset.R locally once and commit/upload only the data_final file."
        ),
        call. = FALSE
      )
    }
  }

  if (!"participant_id" %in% names(ds)) {
    stop("The final dataset must contain 'participant_id'.", call. = FALSE)
  }

  suspicious_identifier_cols <- names(ds)[
    stringr::str_detect(
      names(ds),
      stringr::regex(
        "email|e-mail|matched_email|IPAddress|IP Address|Recipient|ExternalReference|Location.*Latitude|Location.*Longitude|ResponseId|Case_Response_ID",
        ignore_case = TRUE
      )
    )
  ]

  if (length(suspicious_identifier_cols) > 0) {
    stop(
      paste0(
        "Potential direct identifiers are still present in the final dataset:\n",
        paste(suspicious_identifier_cols, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  ds <- dplyr::as_tibble(ds)
  final_analysis_dataset <<- ds
  final_analysis_dataset_anonymized <<- ds
  final_analysis_dataset_full <<- ds

  pre_cols <- names(ds)[stringr::str_detect(names(ds), "^Pre_Survey_")]
  main_cols <- names(ds)[stringr::str_detect(names(ds), "^Main_Survey_")]
  viviq_cols <- names(ds)[stringr::str_detect(names(ds), "^viviq_|^VIVIQ|^Main_Survey_Q([4-9]|1[0-9])_score$")]
  prompt_cols <- names(ds)[stringr::str_detect(names(ds), "^(R[123]_|Case_|Overall_|Sequence_|Prompt_|Coding_)")]

  pre_clean_full <<- ds %>%
    dplyr::select(participant_id, dplyr::all_of(pre_cols))

  main_clean_full <<- ds %>%
    dplyr::select(participant_id, dplyr::all_of(main_cols), dplyr::any_of(c(viviq_cols, prompt_cols)))

  pre_raw <<- pre_clean_full
  main_raw <<- main_clean_full

  pre_feature_lookup <<- tibble::tibble(
    variable_name = pre_cols,
    question_text = pre_cols,
    source = "final_analysis_dataset_anonymized"
  )

  main_feature_lookup <<- tibble::tibble(
    variable_name = main_cols,
    question_text = main_cols,
    source = "final_analysis_dataset_anonymized"
  )

  matched_pre_main <<- ds %>%
    dplyr::select(participant_id) %>%
    dplyr::distinct()

  matched_pre_main_valid <<- matched_pre_main

  main_matched_viviq_final <<- ds %>%
    dplyr::select(participant_id, dplyr::any_of(viviq_cols)) %>%
    dplyr::distinct(participant_id, .keep_all = TRUE)

  preview_removal_summary <<- tibble::tibble(
    dataset = c("Pre_Survey", "Main_Survey"),
    n_removed = NA_integer_,
    n_kept = nrow(ds),
    note = "Final-dataset-only mode: raw cleaning counts are not available from the public anonymized dataset."
  )

  pre_n_overview <<- tibble::tibble(
    data_cleaning_step = c("DC1_Finished_false_removed", "DC3_Consent_no_removed"),
    n_removed = NA_integer_,
    n_kept = nrow(ds),
    note = "Final-dataset-only mode"
  )

  main_n_overview <<- pre_n_overview

  pre_n_duplicate_ip <<- tibble::tibble(dataset = "Pre_Survey", n_duplicate_ip = NA_integer_, note = "IP address removed from final dataset")
  main_n_duplicate_ip <<- tibble::tibble(dataset = "Main_Survey", n_duplicate_ip = NA_integer_, note = "IP address removed from final dataset")

  match_summary <<- tibble::tibble(
    metric = c("final_public_cases", "final_public_participants"),
    value = c(nrow(ds), dplyr::n_distinct(ds$participant_id)),
    note = "Calculated from final anonymized dataset"
  )

  match_summary_valid_only <<- match_summary

  config_pre <<- list(dataset_label = "Pre_Survey", final_dataset_only = TRUE)
  config_main <<- list(dataset_label = "Main_Survey", final_dataset_only = TRUE)

  invisible(ds)
}

final_analysis_dataset <- load_final_analysis_dataset_only(project_root)

#####################################################################
###                    Hilfsfunktionen                            ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden Hilfsfunktionen definiert, die für
# - die Erkennung des Antworttyps,
# - die Auswahl eines beobachteten Beispielwertes,
# - und den Aufbau des Variableninventars
# verwendet werden.

# =========================================================
# 4) Hilfsfunktionen                                      ===
# =========================================================

detect_answer_type <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) return("Date")
  if (is.logical(x)) return("Logical")
  if (is.numeric(x)) return("Number")
  if (is.factor(x)) return("Factor")
  if (is.character(x)) return("String")
  class(x)[1]
}

sample_nonmissing_value <- function(x) {
  vals <- x[!is.na(x) & as.character(x) != ""]
  if (length(vals) == 0) return(NA_character_)
  as.character(sample(vals, 1))
}

build_inventory <- function(df_raw, df_clean, lookup_df, dataset_label, normalization_codebook) {
  tibble(variable = names(df_raw)) %>%
    mutate(
      dataset = dataset_label,
      random_value = purrr::map_chr(variable, ~ sample_nonmissing_value(df_raw[[.x]])),
      answer_type = purrr::map_chr(variable, ~ detect_answer_type(df_raw[[.x]])),
      score_variable = if_else(
        paste0(variable, "_score") %in% names(df_clean),
        paste0(variable, "_score"),
        NA_character_
      )
    ) %>%
    left_join(
      lookup_df %>% rename(variable = variable_name),
      by = "variable"
    ) %>%
    left_join(
      normalization_codebook %>%
        select(dataset, variable, score_variable, normalization),
      by = c("dataset", "variable", "score_variable")
    ) %>%
    mutate(
      has_score_variable = !is.na(score_variable),
      normalization = if_else(is.na(normalization), "No explicit normalization", normalization)
    ) %>%
    select(
      dataset,
      variable,
      question_text,
      random_value,
      answer_type,
      has_score_variable,
      score_variable,
      normalization
    )
}

make_gt_table <- function(data, title_text, subtitle_text = NULL) {
  gt_tbl <- data %>%
    gt() %>%
    tab_header(
      title = title_text,
      subtitle = subtitle_text
    ) %>%
    tab_options(
      table.font.size = 12,
      heading.title.font.size = 14,
      data_row.padding = px(4)
    )

  return(gt_tbl)
}

#####################################################################
###                  Normierungscodebuch erstellen                ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt wird das Codebuch der explizit normierten Variablen
# erstellt. Es dokumentiert pro Variable den zugehörigen Datensatz,
# die numerische Score-Variable sowie die textuelle Beschreibung
# der verwendeten Normierung.

# =========================================================
# 5) Normierungscodebuch                                  ===
# =========================================================

normalization_codebook <- tribble(
  ~dataset, ~variable, ~score_variable, ~normalization,
  "Pre_Survey", "Pre_Survey_Q8", "Pre_Survey_Q8_score", "1=Very inexperienced; 2=Inexperienced; 3=Slightly inexperienced; 4=Neither inexperienced nor experienced; 5=Slightly experienced; 6=Experienced; 7=Very experienced",
  "Pre_Survey", "Pre_Survey_Q13_1", "Pre_Survey_Q13_1_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_2", "Pre_Survey_Q13_2_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_3", "Pre_Survey_Q13_3_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_4", "Pre_Survey_Q13_4_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_5", "Pre_Survey_Q13_5_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_6", "Pre_Survey_Q13_6_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q16", "Pre_Survey_Q16_score", "1=Strongly disagree; 2=Disagree; 3=Somewhat disagree; 4=Neither agree nor disagree; 5=Somewhat agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q18", "Pre_Survey_Q18_score", "1=Extremely easy; 2=Very easy; 3=Somewhat easy; 4=Neither easy nor difficult; 5=Somewhat difficult; 6=Very difficult; 7=Extremely difficult",
  "Pre_Survey", "Pre_Survey_Q21_1", "Pre_Survey_Q21_1_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q21_2", "Pre_Survey_Q21_2_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q21_3", "Pre_Survey_Q21_3_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q21_4", "Pre_Survey_Q21_4_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q21_5", "Pre_Survey_Q21_5_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q21", "Main_Survey_Q21_score", "1=Not vivid at all; 2=Very weak; 3=Weak; 4=Moderately vivid; 5=Quite vivid; 6=Very vivid; 7=Extremely vivid",
  "Main_Survey", "Main_Survey_Q26", "Main_Survey_Q26_score", "1=Not at all; 2=Very weakly; 3=Weakly; 4=Moderately; 5=Strongly; 6=Very strongly; 7=Almost exactly",
  "Main_Survey", "Main_Survey_Q34", "Main_Survey_Q34_score", "1=Not at all; 2=Very weakly; 3=Weakly; 4=Moderately; 5=Strongly; 6=Very strongly; 7=Almost exactly",
  "Main_Survey", "Main_Survey_Q42", "Main_Survey_Q42_score", "1=Not at all; 2=Very weakly; 3=Weakly; 4=Moderately; 5=Strongly; 6=Very strongly; 7=Almost exactly",
  "Main_Survey", "Main_Survey_Q27_1", "Main_Survey_Q27_1_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q27_2", "Main_Survey_Q27_2_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q27_3", "Main_Survey_Q27_3_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q35_1", "Main_Survey_Q35_1_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q35_2", "Main_Survey_Q35_2_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q35_3", "Main_Survey_Q35_3_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q43_1", "Main_Survey_Q43_1_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q43_2", "Main_Survey_Q43_2_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q43_3", "Main_Survey_Q43_3_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q28", "Main_Survey_Q28_score", "1=Strongly disagree; 2=Disagree; 3=Somewhat disagree; 4=Neither agree nor disagree; 5=Somewhat agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q36", "Main_Survey_Q36_score", "1=Strongly disagree; 2=Disagree; 3=Somewhat disagree; 4=Neither agree nor disagree; 5=Somewhat agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q44", "Main_Survey_Q44_score", "1=Strongly disagree; 2=Disagree; 3=Somewhat disagree; 4=Neither agree nor disagree; 5=Somewhat agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q48", "Main_Survey_Q48_score", "1=Completely different; 2=Very different; 3=Somewhat different; 4=Moderately similar; 5=Quite similar; 6=Very similar; 7=Essentially the same",
  "Main_Survey", "Main_Survey_Q52", "Main_Survey_Q52_score", "1=Not at all; 2=Very weakly; 3=Weakly; 4=Moderately; 5=Strongly; 6=Very strongly; 7=Almost exactly",
  "Main_Survey", "Main_Survey_Q53", "Main_Survey_Q53_score", "1=Strongly prefer my own mental images; 2=Prefer my own mental images; 3=Slightly prefer my own mental images; 4=No preference; 5=Slightly prefer AI-generated images; 6=Prefer AI-generated images; 7=Strongly prefer AI-generated images"
)


#####################################################################
###                Variableninventar aufbauen                     ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt wird für beide Datensätze jeweils ein Inventar
# aller Variablen erzeugt. Anschließend werden beide Tabellen zu einer
# gemeinsamen Übersicht zusammengeführt.

# =========================================================
# 6) Variableninventar erstellen                          ===
# =========================================================

variable_inventory_pre <- build_inventory(
  df_raw = pre_raw,
  df_clean = pre_clean_full,
  lookup_df = pre_feature_lookup,
  dataset_label = "Pre_Survey",
  normalization_codebook = normalization_codebook
)

variable_inventory_main <- build_inventory(
  df_raw = main_raw,
  df_clean = main_clean_full,
  lookup_df = main_feature_lookup,
  dataset_label = "Main_Survey",
  normalization_codebook = normalization_codebook
)

derived_main_variable_target_word_category <- tibble(
  dataset = "Main_Survey",
  variable = "Main_Survey_target_word_category",
  question_text = "Derived categorization of Main_Survey_target_word (abstract vs. concrete).",
  random_value = sample_nonmissing_value(main_clean_full$Main_Survey_target_word_category),
  answer_type = detect_answer_type(main_clean_full$Main_Survey_target_word_category),
  has_score_variable = FALSE,
  score_variable = NA_character_,
  normalization = "No explicit normalization; rule-based assignment from Main_Survey_target_word."
)

variable_inventory_main <- bind_rows(
  variable_inventory_main,
  derived_main_variable_target_word_category
)

variable_inventory_all <- bind_rows(variable_inventory_pre, variable_inventory_main)

#####################################################################
###                     Ergebnisse exportieren                     ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden das vollständige Variableninventar,
# die datensatzspezifischen Teiltabellen sowie das Normierungscodebuch
# als CSV- und Excel-Dateien exportiert.

# =========================================================
# 7) Exporte                                               ===
# =========================================================

readr::write_csv(variable_inventory_all, file.path(out_tables_dir, "04_variable_overview_with_normalization.csv"))
readr::write_csv(variable_inventory_pre, file.path(out_tables_dir, "04_pre_variable_overview_with_normalization.csv"))
readr::write_csv(variable_inventory_main, file.path(out_tables_dir, "04_main_variable_overview_with_normalization.csv"))
readr::write_csv(normalization_codebook, file.path(out_document_dir, "04_normalization_codebook.csv"))

writexl::write_xlsx(
  list(
    variable_inventory_all = variable_inventory_all,
    variable_inventory_pre = variable_inventory_pre,
    variable_inventory_main = variable_inventory_main,
    normalization_codebook = normalization_codebook
  ),
  path = file.path(out_inventory_dir, "04_variable_overview_with_normalization.xlsx")
)


#####################################################################
###                    Schnellcheck in der Konsole                ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden zentrale Kennzahlen des Variableninventars
# direkt in der Konsole ausgegeben, damit der erfolgreiche Aufbau der
# Übersichten rasch überprüft werden kann.

# =========================================================
# 8) Konsolenausgaben                                    ===
# =========================================================

cat("\n==================== VARIABLE INVENTORY OVERVIEW ====================\n")
print(tibble(
  n_variables_pre = nrow(variable_inventory_pre),
  n_variables_main = nrow(variable_inventory_main),
  n_variables_total = nrow(variable_inventory_all),
  n_normalized_variables = sum(variable_inventory_all$has_score_variable, na.rm = TRUE)
))

cat("\n==================== EXAMPLE PRE SURVEY ====================\n")
print(head(variable_inventory_pre, 10))

cat("\n==================== EXAMPLE MAIN SURVEY ====================\n")
print(head(variable_inventory_main, 10))

#####################################################################
###              Wissenschaftliche Tabellen anzeigen              ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden die zentralen Tabellen des
# Variableninventars zusätzlich als wissenschaftlich formatierte
# Tabellen dargestellt. Die Inhalte bleiben unverändert; es erfolgt
# ausschließlich eine zusätzliche tabellarische Darstellung.

# =========================================================
# 9) Wissenschaftliche Tabellen anzeigen                ===
# =========================================================

gt_variable_inventory_all <- make_gt_table(
  variable_inventory_all,
  title_text = "Overview of all variables with normalization and types",
  subtitle_text = "Pre-Survey and Main-Survey"
)

gt_variable_inventory_pre <- make_gt_table(
  variable_inventory_pre,
  title_text = "Overview of variables in the Pre-Survey",
  subtitle_text = "With normalization and answer types"
)

gt_variable_inventory_main <- make_gt_table(
  variable_inventory_main,
  title_text = "Overview of variables in the Main-Survey",
  subtitle_text = "With normalization and answer types"
)

gt_normalization_codebook <- make_gt_table(
  normalization_codebook,
  title_text = "Overview of the normalization codebook",
  subtitle_text = NULL
)

gt_output_list <- list(
  gt_variable_inventory_all = gt_variable_inventory_all,
  gt_variable_inventory_pre = gt_variable_inventory_pre,
  gt_variable_inventory_main = gt_variable_inventory_main,
  gt_normalization_codebook = gt_normalization_codebook
)

gt_manifest <- purrr::imap_dfr(
  gt_output_list,
  function(gt_tbl, object_name) {
    save_gt_table(
      gt_tbl = gt_tbl,
      file_stem = object_name,
      out_gt_html_dir = out_gt_html_dir,
      out_gt_rtf_dir = out_gt_rtf_dir
    )
  }
)

save_table_outputs(
  gt_manifest,
  base_filename = "04_variable_inventory_gt_manifest",
  out_dir = out_gt_doc_dir
)

build_simple_html_index(
  manifest = gt_manifest,
  output_path = file.path(out_gt_dir, "00_gt_index.html"),
  title_text = "Variable inventory - gt outputs",
  intro_text = "This index links to all formatted gt tables created from the variable inventory workflow."
)

gt_console_summary <- c(
  "==================== VARIABLE INVENTORY GT TABLES ====================",
  capture.output(print(gt_manifest)),
  "",
  paste0("HTML directory: ", out_gt_html_dir),
  paste0("RTF directory: ", out_gt_rtf_dir),
  paste0("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
)

writeLines(
  gt_console_summary,
  con = file.path(out_gt_doc_dir, "04_variable_inventory_gt_console_summary.txt")
)

message("Confirmation: GT tables for the variable inventory workflow were exported successfully.")
message("HTML tables: ", out_gt_html_dir)
message("RTF tables (where supported): ", out_gt_rtf_dir)
message("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
message("Manifest: ", file.path(out_gt_doc_dir, "04_variable_inventory_gt_manifest.csv"))

gt_variable_inventory_all
gt_variable_inventory_pre
gt_variable_inventory_main
gt_normalization_codebook

#####################################################################
###                    Ende des Workflows                         ###
#####################################################################

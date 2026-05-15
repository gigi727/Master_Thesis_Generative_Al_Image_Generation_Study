#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.
#
#
#####################################################################
### Erweiterung des Main-Datensatzes um manuell codierte Prompts  ###
#####################################################################

# Dieses Skript:
# 1) liest die manuell codierte CSV-Datei ein,
# 2) matched sie über die Response-ID,
# 3) ergänzt die codierten Variablen im bestehenden Main-Datensatz
#    final_analysis_dataset_full_viviq,
# 4) erzeugt Overall_Sequence und Case_type,
# 5) erstellt Plausibilitätsprüfungen,
# 6) überschreibt keine bestehenden Objekte.

# =========================================================
# 0) Pakete
# =========================================================

library(dplyr)
library(readr)
library(here)
library(tibble)
library(purrr)
library(tidyr)
library(gt)
library(writexl)

# =========================================================
# 1) Pfade
# =========================================================

output_dir <- get_output_dir("03")

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

scripts_dir  <- file.path(project_root, "scripts")

coded_prompts_file <- file.path(scripts_dir, "Dataset_Coded_Prompts_2026_04_09.csv")
codebook_file      <- file.path(scripts_dir, "The_Code_Book_for_Prompt_Coding_adjusted.docx")

out_prompt_dir <- file.path(project_root, "data_output", "prompt_coding")
out_document_dir <- file.path(out_prompt_dir, "documentation")
out_gt_dir      <- file.path(out_prompt_dir, "gt_tables")
out_gt_html_dir <- file.path(out_gt_dir, "html")
out_gt_rtf_dir  <- file.path(out_gt_dir, "rtf")
out_gt_doc_dir  <- file.path(out_gt_dir, "documentation")

ensure_directories(c(
  out_prompt_dir,
  out_document_dir,
  out_gt_dir,
  out_gt_html_dir,
  out_gt_rtf_dir,
  out_gt_doc_dir
))


# =========================================================
# 2) Erwartete Objekte und Variablennamen
# =========================================================

main_dataset_object <- "final_analysis_dataset_full_viviq"

# NEU: Join über Response-ID statt IP
main_join_var   <- "Pre_Survey_ResponseId"
coding_join_var <- "Case_Response_ID"

# Variablen aus der Coding-Datei, die in den Main-Datensatz übernommen werden sollen
# CHANGED: R1_Typ/R2_Typ/R3_Typ entfernt.
# CHANGED: R1_Vibe/R2_Vibe/R3_Vibe ergänzt.
prompt_coding_vars <- c(
  "Case_ID",
  "R1_Text",
  "R1_Function",
  "R1_Level",
  "R1_Vibe",
  "R2_Text",
  "R2_Function",
  "R2_Level",
  "R2_Vibe",
  "R3_Text",
  "R3_Function",
  "R3_Level",
  "R3_Vibe"
)

vibe_vars <- c("R1_Vibe", "R2_Vibe", "R3_Vibe")
function_vars <- c("R1_Function", "R2_Function", "R3_Function")
level_vars <- c("R1_Level", "R2_Level", "R3_Level")
vibe_levels <- c("No Vibe", "Vibe")

# Diese Variable soll zusätzlich im finalen Datensatz vorhanden sein
copied_join_var <- "Case_Response_ID"

derived_prompt_vars <- c("Overall_Sequence", "Case_type")

# =========================================================
# 3) Main-Datensatz prüfen
# =========================================================

if (!exists(main_dataset_object, envir = .GlobalEnv, inherits = FALSE)) {
  stop(
    paste0(
      "The required object '", main_dataset_object, "' is not available in the global environment.\n",
      "TODO: Load the script or data object that creates '", main_dataset_object, "' before running this script."
    ),
    call. = FALSE
  )
}

main_dataset <- get(main_dataset_object, envir = .GlobalEnv)

if (!is.data.frame(main_dataset)) {
  stop(
    paste0("Object '", main_dataset_object, "' exists but is not a data frame."),
    call. = FALSE
  )
}

if (!main_join_var %in% names(main_dataset)) {
  stop(
    paste0(
      "The join variable '", main_join_var, "' is missing in '", main_dataset_object, "'."
    ),
    call. = FALSE
  )
}

already_existing_new_vars <- intersect(
  c(copied_join_var, prompt_coding_vars, derived_prompt_vars),
  names(main_dataset)
)

if (length(already_existing_new_vars) > 0) {
  stop(
    paste0(
      "The following target variable(s) already exist in '", main_dataset_object, "': ",
      paste(already_existing_new_vars, collapse = ", "),
      ". This script does not overwrite existing variables."
    ),
    call. = FALSE
  )
}

if (!file.exists(coded_prompts_file)) {
  stop(
    paste0(
      "The coded prompts file was not found at:\n",
      coded_prompts_file
    ),
    call. = FALSE
  )
}

if (!file.exists(codebook_file)) {
  warning(
    paste0(
      "The codebook file was not found at:\n",
      codebook_file,
      "\nThe sequence logic below is implemented from the available Chapter 5 rules."
    ),
    call. = FALSE
  )
}

# =========================================================
# 4) Hilfsfunktionen
# =========================================================

normalize_function_code <- function(x) {
  x <- normalize_missing_text(x)
  x <- toupper(x)
  x
}

# CHANGED: zusätzliche Normalisierung für Vibe-Codes.
# Erwartete Standardwerte sind "Vibe" und "No Vibe". Häufige Varianten werden
# vereinheitlicht, andere explizite Werte bleiben zur manuellen Kontrolle erhalten.
normalize_vibe_code <- function(x) {
  x <- normalize_missing_text(x)
  x[stringr::str_to_lower(x) %in% c("na", "n.a.", "n/a", "none", "missing")] <- NA_character_

  x_lower <- stringr::str_to_lower(x)

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x_lower %in% c("yes", "y", "1", "true", "present", "vibe", "vibe present") ~ "Vibe",
    x_lower %in% c("no", "n", "0", "false", "absent", "no vibe", "not present") ~ "No Vibe",
    TRUE ~ x
  )
}

# CHANGED: tabellarische Verteilung im Stil der Pre-Survey-Deskriptiven,
# aber zusätzlich segmentiert nach Overall / abstract / concrete.
make_prompt_single_choice_distribution <- function(df,
                                                   var_name,
                                                   question_focus,
                                                   response_levels = NULL,
                                                   analysis_type = "prompt_coding_distribution") {
  make_one_segment <- function(segment_df, segment_label, filter_note) {
    x <- normalize_missing_text(segment_df[[var_name]])

    n_eligible <- nrow(segment_df)
    n_valid <- sum(!is.na(x))
    n_missing <- sum(is.na(x))

    x_out <- ifelse(is.na(x), "Missing", x)

    if (!is.null(response_levels)) {
      observed_extra_levels <- setdiff(unique(x_out), c(response_levels, "Missing", NA_character_))
      level_order <- c(response_levels, observed_extra_levels, "Missing")
      x_out <- factor(x_out, levels = level_order, ordered = TRUE)
    }

    out <- tibble(response = x_out) %>%
      count(response, name = "n", .drop = FALSE)

    if (!is.null(response_levels)) {
      out <- out %>%
        mutate(
          response_chr = as.character(response),
          response_order = match(response_chr, c(response_levels, setdiff(response_chr, c(response_levels, "Missing")), "Missing"))
        ) %>%
        arrange(response_order) %>%
        transmute(response = response_chr, n = n)
    } else {
      out <- out %>%
        mutate(response = as.character(response)) %>%
        arrange(desc(response != "Missing"), desc(n), response)
    }

    out %>%
      mutate(
        dataset = "Final consolidated sample with prompt coding",
        target_word_category = segment_label,
        variable_name = var_name,
        question_focus = question_focus,
        analysis_type = analysis_type,
        filter_note = filter_note,
        denominator_note = "Percent of eligible cases and percent of valid non-missing coding values.",
        n_eligible = n_eligible,
        n_valid = n_valid,
        n_missing = n_missing,
        percent_of_eligible = pct(n, n_eligible),
        percent_of_valid = if_else(response == "Missing", NA_real_, pct(n, n_valid)),
        .before = 1
      )
  }

  bind_rows(
    make_one_segment(
      df,
      segment_label = "Overall",
      filter_note = "All cases in the final consolidated sample with prompt coding"
    ),
    make_one_segment(
      df %>% filter(Main_Survey_target_word_category == "abstract"),
      segment_label = "abstract",
      filter_note = 'Only cases with Main_Survey_target_word_category == "abstract"'
    ),
    make_one_segment(
      df %>% filter(Main_Survey_target_word_category == "concrete"),
      segment_label = "concrete",
      filter_note = 'Only cases with Main_Survey_target_word_category == "concrete"'
    )
  ) %>%
    mutate(
      target_word_category = factor(target_word_category, levels = c("Overall", "abstract", "concrete"), ordered = TRUE)
    ) %>%
    arrange(target_word_category, variable_name) %>%
    mutate(target_word_category = as.character(target_word_category))
}

capture_column_attributes <- function(df) {
  stats::setNames(lapply(df, attributes), names(df))
}

restore_column_attributes <- function(df, attribute_list) {
  original_names <- intersect(names(attribute_list), names(df))

  for (nm in original_names) {
    old_attributes <- attribute_list[[nm]]

    if (is.null(old_attributes)) {
      next
    }

    attributes_to_restore <- setdiff(names(old_attributes), "names")

    for (att_name in attributes_to_restore) {
      attr(df[[nm]], att_name) <- old_attributes[[att_name]]
    }
  }

  df
}

# =========================================================
# 5) Coding-Datei einlesen
# =========================================================

coded_prompts_raw <- read_csv_auto(coded_prompts_file)

required_coding_columns <- c(coding_join_var, prompt_coding_vars)

missing_coding_columns <- setdiff(required_coding_columns, names(coded_prompts_raw))

if (length(missing_coding_columns) > 0) {
  stop(
    paste0(
      "The coded prompts file is missing the following required column(s): ",
      paste(missing_coding_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}

coded_prompts_prepared <- coded_prompts_raw %>%
  select(all_of(required_coding_columns)) %>%
  mutate(
    across(all_of(required_coding_columns), normalize_missing_text),
    across(all_of(function_vars), normalize_function_code),
    across(all_of(vibe_vars), normalize_vibe_code) # CHANGED
  ) %>%
  rename(join_response_id = all_of(coding_join_var))

# CHANGED: harte Prüfung der erlaubten Vibe-Werte.
# Erlaubt sind ausschließlich "No Vibe" und "Vibe".
invalid_vibe_values <- coded_prompts_prepared %>%
  select(all_of(vibe_vars)) %>%
  pivot_longer(cols = everything(), names_to = "vibe_variable", values_to = "vibe_value") %>%
  filter(!is.na(vibe_value), !vibe_value %in% vibe_levels) %>%
  distinct(vibe_variable, vibe_value) %>%
  arrange(vibe_variable, vibe_value)

if (nrow(invalid_vibe_values) > 0) {
  stop(
    paste0(
      "Invalid Vibe coding values found. Allowed values are only 'No Vibe' and 'Vibe'.\n",
      paste(
        paste0(invalid_vibe_values$vibe_variable, ": ", invalid_vibe_values$vibe_value),
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

# =========================================================
# 6) Plausibilitätsprüfungen vor dem Join
# =========================================================

main_join_base <- main_dataset %>%
  mutate(join_response_id = normalize_missing_text(.data[[main_join_var]]))

duplicate_coding_ids <- coded_prompts_prepared %>%
  filter(!is.na(join_response_id)) %>%
  add_count(join_response_id, name = "id_frequency") %>%
  filter(id_frequency > 1) %>%
  arrange(desc(id_frequency), join_response_id)

unmatched_main_ids <- main_join_base %>%
  filter(!is.na(join_response_id)) %>%
  distinct(join_response_id) %>%
  anti_join(
    coded_prompts_prepared %>%
      filter(!is.na(join_response_id)) %>%
      distinct(join_response_id),
    by = "join_response_id"
  ) %>%
  arrange(join_response_id)

unmatched_coding_ids <- coded_prompts_prepared %>%
  filter(!is.na(join_response_id)) %>%
  distinct(join_response_id) %>%
  anti_join(
    main_join_base %>%
      filter(!is.na(join_response_id)) %>%
      distinct(join_response_id),
    by = "join_response_id"
  ) %>%
  arrange(join_response_id)

prompt_coding_plausibility_summary <- tibble(
  check = c(
    "Rows in main dataset",
    "Rows in coded prompts file",
    "Non-missing Response IDs in main dataset",
    "Non-missing Response IDs in coded prompts file",
    "Unique non-missing Response IDs in main dataset",
    "Unique non-missing Response IDs in coded prompts file",
    "Matching unique Response IDs in both datasets",
    "Unique Response IDs only in main dataset",
    "Unique Response IDs only in coded prompts file",
    "Rows with duplicate Response IDs in coded prompts file",
    "Unique duplicate Response IDs in coded prompts file"
  ),
  n = c(
    nrow(main_dataset),
    nrow(coded_prompts_prepared),
    sum(!is.na(main_join_base$join_response_id)),
    sum(!is.na(coded_prompts_prepared$join_response_id)),
    dplyr::n_distinct(stats::na.omit(main_join_base$join_response_id)),
    dplyr::n_distinct(stats::na.omit(coded_prompts_prepared$join_response_id)),
    length(intersect(
      stats::na.omit(unique(main_join_base$join_response_id)),
      stats::na.omit(unique(coded_prompts_prepared$join_response_id))
    )),
    nrow(unmatched_main_ids),
    nrow(unmatched_coding_ids),
    nrow(duplicate_coding_ids),
    dplyr::n_distinct(duplicate_coding_ids$join_response_id)
  )
)

message(
  paste0(
    "Prompt-coding plausibility check: ",
    nrow(unmatched_main_ids), " unique Response ID(s) exist only in the main dataset; ",
    nrow(unmatched_coding_ids), " unique Response ID(s) exist only in the coding file; ",
    dplyr::n_distinct(duplicate_coding_ids$join_response_id), " unique duplicate Response ID(s) were found in the coding file."
  )
)

# CHANGED: Plausibility objects are kept for internal checks, but not exported as separate tables.

if (nrow(duplicate_coding_ids) > 0) {
  stop(
    paste0(
      "The coded prompts file contains duplicate Response IDs. A safe join by Response ID is therefore not possible. ",
      "Please resolve the duplicate Response IDs in the coding file first."
    ),
    call. = FALSE
  )
}

# =========================================================
# 7) Coding-Variablen an Main-Datensatz anfügen
# =========================================================

original_main_attributes <- capture_column_attributes(main_dataset)

coded_prompts_unique <- coded_prompts_prepared %>%
  distinct(join_response_id, .keep_all = TRUE)

final_analysis_dataset_full_viviq_prompt_coding <- main_join_base %>%
  left_join(
    coded_prompts_unique,
    by = "join_response_id"
  ) %>%
  mutate(
    Case_Response_ID = join_response_id
  ) %>%
  select(-join_response_id)

final_analysis_dataset_full_viviq_prompt_coding <- restore_column_attributes(
  df = final_analysis_dataset_full_viviq_prompt_coding,
  attribute_list = original_main_attributes
)

# =========================================================
# 8) Overall_Sequence und Case_type erzeugen
# =========================================================

final_analysis_dataset_full_viviq_prompt_coding <- final_analysis_dataset_full_viviq_prompt_coding %>%
  mutate(
    Overall_Sequence = dplyr::case_when(
      !is.na(R1_Function) & !is.na(R2_Function) & !is.na(R3_Function) ~
        paste(R1_Function, R2_Function, R3_Function, sep = "-"),
      TRUE ~ NA_character_
    ),
    Case_type = dplyr::case_when(
      Overall_Sequence == "F1-F2-F2" ~ "S1 - Iterative Refinement",
      Overall_Sequence == "F1-F3-F3" ~ "S2 - Double Re-specification",
      Overall_Sequence == "F1-F3-F2" ~ "S3 - Re-specification with Final Refinement",
      Overall_Sequence == "F1-F2-F3" ~ "S4 - Refinement Followed by Re-specification",
      Overall_Sequence == "F1-F2-F4" ~ "S5 - Refinement with Reversion",
      TRUE ~ NA_character_
    )
  )

# TODO:
# Falls weitere Funktionssequenzen auftreten, die nicht durch S1-S5
# abgedeckt sind, müssen diese hier explizit ergänzt und dokumentiert werden.
# Es wird bewusst keine zusätzliche Klassifikationslogik erfunden.

unclassified_sequences <- final_analysis_dataset_full_viviq_prompt_coding %>%
  filter(!is.na(Overall_Sequence), is.na(Case_type)) %>%
  count(Overall_Sequence, sort = TRUE, name = "n_cases")

sequence_review <- final_analysis_dataset_full_viviq_prompt_coding %>%
  count(Overall_Sequence, Case_type, sort = TRUE, name = "n_cases")

# =========================================================
# 8b) Prompt-Coding-Tabellen gemäß finaler Auswahl
# =========================================================

# CHANGED:
# Dieser Reporting-Teil erzeugt nur noch die gewünschten Prompt-Coding-Tabellen:
# 1) Observed prompt sequences by target word category mit allen S1-S5 Case_type-Werten
# 2) Level-Sequenzen nach target word category
# 3) Vibe-Sequenzen nach target word category
# 4) Level-Übersicht über alle Runden nach target word category
# 5) Level-Übersicht je Runde nach target word category
# 6) Vibe-Übersicht über alle Runden nach target word category
# 7) Vibe-Übersicht je Runde nach target word category
# Alle übrigen inhaltlichen Reporting-Tabellen aus Script 07 wurden entfernt.

category_levels <- c("Overall", "abstract", "concrete")
round_levels <- c("R1", "R2", "R3")
level_code_levels <- c("L1", "L2", "L3")
level_label_lookup <- c(
  "L1" = "L1 - low",
  "L2" = "L2 - medium",
  "L3" = "L3 - high"
)
vibe_code_levels <- c("No Vibe", "Vibe")
vibe_label_lookup <- c(
  "No Vibe" = "No Vibe",
  "Vibe" = "Vibe"
)

case_type_lookup <- tibble::tribble(
  ~Overall_Sequence, ~Case_type,
  "F1-F2-F2", "S1 - Iterative Refinement",
  "F1-F3-F3", "S2 - Double Re-specification",
  "F1-F3-F2", "S3 - Re-specification with Final Refinement",
  "F1-F2-F3", "S4 - Refinement Followed by Re-specification",
  "F1-F2-F4", "S5 - Refinement with Reversion"
) %>%
  mutate(
    Case_type = factor(
      Case_type,
      levels = c(
        "S1 - Iterative Refinement",
        "S2 - Double Re-specification",
        "S3 - Re-specification with Final Refinement",
        "S4 - Refinement Followed by Re-specification",
        "S5 - Refinement with Reversion"
      ),
      ordered = TRUE
    )
  )

make_category_augmented_data <- function(df) {
  bind_rows(
    df %>% mutate(target_word_category_reporting = "Overall"),
    df %>%
      filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
      mutate(target_word_category_reporting = Main_Survey_target_word_category)
  ) %>%
    mutate(
      target_word_category_reporting = factor(
        target_word_category_reporting,
        levels = category_levels,
        ordered = TRUE
      )
    )
}

prompt_coding_category_data <- make_category_augmented_data(
  final_analysis_dataset_full_viviq_prompt_coding
)

# =========================================================
# 8b.1) Observed prompt sequences by target word category
# =========================================================

observed_prompt_sequences_by_category <- prompt_coding_category_data %>%
  filter(!is.na(Case_type)) %>%
  count(target_word_category_reporting, Overall_Sequence, Case_type, name = "n_cases") %>%
  right_join(
    tidyr::expand_grid(
      target_word_category_reporting = factor(category_levels, levels = category_levels, ordered = TRUE),
      case_type_lookup
    ),
    by = c("target_word_category_reporting", "Overall_Sequence", "Case_type")
  ) %>%
  mutate(n_cases = replace_na(n_cases, 0L)) %>%
  group_by(target_word_category_reporting) %>%
  mutate(
    n_eligible = sum(n_cases),
    percent_of_eligible = pct(n_cases, n_eligible)
  ) %>%
  ungroup() %>%
  arrange(target_word_category_reporting, Case_type) %>%
  mutate(
    target_word_category = as.character(target_word_category_reporting),
    Case_type = as.character(Case_type)
  ) %>%
  select(
    target_word_category,
    Overall_Sequence,
    Case_type,
    n_eligible,
    n_cases,
    percent_of_eligible
  )

# =========================================================
# 8b.2) Level-Sequenzen nach target word category
# =========================================================

level_sequence_lookup <- tidyr::expand_grid(
  R1_Level = level_code_levels,
  R2_Level = level_code_levels,
  R3_Level = level_code_levels
) %>%
  mutate(
    Level_Sequence = paste(R1_Level, R2_Level, R3_Level, sep = "-"),
    Level_Sequence = factor(Level_Sequence, levels = Level_Sequence, ordered = TRUE)
  )

level_sequences_by_category <- prompt_coding_category_data %>%
  mutate(
    Level_Sequence = case_when(
      !is.na(R1_Level) & !is.na(R2_Level) & !is.na(R3_Level) ~
        paste(R1_Level, R2_Level, R3_Level, sep = "-"),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Level_Sequence)) %>%
  count(target_word_category_reporting, Level_Sequence, name = "n_cases") %>%
  right_join(
    tidyr::expand_grid(
      target_word_category_reporting = factor(category_levels, levels = category_levels, ordered = TRUE),
      Level_Sequence = level_sequence_lookup$Level_Sequence
    ),
    by = c("target_word_category_reporting", "Level_Sequence")
  ) %>%
  mutate(n_cases = replace_na(n_cases, 0L)) %>%
  left_join(
    level_sequence_lookup %>% select(Level_Sequence, R1_Level, R2_Level, R3_Level),
    by = "Level_Sequence"
  ) %>%
  group_by(target_word_category_reporting) %>%
  mutate(
    n_eligible = sum(n_cases),
    percent_of_eligible = pct(n_cases, n_eligible)
  ) %>%
  ungroup() %>%
  arrange(target_word_category_reporting, Level_Sequence) %>%
  mutate(
    target_word_category = as.character(target_word_category_reporting),
    Level_Sequence = as.character(Level_Sequence)
  ) %>%
  select(
    target_word_category,
    Level_Sequence,
    R1_Level,
    R2_Level,
    R3_Level,
    n_eligible,
    n_cases,
    percent_of_eligible
  )

# =========================================================
# 8b.3) Vibe-Sequenzen nach target word category
# =========================================================

vibe_sequence_lookup <- tidyr::expand_grid(
  R1_Vibe = vibe_code_levels,
  R2_Vibe = vibe_code_levels,
  R3_Vibe = vibe_code_levels
) %>%
  mutate(
    Vibe_Sequence = paste(R1_Vibe, R2_Vibe, R3_Vibe, sep = "-"),
    Vibe_Sequence = factor(Vibe_Sequence, levels = Vibe_Sequence, ordered = TRUE)
  )

vibe_sequences_by_category <- prompt_coding_category_data %>%
  mutate(
    Vibe_Sequence = case_when(
      !is.na(R1_Vibe) & !is.na(R2_Vibe) & !is.na(R3_Vibe) ~
        paste(R1_Vibe, R2_Vibe, R3_Vibe, sep = "-"),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Vibe_Sequence)) %>%
  count(target_word_category_reporting, Vibe_Sequence, name = "n_cases") %>%
  right_join(
    tidyr::expand_grid(
      target_word_category_reporting = factor(category_levels, levels = category_levels, ordered = TRUE),
      Vibe_Sequence = vibe_sequence_lookup$Vibe_Sequence
    ),
    by = c("target_word_category_reporting", "Vibe_Sequence")
  ) %>%
  mutate(n_cases = replace_na(n_cases, 0L)) %>%
  left_join(
    vibe_sequence_lookup %>% select(Vibe_Sequence, R1_Vibe, R2_Vibe, R3_Vibe),
    by = "Vibe_Sequence"
  ) %>%
  group_by(target_word_category_reporting) %>%
  mutate(
    n_eligible = sum(n_cases),
    percent_of_eligible = pct(n_cases, n_eligible)
  ) %>%
  ungroup() %>%
  arrange(target_word_category_reporting, Vibe_Sequence) %>%
  mutate(
    target_word_category = as.character(target_word_category_reporting),
    Vibe_Sequence = as.character(Vibe_Sequence)
  ) %>%
  select(
    target_word_category,
    Vibe_Sequence,
    R1_Vibe,
    R2_Vibe,
    R3_Vibe,
    n_eligible,
    n_cases,
    percent_of_eligible
  )

# =========================================================
# 8b.4) Level-Übersicht über alle Runden nach Kategorie
# =========================================================

prompt_level_overview_all_rounds_by_category <- prompt_coding_category_data %>%
  select(target_word_category_reporting, all_of(level_vars)) %>%
  pivot_longer(cols = all_of(level_vars), names_to = "round", values_to = "Level") %>%
  mutate(
    round = recode(round, R1_Level = "R1", R2_Level = "R2", R3_Level = "R3"),
    Level = factor(Level, levels = level_code_levels, ordered = TRUE)
  ) %>%
  filter(!is.na(Level)) %>%
  count(target_word_category_reporting, Level, name = "n_codes") %>%
  right_join(
    tidyr::expand_grid(
      target_word_category_reporting = factor(category_levels, levels = category_levels, ordered = TRUE),
      Level = factor(level_code_levels, levels = level_code_levels, ordered = TRUE)
    ),
    by = c("target_word_category_reporting", "Level")
  ) %>%
  mutate(n_codes = replace_na(n_codes, 0L)) %>%
  group_by(target_word_category_reporting) %>%
  mutate(
    n_eligible_codes = sum(n_codes),
    percent_of_eligible_codes = pct(n_codes, n_eligible_codes)
  ) %>%
  ungroup() %>%
  arrange(target_word_category_reporting, Level) %>%
  mutate(
    target_word_category = as.character(target_word_category_reporting),
    Level = as.character(Level),
    Level_label = unname(level_label_lookup[Level])
  ) %>%
  select(target_word_category, Level, Level_label, n_eligible_codes, n_codes, percent_of_eligible_codes)

# =========================================================
# 8b.5) Level-Übersicht je Runde nach Kategorie
# =========================================================

prompt_level_overview_by_round_and_category <- prompt_coding_category_data %>%
  select(target_word_category_reporting, all_of(level_vars)) %>%
  pivot_longer(cols = all_of(level_vars), names_to = "round", values_to = "Level") %>%
  mutate(
    round = recode(round, R1_Level = "R1", R2_Level = "R2", R3_Level = "R3"),
    round = factor(round, levels = round_levels, ordered = TRUE),
    Level = factor(Level, levels = level_code_levels, ordered = TRUE)
  ) %>%
  filter(!is.na(Level)) %>%
  count(target_word_category_reporting, round, Level, name = "n_codes") %>%
  right_join(
    tidyr::expand_grid(
      target_word_category_reporting = factor(category_levels, levels = category_levels, ordered = TRUE),
      round = factor(round_levels, levels = round_levels, ordered = TRUE),
      Level = factor(level_code_levels, levels = level_code_levels, ordered = TRUE)
    ),
    by = c("target_word_category_reporting", "round", "Level")
  ) %>%
  mutate(n_codes = replace_na(n_codes, 0L)) %>%
  group_by(target_word_category_reporting, round) %>%
  mutate(
    n_eligible_codes = sum(n_codes),
    percent_of_eligible_codes = pct(n_codes, n_eligible_codes)
  ) %>%
  ungroup() %>%
  arrange(target_word_category_reporting, round, Level) %>%
  mutate(
    target_word_category = as.character(target_word_category_reporting),
    round = as.character(round),
    Level = as.character(Level),
    Level_label = unname(level_label_lookup[Level])
  ) %>%
  select(target_word_category, round, Level, Level_label, n_eligible_codes, n_codes, percent_of_eligible_codes)

# =========================================================
# 8b.6) Vibe-Übersicht über alle Runden nach Kategorie
# =========================================================

prompt_vibe_overview_all_rounds_by_category <- prompt_coding_category_data %>%
  select(target_word_category_reporting, all_of(vibe_vars)) %>%
  pivot_longer(cols = all_of(vibe_vars), names_to = "round", values_to = "Vibe") %>%
  mutate(
    round = recode(round, R1_Vibe = "R1", R2_Vibe = "R2", R3_Vibe = "R3"),
    Vibe = factor(Vibe, levels = vibe_code_levels, ordered = TRUE)
  ) %>%
  filter(!is.na(Vibe)) %>%
  count(target_word_category_reporting, Vibe, name = "n_codes") %>%
  right_join(
    tidyr::expand_grid(
      target_word_category_reporting = factor(category_levels, levels = category_levels, ordered = TRUE),
      Vibe = factor(vibe_code_levels, levels = vibe_code_levels, ordered = TRUE)
    ),
    by = c("target_word_category_reporting", "Vibe")
  ) %>%
  mutate(n_codes = replace_na(n_codes, 0L)) %>%
  group_by(target_word_category_reporting) %>%
  mutate(
    n_eligible_codes = sum(n_codes),
    percent_of_eligible_codes = pct(n_codes, n_eligible_codes)
  ) %>%
  ungroup() %>%
  arrange(target_word_category_reporting, Vibe) %>%
  mutate(
    target_word_category = as.character(target_word_category_reporting),
    Vibe = as.character(Vibe),
    Vibe_label = unname(vibe_label_lookup[Vibe])
  ) %>%
  select(target_word_category, Vibe, Vibe_label, n_eligible_codes, n_codes, percent_of_eligible_codes)

# =========================================================
# 8b.7) Vibe-Übersicht je Runde nach Kategorie
# =========================================================

prompt_vibe_overview_by_round_and_category <- prompt_coding_category_data %>%
  select(target_word_category_reporting, all_of(vibe_vars)) %>%
  pivot_longer(cols = all_of(vibe_vars), names_to = "round", values_to = "Vibe") %>%
  mutate(
    round = recode(round, R1_Vibe = "R1", R2_Vibe = "R2", R3_Vibe = "R3"),
    round = factor(round, levels = round_levels, ordered = TRUE),
    Vibe = factor(Vibe, levels = vibe_code_levels, ordered = TRUE)
  ) %>%
  filter(!is.na(Vibe)) %>%
  count(target_word_category_reporting, round, Vibe, name = "n_codes") %>%
  right_join(
    tidyr::expand_grid(
      target_word_category_reporting = factor(category_levels, levels = category_levels, ordered = TRUE),
      round = factor(round_levels, levels = round_levels, ordered = TRUE),
      Vibe = factor(vibe_code_levels, levels = vibe_code_levels, ordered = TRUE)
    ),
    by = c("target_word_category_reporting", "round", "Vibe")
  ) %>%
  mutate(n_codes = replace_na(n_codes, 0L)) %>%
  group_by(target_word_category_reporting, round) %>%
  mutate(
    n_eligible_codes = sum(n_codes),
    percent_of_eligible_codes = pct(n_codes, n_eligible_codes)
  ) %>%
  ungroup() %>%
  arrange(target_word_category_reporting, round, Vibe) %>%
  mutate(
    target_word_category = as.character(target_word_category_reporting),
    round = as.character(round),
    Vibe = as.character(Vibe),
    Vibe_label = unname(vibe_label_lookup[Vibe])
  ) %>%
  select(target_word_category, round, Vibe, Vibe_label, n_eligible_codes, n_codes, percent_of_eligible_codes)


# =========================================================
# 8b.8) Fälle mit L3/L1-Kombinationen nach Kategorie
# =========================================================

# CHANGED:
# Diese Tabelle zählt Fälle/Antworten nach target word category,
# abhängig davon, wie oft L3 bzw. L1 in R1_Level, R2_Level und R3_Level vorkommt.
# Gezählt werden:
# - mindestens 1x L3
# - mindestens 2x L3
# - mindestens 3x L3
# - genau 1x L1

prompt_level_case_l3_l1_summary_by_category <- prompt_coding_category_data %>%
  mutate(
    n_l3_in_case = rowSums(across(all_of(level_vars), ~ .x == "L3"), na.rm = TRUE),
    n_l1_in_case = rowSums(across(all_of(level_vars), ~ .x == "L1"), na.rm = TRUE),
    has_complete_level_sequence = if_all(all_of(level_vars), ~ !is.na(.x))
  ) %>%
  group_by(target_word_category_reporting) %>%
  summarise(
    n_eligible_cases = n(),
    n_complete_level_sequences = sum(has_complete_level_sequence, na.rm = TRUE),
    n_cases_at_least_1_l3 = sum(n_l3_in_case >= 1, na.rm = TRUE),
    percent_cases_at_least_1_l3 = pct(n_cases_at_least_1_l3, n_eligible_cases),
    n_cases_at_least_2_l3 = sum(n_l3_in_case >= 2, na.rm = TRUE),
    percent_cases_at_least_2_l3 = pct(n_cases_at_least_2_l3, n_eligible_cases),
    n_cases_at_least_3_l3 = sum(n_l3_in_case >= 3, na.rm = TRUE),
    percent_cases_at_least_3_l3 = pct(n_cases_at_least_3_l3, n_eligible_cases),
    n_cases_exactly_1_l1 = sum(n_l1_in_case == 1, na.rm = TRUE),
    percent_cases_exactly_1_l1 = pct(n_cases_exactly_1_l1, n_eligible_cases),
    .groups = "drop"
  ) %>%
  arrange(target_word_category_reporting) %>%
  mutate(
    target_word_category = as.character(target_word_category_reporting)
  ) %>%
  select(
    target_word_category,
    n_eligible_cases,
    n_complete_level_sequences,
    n_cases_at_least_1_l3,
    percent_cases_at_least_1_l3,
    n_cases_at_least_2_l3,
    percent_cases_at_least_2_l3,
    n_cases_at_least_3_l3,
    percent_cases_at_least_3_l3,
    n_cases_exactly_1_l1,
    percent_cases_exactly_1_l1
  )



# =========================================================
# 8b.9) Fälle mit nicht vollständig eligible Level-/Vibe-Codes
# =========================================================

# CHANGED:
# Diese Tabelle listet Fälle auf, die für Level oder Vibe nicht vollständig
# eligible sind, weil über R1-R3 nicht alle drei Codes gültig vorhanden sind.
# Level-eligible: R1_Level, R2_Level, R3_Level sind jeweils L1/L2/L3.
# Vibe-eligible: R1_Vibe, R2_Vibe, R3_Vibe sind jeweils No Vibe/Vibe.

prompt_level_vibe_not_eligible_cases <- final_analysis_dataset_full_viviq_prompt_coding %>%
  mutate(
    n_eligible_level_codes = rowSums(
      across(all_of(level_vars), ~ .x %in% level_code_levels),
      na.rm = TRUE
    ),
    n_eligible_vibe_codes = rowSums(
      across(all_of(vibe_vars), ~ .x %in% vibe_code_levels),
      na.rm = TRUE
    ),
    level_not_fully_eligible = n_eligible_level_codes < length(level_vars),
    vibe_not_fully_eligible = n_eligible_vibe_codes < length(vibe_vars),
    non_eligible_reason = case_when(
      level_not_fully_eligible & vibe_not_fully_eligible ~ "Level and Vibe not fully eligible",
      level_not_fully_eligible ~ "Level not fully eligible",
      vibe_not_fully_eligible ~ "Vibe not fully eligible",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(level_not_fully_eligible | vibe_not_fully_eligible) %>%
  transmute(
    Case_ID,
    Case_Response_ID,
    Main_Survey_target_word_category,
    n_eligible_level_codes,
    n_eligible_vibe_codes,
    non_eligible_reason,
    R1_Level,
    R2_Level,
    R3_Level,
    R1_Vibe,
    R2_Vibe,
    R3_Vibe
  ) %>%
  arrange(Main_Survey_target_word_category, non_eligible_reason, Case_ID)

# =========================================================
# 8b.10) Fälle mit No-Vibe-Kombinationen nach Kategorie
# =========================================================

# CHANGED:
# Analog zu prompt_level_case_l3_l1_summary_by_category, aber für Vibe.
# Gezählt wird, wie viele Fälle über R1_Vibe, R2_Vibe und R3_Vibe
# mindestens 1x, mindestens 2x und mindestens 3x No Vibe enthalten.
# Bei drei Runden entspricht mindestens 3x No Vibe exakt 3x No Vibe.

prompt_vibe_case_no_vibe_summary_by_category <- prompt_coding_category_data %>%
  mutate(
    n_no_vibe_in_case = rowSums(across(all_of(vibe_vars), ~ .x == "No Vibe"), na.rm = TRUE),
    has_complete_vibe_sequence = if_all(all_of(vibe_vars), ~ .x %in% vibe_code_levels)
  ) %>%
  group_by(target_word_category_reporting) %>%
  summarise(
    n_eligible_cases = n(),
    n_complete_vibe_sequences = sum(has_complete_vibe_sequence, na.rm = TRUE),
    n_cases_at_least_1_no_vibe = sum(n_no_vibe_in_case >= 1, na.rm = TRUE),
    percent_cases_at_least_1_no_vibe = pct(n_cases_at_least_1_no_vibe, n_eligible_cases),
    n_cases_at_least_2_no_vibe = sum(n_no_vibe_in_case >= 2, na.rm = TRUE),
    percent_cases_at_least_2_no_vibe = pct(n_cases_at_least_2_no_vibe, n_eligible_cases),
    n_cases_at_least_3_no_vibe = sum(n_no_vibe_in_case >= 3, na.rm = TRUE),
    percent_cases_at_least_3_no_vibe = pct(n_cases_at_least_3_no_vibe, n_eligible_cases),
    n_cases_exactly_3_no_vibe = sum(n_no_vibe_in_case == 3, na.rm = TRUE),
    percent_cases_exactly_3_no_vibe = pct(n_cases_exactly_3_no_vibe, n_eligible_cases),
    .groups = "drop"
  ) %>%
  arrange(target_word_category_reporting) %>%
  mutate(
    target_word_category = as.character(target_word_category_reporting)
  ) %>%
  select(
    target_word_category,
    n_eligible_cases,
    n_complete_vibe_sequences,
    n_cases_at_least_1_no_vibe,
    percent_cases_at_least_1_no_vibe,
    n_cases_at_least_2_no_vibe,
    percent_cases_at_least_2_no_vibe,
    n_cases_at_least_3_no_vibe,
    percent_cases_at_least_3_no_vibe,
    n_cases_exactly_3_no_vibe,
    percent_cases_exactly_3_no_vibe
  )

prompt_coding_table_notes <- tibble::tribble(
  ~object_name, ~title_text, ~subtitle_text, ~source_note,
  "observed_prompt_sequences_by_category", "Observed prompt sequences by target word category", "All S1-S5 case types shown for Overall, abstract and concrete", "Remark: Case types are based on the ordered sequence of R1-R3 function codes. Rows with n = 0 are retained.",
  "level_sequences_by_category", "Observed prompt level sequences by target word category", "All L1-L3 round-level sequences shown for Overall, abstract and concrete", "Remark: Level sequences concatenate R1_Level, R2_Level and R3_Level. Rows with n = 0 are retained.",
  "vibe_sequences_by_category", "Observed vibe prompting sequences by target word category", "All No Vibe/Vibe round-level sequences shown for Overall, abstract and concrete", "Remark: Vibe sequences concatenate R1_Vibe, R2_Vibe and R3_Vibe. Rows with n = 0 are retained.",
  "prompt_level_overview_all_rounds_by_category", "Prompt-engineering level across all rounds by target word category", "L1, L2 and L3 across R1-R3 combined", "Remark: Percentages are based on all non-missing round-level level codes within each target word category.",
  "prompt_level_overview_by_round_and_category", "Prompt-engineering level by round and target word category", "L1, L2 and L3 separately for R1, R2 and R3", "Remark: Sorted by target word category, then round, then level.",
  "prompt_vibe_overview_all_rounds_by_category", "Vibe prompting across all rounds by target word category", "No Vibe/Vibe coding across R1-R3 combined", "Remark: Percentages are based on all non-missing round-level vibe codes within each target word category.",
  "prompt_vibe_overview_by_round_and_category", "Vibe prompting by round and target word category", "No Vibe/Vibe coding separately for R1, R2 and R3", "Remark: Sorted by target word category, then round, then vibe code (No Vibe before Vibe).",
  "prompt_level_case_l3_l1_summary_by_category", "Cases with L3 and L1 occurrences by target word category", "Counts of cases with at least one, two or three L3 codes and exactly one L1 code", "Remark: Counts are based on R1_Level, R2_Level and R3_Level within each target word category. Percentages use n_eligible_cases as denominator.",
  "prompt_level_vibe_not_eligible_cases", "Cases not fully eligible for Level or Vibe coding", "Cases with fewer than three valid Level or Vibe codes across R1-R3", "Remark: This quality-control table lists cases where Level or Vibe coding is incomplete or invalid.",
  "prompt_vibe_case_no_vibe_summary_by_category", "Cases with No Vibe occurrences by target word category", "Counts of cases with at least one, two or three No Vibe codes across R1-R3", "Remark: Counts are based on R1_Vibe, R2_Vibe and R3_Vibe. Percentages use n_eligible_cases as denominator."
)

# =========================================================
# 9) Ergebnisse exportieren
# =========================================================

write_csv_project(
  final_analysis_dataset_full_viviq_prompt_coding,
  file.path(out_prompt_dir, "07_final_analysis_dataset_full_viviq_prompt_coding.csv")
)

save_table_outputs(observed_prompt_sequences_by_category, "07_observed_prompt_sequences_by_category", out_dir = out_prompt_dir)
save_table_outputs(level_sequences_by_category, "07_level_sequences_by_category", out_dir = out_prompt_dir)
save_table_outputs(vibe_sequences_by_category, "07_vibe_sequences_by_category", out_dir = out_prompt_dir)
save_table_outputs(prompt_level_overview_all_rounds_by_category, "07_prompt_level_overview_all_rounds_by_category", out_dir = out_prompt_dir)
save_table_outputs(prompt_level_overview_by_round_and_category, "07_prompt_level_overview_by_round_and_category", out_dir = out_prompt_dir)
save_table_outputs(prompt_vibe_overview_all_rounds_by_category, "07_prompt_vibe_overview_all_rounds_by_category", out_dir = out_prompt_dir)
save_table_outputs(prompt_vibe_overview_by_round_and_category, "07_prompt_vibe_overview_by_round_and_category", out_dir = out_prompt_dir)
save_table_outputs(prompt_level_case_l3_l1_summary_by_category, "07_prompt_level_case_l3_l1_summary_by_category", out_dir = out_prompt_dir)
save_table_outputs(prompt_level_vibe_not_eligible_cases, "07_prompt_level_vibe_not_eligible_cases", out_dir = out_prompt_dir)
save_table_outputs(prompt_vibe_case_no_vibe_summary_by_category, "07_prompt_vibe_case_no_vibe_summary_by_category", out_dir = out_prompt_dir)
save_table_outputs(prompt_coding_table_notes, "07_prompt_coding_table_notes", out_dir = out_document_dir)

writexl::write_xlsx(
  list(
    observed_prompt_sequences_by_category = observed_prompt_sequences_by_category,
    level_sequences_by_category = level_sequences_by_category,
    vibe_sequences_by_category = vibe_sequences_by_category,
    prompt_level_overview_all_rounds_by_category = prompt_level_overview_all_rounds_by_category,
    prompt_level_overview_by_round_and_category = prompt_level_overview_by_round_and_category,
    prompt_vibe_overview_all_rounds_by_category = prompt_vibe_overview_all_rounds_by_category,
    prompt_vibe_overview_by_round_and_category = prompt_vibe_overview_by_round_and_category,
    prompt_level_case_l3_l1_summary_by_category = prompt_level_case_l3_l1_summary_by_category,
    prompt_level_vibe_not_eligible_cases = prompt_level_vibe_not_eligible_cases,
    prompt_vibe_case_no_vibe_summary_by_category = prompt_vibe_case_no_vibe_summary_by_category,
    prompt_coding_table_notes = prompt_coding_table_notes
  ),
  path = file.path(out_prompt_dir, "07_prompt_coding_outputs.xlsx")
)

# =========================================================
# 10) GT-Tabellen erzeugen
# =========================================================

gt_observed_prompt_sequences_by_category <- make_gt_table_standard(
  observed_prompt_sequences_by_category,
  title_text = "Observed prompt sequences by target word category",
  subtitle_text = "All S1-S5 case types shown for Overall, abstract and concrete",
  source_note = "Remark: Case types are based on the ordered sequence of R1-R3 function codes. Rows with n = 0 are retained."
)

gt_level_sequences_by_category <- make_gt_table_standard(
  level_sequences_by_category,
  title_text = "Observed prompt level sequences by target word category",
  subtitle_text = "All L1-L3 round-level sequences shown for Overall, abstract and concrete",
  source_note = "Remark: Level sequences concatenate R1_Level, R2_Level and R3_Level. Rows with n = 0 are retained."
)

gt_vibe_sequences_by_category <- make_gt_table_standard(
  vibe_sequences_by_category,
  title_text = "Observed vibe prompting sequences by target word category",
  subtitle_text = "All No Vibe/Vibe round-level sequences shown for Overall, abstract and concrete",
  source_note = "Remark: Vibe sequences concatenate R1_Vibe, R2_Vibe and R3_Vibe. Rows with n = 0 are retained."
)

gt_prompt_level_overview_all_rounds_by_category <- make_gt_table_standard(
  prompt_level_overview_all_rounds_by_category,
  title_text = "Prompt-engineering level across all rounds by target word category",
  subtitle_text = "L1, L2 and L3 across R1-R3 combined",
  source_note = "Remark: Percentages are based on all non-missing round-level level codes within each target word category."
)

gt_prompt_level_overview_by_round_and_category <- make_gt_table_standard(
  prompt_level_overview_by_round_and_category,
  title_text = "Prompt-engineering level by round and target word category",
  subtitle_text = "L1, L2 and L3 separately for R1, R2 and R3",
  source_note = "Remark: Sorted by target word category, then round, then level."
)

gt_prompt_vibe_overview_all_rounds_by_category <- make_gt_table_standard(
  prompt_vibe_overview_all_rounds_by_category,
  title_text = "Vibe prompting across all rounds by target word category",
  subtitle_text = "No Vibe/Vibe coding across R1-R3 combined",
  source_note = "Remark: Percentages are based on all non-missing round-level vibe codes within each target word category."
)

gt_prompt_vibe_overview_by_round_and_category <- make_gt_table_standard(
  prompt_vibe_overview_by_round_and_category,
  title_text = "Vibe prompting by round and target word category",
  subtitle_text = "No Vibe/Vibe coding separately for R1, R2 and R3",
  source_note = "Remark: Sorted by target word category, then round, then vibe code (No Vibe before Vibe)."
)

gt_prompt_level_case_l3_l1_summary_by_category <- make_gt_table_standard(
  prompt_level_case_l3_l1_summary_by_category,
  title_text = "Cases with L3 and L1 occurrences by target word category",
  subtitle_text = "At least 1x, 2x or 3x L3 and exactly 1x L1 across R1-R3",
  source_note = "Remark: Counts are based on R1_Level, R2_Level and R3_Level. Percentages use n_eligible_cases as denominator."
)

gt_prompt_level_vibe_not_eligible_cases <- make_gt_table_standard(
  prompt_level_vibe_not_eligible_cases,
  title_text = "Cases not fully eligible for Level or Vibe coding",
  subtitle_text = "Cases with fewer than three valid Level or Vibe codes across R1-R3",
  source_note = "Remark: Level-eligible codes are L1/L2/L3; Vibe-eligible codes are No Vibe/Vibe."
)

gt_prompt_vibe_case_no_vibe_summary_by_category <- make_gt_table_standard(
  prompt_vibe_case_no_vibe_summary_by_category,
  title_text = "Cases with No Vibe occurrences by target word category",
  subtitle_text = "At least 1x, 2x or 3x No Vibe across R1-R3",
  source_note = "Remark: Counts are based on R1_Vibe, R2_Vibe and R3_Vibe. Percentages use n_eligible_cases as denominator."
)

gt_output_list <- list(
  gt_observed_prompt_sequences_by_category = gt_observed_prompt_sequences_by_category,
  gt_level_sequences_by_category = gt_level_sequences_by_category,
  gt_vibe_sequences_by_category = gt_vibe_sequences_by_category,
  gt_prompt_level_overview_all_rounds_by_category = gt_prompt_level_overview_all_rounds_by_category,
  gt_prompt_level_overview_by_round_and_category = gt_prompt_level_overview_by_round_and_category,
  gt_prompt_vibe_overview_all_rounds_by_category = gt_prompt_vibe_overview_all_rounds_by_category,
  gt_prompt_vibe_overview_by_round_and_category = gt_prompt_vibe_overview_by_round_and_category,
  gt_prompt_level_case_l3_l1_summary_by_category = gt_prompt_level_case_l3_l1_summary_by_category,
  gt_prompt_level_vibe_not_eligible_cases = gt_prompt_level_vibe_not_eligible_cases,
  gt_prompt_vibe_case_no_vibe_summary_by_category = gt_prompt_vibe_case_no_vibe_summary_by_category
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
  base_filename = "07_gt_manifest",
  out_dir = out_gt_doc_dir
)

build_simple_html_index(
  manifest = gt_manifest,
  output_path = file.path(out_gt_dir, "00_gt_index.html"),
  title_text = "Prompt coding GT tables",
  intro_text = "Selected prompt-coding tables generated by 07_additional_coding_promts.R"
)

# =========================================================
# 11) Allgemeiner Export-Index
# =========================================================

export_manifest <- tibble::tribble(
  ~label, ~path, ~notes,
  "Merged final dataset with prompt coding (CSV)", file.path(out_prompt_dir, "07_final_analysis_dataset_full_viviq_prompt_coding.csv"), "Raw export",
  "Observed prompt sequences by category (CSV)", file.path(out_prompt_dir, "07_observed_prompt_sequences_by_category.csv"), "Requested table",
  "Level sequences by category (CSV)", file.path(out_prompt_dir, "07_level_sequences_by_category.csv"), "Requested table",
  "Vibe sequences by category (CSV)", file.path(out_prompt_dir, "07_vibe_sequences_by_category.csv"), "Requested table",
  "Level overview across all rounds by category (CSV)", file.path(out_prompt_dir, "07_prompt_level_overview_all_rounds_by_category.csv"), "Requested table",
  "Level overview by round and category (CSV)", file.path(out_prompt_dir, "07_prompt_level_overview_by_round_and_category.csv"), "Requested table",
  "Vibe overview across all rounds by category (CSV)", file.path(out_prompt_dir, "07_prompt_vibe_overview_all_rounds_by_category.csv"), "Requested table",
  "Vibe overview by round and category (CSV)", file.path(out_prompt_dir, "07_prompt_vibe_overview_by_round_and_category.csv"), "Requested table",
  "L3/L1 case summary by category (CSV)", file.path(out_prompt_dir, "07_prompt_level_case_l3_l1_summary_by_category.csv"), "Requested table",
  "Level/Vibe not fully eligible cases (CSV)", file.path(out_prompt_dir, "07_prompt_level_vibe_not_eligible_cases.csv"), "Quality-control table",
  "No Vibe case summary by category (CSV)", file.path(out_prompt_dir, "07_prompt_vibe_case_no_vibe_summary_by_category.csv"), "Requested table",
  "Prompt coding outputs workbook (XLSX)", file.path(out_prompt_dir, "07_prompt_coding_outputs.xlsx"), "Combined workbook with requested tables",
  "GT index", file.path(out_gt_dir, "00_gt_index.html"), "GT tables"
)

save_table_outputs(
  export_manifest,
  base_filename = "07_prompt_coding_export_manifest",
  out_dir = out_document_dir
)

build_general_export_index(
  manifest = export_manifest,
  output_path = file.path(out_document_dir, "00_prompt_coding_index.html"),
  title_text = "Prompt coding outputs",
  intro_text = "General export index of selected prompt-coding outputs generated by 07_additional_coding_promts.R",
  path_col = "path",
  label_col = "label",
  notes_col = "notes"
)

# =========================================================
# 12) Schnellcheck in der Konsole
# =========================================================

cat("\n==================== OBSERVED PROMPT SEQUENCES BY CATEGORY ====================\n")
print(observed_prompt_sequences_by_category)

cat("\n==================== LEVEL SEQUENCES BY CATEGORY ====================\n")
print(level_sequences_by_category)

cat("\n==================== VIBE SEQUENCES BY CATEGORY ====================\n")
print(vibe_sequences_by_category)

cat("\n==================== LEVEL OVERVIEW ALL ROUNDS BY CATEGORY ====================\n")
print(prompt_level_overview_all_rounds_by_category)

cat("\n==================== LEVEL OVERVIEW BY ROUND AND CATEGORY ====================\n")
print(prompt_level_overview_by_round_and_category)

cat("\n==================== VIBE OVERVIEW ALL ROUNDS BY CATEGORY ====================\n")
print(prompt_vibe_overview_all_rounds_by_category)

cat("\n==================== VIBE OVERVIEW BY ROUND AND CATEGORY ====================\n")
print(prompt_vibe_overview_by_round_and_category)

cat("\n==================== LEVEL/VIBE NOT FULLY ELIGIBLE CASES ====================\n")
print(prompt_level_vibe_not_eligible_cases)

cat("\n==================== NO VIBE CASE SUMMARY BY CATEGORY ====================\n")
print(prompt_vibe_case_no_vibe_summary_by_category)

message("Prompt coding extension completed successfully.")
message("Selected prompt-coding tables only were exported.")
message("New object created: final_analysis_dataset_full_viviq_prompt_coding")
message("GT index: ", file.path(out_gt_dir, "00_gt_index.html"))
message("General export index: ", file.path(out_document_dir, "00_prompt_coding_index.html"))
message(
  paste0(
    "Unclassified sequence types requiring manual review: ",
    nrow(unclassified_sequences)
  )
)

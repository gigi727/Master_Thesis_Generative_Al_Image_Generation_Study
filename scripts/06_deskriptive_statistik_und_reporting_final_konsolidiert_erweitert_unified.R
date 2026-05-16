#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.

#####################################################################
###        Deskriptive Statistik, Tabellen und Ergebnisgrafiken   ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript erstellt auf Basis der bereits bereinigten und gematchten
# Survey-Datensätze eine wissenschaftlich nutzbare Ergebnisdokumentation.
# Es verwendet die neuen einheitlich präfixierten Variablennamen
# (Pre_Survey_ / Main_Survey_) und greift auf die bereits im Cleaning-Skript
# erzeugten Score-Variablen zurück.
#
# Wichtig:
# Die Rohdaten- und Cleaning-Übersichten werden weiterhin separat berichtet,
# weil sie die methodische Herleitung des finalen Samples dokumentieren.
# Ab der inhaltlichen Ergebnisdarstellung wird jedoch ausschließlich der
# konsolidierte finale Analyse-Datensatz verwendet.
#
# ERWEITERUNG:
# Zusätzlich werden erweiterte deskriptive Statistiken für ausgewählte
# Variablen des gematchten Pre- und Main-Survey-Samples erstellt.

# =========================================================
# 0) Pakete                                              ===
# =========================================================

#install.packages(c("tidyverse", "writexl", "here", "scales", "readr"), dependencies = TRUE)

library(tidyverse)
library(writexl)
library(here)
library(scales)
library(readr)
library(gt)
library(grid)
library(gridExtra)

# =========================================================
# 1) Pfade und Abhängigkeiten                            ===
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

output_dir <- get_output_dir("05")

out_desc_dir     <- file.path(project_root, "data_output", "descriptives")
out_tables_dir   <- file.path(out_desc_dir, "tables")
out_figures_dir  <- file.path(out_desc_dir, "figures")
out_captions_dir <- file.path(out_desc_dir, "captions")

# NEU: Erweiterte Outputs
out_extended_dir      <- file.path(out_desc_dir, "extended")
out_extended_tables   <- file.path(out_extended_dir, "tables")
out_extended_figures  <- file.path(out_extended_dir, "figures")
out_extended_captions <- file.path(out_extended_dir, "captions")

# NEU: Gezielte Zusatzanalysen für das Reporting
out_requested_dir      <- file.path(out_desc_dir, "requested_analysis")
out_requested_tables   <- file.path(out_requested_dir, "tables")
out_requested_figures  <- file.path(out_requested_dir, "figures")
out_requested_captions <- file.path(out_requested_dir, "captions")
out_gt_dir             <- file.path(out_desc_dir, "gt_tables")
out_gt_html_dir        <- file.path(out_gt_dir, "html")
out_gt_rtf_dir         <- file.path(out_gt_dir, "rtf")
out_gt_doc_dir         <- file.path(out_gt_dir, "documentation")

purrr::walk(
  c(
    out_desc_dir, out_tables_dir, out_figures_dir, out_captions_dir,
    out_extended_dir, out_extended_tables, out_extended_figures, out_extended_captions,
    out_requested_dir, out_requested_tables, out_requested_figures, out_requested_captions,
    out_gt_dir, out_gt_html_dir, out_gt_rtf_dir, out_gt_doc_dir
  ),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)


#####################################################################
###      Prüfung und Laden der benötigten Vorgänger-Skripte       ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt wird geprüft, ob die beiden vorgelagerten Skripte
# vorhanden sind. Falls ja, werden sie explizit in die Global Environment
# geladen, damit die dort erzeugten Objekte für das Reporting-Skript
# sicher verfügbar sind. Anschließend wird geprüft, ob alle benötigten
# Objekte tatsächlich vorhanden sind.

# =========================================================
# 2) Benötigte Objekte definieren                        ===
# =========================================================

# =========================================================
# 2) Anonymisierte Analyse-Datensätze laden              ====
# =========================================================

# Dieses Reporting-Skript verwendet keine Rohdaten und keine Outputs aus 01-03.
# Die Datensatzbasis ist explizit getrennt:
# - Pre-Survey-Auswertungen: data_final/pre_survey_anonymized.rds
# - Main-Survey-Auswertungen: data_final/main_survey_anonymized.rds
# - gematchte / längsschnittliche Analysen: data_final/final_analysis_dataset_anonymized.rds

loaded_datasets <- load_anonymized_analysis_datasets(
  project_root = project_root,
  require_pre = TRUE,
  require_main = TRUE,
  require_final = TRUE
)

pre_survey_dataset <- loaded_datasets$pre_survey_dataset
main_survey_dataset <- loaded_datasets$main_survey_dataset
final_analysis_dataset <- loaded_datasets$final_analysis_dataset

pre_feature_lookup <- loaded_datasets$pre_feature_lookup
main_feature_lookup <- loaded_datasets$main_feature_lookup

# Namen für Kompatibilität mit der bestehenden Tabellenlogik.
pre_clean_full <- pre_survey_dataset
main_clean_full <- main_survey_dataset
pre_raw <- pre_survey_dataset
main_raw <- main_survey_dataset
final_analysis_dataset_full <- final_analysis_dataset

# Dokumentationsobjekte ohne Zugriff auf private Cleaning-Logs.
preview_removal_summary <- tibble(
  dataset = c("Pre_Survey", "Main_Survey"),
  n_kept = c(nrow(pre_survey_dataset), nrow(main_survey_dataset)),
  note = "Anonymized cleaned dataset loaded from data_final/"
)

pre_n_overview <- tibble(
  data_cleaning_step = c("Loaded anonymized cleaned Pre-Survey dataset"),
  n_kept = nrow(pre_survey_dataset)
)

main_n_overview <- tibble(
  data_cleaning_step = c("Loaded anonymized cleaned Main-Survey dataset"),
  n_kept = nrow(main_survey_dataset)
)

pre_followup_summary <- tibble(
  category = c("Pre-Survey anonymized cleaned cases"),
  n = nrow(pre_survey_dataset),
  note = "Full cleaned Pre-Survey dataset before matching"
)

matched_pre_main <- final_analysis_dataset %>%
  distinct(participant_id)

message("Confirmation: Script 05 uses only anonymized datasets from data_final/.")

#####################################################################
###                    Hilfsfunktionen                            ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden Hilfsfunktionen definiert, die für die
# tabellarische Ergebnisdarstellung, Prozentberechnung, robuste Kennwerte
# und die Speicherung von Tabellen verwendet werden.

# =========================================================
# 6) Hilfsfunktionen                                     ===
# =========================================================

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (sum(!is.na(x)) <= 1) NA_real_ else sd(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

safe_min <- function(x) {
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

pct <- function(x, base) {
  ifelse(is.na(base) | base == 0, NA_real_, 100 * x / base)
}

summarise_raw_dataset <- function(df, cfg, dataset_label) {
  finished_clean <- parse_finished_to_logical(df[[cfg$finished_var]])
  consent_clean  <- as.character(df[[cfg$consent_var]])
  email_clean    <- clean_email(df[[cfg$email_var]])

  tibble(
    dataset = dataset_label,
    n_cases = nrow(df),
    n_variables = ncol(df),
    n_finished_true = sum(finished_clean == TRUE, na.rm = TRUE),
    n_finished_false_or_na = sum(is.na(finished_clean) | finished_clean != TRUE, na.rm = TRUE),
    n_consent_yes = sum(consent_clean == cfg$consent_yes, na.rm = TRUE),
    n_consent_no = sum(consent_clean == cfg$consent_no, na.rm = TRUE),
    n_email_available = sum(!is.na(email_clean)),
    n_valid_email = sum(is_valid_email(email_clean), na.rm = TRUE)
  )
}

save_table_outputs <- function(df, base_filename, out_dir = out_tables_dir) {
  readr::write_csv(df, file.path(out_dir, paste0(base_filename, ".csv")))
  writexl::write_xlsx(df, path = file.path(out_dir, paste0(base_filename, ".xlsx")))
}

save_extended_table_outputs <- function(df, base_filename, out_dir = out_extended_tables) {
  readr::write_csv(df, file.path(out_dir, paste0(base_filename, ".csv")))
  writexl::write_xlsx(df, path = file.path(out_dir, paste0(base_filename, ".xlsx")))
}

make_numeric_desc <- function(df, var, var_label, dataset_label) {
  x <- readr::parse_number(as.character(df[[var]]), na = c("", "NA"))
  tibble(
    dataset = dataset_label,
    variable_name = var,
    variable_label = var_label,
    n_valid = sum(!is.na(x)),
    n_missing = sum(is.na(x)),
    mean = safe_mean(x),
    sd = safe_sd(x),
    median = safe_median(x),
    min = safe_min(x),
    max = safe_max(x)
  )
}

make_categorical_desc <- function(df, var, var_label, dataset_label) {
  x <- as.character(df[[var]])
  x[is.na(x) | stringr::str_trim(x) == ""] <- "Missing"

  tibble(response = x) %>%
    count(response, name = "n") %>%
    mutate(
      percent = pct(n, sum(n)),
      dataset = dataset_label,
      variable_name = var,
      variable_label = var_label,
      .before = 1
    )
}

make_multiselect_desc <- function(df, var, var_label, dataset_label, sep_pattern = ";") {
  x <- as.character(df[[var]])
  x <- x[!is.na(x) & stringr::str_trim(x) != ""]

  if (length(x) == 0) {
    return(
      tibble(
        dataset = dataset_label,
        variable_name = var,
        variable_label = var_label,
        option = character(),
        n = integer(),
        percent_of_cases = numeric()
      )
    )
  }

  tibble(raw = x) %>%
    separate_rows(raw, sep = sep_pattern) %>%
    mutate(raw = stringr::str_squish(raw)) %>%
    filter(raw != "") %>%
    count(raw, name = "n") %>%
    mutate(
      percent_of_cases = pct(n, length(x)),
      dataset = dataset_label,
      variable_name = var,
      variable_label = var_label,
      .before = 1
    ) %>%
    rename(option = raw)
}

make_age_group <- function(x) {
  age_num <- readr::parse_number(as.character(x))
  cut(
    age_num,
    breaks = c(17, 24, 34, 44, 54, Inf),
    labels = c("18-24", "25-34", "35-44", "45-54", "55+"),
    right = TRUE
  )
}

theme_result <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
}

agreement_levels <- c(
  "Not at all",
  "Very weakly",
  "Weakly",
  "Moderately",
  "Strongly",
  "Very strongly",
  "Almost exactly"
)

change_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Agree",
  "Strongly agree"
)

process_eval_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Agree",
  "Strongly agree"
)

three_ita_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Slightly disagree",
  "Neither agree nor disagree",
  "Slightly agree",
  "Agree",
  "Strongly agree"
)

apply_longitudinal_response_order <- function(distribution_table) {
  distribution_table %>%
    mutate(
      response = case_when(
        variable_name %in% c("Main_Survey_Q26", "Main_Survey_Q34", "Main_Survey_Q42") ~
          factor(response, levels = agreement_levels, ordered = TRUE),

        variable_name %in% c("Main_Survey_Q28", "Main_Survey_Q36", "Main_Survey_Q44") ~
          factor(response, levels = change_levels, ordered = TRUE),

        variable_name %in% c("Main_Survey_Q29", "Main_Survey_Q37", "Main_Survey_Q45") ~
          factor(response, levels = process_eval_levels, ordered = TRUE),

        TRUE ~ factor(response)
      )
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

save_requested_table_outputs <- function(df, base_filename, out_dir = out_requested_tables) {
  readr::write_csv(df, file.path(out_dir, paste0(base_filename, ".csv")))
  writexl::write_xlsx(df, path = file.path(out_dir, paste0(base_filename, ".xlsx")))
}

get_variable_label <- function(var_name) {
  lookup_tables <- list()

  if (exists("pre_feature_lookup", inherits = FALSE)) {
    lookup_tables <- append(lookup_tables, list(pre_feature_lookup))
  }

  if (exists("main_feature_lookup", inherits = FALSE)) {
    lookup_tables <- append(lookup_tables, list(main_feature_lookup))
  }

  if (length(lookup_tables) == 0) {
    return(var_name)
  }

  lookup_all <- bind_rows(lookup_tables)

  label_match <- lookup_all %>%
    filter(variable_name == var_name) %>%
    pull(question_text)

  label_match <- label_match[!is.na(label_match)]

  if (length(label_match) == 0) {
    return(var_name)
  }

  label_match[[1]]
}

resolve_analysis_column <- function(df, var_name) {
  if (var_name %in% names(df)) {
    return(var_name)
  }

  candidate_names <- c(paste0(var_name, ".x"), paste0(var_name, ".y"))
  candidate_names <- candidate_names[candidate_names %in% names(df)]

  if (length(candidate_names) == 0) {
    return(NA_character_)
  }

  candidate_names[[1]]
}

resolve_analysis_vector <- function(df, var_name) {
  if (var_name %in% names(df)) {
    return(df[[var_name]])
  }

  candidate_names <- c(paste0(var_name, ".x"), paste0(var_name, ".y"))
  candidate_names <- candidate_names[candidate_names %in% names(df)]

  if (length(candidate_names) == 0) {
    return(rep(NA, nrow(df)))
  }

  resolved <- df[[candidate_names[[1]]]]

  if (length(candidate_names) > 1) {
    for (candidate_name in candidate_names[-1]) {
      resolved <- dplyr::coalesce(resolved, df[[candidate_name]])
    }
  }

  resolved
}

make_frequency_table_requested <- function(df, var_name, dataset_label = "Final consolidated sample") {
  var_label <- get_variable_label(var_name)

  x <- resolve_analysis_vector(df, var_name) %>%
    as.character()

  x[is.na(x) | stringr::str_trim(x) == ""] <- "Missing"

  freq_table <- tibble(response = x) %>%
    count(response, name = "n") %>%
    arrange(desc(n), response)

  n_total <- sum(freq_table$n)

  freq_table %>%
    mutate(
      dataset = dataset_label,
      variable_name = var_name,
      variable_label = var_label,
      n_total = n_total,
      percent = round(100 * n / n_total, 1),
      .before = 1
    ) %>%
    select(dataset, variable_name, variable_label, n_total, response, n, percent)
}

make_multiselect_frequency_table_requested <- function(
    df,
    var_name,
    dataset_label = "Final consolidated sample",
    sep_pattern = ";"
) {
  var_label <- get_variable_label(var_name)

  x <- resolve_analysis_vector(df, var_name) %>%
    as.character()

  n_total_cases <- length(x)
  n_valid_cases <- sum(!is.na(x) & stringr::str_trim(x) != "")

  option_table <- tibble(raw = x) %>%
    filter(!is.na(raw), stringr::str_trim(raw) != "") %>%
    tidyr::separate_rows(raw, sep = sep_pattern) %>%
    mutate(response = stringr::str_squish(raw)) %>%
    filter(response != "") %>%
    count(response, name = "n") %>%
    arrange(desc(n), response) %>%
    mutate(
      dataset = dataset_label,
      variable_name = var_name,
      variable_label = var_label,
      n_total_cases = n_total_cases,
      n_valid_cases = n_valid_cases,
      percent_of_valid_cases = round(100 * n / n_valid_cases, 1),
      .before = 1
    ) %>%
    select(
      dataset,
      variable_name,
      variable_label,
      n_total_cases,
      n_valid_cases,
      response,
      n,
      percent_of_valid_cases
    )

  option_table
}
make_top_n_requested <- function(freq_table, top_n = 3) {
  freq_table %>%
    filter(response != "Missing") %>%
    arrange(desc(n), response) %>%
    slice_head(n = top_n) %>%
    mutate(rank = row_number(), .before = response)
}

resolve_analysis_numeric_vector <- function(df, var_name) {
  score_candidate <- paste0(var_name, "_score")
  score_vector <- resolve_analysis_vector(df, score_candidate)

  score_vector_num <- suppressWarnings(as.numeric(score_vector))

  if (sum(!is.na(score_vector_num)) > 0) {
    return(score_vector_num)
  }

  raw_vector <- resolve_analysis_vector(df, var_name)

  if (is.numeric(raw_vector)) {
    return(as.numeric(raw_vector))
  }

  suppressWarnings(readr::parse_number(as.character(raw_vector), na = c("", "NA")))
}

make_variable_summary_requested <- function(df, var_name, dataset_label = "Final consolidated sample") {
  var_label <- get_variable_label(var_name)

  x_raw <- resolve_analysis_vector(df, var_name) %>%
    as.character()

  x_raw[stringr::str_trim(replace_na(x_raw, "")) == ""] <- NA_character_

  x_numeric <- resolve_analysis_numeric_vector(df, var_name)

  valid_values <- x_raw[!is.na(x_raw)]
  n_possible_answers <- dplyr::n_distinct(valid_values)
  n_valid <- sum(!is.na(x_raw))

  top_values <- tibble(response = valid_values) %>%
    count(response, name = "n") %>%
    arrange(desc(n), response) %>%
    mutate(percent = round(pct(n, sum(n)), 1))

  tibble(
    dataset = dataset_label,
    variable_name = var_name,
    variable_label = var_label,
    n_possible_answers = n_possible_answers,
    n_valid = n_valid,
    mean = safe_mean(x_numeric),
    median = safe_median(x_numeric),

    most_frequent_value = if (nrow(top_values) >= 1) top_values$response[[1]] else NA_character_,
    n_most_frequent_value = if (nrow(top_values) >= 1) top_values$n[[1]] else NA_integer_,
    percent_most_frequent_value = if (nrow(top_values) >= 1 && n_valid > 0) {
      round(100 * top_values$n[[1]] / n_valid, 1)
    } else {
      NA_real_
    },

    second_most_frequent_value = if (nrow(top_values) >= 2) top_values$response[[2]] else NA_character_,
    n_second_most_frequent_value = if (nrow(top_values) >= 2) top_values$n[[2]] else NA_integer_,
    percent_second_most_frequent_value = if (nrow(top_values) >= 2 && n_valid > 0) {
      round(100 * top_values$n[[2]] / n_valid, 1)
    } else {
      NA_real_
    }
  )
}

make_variable_summary_table_requested <- function(df, vars, dataset_label = "Final consolidated sample") {
  purrr::map_dfr(vars, ~ make_variable_summary_requested(df, .x, dataset_label = dataset_label))
}

make_pre_post_requested_summary <- function(df, vars, top_n_vars = character()) {
  freq_tables <- purrr::map(vars, ~ make_frequency_table_requested(df, .x))
  names(freq_tables) <- vars
  freq_table_all <- bind_rows(freq_tables)

  top_n_table_all <- purrr::map(
    vars,
    ~ if (.x %in% top_n_vars) make_top_n_requested(freq_tables[[.x]], top_n = 3) else NULL
  ) %>%
    bind_rows()

  variable_summary_table <- make_variable_summary_table_requested(df, vars)

  list(
    variable_summary_table = variable_summary_table,
    frequency_table = freq_table_all,
    top_n_table = top_n_table_all
  )
}

make_iteration_distribution <- function(df, var_names, group_label) {
  purrr::imap_dfr(
    var_names,
    function(var_name, iteration_label) {
      make_frequency_table_requested(df, var_name) %>%
        mutate(
          group_label = group_label,
          iteration = iteration_label,
          .before = 1
        )
    }
  )
}

make_iteration_change_table <- function(distribution_table) {
  distribution_table %>%
    select(group_label, iteration, response, n, percent) %>%
    pivot_wider(
      names_from = iteration,
      values_from = c(n, percent),
      values_fill = 0
    ) %>%
    mutate(
      diff_n_2_vs_1 = n_Iteration_2 - n_Iteration_1,
      diff_n_3_vs_2 = n_Iteration_3 - n_Iteration_2,
      diff_n_3_vs_1 = n_Iteration_3 - n_Iteration_1,
      diff_pct_2_vs_1 = round(percent_Iteration_2 - percent_Iteration_1, 1),
      diff_pct_3_vs_2 = round(percent_Iteration_3 - percent_Iteration_2, 1),
      diff_pct_3_vs_1 = round(percent_Iteration_3 - percent_Iteration_1, 1)
    ) %>%
    arrange(group_label, desc(n_Iteration_1 + n_Iteration_2 + n_Iteration_3), response)
}

make_iteration_block_distribution <- function(df, block_map, group_label) {
  purrr::imap_dfr(
    block_map,
    function(var_names, iteration_label) {
      purrr::map_dfr(
        var_names,
        function(var_name) {
          item_label <- stringr::str_extract(var_name, "[0-9]+$")

          make_frequency_table_requested(df, var_name) %>%
            mutate(
              group_label = group_label,
              iteration = iteration_label,
              item = paste0("Item_", item_label),
              .before = 1
            )
        }
      )
    }
  )
}

make_iteration_block_change_table <- function(distribution_table) {
  distribution_table %>%
    select(group_label, item, iteration, response, n, percent) %>%
    pivot_wider(
      names_from = iteration,
      values_from = c(n, percent),
      values_fill = 0
    ) %>%
    mutate(
      diff_n_2_vs_1 = n_Iteration_2 - n_Iteration_1,
      diff_n_3_vs_2 = n_Iteration_3 - n_Iteration_2,
      diff_n_3_vs_1 = n_Iteration_3 - n_Iteration_1,
      diff_pct_2_vs_1 = round(percent_Iteration_2 - percent_Iteration_1, 1),
      diff_pct_3_vs_2 = round(percent_Iteration_3 - percent_Iteration_2, 1),
      diff_pct_3_vs_1 = round(percent_Iteration_3 - percent_Iteration_1, 1)
    ) %>%
    arrange(group_label, item, desc(n_Iteration_1 + n_Iteration_2 + n_Iteration_3), response)
}

make_longitudinal_plot <- function(distribution_table, title_text, response_levels = NULL) {
  plot_data <- distribution_table %>%
    filter(response != "Missing") %>%
    mutate(response = as.character(response))

  if (!is.null(response_levels)) {
    plot_data <- plot_data %>%
      mutate(response = factor(response, levels = response_levels, ordered = TRUE))
  }

  ggplot(plot_data, aes(x = iteration, y = percent, fill = response)) +
    geom_col(position = "stack") +
    labs(
      title = title_text,
      x = NULL,
      y = "Percent",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}

make_longitudinal_block_plot <- function(distribution_table, title_text, response_levels = NULL) {
  item_labels <- c(
    "Item_1" = "The scene or composition\n(arrangement of elements)\nmatches my mental image.",
    "Item_2" = "The overall atmosphere or style\nmatches my mental image.",
    "Item_3" = "Important details and components\nfrom my mental image are present\nin this image."
  )

  plot_data <- distribution_table %>%
    filter(response != "Missing") %>%
    mutate(response = as.character(response))

  if (!is.null(response_levels)) {
    plot_data <- plot_data %>%
      mutate(response = factor(response, levels = response_levels, ordered = TRUE))
  }

  ggplot(plot_data, aes(x = iteration, y = percent, fill = response)) +
    geom_col(position = "stack") +
    facet_wrap(
      ~ item,
      labeller = as_labeller(item_labels)
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = "Percent",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}


#####################################################################
###      Konsolidierten Analyse-Datensatz als Hauptbasis setzen   ###
#####################################################################

### BESCHREIBUNG ###

# Ab diesem Punkt wird für die inhaltliche Ergebnisdarstellung ausschließlich
# der konsolidierte finale Analyse-Datensatz verwendet. Dieser enthält bereits
# die zusammengeführten Variablen aus Pre-Survey und Main-Survey sowie – falls
# vorhanden – die ergänzten VIVIQ-Kennwerte.

# =========================================================
# 7) Konsolidierten Analyse-Datensatz setzen             ===
# =========================================================

# The final matched dataset was loaded above from data_final/final_analysis_dataset_anonymized.rds.
# It remains the basis for all matched, longitudinal, VIVIQ, prompt-coding and main-study analyses.

id_col <- "participant_id"
pre_id_col <- "pre_participant_id"
main_id_col <- "main_participant_id"

viviq_score_vars <- names(final_analysis_dataset)[
  stringr::str_detect(
    names(final_analysis_dataset),
    "^Main_Survey_Q([4-9]|1[0-9])_score$"
  )
]


make_main_single_summary_overall_category <- function(df, var_name) {
  bind_rows(
    make_variable_summary_requested(
      df,
      var_name = var_name,
      dataset_label = "Overall"
    ),
    df %>%
      filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
      group_by(Main_Survey_target_word_category) %>%
      group_modify(
        ~ make_variable_summary_requested(
          .x,
          var_name = var_name,
          dataset_label = unique(.y$Main_Survey_target_word_category)
        )
      ) %>%
      ungroup() %>%
      select(-Main_Survey_target_word_category)
  ) %>%
    mutate(
      dataset = factor(
        dataset,
        levels = c("Overall", "abstract", "concrete"),
        ordered = TRUE
      )
    ) %>%
    arrange(dataset) %>%
    mutate(dataset = as.character(dataset))
}



main_q50_boxplot_data <- bind_rows(
  final_analysis_dataset %>%
    transmute(
      category_group = "Overall",
      q50_value = suppressWarnings(as.numeric(Main_Survey_Q50))
    ),
  final_analysis_dataset %>%
    filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
    transmute(
      category_group = Main_Survey_target_word_category,
      q50_value = suppressWarnings(as.numeric(Main_Survey_Q50))
    )
) %>%
  filter(!is.na(q50_value)) %>%
  mutate(
    category_group = factor(
      category_group,
      levels = c("Overall", "abstract", "concrete"),
      ordered = TRUE
    )
  )

#####################################################################
###      Finalen Analyse-Datensatz für das Reporting vorbereiten  ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden die für das Reporting relevanten Übersichts-
# und Deskriptionstabellen erstellt. Die Rohdaten- und Cleaning-Übersichten
# dokumentieren weiterhin die methodische Herleitung des finalen Samples.
# Alle inhaltlichen Ergebnisübersichten ab dem finalen Matching basieren
# dagegen ausschließlich auf dem konsolidierten Analyse-Datensatz.

# =========================================================
# 8) Übersichts- und Reportingtabellen erstellen         ===
# =========================================================

raw_dataset_overview <- tibble(
  dataset = c("Pre_Survey", "Main_Survey", "Final matched dataset"),
  n_cases = c(nrow(pre_survey_dataset), nrow(main_survey_dataset), nrow(final_analysis_dataset)),
  n_variables = c(ncol(pre_survey_dataset), ncol(main_survey_dataset), ncol(final_analysis_dataset)),
  data_source = c(
    "data_final/pre_survey_anonymized.rds",
    "data_final/main_survey_anonymized.rds",
    "data_final/final_analysis_dataset_anonymized.rds"
  )
)

cleaning_flow_table <- tibble(
  dataset = c("Pre_Survey", "Main_Survey", "Final matched dataset"),
  phase = c(
    "Anonymized cleaned Pre-Survey dataset loaded",
    "Anonymized cleaned Main-Survey dataset loaded",
    "Anonymized matched final dataset loaded"
  ),
  n_cases = c(nrow(pre_survey_dataset), nrow(main_survey_dataset), nrow(final_analysis_dataset)),
  percent_of_analysis_start = 100
)

key_dataset_overview <- tibble(
  dataset = c(
    "Pre-Survey cleaned anonymized dataset before matching",
    "Main-Survey cleaned anonymized dataset before matching",
    "Final matched anonymized dataset with VIVIQ and prompt coding",
    "Fully completed VIVIQ in the final matched dataset"
  ),
  n_cases = c(
    nrow(pre_survey_dataset),
    nrow(main_survey_dataset),
    nrow(final_analysis_dataset),
    sum(!is.na(final_analysis_dataset$viviq_total_score))
  )
)

email_matching_overview <- tibble(
  group = c(
    "Pre-Survey anonymized cleaned cases",
    "Main-Survey anonymized cleaned cases",
    "Final matched anonymized cases",
    "Final matched dataset: VIVIQ available",
    "Final matched dataset: VIVIQ complete"
  ),
  n = c(
    nrow(pre_survey_dataset),
    nrow(main_survey_dataset),
    dplyr::n_distinct(final_analysis_dataset$participant_id, na.rm = TRUE),
    sum(!is.na(final_analysis_dataset$viviq_mean_score)),
    sum(!is.na(final_analysis_dataset$viviq_total_score))
  )
)

viviq_total_summary_table <- tibble(
  n_cases = nrow(final_analysis_dataset),
  n_complete = sum(!is.na(final_analysis_dataset$viviq_total_score)),
  mean_viviq_total_score = safe_mean(final_analysis_dataset$viviq_total_score),
  sd_viviq_total_score = safe_sd(final_analysis_dataset$viviq_total_score),
  mean_viviq_mean_score = safe_mean(final_analysis_dataset$viviq_mean_score),
  sd_viviq_mean_score = safe_sd(final_analysis_dataset$viviq_mean_score)
)

viviq_item_summary_table <- tibble(variable = viviq_score_vars) %>%
  mutate(
    n_valid = purrr::map_int(variable, ~ sum(!is.na(final_analysis_dataset[[.x]]))),
    mean = purrr::map_dbl(variable, ~ safe_mean(final_analysis_dataset[[.x]])),
    sd = purrr::map_dbl(variable, ~ safe_sd(final_analysis_dataset[[.x]])),
    median = purrr::map_dbl(variable, ~ safe_median(final_analysis_dataset[[.x]]))
  )

final_analysis_overview <- tibble(
  n_cases = nrow(final_analysis_dataset),
  n_variables = ncol(final_analysis_dataset),
  n_participants = dplyr::n_distinct(final_analysis_dataset$participant_id, na.rm = TRUE),
  n_viviq_mean_nonmissing = sum(!is.na(final_analysis_dataset$viviq_mean_score)),
  n_viviq_total_nonmissing = sum(!is.na(final_analysis_dataset$viviq_total_score))
)

target_word_distribution <- final_analysis_dataset %>%
  count(Main_Survey_target_word, sort = TRUE) %>%
  mutate(percent = round(100 * n / sum(n), 1))

target_word_category_distribution <- final_analysis_dataset %>%
  count(Main_Survey_target_word_category, sort = TRUE) %>%
  mutate(percent = round(100 * n / sum(n), 1))

target_word_category_check <- final_analysis_dataset %>%
  count(Main_Survey_target_word, Main_Survey_target_word_category, sort = TRUE)

unmapped_target_words <- final_analysis_dataset %>%
  filter(!is.na(Main_Survey_target_word), is.na(Main_Survey_target_word_category)) %>%
  distinct(Main_Survey_target_word)

#####################################################################
###      Erweiterte Deskriptivstatistik für gematchte Fälle       ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden zusätzliche deskriptive Statistiken für
# ausgewählte Variablen aus Pre- und Main-Survey erstellt.
# Grundlage sind ausschließlich die Fälle, die im finalen E-Mail-Matching
# vorkommen.

# =========================================================
# 8b) Erweiterte Deskriptivtabellen erstellen            ===
# =========================================================

# Pre-Survey-only descriptives are based on the full cleaned anonymized Pre-Survey dataset,
# not on the matched final sample.
pre_desc_base <- pre_survey_dataset

pre_duration_base <- if ("Pre_Survey_Q3" %in% names(pre_survey_dataset)) {
  pre_survey_dataset %>% filter(Pre_Survey_Q3 == "Yes")
} else {
  pre_survey_dataset
}

# Main-Survey-only descriptives are based on the full cleaned anonymized Main-Survey dataset.
main_desc_base <- main_survey_dataset

if ("Pre_Survey_Q28" %in% names(pre_desc_base)) {
  pre_desc_base <- pre_desc_base %>%
    mutate(age_group_pre = make_age_group(.data[["Pre_Survey_Q28"]]))
}

if ("Main_Survey_Q55" %in% names(main_desc_base)) {
  main_desc_base <- main_desc_base %>%
    mutate(age_group_main = make_age_group(.data[["Main_Survey_Q55"]]))
}

survey_duration_summary_table <- bind_rows(
  make_numeric_desc(
    pre_duration_base,                                          # CHANGED
    "Pre_Survey_Duration",
    "Pre Survey duration",
    'Pre-Eligible (Pre_Survey_Q3 == "Yes")'                     # CHANGED
  ),
  make_numeric_desc(
    main_desc_base,
    "Main_Survey_Duration",
    "Main Survey duration",
    "Main-Survey cleaned"
  )
)
selected_variables <- tribble(
  ~survey, ~variable_name, ~short_label, ~table_type, ~priority, ~reporting_note,
  "Pre",  "Pre_Survey_Q28", "Age", "numeric", "core", "Central sociodemographic variable.",
  "Pre",  "Pre_Survey_Q29", "Gender", "categorical", "core", "Central sociodemographic variable.",
  "Pre",  "Pre_Survey_Q3",  "Ever used GenAI image tools", "categorical", "core", "GenAI experience.",
  "Pre",  "Pre_Survey_Q6",  "Duration of use", "categorical", "core", "Usage experience.",
  "Pre",  "Pre_Survey_Q7",  "Usage frequency", "categorical", "core", "Usage intensity.",
  "Pre",  "Pre_Survey_Q10", "Paid use", "categorical", "core", "Tool investment.",
  "Pre",  "Pre_Survey_Q13_6", "Alignment with mental image", "categorical", "core", "Theoretically especially relevant.",
  "Pre",  "Pre_Survey_Q16", "Prompt adjustment", "categorical", "core", "Prompting behavior.",
  "Pre",  "Pre_Survey_Q18", "Difficulty of prompts", "categorical", "core", "Prompting barrier.",
  "Pre",  "Pre_Survey_Q19", "Iterations", "categorical", "core", "Prompting effort.",
  "Pre",  "Pre_Survey_Q20", "Perception of GenAI", "categorical", "core", "General attitude.",
  "Pre",  "Pre_Survey_Q21_4", "Autonomy attitude", "categorical", "core", "Theoretically relevant attitude.",
  "Pre",  "Pre_Survey_Q21_5", "AI understands intention", "categorical", "core", "Theoretically relevant attitude.",
  "Main", "Main_Survey_Q55", "Age", "numeric", "core", "Central sociodemographic variable.",
  "Main", "Main_Survey_Q56", "Gender", "categorical", "core", "Central sociodemographic variable.",
  "Main", "Main_Survey_target_word", "Processed target word", "categorical", "appendix", "Stimulus distribution.",
  "Main", "Main_Survey_target_word_category", "Target word category", "categorical", "appendix", "Abstract vs. concrete.",
  "Main", "Main_Survey_Q21", "Vividness of initial image", "categorical", "core", "Content-relevant starting variable.",
  "Main", "Main_Survey_Q26", "Image alignment round 1", "categorical", "core", "Central outcome variable.",
  "Main", "Main_Survey_Q28", "Change in mental image round 1", "categorical", "core", "Central outcome variable.",
  "Main", "Main_Survey_Q34", "Image alignment round 2", "categorical", "core", "Central outcome variable.",
  "Main", "Main_Survey_Q36", "Change in mental image round 2", "categorical", "core", "Central outcome variable.",
  "Main", "Main_Survey_Q42", "Image alignment round 3", "categorical", "core", "Central outcome variable.",
  "Main", "Main_Survey_Q44", "Change in mental image round 3", "categorical", "core", "Central outcome variable.",
  "Main", "Main_Survey_Q48", "Similarity to the original image", "categorical", "core", "Important end state.",
  "Main", "Main_Survey_Q49", "Source of the current mental image", "categorical", "core", "Imagination vs. AI influence.",
  "Main", "Main_Survey_Q50", "Contribution of own imagination vs. AI", "numeric", "core", "Slider variable.",
  "Main", "Main_Survey_Q51", "Authorship", "numeric", "core", "Slider variable.",
  "Main", "Main_Survey_Q52", "Strength of change", "categorical", "core", "Global outcome variable.",
  "Main", "Main_Survey_Q53", "Preference for own vs. AI images", "categorical", "core", "Final preference variable.",
  "Main", "viviq_mean_score", "VIVIQ mean", "numeric", "core", "Derived metric.",
  "Main", "viviq_total_score", "VIVIQ total score", "numeric", "appendix", "Derived metric."

)

selected_variables_checked <- bind_rows(
  selected_variables %>%
    filter(survey == "Pre") %>%
    mutate(variable_exists = variable_name %in% names(pre_desc_base)),
  selected_variables %>%
    filter(survey == "Main") %>%
    mutate(variable_exists = variable_name %in% names(main_desc_base))
) %>%
  filter(variable_exists)

pre_numeric_desc <- selected_variables_checked %>%
  filter(survey == "Pre", table_type == "numeric") %>%
  mutate(result = purrr::map2(variable_name, short_label, ~ make_numeric_desc(pre_desc_base, .x, .y, "Pre-Survey cleaned"))) %>%
  pull(result) %>%
  bind_rows()

pre_categorical_desc <- selected_variables_checked %>%
  filter(survey == "Pre", table_type == "categorical") %>%
  mutate(result = purrr::map2(variable_name, short_label, ~ make_categorical_desc(pre_desc_base, .x, .y, "Pre-Survey cleaned"))) %>%
  pull(result) %>%
  bind_rows()

main_numeric_desc <- selected_variables_checked %>%
  filter(survey == "Main", table_type == "numeric") %>%
  mutate(result = purrr::map2(variable_name, short_label, ~ make_numeric_desc(main_desc_base, .x, .y, "Main-Survey cleaned"))) %>%
  pull(result) %>%
  bind_rows()

main_categorical_desc <- selected_variables_checked %>%
  filter(survey == "Main", table_type == "categorical") %>%
  mutate(result = purrr::map2(variable_name, short_label, ~ make_categorical_desc(main_desc_base, .x, .y, "Main-Survey cleaned"))) %>%
  pull(result) %>%
  bind_rows()

core_variables_overview <- selected_variables_checked %>%
  filter(priority == "core") %>%
  select(survey, variable_name, short_label, table_type, reporting_note)


#####################################################################
###      Zusatzanalysen für Pre-Survey und Längsschnitt         ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden die zusätzlich angeforderten
# Häufigkeitsverteilungen, Top-3-Auswertungen sowie die
# Längsschnittvergleiche über die drei Iterationen erstellt.
# Grundlage ist der konsolidierte finale Analyse-Datensatz mit VIVIQ.

# =========================================================
# 8c) Angeforderte Zusatzanalysen erstellen              ===
# =========================================================

requested_pre_vars <- c(
  "Pre_Survey_Q2",
  "Pre_Survey_Q3",
  "Pre_Survey_Q4_3",
  "Pre_Survey_Q6",
  "Pre_Survey_Q7",
  "Pre_Survey_Q8",
  "Pre_Survey_Q9",
  "Pre_Survey_Q10",
  "Pre_Survey_Q11",
  "Pre_Survey_Q12",
  paste0("Pre_Survey_Q13_", 1:6),
  paste0("Pre_Survey_Q", 14:23)
)

requested_pre_top3_vars <- c(
  "Pre_Survey_Q2",
  "Pre_Survey_Q4_3",
  "Pre_Survey_Q9",
  "Pre_Survey_Q11",
  "Pre_Survey_Q12",
  paste0("Pre_Survey_Q13_", 1:6),
  paste0("Pre_Survey_Q", 14:23)
)

requested_pre_summary <- make_pre_post_requested_summary(
  pre_survey_dataset,
  vars = requested_pre_vars,
  top_n_vars = requested_pre_top3_vars
)

requested_pre_variable_summary_table <- requested_pre_summary$variable_summary_table
requested_pre_frequency_table <- requested_pre_summary$frequency_table
requested_pre_top3_table <- requested_pre_summary$top_n_table

longitudinal_group_1_distribution <- make_iteration_distribution(
  final_analysis_dataset,
  var_names = c(
    Iteration_1 = "Main_Survey_Q26",
    Iteration_2 = "Main_Survey_Q34",
    Iteration_3 = "Main_Survey_Q42"
  ),
  group_label = "Image alignment"
)

longitudinal_group_1_change <- make_iteration_change_table(longitudinal_group_1_distribution)
longitudinal_group_1_variable_summary_table <- make_variable_summary_table_requested(
  final_analysis_dataset,
  vars = c("Main_Survey_Q26", "Main_Survey_Q34", "Main_Survey_Q42")
)


longitudinal_group_1_variable_summary_by_category_table <- final_analysis_dataset %>%
  filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
  group_split(Main_Survey_target_word_category) %>%
  purrr::map_dfr(
    function(df_cat) {
      category_value <- unique(df_cat$Main_Survey_target_word_category)

      make_variable_summary_table_requested(
        df_cat,
        vars = c("Main_Survey_Q26", "Main_Survey_Q34", "Main_Survey_Q42"),
        dataset_label = paste0("Final consolidated sample - ", category_value)
      ) %>%
        mutate(
          Main_Survey_target_word_category = category_value,
          .before = dataset
        )
    }
  )

longitudinal_group_2_distribution <- make_iteration_distribution(
  final_analysis_dataset,
  var_names = c(
    Iteration_1 = "Main_Survey_Q28",
    Iteration_2 = "Main_Survey_Q36",
    Iteration_3 = "Main_Survey_Q44"
  ),
  group_label = "Change in mental image"
)

longitudinal_group_2_change <- make_iteration_change_table(longitudinal_group_2_distribution)
longitudinal_group_2_variable_summary_table <- make_variable_summary_table_requested(
  final_analysis_dataset,
  vars = c("Main_Survey_Q28", "Main_Survey_Q36", "Main_Survey_Q44")
)


longitudinal_group_2_variable_summary_by_category_table <- final_analysis_dataset %>%
  filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
  group_by(Main_Survey_target_word_category) %>%
  group_modify(
    ~ make_variable_summary_table_requested(
      .x,
      vars = c("Main_Survey_Q28", "Main_Survey_Q36", "Main_Survey_Q44"),
      dataset_label = paste0(
        "Final consolidated sample - ",
        unique(.y$Main_Survey_target_word_category)
      )
    )
  ) %>%
  ungroup() %>%
  rename(target_word_category = Main_Survey_target_word_category)


longitudinal_group_3_distribution <- make_iteration_distribution(
  final_analysis_dataset,
  var_names = c(
    Iteration_1 = "Main_Survey_Q29",
    Iteration_2 = "Main_Survey_Q37",
    Iteration_3 = "Main_Survey_Q45"
  ),
  group_label = "Evaluation of the image process"
)

longitudinal_group_3_change <- make_iteration_change_table(longitudinal_group_3_distribution)


longitudinal_group_3_variable_summary_table <- make_variable_summary_table_requested(
  final_analysis_dataset,
  vars = c("Main_Survey_Q29", "Main_Survey_Q37", "Main_Survey_Q45")
)


longitudinal_group_3_variable_summary_by_category_table <- final_analysis_dataset %>%
  filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
  group_by(Main_Survey_target_word_category) %>%
  group_modify(
    ~ make_variable_summary_table_requested(
      .x,
      vars = c("Main_Survey_Q29", "Main_Survey_Q37", "Main_Survey_Q45"),
      dataset_label = paste0(
        "Final consolidated sample - ",
        unique(.y$Main_Survey_target_word_category)
      )
    )
  ) %>%
  ungroup() %>%
  rename(target_word_category = Main_Survey_target_word_category)


longitudinal_block_distribution <- make_iteration_block_distribution(
  final_analysis_dataset,
  block_map = list(
    Iteration_1 = paste0("Main_Survey_Q27_", 1:3),
    Iteration_2 = paste0("Main_Survey_Q35_", 1:3),
    Iteration_3 = paste0("Main_Survey_Q43_", 1:3)
  ),
  group_label = "Multi-part evaluation block"
)

longitudinal_block_change <- make_iteration_block_change_table(longitudinal_block_distribution)

longitudinal_block_variable_summary_table <- make_variable_summary_table_requested(
  final_analysis_dataset,
  vars = c(
    paste0("Main_Survey_Q27_", 1:3),
    paste0("Main_Survey_Q35_", 1:3),
    paste0("Main_Survey_Q43_", 1:3)
  )
)

longitudinal_block_variable_order <- c(
  "Main_Survey_Q27_1",
  "Main_Survey_Q35_1",
  "Main_Survey_Q43_1",
  "Main_Survey_Q27_2",
  "Main_Survey_Q35_2",
  "Main_Survey_Q43_2",
  "Main_Survey_Q27_3",
  "Main_Survey_Q35_3",
  "Main_Survey_Q43_3"
)

longitudinal_block_variable_summary_table <- longitudinal_block_variable_summary_table %>%
  mutate(
    variable_name = factor(
      variable_name,
      levels = longitudinal_block_variable_order,
      ordered = TRUE
    )
  ) %>%
  arrange(variable_name) %>%
  mutate(variable_name = as.character(variable_name))



longitudinal_block_variable_summary_by_category_table <- final_analysis_dataset %>%
  filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
  group_by(Main_Survey_target_word_category) %>%
  group_modify(
    ~ make_variable_summary_table_requested(
      .x,
      vars = c(
        paste0("Main_Survey_Q27_", 1:3),
        paste0("Main_Survey_Q35_", 1:3),
        paste0("Main_Survey_Q43_", 1:3)
      ),
      dataset_label = paste0(
        "Final consolidated sample - ",
        unique(.y$Main_Survey_target_word_category)
      )
    )
  ) %>%
  ungroup() %>%
  rename(target_word_category = Main_Survey_target_word_category)


longitudinal_block_variable_summary_by_category_table <- longitudinal_block_variable_summary_by_category_table %>%
  mutate(
    target_word_category = factor(
      target_word_category,
      levels = c("abstract", "concrete"),
      ordered = TRUE
    ),
    variable_name = factor(
      variable_name,
      levels = longitudinal_block_variable_order,
      ordered = TRUE
    )
  ) %>%
  arrange(target_word_category, variable_name) %>%
  mutate(
    target_word_category = as.character(target_word_category),
    variable_name = as.character(variable_name)
  )

requested_main_single_vars <- c(
  "Main_Survey_Q48",
  "Main_Survey_Q49",
  "Main_Survey_Q50",
  "Main_Survey_Q51",
  "Main_Survey_Q52",
  "Main_Survey_Q53",
  "Main_Survey_Q54"
)

requested_main_single_vars_without_q54 <- setdiff(
  requested_main_single_vars,
  "Main_Survey_Q54"
)

requested_main_single_summary <- make_pre_post_requested_summary(
  final_analysis_dataset,
  vars = requested_main_single_vars_without_q54,
  top_n_vars = character()
)

requested_main_single_variable_summary_table <- requested_main_single_summary$variable_summary_table
requested_main_single_frequency_table <- requested_main_single_summary$frequency_table

requested_main_q54_multiselect_distribution <- make_multiselect_frequency_table_requested(
  final_analysis_dataset,
  var_name = "Main_Survey_Q54",
  dataset_label = "Final consolidated sample",
  sep_pattern = ";"
)

requested_main_q54_top3_table <- requested_main_q54_multiselect_distribution %>%
  slice_head(n = 3) %>%
  mutate(rank = row_number(), .before = response)

main_q48_summary_overall_category_table <- bind_rows(
  make_variable_summary_requested(
    final_analysis_dataset,
    var_name = "Main_Survey_Q48",
    dataset_label = "Overall"
  ),
  final_analysis_dataset %>%
    filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
    group_by(Main_Survey_target_word_category) %>%
    group_modify(
      ~ make_variable_summary_requested(
        .x,
        var_name = "Main_Survey_Q48",
        dataset_label = unique(.y$Main_Survey_target_word_category)
      )
    ) %>%
    ungroup() %>%
    select(-Main_Survey_target_word_category)
) %>%
  mutate(
    dataset = factor(
      dataset,
      levels = c("Overall", "abstract", "concrete"),
      ordered = TRUE
    )
  ) %>%
  arrange(dataset) %>%
  mutate(dataset = as.character(dataset))

main_q48_summary_overall_category_table <- make_main_single_summary_overall_category(
  final_analysis_dataset,
  "Main_Survey_Q48"
)

main_q49_summary_overall_category_table <- make_main_single_summary_overall_category(
  final_analysis_dataset,
  "Main_Survey_Q49"
)

main_q50_summary_overall_category_table <- make_main_single_summary_overall_category(
  final_analysis_dataset,
  "Main_Survey_Q50"
)



main_q51_summary_overall_category_table <- make_main_single_summary_overall_category(
  final_analysis_dataset,
  "Main_Survey_Q51"
)

main_q52_summary_overall_category_table <- make_main_single_summary_overall_category(
  final_analysis_dataset,
  "Main_Survey_Q52"
)

main_q53_summary_overall_category_table <- make_main_single_summary_overall_category(
  final_analysis_dataset,
  "Main_Survey_Q53"
)

main_q49_frequency_overall_category_table <- bind_rows(
  make_frequency_table_requested(
    final_analysis_dataset,
    var_name = "Main_Survey_Q49",
    dataset_label = "Overall"
  ),
  final_analysis_dataset %>%
    filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
    group_by(Main_Survey_target_word_category) %>%
    group_modify(
      ~ make_frequency_table_requested(
        .x,
        var_name = "Main_Survey_Q49",
        dataset_label = unique(.y$Main_Survey_target_word_category)
      )
    ) %>%
    ungroup() %>%
    select(-Main_Survey_target_word_category)
) %>%
  mutate(
    dataset = factor(
      dataset,
      levels = c("Overall", "abstract", "concrete"),
      ordered = TRUE
    )
  ) %>%
  arrange(dataset, desc(n), response) %>%
  mutate(dataset = as.character(dataset))

main_q50_boxplot_data <- bind_rows(
  final_analysis_dataset %>%
    transmute(
      category_group = "Overall",
      q50_value = suppressWarnings(as.numeric(Main_Survey_Q50))
    ),
  final_analysis_dataset %>%
    filter(Main_Survey_target_word_category %in% c("abstract", "concrete")) %>%
    transmute(
      category_group = Main_Survey_target_word_category,
      q50_value = suppressWarnings(as.numeric(Main_Survey_Q50))
    )
) %>%
  filter(!is.na(q50_value)) %>%
  mutate(
    category_group = factor(
      category_group,
      levels = c("Overall", "abstract", "concrete"),
      ordered = TRUE
    )
  )


main_q50_boxplot_stats_table <- main_q50_boxplot_data %>%
  group_by(category_group) %>%
  summarise(
    n_valid = n(),
    mean = round(mean(q50_value, na.rm = TRUE), 2),
    median = round(median(q50_value, na.rm = TRUE), 2),
    q1 = round(quantile(q50_value, 0.25, na.rm = TRUE), 2),
    q3 = round(quantile(q50_value, 0.75, na.rm = TRUE), 2),
    min = round(min(q50_value, na.rm = TRUE), 2),
    max = round(max(q50_value, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(category_group = as.character(category_group))


# ---------------------------------------------------------
# ReqFig5: Main_Survey_Q50 boxplot with table-style remark
# ---------------------------------------------------------

main_q50_question_title <- get_variable_label("Main_Survey_Q50")

if (
  is.na(main_q50_question_title) ||
  !nzchar(main_q50_question_title) ||
  main_q50_question_title == "Main_Survey_Q50"
) {
  main_q50_question_title <- "Contribution of own imagination vs. AI"
}

main_q50_question_title <- stringr::str_wrap(main_q50_question_title, width = 90)
main_q50_question_subtitle <- "Question No. Main_Survey_Q50"

main_q50_boxplot_stats_table_for_plot <- main_q50_boxplot_stats_table %>%
  transmute(
    `Target word category` = category_group,
    `n` = n_valid,
    `Mean` = sprintf("%.2f", mean),
    `Median` = sprintf("%.2f", median),
    `Q1` = sprintf("%.2f", q1),
    `Q3` = sprintf("%.2f", q3),
    `Min` = sprintf("%.2f", min),
    `Max` = sprintf("%.2f", max)
  )

main_q50_boxplot_stats_grob <- gridExtra::tableGrob(
  main_q50_boxplot_stats_table_for_plot,
  rows = NULL,
  theme = gridExtra::ttheme_minimal(
    base_size = 9,
    core = list(
      fg_params = list(hjust = 0.5, x = 0.5),
      padding = grid::unit(c(3, 3), "mm")
    ),
    colhead = list(
      fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5),
      padding = grid::unit(c(3, 3), "mm")
    )
  )
)

plot_main_q50_boxplot_overall_category_base <- ggplot(
  main_q50_boxplot_data,
  aes(x = category_group, y = q50_value)
) +
  geom_boxplot(width = 0.45, outlier.alpha = 0.7) +
  facet_wrap(~ category_group, nrow = 1) +
  labs(
    title = main_q50_question_title,
    subtitle = main_q50_question_subtitle,
    x = NULL,
    y = "Slider value (0–100)"
  ) +
  theme_result() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.caption = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

plot_main_q50_boxplot_overall_category <- gridExtra::arrangeGrob(
  plot_main_q50_boxplot_overall_category_base,
  main_q50_boxplot_stats_grob,
  ncol = 1,
  heights = c(4.8, 1.2)
)



requested_analysis_overview <- tibble(
  analysis_block = c(
    "Pre-Survey summary tables",
    "Pre-Survey frequency distributions",
    "Pre-Survey top-3 tables",
    "Longitudinal group 1 summary",
    "Longitudinal group 1 distributions",
    "Longitudinal group 1 changes",
    "Longitudinal group 2 summary",
    "Longitudinal group 2 distributions",
    "Longitudinal group 2 changes",
    "Longitudinal group 3 summary",
    "Longitudinal group 3 distributions",
    "Longitudinal group 3 changes",
    "Longitudinal multi-block summary",
    "Longitudinal multi-block distributions",
    "Longitudinal multi-block changes",
    "Main-Survey single-variable summary",
    "Main-Survey single distributions",
    "Main-Survey Q54 top 3"
  ),
  n_rows = c(
    nrow(requested_pre_variable_summary_table),
    nrow(requested_pre_frequency_table),
    nrow(requested_pre_top3_table),
    nrow(longitudinal_group_1_variable_summary_table),
    nrow(longitudinal_group_1_distribution),
    nrow(longitudinal_group_1_change),
    nrow(longitudinal_group_2_variable_summary_table),
    nrow(longitudinal_group_2_distribution),
    nrow(longitudinal_group_2_change),
    nrow(longitudinal_group_3_variable_summary_table),
    nrow(longitudinal_group_3_distribution),
    nrow(longitudinal_group_3_change),
    nrow(longitudinal_block_variable_summary_table),
    nrow(longitudinal_block_distribution),
    nrow(longitudinal_block_change),
    nrow(requested_main_single_variable_summary_table),
    nrow(requested_main_single_frequency_table),
    nrow(requested_main_q54_top3_table)
  )
)

plot_longitudinal_group_1 <- make_longitudinal_plot(
  longitudinal_group_1_distribution,
  title_text = "Distribution of image agreement (overall) across three iterations",
  response_levels = agreement_levels
)

plot_longitudinal_group_2 <- make_longitudinal_plot(
  longitudinal_group_2_distribution,
  title_text = "Distribution of change in the mental image across three iterations",
  response_levels = change_levels
)

plot_longitudinal_group_3 <- make_longitudinal_plot(
  longitudinal_group_3_distribution,
  title_text = "Distribution of process evaluation across three iterations",
  response_levels = process_eval_levels
)

plot_longitudinal_block <- make_longitudinal_block_plot(
  longitudinal_block_distribution,
  title_text = "Distribution of image agreement subscales across three iterations",
  response_levels = three_ita_levels
)


#####################################################################
###                 Ergebnisgrafiken erstellen                    ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden die Ergebnisgrafiken des Reporting-Skripts
# erstellt. Die zugrunde liegende Methodik sowie die verwendeten Variablen
# stammen aus dem präfixierten Skript; die grafische Struktur und Gliederung
# orientieren sich am bisherigen Aufbau.

# =========================================================
# 9) Grafiken erstellen                                  ===
# =========================================================

plot_cleaning_flow <- cleaning_flow_table %>%
  ggplot(aes(x = phase, y = n_cases, fill = dataset)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Case flow of the cleaning process",
    subtitle = NULL,
    x = NULL,
    y = "Number of cases"
  ) +
  theme_result()

plot_followup_selection <- pre_followup_summary %>%
  ggplot(aes(x = reorder(category, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Pre-Survey: central follow-up subgroups",
    subtitle = "Display of the central subsets based on Q30 and Q32",
    x = NULL,
    y = "Number of cases"
  ) +
  theme_result()

plot_email_matching <- email_matching_overview %>%
  ggplot(aes(x = reorder(group, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Overview of the final consolidated analysis dataset",
    subtitle = NULL,
    x = NULL,
    y = "Number of cases"
  ) +
  theme_result()

plot_viviq_distribution <- final_analysis_dataset %>%
  filter(!is.na(viviq_mean_score)) %>%
  ggplot(aes(x = viviq_mean_score)) +
  geom_histogram(binwidth = 0.25) +
  labs(
    title = "Distribution of the VIVIQ mean score",
    subtitle = "Only cases from the final consolidated dataset are included",
    x = "VIVIQ mean score",
    y = "Frequency"
  ) +
  theme_result()

plot_age_distribution <- NULL
if ("Main_Survey_Q55" %in% names(main_desc_base)) {
  plot_age_distribution <- main_desc_base %>%
    mutate(age_num = readr::parse_number(as.character(Main_Survey_Q55))) %>%
    ggplot(aes(x = age_num)) +
    geom_histogram(binwidth = 5) +
    labs(
      title = "Age distribution in the final matched Main-Survey sample",
      x = "Age",
      y = "Frequency"
    ) +
    theme_result()
}

plot_gender_distribution <- NULL
if ("Main_Survey_Q56" %in% names(main_desc_base)) {
  plot_gender_distribution <- main_desc_base %>%
    mutate(
      gender = if_else(
        is.na(Main_Survey_Q56) | stringr::str_trim(as.character(Main_Survey_Q56)) == "",
        "Missing",
        as.character(Main_Survey_Q56)
      )
    ) %>%
    count(gender) %>%
    ggplot(aes(x = reorder(gender, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Gender distribution in the final matched Main-Survey sample",
      x = NULL,
      y = "Number of cases"
    ) +
    theme_result()
}


#####################################################################
###      Beschriftungsvorschläge für Tabellen und Abbildungen    ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden Vorschläge für eine wissenschaftliche Benennung
# und Beschriftung der exportierten Tabellen und Grafiken erstellt. Diese
# Texte können direkt im Ergebnisteil oder im Abbildungsverzeichnis der
# Masterarbeit verwendet und bei Bedarf sprachlich angepasst werden.

# =========================================================
# 10) Caption-Tabellen erstellen                         ===
# =========================================================

table_caption_guide <- tibble(
  table_id = c("Table1", "Table2", "Table3", "Table4", "Table5", "Table6"),
  object_name = c(
    "raw_dataset_overview",
    "cleaning_flow_table",
    "key_dataset_overview",
    "email_matching_overview",
    "viviq_total_summary_table",
    "viviq_item_summary_table"
  ),
  caption_suggestion = c(
    "Overview of the raw datasets from the Pre-Survey and Main Survey.",
    "Case flow of the cleaning steps for the Pre-Survey and Main Survey.",
    "Overview of the central analytical datasets from the raw dataset to the final consolidated sample.",
    "Overview of the final consolidated analysis dataset and the availability of email and VIVIQ information.",
    "Descriptive metrics of the VIVIQ total score in the final consolidated sample.",
    "Descriptive metrics of the individual VIVIQ items in the final consolidated sample."
  )
)

figure_caption_guide <- tibble(
  figure_id = c("Fig1", "Fig2", "Fig3", "Fig4", "ExtFig1", "ExtFig2"),
  filename = c(
    "Fig1_cleaning_flow.png",
    "Fig2_followup_selection.png",
    "Fig3_email_matching_summary.png",
    "Fig4_viviq_distribution.png",
    "ExtFig1_age_distribution.png",
    "ExtFig2_gender_distribution.png"
  ),
  caption_suggestion = c(
    "Case flow of the cleaning steps in the Pre-Survey and Main Survey.",
    "Distribution of the central follow-up subgroups in the Pre-Survey.",
    "Overview of the final consolidated analysis dataset.",
    "Distribution of the VIVIQ mean score in the final consolidated sample.",
    "Age distribution in the final matched Main-Survey sample.",
    "Gender distribution in the final matched Main-Survey sample."
  )
)

extended_caption_guide <- tibble(
  table_id = c("ExtTable1", "ExtTable2", "ExtTable3", "ExtTable4", "ExtTable5", "ExtTable6"),  # CHANGED
  object_name = c(
    "pre_numeric_desc",
    "pre_categorical_desc",
    "main_numeric_desc",
    "main_categorical_desc",
    "core_variables_overview",
    "survey_duration_summary_table"  # CHANGED
  ),
  caption_suggestion = c(
    "Extended descriptive metrics of numeric variables in the matched Pre-Survey sample.",
    "Extended frequency distributions of categorical variables in the matched Pre-Survey sample.",
    "Extended descriptive metrics of numeric variables in the matched Main-Survey sample.",
    "Extended frequency distributions of categorical variables in the matched Main-Survey sample.",
    "Overview of the content-prioritized core variables for the extended descriptive presentation of results.",
    "Descriptive metrics of the survey duration variables in the matched Pre-Survey and Main-Survey samples."  # CHANGED
  )
)

requested_caption_guide <- tibble(
  table_id = c("ReqTable1", "ReqTable2", "ReqTable3", "ReqTable4", "ReqTable5", "ReqTable6", "ReqTable7", "ReqTable8", "ReqTable9", "ReqTable10", "ReqTable11", "ReqTable12"),
  object_name = c(
    "requested_pre_frequency_table",
    "requested_pre_top3_table",
    "longitudinal_group_1_distribution",
    "longitudinal_group_1_change",
    "longitudinal_group_2_distribution",
    "longitudinal_group_2_change",
    "longitudinal_group_3_distribution",
    "longitudinal_group_3_change",
    "longitudinal_block_distribution",
    "longitudinal_block_change",
    "requested_main_single_frequency_table",
    "requested_main_q54_top3_table"
  ),
  caption_suggestion = c(
    "Frequency distributions of selected Pre-Survey variables in the final consolidated sample.",
    "Top-3 responses of selected Pre-Survey variables in the final consolidated sample.",
    "Distributions of group 1 across the three iterations.",
    "Absolute and percentage changes in group 1 between iterations.",
    "Distributions of group 2 across the three iterations.",
    "Absolute and percentage changes in group 2 between iterations.",
    "Distributions of group 3 across the three iterations.",
    "Absolute and percentage changes in group 3 between iterations.",
    "Distributions of the multi-part evaluation block across the three iterations.",
    "Absolute and percentage changes of the multi-part evaluation block between iterations.",
    "Frequency distributions of selected Main-Survey single variables in the final consolidated sample.",
    "Top-3 responses for the variable Main_Survey_Q54 in the final consolidated sample."
  )
)


#####################################################################
###                  Export der Tabellen und Grafiken            ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden alle Tabellen, Grafiken und Caption-Tabellen in
# einer sauberen Ordnerstruktur gespeichert. Dadurch können die Ergebnisse
# direkt in den Ergebnisteil, den Anhang oder ein separates Reporting-Dokument
# übernommen werden.

# =========================================================
# 11) Export                                              ===
# =========================================================

required_export_objects <- c(
  "raw_dataset_overview", "cleaning_flow_table", "key_dataset_overview",
  "email_matching_overview", "viviq_total_summary_table",
  "viviq_item_summary_table", "final_analysis_overview",
  "table_caption_guide", "figure_caption_guide", "final_analysis_dataset",
  "pre_desc_base", "main_desc_base",
  "pre_numeric_desc", "pre_categorical_desc",
  "main_numeric_desc", "main_categorical_desc",
  "core_variables_overview", "extended_caption_guide",
  "survey_duration_summary_table",
  "requested_pre_frequency_table", "requested_pre_top3_table",
  "longitudinal_group_1_variable_summary_by_category_table",
  "longitudinal_group_1_distribution", "longitudinal_group_1_change",
  "longitudinal_block_variable_summary_by_category_table",
  "longitudinal_group_2_variable_summary_by_category_table",
  "longitudinal_group_2_distribution", "longitudinal_group_2_change",
  "longitudinal_group_3_distribution", "longitudinal_group_3_change",
  "longitudinal_block_distribution", "longitudinal_block_change",
  "requested_main_single_frequency_table", "requested_main_q54_top3_table",
  "requested_analysis_overview", "requested_caption_guide", "target_word_distribution",
  "longitudinal_group_3_variable_summary_by_category_table",
  "main_q48_summary_overall_category_table",
  "main_q49_summary_overall_category_table",
  "target_word_category_distribution",
  "target_word_category_check",
  "unmapped_target_words",
  "main_q48_summary_overall_category_table",
  "main_q49_summary_overall_category_table",
  "main_q50_summary_overall_category_table",
  "main_q51_summary_overall_category_table",
  "main_q52_summary_overall_category_table",
  "main_q53_summary_overall_category_table",
  "requested_main_q54_multiselect_distribution",
  "requested_main_q54_top3_table"
)

missing_export_objects <- required_export_objects[
  !vapply(
    required_export_objects,
    exists,
    logical(1),
    envir = .GlobalEnv,
    inherits = FALSE
  )
]

if (length(missing_export_objects) > 0) {
  stop(
    paste0(
      "The export cannot be started. The following objects are still missing:\n",
      paste(missing_export_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

save_table_outputs(preview_removal_summary, "00_preview_removal_summary")
save_table_outputs(raw_dataset_overview, "01_raw_dataset_overview")
save_table_outputs(cleaning_flow_table, "02_cleaning_flow_table")
save_table_outputs(key_dataset_overview, "03_key_dataset_overview")
save_table_outputs(email_matching_overview, "04_email_matching_overview")
save_table_outputs(viviq_total_summary_table, "05_viviq_total_summary_table")
save_table_outputs(viviq_item_summary_table, "06_viviq_item_summary_table")
save_table_outputs(final_analysis_overview, "07_final_analysis_overview")
save_table_outputs(table_caption_guide, "08_table_caption_guide")
save_table_outputs(figure_caption_guide, "09_figure_caption_guide")

writexl::write_xlsx(
  list(
    preview_removal_summary = preview_removal_summary,
    raw_dataset_overview = raw_dataset_overview,
    cleaning_flow_table = cleaning_flow_table,
    key_dataset_overview = key_dataset_overview,
    email_matching_overview = email_matching_overview,
    viviq_total_summary_table = viviq_total_summary_table,
    viviq_item_summary_table = viviq_item_summary_table,
    final_analysis_overview = final_analysis_overview,
    table_caption_guide = table_caption_guide,
    figure_caption_guide = figure_caption_guide,
    target_word_distribution = target_word_distribution,
    target_word_category_distribution = target_word_category_distribution,
    target_word_category_check = target_word_category_check,
    unmapped_target_words = unmapped_target_words
  ),
  path = file.path(out_tables_dir, "05_descriptive_reporting_tables.xlsx")
)

ggsave(file.path(out_figures_dir, "Fig1_cleaning_flow.png"), plot_cleaning_flow, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_figures_dir, "Fig2_followup_selection.png"), plot_followup_selection, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_figures_dir, "Fig3_email_matching_summary.png"), plot_email_matching, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_figures_dir, "Fig4_viviq_distribution.png"), plot_viviq_distribution, width = 8, height = 5, dpi = 300)
ggsave(
  file.path(out_requested_figures, "ReqFig5_main_q50_boxplot_overall_category.png"),
  plot_main_q50_boxplot_overall_category,
  width = 12,
  height = 7.2,
  dpi = 300,
  bg = "white"
)

if (!is.null(plot_age_distribution)) {
  ggsave(file.path(out_extended_figures, "ExtFig1_age_distribution.png"), plot_age_distribution, width = 8, height = 5, dpi = 300)
}

if (!is.null(plot_gender_distribution)) {
  ggsave(file.path(out_extended_figures, "ExtFig2_gender_distribution.png"), plot_gender_distribution, width = 8, height = 5, dpi = 300)
}

save_extended_table_outputs(pre_numeric_desc, "01_pre_numeric_desc")
save_extended_table_outputs(pre_categorical_desc, "02_pre_categorical_desc")
save_extended_table_outputs(main_numeric_desc, "03_main_numeric_desc")
save_extended_table_outputs(main_categorical_desc, "04_main_categorical_desc")
save_extended_table_outputs(core_variables_overview, "05_core_variables_overview")
save_extended_table_outputs(extended_caption_guide, "06_extended_caption_guide")
save_extended_table_outputs(survey_duration_summary_table, "07_survey_duration_summary_table")  # CHANGED
save_table_outputs(target_word_distribution, "07a_target_word_distribution")
save_table_outputs(target_word_category_distribution, "07b_target_word_category_distribution")
save_table_outputs(target_word_category_check, "07c_target_word_category_check")
save_table_outputs(unmapped_target_words, "07d_unmapped_target_words")


writexl::write_xlsx(
  list(
    pre_desc_base_matched_full = pre_desc_base,
    main_desc_base_matched_full = main_desc_base,
    pre_numeric_desc = pre_numeric_desc,
    pre_categorical_desc = pre_categorical_desc,
    main_numeric_desc = main_numeric_desc,
    main_categorical_desc = main_categorical_desc,
    core_variables_overview = core_variables_overview,
    extended_caption_guide = extended_caption_guide,
    survey_duration_summary_table = survey_duration_summary_table  # CHANGED
  ),
  path = file.path(out_extended_dir, "05b_extended_descriptives.xlsx")
)

save_requested_table_outputs(requested_pre_variable_summary_table, "01_requested_pre_variable_summary_table")
save_requested_table_outputs(requested_pre_frequency_table, "02_requested_pre_frequency_table")
save_requested_table_outputs(requested_pre_top3_table, "03_requested_pre_top3_table")
save_requested_table_outputs(longitudinal_group_1_variable_summary_table, "04_longitudinal_group_1_variable_summary_table")
save_requested_table_outputs(longitudinal_group_1_distribution, "05_longitudinal_group_1_distribution")
save_requested_table_outputs(longitudinal_group_1_change, "06_longitudinal_group_1_change")
save_requested_table_outputs(longitudinal_group_2_variable_summary_table, "07_longitudinal_group_2_variable_summary_table")
save_requested_table_outputs(longitudinal_group_2_distribution, "08_longitudinal_group_2_distribution")
save_requested_table_outputs(longitudinal_group_2_change, "09_longitudinal_group_2_change")
save_requested_table_outputs(longitudinal_group_3_variable_summary_table, "10_longitudinal_group_3_variable_summary_table")
save_requested_table_outputs(longitudinal_group_3_distribution, "11_longitudinal_group_3_distribution")
save_requested_table_outputs(longitudinal_group_3_change, "12_longitudinal_group_3_change")
save_requested_table_outputs(longitudinal_block_variable_summary_table, "13_longitudinal_block_variable_summary_table")
save_requested_table_outputs(longitudinal_block_distribution, "14_longitudinal_block_distribution")
save_requested_table_outputs(longitudinal_block_change, "15_longitudinal_block_change")
save_requested_table_outputs(requested_main_single_variable_summary_table, "16_requested_main_single_variable_summary_table")
save_requested_table_outputs(requested_main_single_frequency_table, "17_requested_main_single_frequency_table")
save_requested_table_outputs(requested_main_q54_top3_table, "18_requested_main_q54_top3_table")
save_requested_table_outputs(requested_analysis_overview, "19_requested_analysis_overview")
save_requested_table_outputs(requested_caption_guide, "20_requested_caption_guide")
save_requested_table_outputs(longitudinal_group_1_variable_summary_by_category_table,"04b_longitudinal_group_1_variable_summary_by_category_table")
save_requested_table_outputs(longitudinal_group_2_variable_summary_by_category_table,"07b_longitudinal_group_2_variable_summary_by_category_table")
save_requested_table_outputs(longitudinal_block_variable_summary_by_category_table,"13b_longitudinal_block_variable_summary_by_category_table")
save_requested_table_outputs(longitudinal_group_3_variable_summary_by_category_table,"10b_longitudinal_group_3_variable_summary_by_category_table")
save_requested_table_outputs(main_q48_summary_overall_category_table,"18b_main_q48_summary_overall_category_table")
save_requested_table_outputs(main_q49_summary_overall_category_table,"18c_main_q49_summary_overall_category_table")
save_requested_table_outputs(main_q48_summary_overall_category_table, "18b_main_q48_summary_overall_category_table")
save_requested_table_outputs(main_q49_summary_overall_category_table, "18c_main_q49_summary_overall_category_table")
save_requested_table_outputs(main_q50_summary_overall_category_table, "18d_main_q50_summary_overall_category_table")
save_requested_table_outputs(main_q51_summary_overall_category_table, "18e_main_q51_summary_overall_category_table")
save_requested_table_outputs(main_q52_summary_overall_category_table, "18f_main_q52_summary_overall_category_table")
save_requested_table_outputs(main_q53_summary_overall_category_table, "18g_main_q53_summary_overall_category_table")
save_requested_table_outputs(main_q50_boxplot_stats_table,"18h_main_q50_boxplot_stats_table")
save_requested_table_outputs(requested_main_q54_multiselect_distribution,"18_requested_main_q54_multiselect_distribution")
save_requested_table_outputs(requested_main_q54_top3_table,"19_requested_main_q54_top3_table")

writexl::write_xlsx(
  list(
    requested_pre_variable_summary_table = requested_pre_variable_summary_table,
    requested_pre_frequency_table = requested_pre_frequency_table,
    requested_pre_top3_table = requested_pre_top3_table,
    longitudinal_group_1_variable_summary_table = longitudinal_group_1_variable_summary_table,
    longitudinal_group_1_distribution = longitudinal_group_1_distribution,
    longitudinal_group_1_change = longitudinal_group_1_change,
    longitudinal_group_2_variable_summary_table = longitudinal_group_2_variable_summary_table,
    longitudinal_group_2_distribution = longitudinal_group_2_distribution,
    longitudinal_group_2_change = longitudinal_group_2_change,
    longitudinal_group_3_variable_summary_table = longitudinal_group_3_variable_summary_table,
    longitudinal_group_3_distribution = longitudinal_group_3_distribution,
    longitudinal_group_3_change = longitudinal_group_3_change,
    longitudinal_block_variable_summary_table = longitudinal_block_variable_summary_table,
    longitudinal_block_distribution = longitudinal_block_distribution,
    longitudinal_block_change = longitudinal_block_change,
    requested_main_single_variable_summary_table = requested_main_single_variable_summary_table,
    requested_main_single_frequency_table = requested_main_single_frequency_table,
    requested_main_q54_top3_table = requested_main_q54_top3_table,
    requested_analysis_overview = requested_analysis_overview,
    requested_caption_guide = requested_caption_guide,
    longitudinal_group_1_variable_summary_by_category_table = longitudinal_group_1_variable_summary_by_category_table,
    longitudinal_group_2_variable_summary_by_category_table = longitudinal_group_2_variable_summary_by_category_table,
    longitudinal_block_variable_summary_by_category_table = longitudinal_block_variable_summary_by_category_table,
    longitudinal_group_3_variable_summary_by_category_table = longitudinal_group_3_variable_summary_by_category_table,
    main_q48_summary_overall_category_table = main_q48_summary_overall_category_table,
    main_q49_summary_overall_category_table = main_q49_summary_overall_category_table,
    main_q50_summary_overall_category_table = main_q50_summary_overall_category_table,
    main_q51_summary_overall_category_table = main_q51_summary_overall_category_table,
    main_q52_summary_overall_category_table = main_q52_summary_overall_category_table,
    main_q53_summary_overall_category_table = main_q53_summary_overall_category_table,
    main_q50_boxplot_stats_table = main_q50_boxplot_stats_table,
    requested_main_q54_multiselect_distribution = requested_main_q54_multiselect_distribution,
    requested_main_q54_top3_table = requested_main_q54_top3_table),
  path = file.path(out_requested_dir, "05c_requested_analysis_tables.xlsx")
)

ggsave(file.path(out_requested_figures, "ReqFig1_longitudinal_group_1.png"), plot_longitudinal_group_1, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_requested_figures, "ReqFig2_longitudinal_group_2.png"), plot_longitudinal_group_2, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_requested_figures, "ReqFig3_longitudinal_group_3.png"), plot_longitudinal_group_3, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_requested_figures, "ReqFig4_longitudinal_block.png"), plot_longitudinal_block, width = 10, height = 6, dpi = 300)
ggsave(
  file.path(out_requested_figures, "ReqFig5_main_q50_boxplot_overall_category.png"),
  plot_main_q50_boxplot_overall_category,
  width = 12,
  height = 7.2,
  dpi = 300,
  bg = "white"
)


#####################################################################
###                    Schnellcheck in der Konsole                ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden zentrale Kennzahlen direkt in der Konsole
# ausgegeben. Dadurch kann rasch geprüft werden, ob das Reporting erfolgreich
# erstellt wurde und ob die wichtigsten Fallzahlen plausibel erscheinen.

# =========================================================
# 12) Konsolenausgaben für den Schnellcheck              ===
# =========================================================

cat("\n==================== RAW DATASET OVERVIEW ====================\n")
print(raw_dataset_overview)

cat("\n==================== CLEANING FLOW TABLE ====================\n")
print(cleaning_flow_table)

cat("\n==================== KEY DATASET OVERVIEW ====================\n")
print(key_dataset_overview)

cat("\n==================== EMAIL MATCHING OVERVIEW ====================\n")
print(email_matching_overview)

cat("\n==================== PRE NUMERIC DESCRIPTIVES ====================\n")
print(pre_numeric_desc)

cat("\n==================== PRE CATEGORICAL DESCRIPTIVES ====================\n")
print(pre_categorical_desc)

cat("\n==================== MAIN NUMERIC DESCRIPTIVES ====================\n")
print(main_numeric_desc)

cat("\n==================== MAIN CATEGORICAL DESCRIPTIVES ====================\n")
print(main_categorical_desc)

cat("\n==================== REQUESTED PRE FREQUENCY TABLE ====================\n")
print(requested_pre_frequency_table)

cat("\n==================== REQUESTED PRE TOP 3 TABLE ====================\n")
print(requested_pre_top3_table)

cat("\n==================== LONGITUDINAL GROUP 1 DISTRIBUTION ====================\n")
print(longitudinal_group_1_distribution)

cat("\n==================== LONGITUDINAL GROUP 1 CHANGE ====================\n")
print(longitudinal_group_1_change)

cat("\n==================== LONGITUDINAL GROUP 2 DISTRIBUTION ====================\n")
print(longitudinal_group_2_distribution)

cat("\n==================== LONGITUDINAL GROUP 2 CHANGE ====================\n")
print(longitudinal_group_2_change)

cat("\n==================== LONGITUDINAL GROUP 3 DISTRIBUTION ====================\n")
print(longitudinal_group_3_distribution)

cat("\n==================== LONGITUDINAL GROUP 3 CHANGE ====================\n")
print(longitudinal_group_3_change)

cat("\n==================== LONGITUDINAL BLOCK DISTRIBUTION ====================\n")
print(longitudinal_block_distribution)

cat("\n==================== LONGITUDINAL BLOCK CHANGE ====================\n")
print(longitudinal_block_change)

cat("\n==================== REQUESTED MAIN SINGLE FREQUENCY TABLE ====================\n")
print(requested_main_single_frequency_table)

cat("\n==================== REQUESTED MAIN Q54 TOP 3 TABLE ====================\n")
print(requested_main_q54_top3_table)

cat("\n==================== SURVEY DURATION SUMMARY ====================\n")
print(survey_duration_summary_table)

cat("\n==================== LONGITUDINAL GROUP 2 SUMMARY BY TARGET WORD CATEGORY ====================\n")
print(longitudinal_group_2_variable_summary_by_category_table)

cat("\n==================== LONGITUDINAL MULTI-BLOCK SUMMARY BY TARGET WORD CATEGORY ====================\n")
print(longitudinal_block_variable_summary_by_category_table)

cat("\n==================== LONGITUDINAL GROUP 3 SUMMARY BY TARGET WORD CATEGORY ====================\n")
print(longitudinal_group_3_variable_summary_by_category_table)

cat("\n==================== MAIN Q48 SUMMARY OVERALL / ABSTRACT / CONCRETE ====================\n")
print(main_q48_summary_overall_category_table)

cat("\n==================== MAIN Q49 SUMMARY OVERALL / ABSTRACT / CONCRETE ====================\n")
print(main_q49_summary_overall_category_table)

cat("\n==================== MAIN Q48 SUMMARY OVERALL / ABSTRACT / CONCRETE ====================\n")
print(main_q48_summary_overall_category_table)

cat("\n==================== MAIN Q49 SUMMARY OVERALL / ABSTRACT / CONCRETE ====================\n")
print(main_q49_summary_overall_category_table)

cat("\n==================== MAIN Q50 SUMMARY OVERALL / ABSTRACT / CONCRETE ====================\n")
print(main_q50_summary_overall_category_table)

cat("\n==================== MAIN Q51 SUMMARY OVERALL / ABSTRACT / CONCRETE ====================\n")
print(main_q51_summary_overall_category_table)

cat("\n==================== MAIN Q52 SUMMARY OVERALL / ABSTRACT / CONCRETE ====================\n")
print(main_q52_summary_overall_category_table)

cat("\n==================== MAIN Q53 SUMMARY OVERALL / ABSTRACT / CONCRETE ====================\n")
print(main_q53_summary_overall_category_table)

cat("\n==================== MAIN Q50 BOXPLOT STATS OVERALL / ABSTRACT / CONCRETE ====================\n")
print(main_q50_boxplot_stats_table)

#####################################################################
###              Wissenschaftliche Tabellen anzeigen              ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden die zentralen Ergebnis- und
# Deskriptivtabellen zusätzlich als wissenschaftlich formatierte
# Tabellen dargestellt. Die Inhalte bleiben unverändert; es erfolgt
# ausschließlich eine zusätzliche tabellarische Darstellung.

# =========================================================
# 13) Wissenschaftliche Tabellen anzeigen                ===
# =========================================================

gt_preview_removal_summary <- make_gt_table(
  preview_removal_summary,
  title_text = "Documentation of internally removed preview cases",
  subtitle_text = "Pre-Survey and Main-Survey"
)

gt_raw_dataset_overview <- make_gt_table(
  raw_dataset_overview,
  title_text = "Overview of analysis-start datasets",
  subtitle_text = "Pre-Survey and Main-Survey"
)

gt_cleaning_flow_table <- make_gt_table(
  cleaning_flow_table,
  title_text = "Overview of the case flow of the cleaning process",
  subtitle_text = "Pre-Survey and Main-Survey"
)

gt_key_dataset_overview <- make_gt_table(
  key_dataset_overview,
  title_text = "Overview of the central analytical datasets",
  subtitle_text = NULL
)

gt_email_matching_overview <- make_gt_table(
  email_matching_overview,
  title_text = "Overview of the final consolidated analysis dataset",
  subtitle_text = NULL
)

gt_viviq_total_summary_table <- make_gt_table(
  viviq_total_summary_table,
  title_text = "Overview of VIVIQ overall metrics",
  subtitle_text = "Final consolidated sample"
)

gt_viviq_item_summary_table <- make_gt_table(
  viviq_item_summary_table,
  title_text = "Overview of VIVIQ item metrics",
  subtitle_text = "Final consolidated sample"
)

gt_final_analysis_overview <- make_gt_table(
  final_analysis_overview,
  title_text = "Overview of the final analysis dataset",
  subtitle_text = NULL
)

gt_pre_numeric_desc <- make_gt_table(
  pre_numeric_desc,
  title_text = "Extended descriptive metrics of numeric variables",
  subtitle_text = "Matched Pre-Survey sample"
)

gt_longitudinal_block_variable_summary_by_category_table <- make_gt_table(
  longitudinal_block_variable_summary_by_category_table,
  title_text = "Longitudinal multi-block analysis: summary by variable and target word category",
  subtitle_text = "Three items across three iterations by abstract and concrete target words"
)

gt_pre_categorical_desc <- make_gt_table(
  pre_categorical_desc,
  title_text = "Extended frequency distributions of categorical variables",
  subtitle_text = "Matched Pre-Survey sample"
)
gt_longitudinal_group_2_variable_summary_by_category_table <- make_gt_table(
  longitudinal_group_2_variable_summary_by_category_table,
  title_text = "Longitudinal analysis group 2: summary by variable and target word category",
  subtitle_text = "Change in the mental image across three iterations by abstract and concrete target words"
)

gt_main_numeric_desc <- make_gt_table(
  main_numeric_desc,
  title_text = "Extended descriptive metrics of numeric variables",
  subtitle_text = "Matched Main-Survey sample"
)

gt_longitudinal_group_1_variable_summary_by_category_table <- make_gt_table(
  longitudinal_group_1_variable_summary_by_category_table,
  title_text = "Longitudinal analysis group 1: summary by variable and target word category",
  subtitle_text = "Image alignment across three iterations by abstract and concrete target words"
)

gt_main_categorical_desc <- make_gt_table(
  main_categorical_desc,
  title_text = "Extended frequency distributions of categorical variables",
  subtitle_text = "Matched Main-Survey sample"
)

gt_core_variables_overview <- make_gt_table(
  core_variables_overview,
  title_text = "Overview of prioritized core variables",
  subtitle_text = "Extended descriptive presentation of results"
)

gt_table_caption_guide <- make_gt_table(
  table_caption_guide,
  title_text = "Overview of table captions",
  subtitle_text = NULL
)

gt_figure_caption_guide <- make_gt_table(
  figure_caption_guide,
  title_text = "Overview of figure captions",
  subtitle_text = NULL
)

gt_extended_caption_guide <- make_gt_table(
  extended_caption_guide,
  title_text = "Overview of extended table captions",
  subtitle_text = NULL
)

gt_requested_analysis_overview <- make_gt_table(
  requested_analysis_overview,
  title_text = "Overview of requested additional analyses",
  subtitle_text = NULL
)

gt_requested_pre_variable_summary_table <- make_gt_table(
  requested_pre_variable_summary_table,
  title_text = "Summary of selected Pre-Survey variables",
  subtitle_text = "Final consolidated sample"
)

gt_requested_pre_frequency_table <- make_gt_table(
  requested_pre_frequency_table,
  title_text = "Frequency distributions of selected Pre-Survey variables",
  subtitle_text = "Final consolidated sample"
)

gt_requested_pre_top3_table <- make_gt_table(
  requested_pre_top3_table,
  title_text = "Top-3 responses of selected Pre-Survey variables",
  subtitle_text = "Final consolidated sample"
)

gt_longitudinal_group_1_variable_summary_table <- make_gt_table(
  longitudinal_group_1_variable_summary_table,
  title_text = "Longitudinal analysis group 1: summary by variable",
  subtitle_text = "Image alignment across three iterations"
)

gt_longitudinal_group_1_distribution <- make_gt_table(
  longitudinal_group_1_distribution,
  title_text = "Longitudinal analysis group 1: distributions",
  subtitle_text = "Image alignment across three iterations"
)

gt_longitudinal_group_1_change <- make_gt_table(
  longitudinal_group_1_change,
  title_text = "Longitudinal analysis group 1: changes",
  subtitle_text = "Absolute and percentage differences"
)

gt_longitudinal_group_2_variable_summary_table <- make_gt_table(
  longitudinal_group_2_variable_summary_table,
  title_text = "Longitudinal analysis group 2: summary by variable",
  subtitle_text = "Change in the mental image across three iterations"
)

gt_longitudinal_group_2_distribution <- make_gt_table(
  longitudinal_group_2_distribution,
  title_text = "Longitudinal analysis group 2: distributions",
  subtitle_text = "Change in the mental image across three iterations"
)

gt_longitudinal_group_2_change <- make_gt_table(
  longitudinal_group_2_change,
  title_text = "Longitudinal analysis group 2: changes",
  subtitle_text = "Absolute and percentage differences"
)

gt_longitudinal_group_3_variable_summary_table <- make_gt_table(
  longitudinal_group_3_variable_summary_table,
  title_text = "Longitudinal analysis group 3: summary by variable",
  subtitle_text = "Process evaluation across three iterations"
)

gt_longitudinal_group_3_distribution <- make_gt_table(
  longitudinal_group_3_distribution,
  title_text = "Longitudinal analysis group 3: distributions",
  subtitle_text = "Process evaluation across three iterations"
)

gt_longitudinal_group_3_change <- make_gt_table(
  longitudinal_group_3_change,
  title_text = "Longitudinal analysis group 3: changes",
  subtitle_text = "Absolute and percentage differences"
)

gt_longitudinal_block_variable_summary_table <- make_gt_table(
  longitudinal_block_variable_summary_table,
  title_text = "Longitudinal multi-block analysis: summary by variable",
  subtitle_text = "Three items across three iterations"
)

gt_longitudinal_block_distribution <- make_gt_table(
  longitudinal_block_distribution,
  title_text = "Longitudinal multi-block analysis: distributions",
  subtitle_text = "Three items across three iterations"
)

gt_longitudinal_block_change <- make_gt_table(
  longitudinal_block_change,
  title_text = "Longitudinal multi-block analysis: changes",
  subtitle_text = "Absolute and percentage differences"
)

gt_requested_main_single_variable_summary_table <- make_gt_table(
  requested_main_single_variable_summary_table,
  title_text = "Summary of selected Main-Survey single variables",
  subtitle_text = "Final consolidated sample"
)

gt_requested_main_single_frequency_table <- make_gt_table(
  requested_main_single_frequency_table,
  title_text = "Frequency distributions of selected Main-Survey single variables",
  subtitle_text = "Final consolidated sample"
)

gt_requested_main_q54_top3_table <- make_gt_table(
  requested_main_q54_top3_table,
  title_text = "Top-3 responses for Main_Survey_Q54",
  subtitle_text = "Final consolidated sample"
)

gt_requested_caption_guide <- make_gt_table(
  requested_caption_guide,
  title_text = "Overview of captions for the additional analyses",
  subtitle_text = NULL
)

gt_survey_duration_summary_table <- make_gt_table(
  survey_duration_summary_table,
  title_text = "Descriptive metrics of survey duration",
  subtitle_text = "Matched Pre-Survey and Main-Survey samples"
)


gt_longitudinal_group_3_variable_summary_by_category_table <- make_gt_table(
  longitudinal_group_3_variable_summary_by_category_table,
  title_text = "Longitudinal analysis group 3: summary by variable and target word category",
  subtitle_text = "Process evaluation across three iterations by abstract and concrete target words"
)

gt_main_q48_summary_overall_category_table <- make_gt_table(
  main_q48_summary_overall_category_table,
  title_text = "Summary of Main_Survey_Q48 by target word category",
  subtitle_text = "Overall, abstract and concrete target words"
)

# GT-Tabelle
gt_main_q49_summary_overall_category_table <- make_gt_table(
  main_q49_summary_overall_category_table,
  title_text = "Summary of Main_Survey_Q49 by target word category",
  subtitle_text = "Overall, abstract and concrete target words"
)
gt_main_q48_summary_overall_category_table <- make_gt_table(
  main_q48_summary_overall_category_table,
  title_text = "Summary of Main_Survey_Q48 by target word category",
  subtitle_text = "Overall, abstract and concrete target words"
)

gt_main_q49_summary_overall_category_table <- make_gt_table(
  main_q49_summary_overall_category_table,
  title_text = "Summary of Main_Survey_Q49 by target word category",
  subtitle_text = "Overall, abstract and concrete target words"
)

gt_main_q50_summary_overall_category_table <- make_gt_table(
  main_q50_summary_overall_category_table,
  title_text = "Summary of Main_Survey_Q50 by target word category",
  subtitle_text = "Overall, abstract and concrete target words"
)

gt_main_q51_summary_overall_category_table <- make_gt_table(
  main_q51_summary_overall_category_table,
  title_text = "Summary of Main_Survey_Q51 by target word category",
  subtitle_text = "Overall, abstract and concrete target words"
)

gt_main_q52_summary_overall_category_table <- make_gt_table(
  main_q52_summary_overall_category_table,
  title_text = "Summary of Main_Survey_Q52 by target word category",
  subtitle_text = "Overall, abstract and concrete target words"
)

gt_main_q53_summary_overall_category_table <- make_gt_table(
  main_q53_summary_overall_category_table,
  title_text = "Summary of Main_Survey_Q53 by target word category",
  subtitle_text = "Overall, abstract and concrete target words"
)

gt_requested_main_q54_multiselect_distribution <- make_gt_table(
  requested_main_q54_multiselect_distribution,
  title_text = "Distribution of Main_Survey_Q54 responses",
  subtitle_text = "Multiple-choice answers split by semicolon"
)

gt_requested_main_q54_top3_table <- make_gt_table(
  requested_main_q54_top3_table,
  title_text = "Top-3 responses for Main_Survey_Q54",
  subtitle_text = "Multiple-choice answers split by semicolon"
)


gt_output_list <- list(
  gt_preview_removal_summary = gt_preview_removal_summary,
  gt_raw_dataset_overview = gt_raw_dataset_overview,
  gt_cleaning_flow_table = gt_cleaning_flow_table,
  gt_key_dataset_overview = gt_key_dataset_overview,
  gt_email_matching_overview = gt_email_matching_overview,
  gt_viviq_total_summary_table = gt_viviq_total_summary_table,
  gt_viviq_item_summary_table = gt_viviq_item_summary_table,
  gt_final_analysis_overview = gt_final_analysis_overview,
  gt_pre_numeric_desc = gt_pre_numeric_desc,
  gt_pre_categorical_desc = gt_pre_categorical_desc,
  gt_main_numeric_desc = gt_main_numeric_desc,
  gt_main_categorical_desc = gt_main_categorical_desc,
  gt_core_variables_overview = gt_core_variables_overview,
  gt_survey_duration_summary_table = gt_survey_duration_summary_table,  # CHANGED
  gt_table_caption_guide = gt_table_caption_guide,
  gt_figure_caption_guide = gt_figure_caption_guide,
  gt_extended_caption_guide = gt_extended_caption_guide,
  gt_requested_analysis_overview = gt_requested_analysis_overview,
  gt_requested_pre_variable_summary_table = gt_requested_pre_variable_summary_table,
  gt_requested_pre_frequency_table = gt_requested_pre_frequency_table,
  gt_requested_pre_top3_table = gt_requested_pre_top3_table,
  gt_longitudinal_group_1_variable_summary_table = gt_longitudinal_group_1_variable_summary_table,
  gt_longitudinal_group_1_distribution = gt_longitudinal_group_1_distribution,
  gt_longitudinal_group_1_change = gt_longitudinal_group_1_change,
  gt_longitudinal_group_2_variable_summary_table = gt_longitudinal_group_2_variable_summary_table,
  gt_longitudinal_group_2_distribution = gt_longitudinal_group_2_distribution,
  gt_longitudinal_group_2_change = gt_longitudinal_group_2_change,
  gt_longitudinal_group_3_variable_summary_table = gt_longitudinal_group_3_variable_summary_table,
  gt_longitudinal_group_3_distribution = gt_longitudinal_group_3_distribution,
  gt_longitudinal_group_3_change = gt_longitudinal_group_3_change,
  gt_longitudinal_block_variable_summary_table = gt_longitudinal_block_variable_summary_table,
  gt_longitudinal_block_distribution = gt_longitudinal_block_distribution,
  gt_longitudinal_block_change = gt_longitudinal_block_change,
  gt_requested_main_single_variable_summary_table = gt_requested_main_single_variable_summary_table,
  gt_requested_main_single_frequency_table = gt_requested_main_single_frequency_table,
  gt_requested_main_q54_top3_table = gt_requested_main_q54_top3_table,
  gt_requested_caption_guide = gt_requested_caption_guide,
  gt_longitudinal_group_1_variable_summary_by_category_table = gt_longitudinal_group_1_variable_summary_by_category_table,
  gt_longitudinal_group_2_variable_summary_by_category_table = gt_longitudinal_group_2_variable_summary_by_category_table,
  gt_longitudinal_block_variable_summary_by_category_table = gt_longitudinal_block_variable_summary_by_category_table,
  gt_longitudinal_group_3_variable_summary_by_category_table = gt_longitudinal_group_3_variable_summary_by_category_table,
  gt_main_q48_summary_overall_category_table = gt_main_q48_summary_overall_category_table,
  gt_main_q49_summary_overall_category_table = gt_main_q49_summary_overall_category_table,
  gt_main_q48_summary_overall_category_table = gt_main_q48_summary_overall_category_table,
  gt_main_q49_summary_overall_category_table = gt_main_q49_summary_overall_category_table,
  gt_main_q50_summary_overall_category_table = gt_main_q50_summary_overall_category_table,
  gt_main_q51_summary_overall_category_table = gt_main_q51_summary_overall_category_table,
  gt_main_q52_summary_overall_category_table = gt_main_q52_summary_overall_category_table,
  gt_main_q53_summary_overall_category_table = gt_main_q53_summary_overall_category_table,
  gt_requested_main_q54_multiselect_distribution = gt_requested_main_q54_multiselect_distribution,
  gt_requested_main_q54_top3_table = gt_requested_main_q54_top3_table
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
  base_filename = "05_reporting_gt_manifest",
  out_dir = out_gt_doc_dir
)
gt_index_path <- file.path(out_gt_dir, "00_gt_index.html")   # CHANGED

build_simple_html_index(
  manifest = gt_manifest,
  output_path = gt_index_path,                               # CHANGED
  title_text = "Descriptive statistics and reporting - gt outputs",
  intro_text = "This index links to all formatted gt tables created from the descriptive statistics and reporting workflow."
)

if (!file.exists(gt_index_path)) {                           # CHANGED
  stop(
    paste0("GT index file was not created: ", gt_index_path),
    call. = FALSE
  )
}

message("Index file: ", gt_index_path)                       # CHANGED
gt_index_path                                                # CHANGED



gt_console_summary <- c(
  "==================== REPORTING GT TABLES ====================",
  capture.output(print(gt_manifest)),
  "",
  paste0("HTML directory: ", out_gt_html_dir),
  paste0("RTF directory: ", out_gt_rtf_dir),
  paste0("Index file: ", gt_index_path)                    # CHANGED
)

writeLines(
  gt_console_summary,
  con = file.path(out_gt_doc_dir, "05_reporting_gt_console_summary.txt")
)

message("Confirmation: GT tables for the reporting workflow were exported successfully.")
message("HTML tables: ", out_gt_html_dir)
message("RTF tables (where supported): ", out_gt_rtf_dir)
message("Index file: ", gt_index_path)                     # CHANGED
message("Manifest: ", file.path(out_gt_doc_dir, "05_reporting_gt_manifest.csv"))

# =========================================================
# CHANGED: Pfad am Ende explizit zurückgeben
# =========================================================
gt_index_path                                               # CHANGED


gt_console_summary <- c(
  "==================== REPORTING GT TABLES ====================",
  capture.output(print(gt_manifest)),
  "",
  paste0("HTML directory: ", out_gt_html_dir),
  paste0("RTF directory: ", out_gt_rtf_dir),
  paste0("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
)

writeLines(
  gt_console_summary,
  con = file.path(out_gt_doc_dir, "05_reporting_gt_console_summary.txt")
)

message("Confirmation: GT tables for the reporting workflow were exported successfully.")
message("HTML tables: ", out_gt_html_dir)
message("RTF tables (where supported): ", out_gt_rtf_dir)
message("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
message("Manifest: ", file.path(out_gt_doc_dir, "05_reporting_gt_manifest.csv"))


# =========================================================
# 13) Lokaler Export-Index für Skript 05                  ===
# =========================================================

local_index_path_05 <- file.path(out_desc_dir, "00_export_index.html")
local_manifest_csv_05 <- file.path(out_captions_dir, "00_export_manifest.csv")
local_manifest_xlsx_05 <- file.path(out_captions_dir, "00_export_manifest.xlsx")

local_export_files_05 <- list.files(
  out_desc_dir,
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE,
  no.. = TRUE
)

local_export_files_05 <- local_export_files_05[
  file.exists(local_export_files_05) & !dir.exists(local_export_files_05)
]

local_export_files_05 <- setdiff(
  local_export_files_05,
  c(local_index_path_05, local_manifest_csv_05, local_manifest_xlsx_05)
)

export_manifest_05 <- tibble(
  label = basename(local_export_files_05),
  path = local_export_files_05,
  notes = paste0("Exportdatei aus Skript 05 (", toupper(tools::file_ext(local_export_files_05)), ")")
) %>%
  bind_rows(
    tibble(
      label = c("00_export_manifest.csv", "00_export_manifest.xlsx"),
      path = c(local_manifest_csv_05, local_manifest_xlsx_05),
      notes = c("Lokales Export-Manifest als CSV", "Lokales Export-Manifest als XLSX")
    )
  ) %>%
  arrange(path)

save_table_outputs(
  export_manifest_05,
  base_filename = "00_export_manifest",
  out_dir = out_captions_dir
)

build_general_export_index(
  manifest = export_manifest_05,
  output_path = local_index_path_05,
  title_text = "Deskriptive Statistik & Reporting: Export index",
  intro_text = "Dieser Unterindex bündelt Tabellen, Grafiken, GT-Tabellen und Dokumentationsdateien des Skripts 05."
)

message("Local export index: ", local_index_path_05)

gt_preview_removal_summary
gt_raw_dataset_overview
gt_cleaning_flow_table
gt_key_dataset_overview
gt_email_matching_overview
gt_viviq_total_summary_table
gt_viviq_item_summary_table
gt_final_analysis_overview
gt_pre_numeric_desc
gt_pre_categorical_desc
gt_main_numeric_desc
gt_main_categorical_desc
gt_core_variables_overview
gt_table_caption_guide
gt_figure_caption_guide
gt_extended_caption_guide
gt_requested_analysis_overview
gt_requested_pre_variable_summary_table
gt_requested_pre_frequency_table
gt_requested_pre_top3_table
gt_longitudinal_group_1_variable_summary_table
gt_longitudinal_group_1_distribution
gt_longitudinal_group_1_change
gt_longitudinal_group_2_variable_summary_table
gt_longitudinal_group_2_distribution
gt_longitudinal_group_2_change
gt_longitudinal_group_3_variable_summary_table
gt_longitudinal_group_3_distribution
gt_longitudinal_group_3_change
gt_longitudinal_block_variable_summary_table
gt_longitudinal_block_distribution
gt_longitudinal_block_change
gt_requested_main_single_variable_summary_table
gt_requested_main_single_frequency_table
gt_requested_main_q54_top3_table
gt_requested_caption_guide
gt_longitudinal_block_variable_summary_by_category_table

#####################################################################
###                    Ende des Workflows                         ###
#####################################################################

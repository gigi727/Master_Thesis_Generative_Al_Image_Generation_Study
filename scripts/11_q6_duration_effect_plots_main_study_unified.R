# 11_q6_duration_effect_plots_main_study_unified.R

#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.

#####################################################################
### Q6-Dauer-der-Nutzung-Effekte auf Main-Study-Ergebnisvariablen ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript erzeugt zusätzliche Ergebnisplots für die Main Study,
# bei denen die Verteilungen zentraler Outcome-Variablen nach der Angabe
# aus Pre_Survey_Q6 (Dauer der Nutzung von GenAI-Bildgeneratoren)
# dargestellt werden.
#
# Ziel:
# - Image Agreement (overall) über Iteration 1–3
# - Image Agreement (subscales) über Iteration 1–3
# - Change in Mental Image über Iteration 1–3
# - Main_Survey_Q52 nach Q6-Level
#
# Die Darstellung orientiert sich an den vorhandenen ReqFig-Plots aus
# 06_deskriptive_statistik_und_reporting_final_konsolidiert_erweitert.R,
# ergänzt diese jedoch um eine Stratifizierung nach Pre_Survey_Q6.
#
# Wichtige Annahme in diesem Skript:
# - Es werden nur Fälle mit gültiger Angabe in Pre_Survey_Q6 verwendet.
# - Fälle ohne gültige Q6-Angabe werden aus diesen Effektplots ausgeschlossen.
# - Die Reihenfolge der Q6-Kategorien folgt der bereits im Pre-Survey-
#   Deskriptivskript verwendeten Antwortreihenfolge.

# =========================================================
# 0) Pakete                                              ===
# =========================================================

# install.packages(c("tidyverse", "writexl", "here", "readr"), dependencies = TRUE)

library(tidyverse)
library(writexl)
library(here)
library(readr)

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

out_base_dir    <- file.path(project_root, "data_output", "main_study_q6_duration_effects")
out_tables_dir  <- file.path(out_base_dir, "tables")
out_figures_dir <- file.path(out_base_dir, "figures")
out_doc_dir     <- file.path(out_base_dir, "documentation")

purrr::walk(
  c(out_base_dir, out_tables_dir, out_figures_dir, out_doc_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# =========================================================
# 2) Benötigte Objekte prüfen und bei Bedarf laden       ===
# =========================================================

# =========================================================
# 2) Anonymisierte Analyse-Datensätze laden              ====
# =========================================================

# Dieses Skript verwendet direkt den finalen gematchten anonymisierten
# Datensatz aus data_final/. Es lädt nicht mehr Skript 06, damit keine
# Rohdaten- oder Cleaning-Objekte aus 01-03 benötigt werden.

loaded_datasets <- load_anonymized_analysis_datasets(
  project_root = project_root,
  require_pre = TRUE,
  require_main = TRUE,
  require_final = TRUE
)

final_analysis_dataset <- loaded_datasets$final_analysis_dataset
main_feature_lookup <- loaded_datasets$main_feature_lookup
pre_feature_lookup <- loaded_datasets$pre_feature_lookup

message("Confirmation: This script uses data_final/final_analysis_dataset_anonymized.rds as its analysis base.")

# Fallbacks, damit dieses Skript ohne Skript 06 lauffähig bleibt.
if (!exists("theme_result", envir = .GlobalEnv, inherits = FALSE)) {
  theme_result <- function() {
    ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 13),
        plot.subtitle = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(face = "bold"),
        strip.text = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
  }
}

agreement_levels <- if (exists("agreement_levels", envir = .GlobalEnv, inherits = FALSE)) {
  agreement_levels
} else {
  c(
    "Not at all",
    "Very weakly",
    "Weakly",
    "Moderately",
    "Strongly",
    "Very strongly",
    "Almost exactly"
  )
}

change_levels <- if (exists("change_levels", envir = .GlobalEnv, inherits = FALSE)) {
  change_levels
} else {
  c(
    "Strongly disagree",
    "Disagree",
    "Somewhat disagree",
    "Neither agree nor disagree",
    "Somewhat agree",
    "Agree",
    "Strongly agree"
  )
}

three_ita_levels <- if (exists("three_ita_levels", envir = .GlobalEnv, inherits = FALSE)) {
  three_ita_levels
} else {
  c(
    "Strongly disagree",
    "Disagree",
    "Slightly disagree",
    "Neither agree nor disagree",
    "Slightly agree",
    "Agree",
    "Strongly agree"
  )
}

q6_levels <- if (exists("q6_levels", envir = .GlobalEnv, inherits = FALSE)) {
  q6_levels
} else {
  c("Less than 1 month", "1–6 months", "7–12 months", "1–2 years", "More than 2 years")
}

# Laut Variablenübersicht ist Q52 auf derselben 1-7-Skala kodiert wie Q26/Q34/Q42.
q52_levels <- agreement_levels

analysis_df <- final_analysis_dataset
q6_var <- "Pre_Survey_Q6"


# =========================================================
# 3) Hilfsfunktionen                                     ===
# =========================================================

normalize_missing_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_
  x
}

pct <- function(x, base) {
  ifelse(is.na(base) | base == 0, NA_real_, round(100 * x / base, 1))
}

get_question_text <- function(lookup_df, var_name) {
  out <- lookup_df %>%
    filter(variable_name == var_name) %>%
    pull(question_text)

  if (length(out) == 0) NA_character_ else as.character(out[1])
}

save_table_outputs <- function(df, base_filename, out_dir = out_tables_dir) {
  readr::write_csv(df, file.path(out_dir, paste0(base_filename, ".csv")))
  writexl::write_xlsx(df, path = file.path(out_dir, paste0(base_filename, ".xlsx")))
}

make_q6_factor <- function(x, levels = q6_levels) {
  x <- normalize_missing_text(x)
  factor(x, levels = levels, ordered = TRUE)
}

make_q6_level_overview <- function(df, q6_var = "Pre_Survey_Q6", levels = q6_levels) {
  question_text <- get_question_text(pre_feature_lookup, q6_var)

  df %>%
    transmute(q6_level = make_q6_factor(.data[[q6_var]], levels = levels)) %>%
    filter(!is.na(q6_level)) %>%
    count(q6_level, name = "n", .drop = FALSE) %>%
    mutate(
      question_text = question_text,
      percent = pct(n, sum(n))
    ) %>%
    mutate(q6_level = as.character(q6_level))
}

make_q6_iteration_distribution <- function(df, var_map, response_levels, question_family,
                                           q6_var = "Pre_Survey_Q6", q6_levels = q6_levels) {
  purrr::imap_dfr(
    var_map,
    function(var_name, iteration_label) {
      question_text <- get_question_text(main_feature_lookup, var_name)

      df %>%
        transmute(
          q6_level = make_q6_factor(.data[[q6_var]], levels = q6_levels),
          response = normalize_missing_text(.data[[var_name]])
        ) %>%
        filter(!is.na(q6_level)) %>%
        mutate(response = if_else(is.na(response), "Missing", response)) %>%
        mutate(response = factor(response, levels = c(response_levels, "Missing"), ordered = TRUE)) %>%
        count(q6_level, response, name = "n", .drop = FALSE) %>%
        group_by(q6_level) %>%
        mutate(
          iteration = iteration_label,
          variable_name = var_name,
          question_family = question_family,
          question_text = question_text,
          n_group_total = sum(n),
          percent = pct(n, n_group_total),
          .before = 1
        ) %>%
        ungroup() %>%
        mutate(
          q6_level = as.character(q6_level),
          response = as.character(response)
        )
    }
  )
}

make_q6_block_distribution <- function(df, block_map, response_levels, question_family,
                                       q6_var = "Pre_Survey_Q6", q6_levels = q6_levels) {
  purrr::imap_dfr(
    block_map,
    function(var_names, iteration_label) {
      purrr::imap_dfr(
        var_names,
        function(var_name, item_index) {
          item_label <- paste0("Item_", item_index)
          question_text <- get_question_text(main_feature_lookup, var_name)

          df %>%
            transmute(
              q6_level = make_q6_factor(.data[[q6_var]], levels = q6_levels),
              response = normalize_missing_text(.data[[var_name]])
            ) %>%
            filter(!is.na(q6_level)) %>%
            mutate(response = if_else(is.na(response), "Missing", response)) %>%
            mutate(response = factor(response, levels = c(response_levels, "Missing"), ordered = TRUE)) %>%
            count(q6_level, response, name = "n", .drop = FALSE) %>%
            group_by(q6_level) %>%
            mutate(
              iteration = iteration_label,
              item = item_label,
              variable_name = var_name,
              question_family = question_family,
              question_text = question_text,
              n_group_total = sum(n),
              percent = pct(n, n_group_total),
              .before = 1
            ) %>%
            ungroup() %>%
            mutate(
              q6_level = as.character(q6_level),
              response = as.character(response)
            )
        }
      )
    }
  )
}

make_q6_single_distribution <- function(df, var_name, response_levels, question_family,
                                        q6_var = "Pre_Survey_Q6", q6_levels = q6_levels) {
  question_text <- get_question_text(main_feature_lookup, var_name)

  df %>%
    transmute(
      q6_level = make_q6_factor(.data[[q6_var]], levels = q6_levels),
      response = normalize_missing_text(.data[[var_name]])
    ) %>%
    filter(!is.na(q6_level)) %>%
    mutate(response = if_else(is.na(response), "Missing", response)) %>%
    mutate(response = factor(response, levels = c(response_levels, "Missing"), ordered = TRUE)) %>%
    count(q6_level, response, name = "n", .drop = FALSE) %>%
    group_by(q6_level) %>%
    mutate(
      variable_name = var_name,
      question_family = question_family,
      question_text = question_text,
      n_group_total = sum(n),
      percent = pct(n, n_group_total),
      .before = 1
    ) %>%
    ungroup() %>%
    mutate(
      q6_level = as.character(q6_level),
      response = as.character(response)
    )
}

make_q6_level_longitudinal_plot <- function(distribution_table, title_text, subtitle_text = NULL, response_levels = NULL) {
  plot_data <- distribution_table %>%
    filter(response != "Missing") %>%
    mutate(response = as.character(response))

  if (!is.null(response_levels)) {
    plot_data <- plot_data %>%
      mutate(response = factor(response, levels = response_levels, ordered = TRUE))
  }

  ggplot(plot_data, aes(x = iteration, y = percent, fill = response)) +
    geom_col(position = "stack") +
    facet_wrap(~ q6_level) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL,
      y = "Percent within Q6 level",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}

make_q6_level_block_plot <- function(distribution_table, title_text, subtitle_text = NULL, response_levels = NULL) {
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
    facet_grid(q6_level ~ item, labeller = labeller(item = as_labeller(item_labels))) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL,
      y = "Percent within Q6 level",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}

make_q6_level_single_plot <- function(distribution_table, title_text, subtitle_text = NULL, response_levels = NULL) {
  plot_data <- distribution_table %>%
    filter(response != "Missing") %>%
    mutate(response = as.character(response))

  if (!is.null(response_levels)) {
    plot_data <- plot_data %>%
      mutate(response = factor(response, levels = response_levels, ordered = TRUE))
  }

  ggplot(plot_data, aes(x = factor(q6_level, levels = q6_levels, ordered = TRUE), y = percent, fill = response)) +
    geom_col(position = "stack") +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "Pre-Survey Q6 duration of use",
      y = "Percent within Q6 level",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}

# =========================================================
# 4) Tabellen für Q6-Level und Verteilungen              ===
# =========================================================

q6_level_overview <- make_q6_level_overview(analysis_df, q6_var = q6_var, levels = q6_levels)

subtitle_counts <- q6_level_overview %>%
  transmute(label = paste0(q6_level, " (n=", n, ")")) %>%
  pull(label) %>%
  paste(collapse = " | ")

q6_group_1_distribution <- make_q6_iteration_distribution(
  df = analysis_df,
  var_map = c(
    Iteration_1 = "Main_Survey_Q26",
    Iteration_2 = "Main_Survey_Q34",
    Iteration_3 = "Main_Survey_Q42"
  ),
  response_levels = agreement_levels,
  question_family = "Image agreement (overall)",
  q6_var = q6_var,
  q6_levels = q6_levels
)

q6_group_2_distribution <- make_q6_iteration_distribution(
  df = analysis_df,
  var_map = c(
    Iteration_1 = "Main_Survey_Q28",
    Iteration_2 = "Main_Survey_Q36",
    Iteration_3 = "Main_Survey_Q44"
  ),
  response_levels = change_levels,
  question_family = "Change in mental image",
  q6_var = q6_var,
  q6_levels = q6_levels
)

q6_block_distribution <- make_q6_block_distribution(
  df = analysis_df,
  block_map = list(
    Iteration_1 = c("Main_Survey_Q27_1", "Main_Survey_Q27_2", "Main_Survey_Q27_3"),
    Iteration_2 = c("Main_Survey_Q35_1", "Main_Survey_Q35_2", "Main_Survey_Q35_3"),
    Iteration_3 = c("Main_Survey_Q43_1", "Main_Survey_Q43_2", "Main_Survey_Q43_3")
  ),
  response_levels = three_ita_levels,
  question_family = "Image agreement (subscales)",
  q6_var = q6_var,
  q6_levels = q6_levels
)

q6_q52_distribution <- make_q6_single_distribution(
  df = analysis_df,
  var_name = "Main_Survey_Q52",
  response_levels = q52_levels,
  question_family = "Main Survey Q52",
  q6_var = q6_var,
  q6_levels = q6_levels
)

# =========================================================
# 5) Plots erstellen                                      ===
# =========================================================

plot_q6_group_1 <- make_q6_level_longitudinal_plot(
  q6_group_1_distribution,
  title_text = "Image Agreement (overall) across iterations by Pre-Survey Q6 duration of use",
  subtitle_text = subtitle_counts,
  response_levels = agreement_levels
)

plot_q6_group_2 <- make_q6_level_longitudinal_plot(
  q6_group_2_distribution,
  title_text = "Change in Mental Image across iterations by Pre-Survey Q6 duration of use",
  subtitle_text = subtitle_counts,
  response_levels = change_levels
)

plot_q6_block <- make_q6_level_block_plot(
  q6_block_distribution,
  title_text = "Image Agreement (subscales) across iterations by Pre-Survey Q6 duration of use",
  subtitle_text = subtitle_counts,
  response_levels = three_ita_levels
)

plot_q6_q52 <- make_q6_level_single_plot(
  q6_q52_distribution,
  title_text = "Main Survey Q52 by Pre-Survey Q6 duration of use",
  subtitle_text = subtitle_counts,
  response_levels = q52_levels
)

# =========================================================
# 6) Export Tabellen                                      ===
# =========================================================

save_table_outputs(q6_level_overview, "01_q6_level_overview")
save_table_outputs(q6_group_1_distribution, "02_q6_group_1_distribution")
save_table_outputs(q6_group_2_distribution, "03_q6_group_2_distribution")
save_table_outputs(q6_block_distribution, "04_q6_block_distribution")
save_table_outputs(q6_q52_distribution, "05_q6_q52_distribution")

writexl::write_xlsx(
  list(
    q6_level_overview = q6_level_overview,
    q6_group_1_distribution = q6_group_1_distribution,
    q6_group_2_distribution = q6_group_2_distribution,
    q6_block_distribution = q6_block_distribution,
    q6_q52_distribution = q6_q52_distribution
  ),
  path = file.path(out_base_dir, "11_q6_duration_effect_tables.xlsx")
)

# =========================================================
# 7) Export Grafiken                                      ===
# =========================================================

ggsave(
  file.path(out_figures_dir, "Q6Fig1_longitudinal_group_1_by_level.png"),
  plot_q6_group_1,
  width = 11,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "Q6Fig2_longitudinal_group_2_by_level.png"),
  plot_q6_group_2,
  width = 11,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "Q6Fig3_longitudinal_block_by_level.png"),
  plot_q6_block,
  width = 14,
  height = 9,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "Q6Fig4_q52_by_level.png"),
  plot_q6_q52,
  width = 9,
  height = 6,
  dpi = 300
)

# =========================================================
# 8) Konsolen- und Dokumentationsausgabe                 ===
# =========================================================

console_summary <- c(
  "==================== PRE_SURVEY_Q6 DURATION EFFECT PLOTS ====================",
  "",
  "Q6 level overview:",
  capture.output(print(q6_level_overview)),
  "",
  "Exported figures:",
  paste(
    c(
      file.path(out_figures_dir, "Q6Fig1_longitudinal_group_1_by_level.png"),
      file.path(out_figures_dir, "Q6Fig2_longitudinal_group_2_by_level.png"),
      file.path(out_figures_dir, "Q6Fig3_longitudinal_block_by_level.png"),
      file.path(out_figures_dir, "Q6Fig4_q52_by_level.png")
    ),
    collapse = "\n"
  ),
  "",
  "Exported workbook:",
  file.path(out_base_dir, "11_q6_duration_effect_tables.xlsx")
)

writeLines(
  console_summary,
  con = file.path(out_doc_dir, "11_q6_duration_effect_console_summary.txt")
)

# =========================================================
# 9) Lokaler Export-Index                                ===
# =========================================================

export_manifest <- tibble::tibble(
  label = c(
    "Q6 level overview (CSV)",
    "Q6 level overview (XLSX)",
    "Q6 group 1 distribution (CSV)",
    "Q6 group 1 distribution (XLSX)",
    "Q6 group 2 distribution (CSV)",
    "Q6 group 2 distribution (XLSX)",
    "Q6 block distribution (CSV)",
    "Q6 block distribution (XLSX)",
    "Q6 Q52 distribution (CSV)",
    "Q6 Q52 distribution (XLSX)",
    "Combined workbook",
    "Q6Fig1 longitudinal group 1 by level",
    "Q6Fig2 longitudinal group 2 by level",
    "Q6Fig3 longitudinal block by level",
    "Q6Fig4 Q52 by level",
    "Console summary"
  ),
  path = c(
    file.path(out_tables_dir, "01_q6_level_overview.csv"),
    file.path(out_tables_dir, "01_q6_level_overview.xlsx"),
    file.path(out_tables_dir, "02_q6_group_1_distribution.csv"),
    file.path(out_tables_dir, "02_q6_group_1_distribution.xlsx"),
    file.path(out_tables_dir, "03_q6_group_2_distribution.csv"),
    file.path(out_tables_dir, "03_q6_group_2_distribution.xlsx"),
    file.path(out_tables_dir, "04_q6_block_distribution.csv"),
    file.path(out_tables_dir, "04_q6_block_distribution.xlsx"),
    file.path(out_tables_dir, "05_q6_q52_distribution.csv"),
    file.path(out_tables_dir, "05_q6_q52_distribution.xlsx"),
    file.path(out_base_dir, "11_q6_duration_effect_tables.xlsx"),
    file.path(out_figures_dir, "Q6Fig1_longitudinal_group_1_by_level.png"),
    file.path(out_figures_dir, "Q6Fig2_longitudinal_group_2_by_level.png"),
    file.path(out_figures_dir, "Q6Fig3_longitudinal_block_by_level.png"),
    file.path(out_figures_dir, "Q6Fig4_q52_by_level.png"),
    file.path(out_doc_dir, "11_q6_duration_effect_console_summary.txt")
  ),
  notes = c(
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "Kombinierte Excel-Arbeitsmappe",
    "PNG-Grafik",
    "PNG-Grafik",
    "PNG-Grafik",
    "PNG-Grafik",
    "Konsolen- und Prüfzusammenfassung"
  )
)

save_table_outputs(export_manifest, "00_export_manifest", out_dir = out_doc_dir)

build_general_export_index(
  manifest = export_manifest,
  output_path = file.path(out_doc_dir, "00_export_index.html"),
  title_text = "Q6 duration effect plots: Export index",
  intro_text = "Dieser Unterindex bündelt Tabellen, Grafiken und Dokumentation des Skripts 11."
)

message("Confirmation: Pre_Survey_Q6 duration effect plots for the main study were exported successfully.")
message("Figures: ", out_figures_dir)
message("Tables: ", out_tables_dir)
message("Workbook: ", file.path(out_base_dir, "11_q6_duration_effect_tables.xlsx"))
message("Console summary: ", file.path(out_doc_dir, "11_q6_duration_effect_console_summary.txt"))

#####################################################################
### End of workflow                                               ###
#####################################################################

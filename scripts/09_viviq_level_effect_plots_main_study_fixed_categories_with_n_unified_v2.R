#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.

#####################################################################
### VIVIQ-Level-Effekte auf Main-Study-Ergebnisvariablen          ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript erzeugt zusätzliche Ergebnisplots für die Main Study,
# bei denen die Verteilungen zentraler Outcome-Variablen nach dem Level
# des viviq_total_score dargestellt werden.
#
# Ziel:
# - Image Agreement (overall) über Iteration 1–3
# - Image Agreement (subscales) über Iteration 1–3
# - Change in Mental Image über Iteration 1–3
# - Main_Survey_Q52 nach VIVIQ-Level
#
# Die Darstellung orientiert sich an den vorhandenen ReqFig-Plots aus
# 05_deskriptive_statistik_und_reporting_final_konsolidiert_erweitert.R,
# ergänzt diese jedoch um eine Stratifizierung nach VIVIQ-Level.
#
# Kategorisierung in diesem Skript:
# - aphantasia: VIVIQ total score = 16
# - hypophantasia: VIVIQ total score = 17-32
# - typical imagery ability: VIVIQ total score = 33-74
# - hyperphantasia: VIVIQ total score = 75-80
# - Die Gruppierung erfolgt nur unter Fällen mit gültigem viviq_total_score
# - Fälle mit fehlendem viviq_total_score werden aus diesen Effektplots
#   ausgeschlossen

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

out_base_dir    <- file.path(project_root, "data_output", "main_study_viviq_level_effects")
out_tables_dir  <- file.path(out_base_dir, "tables")
out_figures_dir <- file.path(out_base_dir, "figures")
out_doc_dir     <- file.path(out_base_dir, "documentation")
out_gt_dir      <- file.path(out_base_dir, "gt_tables")
out_gt_html_dir <- file.path(out_gt_dir, "html")
out_gt_rtf_dir  <- file.path(out_gt_dir, "rtf")
out_gt_doc_dir  <- file.path(out_gt_dir, "documentation")

purrr::walk(
  c(out_base_dir, out_tables_dir, out_figures_dir, out_doc_dir, out_gt_dir, out_gt_html_dir, out_gt_rtf_dir, out_gt_doc_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# =========================================================
# 2) Benötigte Objekte prüfen                            ===
# =========================================================

# final_analysis_dataset, pre_feature_lookup und main_feature_lookup
# werden im Final-Dataset-Only Bootstrap oben erzeugt. Es werden keine
# Reporting-, Cleaning-, Matching- oder VIVIQ-Skripte geladen.

# Fallbacks, falls das geladene Reporting-Skript einzelne Hilfsobjekte
# in einer älteren Version nicht bereitstellt
if (!exists("theme_result", envir = .GlobalEnv, inherits = FALSE)) {
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
}

agreement_levels <- if (exists("agreement_levels", inherits = FALSE)) {
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

change_levels <- if (exists("change_levels", inherits = FALSE)) {
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

three_ita_levels <- if (exists("three_ita_levels", inherits = FALSE)) {
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

# Laut Variablenübersicht ist Q52 auf derselben 1-7-Skala kodiert wie Q26/Q34/Q42.
q52_levels <- agreement_levels

analysis_df <- final_analysis_dataset

# =========================================================
# 3) Hilfsfunktionen                                     ===
# =========================================================

normalize_missing_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_
  x
}

safe_numeric <- function(x) {
  readr::parse_number(as.character(x), na = c("", "NA", "N/A", "na", "n/a"))
}

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
  ifelse(is.na(base) | base == 0, NA_real_, round(100 * x / base, 1))
}

make_n_label <- function(n) {
  paste0("N = ", n)
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

make_viviq_level <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  labels <- c(
    "Aphantasia",
    "Hypophantasia",
    "Typical imagery ability",
    "Hyperphantasia"
  )

  out <- dplyr::case_when(
    is.na(x_num) ~ NA_character_,
    x_num == 16 ~ "Aphantasia",
    dplyr::between(x_num, 17, 32) ~ "Hypophantasia",
    dplyr::between(x_num, 33, 74) ~ "Typical imagery ability",
    dplyr::between(x_num, 75, 80) ~ "Hyperphantasia",
    TRUE ~ NA_character_
  )

  factor(out, levels = labels, ordered = TRUE)
}

make_viviq_level_overview <- function(df, viviq_var = "viviq_total_score") {
  df %>%
    transmute(
      viviq_total_score = suppressWarnings(as.numeric(.data[[viviq_var]])),
      viviq_level = make_viviq_level(.data[[viviq_var]])
    ) %>%
    filter(!is.na(viviq_total_score), !is.na(viviq_level)) %>%
    group_by(viviq_level) %>%
    summarise(
      n = n(),
      n_label = make_n_label(n),
      mean_viviq_total_score = safe_mean(viviq_total_score),
      sd_viviq_total_score = safe_sd(viviq_total_score),
      median_viviq_total_score = safe_median(viviq_total_score),
      min_viviq_total_score = safe_min(viviq_total_score),
      max_viviq_total_score = safe_max(viviq_total_score),
      .groups = "drop"
    )
}

make_viviq_iteration_distribution <- function(df, var_map, response_levels, question_family, viviq_var = "viviq_total_score") {
  purrr::imap_dfr(
    var_map,
    function(var_name, iteration_label) {
      question_text <- get_question_text(main_feature_lookup, var_name)

      df %>%
        transmute(
          viviq_total_score = suppressWarnings(as.numeric(.data[[viviq_var]])),
          viviq_level = make_viviq_level(.data[[viviq_var]]),
          response = normalize_missing_text(.data[[var_name]])
        ) %>%
        filter(!is.na(viviq_total_score), !is.na(viviq_level)) %>%
        mutate(response = if_else(is.na(response), "Missing", response)) %>%
        mutate(response = factor(response, levels = c(response_levels, "Missing"), ordered = TRUE)) %>%
        count(viviq_level, response, name = "n", .drop = FALSE) %>%
        group_by(viviq_level) %>%
        mutate(n_group_total = sum(n)) %>%
        ungroup() %>%
        mutate(
          iteration = iteration_label,
          variable_name = var_name,
          question_family = question_family,
          question_text = question_text,
          n_group_label = make_n_label(n_group_total),
          n_used_total = sum(n),
          n_used_total_label = make_n_label(n_used_total),
          percent = pct(n, n_group_total),
          .before = 1
        ) %>%
        mutate(response = as.character(response))
    }
  )
}

make_viviq_block_distribution <- function(df, block_map, response_levels, question_family, viviq_var = "viviq_total_score") {
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
              viviq_total_score = suppressWarnings(as.numeric(.data[[viviq_var]])),
              viviq_level = make_viviq_level(.data[[viviq_var]]),
              response = normalize_missing_text(.data[[var_name]])
            ) %>%
            filter(!is.na(viviq_total_score), !is.na(viviq_level)) %>%
            mutate(response = if_else(is.na(response), "Missing", response)) %>%
            mutate(response = factor(response, levels = c(response_levels, "Missing"), ordered = TRUE)) %>%
            count(viviq_level, response, name = "n", .drop = FALSE) %>%
            group_by(viviq_level) %>%
            mutate(n_group_total = sum(n)) %>%
            ungroup() %>%
            mutate(
              iteration = iteration_label,
              item = item_label,
              variable_name = var_name,
              question_family = question_family,
              question_text = question_text,
              n_group_label = make_n_label(n_group_total),
              n_used_total = sum(n),
              n_used_total_label = make_n_label(n_used_total),
              percent = pct(n, n_group_total),
              .before = 1
            ) %>%
            mutate(response = as.character(response))
        }
      )
    }
  )
}

make_viviq_single_distribution <- function(df, var_name, response_levels, question_family, viviq_var = "viviq_total_score") {
  question_text <- get_question_text(main_feature_lookup, var_name)

  df %>%
    transmute(
      viviq_total_score = suppressWarnings(as.numeric(.data[[viviq_var]])),
      viviq_level = make_viviq_level(.data[[viviq_var]]),
      response = normalize_missing_text(.data[[var_name]])
    ) %>%
    filter(!is.na(viviq_total_score), !is.na(viviq_level)) %>%
    mutate(response = if_else(is.na(response), "Missing", response)) %>%
    mutate(response = factor(response, levels = c(response_levels, "Missing"), ordered = TRUE)) %>%
    count(viviq_level, response, name = "n", .drop = FALSE) %>%
    group_by(viviq_level) %>%
    mutate(n_group_total = sum(n)) %>%
    ungroup() %>%
    mutate(
      variable_name = var_name,
      question_family = question_family,
      question_text = question_text,
      n_group_label = make_n_label(n_group_total),
      n_used_total = sum(n),
      n_used_total_label = make_n_label(n_used_total),
      percent = pct(n, n_group_total),
      .before = 1
    ) %>%
    mutate(response = as.character(response))
}

make_viviq_facet_labels <- function(distribution_table) {
  distribution_table %>%
    distinct(viviq_level, n_group_total) %>%
    mutate(label = paste0(as.character(viviq_level), "\n", make_n_label(n_group_total))) %>%
    { stats::setNames(.$label, .$viviq_level) }
}

make_viviq_axis_labels <- function(distribution_table) {
  distribution_table %>%
    distinct(viviq_level, n_group_total) %>%
    mutate(label = paste0(as.character(viviq_level), "\n", make_n_label(n_group_total))) %>%
    { stats::setNames(.$label, .$viviq_level) }
}

make_plot_subtitle <- function(distribution_table, cutpoints_text) {
  n_used_total <- distribution_table %>%
    dplyr::pull(n_used_total) %>%
    unique()

  n_used_total <- n_used_total[!is.na(n_used_total)]
  n_used_text <- if (length(n_used_total) == 0) NA_character_ else make_n_label(n_used_total[1])

  bits <- c(cutpoints_text, if (!is.na(n_used_text)) paste0("Total used cases: ", n_used_text) else NA_character_)
  bits <- bits[!is.na(bits) & bits != ""]
  paste(bits, collapse = " | ")
}

make_viviq_level_longitudinal_plot <- function(distribution_table, title_text, subtitle_text = NULL, response_levels = NULL) {
  plot_data <- distribution_table %>%
    filter(response != "Missing") %>%
    mutate(response = as.character(response))

  if (!is.null(response_levels)) {
    plot_data <- plot_data %>%
      mutate(response = factor(response, levels = response_levels, ordered = TRUE))
  }

  facet_labels <- make_viviq_facet_labels(distribution_table)

  ggplot(plot_data, aes(x = iteration, y = percent, fill = response)) +
    geom_col(position = "stack") +
    facet_wrap(~ viviq_level, labeller = labeller(viviq_level = as_labeller(facet_labels))) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL,
      y = "Percent within VIVIQ level",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}

make_viviq_level_block_plot <- function(distribution_table, title_text, subtitle_text = NULL, response_levels = NULL) {
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

  facet_labels <- make_viviq_facet_labels(distribution_table)

  ggplot(plot_data, aes(x = iteration, y = percent, fill = response)) +
    geom_col(position = "stack") +
    facet_grid(viviq_level ~ item, labeller = labeller(viviq_level = as_labeller(facet_labels), item = as_labeller(item_labels))) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL,
      y = "Percent within VIVIQ level",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}

make_viviq_level_single_plot <- function(distribution_table, title_text, subtitle_text = NULL, response_levels = NULL) {
  plot_data <- distribution_table %>%
    filter(response != "Missing") %>%
    mutate(response = as.character(response))

  if (!is.null(response_levels)) {
    plot_data <- plot_data %>%
      mutate(response = factor(response, levels = response_levels, ordered = TRUE))
  }

  axis_labels <- make_viviq_axis_labels(distribution_table)

  ggplot(plot_data, aes(x = viviq_level, y = percent, fill = response)) +
    geom_col(position = "stack") +
    scale_x_discrete(labels = axis_labels) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "VIVIQ total score level",
      y = "Percent within VIVIQ level",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}

# =========================================================
# 4) Tabellen für VIVIQ-Level und Verteilungen           ===
# =========================================================

viviq_level_overview <- make_viviq_level_overview(analysis_df)

subtitle_cutpoints <- paste(
  c(
    "Aphantasia: 16",
    "Hypophantasia: 17–32",
    "Typical imagery ability: 33–74",
    "Hyperphantasia: 75–80"
  ),
  collapse = " | "
)

viviq_group_1_distribution <- make_viviq_iteration_distribution(
  df = analysis_df,
  var_map = c(
    Iteration_1 = "Main_Survey_Q26",
    Iteration_2 = "Main_Survey_Q34",
    Iteration_3 = "Main_Survey_Q42"
  ),
  response_levels = agreement_levels,
  question_family = "Image agreement (overall)"
)

viviq_group_2_distribution <- make_viviq_iteration_distribution(
  df = analysis_df,
  var_map = c(
    Iteration_1 = "Main_Survey_Q28",
    Iteration_2 = "Main_Survey_Q36",
    Iteration_3 = "Main_Survey_Q44"
  ),
  response_levels = change_levels,
  question_family = "Change in mental image"
)

viviq_block_distribution <- make_viviq_block_distribution(
  df = analysis_df,
  block_map = list(
    Iteration_1 = c("Main_Survey_Q27_1", "Main_Survey_Q27_2", "Main_Survey_Q27_3"),
    Iteration_2 = c("Main_Survey_Q35_1", "Main_Survey_Q35_2", "Main_Survey_Q35_3"),
    Iteration_3 = c("Main_Survey_Q43_1", "Main_Survey_Q43_2", "Main_Survey_Q43_3")
  ),
  response_levels = three_ita_levels,
  question_family = "Image agreement (subscales)"
)

viviq_q52_distribution <- make_viviq_single_distribution(
  df = analysis_df,
  var_name = "Main_Survey_Q52",
  response_levels = q52_levels,
  question_family = "Main Survey Q52"
)

viviq_analysis_n_overview <- bind_rows(
  viviq_group_1_distribution %>%
    distinct(question_family, variable_name, iteration, n_used_total, n_used_total_label) %>%
    mutate(item = NA_character_),
  viviq_group_2_distribution %>%
    distinct(question_family, variable_name, iteration, n_used_total, n_used_total_label) %>%
    mutate(item = NA_character_),
  viviq_block_distribution %>%
    distinct(question_family, variable_name, iteration, item, n_used_total, n_used_total_label),
  viviq_q52_distribution %>%
    distinct(question_family, variable_name, n_used_total, n_used_total_label) %>%
    mutate(iteration = NA_character_, item = NA_character_)
) %>%
  arrange(question_family, iteration, item, variable_name)

# =========================================================
# 5) Plots erstellen                                      ===
# =========================================================

plot_viviq_group_1 <- make_viviq_level_longitudinal_plot(
  viviq_group_1_distribution,
  title_text = "Image Agreement (overall) across iterations by VIVIQ total score level",
  subtitle_text = make_plot_subtitle(viviq_group_1_distribution, subtitle_cutpoints),
  response_levels = agreement_levels
)

plot_viviq_group_2 <- make_viviq_level_longitudinal_plot(
  viviq_group_2_distribution,
  title_text = "Change in Mental Image across iterations by VIVIQ total score level",
  subtitle_text = make_plot_subtitle(viviq_group_2_distribution, subtitle_cutpoints),
  response_levels = change_levels
)

plot_viviq_block <- make_viviq_level_block_plot(
  viviq_block_distribution,
  title_text = "Image Agreement (subscales) across iterations by VIVIQ total score level",
  subtitle_text = make_plot_subtitle(viviq_block_distribution, subtitle_cutpoints),
  response_levels = three_ita_levels
)

plot_viviq_q52 <- make_viviq_level_single_plot(
  viviq_q52_distribution,
  title_text = "Main Survey Q52 by VIVIQ total score level",
  subtitle_text = make_plot_subtitle(viviq_q52_distribution, subtitle_cutpoints),
  response_levels = q52_levels
)

# =========================================================
# 6) Export Tabellen                                      ===
# =========================================================

save_table_outputs(viviq_level_overview, "01_viviq_level_overview")
save_table_outputs(viviq_analysis_n_overview, "02_viviq_analysis_n_overview")
save_table_outputs(viviq_group_1_distribution, "03_viviq_group_1_distribution")
save_table_outputs(viviq_group_2_distribution, "04_viviq_group_2_distribution")
save_table_outputs(viviq_block_distribution, "05_viviq_block_distribution")
save_table_outputs(viviq_q52_distribution, "06_viviq_q52_distribution")

viviq_tables_for_research <- list(
  "01_viviq_level_overview" = viviq_level_overview,
  "02_viviq_analysis_n_overview" = viviq_analysis_n_overview,
  "03_viviq_group_1_distribution" = viviq_group_1_distribution,
  "04_viviq_group_2_distribution" = viviq_group_2_distribution,
  "05_viviq_block_distribution" = viviq_block_distribution,
  "06_viviq_q52_distribution" = viviq_q52_distribution
)

writexl::write_xlsx(
  viviq_tables_for_research,
  path = file.path(out_base_dir, "09_viviq_level_effect_tables.xlsx")
)

gt_manifest_09 <- save_table_collection_as_gt(
  table_list = viviq_tables_for_research,
  out_gt_html_dir = out_gt_html_dir,
  out_gt_rtf_dir = out_gt_rtf_dir,
  out_gt_doc_dir = out_gt_doc_dir,
  manifest_base_filename = "09_viviq_level_effect_gt_manifest",
  index_title = "VIVIQ-level effect tables - GT outputs",
  index_intro = "This index links to publication-oriented HTML and RTF versions of all VIVIQ-level effect tables.",
  source_note = "Generated from data_final/final_analysis_dataset_anonymized."
)

# =========================================================
# 7) Export Grafiken                                      ===
# =========================================================

ggsave(
  file.path(out_figures_dir, "VIVIQFig1_longitudinal_group_1_by_level.png"),
  plot_viviq_group_1,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "VIVIQFig2_longitudinal_group_2_by_level.png"),
  plot_viviq_group_2,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "VIVIQFig3_longitudinal_block_by_level.png"),
  plot_viviq_block,
  width = 13,
  height = 8,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "VIVIQFig4_q52_by_level.png"),
  plot_viviq_q52,
  width = 8,
  height = 6,
  dpi = 300
)

# =========================================================
# 8) Konsolen- und Dokumentationsausgabe                 ===
# =========================================================

console_summary <- c(
  "==================== VIVIQ LEVEL EFFECT PLOTS ====================",
  "",
  "VIVIQ level overview:",
  capture.output(print(viviq_level_overview)),
  "",
  "Analysis N overview:",
  capture.output(print(viviq_analysis_n_overview)),
  "",
  "Exported figures:",
  paste(
    c(
      file.path(out_figures_dir, "VIVIQFig1_longitudinal_group_1_by_level.png"),
      file.path(out_figures_dir, "VIVIQFig2_longitudinal_group_2_by_level.png"),
      file.path(out_figures_dir, "VIVIQFig3_longitudinal_block_by_level.png"),
      file.path(out_figures_dir, "VIVIQFig4_q52_by_level.png")
    ),
    collapse = "\n"
  ),
  "",
  "Exported workbook:",
  file.path(out_base_dir, "09_viviq_level_effect_tables.xlsx")
)

writeLines(
  console_summary,
  con = file.path(out_doc_dir, "09_viviq_level_effect_console_summary.txt")
)

# =========================================================
# 9) Lokaler Export-Index                                  ===
# =========================================================

export_manifest <- tibble(
  label = c(
    "VIVIQ level overview (CSV)",
    "VIVIQ level overview (XLSX)",
    "Analysis N overview (CSV)",
    "Analysis N overview (XLSX)",
    "Group 1 distribution (CSV)",
    "Group 1 distribution (XLSX)",
    "Group 2 distribution (CSV)",
    "Group 2 distribution (XLSX)",
    "Block distribution (CSV)",
    "Block distribution (XLSX)",
    "Q52 distribution (CSV)",
    "Q52 distribution (XLSX)",
    "Combined workbook",
    "Figure: VIVIQFig1",
    "Figure: VIVIQFig2",
    "Figure: VIVIQFig3",
    "Figure: VIVIQFig4",
    "Console summary"
  ),
  path = c(
    file.path(out_tables_dir, "01_viviq_level_overview.csv"),
    file.path(out_tables_dir, "01_viviq_level_overview.xlsx"),
    file.path(out_tables_dir, "02_viviq_analysis_n_overview.csv"),
    file.path(out_tables_dir, "02_viviq_analysis_n_overview.xlsx"),
    file.path(out_tables_dir, "03_viviq_group_1_distribution.csv"),
    file.path(out_tables_dir, "03_viviq_group_1_distribution.xlsx"),
    file.path(out_tables_dir, "04_viviq_group_2_distribution.csv"),
    file.path(out_tables_dir, "04_viviq_group_2_distribution.xlsx"),
    file.path(out_tables_dir, "05_viviq_block_distribution.csv"),
    file.path(out_tables_dir, "05_viviq_block_distribution.xlsx"),
    file.path(out_tables_dir, "06_viviq_q52_distribution.csv"),
    file.path(out_tables_dir, "06_viviq_q52_distribution.xlsx"),
    file.path(out_base_dir, "09_viviq_level_effect_tables.xlsx"),
    file.path(out_figures_dir, "VIVIQFig1_longitudinal_group_1_by_level.png"),
    file.path(out_figures_dir, "VIVIQFig2_longitudinal_group_2_by_level.png"),
    file.path(out_figures_dir, "VIVIQFig3_longitudinal_block_by_level.png"),
    file.path(out_figures_dir, "VIVIQFig4_q52_by_level.png"),
    file.path(out_doc_dir, "09_viviq_level_effect_console_summary.txt")
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
  title_text = "VIVIQ level effect plots: Export index",
  intro_text = "Dieser Unterindex bündelt Tabellen, Grafiken und Dokumentation des Skripts 09."
)

message("Confirmation: VIVIQ-level effect plots for the main study were exported successfully.")
message("Figures: ", out_figures_dir)
message("Tables: ", out_tables_dir)
message("Workbook: ", file.path(out_base_dir, "09_viviq_level_effect_tables.xlsx"))
message("Console summary: ", file.path(out_doc_dir, "09_viviq_level_effect_console_summary.txt"))

#####################################################################
### End of workflow                                               ###
#####################################################################

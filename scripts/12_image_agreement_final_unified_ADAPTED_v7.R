#####################################################################
### IMAGE AGREEMENT ANALYSIS - FINAL UNIFIED                      ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript führt die finale Image-Agreement-Analyse aus.
#
# Inhalt:
# - Verwendung des bereits konsolidierten Analyse-Datensatzes
# - Recodierung von Pre_Survey_Q6 zu q6_duration_num
# - Recodierung von Pre_Survey_Q7 zu GenAI-Nutzungsfrequenz
# - Long-Format für Agreement über drei Bildgenerationsrunden
# - Deskriptive Analyse der Agreement-Werte pro Runde
# - Primärmodell: Round als Faktor
# - Kontrollmodell: Round + Q6 + Q7 + VIVIQ + Zielwortkategorie
# - Ordinales Robustheitsmodell
# - Change-Score-Analyse von Runde 1 bis Runde 3
# - Zusätzliche Mittelwertplots pro Zielwortkategorie
# - Zusätzliche Mittelwertplots pro Image-Agreement-Subskala und Zielwortkategorie
# - Zusätzliche Mittelwertplots für Change in mental image pro Zielwortkategorie
# - Ordinale Darstellungsvarianten: gestapelte Verteilungen, Median-IQR-Plots mit IQR-Remark, Violin-/Boxplots und kumulative Verteilungen
# - ML-Modellvergleiche
# - Exporte als CSV, XLSX, TXT, PNG und HTML-Index
#
# Nicht enthalten:
# - kein erneuter Import der Rohdaten
# - kein erneutes Data Cleaning
# - kein erneutes Matching
# - kein Prompt-Coding-Join
# - keine zusätzlichen Q52- oder Prompt-Sequence-Analysen

# =========================================================
# 0) Pakete                                              ===
# =========================================================

# install.packages(c(
#   "tidyverse", "lme4", "lmerTest", "broom.mixed", "writexl",
#   "here", "readr", "emmeans", "ordinal"
# ), dependencies = TRUE)

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(writexl)
library(here)
library(readr)
library(emmeans)
library(ordinal)

# =========================================================
# 1) Pfade und zentrale Helper                           ===
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

out_base_dir    <- file.path(project_root, "data_output", "image_agreement_lmm_analysis")
out_tables_dir  <- file.path(out_base_dir, "tables")
out_figures_dir <- file.path(out_base_dir, "figures")
out_doc_dir     <- file.path(out_base_dir, "documentation")

ensure_directories(c(out_base_dir, out_tables_dir, out_figures_dir, out_doc_dir))

# =========================================================
# 2) Benötigten konsolidierten Datensatz bereitstellen    ===
# =========================================================

final_analysis_dataset <- read_required_rds(
  file.path(project_root, "data_final", "final_analysis_dataset_anonymized.rds"),
  "Final matched anonymized analysis dataset"
)

message("Confirmation: Script 12 uses data_final/final_analysis_dataset_anonymized.rds.")

analysis_source_name <- "final_analysis_dataset"
analysis_raw <- final_analysis_dataset

# =========================================================
# 3) Benötigte Variablen prüfen                          ===
# =========================================================

required_vars <- c(
  "participant_id",
  "Pre_Survey_Q6",
  "Pre_Survey_Q7",
  "viviq_total_score",
  "Main_Survey_target_word_category",
  "Main_Survey_Q26_score",
  "Main_Survey_Q34_score",
  "Main_Survey_Q42_score"
)

missing_vars <- setdiff(required_vars, names(analysis_raw))

if (length(missing_vars) > 0) {
  stop(
    paste0(
      "The following required variables are missing in ", analysis_source_name, ":\n",
      paste(missing_vars, collapse = ", ")
    ),
    call. = FALSE
  )
}

# =========================================================
# 4) Hilfsfunktionen                                     ===
# =========================================================

normalize_q6_text <- function(x) {
  x <- normalize_missing_text(x)
  x <- stringr::str_replace_all(x, "â€“|â€”|Ð|–|—|−", "-")
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_squish(x)
}

recode_q6_duration <- function(x) {
  x_norm <- normalize_q6_text(x)
  x_lower <- stringr::str_to_lower(x_norm)

  dplyr::case_when(
    x_lower == "less than 1 month" ~ 1,
    stringr::str_detect(x_lower, "^1\\s*-\\s*6\\s*months$") ~ 2,
    stringr::str_detect(x_lower, "^7\\s*-\\s*12\\s*months$") ~ 3,
    stringr::str_detect(x_lower, "^1\\s*-\\s*2\\s*years$") ~ 4,
    x_lower == "more than 2 years" ~ 5,
    TRUE ~ NA_real_
  )
}

normalize_q7_text <- function(x) {
  x <- normalize_missing_text(x)
  x <- stringr::str_replace_all(x, "â€“|â€”|Ð|–|—|−", "-")
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_squish(x)
}

recode_q7_genai_frequency <- function(x) {
  x_norm <- normalize_q7_text(x)
  x_lower <- stringr::str_to_lower(x_norm)

  dplyr::case_when(
    stringr::str_detect(x_lower, "^1$|\\(1\\)|daily") ~ "Daily",
    stringr::str_detect(x_lower, "^2$|\\(2\\)|several times per week") ~ "Several times per week",
    stringr::str_detect(x_lower, "^3$|\\(3\\)|weekly") ~ "Weekly",
    stringr::str_detect(x_lower, "^4$|\\(4\\)|monthly") ~ "Monthly",
    stringr::str_detect(x_lower, "^5$|\\(5\\)|less often") ~ "Less often",
    stringr::str_detect(x_lower, "^6$|\\(6\\)|no longer") ~ "No longer use it",
    TRUE ~ NA_character_
  )
}

normalize_target_word_category_analysis <- function(x) {
  x <- normalize_missing_text(x)
  x <- stringr::str_squish(x)
  x_lower <- stringr::str_to_lower(x)

  out <- dplyr::case_when(
    is.na(x_lower) ~ NA_character_,
    x_lower %in% c("abstract", "abstrakt") ~ "Abstract",
    x_lower %in% c("concrete", "konkret") ~ "Concrete",
    TRUE ~ stringr::str_to_title(x)
  )

  out
}

get_lmm_fixed_table <- function(model, model_label) {
  broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE,
    conf.method = "Wald"
  ) %>%
    mutate(model = model_label, .before = 1)
}

get_lmm_nobs <- function(lmm_model) {
  nrow(stats::model.frame(lmm_model))
}

get_lmm_fit_table <- function(lmm_model, model_label) {
  n_observations_value <- get_lmm_nobs(lmm_model)

  broom.mixed::glance(lmm_model) %>%
    as_tibble() %>%
    mutate(
      model = model_label,
      n_observations = n_observations_value,
      .before = 1
    )
}

get_lmm_icc_table <- function(lmm_model, model_label) {
  var_table <- as.data.frame(lme4::VarCorr(lmm_model))

  participant_variance <- var_table$vcov[var_table$grp == "participant_id"][1]
  residual_variance <- var_table$vcov[var_table$grp == "Residual"][1]

  model_data <- stats::model.frame(lmm_model)

  tibble(
    model = model_label,
    participant_variance = participant_variance,
    residual_variance = residual_variance,
    icc = participant_variance / (participant_variance + residual_variance),
    n_observations = nrow(model_data),
    n_participants = dplyr::n_distinct(model_data$participant_id)
  )
}

format_table_for_txt <- function(label, df) {
  c(
    "",
    "",
    paste0("==================== ", label, " ===================="),
    capture.output(print(tibble::as_tibble(df), n = Inf, width = Inf))
  )
}


safe_mode_label <- function(x, digits = 2) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_character_)
  }

  mode_table <- table(x)
  mode_values <- names(mode_table)[mode_table == max(mode_table)]

  mode_numeric <- suppressWarnings(as.numeric(mode_values))

  if (all(!is.na(mode_numeric))) {
    mode_values <- format(round(mode_numeric, digits), nsmall = digits, trim = TRUE)
  }

  paste(mode_values, collapse = " / ")
}

format_plot_number <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x)) {
    return("NA")
  }

  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE)
}

format_plot_text <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(as.character(x))) {
    return("NA")
  }

  as.character(x)
}

make_round_statistics_label_data <- function(
    descriptives,
    mean_col,
    median_col,
    mode_col,
    se_col = NULL,
    facet_col = NULL,
    n_col = "n",
    digits = 2
) {
  required_cols <- c("round", "round_factor", mean_col, median_col, mode_col, n_col)

  if (!is.null(se_col)) {
    required_cols <- c(required_cols, se_col)
  }

  if (!is.null(facet_col)) {
    required_cols <- c(required_cols, facet_col)
  }

  missing_cols <- setdiff(required_cols, names(descriptives))

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "The statistics annotation cannot be created because these columns are missing: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  lower_values <- descriptives[[mean_col]]
  upper_values <- descriptives[[mean_col]]

  if (!is.null(se_col)) {
    se_values <- descriptives[[se_col]]
    se_values[is.na(se_values)] <- 0
    lower_values <- lower_values - se_values
    upper_values <- upper_values + se_values
  }

  if (all(is.na(lower_values)) || all(is.na(upper_values))) {
    y_min <- 0
    y_max <- 1
  } else {
    y_min <- min(lower_values, na.rm = TRUE)
    y_max <- max(upper_values, na.rm = TRUE)
  }

  y_span <- y_max - y_min

  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
  }

  label_y <- y_min - 0.22 * y_span
  y_lower_limit <- y_min - 0.50 * y_span

  label_source <- descriptives %>%
    arrange(round) %>%
    mutate(
      statistics_line = paste0(
        as.character(round_factor),
        ": Median = ", purrr::map_chr(.data[[median_col]], format_plot_number, digits = digits),
        "; Mode = ", purrr::map_chr(.data[[mode_col]], format_plot_text),
        "; Mean = ", purrr::map_chr(.data[[mean_col]], format_plot_number, digits = digits),
        "; n = ", .data[[n_col]]
      )
    )

  if (is.null(facet_col)) {
    label_source %>%
      summarise(
        x = 2,
        y = label_y,
        y_lower_limit = y_lower_limit,
        statistics_label = paste(statistics_line, collapse = "\n"),
        .groups = "drop"
      )
  } else {
    label_source %>%
      group_by(across(all_of(facet_col))) %>%
      summarise(
        x = 2,
        y = label_y,
        y_lower_limit = y_lower_limit,
        statistics_label = paste(statistics_line, collapse = "\n"),
        .groups = "drop"
      )
  }
}

add_round_statistics_annotation <- function(
    plot_object,
    descriptives,
    mean_col,
    median_col,
    mode_col,
    se_col = NULL,
    facet_col = NULL,
    n_col = "n",
    digits = 2,
    text_size = 3
) {
  statistics_label_data <- make_round_statistics_label_data(
    descriptives = descriptives,
    mean_col = mean_col,
    median_col = median_col,
    mode_col = mode_col,
    se_col = se_col,
    facet_col = facet_col,
    n_col = n_col,
    digits = digits
  )

  plot_object +
    geom_text(
      data = statistics_label_data,
      aes(x = x, y = y, label = statistics_label),
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = 1,
      lineheight = 0.95,
      size = text_size
    ) +
    expand_limits(y = min(statistics_label_data$y_lower_limit, na.rm = TRUE)) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 25, l = 10)
    )
}

clean_filename_component <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "") %>%
    stringr::str_to_lower()
}

validate_variable_specs <- function(variable_specs, data, spec_label) {
  required_spec_cols <- c("round", "round_var")
  missing_spec_cols <- setdiff(required_spec_cols, names(variable_specs))

  if (length(missing_spec_cols) > 0) {
    stop(
      paste0(
        spec_label,
        " is missing these required columns: ",
        paste(missing_spec_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  missing_data_vars <- setdiff(unique(variable_specs$round_var), names(data))

  if (length(missing_data_vars) > 0) {
    stop(
      paste0(
        "The following variables listed in ",
        spec_label,
        " are missing in ",
        analysis_source_name,
        ":\n",
        paste(missing_data_vars, collapse = ", "),
        "\n\nPlease update the variable-mapping block in section 4b."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

make_round_long_from_specs <- function(data, variable_specs, value_name = "score") {
  variable_specs_clean <- variable_specs %>%
    mutate(
      round = as.numeric(round),
      round_var = as.character(round_var)
    )

  round_vars <- unique(variable_specs_clean$round_var)

  data %>%
    select(
      participant_id,
      q6_duration_num,
      q7_genai_frequency,
      q7_genai_frequency_factor,
      q7_genai_frequency_model_group,
      viviq_total_score,
      Main_Survey_target_word_category,
      target_word_category_analysis,
      all_of(round_vars)
    ) %>%
    pivot_longer(
      cols = all_of(round_vars),
      names_to = "round_var",
      values_to = "score"
    ) %>%
    left_join(variable_specs_clean, by = "round_var") %>%
    mutate(
      round_factor = factor(
        round,
        levels = c(1, 2, 3),
        labels = c("Round 1", "Round 2", "Round 3")
      ),
      score = as.numeric(score),
      q6_duration_num = as.numeric(q6_duration_num),
      q6_duration_num_c = q6_duration_num - mean(q6_duration_num, na.rm = TRUE),
      viviq_total_score = as.numeric(viviq_total_score),
      viviq_total_z = as.numeric(scale(viviq_total_score)),
      target_word_category_analysis = factor(
        target_word_category_analysis,
        levels = target_word_category_levels,
        ordered = FALSE
      )
    ) %>%
    filter(
      !is.na(participant_id),
      !is.na(round),
      !is.na(score)
    )
}

summarise_round_target_category <- function(data, extra_group_vars = character()) {
  grouping_vars <- c(
    extra_group_vars,
    "target_word_category_analysis",
    "round",
    "round_factor"
  )

  data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n = n(),
      n_participants = n_distinct(participant_id),
      mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      se_score = sd_score / sqrt(n),
      median_score = median(score, na.rm = TRUE),
      mode_score = safe_mode_label(score),
      q1_score = as.numeric(stats::quantile(score, probs = 0.25, na.rm = TRUE, names = FALSE)),
      q3_score = as.numeric(stats::quantile(score, probs = 0.75, na.rm = TRUE, names = FALSE)),
      min_score = min(score, na.rm = TRUE),
      max_score = max(score, na.rm = TRUE),
      .groups = "drop"
    )
}

make_target_category_round_plot <- function(
    descriptives,
    title_text,
    y_label,
    mean_col = "mean_score",
    median_col = "median_score",
    mode_col = "mode_score",
    se_col = "se_score"
) {
  base_plot <- ggplot(
    descriptives,
    aes(
      x = round,
      y = .data[[mean_col]],
      group = 1
    )
  ) +
    geom_line() +
    geom_point(size = 3) +
    geom_errorbar(
      aes(
        ymin = .data[[mean_col]] - .data[[se_col]],
        ymax = .data[[mean_col]] + .data[[se_col]]
      ),
      width = 0.1
    ) +
    facet_wrap(
      vars(target_word_category_analysis),
      nrow = 1,
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(1, 2, 3),
      labels = c("Round 1", "Round 2", "Round 3")
    ) +
    scale_y_continuous(breaks = ordinal_response_levels) +
    labs(
      title = title_text,
      x = "Image-generation round",
      y = y_label
    ) +
    plot_theme

  add_round_statistics_annotation(
    plot_object = base_plot,
    descriptives = descriptives,
    mean_col = mean_col,
    median_col = median_col,
    mode_col = mode_col,
    se_col = se_col,
    facet_col = "target_word_category_analysis"
  )
}

ordinal_response_levels <- 1:7

make_fixed_round_statistics_label_data <- function(
    descriptives,
    mean_col,
    median_col,
    mode_col = NULL,
    q1_col = NULL,
    q3_col = NULL,
    facet_col = NULL,
    n_col = "n",
    label_x = 2,
    label_y = -0.5,
    y_lower_limit = -1,
    digits = 2,
    include_mode = TRUE,
    include_iqr = FALSE
) {
  required_cols <- c("round", "round_factor", mean_col, median_col, n_col)

  if (isTRUE(include_mode)) {
    required_cols <- c(required_cols, mode_col)
  }

  if (isTRUE(include_iqr)) {
    required_cols <- c(required_cols, q1_col, q3_col)
  }

  if (!is.null(facet_col)) {
    required_cols <- c(required_cols, facet_col)
  }

  missing_cols <- setdiff(required_cols, names(descriptives))

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "The statistics annotation cannot be created because these columns are missing: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  label_source <- descriptives %>%
    arrange(round) %>%
    mutate(
      iqr_text = if (isTRUE(include_iqr)) {
        paste0(
          "; IQR = ",
          purrr::map_chr(.data[[q1_col]], format_plot_number, digits = digits),
          "–",
          purrr::map_chr(.data[[q3_col]], format_plot_number, digits = digits)
        )
      } else {
        ""
      },
      statistics_line = if (isTRUE(include_mode)) {
        paste0(
          as.character(round_factor),
          ": Median = ", purrr::map_chr(.data[[median_col]], format_plot_number, digits = digits),
          "; Mode = ", purrr::map_chr(.data[[mode_col]], format_plot_text),
          "; Mean = ", purrr::map_chr(.data[[mean_col]], format_plot_number, digits = digits),
          iqr_text,
          "; n = ", .data[[n_col]]
        )
      } else {
        paste0(
          as.character(round_factor),
          ": Median = ", purrr::map_chr(.data[[median_col]], format_plot_number, digits = digits),
          "; Mean = ", purrr::map_chr(.data[[mean_col]], format_plot_number, digits = digits),
          iqr_text,
          "; n = ", .data[[n_col]]
        )
      }
    )

  if (is.null(facet_col)) {
    label_source %>%
      summarise(
        x = label_x,
        y = label_y,
        y_lower_limit = y_lower_limit,
        statistics_label = paste(statistics_line, collapse = "\n"),
        .groups = "drop"
      )
  } else {
    label_source %>%
      group_by(across(all_of(facet_col))) %>%
      summarise(
        x = label_x,
        y = label_y,
        y_lower_limit = y_lower_limit,
        statistics_label = paste(statistics_line, collapse = "\n"),
        .groups = "drop"
      )
  }
}

add_fixed_round_statistics_annotation <- function(
    plot_object,
    descriptives,
    mean_col,
    median_col,
    mode_col = NULL,
    q1_col = NULL,
    q3_col = NULL,
    facet_col = NULL,
    label_x = 2,
    label_y = -0.5,
    y_lower_limit = -1,
    text_size = 2.8,
    digits = 2,
    include_mode = TRUE,
    include_iqr = FALSE,
    bottom_margin = 30
) {
  statistics_label_data <- make_fixed_round_statistics_label_data(
    descriptives = descriptives,
    mean_col = mean_col,
    median_col = median_col,
    mode_col = mode_col,
    q1_col = q1_col,
    q3_col = q3_col,
    facet_col = facet_col,
    label_x = label_x,
    label_y = label_y,
    y_lower_limit = y_lower_limit,
    digits = digits,
    include_mode = include_mode,
    include_iqr = include_iqr
  )

  plot_object +
    geom_text(
      data = statistics_label_data,
      aes(x = x, y = y, label = statistics_label),
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = 1,
      lineheight = 0.95,
      size = text_size
    ) +
    expand_limits(y = min(statistics_label_data$y_lower_limit, na.rm = TRUE)) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 10, r = 10, b = bottom_margin, l = 10)
    )
}

make_ordinal_distribution_table <- function(
    data,
    score_col,
    facet_col = NULL,
    extra_group_vars = character(),
    response_levels = ordinal_response_levels
) {
  group_vars <- c(extra_group_vars, facet_col, "round", "round_factor")
  group_vars <- group_vars[!is.na(group_vars) & nzchar(group_vars)]

  data_valid <- data %>%
    mutate(
      score_numeric = as.numeric(.data[[score_col]]),
      score_ordinal = as.integer(round(score_numeric))
    ) %>%
    filter(
      !is.na(score_ordinal),
      score_ordinal %in% response_levels
    )

  if (nrow(data_valid) == 0) {
    stop("No valid ordinal scores are available for the requested ordinal distribution plot.", call. = FALSE)
  }

  group_keys <- data_valid %>%
    distinct(across(all_of(group_vars)))

  counts <- data_valid %>%
    count(across(all_of(group_vars)), score_ordinal, name = "n")

  group_keys %>%
    tidyr::crossing(score_ordinal = response_levels) %>%
    left_join(counts, by = c(group_vars, "score_ordinal")) %>%
    mutate(n = tidyr::replace_na(n, 0L)) %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(
      total_n = sum(n),
      percent = if_else(total_n > 0, 100 * n / total_n, NA_real_)
    ) %>%
    arrange(across(all_of(group_vars)), score_ordinal) %>%
    mutate(cumulative_percent = cumsum(tidyr::replace_na(percent, 0))) %>%
    ungroup() %>%
    mutate(
      score_ordinal_factor = factor(score_ordinal, levels = response_levels, ordered = TRUE)
    )
}

make_ordinal_stacked_bar_plot <- function(
    data,
    descriptives,
    title_text,
    score_col,
    mean_col,
    median_col,
    mode_col,
    facet_col = NULL,
    extra_group_vars = character(),
    y_label = "Percentage of responses",
    x_label = "Image-generation round"
) {
  dist_table <- make_ordinal_distribution_table(
    data = data,
    score_col = score_col,
    facet_col = facet_col,
    extra_group_vars = extra_group_vars
  )

  base_plot <- ggplot(
    dist_table,
    aes(x = round_factor, y = percent, fill = score_ordinal_factor)
  ) +
    geom_col(width = 0.72) +
    scale_y_continuous(labels = function(x) ifelse(x < 0, "", paste0(x, "%"))) +
    labs(
      title = title_text,
      x = x_label,
      y = y_label,
      fill = "Likert response"
    ) +
    plot_theme

  if (!is.null(facet_col)) {
    base_plot <- base_plot + facet_wrap(vars(.data[[facet_col]]), nrow = 1, drop = FALSE)
  }

  add_fixed_round_statistics_annotation(
    plot_object = base_plot,
    descriptives = descriptives,
    mean_col = mean_col,
    median_col = median_col,
    mode_col = mode_col,
    facet_col = facet_col,
    label_x = factor("Round 2", levels = c("Round 1", "Round 2", "Round 3")),
    label_y = -7,
    y_lower_limit = -28
  )
}

median_iqr_remark_sentences <- c(
  "Remarks: Points represent medians.",
  "Vertical bars show the interquartile range (IQR): Q1 to Q3.",
  "The IQR contains the middle 50% of responses.",
  "It is not a standard error, confidence interval, or significance test.",
  "Lines connect medians across rounds."
)

make_median_iqr_caption <- function(subscale_label = NULL) {
  caption_lines <- median_iqr_remark_sentences

  if (!is.null(subscale_label) && !is.na(subscale_label) && nzchar(as.character(subscale_label))) {
    caption_lines <- c(paste0("Subscale: ", as.character(subscale_label), "."), caption_lines)
  }

  paste(caption_lines, collapse = "\n")
}

make_ordinal_median_iqr_plot <- function(
    descriptives,
    title_text,
    mean_col,
    median_col,
    mode_col,
    q1_col,
    q3_col,
    facet_col = NULL,
    subscale_label = NULL,
    y_label = "Median score with IQR",
    x_label = "Image-generation round"
) {
  y_min <- min(descriptives[[q1_col]], na.rm = TRUE)
  y_max <- max(descriptives[[q3_col]], na.rm = TRUE)

  if (!is.finite(y_min) || !is.finite(y_max)) {
    y_min <- 1
    y_max <- 7
  }

  label_y <- y_min - 0.65
  y_lower_limit <- y_min - 1.85

  base_plot <- ggplot(
    descriptives,
    aes(x = round, y = .data[[median_col]], group = 1)
  ) +
    geom_line() +
    geom_point(size = 3) +
    geom_errorbar(
      aes(ymin = .data[[q1_col]], ymax = .data[[q3_col]]),
      width = 0.1
    ) +
    scale_x_continuous(
      breaks = c(1, 2, 3),
      labels = c("Round 1", "Round 2", "Round 3")
    ) +
    scale_y_continuous(breaks = ordinal_response_levels) +
    labs(
      title = title_text,
      x = x_label,
      y = y_label,
      caption = make_median_iqr_caption(subscale_label = subscale_label)
    ) +
    plot_theme +
    theme(
      plot.caption.position = "plot",
      plot.caption = element_text(
        hjust = 0,
        size = 8.5,
        lineheight = 1.05,
        margin = margin(t = 16, b = 8)
      ),
      plot.margin = margin(t = 10, r = 14, b = 22, l = 14)
    )

  if (!is.null(facet_col)) {
    base_plot <- base_plot + facet_wrap(vars(.data[[facet_col]]), nrow = 1, drop = FALSE)
  }

  add_fixed_round_statistics_annotation(
    plot_object = base_plot,
    descriptives = descriptives,
    mean_col = mean_col,
    median_col = median_col,
    mode_col = mode_col,
    facet_col = facet_col,
    label_x = 2,
    label_y = label_y,
    y_lower_limit = y_lower_limit,
    include_mode = FALSE,
    include_iqr = TRUE,
    q1_col = q1_col,
    q3_col = q3_col,
    bottom_margin = 115
  )
}

make_ordinal_violin_box_jitter_plot <- function(
    data,
    descriptives,
    title_text,
    score_col,
    mean_col,
    median_col,
    mode_col,
    facet_col = NULL,
    y_label = "Likert response",
    x_label = "Image-generation round"
) {
  score_values <- as.numeric(data[[score_col]])
  y_min <- min(score_values, na.rm = TRUE)

  if (!is.finite(y_min)) {
    y_min <- 1
  }

  label_y <- y_min - 0.65
  y_lower_limit <- y_min - 1.85

  base_plot <- ggplot(
    data,
    aes(x = round_factor, y = as.numeric(.data[[score_col]]))
  ) +
    geom_violin(trim = FALSE, alpha = 0.35) +
    geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.65) +
    geom_jitter(width = 0.08, height = 0.03, alpha = 0.30, size = 1.2) +
    scale_y_continuous(breaks = ordinal_response_levels) +
    labs(
      title = title_text,
      x = x_label,
      y = y_label
    ) +
    plot_theme

  if (!is.null(facet_col)) {
    base_plot <- base_plot + facet_wrap(vars(.data[[facet_col]]), nrow = 1, drop = FALSE)
  }

  add_fixed_round_statistics_annotation(
    plot_object = base_plot,
    descriptives = descriptives,
    mean_col = mean_col,
    median_col = median_col,
    mode_col = mode_col,
    facet_col = facet_col,
    label_x = factor("Round 2", levels = c("Round 1", "Round 2", "Round 3")),
    label_y = label_y,
    y_lower_limit = y_lower_limit,
    text_size = 2.7
  )
}

make_ordinal_cumulative_proportion_plot <- function(
    data,
    descriptives,
    title_text,
    score_col,
    mean_col,
    median_col,
    mode_col,
    facet_col = NULL,
    extra_group_vars = character(),
    y_label = "Cumulative percentage of responses",
    x_label = "Likert response threshold"
) {
  dist_table <- make_ordinal_distribution_table(
    data = data,
    score_col = score_col,
    facet_col = facet_col,
    extra_group_vars = extra_group_vars
  )

  base_plot <- ggplot(
    dist_table,
    aes(x = score_ordinal, y = cumulative_percent, group = round_factor, linetype = round_factor)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = ordinal_response_levels) +
    scale_y_continuous(labels = function(x) ifelse(x < 0, "", paste0(x, "%"))) +
    labs(
      title = title_text,
      x = x_label,
      y = y_label,
      linetype = "Round"
    ) +
    plot_theme

  if (!is.null(facet_col)) {
    base_plot <- base_plot + facet_wrap(vars(.data[[facet_col]]), nrow = 1, drop = FALSE)
  }

  add_fixed_round_statistics_annotation(
    plot_object = base_plot,
    descriptives = descriptives,
    mean_col = mean_col,
    median_col = median_col,
    mode_col = mode_col,
    facet_col = facet_col,
    label_x = 4,
    label_y = -7,
    y_lower_limit = -28
  )
}

make_ordinal_plot_suite <- function(
    data,
    descriptives,
    title_prefix,
    score_col,
    mean_col,
    median_col,
    mode_col,
    q1_col,
    q3_col,
    facet_col = NULL,
    extra_group_vars = character(),
    subscale_label = NULL,
    score_label = "Likert response"
) {
  list(
    stacked_distribution = make_ordinal_stacked_bar_plot(
      data = data,
      descriptives = descriptives,
      title_text = paste0(title_prefix, ": stacked response distribution"),
      score_col = score_col,
      mean_col = mean_col,
      median_col = median_col,
      mode_col = mode_col,
      facet_col = facet_col,
      extra_group_vars = extra_group_vars
    ),
    median_iqr = make_ordinal_median_iqr_plot(
      descriptives = descriptives,
      title_text = paste0(title_prefix, ": median with interquartile range"),
      mean_col = mean_col,
      median_col = median_col,
      mode_col = mode_col,
      q1_col = q1_col,
      q3_col = q3_col,
      facet_col = facet_col,
      subscale_label = subscale_label,
      y_label = "Median score with IQR"
    ),
    violin_box_jitter = make_ordinal_violin_box_jitter_plot(
      data = data,
      descriptives = descriptives,
      title_text = paste0(title_prefix, ": distribution with violin, boxplot and individual points"),
      score_col = score_col,
      mean_col = mean_col,
      median_col = median_col,
      mode_col = mode_col,
      facet_col = facet_col,
      y_label = score_label
    ),
    cumulative_proportion = make_ordinal_cumulative_proportion_plot(
      data = data,
      descriptives = descriptives,
      title_text = paste0(title_prefix, ": cumulative response distribution"),
      score_col = score_col,
      mean_col = mean_col,
      median_col = median_col,
      mode_col = mode_col,
      facet_col = facet_col,
      extra_group_vars = extra_group_vars
    )
  )
}

# =========================================================
# 4b) Neue Outcome-Mappings für Zusatzplots               ===
# =========================================================

# IMPORTANT:
# These mappings follow the score variables created in the unified
# data-cleaning workflow:
# - Overall agreement: Q26, Q34, Q42
# - Image-agreement subscales: Q27_1-Q27_3, Q35_1-Q35_3, Q43_1-Q43_3
# - Change in mental image: Q28, Q36, Q44
# Update only this block if the survey/source variable names change.

image_agreement_subscale_specs <- tibble::tribble(
  ~subscale,              ~round, ~round_var,
  "Agreement subscale 1", 1,      "Main_Survey_Q27_1_score",
  "Agreement subscale 1", 2,      "Main_Survey_Q35_1_score",
  "Agreement subscale 1", 3,      "Main_Survey_Q43_1_score",
  "Agreement subscale 2", 1,      "Main_Survey_Q27_2_score",
  "Agreement subscale 2", 2,      "Main_Survey_Q35_2_score",
  "Agreement subscale 2", 3,      "Main_Survey_Q43_2_score",
  "Agreement subscale 3", 1,      "Main_Survey_Q27_3_score",
  "Agreement subscale 3", 2,      "Main_Survey_Q35_3_score",
  "Agreement subscale 3", 3,      "Main_Survey_Q43_3_score"
)

mental_image_change_specs <- tibble::tribble(
  ~change_measure,           ~round, ~round_var,
  "Change in mental image",  1,      "Main_Survey_Q28_score",
  "Change in mental image",  2,      "Main_Survey_Q36_score",
  "Change in mental image",  3,      "Main_Survey_Q44_score"
)

validate_variable_specs(
  image_agreement_subscale_specs,
  analysis_raw,
  "image_agreement_subscale_specs"
)

validate_variable_specs(
  mental_image_change_specs,
  analysis_raw,
  "mental_image_change_specs"
)

# =========================================================
# 5) Q6, Q7 und Zielwortkategorie recodieren              ===
# =========================================================

analysis_agreement <- analysis_raw %>%
  mutate(
    q6_duration_num = recode_q6_duration(Pre_Survey_Q6),
    q7_genai_frequency = recode_q7_genai_frequency(Pre_Survey_Q7),
    q7_genai_frequency_factor = factor(
      q7_genai_frequency,
      levels = c(
        "Daily",
        "Several times per week",
        "Weekly",
        "Monthly",
        "Less often",
        "No longer use it"
      ),
      ordered = FALSE
    ),
    q7_genai_frequency_model_group = case_when(
      q7_genai_frequency %in% c(
        "Daily",
        "Several times per week",
        "Weekly"
      ) ~ "Frequent use",
      q7_genai_frequency %in% c(
        "Monthly",
        "Less often",
        "No longer use it"
      ) ~ "Infrequent or no use",
      TRUE ~ NA_character_
    ),
    q7_genai_frequency_model_group = factor(
      q7_genai_frequency_model_group,
      levels = c("Frequent use", "Infrequent or no use"),
      ordered = FALSE
    ),
    target_word_category_analysis = normalize_target_word_category_analysis(
      Main_Survey_target_word_category
    )
  )

target_word_category_levels <- analysis_agreement %>%
  filter(!is.na(target_word_category_analysis)) %>%
  distinct(target_word_category_analysis) %>%
  arrange(target_word_category_analysis) %>%
  pull(target_word_category_analysis)

target_word_category_plot_levels <- c("Overall", target_word_category_levels)

analysis_agreement <- analysis_agreement %>%
  mutate(
    target_word_category_analysis = factor(
      target_word_category_analysis,
      levels = target_word_category_levels,
      ordered = FALSE
    )
  )

add_overall_target_category_rows <- function(data) {
  bind_rows(
    data %>%
      mutate(
        target_word_category_analysis = factor(
          "Overall",
          levels = target_word_category_plot_levels,
          ordered = FALSE
        )
      ),
    data %>%
      mutate(
        target_word_category_analysis = factor(
          as.character(target_word_category_analysis),
          levels = target_word_category_plot_levels,
          ordered = FALSE
        )
      )
  )
}

q6_recoding_check <- analysis_agreement %>%
  mutate(Pre_Survey_Q6_normalized = normalize_q6_text(Pre_Survey_Q6)) %>%
  count(
    Pre_Survey_Q6,
    Pre_Survey_Q6_normalized,
    q6_duration_num,
    sort = TRUE,
    name = "n"
  )

q6_recoding_overview <- tibble(
  n_cases = nrow(analysis_agreement),
  n_q6_raw_available = sum(!is.na(normalize_missing_text(analysis_agreement$Pre_Survey_Q6))),
  n_q6_numeric_available = sum(!is.na(analysis_agreement$q6_duration_num)),
  n_q6_unmapped = sum(!is.na(normalize_missing_text(analysis_agreement$Pre_Survey_Q6)) & is.na(analysis_agreement$q6_duration_num))
)

q7_recoding_check <- analysis_agreement %>%
  mutate(Pre_Survey_Q7_normalized = normalize_q7_text(Pre_Survey_Q7)) %>%
  count(
    Pre_Survey_Q7,
    Pre_Survey_Q7_normalized,
    q7_genai_frequency,
    q7_genai_frequency_model_group,
    sort = TRUE,
    name = "n"
  )

q7_distribution_full <- analysis_agreement %>%
  distinct(participant_id, q7_genai_frequency_factor) %>%
  count(q7_genai_frequency_factor, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))

q7_distribution_model_group <- analysis_agreement %>%
  distinct(participant_id, q7_genai_frequency_model_group) %>%
  count(q7_genai_frequency_model_group, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))

target_category_distribution <- analysis_agreement %>%
  distinct(participant_id, Main_Survey_target_word_category, target_word_category_analysis) %>%
  count(Main_Survey_target_word_category, target_word_category_analysis, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))

# =========================================================
# 6) Agreement-Daten ins Long Format bringen              ===
# =========================================================

agreement_long <- analysis_agreement %>%
  select(
    participant_id,
    q6_duration_num,
    q7_genai_frequency,
    q7_genai_frequency_factor,
    q7_genai_frequency_model_group,
    viviq_total_score,
    Main_Survey_target_word_category,
    target_word_category_analysis,
    Main_Survey_Q26_score,
    Main_Survey_Q34_score,
    Main_Survey_Q42_score
  ) %>%
  pivot_longer(
    cols = c(
      Main_Survey_Q26_score,
      Main_Survey_Q34_score,
      Main_Survey_Q42_score
    ),
    names_to = "round_var",
    values_to = "agreement_score"
  ) %>%
  mutate(
    round = case_when(
      round_var == "Main_Survey_Q26_score" ~ 1,
      round_var == "Main_Survey_Q34_score" ~ 2,
      round_var == "Main_Survey_Q42_score" ~ 3,
      TRUE ~ NA_real_
    ),
    round_factor = factor(
      round,
      levels = c(1, 2, 3),
      labels = c("Round 1", "Round 2", "Round 3")
    ),
    agreement_score = as.numeric(agreement_score),
    agreement_score_ord = factor(agreement_score, ordered = TRUE),
    q6_duration_num = as.numeric(q6_duration_num),
    q6_duration_num_c = q6_duration_num - mean(q6_duration_num, na.rm = TRUE),
    viviq_total_score = as.numeric(viviq_total_score),
    viviq_total_z = as.numeric(scale(viviq_total_score)),
    target_word_category_analysis = factor(
      target_word_category_analysis,
      levels = target_word_category_levels,
      ordered = FALSE
    )
  ) %>%
  filter(
    !is.na(participant_id),
    !is.na(round),
    !is.na(agreement_score)
  )

agreement_model_data <- agreement_long %>%
  filter(
    !is.na(q6_duration_num_c),
    !is.na(q7_genai_frequency_model_group),
    !is.na(viviq_total_z),
    !is.na(target_word_category_analysis)
  )

if (nrow(agreement_long) == 0) {
  stop("agreement_long contains no usable observations.", call. = FALSE)
}

if (nrow(agreement_model_data) == 0) {
  stop("agreement_model_data contains no complete cases.", call. = FALSE)
}

agreement_long_check <- agreement_long %>%
  summarise(
    n_observations = n(),
    n_participants = n_distinct(participant_id),
    n_rounds = n_distinct(round),
    min_round = min(round, na.rm = TRUE),
    max_round = max(round, na.rm = TRUE),
    n_agreement_available = sum(!is.na(agreement_score)),
    n_q6_available = sum(!is.na(q6_duration_num)),
    n_q7_available = sum(!is.na(q7_genai_frequency_model_group)),
    n_viviq_available = sum(!is.na(viviq_total_score)),
    n_target_category_available = sum(!is.na(target_word_category_analysis))
  )

agreement_descriptives <- agreement_long %>%
  group_by(round, round_factor) %>%
  summarise(
    n = n(),
    n_participants = n_distinct(participant_id),
    mean_agreement = mean(agreement_score, na.rm = TRUE),
    sd_agreement = sd(agreement_score, na.rm = TRUE),
    se_agreement = sd_agreement / sqrt(n),
    median_agreement = median(agreement_score, na.rm = TRUE),
    mode_agreement = safe_mode_label(agreement_score),
    q1_agreement = as.numeric(stats::quantile(agreement_score, probs = 0.25, na.rm = TRUE, names = FALSE)),
    q3_agreement = as.numeric(stats::quantile(agreement_score, probs = 0.75, na.rm = TRUE, names = FALSE)),
    min_agreement = min(agreement_score, na.rm = TRUE),
    max_agreement = max(agreement_score, na.rm = TRUE),
    .groups = "drop"
  )

model_sample_overview <- bind_rows(
  agreement_long %>%
    summarise(
      model = "Descriptive long data",
      n_observations = n(),
      n_participants = n_distinct(participant_id),
      n_complete_q6 = sum(!is.na(q6_duration_num)),
      n_complete_q7 = sum(!is.na(q7_genai_frequency_model_group)),
      n_complete_viviq = sum(!is.na(viviq_total_score)),
      n_complete_target_category = sum(!is.na(target_word_category_analysis))
    ),
  agreement_model_data %>%
    summarise(
      model = "Final complete-case model data",
      n_observations = n(),
      n_participants = n_distinct(participant_id),
      n_complete_q6 = sum(!is.na(q6_duration_num)),
      n_complete_q7 = sum(!is.na(q7_genai_frequency_model_group)),
      n_complete_viviq = sum(!is.na(viviq_total_score)),
      n_complete_target_category = sum(!is.na(target_word_category_analysis))
    )
)

control_descriptives <- agreement_model_data %>%
  summarise(
    n_observations = n(),
    n_participants = n_distinct(participant_id),
    mean_q6_duration = mean(q6_duration_num, na.rm = TRUE),
    sd_q6_duration = sd(q6_duration_num, na.rm = TRUE),
    mean_viviq = mean(viviq_total_score, na.rm = TRUE),
    sd_viviq = sd(viviq_total_score, na.rm = TRUE)
  )

# =========================================================
# 6b) Zusatzdaten: Zielwortkategorie, Subskalen, Change   ===
# =========================================================

agreement_long_for_target_category_plots <- agreement_long %>%
  add_overall_target_category_rows()

agreement_descriptives_by_target_category <- agreement_long_for_target_category_plots %>%
  group_by(target_word_category_analysis, round, round_factor) %>%
  summarise(
    n = n(),
    n_participants = n_distinct(participant_id),
    mean_agreement = mean(agreement_score, na.rm = TRUE),
    sd_agreement = sd(agreement_score, na.rm = TRUE),
    se_agreement = sd_agreement / sqrt(n),
    median_agreement = median(agreement_score, na.rm = TRUE),
    mode_agreement = safe_mode_label(agreement_score),
    q1_agreement = as.numeric(stats::quantile(agreement_score, probs = 0.25, na.rm = TRUE, names = FALSE)),
    q3_agreement = as.numeric(stats::quantile(agreement_score, probs = 0.75, na.rm = TRUE, names = FALSE)),
    min_agreement = min(agreement_score, na.rm = TRUE),
    max_agreement = max(agreement_score, na.rm = TRUE),
    .groups = "drop"
  )

image_agreement_subscale_long <- make_round_long_from_specs(
  analysis_agreement,
  image_agreement_subscale_specs
)

image_agreement_subscale_model_data <- image_agreement_subscale_long %>%
  filter(
    !is.na(q6_duration_num_c),
    !is.na(q7_genai_frequency_model_group),
    !is.na(viviq_total_z),
    !is.na(target_word_category_analysis)
  )

image_agreement_subscale_long_check <- image_agreement_subscale_long %>%
  group_by(subscale) %>%
  summarise(
    n_observations = n(),
    n_participants = n_distinct(participant_id),
    n_rounds = n_distinct(round),
    min_round = min(round, na.rm = TRUE),
    max_round = max(round, na.rm = TRUE),
    n_score_available = sum(!is.na(score)),
    n_target_category_available = sum(!is.na(target_word_category_analysis)),
    .groups = "drop"
  )

image_agreement_subscale_long_for_target_category_plots <- image_agreement_subscale_long %>%
  add_overall_target_category_rows()

image_agreement_subscale_descriptives_by_target_category <- image_agreement_subscale_long_for_target_category_plots %>%
  summarise_round_target_category(extra_group_vars = "subscale")

mental_image_change_long <- make_round_long_from_specs(
  analysis_agreement,
  mental_image_change_specs
)

mental_image_change_model_data <- mental_image_change_long %>%
  filter(
    !is.na(q6_duration_num_c),
    !is.na(q7_genai_frequency_model_group),
    !is.na(viviq_total_z),
    !is.na(target_word_category_analysis)
  )

mental_image_change_long_check <- mental_image_change_long %>%
  summarise(
    n_observations = n(),
    n_participants = n_distinct(participant_id),
    n_rounds = n_distinct(round),
    min_round = min(round, na.rm = TRUE),
    max_round = max(round, na.rm = TRUE),
    n_score_available = sum(!is.na(score)),
    n_target_category_available = sum(!is.na(target_word_category_analysis))
  )

mental_image_change_long_for_target_category_plots <- mental_image_change_long %>%
  add_overall_target_category_rows()

mental_image_change_descriptives_by_target_category <- mental_image_change_long_for_target_category_plots %>%
  summarise_round_target_category(extra_group_vars = "change_measure")

agreement_ordinal_distribution_by_round <- make_ordinal_distribution_table(
  data = agreement_long,
  score_col = "agreement_score"
)

agreement_ordinal_distribution_by_target_category <- make_ordinal_distribution_table(
  data = agreement_long_for_target_category_plots,
  score_col = "agreement_score",
  facet_col = "target_word_category_analysis"
)

image_agreement_subscale_ordinal_distribution_by_target_category <- make_ordinal_distribution_table(
  data = image_agreement_subscale_long_for_target_category_plots,
  score_col = "score",
  facet_col = "target_word_category_analysis",
  extra_group_vars = "subscale"
)

mental_image_change_ordinal_distribution_by_target_category <- make_ordinal_distribution_table(
  data = mental_image_change_long_for_target_category_plots,
  score_col = "score",
  facet_col = "target_word_category_analysis",
  extra_group_vars = "change_measure"
)

# =========================================================
# 7) Primärmodell: Round als Faktor                       ===
# =========================================================

model_primary_round_factor <- lmerTest::lmer(
  agreement_score ~ round_factor + (1 | participant_id),
  data = agreement_model_data,
  REML = TRUE
)

model_primary_fixed <- get_lmm_fixed_table(
  model_primary_round_factor,
  model_label = "Model 1: Primary round-factor model"
)

model_primary_fit <- get_lmm_fit_table(
  model_primary_round_factor,
  model_label = "Model 1: Primary round-factor model"
)

model_primary_icc <- get_lmm_icc_table(
  model_primary_round_factor,
  model_label = "Model 1: Primary round-factor model"
)

round_emmeans <- emmeans::emmeans(
  model_primary_round_factor,
  ~ round_factor
)

round_emmeans_table <- as.data.frame(round_emmeans) %>%
  as_tibble()

round_pairwise_holm_table <- pairs(
  round_emmeans,
  adjust = "holm"
) %>%
  as.data.frame() %>%
  as_tibble()

# =========================================================
# 8) Kontrollmodell mit Q6, Q7, VIVIQ, Zielwortkategorie  ===
# =========================================================

model_controlled_q7 <- lmerTest::lmer(
  agreement_score ~
    round_factor +
    q6_duration_num_c +
    q7_genai_frequency_model_group +
    viviq_total_z +
    target_word_category_analysis +
    (1 | participant_id),
  data = agreement_model_data,
  REML = TRUE
)

model_controlled_q7_fixed <- get_lmm_fixed_table(
  model_controlled_q7,
  model_label = "Model 2: Controlled model with Q6, Q7, VIVIQ, target category"
)

model_controlled_q7_fit <- get_lmm_fit_table(
  model_controlled_q7,
  model_label = "Model 2: Controlled model with Q6, Q7, VIVIQ, target category"
)

model_controlled_q7_icc <- get_lmm_icc_table(
  model_controlled_q7,
  model_label = "Model 2: Controlled model with Q6, Q7, VIVIQ, target category"
)

controlled_round_emmeans <- emmeans::emmeans(
  model_controlled_q7,
  ~ round_factor
)

controlled_round_emmeans_table <- as.data.frame(controlled_round_emmeans) %>%
  as_tibble()

controlled_round_pairwise_holm_table <- pairs(
  controlled_round_emmeans,
  adjust = "holm"
) %>%
  as.data.frame() %>%
  as_tibble()

# =========================================================
# 9) Ordinales Robustheitsmodell                         ===
# =========================================================

model_ordinal_q7 <- ordinal::clmm(
  agreement_score_ord ~
    round_factor +
    q6_duration_num_c +
    q7_genai_frequency_model_group +
    viviq_total_z +
    target_word_category_analysis +
    (1 | participant_id),
  data = agreement_model_data
)

model_ordinal_q7_coefficients <- as.data.frame(coef(summary(model_ordinal_q7))) %>%
  rownames_to_column("term") %>%
  as_tibble()

# =========================================================
# 10) ML-Modellvergleiche                                ===
# =========================================================

model_primary_ml <- lmerTest::lmer(
  agreement_score ~ round_factor + (1 | participant_id),
  data = agreement_model_data,
  REML = FALSE
)

model_controlled_no_q7_ml <- lmerTest::lmer(
  agreement_score ~
    round_factor +
    q6_duration_num_c +
    viviq_total_z +
    target_word_category_analysis +
    (1 | participant_id),
  data = agreement_model_data,
  REML = FALSE
)

model_controlled_with_q7_ml <- lmerTest::lmer(
  agreement_score ~
    round_factor +
    q6_duration_num_c +
    q7_genai_frequency_model_group +
    viviq_total_z +
    target_word_category_analysis +
    (1 | participant_id),
  data = agreement_model_data,
  REML = FALSE
)

model_comparison_primary_vs_controlled <- anova(
  model_primary_ml,
  model_controlled_with_q7_ml
) %>%
  as.data.frame() %>%
  rownames_to_column("model_object") %>%
  as_tibble()

model_comparison_without_vs_with_q7 <- anova(
  model_controlled_no_q7_ml,
  model_controlled_with_q7_ml
) %>%
  as.data.frame() %>%
  rownames_to_column("model_object") %>%
  as_tibble()

# =========================================================
# 11) Change-Score-Analyse                               ===
# =========================================================

agreement_change_wide <- agreement_model_data %>%
  select(participant_id, round, agreement_score) %>%
  pivot_wider(
    names_from = round,
    values_from = agreement_score,
    names_prefix = "round_"
  ) %>%
  mutate(
    change_r2_r1 = round_2 - round_1,
    change_r3_r2 = round_3 - round_2,
    change_r3_r1 = round_3 - round_1,
    change_direction_r3_r1 = case_when(
      change_r3_r1 > 0 ~ "Improved",
      change_r3_r1 == 0 ~ "No change",
      change_r3_r1 < 0 ~ "Decreased",
      TRUE ~ NA_character_
    )
  )

change_summary <- agreement_change_wide %>%
  summarise(
    n = n(),
    mean_change_r2_r1 = mean(change_r2_r1, na.rm = TRUE),
    sd_change_r2_r1 = sd(change_r2_r1, na.rm = TRUE),
    mean_change_r3_r2 = mean(change_r3_r2, na.rm = TRUE),
    sd_change_r3_r2 = sd(change_r3_r2, na.rm = TRUE),
    mean_change_r3_r1 = mean(change_r3_r1, na.rm = TRUE),
    sd_change_r3_r1 = sd(change_r3_r1, na.rm = TRUE),
    median_change_r3_r1 = median(change_r3_r1, na.rm = TRUE)
  )

change_direction_summary <- agreement_change_wide %>%
  count(change_direction_r3_r1, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))

# =========================================================
# 12) Kombinierte Modelltabellen                         ===
# =========================================================

combined_fixed_effects <- bind_rows(
  model_primary_fixed,
  model_controlled_q7_fixed
) %>%
  select(
    model,
    term,
    estimate,
    std.error,
    statistic,
    df,
    p.value,
    conf.low,
    conf.high
  )

combined_fit_statistics <- bind_rows(
  model_primary_fit,
  model_controlled_q7_fit
)

combined_icc <- bind_rows(
  model_primary_icc,
  model_controlled_q7_icc
)

# =========================================================
# 13) Visualisierungen                                   ===
# =========================================================

plot_theme <- if (exists("theme_result")) {
  theme_result()
} else {
  theme_minimal()
}

agreement_plot_base <- ggplot(
  agreement_descriptives,
  aes(x = round, y = mean_agreement)
) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_agreement - se_agreement,
      ymax = mean_agreement + se_agreement
    ),
    width = 0.1
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Round 1", "Round 2", "Round 3")
  ) +
  labs(
    title = "Image-mental-image agreement across generation rounds",
    x = "Image-generation round",
    y = "Mean image-mental-image agreement"
  ) +
  plot_theme

agreement_plot <- add_round_statistics_annotation(
  plot_object = agreement_plot_base,
  descriptives = agreement_descriptives,
  mean_col = "mean_agreement",
  median_col = "median_agreement",
  mode_col = "mode_agreement",
  se_col = "se_agreement"
)

agreement_plot_by_target_category <- make_target_category_round_plot(
  descriptives = agreement_descriptives_by_target_category,
  title_text = "Image-mental-image agreement across rounds by target word category",
  y_label = "Mean image-mental-image agreement",
  mean_col = "mean_agreement",
  median_col = "median_agreement",
  mode_col = "mode_agreement",
  se_col = "se_agreement"
)

image_agreement_subscale_plots <- image_agreement_subscale_descriptives_by_target_category %>%
  split(.$subscale) %>%
  purrr::imap(
    ~ make_target_category_round_plot(
      descriptives = .x,
      title_text = paste0(.y, " across rounds by target word category"),
      y_label = paste0("Mean ", .y),
      mean_col = "mean_score",
      se_col = "se_score"
    )
  )

image_agreement_subscale_figure_manifest <- tibble(
  label = paste0("Image agreement ", names(image_agreement_subscale_plots), " by target category"),
  path = file.path(
    out_figures_dir,
    paste0(
      "AgreementSubscale_",
      clean_filename_component(names(image_agreement_subscale_plots)),
      "_by_round_by_target_category.png"
    )
  ),
  notes = "PNG-Grafik"
)

mental_image_change_plot_by_target_category <- make_target_category_round_plot(
  descriptives = mental_image_change_descriptives_by_target_category,
  title_text = "Change in mental image across rounds by target word category",
  y_label = "Mean change in mental image",
  mean_col = "mean_score",
  se_col = "se_score"
)

agreement_ordinal_plot_variants <- make_ordinal_plot_suite(
  data = agreement_long,
  descriptives = agreement_descriptives,
  title_prefix = "Image-mental-image agreement across rounds",
  score_col = "agreement_score",
  mean_col = "mean_agreement",
  median_col = "median_agreement",
  mode_col = "mode_agreement",
  q1_col = "q1_agreement",
  q3_col = "q3_agreement",
  score_label = "Image-mental-image agreement score"
)

agreement_target_category_ordinal_plot_variants <- make_ordinal_plot_suite(
  data = agreement_long_for_target_category_plots,
  descriptives = agreement_descriptives_by_target_category,
  title_prefix = "Image-mental-image agreement across rounds by target word category",
  score_col = "agreement_score",
  mean_col = "mean_agreement",
  median_col = "median_agreement",
  mode_col = "mode_agreement",
  q1_col = "q1_agreement",
  q3_col = "q3_agreement",
  facet_col = "target_word_category_analysis",
  score_label = "Image-mental-image agreement score"
)

image_agreement_subscale_ordinal_plot_variants <- image_agreement_subscale_long_for_target_category_plots %>%
  split(.$subscale) %>%
  purrr::imap(
    ~ make_ordinal_plot_suite(
      data = .x,
      descriptives = image_agreement_subscale_descriptives_by_target_category %>%
        filter(subscale == .y),
      title_prefix = paste0(.y, " across rounds by target word category"),
      score_col = "score",
      mean_col = "mean_score",
      median_col = "median_score",
      mode_col = "mode_score",
      q1_col = "q1_score",
      q3_col = "q3_score",
      facet_col = "target_word_category_analysis",
      extra_group_vars = "subscale",
      subscale_label = .y,
      score_label = paste0(.y, " score")
    )
  )

mental_image_change_ordinal_plot_variants <- make_ordinal_plot_suite(
  data = mental_image_change_long_for_target_category_plots,
  descriptives = mental_image_change_descriptives_by_target_category,
  title_prefix = "Change in mental image across rounds by target word category",
  score_col = "score",
  mean_col = "mean_score",
  median_col = "median_score",
  mode_col = "mode_score",
  q1_col = "q1_score",
  q3_col = "q3_score",
  facet_col = "target_word_category_analysis",
  extra_group_vars = "change_measure",
  score_label = "Change in mental image score"
)

ordinal_variant_labels <- c(
  stacked_distribution = "Stacked response distribution",
  median_iqr = "Median with IQR",
  violin_box_jitter = "Violin, boxplot and jittered points",
  cumulative_proportion = "Cumulative response distribution"
)

make_ordinal_registry_rows <- function(plot_list, label_prefix, file_prefix, width = 15, height = 8) {
  purrr::imap_dfr(
    plot_list,
    ~ tibble(
      label = paste(label_prefix, ordinal_variant_labels[[.y]], sep = " - "),
      path = file.path(out_figures_dir, paste0(file_prefix, "_", .y, ".png")),
      notes = if_else(
        .y == "median_iqr",
        "Ordinal PNG-Grafik with median, mean, IQR, n annotation and IQR remark",
        "Ordinal PNG-Grafik with median, mode, mean and n annotation"
      ),
      width = width,
      height = if_else(.y == "median_iqr", height + 2.4, height),
      plot = list(.x)
    )
  )
}

agreement_ordinal_figure_registry <- make_ordinal_registry_rows(
  agreement_ordinal_plot_variants,
  label_prefix = "Agreement overall",
  file_prefix = "AgreementOrdinal_overall_by_round",
  width = 10,
  height = 8
)

agreement_target_category_ordinal_figure_registry <- make_ordinal_registry_rows(
  agreement_target_category_ordinal_plot_variants,
  label_prefix = "Agreement by target word category",
  file_prefix = "AgreementOrdinal_by_round_by_target_category",
  width = 15,
  height = 8.5
)

image_agreement_subscale_ordinal_figure_registry <- purrr::imap_dfr(
  image_agreement_subscale_ordinal_plot_variants,
  ~ make_ordinal_registry_rows(
    .x,
    label_prefix = paste("Image agreement", .y, "by target word category"),
    file_prefix = paste0(
      "AgreementSubscale_",
      clean_filename_component(.y),
      "_ordinal_by_round_by_target_category"
    ),
    width = 15,
    height = 8.5
  )
)

mental_image_change_ordinal_figure_registry <- make_ordinal_registry_rows(
  mental_image_change_ordinal_plot_variants,
  label_prefix = "Change in mental image by target word category",
  file_prefix = "MentalImageChangeOrdinal_by_round_by_target_category",
  width = 15,
  height = 8.5
)

ordinal_figure_registry <- bind_rows(
  agreement_ordinal_figure_registry,
  agreement_target_category_ordinal_figure_registry,
  image_agreement_subscale_ordinal_figure_registry,
  mental_image_change_ordinal_figure_registry
)

ordinal_figure_manifest <- ordinal_figure_registry %>%
  select(label, path, notes)

individual_change_plot <- ggplot(
  agreement_model_data,
  aes(
    x = round,
    y = agreement_score,
    group = participant_id
  )
) +
  geom_line(alpha = 0.35) +
  geom_point(alpha = 0.60) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1.2
  ) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "point",
    size = 3
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Round 1", "Round 2", "Round 3")
  ) +
  labs(
    title = "Individual agreement trajectories across rounds",
    x = "Image-generation round",
    y = "Agreement score"
  ) +
  plot_theme

# =========================================================
# 14) Tabellen exportieren                               ===
# =========================================================

tables_to_export <- list(
  "01_q6_recoding_check" = q6_recoding_check,
  "02_q6_recoding_overview" = q6_recoding_overview,
  "03_q7_recoding_check" = q7_recoding_check,
  "04_q7_distribution_full" = q7_distribution_full,
  "05_q7_distribution_model_group" = q7_distribution_model_group,
  "06_agreement_long_check" = agreement_long_check,
  "07_agreement_descriptives_by_round" = agreement_descriptives,
  "08_model_sample_overview" = model_sample_overview,
  "09_control_descriptives" = control_descriptives,
  "10_target_category_distribution" = target_category_distribution,
  "11_model_primary_fixed_effects" = model_primary_fixed,
  "12_model_primary_fit_statistics" = model_primary_fit,
  "13_model_primary_random_effects_icc" = model_primary_icc,
  "14_round_estimated_marginal_means" = round_emmeans_table,
  "15_round_pairwise_comparisons_holm" = round_pairwise_holm_table,
  "16_model_controlled_q7_fixed_effects" = model_controlled_q7_fixed,
  "17_model_controlled_q7_fit_statistics" = model_controlled_q7_fit,
  "18_model_controlled_q7_random_effects_icc" = model_controlled_q7_icc,
  "19_controlled_round_estimated_marginal_means" = controlled_round_emmeans_table,
  "20_controlled_round_pairwise_comparisons_holm" = controlled_round_pairwise_holm_table,
  "21_ordinal_model_q7_coefficients" = model_ordinal_q7_coefficients,
  "22_model_comparison_primary_vs_controlled_ml" = model_comparison_primary_vs_controlled,
  "23_model_comparison_without_vs_with_q7_ml" = model_comparison_without_vs_with_q7,
  "24_agreement_change_wide_by_person" = agreement_change_wide,
  "25_agreement_change_summary" = change_summary,
  "26_agreement_change_direction_summary" = change_direction_summary,
  "27_combined_fixed_effects" = combined_fixed_effects,
  "28_combined_fit_statistics" = combined_fit_statistics,
  "29_combined_icc" = combined_icc,
  "30_agreement_descriptives_by_target_category" = agreement_descriptives_by_target_category,
  "31_image_agreement_subscale_specs" = image_agreement_subscale_specs,
  "32_image_agreement_subscale_long_check" = image_agreement_subscale_long_check,
  "33_image_agreement_subscale_descriptives_by_target_category" = image_agreement_subscale_descriptives_by_target_category,
  "34_mental_image_change_specs" = mental_image_change_specs,
  "35_mental_image_change_long_check" = mental_image_change_long_check,
  "36_mental_image_change_descriptives_by_target_category" = mental_image_change_descriptives_by_target_category,
  "37_agreement_ordinal_distribution_by_round" = agreement_ordinal_distribution_by_round,
  "38_agreement_ordinal_distribution_by_target_category" = agreement_ordinal_distribution_by_target_category,
  "39_image_agreement_subscale_ordinal_distribution_by_target_category" = image_agreement_subscale_ordinal_distribution_by_target_category,
  "40_mental_image_change_ordinal_distribution_by_target_category" = mental_image_change_ordinal_distribution_by_target_category
)

purrr::iwalk(
  tables_to_export,
  ~ save_table_outputs(.x, .y, out_dir = out_tables_dir)
)

writexl::write_xlsx(
  tables_to_export,
  path = file.path(out_base_dir, "12_image_agreement_final_unified_tables.xlsx")
)

# =========================================================
# 15) Grafiken und Modell-Summaries exportieren           ===
# =========================================================

ggsave(
  file.path(out_figures_dir, "AgreementFig1_mean_agreement_by_round.png"),
  agreement_plot,
  width = 9,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "AgreementFig1b_mean_agreement_by_round_by_target_category.png"),
  agreement_plot_by_target_category,
  width = 15,
  height = 7.5,
  dpi = 300
)

purrr::iwalk(
  image_agreement_subscale_plots,
  ~ ggsave(
    filename = image_agreement_subscale_figure_manifest$path[
      image_agreement_subscale_figure_manifest$label ==
        paste0("Image agreement ", .y, " by target category")
    ],
    plot = .x,
    width = 15,
    height = 7.5,
    dpi = 300
  )
)

ggsave(
  file.path(out_figures_dir, "AgreementFig1c_mean_mental_image_change_by_round_by_target_category.png"),
  mental_image_change_plot_by_target_category,
  width = 15,
  height = 7.5,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "AgreementFig2_individual_agreement_trajectories.png"),
  individual_change_plot,
  width = 9,
  height = 6,
  dpi = 300
)

for (i in seq_len(nrow(ordinal_figure_registry))) {
  ggsave(
    filename = ordinal_figure_registry$path[[i]],
    plot = ordinal_figure_registry$plot[[i]],
    width = ordinal_figure_registry$width[[i]],
    height = ordinal_figure_registry$height[[i]],
    dpi = 300
  )
}

writeLines(
  capture.output(summary(model_primary_round_factor)),
  con = file.path(out_doc_dir, "12_model1_primary_round_factor_summary.txt")
)

writeLines(
  capture.output(summary(model_controlled_q7)),
  con = file.path(out_doc_dir, "12_model2_controlled_q7_summary.txt")
)

writeLines(
  capture.output(summary(model_ordinal_q7)),
  con = file.path(out_doc_dir, "12_model3_ordinal_q7_summary.txt")
)

# =========================================================
# 16) ChatGPT- und Konsolenzusammenfassung                ===
# =========================================================

console_summary <- c(
  "==================== IMAGE AGREEMENT FINAL UNIFIED ANALYSIS ====================",
  "",
  "Source dataset:",
  analysis_source_name,
  "",
  "Agreement long check:",
  capture.output(print(agreement_long_check)),
  "",
  "Q6 recoding overview:",
  capture.output(print(q6_recoding_overview)),
  "",
  "Q7 full distribution:",
  capture.output(print(q7_distribution_full)),
  "",
  "Q7 model-group distribution:",
  capture.output(print(q7_distribution_model_group)),
  "",
  "Agreement descriptives by round:",
  capture.output(print(agreement_descriptives)),
  "",
  "Agreement descriptives by round and target word category:",
  capture.output(print(agreement_descriptives_by_target_category)),
  "",
  "Image agreement subscale long check:",
  capture.output(print(image_agreement_subscale_long_check)),
  "",
  "Image agreement subscale descriptives by target word category:",
  capture.output(print(image_agreement_subscale_descriptives_by_target_category)),
  "",
  "Mental image change long check:",
  capture.output(print(mental_image_change_long_check)),
  "",
  "Mental image change descriptives by target word category:",
  capture.output(print(mental_image_change_descriptives_by_target_category)),
  "",
  "Model sample overview:",
  capture.output(print(model_sample_overview)),
  "",
  "Primary model fixed effects:",
  capture.output(print(model_primary_fixed)),
  "",
  "Primary model ICC:",
  capture.output(print(model_primary_icc)),
  "",
  "Round pairwise comparisons Holm:",
  capture.output(print(round_pairwise_holm_table)),
  "",
  "Controlled Q7 model fixed effects:",
  capture.output(print(model_controlled_q7_fixed)),
  "",
  "Controlled Q7 model ICC:",
  capture.output(print(model_controlled_q7_icc)),
  "",
  "Controlled round pairwise comparisons Holm:",
  capture.output(print(controlled_round_pairwise_holm_table)),
  "",
  "Ordinal model coefficients:",
  capture.output(print(model_ordinal_q7_coefficients)),
  "",
  "ML comparison: primary vs controlled with Q7:",
  capture.output(print(model_comparison_primary_vs_controlled)),
  "",
  "ML comparison: controlled without Q7 vs controlled with Q7:",
  capture.output(print(model_comparison_without_vs_with_q7)),
  "",
  "Change summary:",
  capture.output(print(change_summary)),
  "",
  "Change direction summary:",
  capture.output(print(change_direction_summary)),
  "",
  "Exported workbook:",
  file.path(out_base_dir, "12_image_agreement_final_unified_tables.xlsx"),
  "",
  "Exported figures:",
  file.path(out_figures_dir, "AgreementFig1_mean_agreement_by_round.png"),
  file.path(out_figures_dir, "AgreementFig1b_mean_agreement_by_round_by_target_category.png"),
  image_agreement_subscale_figure_manifest$path,
  file.path(out_figures_dir, "AgreementFig1c_mean_mental_image_change_by_round_by_target_category.png"),
  file.path(out_figures_dir, "AgreementFig2_individual_agreement_trajectories.png"),
  ordinal_figure_manifest$path,
  "",
  "Model summaries:",
  file.path(out_doc_dir, "12_model1_primary_round_factor_summary.txt"),
  file.path(out_doc_dir, "12_model2_controlled_q7_summary.txt"),
  file.path(out_doc_dir, "12_model3_ordinal_q7_summary.txt")
)

writeLines(
  console_summary,
  con = file.path(out_doc_dir, "12_image_agreement_final_unified_console_summary.txt")
)

writeLines(
  console_summary,
  con = file.path(project_root, "data_output", "RESULTS_FOR_CHATGPT_image_agreement_final_unified.txt")
)

# =========================================================
# 16b) Narrative Methodik- und Ergebnisdokumentation      ===
# =========================================================

method_results_report <- c(
  "==================== IMAGE AGREEMENT ANALYSIS REPORT ====================",
  "",
  "1. Ziel der Analyse",
  "",
  "Diese Analyse untersucht, ob sich die Übereinstimmung zwischen den generierten Bildern",
  "und den mentalen Bildern der Teilnehmenden über drei Bildgenerationsrunden hinweg verändert.",
  "Die abhängige Variable ist der Agreement-Score aus den drei Haupterhebungsvariablen:",
  "Main_Survey_Q26_score, Main_Survey_Q34_score und Main_Survey_Q42_score.",
  "",
  "2. Datengrundlage",
  "",
  paste0("Verwendeter Datensatz: ", analysis_source_name),
  "",
  "Der bereits konsolidierte Analysedatensatz wird verwendet. Es findet kein erneuter Rohdatenimport,",
  "kein erneutes Data Cleaning und kein erneutes Matching statt.",
  "",
  "3. Datenaufbereitung",
  "",
  "Die Agreement-Werte aus drei Bildgenerationsrunden werden vom Wide- ins Long-Format überführt.",
  "Dadurch erhält jede Person bis zu drei Beobachtungen, eine pro Runde.",
  "",
  "Zusätzlich werden folgende Kontrollvariablen recodiert bzw. standardisiert:",
  "- Q6: Dauer der GenAI-Nutzung als numerische ordinale Variable",
  "- Q7: GenAI-Nutzungsfrequenz als kategoriale Variable und zusätzlich als Modellgruppe",
  "- VIVIQ: z-standardisierter VIVIQ-Gesamtscore",
  "- Zielwortkategorie: Abstract vs. Concrete",
  "",
  "4. Deskriptive Analyse",
  "",
  "Zunächst werden Mittelwerte, Standardabweichungen, Standardfehler, Mediane sowie Minimum und Maximum",
  "der Agreement-Scores pro Runde berechnet.",
  "",
  capture.output(print(agreement_descriptives, n = Inf, width = Inf)),
  "",
  "Zusätzlich werden die Agreement-Mittelwerte nach Zielwortkategorie berechnet.",
  "",
  capture.output(print(agreement_descriptives_by_target_category, n = Inf, width = Inf)),
  "",
  "Für die Image-Agreement-Subskalen werden dieselben deskriptiven Kennwerte pro Runde und",
  "Zielwortkategorie berechnet.",
  "",
  capture.output(print(image_agreement_subscale_descriptives_by_target_category, n = Inf, width = Inf)),
  "",
  "Für Change in mental image werden dieselben deskriptiven Kennwerte pro Runde und Zielwortkategorie",
  "berechnet.",
  "",
  capture.output(print(mental_image_change_descriptives_by_target_category, n = Inf, width = Inf)),
  "",
  "5. Primärmodell",
  "",
  "Das Primärmodell ist ein lineares Mixed-Effects-Modell mit Round als kategorialem Prädiktor",
  "und einem Random Intercept für Teilnehmende:",
  "",
  "agreement_score ~ round_factor + (1 | participant_id)",
  "",
  "Dieses Modell prüft, ob sich die mittlere Agreement-Bewertung zwischen den drei Runden unterscheidet.",
  "Der Random Intercept berücksichtigt, dass mehrere Messungen von derselben Person stammen und daher",
  "nicht unabhängig voneinander sind.",
  "",
  "Fixed Effects des Primärmodells:",
  "",
  capture.output(print(model_primary_fixed, n = Inf, width = Inf)),
  "",
  "Estimated Marginal Means pro Runde:",
  "",
  capture.output(print(round_emmeans_table, n = Inf, width = Inf)),
  "",
  "Paarweise Vergleiche mit Holm-Korrektur:",
  "",
  capture.output(print(round_pairwise_holm_table, n = Inf, width = Inf)),
  "",
  "ICC des Primärmodells:",
  "",
  capture.output(print(model_primary_icc, n = Inf, width = Inf)),
  "",
  "6. Kontrollmodell",
  "",
  "Das Kontrollmodell erweitert das Primärmodell um zusätzliche Kovariaten:",
  "",
  "agreement_score ~ round_factor + q6_duration_num_c +",
  "q7_genai_frequency_model_group + viviq_total_z +",
  "target_word_category_analysis + (1 | participant_id)",
  "",
  "Dieses Modell prüft, ob der Effekt der Runde bestehen bleibt, wenn Dauer und Frequenz der GenAI-Nutzung,",
  "VIVIQ-Werte und Zielwortkategorie statistisch kontrolliert werden.",
  "",
  "Fixed Effects des Kontrollmodells:",
  "",
  capture.output(print(model_controlled_q7_fixed, n = Inf, width = Inf)),
  "",
  "Estimated Marginal Means pro Runde im Kontrollmodell:",
  "",
  capture.output(print(controlled_round_emmeans_table, n = Inf, width = Inf)),
  "",
  "Paarweise Vergleiche mit Holm-Korrektur im Kontrollmodell:",
  "",
  capture.output(print(controlled_round_pairwise_holm_table, n = Inf, width = Inf)),
  "",
  "ICC des Kontrollmodells:",
  "",
  capture.output(print(model_controlled_q7_icc, n = Inf, width = Inf)),
  "",
  "7. Ordinales Robustheitsmodell",
  "",
  "Da Agreement-Scores häufig ordinal skaliert sind, wird zusätzlich ein ordinales Mixed Model",
  "als Robustheitsprüfung geschätzt:",
  "",
  "agreement_score_ord ~ round_factor + q6_duration_num_c +",
  "q7_genai_frequency_model_group + viviq_total_z +",
  "target_word_category_analysis + (1 | participant_id)",
  "",
  "Dieses Modell prüft, ob die Befunde auch unter einer ordinalen Modellannahme vergleichbar sind.",
  "",
  "Koeffizienten des ordinalen Modells:",
  "",
  capture.output(print(model_ordinal_q7_coefficients, n = Inf, width = Inf)),
  "",
  "8. ML-Modellvergleiche",
  "",
  "Für Modellvergleiche werden ML-Modelle verwendet, da Modelle mit unterschiedlicher Fixed-Effects-Struktur",
  "nicht über REML verglichen werden sollten.",
  "",
  "Vergleich Primärmodell vs. Kontrollmodell mit Q7:",
  "",
  capture.output(print(model_comparison_primary_vs_controlled, n = Inf, width = Inf)),
  "",
  "Vergleich Kontrollmodell ohne Q7 vs. Kontrollmodell mit Q7:",
  "",
  capture.output(print(model_comparison_without_vs_with_q7, n = Inf, width = Inf)),
  "",
  "9. Change-Score-Analyse",
  "",
  "Zusätzlich wird pro Person berechnet, wie sich der Agreement-Score zwischen den Runden verändert:",
  "- Runde 2 minus Runde 1",
  "- Runde 3 minus Runde 2",
  "- Runde 3 minus Runde 1",
  "",
  "Change-Score-Zusammenfassung:",
  "",
  capture.output(print(change_summary, n = Inf, width = Inf)),
  "",
  "Richtung der Veränderung von Runde 1 bis Runde 3:",
  "",
  capture.output(print(change_direction_summary, n = Inf, width = Inf)),
  "",
  "10. Exportierte Dateien",
  "",
  paste0("Tabellenordner: ", out_tables_dir),
  paste0("Grafikordner: ", out_figures_dir),
  paste0("Dokumentationsordner: ", out_doc_dir),
  paste0("Kombinierte Excel-Datei: ", file.path(out_base_dir, "12_image_agreement_final_unified_tables.xlsx")),
  "",
  "==================== END OF REPORT ===================="
)

writeLines(
  method_results_report,
  con = file.path(out_doc_dir, "12_image_agreement_method_results_report.txt")
)

# =========================================================
# 17) Lokaler Export-Index                               ===
# =========================================================

table_manifest_csv <- tibble(
  label = paste(names(tables_to_export), "(CSV)"),
  path = file.path(out_tables_dir, paste0(names(tables_to_export), ".csv")),
  notes = "Tabelle als CSV"
)

table_manifest_xlsx <- tibble(
  label = paste(names(tables_to_export), "(XLSX)"),
  path = file.path(out_tables_dir, paste0(names(tables_to_export), ".xlsx")),
  notes = "Tabelle als XLSX"
)

other_manifest <- tibble(
  label = c(
    "Combined workbook",
    "AgreementFig1 mean agreement by round",
    "AgreementFig2 individual agreement trajectories",
    "Model 1 primary round-factor summary TXT",
    "Model 2 controlled Q7 summary TXT",
    "Model 3 ordinal Q7 summary TXT",
    "Console summary",
    "ChatGPT summary"
  ),
  path = c(
    file.path(out_base_dir, "12_image_agreement_final_unified_tables.xlsx"),
    file.path(out_figures_dir, "AgreementFig1_mean_agreement_by_round.png"),
    file.path(out_figures_dir, "AgreementFig2_individual_agreement_trajectories.png"),
    file.path(out_doc_dir, "12_model1_primary_round_factor_summary.txt"),
    file.path(out_doc_dir, "12_model2_controlled_q7_summary.txt"),
    file.path(out_doc_dir, "12_model3_ordinal_q7_summary.txt"),
    file.path(out_doc_dir, "12_image_agreement_final_unified_console_summary.txt"),
    file.path(project_root, "data_output", "RESULTS_FOR_CHATGPT_image_agreement_final_unified.txt")
  ),
  notes = c(
    "Kombinierte Excel-Arbeitsmappe",
    "PNG-Grafik",
    "PNG-Grafik",
    "LMM-Summary als TXT",
    "LMM-Summary als TXT",
    "Ordinales Modell als TXT",
    "Konsolen- und Prüfzusammenfassung",
    "Zusammenfassung für ChatGPT"
  )
)

additional_figure_manifest <- bind_rows(
  tibble(
    label = c(
      "AgreementFig1b mean agreement by round and target category",
      "AgreementFig1c mean mental image change by round and target category"
    ),
    path = c(
      file.path(out_figures_dir, "AgreementFig1b_mean_agreement_by_round_by_target_category.png"),
      file.path(out_figures_dir, "AgreementFig1c_mean_mental_image_change_by_round_by_target_category.png")
    ),
    notes = c(
      "PNG-Grafik",
      "PNG-Grafik"
    )
  ),
  image_agreement_subscale_figure_manifest,
  ordinal_figure_manifest
)

export_manifest <- bind_rows(
  table_manifest_csv,
  table_manifest_xlsx,
  other_manifest,
  additional_figure_manifest
)

save_table_outputs(export_manifest, "00_export_manifest", out_dir = out_doc_dir)

if (exists("build_general_export_index")) {
  build_general_export_index(
    manifest = export_manifest,
    output_path = file.path(out_doc_dir, "00_export_index.html"),
    title_text = "Image agreement final unified analysis: Export index",
    intro_text = "Dieser Unterindex bündelt Tabellen, Grafiken und Dokumentation des finalen Image-Agreement-Skripts."
  )
}

# =========================================================
# 18) Abschlussmeldung                                   ===
# =========================================================

message("Confirmation: Final unified image agreement analysis was exported successfully.")
message("Tables: ", out_tables_dir)
message("Figures: ", out_figures_dir)
message("Additional target-category figure: ", file.path(out_figures_dir, "AgreementFig1b_mean_agreement_by_round_by_target_category.png"))
message("Additional mental-image-change figure: ", file.path(out_figures_dir, "AgreementFig1c_mean_mental_image_change_by_round_by_target_category.png"))
message("Additional subscale figures: ", paste(image_agreement_subscale_figure_manifest$path, collapse = "; "))
message("Ordinal figure variants: ", paste(ordinal_figure_manifest$path, collapse = "; "))
message("Workbook: ", file.path(out_base_dir, "12_image_agreement_final_unified_tables.xlsx"))
message("Local index: ", file.path(out_doc_dir, "00_export_index.html"))
message("Console summary: ", file.path(out_doc_dir, "12_image_agreement_final_unified_console_summary.txt"))
message("ChatGPT summary: ", file.path(project_root, "data_output", "RESULTS_FOR_CHATGPT_image_agreement_final_unified.txt"))

#####################################################################
### End of workflow                                               ###
#####################################################################

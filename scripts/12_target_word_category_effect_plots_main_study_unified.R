#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine.
#
# Ergänzung:
# - Zusätzlich zu den bisherigen Variablen werden nun auch
#   Main_Survey_Q48 bis Main_Survey_Q54 ausgewertet.
# - Die Tabelle 06_target_word_category_comparison_distribution
#   enthält für alle ausgewerteten Variablen die Unterscheidung nach
#   Abstract vs. Concrete.
# - Zusätzlich enthält Tabelle 06 je Segment:
#   valid_n, mean, median, most frequent value,
#   percent most frequent, second most frequent value,
#   percent second most frequent.
#
# Wichtig:
# - Mean und Median werden auf Basis der ordinalen Antwortreihenfolge
#   berechnet.
# - Wenn Variablen als ordered factor gespeichert sind, wird deren
#   Level-Reihenfolge verwendet.
# - Wenn Variablen reine Textvariablen sind, versucht das Skript bekannte
#   Likert-/Similarity-Skalen zu erkennen. Andernfalls wird die Reihenfolge
#   des ersten Auftretens verwendet und eine Warnung ausgegeben.

#####################################################################
### Target-Word-Category-Effekte auf Main-Study-Ergebnisvariablen  ###
#####################################################################

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

out_base_dir    <- file.path(project_root, "data_output", "main_study_target_word_category_effects")
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
  require_pre = FALSE,
  require_main = TRUE,
  require_final = TRUE
)

final_analysis_dataset <- loaded_datasets$final_analysis_dataset
main_feature_lookup <- loaded_datasets$main_feature_lookup

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

normalize_target_word_category <- function(x) {
  x <- normalize_missing_text(x)
  x_lower <- stringr::str_to_lower(x)

  out <- dplyr::case_when(
    x_lower %in% c("abstract", "abstrakt") ~ "Abstract",
    x_lower %in% c("concrete", "konkret") ~ "Concrete",
    TRUE ~ NA_character_
  )

  factor(out, levels = c("Abstract", "Concrete"), ordered = TRUE)
}

make_segment_panel_data <- function(df) {
  df_valid <- df %>%
    mutate(
      target_word_category_segment =
        normalize_target_word_category(.data[["Main_Survey_target_word_category"]])
    ) %>%
    filter(!is.na(target_word_category_segment))

  overall_df <- df_valid %>%
    mutate(
      segment = factor(
        "Overall",
        levels = c("Overall", "Abstract", "Concrete"),
        ordered = TRUE
      )
    )

  segmented_df <- df_valid %>%
    mutate(
      segment = factor(
        as.character(target_word_category_segment),
        levels = c("Overall", "Abstract", "Concrete"),
        ordered = TRUE
      )
    )

  bind_rows(overall_df, segmented_df)
}

make_segment_overview <- function(df) {
  segment_df <- make_segment_panel_data(df)

  segment_df %>%
    count(segment, name = "n") %>%
    mutate(n_label = make_n_label(n))
}

weighted_mean_from_counts <- function(scores, counts) {
  idx <- !is.na(scores) & !is.na(counts) & counts > 0

  if (!any(idx)) {
    return(NA_real_)
  }

  round(stats::weighted.mean(scores[idx], w = counts[idx]), 2)
}

weighted_median_from_counts <- function(scores, counts) {
  idx <- !is.na(scores) & !is.na(counts) & counts > 0

  if (!any(idx)) {
    return(NA_real_)
  }

  expanded_values <- rep(scores[idx], times = as.integer(counts[idx]))

  if (length(expanded_values) == 0) {
    return(NA_real_)
  }

  round(stats::median(expanded_values), 2)
}

get_ranked_frequency <- function(response, counts, scores, rank = 1, output = "value") {
  tmp <- tibble::tibble(
    response = as.character(response),
    counts = as.numeric(counts),
    scores = as.numeric(scores)
  ) %>%
    filter(!is.na(response), !is.na(counts), counts > 0) %>%
    arrange(desc(counts), scores)

  if (nrow(tmp) < rank) {
    if (output == "value") {
      return(NA_character_)
    } else {
      return(NA_real_)
    }
  }

  if (output == "value") {
    return(tmp$response[rank])
  }

  if (output == "n") {
    return(tmp$counts[rank])
  }

  if (output == "percent") {
    return(pct(tmp$counts[rank], sum(tmp$counts, na.rm = TRUE)))
  }

  NA
}

# =========================================================
# 3.1) Antwortlevel für Q48-Q54 und andere Variablen     ===
# =========================================================

known_similarity_levels_6 <- c(
  "Completely different",
  "Quite different",
  "Somewhat different",
  "Somewhat similar",
  "Quite similar",
  "Essentially the same"
)

known_similarity_levels_7 <- c(
  "Completely different",
  "Very different",
  "Quite different",
  "Somewhat different",
  "Somewhat similar",
  "Quite similar",
  "Essentially the same"
)

known_similarity_levels_alt_7 <- c(
  "Not at all similar",
  "Very weakly similar",
  "Weakly similar",
  "Moderately similar",
  "Strongly similar",
  "Very strongly similar",
  "Almost exactly the same"
)

known_ordinal_level_sets <- list(
  agreement_levels,
  change_levels,
  three_ita_levels,
  known_similarity_levels_6,
  known_similarity_levels_7,
  known_similarity_levels_alt_7
)

infer_response_levels <- function(df, var_name) {
  if (!var_name %in% names(df)) {
    warning("Variable not found and will be skipped: ", var_name, call. = FALSE)
    return(character(0))
  }

  x <- df[[var_name]]

  if (is.factor(x)) {
    lev <- levels(x)
    lev <- normalize_missing_text(lev)
    lev <- lev[!is.na(lev)]
    lev <- setdiff(lev, "Missing")

    if (length(lev) > 0) {
      return(lev)
    }
  }

  x_norm <- normalize_missing_text(x)
  observed <- unique(x_norm[!is.na(x_norm)])

  if (length(observed) == 0) {
    return(character(0))
  }

  for (level_set in known_ordinal_level_sets) {
    if (all(observed %in% level_set)) {
      return(level_set)
    }
  }

  suppressWarnings(observed_numeric <- as.numeric(observed))

  if (all(!is.na(observed_numeric))) {
    return(observed[order(observed_numeric)])
  }

  warning(
    paste0(
      "No explicit response order found for ", var_name,
      ". Using order of first appearance. Check whether this is correct for mean/median."
    ),
    call. = FALSE
  )

  observed
}

get_custom_response_levels <- function(var_name, response_levels_map = NULL) {
  if (is.null(response_levels_map)) {
    return(NULL)
  }

  if (is.list(response_levels_map) && var_name %in% names(response_levels_map)) {
    return(response_levels_map[[var_name]])
  }

  if (is.character(response_levels_map) && is.null(names(response_levels_map))) {
    return(response_levels_map)
  }

  NULL
}

get_response_levels_for_variable <- function(df, var_name, response_levels_map = NULL) {
  custom_levels <- get_custom_response_levels(var_name, response_levels_map)

  if (!is.null(custom_levels) && length(custom_levels) > 0) {
    return(as.character(custom_levels))
  }

  infer_response_levels(df, var_name)
}

# Optionaler Override:
# Falls Q48-Q54 als reine Textvariablen gespeichert sind und die automatische
# Erkennung nicht passt, kann vor Ausführung dieses Skripts ein Objekt
# `final_question_response_levels` als named list definiert werden, z. B.:
#
# final_question_response_levels <- list(
#   Main_Survey_Q48 = c("Completely different", "Quite different", "Somewhat different",
#                      "Somewhat similar", "Quite similar", "Essentially the same")
# )
#
# Wenn kein solches Objekt existiert, nutzt das Skript automatische Erkennung.

final_question_response_levels <- if (exists("final_question_response_levels", envir = .GlobalEnv, inherits = FALSE)) {
  final_question_response_levels
} else {
  NULL
}

# =========================================================
# 3.2) Distribution-Funktionen                           ===
# =========================================================

make_category_iteration_distribution <- function(df, var_map, response_levels, question_family) {
  segment_df <- make_segment_panel_data(df)

  missing_vars <- setdiff(unname(var_map), names(df))

  if (length(missing_vars) > 0) {
    warning(
      paste0(
        "The following variables are missing and will be skipped: ",
        paste(missing_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  var_map <- var_map[unname(var_map) %in% names(df)]

  if (length(var_map) == 0) {
    return(tibble::tibble())
  }

  purrr::imap_dfr(
    var_map,
    function(var_name, iteration_label) {
      question_text <- get_question_text(main_feature_lookup, var_name)

      segment_df %>%
        transmute(
          segment,
          response = normalize_missing_text(.data[[var_name]])
        ) %>%
        mutate(response = if_else(is.na(response), "Missing", response)) %>%
        mutate(response = factor(response, levels = c(response_levels, "Missing"), ordered = TRUE)) %>%
        count(segment, response, name = "n", .drop = FALSE) %>%
        group_by(segment) %>%
        mutate(n_group_total = sum(n)) %>%
        ungroup() %>%
        mutate(
          iteration = iteration_label,
          variable_name = var_name,
          question_family = question_family,
          question_text = question_text,
          n_group_label = make_n_label(n_group_total),
          n_used_total = max(n_group_total[segment == "Overall"], na.rm = TRUE),
          n_used_total_label = make_n_label(n_used_total),
          percent = pct(n, n_group_total),
          .before = 1
        ) %>%
        mutate(response = as.character(response))
    }
  )
}

make_category_block_distribution <- function(df, block_map, response_levels, question_family) {
  segment_df <- make_segment_panel_data(df)

  purrr::imap_dfr(
    block_map,
    function(var_names, iteration_label) {
      missing_vars <- setdiff(var_names, names(df))

      if (length(missing_vars) > 0) {
        warning(
          paste0(
            "The following variables are missing and will be skipped: ",
            paste(missing_vars, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      var_names <- var_names[var_names %in% names(df)]

      if (length(var_names) == 0) {
        return(tibble::tibble())
      }

      purrr::imap_dfr(
        var_names,
        function(var_name, item_index) {
          item_label <- paste0("Item_", item_index)
          question_text <- get_question_text(main_feature_lookup, var_name)

          segment_df %>%
            transmute(
              segment,
              response = normalize_missing_text(.data[[var_name]])
            ) %>%
            mutate(response = if_else(is.na(response), "Missing", response)) %>%
            mutate(response = factor(response, levels = c(response_levels, "Missing"), ordered = TRUE)) %>%
            count(segment, response, name = "n", .drop = FALSE) %>%
            group_by(segment) %>%
            mutate(n_group_total = sum(n)) %>%
            ungroup() %>%
            mutate(
              iteration = iteration_label,
              item = item_label,
              variable_name = var_name,
              question_family = question_family,
              question_text = question_text,
              n_group_label = make_n_label(n_group_total),
              n_used_total = max(n_group_total[segment == "Overall"], na.rm = TRUE),
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

make_category_variable_distribution <- function(df, var_map, question_family, response_levels_map = NULL) {
  segment_df <- make_segment_panel_data(df)

  missing_vars <- setdiff(unname(var_map), names(df))

  if (length(missing_vars) > 0) {
    warning(
      paste0(
        "The following variables are missing and will be skipped: ",
        paste(missing_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  var_map <- var_map[unname(var_map) %in% names(df)]

  if (length(var_map) == 0) {
    return(tibble::tibble())
  }

  purrr::imap_dfr(
    var_map,
    function(var_name, question_label) {
      response_levels <- get_response_levels_for_variable(
        df = df,
        var_name = var_name,
        response_levels_map = response_levels_map
      )

      question_text <- get_question_text(main_feature_lookup, var_name)

      segment_df %>%
        transmute(
          segment,
          response = normalize_missing_text(.data[[var_name]])
        ) %>%
        mutate(response = if_else(is.na(response), "Missing", response)) %>%
        mutate(response = factor(response, levels = c(response_levels, "Missing"), ordered = TRUE)) %>%
        count(segment, response, name = "n", .drop = FALSE) %>%
        group_by(segment) %>%
        mutate(n_group_total = sum(n)) %>%
        ungroup() %>%
        mutate(
          iteration = question_label,
          item = NA_character_,
          variable_name = var_name,
          question_family = question_family,
          question_text = question_text,
          n_group_label = make_n_label(n_group_total),
          n_used_total = max(n_group_total[segment == "Overall"], na.rm = TRUE),
          n_used_total_label = make_n_label(n_used_total),
          percent = pct(n, n_group_total),
          .before = 1
        ) %>%
        mutate(response = as.character(response))
    }
  )
}

# =========================================================
# 3.3) Plot-Funktionen                                   ===
# =========================================================

make_segment_facet_labels <- function(distribution_table) {
  distribution_table %>%
    distinct(segment, n_group_total) %>%
    mutate(label = paste0(as.character(segment), "\n", make_n_label(n_group_total))) %>%
    { stats::setNames(.$label, .$segment) }
}

make_plot_subtitle <- function(distribution_table) {
  n_used_total <- distribution_table %>%
    dplyr::pull(n_used_total) %>%
    unique()

  n_used_total <- n_used_total[!is.na(n_used_total)]
  n_used_text <- if (length(n_used_total) == 0) NA_character_ else make_n_label(n_used_total[1])

  bits <- c(
    "Segmented by Main_Survey_target_word_category: Overall, Abstract, Concrete",
    if (!is.na(n_used_text)) paste0("Total used cases: ", n_used_text) else NA_character_
  )

  bits <- bits[!is.na(bits) & bits != ""]
  paste(bits, collapse = " | ")
}

make_category_longitudinal_plot <- function(distribution_table, title_text, subtitle_text = NULL, response_levels = NULL) {
  plot_data <- distribution_table %>%
    filter(response != "Missing") %>%
    mutate(response = as.character(response))

  if (!is.null(response_levels)) {
    plot_data <- plot_data %>%
      mutate(response = factor(response, levels = response_levels, ordered = TRUE))
  }

  facet_labels <- make_segment_facet_labels(distribution_table)

  ggplot(plot_data, aes(x = iteration, y = percent, fill = response)) +
    geom_col(position = "stack") +
    facet_wrap(~ segment, labeller = labeller(segment = as_labeller(facet_labels))) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL,
      y = "Percent within segment",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}

make_category_block_plot <- function(distribution_table, title_text, subtitle_text = NULL, response_levels = NULL) {
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

  facet_labels <- make_segment_facet_labels(distribution_table)

  ggplot(plot_data, aes(x = iteration, y = percent, fill = response)) +
    geom_col(position = "stack") +
    facet_grid(
      segment ~ item,
      labeller = labeller(
        segment = as_labeller(facet_labels),
        item = as_labeller(item_labels)
      )
    ) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL,
      y = "Percent within segment",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_result()
}

# =========================================================
# 3.4) HTML-Hilfsfunktionen                              ===
# =========================================================

html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "&", "&amp;")
  x <- stringr::str_replace_all(x, "<", "&lt;")
  x <- stringr::str_replace_all(x, ">", "&gt;")
  x <- stringr::str_replace_all(x, '"', "&quot;")
  x
}

format_html_value <- function(x) {
  if (inherits(x, "Date")) {
    out <- format(x)
  } else if (is.numeric(x)) {
    out <- ifelse(is.na(x), "", format(x, trim = TRUE, scientific = FALSE))
  } else {
    out <- as.character(x)
    out[is.na(out)] <- ""
  }

  out
}

ensure_columns <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))

  for (col in missing_cols) {
    df[[col]] <- NA
  }

  df
}

write_html_table <- function(df, output_path, title_text, intro_text = NULL) {
  df_for_html <- as.data.frame(df, stringsAsFactors = FALSE)

  header <- paste0(
    "<tr>",
    paste0("<th>", html_escape(names(df_for_html)), "</th>", collapse = ""),
    "</tr>"
  )

  rows <- purrr::map_chr(seq_len(nrow(df_for_html)), function(i) {
    row_values <- vapply(
      df_for_html[i, , drop = FALSE],
      function(col) format_html_value(col[[1]]),
      character(1)
    )

    paste0(
      "<tr>",
      paste0("<td>", html_escape(row_values), "</td>", collapse = ""),
      "</tr>"
    )
  })

  html <- paste0(
    "<!DOCTYPE html>
<html>
<head>
<meta charset='UTF-8'>
<title>", html_escape(title_text), "</title>
<style>
body {
  font-family: Arial, Helvetica, sans-serif;
  margin: 24px;
  color: #222;
  background: #ffffff;
}
h1 {
  font-size: 22px;
  margin-bottom: 6px;
  font-weight: 600;
}
p {
  font-size: 14px;
  color: #555;
  max-width: 1100px;
  line-height: 1.45;
}
.table-wrapper {
  overflow-x: auto;
  margin-top: 20px;
  border-top: 2px solid #b5b5b5;
}
table {
  border-collapse: collapse;
  width: 100%;
  font-size: 13px;
}
th, td {
  border-bottom: 1px solid #d9d9d9;
  padding: 7px 9px;
  text-align: left;
  vertical-align: top;
  white-space: nowrap;
}
th {
  background-color: #f3f3f3;
  font-weight: 600;
  position: sticky;
  top: 0;
  z-index: 1;
}
tr:nth-child(even) {
  background-color: #fafafa;
}
td:last-child {
  white-space: normal;
  min-width: 320px;
}
</style>
</head>
<body>
<h1>", html_escape(title_text), "</h1>",
if (!is.null(intro_text)) paste0("<p>", html_escape(intro_text), "</p>") else "",
"<div class='table-wrapper'>
<table>",
header,
paste(rows, collapse = "\n"),
"</table>
</div>
</body>
</html>"
  )

  writeLines(html, con = output_path)
}

# =========================================================
# 3.5) Comparison-Tabelle mit Lagemaßen                  ===
# =========================================================

make_target_word_category_comparison_table <- function(distribution_table) {
  dist <- distribution_table %>%
    mutate(
      row_id = dplyr::row_number(),
      segment = as.character(segment),
      item = if_else(is.na(item), "", as.character(item)),
      response = as.character(response)
    )

  base <- dist %>%
    filter(
      response != "Missing",
      segment %in% c("Abstract", "Concrete")
    ) %>%
    group_by(question_family, iteration, item, variable_name, question_text, segment) %>%
    arrange(row_id, .by_group = TRUE) %>%
    mutate(
      response_score = dplyr::row_number(),
      valid_n = sum(n, na.rm = TRUE),
      percent_valid = pct(n, valid_n),
      n_percent_valid = if_else(
        is.na(percent_valid),
        paste0(n, " (NA)"),
        paste0(n, " (", percent_valid, "%)")
      )
    ) %>%
    ungroup()

  if (nrow(base) == 0) {
    return(
      tibble::tibble(
        question_family = character(),
        iteration = character(),
        item = character(),
        variable_name = character(),
        response = character(),
        Abstract_valid_n = integer(),
        Abstract_mean = numeric(),
        Abstract_median = numeric(),
        Abstract_most_frequent_value = character(),
        Abstract_n_most_frequent_value = numeric(),
        Abstract_percent_most_frequent = numeric(),
        Abstract_second_most_frequent_value = character(),
        Abstract_n_second_most_frequent_value = numeric(),
        Abstract_percent_second_most_frequent = numeric(),
        Abstract_n_percent = character(),
        Concrete_valid_n = integer(),
        Concrete_mean = numeric(),
        Concrete_median = numeric(),
        Concrete_most_frequent_value = character(),
        Concrete_n_most_frequent_value = numeric(),
        Concrete_percent_most_frequent = numeric(),
        Concrete_second_most_frequent_value = character(),
        Concrete_n_second_most_frequent_value = numeric(),
        Concrete_percent_second_most_frequent = numeric(),
        Concrete_n_percent = character(),
        question_text = character()
      )
    )
  }

  id_cols <- c(
    "question_family",
    "iteration",
    "item",
    "variable_name",
    "question_text",
    "response"
  )

  row_order <- base %>%
    group_by(across(all_of(id_cols))) %>%
    summarise(row_order = min(row_id), .groups = "drop")

  segment_stats <- base %>%
    group_by(question_family, iteration, item, variable_name, question_text, segment) %>%
    summarise(
      valid_n = sum(n, na.rm = TRUE),
      mean = weighted_mean_from_counts(response_score, n),
      median = weighted_median_from_counts(response_score, n),
      most_frequent_value = get_ranked_frequency(response, n, response_score, rank = 1, output = "value"),
      n_most_frequent_value = get_ranked_frequency(response, n, response_score, rank = 1, output = "n"),
      percent_most_frequent = get_ranked_frequency(response, n, response_score, rank = 1, output = "percent"),
      second_most_frequent_value = get_ranked_frequency(response, n, response_score, rank = 2, output = "value"),
      n_second_most_frequent_value = get_ranked_frequency(response, n, response_score, rank = 2, output = "n"),
      percent_second_most_frequent = get_ranked_frequency(response, n, response_score, rank = 2, output = "percent"),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = segment,
      values_from = c(
        valid_n,
        mean,
        median,
        most_frequent_value,
        n_most_frequent_value,
        percent_most_frequent,
        second_most_frequent_value,
        n_second_most_frequent_value,
        percent_second_most_frequent
      ),
      names_glue = "{segment}_{.value}"
    )

  value_table <- base %>%
    select(
      question_family,
      iteration,
      item,
      variable_name,
      question_text,
      response,
      segment,
      n_percent_valid
    ) %>%
    tidyr::pivot_wider(
      names_from = segment,
      values_from = n_percent_valid,
      names_glue = "{segment}_n_percent",
      values_fill = list(n_percent_valid = "0 (0%)")
    )

  out <- value_table %>%
    left_join(
      segment_stats,
      by = c(
        "question_family",
        "iteration",
        "item",
        "variable_name",
        "question_text"
      )
    ) %>%
    left_join(row_order, by = id_cols)

  out <- ensure_columns(
    out,
    c(
      "Abstract_valid_n",
      "Abstract_mean",
      "Abstract_median",
      "Abstract_most_frequent_value",
      "Abstract_n_most_frequent_value",
      "Abstract_percent_most_frequent",
      "Abstract_second_most_frequent_value",
      "Abstract_n_second_most_frequent_value",
      "Abstract_percent_second_most_frequent",
      "Abstract_n_percent",
      "Concrete_valid_n",
      "Concrete_mean",
      "Concrete_median",
      "Concrete_most_frequent_value",
      "Concrete_n_most_frequent_value",
      "Concrete_percent_most_frequent",
      "Concrete_second_most_frequent_value",
      "Concrete_n_second_most_frequent_value",
      "Concrete_percent_second_most_frequent",
      "Concrete_n_percent"
    )
  )

  out %>%
    arrange(row_order) %>%
    select(
      question_family,
      iteration,
      item,
      variable_name,
      response,
      Abstract_valid_n,
      Abstract_mean,
      Abstract_median,
      Abstract_most_frequent_value,
      Abstract_n_most_frequent_value,
      Abstract_percent_most_frequent,
      Abstract_second_most_frequent_value,
      Abstract_n_second_most_frequent_value,
      Abstract_percent_second_most_frequent,
      Abstract_n_percent,
      Concrete_valid_n,
      Concrete_mean,
      Concrete_median,
      Concrete_most_frequent_value,
      Concrete_n_most_frequent_value,
      Concrete_percent_most_frequent,
      Concrete_second_most_frequent_value,
      Concrete_n_second_most_frequent_value,
      Concrete_percent_second_most_frequent,
      Concrete_n_percent,
      question_text
    )
}

# =========================================================
# 4) Tabellen für Segmentierung und Verteilungen         ===
# =========================================================

target_word_category_overview <- make_segment_overview(analysis_df)

category_group_1_distribution <- make_category_iteration_distribution(
  df = analysis_df,
  var_map = c(
    Iteration_1 = "Main_Survey_Q26",
    Iteration_2 = "Main_Survey_Q34",
    Iteration_3 = "Main_Survey_Q42"
  ),
  response_levels = agreement_levels,
  question_family = "Image agreement (overall)"
)

category_group_2_distribution <- make_category_iteration_distribution(
  df = analysis_df,
  var_map = c(
    Iteration_1 = "Main_Survey_Q28",
    Iteration_2 = "Main_Survey_Q36",
    Iteration_3 = "Main_Survey_Q44"
  ),
  response_levels = change_levels,
  question_family = "Change in mental image"
)

category_block_distribution <- make_category_block_distribution(
  df = analysis_df,
  block_map = list(
    Iteration_1 = c("Main_Survey_Q27_1", "Main_Survey_Q27_2", "Main_Survey_Q27_3"),
    Iteration_2 = c("Main_Survey_Q35_1", "Main_Survey_Q35_2", "Main_Survey_Q35_3"),
    Iteration_3 = c("Main_Survey_Q43_1", "Main_Survey_Q43_2", "Main_Survey_Q43_3")
  ),
  response_levels = three_ita_levels,
  question_family = "Image agreement (subscales)"
)

category_final_questions_distribution <- make_category_variable_distribution(
  df = analysis_df,
  var_map = c(
    Q48 = "Main_Survey_Q48",
    Q49 = "Main_Survey_Q49",
    Q50 = "Main_Survey_Q50",
    Q51 = "Main_Survey_Q51",
    Q52 = "Main_Survey_Q52",
    Q53 = "Main_Survey_Q53",
    Q54 = "Main_Survey_Q54"
  ),
  question_family = "Final Main-Survey questions Q48-Q54",
  response_levels_map = final_question_response_levels
)

target_word_category_analysis_n_overview <- bind_rows(
  category_group_1_distribution %>%
    distinct(question_family, variable_name, iteration, n_used_total, n_used_total_label) %>%
    mutate(item = NA_character_),
  category_group_2_distribution %>%
    distinct(question_family, variable_name, iteration, n_used_total, n_used_total_label) %>%
    mutate(item = NA_character_),
  category_block_distribution %>%
    distinct(question_family, variable_name, iteration, item, n_used_total, n_used_total_label),
  category_final_questions_distribution %>%
    distinct(question_family, variable_name, iteration, item, n_used_total, n_used_total_label)
) %>%
  arrange(question_family, iteration, item, variable_name)

target_word_category_all_distribution <- bind_rows(
  category_group_1_distribution %>%
    mutate(item = NA_character_),
  category_group_2_distribution %>%
    mutate(item = NA_character_),
  category_block_distribution,
  category_final_questions_distribution
)

target_word_category_comparison_distribution <- make_target_word_category_comparison_table(
  target_word_category_all_distribution
)

# =========================================================
# 5) Plots erstellen                                     ===
# =========================================================

plot_category_group_1 <- make_category_longitudinal_plot(
  category_group_1_distribution,
  title_text = "Distribution of image agreement (overall) across three iterations by target word category (overall, abstract, concrete)",
  subtitle_text = make_plot_subtitle(category_group_1_distribution),
  response_levels = agreement_levels
)

plot_category_group_2 <- make_category_longitudinal_plot(
  category_group_2_distribution,
  title_text = "Distribution of change in the mental image across three iterations by target word category (overall, abstract, concrete)",
  subtitle_text = make_plot_subtitle(category_group_2_distribution),
  response_levels = change_levels
)

plot_category_block <- make_category_block_plot(
  category_block_distribution,
  title_text = "Distribution of image agreement subscales across three iterations by target word category (overall, abstract, concrete)",
  subtitle_text = make_plot_subtitle(category_block_distribution),
  response_levels = three_ita_levels
)

# =========================================================
# 6) Export Tabellen                                     ===
# =========================================================

save_table_outputs(target_word_category_overview, "01_target_word_category_overview")
save_table_outputs(target_word_category_analysis_n_overview, "02_target_word_category_analysis_n_overview")
save_table_outputs(category_group_1_distribution, "03_target_word_category_group_1_distribution")
save_table_outputs(category_group_2_distribution, "04_target_word_category_group_2_distribution")
save_table_outputs(category_block_distribution, "05_target_word_category_block_distribution")
save_table_outputs(target_word_category_comparison_distribution, "06_target_word_category_comparison_distribution")
save_table_outputs(category_final_questions_distribution, "07_target_word_category_final_questions_distribution")

write_html_table(
  df = target_word_category_overview,
  output_path = file.path(out_tables_dir, "01_target_word_category_overview.html"),
  title_text = "Target word category overview",
  intro_text = "Overview of the number of cases in the Overall, Abstract, and Concrete segments."
)

write_html_table(
  df = target_word_category_comparison_distribution,
  output_path = file.path(out_tables_dir, "06_target_word_category_comparison_distribution.html"),
  title_text = "Distribution and summary statistics of Main-Survey variables by target word category",
  intro_text = "This table shows valid response distributions separately for Abstract and Concrete target-word categories. It also includes valid N, mean, median, most frequent value, and second most frequent value for each segment. Main_Survey_Q48 to Main_Survey_Q54 are included. Cells in the response columns contain n and percent within the respective valid category segment; missing responses are excluded from the displayed percentages and summary statistics."
)

write_html_table(
  df = category_final_questions_distribution,
  output_path = file.path(out_tables_dir, "07_target_word_category_final_questions_distribution.html"),
  title_text = "Raw distribution of Main_Survey_Q48 to Main_Survey_Q54 by target word category",
  intro_text = "This table contains the long-format response distributions for Main_Survey_Q48 to Main_Survey_Q54 by Overall, Abstract, and Concrete segments."
)

writexl::write_xlsx(
  list(
    overview = target_word_category_overview,
    analysis_n_overview = target_word_category_analysis_n_overview,
    group_1_distribution = category_group_1_distribution,
    group_2_distribution = category_group_2_distribution,
    block_distribution = category_block_distribution,
    final_questions_distribution = category_final_questions_distribution,
    category_comparison = target_word_category_comparison_distribution
  ),
  path = file.path(out_base_dir, "12_target_word_category_effect_tables.xlsx")
)

# =========================================================
# 7) Export Grafiken                                     ===
# =========================================================

ggsave(
  file.path(out_figures_dir, "TargetWordCategoryFig1_longitudinal_group_1.png"),
  plot_category_group_1,
  width = 11,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "TargetWordCategoryFig2_longitudinal_group_2.png"),
  plot_category_group_2,
  width = 11,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "TargetWordCategoryFig3_longitudinal_block.png"),
  plot_category_block,
  width = 13,
  height = 9,
  dpi = 300
)

# =========================================================
# 8) Konsolen- und Dokumentationsausgabe                 ===
# =========================================================

console_summary <- c(
  "==================== TARGET WORD CATEGORY EFFECT PLOTS ====================",
  "",
  "Target word category overview:",
  capture.output(print(target_word_category_overview)),
  "",
  "Analysis N overview:",
  capture.output(print(target_word_category_analysis_n_overview)),
  "",
  "Final questions Q48-Q54 distribution preview:",
  capture.output(print(utils::head(category_final_questions_distribution, 30))),
  "",
  "Abstract vs. Concrete distribution and summary statistics table preview:",
  capture.output(print(utils::head(target_word_category_comparison_distribution, 30))),
  "",
  "Exported figures:",
  paste(
    c(
      file.path(out_figures_dir, "TargetWordCategoryFig1_longitudinal_group_1.png"),
      file.path(out_figures_dir, "TargetWordCategoryFig2_longitudinal_group_2.png"),
      file.path(out_figures_dir, "TargetWordCategoryFig3_longitudinal_block.png")
    ),
    collapse = "\n"
  ),
  "",
  "Exported HTML tables:",
  paste(
    c(
      file.path(out_tables_dir, "01_target_word_category_overview.html"),
      file.path(out_tables_dir, "06_target_word_category_comparison_distribution.html"),
      file.path(out_tables_dir, "07_target_word_category_final_questions_distribution.html")
    ),
    collapse = "\n"
  ),
  "",
  "Exported workbook:",
  file.path(out_base_dir, "12_target_word_category_effect_tables.xlsx")
)

writeLines(
  console_summary,
  con = file.path(out_doc_dir, "12_target_word_category_effect_console_summary.txt")
)

# =========================================================
# 9) Lokaler Export-Index                                ===
# =========================================================

export_manifest <- tibble::tibble(
  label = c(
    "Target word category overview (CSV)",
    "Target word category overview (XLSX)",
    "Target word category overview (HTML)",
    "Target word category analysis N overview (CSV)",
    "Target word category analysis N overview (XLSX)",
    "Target word category group 1 distribution (CSV)",
    "Target word category group 1 distribution (XLSX)",
    "Target word category group 2 distribution (CSV)",
    "Target word category group 2 distribution (XLSX)",
    "Target word category block distribution (CSV)",
    "Target word category block distribution (XLSX)",
    "Target word category comparison distribution with summary statistics (CSV)",
    "Target word category comparison distribution with summary statistics (XLSX)",
    "Target word category comparison distribution with summary statistics (HTML)",
    "Target word category final questions Q48-Q54 distribution (CSV)",
    "Target word category final questions Q48-Q54 distribution (XLSX)",
    "Target word category final questions Q48-Q54 distribution (HTML)",
    "Combined workbook",
    "TargetWordCategoryFig1 longitudinal group 1",
    "TargetWordCategoryFig2 longitudinal group 2",
    "TargetWordCategoryFig3 longitudinal block",
    "Console summary"
  ),
  path = c(
    file.path(out_tables_dir, "01_target_word_category_overview.csv"),
    file.path(out_tables_dir, "01_target_word_category_overview.xlsx"),
    file.path(out_tables_dir, "01_target_word_category_overview.html"),
    file.path(out_tables_dir, "02_target_word_category_analysis_n_overview.csv"),
    file.path(out_tables_dir, "02_target_word_category_analysis_n_overview.xlsx"),
    file.path(out_tables_dir, "03_target_word_category_group_1_distribution.csv"),
    file.path(out_tables_dir, "03_target_word_category_group_1_distribution.xlsx"),
    file.path(out_tables_dir, "04_target_word_category_group_2_distribution.csv"),
    file.path(out_tables_dir, "04_target_word_category_group_2_distribution.xlsx"),
    file.path(out_tables_dir, "05_target_word_category_block_distribution.csv"),
    file.path(out_tables_dir, "05_target_word_category_block_distribution.xlsx"),
    file.path(out_tables_dir, "06_target_word_category_comparison_distribution.csv"),
    file.path(out_tables_dir, "06_target_word_category_comparison_distribution.xlsx"),
    file.path(out_tables_dir, "06_target_word_category_comparison_distribution.html"),
    file.path(out_tables_dir, "07_target_word_category_final_questions_distribution.csv"),
    file.path(out_tables_dir, "07_target_word_category_final_questions_distribution.xlsx"),
    file.path(out_tables_dir, "07_target_word_category_final_questions_distribution.html"),
    file.path(out_base_dir, "12_target_word_category_effect_tables.xlsx"),
    file.path(out_figures_dir, "TargetWordCategoryFig1_longitudinal_group_1.png"),
    file.path(out_figures_dir, "TargetWordCategoryFig2_longitudinal_group_2.png"),
    file.path(out_figures_dir, "TargetWordCategoryFig3_longitudinal_block.png"),
    file.path(out_doc_dir, "12_target_word_category_effect_console_summary.txt")
  ),
  notes = c(
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "HTML-Übersicht der Segmentgrößen",
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "Tabelle als CSV",
    "Tabelle als XLSX",
    "Abstract-vs.-Concrete-Verteilung inklusive Mean, Median und Häufigkeitswerten als CSV; enthält auch Q48-Q54",
    "Abstract-vs.-Concrete-Verteilung inklusive Mean, Median und Häufigkeitswerten als XLSX; enthält auch Q48-Q54",
    "Abstract-vs.-Concrete-Verteilung inklusive Mean, Median und Häufigkeitswerten als HTML-Tabelle; enthält auch Q48-Q54",
    "Long-format-Verteilung für Q48-Q54 als CSV",
    "Long-format-Verteilung für Q48-Q54 als XLSX",
    "Long-format-Verteilung für Q48-Q54 als HTML",
    "Kombinierte Excel-Arbeitsmappe",
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
  title_text = "Target-word-category effect plots: Export index",
  intro_text = "Dieser Unterindex bündelt Tabellen, Grafiken und Dokumentation des Skripts 12. Tabelle 06 enthält die valide Antwortverteilung nach Abstract vs. Concrete sowie Mean, Median, häufigsten und zweithäufigsten Wert je Segment. Main_Survey_Q48 bis Main_Survey_Q54 sind eingeschlossen."
)

message("Confirmation: Target-word-category effect plots for the main study were exported successfully.")
message("Figures: ", out_figures_dir)
message("Tables: ", out_tables_dir)
message("HTML comparison table with Q48-Q54 and summary statistics: ", file.path(out_tables_dir, "06_target_word_category_comparison_distribution.html"))
message("Final questions Q48-Q54 distribution: ", file.path(out_tables_dir, "07_target_word_category_final_questions_distribution.html"))
message("Workbook: ", file.path(out_base_dir, "12_target_word_category_effect_tables.xlsx"))
message("Console summary: ", file.path(out_doc_dir, "12_target_word_category_effect_console_summary.txt"))

#####################################################################
### End of workflow                                               ###
#####################################################################

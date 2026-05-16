#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine.
#
# Alle Bezüge zum gelöschten Hypothesenanalyse-Skript
# `04_hypothesenanalyse_hauptteil_unified_v2.R` wurden entfernt.
# Das Skript basiert nun ausschließlich auf den Reporting-/
# Deskriptiv-Outputs aus Skript 05.

#####################################################################
### Main study reporting results as GT tables                      ###
#####################################################################

### DESCRIPTION ###

# This script builds publication-ready gt tables for the main study
# reporting results. It is designed as a presentation layer on top of
# the existing project script for:
# - descriptive and reporting outputs
#
# Workflow:
# - source the reporting script when needed
# - collect the main-study reporting result tables that already exist
#   in memory or are created by the reporting script
# - create formatted gt tables for each result object
# - save each gt table as HTML
# - try to save each gt table as RTF as well
# - export a manifest and a simple HTML index of all gt outputs

# =========================================================
# 0) Packages                                           ===
# =========================================================

# install.packages(c("tidyverse", "gt", "here", "readr"), dependencies = TRUE)

library(tidyverse)
library(gt)
library(here)
library(readr)

# =========================================================
# 1) Paths and output folders                           ===
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

reporting_script_candidates <- c(
  file.path(project_root, "scripts", "06_deskriptive_statistik_und_reporting_final_konsolidiert_erweitert_unified.R"),
  file.path(project_root, "06_deskriptive_statistik_und_reporting_final_konsolidiert_erweitert_unified.R")
)

reporting_candidates_existing <- reporting_script_candidates[file.exists(reporting_script_candidates)]

reporting_script_path <- if (length(reporting_candidates_existing) == 0) {
  NA_character_
} else {
  reporting_candidates_existing[[1]]
}

if (is.na(reporting_script_path)) {
  stop(
    paste0(
      "The reporting script could not be found. Expected one of these locations:\n",
      paste(reporting_script_candidates, collapse = "\n")
    ),
    call. = FALSE
  )
}

out_base_dir    <- file.path(project_root, "data_output", "main_study_results")
out_gt_dir      <- file.path(out_base_dir, "gt_tables")
out_gt_html_dir <- file.path(out_gt_dir, "html")
out_gt_rtf_dir  <- file.path(out_gt_dir, "rtf")
out_gt_docx_dir <- file.path(out_gt_dir, "docx")
out_gt_doc_dir  <- file.path(out_gt_dir, "documentation")

purrr::walk(
  c(out_base_dir, out_gt_dir, out_gt_html_dir, out_gt_rtf_dir, out_gt_docx_dir, out_gt_doc_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# =========================================================
# 2) Ensure main-study reporting objects exist           ===
# =========================================================

required_reporting_objects <- c(
  "main_numeric_desc",
  "main_categorical_desc",
  "target_word_distribution",
  "target_word_category_distribution",
  "longitudinal_group_1_variable_summary_table",
  "longitudinal_group_1_distribution",
  "longitudinal_group_1_change",
  "longitudinal_group_2_variable_summary_table",
  "longitudinal_group_2_distribution",
  "longitudinal_group_2_change",
  "longitudinal_group_3_variable_summary_table",
  "longitudinal_group_3_distribution",
  "longitudinal_group_3_change",
  "longitudinal_block_variable_summary_table",
  "longitudinal_block_distribution",
  "longitudinal_block_change",
  "requested_main_single_variable_summary_table",
  "requested_main_single_frequency_table",
  "requested_main_q54_top3_table"
)

if (!all(vapply(required_reporting_objects, exists, logical(1), envir = .GlobalEnv, inherits = FALSE))) {
  message("Confirmation: Loading the reporting script: ", reporting_script_path)
  source(reporting_script_path, local = .GlobalEnv)
}

candidate_objects <- c(
  "email_matching_overview",
  "viviq_total_summary_table",
  "viviq_item_summary_table",
  "final_analysis_overview",
  "main_numeric_desc",
  "main_categorical_desc",
  "target_word_distribution",
  "target_word_category_distribution",
  "target_word_category_check",
  "unmapped_target_words",
  "longitudinal_group_1_variable_summary_table",
  "longitudinal_group_1_distribution",
  "longitudinal_group_1_change",
  "longitudinal_group_2_variable_summary_table",
  "longitudinal_group_2_distribution",
  "longitudinal_group_2_change",
  "longitudinal_group_3_variable_summary_table",
  "longitudinal_group_3_distribution",
  "longitudinal_group_3_change",
  "longitudinal_block_variable_summary_table",
  "longitudinal_block_distribution",
  "longitudinal_block_change",
  "requested_main_single_variable_summary_table",
  "requested_main_single_frequency_table",
  "requested_main_q54_top3_table",
  "requested_analysis_overview"
)

available_object_names <- candidate_objects[
  vapply(candidate_objects, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)
]

if (length(available_object_names) == 0) {
  stop(
    "No main-study reporting result objects are available after loading the reporting script.",
    call. = FALSE
  )
}

output_list_main <- purrr::set_names(
  lapply(available_object_names, function(obj_name) get(obj_name, envir = .GlobalEnv)),
  available_object_names
)

# =========================================================
# 3) Helper functions                                   ===
# =========================================================

first_nonmissing <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x <- x[!is.na(x) & stringr::str_squish(x) != ""]
  if (length(x) == 0) NA_character_ else x[1]
}

collapse_unique <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x <- unique(x[!is.na(x) & stringr::str_squish(x) != ""])
  if (length(x) == 0) return(NA_character_)
  if (length(x) == 1) return(x)
  paste(x, collapse = "; ")
}

html_escape_simple <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

rename_for_display <- function(df) {
  label_map <- c(
    dataset = "Dataset",
    group = "Group",
    group_label = "Group label",
    phase = "Phase",
    iteration = "Iteration",
    item = "Item",
    item_label = "Item",
    variable = "Variable",
    variable_name = "Variable",
    variable_label = "Variable label",
    short_label = "Short label",
    survey = "Survey",
    table_type = "Table type",
    priority = "Priority",
    reporting_note = "Reporting note",
    question_focus = "Question focus",
    question_text = "Question text",
    analysis_type = "Analysis type",
    filter_note = "Filter",
    denominator_note = "Denominator",
    response = "Response",
    option = "Option",
    analysis_block = "Analysis block",
    n = "n",
    n_rows = "Rows",
    n_total = "Total n",
    n_cases = "n cases",
    n_variables = "n variables",
    n_complete = "n complete",
    n_valid = "Valid n",
    n_missing = "Missing n",
    n_used = "Used n",
    n_r1 = "n round 1",
    n_r2 = "n round 2",
    n_r3 = "n round 3",
    n_change_mean = "n change mean",
    mean = "Mean",
    sd = "SD",
    median = "Median",
    min = "Min",
    max = "Max",
    percent = "Percent",
    percent_of_raw_data = "% of raw data",
    mean_r1 = "Mean round 1",
    mean_r2 = "Mean round 2",
    mean_r3 = "Mean round 3",
    mean_change_mean = "Mean change mean",
    median_r1 = "Median round 1",
    median_r2 = "Median round 2",
    median_r3 = "Median round 3",
    median_change_mean = "Median change mean",
    Main_Survey_Q53 = "Main_Survey_Q53",
    Main_Survey_target_word = "Target word",
    Main_Survey_target_word_category = "Target word category",
    diff_n_2_vs_1 = "Δ n 2 vs 1",
    diff_n_3_vs_2 = "Δ n 3 vs 2",
    diff_n_3_vs_1 = "Δ n 3 vs 1",
    diff_pct_2_vs_1 = "Δ % 2 vs 1",
    diff_pct_3_vs_2 = "Δ % 3 vs 2",
    diff_pct_3_vs_1 = "Δ % 3 vs 1",
    n_Iteration_1 = "n Iteration 1",
    n_Iteration_2 = "n Iteration 2",
    n_Iteration_3 = "n Iteration 3",
    percent_Iteration_1 = "% Iteration 1",
    percent_Iteration_2 = "% Iteration 2",
    percent_Iteration_3 = "% Iteration 3",
    most_frequent_value = "Most frequent value",
    percent_most_frequent_value = "% most frequent",
    second_most_frequent_value = "Second most frequent value",
    percent_second_most_frequent_value = "% second most frequent",
    n_possible_answers = "Possible answers",
    object_name = "Object",
    html_file = "HTML file",
    rtf_file = "RTF file"
  )

  names(df) <- ifelse(
    names(df) %in% names(label_map),
    unname(label_map[names(df)]),
    names(df)
  )

  df
}

select_display_columns <- function(df, object_name) {
  df
}

title_map <- c(
  email_matching_overview = "Overview of the final consolidated analysis dataset",
  viviq_total_summary_table = "Overview of VIVIQ overall metrics",
  viviq_item_summary_table = "Overview of VIVIQ item metrics",
  final_analysis_overview = "Overview of the final analysis dataset",
  main_numeric_desc = "Extended descriptive metrics of numeric variables",
  main_categorical_desc = "Extended frequency distributions of categorical variables",
  target_word_distribution = "Distribution of processed target words",
  target_word_category_distribution = "Distribution of target word categories",
  target_word_category_check = "Cross-check of target words and target word categories",
  unmapped_target_words = "Unmapped target words",
  longitudinal_group_1_variable_summary_table = "Longitudinal analysis group 1: summary by variable",
  longitudinal_group_1_distribution = "Longitudinal analysis group 1: distributions",
  longitudinal_group_1_change = "Longitudinal analysis group 1: changes",
  longitudinal_group_2_variable_summary_table = "Longitudinal analysis group 2: summary by variable",
  longitudinal_group_2_distribution = "Longitudinal analysis group 2: distributions",
  longitudinal_group_2_change = "Longitudinal analysis group 2: changes",
  longitudinal_group_3_variable_summary_table = "Longitudinal analysis group 3: summary by variable",
  longitudinal_group_3_distribution = "Longitudinal analysis group 3: distributions",
  longitudinal_group_3_change = "Longitudinal analysis group 3: changes",
  longitudinal_block_variable_summary_table = "Longitudinal multi-block analysis: summary by variable",
  longitudinal_block_distribution = "Longitudinal multi-block analysis: distributions",
  longitudinal_block_change = "Longitudinal multi-block analysis: changes",
  requested_main_single_variable_summary_table = "Summary of selected Main-Survey single variables",
  requested_main_single_frequency_table = "Frequency distributions of selected Main-Survey single variables",
  requested_main_q54_top3_table = "Top-3 responses for Main_Survey_Q54",
  requested_analysis_overview = "Overview of requested additional analyses"
)

subtitle_map <- c(
  main_numeric_desc = "Matched Main-Survey sample",
  main_categorical_desc = "Matched Main-Survey sample",
  longitudinal_group_1_variable_summary_table = "Image alignment across three iterations",
  longitudinal_group_1_distribution = "Image alignment across three iterations",
  longitudinal_group_1_change = "Absolute and percentage differences",
  longitudinal_group_2_variable_summary_table = "Change in the mental image across three iterations",
  longitudinal_group_2_distribution = "Change in the mental image across three iterations",
  longitudinal_group_2_change = "Absolute and percentage differences",
  longitudinal_group_3_variable_summary_table = "Process evaluation across three iterations",
  longitudinal_group_3_distribution = "Process evaluation across three iterations",
  longitudinal_group_3_change = "Absolute and percentage differences",
  longitudinal_block_variable_summary_table = "Three items across three iterations",
  longitudinal_block_distribution = "Three items across three iterations",
  longitudinal_block_change = "Absolute and percentage differences",
  requested_main_single_variable_summary_table = "Final consolidated sample",
  requested_main_single_frequency_table = "Final consolidated sample",
  requested_main_q54_top3_table = "Final consolidated sample"
)

derive_title <- function(df, object_name) {
  if (object_name %in% names(title_map)) {
    return(unname(title_map[[object_name]]))
  }

  variable_text <- collapse_unique(df$variable_name)
  focus_text <- first_nonmissing(df$question_focus)

  if (!is.na(variable_text) && !is.na(focus_text)) {
    return(paste0(variable_text, " - ", focus_text))
  }

  if (!is.na(focus_text)) {
    return(focus_text)
  }

  stringr::str_replace_all(object_name, "_", " ") %>%
    stringr::str_to_title()
}

derive_subtitle <- function(df, object_name) {
  if (object_name %in% names(subtitle_map)) {
    return(unname(subtitle_map[[object_name]]))
  }

  analysis_type <- first_nonmissing(df$analysis_type)
  filter_note <- first_nonmissing(df$filter_note)

  bits <- c(analysis_type, filter_note)
  bits <- bits[!is.na(bits) & bits != ""]

  if (length(bits) == 0) NULL else paste(bits, collapse = " | ")
}

derive_source_note <- function(df) {
  question_text <- first_nonmissing(df$question_text)
  denominator_note <- first_nonmissing(df$denominator_note)
  note_text <- first_nonmissing(df$note)

  bits <- c(
    if (!is.na(question_text)) paste0("Question: ", question_text) else NA_character_,
    if (!is.na(denominator_note)) paste0("Denominator: ", denominator_note) else NA_character_,
    if (!is.na(note_text)) paste0("Note: ", note_text) else NA_character_
  )

  bits <- bits[!is.na(bits) & bits != ""]

  if (length(bits) == 0) NULL else paste(bits, collapse = " | ")
}

format_gt_columns <- function(gt_tbl, display_df) {
  numeric_cols <- names(display_df)[vapply(display_df, is.numeric, logical(1))]

  if (length(numeric_cols) == 0) {
    return(gt_tbl)
  }

  numeric_name_lower <- stringr::str_to_lower(numeric_cols)

  pct_cols <- numeric_cols[stringr::str_detect(numeric_name_lower, "percent|%|pct")]
  p_cols <- numeric_cols[stringr::str_detect(numeric_name_lower, "^p$|p value|p_value|p-value")]

  remaining_numeric_cols <- setdiff(numeric_cols, union(pct_cols, p_cols))

  int_cols <- remaining_numeric_cols[
    vapply(
      display_df[remaining_numeric_cols],
      function(x) {
        x_nonmissing <- x[!is.na(x)]
        if (length(x_nonmissing) == 0) return(FALSE)
        all(abs(x_nonmissing - round(x_nonmissing)) < 1e-9)
      },
      logical(1)
    )
  ]

  dec_cols <- setdiff(remaining_numeric_cols, int_cols)

  if (length(int_cols) > 0) {
    gt_tbl <- gt_tbl %>%
      gt::fmt_number(columns = all_of(int_cols), decimals = 0)
  }

  if (length(pct_cols) > 0) {
    gt_tbl <- gt_tbl %>%
      gt::fmt_number(columns = all_of(pct_cols), decimals = 1)
  }

  if (length(dec_cols) > 0) {
    gt_tbl <- gt_tbl %>%
      gt::fmt_number(columns = all_of(dec_cols), decimals = 2)
  }

  if (length(p_cols) > 0) {
    gt_tbl <- gt_tbl %>%
      gt::fmt_number(columns = all_of(p_cols), decimals = 3)
  }

  gt_tbl
}

apply_row_grouping <- function(gt_tbl, display_df) {
  grouping_candidates <- c("Item", "Group label", "Iteration")
  grouping_var <- grouping_candidates[grouping_candidates %in% names(display_df)][1]

  if (is.na(grouping_var) || length(grouping_var) == 0) {
    return(gt_tbl)
  }

  group_values <- as.character(display_df[[grouping_var]])
  group_values_clean <- group_values[!is.na(group_values) & group_values != ""]

  if (length(unique(group_values_clean)) <= 1) {
    return(gt_tbl)
  }

  row_groups <- split(seq_len(nrow(display_df)), group_values)
  row_groups <- row_groups[!is.na(names(row_groups)) & names(row_groups) != ""]

  for (grp in names(row_groups)) {
    gt_tbl <- gt_tbl %>%
      gt::tab_row_group(label = grp, rows = row_groups[[grp]])
  }

  gt_tbl %>%
    gt::cols_hide(columns = all_of(grouping_var))
}

make_gt_table_main <- function(df, object_name) {
  display_df <- df %>%
    select_display_columns(object_name) %>%
    rename_for_display()

  title_text <- derive_title(df, object_name)
  subtitle_text <- derive_subtitle(df, object_name)
  source_note <- derive_source_note(df)

  gt_tbl <- gt::gt(display_df) %>%
    gt::tab_header(
      title = title_text,
      subtitle = subtitle_text
    ) %>%
    gt::tab_options(
      table.font.size = 12,
      heading.title.font.size = 14,
      data_row.padding = gt::px(4),
      table.width = gt::pct(100)
    ) %>%
    gt::opt_row_striping() %>%
    gt::fmt_missing(columns = everything(), missing_text = "—")

  if (!is.null(source_note) && !is.na(source_note) && nzchar(source_note)) {
    gt_tbl <- gt_tbl %>%
      gt::tab_source_note(source_note)
  }

  gt_tbl <- gt_tbl %>%
    format_gt_columns(display_df) %>%
    apply_row_grouping(display_df)

  docx_display_df <- display_df %>%
    dplyr::select(-dplyr::any_of(c("Item", "Group label", "Iteration")))

  attach_gt_docx_source(gt_tbl, docx_display_df, title_text, subtitle_text, source_note)
}

save_gt_table <- function(gt_tbl, file_stem) {
  html_path <- file.path(out_gt_html_dir, paste0(file_stem, ".html"))
  rtf_path  <- file.path(out_gt_rtf_dir,  paste0(file_stem, ".rtf"))
  docx_path <- file.path(out_gt_docx_dir, paste0(file_stem, ".docx"))
  saved_rtf <- NA_character_
  saved_docx <- NA_character_

  gt::gtsave(gt_tbl, filename = html_path)

  tryCatch(
    {
      gt::gtsave(gt_tbl, filename = rtf_path)
      saved_rtf <- rtf_path
    },
    error = function(e) {
      message(
        "Note: RTF export failed for '",
        file_stem,
        "'. HTML export still succeeded. Details: ",
        e$message
      )
    }
  )

  tryCatch(
    {
      save_gt_docx_table(gt_tbl, path = docx_path, file_stem = file_stem)
      saved_docx <- docx_path
    },
    error = function(e) {
        message(
          "Note: DOCX export failed for '",
          file_stem,
          "'. HTML/RTF exports still succeeded where supported. Details: ",
          e$message
        )
    }
  )

  tibble(
    object_name = file_stem,
    html_file = html_path,
    rtf_file = saved_rtf,
    docx_file = saved_docx
  )
}

# =========================================================
# 4) Create and save gt tables                           ===
# =========================================================

gt_manifest <- purrr::imap_dfr(
  output_list_main,
  function(df, object_name) {
    gt_tbl <- make_gt_table_main(df, object_name)
    save_info <- save_gt_table(gt_tbl, object_name)

    save_info %>%
      mutate(
        variable_name = collapse_unique(df$variable_name),
        question_focus = first_nonmissing(df$question_focus),
        analysis_type = first_nonmissing(df$analysis_type),
        filter_note = first_nonmissing(df$filter_note)
      ) %>%
      select(
        object_name,
        variable_name,
        question_focus,
        analysis_type,
        filter_note,
        html_file,
        rtf_file
      )
  }
)

readr::write_csv(
  gt_manifest,
  file.path(out_gt_doc_dir, "08_main_study_results_gt_manifest.csv")
)

# =========================================================
# 5) Build simple HTML index                             ===
# =========================================================

index_rows <- purrr::pmap_chr(
  gt_manifest,
  function(object_name, variable_name, question_focus, analysis_type, filter_note, html_file, rtf_file) {
    html_href <- paste0("html/", basename(html_file))
    rtf_part <- if (!is.na(rtf_file) && nzchar(rtf_file)) {
      paste0(" | <a href=\"rtf/", basename(rtf_file), "\">RTF</a>")
    } else {
      ""
    }

    paste0(
      "<tr>",
      "<td>", html_escape_simple(object_name), "</td>",
      "<td>", html_escape_simple(ifelse(is.na(variable_name), "", variable_name)), "</td>",
      "<td>", html_escape_simple(ifelse(is.na(question_focus), "", question_focus)), "</td>",
      "<td>", html_escape_simple(ifelse(is.na(analysis_type), "", analysis_type)), "</td>",
      "<td><a href=\"", html_href, "\">HTML</a>", rtf_part, "</td>",
      "</tr>"
    )
  }
)

index_html <- c(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  "  <meta charset=\"utf-8\">",
  "  <title>Main study reporting results - gt index</title>",
  "  <style>",
  "    body { font-family: Arial, sans-serif; margin: 24px; }",
  "    h1, h2 { margin-bottom: 8px; }",
  "    table { border-collapse: collapse; width: 100%; margin-top: 16px; }",
  "    th, td { border: 1px solid #d9d9d9; padding: 8px; text-align: left; vertical-align: top; }",
  "    th { background: #f5f5f5; }",
  "    tr:nth-child(even) { background: #fafafa; }",
  "  </style>",
  "</head>",
  "<body>",
  "  <h1>Main study reporting results - gt outputs</h1>",
  "  <p>This index links to all formatted gt tables created from the main-study reporting workflow.</p>",
  paste0("  <p><strong>Total tables:</strong> ", nrow(gt_manifest), "</p>"),
  "  <table>",
  "    <thead>",
  "      <tr><th>Object</th><th>Variable</th><th>Question focus</th><th>Analysis type</th><th>Files</th></tr>",
  "    </thead>",
  "    <tbody>",
  index_rows,
  "    </tbody>",
  "  </table>",
  "</body>",
  "</html>"
)

writeLines(index_html, con = file.path(out_gt_dir, "00_gt_index.html"))

# =========================================================
# 6) Console summary                                     ===
# =========================================================

console_summary <- c(
  "==================== MAIN STUDY REPORTING GT TABLES ====================",
  capture.output(print(gt_manifest)),
  "",
  paste0("Reporting script used: ", reporting_script_path),
  paste0("HTML directory: ", out_gt_html_dir),
  paste0("RTF directory: ", out_gt_rtf_dir),
  paste0("DOCX directory: ", out_gt_docx_dir),
  paste0("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
)

writeLines(
  console_summary,
  con = file.path(out_gt_doc_dir, "08_main_study_results_gt_console_summary.txt")
)

message("Confirmation: GT tables for the main study reporting results were exported successfully.")
message("HTML tables: ", out_gt_html_dir)
message("RTF tables (where supported): ", out_gt_rtf_dir)
message("DOCX tables: ", out_gt_docx_dir)
message("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
message("Manifest: ", file.path(out_gt_doc_dir, "08_main_study_results_gt_manifest.csv"))

#####################################################################
### End of workflow                                               ###
#####################################################################

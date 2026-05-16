#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.

#####################################################################
### Requested Pre-Survey descriptives as GT tables                ###
#####################################################################

### DESCRIPTION ###

# This script builds publication-ready gt tables for the requested
# pre-survey descriptives (cleaned pre-survey data before merging).
#
# It is designed as a presentation layer on top of:
#   06_pre_survey_premerge_requested_descriptives_fixed_unified.R
#
# Workflow:
# - source the base descriptives script when needed
# - reuse the already created output tables in `output_list`
# - create formatted gt tables for each output object
# - save each gt table as HTML
# - try to save each gt table as RTF as well (optional; depends on local setup)
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

base_script_path <- file.path(project_root, "scripts", "06_pre_survey_premerge_requested_descriptives_fixed_unified.R")


if (is.na(base_script_path)) {
  stop(
    paste0(
      "The base descriptives script could not be found. Expected one of these locations:\n",
      paste(base_script_candidates, collapse = "\n")
    ),
    call. = FALSE
  )
}

out_base_dir     <- file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested")
out_gt_dir       <- file.path(out_base_dir, "gt_tables")
out_gt_html_dir  <- file.path(out_gt_dir, "html")
out_gt_rtf_dir   <- file.path(out_gt_dir, "rtf")
out_gt_docx_dir  <- file.path(out_gt_dir, "docx")
out_gt_doc_dir   <- file.path(out_gt_dir, "documentation")

purrr::walk(
  c(out_base_dir, out_gt_dir, out_gt_html_dir, out_gt_rtf_dir, out_gt_docx_dir, out_gt_doc_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# =========================================================
# 2) Ensure base descriptives exist in memory           ===
# =========================================================

required_objects <- c(
  "output_list",
  "question_recommendations"
)

if (!all(vapply(required_objects, exists, logical(1), envir = .GlobalEnv, inherits = FALSE))) {
  message("Confirmation: Loading the base descriptives script: ", base_script_path)
  source(base_script_path, local = .GlobalEnv)
}

missing_after_source <- required_objects[
  !vapply(required_objects, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)
]

if (length(missing_after_source) > 0) {
  stop(
    paste0(
      "After loading the base descriptives script, the following objects are still missing:\n",
      paste(missing_after_source, collapse = ", ")
    ),
    call. = FALSE
  )
}

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
    variable_name = "Variable",
    question_focus = "Question focus",
    question_text = "Question text",
    analysis_type = "Analysis type",
    filter_note = "Filter",
    denominator_note = "Denominator",
    response = "Response",
    option = "Option",
    n = "n",
    n_eligible = "Eligible n",
    n_valid = "Valid n",
    n_missing = "Missing n",
    n_with_any_response_raw = "Cases with any response",
    n_with_included_option = "Cases with included option",
    percent_of_eligible = "% of eligible",
    percent_of_valid = "% of valid",
    percent_of_valid_cases = "% of valid cases",
    score_variable_used = "Score variable",
    mean = "Mean",
    sd = "SD",
    median = "Median",
    q1 = "Q1",
    q3 = "Q3",
    min = "Min",
    max = "Max",
    item_label = "Item",
    cluster = "Cluster",
    analysis_planned = "Planned analysis",
    reporting_suggestion = "Reporting suggestion",
    response_id = "Response ID",
    response_text = "Response text",
    word_count = "Word count",
    character_count = "Character count",
    n_clean_pre_cases = "Clean pre-survey n",
    n_q3_yes = "Q3 yes n",
    n_q3_no = "Q3 no n",
    n_age_nonmissing = "Age non-missing n",
    n_gender_nonmissing = "Gender non-missing n",
    n_unique_exact = "Unique exact responses",
    percent_unique_exact = "% unique exact",
    n_repeated_exact_patterns = "Repeated exact patterns",
    mean_words = "Mean words",
    median_words = "Median words",
    min_words = "Min words",
    max_words = "Max words",
    mean_characters = "Mean characters",
    median_characters = "Median characters"
  )

  names(df) <- ifelse(
    names(df) %in% names(label_map),
    unname(label_map[names(df)]),
    names(df)
  )

  df
}

select_display_columns <- function(df, object_name) {
  if (identical(object_name, "question_recommend")) {
    return(
      df %>%
        select(any_of(c(
          "cluster", "variable_name", "question_focus",
          "analysis_planned", "filter_note", "reporting_suggestion"
        )))
    )
  }

  if (identical(object_name, "sample_overview")) {
    return(df)
  }

  analysis_type <- first_nonmissing(df$analysis_type)

  if (!is.na(analysis_type) && analysis_type %in% c("single_choice_distribution", "multiselect_distribution", "numeric_band_distribution", "numeric_percentage_band_distribution")) {
    return(
      df %>%
        select(any_of(c(
          "item_label", "response", "option", "n",
          "percent_of_eligible", "percent_of_valid", "percent_of_valid_cases"
        )))
    )
  }

  if (!is.na(analysis_type) && analysis_type %in% c(
    "score_summary", "age_numeric_summary", "collaboration_percentage_summary",
    "likert_score_summary", "block_item_score_summary", "numeric_percentage_summary"
  )) {
    return(
      df %>%
        select(any_of(c(
          "item_label", "score_variable_used", "n_eligible", "n_valid",
          "n_missing", "mean", "sd", "median", "q1", "q3", "min", "max"
        )))
    )
  }

  if (!is.na(analysis_type) && analysis_type == "open_text_summary") {
    return(
      df %>%
        select(any_of(c(
          "n_eligible", "n_valid", "n_missing",
          "n_unique_exact", "percent_unique_exact", "n_repeated_exact_patterns",
          "mean_words", "median_words", "min_words", "max_words",
          "mean_characters", "median_characters"
        )))
    )
  }

  if (!is.na(analysis_type) && analysis_type == "open_text_exact_repeats") {
    return(df %>% select(any_of(c("response", "n"))))
  }

  if (!is.na(analysis_type) && analysis_type == "open_text_inventory") {
    return(df %>% select(any_of(c("response_id", "response_text", "word_count", "character_count"))))
  }

  df
}

derive_title <- function(df, object_name) {
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

derive_subtitle <- function(df) {
  analysis_type <- first_nonmissing(df$analysis_type)
  filter_note <- first_nonmissing(df$filter_note)

  bits <- c(analysis_type, filter_note)
  bits <- bits[!is.na(bits) & bits != ""]

  if (length(bits) == 0) NULL else paste(bits, collapse = " | ")
}

derive_source_note <- function(df) {
  question_text <- first_nonmissing(df$question_text)
  denominator_note <- first_nonmissing(df$denominator_note)
  eligible_n <- if ("n_eligible" %in% names(df)) first_nonmissing(df$n_eligible) else NA_character_

  bits <- c(
    if (!is.na(question_text)) paste0("Question: ", question_text) else NA_character_,
    if (!is.na(eligible_n)) paste0("Eligible n: ", eligible_n) else NA_character_,
    if (!is.na(denominator_note)) paste0("Denominator: ", denominator_note) else NA_character_
  )

  bits <- bits[!is.na(bits) & bits != ""]

  if (length(bits) == 0) NULL else paste(bits, collapse = " | ")
}

format_gt_columns <- function(gt_tbl, display_df) {
  pct_cols <- intersect(
    c("% of eligible", "% of valid", "% of valid cases", "% unique exact"),
    names(display_df)
  )

  int_cols <- intersect(
    c(
      "n", "Eligible n", "Valid n", "Missing n", "Cases with any response",
      "Cases with included option", "Word count", "Character count",
      "Clean pre-survey n", "Q3 yes n", "Q3 no n", "Age non-missing n",
      "Gender non-missing n", "Unique exact responses", "Repeated exact patterns"
    ),
    names(display_df)
  )

  dec_cols <- intersect(
    c(
      "Mean", "SD", "Median", "Q1", "Q3", "Min", "Max",
      "Mean words", "Median words", "Min words", "Max words",
      "Mean characters", "Median characters"
    ),
    names(display_df)
  )

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

  gt_tbl
}

apply_item_grouping <- function(gt_tbl, display_df) {
  if (!"Item" %in% names(display_df)) {
    return(gt_tbl)
  }

  item_values <- as.character(display_df$Item)
  item_values_clean <- item_values[!is.na(item_values) & item_values != ""]

  if (length(unique(item_values_clean)) <= 1) {
    return(gt_tbl)
  }

  row_groups <- split(seq_len(nrow(display_df)), item_values)
  row_groups <- row_groups[!is.na(names(row_groups)) & names(row_groups) != ""]

  for (grp in names(row_groups)) {
    gt_tbl <- gt_tbl %>%
      gt::tab_row_group(label = grp, rows = row_groups[[grp]])
  }

  gt_tbl %>%
    gt::cols_hide(columns = "Item")
}

make_gt_table_requested <- function(df, object_name) {
  display_df <- df %>%
    select_display_columns(object_name) %>%
    rename_for_display()

  title_text <- derive_title(df, object_name)
  subtitle_text <- derive_subtitle(df)
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
    apply_item_grouping(display_df)

  docx_display_df <- display_df %>%
    dplyr::select(-dplyr::any_of(c("Item")))

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
      message("Note: RTF export failed for '", file_stem, "'. HTML export still succeeded. Details: ", e$message)
    }
  )

  tryCatch(
    {
      save_gt_docx_table(gt_tbl, path = docx_path, file_stem = file_stem)
      saved_docx <- docx_path
    },
    error = function(e) {
        message("Note: DOCX export failed for '", file_stem, "'. HTML/RTF exports still succeeded where supported. Details: ", e$message)
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
  output_list,
  function(df, object_name) {
    gt_tbl <- make_gt_table_requested(df, object_name)
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
  file.path(out_gt_doc_dir, "07_pre_survey_premerge_requested_gt_manifest.csv")
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
  "  <title>Pre-Survey requested descriptives - gt index</title>",
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
  "  <h1>Pre-Survey requested descriptives - gt outputs</h1>",
  "  <p>This index links to all formatted gt tables created from the requested pre-survey descriptives workflow.</p>",
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
  "==================== PRE-SURVEY REQUESTED GT TABLES ====================",
  capture.output(print(gt_manifest)),
  "",
  paste0("Base descriptives script used: ", base_script_path),
  paste0("HTML directory: ", out_gt_html_dir),
  paste0("RTF directory: ", out_gt_rtf_dir),
  paste0("DOCX directory: ", out_gt_docx_dir),
  paste0("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
)

writeLines(
  console_summary,
  con = file.path(out_gt_doc_dir, "07_pre_survey_premerge_requested_gt_console_summary.txt")
)

message("Confirmation: GT tables for the requested pre-survey descriptives were exported successfully.")
message("HTML tables: ", out_gt_html_dir)
message("RTF tables (where supported): ", out_gt_rtf_dir)
message("DOCX tables: ", out_gt_docx_dir)
message("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
message("Manifest: ", file.path(out_gt_doc_dir, "07_pre_survey_premerge_requested_gt_manifest.csv"))

#####################################################################
### End of workflow                                               ###
#####################################################################

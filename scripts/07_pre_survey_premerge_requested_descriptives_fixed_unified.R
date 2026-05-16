#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.

#####################################################################
### Requested Pre-Survey descriptives (cleaned, before merging)   ###
#####################################################################

### DESCRIPTION ###

# This script creates separate descriptive statistics tables for the
# requested Pre-Survey variables on the cleaned Pre-Survey dataset
# BEFORE merging with the Main Survey.
#
# It follows the project logic of the existing workflow:
# - source the cleaning script if required objects are missing
# - use `pre_clean_full` as the cleaned Pre-Survey base dataset
# - export each requested table separately as CSV and XLSX
# - collect all results in one combined Excel workbook
# - provide a question-level recommendation table for reporting

# =========================================================
# 0) Packages                                           ===
# =========================================================

# install.packages(c("tidyverse", "writexl", "here", "readr"), dependencies = TRUE)

library(tidyverse)
library(writexl)
library(here)
library(readr)

# =========================================================
# 1) Paths and dependencies                              ===
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

# The Pre-Survey analyses in this script must be based on the full cleaned
# Pre-Survey dataset before matching. Therefore this script loads only the
# anonymized Pre-Survey dataset from data_final/ and does not source scripts 01-03.

loaded_datasets <- load_anonymized_analysis_datasets(
  project_root = project_root,
  require_pre = TRUE,
  require_main = FALSE,
  require_final = FALSE
)

pre_survey_dataset <- loaded_datasets$pre_survey_dataset
pre_clean_full <- pre_survey_dataset
pre_feature_lookup <- loaded_datasets$pre_feature_lookup

out_base_dir   <- file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested")
out_tables_dir <- file.path(out_base_dir, "tables")
out_doc_dir    <- file.path(out_base_dir, "documentation")
out_gt_dir     <- file.path(out_base_dir, "gt_tables")
out_gt_html_dir <- file.path(out_gt_dir, "html")
out_gt_rtf_dir  <- file.path(out_gt_dir, "rtf")

purrr::walk(
  c(out_base_dir, out_tables_dir, out_doc_dir, out_gt_dir, out_gt_html_dir, out_gt_rtf_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

message("Confirmation: Pre-Survey analysis uses data_final/pre_survey_anonymized.rds (unmatched cleaned Pre-Survey cases).")

# =========================================================
# 3) Analysis base                                       ===
# =========================================================

pre_analysis <- pre_clean_full
analysis_dataset_label <- "Pre-Survey cleaned before merging (anonymized, unmatched)"

# =========================================================
# 4) Helper functions                                    ===
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

safe_quantile <- function(x, prob) {
  if (all(is.na(x))) NA_real_ else as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
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

make_age_group <- function(x) {
  age_num <- safe_numeric(x)
  cut(
    age_num,
    breaks = c(17, 24, 34, 44, 54, Inf),
    labels = c("18-24", "25-34", "35-44", "45-54", "55+"),
    right = TRUE
  )
}

apply_filter_condition <- function(df, filter_expr = NULL) {
  if (is.null(filter_expr) || is.na(filter_expr) || filter_expr == "") {
    return(df)
  }

  df %>%
    filter(!!rlang::parse_expr(filter_expr))
}

save_table_outputs <- function(df, base_filename, out_dir = out_tables_dir) {
  readr::write_csv(df, file.path(out_dir, paste0(base_filename, ".csv")))
  writexl::write_xlsx(df, path = file.path(out_dir, paste0(base_filename, ".xlsx")))

  tryCatch(
    save_docx_table(df, path = file.path(out_dir, paste0(base_filename, ".docx")), title_text = base_filename),
    error = function(e) message("Note: DOCX export failed for '", base_filename, "'. Details: ", e$message)
  )
}

build_empty_result <- function(var_name, question_focus, question_text, analysis_type, filter_note, denominator_note) {
  tibble(
    dataset = analysis_dataset_label,
    variable_name = var_name,
    question_focus = question_focus,
    question_text = question_text,
    analysis_type = analysis_type,
    filter_note = filter_note,
    denominator_note = denominator_note
  )
}

make_single_choice_distribution <- function(df, var_name, question_focus,
                                            filter_expr = NULL,
                                            filter_note = "All cleaned Pre-Survey cases",
                                            response_levels = NULL) {
  df_sub <- apply_filter_condition(df, filter_expr)
  question_text <- get_question_text(pre_feature_lookup, var_name)
  x <- normalize_missing_text(df_sub[[var_name]])

  n_eligible <- nrow(df_sub)
  n_valid <- sum(!is.na(x))
  n_missing <- sum(is.na(x))

  x_out <- ifelse(is.na(x), "Missing", x)

  if (!is.null(response_levels)) {
    x_out <- factor(x_out, levels = c(response_levels, "Missing"), ordered = TRUE)
  }

  out <- tibble(response = x_out) %>%
    count(response, name = "n", .drop = FALSE)

  if (!is.null(response_levels)) {
    out <- out %>%
      mutate(
        response_chr = as.character(response),
        response_order = match(response_chr, c(response_levels, "Missing"))
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
      dataset = analysis_dataset_label,
      variable_name = var_name,
      question_focus = question_focus,
      question_text = question_text,
      analysis_type = "single_choice_distribution",
      filter_note = filter_note,
      denominator_note = "Percent of eligible respondents and percent of valid non-missing responses.",
      n_eligible = n_eligible,
      n_valid = n_valid,
      n_missing = n_missing,
      percent_of_eligible = pct(n, n_eligible),
      percent_of_valid = if_else(response == "Missing", NA_real_, pct(n, n_valid)),
      .before = 1
    )
}

make_multiselect_distribution <- function(df, var_name, question_focus,
                                          filter_expr = NULL,
                                          filter_note = "All cleaned Pre-Survey cases",
                                          exclude_options = character(),
                                          sep_pattern = ";",
                                          response_levels = NULL) {
  df_sub <- apply_filter_condition(df, filter_expr) %>%
    mutate(.case_id_internal = row_number())

  question_text <- get_question_text(pre_feature_lookup, var_name)
  x <- normalize_missing_text(df_sub[[var_name]])

  n_eligible <- nrow(df_sub)
  n_with_any_response_raw <- sum(!is.na(x))

  if (all(is.na(x))) {
    return(
      build_empty_result(
        var_name = var_name,
        question_focus = question_focus,
        question_text = question_text,
        analysis_type = "multiselect_distribution",
        filter_note = filter_note,
        denominator_note = "No non-missing responses available in the filtered sample."
      )
    )
  }

  long_df <- df_sub %>%
    transmute(
      .case_id_internal,
      response_raw = normalize_missing_text(.data[[var_name]])
    ) %>%
    filter(!is.na(response_raw)) %>%
    tidyr::separate_rows(response_raw, sep = sep_pattern) %>%
    mutate(response_raw = stringr::str_squish(response_raw)) %>%
    filter(response_raw != "") %>%
    distinct(.case_id_internal, response_raw)

  if (length(exclude_options) > 0) {
    long_df <- long_df %>%
      filter(!response_raw %in% exclude_options)
  }

  n_with_included_option <- long_df %>%
    distinct(.case_id_internal) %>%
    nrow()

  if (nrow(long_df) == 0) {
    return(
      build_empty_result(
        var_name = var_name,
        question_focus = question_focus,
        question_text = question_text,
        analysis_type = "multiselect_distribution",
        filter_note = filter_note,
        denominator_note = "All responses were removed by the exclusion rule."
      )
    )
  }

  out <- long_df %>%
    count(response_raw, name = "n")

  if (!is.null(response_levels)) {
    out <- tibble(response_raw = response_levels) %>%
      left_join(out, by = "response_raw") %>%
      mutate(n = dplyr::coalesce(n, 0L)) %>%
      mutate(response_order = match(response_raw, response_levels)) %>%
      arrange(response_order) %>%
      select(-response_order)
  } else {
    out <- out %>%
      arrange(desc(n), response_raw)
  }

  out %>%
    mutate(
      dataset = analysis_dataset_label,
      variable_name = var_name,
      question_focus = question_focus,
      question_text = question_text,
      analysis_type = "multiselect_distribution",
      filter_note = filter_note,
      denominator_note = if (length(exclude_options) == 0) {
        "Percent of eligible respondents and percent of respondents with any non-missing answer."
      } else {
        "Percent of eligible respondents and percent of respondents with at least one included option after exclusions."
      },
      n_eligible = n_eligible,
      n_with_any_response_raw = n_with_any_response_raw,
      n_with_included_option = n_with_included_option,
      percent_of_eligible = pct(n, n_eligible),
      percent_of_valid_cases = pct(
        n,
        ifelse(length(exclude_options) == 0, n_with_any_response_raw, n_with_included_option)
      ),
      .before = 1
    ) %>%
    rename(option = response_raw)
}

make_score_summary <- function(df, var_name, question_focus,
                               filter_expr = NULL,
                               filter_note = "All cleaned Pre-Survey cases",
                               score_var = NULL,
                               analysis_type = "score_summary") {
  df_sub <- apply_filter_condition(df, filter_expr)
  question_text <- get_question_text(pre_feature_lookup, var_name)

  score_candidate <- if (is.null(score_var)) paste0(var_name, "_score") else score_var

  x_num <- if (score_candidate %in% names(df_sub)) {
    suppressWarnings(as.numeric(df_sub[[score_candidate]]))
  } else {
    safe_numeric(df_sub[[var_name]])
  }

  tibble(
    dataset = analysis_dataset_label,
    variable_name = var_name,
    question_focus = question_focus,
    question_text = question_text,
    analysis_type = analysis_type,
    filter_note = filter_note,
    denominator_note = "Summary based on valid numeric values in the filtered sample.",
    score_variable_used = ifelse(score_candidate %in% names(df_sub), score_candidate, var_name),
    n_eligible = nrow(df_sub),
    n_valid = sum(!is.na(x_num)),
    n_missing = sum(is.na(x_num)),
    mean = safe_mean(x_num),
    sd = safe_sd(x_num),
    median = safe_median(x_num),
    q1 = safe_quantile(x_num, 0.25),
    q3 = safe_quantile(x_num, 0.75),
    min = safe_min(x_num),
    max = safe_max(x_num)
  )
}

make_numeric_band_distribution <- function(df, var_name, question_focus,
                                           filter_expr = NULL,
                                           filter_note = "All cleaned Pre-Survey cases",
                                           breaks,
                                           labels,
                                           include_lowest = TRUE,
                                           right = TRUE,
                                           analysis_type = "numeric_band_distribution") {
  df_sub <- apply_filter_condition(df, filter_expr)
  question_text <- get_question_text(pre_feature_lookup, var_name)
  x_num <- safe_numeric(df_sub[[var_name]])

  band <- cut(
    x_num,
    breaks = breaks,
    labels = labels,
    include_lowest = include_lowest,
    right = right
  )

  band_out <- ifelse(is.na(band), "Missing", as.character(band))

  tibble(response = factor(band_out, levels = c(labels, "Missing"), ordered = TRUE)) %>%
    count(response, name = "n", .drop = FALSE) %>%
    mutate(
      response = as.character(response),
      dataset = analysis_dataset_label,
      variable_name = var_name,
      question_focus = question_focus,
      question_text = question_text,
      analysis_type = analysis_type,
      filter_note = filter_note,
      denominator_note = "Percent of eligible respondents and percent of valid numeric values.",
      n_eligible = nrow(df_sub),
      n_valid = sum(!is.na(x_num)),
      n_missing = sum(is.na(x_num)),
      percent_of_eligible = pct(n, nrow(df_sub)),
      percent_of_valid = if_else(response == "Missing", NA_real_, pct(n, sum(!is.na(x_num)))),
      .before = 1
    )
}

make_open_text_summary <- function(df, var_name, question_focus,
                                   filter_expr = NULL,
                                   filter_note = "All cleaned Pre-Survey cases") {
  df_sub <- apply_filter_condition(df, filter_expr)
  question_text <- get_question_text(pre_feature_lookup, var_name)
  x <- normalize_missing_text(df_sub[[var_name]])
  valid <- x[!is.na(x)]

  if (length(valid) == 0) {
    return(
      tibble(
        dataset = analysis_dataset_label,
        variable_name = var_name,
        question_focus = question_focus,
        question_text = question_text,
        analysis_type = "open_text_summary",
        filter_note = filter_note,
        denominator_note = "No non-missing responses available in the filtered sample.",
        n_eligible = nrow(df_sub),
        n_valid = 0,
        n_missing = nrow(df_sub),
        n_unique_exact = 0,
        percent_unique_exact = NA_real_,
        n_repeated_exact_patterns = 0,
        mean_words = NA_real_,
        median_words = NA_real_,
        min_words = NA_real_,
        max_words = NA_real_,
        mean_characters = NA_real_,
        median_characters = NA_real_
      )
    )
  }

  words <- stringr::str_count(valid, boundary("word"))
  chars <- nchar(valid)
  repeated_exact <- tibble(response = valid) %>%
    count(response, name = "n") %>%
    filter(n > 1)

  tibble(
    dataset = analysis_dataset_label,
    variable_name = var_name,
    question_focus = question_focus,
    question_text = question_text,
    analysis_type = "open_text_summary",
    filter_note = filter_note,
    denominator_note = "Summary based on non-missing open-text responses in the filtered sample.",
    n_eligible = nrow(df_sub),
    n_valid = length(valid),
    n_missing = sum(is.na(x)),
    n_unique_exact = dplyr::n_distinct(valid),
    percent_unique_exact = pct(dplyr::n_distinct(valid), length(valid)),
    n_repeated_exact_patterns = nrow(repeated_exact),
    mean_words = safe_mean(words),
    median_words = safe_median(words),
    min_words = safe_min(words),
    max_words = safe_max(words),
    mean_characters = safe_mean(chars),
    median_characters = safe_median(chars)
  )
}

make_open_text_repeats <- function(df, var_name, question_focus,
                                   filter_expr = NULL,
                                   filter_note = "All cleaned Pre-Survey cases") {
  df_sub <- apply_filter_condition(df, filter_expr)
  question_text <- get_question_text(pre_feature_lookup, var_name)
  x <- normalize_missing_text(df_sub[[var_name]])

  repeats <- tibble(response = x) %>%
    filter(!is.na(response)) %>%
    count(response, name = "n") %>%
    filter(n > 1) %>%
    arrange(desc(n), response)

  if (nrow(repeats) == 0) {
    return(
      build_empty_result(
        var_name = var_name,
        question_focus = question_focus,
        question_text = question_text,
        analysis_type = "open_text_exact_repeats",
        filter_note = filter_note,
        denominator_note = "No exact repeated responses available in the filtered sample."
      )
    )
  }

  repeats %>%
    mutate(
      dataset = analysis_dataset_label,
      variable_name = var_name,
      question_focus = question_focus,
      question_text = question_text,
      analysis_type = "open_text_exact_repeats",
      filter_note = filter_note,
      denominator_note = "Only exact repeated responses are shown.",
      .before = 1
    )
}

make_open_text_inventory <- function(df, var_name, question_focus,
                                     filter_expr = NULL,
                                     filter_note = "All cleaned Pre-Survey cases") {
  df_sub <- apply_filter_condition(df, filter_expr)
  question_text <- get_question_text(pre_feature_lookup, var_name)

  id_var <- if ("Pre_Survey_ResponseId" %in% names(df_sub)) "Pre_Survey_ResponseId" else NULL

  inventory <- df_sub %>%
    transmute(
      response_id = if (!is.null(id_var)) .data[[id_var]] else row_number(),
      response_text = normalize_missing_text(.data[[var_name]])
    ) %>%
    filter(!is.na(response_text)) %>%
    mutate(
      word_count = stringr::str_count(response_text, boundary("word")),
      character_count = nchar(response_text),
      dataset = analysis_dataset_label,
      variable_name = var_name,
      question_focus = question_focus,
      question_text = question_text,
      analysis_type = "open_text_inventory",
      filter_note = filter_note,
      denominator_note = "All non-missing verbatim responses from the filtered sample.",
      .before = 1
    )

  if (nrow(inventory) == 0) {
    return(
      build_empty_result(
        var_name = var_name,
        question_focus = question_focus,
        question_text = question_text,
        analysis_type = "open_text_inventory",
        filter_note = filter_note,
        denominator_note = "No non-missing responses available in the filtered sample."
      )
    )
  }

  inventory
}

make_block_distribution <- function(df, vars, question_focus,
                                    filter_expr = NULL,
                                    filter_note = "All cleaned Pre-Survey cases",
                                    response_levels = NULL,
                                    item_labels = NULL) {
  purrr::imap_dfr(
    vars,
    function(var_name, idx) {
      item_label <- if (!is.null(item_labels) && length(item_labels) >= idx) item_labels[[idx]] else var_name

      make_single_choice_distribution(
        df = df,
        var_name = var_name,
        question_focus = question_focus,
        filter_expr = filter_expr,
        filter_note = filter_note,
        response_levels = response_levels
      ) %>%
        mutate(item_label = item_label, .after = question_focus)
    }
  )
}

make_block_score_summary <- function(df, vars, question_focus,
                                     filter_expr = NULL,
                                     filter_note = "All cleaned Pre-Survey cases",
                                     item_labels = NULL) {
  purrr::imap_dfr(
    vars,
    function(var_name, idx) {
      item_label <- if (!is.null(item_labels) && length(item_labels) >= idx) item_labels[[idx]] else var_name

      make_score_summary(
        df = df,
        var_name = var_name,
        question_focus = question_focus,
        filter_expr = filter_expr,
        filter_note = filter_note,
        analysis_type = "block_item_score_summary"
      ) %>%
        mutate(item_label = item_label, .after = question_focus)
    }
  )
}

# =========================================================
# 5) Recommendations table                               ===
# =========================================================

question_recommendations <- tribble(
  ~cluster, ~variable_name, ~question_focus, ~analysis_planned, ~filter_note, ~reporting_suggestion,
  "Cluster 1", "Pre_Survey_Q2", "How participants used to visualize mental images before image generators", "Multiselect option distribution", "All cleaned Pre-Survey cases", "Report percentages by option; mention that multiple answers were possible.",
  "Cluster 1", "Pre_Survey_Q4_3", "What prior GenAI users use GenAI for beyond image generation", "Multiselect option distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Report option percentages among prior image-generator users.",
  "Cluster 1", "Pre_Survey_Q4_1", "What non-users use GenAI for beyond image generation", "Multiselect option distribution", "Only respondents with Pre_Survey_Q3 == No", "Report separately from Q4_3 because the target populations differ.",
  "Cluster 1", "Pre_Survey_Q3", "Whether participants have ever used GenAI for image generation", "Single-choice distribution", "All cleaned Pre-Survey cases", "Use as the main branching variable for the later pre-survey experience block.",
  "Cluster 1", "Pre_Survey_Q22", "Percentage of usage with others", "Numeric summary plus collaboration bands", "Only respondents with Pre_Survey_Q3 == Yes", "Report mean and median together because the 0% and 100% endpoints are substantively meaningful.",
  "Cluster 1", "Pre_Survey_Q23", "How collaboration typically takes place", "Multiselect option distribution excluding 'I don't work with others'", "Only respondents with Pre_Survey_Q3 == Yes and at least one collaborative option", "Use percentages among collaborating respondents, not the full sample.",
  "Cluster 2", "Pre_Survey_Q9", "Which GenAI image tools participants had tried", "Multiselect option distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Report tool prevalence and optionally highlight the top 5 tools in the text.",
  "Cluster 2", "Pre_Survey_Q6", "Since when participants have used the tools", "Single-choice distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Interpret as experience duration, not intensity.",
  "Cluster 2", "Pre_Survey_Q7", "How often participants use image generators", "Single-choice distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Useful as the main usage-intensity descriptive table.",
  "Cluster 2", "Pre_Survey_Q10", "Whether participants pay for image-generation services", "Single-choice distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Mention any inconsistent multi-option raw responses as a small data-quality note if they occur.",
  "Cluster 2", "Pre_Survey_Q11", "For what purposes participants use image generators", "Multiselect option distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Report as purpose profile; multiple answers were possible.",
  "Cluster 2", "Pre_Survey_Q12", "What image types participants generate most often", "Multiselect option distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Use option percentages and discuss the dominant content categories.",
  "Cluster 2", "Pre_Survey_Q13", "Importance of realism, creativity, output control, speed, variety, and alignment", "Block item score summary plus per-item response distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Report both item means and full distributions because priorities can have skewed Likert shapes.",
  "Cluster 3", "Pre_Survey_Q14", "Main challenges when generating images", "Multiselect option distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Good candidate for a rank-ordered problem profile in the thesis text.",
  "Cluster 3", "Pre_Survey_Q15", "How prompts are usually created", "Multiselect option distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Useful for describing prompt-writing strategies and external prompt support.",
  "Cluster 3", "Pre_Survey_Q16", "Whether participants actively adjust prompts", "Single-choice distribution plus score summary", "Only respondents with Pre_Survey_Q3 == Yes", "Report as one of the main prompting-practice indicators.",
  "Cluster 3", "Pre_Survey_Q17", "What elements participants include in prompts", "Multiselect option distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Use to show prompt-content preferences; multiple answers were possible.",
  "Cluster 3", "Pre_Survey_Q18", "How difficult it is to phrase prompts", "Single-choice distribution plus score summary", "Only respondents with Pre_Survey_Q3 == Yes", "Report mean/median together with the distribution because difficulty is ordinal.",
  "Cluster 3", "Pre_Survey_Q19", "How many iterations are usually needed", "Single-choice distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Interpret as perceived iteration burden; keep category order in the table.",
  "Cluster 3", "Pre_Survey_Q20", "What role GenAI plays in the image-creation process", "Multiselect option distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Use to describe perceived authorship and tool-role framing.",
  "Cluster 3", "Pre_Survey_Q21", "Agreement with different statements", "Block item score summary plus per-item response distribution", "Only respondents with Pre_Survey_Q3 == Yes", "Report both central tendency and item-level response profiles.",
  "Cluster 3", "Pre_Survey_Q24", "Typical use situations", "Open-text summary plus response inventory", "Only respondents with Pre_Survey_Q3 == Yes", "Best followed by thematic coding; exact frequencies alone are not sufficient.",
  "Cluster 3", "Pre_Survey_Q25", "What makes an AI-generated image successful", "Open-text summary plus response inventory", "Only respondents with Pre_Survey_Q3 == Yes", "Use as qualitative material; repeated exact answers are likely rare.",
  "Cluster 3", "Pre_Survey_Q26", "What makes an AI-generated image unsuccessful", "Open-text summary plus response inventory", "Only respondents with Pre_Survey_Q3 == Yes", "Suitable for later thematic coding or codebook development.",
  "Cluster 3", "Pre_Survey_Q27", "What participants find problematic or concerning", "Open-text summary plus response inventory", "Only respondents with Pre_Survey_Q3 == Yes", "A qualitative concern inventory is more informative than simple raw frequency counts.",
  "Cluster 3", "Pre_Survey_Q28", "Age", "Numeric summary plus age-group distribution", "All cleaned Pre-Survey cases", "Report mean/SD and median/IQR; add age groups for easier interpretation.",
  "Cluster 3", "Pre_Survey_Q29", "Gender", "Single-choice distribution", "All cleaned Pre-Survey cases", "Straight categorical distribution is sufficient."
)

# =========================================================
# 6) Filters and response orders                         ===
# =========================================================

filter_all <- NULL
filter_q3_yes <- 'Pre_Survey_Q3 == "Yes"'
filter_q3_no  <- 'Pre_Survey_Q3 == "No"'

q2_levels <- c(
  "Drawing or sketching by hand (on paper or digitally)",
  "Describing the image verbally (spoken explanation)",
  "Writing a detailed description (e.g., notes, text messages)",
  "Using existing images (photos, illustrations, references)",
  "Googling",
  "Gesturing or acting things out",
  "Creating diagrams or schematics",
  "Using physical objects or models",
  "Other (please specify):",
  "Not applicable / I did not do this"
)

q6_levels <- c("Less than 1 month", "1–6 months", "7–12 months", "1–2 years", "More than 2 years")
q7_levels <- c("Daily", "Several times per week", "Weekly", "Monthly", "Less often", "I no longer use it. If so, why?")
q10_levels <- c("No", "Yes, occasionally", "Yes, subscription-based")
q16_levels <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
q18_levels <- c("Extremely easy", "Very easy", "Somewhat easy", "Neither easy nor difficult", "Somewhat difficult", "Very difficult", "Extremely difficult")
q19_levels <- c("1", "2-3", "4-6", "7-10", "More than 10", "Hard to say (please describe shorty)")
q21_levels <- c("Strongly disagree", "Disagree", "Slightly disagree", "Neither agree nor disagree", "Slightly agree", "Agree", "Strongly agree")
q13_levels <- c("Not a priority", "Low priority", "Somewhat priority", "Neutral", "Moderate priority", "High priority", "Essential priority")
q23_exclude <- c("I don't work with others")

q13_vars <- paste0("Pre_Survey_Q13_", 1:6)
q13_item_labels <- c(
  "Realism",
  "Creativity",
  "Output control",
  "Speed",
  "Variety",
  "Alignment with mental image"
)

q21_vars <- paste0("Pre_Survey_Q21_", 1:5)
q21_item_labels <- purrr::map_chr(q21_vars, ~ get_question_text(pre_feature_lookup, .x))

# =========================================================
# 7) Create requested tables                             ===
# =========================================================

sample_overview_pre_requested <- tibble(
  dataset = analysis_dataset_label,
  n_clean_pre_cases = nrow(pre_analysis),
  n_q3_yes = sum(pre_analysis$Pre_Survey_Q3 == "Yes", na.rm = TRUE),
  n_q3_no = sum(pre_analysis$Pre_Survey_Q3 == "No", na.rm = TRUE),
  n_age_nonmissing = sum(!is.na(safe_numeric(pre_analysis$Pre_Survey_Q28))),
  n_gender_nonmissing = sum(!is.na(normalize_missing_text(pre_analysis$Pre_Survey_Q29)))
)

# Cluster 1
q2_distribution <- make_multiselect_distribution(
  df = pre_analysis,
  var_name = "Pre_Survey_Q2",
  question_focus = "How participants normally visualized mental images before image generators",
  filter_expr = filter_all,
  filter_note = "All cleaned Pre-Survey cases",
  sep_pattern = ";",
  response_levels = q2_levels
)

q3_distribution <- make_single_choice_distribution(
  pre_analysis, "Pre_Survey_Q3",
  "Whether participants had ever used GenAI for image generation",
  filter_expr = filter_all,
  filter_note = "All cleaned Pre-Survey cases",
  response_levels = c("Yes", "No")
)

q4_1_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q4_1",
  "What non-users use GenAI for beyond image generation",
  filter_expr = filter_q3_no,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "No"'
)

q4_3_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q4_3",
  "What prior image-generator users use GenAI for beyond image generation",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q22_numeric_summary <- make_score_summary(
  pre_analysis, "Pre_Survey_Q22",
  "Percentage of usage with others",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  score_var = "Pre_Survey_Q22",
  analysis_type = "numeric_percentage_summary"
)

q22_band_distribution <- make_numeric_band_distribution(
  pre_analysis, "Pre_Survey_Q22",
  "Percentage of usage with others",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  breaks = c(-Inf, 0, 24, 49, 74, 99, Inf),
  labels = c("0%", "1-24%", "25-49%", "50-74%", "75-99%", "100%"),
  include_lowest = TRUE,
  right = TRUE,
  analysis_type = "numeric_percentage_band_distribution"
)

q23_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q23",
  "How collaboration typically takes place",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"; option "I don\'t work with others" excluded',
  exclude_options = q23_exclude
)

# Cluster 2
q6_distribution <- make_single_choice_distribution(
  pre_analysis, "Pre_Survey_Q6",
  "Since when participants have used image generators",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  response_levels = q6_levels
)

q7_distribution <- make_single_choice_distribution(
  pre_analysis, "Pre_Survey_Q7",
  "How often participants use image generators",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  response_levels = q7_levels
)

q9_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q9",
  "Which image-generation tools participants had tried",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q10_distribution <- make_single_choice_distribution(
  pre_analysis, "Pre_Survey_Q10",
  "Whether participants pay for image-generation services",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  response_levels = q10_levels
)

q11_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q11",
  "For what purposes participants use image generators",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q12_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q12",
  "What image types participants generate most frequently",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q13_score_summary <- make_block_score_summary(
  pre_analysis,
  vars = q13_vars,
  question_focus = "Importance of different image-generator characteristics",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  item_labels = q13_item_labels
)

q13_distribution <- make_block_distribution(
  pre_analysis,
  vars = q13_vars,
  question_focus = "Importance of different image-generator characteristics",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  response_levels = q13_levels,
  item_labels = q13_item_labels
)

# Cluster 3
q14_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q14",
  "Main challenges when generating images",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q15_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q15",
  "How prompts are usually created",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q16_distribution <- make_single_choice_distribution(
  pre_analysis, "Pre_Survey_Q16",
  "Whether participants actively adjust prompts",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  response_levels = q16_levels
)

q16_score_summary <- make_score_summary(
  pre_analysis, "Pre_Survey_Q16",
  "Whether participants actively adjust prompts",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  analysis_type = "likert_score_summary"
)

q17_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q17",
  "What elements participants include in prompts",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q18_distribution <- make_single_choice_distribution(
  pre_analysis, "Pre_Survey_Q18",
  "How difficult it is to phrase prompts",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  response_levels = q18_levels
)

q18_score_summary <- make_score_summary(
  pre_analysis, "Pre_Survey_Q18",
  "How difficult it is to phrase prompts",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  analysis_type = "likert_score_summary"
)

q19_distribution <- make_single_choice_distribution(
  pre_analysis, "Pre_Survey_Q19",
  "How many iterations are typically needed for a satisfying result",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  response_levels = q19_levels
)

q20_distribution <- make_multiselect_distribution(
  pre_analysis, "Pre_Survey_Q20",
  "What role GenAI plays in the image-creation process",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q21_score_summary <- make_block_score_summary(
  pre_analysis,
  vars = q21_vars,
  question_focus = "Agreement with different statements about image generation",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  item_labels = q21_item_labels
)

q21_distribution <- make_block_distribution(
  pre_analysis,
  vars = q21_vars,
  question_focus = "Agreement with different statements about image generation",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',
  response_levels = q21_levels,
  item_labels = q21_item_labels
)

q24_text_summary <- make_open_text_summary(
  pre_analysis, "Pre_Survey_Q24",
  "Typical situations for using image generators",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q24_text_repeats <- make_open_text_repeats(
  pre_analysis, "Pre_Survey_Q24",
  "Typical situations for using image generators",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q24_text_inventory <- make_open_text_inventory(
  pre_analysis, "Pre_Survey_Q24",
  "Typical situations for using image generators",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q25_text_summary <- make_open_text_summary(
  pre_analysis, "Pre_Survey_Q25",
  "What makes an AI-generated image successful",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q25_text_repeats <- make_open_text_repeats(
  pre_analysis, "Pre_Survey_Q25",
  "What makes an AI-generated image successful",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q25_text_inventory <- make_open_text_inventory(
  pre_analysis, "Pre_Survey_Q25",
  "What makes an AI-generated image successful",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q26_text_summary <- make_open_text_summary(
  pre_analysis, "Pre_Survey_Q26",
  "What makes an AI-generated image unsuccessful",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q26_text_repeats <- make_open_text_repeats(
  pre_analysis, "Pre_Survey_Q26",
  "What makes an AI-generated image unsuccessful",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q26_text_inventory <- make_open_text_inventory(
  pre_analysis, "Pre_Survey_Q26",
  "What makes an AI-generated image unsuccessful",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q27_text_summary <- make_open_text_summary(
  pre_analysis, "Pre_Survey_Q27",
  "What participants find problematic or concerning",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q27_text_repeats <- make_open_text_repeats(
  pre_analysis, "Pre_Survey_Q27",
  "What participants find problematic or concerning",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)

q27_text_inventory <- make_open_text_inventory(
  pre_analysis, "Pre_Survey_Q27",
  "What participants find problematic or concerning",
  filter_expr = filter_q3_yes,
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'
)


q28_numeric_summary <- make_score_summary(
  pre_analysis, "Pre_Survey_Q28",
  "Age",
  filter_expr = filter_all,
  filter_note = "All cleaned Pre-Survey cases",
  score_var = "Pre_Survey_Q28",
  analysis_type = "age_numeric_summary"
)

q28_age_group_distribution <- pre_analysis %>%
  mutate(Pre_Survey_Q28_age_group = make_age_group(Pre_Survey_Q28)) %>%
  make_single_choice_distribution(
    var_name = "Pre_Survey_Q28_age_group",
    question_focus = "Age group",
    filter_expr = filter_q3_yes,  # CHANGED
    filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"',  # CHANGED
    response_levels = c("18-24", "25-34", "35-44", "45-54", "55+")
  )


q29_distribution <- make_single_choice_distribution(
  pre_analysis, "Pre_Survey_Q29",
  "Gender",
  filter_expr = filter_q3_yes,  # CHANGED
  filter_note = 'Only respondents with Pre_Survey_Q3 == "Yes"'  # CHANGED
)


# =========================================================
# 8) Export individual tables                            ===
# =========================================================

output_list <- list(
  "sample_overview" = sample_overview_pre_requested,
  "question_recommend" = question_recommendations,

  "q2_dist" = q2_distribution,
  "q3_dist" = q3_distribution,
  "q4_1_dist" = q4_1_distribution,
  "q4_3_dist" = q4_3_distribution,
  "q22_num_sum" = q22_numeric_summary,
  "q22_band_dist" = q22_band_distribution,
  "q23_dist" = q23_distribution,

  "q6_dist" = q6_distribution,
  "q7_dist" = q7_distribution,
  "q9_dist" = q9_distribution,
  "q10_dist" = q10_distribution,
  "q11_dist" = q11_distribution,
  "q12_dist" = q12_distribution,
  "q13_score_sum" = q13_score_summary,
  "q13_dist" = q13_distribution,

  "q14_dist" = q14_distribution,
  "q15_dist" = q15_distribution,
  "q16_dist" = q16_distribution,
  "q16_score_sum" = q16_score_summary,
  "q17_dist" = q17_distribution,
  "q18_dist" = q18_distribution,
  "q18_score_sum" = q18_score_summary,
  "q19_dist" = q19_distribution,
  "q20_dist" = q20_distribution,
  "q21_score_sum" = q21_score_summary,
  "q21_dist" = q21_distribution,
  "q24_text_sum" = q24_text_summary,
  "q24_text_rep" = q24_text_repeats,
  "q24_text_inv" = q24_text_inventory,
  "q25_text_sum" = q25_text_summary,
  "q25_text_rep" = q25_text_repeats,
  "q25_text_inv" = q25_text_inventory,
  "q26_text_sum" = q26_text_summary,
  "q26_text_rep" = q26_text_repeats,
  "q26_text_inv" = q26_text_inventory,
  "q27_text_sum" = q27_text_summary,
  "q27_text_rep" = q27_text_repeats,
  "q27_text_inv" = q27_text_inventory,
  "q28_num_sum" = q28_numeric_summary,
  "q28_age_dist" = q28_age_group_distribution,
  "q29_dist" = q29_distribution
)

purrr::iwalk(output_list, ~ save_table_outputs(.x, .y))

writexl::write_xlsx(
  output_list,
  path = file.path(out_base_dir, "06_pre_survey_premerge_requested_descriptives.xlsx")
)

# =========================================================
# 9) Console summary and quick check                     ===
# =========================================================

console_summary <- c(
  "==================== REQUESTED PRE-SURVEY DESCRIPTIVES ====================",
  capture.output(print(sample_overview_pre_requested)),
  "",
  "==================== QUESTION RECOMMENDATIONS ====================",
  capture.output(print(question_recommendations)),
  "",
  "==================== OUTPUT OBJECTS ====================",
  paste(names(output_list), collapse = "\n")
)

writeLines(
  console_summary,
  con = file.path(out_doc_dir, "06_pre_survey_premerge_requested_descriptives_console_summary.txt")
)

# =========================================================
# 10) Lokaler Export-Index                                 ===
# =========================================================

export_manifest <- purrr::imap_dfr(
  output_list,
  function(obj, nm) {
    tibble(
      label = c(paste0(nm, " (CSV)"), paste0(nm, " (XLSX)")),
      path = c(
        file.path(out_tables_dir, paste0(nm, ".csv")),
        file.path(out_tables_dir, paste0(nm, ".xlsx"))
      ),
      notes = c("Einzeltabelle als CSV", "Einzeltabelle als XLSX")
    )
  }
) %>%
  bind_rows(
    tibble(
      label = c(
        "Combined workbook",
        "Console summary"
      ),
      path = c(
        file.path(out_base_dir, "06_pre_survey_premerge_requested_descriptives.xlsx"),
        file.path(out_doc_dir, "06_pre_survey_premerge_requested_descriptives_console_summary.txt")
      ),
      notes = c(
        "Kombinierte Excel-Arbeitsmappe aller Outputs",
        "Konsolen- und Prüfzusammenfassung"
      )
    )
  )

save_table_outputs(export_manifest, "00_export_manifest", out_dir = out_doc_dir)

build_general_export_index(
  manifest = export_manifest,
  output_path = file.path(out_doc_dir, "00_export_index.html"),
  title_text = "Pre-Survey descriptives (pre-merge): Export index",
  intro_text = "Dieser Unterindex bündelt die Tabellen- und Dokumentationsdateien des Skripts 06."
)

message("Confirmation: All requested pre-survey descriptive tables were exported successfully.")
message("Individual CSV/XLSX tables: ", out_tables_dir)
message("Combined workbook: ", file.path(out_base_dir, "06_pre_survey_premerge_requested_descriptives.xlsx"))
message("Console summary: ", file.path(out_doc_dir, "06_pre_survey_premerge_requested_descriptives_console_summary.txt"))

#####################################################################
### End of workflow                                               ###
#####################################################################

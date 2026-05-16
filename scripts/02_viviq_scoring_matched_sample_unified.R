#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.

#####################################################################
###          VIVIQ-Auswertung im gematchten Main-Survey           ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript wertet den VIVIQ-Test für jene Personen aus, die nach dem
# vorherigen Data-Cleaning und E-Mail-Matching in beiden Surveys eindeutig
# zugeordnet werden konnten. Grundlage sind die vollständig bereinigten,
# gematchten Fälle mit den neuen, einheitlich präfixierten Variablennamen.

# =========================================================
# 0) Pakete und Voraussetzungen                         ===
# =========================================================

library(tidyverse)
library(writexl)
library(here)
library(gt)

required_objects <- c("main_clean_full", "final_analysis_dataset_full")

if (!all(required_objects %in% ls())) {
  cleaning_script_path <- file.path(here::here(), "scripts", "01_masterarbeit_data_cleaning_workflow_csv.R")

  if (file.exists(cleaning_script_path)) {
    source(cleaning_script_path)
  } else {
    stop(
      "The required objects from the first script were not found. Please run the data cleaning script first or adjust 'cleaning_script_path'.",
      call. = FALSE
    )
  }
} else {
  message("All required objects from the first script are already available.")
}

# =========================================================
# 1) Ordnerstruktur für die VIVIQ-Auswertung            ===
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

#####################################################################
### DOCX-SAFE GT EXPORT COMPATIBILITY (ADDED)                     ###
#####################################################################

# This wrapper keeps the existing HTML/RTF export logic intact and adds
# DOCX output without changing any analysis or table-construction method.
# It also remains compatible with older versions of 00_project_helpers_unified.R
# where save_gt_table() did not yet accept out_gt_docx_dir.
save_gt_table_docx_safe <- function(gt_tbl,
                                    file_stem,
                                    out_gt_html_dir,
                                    out_gt_rtf_dir = NULL,
                                    out_gt_docx_dir = NULL) {
  if (
    exists("save_gt_table", mode = "function") &&
      "out_gt_docx_dir" %in% names(formals(save_gt_table))
  ) {
    return(
      save_gt_table(
        gt_tbl = gt_tbl,
        file_stem = file_stem,
        out_gt_html_dir = out_gt_html_dir,
        out_gt_rtf_dir = out_gt_rtf_dir,
        out_gt_docx_dir = out_gt_docx_dir
      )
    )
  }

  html_path <- file.path(out_gt_html_dir, paste0(file_stem, ".html"))
  saved_rtf <- NA_character_
  saved_docx <- NA_character_

  dir.create(out_gt_html_dir, recursive = TRUE, showWarnings = FALSE)
  gt::gtsave(gt_tbl, filename = html_path)

  if (!is.null(out_gt_rtf_dir)) {
    dir.create(out_gt_rtf_dir, recursive = TRUE, showWarnings = FALSE)
    rtf_path <- file.path(out_gt_rtf_dir, paste0(file_stem, ".rtf"))

    tryCatch(
      {
        gt::gtsave(gt_tbl, filename = rtf_path)
        saved_rtf <- rtf_path
      },
      error = function(e) {
        message(
          "Note: RTF export failed for '", file_stem,
          "'. HTML export still succeeded. Details: ", e$message
        )
      }
    )
  }

  if (!is.null(out_gt_docx_dir)) {
    dir.create(out_gt_docx_dir, recursive = TRUE, showWarnings = FALSE)
    docx_path <- file.path(out_gt_docx_dir, paste0(file_stem, ".docx"))

    source_data <- attr(gt_tbl, "docx_source_data", exact = TRUE)
    if (is.null(source_data) && "_data" %in% names(gt_tbl) && is.data.frame(gt_tbl[["_data"]])) {
      source_data <- gt_tbl[["_data"]]
    }

    title_text <- attr(gt_tbl, "docx_title_text", exact = TRUE)
    if (is.null(title_text) || is.na(title_text) || !nzchar(as.character(title_text))) {
      title_text <- file_stem
    }

    subtitle_text <- attr(gt_tbl, "docx_subtitle_text", exact = TRUE)
    source_note <- attr(gt_tbl, "docx_source_note", exact = TRUE)

    if (!is.null(source_data)) {
      tryCatch(
        {
          if (exists("save_docx_table", mode = "function")) {
            save_docx_table(
              source_data,
              path = docx_path,
              title_text = title_text,
              subtitle_text = subtitle_text,
              source_note = source_note
            )
          } else {
            if (!requireNamespace("flextable", quietly = TRUE) || !requireNamespace("officer", quietly = TRUE)) {
              stop("Packages 'flextable' and 'officer' are required for DOCX export.")
            }

            source_data <- as.data.frame(source_data, stringsAsFactors = FALSE)
            source_data[] <- lapply(source_data, function(x) {
              if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(x))
              if (is.factor(x)) return(as.character(x))
              if (is.list(x)) return(vapply(x, function(z) paste(as.character(z), collapse = "; "), character(1)))
              x
            })

            border_main <- officer::fp_border(color = "#666666", width = 1.25)

            ft <- flextable::flextable(source_data) %>%
              flextable::border_remove() %>%
              flextable::hline_top(part = "header", border = border_main) %>%
              flextable::hline_bottom(part = "header", border = border_main) %>%
              flextable::hline_bottom(part = "body", border = border_main) %>%
              flextable::bold(part = "header") %>%
              flextable::font(fontname = "Arial", part = "all") %>%
              flextable::fontsize(size = 9, part = "all") %>%
              flextable::align(align = "left", part = "all") %>%
              flextable::valign(valign = "top", part = "all") %>%
              flextable::padding(padding.top = 3, padding.bottom = 3, padding.left = 4, padding.right = 4, part = "all") %>%
              flextable::set_table_properties(layout = "autofit", width = 1) %>%
              flextable::autofit()

            header_lines <- c(title_text, subtitle_text)
            header_lines <- header_lines[!is.na(header_lines) & nzchar(as.character(header_lines))]
            if (length(header_lines) > 0) {
              for (header_line in rev(header_lines)) {
                ft <- ft %>% flextable::add_header_lines(values = header_line)
              }
              ft <- ft %>%
                flextable::bold(i = 1, part = "header") %>%
                flextable::fontsize(i = 1, size = 10, part = "header")
            }

            if (!is.null(source_note) && !is.na(source_note) && nzchar(as.character(source_note))) {
              ft <- ft %>%
                flextable::add_footer_lines(values = source_note) %>%
                flextable::italic(part = "footer") %>%
                flextable::fontsize(size = 8, part = "footer")
            }

            flextable::save_as_docx(ft, path = docx_path)
          }
          saved_docx <- docx_path
        },
        error = function(e) {
          message(
            "Note: DOCX export failed for '", file_stem,
            "'. HTML/RTF exports still succeeded where supported. Details: ", e$message
          )
        }
      )
    } else {
      message("Note: DOCX export skipped for '", file_stem, "' because no source data were available in the gt object.")
    }
  }

  tibble::tibble(
    object_name = file_stem,
    html_file = html_path,
    rtf_file = saved_rtf,
    docx_file = saved_docx
  )
}


output_dir <- get_output_dir("02")

out_viviq_dir <- file.path(project_root, "data_output", "viviq")
out_gt_dir <- file.path(out_viviq_dir, "gt_tables")
out_gt_html_dir <- file.path(out_gt_dir, "html")
out_gt_rtf_dir <- file.path(out_gt_dir, "rtf")
out_gt_docx_dir <- file.path(out_gt_dir, "docx")
out_gt_doc_dir <- file.path(out_gt_dir, "documentation")

purrr::walk(
  c(out_viviq_dir, out_gt_dir, out_gt_html_dir, out_gt_rtf_dir, out_gt_docx_dir, out_gt_doc_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# =========================================================
# 2) Vorbereitung der VIVIQ-Auswertung                  ===
# =========================================================

viviq_items <- paste0("Main_Survey_Q", 4:19)
missing_viviq_items <- setdiff(viviq_items, names(main_clean_full))

if (length(missing_viviq_items) > 0) {
  stop(
    paste0(
      "The following VIVIQ variables are missing in the cleaned Main Survey dataset: ",
      paste(missing_viviq_items, collapse = ", ")
    ),
    call. = FALSE
  )
} else {
  message("All VIVIQ variables are available in the cleaned Main Survey dataset.")
}

matched_emails <- final_analysis_dataset_full %>%
  distinct(matched_email)

main_matched_viviq_base <- main_clean_full %>%
  filter(!is.na(Main_Survey_email_clean)) %>%
  semi_join(matched_emails, by = c("Main_Survey_email_clean" = "matched_email")) %>%
  distinct(Main_Survey_email_clean, .keep_all = TRUE)

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

  attach_gt_docx_source(gt_tbl, data, title_text, subtitle_text, NULL)
}

# =========================================================
# 3) Umkodierung der VIVIQ-Antworten                    ===
# =========================================================

score_viviq_response <- function(x) {
  x_clean <- x %>%
    as.character() %>%
    stringr::str_squish()

  dplyr::case_when(
    stringr::str_detect(x_clean, stringr::fixed("Perfectly clear and as vivid as normal vision")) ~ 5,
    stringr::str_detect(x_clean, stringr::fixed("Clear and reasonably vivid")) ~ 4,
    stringr::str_detect(x_clean, stringr::fixed("Moderately clear and vivid")) ~ 3,
    stringr::str_detect(x_clean, stringr::fixed("Vague and dim")) ~ 2,
    stringr::str_detect(x_clean, stringr::fixed("No image at all")) ~ 1,
    TRUE ~ NA_real_
  )
}

main_matched_viviq_scored <- main_matched_viviq_base %>%
  mutate(
    across(
      all_of(viviq_items),
      score_viviq_response,
      .names = "{.col}_score"
    )
  )

# =========================================================
# 4) Berechnung von Mittelwert und Gesamtscore          ===
# =========================================================

viviq_score_vars <- paste0(viviq_items, "_score")

main_matched_viviq_scored <- main_matched_viviq_scored %>%
  rowwise() %>%
  mutate(
    viviq_n_answered = sum(!is.na(c_across(all_of(viviq_score_vars)))),
    viviq_mean_score = if_else(
      viviq_n_answered > 0,
      mean(c_across(all_of(viviq_score_vars)), na.rm = TRUE),
      NA_real_
    ),
    viviq_total_score = if_else(
      viviq_n_answered == length(viviq_score_vars),
      sum(c_across(all_of(viviq_score_vars)), na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup()

# =========================================================
# 5) VIVIQ-Daten in finalen Analyse-Datensatz integrieren ===
# =========================================================

main_matched_viviq_final <- main_matched_viviq_scored %>%
  select(
    Main_Survey_ResponseId,
    Main_Survey_email_clean,
    all_of(viviq_items),
    all_of(viviq_score_vars),
    viviq_n_answered,
    viviq_mean_score,
    viviq_total_score,
    everything()
  )

final_analysis_dataset_full_viviq <- final_analysis_dataset_full %>%
  left_join(
    main_matched_viviq_final %>%
      select(
        Main_Survey_email_clean,
        all_of(viviq_items),
        all_of(viviq_score_vars),
        viviq_n_answered,
        viviq_mean_score,
        viviq_total_score
      ),
    by = c("matched_email" = "Main_Survey_email_clean")
  )

# =========================================================
# 6) Dokumentation der VIVIQ-Auswertung                 ===
# =========================================================

### BESCHREIBUNG ###

# In diesem Abschnitt werden die wichtigsten Kennzahlen zur VIVIQ-Auswertung
# dokumentiert und exportiert. Dadurch kann die Score-Bildung später im
# Methoden- oder Anhangsteil der Masterarbeit transparent dargestellt werden.

viviq_score_summary <- tibble(
  category = c(
    "Total matched Main Survey cases",
    "Cases with at least one answered VIVIQ item",
    "Cases with fully completed VIVIQ (all 16 items)",
    "Mean VIVIQ total score",
    "Standard deviation of VIVIQ total score",
    "Minimum VIVIQ total score",
    "Maximum VIVIQ total score"
  ),
  value = c(
    nrow(main_matched_viviq_final),
    sum(main_matched_viviq_final$viviq_n_answered > 0, na.rm = TRUE),
    sum(main_matched_viviq_final$viviq_n_answered == length(viviq_items), na.rm = TRUE),
    mean(main_matched_viviq_final$viviq_total_score, na.rm = TRUE),
    sd(main_matched_viviq_final$viviq_total_score, na.rm = TRUE),
    min(main_matched_viviq_final$viviq_total_score, na.rm = TRUE),
    max(main_matched_viviq_final$viviq_total_score, na.rm = TRUE)
  )
)

viviq_item_summary <- main_matched_viviq_final %>%
  summarise(
    across(
      all_of(viviq_score_vars),
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item_stat",
    values_to = "value"
  ) %>%
  mutate(
    value = round(value, 2)
  )

# =========================================================
# 8) Ergebnisse exportieren                             ===
# =========================================================

writexl::write_xlsx(
  list(
    main_matched_viviq_final = main_matched_viviq_final,
    final_analysis_dataset_full_viviq = final_analysis_dataset_full_viviq,
    viviq_score_summary = viviq_score_summary,
    viviq_item_summary = viviq_item_summary
  ),
  path = file.path(out_viviq_dir, "07_viviq_scoring_matched_sample.xlsx")
)

readr::write_csv(
  main_matched_viviq_final,
  file = file.path(out_viviq_dir, "07_viviq_scoring_matched_sample.csv")
)

readr::write_csv(
  final_analysis_dataset_full_viviq,
  file = file.path(out_viviq_dir, "07_viviq_final_analysis_dataset_full.csv")
)


# =========================================================
# 10) Schnellcheck in der Konsole                    ===
# =========================================================

### BESCHREIBUNG ###

# In diesem Abschnitt werden die wichtigsten Kennzahlen direkt in der Konsole
# ausgegeben. Dadurch kann schnell geprüft werden, ob die VIVIQ-Umkodierung
# und Score-Bildung plausibel funktioniert haben.

cat("\n==================== VIVIQ SCORE SUMMARY ====================\n")
print(viviq_score_summary)

# =========================================================
# 11) Wissenschaftliche Tabellen anzeigen                ===
# =========================================================

gt_viviq_score_summary <- make_gt_table(
  viviq_score_summary,
  title_text = "Overview of VIVIQ overall metrics",
  subtitle_text = "Matched Main Survey cases"
) %>%
  gt::fmt(
    columns = value,
    fns = function(x) {
      out <- ifelse(
        viviq_score_summary$category %in% c(
          "Total matched Main Survey cases",
          "Cases with at least one answered VIVIQ item",
          "Cases with fully completed VIVIQ (all 16 items)"
        ),
        as.character(as.integer(round(x, 0))),
        sprintf("%.2f", x)
      )
      out
    }
  )

gt_viviq_item_summary <- make_gt_table(
  viviq_item_summary,
  title_text = "Overview of VIVIQ item metrics",
  subtitle_text = "Means and standard deviations of the scored items"
) %>%
  gt::fmt_number(
    columns = value,
    decimals = 2
  ) %>%
  gt::tab_source_note(
    source_note = paste0(
      "Note: total number of VIVIQ items = ", length(viviq_items),
      ", mean VIVIQ total score = ", sprintf("%.2f", mean(main_matched_viviq_final$viviq_total_score, na.rm = TRUE)),
      ", standard deviation of VIVIQ total score = ", sprintf("%.2f", sd(main_matched_viviq_final$viviq_total_score, na.rm = TRUE))
    )
  )

gt_output_list <- list(
  gt_viviq_score_summary = gt_viviq_score_summary,
  gt_viviq_item_summary = gt_viviq_item_summary
)

gt_manifest <- purrr::imap_dfr(
  gt_output_list,
  function(gt_tbl, object_name) {
    save_gt_table_docx_safe(
      gt_tbl = gt_tbl,
      file_stem = object_name,
      out_gt_html_dir = out_gt_html_dir,
      out_gt_rtf_dir = out_gt_rtf_dir,
      out_gt_docx_dir = out_gt_docx_dir
    )
  }
)

save_table_outputs(
  gt_manifest,
  base_filename = "02_viviq_gt_manifest",
  out_dir = out_gt_doc_dir
)

build_simple_html_index(
  manifest = gt_manifest,
  output_path = file.path(out_gt_dir, "00_gt_index.html"),
  title_text = "VIVIQ scoring - gt outputs",
  intro_text = "This index links to all formatted gt tables created from the VIVIQ scoring workflow."
)

gt_console_summary <- c(
  "==================== VIVIQ GT TABLES ====================",
  capture.output(print(gt_manifest)),
  "",
  paste0("HTML directory: ", out_gt_html_dir),
  paste0("RTF directory: ", out_gt_rtf_dir),
  paste0("DOCX directory: ", out_gt_docx_dir),
  paste0("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
)

writeLines(
  gt_console_summary,
  con = file.path(out_gt_doc_dir, "02_viviq_gt_console_summary.txt")
)

message("Confirmation: GT tables for the VIVIQ workflow were exported successfully.")
message("HTML tables: ", out_gt_html_dir)
message("RTF tables (where supported): ", out_gt_rtf_dir)
message("DOCX tables: ", out_gt_docx_dir)
message("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
message("Manifest: ", file.path(out_gt_doc_dir, "02_viviq_gt_manifest.csv"))

# gt_viviq_score_summary
# gt_viviq_item_summary

#####################################################################
###                    Ende des Workflows                         ###
#####################################################################

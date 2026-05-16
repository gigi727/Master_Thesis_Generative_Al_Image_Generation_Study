#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.

#####################################################################
###         Variablenübersicht mit Normierung und Typen           ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript erstellt eine vollständige Übersicht über alle Variablen
# der anonymisierten Pre- und Main-Survey-Datensätze. Für jede Variable werden Datensatz,
# Variablenname, Fragetext, ein beobachteter Beispielwert,
# Antworttyp sowie eine vorhandene Normierung dokumentiert.
# Das Skript baut direkt auf dem zentralen Cleaning-Skript auf und
# verwendet die dort definierten präfixierten Variablennamen.

# =========================================================
# 0) Pakete                                              ===
#
# In diesem Abschnitt werden die für das Erstellen,
# Dokumentieren und Exportieren der Variablenübersicht
# benötigten Pakete installiert und geladen.
# =========================================================

#install.packages(c("tidyverse", "writexl", "here", “gt“), dependencies = TRUE)

library(tidyverse)
library(writexl)
library(here)
library(gt)

#####################################################################
###                Pfade und Ordnerstruktur definieren            ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden die Pfade zum zentralen Cleaning-Skript
# sowie die Ausgabeordner für Tabellen und Dokumentation definiert.
# Die Ergebnisse werden in einem eigenen Unterordner gespeichert,
# damit das Variableninventar separat von den Analyseergebnissen
# dokumentiert werden kann.

# =========================================================
# 1) Pfade und Ausgabeordner                             ===
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


out_inventory_dir <- file.path(project_root, "data_output", "variable_inventory")
out_tables_dir <- file.path(out_inventory_dir, "tables")
out_document_dir <- file.path(out_inventory_dir, "documentation")
out_gt_dir <- file.path(out_inventory_dir, "gt_tables")
out_gt_html_dir <- file.path(out_gt_dir, "html")
out_gt_rtf_dir <- file.path(out_gt_dir, "rtf")
out_gt_docx_dir <- file.path(out_gt_dir, "docx")
out_gt_doc_dir <- file.path(out_gt_dir, "documentation")

purrr::walk(
  c(out_inventory_dir, out_tables_dir, out_document_dir, out_gt_dir, out_gt_html_dir, out_gt_rtf_dir, out_gt_docx_dir, out_gt_doc_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)


#####################################################################
###         Benötigte Objekte prüfen und Cleaning laden           ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript setzt auf dem zentralen Cleaning-Skript auf.
# Falls die dort erzeugten Objekte noch nicht in der Global Environment
# vorhanden sind, wird das Skript automatisch geladen.
# Anschließend wird geprüft, ob alle benötigten Objekte tatsächlich
# vorhanden sind.

# =========================================================
# 2) Benötigte Objekte definieren                        ===
# =========================================================

# =========================================================
# 2) Anonymisierte Analyse-Datensätze laden              ===
# =========================================================

# Das Variableninventar basiert nun ausschließlich auf den öffentlich
# verwendbaren anonymisierten Datensätzen in data_final/. Es werden keine
# Rohdaten und keine Outputs aus 01-03 geladen.

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

# Für die bestehende Inventarlogik werden raw/clean-Objekte gleichgesetzt.
# Methodisch ändert sich dadurch keine Analyse; es wird lediglich verhindert,
# dass Rohdaten oder nicht-anonymisierte Cleaning-Objekte benötigt werden.
pre_raw <- pre_survey_dataset
main_raw <- main_survey_dataset
pre_clean_full <- pre_survey_dataset
main_clean_full <- main_survey_dataset

message("Confirmation: Variable inventory uses only anonymized datasets from data_final/.")

#####################################################################
###                    Hilfsfunktionen                            ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden Hilfsfunktionen definiert, die für
# - die Erkennung des Antworttyps,
# - die Auswahl eines beobachteten Beispielwertes,
# - und den Aufbau des Variableninventars
# verwendet werden.

# =========================================================
# 4) Hilfsfunktionen                                      ===
# =========================================================

detect_answer_type <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) return("Date")
  if (is.logical(x)) return("Logical")
  if (is.numeric(x)) return("Number")
  if (is.factor(x)) return("Factor")
  if (is.character(x)) return("String")
  class(x)[1]
}

sample_nonmissing_value <- function(x) {
  vals <- x[!is.na(x) & as.character(x) != ""]
  if (length(vals) == 0) return(NA_character_)
  as.character(sample(vals, 1))
}

build_inventory <- function(df_raw, df_clean, lookup_df, dataset_label, normalization_codebook) {
  tibble(variable = names(df_raw)) %>%
    mutate(
      dataset = dataset_label,
      random_value = purrr::map_chr(variable, ~ sample_nonmissing_value(df_raw[[.x]])),
      answer_type = purrr::map_chr(variable, ~ detect_answer_type(df_raw[[.x]])),
      score_variable = if_else(
        paste0(variable, "_score") %in% names(df_clean),
        paste0(variable, "_score"),
        NA_character_
      )
    ) %>%
    left_join(
      lookup_df %>% rename(variable = variable_name),
      by = "variable"
    ) %>%
    left_join(
      normalization_codebook %>%
        select(dataset, variable, score_variable, normalization),
      by = c("dataset", "variable", "score_variable")
    ) %>%
    mutate(
      has_score_variable = !is.na(score_variable),
      normalization = if_else(is.na(normalization), "No explicit normalization", normalization)
    ) %>%
    select(
      dataset,
      variable,
      question_text,
      random_value,
      answer_type,
      has_score_variable,
      score_variable,
      normalization
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

  attach_gt_docx_source(gt_tbl, data, title_text, subtitle_text, NULL)
}

#####################################################################
###                  Normierungscodebuch erstellen                ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt wird das Codebuch der explizit normierten Variablen
# erstellt. Es dokumentiert pro Variable den zugehörigen Datensatz,
# die numerische Score-Variable sowie die textuelle Beschreibung
# der verwendeten Normierung.

# =========================================================
# 5) Normierungscodebuch                                  ===
# =========================================================

normalization_codebook <- tribble(
  ~dataset, ~variable, ~score_variable, ~normalization,
  "Pre_Survey", "Pre_Survey_Q8", "Pre_Survey_Q8_score", "1=Very inexperienced; 2=Inexperienced; 3=Slightly inexperienced; 4=Neither inexperienced nor experienced; 5=Slightly experienced; 6=Experienced; 7=Very experienced",
  "Pre_Survey", "Pre_Survey_Q13_1", "Pre_Survey_Q13_1_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_2", "Pre_Survey_Q13_2_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_3", "Pre_Survey_Q13_3_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_4", "Pre_Survey_Q13_4_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_5", "Pre_Survey_Q13_5_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q13_6", "Pre_Survey_Q13_6_score", "1=Not a priority; 2=Low priority; 3=Somewhat priority; 4=Neutral; 5=Moderate priority; 6=High priority; 7=Essential priority",
  "Pre_Survey", "Pre_Survey_Q16", "Pre_Survey_Q16_score", "1=Strongly disagree; 2=Disagree; 3=Somewhat disagree; 4=Neither agree nor disagree; 5=Somewhat agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q18", "Pre_Survey_Q18_score", "1=Extremely easy; 2=Very easy; 3=Somewhat easy; 4=Neither easy nor difficult; 5=Somewhat difficult; 6=Very difficult; 7=Extremely difficult",
  "Pre_Survey", "Pre_Survey_Q21_1", "Pre_Survey_Q21_1_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q21_2", "Pre_Survey_Q21_2_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q21_3", "Pre_Survey_Q21_3_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q21_4", "Pre_Survey_Q21_4_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Pre_Survey", "Pre_Survey_Q21_5", "Pre_Survey_Q21_5_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q21", "Main_Survey_Q21_score", "1=Not vivid at all; 2=Very weak; 3=Weak; 4=Moderately vivid; 5=Quite vivid; 6=Very vivid; 7=Extremely vivid",
  "Main_Survey", "Main_Survey_Q26", "Main_Survey_Q26_score", "1=Not at all; 2=Very weakly; 3=Weakly; 4=Moderately; 5=Strongly; 6=Very strongly; 7=Almost exactly",
  "Main_Survey", "Main_Survey_Q34", "Main_Survey_Q34_score", "1=Not at all; 2=Very weakly; 3=Weakly; 4=Moderately; 5=Strongly; 6=Very strongly; 7=Almost exactly",
  "Main_Survey", "Main_Survey_Q42", "Main_Survey_Q42_score", "1=Not at all; 2=Very weakly; 3=Weakly; 4=Moderately; 5=Strongly; 6=Very strongly; 7=Almost exactly",
  "Main_Survey", "Main_Survey_Q27_1", "Main_Survey_Q27_1_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q27_2", "Main_Survey_Q27_2_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q27_3", "Main_Survey_Q27_3_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q35_1", "Main_Survey_Q35_1_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q35_2", "Main_Survey_Q35_2_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q35_3", "Main_Survey_Q35_3_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q43_1", "Main_Survey_Q43_1_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q43_2", "Main_Survey_Q43_2_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q43_3", "Main_Survey_Q43_3_score", "1=Strongly disagree; 2=Disagree; 3=Slightly disagree; 4=Neither agree nor disagree; 5=Slightly agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q28", "Main_Survey_Q28_score", "1=Strongly disagree; 2=Disagree; 3=Somewhat disagree; 4=Neither agree nor disagree; 5=Somewhat agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q36", "Main_Survey_Q36_score", "1=Strongly disagree; 2=Disagree; 3=Somewhat disagree; 4=Neither agree nor disagree; 5=Somewhat agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q44", "Main_Survey_Q44_score", "1=Strongly disagree; 2=Disagree; 3=Somewhat disagree; 4=Neither agree nor disagree; 5=Somewhat agree; 6=Agree; 7=Strongly agree",
  "Main_Survey", "Main_Survey_Q48", "Main_Survey_Q48_score", "1=Completely different; 2=Very different; 3=Somewhat different; 4=Moderately similar; 5=Quite similar; 6=Very similar; 7=Essentially the same",
  "Main_Survey", "Main_Survey_Q52", "Main_Survey_Q52_score", "1=Not at all; 2=Very weakly; 3=Weakly; 4=Moderately; 5=Strongly; 6=Very strongly; 7=Almost exactly",
  "Main_Survey", "Main_Survey_Q53", "Main_Survey_Q53_score", "1=Strongly prefer my own mental images; 2=Prefer my own mental images; 3=Slightly prefer my own mental images; 4=No preference; 5=Slightly prefer AI-generated images; 6=Prefer AI-generated images; 7=Strongly prefer AI-generated images"
)


#####################################################################
###                Variableninventar aufbauen                     ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt wird für beide Datensätze jeweils ein Inventar
# aller Variablen erzeugt. Anschließend werden beide Tabellen zu einer
# gemeinsamen Übersicht zusammengeführt.

# =========================================================
# 6) Variableninventar erstellen                          ===
# =========================================================

variable_inventory_pre <- build_inventory(
  df_raw = pre_raw,
  df_clean = pre_clean_full,
  lookup_df = pre_feature_lookup,
  dataset_label = "Pre_Survey",
  normalization_codebook = normalization_codebook
)

variable_inventory_main <- build_inventory(
  df_raw = main_raw,
  df_clean = main_clean_full,
  lookup_df = main_feature_lookup,
  dataset_label = "Main_Survey",
  normalization_codebook = normalization_codebook
)

derived_main_variable_target_word_category <- tibble(
  dataset = "Main_Survey",
  variable = "Main_Survey_target_word_category",
  question_text = "Derived categorization of Main_Survey_target_word (abstract vs. concrete).",
  random_value = sample_nonmissing_value(main_clean_full$Main_Survey_target_word_category),
  answer_type = detect_answer_type(main_clean_full$Main_Survey_target_word_category),
  has_score_variable = FALSE,
  score_variable = NA_character_,
  normalization = "No explicit normalization; rule-based assignment from Main_Survey_target_word."
)

variable_inventory_main <- bind_rows(
  variable_inventory_main,
  derived_main_variable_target_word_category
)

variable_inventory_all <- bind_rows(variable_inventory_pre, variable_inventory_main)

#####################################################################
###                     Ergebnisse exportieren                     ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden das vollständige Variableninventar,
# die datensatzspezifischen Teiltabellen sowie das Normierungscodebuch
# als CSV- und Excel-Dateien exportiert.

# =========================================================
# 7) Exporte                                               ===
# =========================================================

readr::write_csv(variable_inventory_all, file.path(out_tables_dir, "04_variable_overview_with_normalization.csv"))
readr::write_csv(variable_inventory_pre, file.path(out_tables_dir, "04_pre_variable_overview_with_normalization.csv"))
readr::write_csv(variable_inventory_main, file.path(out_tables_dir, "04_main_variable_overview_with_normalization.csv"))
readr::write_csv(normalization_codebook, file.path(out_document_dir, "04_normalization_codebook.csv"))

writexl::write_xlsx(
  list(
    variable_inventory_all = variable_inventory_all,
    variable_inventory_pre = variable_inventory_pre,
    variable_inventory_main = variable_inventory_main,
    normalization_codebook = normalization_codebook
  ),
  path = file.path(out_inventory_dir, "04_variable_overview_with_normalization.xlsx")
)


#####################################################################
###                    Schnellcheck in der Konsole                ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden zentrale Kennzahlen des Variableninventars
# direkt in der Konsole ausgegeben, damit der erfolgreiche Aufbau der
# Übersichten rasch überprüft werden kann.

# =========================================================
# 8) Konsolenausgaben                                    ===
# =========================================================

cat("\n==================== VARIABLE INVENTORY OVERVIEW ====================\n")
print(tibble(
  n_variables_pre = nrow(variable_inventory_pre),
  n_variables_main = nrow(variable_inventory_main),
  n_variables_total = nrow(variable_inventory_all),
  n_normalized_variables = sum(variable_inventory_all$has_score_variable, na.rm = TRUE)
))

cat("\n==================== EXAMPLE PRE SURVEY ====================\n")
print(head(variable_inventory_pre, 10))

cat("\n==================== EXAMPLE MAIN SURVEY ====================\n")
print(head(variable_inventory_main, 10))

#####################################################################
###              Wissenschaftliche Tabellen anzeigen              ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden die zentralen Tabellen des
# Variableninventars zusätzlich als wissenschaftlich formatierte
# Tabellen dargestellt. Die Inhalte bleiben unverändert; es erfolgt
# ausschließlich eine zusätzliche tabellarische Darstellung.

# =========================================================
# 9) Wissenschaftliche Tabellen anzeigen                ===
# =========================================================

gt_variable_inventory_all <- make_gt_table(
  variable_inventory_all,
  title_text = "Overview of all variables with normalization and types",
  subtitle_text = "Pre-Survey and Main-Survey"
)

gt_variable_inventory_pre <- make_gt_table(
  variable_inventory_pre,
  title_text = "Overview of variables in the Pre-Survey",
  subtitle_text = "With normalization and answer types"
)

gt_variable_inventory_main <- make_gt_table(
  variable_inventory_main,
  title_text = "Overview of variables in the Main-Survey",
  subtitle_text = "With normalization and answer types"
)

gt_normalization_codebook <- make_gt_table(
  normalization_codebook,
  title_text = "Overview of the normalization codebook",
  subtitle_text = NULL
)

gt_output_list <- list(
  gt_variable_inventory_all = gt_variable_inventory_all,
  gt_variable_inventory_pre = gt_variable_inventory_pre,
  gt_variable_inventory_main = gt_variable_inventory_main,
  gt_normalization_codebook = gt_normalization_codebook
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
  base_filename = "04_variable_inventory_gt_manifest",
  out_dir = out_gt_doc_dir
)

build_simple_html_index(
  manifest = gt_manifest,
  output_path = file.path(out_gt_dir, "00_gt_index.html"),
  title_text = "Variable inventory - gt outputs",
  intro_text = "This index links to all formatted gt tables created from the variable inventory workflow."
)

gt_console_summary <- c(
  "==================== VARIABLE INVENTORY GT TABLES ====================",
  capture.output(print(gt_manifest)),
  "",
  paste0("HTML directory: ", out_gt_html_dir),
  paste0("RTF directory: ", out_gt_rtf_dir),
  paste0("DOCX directory: ", out_gt_docx_dir),
  paste0("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
)

writeLines(
  gt_console_summary,
  con = file.path(out_gt_doc_dir, "04_variable_inventory_gt_console_summary.txt")
)

message("Confirmation: GT tables for the variable inventory workflow were exported successfully.")
message("HTML tables: ", out_gt_html_dir)
message("RTF tables (where supported): ", out_gt_rtf_dir)
message("DOCX tables: ", out_gt_docx_dir)
message("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
message("Manifest: ", file.path(out_gt_doc_dir, "04_variable_inventory_gt_manifest.csv"))

gt_variable_inventory_all
gt_variable_inventory_pre
gt_variable_inventory_main
gt_normalization_codebook

#####################################################################
###                    Ende des Workflows                         ###
#####################################################################

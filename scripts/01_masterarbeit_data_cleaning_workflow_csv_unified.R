#####################################################################
### KONSOLIDIERTE VERSION                                         ###
#####################################################################

# Diese Version verwendet das zentrale Helper-Skript
# `00_project_helpers_unified.R` für methodisch neutrale
# Infrastrukturbausteine. Die inhaltliche Analyse- und
# Methodenlogik des Ursprungsskripts bleibt unverändert.

#####################################################################
###           Qualtrics Data-Cleaning Script                     ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript bildet einen vollständigen, reproduzierbaren Workflow für das
# Einlesen, Bereinigen, Dokumentieren, Normieren und Matching der beiden
# Survey-Datensätze auf Basis von CSV-Dateien. Alle Pre-Survey-Variablen
# erhalten direkt nach dem Einlesen das Präfix "Pre_Survey_", alle Main-Survey-
# Variablen das Präfix "Main_Survey_". Numerische Skalenvariablen werden
# ebenfalls früh im Workflow ergänzt und jeweils direkt hinter ihrer
# Ursprungsvariable platziert.

# =========================================================
# 0) Pakete                                          ======
# =========================================================

#install.packages(c("tidyverse", "writexl", "janitor", "here", "gt"), dependencies = TRUE)

library(tidyverse)
library(writexl)
library(janitor)
library(here)
library(gt)

#####################################################################
### GT STANDARD STYLE (ADDED)                                     ###
#####################################################################

apply_standard_gt_style <- function(gt_table, title_text, subtitle_text, source_note_text) {
  gt_table %>%
    gt::tab_header(
      title = title_text,
      subtitle = subtitle_text
    ) %>%
    gt::tab_source_note(
      source_note = source_note_text
    )
}


# =========================================================
# 1) Projektstruktur und Pfade                         ====
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

# get_output_dir() is defined in 00_project_helpers_unified.R,
# so the helper must be sourced before this call.
output_dir <- get_output_dir("01")

raw_dir            <- file.path(project_root, "data_raw")
file_pre_data      <- file.path(raw_dir, "Pre_Survey_All.csv")

message("Project root: ", project_root)
message("Pre-survey input file: ", file_pre_data)
if (!file.exists(file_pre_data)) {
  warning("Pre-survey input file not found at: ", file_pre_data)
}

raw_dir            <- file.path(project_root, "data_raw")
out_clean_dir      <- file.path(project_root, "data_output", "clean")
out_excluded_dir   <- file.path(project_root, "data_output", "excluded")
out_review_dir     <- file.path(project_root, "data_output", "review")
out_matching_dir   <- file.path(project_root, "data_output", "matching")
out_document_dir   <- file.path(project_root, "data_output", "documentation")
out_gt_dir         <- file.path(out_document_dir, "gt_tables")
out_gt_html_dir    <- file.path(out_gt_dir, "html")
out_gt_rtf_dir     <- file.path(out_gt_dir, "rtf")
out_gt_doc_dir     <- file.path(out_gt_dir, "documentation")

purrr::walk(
  c(
    out_clean_dir, out_excluded_dir, out_review_dir, out_matching_dir, out_document_dir,
    out_gt_dir, out_gt_html_dir, out_gt_rtf_dir, out_gt_doc_dir
  ),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

file_pre_data      <- file.path(raw_dir, "Pre_Survey_All.csv")
file_pre_features  <- file.path(raw_dir, "Pre_Survey_All_Features.csv")
file_main_data     <- file.path(raw_dir, "Main_Survey_ALL.csv")
file_main_features <- file.path(raw_dir, "Main_Survey_ALL_Features.csv")

# =========================================================
# 2) Zentrale Konfiguration                            ====
# =========================================================

config_pre <- list(
  id_var               = "Pre_Survey_ResponseId",
  status_var           = "Pre_Survey_Status",
  finished_var         = "Pre_Survey_Finished",
  ip_var               = "Pre_Survey_IPAddress",
  consent_var          = "Pre_Survey_Q1",
  followup_var         = "Pre_Survey_Q30",
  email_var            = "Pre_Survey_Q32",
  preview_value        = "Survey Preview",
  keep_status_value    = "IP Address",
  consent_yes          = "Yes, I agree to participate",
  consent_no           = "No, I do not wish to participate",
  followup_yes_text    = "YES! I want to fill out the second survey and get a chance to win 2 cinema tickets (please write your e-mail in the text box below)",
  dataset_name         = "Pre_Survey",
  column_prefix        = "Pre_Survey_"
)

config_main <- list(
  id_var               = "Main_Survey_ResponseId",
  status_var           = "Main_Survey_Status",
  finished_var         = "Main_Survey_Finished",
  ip_var               = "Main_Survey_IPAddress",
  consent_var          = "Main_Survey_Q1",
  email_var            = "Main_Survey_Q2",
  preview_value        = "Survey Preview",
  keep_status_value    = "IP Address",
  consent_yes          = "Yes, I agree to participate",
  consent_no           = "No, I do not wish to participate",
  dataset_name         = "Main_Survey",
  column_prefix        = "Main_Survey_"
)

# =========================================================
# 3) Hilfsfunktionen                                    ====
# =========================================================

## CSV-Einlesefunktion ##
# `read_csv_auto()` wird zentral aus `00_project_helpers_unified.R` geladen.

## Spaltenpräfix-Funktion ##
# Diese Funktion fügt allen Spaltennamen eines Data Frames ein angegebenes Präfix hinzu,
# um die Variablen klar einem bestimmten Datensatz zuordnen zu können. Dies ist besonders hilfreich,
# wenn später beide Datensätze zusammengeführt oder verglichen werden sollen.
prefix_dataset_columns <- function(df, prefix) {
  names(df) <- paste0(prefix, names(df))
  df
}

## Feature-Lookup-Funktion ##
# Diese Funktion erstellt aus einem Data Frame, der die ursprünglichen Spaltennamen
# und die zugehörigen Frageformulierungen enthält, einen Lookup-Table. Dabei wird das
# Präfix für die Variablennamen entsprechend dem Datensatz (Pre oder Main) hinzugefügt,
# um eine eindeutige Zuordnung zu ermöglichen. Der resultierende Data Frame
# enthält die Spalten "survey", "variable_name" und "question_text".
make_feature_lookup <- function(feature_df, survey_name, column_prefix) {
  feature_df <- dplyr::as_tibble(feature_df)

  if (nrow(feature_df) == 0) {
    stop(
      paste0("Feature lookup input for ", survey_name, " is empty."),
      call. = FALSE
    )
  }

  # Use base row subsetting instead of unqualified slice().
  # This avoids namespace conflicts in interactive RStudio sessions where
  # another package may mask dplyr::slice().
  feature_df[1, , drop = FALSE] %>%
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "variable_name",
      values_to = "question_text"
    ) %>%
    dplyr::mutate(
      survey = survey_name,
      variable_name = paste0(column_prefix, variable_name),
      question_text = as.character(question_text)
    ) %>%
    dplyr::select(survey, variable_name, question_text)
}

## E-Mail-Bereinigungsfunktion ##
# Diese Funktion bereinigt E-Mail-Adressen, indem sie führende und nachfolgende Whitespaces entfernt,
# alle Zeichen in Kleinbuchstaben umwandelt und alle inneren Whitespaces entfernt.
# Leere Strings werden in NA umgewandelt. Das Ziel ist es, eine standardisierte Version
# der E-Mail-Adressen zu erhalten, die für weitere Validierungs- und Matching-Schritte verwendet werden kann.
clean_email <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim(side = "both") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", "") %>%
    na_if("")
}

## E-Mail-Validierungsfunktion ##
# Diese Funktion überprüft, ob eine gegebene Zeichenkette dem allgemeinen Format
# einer E-Mail-Adresse entspricht. Sie verwendet einen regulären Ausdruck, um sicherzustellen,
# dass die Adresse aus einem lokalen Teil, einem "@"-Symbol und einem Domain-Teil besteht,
# wobei der Domain-Teil mindestens einen Punkt und eine gültige Top-Level-Domain enthält.
# Das Ziel ist es, formal ungültige E-Mail-Adressen zu identifizieren.
is_valid_email <- function(x) {
  stringr::str_detect(
    x,
    "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
  )
}

## Textbereinigungsfunktion ##
## Diese Funktion bereinigt Textspalten, indem sie problematische Zeichenfolgen robust ersetzt,
# häufige typografische Varianten vereinheitlicht und überflüssige Whitespaces entfernt.
# Ziel ist es, eine konsistente und saubere Textdarstellung zu gewährleisten, die für weitere Analysen oder Vergleiche besser geeignet ist.
clean_text_encoding <- function(x) {
  x <- as.character(x)

  # problematische Zeichenfolgen robust ersetzen
  x <- iconv(x, from = "", to = "UTF-8", sub = "-")

  # häufige typografische Varianten vereinheitlichen
  x <- stringr::str_replace_all(x, "[\u2013\u2014]", "-")
  x <- stringr::str_replace_all(x, "[\u2018\u2019]", "'")
  x <- stringr::str_replace_all(x, "[\u201C\u201D]", "\"")

  # Whitespaces säubern
  x <- stringr::str_squish(x)

  x
}

## Funktion zur Bereinigung aller Character-Spalten in einem Data Frame ##
# Diese Funktion wendet die Textbereinigungsfunktion auf alle Spalten eines Data Frames
# an, die den Datentyp "character" haben. Ziel ist es, sicherzustellen, dass alle Textdaten
# in einem konsistenten und bereinigten Format vorliegen, bevor weitere Analysen oder Vergleiche durchgeführt werden.
clean_character_columns <- function(df) {
  df %>%
    mutate(
      across(
        where(is.character),
        clean_text_encoding
      )
    )
}

## Umwandlungsfunktion für "Finished" ##
# Diese Funktion wandelt die Werte der "Finished"-Spalte in logische Werte um, indem sie
# verschiedene mögliche Darstellungen von "true" und "false" erkennt. Sie bereinigt die Eingabe,
# indem sie führende und nachfolgende Whitespaces entfernt, alle Zeichen in Kleinbuchstaben umwandelt
# und dann prüft, ob die bereinigte Zeichenkette einer der bekannten Wahrheitswerte entspricht.
# Ziel ist es, eine konsistente logische Darstellung der Fertigstellungsstatus zu erhalten.
parse_finished_to_logical <- function(x) {
  x_clean <- x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()

  case_when(
    x_clean %in% c("true", "t", "1", "yes")  ~ TRUE,
    x_clean %in% c("false", "f", "0", "no") ~ FALSE,
    TRUE ~ NA
  )
}

## Make-n-Tabelle-Funktion ##
# Diese Funktion erstellt eine Tabelle, die die Anzahl der Fälle vor und nach einem Bereinigungsschritt
# sowie die Anzahl der entfernten Fälle dokumentiert. Sie nimmt den Namen des Bereinigungsschritts,
# die Anzahl der Fälle vor der Bereinigung, die Anzahl der entfernten Fälle und die Anzahl der
# verbleibenden Fälle als Eingabe und gibt einen Data Frame mit diesen Informationen zurück.
# Ziel ist es, eine klare und strukturierte Übersicht über die Auswirkungen jedes Bereinigungsschritts zu erhalten.
make_n_table <- function(step, n_before, n_removed, n_kept) {
  tibble(data_cleaning_step = step, n_before = n_before, n_removed = n_removed, n_kept = n_kept)
}

## Normalisierungsfunktion für Skalentexte ##
# Diese Funktion bereinigt und normalisiert Textwerte, die Skalenantworten repräsentieren,
# indem sie führende und nachfolgende Whitespaces entfernt, überflüssige Whitespaces innerhalb
# des Textes reduziert und alle Zeichen in Kleinbuchstaben umwandelt. Ziel ist es, eine konsistente
# Textdarstellung zu gewährleisten, die für die Umkodierung von Skalenantworten in numerische Werte besser geeignet ist.
normalize_scale_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
}

## Funktion zur Umkodierung von Skalentexten mit Mapping ##
# Diese Funktion nimmt einen Vektor von Skalentexten und eine benutzerdefinierte Mapping-Liste,
# die die Normalisierten Skalentexte den entsprechenden numerischen Werten zuordnet.
# Sie bereinigt und normalisiert die Skalentexte, konvertiert numerische Werte direkt und verwendet das Mapping,
# um die entsprechenden numerischen Werte für die Textantworten zuzuordnen.
# Ziel ist es, eine flexible und robuste Umkodierung von Skalentexten in numerische Werte zu ermöglichen, basierend auf einem benutzerdefinierten Mapping.
recode_scale_with_map <- function(x, mapping) {
  x_chr <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_chr))
  x_norm <- normalize_scale_text(x_chr)

  out <- rep(NA_real_, length(x_chr))
  out[!is.na(x_num)] <- x_num[!is.na(x_num)]

  for (label in names(mapping)) {
    out[is.na(out) & x_norm == label] <- mapping[[label]]
  }

  out
}

## Scoring-Funktion für Skalentexte mit Mapping ##
# Diese Funktion fügt einem Data Frame eine neue Spalte hinzu, die die numerischen Scores
# für eine gegebene Skalentext-Variable enthält. Sie überprüft, ob die angegebene Variable
# im Data Frame vorhanden ist, und wenn ja, verwendet sie die recode_scale_with_map-Funktion,
# um die Skalentexte in numerische Werte umzuwandeln, basierend auf einem bereitgestellten Mapping.
# Die neue Score-Spalte wird direkt hinter der Originalvariable platziert.
# Ziel ist es, eine einfache Möglichkeit zu bieten, Skalentexte in numerische Scores umzuwandeln und diese im Data Frame zu integrieren.
add_score_var <- function(df, var_name, mapping) {
  if (!var_name %in% names(df)) return(df)
  score_name <- paste0(var_name, "_score")
  df %>%
    mutate(
      "{score_name}" := recode_scale_with_map(.data[[var_name]], mapping),
      .after = all_of(var_name)
    )
}

## Umkodierungsfunktion für alle relevanten Skalentext-Variablen im Pre-Survey ##
# Diese Funktion definiert spezifische Mapping-Listen für verschiedene Skalentypen, die im Pre-Survey verwendet werden,
# und wendet die add_score_var-Funktion auf alle relevanten Skalentext-Variablen an. Sie überprüft, ob jede Variable
# im Data Frame vorhanden ist, bevor sie die Umkodierung durchführt, und fügt die entsprechenden Score-Spalten
# direkt hinter den Originalvariablen hinzu.
# Ziel ist es, alle Skalentext-Variablen im Pre-Survey in numerische Scores umzuwandeln, um spätere Analysen zu erleichtern.
# Wichtig: Die Skalen sind entsprechend der Frageformulierungen und Antwortoptionen definiert, um eine genaue Umkodierung zu gewährleisten.
apply_pre_scale_variables <- function(df) {
  experience7_map <- c(
    "very inexperienced" = 1, "inexperienced" = 2, "slightly inexperienced" = 3,
    "neither inexperienced nor experienced" = 4, "slightly experienced" = 5,
    "experienced" = 6, "very experienced" = 7
  )

  priority7_map <- c(
    "not a priority" = 1, "low priority" = 2, "somewhat priority" = 3,
    "neutral" = 4, "moderate priority" = 5, "high priority" = 6,
    "essential priority" = 7
  )

  agree7_somewhat_map <- c(
    "strongly disagree" = 1, "disagree" = 2, "somewhat disagree" = 3,
    "neither agree nor disagree" = 4, "somewhat agree" = 5,
    "agree" = 6, "strongly agree" = 7
  )

  agree7_slightly_map <- c(
    "strongly disagree" = 1, "disagree" = 2, "slightly disagree" = 3,
    "neither agree nor disagree" = 4, "slightly agree" = 5,
    "agree" = 6, "strongly agree" = 7
  )

  difficulty7_map <- c(
    "extremely easy" = 1, "very easy" = 2, "somewhat easy" = 3,
    "neither easy nor difficult" = 4, "somewhat difficult" = 5,
    "very difficult" = 6, "extremely difficult" = 7
  )

  df <- add_score_var(df, "Pre_Survey_Q8", experience7_map)
  for (var in paste0("Pre_Survey_Q13_", 1:6)) df <- add_score_var(df, var, priority7_map)
  df <- add_score_var(df, "Pre_Survey_Q16", agree7_somewhat_map)
  df <- add_score_var(df, "Pre_Survey_Q18", difficulty7_map)
  for (var in paste0("Pre_Survey_Q21_", 1:5)) df <- add_score_var(df, var, agree7_slightly_map)
  df
}

# ## Umkodierungsfunktion für alle relevanten Skalentext-Variablen im Main-Survey ##
# Diese Funktion definiert spezifische Mapping-Listen für verschiedene Skalentypen, die im Main-Survey verwendet werden,
# und wendet die add_score_var-Funktion auf alle relevanten Skalentext-Variablen an. Sie überprüft, ob jede Variable
# im Data Frame vorhanden ist, bevor sie die Umkodierung durchführt, und fügt die entsprechenden Score-Spalten
# direkt hinter den Originalvariablen hinzu.
# Ziel ist es, alle Skalentext-Variablen im Main-Survey in numerische Scores umzuwandeln, um spätere Analysen zu erleichtern.
# Wichtig: Die Skalen sind entsprechend der Frageformulierungen und Antwortoptionen definiert, um eine genaue Umkodierung zu gewährleisten.
apply_main_scale_variables <- function(df) {
  vividness7_map <- c(
    "not vivid at all" = 1, "very weak" = 2, "weak" = 3,
    "moderately vivid" = 4, "quite vivid" = 5, "very vivid" = 6,
    "extremely vivid" = 7
  )

  agreement7_map <- c(
    "not at all" = 1, "very weakly" = 2, "weakly" = 3,
    "moderately" = 4, "strongly" = 5, "very strongly" = 6,
    "almost exactly" = 7
  )

  agree7_slightly_map <- c(
    "strongly disagree" = 1, "disagree" = 2, "slightly disagree" = 3,
    "neither agree nor disagree" = 4, "slightly agree" = 5,
    "agree" = 6, "strongly agree" = 7
  )

  agree7_somewhat_map <- c(
    "strongly disagree" = 1, "disagree" = 2, "somewhat disagree" = 3,
    "neither agree nor disagree" = 4, "somewhat agree" = 5,
    "agree" = 6, "strongly agree" = 7
  )

  similarity7_map <- c(
    "completely different" = 1, "very different" = 2, "somewhat different" = 3,
    "moderately similar" = 4, "quite similar" = 5, "very similar" = 6,
    "essentially the same" = 7
  )

  preference7_map <- c(
    "strongly prefer my own mental images" = 1,
    "prefer my own mental images" = 2,
    "slightly prefer my own mental images" = 3,
    "no preference" = 4,
    "slightly prefer ai-generated images" = 5,
    "prefer ai-generated images" = 6,
    "strongly prefer ai-generated images" = 7,
    "slightly prefer al-generated images" = 5,
    "prefer al-generated images" = 6,
    "strongly prefer al-generated images" = 7
  )

  df <- add_score_var(df, "Main_Survey_Q21", vividness7_map)
  for (var in c("Main_Survey_Q26","Main_Survey_Q34","Main_Survey_Q42","Main_Survey_Q52")) df <- add_score_var(df, var, agreement7_map)
  for (var in c(paste0("Main_Survey_Q27_", 1:3), paste0("Main_Survey_Q35_", 1:3), paste0("Main_Survey_Q43_", 1:3))) df <- add_score_var(df, var, agree7_slightly_map)
  for (var in c("Main_Survey_Q28","Main_Survey_Q36","Main_Survey_Q44")) df <- add_score_var(df, var, agree7_somewhat_map)
  df <- add_score_var(df, "Main_Survey_Q48", similarity7_map)
  df <- add_score_var(df, "Main_Survey_Q53", preference7_map)
  df
}

## Funktion zur Überprüfung der erforderlichen Spalten in einem Data Frame ##
# Diese Funktion überprüft, ob alle erforderlichen Spalten in einem gegebenen Data Frame vorhanden sind.
# Sie nimmt den Data Frame, eine Liste der erforderlichen Spalten und den Namen des Datensatzes als Eingabe.
# Wenn eine oder mehrere erforderliche Spalten fehlen, wird eine informative Fehlermeldung ausgegeben, die
# die fehlenden Spalten auflistet und den Benutzer auffordert, die CSV-Datei zu überprüfen oder die Konfiguration
# im Skript anzupassen.
# Ziel ist es, sicherzustellen, dass alle notwendigen Spalten für die weitere Verarbeitung vorhanden sind, bevor der Bereinigungsprozess fortgesetzt wird.
assert_required_columns <- function(df, required_cols, dataset_name) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "The following columns are missing in dataset '", dataset_name, "': ",
        paste(missing_cols, collapse = ", "),
        ". Please check the CSV file or adjust the configuration in the script."
      ),
      call. = FALSE
    )
  }
}

## Hauptfunktion für die Bereinigung der Survey-Daten ##
# Diese Funktion führt den gesamten Bereinigungsprozess für die Survey-Daten durch,
# einschließlich der Umwandlung des "Finished"-Status in logische Werte,
# der Identifikation und Entfernung von Fällen im "Survey Preview"-Status,
# der Entfernung unvollständiger Fälle,
# der Identifikation von doppelten IP-Adressen
# und der Entfernung von Fällen ohne Einwilligung.
# Sie dokumentiert die Anzahl der Fälle vor und nach jedem Bereinigungsschritt
# sowie die Anzahl der entfernten Fälle.
# Das Ergebnis ist eine Liste, die den bereinigten Datensatz, die ausgeschlossenen Fälle und Übersichten zu den Bereinigungsschritten enthält.
clean_survey_data <- function(df, cfg) {
  df <- df %>%
    mutate(finished_clean = parse_finished_to_logical(.data[[cfg$finished_var]]))

  excluded_preview <- df %>%
    filter(.data[[cfg$status_var]] == cfg$preview_value)

  data_after_dc0 <- df %>%
    filter(.data[[cfg$status_var]] != cfg$preview_value | is.na(.data[[cfg$status_var]]))

  n_dc0 <- make_n_table("DC0_Status_preview_removed", nrow(df), nrow(excluded_preview), nrow(data_after_dc0))

  excluded_unfinished <- data_after_dc0 %>%
    filter(finished_clean != TRUE | is.na(finished_clean))

  data_after_dc1 <- data_after_dc0 %>%
    filter(finished_clean == TRUE)

  n_dc1 <- make_n_table("DC1_Finished_false_removed", nrow(data_after_dc0), nrow(excluded_unfinished), nrow(data_after_dc1))

  duplicate_ip_overview <- data_after_dc1 %>%
    filter(!is.na(.data[[cfg$ip_var]])) %>%
    add_count(.data[[cfg$ip_var]], name = "ip_frequency") %>%
    filter(ip_frequency > 1) %>%
    arrange(desc(ip_frequency), .data[[cfg$ip_var]])

  duplicate_ip_summary <- duplicate_ip_overview %>%
    distinct(.data[[cfg$ip_var]], ip_frequency) %>%
    arrange(desc(ip_frequency), .data[[cfg$ip_var]])

  n_dc2 <- tibble(
    data_cleaning_step = "DC2_Duplicate_IPs_flagged",
    n_rows_with_duplicate_ip = nrow(duplicate_ip_overview),
    n_unique_duplicate_ips   = nrow(duplicate_ip_summary)
  )

  excluded_no_consent <- data_after_dc1 %>%
    filter(.data[[cfg$consent_var]] == cfg$consent_no)

  data_clean <- data_after_dc1 %>%
    filter(.data[[cfg$consent_var]] == cfg$consent_yes)

  n_dc3 <- make_n_table("DC3_Consent_no_removed", nrow(data_after_dc1), nrow(excluded_no_consent), nrow(data_clean))
  n_overview <- bind_rows(n_dc0, n_dc1, n_dc3)

  list(
    raw_data              = df,
    clean_data            = data_clean,
    excluded_preview      = excluded_preview,
    excluded_unfinished   = excluded_unfinished,
    excluded_no_consent   = excluded_no_consent,
    duplicate_ip_overview = duplicate_ip_overview,
    duplicate_ip_summary  = duplicate_ip_summary,
    n_overview            = n_overview,
    n_duplicate_ip        = n_dc2
  )
}

## Funktion zur Erstellung von GT-Tabellen ##
make_gt_table <- function(df, title_text, subtitle_text = NULL) {
  gt::gt(df) %>%
    gt::tab_header(
      title = title_text,
      subtitle = subtitle_text
    ) %>%
    gt::tab_options(
      table.font.size = 12,
      heading.title.font.size = 14,
      data_row.padding = gt::px(4)
    )
}

## Funktion zur Kategorisierung von Zielwörtern im Main-Survey ##
add_target_word_category <- function(x) {
  x_clean <- x %>%
    as.character() %>%
    stringr::str_squish()

  dplyr::case_when(
    x_clean %in% c(
      "Justice", "Euphoria", "Anarchy", "Anger", "Infinity",
      "Laziness", "Love", "Luck", "Nostalgia", "Wisdom"
    ) ~ "abstract",
    x_clean %in% c(
      "Glacier", "House", "Margarita", "Mirror", "Wig",
      "Lake", "Man", "Microscope", "Steering Wheel",
      "Night"
    ) ~ "concrete",
    TRUE ~ NA_character_
  )
}



#####################################################################
###                CSV-Dateien einlesen und prüfen                ###
#####################################################################

pre_raw           <- read_csv_auto(file_pre_data) %>% clean_character_columns()
pre_features_raw  <- read_csv_auto(file_pre_features) %>% clean_character_columns()
main_raw          <- read_csv_auto(file_main_data) %>% clean_character_columns()
main_features_raw <- read_csv_auto(file_main_features) %>% clean_character_columns()

pre_raw  <- prefix_dataset_columns(pre_raw, config_pre$column_prefix)
main_raw <- prefix_dataset_columns(main_raw, config_main$column_prefix)

pre_raw  <- apply_pre_scale_variables(pre_raw)
main_raw <- apply_main_scale_variables(main_raw)

# Überprüfung
assert_required_columns(
  pre_raw,
  required_cols = c(config_pre$id_var, config_pre$status_var, config_pre$finished_var, config_pre$ip_var, config_pre$consent_var, config_pre$followup_var, config_pre$email_var),
  dataset_name = "Pre_Survey_All.csv"
)

assert_required_columns(
  main_raw,
  required_cols = c(config_main$id_var, config_main$status_var, config_main$finished_var, config_main$ip_var, config_main$consent_var, config_main$email_var),
  dataset_name = "Main_Survey_ALL.csv"
)

pre_feature_lookup  <- make_feature_lookup(pre_features_raw,  "Pre_Survey", config_pre$column_prefix)
main_feature_lookup <- make_feature_lookup(main_features_raw, "Main_Survey", config_main$column_prefix)
feature_lookup_all <- bind_rows(pre_feature_lookup, main_feature_lookup)

variable_overview_pre <- tibble(variable_name = names(pre_raw)) %>%
  left_join(pre_feature_lookup, by = "variable_name")

variable_overview_main <- tibble(variable_name = names(main_raw)) %>%
  left_join(main_feature_lookup, by = "variable_name")

#####################################################################
###                 Data Cleaning der Survey-Daten                ###
#####################################################################

pre_cleaning <- clean_survey_data(pre_raw, config_pre)
pre_clean                 <- pre_cleaning$clean_data
pre_excluded_preview      <- pre_cleaning$excluded_preview
pre_excluded_unfinished   <- pre_cleaning$excluded_unfinished
pre_excluded_no_consent   <- pre_cleaning$excluded_no_consent
pre_duplicate_ip_overview <- pre_cleaning$duplicate_ip_overview
pre_duplicate_ip_summary  <- pre_cleaning$duplicate_ip_summary
pre_n_overview            <- pre_cleaning$n_overview
pre_n_duplicate_ip        <- pre_cleaning$n_duplicate_ip

main_cleaning <- clean_survey_data(main_raw, config_main)
main_clean                 <- main_cleaning$clean_data
main_excluded_preview      <- main_cleaning$excluded_preview
main_excluded_unfinished   <- main_cleaning$excluded_unfinished
main_excluded_no_consent   <- main_cleaning$excluded_no_consent
main_duplicate_ip_overview <- main_cleaning$duplicate_ip_overview
main_duplicate_ip_summary  <- main_cleaning$duplicate_ip_summary
main_n_overview            <- main_cleaning$n_overview
main_n_duplicate_ip        <- main_cleaning$n_duplicate_ip

#####################################################################
###                  E-Mail-Bereinigung und Prüfung               ###
#####################################################################

pre_clean <- pre_clean %>%
  mutate(
    Pre_Survey_email_raw          = .data[[config_pre$email_var]],
    Pre_Survey_email_clean        = clean_email(.data[[config_pre$email_var]]),
    Pre_Survey_email_valid_format = is_valid_email(Pre_Survey_email_clean),
    .after = config_pre$email_var
  )

main_clean <- main_clean %>%
  mutate(
    Main_Survey_email_raw          = .data[[config_main$email_var]],
    Main_Survey_email_clean        = clean_email(.data[[config_main$email_var]]),
    Main_Survey_email_valid_format = is_valid_email(Main_Survey_email_clean),
    .after = config_main$email_var
  ) %>%
  mutate(
    Main_Survey_target_word_category = add_target_word_category(Main_Survey_target_word),
    .after = Main_Survey_target_word
  )


pre_invalid_email_review <- pre_clean %>%
  filter(!is.na(Pre_Survey_email_clean), !Pre_Survey_email_valid_format)

main_invalid_email_review <- main_clean %>%
  filter(!is.na(Main_Survey_email_clean), !Main_Survey_email_valid_format)

pre_duplicate_email_review <- pre_clean %>%
  filter(!is.na(Pre_Survey_email_clean)) %>%
  add_count(Pre_Survey_email_clean, name = "email_frequency") %>%
  filter(email_frequency > 1) %>%
  arrange(desc(email_frequency), Pre_Survey_email_clean)

main_duplicate_email_review <- main_clean %>%
  filter(!is.na(Main_Survey_email_clean)) %>%
  add_count(Main_Survey_email_clean, name = "email_frequency") %>%
  filter(email_frequency > 1) %>%
  arrange(desc(email_frequency), Main_Survey_email_clean)

#####################################################################
###          Identifikation der relevanten Follow-up-Fälle       ###
#####################################################################

pre_clean_followup <- pre_clean %>%
  mutate(
    followup_opt_in = stringr::str_detect(
      stringr::str_squish(replace_na(as.character(.data[[config_pre$followup_var]]), "")),
      stringr::fixed(config_pre$followup_yes_text)
    ),
    has_email_in_q32 = !is.na(Pre_Survey_email_clean)
  )

pre_cases_with_email_in_q32 <- pre_clean_followup %>%
  filter(has_email_in_q32)

pre_followup_candidates <- pre_clean_followup %>%
  filter(followup_opt_in, has_email_in_q32)

pre_followup_no_q30_confirmation <- pre_clean_followup %>%
  filter(!followup_opt_in)

pre_followup_no_email_in_q32 <- pre_clean_followup %>%
  filter(followup_opt_in, !has_email_in_q32)

pre_followup_invalid_email_format <- pre_followup_candidates %>%
  filter(!Pre_Survey_email_valid_format)

pre_followup_summary <- tibble(
  category = c(
    "Total cleaned Pre-Survey cases",
    "Cases with email address in Q32 (regardless of follow-up)",
    "Follow-up candidates (Q30 yes + email available)",
    "No follow-up opt-in in Q30",
    "Follow-up opt-in, but no email in Q32",
    "Follow-up candidates with formally invalid email"
  ),
  n = c(
    nrow(pre_clean_followup),
    nrow(pre_cases_with_email_in_q32),
    nrow(pre_followup_candidates),
    nrow(pre_followup_no_q30_confirmation),
    nrow(pre_followup_no_email_in_q32),
    nrow(pre_followup_invalid_email_format)
  )
)

#####################################################################
###               E-Mail-Abgleich zwischen Pre und Main          ###
#####################################################################

pre_select_vars <- c(
  config_pre$id_var,
  "Pre_Survey_StartDate",
  "Pre_Survey_EndDate",
  config_pre$status_var,
  config_pre$ip_var,
  config_pre$followup_var,
  config_pre$email_var,
  "Pre_Survey_email_clean",
  "Pre_Survey_email_valid_format"
)

main_select_vars <- c(
  config_main$id_var,
  "Main_Survey_StartDate",
  "Main_Survey_EndDate",
  config_main$status_var,
  config_main$ip_var,
  config_main$email_var,
  "Main_Survey_email_clean",
  "Main_Survey_email_valid_format"
)

main_target_vars <- c(
  "Main_Survey_target_word",
  "Main_Survey_target_word_category"
)

main_select_vars <- c(
  main_select_vars,
  intersect(main_target_vars, names(main_clean))
)

pre_match_base <- pre_followup_candidates %>%
  select(all_of(pre_select_vars)) %>%
  filter(!is.na(Pre_Survey_email_clean)) %>%
  distinct(Pre_Survey_email_clean, .keep_all = TRUE)

main_match_base <- main_clean %>%
  select(all_of(main_select_vars)) %>%
  filter(!is.na(Main_Survey_email_clean)) %>%
  distinct(Main_Survey_email_clean, .keep_all = TRUE)

pre_match_valid <- pre_match_base %>%
  filter(Pre_Survey_email_valid_format)

main_match_valid <- main_match_base %>%
  filter(Main_Survey_email_valid_format)

# Vollständige bereinigte Datensätze
pre_clean_full  <- pre_clean_followup
main_clean_full <- main_clean

pre_match_full <- pre_clean_full %>%
  filter(!is.na(Pre_Survey_email_clean)) %>%
  distinct(Pre_Survey_email_clean, .keep_all = TRUE)

main_match_full <- main_clean_full %>%
  filter(!is.na(Main_Survey_email_clean)) %>%
  distinct(Main_Survey_email_clean, .keep_all = TRUE)

matched_pre_main <- pre_match_full %>%
  inner_join(
    main_match_full,
    by = join_by(Pre_Survey_email_clean == Main_Survey_email_clean),
    keep = TRUE
  ) %>%
  mutate(
    matched_email = coalesce(Pre_Survey_email_clean, Main_Survey_email_clean)
  ) %>%
  relocate(matched_email, .before = 1)

matched_pre_main_valid <- pre_match_valid %>%
  inner_join(
    main_match_valid,
    by = join_by(Pre_Survey_email_clean == Main_Survey_email_clean),
    keep = TRUE
  ) %>%
  mutate(
    matched_email = coalesce(Pre_Survey_email_clean, Main_Survey_email_clean)
  ) %>%
  relocate(matched_email, .before = 1)

final_analysis_dataset_full <- matched_pre_main

match_summary <- tibble(
  category = c(
    "Unique email cases in the Pre-Survey",
    "Unique email cases in the Main Survey",
    "Uniquely matched cases in both datasets"
  ),
  n = c(
    nrow(pre_match_base),
    nrow(main_match_base),
    nrow(matched_pre_main)
  )
)

match_summary_valid_only <- tibble(
  category = c(
    "Unique formally valid email cases in the Pre-Survey",
    "Unique formally valid email cases in the Main Survey",
    "Uniquely matched cases in both datasets (only formally valid emails)"
  ),
  n = c(
    nrow(pre_match_valid),
    nrow(main_match_valid),
    nrow(matched_pre_main_valid)
  )
)

#####################################################################
###                Dokumentation und Export der Ergebnisse        ###
#####################################################################

cleaning_summary_all <- bind_rows(
  pre_n_overview %>% mutate(dataset = "Pre_Survey"),
  main_n_overview %>% mutate(dataset = "Main_Survey")
) %>%
  select(dataset, everything())

ip_duplicate_summary_all <- bind_rows(
  pre_n_duplicate_ip %>% mutate(dataset = "Pre_Survey"),
  main_n_duplicate_ip %>% mutate(dataset = "Main_Survey")
) %>%
  select(dataset, everything())

email_quality_summary <- tibble(
  dataset = c("Pre_Survey", "Main_Survey"),
  n_clean_cases = c(nrow(pre_clean), nrow(main_clean)),
  n_nonmissing_email = c(sum(!is.na(pre_clean$Pre_Survey_email_clean)), sum(!is.na(main_clean$Main_Survey_email_clean))),
  n_invalid_email_format = c(nrow(pre_invalid_email_review), nrow(main_invalid_email_review)),
  n_duplicate_emails = c(
    dplyr::n_distinct(pre_duplicate_email_review$Pre_Survey_email_clean),
    dplyr::n_distinct(main_duplicate_email_review$Main_Survey_email_clean)
  )
)

writexl::write_xlsx(
  list(
    pre_clean_full = pre_clean_full,
    main_clean_full = main_clean_full
  ),
  path = file.path(out_clean_dir, "01_clean_datasets.xlsx")
)

writexl::write_xlsx(
  list(
    pre_excluded_preview = pre_excluded_preview,
    pre_excluded_unfinished = pre_excluded_unfinished,
    pre_excluded_no_consent = pre_excluded_no_consent,
    main_excluded_preview = main_excluded_preview,
    main_excluded_unfinished = main_excluded_unfinished,
    main_excluded_no_consent = main_excluded_no_consent
  ),
  path = file.path(out_excluded_dir, "02_excluded_cases.xlsx")
)

writexl::write_xlsx(
  list(
    pre_duplicate_ip_overview = pre_duplicate_ip_overview,
    pre_duplicate_ip_summary = pre_duplicate_ip_summary,
    main_duplicate_ip_overview = main_duplicate_ip_overview,
    main_duplicate_ip_summary = main_duplicate_ip_summary,
    pre_invalid_email_review = pre_invalid_email_review,
    main_invalid_email_review = main_invalid_email_review,
    pre_duplicate_email_review = pre_duplicate_email_review,
    main_duplicate_email_review = main_duplicate_email_review
  ),
  path = file.path(out_review_dir, "03_review_files.xlsx")
)

writexl::write_xlsx(
  list(
    pre_clean_followup = pre_clean_followup,
    pre_followup_candidates = pre_followup_candidates,
    pre_followup_no_q30_confirmation = pre_followup_no_q30_confirmation,
    pre_followup_no_email_in_q32 = pre_followup_no_email_in_q32,
    pre_followup_invalid_email_format = pre_followup_invalid_email_format,
    pre_followup_summary = pre_followup_summary
  ),
  path = file.path(out_clean_dir, "04_pre_followup_selection.xlsx")
)

writexl::write_xlsx(
  list(
    pre_match_base = pre_match_base,
    main_match_base = main_match_base,
    matched_pre_main = matched_pre_main,
    matched_pre_main_valid = matched_pre_main_valid,
    match_summary = match_summary,
    match_summary_valid_only = match_summary_valid_only,
    final_analysis_dataset_full = final_analysis_dataset_full
  ),
  path = file.path(out_matching_dir, "05_email_matching_results.xlsx")
)

writexl::write_xlsx(
  list(
    feature_lookup_all = feature_lookup_all,
    variable_overview_pre = variable_overview_pre,
    variable_overview_main = variable_overview_main,
    cleaning_summary_all = cleaning_summary_all,
    ip_duplicate_summary_all = ip_duplicate_summary_all,
    email_quality_summary = email_quality_summary,
    pre_followup_summary = pre_followup_summary,
    match_summary = match_summary,
    match_summary_valid_only = match_summary_valid_only
  ),
  path = file.path(out_document_dir, "06_documentation_tables.xlsx")
)

cat("\n==================== CLEANING SUMMARY ====================\n")
print(cleaning_summary_all)

cat("\n==================== DUPLICATE IP SUMMARY ====================\n")
print(ip_duplicate_summary_all)

cat("\n==================== PRE FOLLOW-UP SUMMARY ====================\n")
print(pre_followup_summary)

cat("\n==================== EMAIL MATCH SUMMARY ====================\n")
print(match_summary)

cat("\n==================== EMAIL MATCH SUMMARY (VALID ONLY) ====================\n")
print(match_summary_valid_only)

#####################################################################
###          Wissenschaftliche Tabellen im Viewer anzeigen       ###
#####################################################################

### BESCHREIBUNG ###

# In diesem Abschnitt werden die zentralen Ergebnis- und Dokumentationstabellen
# zusätzlich als wissenschaftlich formatierte Tabellen mit dem Paket gt im
# Viewer dargestellt. Die inhaltliche Methodik und die zuvor erzeugten Objekte
# bleiben dabei unverändert.

# =========================================================
# 4) Wissenschaftliche Tabellen anzeigen                ====
# =========================================================

gt_cleaning_summary_all <- make_gt_table(
  cleaning_summary_all,
  title_text = "Overview of cleaning steps",
  subtitle_text = "Pre-Survey and Main-Survey"
)

gt_ip_duplicate_summary_all <- make_gt_table(
  ip_duplicate_summary_all,
  title_text = "Overview of flagged duplicate IP addresses",
  subtitle_text = "Pre-Survey and Main-Survey"
)

gt_pre_followup_summary <- make_gt_table(
  pre_followup_summary,
  title_text = "Overview of follow-up selection in the Pre-Survey",
  subtitle_text = NULL
)

gt_match_summary <- make_gt_table(
  match_summary,
  title_text = "Overview of email matching",
  subtitle_text = NULL
)

gt_match_summary_valid_only <- make_gt_table(
  match_summary_valid_only,
  title_text = "Overview of email matching with formally valid emails",
  subtitle_text = NULL
)

gt_email_quality_summary <- make_gt_table(
  email_quality_summary,
  title_text = "Overview of email quality",
  subtitle_text = "Cleaned datasets"
)

gt_output_list <- list(
  gt_cleaning_summary_all = gt_cleaning_summary_all,
  gt_ip_duplicate_summary_all = gt_ip_duplicate_summary_all,
  gt_pre_followup_summary = gt_pre_followup_summary,
  gt_match_summary = gt_match_summary,
  gt_match_summary_valid_only = gt_match_summary_valid_only,
  gt_email_quality_summary = gt_email_quality_summary
)

preview_removal_summary <- bind_rows(
  pre_n_overview %>%
    filter(data_cleaning_step == "DC0_Status_preview_removed") %>%
    mutate(dataset = "Pre_Survey"),
  main_n_overview %>%
    filter(data_cleaning_step == "DC0_Status_preview_removed") %>%
    mutate(dataset = "Main_Survey")
) %>%
  select(dataset, everything())

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
  base_filename = "01_cleaning_gt_manifest",
  out_dir = out_gt_doc_dir
)

build_simple_html_index(
  manifest = gt_manifest,
  output_path = file.path(out_gt_dir, "00_gt_index.html"),
  title_text = "Cleaning workflow - gt outputs",
  intro_text = "This index links to all formatted gt tables created from the data-cleaning workflow."
)

gt_console_summary <- c(
  "==================== CLEANING GT TABLES ====================",
  capture.output(print(gt_manifest)),
  "",
  paste0("HTML directory: ", out_gt_html_dir),
  paste0("RTF directory: ", out_gt_rtf_dir),
  paste0("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
)

writeLines(
  gt_console_summary,
  con = file.path(out_gt_doc_dir, "01_cleaning_gt_console_summary.txt")
)

message("Confirmation: GT tables for the cleaning workflow were exported successfully.")
message("HTML tables: ", out_gt_html_dir)
message("RTF tables (where supported): ", out_gt_rtf_dir)
message("Index file: ", file.path(out_gt_dir, "00_gt_index.html"))
message("Manifest: ", file.path(out_gt_doc_dir, "01_cleaning_gt_manifest.csv"))

# gt_cleaning_summary_all
# gt_ip_duplicate_summary_all
# gt_pre_followup_summary
# gt_match_summary
# gt_match_summary_valid_only
# gt_email_quality_summary

#####################################################################
###                    Ende des Workflows                         ###
#####################################################################

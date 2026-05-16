# 99_master_konsolidierter_export_index.R

#####################################################################
### Projektweiter Master-Export- und Index-Block                  ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript erzeugt einen zentralen Master-Index über die Export- und
# Dokumentationsordner aller konsolidierten Projektskripte.
#
# Zusätzlich erzeugt es pro Skriptabschnitt einen bereinigten Sammelordner
# "Output for Research". Dieser enthält ausschließlich publikationsnahe
# Arbeitsergebnisse:
# - HTML-Tabellen
# - RTF-Tabellen
# - DOCX-Tabellen
# - PNG-Plots
#
# Nicht in "Output for Research" übernommen werden:
# - CSV-Dateien
# - XLSX-Dateien
# - TXT-Dateien
# - lokale Index-/Dokumentationsdateien
# - Manifest-/Hilfsdateien
#
# Ziel:
# - ein einziger Einstiegspunkt: 00_master_export_index.html
# - pro Skriptabschnitt ein Link "Output for Research"
# - dort nur jene Tabellen und Plots, die direkt für die Arbeit nutzbar sind
# - keine Änderung an Analyse-, Tabellen- oder Plot-Methodik
#
# WICHTIG:
# - Dieses Skript verändert keine Analyse- oder Bereinigungslogik.
# - Es sammelt ausschließlich bereits erzeugte Outputs.
# - Falls einzelne Skripte noch nicht gelaufen sind, werden die Abschnitte
#   trotzdem erzeugt und als "noch nicht vorhanden" markiert.

# =========================================================
# 0) Pakete                                              ===
# =========================================================

library(tidyverse)
library(readr)
library(writexl)
library(here)

# =========================================================
# 1) Projektpfade und Helper                             ===
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

# Kleine Fallbacks, falls einzelne Helper-Funktionen im Projekt nicht geladen wurden.
if (!exists("ensure_directories", mode = "function")) {
  ensure_directories <- function(paths) {
    purrr::walk(paths[!is.na(paths) & nzchar(paths)], ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))
    invisible(paths)
  }
}

if (!exists("html_escape_simple", mode = "function")) {
  html_escape_simple <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }
}

if (!exists("save_table_outputs", mode = "function")) {
  save_table_outputs <- function(data, base_filename, out_dir) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(data, file.path(out_dir, paste0(base_filename, ".csv")))
    writexl::write_xlsx(data, file.path(out_dir, paste0(base_filename, ".xlsx")))
    invisible(data)
  }
}

out_master_dir <- file.path(project_root, "data_output", "project_master_index")
out_doc_dir    <- file.path(out_master_dir, "documentation")

# Zentraler Sammelordner. Darin erhält jeder Skriptabschnitt einen eigenen
# Unterordner mit ausschließlich HTML-/RTF-/DOCX-Tabellen und PNG-Plots.
out_research_root_dir <- file.path(out_master_dir, "Output for Research")

ensure_directories(c(out_master_dir, out_doc_dir, out_research_root_dir))

# =========================================================
# 2) Hilfsfunktionen                                      ===
# =========================================================

normalize_path_safe <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

path_exists_safe <- function(path) {
  !is.na(path) && nzchar(path) && file.exists(path)
}

safe_slug <- function(x) {
  x <- as.character(x)
  x_ascii <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x_ascii[is.na(x_ascii)] <- x[is.na(x_ascii)]
  slug <- stringr::str_to_lower(x_ascii)
  slug <- stringr::str_replace_all(slug, "[^a-z0-9]+", "_")
  slug <- stringr::str_replace_all(slug, "^_+|_+$", "")
  ifelse(is.na(slug) | !nzchar(slug), "section", slug)
}

find_local_index_file <- function(local_index_file, base_output_dir = NA_character_) {
  candidates <- c(local_index_file)

  if (!is.na(base_output_dir) && nzchar(base_output_dir) && dir.exists(base_output_dir)) {
    candidates <- c(
      candidates,
      file.path(base_output_dir, "00_export_index.html"),
      file.path(base_output_dir, "documentation", "00_export_index.html"),
      file.path(base_output_dir, "captions", "00_export_index.html"),
      file.path(base_output_dir, "00_gt_index.html"),
      file.path(base_output_dir, "gt_tables", "00_gt_index.html"),
      file.path(base_output_dir, "gt_tables", "html", "00_gt_index.html"),
      file.path(base_output_dir, "documentation", "gt_tables", "00_gt_index.html"),
      file.path(base_output_dir, "html", "00_gt_index.html"),
      list.files(
        base_output_dir,
        pattern = "(^00_.*index|_index)\\.html$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE,
        all.files = FALSE,
        no.. = TRUE
      )
    )
  }

  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
  existing <- candidates[file.exists(candidates)]

  if (length(existing) > 0) existing[[1]] else NA_character_
}

make_relative_path_scalar <- function(target_path, from_dir) {
  target_norm <- normalize_path_safe(target_path)
  from_norm   <- normalize_path_safe(from_dir)

  target_parts <- strsplit(target_norm, "/", fixed = TRUE)[[1]]
  from_parts   <- strsplit(from_norm, "/", fixed = TRUE)[[1]]

  max_common <- min(length(target_parts), length(from_parts))
  common_idx <- 0L

  for (i in seq_len(max_common)) {
    if (identical(target_parts[[i]], from_parts[[i]])) {
      common_idx <- i
    } else {
      break
    }
  }

  up_parts   <- rep("..", max(length(from_parts) - common_idx, 0))
  down_parts <- target_parts[seq.int(common_idx + 1L, length(target_parts))]

  rel_parts <- c(up_parts, down_parts)

  if (length(rel_parts) == 0) "." else paste(rel_parts, collapse = "/")
}

make_relative_path <- function(target_path, from_dir) {
  # Vector-safe wrapper: needed inside dplyr::mutate(), where several
  # files are processed at once. Scalar calls still return a length-1 value.
  if (length(target_path) == 0) {
    return(character())
  }

  max_len <- max(length(target_path), length(from_dir))
  target_path <- rep(target_path, length.out = max_len)
  from_dir <- rep(from_dir, length.out = max_len)

  purrr::map2_chr(target_path, from_dir, make_relative_path_scalar)
}

make_link_html_scalar <- function(path, label, from_dir) {
  if (!path_exists_safe(path)) {
    return('<span class="missing">nicht vorhanden</span>')
  }

  rel <- make_relative_path_scalar(path, from_dir)
  paste0('<a href="', html_escape_simple(rel), '">', html_escape_simple(label), '</a>')
}

make_link_html <- function(path, label, from_dir) {
  # Vector-safe wrapper: avoids errors such as
  # "'length = x' in coercion to 'logical(1)'" when used in mutate().
  if (length(path) == 0) {
    return(character())
  }

  max_len <- max(length(path), length(label), length(from_dir))
  path <- rep(path, length.out = max_len)
  label <- rep(label, length.out = max_len)
  from_dir <- rep(from_dir, length.out = max_len)

  purrr::pmap_chr(
    list(path = path, label = label, from_dir = from_dir),
    ~ make_link_html_scalar(..1, ..2, ..3)
  )
}

list_files_safe <- function(path, recursive = TRUE) {
  if (!path_exists_safe(path) || !dir.exists(path)) {
    return(character())
  }

  files <- list.files(path, recursive = recursive, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  files <- files[!dir.exists(files)]
  normalize_path_safe(files)
}

count_by_extension <- function(files, pattern) {
  sum(stringr::str_detect(tolower(files), pattern))
}

summarise_export_space <- function(base_dir) {
  files <- list_files_safe(base_dir, recursive = TRUE)

  tibble(
    base_dir = base_dir,
    n_files_total = length(files),
    n_html = count_by_extension(files, "\\.html$"),
    n_csv  = count_by_extension(files, "\\.csv$"),
    n_xlsx = count_by_extension(files, "\\.xlsx$"),
    n_png  = count_by_extension(files, "\\.png$"),
    n_rtf  = count_by_extension(files, "\\.rtf$"),
    n_docx = count_by_extension(files, "\\.docx$"),
    n_txt  = count_by_extension(files, "\\.txt$")
  )
}

collapse_existing_links <- function(paths, labels, from_dir) {
  pieces <- purrr::map2_chr(paths, labels, ~ make_link_html(.x, .y, from_dir))
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
  paste(pieces, collapse = " | ")
}

# =========================================================
# 2a) Output-for-Research-Funktionen                     ===
# =========================================================

get_research_section_dir <- function(script_id, section_title) {
  file.path(
    out_research_root_dir,
    paste0(stringr::str_pad(script_id, width = 2, pad = "0"), "_", safe_slug(section_title))
  )
}

is_research_candidate <- function(files) {
  files_norm <- stringr::str_replace_all(normalize_path_safe(files), "\\\\", "/")
  basenames  <- basename(files_norm)
  ext        <- stringr::str_to_lower(tools::file_ext(files_norm))

  is_index_or_manifest <- stringr::str_detect(
    basenames,
    regex("(^00_.*index|_index|master_export_manifest|export_manifest|console_summary)\\.(html|csv|xlsx|txt|docx)$", ignore_case = TRUE)
  )

  # Documentation/caption folders usually contain manifests, console summaries,
  # and helper files that should not be copied into "Output for Research".
  # Exception: some scripts, especially 01, store the actual gt table exports
  # under data_output/documentation/gt_tables/html, .../rtf and .../docx. These are
  # genuine research tables and must therefore be kept.
  is_documentation_file <- stringr::str_detect(
    files_norm,
    regex("/(documentation|captions)/", ignore_case = TRUE)
  )

  is_gt_table_export <- stringr::str_detect(
    files_norm,
    regex("/gt_tables/(html|rtf|docx)/", ignore_case = TRUE)
  )

  # HTML/RTF tables and PNG plots can come from the registered table/figure
  # folders. DOCX tables are only copied from gt_tables/docx so that the Word
  # outputs have the same title, subtitle, column labels and remarks as the GT
  # publication tables. Generic CSV/XLSX-derived DOCX files are intentionally
  # not copied into "Output for Research".
  allowed_ext <- ext %in% c("html", "rtf", "png") | (ext == "docx" & is_gt_table_export)

  allowed_ext & !is_index_or_manifest & (!is_documentation_file | is_gt_table_export)
}

classify_research_file <- function(file) {
  ext <- stringr::str_to_lower(tools::file_ext(file))

  dplyr::case_when(
    ext == "html" ~ "Tables_HTML",
    ext == "rtf"  ~ "Tables_RTF",
    ext == "docx" ~ "Tables_DOCX",
    ext == "png"  ~ "Plots",
    TRUE ~ "Other"
  )
}

collect_research_files <- function(section_row) {
  source_dirs <- c(
    section_row$tables_dir,
    section_row$gt_dir,
    section_row$figures_dir,
    section_row$base_output_dir
  )

  source_dirs <- unique(source_dirs[!is.na(source_dirs) & nzchar(source_dirs) & dir.exists(source_dirs)])

  if (length(source_dirs) == 0) {
    return(tibble())
  }

  files <- purrr::map(source_dirs, list_files_safe, recursive = TRUE) %>%
    unlist(use.names = FALSE) %>%
    unique()

  if (length(files) == 0) {
    return(tibble())
  }

  files <- files[is_research_candidate(files)]

  if (length(files) == 0) {
    return(tibble())
  }

  tibble(source_path = files) %>%
    mutate(
      extension = stringr::str_to_lower(tools::file_ext(source_path)),
      category = classify_research_file(source_path)
    ) %>%
    filter(category %in% c("Tables_HTML", "Tables_RTF", "Tables_DOCX", "Plots")) %>%
    arrange(category, basename(source_path))
}

build_research_index_html <- function(section_row, research_tbl, research_dir, research_index_file) {
  if (nrow(research_tbl) == 0) {
    file_list_html <- '<p><span class="missing">Keine HTML-/RTF-Tabellen oder PNG-Plots gefunden.</span></p>'
  } else {
    file_list_html <- research_tbl %>%
      mutate(
        link = make_link_html(copied_path, basename(copied_path), research_dir),
        source_rel = make_relative_path(source_path, project_root)
      ) %>%
      group_by(category) %>%
      summarise(
        html = paste0(
          '<h2>', html_escape_simple(first(category)), '</h2>',
          '<ul>',
          paste0(
            '<li>', link,
            ' <span class="source">Quelle: ', html_escape_simple(source_rel), '</span></li>',
            collapse = ''
          ),
          '</ul>'
        ),
        .groups = "drop"
      ) %>%
      pull(html) %>%
      paste(collapse = "\n")
  }

  html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "  <meta charset=\"utf-8\">",
    paste0("  <title>Output for Research – ", html_escape_simple(section_row$script_id), "</title>"),
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 24px; line-height: 1.5; }",
    "    h1 { margin-bottom: 8px; }",
    "    h2 { margin-top: 24px; border-bottom: 1px solid #ddd; padding-bottom: 4px; }",
    "    ul { margin-top: 8px; }",
    "    li { margin-bottom: 6px; }",
    "    a { text-decoration: none; }",
    "    a:hover { text-decoration: underline; }",
    "    .source { color: #666; font-size: 0.9em; margin-left: 8px; }",
    "    .missing { color: #a94442; font-style: italic; }",
    "    .summary-box { background: #f7f7f7; border: 1px solid #e2e2e2; padding: 12px; margin: 16px 0; }",
    "  </style>",
    "</head>",
    "<body>",
    paste0("  <h1>Output for Research – ", html_escape_simple(section_row$script_id), " – ", html_escape_simple(section_row$section_title), "</h1>"),
    paste0("  <p>", html_escape_simple(section_row$description), "</p>"),
    paste0(
      "  <div class=\"summary-box\">",
      "<strong>Enthalten:</strong> nur HTML-Tabellen, RTF-Tabellen, DOCX-Tabellen und PNG-Plots.",
      "<br><strong>Nicht enthalten:</strong> CSV, XLSX, TXT, lokale Index- und Dokumentationsdateien.",
      "<br><strong>Dateien:</strong> gesamt=", nrow(research_tbl),
      " | html=", sum(research_tbl$extension == "html"),
      " | rtf=", sum(research_tbl$extension == "rtf"),
      " | docx=", sum(research_tbl$extension == "docx"),
      " | png=", sum(research_tbl$extension == "png"),
      "</div>"
    ),
    file_list_html,
    "</body>",
    "</html>"
  )

  writeLines(html, con = research_index_file)
  invisible(research_index_file)
}

create_research_output_for_section <- function(section_row) {
  research_dir <- get_research_section_dir(section_row$script_id, section_row$section_title)

  # Ordner jedes Mal frisch erzeugen, damit keine alten CSV/XLSX/HTML-Dateien
  # aus früheren Läufen liegen bleiben.
  if (dir.exists(research_dir)) {
    unlink(research_dir, recursive = TRUE, force = TRUE)
  }

  ensure_directories(c(
    research_dir,
    file.path(research_dir, "Tables_HTML"),
    file.path(research_dir, "Tables_RTF"),
    file.path(research_dir, "Tables_DOCX"),
    file.path(research_dir, "Plots")
  ))

  research_tbl <- collect_research_files(section_row)

  if (nrow(research_tbl) > 0) {
    research_tbl <- research_tbl %>%
      group_by(category) %>%
      mutate(
        copied_filename = paste0(stringr::str_pad(row_number(), width = 3, pad = "0"), "_", basename(source_path)),
        copied_path = file.path(research_dir, category, copied_filename)
      ) %>%
      ungroup()

    purrr::walk2(
      research_tbl$source_path,
      research_tbl$copied_path,
      ~ file.copy(.x, .y, overwrite = TRUE, copy.date = TRUE)
    )
  } else {
    research_tbl <- tibble(
      source_path = character(),
      extension = character(),
      category = character(),
      copied_filename = character(),
      copied_path = character()
    )
  }

  research_index_file <- file.path(research_dir, "00_output_for_research_index.html")
  build_research_index_html(section_row, research_tbl, research_dir, research_index_file)

  tibble(
    script_id = section_row$script_id,
    research_output_dir = research_dir,
    research_index_file = research_index_file,
    n_research_files_total = nrow(research_tbl),
    n_research_html = sum(research_tbl$extension == "html"),
    n_research_rtf = sum(research_tbl$extension == "rtf"),
    n_research_docx = sum(research_tbl$extension == "docx"),
    n_research_png = sum(research_tbl$extension == "png")
  )
}

build_section_table <- function(section_row, from_dir) {
  files_summary <- summarise_export_space(section_row$base_output_dir)
  resolved_local_index_file <- find_local_index_file(section_row$local_index_file, section_row$base_output_dir)

  tibble(
    `Script` = section_row$script_id,
    `Bereich` = section_row$section_title,
    `Status Output-Ordner` = ifelse(path_exists_safe(section_row$base_output_dir), "vorhanden", "noch nicht vorhanden"),
    `Lokaler Index` = make_link_html(resolved_local_index_file, "lokalen Index öffnen", from_dir),
    `Output for Research` = make_link_html(section_row$research_index_file, "Output for Research öffnen", from_dir),
    `Basisordner` = make_link_html(section_row$base_output_dir, basename(section_row$base_output_dir), from_dir),
    `Unterordner / Schlüsseldateien` = collapse_existing_links(
      c(
        section_row$tables_dir,
        section_row$figures_dir,
        section_row$gt_dir,
        section_row$documentation_dir,
        section_row$combined_workbook,
        section_row$console_summary
      ),
      c("tables", "figures", "gt_tables", "documentation", "workbook", "console summary"),
      from_dir
    ),
    `Dateien gesamt` = files_summary$n_files_total,
    `HTML` = files_summary$n_html,
    `CSV` = files_summary$n_csv,
    `XLSX` = files_summary$n_xlsx,
    `PNG` = files_summary$n_png,
    `RTF` = files_summary$n_rtf,
    `DOCX` = files_summary$n_docx,
    `TXT` = files_summary$n_txt,
    `Research Dateien gesamt` = section_row$n_research_files_total,
    `Research HTML` = section_row$n_research_html,
    `Research RTF` = section_row$n_research_rtf,
    `Research DOCX` = section_row$n_research_docx,
    `Research PNG` = section_row$n_research_png
  )
}

# =========================================================
# 3) Registry aller Skript-Outputs                       ===
# =========================================================

script_registry <- tibble::tribble(
  ~script_id, ~section_title, ~description, ~base_output_dir, ~local_index_file, ~tables_dir, ~figures_dir, ~gt_dir, ~documentation_dir, ~combined_workbook, ~console_summary,

  "01", "Data cleaning & matching", "Bereinigung, Exklusion, Review, Matching und GT-Dokumentation des Cleaning-Workflows.",
  file.path(project_root, "data_output", "documentation"),
  file.path(project_root, "data_output", "documentation", "gt_tables", "00_gt_index.html"),
  file.path(project_root, "data_output", "clean"),
  NA_character_,
  file.path(project_root, "data_output", "documentation", "gt_tables"),
  file.path(project_root, "data_output", "documentation"),
  NA_character_,
  NA_character_,

  "02", "VIVIQ scoring", "VIVIQ-Scoring im gematchten Main-Survey inklusive GT-Tabellen.",
  file.path(project_root, "data_output", "viviq"),
  file.path(project_root, "data_output", "viviq", "gt_tables", "00_gt_index.html"),
  file.path(project_root, "data_output", "viviq"),
  NA_character_,
  file.path(project_root, "data_output", "viviq", "gt_tables"),
  file.path(project_root, "data_output", "viviq", "gt_tables", "documentation"),
  NA_character_,
  NA_character_,

  "03", "Prompt coding", "Prompt-Coding aus 03_coding_promts.qc.R: Join des Prompt-Codings, Plausibilitätschecks und Sequenzübersichten.",
  file.path(project_root, "data_output", "prompt_coding"),
  file.path(project_root, "data_output", "prompt_coding", "documentation", "00_prompt_coding_index.html"),
  file.path(project_root, "data_output", "prompt_coding"),
  NA_character_,
  NA_character_,
  file.path(project_root, "data_output", "prompt_coding", "documentation"),
  NA_character_,
  NA_character_,

  "05", "Variablenübersicht", "Variablenübersicht aus 04_variablenuebersicht.R: Variableninventar mit Normierung, Typen und GT-Index.",
  file.path(project_root, "data_output", "variable_inventory"),
  file.path(project_root, "data_output", "variable_inventory", "gt_tables", "00_gt_index.html"),
  file.path(project_root, "data_output", "variable_inventory", "tables"),
  NA_character_,
  file.path(project_root, "data_output", "variable_inventory", "gt_tables"),
  file.path(project_root, "data_output", "variable_inventory", "documentation"),
  NA_character_,
  NA_character_,

  "06", "Deskriptive Statistik & Reporting", "Konsolidierte Deskriptiven, erweiterte Tabellen, Grafiken und GT-Index.",
  file.path(project_root, "data_output", "descriptives"),
  file.path(project_root, "data_output", "descriptives", "00_export_index.html"),
  file.path(project_root, "data_output", "descriptives", "tables"),
  file.path(project_root, "data_output", "descriptives", "figures"),
  file.path(project_root, "data_output", "descriptives", "gt_tables"),
  file.path(project_root, "data_output", "descriptives", "captions"),
  NA_character_,
  NA_character_,

  "07", "Pre-survey descriptives", "Angeforderte Pre-Survey-Deskriptiven auf Basis des finalen anonymisierten Datensatzes.",
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested"),
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "documentation", "00_export_index.html"),
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "tables"),
  NA_character_,
  NA_character_,
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "documentation"),
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "08_pre_survey_premerge_requested_descriptives.xlsx"),
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "documentation", "08_pre_survey_premerge_requested_descriptives_console_summary.txt"),

  "08", "Pre-survey GT tables", "Publikationsnahe GT-Tabellen für die angeforderten Pre-Survey-Deskriptiven.",
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "gt_tables"),
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "gt_tables", "00_gt_index.html"),
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "tables"),
  NA_character_,
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "gt_tables"),
  file.path(project_root, "data_output", "descriptives", "pre_survey_premerge_requested", "gt_tables", "documentation"),
  NA_character_,
  NA_character_,

  "09", "Main study GT tables", "Publikationsnahe GT-Tabellen für Main-Study-Resultate.",
  file.path(project_root, "data_output", "main_study_results", "gt_tables"),
  file.path(project_root, "data_output", "main_study_results", "gt_tables", "00_gt_index.html"),
  file.path(project_root, "data_output", "main_study_results"),
  NA_character_,
  file.path(project_root, "data_output", "main_study_results", "gt_tables"),
  file.path(project_root, "data_output", "main_study_results", "gt_tables", "documentation"),
  NA_character_,
  NA_character_,

  "10", "VIVIQ level effect plots", "Zusatzplots und Tabellen nach VIVIQ-Level.",
  file.path(project_root, "data_output", "main_study_viviq_level_effects"),
  file.path(project_root, "data_output", "main_study_viviq_level_effects", "documentation", "00_export_index.html"),
  file.path(project_root, "data_output", "main_study_viviq_level_effects", "tables"),
  file.path(project_root, "data_output", "main_study_viviq_level_effects", "figures"),
  NA_character_,
  file.path(project_root, "data_output", "main_study_viviq_level_effects", "documentation"),
  file.path(project_root, "data_output", "main_study_viviq_level_effects", "11_viviq_level_effect_tables.xlsx"),
  file.path(project_root, "data_output", "main_study_viviq_level_effects", "documentation", "11_viviq_level_effect_console_summary.txt"),

  "11", "Q6 duration effect plots", "Zusatzplots und Tabellen nach Nutzungsdauer aus Pre_Survey_Q6.",
  file.path(project_root, "data_output", "main_study_q6_duration_effects"),
  file.path(project_root, "data_output", "main_study_q6_duration_effects", "documentation", "00_export_index.html"),
  file.path(project_root, "data_output", "main_study_q6_duration_effects", "tables"),
  file.path(project_root, "data_output", "main_study_q6_duration_effects", "figures"),
  NA_character_,
  file.path(project_root, "data_output", "main_study_q6_duration_effects", "documentation"),
  file.path(project_root, "data_output", "main_study_q6_duration_effects", "12_q6_duration_effect_tables.xlsx"),
  file.path(project_root, "data_output", "main_study_q6_duration_effects", "documentation", "12_q6_duration_effect_console_summary.txt"),

  "12", "Target-word-category effect plots", "Zusatzplots und Tabellen nach Target-Word-Kategorie.",
  file.path(project_root, "data_output", "main_study_target_word_category_effects"),
  file.path(project_root, "data_output", "main_study_target_word_category_effects", "documentation", "00_export_index.html"),
  file.path(project_root, "data_output", "main_study_target_word_category_effects", "tables"),
  file.path(project_root, "data_output", "main_study_target_word_category_effects", "figures"),
  NA_character_,
  file.path(project_root, "data_output", "main_study_target_word_category_effects", "documentation"),
  file.path(project_root, "data_output", "main_study_target_word_category_effects", "13_target_word_category_effect_tables.xlsx"),
  file.path(project_root, "data_output", "main_study_target_word_category_effects", "documentation", "13_target_word_category_effect_console_summary.txt"),

  "13", "Image agreement LMM analysis", "Abschließende Image-Agreement-Analyse mit Round-only und kontrolliertem LMM.",
  file.path(project_root, "data_output", "image_agreement_lmm_analysis"),
  file.path(project_root, "data_output", "image_agreement_lmm_analysis", "documentation", "00_export_index.html"),
  file.path(project_root, "data_output", "image_agreement_lmm_analysis", "tables"),
  file.path(project_root, "data_output", "image_agreement_lmm_analysis", "figures"),
  NA_character_,
  file.path(project_root, "data_output", "image_agreement_lmm_analysis", "documentation"),
  file.path(project_root, "data_output", "image_agreement_lmm_analysis", "12_image_agreement_lmm_analysis_tables.xlsx"),
  file.path(project_root, "data_output", "image_agreement_lmm_analysis", "documentation", "12_image_agreement_lmm_console_summary.txt")
)

# =========================================================
# 4) Output-for-Research-Ordner erzeugen                 ===
# =========================================================

research_manifest <- purrr::pmap_dfr(
  script_registry,
  function(script_id,
           section_title,
           description,
           base_output_dir,
           local_index_file,
           tables_dir,
           figures_dir,
           gt_dir,
           documentation_dir,
           combined_workbook,
           console_summary) {

    section_row <- tibble(
      script_id = script_id,
      section_title = section_title,
      description = description,
      base_output_dir = base_output_dir,
      local_index_file = local_index_file,
      tables_dir = tables_dir,
      figures_dir = figures_dir,
      gt_dir = gt_dir,
      documentation_dir = documentation_dir,
      combined_workbook = combined_workbook,
      console_summary = console_summary
    )

    create_research_output_for_section(section_row)
  }
)

script_registry_augmented <- script_registry %>%
  left_join(research_manifest, by = "script_id")

# =========================================================
# 5) Master-Manifest                                     ===
# =========================================================

master_manifest <- purrr::pmap_dfr(
  script_registry_augmented,
  function(script_id,
           section_title,
           description,
           base_output_dir,
           local_index_file,
           tables_dir,
           figures_dir,
           gt_dir,
           documentation_dir,
           combined_workbook,
           console_summary,
           research_output_dir,
           research_index_file,
           n_research_files_total,
           n_research_html,
           n_research_rtf,
           n_research_docx,
           n_research_png) {

    section_row <- tibble(
      script_id = script_id,
      section_title = section_title,
      description = description,
      base_output_dir = base_output_dir,
      local_index_file = local_index_file,
      tables_dir = tables_dir,
      figures_dir = figures_dir,
      gt_dir = gt_dir,
      documentation_dir = documentation_dir,
      combined_workbook = combined_workbook,
      console_summary = console_summary,
      research_output_dir = research_output_dir,
      research_index_file = research_index_file,
      n_research_files_total = n_research_files_total,
      n_research_html = n_research_html,
      n_research_rtf = n_research_rtf,
      n_research_docx = n_research_docx,
      n_research_png = n_research_png
    )

    build_section_table(section_row, from_dir = out_master_dir) %>%
      mutate(description = description)
  }
)

save_table_outputs(
  master_manifest,
  base_filename = "99_master_export_manifest",
  out_dir = out_master_dir
)

# Zusätzlich ein Research-spezifisches Manifest ohne CSV/XLSX-Quellen erzeugen.
readr::write_csv(
  research_manifest,
  file.path(out_research_root_dir, "00_output_for_research_manifest.csv")
)

# =========================================================
# 6) HTML-Master-Index                                   ===
# =========================================================

section_html <- purrr::pmap_chr(
  script_registry_augmented,
  function(script_id,
           section_title,
           description,
           base_output_dir,
           local_index_file,
           tables_dir,
           figures_dir,
           gt_dir,
           documentation_dir,
           combined_workbook,
           console_summary,
           research_output_dir,
           research_index_file,
           n_research_files_total,
           n_research_html,
           n_research_rtf,
           n_research_docx,
           n_research_png) {

    summary_tbl <- summarise_export_space(base_output_dir)
    resolved_local_index_file <- find_local_index_file(local_index_file, base_output_dir)

    links <- c(
      if (path_exists_safe(resolved_local_index_file)) paste0("<li>", make_link_html(resolved_local_index_file, "Lokalen Unterindex öffnen", out_master_dir), "</li>") else "",
      if (path_exists_safe(research_index_file)) paste0("<li><strong>", make_link_html(research_index_file, "Output for Research", out_master_dir), "</strong> <span class=\"hint\">nur HTML-/RTF-/DOCX-Tabellen und PNG-Plots</span></li>") else "",
      if (path_exists_safe(base_output_dir)) paste0("<li>", make_link_html(base_output_dir, "Basisordner öffnen", out_master_dir), "</li>") else "",
      if (path_exists_safe(tables_dir)) paste0("<li>", make_link_html(tables_dir, "Tabellenordner", out_master_dir), "</li>") else "",
      if (path_exists_safe(figures_dir)) paste0("<li>", make_link_html(figures_dir, "Figurenordner", out_master_dir), "</li>") else "",
      if (path_exists_safe(gt_dir)) paste0("<li>", make_link_html(gt_dir, "GT-Ordner", out_master_dir), "</li>") else "",
      if (path_exists_safe(documentation_dir)) paste0("<li>", make_link_html(documentation_dir, "Dokumentationsordner", out_master_dir), "</li>") else "",
      if (path_exists_safe(combined_workbook)) paste0("<li>", make_link_html(combined_workbook, basename(combined_workbook), out_master_dir), "</li>") else "",
      if (path_exists_safe(console_summary)) paste0("<li>", make_link_html(console_summary, basename(console_summary), out_master_dir), "</li>") else ""
    )

    links <- links[nzchar(links)]

    if (length(links) == 0) {
      links <- '<li><span class="missing">Noch keine Exporte gefunden.</span></li>'
    }

    paste0(
      '<section class="script-section">',
      '<h2>', html_escape_simple(paste0(script_id, " – ", section_title)), '</h2>',
      '<p>', html_escape_simple(description), '</p>',
      '<p><strong>Status:</strong> ', ifelse(path_exists_safe(base_output_dir), 'Output-Ordner vorhanden', '<span class="missing">Output-Ordner noch nicht vorhanden</span>'), '</p>',
      '<p><strong>Dateiüberblick:</strong> ',
      'gesamt=', summary_tbl$n_files_total,
      ' | html=', summary_tbl$n_html,
      ' | csv=', summary_tbl$n_csv,
      ' | xlsx=', summary_tbl$n_xlsx,
      ' | png=', summary_tbl$n_png,
      ' | rtf=', summary_tbl$n_rtf,
      ' | docx=', summary_tbl$n_docx,
      ' | txt=', summary_tbl$n_txt,
      '</p>',
      '<p><strong>Output for Research:</strong> ',
      'gesamt=', n_research_files_total,
      ' | html=', n_research_html,
      ' | rtf=', n_research_rtf,
      ' | docx=', n_research_docx,
      ' | png=', n_research_png,
      ' <span class="hint">CSV/XLSX werden hier bewusst nicht übernommen.</span>',
      '</p>',
      '<ul>', paste(links, collapse = ''), '</ul>',
      '</section>'
    )
  }
)

master_index_html <- c(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  "  <meta charset=\"utf-8\">",
  "  <title>Master Export Index</title>",
  "  <style>",
  "    body { font-family: Arial, sans-serif; margin: 24px; line-height: 1.5; }",
  "    h1 { margin-bottom: 8px; }",
  "    h2 { margin-top: 32px; margin-bottom: 8px; border-bottom: 1px solid #ddd; padding-bottom: 6px; }",
  "    p { margin: 8px 0; }",
  "    ul { margin-top: 8px; margin-bottom: 16px; }",
  "    li { margin-bottom: 6px; }",
  "    a { text-decoration: none; }",
  "    a:hover { text-decoration: underline; }",
  "    .missing { color: #a94442; font-style: italic; }",
  "    .hint { color: #666; font-size: 0.92em; }",
  "    .summary-box { background: #f7f7f7; border: 1px solid #e2e2e2; padding: 12px; margin-top: 16px; }",
  "    .script-section { margin-bottom: 18px; }",
  "  </style>",
  "</head>",
  "<body>",
  "  <h1>Projektweiter Master-Export-Index</h1>",
  "  <p>Dieser Index bündelt die Export- und Dokumentationszugänge aller aktuell registrierten konsolidierten Projektskripte. Er dient als letzter zentraler Einstiegspunkt über die gesamte Output-Struktur.</p>",
  paste0(
    "  <div class=\"summary-box\"><strong>Anzahl Skriptabschnitte:</strong> ",
    nrow(script_registry_augmented),
    "<br><strong>Master-Manifest:</strong> ",
    make_link_html(file.path(out_master_dir, "99_master_export_manifest.xlsx"), "99_master_export_manifest.xlsx", out_master_dir),
    " | ",
    make_link_html(file.path(out_master_dir, "99_master_export_manifest.csv"), "99_master_export_manifest.csv", out_master_dir),
    "<br><strong>Output for Research:</strong> ",
    make_link_html(out_research_root_dir, "zentralen Sammelordner öffnen", out_master_dir),
    "<br><span class=\"hint\">Die Output-for-Research-Ordner enthalten nur HTML-/RTF-/DOCX-Tabellen und PNG-Plots; keine CSV- oder XLSX-Dateien.</span>",
    "</div>"
  ),
  section_html,
  "</body>",
  "</html>"
)

master_index_path <- file.path(out_master_dir, "00_master_export_index.html")
writeLines(master_index_html, con = master_index_path)

# =========================================================
# 7) Konsolenhinweise                                    ===
# =========================================================

message("Master index created: ", master_index_path)
message("Master manifest CSV: ", file.path(out_master_dir, "99_master_export_manifest.csv"))
message("Master manifest XLSX: ", file.path(out_master_dir, "99_master_export_manifest.xlsx"))
message("Output for Research root: ", out_research_root_dir)
message("Research manifest CSV: ", file.path(out_research_root_dir, "00_output_for_research_manifest.csv"))

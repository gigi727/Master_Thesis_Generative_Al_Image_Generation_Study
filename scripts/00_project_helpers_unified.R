#####################################################################
### Zentrale Projekt-Hilfsfunktionen                              ###
#####################################################################

### BESCHREIBUNG ###

# Dieses Skript bündelt methodisch neutrale Hilfsfunktionen, die in
# mehreren Projekt-Skripten wiederholt vorkommen. Es verändert keine
# inhaltliche Auswertungslogik, sondern vereinheitlicht:
# - CSV-Import für hochgeladene Dateien in latin1 / cp1252
# - CSV-/Excel-Exporte
# - Plot-Themes
# - einfache GT-Basisstile
# - GT-Datei-Exporte
# - DOCX-Tabellenexports für Word-kompatible Tabellen
# - einfache HTML-Indizes für Exportordner
#
# WICHTIG:
# - Die Analysemethodik der bestehenden Skripte bleibt unberührt.
# - Dieses Skript ist als gemeinsame Infrastruktur gedacht.
# - Bereits bestehende, skriptspezifische Ableitungslogiken (z. B.
#   derive_title(), derive_subtitle(), derive_source_note(), spezielle
#   Row-Grouping-Logik) sollen in den jeweiligen Skripten verbleiben.

# =========================================================
# 0) Pakete                                              ===
# =========================================================

library(tidyverse)
library(readr)
library(writexl)
library(gt)
library(flextable)
library(officer)

# =========================================================
# 1) Zentrale Defaults                                   ===
# =========================================================

project_input_encodings <- c("windows-1252", "latin1", "UTF-8")
project_missing_tokens  <- c("", "NA", "N/A", "na", "n/a")
project_csv_export_mode <- "excel_utf8_semicolon"

# =========================================================
# 2) Pfad- und Ordnerhilfen                              ===
# =========================================================

ensure_directories <- function(paths) {
  purrr::walk(paths, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))
}

resolve_first_existing_path <- function(candidates, label = "required file") {
  existing_candidates <- candidates[file.exists(candidates)]

  if (length(existing_candidates) == 0) {
    stop(
      paste0(
        "The ", label, " could not be found. Expected one of these locations:\n",
        paste(candidates, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  existing_candidates[[1]]
}

# =========================================================
# 3) Allgemeine Text- und Zahlenhilfen                   ===
# =========================================================

normalize_missing_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% project_missing_tokens] <- NA_character_
  x
}

safe_numeric <- function(x) {
  readr::parse_number(as.character(x), na = project_missing_tokens)
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

pct <- function(x, base, digits = 1) {
  ifelse(is.na(base) | base == 0, NA_real_, round(100 * x / base, digits))
}

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

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

# =========================================================
# 4) CSV-Import für latin1 / cp1252                      ===
# =========================================================

count_fixed <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (identical(matches, -1L)) 0L else length(matches)
}

detect_csv_delimiter <- function(path, encoding = project_input_encodings[[1]]) {
  first_line <- readLines(path, n = 1, warn = FALSE, encoding = encoding)
  n_semicolon <- count_fixed(first_line, ";")
  n_comma     <- count_fixed(first_line, ",")
  ifelse(n_semicolon > n_comma, ";", ",")
}

read_csv_auto <- function(path,
                          encodings = project_input_encodings,
                          na_tokens = project_missing_tokens,
                          trim_ws = TRUE,
                          col_types = readr::cols(.default = readr::col_character())) {

  delim <- detect_csv_delimiter(path, encoding = encodings[[1]])
  last_error <- NULL

  for (enc in encodings) {
    attempt <- tryCatch(
      {
        readr::read_delim(
          file = path,
          delim = delim,
          col_types = col_types,
          na = na_tokens,
          locale = readr::locale(encoding = enc),
          show_col_types = FALSE,
          name_repair = "minimal",
          trim_ws = trim_ws
        )
      },
      error = function(e) e
    )

    if (!inherits(attempt, "error")) {
      attr(attempt, "source_encoding") <- enc
      attr(attempt, "source_delimiter") <- delim
      return(attempt)
    }

    last_error <- attempt
  }

  stop(
    paste0(
      "The CSV file could not be read with the configured encodings (",
      paste(encodings, collapse = ", "),
      "). Last error: ",
      last_error$message
    ),
    call. = FALSE
  )
}

# =========================================================
# 5) Einheitliche CSV-/Excel-Exporte                     ===
# =========================================================

write_csv_project <- function(df, path, mode = project_csv_export_mode, na = "") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (identical(mode, "excel_utf8_semicolon")) {
    readr::write_excel_csv2(df, file = path, na = na)
  } else if (identical(mode, "excel_utf8_comma")) {
    readr::write_excel_csv(df, file = path, na = na)
  } else if (identical(mode, "utf8_comma")) {
    readr::write_csv(df, file = path, na = na)
  } else if (identical(mode, "utf8_semicolon")) {
    readr::write_csv2(df, file = path, na = na)
  } else {
    stop(
      paste0(
        "Unknown CSV export mode: ", mode,
        ". Supported modes are: excel_utf8_semicolon, excel_utf8_comma, utf8_comma, utf8_semicolon."
      ),
      call. = FALSE
    )
  }
}

# =========================================================
# 5a) Einheitliche DOCX-Tabellenexports                    ===
# =========================================================

# Diese Funktionen ergänzen ausschließlich die Exportebene.
# Sie verändern keine Analyse-, Bereinigungs- oder Tabellenmethodik.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

docx_text_clean <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  if (inherits(x, c("html", "shiny.tag", "shiny.tag.list"))) {
    x <- as.character(x)
  }

  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }

  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    return(NA_character_)
  }

  x <- paste(x, collapse = " ")
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", '"', x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  stringr::str_squish(x)
}

coerce_docx_table_data <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  df[] <- lapply(df, function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      return(as.character(x))
    }

    if (is.factor(x)) {
      return(as.character(x))
    }

    if (is.list(x)) {
      return(vapply(x, function(z) paste(as.character(z), collapse = "; "), character(1)))
    }

    x
  })

  df
}

extract_gt_heading_value <- function(gt_tbl, field = c("title", "subtitle")) {
  field <- match.arg(field)

  attr_name <- if (field == "title") "docx_title_text" else "docx_subtitle_text"
  value <- attr(gt_tbl, attr_name, exact = TRUE)
  value <- docx_text_clean(value)

  if (!is.na(value) && nzchar(value)) {
    return(value)
  }

  if ("_heading" %in% names(gt_tbl)) {
    heading <- gt_tbl[["_heading"]]
    if (is.list(heading) && field %in% names(heading)) {
      value <- docx_text_clean(heading[[field]])
      if (!is.na(value) && nzchar(value)) {
        return(value)
      }
    }
  }

  NA_character_
}

extract_gt_source_note_value <- function(gt_tbl) {
  value <- attr(gt_tbl, "docx_source_note", exact = TRUE)
  value <- docx_text_clean(value)

  if (!is.na(value) && nzchar(value)) {
    return(value)
  }

  if ("_source_notes" %in% names(gt_tbl)) {
    notes <- gt_tbl[["_source_notes"]]
    value <- docx_text_clean(notes)
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }

  NA_character_
}

extract_gt_docx_source_data <- function(gt_tbl) {
  source_data <- attr(gt_tbl, "docx_source_data", exact = TRUE)

  if (!is.null(source_data)) {
    return(source_data)
  }

  if ("_data" %in% names(gt_tbl) && is.data.frame(gt_tbl[["_data"]])) {
    return(gt_tbl[["_data"]])
  }

  NULL
}

extract_gt_column_labels <- function(gt_tbl, source_data = NULL) {
  if (is.null(source_data)) {
    source_data <- extract_gt_docx_source_data(gt_tbl)
  }

  if (is.null(source_data)) {
    return(NULL)
  }

  column_names <- names(source_data)
  labels <- column_names

  if ("_boxhead" %in% names(gt_tbl) && is.data.frame(gt_tbl[["_boxhead"]])) {
    boxhead <- gt_tbl[["_boxhead"]]

    if (all(c("var", "column_label") %in% names(boxhead))) {
      matched <- match(column_names, as.character(boxhead$var))
      gt_labels <- vapply(matched, function(i) {
        if (is.na(i)) {
          return(NA_character_)
        }
        docx_text_clean(boxhead$column_label[[i]])
      }, character(1))

      labels <- ifelse(!is.na(gt_labels) & nzchar(gt_labels), gt_labels, labels)
    }
  }

  stats::setNames(as.list(labels), column_names)
}

make_flextable_academic <- function(df,
                                    title_text = NULL,
                                    subtitle_text = NULL,
                                    source_note = NULL,
                                    column_labels = NULL,
                                    fontname = "Arial") {
  df <- coerce_docx_table_data(df)

  border_main <- officer::fp_border(color = "#666666", width = 1.25)

  ft <- flextable::flextable(df)

  if (!is.null(column_labels)) {
    column_labels <- column_labels[names(column_labels) %in% names(df)]
    if (length(column_labels) > 0) {
      ft <- ft %>% flextable::set_header_labels(values = column_labels)
    }
  }

  ft <- ft %>%
    flextable::border_remove() %>%
    flextable::hline_top(part = "header", border = border_main) %>%
    flextable::hline_bottom(part = "header", border = border_main) %>%
    flextable::hline_bottom(part = "body", border = border_main) %>%
    flextable::bold(part = "header") %>%
    flextable::font(fontname = fontname, part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::valign(valign = "top", part = "all") %>%
    flextable::padding(padding.top = 3, padding.bottom = 3, padding.left = 4, padding.right = 4, part = "all") %>%
    flextable::set_table_properties(layout = "autofit", width = 1) %>%
    flextable::autofit()

  title_text <- docx_text_clean(title_text)
  subtitle_text <- docx_text_clean(subtitle_text)
  source_note <- docx_text_clean(source_note)

  header_lines <- c(title_text, subtitle_text)
  header_lines <- header_lines[!is.na(header_lines) & nzchar(header_lines)]

  if (length(header_lines) > 0) {
    for (header_line in rev(header_lines)) {
      ft <- ft %>% flextable::add_header_lines(values = header_line)
    }
    ft <- ft %>%
      flextable::bold(i = 1, part = "header") %>%
      flextable::fontsize(i = 1, size = 10, part = "header")
  }

  if (!is.na(source_note) && nzchar(source_note)) {
    ft <- ft %>%
      flextable::add_footer_lines(values = source_note) %>%
      flextable::italic(part = "footer") %>%
      flextable::fontsize(size = 8, part = "footer")
  }

  ft
}

save_docx_table <- function(df,
                            path,
                            title_text = NULL,
                            subtitle_text = NULL,
                            source_note = NULL,
                            column_labels = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  ft <- make_flextable_academic(
    df = df,
    title_text = title_text,
    subtitle_text = subtitle_text,
    source_note = source_note,
    column_labels = column_labels
  )

  flextable::save_as_docx(ft, path = path)
  invisible(path)
}

save_gt_docx_table <- function(gt_tbl,
                               path,
                               file_stem = NULL) {
  source_data <- extract_gt_docx_source_data(gt_tbl)

  if (is.null(source_data)) {
    stop("No source data are available for this gt object.", call. = FALSE)
  }

  title_text <- extract_gt_heading_value(gt_tbl, "title")
  if ((is.na(title_text) || !nzchar(title_text)) && !is.null(file_stem)) {
    title_text <- file_stem
  }

  subtitle_text <- extract_gt_heading_value(gt_tbl, "subtitle")
  source_note <- extract_gt_source_note_value(gt_tbl)
  column_labels <- extract_gt_column_labels(gt_tbl, source_data)

  save_docx_table(
    df = source_data,
    path = path,
    title_text = title_text,
    subtitle_text = subtitle_text,
    source_note = source_note,
    column_labels = column_labels
  )
}

attach_gt_docx_source <- function(gt_tbl,
                                  data,
                                  title_text = NULL,
                                  subtitle_text = NULL,
                                  source_note = NULL) {
  attr(gt_tbl, "docx_source_data") <- data
  attr(gt_tbl, "docx_title_text") <- docx_text_clean(title_text)
  attr(gt_tbl, "docx_subtitle_text") <- docx_text_clean(subtitle_text)
  attr(gt_tbl, "docx_source_note") <- docx_text_clean(source_note)
  gt_tbl
}

save_table_outputs <- function(df,
                               base_filename,
                               out_dir,
                               csv_mode = project_csv_export_mode,
                               docx = FALSE,
                               title_text = NULL,
                               subtitle_text = NULL,
                               source_note = NULL,
                               column_labels = NULL) {
  csv_path  <- file.path(out_dir, paste0(base_filename, ".csv"))
  xlsx_path <- file.path(out_dir, paste0(base_filename, ".xlsx"))
  docx_path <- file.path(out_dir, paste0(base_filename, ".docx"))
  saved_docx <- NA_character_

  write_csv_project(df, csv_path, mode = csv_mode)
  writexl::write_xlsx(df, path = xlsx_path)

  if (isTRUE(docx)) {
    tryCatch(
      {
        save_docx_table(
          df,
          path = docx_path,
          title_text = title_text %||% base_filename,
          subtitle_text = subtitle_text,
          source_note = source_note,
          column_labels = column_labels
        )
        saved_docx <- docx_path
      },
      error = function(e) {
        message(
          "Note: DOCX export failed for '", base_filename,
          "'. CSV/XLSX exports still succeeded. Details: ", e$message
        )
      }
    )
  }

  invisible(
    list(
      csv_file = csv_path,
      xlsx_file = xlsx_path,
      docx_file = saved_docx
    )
  )
}

# =========================================================
# 6) Einheitlicher Plot-Stil                             ===
# =========================================================

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

# =========================================================
# 7) Einheitlicher GT-Basisstil                          ===
# =========================================================

make_gt_table_standard <- function(df,
                                   title_text,
                                   subtitle_text = NULL,
                                   source_note = NULL) {
  gt_tbl <- gt::gt(df) %>%
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

  attach_gt_docx_source(gt_tbl, df, title_text, subtitle_text, source_note)
}

save_gt_table <- function(gt_tbl,
                          file_stem,
                          out_gt_html_dir,
                          out_gt_rtf_dir = NULL,
                          out_gt_docx_dir = NULL) {
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

    tryCatch(
      {
        save_gt_docx_table(gt_tbl, path = docx_path, file_stem = file_stem)
        saved_docx <- docx_path
      },
      error = function(e) {
        message(
          "Note: DOCX export failed for '", file_stem,
          "'. HTML/RTF exports still succeeded where supported. Details: ", e$message
        )
      }
    )
  }

  tibble::tibble(
    object_name = file_stem,
    html_file = html_path,
    rtf_file = saved_rtf,
    docx_file = saved_docx
  )
}

# =========================================================
# 8) Einheitlicher HTML-Index                            ===
# =========================================================

html_escape_simple <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

build_simple_html_index <- function(manifest,
                                    output_path,
                                    title_text,
                                    intro_text,
                                    column_order = c("object_name", "variable_name", "question_focus", "analysis_type", "html_file", "rtf_file", "docx_file"),
                                    display_labels = c(
                                      object_name = "Object",
                                      variable_name = "Variable",
                                      question_focus = "Question focus",
                                      analysis_type = "Analysis type",
                                      html_file = "HTML",
                                      rtf_file = "RTF",
                                      docx_file = "DOCX"
                                    )) {
  manifest <- manifest %>%
    dplyr::select(dplyr::any_of(column_order))

  make_file_cell <- function(html_file = NA_character_, rtf_file = NA_character_, docx_file = NA_character_) {
    html_part <- if (!is.na(html_file) && nzchar(html_file)) {
      paste0('<a href="html/', basename(html_file), '">HTML</a>')
    } else {
      ""
    }

    rtf_part <- if (!is.na(rtf_file) && nzchar(rtf_file)) {
      paste0('<a href="rtf/', basename(rtf_file), '">RTF</a>')
    } else {
      ""
    }

    docx_part <- if (!is.na(docx_file) && nzchar(docx_file)) {
      paste0('<a href="docx/', basename(docx_file), '">DOCX</a>')
    } else {
      ""
    }

    parts <- c(html_part, rtf_part, docx_part)
    parts <- parts[nzchar(parts)]
    paste(parts, collapse = " | ")
  }

  header_cols <- names(manifest)

  if (all(c("html_file", "rtf_file", "docx_file") %in% names(manifest))) {
    file_cell <- purrr::pmap_chr(
      list(manifest$html_file, manifest$rtf_file, manifest$docx_file),
      ~ make_file_cell(..1, ..2, ..3)
    )
    manifest <- manifest %>%
      dplyr::mutate(Files = file_cell) %>%
      dplyr::select(-html_file, -rtf_file, -docx_file)
    header_cols <- names(manifest)
  } else if (all(c("html_file", "rtf_file") %in% names(manifest))) {
    file_cell <- purrr::map2_chr(manifest$html_file, manifest$rtf_file, ~ make_file_cell(.x, .y, NA_character_))
    manifest <- manifest %>%
      dplyr::mutate(Files = file_cell) %>%
      dplyr::select(-html_file, -rtf_file)
    header_cols <- names(manifest)
  }

  index_rows <- apply(manifest, 1, function(row_values) {
    cells <- purrr::imap_chr(
      as.list(row_values),
      function(value, nm) {
        if (identical(nm, "Files")) {
          paste0("<td>", value, "</td>")
        } else {
          paste0(
            "<td>",
            html_escape_simple(ifelse(is.na(value), "", as.character(value))),
            "</td>"
          )
        }
      }
    )

    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  })

  header_html <- paste0(
    "<tr>",
    paste0(
      "<th>",
      html_escape_simple(ifelse(is.na(display_labels[header_cols]), header_cols, display_labels[header_cols])),
      "</th>",
      collapse = ""
    ),
    "</tr>"
  )

  index_html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "  <meta charset=\"utf-8\">",
    paste0("  <title>", html_escape_simple(title_text), "</title>"),
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
    paste0("  <h1>", html_escape_simple(title_text), "</h1>"),
    paste0("  <p>", html_escape_simple(intro_text), "</p>"),
    paste0("  <p><strong>Total entries:</strong> ", nrow(manifest), "</p>"),
    "  <table>",
    "    <thead>",
    paste0("      ", header_html),
    "    </thead>",
    "    <tbody>",
    index_rows,
    "    </tbody>",
    "  </table>",
    "</body>",
    "</html>"
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(index_html, con = output_path)

  invisible(output_path)
}


# =========================================================
# 9) Allgemeiner Export-Index                            ===
# =========================================================

make_relative_path_safe <- function(path, from_dir) {
  if (is.na(path) || !nzchar(path)) return(NA_character_)
  if (!file.exists(path)) return(NA_character_)

  split_path <- function(x) {
    strsplit(
      normalizePath(x, winslash = "/", mustWork = TRUE),
      "/",
      fixed = TRUE
    )[[1]]
  }

  from_parts <- split_path(from_dir)
  to_parts   <- split_path(path)

  common_len <- 0L
  max_common <- min(length(from_parts), length(to_parts))

  while (common_len < max_common &&
         identical(from_parts[common_len + 1], to_parts[common_len + 1])) {
    common_len <- common_len + 1L
  }

  up_parts <- if (common_len < length(from_parts)) {
    rep("..", length(from_parts) - common_len)
  } else {
    character(0)
  }

  down_parts <- if (common_len < length(to_parts)) {
    to_parts[(common_len + 1):length(to_parts)]
  } else {
    character(0)
  }

  rel_parts <- c(up_parts, down_parts)

  if (length(rel_parts) == 0) "." else paste(rel_parts, collapse = "/")
}

build_general_export_index <- function(manifest,
                                       output_path,
                                       title_text,
                                       intro_text,
                                       path_col = "path",
                                       label_col = "label",
                                       notes_col = "notes") {
  manifest <- manifest %>%
    dplyr::mutate(
      link_rel = purrr::map_chr(.data[[path_col]], make_relative_path_safe, from_dir = dirname(output_path)),
      exists = !is.na(link_rel)
    )

  make_link_cell <- function(rel_path, label, exists) {
    if (!isTRUE(exists) || is.na(rel_path) || !nzchar(rel_path)) {
      return("noch nicht vorhanden")
    }
    paste0('<a href="', html_escape_simple(rel_path), '">', html_escape_simple(label), '</a>')
  }

  header_html <- paste0(
    "<tr>",
    "<th>Datei</th>",
    "<th>Pfad</th>",
    "<th>Hinweis</th>",
    "</tr>"
  )

  row_html <- purrr::pmap_chr(
    list(manifest$link_rel, manifest[[label_col]], manifest$exists, manifest[[notes_col]]),
    function(rel_path, label, exists, notes) {
      link_cell <- make_link_cell(rel_path, label, exists)
      path_cell <- if (isTRUE(exists) && !is.na(rel_path)) html_escape_simple(rel_path) else ""
      notes_cell <- ifelse(is.na(notes), "", html_escape_simple(as.character(notes)))
      paste0(
        "<tr>",
        "<td>", link_cell, "</td>",
        "<td>", path_cell, "</td>",
        "<td>", notes_cell, "</td>",
        "</tr>"
      )
    }
  )

  index_html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    '  <meta charset="utf-8">',
    paste0("  <title>", html_escape_simple(title_text), "</title>"),
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
    paste0("  <h1>", html_escape_simple(title_text), "</h1>"),
    paste0("  <p>", html_escape_simple(intro_text), "</p>"),
    paste0("  <p><strong>Total entries:</strong> ", nrow(manifest), "</p>"),
    "  <table>",
    "    <thead>",
    paste0("      ", header_html),
    "    </thead>",
    "    <tbody>",
    row_html,
    "    </tbody>",
    "  </table>",
    "</body>",
    "</html>"
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(index_html, con = output_path)

  invisible(output_path)
}


get_output_dir <- function(script_id) {
  dir <- file.path("data_output", script_id)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  return(dir)
}

# =========================================================
# 10) Public/anonymized analysis datasets                 ===
# =========================================================

read_required_rds <- function(path, object_label = "RDS object") {
  if (!file.exists(path)) {
    stop(
      paste0(
        object_label, " could not be found at:\n", path,
        "\nPlease run script 13_create_final_anonymized_dataset.R locally first, ",
        "or place the anonymized dataset file in data_final/."
      ),
      call. = FALSE
    )
  }
  readRDS(path)
}

derive_feature_lookup_from_dataset <- function(df, survey_name = NA_character_) {
  tibble::tibble(
    survey = survey_name,
    variable_name = names(df),
    question_text = names(df)
  )
}

load_feature_lookup_or_derive <- function(project_root,
                                          file_stem,
                                          df,
                                          survey_name = NA_character_) {
  rds_path <- file.path(project_root, "data_final", paste0(file_stem, ".rds"))
  csv_path <- file.path(project_root, "data_final", paste0(file_stem, ".csv"))

  if (file.exists(rds_path)) {
    return(readRDS(rds_path))
  }

  if (file.exists(csv_path)) {
    return(readr::read_csv(csv_path, show_col_types = FALSE))
  }

  derive_feature_lookup_from_dataset(df, survey_name = survey_name)
}

load_anonymized_analysis_datasets <- function(project_root = here::here(),
                                              require_pre = FALSE,
                                              require_main = FALSE,
                                              require_final = TRUE) {
  data_final_dir <- file.path(project_root, "data_final")

  out <- list()

  if (isTRUE(require_pre)) {
    out$pre_survey_dataset <- read_required_rds(
      file.path(data_final_dir, "pre_survey_anonymized.rds"),
      "Pre-Survey anonymized dataset"
    )
    out$pre_feature_lookup <- load_feature_lookup_or_derive(
      project_root,
      "pre_feature_lookup_public",
      out$pre_survey_dataset,
      "Pre_Survey"
    )
  }

  if (isTRUE(require_main)) {
    out$main_survey_dataset <- read_required_rds(
      file.path(data_final_dir, "main_survey_anonymized.rds"),
      "Main-Survey anonymized dataset"
    )
    out$main_feature_lookup <- load_feature_lookup_or_derive(
      project_root,
      "main_feature_lookup_public",
      out$main_survey_dataset,
      "Main_Survey"
    )
  }

  if (isTRUE(require_final)) {
    out$final_analysis_dataset <- read_required_rds(
      file.path(data_final_dir, "final_analysis_dataset_anonymized.rds"),
      "Final matched anonymized analysis dataset"
    )
  }

  out
}

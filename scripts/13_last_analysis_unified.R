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
# - ML-Modellvergleiche
# - Exporte als CSV, XLSX, TXT, PNG und HTML-Index
#
# Nicht enthalten:
# - kein erneuter Import der Rohdaten
# - kein erneutes Data Cleaning
# - kein erneutes Matching
# - kein Prompt-Coding-Join
# - keine zusätzlichen Q52-, Change- oder Prompt-Sequence-Analysen

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

message("Confirmation: Script 13 uses data_final/final_analysis_dataset_anonymized.rds.")

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
  x_lower <- stringr::str_to_lower(x)

  out <- dplyr::case_when(
    x_lower %in% c("abstract", "abstrakt") ~ "Abstract",
    x_lower %in% c("concrete", "konkret") ~ "Concrete",
    TRUE ~ NA_character_
  )

  factor(out, levels = c("Abstract", "Concrete"), ordered = FALSE)
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
      levels = c("Abstract", "Concrete"),
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

agreement_plot <- ggplot(
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
  "29_combined_icc" = combined_icc
)

purrr::iwalk(
  tables_to_export,
  ~ save_table_outputs(.x, .y, out_dir = out_tables_dir)
)

writexl::write_xlsx(
  tables_to_export,
  path = file.path(out_base_dir, "13_image_agreement_final_unified_tables.xlsx")
)

# =========================================================
# 15) Grafiken und Modell-Summaries exportieren           ===
# =========================================================

ggsave(
  file.path(out_figures_dir, "AgreementFig1_mean_agreement_by_round.png"),
  agreement_plot,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(out_figures_dir, "AgreementFig2_individual_agreement_trajectories.png"),
  individual_change_plot,
  width = 9,
  height = 6,
  dpi = 300
)

writeLines(
  capture.output(summary(model_primary_round_factor)),
  con = file.path(out_doc_dir, "13_model1_primary_round_factor_summary.txt")
)

writeLines(
  capture.output(summary(model_controlled_q7)),
  con = file.path(out_doc_dir, "13_model2_controlled_q7_summary.txt")
)

writeLines(
  capture.output(summary(model_ordinal_q7)),
  con = file.path(out_doc_dir, "13_model3_ordinal_q7_summary.txt")
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
  file.path(out_base_dir, "13_image_agreement_final_unified_tables.xlsx"),
  "",
  "Exported figures:",
  file.path(out_figures_dir, "AgreementFig1_mean_agreement_by_round.png"),
  file.path(out_figures_dir, "AgreementFig2_individual_agreement_trajectories.png"),
  "",
  "Model summaries:",
  file.path(out_doc_dir, "13_model1_primary_round_factor_summary.txt"),
  file.path(out_doc_dir, "13_model2_controlled_q7_summary.txt"),
  file.path(out_doc_dir, "13_model3_ordinal_q7_summary.txt")
)

writeLines(
  console_summary,
  con = file.path(out_doc_dir, "13_image_agreement_final_unified_console_summary.txt")
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
  paste0("Kombinierte Excel-Datei: ", file.path(out_base_dir, "13_image_agreement_final_unified_tables.xlsx")),
  "",
  "==================== END OF REPORT ===================="
)

writeLines(
  method_results_report,
  con = file.path(out_doc_dir, "13_image_agreement_method_results_report.txt")
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
    file.path(out_base_dir, "13_image_agreement_final_unified_tables.xlsx"),
    file.path(out_figures_dir, "AgreementFig1_mean_agreement_by_round.png"),
    file.path(out_figures_dir, "AgreementFig2_individual_agreement_trajectories.png"),
    file.path(out_doc_dir, "13_model1_primary_round_factor_summary.txt"),
    file.path(out_doc_dir, "13_model2_controlled_q7_summary.txt"),
    file.path(out_doc_dir, "13_model3_ordinal_q7_summary.txt"),
    file.path(out_doc_dir, "13_image_agreement_final_unified_console_summary.txt"),
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

export_manifest <- bind_rows(
  table_manifest_csv,
  table_manifest_xlsx,
  other_manifest
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
message("Workbook: ", file.path(out_base_dir, "13_image_agreement_final_unified_tables.xlsx"))
message("Local index: ", file.path(out_doc_dir, "00_export_index.html"))
message("Console summary: ", file.path(out_doc_dir, "13_image_agreement_final_unified_console_summary.txt"))
message("ChatGPT summary: ", file.path(project_root, "data_output", "RESULTS_FOR_CHATGPT_image_agreement_final_unified.txt"))

#####################################################################
### End of workflow                                               ###
#####################################################################

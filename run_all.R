project_root <- here::here()

source(file.path(project_root, "scripts", "00_project_helpers_unified.R"), local = .GlobalEnv)

run_private_rebuild <- identical(tolower(Sys.getenv("RUN_PRIVATE_REBUILD", "false")), "true")

if (run_private_rebuild) {
  message("Running private rebuild from raw/private inputs.")
  source(file.path(project_root, "scripts", "01_masterarbeit_data_cleaning_workflow_csv_unified.R"), local = .GlobalEnv)
  source(file.path(project_root, "scripts", "02_viviq_scoring_matched_sample_unified.R"), local = .GlobalEnv)
  source(file.path(project_root, "scripts", "03_coding_promts.qc.R"), local = .GlobalEnv)
  source(file.path(project_root, "scripts", "04_create_final_anonymized_dataset.R"), local = .GlobalEnv)
} else {
  message("Running public workflow from data_final/final_analysis_dataset_anonymized.*")
}

# Public analysis workflow based on the final anonymized dataset.
source(file.path(project_root, "scripts", "05_variablenuebersicht.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "06_deskriptive_statistik_und_reporting_final_konsolidiert_erweitert_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "07_pre_survey_premerge_requested_descriptives_fixed_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "08_pre_survey_premerge_requested_descriptives_gt_fixed_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "09_main_study_results_gt_fixed_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "10_viviq_level_effect_plots_main_study_fixed_categories_with_n_unified_v2.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "11_q6_duration_effect_plots_main_study_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "12_target_word_category_effect_plots_main_study_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "13_last_analysis_unified.R"), local = .GlobalEnv)
source(file.path(project_root, "scripts", "99_master_konsolidierter_export_index_FIXED_research_gt_docs.R"), local = .GlobalEnv)

message("Pipeline completed.")

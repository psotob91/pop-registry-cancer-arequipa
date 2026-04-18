# =====================================================
# SCRIPT: 06_qc_epidemiologic_registry.R
# PURPOSE: QC epidemiológico del dataset analítico canónico
# PROJECT: pop-registry-cancer-arequipa
# PRINCIPIOS:
# - usa como única fuente de verdad el .rds analítico de 05
# - no reconstruye el dataset analítico
# - prioriza flags ya calculados en 05 cuando existen
# - degrada con prudencia si alguna columna auxiliar falta
# =====================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(glue)
  library(fs)
  library(cli)
})

# =========================
# 01. Helpers
# =========================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

read_csv_if_exists <- function(path, ...) {
  if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE, ...) else NULL
}

safe_pull <- function(data, col) {
  if (is.na(col) || !col %in% names(data)) return(rep(NA, nrow(data)))
  data[[col]]
}

pick_first_existing <- function(data, candidates, required = FALSE, label = NULL) {
  hit <- candidates[candidates %in% names(data)][1]
  if (length(hit) == 0 || is.na(hit)) {
    msg <- glue("No se encontró columna para: {label %||% paste(candidates, collapse = ', ')}")
    if (isTRUE(required)) rlang::abort(msg)
    cli::cli_alert_warning(msg)
    return(NA_character_)
  }
  hit
}

normalize_chr <- function(x) {
  x %>% as.character() %>% stringr::str_trim() %>% na_if("")
}

normalize_lower_chr <- function(x) {
  normalize_chr(x) %>% stringr::str_to_lower()
}

is_unknown_like <- function(x) {
  x_norm <- normalize_lower_chr(x)
  x_norm %in% c("unknown", "unknown_or_unresolved", "unresolved", "unk", "sin dato", "missing", "not_known", "not known")
}

is_missing_like <- function(x) {
  x_norm <- normalize_lower_chr(x)
  is.na(x_norm) | x_norm %in% c("", "missing", "na", "null")
}

as_true_vec <- function(x) {
  (!is.na(x)) & (x %in% c(TRUE, 1, "1", "true", "TRUE", "yes", "YES"))
}

safe_mean_true <- function(x) {
  mean(as_true_vec(x), na.rm = TRUE)
}

count_with_prop <- function(data, ..., .drop = FALSE, sort = FALSE) {
  data %>%
    count(..., .drop = .drop, sort = sort, name = "n") %>%
    mutate(prop = n / sum(n))
}

write_csv_utf8 <- function(data, path) {
  readr::write_csv(data, path, na = "")
  invisible(path)
}

build_indicator_row <- function(data, name, expr_num, expr_den = rep(TRUE, nrow(data))) {
  expr_num <- (!is.na(expr_num)) & expr_num
  expr_den <- (!is.na(expr_den)) & expr_den
  den <- sum(expr_den, na.rm = TRUE)
  num <- sum(expr_num & expr_den, na.rm = TRUE)
  tibble(indicator = name, numerator = num, denominator = den, percent = ifelse(den > 0, 100 * num / den, NA_real_))
}

# =========================
# 02. Paths
# =========================

path_input <- here("DATA", "DERIVED", "ANALYTIC", "rcpa_arequipa_2015_2022_analytic_dataset.rds")
path_qc_data <- here("DATA", "DERIVED", "QC")
path_qc_reports <- here("REPORTS", "QC")

fs::dir_create(path_qc_data)
fs::dir_create(path_qc_reports)

if (!file.exists(path_input)) {
  rlang::abort(glue("No existe el input canónico: {path_input}"))
}

# =========================
# 03. Load inputs
# =========================

dt <- readRDS(path_input) %>% as_tibble()

prior_qc_summary <- read_csv_if_exists(here("DATA", "DERIVED", "ANALYTIC", "rcpa_arequipa_2015_2022_analytic_dataset_qc_summary.csv"))
prior_active_denom_rule <- read_csv_if_exists(here("DATA", "DERIVED", "ANALYTIC", "rcpa_arequipa_2015_2022_analytic_dataset_active_denominator_rule.csv"))

# =========================
# 04. Column resolution aligned to 05
# =========================

col_record_id <- pick_first_existing(dt, c("row_id"), label = "record id")
col_year <- pick_first_existing(dt, c("incident_year"), required = TRUE, label = "year")
col_sex <- pick_first_existing(dt, c("sex_analytic"), label = "sex")
col_age <- pick_first_existing(dt, c("age_numeric"), label = "age")
col_age_group <- pick_first_existing(dt, c("age_group_iarc"), label = "age group")
col_topography <- pick_first_existing(dt, c("topography_icdo"), label = "topography")
col_morphology <- pick_first_existing(dt, c("morphology_icdo"), label = "morphology")
col_basis_value <- pick_first_existing(dt, c("basis_of_diagnosis_value"), label = "basis value")
col_basis_group <- pick_first_existing(dt, c("basis_of_diagnosis_group"), label = "basis group")
col_residence_area <- pick_first_existing(dt, c("residence_analytic_area"), label = "residence analytic area")
col_vital_status <- pick_first_existing(dt, c("vital_status_analytic"), label = "vital status")
col_vital_status_raw <- pick_first_existing(dt, c("vital_status_raw_mapped", "vital_status_analytic"), label = "vital status raw mapped")
col_vital_status_resolved <- pick_first_existing(dt, c("vital_status_resolved_for_analysis"), label = "vital status resolved")
col_laterality <- pick_first_existing(dt, c("laterality_analytic"), label = "laterality")
col_incidence_included <- pick_first_existing(dt, c("analytic_inclusion_incidence"), label = "incidence included")
col_incidence_included_official <- pick_first_existing(dt, c("analytic_inclusion_incidence_official", "analytic_inclusion_incidence"), label = "incidence included official")
col_incidence_included_sensitivity <- pick_first_existing(dt, c("analytic_inclusion_incidence_sensitivity"), label = "incidence included sensitivity")
col_incidence_date_status <- pick_first_existing(dt, c("incident_date_validity_status"), label = "incident date validity status")
col_incidence_year_for_analysis <- pick_first_existing(dt, c("incident_year_for_analysis", "incident_year"), label = "incident year for analysis")
col_incidence_rate_eligibility <- pick_first_existing(dt, c("incidence_rate_eligibility_status"), label = "incidence rate eligibility status")
col_death_date <- pick_first_existing(dt, c("fecha_muerte"), label = "death date")
col_death_date_raw_present <- pick_first_existing(dt, c("death_date_raw_present"), label = "death date raw present")
col_death_date_valid <- pick_first_existing(dt, c("death_date_valid"), label = "death date valid")
col_death_date_resolution_status <- pick_first_existing(dt, c("death_date_resolution_status"), label = "death date resolution status")
col_death_year_for_analysis <- pick_first_existing(dt, c("death_year_for_analysis", "death_year"), label = "death year for analysis")
col_mortality_official <- pick_first_existing(dt, c("analytic_inclusion_mortality_official", "analytic_inclusion_mortality"), label = "mortality official inclusion")
col_mortality_sensitivity <- pick_first_existing(dt, c("analytic_inclusion_mortality_sensitivity"), label = "mortality sensitivity inclusion")

flag_topography_missing <- pick_first_existing(dt, c("flag_topography_missing"), label = "flag_topography_missing")
flag_morphology_missing <- pick_first_existing(dt, c("flag_morphology_missing"), label = "flag_morphology_missing")
flag_mv_candidate <- pick_first_existing(dt, c("flag_mv_candidate"), label = "flag_mv_candidate")
flag_dco_candidate <- pick_first_existing(dt, c("flag_dco_candidate"), label = "flag_dco_candidate")
flag_psu_candidate <- pick_first_existing(dt, c("flag_psu_candidate"), label = "flag_psu_candidate")
flag_dead_without_death_date <- pick_first_existing(dt, c("flag_dead_without_death_date"), label = "flag_dead_without_death_date")
flag_alive_with_death_date <- pick_first_existing(dt, c("flag_alive_with_death_date"), label = "flag_alive_with_death_date")
flag_vital_inconsistency <- pick_first_existing(dt, c("flag_vital_inconsistency"), label = "flag_vital_inconsistency")
flag_laterality_unknown <- pick_first_existing(dt, c("flag_laterality_unknown"), label = "flag_laterality_unknown")
flag_death_placeholder <- pick_first_existing(dt, c("flag_death_date_placeholder_or_invalid"), label = "flag_death_date_placeholder_or_invalid")
flag_death_before_diagnosis <- pick_first_existing(dt, c("flag_death_before_diagnosis"), label = "flag_death_before_diagnosis")
flag_fuc_after_death <- pick_first_existing(dt, c("flag_fuc_after_death"), label = "flag_fuc_after_death")
flag_proxy_fuc_candidate <- pick_first_existing(dt, c("flag_proxy_fuc_candidate"), label = "flag_proxy_fuc_candidate")
flag_incident_placeholder <- pick_first_existing(dt, c("flag_incident_date_placeholder_or_invalid"), label = "flag_incident_date_placeholder_or_invalid")
flag_incident_outside_period <- pick_first_existing(dt, c("flag_incident_date_outside_period"), label = "flag_incident_date_outside_period")
flag_incident_missing <- pick_first_existing(dt, c("flag_incident_date_missing"), label = "flag_incident_date_missing")
flag_incidence_crude_only <- pick_first_existing(dt, c("flag_incidence_crude_only_case"), label = "flag_incidence_crude_only_case")

# =========================
# 05. Derived working variables
# =========================

dt_qc <- dt %>%
  mutate(
    qc_year = safe_pull(., .env$col_year),
    qc_sex = safe_pull(., .env$col_sex),
    qc_age = suppressWarnings(as.numeric(safe_pull(., .env$col_age))),
    qc_age_group = safe_pull(., .env$col_age_group),
    qc_topography = safe_pull(., .env$col_topography),
    qc_topography_group = safe_pull(., .env$col_topography),
    qc_morphology = safe_pull(., .env$col_morphology),
    qc_morphology_group = safe_pull(., .env$col_morphology),
    qc_basis_value = safe_pull(., .env$col_basis_value),
    qc_basis_group = safe_pull(., .env$col_basis_group),
    qc_residence_area = safe_pull(., .env$col_residence_area),
    qc_vital_status = safe_pull(., .env$col_vital_status),
    qc_vital_status_raw = safe_pull(., .env$col_vital_status_raw),
    qc_vital_status_resolved = safe_pull(., .env$col_vital_status_resolved),
    qc_laterality = safe_pull(., .env$col_laterality),
    qc_incidence_included_raw = safe_pull(., .env$col_incidence_included),
    qc_incidence_included_official = safe_pull(., .env$col_incidence_included_official),
    qc_incidence_included_sensitivity = safe_pull(., .env$col_incidence_included_sensitivity),
    qc_incidence_date_status = safe_pull(., .env$col_incidence_date_status),
    qc_incidence_year_for_analysis = safe_pull(., .env$col_incidence_year_for_analysis),
    qc_incidence_rate_eligibility = safe_pull(., .env$col_incidence_rate_eligibility),
    qc_death_date = safe_pull(., .env$col_death_date),
    qc_death_date_raw_present = safe_pull(., .env$col_death_date_raw_present),
    qc_death_date_valid = safe_pull(., .env$col_death_date_valid),
    qc_death_date_resolution_status = safe_pull(., .env$col_death_date_resolution_status),
    qc_death_year_for_analysis = safe_pull(., .env$col_death_year_for_analysis),
    qc_mortality_official = safe_pull(., .env$col_mortality_official),
    qc_mortality_sensitivity = safe_pull(., .env$col_mortality_sensitivity),
    qc_flag_topography_missing = safe_pull(., .env$flag_topography_missing),
    qc_flag_morphology_missing = safe_pull(., .env$flag_morphology_missing),
    qc_flag_mv_candidate = safe_pull(., .env$flag_mv_candidate),
    qc_flag_dco_candidate = safe_pull(., .env$flag_dco_candidate),
    qc_flag_psu_candidate = safe_pull(., .env$flag_psu_candidate),
    qc_flag_dead_without_death_date = safe_pull(., .env$flag_dead_without_death_date),
    qc_flag_alive_with_death_date = safe_pull(., .env$flag_alive_with_death_date),
    qc_flag_vital_inconsistency = safe_pull(., .env$flag_vital_inconsistency),
    qc_flag_laterality_unknown = safe_pull(., .env$flag_laterality_unknown),
    qc_flag_death_placeholder = safe_pull(., .env$flag_death_placeholder),
    qc_flag_death_before_diagnosis = safe_pull(., .env$flag_death_before_diagnosis),
    qc_flag_fuc_after_death = safe_pull(., .env$flag_fuc_after_death),
    qc_flag_proxy_fuc_candidate = safe_pull(., .env$flag_proxy_fuc_candidate),
    qc_flag_incident_placeholder = safe_pull(., .env$flag_incident_placeholder),
    qc_flag_incident_outside_period = safe_pull(., .env$flag_incident_outside_period),
    qc_flag_incident_missing = safe_pull(., .env$flag_incident_missing),
    qc_flag_incidence_crude_only = safe_pull(., .env$flag_incidence_crude_only)
  ) %>%
  mutate(
    qc_incidence_included_norm = normalize_lower_chr(qc_incidence_included_raw),
    qc_incidence_included = case_when(
      qc_incidence_included_norm == "yes" ~ TRUE,
      qc_incidence_included_norm == "no" ~ FALSE,
      qc_incidence_included_norm == "unknown" ~ NA,
      is.na(qc_incidence_included_norm) ~ NA,
      TRUE ~ NA
    ),
    qc_age_implausible = !is.na(qc_age) & (qc_age < 0 | qc_age > 110),
    qc_topography_missing_derived = is_missing_like(qc_topography) | is_unknown_like(qc_topography),
    qc_morphology_missing_derived = is_missing_like(qc_morphology) | is_unknown_like(qc_morphology),
    qc_basis_unknown_derived = is_missing_like(qc_basis_group) | is_unknown_like(qc_basis_group),
    qc_residence_unknown_derived = is_missing_like(qc_residence_area) | is_unknown_like(qc_residence_area),
    qc_laterality_unknown_derived = dplyr::case_when(
      !all(is.na(qc_flag_laterality_unknown)) ~ as_true_vec(qc_flag_laterality_unknown),
      TRUE ~ is_missing_like(qc_laterality) | is_unknown_like(qc_laterality)
    ),
    qc_vital_unknown_derived = is_missing_like(qc_vital_status) | is_unknown_like(qc_vital_status),
    qc_psu_missing_topography = as_true_vec(qc_flag_psu_candidate) & (as_true_vec(qc_flag_topography_missing) | qc_topography_missing_derived)
  )

# =========================
# 06. Run metadata
# =========================

run_metadata <- tibble(
  run_date = as.character(Sys.Date()),
  input_path = path_input,
  n_rows = nrow(dt_qc),
  n_cols = ncol(dt_qc),
  column_year = col_year,
  column_sex = col_sex,
  column_age = col_age,
  column_age_group = col_age_group,
  column_topography = col_topography,
  column_morphology = col_morphology,
  column_basis_group = col_basis_group,
  column_residence_area = col_residence_area,
  column_vital_status = col_vital_status,
  column_incidence_included = col_incidence_included
)
write_csv_utf8(run_metadata, file.path(path_qc_data, "qc_epidemiologic_run_metadata.csv"))

# =========================
# 07. General structure
# =========================

qc_structure_summary <- tibble(
  n_total = nrow(dt_qc),
  n_incidence_yes = sum(dt_qc$qc_incidence_included %in% TRUE, na.rm = TRUE),
  n_incidence_no = sum(dt_qc$qc_incidence_included %in% FALSE, na.rm = TRUE),
  n_incidence_unknown = sum(is.na(dt_qc$qc_incidence_included))
)
write_csv_utf8(qc_structure_summary, file.path(path_qc_data, "qc_structure_summary.csv"))

write_csv_utf8(count_with_prop(dt_qc, qc_year, sort = FALSE), file.path(path_qc_data, "qc_distribution_year.csv"))
write_csv_utf8(count_with_prop(dt_qc, qc_year, qc_sex, sort = FALSE), file.path(path_qc_data, "qc_distribution_year_sex.csv"))
write_csv_utf8(count_with_prop(dt_qc, qc_year, qc_sex, qc_age_group, sort = FALSE), file.path(path_qc_data, "qc_distribution_year_sex_age.csv"))
write_csv_utf8(count_with_prop(dt_qc, qc_topography_group, sort = TRUE), file.path(path_qc_data, "qc_topography_distribution.csv"))
write_csv_utf8(count_with_prop(dt_qc, qc_morphology_group, sort = TRUE), file.path(path_qc_data, "qc_morphology_distribution.csv"))

# Duplicate screen técnico: row_id es identificador técnico, no QC epidemiológico sustantivo
qc_duplicate_screen <- tibble(
  duplicate_screen_note = "row_id es identificador técnico único; no usar este output como proxy de duplicados epidemiológicos reales.",
  duplicate_id_rows = NA_integer_,
  max_duplicate_count = NA_integer_
)
write_csv_utf8(qc_duplicate_screen, file.path(path_qc_data, "qc_duplicate_screen.csv"))

# =========================
# 08. Critical variable quality
# =========================

qc_missing_core <- bind_rows(
  build_indicator_row(dt_qc, "sex_missing", is_missing_like(dt_qc$qc_sex)),
  build_indicator_row(dt_qc, "age_missing", is.na(dt_qc$qc_age)),
  build_indicator_row(dt_qc, "age_implausible", dt_qc$qc_age_implausible),
  build_indicator_row(dt_qc, "topography_missing", if (!is.na(flag_topography_missing)) as_true_vec(dt_qc$qc_flag_topography_missing) else dt_qc$qc_topography_missing_derived),
  build_indicator_row(dt_qc, "morphology_missing", if (!is.na(flag_morphology_missing)) as_true_vec(dt_qc$qc_flag_morphology_missing) else dt_qc$qc_morphology_missing_derived),
  build_indicator_row(dt_qc, "basis_unknown_or_missing", dt_qc$qc_basis_unknown_derived),
  build_indicator_row(dt_qc, "residence_unknown_or_missing", dt_qc$qc_residence_unknown_derived),
  build_indicator_row(dt_qc, "vital_inconsistency", as_true_vec(dt_qc$qc_flag_vital_inconsistency))
)
write_csv_utf8(qc_missing_core, file.path(path_qc_data, "qc_missing_core.csv"))

qc_missing_core_by_year <- dt_qc %>%
  group_by(qc_year) %>%
  summarise(
    n = n(),
    sex_missing_pct = 100 * mean(is_missing_like(qc_sex)),
    age_missing_pct = 100 * mean(is.na(qc_age)),
    age_implausible_pct = 100 * mean(qc_age_implausible, na.rm = TRUE),
    topography_missing_pct = 100 * mean(qc_topography_missing_derived, na.rm = TRUE),
    morphology_missing_pct = 100 * mean(qc_morphology_missing_derived, na.rm = TRUE),
    basis_unknown_or_missing_pct = 100 * mean(qc_basis_unknown_derived, na.rm = TRUE),
    residence_unknown_or_missing_pct = 100 * mean(qc_residence_unknown_derived, na.rm = TRUE),
    vital_inconsistency_pct = 100 * safe_mean_true(qc_flag_vital_inconsistency),
    .groups = "drop"
  )
write_csv_utf8(qc_missing_core_by_year, file.path(path_qc_data, "qc_missing_core_by_year.csv"))

# =========================
# 09. Registry indicators
# =========================

qc_registry_indicators <- bind_rows(
  build_indicator_row(dt_qc, "mv_candidate_pct_all_records", as_true_vec(dt_qc$qc_flag_mv_candidate)),
  build_indicator_row(dt_qc, "dco_candidate_pct_all_records", as_true_vec(dt_qc$qc_flag_dco_candidate)),
  build_indicator_row(dt_qc, "psu_candidate_pct_all_records", as_true_vec(dt_qc$qc_flag_psu_candidate)),
  build_indicator_row(dt_qc, "psu_missing_topography_pct_all_records", dt_qc$qc_psu_missing_topography),
  build_indicator_row(dt_qc, "laterality_unknown_pct_all_records", dt_qc$qc_laterality_unknown_derived),
  build_indicator_row(dt_qc, "mv_candidate_pct_incidence_included", as_true_vec(dt_qc$qc_flag_mv_candidate), dt_qc$qc_incidence_included %in% TRUE),
  build_indicator_row(dt_qc, "dco_candidate_pct_incidence_included", as_true_vec(dt_qc$qc_flag_dco_candidate), dt_qc$qc_incidence_included %in% TRUE),
  build_indicator_row(dt_qc, "psu_candidate_pct_incidence_included", as_true_vec(dt_qc$qc_flag_psu_candidate), dt_qc$qc_incidence_included %in% TRUE)
)
write_csv_utf8(qc_registry_indicators, file.path(path_qc_data, "qc_registry_indicators.csv"))

qc_registry_indicators_by_year <- dt_qc %>%
  group_by(qc_year) %>%
  summarise(
    n = n(),
    mv_candidate_pct = 100 * safe_mean_true(qc_flag_mv_candidate),
    dco_candidate_pct = 100 * safe_mean_true(qc_flag_dco_candidate),
    psu_candidate_pct = 100 * safe_mean_true(qc_flag_psu_candidate),
    psu_missing_topography_pct = 100 * mean(qc_psu_missing_topography, na.rm = TRUE),
    laterality_unknown_pct = 100 * mean(qc_laterality_unknown_derived, na.rm = TRUE),
    .groups = "drop"
  )

qc_registry_indicators_by_year_sex <- dt_qc %>%
  group_by(qc_year, qc_sex) %>%
  summarise(
    n = n(),
    mv_candidate_pct = 100 * safe_mean_true(qc_flag_mv_candidate),
    dco_candidate_pct = 100 * safe_mean_true(qc_flag_dco_candidate),
    psu_candidate_pct = 100 * safe_mean_true(qc_flag_psu_candidate),
    .groups = "drop"
  )

write_csv_utf8(qc_registry_indicators_by_year, file.path(path_qc_data, "qc_registry_indicators_by_year.csv"))
write_csv_utf8(qc_registry_indicators_by_year_sex, file.path(path_qc_data, "qc_registry_indicators_by_year_sex.csv"))

# =========================
# 10. Residence
# =========================

qc_residence_distribution <- count_with_prop(dt_qc, qc_residence_area, sort = TRUE)
qc_residence_incidence_cross <- dt_qc %>%
  count(qc_residence_area, qc_incidence_included, name = "n") %>%
  group_by(qc_residence_area) %>%
  mutate(prop_within_residence = n / sum(n)) %>%
  ungroup()

write_csv_utf8(qc_residence_distribution, file.path(path_qc_data, "qc_residence_distribution.csv"))
write_csv_utf8(qc_residence_incidence_cross, file.path(path_qc_data, "qc_residence_incidence_cross.csv"))

# =========================
# 11. Basis of diagnosis
# =========================

write_csv_utf8(count_with_prop(dt_qc, qc_basis_value, sort = TRUE), file.path(path_qc_data, "qc_basis_value_distribution.csv"))
qc_basis_group_distribution <- count_with_prop(dt_qc, qc_basis_group, sort = TRUE)
write_csv_utf8(qc_basis_group_distribution, file.path(path_qc_data, "qc_basis_group_distribution.csv"))

qc_basis_group_by_year <- dt_qc %>%
  count(qc_year, qc_basis_group, name = "n") %>%
  group_by(qc_year) %>%
  mutate(prop_within_year = n / sum(n)) %>%
  ungroup()

qc_basis_group_by_year_sex <- dt_qc %>%
  count(qc_year, qc_sex, qc_basis_group, name = "n") %>%
  group_by(qc_year, qc_sex) %>%
  mutate(prop_within_year_sex = n / sum(n)) %>%
  ungroup()

write_csv_utf8(qc_basis_group_by_year, file.path(path_qc_data, "qc_basis_group_by_year.csv"))
write_csv_utf8(qc_basis_group_by_year_sex, file.path(path_qc_data, "qc_basis_group_by_year_sex.csv"))

# =========================
# 12. Vital status
# =========================

write_csv_utf8(count_with_prop(dt_qc, qc_vital_status, sort = TRUE), file.path(path_qc_data, "qc_vital_distribution.csv"))

qc_vital_flags <- bind_rows(
  build_indicator_row(dt_qc, "dead_without_death_date", as_true_vec(dt_qc$qc_flag_dead_without_death_date)),
  build_indicator_row(dt_qc, "alive_with_death_date", as_true_vec(dt_qc$qc_flag_alive_with_death_date)),
  build_indicator_row(dt_qc, "vital_inconsistency", as_true_vec(dt_qc$qc_flag_vital_inconsistency)),
  build_indicator_row(dt_qc, "vital_status_unknown_or_missing", dt_qc$qc_vital_unknown_derived)
)
write_csv_utf8(qc_vital_flags, file.path(path_qc_data, "qc_vital_flags.csv"))

qc_vital_by_year <- dt_qc %>%
  count(qc_year, qc_vital_status, name = "n") %>%
  group_by(qc_year) %>%
  mutate(prop_within_year = n / sum(n)) %>%
  ungroup()
write_csv_utf8(qc_vital_by_year, file.path(path_qc_data, "qc_vital_by_year.csv"))

qc_raw_followup_external <- bind_rows(
  build_indicator_row(dt_qc, "estvit_missing_raw", is_missing_like(dt_qc$qc_vital_status_raw)),
  build_indicator_row(dt_qc, "fecdef_present_raw", as_true_vec(dt_qc$qc_death_date_raw_present)),
  build_indicator_row(dt_qc, "fuc_present_raw", !is_missing_like(dt_qc$fecha_ultimo_contacto)),
  build_indicator_row(dt_qc, "alive_with_any_fecdef_raw", as_true_vec(dt_qc$qc_flag_alive_with_death_date)),
  build_indicator_row(dt_qc, "dead_without_any_fecdef_raw", as_true_vec(dt_qc$qc_flag_dead_without_death_date)),
  build_indicator_row(dt_qc, "fecha_muerte_placeholder_or_invalid", as_true_vec(dt_qc$qc_flag_death_placeholder)),
  build_indicator_row(dt_qc, "fecha_muerte_before_diagnostico", as_true_vec(dt_qc$qc_flag_death_before_diagnosis)),
  build_indicator_row(dt_qc, "fuc_after_death", as_true_vec(dt_qc$qc_flag_fuc_after_death))
)
write_csv_utf8(qc_raw_followup_external, file.path(path_qc_data, "qc_raw_followup_external_summary.csv"))

qc_raw_followup_cross <- dt_qc %>%
  mutate(
    qc_fecdef_state = case_when(
      as_true_vec(qc_death_date_valid) ~ "valid_death_date",
      as_true_vec(qc_death_date_raw_present) ~ "raw_date_present_invalid_or_placeholder",
      TRUE ~ "no_death_date"
    ),
    qc_fuc_state = case_when(
      is_missing_like(fecha_ultimo_contacto) ~ "no_fuc",
      TRUE ~ "fuc_present"
    )
  ) %>%
  count(qc_year, qc_vital_status_raw, qc_fecdef_state, qc_fuc_state, name = "n")
write_csv_utf8(qc_raw_followup_cross, file.path(path_qc_data, "qc_raw_followup_cross.csv"))

qc_internal_mortality_resolution <- dt_qc %>%
  count(
    qc_vital_status_raw,
    qc_vital_status_resolved,
    qc_death_date_resolution_status,
    qc_mortality_official,
    qc_mortality_sensitivity,
    name = "n"
  )
write_csv_utf8(qc_internal_mortality_resolution, file.path(path_qc_data, "qc_internal_mortality_resolution.csv"))

qc_internal_mortality_ruleset_impact <- bind_rows(
  build_indicator_row(dt_qc, "official_mortality_yes", normalize_lower_chr(dt_qc$qc_mortality_official) == "yes"),
  build_indicator_row(dt_qc, "sensitivity_mortality_yes", normalize_lower_chr(dt_qc$qc_mortality_sensitivity) == "yes"),
  build_indicator_row(dt_qc, "proxy_fuc_candidate", as_true_vec(dt_qc$qc_flag_proxy_fuc_candidate))
)
write_csv_utf8(qc_internal_mortality_ruleset_impact, file.path(path_qc_data, "qc_internal_mortality_ruleset_impact.csv"))

qc_mortality_conflict_case_review <- dt_qc %>%
  filter(
    as_true_vec(qc_flag_alive_with_death_date) |
      as_true_vec(qc_flag_dead_without_death_date) |
      as_true_vec(qc_flag_death_placeholder) |
      as_true_vec(qc_flag_death_before_diagnosis) |
      as_true_vec(qc_flag_fuc_after_death)
  ) %>%
  transmute(
    row_id,
    source_year = qc_year,
    incident_year = qc_incidence_year_for_analysis,
    sex = qc_sex,
    age_group_iarc = qc_age_group,
    topography_icdo = qc_topography,
    vital_status_raw = qc_vital_status_raw,
    vital_status_resolved = qc_vital_status_resolved,
    fecha_diagnostico = fecha_diagnostico,
    fecha_muerte = qc_death_date,
    fecha_ultimo_contacto = fecha_ultimo_contacto,
    death_date_resolution_status = qc_death_date_resolution_status,
    mortality_official = qc_mortality_official,
    mortality_sensitivity = qc_mortality_sensitivity,
    alive_with_valid_fecha_muerte = as_true_vec(qc_flag_alive_with_death_date) & qc_death_date_resolution_status == "valid_consistent",
    alive_with_placeholder_fecha_muerte = as_true_vec(qc_flag_alive_with_death_date) & qc_death_date_resolution_status == "placeholder_or_invalid",
    fecha_muerte_before_diagnosis = as_true_vec(qc_flag_death_before_diagnosis),
    fuc_after_death = as_true_vec(qc_flag_fuc_after_death),
    dead_without_fecha_muerte = as_true_vec(qc_flag_dead_without_death_date)
  )
write_csv_utf8(qc_mortality_conflict_case_review, file.path(path_qc_data, "qc_mortality_conflict_case_review.csv"))

qc_mortality_conflict_summary <- qc_mortality_conflict_case_review %>%
  pivot_longer(
    cols = c(alive_with_valid_fecha_muerte, alive_with_placeholder_fecha_muerte, fecha_muerte_before_diagnosis, fuc_after_death, dead_without_fecha_muerte),
    names_to = "conflict_type",
    values_to = "flag_value"
  ) %>%
  filter(flag_value) %>%
  count(conflict_type, source_year, sex, age_group_iarc, topography_icdo, name = "n")
write_csv_utf8(qc_mortality_conflict_summary, file.path(path_qc_data, "qc_mortality_conflict_summary.csv"))

qc_incidence_raw_external <- bind_rows(
  build_indicator_row(dt_qc, "fecdiag_missing_raw", is_missing_like(dt_qc$fecha_diagnostico)),
  build_indicator_row(dt_qc, "incident_date_placeholder_or_invalid", as_true_vec(dt_qc$qc_flag_incident_placeholder)),
  build_indicator_row(dt_qc, "incident_date_outside_period", as_true_vec(dt_qc$qc_flag_incident_outside_period)),
  build_indicator_row(dt_qc, "incident_date_missing", as_true_vec(dt_qc$qc_flag_incident_missing)),
  build_indicator_row(dt_qc, "residence_unknown_or_unspecified", dt_qc$qc_residence_unknown_derived | dt_qc$qc_residence_area %in% c("departamento_arequipa_unspecified_province")),
  build_indicator_row(dt_qc, "topography_missing_raw_context", if (!is.na(flag_topography_missing)) as_true_vec(dt_qc$qc_flag_topography_missing) else dt_qc$qc_topography_missing_derived),
  build_indicator_row(dt_qc, "morphology_missing_raw_context", if (!is.na(flag_morphology_missing)) as_true_vec(dt_qc$qc_flag_morphology_missing) else dt_qc$qc_morphology_missing_derived),
  build_indicator_row(dt_qc, "basis_problematic_raw_context", dt_qc$qc_basis_unknown_derived)
)
write_csv_utf8(qc_incidence_raw_external, file.path(path_qc_data, "qc_incidence_raw_external_summary.csv"))

qc_incidence_internal_resolution <- dt_qc %>%
  count(
    qc_incidence_date_status,
    qc_incidence_included_official,
    qc_incidence_rate_eligibility,
    qc_sex,
    name = "n"
  )
write_csv_utf8(qc_incidence_internal_resolution, file.path(path_qc_data, "qc_incidence_internal_resolution.csv"))

qc_incidence_internal_ruleset_impact <- bind_rows(
  build_indicator_row(dt_qc, "incidence_official_yes", normalize_lower_chr(dt_qc$qc_incidence_included_official) == "yes"),
  build_indicator_row(dt_qc, "incidence_sensitivity_yes", normalize_lower_chr(dt_qc$qc_incidence_included_sensitivity) == "yes"),
  build_indicator_row(dt_qc, "incidence_crude_only_cases", as_true_vec(dt_qc$qc_flag_incidence_crude_only))
)
write_csv_utf8(qc_incidence_internal_ruleset_impact, file.path(path_qc_data, "qc_incidence_internal_ruleset_impact.csv"))

# =========================
# 13. Topographies / sites
# =========================

qc_top10_topography_incidence <- dt_qc %>%
  filter(qc_incidence_included %in% TRUE) %>%
  count(qc_topography_group, sort = TRUE, name = "n") %>%
  mutate(prop = n / sum(n)) %>%
  slice_head(n = 10)

qc_top10_topography_global <- dt_qc %>%
  count(qc_topography_group, sort = TRUE, name = "n") %>%
  mutate(prop = n / sum(n)) %>%
  slice_head(n = 10)

qc_top10_topography_incidence_by_sex <- dt_qc %>%
  filter(qc_incidence_included %in% TRUE) %>%
  count(qc_sex, qc_topography_group, sort = TRUE, name = "n") %>%
  group_by(qc_sex) %>%
  mutate(prop_within_sex = n / sum(n)) %>%
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
  ungroup()

write_csv_utf8(qc_top10_topography_incidence, file.path(path_qc_data, "qc_top10_topography_incidence.csv"))
write_csv_utf8(qc_top10_topography_global, file.path(path_qc_data, "qc_top10_topography_global.csv"))
write_csv_utf8(qc_top10_topography_incidence_by_sex, file.path(path_qc_data, "qc_top10_topography_incidence_by_sex.csv"))

# =========================
# 14. Cross-check against prior analytic summary
# =========================

qc_crosscheck_prior <- tibble(
  metric = c("n_total", "n_incidence_yes", "n_incidence_no", "n_incidence_unknown"),
  current_value = c(
    qc_structure_summary$n_total,
    qc_structure_summary$n_incidence_yes,
    qc_structure_summary$n_incidence_no,
    qc_structure_summary$n_incidence_unknown
  ),
  prior_value = c(
    prior_qc_summary$n_total %||% NA_real_,
    prior_qc_summary$n_incidence_yes %||% NA_real_,
    prior_qc_summary$n_incidence_no %||% NA_real_,
    prior_qc_summary$n_incidence_unknown %||% NA_real_
  )
) %>%
  mutate(match = dplyr::near(current_value, prior_value) | (is.na(current_value) & is.na(prior_value)))
write_csv_utf8(qc_crosscheck_prior, file.path(path_qc_data, "qc_crosscheck_prior_analytic_summary.csv"))

# =========================
# 15. Summary objects
# =========================

qc_epidemiologic_summary <- list(
  run_metadata = run_metadata,
  structure_summary = qc_structure_summary,
  duplicate_screen = qc_duplicate_screen,
  missing_core = qc_missing_core,
  registry_indicators = qc_registry_indicators,
  residence_distribution = qc_residence_distribution,
  basis_group_distribution = qc_basis_group_distribution,
  vital_flags = qc_vital_flags,
  top10_topography_incidence = qc_top10_topography_incidence,
  top10_topography_global = qc_top10_topography_global,
  prior_crosscheck = qc_crosscheck_prior,
  active_denominator_rule = prior_active_denom_rule
)
saveRDS(qc_epidemiologic_summary, file.path(path_qc_data, "qc_epidemiologic_summary.rds"))

qc_summary_flat <- bind_rows(
  qc_missing_core %>% mutate(section = "critical_variables"),
  qc_registry_indicators %>% mutate(section = "registry_indicators"),
  qc_vital_flags %>% mutate(section = "vital_status")
) %>%
  select(section, everything())
write_csv_utf8(qc_summary_flat, file.path(path_qc_data, "qc_epidemiologic_summary_flat.csv"))

# =========================
# 16. Minimal report stub
# =========================

report_stub <- c(
  "# Epidemiologic QC registry run",
  "",
  glue("- Run date: {Sys.Date()}"),
  glue("- Input: {path_input}"),
  glue("- Total records: {qc_structure_summary$n_total}"),
  glue("- Incidence included: {qc_structure_summary$n_incidence_yes}"),
  glue("- Incidence excluded: {qc_structure_summary$n_incidence_no}"),
  glue("- Incidence unknown: {qc_structure_summary$n_incidence_unknown}"),
  "",
  "Este archivo es un stub operativo para apoyar iteración posterior en Quarto.",
  "No reemplaza el informe narrativo final."
)
readr::write_lines(report_stub, file.path(path_qc_reports, "qc_epidemiologic_registry_stub.md"))

# =========================
# 17. Console messages
# =========================

cli::cli_alert_success("QC epidemiológico completado.")
cli::cli_inform(glue("Input canónico usado: {path_input}"))
cli::cli_inform(glue("Salidas de datos QC: {path_qc_data}"))
cli::cli_inform(glue("Salidas report-ready QC: {path_qc_reports}"))

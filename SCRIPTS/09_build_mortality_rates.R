#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(fs)
  library(cli)
  library(glue)
})

source("SCRIPTS/pipeline_runtime_helpers.R")

root <- find_project_root()
setwd(root)

analytic_path <- project_path(root, "DATA", "DERIVED", "ANALYTIC", "rcpa_arequipa_2015_2022_analytic_dataset.rds")
pop_path <- project_path(root, "DATA", "DERIVED", "POPULATION", "arequipa_province_population_denominators.csv")
weights_path <- project_path(root, "DATA", "DERIVED", "METADATA", "standard_population_age_weights.csv")
rate_spec_path <- project_path(root, "DATA", "DERIVED", "METADATA", "MASTER_RATE_SPEC.csv")
out_dir <- project_path(root, "DATA", "DERIVED", "RATES")
qc_dir <- project_path(root, "DATA", "DERIVED", "QC")
ensure_dirs(out_dir, qc_dir)

dt <- readRDS(analytic_path) %>% as_tibble()
pop <- readr::read_csv(pop_path, show_col_types = FALSE)
weights <- readr::read_csv(weights_path, show_col_types = FALSE)
rate_spec <- readr::read_csv(rate_spec_path, show_col_types = FALSE)
dt <- dt %>% mutate(death_year_sensitivity = dplyr::coalesce(death_year_for_analysis, death_proxy_fuc_year))

n_years <- length(2015:2022)
meta_iteration <- format(Sys.time(), "%Y%m%d_%H%M%S")

build_meta <- function(df, denominator_method) {
  df %>%
    mutate(
      source_id = "rcbpa_mortality_module",
      build_date = as.character(Sys.Date()),
      geographic_scope = "provincia_arequipa",
      denominator_method = denominator_method,
      standard_population = "segi_doll_world_standard",
      iteration_id = meta_iteration
    )
}

build_rate_bundle <- function(data, pop, weights, use_col, year_col, label_prefix) {
  filtered <- data %>%
    mutate(
      sex = dplyr::coalesce(sex_analytic, "unknown"),
      site_code = dplyr::coalesce(topography_icdo, "unknown"),
      death_year_work = .data[[year_col]]
    ) %>%
    filter(.data[[use_col]] == "yes", !is.na(death_year_work), death_year_work %in% 2015:2022, sex %in% c("male", "female"))
  
  age_specific <- filtered %>%
    count(year = death_year_work, sex, age_group_iarc, name = "deaths") %>%
    full_join(
      pop %>%
        filter(sex %in% c("male", "female")) %>%
        select(year, sex, age_group_iarc, population),
      by = c("year", "sex", "age_group_iarc")
    ) %>%
    mutate(
      deaths = dplyr::coalesce(deaths, 0L),
      rate_per_100k = ifelse(population > 0, deaths / population * 100000, NA_real_)
    ) %>%
    build_meta("annual_scaled_department_age_structure")
  
  crude_annual <- filtered %>%
    count(year = death_year_work, sex, name = "deaths") %>%
    left_join(
      pop %>%
        filter(sex %in% c("male", "female")) %>%
        group_by(year, sex) %>%
        summarise(population = sum(population), .groups = "drop"),
      by = c("year", "sex")
    ) %>%
    mutate(rate_per_100k = deaths / population * 100000) %>%
    build_meta("annual_scaled_department_age_structure")
  
  period_age <- filtered %>%
    count(sex, age_group_iarc, name = "deaths_period") %>%
    left_join(
      pop %>% filter(year == 2018, sex %in% c("male", "female")) %>% select(sex, age_group_iarc, population_mid = population),
      by = c("sex", "age_group_iarc")
    ) %>%
    left_join(weights, by = "age_group_iarc") %>%
    mutate(
      person_years = population_mid * n_years,
      age_specific_rate = ifelse(person_years > 0, deaths_period / person_years * 100000, NA_real_)
    )
  
  asr_period <- period_age %>%
    group_by(sex) %>%
    summarise(
      deaths_period = sum(deaths_period, na.rm = TRUE),
      population_mid = sum(population_mid, na.rm = TRUE),
      asr_per_100k = sum(age_specific_rate * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      crude_period_rate_per_100k = sum(deaths_period, na.rm = TRUE) / (first(population_mid) * n_years) * 100000,
      .groups = "drop"
    ) %>%
    build_meta("mid_period_2018_scaled_department_age_structure")
  
  top10_period <- filtered %>%
    count(sex, site_code, name = "deaths_period") %>%
    group_by(sex) %>%
    arrange(sex, desc(deaths_period), .by_group = TRUE) %>%
    slice_head(n = 10) %>%
    ungroup() %>%
    left_join(
      pop %>%
        filter(year == 2018, sex %in% c("male", "female")) %>%
        group_by(sex) %>%
        summarise(population_mid = sum(population), .groups = "drop"),
      by = "sex"
    ) %>%
    mutate(crude_period_rate_per_100k = deaths_period / (population_mid * n_years) * 100000) %>%
    build_meta("mid_period_2018_scaled_department_age_structure")
  
  if (nrow(age_specific) == 0) {
    age_specific <- tibble(
      year = integer(), sex = character(), age_group_iarc = character(), deaths = integer(),
      population = double(), rate_per_100k = double(), source_id = character(), build_date = character(),
      geographic_scope = character(), denominator_method = character(), standard_population = character(), iteration_id = character()
    )
  }
  if (nrow(crude_annual) == 0) {
    crude_annual <- tibble(
      year = integer(), sex = character(), deaths = integer(), population = double(), rate_per_100k = double(),
      source_id = character(), build_date = character(), geographic_scope = character(), denominator_method = character(),
      standard_population = character(), iteration_id = character()
    )
  }
  if (nrow(asr_period) == 0) {
    asr_period <- tibble(
      sex = character(), deaths_period = integer(), population_mid = double(), asr_per_100k = double(),
      crude_period_rate_per_100k = double(), source_id = character(), build_date = character(), geographic_scope = character(),
      denominator_method = character(), standard_population = character(), iteration_id = character()
    )
  }
  if (nrow(top10_period) == 0) {
    top10_period <- tibble(
      sex = character(), site_code = character(), deaths_period = integer(), population_mid = double(),
      crude_period_rate_per_100k = double(), source_id = character(), build_date = character(), geographic_scope = character(),
      denominator_method = character(), standard_population = character(), iteration_id = character()
    )
  }
  
  list(
    filtered = filtered,
    age_specific = age_specific,
    crude_annual = crude_annual,
    asr_period = asr_period,
    top10_period = top10_period,
    label_prefix = label_prefix
  )
}

official_bundle <- build_rate_bundle(dt, pop, weights, "analytic_inclusion_mortality_official", "death_year_for_analysis", "official")
sensitivity_bundle <- build_rate_bundle(dt, pop, weights, "analytic_inclusion_mortality_sensitivity", "death_year_sensitivity", "sensitivity_proxy_fuc")

mort_resolution <- dt %>%
  mutate(sex = dplyr::coalesce(sex_analytic, "unknown")) %>%
  filter(sex %in% c("male", "female"))

proxy_candidates <- mort_resolution %>%
  filter(death_proxy_fuc_candidate, analytic_inclusion_incidence == "yes")

sensitivity_summary <- tibble(
  metric = c(
    "official_deaths_in_rates",
    "resolved_dead_for_analysis",
    "proxy_fuc_deaths_in_sensitivity",
    "dead_signals_without_valid_fecha_muerte",
    "total_death_signals_reviewed"
  ),
  value = c(
    nrow(official_bundle$filtered),
    sum(mort_resolution$death_event_resolved & mort_resolution$analytic_inclusion_incidence == "yes", na.rm = TRUE),
    nrow(sensitivity_bundle$filtered %>% filter(!death_event_official)),
    sum(mort_resolution$vital_status_raw_mapped == "dead" & !mort_resolution$death_event_official, na.rm = TRUE),
    sum(mort_resolution$death_event_combined & mort_resolution$analytic_inclusion_incidence == "yes", na.rm = TRUE)
  )
) %>%
  build_meta("not_applicable_for_sensitivity_counts")

ruleset_comparison <- bind_rows(
  official_bundle$asr_period %>% mutate(ruleset = "official_strict_iarc"),
  sensitivity_bundle$asr_period %>% mutate(ruleset = "sensitivity_dead_with_fuc_proxy")
) %>%
  select(ruleset, everything())

proxy_by_incident_year <- proxy_candidates %>%
  count(incident_year, sex, name = "proxy_deaths_without_fecha_muerte")

proxy_profile <- proxy_candidates %>%
  count(incident_year, sex, age_group_iarc, topography_icdo, name = "proxy_deaths_without_fecha_muerte")

write_csv_utf8(official_bundle$age_specific, project_path(out_dir, "mortality_rates_age_specific.csv"))
write_csv_utf8(official_bundle$crude_annual, project_path(out_dir, "mortality_rates_crude_annual.csv"))
write_csv_utf8(official_bundle$asr_period, project_path(out_dir, "mortality_rates_asr_period.csv"))
write_csv_utf8(official_bundle$top10_period, project_path(out_dir, "mortality_rates_top10_by_sex_period.csv"))
write_csv_utf8(sensitivity_bundle$asr_period, project_path(out_dir, "mortality_rates_sensitivity_asr_period.csv"))
write_csv_utf8(ruleset_comparison, project_path(out_dir, "mortality_rates_official_vs_sensitivity_asr_period.csv"))
write_csv_utf8(proxy_by_incident_year, project_path(out_dir, "mortality_sensitivity_dead_without_date_by_incident_year.csv"))
write_csv_utf8(proxy_profile, project_path(out_dir, "mortality_sensitivity_dead_without_date_profile.csv"))
write_csv_utf8(sensitivity_summary, project_path(out_dir, "mortality_sensitivity_summary.csv"))
saveRDS(
  list(
    official = official_bundle,
    sensitivity = sensitivity_bundle,
    ruleset_comparison = ruleset_comparison
  ),
  project_path(out_dir, "mortality_rates_bundle.rds")
)

qc_rates <- bind_rows(
  tibble(module = "mortality", check_name = "official_annual_deaths_match_analytic_included", check_value = sum(official_bundle$crude_annual$deaths) == nrow(official_bundle$filtered), detail = glue("{sum(official_bundle$crude_annual$deaths)} vs {nrow(official_bundle$filtered)}")),
  tibble(module = "mortality", check_name = "official_population_join_complete", check_value = !any(is.na(official_bundle$age_specific$population)), detail = as.character(sum(is.na(official_bundle$age_specific$population)))),
  tibble(module = "mortality", check_name = "rate_spec_present", check_value = any(rate_spec$analysis_domain == "mortality"), detail = as.character(sum(rate_spec$analysis_domain == "mortality"))),
  tibble(module = "mortality", check_name = "official_deaths_positive", check_value = nrow(official_bundle$filtered) > 0, detail = as.character(nrow(official_bundle$filtered))),
  tibble(module = "mortality", check_name = "proxy_fuc_candidates_detected", check_value = nrow(proxy_candidates) > 0, detail = as.character(nrow(proxy_candidates))),
  tibble(module = "mortality", check_name = "sensitivity_recovers_additional_deaths", check_value = nrow(sensitivity_bundle$filtered) >= nrow(official_bundle$filtered), detail = glue("{nrow(sensitivity_bundle$filtered)} vs {nrow(official_bundle$filtered)}"))
)

existing_qc <- read_csv_if_exists(project_path(qc_dir, "qc_rates_consistency.csv"))
if (!is.null(existing_qc)) {
  existing_qc <- existing_qc %>% filter(module != "mortality")
}
write_csv_utf8(bind_rows(existing_qc, qc_rates), project_path(qc_dir, "qc_rates_consistency.csv"))

cli::cli_alert_success("Tasas de mortalidad construidas con escenario oficial y de sensibilidad.")

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
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

n_years <- length(2015:2022)
meta_iteration <- format(Sys.time(), "%Y%m%d_%H%M%S")

inc <- dt %>%
  filter(analytic_inclusion_incidence == "yes") %>%
  mutate(
    sex = dplyr::coalesce(sex_analytic, "unknown"),
    site_code = dplyr::coalesce(topography_icdo, "unknown"),
    year = incident_year
  ) %>%
  filter(!is.na(year), year %in% 2015:2022, sex %in% c("male", "female"))

age_specific <- inc %>%
  count(year, sex, age_group_iarc, name = "cases") %>%
  full_join(pop %>% filter(sex %in% c("male", "female")) %>% select(year, sex, age_group_iarc, population), by = c("year", "sex", "age_group_iarc")) %>%
  mutate(
    cases = dplyr::coalesce(cases, 0L),
    rate_per_100k = ifelse(population > 0, cases / population * 100000, NA_real_),
    source_id = "rcbpa_incidence_module",
    build_date = as.character(Sys.Date()),
    geographic_scope = "provincia_arequipa",
    denominator_method = "annual_scaled_department_age_structure",
    standard_population = "segi_doll_world_standard",
    iteration_id = meta_iteration
  )

crude_annual <- inc %>%
  count(year, sex, name = "cases") %>%
  left_join(pop %>% filter(sex %in% c("male", "female")) %>% group_by(year, sex) %>% summarise(population = sum(population), .groups = "drop"), by = c("year", "sex")) %>%
  mutate(
    rate_per_100k = cases / population * 100000,
    source_id = "rcbpa_incidence_module",
    build_date = as.character(Sys.Date()),
    geographic_scope = "provincia_arequipa",
    denominator_method = "annual_scaled_department_age_structure",
    standard_population = "segi_doll_world_standard",
    iteration_id = meta_iteration
  )

period_age <- inc %>%
  count(sex, age_group_iarc, name = "cases_period") %>%
  left_join(pop %>% filter(year == 2018, sex %in% c("male", "female")) %>% select(sex, age_group_iarc, population_mid = population), by = c("sex", "age_group_iarc")) %>%
  left_join(weights, by = "age_group_iarc") %>%
  mutate(
    person_years = population_mid * n_years,
    age_specific_rate = ifelse(person_years > 0, cases_period / person_years * 100000, NA_real_)
  )

asr_period <- period_age %>%
  group_by(sex) %>%
  summarise(
    cases_period = sum(cases_period, na.rm = TRUE),
    population_mid = sum(population_mid, na.rm = TRUE),
    asr_per_100k = sum(age_specific_rate * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    crude_period_rate_per_100k = sum(cases_period, na.rm = TRUE) / (first(population_mid) * n_years) * 100000,
    .groups = "drop"
  ) %>%
  mutate(
    source_id = "rcbpa_incidence_module",
    build_date = as.character(Sys.Date()),
    geographic_scope = "provincia_arequipa",
    denominator_method = "mid_period_2018_scaled_department_age_structure",
    standard_population = "segi_doll_world_standard",
    iteration_id = meta_iteration
  )

top10_period <- inc %>%
  count(sex, site_code, name = "cases_period") %>%
  group_by(sex) %>%
  arrange(sex, desc(cases_period), .by_group = TRUE) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  left_join(pop %>% filter(year == 2018, sex %in% c("male", "female")) %>% group_by(sex) %>% summarise(population_mid = sum(population), .groups = "drop"), by = "sex") %>%
  mutate(
    crude_period_rate_per_100k = cases_period / (population_mid * n_years) * 100000,
    source_id = "rcbpa_incidence_module",
    build_date = as.character(Sys.Date()),
    geographic_scope = "provincia_arequipa",
    denominator_method = "mid_period_2018_scaled_department_age_structure",
    standard_population = "segi_doll_world_standard",
    iteration_id = meta_iteration
  )

write_csv_utf8(age_specific, project_path(out_dir, "incidence_rates_age_specific.csv"))
write_csv_utf8(crude_annual, project_path(out_dir, "incidence_rates_crude_annual.csv"))
write_csv_utf8(asr_period, project_path(out_dir, "incidence_rates_asr_period.csv"))
write_csv_utf8(top10_period, project_path(out_dir, "incidence_rates_top10_by_sex_period.csv"))
saveRDS(list(age_specific = age_specific, crude_annual = crude_annual, asr_period = asr_period, top10_period = top10_period), project_path(out_dir, "incidence_rates_bundle.rds"))

qc_rates <- bind_rows(
  tibble(module = "incidence", check_name = "annual_cases_match_analytic_included", check_value = sum(crude_annual$cases) == nrow(inc), detail = glue("{sum(crude_annual$cases)} vs {nrow(inc)}")),
  tibble(module = "incidence", check_name = "population_join_complete", check_value = !any(is.na(age_specific$population)), detail = as.character(sum(is.na(age_specific$population)))),
  tibble(module = "incidence", check_name = "rate_spec_present", check_value = any(rate_spec$analysis_domain == "incidence"), detail = as.character(sum(rate_spec$analysis_domain == "incidence")))
)

existing_qc <- read_csv_if_exists(project_path(qc_dir, "qc_rates_consistency.csv"))
if (!is.null(existing_qc)) {
  existing_qc <- existing_qc %>% filter(module != "incidence")
}
write_csv_utf8(bind_rows(existing_qc, qc_rates), project_path(qc_dir, "qc_rates_consistency.csv"))

cli::cli_alert_success("Tasas de incidencia construidas.")

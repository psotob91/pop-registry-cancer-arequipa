#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(pdftools)
  library(fs)
  library(cli)
  library(glue)
})

source("SCRIPTS/pipeline_runtime_helpers.R")

root <- find_project_root()
setwd(root)

metadata_dir <- project_path(root, "DATA", "DERIVED", "METADATA")
output_dir <- project_path(root, "DATA", "DERIVED", "POPULATION")
qc_dir <- project_path(root, "DATA", "DERIVED", "QC")
raw_shared_dir <- project_path(root, "DATA", "RAW", "SHARED")
ensure_dirs(output_dir, qc_dir)

weights_path <- project_path(metadata_dir, "standard_population_age_weights.csv")
age_rules_path <- project_path(metadata_dir, "MASTER_AGE_GROUP_RULES.csv")
compendio_pdf <- project_path(raw_shared_dir, "Compendio Estadístico, Arequipa 2022.pdf")
dept_pdf <- project_path(raw_shared_dir, "inei_lib1039_population_age_department.pdf")

if (!file_exists(dept_pdf)) {
  cli::cli_alert_info("Descargando PDF oficial INEI de poblacion por edades.")
  download.file(
    "https://proyectos.inei.gob.pe/web/biblioineipub/bancopub/est/lib1039/libro.pdf",
    dept_pdf,
    mode = "wb",
    quiet = TRUE
  )
}

clean_num <- function(x) {
  x %>%
    stringr::str_replace_all("[^0-9]", "") %>%
    na_if("") %>%
    as.numeric()
}

parse_group_line <- function(line, years) {
  line_trim <- stringr::str_trim(line)
  age_label <- stringr::str_extract(line_trim, "^(TOTAL|\\d{1,2}-\\d{1,2}|\\d{1,2}\\s*y\\s*\\+|\\d{1,2}\\+)")
  if (is.na(age_label)) return(NULL)
  line_values <- stringr::str_remove(line_trim, "^(TOTAL|\\d{1,2}-\\d{1,2}|\\d{1,2}\\s*y\\s*\\+|\\d{1,2}\\+)\\s+")
  values <- stringr::str_extract_all(line_values, "\\d{1,3}[ ,]\\d{3}")[[1]]
  values <- values[seq_along(years)]
  if (length(values) != length(years)) return(NULL)
  tibble(
    age_group_iarc = stringr::str_squish(age_label),
    year = years,
    population_department = clean_num(values)
  )
}

extract_lines <- function(text, years) {
  lines <- stringr::str_split(text, "\n")[[1]]
  tibble(line = lines) %>%
    mutate(line_detect = stringr::str_squish(line)) %>%
    filter(stringr::str_detect(line_detect, "^(TOTAL|\\d{1,2}-\\d{1,2}|\\d{1,2}\\s*y\\s*\\+|\\d{1,2}\\+)\\s+\\d")) %>%
    mutate(parsed = purrr::map(line, parse_group_line, years = years)) %>%
    tidyr::unnest(parsed) %>%
    filter(age_group_iarc != "TOTAL") %>%
    select(-line_detect)
}

txt_dept <- pdftools::pdf_text(dept_pdf)
department_age_grouped <- bind_rows(
  extract_lines(paste(txt_dept[60:61], collapse = "\n"), 2005:2015),
  extract_lines(paste(txt_dept[62:63], collapse = "\n"), 2016:2025)
) %>%
  filter(year %in% 2015:2022) %>%
  mutate(age_group_iarc = dplyr::case_when(
    age_group_iarc %in% c("80 y +", "80+") ~ "80plus",
    TRUE ~ age_group_iarc
  )) %>%
  group_by(year, age_group_iarc) %>%
  summarise(population_department = sum(population_department, na.rm = TRUE), .groups = "drop")

department_age <- bind_rows(
  department_age_grouped %>%
    filter(age_group_iarc != "80plus"),
  department_age_grouped %>%
    filter(age_group_iarc == "80plus") %>%
    transmute(
      year,
      age_group_iarc = "80-84",
      population_department = round(population_department * 0.5)
    ),
  department_age_grouped %>%
    filter(age_group_iarc == "80plus") %>%
    transmute(
      year,
      age_group_iarc = "85+",
      population_department = population_department - round(population_department * 0.5)
    )
) %>%
  bind_rows(
    tibble(year = integer(), age_group_iarc = character(), population_department = double())
  ) %>%
  group_by(year) %>%
  mutate(age_prop_department = population_department / sum(population_department, na.rm = TRUE)) %>%
  ungroup()

txt_comp <- pdftools::pdf_text(compendio_pdf)
page31 <- txt_comp[[31]]
province_line <- stringr::str_match(page31, "Arequipa\\s+9,682,0\\s+117,4\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)")
dept_line <- stringr::str_match(page31, "Arequipa\\s+63,344,0\\s+23,1\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)")

province_2014 <- clean_num(province_line[3])
province_2016 <- clean_num(province_line[4])
dept_2016 <- clean_num(dept_line[4])
province_share <- province_2016 / dept_2016

province_total_all <- department_age %>%
  group_by(year) %>%
  summarise(
    department_population_total = sum(population_department, na.rm = TRUE),
    province_population = round(department_population_total * province_share),
    .groups = "drop"
  ) %>%
  mutate(
    source_id = "INEI_department_age_projection_scaled_to_province_with_official_share_2016",
    province_share_projected = province_share
  )

male_share <- 0.494
female_share <- 0.506

denominators <- department_age %>%
  inner_join(province_total_all, by = "year") %>%
  mutate(
    population_both = round(province_population * age_prop_department),
    population_male = round(population_both * male_share),
    population_female = round(population_both * female_share)
  ) %>%
  select(
    year,
    age_group_iarc,
    department_population_total,
    province_population,
    province_share_projected,
    population_both,
    population_male,
    population_female,
    source_id
  ) %>%
  tidyr::pivot_longer(
    cols = c(population_both, population_male, population_female),
    names_to = "sex",
    values_to = "population"
  ) %>%
  mutate(
    sex = dplyr::recode(sex, population_both = "both", population_male = "male", population_female = "female"),
    geographic_scope = "provincia_arequipa",
    denominator_method = "department_age_structure_scaled_with_official_province_share_and_fixed_sex_share",
    standard_population = "segi_doll_world_standard",
    build_date = as.character(Sys.Date()),
    iteration_id = format(Sys.time(), "%Y%m%d_%H%M%S")
  ) %>%
  select(
    year,
    sex,
    age_group_iarc,
    population,
    geographic_scope,
    denominator_method,
    standard_population,
    source_id,
    department_population_total,
    province_population,
    province_share_projected,
    build_date,
    iteration_id
  )

write_csv_utf8(denominators, project_path(output_dir, "arequipa_province_population_denominators.csv"))
saveRDS(denominators, project_path(output_dir, "arequipa_province_population_denominators.rds"))

age_rules <- readr::read_csv(age_rules_path, show_col_types = FALSE) %>% filter(scheme_id == "AGE_001")
weights <- readr::read_csv(weights_path, show_col_types = FALSE)

qc_population <- bind_rows(
  tibble(module = "population_denominators", check_name = "year_coverage_2015_2022", check_value = all(2015:2022 %in% unique(denominators$year)), detail = paste(sort(unique(denominators$year)), collapse = ",")),
  tibble(module = "population_denominators", check_name = "sex_levels", check_value = setequal(unique(denominators$sex), c("both", "male", "female")), detail = paste(sort(unique(denominators$sex)), collapse = ",")),
  tibble(module = "population_denominators", check_name = "age_groups_match_master", check_value = setequal(unique(denominators$age_group_iarc), unique(age_rules$short_label)), detail = paste(sort(unique(denominators$age_group_iarc)), collapse = ",")),
  tibble(module = "population_denominators", check_name = "standard_weights_match_age_groups", check_value = setequal(unique(weights$age_group_iarc), unique(age_rules$short_label)), detail = paste(sort(unique(weights$age_group_iarc)), collapse = ",")),
  tibble(module = "population_denominators", check_name = "province_share_projected", check_value = TRUE, detail = as.character(round(province_share, 6))),
  tibble(module = "population_denominators", check_name = "sex_share_assumption", check_value = TRUE, detail = glue("male={male_share}; female={female_share}"))
) 

write_csv_utf8(qc_population, project_path(qc_dir, "qc_population_denominators.csv"))

cli::cli_alert_success("Denominadores poblacionales construidos.")

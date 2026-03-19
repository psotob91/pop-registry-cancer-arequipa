#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(janitor)
  library(glue)
  library(fs)
  library(cli)
  library(lubridate)
})

# ============================================================
# 04_build_analytic_dataset.R
# Registro Poblacional de Cáncer de Arequipa 2015-2022
# Construye dataset analítico a partir del harmonized_wide y
# maestros tabulares en DATA/DERIVED/METADATA/
# ============================================================

# ------------------------------------------------------------
# 0) Configuración
# ------------------------------------------------------------
CFG <- list(
  project_root = ".",
  metadata_dir = "DATA/DERIVED/METADATA",
  input_candidates = c(
    "DATA/DERIVED/rcpa_arequipa_2015_2022_harmonized_wide.rds",
    "DATA/DERIVED/rcpa_arequipa_2015_2022_harmonized_wide.csv",
    "DATA/DERIVED/rcpa_arequipa_2015_2022_harmonized_wide.parquet",
    "DATA/DERIVED/harmonized/rcpa_arequipa_2015_2022_harmonized_wide.rds",
    "DATA/DERIVED/harmonized/rcpa_arequipa_2015_2022_harmonized_wide.csv",
    "DATA/DERIVED/harmonized/rcpa_arequipa_2015_2022_harmonized_wide.parquet",
    "DATA/DERIVED/HARMONIZED/rcpa_arequipa_2015_2022_harmonized_wide.rds",
    "DATA/DERIVED/HARMONIZED/rcpa_arequipa_2015_2022_harmonized_wide.csv",
    "DATA/DERIVED/HARMONIZED/rcpa_arequipa_2015_2022_harmonized_wide.parquet",
    "DATA/DERIVED/METADATA/../rcpa_arequipa_2015_2022_harmonized_wide.rds",
    "DATA/DERIVED/METADATA/../rcpa_arequipa_2015_2022_harmonized_wide.csv",
    "DATA/DERIVED/METADATA/../rcpa_arequipa_2015_2022_harmonized_wide.parquet"
  ),
  input_search_dirs = c(
    "DATA/DERIVED",
    "DATA/INTERMEDIATE",
    "DATA"
  ),
  input_regex = "(?i)harmonized.*wide.*\\.(rds|csv|parquet)$",
  output_dir = "DATA/DERIVED/ANALYTIC",
  output_basename = "rcpa_arequipa_2015_2022_analytic_dataset",
  denominator_mode = "mid_period", # mid_period | annual
  geographic_scope = "provincia_arequipa",
  age_scheme_incidence = "iarc_quinquennial",
  age_scheme_reporting = "broad_reporting",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 1) Utilidades
# ------------------------------------------------------------
resolve_path <- function(...) {
  fs::path(CFG$project_root, ...)
}

assert_columns <- function(df, required, df_name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Faltan columnas requeridas en {.val {df_name}}.",
      "x" = paste(missing, collapse = ", ")
    ))
  }
}

coalesce_chr <- function(...) {
  vals <- list(...)
  vals <- vals[!purrr::map_lgl(vals, is.null)]
  if (length(vals) == 0) return(rep(NA_character_, 0))
  out <- vals[[1]]
  if (length(vals) == 1) return(out)
  for (i in 2:length(vals)) {
    out <- dplyr::coalesce(out, vals[[i]])
  }
  out
}

normalize_code_chr <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    na_if("")
}

parse_numeric_loose <- function(x) {
  x_chr <- as.character(x)
  x_chr <- stringr::str_replace_all(x_chr, ",", ".")
  readr::parse_number(x_chr, locale = locale(decimal_mark = ".", grouping_mark = ","))
}

clean_icdo_code <- function(x) {
  x_chr <- as.character(x)
  x_chr <- stringr::str_trim(x_chr)
  x_chr[x_chr %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  x_chr <- stringr::str_replace(x_chr, "\\.0$", "")
  x_chr
}

normalize_geo_code <- function(x, width = NULL) {
  x_chr <- as.character(x)
  x_chr <- stringr::str_trim(x_chr)
  x_chr[x_chr %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  x_chr <- stringr::str_replace(x_chr, "\\.0$", "")
  x_chr <- stringr::str_replace_all(x_chr, "[^0-9]", "")
  x_chr[x_chr == ""] <- NA_character_
  if (!is.null(width)) {
    x_chr <- ifelse(!is.na(x_chr), stringr::str_pad(x_chr, width = width, side = "left", pad = "0"), NA_character_)
  }
  x_chr
}

safe_parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x_chr <- as.character(x)
  x_chr <- stringr::str_trim(x_chr)
  x_chr[x_chr %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  
  # Detect Excel serials (numeric-like without separators, reasonable range)
  suppressWarnings({
    num <- as.numeric(x_chr)
  })
  is_serial <- !is.na(num) & num > 20000 & num < 60000
  
  out <- as.Date(NA_real_, origin = "1970-01-01")
  # Parse standard formats
  out1 <- suppressWarnings(readr::parse_date(x_chr, format = "%Y-%m-%d"))
  out2 <- suppressWarnings(readr::parse_date(x_chr, format = "%d/%m/%Y"))
  out3 <- suppressWarnings(readr::parse_date(x_chr, format = "%d-%m-%Y"))
  out4 <- suppressWarnings(readr::parse_date(x_chr, format = "%Y/%m/%d"))
  
  out <- coalesce(out1, out2, out3, out4)
  
  # Apply Excel origin for serials (1899-12-30)
  if (any(is_serial, na.rm = TRUE)) {
    out[is_serial] <- as.Date(num[is_serial], origin = "1899-12-30")
  }
  out
}

read_any_tabular <- function(path) {
  ext <- fs::path_ext(path)
  if (ext == "csv") return(readr::read_csv(path, show_col_types = FALSE))
  if (ext == "rds") return(readRDS(path) %>% as_tibble())
  if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      cli::cli_abort("Para leer parquet se requiere el paquete {.pkg arrow}.")
    }
    return(arrow::read_parquet(path) %>% as_tibble())
  }
  cli::cli_abort(glue("Formato no soportado: {ext}"))
}

first_existing_path <- function(paths) {
  hit <- paths[fs::file_exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

find_project_root <- function(start = getwd(), max_up = 8L) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  
  has_project_markers <- function(path) {
    markers <- c(
      fs::path(path, "DATA", "DERIVED", "METADATA"),
      fs::path(path, ".git")
    )
    
    has_metadata_dir <- fs::dir_exists(markers[[1]])
    has_git_dir <- fs::dir_exists(markers[[2]])
    has_master_spec <- fs::file_exists(fs::path(path, "DATA", "DERIVED", "METADATA", "MASTER_ANALYTIC_DATASET_SPEC.csv"))
    
    isTRUE(has_metadata_dir) || isTRUE(has_git_dir) || isTRUE(has_master_spec)
  }
  
  for (i in 0:max_up) {
    if (has_project_markers(current)) return(current)
    parent <- fs::path_dir(current)
    if (identical(parent, current)) break
    current <- parent
  }
  
  normalizePath(start, winslash = "/", mustWork = TRUE)
}

find_input_path <- function(input_candidates, search_dirs, regex_pattern) {
  hit_direct <- first_existing_path(input_candidates)
  if (!is.na(hit_direct)) {
    return(list(
      path = hit_direct,
      search_table = tibble(method = "direct_candidate", path = input_candidates, exists = fs::file_exists(input_candidates))
    ))
  }
  
  resolved_dirs <- search_dirs[fs::dir_exists(search_dirs)]
  recursive_hits <- purrr::map_dfr(resolved_dirs, function(d) {
    files <- fs::dir_ls(d, recurse = TRUE, type = "file", fail = FALSE)
    tibble(path = files) %>%
      mutate(
        file_name = fs::path_file(path),
        matches = stringr::str_detect(file_name, regex_pattern)
      )
  })
  
  recursive_hits <- recursive_hits %>% filter(matches)
  
  if (nrow(recursive_hits) == 0) {
    return(list(
      path = NA_character_,
      search_table = tibble(method = "direct_candidate", path = input_candidates, exists = fs::file_exists(input_candidates))
    ))
  }
  
  ranked_hits <- recursive_hits %>%
    mutate(
      priority = case_when(
        stringr::str_detect(path, regex("rcpa_arequipa_2015_2022_harmonized_wide\\.rds$", ignore_case = TRUE)) ~ 1L,
        stringr::str_detect(path, regex("rcpa_arequipa_2015_2022_harmonized_wide", ignore_case = TRUE)) ~ 2L,
        stringr::str_detect(path, regex("harmonized_wide", ignore_case = TRUE)) ~ 3L,
        stringr::str_detect(path, regex("harmonized", ignore_case = TRUE)) ~ 4L,
        TRUE ~ 9L
      )
    ) %>%
    arrange(priority, path)
  
  list(
    path = ranked_hits$path[[1]],
    search_table = bind_rows(
      tibble(method = "direct_candidate", path = input_candidates, exists = fs::file_exists(input_candidates)),
      ranked_hits %>% transmute(method = "recursive_search", path, exists = TRUE)
    )
  )
}


apply_value_dictionary <- function(df, value_dict, source_var, out_value, out_group = NULL, out_label = NULL) {
  # Explicit mapping to real columns
  source_map <- c(
    "SEXO" = "sexo",
    "BASE #7" = "base_diagnostico",
    "ESTVIT" = "estado_vital",
    "LATE 19" = "lateralidad"
  )
  
  source_col <- source_map[[source_var]]
  
  if (is.null(source_col) || !source_col %in% names(df)) {
    cli::cli_warn(glue("Variable fuente no encontrada para {source_var}."))
    df[[out_value]] <- NA_character_
    if (!is.null(out_group)) df[[out_group]] <- NA_character_
    if (!is.null(out_label)) df[[out_label]] <- NA_character_
    return(df)
  }
  
  dict_sub <- value_dict %>%
    filter(variable_raw == source_var) %>%
    mutate(code_raw = as.character(code_raw))
  
  df %>%
    mutate(.code_join_tmp = normalize_code_chr(.data[[source_col]])) %>%
    left_join(dict_sub, by = c(".code_join_tmp" = "code_raw")) %>%
    mutate(
      !!out_value := analytic_value,
      !!out_group := analytic_group,
      !!out_label := label_local
    ) %>%
    select(-.code_join_tmp, -analytic_value, -analytic_group, -label_local)
}

build_age_group <- function(age_numeric, rules_df, scheme_name) {
  scheme <- rules_df %>%
    filter(scheme_name == !!scheme_name, status == "active") %>%
    arrange(group_order)
  
  if (nrow(scheme) == 0) {
    cli::cli_abort(glue("No hay reglas activas para esquema de edad: {scheme_name}"))
  }
  
  out <- rep(NA_character_, length(age_numeric))
  for (i in seq_len(nrow(scheme))) {
    idx <- !is.na(age_numeric) & age_numeric >= scheme$age_lower[[i]] & age_numeric <= scheme$age_upper[[i]]
    out[idx] <- scheme$label[[i]]
  }
  out
}

summarise_flags <- function(df, flag_vars) {
  tibble(flag_variable = flag_vars) %>%
    mutate(
      n_true = purrr::map_int(flag_variable, ~ sum(as.logical(df[[.x]]), na.rm = TRUE)),
      n_missing = purrr::map_int(flag_variable, ~ sum(is.na(df[[.x]]))),
      n_total = nrow(df),
      pct_true = if_else(n_total > 0, 100 * n_true / n_total, NA_real_)
    )
}

get_first_present <- function(df, candidates) {
  present <- intersect(candidates, names(df))
  if (length(present) == 0) return(rep(NA, nrow(df)))
  df[[present[[1]]]]
}

get_col_or_na <- function(df, candidates, default = NA_character_) {
  present <- intersect(candidates, names(df))
  if (length(present) == 0) {
    return(rep(default, nrow(df)))
  }
  df[[present[[1]]]]
}

# ------------------------------------------------------------
# 2) Rutas y lectura de maestros
# ------------------------------------------------------------
CFG$project_root <- find_project_root(CFG$project_root)
cli::cli_inform(glue("project_root activo: {CFG$project_root}"))

metadata_dir <- resolve_path(CFG$metadata_dir)
output_dir <- resolve_path(CFG$output_dir)
fs::dir_create(output_dir)

master_paths <- list(
  analytic_spec = fs::path(metadata_dir, "MASTER_ANALYTIC_DATASET_SPEC.csv"),
  variable_decisions = fs::path(metadata_dir, "MASTER_VARIABLE_DECISION_TABLE.csv"),
  denominator_rules = fs::path(metadata_dir, "MASTER_DENOMINATOR_RULES.csv"),
  value_dictionary = fs::path(metadata_dir, "MASTER_VALUE_DICTIONARY.csv"),
  residence_rules = fs::path(metadata_dir, "MASTER_RESIDENCE_RULES.csv"),
  analytic_flags = fs::path(metadata_dir, "MASTER_ANALYTIC_FLAGS_RULES.csv"),
  age_group_rules = fs::path(metadata_dir, "MASTER_AGE_GROUP_RULES.csv")
)

missing_masters <- names(master_paths)[!fs::file_exists(unlist(master_paths))]
if (length(missing_masters) > 0) {
  cli::cli_abort(c(
    "Faltan maestros requeridos en DATA/DERIVED/METADATA.",
    "x" = paste(missing_masters, collapse = ", ")
  ))
}

analytic_spec <- readr::read_csv(master_paths$analytic_spec, show_col_types = FALSE)
variable_decisions <- readr::read_csv(master_paths$variable_decisions, show_col_types = FALSE)
denominator_rules <- readr::read_csv(master_paths$denominator_rules, show_col_types = FALSE)
value_dictionary <- readr::read_csv(master_paths$value_dictionary, show_col_types = FALSE)
residence_rules <- readr::read_csv(master_paths$residence_rules, show_col_types = FALSE)
analytic_flags_rules <- readr::read_csv(master_paths$analytic_flags, show_col_types = FALSE)
age_group_rules <- readr::read_csv(master_paths$age_group_rules, show_col_types = FALSE)

assert_columns(analytic_spec, c("analytic_variable", "source_variable", "transformation_rule"), "MASTER_ANALYTIC_DATASET_SPEC.csv")
assert_columns(denominator_rules, c("rule_id", "analysis_domain", "denominator_mode", "geographic_scope", "status"), "MASTER_DENOMINATOR_RULES.csv")
assert_columns(value_dictionary, c("variable_raw", "code_raw", "analytic_value", "analytic_group"), "MASTER_VALUE_DICTIONARY.csv")
assert_columns(residence_rules, c("rule_id", "analytic_context", "priority_order", "source_variable", "status"), "MASTER_RESIDENCE_RULES.csv")
assert_columns(age_group_rules, c("scheme_name", "age_lower", "age_upper", "label", "status"), "MASTER_AGE_GROUP_RULES.csv")

# ------------------------------------------------------------
# 3) Lectura de base harmonized_wide
# ------------------------------------------------------------
input_candidates <- resolve_path(CFG$input_candidates)
input_search_dirs <- resolve_path(CFG$input_search_dirs)
input_lookup <- find_input_path(
  input_candidates = input_candidates,
  search_dirs = input_search_dirs,
  regex_pattern = CFG$input_regex
)
input_path <- input_lookup$path

if (is.na(input_path)) {
  cli::cli_abort(c(
    "No se encontró la base harmonized_wide ni por ruta directa ni por búsqueda recursiva.",
    "i" = glue("getwd(): {getwd()}"),
    "i" = glue("project_root: {CFG$project_root}"),
    "i" = "Rutas candidatas revisadas:",
    paste(input_candidates, collapse = "
"),
    "i" = "Directorios buscados recursivamente:",
    paste(input_search_dirs, collapse = "
")
  ))
}

cli::cli_inform(glue("Leyendo base harmonized_wide: {input_path}"))
raw_df <- read_any_tabular(input_path) %>% janitor::clean_names()

# ------------------------------------------------------------
# 4) Variables mínimas esperadas
# ------------------------------------------------------------
expected_harmonized_candidates <- c(
  "sexo", "edad", "topografia", "morfologia", "base_7", "estvit",
  "residencia_res", "residencia_deptres", "residencia_provdist",
  "fecdef", "fuc", "fecinc", "late_19"
)

present_expected <- intersect(expected_harmonized_candidates, names(raw_df))
if (length(present_expected) == 0) {
  cli::cli_warn("No se detectaron variables mínimas con los nombres esperados tras clean_names(). Revisa nombres de harmonized_wide.")
}

# ------------------------------------------------------------
# 5) Transformaciones base (CORREGIDO)
# ------------------------------------------------------------
incident_date_raw <- get_col_or_na(raw_df, c("fecha_diagnostico", "fecinc"))
age_raw <- get_col_or_na(raw_df, c("edad"))
topography_raw <- get_col_or_na(raw_df, c("topografia_icdo", "topografia", "topo"))
morphology_raw <- get_col_or_na(raw_df, c("morfologia_icdo", "morfologia", "morf"))
fecha_muerte_raw <- get_col_or_na(raw_df, c("fecha_defuncion", "fecdef"))
fecha_ultimo_contacto_raw <- get_col_or_na(raw_df, c("fecha_ultimo_contacto", "fuc"))

analytic_df <- raw_df %>%
  mutate(
    incident_year = lubridate::year(safe_parse_date(incident_date_raw)),
    age_numeric = parse_numeric_loose(age_raw),
    age_numeric_clean = dplyr::if_else(!is.na(age_numeric) & age_numeric >= 0 & age_numeric <= 110, age_numeric, NA_real_),
    topography_icdo = clean_icdo_code(topography_raw),
    morphology_icdo = clean_icdo_code(morphology_raw),
    fecha_muerte = safe_parse_date(fecha_muerte_raw),
    fecha_ultimo_contacto = safe_parse_date(fecha_ultimo_contacto_raw)
  )

# ------------------------------------------------------------
# 6) Aplicar diccionarios de valores
# ------------------------------------------------------------
analytic_df <- analytic_df %>%
  apply_value_dictionary(value_dictionary, source_var = "SEXO", out_value = "sex_analytic", out_group = "sex_group", out_label = "sex_label_local") %>%
  apply_value_dictionary(value_dictionary, source_var = "BASE #7", out_value = "basis_of_diagnosis_value", out_group = "basis_of_diagnosis_group", out_label = "basis_of_diagnosis_label_local") %>%
  apply_value_dictionary(value_dictionary, source_var = "ESTVIT", out_value = "vital_status_analytic", out_group = "vital_status_group", out_label = "vital_status_label_local") %>%
  apply_value_dictionary(value_dictionary, source_var = "LATE 19", out_value = "laterality_analytic", out_group = "laterality_group", out_label = "laterality_label_local")

# ------------------------------------------------------------
# 7) Derivar grupos de edad
# ------------------------------------------------------------
analytic_df <- analytic_df %>%
  mutate(
    age_group_iarc = build_age_group(age_numeric_clean, age_group_rules, CFG$age_scheme_incidence),
    age_group_broad = build_age_group(age_numeric_clean, age_group_rules, CFG$age_scheme_reporting)
  )

# ------------------------------------------------------------
# 8) Derivación de residencia (UBIGEO)
# ------------------------------------------------------------
res_provdist_raw <- get_col_or_na(analytic_df, c("residencia_provdist", "provdist"))
res_res_raw <- get_col_or_na(analytic_df, c("residencia_res", "res"))
res_dept_raw <- get_col_or_na(analytic_df, c("residencia_deptres", "deptres"))

provdist_eval_vec <- normalize_geo_code(res_provdist_raw, width = 6)
res_eval_vec <- normalize_geo_code(res_res_raw)
dept_eval_vec <- normalize_geo_code(res_dept_raw, width = 2)

residence_analytic_value_vec <- coalesce_chr(provdist_eval_vec, res_eval_vec) %>% na_if("")
residence_department_vec <- dept_eval_vec %>% na_if("")

residence_analytic_source_vec <- dplyr::case_when(
  !is.na(provdist_eval_vec) ~ if ("residencia_provdist" %in% names(analytic_df)) "residencia_provdist" else if ("provdist" %in% names(analytic_df)) "provdist" else NA_character_,
  !is.na(res_eval_vec) ~ if ("residencia_res" %in% names(analytic_df)) "residencia_res" else if ("res" %in% names(analytic_df)) "res" else NA_character_,
  !is.na(dept_eval_vec) ~ if ("residencia_deptres" %in% names(analytic_df)) "residencia_deptres" else if ("deptres" %in% names(analytic_df)) "deptres" else NA_character_,
  TRUE ~ NA_character_
)

residence_analytic_area_vec <- dplyr::case_when(
  !is.na(provdist_eval_vec) & stringr::str_sub(provdist_eval_vec, 1, 4) == "0401" ~ "provincia_arequipa",
  !is.na(provdist_eval_vec) & stringr::str_sub(provdist_eval_vec, 1, 2) == "04" ~ "departamento_arequipa_other_province",
  is.na(provdist_eval_vec) & !is.na(dept_eval_vec) & dept_eval_vec == "04" ~ "departamento_arequipa_unspecified_province",
  is.na(provdist_eval_vec) & is.na(dept_eval_vec) ~ NA_character_,
  TRUE ~ "outside_scope"
)

analytic_inclusion_incidence_vec <- dplyr::case_when(
  residence_analytic_area_vec == "provincia_arequipa" ~ "yes",
  residence_analytic_area_vec %in% c("departamento_arequipa_unspecified_province") ~ "unknown",
  is.na(residence_analytic_area_vec) ~ "unknown",
  TRUE ~ "no"
)

analytic_df <- analytic_df %>%
  mutate(
    residence_analytic_source = residence_analytic_source_vec,
    residence_analytic_value = residence_analytic_value_vec,
    residence_department = residence_department_vec,
    residence_analytic_area = residence_analytic_area_vec,
    analytic_inclusion_incidence = analytic_inclusion_incidence_vec
  )

# ------------------------------------------------------------
# 9) Flags analíticos
# ------------------------------------------------------------
analytic_df <- analytic_df %>%
  mutate(
    topography_icdo_num = suppressWarnings(as.integer(readr::parse_number(topography_icdo))),
    flag_topography_missing = is.na(topography_icdo),
    flag_morphology_missing = is.na(morphology_icdo),
    flag_mv_candidate = basis_of_diagnosis_value %in% c("cytology", "histology_metastasis", "histology_primary"),
    flag_dco_candidate = basis_of_diagnosis_value %in% c("dco"),
    flag_dead_without_death_date = dplyr::coalesce(vital_status_analytic == "dead" & is.na(fecha_muerte), FALSE),
    flag_alive_with_death_date = dplyr::coalesce(vital_status_analytic == "alive" & !is.na(fecha_muerte), FALSE),
    flag_vital_inconsistency = flag_dead_without_death_date |
      flag_alive_with_death_date |
      dplyr::coalesce((!is.na(fecha_muerte) & !is.na(fecha_ultimo_contacto) & fecha_ultimo_contacto < fecha_muerte), FALSE),
    flag_residence_unknown = is.na(residence_analytic_value) & is.na(residence_department),
    flag_incidence_exclusion_nonresident = analytic_inclusion_incidence == "no",
    flag_age_unknown = is.na(age_numeric),
    flag_age_implausible = !is.na(age_numeric) & is.na(age_numeric_clean),
    flag_sex_unknown = is.na(sex_analytic) | sex_analytic == "unknown",
    flag_basis_unknown = is.na(basis_of_diagnosis_value) | basis_of_diagnosis_value == "unknown",
    flag_laterality_unknown = is.na(laterality_analytic) | laterality_analytic == "unknown" | laterality_analytic == "other_or_not_stated",
    flag_psu_missing_topography = is.na(topography_icdo_num),
    flag_psu_topography_candidate = !is.na(topography_icdo_num) & (
      dplyr::between(topography_icdo_num, 760L, 769L) |
        dplyr::between(topography_icdo_num, 770L, 779L) |
        topography_icdo_num == 809L
    ),
    flag_psu_candidate = flag_psu_missing_topography | flag_psu_topography_candidate
  )

# ------------------------------------------------------------
# 10) Regla de denominadores activa
# ------------------------------------------------------------
active_denominator_rule <- denominator_rules %>%
  filter(
    status == "active",
    denominator_mode == CFG$denominator_mode,
    geographic_scope == CFG$geographic_scope,
    use_for_primary_analysis == "yes",
    analysis_domain == "incidence"
  ) %>%
  arrange(implementation_priority) %>%
  slice(1)

if (nrow(active_denominator_rule) == 0) {
  cli::cli_warn(glue("No se encontró regla activa para denominator_mode={CFG$denominator_mode} y geographic_scope={CFG$geographic_scope}."))
  denominator_rule_export <- tibble()
} else {
  denominator_rule_export <- active_denominator_rule
  analytic_df <- analytic_df %>%
    mutate(
      denominator_mode_active = active_denominator_rule$denominator_mode[[1]],
      denominator_scope_active = active_denominator_rule$geographic_scope[[1]],
      denominator_reference_year = active_denominator_rule$reference_year[[1]],
      denominator_mid_period_year = active_denominator_rule$mid_period_year[[1]],
      standard_population_active = active_denominator_rule$standard_population[[1]]
    )
}

# ------------------------------------------------------------
# 11) Selección y orden final de variables
# ------------------------------------------------------------
preferred_order <- c(
  "incident_year",
  "sex_analytic",
  "age_numeric",
  "age_numeric_clean",
  "age_group_iarc",
  "age_group_broad",
  "topography_icdo",
  "morphology_icdo",
  "topography_icdo_num",
  "basis_of_diagnosis_value",
  "basis_of_diagnosis_group",
  "basis_of_diagnosis_label_local",
  "vital_status_analytic",
  "laterality_analytic",
  "residence_analytic_source",
  "residence_analytic_value",
  "residence_department",
  "residence_analytic_area",
  "analytic_inclusion_incidence",
  "denominator_mode_active",
  "denominator_scope_active",
  "denominator_reference_year",
  "denominator_mid_period_year",
  "standard_population_active",
  "flag_topography_missing",
  "flag_morphology_missing",
  "flag_mv_candidate",
  "flag_dco_candidate",
  "flag_dead_without_death_date",
  "flag_alive_with_death_date",
  "flag_vital_inconsistency",
  "flag_residence_unknown",
  "flag_incidence_exclusion_nonresident",
  "flag_age_unknown",
  "flag_age_implausible",
  "flag_sex_unknown",
  "flag_basis_unknown",
  "flag_laterality_unknown",
  "flag_psu_missing_topography",
  "flag_psu_topography_candidate",
  "flag_psu_candidate"
)

final_order <- unique(c(preferred_order, names(analytic_df)))
analytic_df <- analytic_df %>% select(any_of(final_order))

# ------------------------------------------------------------
# 12) Exportar
# ------------------------------------------------------------
out_csv <- fs::path(output_dir, glue("{CFG$output_basename}.csv"))
out_rds <- fs::path(output_dir, glue("{CFG$output_basename}.rds"))
out_flags_csv <- fs::path(output_dir, glue("{CFG$output_basename}_flag_summary.csv"))
out_rule_csv <- fs::path(output_dir, glue("{CFG$output_basename}_active_denominator_rule.csv"))
out_log_txt <- fs::path(output_dir, glue("{CFG$output_basename}_build_log.txt"))

if (!CFG$overwrite && (fs::file_exists(out_csv) || fs::file_exists(out_rds))) {
  cli::cli_abort(glue("El archivo ya existe y overwrite=FALSE: {out_csv} / {out_rds}"))
}

readr::write_csv(analytic_df, out_csv, na = "")
saveRDS(analytic_df, out_rds)
flag_summary <- summarise_flags(
  analytic_df,
  c(
    "flag_topography_missing", "flag_morphology_missing",
    "flag_mv_candidate", "flag_dco_candidate",
    "flag_dead_without_death_date", "flag_alive_with_death_date", "flag_vital_inconsistency",
    "flag_residence_unknown", "flag_incidence_exclusion_nonresident",
    "flag_age_unknown", "flag_age_implausible", "flag_sex_unknown", "flag_basis_unknown",
    "flag_laterality_unknown",
    "flag_psu_missing_topography", "flag_psu_topography_candidate", "flag_psu_candidate"
  )
)
readr::write_csv(flag_summary, out_flags_csv, na = "")
readr::write_csv(denominator_rule_export, out_rule_csv, na = "")

log_lines <- c(
  "============================================",
  "ANALYTIC DATASET BUILD COMPLETADO",
  "============================================",
  glue("input_path: {input_path}"),
  glue("output_csv: {out_csv}"),
  glue("output_rds: {out_rds}"),
  glue("n_rows: {nrow(analytic_df)}"),
  glue("n_cols: {ncol(analytic_df)}"),
  glue("denominator_mode: {CFG$denominator_mode}"),
  glue("geographic_scope: {CFG$geographic_scope}"),
  glue("mid_period_year: {if (nrow(denominator_rule_export) > 0) denominator_rule_export$mid_period_year[[1]] else NA_character_}"),
  glue("created_at: {Sys.time()}")
)
writeLines(log_lines, out_log_txt)

cli::cli_alert_success("Dataset analítico construido correctamente.")
cli::cli_inform(glue("Archivo principal CSV: {out_csv}"))
cli::cli_inform(glue("Archivo principal RDS: {out_rds}"))
cli::cli_inform(glue("Resumen de flags: {out_flags_csv}"))
cli::cli_inform(glue("Regla de denominador activa: {out_rule_csv}"))

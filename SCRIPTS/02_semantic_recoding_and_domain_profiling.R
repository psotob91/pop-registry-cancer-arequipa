suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(glue)
  library(here)
  library(jsonlite)
  library(lubridate)
})

# ============================================================
# 02_semantic_recoding_and_domain_profiling.R
# Fase 2: recodificación semántica inicial + perfilado de dominios
# RCBPA Arequipa 2015-2022
# ------------------------------------------------------------
# PRINCIPIOS
# - downstream de la auditoría estructural y de la armonización inicial
# - NO modifica scripts previos ni sobrescribe outputs oficiales previos
# - usa los insumos oficiales ya validados como fuentes vinculantes
# - no impone semántica final sin evidencia documental o empírica
# - toda propuesta de recodificación queda trazada y exportada
# - prepara insumos para QC epidemiológico/IARC posterior
# ============================================================

# ------------------------------------------------------------
# 0) Setup de rutas
# ------------------------------------------------------------
root <- here::here()

dir.create(file.path(root, "REPORTS"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "REPORTS", "SEMANTIC"), recursive = TRUE, showWarnings = FALSE)

# Inputs oficiales mínimos
f_harmonized_wide             <- file.path(root, "DATA", "DERIVED", "rcpa_arequipa_2015_2022_harmonized_wide.rds")
f_harmonized_long             <- file.path(root, "DATA", "DERIVED", "rcpa_arequipa_2015_2022_harmonized_long.rds")
f_harmonization_dictionary    <- file.path(root, "REPORTS", "harmonization_dictionary.csv")
f_harmonization_pending       <- file.path(root, "REPORTS", "harmonization_pending_semantic_review.csv")
f_harmonization_action_log    <- file.path(root, "REPORTS", "harmonization_action_log.csv")
f_quality_field_availability  <- file.path(root, "REPORTS", "quality_indicator_field_availability.csv")
f_data_audit_log              <- file.path(root, "REPORTS", "data_audit_log.json")
f_data_dictionary_crosswalk   <- file.path(root, "REPORTS", "data_dictionary_crosswalk.csv")
f_variable_quality_profile    <- file.path(root, "REPORTS", "variable_quality_profile.csv")
f_master_variable_map_derived <- file.path(root, "DATA", "DERIVED", "METADATA", "MASTER_VARIABLE_MAP.csv")
f_master_variable_map_reports <- file.path(root, "REPORTS", "METADATA", "MASTER_VARIABLE_MAP.csv")
f_master_dictionary_derived   <- file.path(root, "DATA", "DERIVED", "METADATA", "MASTER_DATA_DICTIONARY.csv")
f_master_dictionary_reports   <- file.path(root, "REPORTS", "METADATA", "MASTER_DATA_DICTIONARY.csv")

# Outputs de esta fase
out_domain_profile            <- file.path(root, "REPORTS", "domain_profile_priority_variables.csv")
out_domain_profile_by_year    <- file.path(root, "REPORTS", "domain_profile_priority_variables_by_year.csv")
out_recode_candidates         <- file.path(root, "REPORTS", "semantic_recode_candidates.csv")
out_pending_manual_review     <- file.path(root, "REPORTS", "semantic_recode_pending_manual_review.csv")
out_freq_base                 <- file.path(root, "REPORTS", "value_frequency_base_diagnostico.csv")
out_freq_estado               <- file.path(root, "REPORTS", "value_frequency_estado_vital.csv")
out_freq_lateralidad          <- file.path(root, "REPORTS", "value_frequency_lateralidad.csv")
out_freq_sexo                 <- file.path(root, "REPORTS", "value_frequency_sexo.csv")
out_consistency_checks        <- file.path(root, "REPORTS", "semantic_consistency_checks.csv")
out_date_profile              <- file.path(root, "REPORTS", "date_field_format_profile.csv")
out_top_problem_values        <- file.path(root, "REPORTS", "semantic_problematic_values.csv")
out_summary_md                <- file.path(root, "REPORTS", "semantic_phase_summary.md")
out_run_metadata              <- file.path(root, "REPORTS", "semantic_phase_run_metadata.json")

run_date <- as.character(Sys.Date())

# ------------------------------------------------------------
# 1) Helpers
# ------------------------------------------------------------
read_csv_safe <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

pick_first_existing <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) return(NA_character_)
  existing[[1]]
}

stop_if_missing <- function(paths, label = "required files") {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop(
      paste0(
        "Missing ", label, ":\n",
        paste(missing, collapse = "\n")
      ),
      call. = FALSE
    )
  }
}

na_blank <- function(x) {
  x_chr <- as.character(x)
  x_chr <- stringr::str_trim(x_chr)
  x_chr[x_chr == ""] <- NA_character_
  x_chr
}

safe_unique_collapse <- function(x, sep = " | ") {
  x2 <- x %>%
    as.character() %>%
    stringr::str_trim()
  x2[x2 == ""] <- NA_character_
  x2 <- unique(stats::na.omit(x2))
  if (length(x2) == 0) return(NA_character_)
  paste(sort(x2), collapse = sep)
}

safe_mode_chr <- function(x) {
  x2 <- na_blank(x)
  x2 <- stats::na.omit(x2)
  if (length(x2) == 0) return(NA_character_)
  names(sort(table(x2), decreasing = TRUE))[1]
}

normalize_token <- function(x) {
  x %>%
    na_blank() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

normalize_compact <- function(x) {
  x %>%
    na_blank() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("\\s+", "")
}

classify_unknown_token <- function(x) {
  token <- normalize_compact(x)
  dplyr::case_when(
    is.na(token) ~ FALSE,
    token %in% c("NA", "N/A", "ND", "NI", "NE", "NK", "NR", "NS", "SD", "SIN DATO", "SIN DATO.") ~ TRUE,
    token %in% c("99", "999", "9999", "00", "000", "0000", "88", "888", "77", "777") ~ TRUE,
    token %in% c("DESCONOCIDO", "IGNORADO", "SININFORMACION", "SININFO", "NOINFORMA") ~ TRUE,
    TRUE ~ FALSE
  )
}

classify_date_format <- function(x) {
  x2 <- na_blank(x)
  dplyr::case_when(
    is.na(x2) ~ NA_character_,
    stringr::str_detect(x2, "^\\d{1,2}/\\d{1,2}/\\d{2,4}$") ~ "dmy_slash",
    stringr::str_detect(x2, "^\\d{1,2}-\\d{1,2}-\\d{2,4}$") ~ "dmy_dash",
    stringr::str_detect(x2, "^\\d{4}-\\d{1,2}-\\d{1,2}$") ~ "ymd_dash",
    stringr::str_detect(x2, "^\\d{4}/\\d{1,2}/\\d{1,2}$") ~ "ymd_slash",
    stringr::str_detect(x2, "^\\d{5}$") ~ "excel_serial_5",
    stringr::str_detect(x2, "^\\d{4}$") ~ "year_only",
    TRUE ~ "other_nonstandard"
  )
}

parse_mixed_date <- function(x) {
  x2 <- na_blank(x)
  out <- rep(as.Date(NA), length(x2))
  
  idx_dmy_slash <- !is.na(x2) & stringr::str_detect(x2, "^\\d{1,2}/\\d{1,2}/\\d{2,4}$")
  idx_dmy_dash  <- !is.na(x2) & stringr::str_detect(x2, "^\\d{1,2}-\\d{1,2}-\\d{2,4}$")
  idx_ymd_dash  <- !is.na(x2) & stringr::str_detect(x2, "^\\d{4}-\\d{1,2}-\\d{1,2}$")
  idx_ymd_slash <- !is.na(x2) & stringr::str_detect(x2, "^\\d{4}/\\d{1,2}/\\d{1,2}$")
  idx_excel     <- !is.na(x2) & stringr::str_detect(x2, "^\\d{5}$")
  
  if (any(idx_dmy_slash)) out[idx_dmy_slash] <- suppressWarnings(lubridate::dmy(x2[idx_dmy_slash]))
  if (any(idx_dmy_dash))  out[idx_dmy_dash]  <- suppressWarnings(lubridate::dmy(x2[idx_dmy_dash]))
  if (any(idx_ymd_dash))  out[idx_ymd_dash]  <- suppressWarnings(lubridate::ymd(x2[idx_ymd_dash]))
  if (any(idx_ymd_slash)) out[idx_ymd_slash] <- suppressWarnings(lubridate::ymd(stringr::str_replace_all(x2[idx_ymd_slash], "/", "-")))
  if (any(idx_excel))     out[idx_excel]     <- as.Date(as.numeric(x2[idx_excel]), origin = "1899-12-30")
  
  out
}

classify_value_problem <- function(x) {
  token <- na_blank(x)
  compact <- normalize_compact(token)
  
  dplyr::case_when(
    is.na(token) ~ "missing",
    classify_unknown_token(token) ~ "possible_unknown_code",
    stringr::str_detect(token, "^\\s+$") ~ "blank_space_only",
    stringr::str_detect(token, "^\\.+$") ~ "dots_only",
    stringr::str_detect(token, "^[\\-_/]+$") ~ "separator_only",
    TRUE ~ "none"
  )
}

age_to_numeric <- function(x) {
  x2 <- na_blank(x)
  x2 <- stringr::str_replace_all(x2, ",", ".")
  suppressWarnings(as.numeric(x2))
}

basic_age_issue <- function(x) {
  age_num <- age_to_numeric(x)
  dplyr::case_when(
    is.na(x) ~ "missing",
    classify_unknown_token(x) ~ "possible_unknown_code",
    is.na(age_num) ~ "non_numeric_or_mixed",
    age_num < 0 ~ "negative_age",
    age_num > 120 ~ "age_gt_120",
    TRUE ~ "none"
  )
}

build_value_frequency <- function(data, var_name) {
  data %>%
    transmute(
      source_year,
      variable_harmonized,
      value_raw = value_raw,
      value_norm = normalize_token(value_raw),
      is_missing = is.na(na_blank(value_raw)),
      is_possible_unknown = classify_unknown_token(value_raw),
      problem_class = classify_value_problem(value_raw)
    ) %>%
    filter(variable_harmonized == var_name) %>%
    count(source_year, value_raw, value_norm, is_missing, is_possible_unknown, problem_class, sort = TRUE, name = "n")
}

infer_semantic_candidate <- function(var_name, value_norm) {
  dplyr::case_when(
    var_name == "sexo" & value_norm %in% c("M", "1", "H", "HOMBRE", "MASCULINO", "MALE") ~ "male_candidate",
    var_name == "sexo" & value_norm %in% c("F", "2", "MUJER", "FEMENINO", "FEMALE") ~ "female_candidate",
    var_name == "sexo" & classify_unknown_token(value_norm) ~ "unknown_candidate",
    
    var_name == "estado_vital" & value_norm %in% c("1", "VIVO", "ALIVE", "A") ~ "alive_candidate",
    var_name == "estado_vital" & value_norm %in% c("2", "FALLECIDO", "MUERTO", "DEAD", "D") ~ "dead_candidate",
    var_name == "estado_vital" & classify_unknown_token(value_norm) ~ "unknown_candidate",
    
    var_name == "lateralidad" & value_norm %in% c("1", "D", "DER", "DERECHA", "RIGHT") ~ "right_candidate",
    var_name == "lateralidad" & value_norm %in% c("2", "I", "IZQ", "IZQUIERDA", "LEFT") ~ "left_candidate",
    var_name == "lateralidad" & value_norm %in% c("3", "B", "BILATERAL", "BOTH") ~ "bilateral_candidate",
    var_name == "lateralidad" & value_norm %in% c("4", "9", "NO APLICA", "NOT APPLICABLE", "MIDLINE") ~ "not_applicable_or_other_candidate",
    var_name == "lateralidad" & classify_unknown_token(value_norm) ~ "unknown_candidate",
    
    var_name == "base_diagnostico" & classify_unknown_token(value_norm) ~ "unknown_candidate",
    TRUE ~ NA_character_
  )
}

infer_recode_rationale <- function(var_name, value_norm, semantic_candidate) {
  dplyr::case_when(
    var_name == "sexo" & !is.na(semantic_candidate) ~ "Patrón empírico compatible con codificación usual de sexo; requiere validación local final.",
    var_name == "estado_vital" & !is.na(semantic_candidate) ~ "Patrón empírico compatible con estado vital; debe contrastarse con FECDEF y FUC.",
    var_name == "lateralidad" & !is.na(semantic_candidate) ~ "Patrón empírico preliminar compatible con lateralidad; confirmar con reglas del registro.",
    var_name == "base_diagnostico" & !is.na(semantic_candidate) ~ "Solo se identifica candidato de desconocido; la semántica completa debe basarse en diccionario local de BASE #7.",
    TRUE ~ "Sin evidencia suficiente para propuesta automática robusta."
  )
}

# ------------------------------------------------------------
# 2) Verificación de insumos oficiales
# ------------------------------------------------------------
required_files <- c(
  f_harmonized_wide,
  f_harmonization_dictionary,
  f_harmonization_pending,
  f_harmonization_action_log,
  f_quality_field_availability,
  f_data_audit_log,
  f_data_dictionary_crosswalk,
  f_variable_quality_profile
)

stop_if_missing(required_files)

master_variable_map_path <- pick_first_existing(c(f_master_variable_map_derived, f_master_variable_map_reports))
master_dictionary_path   <- pick_first_existing(c(f_master_dictionary_derived, f_master_dictionary_reports))

# ------------------------------------------------------------
# 3) Lectura de insumos oficiales
# ------------------------------------------------------------
harmonized_wide <- readRDS(f_harmonized_wide)
has_harmonized_long <- file.exists(f_harmonized_long)
harmonized_long <- if (has_harmonized_long) readRDS(f_harmonized_long) else tibble()

harmonization_dictionary <- read_csv_safe(f_harmonization_dictionary)
harmonization_pending    <- read_csv_safe(f_harmonization_pending)
harmonization_action_log <- read_csv_safe(f_harmonization_action_log)
quality_field_availability <- read_csv_safe(f_quality_field_availability)
data_dictionary_crosswalk  <- read_csv_safe(f_data_dictionary_crosswalk)
variable_quality_profile   <- read_csv_safe(f_variable_quality_profile)
audit_log                  <- jsonlite::read_json(f_data_audit_log, simplifyVector = TRUE)
master_variable_map        <- if (!is.na(master_variable_map_path)) read_csv_safe(master_variable_map_path) else tibble()
master_data_dictionary     <- if (!is.na(master_dictionary_path)) read_csv_safe(master_dictionary_path) else tibble()

# ------------------------------------------------------------
# 4) Variables prioritarias de esta fase
# ------------------------------------------------------------
priority_variables <- c(
  "sexo",
  "edad",
  "base_diagnostico",
  "estado_vital",
  "lateralidad",
  "residencia__res",
  "residencia__deptres",
  "residencia__provdist",
  "multiple_primary__pmseq",
  "multiple_primary__pmtot",
  "multiple_primary__pmcod",
  "causa",
  "topografia_icdo",
  "morfologia_icdo",
  "comportamiento",
  "grado",
  "fecha_diagnostico",
  "fecha_muerte",
  "fecha_ultimo_contacto"
)

available_priority_variables <- intersect(priority_variables, names(harmonized_wide))
missing_priority_variables   <- setdiff(priority_variables, names(harmonized_wide))

# ------------------------------------------------------------
# 5) Base de trabajo y trazabilidad mínima
# ------------------------------------------------------------
working_wide <- harmonized_wide %>%
  mutate(
    source_year = as.character(source_year),
    source_sheet = as.character(source_sheet)
  )

for (nm in available_priority_variables) {
  working_wide[[nm]] <- na_blank(working_wide[[nm]])
}

working_long <- working_wide %>%
  select(any_of(c("row_id", "source_sheet", "source_year", available_priority_variables))) %>%
  pivot_longer(
    cols = all_of(available_priority_variables),
    names_to = "variable_harmonized",
    values_to = "value_raw"
  ) %>%
  mutate(
    value_raw = na_blank(value_raw),
    value_norm = normalize_token(value_raw),
    is_missing = is.na(value_raw),
    is_possible_unknown = classify_unknown_token(value_raw),
    problem_class = classify_value_problem(value_raw)
  )

# ------------------------------------------------------------
# 6) Perfilado global de dominios
# ------------------------------------------------------------
domain_profile_priority_variables <- working_long %>%
  group_by(variable_harmonized) %>%
  summarise(
    n_rows_total = n(),
    n_observed = sum(!is_missing),
    n_missing = sum(is_missing),
    pct_missing = round(100 * n_missing / n_rows_total, 4),
    n_distinct_non_missing = n_distinct(value_raw[!is_missing]),
    n_possible_unknown = sum(is_possible_unknown, na.rm = TRUE),
    pct_possible_unknown = round(100 * n_possible_unknown / pmax(n_observed, 1), 4),
    n_problem_values = sum(problem_class != "none" & problem_class != "missing", na.rm = TRUE),
    top_1 = names(sort(table(value_raw[!is_missing]), decreasing = TRUE))[1] %||% NA_character_,
    top_2 = names(sort(table(value_raw[!is_missing]), decreasing = TRUE))[2] %||% NA_character_,
    top_3 = names(sort(table(value_raw[!is_missing]), decreasing = TRUE))[3] %||% NA_character_,
    top_4 = names(sort(table(value_raw[!is_missing]), decreasing = TRUE))[4] %||% NA_character_,
    top_5 = names(sort(table(value_raw[!is_missing]), decreasing = TRUE))[5] %||% NA_character_,
    top_1_n = unname(sort(table(value_raw[!is_missing]), decreasing = TRUE)[1]) %||% NA_integer_,
    top_2_n = unname(sort(table(value_raw[!is_missing]), decreasing = TRUE)[2]) %||% NA_integer_,
    top_3_n = unname(sort(table(value_raw[!is_missing]), decreasing = TRUE)[3]) %||% NA_integer_,
    inferred_type_mix = case_when(
      variable_harmonized == "edad" ~ safe_unique_collapse(unique(basic_age_issue(value_raw))),
      variable_harmonized %in% c("fecha_diagnostico", "fecha_muerte", "fecha_ultimo_contacto") ~ safe_unique_collapse(unique(classify_date_format(value_raw))),
      TRUE ~ safe_unique_collapse(unique(problem_class))
    ),
    notes = case_when(
      variable_harmonized == "base_diagnostico" ~ "Perfilar BASE #7 primero; no decodificar definitivamente sin evidencia.",
      variable_harmonized == "estado_vital" ~ "Cruzar con FECDEF y FUC antes de proponer diccionario final.",
      variable_harmonized == "lateralidad" ~ "LATE 19 requiere confirmación empírica y documental.",
      stringr::str_detect(variable_harmonized, "^residencia__") ~ "Mantener familia de residencia separada.",
      stringr::str_detect(variable_harmonized, "^multiple_primary__") ~ "Mantener familia de múltiples primarios separada.",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  arrange(match(variable_harmonized, priority_variables))

write_csv(domain_profile_priority_variables, out_domain_profile, na = "")

# ------------------------------------------------------------
# 7) Perfilado por año
# ------------------------------------------------------------
domain_profile_priority_variables_by_year <- working_long %>%
  group_by(source_year, variable_harmonized) %>%
  summarise(
    n_rows_total = n(),
    n_observed = sum(!is_missing),
    n_missing = sum(is_missing),
    pct_missing = round(100 * n_missing / n_rows_total, 4),
    n_distinct_non_missing = n_distinct(value_raw[!is_missing]),
    n_possible_unknown = sum(is_possible_unknown, na.rm = TRUE),
    pct_possible_unknown = round(100 * n_possible_unknown / pmax(n_observed, 1), 4),
    top_1 = names(sort(table(value_raw[!is_missing]), decreasing = TRUE))[1] %||% NA_character_,
    top_2 = names(sort(table(value_raw[!is_missing]), decreasing = TRUE))[2] %||% NA_character_,
    top_3 = names(sort(table(value_raw[!is_missing]), decreasing = TRUE))[3] %||% NA_character_,
    top_1_n = unname(sort(table(value_raw[!is_missing]), decreasing = TRUE)[1]) %||% NA_integer_,
    top_2_n = unname(sort(table(value_raw[!is_missing]), decreasing = TRUE)[2]) %||% NA_integer_,
    top_3_n = unname(sort(table(value_raw[!is_missing]), decreasing = TRUE)[3]) %||% NA_integer_,
    .groups = "drop"
  ) %>%
  arrange(source_year, match(variable_harmonized, priority_variables))

write_csv(domain_profile_priority_variables_by_year, out_domain_profile_by_year, na = "")

# ------------------------------------------------------------
# 8) Frecuencias específicas para variables candidatas de recodificación
# ------------------------------------------------------------
value_frequency_base_diagnostico <- build_value_frequency(working_long, "base_diagnostico")
value_frequency_estado_vital     <- build_value_frequency(working_long, "estado_vital")
value_frequency_lateralidad      <- build_value_frequency(working_long, "lateralidad")
value_frequency_sexo             <- build_value_frequency(working_long, "sexo")

write_csv(value_frequency_base_diagnostico, out_freq_base, na = "")
write_csv(value_frequency_estado_vital, out_freq_estado, na = "")
write_csv(value_frequency_lateralidad, out_freq_lateralidad, na = "")
write_csv(value_frequency_sexo, out_freq_sexo, na = "")

# ------------------------------------------------------------
# 9) Candidatos preliminares de recodificación semántica
# ------------------------------------------------------------
recode_candidate_vars <- c("base_diagnostico", "estado_vital", "lateralidad", "sexo")

semantic_recode_candidates <- working_long %>%
  filter(variable_harmonized %in% recode_candidate_vars, !is_missing) %>%
  distinct(variable_harmonized, value_raw, value_norm) %>%
  left_join(
    working_long %>%
      filter(variable_harmonized %in% recode_candidate_vars, !is_missing) %>%
      count(variable_harmonized, value_raw, value_norm, name = "n_observed"),
    by = c("variable_harmonized", "value_raw", "value_norm")
  ) %>%
  left_join(
    working_long %>%
      filter(variable_harmonized %in% recode_candidate_vars, !is_missing) %>%
      group_by(variable_harmonized, value_raw, value_norm) %>%
      summarise(years_observed = safe_unique_collapse(source_year), .groups = "drop"),
    by = c("variable_harmonized", "value_raw", "value_norm")
  ) %>%
  mutate(
    semantic_candidate = infer_semantic_candidate(variable_harmonized, value_norm),
    proposed_standard_label = dplyr::case_when(
      semantic_candidate == "male_candidate" ~ "male",
      semantic_candidate == "female_candidate" ~ "female",
      semantic_candidate == "alive_candidate" ~ "alive",
      semantic_candidate == "dead_candidate" ~ "dead",
      semantic_candidate == "right_candidate" ~ "right",
      semantic_candidate == "left_candidate" ~ "left",
      semantic_candidate == "bilateral_candidate" ~ "bilateral",
      semantic_candidate == "not_applicable_or_other_candidate" ~ "not_applicable_or_other",
      semantic_candidate == "unknown_candidate" ~ "unknown",
      TRUE ~ NA_character_
    ),
    automation_status = dplyr::case_when(
      variable_harmonized == "base_diagnostico" ~ "manual_review_required",
      !is.na(semantic_candidate) ~ "candidate_for_auto_recode_after_validation",
      TRUE ~ "manual_review_required"
    ),
    evidence_basis = dplyr::case_when(
      variable_harmonized == "base_diagnostico" ~ "empirical_only_insufficient_for_full_decoding",
      !is.na(semantic_candidate) ~ "empirical_pattern",
      TRUE ~ "insufficient"
    ),
    rationale = infer_recode_rationale(variable_harmonized, value_norm, semantic_candidate),
    reversible_rule = dplyr::case_when(
      !is.na(proposed_standard_label) ~ paste0("raw='", value_raw, "' -> proposed='", proposed_standard_label, "'"),
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(variable_harmonized, desc(n_observed), value_norm)

write_csv(semantic_recode_candidates, out_recode_candidates, na = "")

# ------------------------------------------------------------
# 10) Perfil específico de formatos de fecha
# ------------------------------------------------------------
date_field_format_profile <- working_long %>%
  filter(variable_harmonized %in% c("fecha_diagnostico", "fecha_muerte", "fecha_ultimo_contacto")) %>%
  mutate(date_format_class = classify_date_format(value_raw)) %>%
  count(variable_harmonized, source_year, date_format_class, sort = TRUE, name = "n") %>%
  arrange(variable_harmonized, source_year, desc(n))

write_csv(date_field_format_profile, out_date_profile, na = "")

# ------------------------------------------------------------
# 11) Chequeos de consistencia semántica básicos
# ------------------------------------------------------------
working_dates <- working_wide %>%
  transmute(
    row_id,
    source_year,
    estado_vital = na_blank(estado_vital),
    fecha_diagnostico_raw = na_blank(fecha_diagnostico),
    fecha_muerte_raw = na_blank(fecha_muerte),
    fecha_ultimo_contacto_raw = na_blank(fecha_ultimo_contacto),
    edad_raw = na_blank(edad),
    topografia_icdo = na_blank(topografia_icdo),
    morfologia_icdo = na_blank(morfologia_icdo),
    comportamiento = na_blank(comportamiento)
  ) %>%
  mutate(
    fecha_diagnostico_date = parse_mixed_date(fecha_diagnostico_raw),
    fecha_muerte_date = parse_mixed_date(fecha_muerte_raw),
    fecha_ultimo_contacto_date = parse_mixed_date(fecha_ultimo_contacto_raw),
    edad_num = age_to_numeric(edad_raw),
    estado_vital_norm = normalize_token(estado_vital),
    topografia_norm = normalize_compact(topografia_icdo),
    morfologia_norm = normalize_compact(morfologia_icdo),
    comportamiento_norm = normalize_compact(comportamiento)
  )

check_estado_vital_vs_fecha_muerte <- working_dates %>%
  mutate(
    check_name = "estado_vital_vs_fecha_muerte",
    check_result = dplyr::case_when(
      is.na(estado_vital) & is.na(fecha_muerte_raw) ~ "both_missing",
      !is.na(fecha_muerte_raw) & is.na(estado_vital) ~ "date_present_status_missing",
      !is.na(fecha_muerte_raw) & !is.na(estado_vital) ~ "review_cross_signal",
      is.na(fecha_muerte_raw) & !is.na(estado_vital) ~ "status_present_date_missing_or_not_applicable",
      TRUE ~ "other"
    )
  ) %>%
  count(check_name, check_result, name = "n")

check_fecha_muerte_vs_fecha_ultimo_contacto <- working_dates %>%
  mutate(
    check_name = "fecha_muerte_vs_fecha_ultimo_contacto",
    check_result = dplyr::case_when(
      is.na(fecha_muerte_date) | is.na(fecha_ultimo_contacto_date) ~ "insufficient_non_missing_dates",
      fecha_ultimo_contacto_date < fecha_muerte_date ~ "last_contact_before_death",
      fecha_ultimo_contacto_date == fecha_muerte_date ~ "same_day",
      fecha_ultimo_contacto_date > fecha_muerte_date ~ "last_contact_after_death_review",
      TRUE ~ "other"
    )
  ) %>%
  count(check_name, check_result, name = "n")

check_edad_plausibility <- working_dates %>%
  mutate(
    age_issue = basic_age_issue(edad_raw),
    check_name = "edad_plausibility",
    check_result = age_issue
  ) %>%
  count(check_name, check_result, name = "n")

check_date_formats <- working_long %>%
  filter(variable_harmonized %in% c("fecha_diagnostico", "fecha_muerte", "fecha_ultimo_contacto")) %>%
  mutate(
    check_name = paste0(variable_harmonized, "_format"),
    check_result = classify_date_format(value_raw)
  ) %>%
  count(check_name, check_result, name = "n")

check_topo_morf_comp <- working_dates %>%
  mutate(
    topo_issue = dplyr::case_when(
      is.na(topografia_norm) ~ "topografia_missing",
      stringr::str_detect(topografia_norm, "^(C|D)\\d{2}") ~ "topografia_pattern_ok",
      classify_unknown_token(topografia_norm) ~ "topografia_unknown_candidate",
      TRUE ~ "topografia_nonstandard"
    ),
    morf_issue = dplyr::case_when(
      is.na(morfologia_norm) ~ "morfologia_missing",
      stringr::str_detect(morfologia_norm, "^\\d{4,5}$") ~ "morfologia_pattern_ok",
      classify_unknown_token(morfologia_norm) ~ "morfologia_unknown_candidate",
      TRUE ~ "morfologia_nonstandard"
    ),
    comportamiento_issue = dplyr::case_when(
      is.na(comportamiento_norm) ~ "comportamiento_missing",
      stringr::str_detect(comportamiento_norm, "^[0-9]$") ~ "comportamiento_pattern_ok",
      classify_unknown_token(comportamiento_norm) ~ "comportamiento_unknown_candidate",
      TRUE ~ "comportamiento_nonstandard"
    )
  ) %>%
  summarise(
    check_name = "topografia_morfologia_comportamiento_surface_consistency",
    n_topografia_pattern_ok = sum(topo_issue == "topografia_pattern_ok", na.rm = TRUE),
    n_topografia_nonstandard = sum(topo_issue == "topografia_nonstandard", na.rm = TRUE),
    n_topografia_unknown_candidate = sum(topo_issue == "topografia_unknown_candidate", na.rm = TRUE),
    n_morfologia_pattern_ok = sum(morf_issue == "morfologia_pattern_ok", na.rm = TRUE),
    n_morfologia_nonstandard = sum(morf_issue == "morfologia_nonstandard", na.rm = TRUE),
    n_morfologia_unknown_candidate = sum(morf_issue == "morfologia_unknown_candidate", na.rm = TRUE),
    n_comportamiento_pattern_ok = sum(comportamiento_issue == "comportamiento_pattern_ok", na.rm = TRUE),
    n_comportamiento_nonstandard = sum(comportamiento_issue == "comportamiento_nonstandard", na.rm = TRUE),
    n_comportamiento_unknown_candidate = sum(comportamiento_issue == "comportamiento_unknown_candidate", na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -check_name,
    names_to = "check_result",
    values_to = "n"
  )

semantic_consistency_checks <- bind_rows(
  check_estado_vital_vs_fecha_muerte,
  check_fecha_muerte_vs_fecha_ultimo_contacto,
  check_edad_plausibility,
  check_date_formats,
  check_topo_morf_comp
) %>%
  arrange(check_name, desc(n))

write_csv(semantic_consistency_checks, out_consistency_checks, na = "")

# ------------------------------------------------------------
# 12) Valores problemáticos priorizados
# ------------------------------------------------------------
semantic_problematic_values <- working_long %>%
  mutate(
    specific_issue = dplyr::case_when(
      variable_harmonized == "edad" ~ basic_age_issue(value_raw),
      variable_harmonized %in% c("fecha_diagnostico", "fecha_muerte", "fecha_ultimo_contacto") ~ classify_date_format(value_raw),
      TRUE ~ problem_class
    )
  ) %>%
  filter(
    (!is_missing & problem_class != "none") |
      (variable_harmonized == "edad" & basic_age_issue(value_raw) != "none") |
      (variable_harmonized %in% c("fecha_diagnostico", "fecha_muerte", "fecha_ultimo_contacto") & !classify_date_format(value_raw) %in% c("dmy_slash", "dmy_dash", "ymd_dash", "ymd_slash", "excel_serial_5", NA_character_))
  ) %>%
  count(variable_harmonized, value_raw, value_norm, specific_issue, sort = TRUE, name = "n") %>%
  arrange(variable_harmonized, desc(n))

write_csv(semantic_problematic_values, out_top_problem_values, na = "")

# ------------------------------------------------------------
# 13) Pendientes de revisión manual
# ------------------------------------------------------------
manual_review_from_candidates <- semantic_recode_candidates %>%
  filter(automation_status == "manual_review_required" | is.na(proposed_standard_label)) %>%
  transmute(
    variable_harmonized,
    value_raw,
    value_norm,
    n_observed,
    years_observed,
    pending_reason = dplyr::case_when(
      variable_harmonized == "base_diagnostico" ~ "requires_local_codebook_before_decoding",
      variable_harmonized == "estado_vital" ~ "requires_cross_validation_with_dates",
      variable_harmonized == "lateralidad" ~ "requires_registry_confirmation_of_domain",
      variable_harmonized == "sexo" ~ "requires_manual_confirmation_of_labels",
      TRUE ~ "requires_manual_review"
    ),
    recommended_action = dplyr::case_when(
      variable_harmonized == "base_diagnostico" ~ "Validar con documento local de base diagnóstica y revisar frecuencias por año.",
      variable_harmonized == "estado_vital" ~ "Contrastar cada código con presencia/ausencia de FECDEF y FUC.",
      variable_harmonized == "lateralidad" ~ "Confirmar semántica de LATE 19 antes de estandarizar.",
      variable_harmonized == "sexo" ~ "Validar si la codificación observada representa sexo biológico/registral y cómo se codifica desconocido.",
      TRUE ~ "Revisión manual focalizada."
    )
  )

family_pending <- tibble(
  variable_harmonized = c(
    "residencia__res",
    "residencia__deptres",
    "residencia__provdist",
    "multiple_primary__pmseq",
    "multiple_primary__pmtot",
    "multiple_primary__pmcod",
    "causa",
    "topografia_icdo",
    "morfologia_icdo",
    "comportamiento",
    "grado",
    "fecha_diagnostico",
    "fecha_muerte",
    "fecha_ultimo_contacto",
    "edad"
  ),
  value_raw = NA_character_,
  value_norm = NA_character_,
  n_observed = NA_integer_,
  years_observed = NA_character_,
  pending_reason = c(
    "residence_family_review_required",
    "residence_family_review_required",
    "residence_family_review_required",
    "multiple_primary_family_review_required",
    "multiple_primary_family_review_required",
    "multiple_primary_family_review_required",
    "cause_field_semantics_unclear",
    "site_code_domain_review",
    "morphology_code_domain_review",
    "behaviour_code_domain_review",
    "grade_domain_review",
    "date_standardization_pending",
    "date_standardization_pending",
    "date_standardization_pending",
    "age_unknown_convention_review"
  ),
  recommended_action = c(
    "Mantener RES separado; revisar catálogo y posibles códigos desconocidos.",
    "Mantener DEPTRES separado; revisar catálogo y consistencia geográfica.",
    "Mantener PROVDIST separado; revisar granularidad y convenciones locales.",
    "Definir semántica de PMSEQ sin mezclar con PMTOT/PMCOD.",
    "Definir semántica de PMTOT sin mezclar con PMSEQ/PMCOD.",
    "Definir semántica de PMCOD sin mezclar con PMSEQ/PMTOT.",
    "Confirmar significado operativo de CAUSA antes de usarla en DCO o mortalidad.",
    "Revisar patrón de códigos topográficos y candidatos de sitio desconocido.",
    "Revisar patrón de morfología y longitud de código.",
    "Revisar catálogo real de comportamiento antes de normalizar.",
    "Revisar catálogo real de grado antes de normalizar.",
    "Estandarizar formatos de FECDIAG y validar parseo.",
    "Estandarizar formatos de FECDEF y validar parseo.",
    "Estandarizar formatos de FUC y validar parseo.",
    "Definir convención de edad desconocida y rangos plausibles."
  )
)

semantic_recode_pending_manual_review <- bind_rows(
  manual_review_from_candidates,
  family_pending
) %>%
  distinct() %>%
  left_join(
    harmonization_dictionary %>%
      select(variable_harmonized, role, qc_priority, notes),
    by = "variable_harmonized"
  ) %>%
  mutate(
    pending_phase = dplyr::case_when(
      variable_harmonized %in% c("base_diagnostico", "estado_vital", "lateralidad", "sexo") ~ "semantic_dictionary_building",
      stringr::str_detect(variable_harmonized, "^residencia__") ~ "geographic_semantic_review",
      stringr::str_detect(variable_harmonized, "^multiple_primary__") ~ "multiple_primary_semantic_review",
      variable_harmonized %in% c("fecha_diagnostico", "fecha_muerte", "fecha_ultimo_contacto") ~ "date_standardization",
      variable_harmonized %in% c("topografia_icdo", "morfologia_icdo", "comportamiento", "grado", "causa", "edad") ~ "domain_qc_before_epidemiologic_phase",
      TRUE ~ "manual_review"
    )
  ) %>%
  arrange(desc(qc_priority), variable_harmonized, desc(n_observed))

write_csv(semantic_recode_pending_manual_review, out_pending_manual_review, na = "")

# ------------------------------------------------------------
# 14) Metadatos de corrida
# ------------------------------------------------------------
run_metadata <- list(
  run_date = run_date,
  source_inputs = list(
    harmonized_wide = f_harmonized_wide,
    harmonized_long = if (has_harmonized_long) f_harmonized_long else NA_character_,
    harmonization_dictionary = f_harmonization_dictionary,
    harmonization_pending = f_harmonization_pending,
    harmonization_action_log = f_harmonization_action_log,
    quality_indicator_field_availability = f_quality_field_availability,
    data_audit_log = f_data_audit_log,
    data_dictionary_crosswalk = f_data_dictionary_crosswalk,
    variable_quality_profile = f_variable_quality_profile,
    master_variable_map = master_variable_map_path,
    master_data_dictionary = master_dictionary_path
  ),
  output_files = list(
    domain_profile = out_domain_profile,
    domain_profile_by_year = out_domain_profile_by_year,
    semantic_recode_candidates = out_recode_candidates,
    semantic_recode_pending_manual_review = out_pending_manual_review,
    value_frequency_base_diagnostico = out_freq_base,
    value_frequency_estado_vital = out_freq_estado,
    value_frequency_lateralidad = out_freq_lateralidad,
    value_frequency_sexo = out_freq_sexo,
    semantic_consistency_checks = out_consistency_checks,
    date_field_format_profile = out_date_profile,
    semantic_problematic_values = out_top_problem_values,
    summary_md = out_summary_md
  ),
  key_counts = list(
    n_rows_harmonized_wide = nrow(harmonized_wide),
    n_cols_harmonized_wide = ncol(harmonized_wide),
    n_priority_variables_requested = length(priority_variables),
    n_priority_variables_available = length(available_priority_variables),
    n_priority_variables_missing = length(missing_priority_variables),
    n_recode_candidates = nrow(semantic_recode_candidates),
    n_pending_manual_review = nrow(semantic_recode_pending_manual_review),
    n_consistency_check_rows = nrow(semantic_consistency_checks)
  ),
  notes = list(
    policy = "No se calcularon indicadores finales IARC en esta fase.",
    preserved_families = c(
      "residencia__res",
      "residencia__deptres",
      "residencia__provdist",
      "multiple_primary__pmseq",
      "multiple_primary__pmtot",
      "multiple_primary__pmcod"
    ),
    semantic_caution = "Las propuestas automáticas son preliminares y reversibles; requieren validación manual.",
    missing_priority_variables = missing_priority_variables
  )
)

write_json(run_metadata, out_run_metadata, pretty = TRUE, auto_unbox = TRUE, na = "null")

# ------------------------------------------------------------
# 15) Resumen markdown de fase
# ------------------------------------------------------------
summary_lines <- c(
  "# Semantic phase summary",
  "",
  glue("Fecha de corrida: {run_date}"),
  "",
  "## Alcance de esta fase",
  "- Perfilado de dominios global y por año para variables prioritarias.",
  "- Construcción de candidatos preliminares de recodificación semántica para sexo, estado_vital, lateralidad y base_diagnostico.",
  "- Chequeos básicos de consistencia entre estado vital y fechas, plausibilidad de edad y formatos de fechas.",
  "- Preparación de pendientes manuales y trazabilidad exportable.",
  "",
  "## Reglas metodológicas respetadas",
  "- No se rehizo la auditoría estructural ni la armonización previa.",
  "- No se impuso semántica final a BASE #7.",
  "- RES, DEPTRES y PROVDIST se mantuvieron separados.",
  "- PMSEQ, PMTOT y PMCOD se mantuvieron separados.",
  "- No se calcularon todavía MV, DCO, PSU ni otros indicadores finales IARC.",
  "",
  "## Cobertura",
  glue("- Variables prioritarias solicitadas: {length(priority_variables)}"),
  glue("- Variables prioritarias disponibles en harmonized_wide: {length(available_priority_variables)}"),
  if (length(missing_priority_variables) > 0) glue("- Variables prioritarias ausentes en harmonized_wide: {paste(missing_priority_variables, collapse = ', ')}") else "- No hay variables prioritarias ausentes en harmonized_wide.",
  "",
  "## Outputs principales",
  glue("- `{basename(out_domain_profile)}`"),
  glue("- `{basename(out_domain_profile_by_year)}`"),
  glue("- `{basename(out_recode_candidates)}`"),
  glue("- `{basename(out_pending_manual_review)}`"),
  glue("- `{basename(out_consistency_checks)}`"),
  "",
  "## Próximo paso sugerido",
  "Validar manualmente el diccionario local de BASE #7, ESTVIT y LATE 19, y luego cerrar reglas reversibles de recodificación para sexo, estado_vital y lateralidad antes de pasar al QC epidemiológico."
)

writeLines(summary_lines, out_summary_md, useBytes = TRUE)

# ------------------------------------------------------------
# 16) Mensajes finales
# ------------------------------------------------------------
cat("\n============================================\n")
cat("SEMANTIC RECODING + DOMAIN PROFILING COMPLETADO\n")
cat("============================================\n")
cat(glue("domain profile       : {out_domain_profile}\n"))
cat(glue("domain profile year  : {out_domain_profile_by_year}\n"))
cat(glue("recode candidates    : {out_recode_candidates}\n"))
cat(glue("pending manual review: {out_pending_manual_review}\n"))
cat(glue("consistency checks   : {out_consistency_checks}\n"))
cat(glue("summary md           : {out_summary_md}\n"))
cat(glue("run metadata         : {out_run_metadata}\n"))
cat("============================================\n\n")

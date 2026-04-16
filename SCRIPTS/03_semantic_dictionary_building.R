suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(glue)
  library(here)
  library(jsonlite)
})

# ============================================================
# 03_semantic_dictionary_building.R
# Fase 3: construcción de diccionarios semánticos locales
# RCBPA Arequipa 2015-2022
# ------------------------------------------------------------
# Esta fase usa outputs oficiales de la fase 2 para construir
# diccionarios locales explícitos, reversibles y auditables.
# No calcula aún indicadores IARC finales.
# ============================================================

# ------------------------------------------------------------
# 0) Rutas
# ------------------------------------------------------------
root <- here::here()

dir.create(file.path(root, "REPORTS"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "REPORTS", "SEMANTIC"), recursive = TRUE, showWarnings = FALSE)

# Insumos upstream
f_harmonized_wide          <- file.path(root, "DATA", "DERIVED", "rcpa_arequipa_2015_2022_harmonized_wide.rds")
f_recode_candidates        <- file.path(root, "REPORTS", "semantic_recode_candidates.csv")
f_pending_manual           <- file.path(root, "REPORTS", "semantic_recode_pending_manual_review.csv")
f_problematic_values       <- file.path(root, "REPORTS", "semantic_problematic_values.csv")
f_date_profile             <- file.path(root, "REPORTS", "date_field_format_profile.csv")
f_consistency_checks       <- file.path(root, "REPORTS", "semantic_consistency_checks.csv")
f_freq_base                <- file.path(root, "REPORTS", "value_frequency_base_diagnostico.csv")
f_freq_estado              <- file.path(root, "REPORTS", "value_frequency_estado_vital.csv")
f_freq_lateralidad         <- file.path(root, "REPORTS", "value_frequency_lateralidad.csv")
f_freq_sexo                <- file.path(root, "REPORTS", "value_frequency_sexo.csv")
f_harmonization_dictionary <- file.path(root, "REPORTS", "harmonization_dictionary.csv")
f_harmonization_pending    <- file.path(root, "REPORTS", "harmonization_pending_semantic_review.csv")
f_data_audit_log           <- file.path(root, "REPORTS", "data_audit_log.json")

# Outputs principales
out_dict_base              <- file.path(root, "REPORTS", "local_dictionary_base_diagnostico.csv")
out_dict_estado            <- file.path(root, "REPORTS", "local_dictionary_estado_vital.csv")
out_dict_lateralidad       <- file.path(root, "REPORTS", "local_dictionary_lateralidad.csv")
out_dict_sexo              <- file.path(root, "REPORTS", "local_dictionary_sexo.csv")
out_unknown_registry       <- file.path(root, "REPORTS", "local_unknown_codes_registry.csv")
out_manual_resolution      <- file.path(root, "REPORTS", "semantic_dictionary_manual_resolution_template.csv")
out_semantic_crosswalk     <- file.path(root, "REPORTS", "semantic_crosswalk_proposed.csv")
out_rulebook_md            <- file.path(root, "REPORTS", "semantic_dictionary_rulebook.md")
out_summary_md             <- file.path(root, "REPORTS", "semantic_dictionary_summary.md")
out_run_metadata           <- file.path(root, "REPORTS", "semantic_dictionary_run_metadata.json")

run_date <- as.character(Sys.Date())

# ------------------------------------------------------------
# 1) Helpers
# ------------------------------------------------------------
read_csv_safe <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

stop_if_missing <- function(paths, label = "required files") {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop(
      paste0("Missing ", label, ":\n", paste(missing, collapse = "\n")),
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
    token %in% c("NA", "N/A", "ND", "NI", "NE", "NK", "NR", "NS", "SD", "00", "000", "0000", "77", "88", "99", "777", "888", "999", "9999") ~ TRUE,
    token %in% c("DESCONOCIDO", "IGNORADO", "SININFORMACION", "SININFO", "NOINFORMA") ~ TRUE,
    TRUE ~ FALSE
  )
}

safe_unique_collapse <- function(x, sep = " | ") {
  x2 <- x %>% as.character() %>% stringr::str_trim()
  x2[x2 == ""] <- NA_character_
  x2 <- unique(stats::na.omit(x2))
  if (length(x2) == 0) return(NA_character_)
  paste(sort(x2), collapse = sep)
}

# ------------------------------------------------------------
# 2) Verificación y lectura
# ------------------------------------------------------------
required_files <- c(
  f_recode_candidates,
  f_pending_manual,
  f_problematic_values,
  f_consistency_checks,
  f_freq_base,
  f_freq_estado,
  f_freq_lateralidad,
  f_freq_sexo,
  f_harmonization_dictionary,
  f_harmonization_pending,
  f_data_audit_log
)
stop_if_missing(required_files)

recode_candidates        <- read_csv_safe(f_recode_candidates)
pending_manual           <- read_csv_safe(f_pending_manual)
problematic_values       <- read_csv_safe(f_problematic_values)
date_profile             <- if (file.exists(f_date_profile)) read_csv_safe(f_date_profile) else tibble()
consistency_checks       <- read_csv_safe(f_consistency_checks)
freq_base                <- read_csv_safe(f_freq_base)
freq_estado              <- read_csv_safe(f_freq_estado)
freq_lateralidad         <- read_csv_safe(f_freq_lateralidad)
freq_sexo                <- read_csv_safe(f_freq_sexo)
harmonization_dictionary <- read_csv_safe(f_harmonization_dictionary)
harmonization_pending    <- read_csv_safe(f_harmonization_pending)
audit_log                <- jsonlite::read_json(f_data_audit_log, simplifyVector = TRUE)

# ------------------------------------------------------------
# 3) Definiciones provisionales basadas en fase 2
# ------------------------------------------------------------
# Estas reglas son EXPLÍCITAS, REVERSIBLES y auditables.
# La única variable que permanece ampliamente abierta es BASE #7.

sexo_rule_seed <- tribble(
  ~value_norm, ~proposed_standard_code, ~proposed_standard_label, ~dictionary_status, ~evidence_basis, ~requires_manual_validation, ~priority_order,
  "1", "1", "male", "provisional_high_confidence", "empirical_pattern_consistent", TRUE, 1,
  "2", "2", "female", "provisional_high_confidence", "empirical_pattern_consistent", TRUE, 2
)

estado_rule_seed <- tribble(
  ~value_norm, ~proposed_standard_code, ~proposed_standard_label, ~dictionary_status, ~evidence_basis, ~requires_manual_validation, ~priority_order,
  "1", "1", "alive", "provisional_high_confidence", "empirical_pattern_consistent", TRUE, 1,
  "2", "2", "dead", "provisional_high_confidence", "empirical_pattern_consistent", TRUE, 2,
  "5", NA_character_, "unresolved", "manual_review_required", "rare_empirical_value", TRUE, 90,
  "9", NA_character_, "unresolved", "manual_review_required", "rare_empirical_value", TRUE, 91,
  "[1] CANCER", NA_character_, "structural_or_data_entry_anomaly", "manual_review_required", "non_domain_text_value", TRUE, 99
)

lateralidad_rule_seed <- tribble(
  ~value_norm, ~proposed_standard_code, ~proposed_standard_label, ~dictionary_status, ~evidence_basis, ~requires_manual_validation, ~priority_order,
  "1", "1", "right", "provisional_moderate_confidence", "empirical_pattern_consistent", TRUE, 1,
  "2", "2", "left", "provisional_moderate_confidence", "empirical_pattern_consistent", TRUE, 2,
  "3", "3", "bilateral", "provisional_moderate_confidence", "empirical_pattern_consistent", TRUE, 3,
  "4", "4", "not_applicable_or_other", "provisional_low_to_moderate_confidence", "empirical_pattern_consistent", TRUE, 4,
  "9", "9", "not_applicable_or_other", "provisional_low_to_moderate_confidence", "empirical_pattern_consistent", TRUE, 9,
  "0", NA_character_, "unresolved", "manual_review_required", "dominant_but_semantically_unclear", TRUE, 0
)

# BASE #7: no se cierra semántica final sin evidencia documental.
# Se propone una columna de clase operativa preliminar SOLO para ayudar a revisión.
classify_base_operational <- function(value_norm) {
  dplyr::case_when(
    is.na(value_norm) ~ "missing",
    classify_unknown_token(value_norm) ~ "unknown_or_invalid_candidate",
    value_norm == "7" ~ "dominant_code_review_first",
    value_norm %in% c("6", "8") ~ "high_frequency_secondary_code",
    value_norm %in% c("1", "2", "4", "5", "9", "10") ~ "medium_frequency_code",
    value_norm == "3" ~ "rare_code",
    value_norm == "0" ~ "zero_code_review",
    TRUE ~ "other_observed_code"
  )
}

# ------------------------------------------------------------
# 4) Constructor genérico de diccionario local
# ------------------------------------------------------------
prepare_frequency_input <- function(freq_df, var_name) {
  df <- freq_df %>% as_tibble()
  
  if (!"value_raw" %in% names(df)) {
    stop(glue("El input de frecuencias para '{var_name}' no contiene la columna 'value_raw'."), call. = FALSE)
  }
  if (!"n" %in% names(df)) {
    stop(glue("El input de frecuencias para '{var_name}' no contiene la columna 'n'."), call. = FALSE)
  }
  
  if (!"source_year" %in% names(df)) df$source_year <- NA_character_
  if (!"is_possible_unknown" %in% names(df)) df$is_possible_unknown <- NA
  if (!"problem_class" %in% names(df)) df$problem_class <- NA_character_
  if (!"value_norm" %in% names(df)) df$value_norm <- normalize_token(df$value_raw)
  
  df %>%
    mutate(
      source_year = as.character(source_year),
      value_raw = as.character(value_raw),
      value_norm = normalize_token(value_raw),
      n = suppressWarnings(as.numeric(n)),
      n = coalesce(n, 0),
      is_possible_unknown = dplyr::case_when(
        is.na(is_possible_unknown) ~ classify_unknown_token(value_raw),
        is.logical(is_possible_unknown) ~ is_possible_unknown,
        TRUE ~ as.character(is_possible_unknown) %in% c("TRUE", "True", "true", "1")
      ),
      problem_class = dplyr::coalesce(as.character(problem_class), if_else(is_possible_unknown, "possible_unknown_code", "none"))
    )
}

ensure_dictionary_seed <- function(rule_seed) {
  needed <- c(
    "value_norm",
    "proposed_standard_code",
    "proposed_standard_label",
    "dictionary_status",
    "evidence_basis",
    "requires_manual_validation",
    "priority_order"
  )
  
  if (is.null(rule_seed)) {
    out <- tibble::tibble(value_norm = character())
    for (nm in setdiff(needed, "value_norm")) out[[nm]] <- vector("logical", 0)
    out$proposed_standard_code <- character()
    out$proposed_standard_label <- character()
    out$dictionary_status <- character()
    out$evidence_basis <- character()
    out$priority_order <- integer()
    return(out)
  }
  
  out <- rule_seed %>% as_tibble()
  missing_cols <- setdiff(needed, names(out))
  if (length(missing_cols) > 0) {
    stop(
      glue("rule_seed tiene columnas faltantes: {paste(missing_cols, collapse = ', ')}"),
      call. = FALSE
    )
  }
  
  out %>%
    mutate(
      value_norm = normalize_token(value_norm),
      proposed_standard_code = as.character(proposed_standard_code),
      proposed_standard_label = as.character(proposed_standard_label),
      dictionary_status = as.character(dictionary_status),
      evidence_basis = as.character(evidence_basis),
      requires_manual_validation = as.logical(requires_manual_validation),
      priority_order = suppressWarnings(as.integer(priority_order))
    )
}

build_local_dictionary <- function(freq_df, var_name, rule_seed = NULL, unresolved_default = TRUE) {
  # Asegurar que columnas esperadas existan incluso cuando rule_seed = NULL
  ensure_cols <- function(df) {
    needed <- c(
      "proposed_standard_code",
      "proposed_standard_label",
      "dictionary_status",
      "evidence_basis",
      "requires_manual_validation",
      "priority_order"
    )
    for (nm in needed) {
      if (!nm %in% names(df)) df[[nm]] <- NA
    }
    df
  }
  
  freq_df_prepared <- prepare_frequency_input(freq_df, var_name)
  rule_seed_prepared <- ensure_dictionary_seed(rule_seed)
  
  out <- freq_df_prepared %>%
    mutate(
      variable_harmonized = var_name,
      value_raw = as.character(value_raw),
      value_norm = normalize_token(value_raw)
    ) %>%
    group_by(variable_harmonized, value_raw, value_norm) %>%
    summarise(
      n_observed = sum(n, na.rm = TRUE),
      years_observed = safe_unique_collapse(source_year),
      n_years_observed = dplyr::n_distinct(source_year),
      is_possible_unknown = any(is_possible_unknown, na.rm = TRUE),
      problem_class = safe_unique_collapse(problem_class),
      .groups = "drop"
    )
  
  out <- out %>%
    left_join(rule_seed_prepared, by = "value_norm")
  
  # Garantizar presencia de columnas esperadas antes de coalesce
  out <- ensure_cols(out)
  
  if (unresolved_default) {
    out <- out %>%
      mutate(
        proposed_standard_label = coalesce(proposed_standard_label, if_else(is_possible_unknown, "unknown", "unresolved")),
        dictionary_status = coalesce(dictionary_status, if_else(is_possible_unknown, "manual_review_required", "manual_review_required")),
        evidence_basis = coalesce(evidence_basis, if_else(is_possible_unknown, "unknown_code_pattern", "insufficient_documentary_evidence")),
        requires_manual_validation = coalesce(requires_manual_validation, TRUE),
        priority_order = coalesce(priority_order, 999L)
      ) %>%
      mutate(
        requires_manual_validation = coalesce(requires_manual_validation, TRUE),
        priority_order = as.integer(priority_order)
      )
  }
  
  out %>%
    mutate(
      proposed_standard_code = as.character(proposed_standard_code),
      recode_rule = dplyr::case_when(
        !is.na(proposed_standard_code) ~ paste0("raw='", value_raw, "' -> std_code='", proposed_standard_code, "' / std_label='", proposed_standard_label, "'"),
        TRUE ~ NA_character_
      )
    ) %>%
    arrange(priority_order, desc(n_observed), value_norm)
}

# ------------------------------------------------------------
# 5) Diccionarios locales
# ------------------------------------------------------------
local_dictionary_sexo <- build_local_dictionary(freq_sexo, "sexo", sexo_rule_seed) %>%
  mutate(
    validation_note = "Validar localmente que 1=male y 2=female antes de congelar regla.",
    downstream_use = "sexo_desconocido, estratificación por sexo, QC epidemiológico"
  )

local_dictionary_estado_vital <- build_local_dictionary(freq_estado, "estado_vital", estado_rule_seed) %>%
  mutate(
    validation_note = dplyr::case_when(
      value_norm %in% c("1", "2") ~ "Contrastar con FECDEF y FUC antes de congelar regla.",
      TRUE ~ "Resolver manualmente antes de usar en seguimiento o PSU/DCO downstream."
    ),
    downstream_use = "seguimiento, vital status, QC epidemiológico"
  )

local_dictionary_lateralidad <- build_local_dictionary(freq_lateralidad, "lateralidad", lateralidad_rule_seed) %>%
  mutate(
    validation_note = dplyr::case_when(
      value_norm == "0" ~ "Código dominante sin semántica confirmada; revisar manualmente con reglas del registro.",
      TRUE ~ "Confirmar con manual local de LATE 19 antes de congelar regla."
    ),
    downstream_use = "QC semántico; eventualmente análisis por sitio/lateralidad"
  )

local_dictionary_base_diagnostico <- build_local_dictionary(freq_base, "base_diagnostico", rule_seed = NULL) %>%
  mutate(
    operational_class = classify_base_operational(value_norm),
    proposed_standard_code = NA_character_,
    proposed_standard_label = dplyr::case_when(
      is_possible_unknown ~ "unknown",
      TRUE ~ "unresolved_basis_code"
    ),
    dictionary_status = dplyr::case_when(
      is_possible_unknown ~ "manual_review_required_unknown_code",
      TRUE ~ "manual_review_required_no_local_codebook"
    ),
    evidence_basis = dplyr::case_when(
      is_possible_unknown ~ "unknown_code_pattern",
      TRUE ~ "empirical_frequency_only"
    ),
    requires_manual_validation = TRUE,
    validation_note = dplyr::case_when(
      value_norm == "7" ~ "Código dominante; revisar primero contra manual/local practice de BASE #7.",
      value_norm == "8" ~ "Código concentrado en 2018; revisar si hubo cambio operativo o de digitación.",
      value_norm == "10" ~ "Código raro/moderado; validar si pertenece a extensión del dominio local.",
      TRUE ~ "No congelar semántica hasta validar diccionario local de BASE #7."
    ),
    downstream_use = "MV, DCO, distribución por base diagnóstica"
  ) %>%
  select(
    variable_harmonized, value_raw, value_norm, n_observed, years_observed, n_years_observed,
    is_possible_unknown, problem_class, operational_class, proposed_standard_code,
    proposed_standard_label, dictionary_status, evidence_basis,
    requires_manual_validation, validation_note, downstream_use
  ) %>%
  arrange(desc(n_observed), value_norm)

# Validaciones mínimas previas a exportación
validate_dictionary_output <- function(df, var_name) {
  required_cols <- c(
    "variable_harmonized", "value_raw", "value_norm", "n_observed",
    "proposed_standard_label", "dictionary_status", "evidence_basis",
    "requires_manual_validation"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      glue("El diccionario '{var_name}' no contiene columnas requeridas: {paste(missing_cols, collapse = ', ')}"),
      call. = FALSE
    )
  }
  
  invisible(df)
}

validate_dictionary_output(local_dictionary_base_diagnostico, "base_diagnostico")
validate_dictionary_output(local_dictionary_estado_vital, "estado_vital")
validate_dictionary_output(local_dictionary_lateralidad, "lateralidad")
validate_dictionary_output(local_dictionary_sexo, "sexo")

write_csv(local_dictionary_base_diagnostico, out_dict_base, na = "")
write_csv(local_dictionary_estado_vital, out_dict_estado, na = "")
write_csv(local_dictionary_lateralidad, out_dict_lateralidad, na = "")
write_csv(local_dictionary_sexo, out_dict_sexo, na = "")

# ------------------------------------------------------------
# 6) Registro local de códigos desconocidos y anómalos
# ------------------------------------------------------------
local_unknown_codes_registry <- bind_rows(
  local_dictionary_base_diagnostico %>% select(variable_harmonized, value_raw, value_norm, n_observed, years_observed, proposed_standard_label, dictionary_status),
  local_dictionary_estado_vital %>% select(variable_harmonized, value_raw, value_norm, n_observed, years_observed, proposed_standard_label, dictionary_status),
  local_dictionary_lateralidad %>% select(variable_harmonized, value_raw, value_norm, n_observed, years_observed, proposed_standard_label, dictionary_status),
  local_dictionary_sexo %>% select(variable_harmonized, value_raw, value_norm, n_observed, years_observed, proposed_standard_label, dictionary_status)
) %>%
  filter(
    proposed_standard_label %in% c("unknown", "unresolved", "structural_or_data_entry_anomaly") |
      stringr::str_detect(dictionary_status, "manual_review_required")
  ) %>%
  arrange(variable_harmonized, desc(n_observed), value_norm)

write_csv(local_unknown_codes_registry, out_unknown_registry, na = "")

# ------------------------------------------------------------
# 7) Crosswalk semántico propuesto
# ------------------------------------------------------------
semantic_crosswalk_proposed <- bind_rows(
  local_dictionary_sexo %>% select(variable_harmonized, value_raw, value_norm, proposed_standard_code, proposed_standard_label, dictionary_status, evidence_basis, requires_manual_validation),
  local_dictionary_estado_vital %>% select(variable_harmonized, value_raw, value_norm, proposed_standard_code, proposed_standard_label, dictionary_status, evidence_basis, requires_manual_validation),
  local_dictionary_lateralidad %>% select(variable_harmonized, value_raw, value_norm, proposed_standard_code, proposed_standard_label, dictionary_status, evidence_basis, requires_manual_validation),
  local_dictionary_base_diagnostico %>% select(variable_harmonized, value_raw, value_norm, proposed_standard_code, proposed_standard_label, dictionary_status, evidence_basis, requires_manual_validation)
) %>%
  mutate(
    recode_ready = !is.na(proposed_standard_code) & !requires_manual_validation,
    reversible_rule = dplyr::case_when(
      !is.na(proposed_standard_code) ~ paste0("raw='", value_raw, "' -> std='", proposed_standard_code, "'"),
      TRUE ~ NA_character_
    )
  )

write_csv(semantic_crosswalk_proposed, out_semantic_crosswalk, na = "")

# ------------------------------------------------------------
# 8) Plantilla de resolución manual
# ------------------------------------------------------------
semantic_dictionary_manual_resolution_template <- bind_rows(
  local_dictionary_base_diagnostico %>% transmute(variable_harmonized, value_raw, value_norm, n_observed, years_observed, current_status = dictionary_status, current_label = proposed_standard_label),
  local_dictionary_estado_vital %>% transmute(variable_harmonized, value_raw, value_norm, n_observed, years_observed, current_status = dictionary_status, current_label = proposed_standard_label),
  local_dictionary_lateralidad %>% transmute(variable_harmonized, value_raw, value_norm, n_observed, years_observed, current_status = dictionary_status, current_label = proposed_standard_label),
  local_dictionary_sexo %>% transmute(variable_harmonized, value_raw, value_norm, n_observed, years_observed, current_status = dictionary_status, current_label = proposed_standard_label)
) %>%
  filter(
    stringr::str_detect(current_status, "manual_review_required") |
      current_label %in% c("unknown", "unresolved", "structural_or_data_entry_anomaly")
  ) %>%
  mutate(
    final_decision = NA_character_,
    final_standard_code = NA_character_,
    final_standard_label = NA_character_,
    reviewer_name = NA_character_,
    review_date = NA_character_,
    evidence_used = NA_character_,
    comments = NA_character_
  )

write_csv(semantic_dictionary_manual_resolution_template, out_manual_resolution, na = "")

# ------------------------------------------------------------
# 9) Rulebook markdown
# ------------------------------------------------------------
rulebook_lines <- c(
  "# Semantic dictionary rulebook",
  "",
  glue("Fecha de corrida: {run_date}"),
  "",
  "## Alcance",
  "Esta fase construye diccionarios locales provisionales y reversibles para sexo, estado_vital, lateralidad y base_diagnostico.",
  "No calcula todavía indicadores IARC finales.",
  "",
  "## Reglas provisionales sugeridas",
  "",
  "### SEXO",
  "- `1` -> `male` (provisional alta confianza)",
  "- `2` -> `female` (provisional alta confianza)",
  "",
  "### ESTADO_VITAL",
  "- `1` -> `alive` (provisional alta confianza)",
  "- `2` -> `dead` (provisional alta confianza)",
  "- otros códigos/textos observados -> revisión manual",
  "",
  "### LATERALIDAD",
  "- `1` -> `right` (provisional)",
  "- `2` -> `left` (provisional)",
  "- `3` -> `bilateral` (provisional)",
  "- `4` y `9` -> `not_applicable_or_other` (provisional)",
  "- `0` -> NO congelar semántica; requiere revisión manual prioritaria",
  "",
  "### BASE_DIAGNOSTICO",
  "- No congelar equivalencias semánticas finales sin documento local de BASE #7.",
  "- Priorizar revisión de códigos `7`, `6`, `8`, `2`, `10` por frecuencia y/o patrón temporal.",
  "",
  "## Criterios de resolución manual",
  "1. Priorizar códigos frecuentes y presentes en múltiples años.",
  "2. Contrastar con documentos del registro, práctica operativa local y consistencia con otras variables.",
  "3. Toda decisión final debe registrarse en la plantilla de resolución manual.",
  "4. Mantener siempre una regla reversible raw -> std.",
  "",
  "## Próxima fase sugerida",
  "Aplicar diccionarios congelados sobre la base armonizada y recién después construir el script de QC epidemiológico (MV, DCO, PSU, edad/sexo desconocidos, seguimiento)."
)
writeLines(rulebook_lines, out_rulebook_md, useBytes = TRUE)

# ------------------------------------------------------------
# 10) Summary markdown
# ------------------------------------------------------------
summary_lines <- c(
  "# Semantic dictionary summary",
  "",
  glue("Fecha de corrida: {run_date}"),
  "",
  "## Outputs generados",
  glue("- `{basename(out_dict_base)}`"),
  glue("- `{basename(out_dict_estado)}`"),
  glue("- `{basename(out_dict_lateralidad)}`"),
  glue("- `{basename(out_dict_sexo)}`"),
  glue("- `{basename(out_unknown_registry)}`"),
  glue("- `{basename(out_manual_resolution)}`"),
  glue("- `{basename(out_semantic_crosswalk)}`"),
  glue("- `{basename(out_rulebook_md)}`"),
  "",
  "## Estado",
  "- SEXO: casi cerrable tras validación local corta.",
  "- ESTADO_VITAL: casi cerrable para 1/2, con revisión de valores raros.",
  "- LATERALIDAD: parcialmente cerrable, pero el código 0 requiere revisión manual prioritaria.",
  "- BASE_DIAGNOSTICO: permanece abierto; solo se construye diccionario operativo preliminar.",
  "",
  "## Recomendación",
  "Completar la plantilla de resolución manual y congelar un crosswalk final antes de aplicar recodificación semántica a la base harmonized_wide."
)
writeLines(summary_lines, out_summary_md, useBytes = TRUE)

# ------------------------------------------------------------
# 11) Metadatos
# ------------------------------------------------------------
run_metadata <- list(
  run_date = run_date,
  source_inputs = list(
    semantic_recode_candidates = f_recode_candidates,
    semantic_recode_pending_manual_review = f_pending_manual,
    semantic_problematic_values = f_problematic_values,
    date_field_format_profile = if (file.exists(f_date_profile)) f_date_profile else NA_character_,
    semantic_consistency_checks = f_consistency_checks,
    value_frequency_base_diagnostico = f_freq_base,
    value_frequency_estado_vital = f_freq_estado,
    value_frequency_lateralidad = f_freq_lateralidad,
    value_frequency_sexo = f_freq_sexo,
    harmonization_dictionary = f_harmonization_dictionary,
    harmonization_pending_semantic_review = f_harmonization_pending,
    data_audit_log = f_data_audit_log
  ),
  output_files = list(
    local_dictionary_base_diagnostico = out_dict_base,
    local_dictionary_estado_vital = out_dict_estado,
    local_dictionary_lateralidad = out_dict_lateralidad,
    local_dictionary_sexo = out_dict_sexo,
    local_unknown_codes_registry = out_unknown_registry,
    semantic_dictionary_manual_resolution_template = out_manual_resolution,
    semantic_crosswalk_proposed = out_semantic_crosswalk,
    semantic_dictionary_rulebook = out_rulebook_md,
    semantic_dictionary_summary = out_summary_md
  ),
  key_counts = list(
    n_base_codes = nrow(local_dictionary_base_diagnostico),
    n_estado_codes = nrow(local_dictionary_estado_vital),
    n_lateralidad_codes = nrow(local_dictionary_lateralidad),
    n_sexo_codes = nrow(local_dictionary_sexo),
    n_unknown_registry_rows = nrow(local_unknown_codes_registry),
    n_manual_resolution_rows = nrow(semantic_dictionary_manual_resolution_template)
  ),
  notes = list(
    policy = "No se calcularon indicadores IARC finales en esta fase.",
    caution = "Los diccionarios son provisionales y reversibles hasta validación manual.",
    base_diagnostico_policy = "No congelar semántica final sin evidencia documental/local para BASE #7.",
    lateralidad_policy = "No congelar el significado de 0 sin revisión manual."
  )
)

write_json(run_metadata, out_run_metadata, pretty = TRUE, auto_unbox = TRUE, na = "null")

# ------------------------------------------------------------
# 12) Mensajes finales
# ------------------------------------------------------------
cat("\n============================================\n")
cat("SEMANTIC DICTIONARY BUILDING COMPLETADO\n")
cat("============================================\n")
cat(glue("dict base           : {out_dict_base}\n"))
cat(glue("dict estado vital   : {out_dict_estado}\n"))
cat(glue("dict lateralidad    : {out_dict_lateralidad}\n"))
cat(glue("dict sexo           : {out_dict_sexo}\n"))
cat(glue("unknown registry    : {out_unknown_registry}\n"))
cat(glue("manual template     : {out_manual_resolution}\n"))
cat(glue("semantic crosswalk  : {out_semantic_crosswalk}\n"))
cat(glue("rulebook md         : {out_rulebook_md}\n"))
cat(glue("summary md          : {out_summary_md}\n"))
cat(glue("run metadata        : {out_run_metadata}\n"))
cat("============================================\n\n")

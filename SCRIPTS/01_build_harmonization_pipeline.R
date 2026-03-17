suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(glue)
  library(here)
  library(jsonlite)
})

# ============================================================
# 01_build_harmonization_pipeline.R
# Harmonización estructural/semántica inicial del RCBPA Arequipa 2015-2022
# ------------------------------------------------------------
# PRINCIPIOS
# - downstream del 00_audit_raw_dictionary.R
# - usa como insumos los RDS y maestros oficiales ya validados
# - no recalcula la auditoría estructural desde cero
# - no impone semántica final sin evidencia explícita
# - deja trazabilidad total en logs/tablas exportables
# - prepara la siguiente fase de recodificación epidemiológica/QC IARC
# ============================================================

# ------------------------------------------------------------
# 0) Setup de rutas
# ------------------------------------------------------------
root <- here::here()

dir.create(file.path(root, "DATA", "DERIVED"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "REPORTS"), recursive = TRUE, showWarnings = FALSE)

# Inputs oficiales
f_raw_exact                  <- file.path(root, "DATA", "DERIVED", "rcpa_raw_exact.rds")
f_raw_tagged                 <- file.path(root, "DATA", "DERIVED", "rcpa_raw_tagged.rds")
f_crosswalk                  <- file.path(root, "REPORTS", "data_dictionary_crosswalk.csv")
f_var_quality                <- file.path(root, "REPORTS", "variable_quality_profile.csv")
f_harm_domain_profile        <- file.path(root, "REPORTS", "harmonized_domain_profile.csv")
f_quality_indicator_avail    <- file.path(root, "REPORTS", "quality_indicator_field_availability.csv")
f_data_audit_log             <- file.path(root, "REPORTS", "data_audit_log.json")
f_master_variable_map_derived <- file.path(root, "DATA", "DERIVED", "METADATA", "MASTER_VARIABLE_MAP.csv")
f_master_variable_map_reports <- file.path(root, "REPORTS", "METADATA", "MASTER_VARIABLE_MAP.csv")
f_master_data_dictionary_der  <- file.path(root, "DATA", "DERIVED", "METADATA", "MASTER_DATA_DICTIONARY.csv")
f_master_data_dictionary_rep  <- file.path(root, "REPORTS", "METADATA", "MASTER_DATA_DICTIONARY.csv")

# Outputs de esta fase
out_harmonized_wide          <- file.path(root, "DATA", "DERIVED", "rcpa_arequipa_2015_2022_harmonized_wide.rds")
out_harmonized_long          <- file.path(root, "DATA", "DERIVED", "rcpa_arequipa_2015_2022_harmonized_long.rds")
out_action_log               <- file.path(root, "REPORTS", "harmonization_action_log.csv")
out_exclusion_log            <- file.path(root, "REPORTS", "harmonization_exclusion_log.csv")
out_dictionary               <- file.path(root, "REPORTS", "harmonization_dictionary.csv")
out_pending_semantic_review  <- file.path(root, "REPORTS", "harmonization_pending_semantic_review.csv")
out_summary_md               <- file.path(root, "REPORTS", "harmonization_summary.md")
out_run_metadata             <- file.path(root, "REPORTS", "harmonization_run_metadata.json")

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
  dplyr::na_if(x_chr, "")
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

coalesce_chr <- function(...) {
  vals <- list(...)
  out <- vals[[1]]
  if (length(vals) == 1) return(out)
  for (i in 2:length(vals)) {
    out <- dplyr::coalesce(out, vals[[i]])
  }
  out
}

normalize_clean_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", "_")
}

classify_role_from_canonical <- function(x) {
  dplyr::case_when(
    x %in% c(
      "sexo", "edad", "fecha_diagnostico", "topografia_icdo", "morfologia_icdo",
      "comportamiento", "grado", "lateralidad", "base_diagnostico",
      "estado_vital", "fecha_muerte", "fecha_ultimo_contacto", "causa", "cie10"
    ) ~ "epidemiologic_core",
    stringr::str_detect(x, "^residencia__") ~ "residence",
    stringr::str_detect(x, "^multiple_primary__") ~ "multiple_primaries",
    x %in% c("tx", "nx", "mx", "dx", "estadio", "cicn") ~ "clinical_auxiliary",
    stringr::str_detect(x, "patient|tumour|record|updated|status|obsolete|notif|inst_number11|prof|veri|estcas|modif|busc|caso|obs") ~ "administrative",
    TRUE ~ "other"
  )
}

classify_pending_reason <- function(canonical_name) {
  dplyr::case_when(
    canonical_name == "base_diagnostico" ~ "requires_local_codebook",
    canonical_name == "estado_vital" ~ "requires_local_codebook",
    canonical_name == "lateralidad" ~ "requires_local_codebook",
    stringr::str_detect(canonical_name, "^residencia__") ~ "requires_geographic_rule_review",
    stringr::str_detect(canonical_name, "^multiple_primary__") ~ "requires_multiple_primary_rule_review",
    canonical_name %in% c("sexo", "edad", "causa") ~ "requires_domain_validation",
    TRUE ~ "review_if_used_for_analysis"
  )
}

infer_target_type <- function(canonical_name) {
  dplyr::case_when(
    canonical_name %in% c("edad", "pmseq", "pmtot", "multiple_primary__pmseq", "multiple_primary__pmtot") ~ "numeric_or_integer_after_review",
    canonical_name %in% c("fecha_diagnostico", "fecha_muerte", "fecha_ultimo_contacto") ~ "date_after_review",
    TRUE ~ "character_preserve_raw"
  )
}

write_md_summary <- function(path, lines) {
  writeLines(lines, con = path, useBytes = TRUE)
}

# ------------------------------------------------------------
# 2) Verificación de insumos oficiales
# ------------------------------------------------------------
required_files <- c(
  f_raw_exact,
  f_raw_tagged,
  f_crosswalk,
  f_var_quality,
  f_harm_domain_profile,
  f_quality_indicator_avail,
  f_data_audit_log
)

stop_if_missing(required_files)

master_variable_map_path <- pick_first_existing(c(f_master_variable_map_derived, f_master_variable_map_reports))
master_data_dictionary_path <- pick_first_existing(c(f_master_data_dictionary_der, f_master_data_dictionary_rep))

# Estos dos maestros son deseables; si faltan, el script sigue y lo deja consignado.

# ------------------------------------------------------------
# 3) Lectura de insumos
# ------------------------------------------------------------
raw_exact <- readRDS(f_raw_exact)
raw_tagged <- readRDS(f_raw_tagged)

crosswalk <- read_csv_safe(f_crosswalk)
var_quality <- read_csv_safe(f_var_quality)
harm_domain_profile <- read_csv_safe(f_harm_domain_profile)
quality_indicator_avail <- read_csv_safe(f_quality_indicator_avail)
audit_log <- jsonlite::read_json(f_data_audit_log, simplifyVector = TRUE)

master_variable_map <- if (!is.na(master_variable_map_path)) read_csv_safe(master_variable_map_path) else tibble()
master_data_dictionary <- if (!is.na(master_data_dictionary_path)) read_csv_safe(master_data_dictionary_path) else tibble()

raw_tagged_data <- raw_tagged$data
raw_tagged_colmap <- raw_tagged$column_map

# Estandarización prudente de tipos para joins.
# La auditoría oficial puede haber guardado algunos identificadores de hoja
# como numéricos en ciertos CSV, mientras que el column_map usa texto.
raw_tagged_colmap <- raw_tagged_colmap %>%
  mutate(
    sheet = as.character(sheet),
    year_sheet = as.character(year_sheet),
    col_position = suppressWarnings(as.integer(col_position))
  )

var_quality <- var_quality %>%
  mutate(
    sheet = as.character(sheet),
    col_position = suppressWarnings(as.integer(col_position))
  )

crosswalk <- crosswalk %>%
  mutate(
    canonical_candidate = as.character(canonical_candidate)
  )

# ------------------------------------------------------------
# 4) Reglas oficiales de armonización para esta fase
# ------------------------------------------------------------
# Reglas explícitas ya validadas o exigidas por el proyecto.
manual_rules <- tribble(
  ~canonical_candidate,            ~source_clean_name, ~source_raw_name, ~action_taken,                    ~role,                 ~evidence_level,   ~rationale,                                                                 ~recoding_pending, ~qc_priority, ~notes,
  "sexo",                         "sexo",            "SEXO",          "rename_to_canonical",          "epidemiologic_core", "evidence_direct", "Variable crítica IARC detectada en todos los años.",                       TRUE,              "high",       "Validar codificación local de sexo.",
  "edad",                         "edad",            "EDAD",          "rename_to_canonical",          "epidemiologic_core", "evidence_direct", "Variable crítica IARC detectada en todos los años.",                       TRUE,              "high",       "Validar convención para edad desconocida.",
  "fecha_diagnostico",            "fecdiag",         "FECDIAG",       "rename_to_canonical",          "epidemiologic_core", "evidence_direct", "Variable crítica IARC detectada en todos los años.",                       TRUE,              "high",       "Convertir a fecha en fase posterior.",
  "topografia_icdo",              "topo",            "TOPO",          "rename_to_canonical",          "epidemiologic_core", "evidence_direct", "Topografía ICD-O candidata estable.",                                      TRUE,              "high",       "No recodificar aún.",
  "morfologia_icdo",              "morf",            "MORF",          "rename_to_canonical",          "epidemiologic_core", "evidence_direct", "Morfología ICD-O candidata estable.",                                      TRUE,              "high",       "No recodificar aún.",
  "comportamiento",               "comport_number5", "COMPORT #5",    "rename_to_canonical",          "epidemiologic_core", "inference_probable", "Comportamiento oncológico con mapeo estructural consistente.",           TRUE,              "high",       "Mantener valor raw mientras se valida codificación.",
  "grado",                        "grado",           "GRADO",         "rename_to_canonical",          "epidemiologic_core", "evidence_direct", "Grado detectado en todos los años.",                                       TRUE,              "medium",     "Revisar dominios luego.",
  "lateralidad",                  "late_19",         "LATE 19",       "rename_to_canonical",          "epidemiologic_core", "hypothesis_uncertain", "Candidato estructural consistente pero con semántica pendiente.",       TRUE,              "medium",     "Revisión semántica obligatoria.",
  "base_diagnostico",             "base_number7",    "BASE #7",       "rename_to_canonical",          "epidemiologic_core", "inference_probable", "Campo crítico para QC IARC; conservar íntegro y revisar codificación local.", TRUE,           "high",       "No decodificar aún; mantener raw.",
  "estado_vital",                 "estvit",          "ESTVIT",        "rename_to_canonical",          "epidemiologic_core", "evidence_direct", "Candidato estable para seguimiento/vital status.",                         TRUE,              "high",       "Validar etiquetas locales.",
  "fecha_muerte",                 "fecdef",          "FECDEF",        "rename_to_canonical",          "epidemiologic_core", "evidence_direct", "Campo estructural estable.",                                               TRUE,              "high",       "Convertir a fecha en fase posterior.",
  "fecha_ultimo_contacto",        "fuc",             "FUC",           "rename_to_canonical",          "epidemiologic_core", "evidence_direct", "Campo estructural estable.",                                               TRUE,              "high",       "Convertir a fecha en fase posterior.",
  "causa",                        "causa",           "CAUSA",         "rename_to_canonical",          "epidemiologic_core", "inference_probable", "Útil para DCO u otras verificaciones; semántica final pendiente.",       TRUE,              "medium",     "No interpretar aún como causa básica sin validación.",
  "cie10",                        "cie10",           "CIE10",         "rename_to_canonical",          "clinical_auxiliary", "evidence_direct", "Variable clínica auxiliar estable.",                                       TRUE,              "medium",     "No normalizar aún.",
  "cicn",                         "cicn",            "CICN",          "rename_to_canonical",          "clinical_auxiliary", "evidence_direct", "Variable clínica auxiliar estable.",                                       TRUE,              "low",        "Pendiente de revisión semántica.",
  "estadio",                      "estadio",         "ESTADIO",       "rename_to_canonical",          "clinical_auxiliary", "evidence_direct", "Variable clínica auxiliar estable.",                                       TRUE,              "medium",     "No tipificar aún.",
  "dx",                           "dx",              "DX",            "rename_to_canonical",          "clinical_auxiliary", "evidence_direct", "Variable clínica auxiliar estable.",                                       TRUE,              "low",        "Pendiente de revisión posterior.",
  "tx",                           "tx",              "TX",            "rename_to_canonical",          "clinical_auxiliary", "evidence_direct", "Variable clínica auxiliar estable.",                                       TRUE,              "low",        "Pendiente de revisión posterior.",
  "nx",                           "nx",              "NX",            "rename_to_canonical",          "clinical_auxiliary", "evidence_direct", "Variable clínica auxiliar estable.",                                       TRUE,              "low",        "Pendiente de revisión posterior.",
  "mx",                           "mx",              "MX",            "rename_to_canonical",          "clinical_auxiliary", "evidence_direct", "Variable clínica auxiliar estable.",                                       TRUE,              "low",        "Pendiente de revisión posterior.",
  "residencia__res",              "res",             "RES",           "keep_as_separate_family_member", "residence",         "inference_probable", "No colapsar ciegamente RES/DEPTRES/PROVDIST.",                           TRUE,              "high",       "Mantener separado y trazable.",
  "residencia__deptres",          "deptres",         "DEPTRES",       "keep_as_separate_family_member", "residence",         "inference_probable", "No colapsar ciegamente RES/DEPTRES/PROVDIST.",                           TRUE,              "high",       "Mantener separado y trazable.",
  "residencia__provdist",         "provdist",        "PROVDIST",      "keep_as_separate_family_member", "residence",         "inference_probable", "No colapsar ciegamente RES/DEPTRES/PROVDIST.",                           TRUE,              "high",       "Mantener separado y trazable.",
  "multiple_primary__pmseq",      "pmseq",           "PMSEQ",         "keep_as_separate_family_member", "multiple_primaries", "inference_probable", "No tratar PMSEQ/PMTOT/PMCOD como equivalentes.",                         TRUE,              "high",       "Mantener separado y trazable.",
  "multiple_primary__pmtot",      "pmtot",           "PMTOT",         "keep_as_separate_family_member", "multiple_primaries", "inference_probable", "No tratar PMSEQ/PMTOT/PMCOD como equivalentes.",                         TRUE,              "high",       "Mantener separado y trazable.",
  "multiple_primary__pmcod",      "pmcod",           "PMCOD",         "keep_as_separate_family_member", "multiple_primaries", "inference_probable", "No tratar PMSEQ/PMTOT/PMCOD como equivalentes.",                         TRUE,              "high",       "Mantener separado y trazable.",
  "blank_col_2",                  "blank_col_2",     "__blank_col_2", "exclude_structural_anomaly",   "structural_anomaly", "evidence_direct", "Encabezado anómalo oficial detectado en 2017.",                             FALSE,             "exclude",    "Excluir del dataset analítico armonizado.",
  "673668",                       "673668",          "673668",        "exclude_structural_anomaly",   "structural_anomaly", "evidence_direct", "Encabezado anómalo oficial detectado en 2018.",                             FALSE,             "exclude",    "Excluir del dataset analítico armonizado."
)

# ------------------------------------------------------------
# 5) Construcción del mapa operativo de armonización
# ------------------------------------------------------------
colmap_enriched <- raw_tagged_colmap %>%
  mutate(
    clean_name = normalize_clean_name(clean_name),
    raw_name = as.character(raw_name)
  ) %>%
  left_join(
    var_quality %>%
      transmute(
        sheet,
        col_position,
        variable_role_audit = variable_role,
        flag_structural_garbage = dplyr::coalesce(flag_structural_garbage, FALSE),
        header_anomaly = dplyr::coalesce(header_anomaly, "none"),
        quality_issue_summary = dplyr::coalesce(quality_issue_summary, "none"),
        possible_iarc_field_audit = possible_iarc_field
      ),
    by = c("sheet", "col_position")
  ) %>%
  left_join(
    crosswalk %>%
      transmute(
        crosswalk_candidate = canonical_candidate,
        clean_name_crosswalk = normalize_clean_name(canonical_candidate),
        possible_iarc_field_crosswalk = possible_iarc_field,
        harmonization_decision,
        decision_status,
        crosswalk_notes = notes
      ),
    by = c("clean_name" = "clean_name_crosswalk")
  )

harmonization_map <- colmap_enriched %>%
  left_join(
    manual_rules %>%
      select(
        canonical_candidate,
        source_clean_name,
        source_raw_name,
        action_taken,
        role,
        evidence_level,
        rationale,
        recoding_pending,
        qc_priority,
        notes
      ) %>%
      rename(manual_canonical_candidate = canonical_candidate),
    by = c("clean_name" = "source_clean_name", "raw_name" = "source_raw_name")
  ) %>%
  mutate(
    canonical_candidate = dplyr::coalesce(manual_canonical_candidate, crosswalk_candidate),
    action_taken = dplyr::coalesce(action_taken,
                                   dplyr::case_when(
                                     flag_structural_garbage ~ "exclude_structural_garbage",
                                     !is.na(canonical_candidate) ~ "retain_as_mapped_candidate",
                                     TRUE ~ "exclude_unmapped"
                                   )
    ),
    role = dplyr::coalesce(role, classify_role_from_canonical(canonical_candidate)),
    evidence_level = dplyr::coalesce(
      evidence_level,
      dplyr::case_when(
        flag_structural_garbage ~ "evidence_direct",
        !is.na(possible_iarc_field_audit) ~ "inference_probable",
        TRUE ~ "weak_signal"
      )
    ),
    rationale = dplyr::coalesce(
      rationale,
      dplyr::case_when(
        flag_structural_garbage ~ "Variable marcada en auditoría oficial como basura estructural o encabezado anómalo.",
        !is.na(canonical_candidate) ~ "Variable conservada como candidata armonizable según crosswalk oficial.",
        TRUE ~ "Sin mapeo explícito para esta fase; se excluye del analítico armonizado preliminar."
      )
    ),
    recoding_pending = dplyr::coalesce(recoding_pending, TRUE),
    qc_priority = dplyr::coalesce(qc_priority,
                                  dplyr::case_when(
                                    role %in% c("epidemiologic_core", "residence", "multiple_primaries") ~ "high",
                                    role == "clinical_auxiliary" ~ "medium",
                                    role == "administrative" ~ "low",
                                    TRUE ~ "review"
                                  )
    ),
    notes = dplyr::coalesce(notes, crosswalk_notes, quality_issue_summary)
  )

# ------------------------------------------------------------
# 6) Logs de acción y exclusión
# ------------------------------------------------------------
harmonization_action_log <- harmonization_map %>%
  arrange(year_sheet, sheet, col_position) %>%
  transmute(
    sheet,
    year_sheet,
    col_position,
    canonical_candidate = dplyr::coalesce(canonical_candidate, clean_name),
    source_raw_name = raw_name,
    source_clean_name = clean_name,
    audit_name,
    action_taken,
    role,
    rationale,
    evidence_level,
    notes
  )

write_csv(harmonization_action_log, out_action_log, na = "")

harmonization_exclusion_log <- harmonization_map %>%
  filter(action_taken %in% c("exclude_structural_anomaly", "exclude_structural_garbage", "exclude_unmapped")) %>%
  mutate(
    exclusion_reason = dplyr::case_when(
      action_taken == "exclude_structural_anomaly" ~ "structural_anomaly",
      action_taken == "exclude_structural_garbage" ~ "structural_garbage",
      action_taken == "exclude_unmapped" & role == "administrative" ~ "administrative",
      action_taken == "exclude_unmapped" ~ "ambiguity_not_resolved",
      TRUE ~ "other"
    )
  ) %>%
  transmute(
    sheet,
    year_sheet,
    col_position,
    raw_name,
    clean_name,
    audit_name,
    canonical_candidate = dplyr::coalesce(canonical_candidate, clean_name),
    exclusion_reason,
    role,
    rationale,
    evidence_level,
    notes
  )

write_csv(harmonization_exclusion_log, out_exclusion_log, na = "")

# ------------------------------------------------------------
# 7) Definición de columnas a retener en dataset armonizado
# ------------------------------------------------------------
keep_map <- harmonization_map %>%
  mutate(
    keep_in_harmonized = action_taken %in% c(
      "rename_to_canonical",
      "keep_as_separate_family_member",
      "retain_as_mapped_candidate"
    ) & !action_taken %in% c("exclude_structural_anomaly", "exclude_structural_garbage", "exclude_unmapped")
  ) %>%
  filter(keep_in_harmonized) %>%
  select(sheet, year_sheet, audit_name, canonical_candidate, role, qc_priority)

# ------------------------------------------------------------
# 8) Construcción de dataset largo armonizado
# ------------------------------------------------------------
# Se preserva row_id interno, año, hoja y audit_name para trazabilidad total.
raw_long <- raw_tagged_data %>%
  mutate(
    row_id = dplyr::row_number(),
    year_sheet = as.character(year_sheet),
    sheet_source = as.character(sheet_source)
  ) %>%
  pivot_longer(
    cols = -c(row_id, sheet_source, year_sheet),
    names_to = "audit_name",
    values_to = "value_raw"
  ) %>%
  mutate(value_raw = na_blank(value_raw)) %>%
  filter(!is.na(value_raw))

harmonized_long <- raw_long %>%
  inner_join(
    keep_map,
    by = c("audit_name", "year_sheet")
  ) %>%
  mutate(
    source_year = year_sheet,
    source_sheet = sheet_source,
    variable_harmonized = canonical_candidate,
    value = value_raw
  ) %>%
  select(
    row_id,
    source_sheet,
    source_year,
    audit_name,
    variable_harmonized,
    role,
    qc_priority,
    value
  )

saveRDS(harmonized_long, out_harmonized_long)

# ------------------------------------------------------------
# 9) Construcción de dataset ancho armonizado preliminar
# ------------------------------------------------------------
# En esta fase se prioriza una sola columna por variable armonizada y caso.
# Si existieran duplicados dentro del mismo row_id-variable, se conserva el primer
# valor no vacío y se documenta aparte.
wide_collision_log <- harmonized_long %>%
  count(row_id, variable_harmonized, name = "n_values") %>%
  filter(n_values > 1)

harmonized_long_resolved <- harmonized_long %>%
  group_by(row_id, source_sheet, source_year, variable_harmonized) %>%
  summarise(
    value = dplyr::first(value),
    n_values_collapsed = dplyr::n(),
    .groups = "drop"
  )

harmonized_wide <- harmonized_long_resolved %>%
  select(row_id, source_sheet, source_year, variable_harmonized, value) %>%
  pivot_wider(
    names_from = variable_harmonized,
    values_from = value
  ) %>%
  arrange(source_year, row_id)

saveRDS(harmonized_wide, out_harmonized_wide)

# ------------------------------------------------------------
# 10) Diccionario de armonización final
# ------------------------------------------------------------
dictionary_base <- harmonization_map %>%
  mutate(variable_harmonized = dplyr::coalesce(canonical_candidate, clean_name)) %>%
  group_by(variable_harmonized) %>%
  summarise(
    source_variables = safe_unique_collapse(raw_name),
    source_clean_names = safe_unique_collapse(clean_name),
    source_audit_names = safe_unique_collapse(audit_name),
    years_present = safe_unique_collapse(year_sheet),
    role = names(sort(table(role), decreasing = TRUE))[1],
    type_target = infer_target_type(variable_harmonized),
    recoding_pending = any(recoding_pending %in% TRUE),
    qc_priority = names(sort(table(qc_priority), decreasing = TRUE))[1],
    action_summary = safe_unique_collapse(action_taken),
    evidence_level = names(sort(table(evidence_level), decreasing = TRUE))[1],
    notes = safe_unique_collapse(notes),
    .groups = "drop"
  )

harmonization_dictionary <- dictionary_base %>%
  distinct(variable_harmonized, .keep_all = TRUE) %>%
  mutate(
    excluded_from_analytic_harmonized = variable_harmonized %in% harmonization_exclusion_log$canonical_candidate,
    pending_semantic_review = recoding_pending,
    target_phase = dplyr::case_when(
      role == "epidemiologic_core" ~ "semantic_recoding_and_qc",
      role %in% c("residence", "multiple_primaries") ~ "family_specific_review",
      role == "clinical_auxiliary" ~ "clinical_auxiliary_review",
      role == "administrative" ~ "metadata_only",
      TRUE ~ "later_review"
    )
  ) %>%
  arrange(
    factor(role, levels = c("epidemiologic_core", "residence", "multiple_primaries", "clinical_auxiliary", "administrative", "other", "structural_anomaly")),
    variable_harmonized
  )

write_csv(harmonization_dictionary, out_dictionary, na = "")

# ------------------------------------------------------------
# 11) Pendientes de revisión semántica
# ------------------------------------------------------------
priority_pending <- c(
  "base_diagnostico",
  "estado_vital",
  "lateralidad",
  "residencia__res",
  "residencia__deptres",
  "residencia__provdist",
  "multiple_primary__pmseq",
  "multiple_primary__pmtot",
  "multiple_primary__pmcod",
  "sexo",
  "edad",
  "causa"
)

harmonization_pending_semantic_review <- harmonization_dictionary %>%
  distinct(variable_harmonized, .keep_all = TRUE) %>%
  filter(
    role %in% c(
      "epidemiologic_core",
      "residence",
      "multiple_primaries",
      "clinical_auxiliary"
    )
  ) %>%
  filter(variable_harmonized %in% priority_pending | pending_semantic_review) %>%
  mutate(
    pending_reason = classify_pending_reason(variable_harmonized),
    recommended_next_step = dplyr::case_when(
      variable_harmonized == "base_diagnostico" ~ "Construir diccionario local de BASE #7 y tabla de frecuencias por año.",
      variable_harmonized == "estado_vital" ~ "Revisar códigos locales y consistencia con FECDEF/FUC.",
      variable_harmonized == "lateralidad" ~ "Confirmar que LATE 19 efectivamente represente lateralidad y su dominio.",
      stringr::str_detect(variable_harmonized, "^residencia__") ~ "Definir reglas de jerarquía geográfica sin colapsar variables prematuramente.",
      stringr::str_detect(variable_harmonized, "^multiple_primary__") ~ "Definir semántica operativa de PMSEQ/PMTOT/PMCOD según reglas del registro.",
      variable_harmonized == "sexo" ~ "Validar categorías observadas y convención de desconocido.",
      variable_harmonized == "edad" ~ "Validar formatos, rangos y codificación de edad desconocida.",
      variable_harmonized == "causa" ~ "Confirmar significado operativo antes de usarla para DCO o mortalidad.",
      TRUE ~ "Revisión semántica dirigida antes de recodificación analítica."
    )
  ) %>%
  select(
    variable_harmonized,
    source_variables,
    role,
    qc_priority,
    pending_reason,
    recommended_next_step,
    notes
  ) %>%
  arrange(desc(qc_priority), variable_harmonized)

write_csv(harmonization_pending_semantic_review, out_pending_semantic_review, na = "")

# ------------------------------------------------------------
# 12) Metadatos de corrida
# ------------------------------------------------------------
run_metadata <- list(
  run_date = run_date,
  source_inputs = list(
    raw_exact = f_raw_exact,
    raw_tagged = f_raw_tagged,
    crosswalk = f_crosswalk,
    variable_quality = f_var_quality,
    harmonized_domain_profile = f_harm_domain_profile,
    quality_indicator_field_availability = f_quality_indicator_avail,
    data_audit_log = f_data_audit_log,
    master_variable_map = master_variable_map_path,
    master_data_dictionary = master_data_dictionary_path
  ),
  output_files = list(
    harmonized_wide = out_harmonized_wide,
    harmonized_long = out_harmonized_long,
    action_log = out_action_log,
    exclusion_log = out_exclusion_log,
    dictionary = out_dictionary,
    pending_semantic_review = out_pending_semantic_review,
    summary_md = out_summary_md
  ),
  key_counts = list(
    n_rows_raw_exact = nrow(raw_exact),
    n_rows_tagged = nrow(raw_tagged_data),
    n_columns_mapped = nrow(harmonization_map),
    n_variables_harmonized = n_distinct(harmonized_long$variable_harmonized),
    n_rows_harmonized_long = nrow(harmonized_long),
    n_rows_harmonized_wide = nrow(harmonized_wide),
    n_exclusions = nrow(harmonization_exclusion_log),
    n_pending_semantic_review = nrow(harmonization_pending_semantic_review),
    n_wide_collisions = nrow(wide_collision_log)
  ),
  notes = list(
    policy = "No se realizaron recodificaciones epidemiológicas finales ni cálculo de indicadores IARC.",
    anomalies_excluded = c("blank_col_2", "673668"),
    residence_policy = "RES/DEPTRES/PROVDIST preservadas como familia separada.",
    multiple_primary_policy = "PMSEQ/PMTOT/PMCOD preservadas como familia separada."
  )
)

write_json(run_metadata, out_run_metadata, pretty = TRUE, auto_unbox = TRUE, na = "null")

# ------------------------------------------------------------
# 13) Resumen técnico markdown
# ------------------------------------------------------------
summary_lines <- c(
  "# Harmonization summary",
  "",
  glue("Fecha de corrida: {run_date}"),
  "",
  "## Insumos oficiales usados",
  glue("- raw_exact: `{f_raw_exact}`"),
  glue("- raw_tagged: `{f_raw_tagged}`"),
  glue("- crosswalk estructural: `{f_crosswalk}`"),
  glue("- perfil de calidad: `{f_var_quality}`"),
  glue("- perfil de dominios armonizados previo: `{f_harm_domain_profile}`"),
  glue("- disponibilidad estructural para indicadores: `{f_quality_indicator_avail}`"),
  glue("- audit log oficial: `{f_data_audit_log}`"),
  if (!is.na(master_variable_map_path)) glue("- MASTER_VARIABLE_MAP: `{master_variable_map_path}`") else "- MASTER_VARIABLE_MAP: no encontrado en rutas esperadas",
  if (!is.na(master_data_dictionary_path)) glue("- MASTER_DATA_DICTIONARY: `{master_data_dictionary_path}`") else "- MASTER_DATA_DICTIONARY: no encontrado en rutas esperadas",
  "",
  "## Resultado de esta fase",
  glue("- Filas en dataset armonizado ancho: {nrow(harmonized_wide)}"),
  glue("- Registros en dataset armonizado largo: {nrow(harmonized_long)}"),
  glue("- Variables armonizadas distintas: {n_distinct(harmonized_long$variable_harmonized)}"),
  glue("- Exclusiones registradas: {nrow(harmonization_exclusion_log)}"),
  glue("- Pendientes de revisión semántica: {nrow(harmonization_pending_semantic_review)}"),
  "",
  "## Reglas metodológicas respetadas",
  "- BASE #7 se preservó como `base_diagnostico` sin decodificación final.",
  "- RES, DEPTRES y PROVDIST se mantuvieron como familia separada.",
  "- PMSEQ, PMTOT y PMCOD se mantuvieron como familia separada.",
  "- `blank_col_2` y `673668` quedaron excluidas como anomalías estructurales oficiales.",
  "- No se calcularon todavía MV, DCO, PSU ni otros indicadores IARC.",
  "",
  "## Próximo paso sugerido",
  "Construir la revisión semántica/codificación local priorizando: BASE #7, ESTVIT, LATE 19, residencia y múltiple primario."
)

write_md_summary(out_summary_md, summary_lines)

# ------------------------------------------------------------
# 14) Mensajes finales
# ------------------------------------------------------------
cat("\n============================================\n")
cat("HARMONIZATION PIPELINE COMPLETADO\n")
cat("============================================\n")
cat(glue("wide harmonized : {out_harmonized_wide}\n"))
cat(glue("long harmonized : {out_harmonized_long}\n"))
cat(glue("action log      : {out_action_log}\n"))
cat(glue("exclusion log   : {out_exclusion_log}\n"))
cat(glue("dictionary      : {out_dictionary}\n"))
cat(glue("pending review  : {out_pending_semantic_review}\n"))
cat(glue("summary md      : {out_summary_md}\n"))
cat(glue("run metadata    : {out_run_metadata}\n"))
cat("============================================\n\n")

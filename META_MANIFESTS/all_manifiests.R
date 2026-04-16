suppressPackageStartupMessages({
  library(fs)
  library(stringr)
  library(readr)
  library(dplyr)
  library(tibble)
  library(tools)
})

# ============================================================
# 00_build_project_manifests.R
# Genera automáticamente:
# - META_MANIFESTS/MASTER_FILE_INDEX.csv
# - META_MANIFESTS/DATA_REGISTRY.csv
# - META_MANIFESTS/PROJECT_SOURCE_PRIORITY.csv
# ============================================================

# ------------------------------------------------------------
# 1) detectar raíz del proyecto por .Rproj
# ------------------------------------------------------------
find_project_root <- function(start = getwd()) {
  current <- path_abs(start)
  repeat {
    rproj_files <- dir_ls(
      path = current,
      regexp = "\\.Rproj$",
      type = "file",
      recurse = FALSE,
      fail = FALSE
    )
    if (length(rproj_files) > 0) return(current)
    
    parent <- path_dir(current)
    if (parent == current) stop("No se encontró ningún archivo .Rproj.")
    current <- parent
  }
}

project_root <- find_project_root()
out_dir <- path(project_root, "META_MANIFESTS")
dir_create(out_dir)

# ------------------------------------------------------------
# 2) reglas generales de exclusión
# ------------------------------------------------------------
exclude_named_dirs <- c(
  "old", "olds", "archive", "archives", "archived",
  "deprecated", "backup", "backups", "bak",
  "tmp", "temp", "trash", "attic", "legacy"
)

exclude_system_dirs <- c(
  ".git", ".Rproj.user", ".quarto", "_cache", "cache",
  "_site", "_book", "__pycache__", "node_modules",
  "renv/library", "renv/staging", "renv/python"
)

make_dir_regex <- function(x) {
  paste0("(^|/|\\\\)(", paste(x, collapse = "|"), ")(/|\\\\|$)")
}

exclude_dir_regex <- paste(
  make_dir_regex(exclude_named_dirs),
  make_dir_regex(exclude_system_dirs),
  sep = "|"
)

# ------------------------------------------------------------
# 3) helpers
# ------------------------------------------------------------
make_id_from_name <- function(x) {
  x %>%
    tolower() %>%
    file_path_sans_ext() %>%
    basename() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

safe_rel <- function(x, root) {
  path_rel(x, start = root)
}

infer_load_instruction <- function(path_relative, file_ext) {
  case_when(
    file_ext == "csv" ~ paste0("readr::read_csv('", path_relative, "')"),
    file_ext == "tsv" ~ paste0("readr::read_tsv('", path_relative, "')"),
    file_ext == "txt" ~ paste0("readr::read_delim('", path_relative, "', delim = '\\t')"),
    file_ext %in% c("xlsx", "xls") ~ paste0("readxl::read_excel('", path_relative, "')"),
    file_ext == "ods" ~ paste0("readODS::read_ods('", path_relative, "')"),
    file_ext == "parquet" ~ paste0("arrow::read_parquet('", path_relative, "')"),
    file_ext == "feather" ~ paste0("arrow::read_feather('", path_relative, "')"),
    file_ext == "fst" ~ paste0("fst::read_fst('", path_relative, "')"),
    file_ext == "qs" ~ paste0("qs::qread('", path_relative, "')"),
    file_ext == "rds" ~ paste0("readRDS('", path_relative, "')"),
    file_ext %in% c("rda", "rdata") ~ paste0("load('", path_relative, "')"),
    TRUE ~ ""
  )
}

# ------------------------------------------------------------
# 4) listar archivos del proyecto
# ------------------------------------------------------------
all_files <- dir_ls(
  path = project_root,
  recurse = TRUE,
  type = "file",
  all = FALSE,
  fail = FALSE
)

all_files <- all_files[!str_detect(all_files, regex(exclude_dir_regex, ignore_case = TRUE))]

file_tbl <- tibble(
  path_absolute = all_files,
  path_relative = safe_rel(all_files, project_root),
  dir_relative = path_dir(safe_rel(all_files, project_root)),
  file_name = path_file(all_files),
  file_ext = str_to_lower(path_ext(all_files))
)

# ============================================================
# A) MASTER_FILE_INDEX.csv
# ============================================================

code_exts <- c("r", "json", "yml", "yaml", "qmd", "md", "txt", "toml", "ini", "css", "scss")

master_file_index <- file_tbl %>%
  filter(file_ext %in% code_exts) %>%
  mutate(
    file_type = case_when(
      file_ext %in% c("yml", "yaml") ~ "yaml",
      file_ext == "json" ~ "json",
      file_ext == "r" ~ "r",
      file_ext == "qmd" ~ "qmd",
      file_ext == "md" ~ "md",
      TRUE ~ file_ext
    ),
    role = case_when(
      str_detect(path_relative, regex("(^|/|\\\\)_quarto\\.yml$", ignore_case = TRUE)) ~ "quarto_root_config",
      str_detect(path_relative, regex("(^|/|\\\\)_metadata\\.yml$", ignore_case = TRUE)) ~ "quarto_metadata",
      str_detect(path_relative, regex("(^|/|\\\\)(config|metadata)(/|\\\\)", ignore_case = TRUE)) &
        file_type == "yaml" ~ "project_config_yaml",
      str_detect(path_relative, regex("(^|/|\\\\)(config|metadata)(/|\\\\)", ignore_case = TRUE)) &
        file_type == "json" ~ "project_config_json",
      str_detect(path_relative, regex("(^|/|\\\\)inst(/|\\\\)schema(/|\\\\)", ignore_case = TRUE)) &
        file_type == "json" ~ "schema_json",
      str_detect(file_name, regex("^MASTER_", ignore_case = TRUE)) ~ "master_governance_doc",
      str_detect(path_relative, regex("(^|/|\\\\)(R|SCRIPTS|scripts)(/|\\\\)", ignore_case = TRUE)) &
        str_detect(file_name, regex("(main|run|pipeline|build|master|workflow|orchestr)", ignore_case = TRUE)) ~ "pipeline_orchestrator",
      str_detect(path_relative, regex("(^|/|\\\\)(R|SCRIPTS|scripts)(/|\\\\)", ignore_case = TRUE)) &
        str_detect(file_name, regex("^(00|01|02)_", ignore_case = TRUE)) ~ "early_core_script",
      file_type == "r" ~ "script",
      file_type == "yaml" ~ "yaml_support",
      file_type == "json" ~ "json_support",
      file_type == "qmd" ~ "analysis_source",
      file_type == "md" ~ "documentation",
      TRUE ~ "other"
    ),
    canonical = case_when(
      role %in% c(
        "quarto_root_config", "quarto_metadata",
        "project_config_yaml", "project_config_json",
        "schema_json", "master_governance_doc",
        "pipeline_orchestrator", "early_core_script"
      ) ~ TRUE,
      TRUE ~ FALSE
    ),
    canonical_reason = case_when(
      role == "quarto_root_config" ~ "Configuración raíz de Quarto.",
      role == "quarto_metadata" ~ "Metadatos globales de Quarto.",
      role == "project_config_yaml" ~ "Configuración estructural YAML.",
      role == "project_config_json" ~ "Configuración estructural JSON.",
      role == "schema_json" ~ "Esquema JSON estructural.",
      role == "master_governance_doc" ~ "Documento maestro explícito.",
      role == "pipeline_orchestrator" ~ "Script orquestador del pipeline.",
      role == "early_core_script" ~ "Script núcleo temprano.",
      TRUE ~ ""
    ),
    priority_rank = case_when(
      role %in% c("quarto_root_config", "quarto_metadata") ~ 1L,
      role %in% c("project_config_yaml", "project_config_json", "schema_json", "master_governance_doc") ~ 2L,
      role %in% c("pipeline_orchestrator", "early_core_script") ~ 3L,
      file_type == "yaml" ~ 4L,
      file_type == "json" ~ 5L,
      file_type == "r" ~ 6L,
      TRUE ~ 9L
    ),
    included_in_index = TRUE,
    notes = ""
  ) %>%
  select(
    file_type,
    path_relative,
    file_name,
    included_in_index,
    canonical,
    priority_rank,
    role,
    canonical_reason,
    notes
  ) %>%
  arrange(desc(canonical), priority_rank, file_type, path_relative)

write_csv(master_file_index, path(out_dir, "MASTER_FILE_INDEX.csv"))

# ============================================================
# B) DATA_REGISTRY.csv
# ============================================================

data_exts <- c(
  "csv", "tsv", "txt", "xlsx", "xls", "ods",
  "parquet", "feather", "fst", "qs", "rds", "rda", "rdata"
)

data_registry <- file_tbl %>%
  filter(file_ext %in% data_exts) %>%
  mutate(
    in_data_raw = str_detect(path_relative, regex("(^|/|\\\\)DATA(/|\\\\)RAW(/|\\\\)", ignore_case = TRUE)),
    in_data_derived = str_detect(path_relative, regex("(^|/|\\\\)DATA(/|\\\\)DERIVED(/|\\\\)", ignore_case = TRUE)),
    in_reports = str_detect(path_relative, regex("(^|/|\\\\)REPORTS(/|\\\\)", ignore_case = TRUE)),
    in_reports_metadata = str_detect(path_relative, regex("(^|/|\\\\)REPORTS(/|\\\\)METADATA(/|\\\\)", ignore_case = TRUE)),
    in_data_metadata = str_detect(path_relative, regex("(^|/|\\\\)DATA(/|\\\\)DERIVED(/|\\\\)METADATA(/|\\\\)", ignore_case = TRUE)),
    in_meta_manifests = str_detect(path_relative, regex("(^|/|\\\\)META_MANIFESTS(/|\\\\)", ignore_case = TRUE)),
    is_master = str_detect(file_name, regex("^MASTER_|_MASTER_|dictionary|diccionario|manifest|registry|crosswalk|map", ignore_case = TRUE)),
    data_level = case_when(
      in_meta_manifests ~ "metadata",
      in_reports_metadata ~ "metadata",
      in_data_metadata ~ "metadata",
      in_data_raw ~ "raw",
      in_data_derived & is_master ~ "metadata",
      in_data_derived ~ "derived",
      in_reports & is_master ~ "metadata",
      in_reports ~ "report_output",
      TRUE ~ "other_data"
    ),
    role = case_when(
      in_meta_manifests ~ "manifest",
      str_detect(file_name, regex("MASTER_VARIABLE_MAP", ignore_case = TRUE)) ~ "master_variable_map",
      str_detect(file_name, regex("MASTER_DATA_DICTIONARY", ignore_case = TRUE)) ~ "master_data_dictionary",
      str_detect(file_name, regex("dictionary|diccionario", ignore_case = TRUE)) ~ "dictionary",
      str_detect(file_name, regex("crosswalk", ignore_case = TRUE)) ~ "crosswalk",
      in_data_raw ~ "input_source",
      in_data_derived ~ "derived_dataset",
      in_reports ~ "report_table",
      TRUE ~ "other"
    ),
    should_be_in_github = case_when(
      in_meta_manifests ~ TRUE,
      in_reports_metadata ~ TRUE,
      in_data_metadata ~ TRUE,
      is_master ~ TRUE,
      TRUE ~ FALSE
    ),
    expected_in_project_sources = case_when(
      should_be_in_github ~ FALSE,
      data_level %in% c("raw", "derived", "report_output", "other_data") ~ TRUE,
      TRUE ~ FALSE
    ),
    source_of_truth = case_when(
      in_meta_manifests ~ TRUE,
      in_reports_metadata ~ TRUE,
      in_data_metadata ~ TRUE,
      is_master ~ TRUE,
      in_data_raw ~ TRUE,
      TRUE ~ FALSE
    ),
    dataset_id = make_id_from_name(file_name),
    load_instructions = infer_load_instruction(path_relative, file_ext),
    notes = case_when(
      should_be_in_github ~ "Dataset maestro/metadata estructural: versionar en GitHub.",
      expected_in_project_sources ~ "No versionar en GitHub; subir a Fuentes del proyecto si se necesita en ChatGPT.",
      TRUE ~ ""
    )
  ) %>%
  select(
    dataset_id,
    path_relative,
    file_name,
    file_ext,
    data_level,
    role,
    is_master,
    should_be_in_github,
    expected_in_project_sources,
    source_of_truth,
    load_instructions,
    notes
  ) %>%
  arrange(desc(source_of_truth), desc(is_master), data_level, path_relative)

write_csv(data_registry, path(out_dir, "DATA_REGISTRY.csv"))

# ============================================================
# C) PROJECT_SOURCE_PRIORITY.csv
# ============================================================

project_source_priority <- tribble(
  ~source_type,              ~priority, ~applies_when,                                                          ~selection_rule,                                                                 ~fallback_rule,                                                                ~notes,
  "project_sources",         1L,        "Dataset o archivo no público subido a Fuentes del proyecto",           "Usar primero esta fuente si el archivo está disponible en el chat/proyecto.",   "Si no está disponible, revisar GitHub o usar la ruta esperada del manifiesto.", "Prioridad máxima para datos no públicos.",
  "github_repo",             2L,        "Código, manifiestos, YAML/JSON y CSV maestros versionados",           "Usar el repositorio GitHub conectado como fuente principal estructural.",        "Si no existe en GitHub, revisar Fuentes del proyecto o ruta esperada.",         "Fuente principal para código y metadata canónica.",
  "expected_project_path",   3L,        "Archivo esperado por estructura del proyecto pero no accesible aquí",  "No inventar contenido; usar path_relative y load_instructions para generar código.", "Esperar a que el usuario suba el archivo o trabajar con mocks/estructura.",   "Fallback estructural reproducible."
)

write_csv(project_source_priority, path(out_dir, "PROJECT_SOURCE_PRIORITY.csv"))

# ============================================================
# D) TXT resumen opcional
# ============================================================

write_lines(
  c(
    "PROJECT MANIFESTS GENERATED",
    paste0("ROOT_RPROJ: ", project_root),
    paste0("GENERATED_AT: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    paste0("MASTER_FILE_INDEX.csv: ", nrow(master_file_index), " registros"),
    paste0("DATA_REGISTRY.csv: ", nrow(data_registry), " registros"),
    paste0("PROJECT_SOURCE_PRIORITY.csv: ", nrow(project_source_priority), " reglas")
  ),
  path(out_dir, "MANIFEST_BUILD_SUMMARY.txt")
)

message("Generados en: ", out_dir)
message("- MASTER_FILE_INDEX.csv")
message("- DATA_REGISTRY.csv")
message("- PROJECT_SOURCE_PRIORITY.csv")
message("- MANIFEST_BUILD_SUMMARY.txt")
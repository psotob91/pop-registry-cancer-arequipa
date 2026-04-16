library(fs)
library(stringr)
library(readr)
library(dplyr)
library(tibble)

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
    
    if (length(rproj_files) > 0) {
      return(current)
    }
    
    parent <- path_dir(current)
    if (parent == current) {
      stop("No se encontró ningún archivo .Rproj subiendo desde: ", start)
    }
    current <- parent
  }
}

project_root <- find_project_root()
out_dir <- path(project_root, "META_MANIFESTS")
dir_create(out_dir)

# ------------------------------------------------------------
# Reglas de exclusión por carpeta
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

exclude_data_output_dirs <- c(
  "DATA", "data", "raw", "derived", "intermediate",
  "output", "outputs", "results", "figures", "plots",
  "tables", "exports", "artifacts"
)

make_dir_regex <- function(x) {
  paste0("(^|/|\\\\)(", paste(x, collapse = "|"), ")(/|\\\\|$)")
}

exclude_dir_regex <- paste(
  make_dir_regex(exclude_named_dirs),
  make_dir_regex(exclude_system_dirs),
  make_dir_regex(exclude_data_output_dirs),
  sep = "|"
)

# ------------------------------------------------------------
# Listado de archivos
# ------------------------------------------------------------

all_files <- dir_ls(
  path = project_root,
  recurse = TRUE,
  type = "file",
  all = FALSE,
  fail = FALSE
)

all_files <- all_files[!str_detect(all_files, regex(exclude_dir_regex, ignore_case = TRUE))]

manifest <- tibble(
  path_absolute = all_files,
  path_relative = path_rel(all_files, start = project_root),
  dir_relative = path_dir(path_rel(all_files, start = project_root)),
  file_name = path_file(all_files),
  file_ext = str_to_lower(path_ext(all_files))
) %>%
  filter(file_ext %in% c("r", "json", "yml", "yaml"))

# ------------------------------------------------------------
# Tipo de archivo
# ------------------------------------------------------------

manifest <- manifest %>%
  mutate(
    file_type = case_when(
      file_ext %in% c("yml", "yaml") ~ "yaml",
      file_ext == "json" ~ "json",
      file_ext == "r" ~ "r",
      TRUE ~ file_ext
    )
  )

# ------------------------------------------------------------
# Reglas heurísticas de rol / canonicidad
# ------------------------------------------------------------

manifest <- manifest %>%
  mutate(
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
      role == "project_config_json" ~ "Configuración o diccionario JSON.",
      role == "schema_json" ~ "Esquema JSON estructural.",
      role == "master_governance_doc" ~ "Documento maestro explícito.",
      role == "pipeline_orchestrator" ~ "Script orquestador del pipeline.",
      role == "early_core_script" ~ "Script núcleo temprano del pipeline.",
      TRUE ~ ""
    )
  )

# ------------------------------------------------------------
# Ranking de prioridad
# ------------------------------------------------------------

manifest <- manifest %>%
  mutate(
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
    excluded_because = ""
  )

# ------------------------------------------------------------
# Salida final
# ------------------------------------------------------------

manifest_out <- manifest %>%
  select(
    file_type,
    path_relative,
    file_name,
    included_in_index,
    canonical,
    priority_rank,
    role,
    canonical_reason,
    excluded_because
  ) %>%
  arrange(desc(canonical), priority_rank, file_type, path_relative)

write_csv(manifest_out, path(out_dir, "MASTER_FILE_INDEX.csv"))

txt_lines <- c(
  "MASTER FILE INDEX",
  paste0("ROOT_RPROJ: ", project_root),
  paste0("GENERATED_AT: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("N_INDEXED: ", nrow(manifest_out)),
  paste0("N_CANONICAL: ", sum(manifest_out$canonical)),
  "",
  "=== CANONICAL FILES ==="
)

canonical_lines <- manifest_out %>%
  filter(canonical) %>%
  transmute(line = paste0(
    "[rank=", priority_rank, "] ",
    file_type, " | ",
    path_relative, " | role=", role,
    ifelse(canonical_reason != "", paste0(" | reason=", canonical_reason), "")
  )) %>%
  pull(line)

other_lines <- c("", "=== ALL OTHER INDEXED FILES ===")

noncanonical_lines <- manifest_out %>%
  filter(!canonical) %>%
  transmute(line = paste0(
    "[rank=", priority_rank, "] ",
    file_type, " | ",
    path_relative, " | role=", role
  )) %>%
  pull(line)

write_lines(
  c(txt_lines, canonical_lines, other_lines, noncanonical_lines),
  path(out_dir, "MASTER_FILE_INDEX.txt")
)

message("Archivo generado: ", path(out_dir, "MASTER_FILE_INDEX.csv"))
message("Archivo generado: ", path(out_dir, "MASTER_FILE_INDEX.txt"))
#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tibble)
  library(fs)
  library(cli)
  library(dplyr)
})

source("SCRIPTS/pipeline_runtime_helpers.R")

root <- find_project_root()
setwd(root)

dry_run <- tolower(Sys.getenv("CLEAN_DRY_RUN", unset = "true")) != "false"
confirm <- Sys.getenv("CLEAN_CONFIRM", unset = "")

collect_existing <- function(paths) {
  unique(paths[file_exists(paths) | dir_exists(paths)])
}

dir_ls_safe <- function(path, ...) {
  if (!dir_exists(path)) return(character())
  dir_ls(path, ..., fail = FALSE)
}

file_targets <- collect_existing(c(
  dir_ls_safe(project_path(root, "DATA", "DERIVED", "ANALYTIC"), recurse = FALSE, type = "file"),
  dir_ls_safe(project_path(root, "DATA", "DERIVED", "QC"), recurse = TRUE, type = "file"),
  dir_ls_safe(project_path(root, "DATA", "DERIVED", "POPULATION"), recurse = TRUE, type = "file"),
  dir_ls_safe(project_path(root, "DATA", "DERIVED", "RATES"), recurse = TRUE, type = "file"),
  dir_ls(project_path(root, "REPORTS"), regexp = "auditoria_integral_rcbpa_arequipa\\.(html|pdf)$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "baseline_contract_comparison.*\\.csv$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "qc_registry_minimo_arequipa\\.pdf$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "cleanup_targets_manifest\\.csv$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "preflight_checks\\.csv$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "annex_.*\\.csv$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "data_.*\\.(csv|json)$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "date_field_format_profile\\.csv$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "domain_profile_.*\\.csv$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "harmonization_.*\\.(csv|json|md)$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "local_.*\\.csv$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "quality_indicator_field_availability\\.csv$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "semantic_.*\\.(csv|json|md)$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "value_frequency_.*\\.csv$", fail = FALSE),
  dir_ls(project_path(root, "REPORTS"), regexp = "variable_.*\\.csv$", fail = FALSE),
  dir_ls_safe(project_path(root, "REPORTS", "pipeline_runs"), recurse = TRUE, type = "file"),
  dir_ls_safe(project_path(root, "REPORTS", "portal_rcbpa"), recurse = TRUE, type = "file"),
  dir_ls_safe(project_path(root, "REPORTS", "FIGURES"), recurse = TRUE, type = "file"),
  dir_ls_safe(project_path(root, "REPORTS", "QC"), recurse = TRUE, type = "file"),
  dir_ls_safe(project_path(root, "REPORTS", "SEMANTIC"), recurse = TRUE, type = "file"),
  dir_ls_safe(project_path(root, "REPORTS", "portal_rcbpa_src", ".quarto"), recurse = TRUE, type = "file")
))

dir_targets <- collect_existing(c(
  project_path(root, "REPORTS", "portal_rcbpa"),
  project_path(root, "REPORTS", "FIGURES"),
  project_path(root, "REPORTS", "pipeline_runs"),
  project_path(root, "REPORTS", "auditoria_integral_rcbpa_arequipa_files"),
  project_path(root, "REPORTS", "portal_rcbpa_src", ".quarto")
))

manifest_path <- project_path(root, "REPORTS", "cleanup_targets_manifest.csv")
write_csv_utf8(
  tibble(
    path = c(file_targets, dir_targets),
    target_type = c(rep("file", length(file_targets)), rep("dir", length(dir_targets)))
  ),
  manifest_path
)

if (dry_run || confirm != "YES") {
  cli::cli_alert_info("Dry run de limpieza completado. Define CLEAN_DRY_RUN=false y CLEAN_CONFIRM=YES para borrar.")
  quit(save = "no", status = 0L)
}

purrr::walk(file_targets, ~ if (file_exists(.x)) fs::file_delete(.x))
purrr::walk(dir_targets, ~ if (dir_exists(.x)) fs::dir_delete(.x))

residual_targets <- tibble(
  path = c(file_targets, dir_targets),
  exists_after_clean = file_exists(c(file_targets, dir_targets)) | dir_exists(c(file_targets, dir_targets))
) %>%
  filter(exists_after_clean)

if (nrow(residual_targets) > 0) {
  cli::cli_abort(c(
    "Persisten artefactos regenerables despues de la limpieza.",
    i = paste(residual_targets$path, collapse = "\n")
  ))
}

cli::cli_alert_success(glue::glue("Se eliminaron {length(file_targets)} archivos y {length(dir_targets)} directorios regenerables."))

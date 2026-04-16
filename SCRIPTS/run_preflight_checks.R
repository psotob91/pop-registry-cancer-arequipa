#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
  library(fs)
  library(cli)
})

source("SCRIPTS/pipeline_runtime_helpers.R")

root <- find_project_root()
setwd(root)

required_packages <- c(
  "tidyverse", "readxl", "janitor", "lubridate", "jsonlite",
  "yaml", "xml2", "rvest", "pdftools", "kableExtra", "glue", "cli", "fs"
)

package_checks <- tibble(
  check_type = "package",
  item = required_packages,
  status = vapply(required_packages, requireNamespace, logical(1), quietly = TRUE),
  details = ifelse(status, "installed", "missing")
)

required_paths <- c(
  "DATA/RAW/RCBPAQP 2015-2022.xlsx",
  "DATA/RAW/SHARED/9.- Principales_Indicadores_AREQUIPA_INEI.XLS",
  "DATA/RAW/SHARED/Compendio Estadístico, Arequipa 2022.pdf",
  "DOCUMENTATION/MASTER/MASTER_DENOMINATOR_AND_STANDARDIZATION_RULES.md",
  "DATA/DERIVED/METADATA/MASTER_ANALYTIC_DATASET_SPEC.csv",
  "DATA/DERIVED/METADATA/MASTER_DENOMINATOR_RULES.csv",
  "DATA/DERIVED/METADATA/MASTER_RATE_SPEC.csv",
  "config/pipeline_steps.csv",
  "config/pipeline_profiles.yml"
)

path_checks <- validate_required_paths(project_path(root, required_paths)) %>%
  mutate(
    check_type = "path",
    item = required_paths,
    status = exists,
    details = ifelse(status, "present", "missing")
  ) %>%
  select(check_type, item, status, details)

quarto_check <- safe_shell("quarto", "--version", wd = root)
rscript_check <- safe_shell("Rscript", "--version", wd = root)

runtime_checks <- tibble(
  check_type = "runtime",
  item = c("quarto", "Rscript"),
  status = c(quarto_check$status == 0L, rscript_check$status == 0L),
  details = c(paste(quarto_check$output, collapse = " "), paste(rscript_check$output, collapse = " "))
)

results <- bind_rows(package_checks, path_checks, runtime_checks) %>%
  mutate(run_date = as.character(Sys.time()))

write_csv_utf8(results, project_path(root, "REPORTS", "preflight_checks.csv"))

if (all(results$status)) {
  cli::cli_alert_success("Preflight completado: todas las verificaciones pasaron.")
} else {
  cli::cli_alert_warning("Preflight detecto elementos faltantes. Revisa REPORTS/preflight_checks.csv.")
  print(results %>% filter(!status))
}

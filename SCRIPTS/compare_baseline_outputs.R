#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(fs)
  library(readr)
  library(dplyr)
  library(cli)
  library(glue)
})

source("SCRIPTS/pipeline_runtime_helpers.R")

root <- find_project_root()
setwd(root)

args <- commandArgs(trailingOnly = TRUE)
baseline_dir <- if (length(args) >= 1) args[[1]] else NA_character_
snapshot_root <- project_path(root, "DATA", "DERIVED", "QC")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

current_manifest <- snapshot_contract_outputs(contract_output_paths(root), project_path(snapshot_root, glue("baseline_contract_current_{timestamp}")))

if (is.na(baseline_dir)) {
  new_baseline_dir <- project_path(snapshot_root, glue("baseline_contract_{timestamp}"))
  snapshot_contract_outputs(contract_output_paths(root), new_baseline_dir)
  cli::cli_alert_success(glue("Baseline creado en {new_baseline_dir}"))
  quit(save = "no", status = 0L)
}

baseline_manifest_path <- project_path(root, baseline_dir, "contract_manifest.csv")
if (!file_exists(baseline_manifest_path)) cli::cli_abort(glue("No existe el manifest baseline: {baseline_manifest_path}"))

baseline_manifest <- readr::read_csv(baseline_manifest_path, show_col_types = FALSE)
comparison <- compare_contract_manifests(baseline_manifest, current_manifest)
comparison_path <- project_path(root, "REPORTS", glue("baseline_contract_comparison_{timestamp}.csv"))
write_csv_utf8(comparison, comparison_path)

cli::cli_alert_success(glue("Comparacion baseline escrita en {comparison_path}"))

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fs)
  library(cli)
  library(glue)
})

source("SCRIPTS/pipeline_runtime_helpers.R")

root <- find_project_root()
setwd(root)

args <- commandArgs(trailingOnly = TRUE)
profile <- "full"
clean_first <- FALSE
iteration_id <- format(Sys.time(), "%Y%m%d_%H%M%S")

if ("--profile" %in% args) profile <- args[match("--profile", args) + 1]
clean_first <- "--clean-first" %in% args

profiles <- read_pipeline_profiles(root)
steps <- read_pipeline_steps(root)

if (!profile %in% names(profiles$profiles)) cli::cli_abort(glue("Perfil no reconocido: {profile}"))

include_cols <- profiles$profiles[[profile]]$include_columns
step_tbl <- steps %>%
  filter(if_all(any_of(include_cols), ~ .x %in% TRUE)) %>%
  arrange(step_order)

run_log_dir <- project_path(root, "REPORTS", "pipeline_runs")
ensure_dirs(run_log_dir)
run_log_path <- project_path(run_log_dir, glue("pipeline_run_{profile}_{iteration_id}.log"))

if (clean_first) {
  clean_res <- safe_shell("Rscript", c("SCRIPTS/clean_regenerable_outputs.R"), wd = root, env = c("CLEAN_DRY_RUN=false", "CLEAN_CONFIRM=YES"))
  writeLines(clean_res$output, run_log_path)
  if (clean_res$status != 0L) cli::cli_abort("La limpieza previa fallo.")
}

log_lines <- c(glue("Pipeline profile: {profile}"), glue("Iteration: {iteration_id}"), "")

for (i in seq_len(nrow(step_tbl))) {
  step <- step_tbl[i, ]
  cli::cli_alert_info(glue("Ejecutando {step$step_id}: {step$script_path_canonical}"))
  res <- safe_shell("Rscript", step$script_path_canonical, wd = root)
  log_lines <- c(log_lines, glue("===== {step$step_id} ====="), res$output, "")
  writeLines(log_lines, run_log_path)
  if (res$status != 0L && isTRUE(step$blocking)) {
    cli::cli_abort(glue("Fallo el paso bloqueante {step$step_id}. Ver {run_log_path}"))
  }
}

cli::cli_alert_success(glue("Pipeline {profile} completado. Log: {run_log_path}"))

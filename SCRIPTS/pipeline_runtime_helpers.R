#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(fs)
  library(readr)
  library(tibble)
  library(dplyr)
  library(stringr)
  library(cli)
  library(glue)
  library(purrr)
  library(yaml)
})

find_project_root <- function(start = getwd(), max_up = 8L) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  has_markers <- function(path) {
    fs::dir_exists(fs::path(path, ".git")) ||
      fs::file_exists(fs::path(path, "META_MANIFESTS", "MASTER_FILE_INDEX.csv")) ||
      fs::file_exists(fs::path(path, "DATA", "DERIVED", "METADATA", "MASTER_ANALYTIC_DATASET_SPEC.csv"))
  }
  for (i in 0:max_up) {
    if (has_markers(current)) return(current)
    parent <- fs::path_dir(current)
    if (identical(parent, current)) break
    current <- parent
  }
  normalizePath(start, winslash = "/", mustWork = TRUE)
}

project_path <- function(root = find_project_root(), ...) {
  fs::path(root, ...)
}

ensure_dirs <- function(...) {
  paths <- c(...)
  purrr::walk(paths, fs::dir_create)
  invisible(paths)
}

write_csv_utf8 <- function(data, path) {
  fs::dir_create(fs::path_dir(path))
  readr::write_csv(data, path, na = "")
  invisible(path)
}

read_csv_if_exists <- function(path, ...) {
  if (!fs::file_exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE, ...)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_shell <- function(command, args = character(), wd = find_project_root(), env = character()) {
  result <- suppressWarnings(system2(command, args = args, stdout = TRUE, stderr = TRUE, wait = TRUE, env = env))
  status <- attr(result, "status") %||% 0L
  list(status = status, output = result)
}

snapshot_contract_outputs <- function(paths, snapshot_dir) {
  fs::dir_create(snapshot_dir, recurse = TRUE)
  manifest <- tibble(path = paths, exists = fs::file_exists(paths)) %>%
    mutate(
      size_bytes = ifelse(exists, as.numeric(fs::file_info(path)$size), NA_real_),
      modified_time = ifelse(exists, as.character(fs::file_info(path)$modification_time), NA_character_)
    )
  write_csv_utf8(manifest, fs::path(snapshot_dir, "contract_manifest.csv"))
  manifest
}

compare_contract_manifests <- function(baseline_manifest, current_manifest) {
  baseline_manifest %>%
    rename_with(~ paste0("baseline_", .x), -path) %>%
    full_join(current_manifest %>% rename_with(~ paste0("current_", .x), -path), by = "path") %>%
    mutate(
      match_exists = baseline_exists == current_exists,
      match_size = dplyr::coalesce(baseline_size_bytes, -1) == dplyr::coalesce(current_size_bytes, -1),
      status = case_when(
        isTRUE(match_exists) && isTRUE(match_size) ~ "match",
        isTRUE(current_exists) && !isTRUE(baseline_exists) ~ "new",
        !isTRUE(current_exists) && isTRUE(baseline_exists) ~ "missing",
        TRUE ~ "changed"
      )
    )
}

contract_output_paths <- function(root = find_project_root()) {
  c(
    project_path(root, "DATA", "DERIVED", "ANALYTIC", "rcpa_arequipa_2015_2022_analytic_dataset.rds"),
    project_path(root, "DATA", "DERIVED", "ANALYTIC", "rcpa_arequipa_2015_2022_analytic_dataset.csv"),
    project_path(root, "DATA", "DERIVED", "QC", "qc_epidemiologic_summary_flat.csv"),
    project_path(root, "DATA", "DERIVED", "POPULATION", "arequipa_province_population_denominators.csv"),
    project_path(root, "DATA", "DERIVED", "RATES", "incidence_rates_age_specific.csv"),
    project_path(root, "DATA", "DERIVED", "RATES", "incidence_rates_asr_period.csv"),
    project_path(root, "DATA", "DERIVED", "RATES", "mortality_rates_age_specific.csv"),
    project_path(root, "DATA", "DERIVED", "RATES", "mortality_rates_asr_period.csv"),
    project_path(root, "DATA", "DERIVED", "QC", "qc_population_denominators.csv"),
    project_path(root, "DATA", "DERIVED", "QC", "qc_rates_consistency.csv"),
    project_path(root, "REPORTS", "auditoria_integral_rcbpa_arequipa.html"),
    project_path(root, "REPORTS", "portal_rcbpa", "index.html")
  )
}

read_pipeline_profiles <- function(root = find_project_root()) {
  yaml::read_yaml(project_path(root, "config", "pipeline_profiles.yml"))
}

read_pipeline_steps <- function(root = find_project_root()) {
  readr::read_csv(project_path(root, "config", "pipeline_steps.csv"), show_col_types = FALSE)
}

validate_required_paths <- function(paths) {
  tibble(path = paths, exists = fs::file_exists(paths))
}

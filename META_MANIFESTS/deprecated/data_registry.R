library(fs)
library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(tools)

find_project_root <- function(start = getwd()) {
  current <- path_abs(start)
  repeat {
    rproj_files <- dir_ls(current, regexp = "\\.Rproj$", type = "file", recurse = FALSE, fail = FALSE)
    if (length(rproj_files) > 0) return(current)
    parent <- path_dir(current)
    if (parent == current) stop("No se encontró ningún .Rproj")
    current <- parent
  }
}

project_root <- find_project_root()
out_dir <- path(project_root, "META_MANIFESTS")
dir_create(out_dir)

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

data_exts <- c(
  "csv", "tsv", "txt", "xlsx", "xls", "ods",
  "parquet", "feather", "fst", "qs", "rds", "rda", "rdata"
)

all_files <- dir_ls(project_root, recurse = TRUE, type = "file", all = FALSE, fail = FALSE)
all_files <- all_files[!str_detect(all_files, regex(exclude_dir_regex, ignore_case = TRUE))]

make_dataset_id <- function(x) {
  x %>%
    tolower() %>%
    file_path_sans_ext() %>%
    basename() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

reg <- tibble(
  path_absolute = all_files,
  path_relative = path_rel(all_files, start = project_root),
  file_name = path_file(all_files),
  file_ext = str_to_lower(path_ext(all_files))
) %>%
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
    dataset_id = make_dataset_id(file_name),
    load_instructions = case_when(
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
    ),
    notes = case_when(
      should_be_in_github ~ "CSV/dataset maestro o metadata estructural: versionar en GitHub.",
      expected_in_project_sources ~ "No versionar en GitHub; subir a Fuentes del proyecto si se necesita en ChatGPT.",
      TRUE ~ ""
    )
  ) %>%
  select(
    dataset_id, path_relative, file_name, file_ext, data_level, role,
    is_master, should_be_in_github, expected_in_project_sources,
    source_of_truth, load_instructions, notes
  ) %>%
  arrange(desc(source_of_truth), desc(is_master), data_level, path_relative)

write_csv(reg, path(out_dir, "DATA_REGISTRY.csv"))
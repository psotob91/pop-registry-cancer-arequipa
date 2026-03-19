suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(glue)
  library(here)
  library(jsonlite)
  library(lubridate)
})

# ============================================================
# 04_semantic_standardization.R (v4.1)
# Mejora: focalización + deduplicación clusters + priorización
# ============================================================

# ------------------------------------------------------------
# 0) Configuración
# ------------------------------------------------------------
root <- here::here()
run_date <- Sys.Date()
run_stamp <- format(run_date, "%Y%m%d")

out_dir <- file.path(root, "REPORTS", "SEMANTIC")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

CFG <- list(
  promote_provisional_to_safe = FALSE,
  promote_vars = c("sexo", "estado_vital", "lateralidad"),
  top_n_cluster_examples = 5L,
  cumulative_review_target = 0.95,
  similarity_threshold = 0.80,
  min_cluster_size = 2L,
  focus_variables = NULL, # c("sexo","estado_vital","lateralidad","base_diagnostico")
  exclude_catalog_like_from_clustering = TRUE
)

# ------------------------------------------------------------
# Helpers (sin cambios relevantes)
# ------------------------------------------------------------
read_csv_safe <- function(path) readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

na_blank <- function(x) {
  x_chr <- as.character(x)
  x_chr <- stringr::str_squish(x_chr)
  x_chr[x_chr == ""] <- NA_character_
  x_chr
}

normalize_token <- function(x) {
  x %>% na_blank() %>% stringr::str_to_upper() %>% stringr::str_squish()
}

string_key <- function(x) normalize_token(x) %>% stringr::str_replace_all("[^A-Z0-9]", "")

classify_unknown_token <- function(x) {
  token <- normalize_token(x)
  token %in% c("NA","N/A","ND","NI","NE","NK","NR","NS","SD",
               "DESCONOCIDO","IGNORADO","SIN INFORMACION","NO INFORMA",
               "99","999","9999","88","888","77","777","00","000","0000")
}

priority_score_fn <- function(n, year_span, status) {
  w <- case_when(status == "desconocido" ~ 3,
                 status == "probable_revision" ~ 2,
                 TRUE ~ 1)
  n * pmax(year_span,1) * w
}

# ------------------------------------------------------------
# Lectura
# ------------------------------------------------------------
f_harmonized_wide <- file.path(root, "DATA", "DERIVED", "rcpa_arequipa_2015_2022_harmonized_wide.rds")
f_pending <- file.path(root, "REPORTS", "harmonization_pending_semantic_review.csv")
f_dict_sexo <- file.path(root, "REPORTS", "local_dictionary_sexo.csv")
f_dict_estado <- file.path(root, "REPORTS", "local_dictionary_estado_vital.csv")
f_dict_lateralidad <- file.path(root, "REPORTS", "local_dictionary_lateralidad.csv")
f_dict_base <- file.path(root, "REPORTS", "local_dictionary_base_diagnostico.csv")
f_unknown <- file.path(root, "REPORTS", "local_unknown_codes_registry.csv")

harmonized_wide <- readRDS(f_harmonized_wide)
pending <- read_csv_safe(f_pending)
unknown_registry <- read_csv_safe(f_unknown)

# ------------------------------------------------------------
# Variables objetivo
# ------------------------------------------------------------
target_vars <- pending %>% distinct(variable_harmonized)

if (!is.null(CFG$focus_variables)) {
  target_vars <- target_vars %>% filter(variable_harmonized %in% CFG$focus_variables)
}

vars <- intersect(target_vars$variable_harmonized, names(harmonized_wide))

working_long <- harmonized_wide %>%
  pivot_longer(cols = all_of(vars), names_to = "variable_harmonized", values_to = "value_raw") %>%
  mutate(
    value_raw = na_blank(value_raw),
    value_norm = normalize_token(value_raw),
    value_key = string_key(value_raw),
    is_missing = is.na(value_raw),
    is_unknown = classify_unknown_token(value_raw)
  )

# ------------------------------------------------------------
# Perfil
# ------------------------------------------------------------
profile <- working_long %>%
  group_by(variable_harmonized, value_raw, value_norm, value_key) %>%
  summarise(n = n(), .groups="drop")

# ------------------------------------------------------------
# Clasificación simplificada
# ------------------------------------------------------------
profile <- profile %>%
  mutate(
    suggested_label = case_when(
      is.na(value_norm) ~ "missing",
      value_norm %in% c("1","M","MALE") & variable_harmonized=="sexo" ~ "male",
      value_norm %in% c("2","F","FEMALE") & variable_harmonized=="sexo" ~ "female",
      TRUE ~ NA_character_
    ),
    confidence = case_when(
      is.na(value_norm) ~ "automatico_seguro",
      variable_harmonized %in% CFG$promote_vars & !is.na(suggested_label) ~ "probable_revision",
      TRUE ~ "desconocido"
    )
  )

# ------------------------------------------------------------
# Priorización
# ------------------------------------------------------------
profile <- profile %>%
  mutate(priority_score = priority_score_fn(n, 1, confidence)) %>%
  arrange(desc(priority_score))

write_csv(profile, file.path(out_dir, glue("semantic_value_profile_{run_stamp}.csv")))

# ------------------------------------------------------------
# Clustering CORREGIDO (sin duplicados)
# ------------------------------------------------------------
cluster_input <- profile %>% filter(confidence != "automatico_seguro")

if (CFG$exclude_catalog_like_from_clustering) {
  cluster_input <- cluster_input %>% filter(!str_detect(variable_harmonized, "^residencia__"))
}

clusters <- cluster_input %>%
  group_by(variable_harmonized) %>%
  summarise(
    n_values = n_distinct(value_norm),
    total_freq = sum(n),
    example_values = paste(head(unique(value_raw),5), collapse=" | "),
    .groups="drop"
  )

write_csv(clusters, file.path(out_dir, glue("semantic_pattern_clusters_{run_stamp}.csv")))

# ------------------------------------------------------------
# Cobertura acumulada
# ------------------------------------------------------------
coverage <- profile %>%
  group_by(variable_harmonized) %>%
  arrange(desc(n)) %>%
  mutate(cum_prop = cumsum(n)/sum(n)) %>%
  filter(cum_prop <= CFG$cumulative_review_target)

write_csv(coverage, file.path(out_dir, glue("semantic_priority_coverage_{run_stamp}.csv")))

# ------------------------------------------------------------
# Mensaje final
# ------------------------------------------------------------
cat("\nFase 4.1 ejecutada correctamente\n")

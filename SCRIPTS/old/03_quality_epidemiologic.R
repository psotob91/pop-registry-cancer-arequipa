# ============================================================
# 03_quality_epidemiologic.R
# QC epidemiológica (indicadores tipo IARC/IACR para registro poblacional)
# Requiere: data/derived/rcpa_arequipa_2015_2022_clean.rds (desde 01_import_harmonize.R)
#
# Salidas:
# - REPORTS/qc_epi_results_<YYYYMMDD>.txt      (log legible)
# - REPORTS/qc_epi_tables_<YYYYMMDD>.rds       (lista con tablas)
# - REPORTS/qc_epi_summary_<YYYYMMDD>.csv      (resumen global)
# - REPORTS/qc_epi_by_year_<YYYYMMDD>.csv      (por año)
# - REPORTS/qc_basis_dx_freq_<YYYYMMDD>.csv    (frecuencias base_dx)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(glue)
  library(readr)
  library(tibble)
})

# ---- CONFIG ----
in_rds  <- "DATA/DERIVED/rcpa_arequipa_2015_2022_clean.rds"
out_dir <- "REPORTS"
tag     <- format(Sys.Date(), "%Y%m%d")

out_txt   <- file.path(out_dir, paste0("qc_epi_results_", tag, ".txt"))
out_rds   <- file.path(out_dir, paste0("qc_epi_tables_", tag, ".rds"))
out_sum   <- file.path(out_dir, paste0("qc_epi_summary_", tag, ".csv"))
out_year  <- file.path(out_dir, paste0("qc_epi_by_year_", tag, ".csv"))
out_basis <- file.path(out_dir, paste0("qc_basis_dx_freq_", tag, ".csv"))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load ----
dt_all <- readRDS(in_rds)

# ============================================================
# 1) Preparación: derivar año de incidencia (preferir fecdiag)
#    Si fecdiag es NA, usar year_sheet solo para tabulados descriptivos (no para tasas).
# ============================================================
dt <- dt_all %>%
  mutate(
    year_diag = if ("fecdiag" %in% names(.)) year(fecdiag) else NA_integer_,
    year_any  = if_else(!is.na(year_diag), year_diag,
                        if ("year_sheet" %in% names(.)) as.integer(year_sheet) else NA_integer_)
  )

# ============================================================
# 2) Normalización mínima de sexo y edad desconocidos
#    (se reporta % desconocido; no impone recodificación definitiva)
# ============================================================
dt <- dt %>%
  mutate(
    sexo_raw = if ("sexo" %in% names(.)) sexo else NA_character_,
    sexo_std = case_when(
      str_to_upper(str_trim(sexo_raw)) %in% c("M","MALE","H","HOMBRE","1") ~ "M",
      str_to_upper(str_trim(sexo_raw)) %in% c("F","FEMALE","MUJER","2")    ~ "F",
      TRUE ~ NA_character_
    ),
    sexo_unknown = is.na(sexo_std),
    
    edad_raw = if ("edad" %in% names(.)) edad else NA_integer_,
    edad_unknown = is.na(edad_raw) | edad_raw %in% c(999, 998)
  )

# ============================================================
# 3) Basis of diagnosis / verificación (para MV% y DCO%)
#    Se privilegia base_dx si existe; si no, se intenta con veri.
#
# Referencia de códigos clásicos (IARC/IACR/ENCR):
# 0 = DCO; 5 = Citología; 6 = Histología metástasis; 7 = Histología primario; 9 = Desconocido. :contentReference[oaicite:0]{index=0}
#
# MV% (microscópicamente verificado) suele agrupar 5-7. :contentReference[oaicite:1]{index=1}
# ============================================================

dt <- dt %>%
  mutate(
    base_dx_raw = if ("base_dx" %in% names(.)) base_dx else NA_character_,
    veri_raw    = if ("veri"    %in% names(.)) veri else NA_character_,
    
    # intentar extraer primer número (si viene como texto mixto)
    base_dx_num = suppressWarnings(as.integer(str_extract(str_trim(base_dx_raw), "\\d+"))),
    veri_num    = suppressWarnings(as.integer(str_extract(str_trim(veri_raw), "\\d+"))),
    
    # elegir fuente principal para BoD (si base_dx_num existe úsalo; sino veri_num)
    bod = coalesce(base_dx_num, veri_num),
    
    bod_unknown = is.na(bod) | bod == 9,
    bod_dco     = bod == 0,
    bod_mv      = bod %in% c(5, 6, 7)   # MV = citología o histología (primario/metástasis)
  )

# ============================================================
# 4) PSU% (Primary Site Unknown)
#    Aproximación mínima y transparente:
#    - CIE10 empieza con C80 (C80.0-C80.9 / C80)
#    - o topografía C80.9 / C809 (si existe topo)
# ============================================================

dt <- dt %>%
  mutate(
    cie10_raw = if ("cie10" %in% names(.)) str_to_upper(str_trim(cie10)) else NA_character_,
    topo_raw  = if ("topo"  %in% names(.)) str_to_upper(str_trim(topo))  else NA_character_,
    
    psu = (!is.na(cie10_raw) & str_detect(cie10_raw, "^C80")) |
      (!is.na(topo_raw)  & str_replace_all(topo_raw, "\\.", "") %in% c("C809","C80"))
  )

# ============================================================
# 5) Indicadores globales
# ============================================================

n_total <- nrow(dt)

qc_global <- tibble(
  n_total = n_total,
  
  n_sexo_unknown = sum(dt$sexo_unknown, na.rm = TRUE),
  pct_sexo_unknown = 100 * mean(dt$sexo_unknown, na.rm = TRUE),
  
  n_edad_unknown = sum(dt$edad_unknown, na.rm = TRUE),
  pct_edad_unknown = 100 * mean(dt$edad_unknown, na.rm = TRUE),
  
  n_bod_known = sum(!dt$bod_unknown, na.rm = TRUE),
  pct_bod_known = 100 * mean(!dt$bod_unknown, na.rm = TRUE),
  
  n_mv = sum(dt$bod_mv, na.rm = TRUE),
  pct_mv = 100 * mean(dt$bod_mv, na.rm = TRUE),
  
  n_dco = sum(dt$bod_dco, na.rm = TRUE),
  pct_dco = 100 * mean(dt$bod_dco, na.rm = TRUE),
  
  n_psu = sum(dt$psu, na.rm = TRUE),
  pct_psu = 100 * mean(dt$psu, na.rm = TRUE),
  
  n_fecdiag_missing = if ("fecdiag" %in% names(dt)) sum(is.na(dt$fecdiag)) else NA_integer_,
  pct_fecdiag_missing = if ("fecdiag" %in% names(dt)) 100 * mean(is.na(dt$fecdiag)) else NA_real_
)

# ============================================================
# 6) Indicadores por año (year_diag) y por sexo
# ============================================================

qc_by_year <- dt %>%
  mutate(year_diag = as.integer(year_diag)) %>%
  group_by(year_diag) %>%
  summarise(
    n = n(),
    pct_sexo_unknown = 100 * mean(sexo_unknown, na.rm = TRUE),
    pct_edad_unknown = 100 * mean(edad_unknown, na.rm = TRUE),
    pct_mv  = 100 * mean(bod_mv, na.rm = TRUE),
    pct_dco = 100 * mean(bod_dco, na.rm = TRUE),
    pct_psu = 100 * mean(psu, na.rm = TRUE),
    pct_fecdiag_missing = if ("fecdiag" %in% names(dt)) 100 * mean(is.na(fecdiag)) else NA_real_,
    .groups = "drop"
  ) %>%
  arrange(year_diag)

qc_by_sex <- dt %>%
  group_by(sexo_std) %>%
  summarise(
    n = n(),
    pct_edad_unknown = 100 * mean(edad_unknown, na.rm = TRUE),
    pct_mv  = 100 * mean(bod_mv, na.rm = TRUE),
    pct_dco = 100 * mean(bod_dco, na.rm = TRUE),
    pct_psu = 100 * mean(psu, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

# ============================================================
# 7) Frecuencias BoD (base_dx / veri)
#    Esto es clave para interpretar MV/DCO si hay codificación local rara.
# ============================================================

basis_freq <- dt %>%
  mutate(bod = as.integer(bod)) %>%
  count(bod, sort = TRUE) %>%
  mutate(pct = 100 * n / sum(n))

base_dx_freq <- NULL
if ("base_dx" %in% names(dt)) {
  base_dx_freq <- dt %>%
    count(base_dx_raw, sort = TRUE) %>%
    mutate(pct = 100 * n / sum(n))
}

veri_freq <- NULL
if ("veri" %in% names(dt)) {
  veri_freq <- dt %>%
    count(veri_raw, sort = TRUE) %>%
    mutate(pct = 100 * n / sum(n))
}

# ============================================================
# 8) Reportes auxiliares relevantes (para interpretación)
#    - CIE10 "raro" (no C##)
#    - incompatibilidades sitio-sexo (C61 en F; C53 en M)
# ============================================================

cie10_bad <- NULL
if ("cie10" %in% names(dt)) {
  cie10_bad <- dt %>%
    filter(!is.na(cie10_raw) & cie10_raw != "" & !str_detect(cie10_raw, "^C\\d{2}")) %>%
    count(cie10_raw, sort = TRUE) %>%
    mutate(pct = 100 * n / sum(n))
}

sex_site_checks <- tibble(
  rule = c("C61 (próstata) en F", "C53 (cérvix) en M", "C50 (mama) en M (posible, raro)"),
  n = c(
    dt %>% filter(sexo_std == "F", !is.na(cie10_raw) & str_detect(cie10_raw, "^C61")) %>% nrow(),
    dt %>% filter(sexo_std == "M", !is.na(cie10_raw) & str_detect(cie10_raw, "^C53")) %>% nrow(),
    dt %>% filter(sexo_std == "M", !is.na(cie10_raw) & str_detect(cie10_raw, "^C50")) %>% nrow()
  )
) %>%
  mutate(pct = 100 * n / n_total)

# ============================================================
# 9) Guardar outputs (tablas)
# ============================================================

write_csv(qc_global, out_sum)
write_csv(qc_by_year, out_year)
write_csv(basis_freq, out_basis)

saveRDS(
  list(
    qc_global = qc_global,
    qc_by_year = qc_by_year,
    qc_by_sex = qc_by_sex,
    basis_freq = basis_freq,
    base_dx_freq = base_dx_freq,
    veri_freq = veri_freq,
    cie10_bad = cie10_bad,
    sex_site_checks = sex_site_checks
  ),
  out_rds
)

# ============================================================
# 10) Imprimir log amigable (para que me pegues la salida)
# ============================================================

sink(out_txt, split = TRUE)

cat("====================================\n")
cat("REPORTE DE CALIDAD EPIDEMIOLÓGICA\n")
cat("====================================\n\n")
cat(glue("Input: {in_rds}\n"))
cat(glue("Fecha: {Sys.Date()}\n\n"))

cat("1) Resumen global\n")
print(qc_global)

cat("\n2) Indicadores por año (year_diag)\n")
print(qc_by_year, n = Inf)

cat("\n3) Indicadores por sexo (sexo_std)\n")
print(qc_by_sex, n = Inf)

cat("\n4) Frecuencias de BoD (bod; desde base_dx o veri)\n")
print(basis_freq, n = Inf)

if (!is.null(base_dx_freq)) {
  cat("\n5) Frecuencias crudas de base_dx (texto original)\n")
  print(head(base_dx_freq, 40), n = 40)
}

if (!is.null(veri_freq)) {
  cat("\n6) Frecuencias crudas de veri (texto original)\n")
  print(head(veri_freq, 40), n = 40)
}

if (!is.null(cie10_bad)) {
  cat("\n7) CIE10 que no cumple patrón ^C\\d{2} (top 30)\n")
  print(head(cie10_bad, 30), n = 30)
}

cat("\n8) Chequeos sitio-sexo\n")
print(sex_site_checks)

cat("\n====================================\n")
cat("FIN REPORTE DE CALIDAD EPIDEMIOLÓGICA\n")
cat("====================================\n")

sink()

cat(glue("\n[03_quality_epidemiologic] Log: {out_txt}\n"))
cat(glue("[03_quality_epidemiologic] Tablas (RDS): {out_rds}\n"))
cat(glue("[03_quality_epidemiologic] CSV resumen: {out_sum}\n"))
cat(glue("[03_quality_epidemiologic] CSV por año : {out_year}\n"))
cat(glue("[03_quality_epidemiologic] CSV BoD    : {out_basis}\n"))

# ============================================================
# 02_quality_structural.R  (VERSIÓN INTEGRADA / EXTENDIDA)
# QC estructural (data cleaning + documentación de calidad)
# Carga el .rds generado por 01_import_harmonize.R
# - Checks básicos + checks útiles adicionales (sin hacerlo infinito)
# - Genera un .txt reproducible en /REPORTS
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(glue)
  library(lubridate)
  library(readr)
  library(tibble)
  library(here)
})

# ---- CONFIG ----
in_rds   <- "DATA/DERIVED/rcpa_arequipa_2015_2022_clean.rds"
out_dir  <- "REPORTS"
out_txt  <- file.path(out_dir, paste0("qc_structural_", format(Sys.Date(), "%Y%m%d"), ".txt"))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load ----
dt_all <- readRDS(in_rds)

# ---- Output to file + console ----
sink(out_txt, split = TRUE)

cat("====================================\n")
cat("REPORTE DE CALIDAD ESTRUCTURAL (QC)\n")
cat("====================================\n\n")
cat(glue("Input: {in_rds}\n"))
cat(glue("Fecha: {Sys.Date()}\n\n"))

cat(glue("Filas: {nrow(dt_all)}\n"))
cat(glue("Cols : {ncol(dt_all)}\n\n"))

# ============================================================
# 0) Missingness global (todas las variables)
# ============================================================
cat("0) Missingness global (% NA por variable)\n")

missing_full <- dt_all %>%
  summarise(across(everything(), ~ mean(is.na(.))*100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  arrange(desc(pct_na))

cat("\n- Variables con 100% NA:\n")
print(missing_full %>% filter(pct_na == 100), n = Inf)

cat("\n- Variables con >50% NA:\n")
print(missing_full %>% filter(pct_na > 50 & pct_na < 100), n = Inf)

cat("\n- Variables con >20% NA:\n")
print(missing_full %>% filter(pct_na > 20 & pct_na <= 50), n = Inf)

cat("\n")

# ============================================================
# 1) Filas completamente vacías
# ============================================================
rows_all_na <- dt_all %>% filter(if_all(everything(), ~ is.na(.)))
cat(glue("1) Filas completamente vacías: {nrow(rows_all_na)}\n\n"))

# ============================================================
# 2) Columnas completamente vacías
# ============================================================
cols_all_na <- dt_all %>%
  summarise(across(everything(), ~ all(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "all_na") %>%
  filter(all_na)

cat(glue("2) Columnas completamente vacías: {nrow(cols_all_na)}\n"))
if (nrow(cols_all_na) > 0) print(cols_all_na)
cat("\n")

# ============================================================
# 3) Duplicados exactos
# ============================================================
dup_exact <- dt_all %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

cat(glue("3) Filas duplicadas exactas: {nrow(dup_exact)}\n\n"))

# ============================================================
# 4) Edad fuera de rango + resumen
# ============================================================
if ("edad" %in% names(dt_all)) {
  edad_fuera_rango <- dt_all %>%
    filter(!is.na(edad) & (edad < 0 | edad > 110))
  
  cat(glue("4) Registros con edad fuera de rango (<0 o >110): {nrow(edad_fuera_rango)}\n"))
  
  # extremos (para detectar digitación rara)
  edad_stats <- dt_all %>%
    summarise(
      min_edad = suppressWarnings(min(edad, na.rm = TRUE)),
      max_edad = suppressWarnings(max(edad, na.rm = TRUE)),
      p01 = suppressWarnings(quantile(edad, 0.01, na.rm = TRUE)),
      p99 = suppressWarnings(quantile(edad, 0.99, na.rm = TRUE))
    )
  
  cat("\n- Resumen edad (min/max/p01/p99):\n")
  print(edad_stats)
  
  cat("\n")
} else {
  cat("4) 'edad' no existe en la base.\n\n")
}

# ============================================================
# 5) Fechas no parseables (tenían algo pero quedaron NA tras parseo)
#    Requiere columnas *_raw generadas en el script 01
# ============================================================
date_vars <- intersect(c("fecdiag","fecnac","fecdef","fuc"), names(dt_all))

cat("5) Fechas no parseables (tenían dato pero quedaron NA tras parseo)\n")
for (v in date_vars) {
  raw_var <- paste0(v, "_raw")
  if (!raw_var %in% names(dt_all)) next
  
  no_parse <- dt_all %>%
    filter(is.na(.data[[v]]) &
             !is.na(.data[[raw_var]]) &
             str_trim(.data[[raw_var]]) != "")
  
  cat(glue("- {v}: {nrow(no_parse)}\n"))
  
  if (nrow(no_parse) > 0) {
    cat("  Top 15 strings:\n")
    print(
      no_parse %>%
        count(.data[[raw_var]], sort = TRUE) %>%
        head(15)
    )
  }
  cat("\n")
}
cat("\n")

# ============================================================
# 6) Diagnóstico posterior a defunción (fecdiag > fecdef)
# ============================================================
if (all(c("fecdiag","fecdef") %in% names(dt_all))) {
  inconsist_fecha <- dt_all %>%
    filter(!is.na(fecdiag), !is.na(fecdef), fecdiag > fecdef)
  
  cat(glue("6) Diagnóstico posterior a defunción (fecdiag > fecdef): {nrow(inconsist_fecha)}\n"))
  
  # opcional: ver cuánto es el desfase
  if (nrow(inconsist_fecha) > 0) {
    desfase <- inconsist_fecha %>%
      mutate(days_diff = as.integer(fecdiag - fecdef)) %>%
      summarise(
        min_days = min(days_diff, na.rm = TRUE),
        p50_days = median(days_diff, na.rm = TRUE),
        p95_days = quantile(days_diff, 0.95, na.rm = TRUE),
        max_days = max(days_diff, na.rm = TRUE)
      )
    cat("\n- Desfase (días) entre fecdiag y fecdef (solo inconsistentes):\n")
    print(desfase)
  }
  cat("\n")
} else {
  cat("6) No existen fecdiag y/o fecdef.\n\n")
}

# ============================================================
# 7) Rango de años de diagnóstico / nacimiento (detección de valores raros)
# ============================================================
if ("fecdiag" %in% names(dt_all)) {
  diag_year <- dt_all %>%
    mutate(year_diag = year(fecdiag)) %>%
    summarise(
      min_year_diag = suppressWarnings(min(year_diag, na.rm = TRUE)),
      max_year_diag = suppressWarnings(max(year_diag, na.rm = TRUE))
    )
  
  cat("7) Rango de años de diagnóstico (fecdiag)\n")
  print(diag_year)
  
  # conteo por año (solo si hay años válidos)
  cat("\n- Conteo por año de diagnóstico (NA excluidos):\n")
  print(
    dt_all %>%
      mutate(year_diag = year(fecdiag)) %>%
      filter(!is.na(year_diag)) %>%
      count(year_diag, sort = FALSE)
  )
  cat("\n")
}

if ("fecnac" %in% names(dt_all)) {
  nac_year <- dt_all %>%
    mutate(year_nac = year(fecnac)) %>%
    summarise(
      min_year_nac = suppressWarnings(min(year_nac, na.rm = TRUE)),
      max_year_nac = suppressWarnings(max(year_nac, na.rm = TRUE))
    )
  
  cat("8) Rango de años de nacimiento (fecnac)\n")
  print(nac_year)
  cat("\n")
}

# ============================================================
# 8) Consistencia Edad vs (fecdiag - fecnac)
#    (solo cuando existen y no son NA)
# ============================================================
if (all(c("edad","fecdiag","fecnac") %in% names(dt_all))) {
  edad_calc <- dt_all %>%
    filter(!is.na(edad), !is.na(fecdiag), !is.na(fecnac)) %>%
    mutate(
      edad_recalc = floor(time_length(interval(fecnac, fecdiag), "years")),
      diff = abs(edad - edad_recalc)
    )
  
  age_cons <- edad_calc %>%
    summarise(
      n = n(),
      inconsistentes_gt1 = sum(diff > 1),
      pct_inconsistentes_gt1 = 100 * mean(diff > 1),
      inconsistentes_gt2 = sum(diff > 2),
      pct_inconsistentes_gt2 = 100 * mean(diff > 2)
    )
  
  cat("9) Consistencia Edad reportada vs edad recalculada (fecnac→fecdiag)\n")
  print(age_cons)
  
  cat("\n- Top 10 diferencias más frecuentes (diff):\n")
  print(
    edad_calc %>%
      count(diff, sort = TRUE) %>%
      head(10)
  )
  
  cat("\n")
} else {
  cat("9) No se pudo evaluar consistencia edad: faltan edad/fecdiag/fecnac.\n\n")
}

# ============================================================
# 9) Distribución y codificación de SEXO (detección de rarezas)
# ============================================================
if ("sexo" %in% names(dt_all)) {
  cat("10) Distribución de SEXO (incluye NA)\n")
  print(dt_all %>% count(sexo, sort = TRUE))
  cat("\n")
}

# ============================================================
# 10) CIE10 formato básico (detección de codificación rara)
# ============================================================
if ("cie10" %in% names(dt_all)) {
  cat("11) CIE10: valores que NO parecen iniciar con 'C' + 2 dígitos (ej. C50)\n")
  bad_cie10 <- dt_all %>%
    filter(!is.na(cie10) & str_trim(cie10) != "" & !str_detect(cie10, "^C\\d{2}")) %>%
    count(cie10, sort = TRUE)
  
  cat(glue("- Cantidad de códigos 'raros': {nrow(bad_cie10)}\n"))
  if (nrow(bad_cie10) > 0) print(head(bad_cie10, 30))
  cat("\n")
}

# ============================================================
# 11) Sitios incompatibles con sexo (QC lógico simple)
#     Nota: depende de la codificación de sexo (M/F o 1/2). Reportamos ambas.
# ============================================================
if (all(c("cie10","sexo") %in% names(dt_all))) {
  sexo_norm <- dt_all %>%
    mutate(
      sexo_std = case_when(
        str_to_upper(sexo) %in% c("M","MALE","H","HOMBRE","1") ~ "M",
        str_to_upper(sexo) %in% c("F","FEMALE","MUJER","2") ~ "F",
        TRUE ~ NA_character_
      )
    )
  
  cat("12) Incompatibilidades sitio-sexo (CIE10)\n")
  
  n_prostata_mujer <- sexo_norm %>%
    filter(!is.na(sexo_std), str_detect(cie10, "^C61"), sexo_std == "F") %>%
    nrow()
  
  n_cervix_hombre <- sexo_norm %>%
    filter(!is.na(sexo_std), str_detect(cie10, "^C53"), sexo_std == "M") %>%
    nrow()
  
  n_mama_hombre <- sexo_norm %>%
    filter(!is.na(sexo_std), str_detect(cie10, "^C50"), sexo_std == "M") %>%
    nrow()
  
  cat(glue("- Próstata (C61) en F: {n_prostata_mujer}\n"))
  cat(glue("- Cérvix (C53) en M: {n_cervix_hombre}\n"))
  cat(glue("- Mama (C50) en M: {n_mama_hombre} (no necesariamente error: raro pero posible)\n\n"))
}

# ============================================================
# 12) % missing en variables críticas para incidencia
# ============================================================
criticas <- intersect(c("fecdiag","topo","morf","sexo","edad","res"), names(dt_all))
cat("13) % missing en variables críticas para incidencia:\n")
if (length(criticas) > 0) {
  dt_all %>%
    summarise(across(all_of(criticas), ~ mean(is.na(.))*100)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
    arrange(desc(pct_na)) %>%
    print(n = Inf)
} else {
  cat("   No se encontraron variables críticas esperadas.\n")
}
cat("\n")

# ============================================================
# 13) Comentarios automáticos (hallazgos relevantes)
# ============================================================
cat("14) Hallazgos/alertas automáticas (interpretación breve)\n")

# alertas simples
alerts <- list()

# res 100% NA
if ("res" %in% names(dt_all)) {
  pct_res_na <- mean(is.na(dt_all$res)) * 100
  if (pct_res_na == 100) alerts <- append(alerts, "Variable 'res' (residencia) está 100% NA: documentar criterio de residencia o fuente alternativa.")
}

# pmseq/pmtot 100% NA
for (v in c("pmseq","pmtot")) {
  if (v %in% names(dt_all)) {
    if (mean(is.na(dt_all[[v]])) * 100 == 100) {
      alerts <- append(alerts, glue("Variable '{v}' está 100% NA: no se puede caracterizar múltiples primarios con estos campos; confirmar si aplica en el registro/exportación."))
    }
  }
}

# inconsistencias diag>def
if (exists("inconsist_fecha")) {
  if (nrow(inconsist_fecha) > 0) alerts <- append(alerts, glue("Hay {nrow(inconsist_fecha)} registros con fecdiag > fecdef: revisar reglas (diagnóstico post-mortem, error de digitación o fechas invertidas)."))
}

if (length(alerts) == 0) {
  cat("- No se detectaron alertas mayores con las reglas actuales.\n")
} else {
  for (a in alerts) cat(glue("- {a}\n"))
}

cat("\n====================================\n")
cat("FIN REPORTE CALIDAD ESTRUCTURAL\n")
cat("====================================\n")

sink()

cat(glue("\n[02_quality_structural] Reporte guardado en: {out_txt}\n"))

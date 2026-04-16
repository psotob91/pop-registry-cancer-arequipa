# ============================================================
# 01_import_harmonize.R
# RCBPAQP 2015-2022 — Importación + Armonización + Tipos + Labels
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(purrr)
  library(janitor)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(lubridate)
  library(labelled)
  library(readr)
  library(glue)
  library(here)
})

# ---- CONFIG ----
path_xlsx <- here("DATA", "RAW", "RCBPAQP 2015-2022.xlsx")

out_dir  <- "DATA/DERIVED"
out_rds  <- file.path(out_dir, "rcpa_arequipa_2015_2022_clean.rds")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Diccionario de nombres canónicos ----
name_map <- c(
  "tumourid"                    = "tumorid",
  "tumouridsourcetable"         = "tumouridsourcetable",
  "sourcerecordid"              = "sourcerecordid",
  "notif"                       = "notif",
  "inst_number_11"              = "inst",
  "inst_11"                     = "inst",
  "hc"                          = "hc",
  "tipoest"                     = "tipoest",
  "numpat"                      = "numpat",
  "prof"                        = "prof",
  "veri"                        = "veri",
  "estcas"                      = "estcas",
  "modif"                       = "modif",
  "edad"                        = "edad",
  "deptres"                     = "deptres",
  "pmcod"                       = "pmcod",
  "pmseq"                       = "pmseq",
  "pmtot"                       = "pmtot",
  "provdist"                    = "provdist",
  "res"                         = "res",
  "fecdiag"                     = "fecdiag",
  "base_number_7"               = "base_dx",
  "base_7"                      = "base_dx",
  "topo"                        = "topo",
  "morf"                        = "morf",
  "comport_number_5"            = "comport",
  "comport_5"                   = "comport",
  "grado"                       = "grado",
  "late_19"                     = "lateralidad",
  "lateralidad"                 = "lateralidad",
  "tx"                          = "tx",
  "nx"                          = "nx",
  "mx"                          = "mx",
  "estadio"                     = "estadio",
  "cie10"                       = "cie10",
  "cicn"                        = "cicn",
  "dx"                          = "dx",
  "obs"                         = "obs",
  "caso"                        = "caso",
  "busc"                        = "busc",
  "x1_ape"                      = "ape1",
  "x2_ape"                      = "ape2",
  "x1_nombre"                   = "nombre1",
  "x2_nombre"                   = "nombre2",
  "x3_nombre"                   = "nombre3",
  "xape1"                       = "ape1",
  "xape2"                       = "ape2",
  "xnombre1"                    = "nombre1",
  "xnombre2"                    = "nombre2",
  "xnombre3"                    = "nombre3",
  "sexo"                        = "sexo",
  "tipo"                        = "tipo",
  "nodoc"                       = "nodoc",
  "fecnac"                      = "fecnac",
  "pais"                        = "pais",
  "estciv"                      = "estciv",
  "ocu"                         = "ocu",
  "instr"                       = "instr",
  "fuc"                         = "fuc",
  "estvit"                      = "estvit",
  "fecdef"                      = "fecdef",
  "causa"                       = "causa",
  "patientidtumourtable"        = "patientidtumourtable",
  "patientrecordidtumourtable"  = "patientrecordidtumourtable",
  "tumourupdatedby"             = "tumourupdatedby",
  "tumourunduplicationstatus"   = "tumourunduplicationstatus",
  "obsoleteflagtumourtable"     = "obsoleteflagtumourtable",
  "obsoleteflagpatienttable"    = "obsoleteflagpatienttable",
  "patientrecordid"             = "patientrecordid",
  "patientupdatedby"            = "patientupdatedby",
  "patientupdatedate"           = "patientupdatedate",
  "patientrecordstatus"         = "patientrecordstatus",
  "patientcheckstatus"          = "patientcheckstatus"
)

# ---- función para normalizar nombres por hoja ----
normalize_sheet_names <- function(df) {
  nm <- names(df)
  
  # quitar columnas basura generadas por headers vacíos o numéricos raros
  keep <- !str_detect(nm, "^\\.\\.\\.\\d+$|^unnamed_\\d+$|^x\\d+$")
  df <- df[, keep, drop = FALSE]
  nm <- names(df)
  
  # renombrar por diccionario
  nm2 <- ifelse(nm %in% names(name_map), unname(name_map[nm]), nm)
  names(df) <- nm2
  
  # si por alguna razón quedaron duplicadas tras renombrar,
  # coalescerlas en una sola columna por nombre
  dup_nms <- unique(names(df)[duplicated(names(df))])
  
  if (length(dup_nms) > 0) {
    for (v in dup_nms) {
      idx <- which(names(df) == v)
      tmp <- df[idx]
      
      merged <- tmp[[1]]
      if (length(idx) > 1) {
        for (j in 2:length(idx)) {
          merged <- dplyr::coalesce(merged, tmp[[j]])
        }
      }
      
      df <- df[, -idx, drop = FALSE]
      df[[v]] <- merged
    }
  }
  
  df
}

# ---- 1) Hojas ----
sheets <- excel_sheets(path_xlsx)

# ---- 2) Importar todo como texto ----
lst <- map(sheets, ~{
  df <- read_excel(
    path_xlsx,
    sheet = .x,
    col_types = "text"
  ) %>%
    as_tibble() %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    janitor::clean_names(case = "snake")
  
  df <- normalize_sheet_names(df)
  
  df %>%
    mutate(
      sheet_source = .x,
      year_sheet   = suppressWarnings(parse_number(.x))
    )
})

# ---- 2b) Auditoría de estructura por hoja ----
sheet_struct <- map_dfr(seq_along(lst), \(i) {
  tibble(
    sheet = sheets[i],
    ncol  = ncol(lst[[i]]),
    cols  = paste(names(lst[[i]]), collapse = " | ")
  )
})

print(sheet_struct, n = Inf)

# variables presentes por hoja
sheet_presence <- map_dfr(seq_along(lst), \(i) {
  tibble(
    sheet = sheets[i],
    variable = names(lst[[i]])
  )
})

# ---- 3) Unir ----
dt_all <- bind_rows(lst)

# ---- chequeo post-bind de variables clave ----
vars_criticas <- c(
  "inst","hc","tipoest","numpat","prof","veri","estcas","modif",
  "edad","deptres","pmcod","pmseq","pmtot","provdist","res",
  "fecdiag","base_dx","topo","morf","comport","grado","lateralidad",
  "tx","nx","mx","estadio","cie10","cicn","dx","obs","caso","busc",
  "ape1","ape2","nombre1","nombre2","nombre3","sexo","tipo","nodoc",
  "fecnac","pais","estciv","ocu","instr","fuc","estvit","fecdef","causa"
)

cat("\n--- Variables críticas faltantes ---\n")
print(setdiff(vars_criticas, names(dt_all)))

cat("\n--- Variables sospechosas remanentes ---\n")
print(names(dt_all)[str_detect(names(dt_all),
                               "base|comport|ape|nombre|late|inst|^x\\d+$|\\.\\.\\.")])

# ---- 4) Guardar columnas raw de fechas ----
date_vars <- intersect(c("fecdiag", "fecnac", "fecdef", "fuc"), names(dt_all))
for (v in date_vars) dt_all[[paste0(v, "_raw")]] <- dt_all[[v]]

# ---- 5) Convertir tipos ----
dt_all <- dt_all %>%
  mutate(
    edad  = if ("edad"  %in% names(dt_all)) suppressWarnings(as.integer(edad))  else edad,
    inst  = if ("inst"  %in% names(dt_all)) suppressWarnings(as.integer(inst))  else inst,
    pmseq = if ("pmseq" %in% names(dt_all)) suppressWarnings(as.integer(pmseq)) else pmseq,
    pmtot = if ("pmtot" %in% names(dt_all)) suppressWarnings(as.integer(pmtot)) else pmtot
  ) %>%
  mutate(
    across(all_of(date_vars), ~{
      x <- str_trim(.x)
      x[x == ""] <- NA_character_
      
      x <- str_replace(x, "\\s+\\d{1,2}:\\d{2}:\\d{2}.*$", "")
      
      x <- na_if(x, "NA");  x <- na_if(x, "N/A")
      x <- na_if(x, "SD");  x <- na_if(x, "S/D")
      x <- na_if(x, "NR");  x <- na_if(x, "0")
      x <- na_if(x, "00/00/0000"); x <- na_if(x, "0000-00-00")
      
      is_serial <- str_detect(x, "^\\d{5}$")
      serial_num <- suppressWarnings(as.numeric(x))
      x_date_serial <- as.Date(serial_num, origin = "1899-12-30")
      
      x_date_parse <- as.Date(lubridate::parse_date_time(
        x,
        orders = c("Y-m-d", "d/m/Y", "d-m-Y", "m/d/Y", "Y/m/d", "d.m.Y"),
        exact = FALSE,
        tz = "UTC"
      ))
      
      out <- ifelse(is_serial, x_date_serial, x_date_parse)
      as.Date(out, origin = "1970-01-01")
    })
  )

# ---- 6) Labels ----
var_labels <- c(
  tumorid = "Identificador único del tumor (sistema)",
  sourcerecordid = "ID del registro fuente (sistema)",
  notif = "Código de notificación/fuente (posible institución)",
  inst = "Código de institución notificadora",
  hc = "Número de historia clínica (hospital)",
  tipoest = "Tipo de establecimiento (campo local; confirmar codificación)",
  numpat = "Número de patología / informe AP",
  prof = "Profesional/servicio reportante (texto)",
  veri = "Verificación (campo local; confirmar codificación)",
  estcas = "Estado del caso (sistema; confirmar codificación)",
  modif = "Modificación/actualización (campo local; confirmar)",
  base_dx = "Base de diagnóstico (IARC/ROADS; confirmar codificación)",
  caso = "Clase de caso (IARC/registro; confirmar codificación)",
  busc = "Método de búsqueda/captación (probable; confirmar)",
  edad = "Edad al diagnóstico (años)",
  sexo = "Sexo",
  pais = "País",
  deptres = "Departamento de residencia",
  provdist = "Provincia/Distrito residencia",
  res = "Residente en el área del registro (sí/no; confirmar codificación)",
  estciv = "Estado civil",
  ocu = "Ocupación",
  instr = "Nivel de instrucción",
  ape1 = "Primer apellido",
  ape2 = "Segundo apellido",
  nombre1 = "Primer nombre",
  nombre2 = "Segundo nombre",
  nombre3 = "Tercer nombre",
  tipo = "Tipo de documento",
  nodoc = "Número de documento",
  fecnac = "Fecha de nacimiento",
  fecdiag = "Fecha de diagnóstico/incidencia",
  topo = "Topografía (CIE-O-3)",
  morf = "Morfología (CIE-O-3)",
  comport = "Comportamiento (CIE-O-3)",
  grado = "Grado histológico",
  lateralidad = "Lateralidad (campo local; confirmar codificación)",
  tx = "Tratamiento (campo local; confirmar)",
  nx = "N (TNM)",
  mx = "M (TNM)",
  estadio = "Estadio (campo local; confirmar)",
  cie10 = "CIE-10",
  cicn = "Clasificación cáncer infantil (posible ICCC; confirmar)",
  dx = "Diagnóstico (texto libre)",
  obs = "Observaciones (texto libre)",
  pmcod = "Código MP (regla aplicada; confirmar)",
  pmseq = "Secuencia del primario",
  pmtot = "Total de primarios en el paciente",
  fuc = "Fecha de último contacto",
  estvit = "Estado vital",
  fecdef = "Fecha de defunción",
  causa = "Causa de muerte (campo local; confirmar)",
  sheet_source = "Hoja de origen",
  year_sheet = "Año inferido desde nombre de hoja"
)

present <- intersect(names(var_labels), names(dt_all))
dt_all <- labelled::set_variable_labels(dt_all, .labels = as.list(var_labels[present]))

# ---- 7) Log ----
cat(glue("\n[01_import_harmonize] Hojas: {length(sheets)}\n"))
cat(glue("[01_import_harmonize] Filas: {nrow(dt_all)}\n"))
cat(glue("[01_import_harmonize] Cols : {ncol(dt_all)}\n"))
cat(glue("[01_import_harmonize] Output: {out_rds}\n\n"))

# ---- 8) Guardar ----
saveRDS(dt_all, out_rds)

# ---- 4) Guardar columnas raw de fechas (para auditoría) ----
date_vars <- intersect(c("fecdiag", "fecnac", "fecdef", "fuc"), names(dt_all))
for (v in date_vars) dt_all[[paste0(v, "_raw")]] <- dt_all[[v]]

# ---- 5) Convertir tipos (explícito y robusto) ----
dt_all <- dt_all %>%
  mutate(
    # numéricos típicos (si existen)
    edad  = if ("edad"  %in% names(dt_all)) suppressWarnings(as.integer(edad))  else edad,
    inst  = if ("inst"  %in% names(dt_all)) suppressWarnings(as.integer(inst))  else inst,
    pmseq = if ("pmseq" %in% names(dt_all)) suppressWarnings(as.integer(pmseq)) else pmseq,
    pmtot = if ("pmtot" %in% names(dt_all)) suppressWarnings(as.integer(pmtot)) else pmtot
  ) %>%
  mutate(
    across(all_of(date_vars), ~{
      x <- str_trim(.x)
      x[x == ""] <- NA_character_
      
      # quitar hora si existe (2022-02-08 00:00:00)
      x <- str_replace(x, "\\s+\\d{1,2}:\\d{2}:\\d{2}.*$", "")
      
      # NA por códigos frecuentes
      x <- na_if(x, "NA");  x <- na_if(x, "N/A")
      x <- na_if(x, "SD");  x <- na_if(x, "S/D")
      x <- na_if(x, "NR");  x <- na_if(x, "0")
      x <- na_if(x, "00/00/0000"); x <- na_if(x, "0000-00-00")
      
      # serial Excel típico (5 dígitos)
      is_serial <- str_detect(x, "^\\d{5}$")
      serial_num <- suppressWarnings(as.numeric(x))
      x_date_serial <- as.Date(serial_num, origin = "1899-12-30")
      
      # parse múltiples formatos
      x_date_parse <- as.Date(lubridate::parse_date_time(
        x,
        orders = c("Y-m-d", "d/m/Y", "d-m-Y", "m/d/Y", "Y/m/d", "d.m.Y"),
        exact = FALSE,
        tz = "UTC"
      ))
      
      out <- ifelse(is_serial, x_date_serial, x_date_parse)
      as.Date(out, origin = "1970-01-01")
    })
  )

# ---- 6) Labels ----
var_labels <- c(
  # IDs / auditoría
  tumorid = "Identificador único del tumor (sistema)",
  sourcerecordid = "ID del registro fuente (sistema)",
  notif = "Código de notificación/fuente (posible institución)",
  inst = "Código de institución notificadora",
  hc = "Número de historia clínica (hospital)",
  tipoest = "Tipo de establecimiento (campo local; confirmar codificación)",
  numpat = "Número de patología / informe AP",
  prof = "Profesional/servicio reportante (texto)",
  
  # caso / calidad
  veri = "Verificación (campo local; confirmar codificación)",
  estcas = "Estado del caso (sistema; confirmar codificación)",
  modif = "Modificación/actualización (campo local; confirmar)",
  base_dx = "Base de diagnóstico (IARC/ROADS; confirmar codificación)",
  caso = "Clase de caso (IARC/registro; confirmar codificación)",
  busc = "Método de búsqueda/captación (probable; confirmar)",
  
  # demografía
  edad = "Edad al diagnóstico (años)",
  sexo = "Sexo",
  pais = "País",
  deptres = "Departamento de residencia",
  provdist = "Provincia/Distrito residencia",
  res = "Residente en el área del registro (sí/no; confirmar codificación)",
  estciv = "Estado civil",
  ocu = "Ocupación",
  instr = "Nivel de instrucción",
  
  # nombres / doc
  ape1 = "Primer apellido",
  ape2 = "Segundo apellido",
  nombre1 = "Primer nombre",
  nombre2 = "Segundo nombre",
  nombre3 = "Tercer nombre",
  tipo = "Tipo de documento",
  nodoc = "Número de documento",
  fecnac = "Fecha de nacimiento",
  
  # tumor
  fecdiag = "Fecha de diagnóstico/incidencia",
  topo = "Topografía (CIE-O-3)",
  morf = "Morfología (CIE-O-3)",
  comport = "Comportamiento (CIE-O-3)",
  grado = "Grado histológico",
  lateralidad = "Lateralidad (campo local; confirmar codificación)",
  tx = "Tratamiento (campo local; confirmar)",
  nx = "N (TNM)",
  mx = "M (TNM)",
  estadio = "Estadio (campo local; confirmar)",
  cie10 = "CIE-10",
  cicn = "Clasificación cáncer infantil (posible ICCC; confirmar)",
  dx = "Diagnóstico (texto libre)",
  obs = "Observaciones (texto libre)",
  
  # múltiples primarios
  pmcod = "Código MP (regla aplicada; confirmar)",
  pmseq = "Secuencia del primario",
  pmtot = "Total de primarios en el paciente",
  
  # seguimiento
  fuc = "Fecha de último contacto",
  estvit = "Estado vital",
  fecdef = "Fecha de defunción",
  causa = "Causa de muerte (campo local; confirmar)",
  
  # trazabilidad
  sheet_source = "Hoja de origen",
  year_sheet = "Año inferido desde nombre de hoja"
)

present <- intersect(names(var_labels), names(dt_all))
dt_all <- labelled::set_variable_labels(dt_all, .labels = as.list(var_labels[present]))

# ---- 7) Log mínimo ----
cat(glue("\n[01_import_harmonize] Hojas: {length(sheets)}\n"))
cat(glue("[01_import_harmonize] Filas: {nrow(dt_all)}\n"))
cat(glue("[01_import_harmonize] Cols : {ncol(dt_all)}\n"))
cat(glue("[01_import_harmonize] Output: {out_rds}\n\n"))

# ---- 8) Guardar ----
saveRDS(dt_all, out_rds)



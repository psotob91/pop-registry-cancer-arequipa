# MASTER_DATA_STATUS_REPORT

## Dataset overview

- Archivo fuente auditado: `RCBPAQP 2015-2022.xlsx`
- Estructura confirmada: **8 hojas anuales** (`2015`–`2022`).
- Cada hoja representa **un año de registro** y **cada fila corresponde a un caso individual de cáncer**.

**Total de registros auditados:** **14,504**

**Número total de nombres de variable únicos (clean_name):** **66**

### Evolución estructural del número de variables

| Periodo | Variables por hoja |
|--------|-------------------|
| 2015–2016 | 58 |
| 2017–2022 | 64 |

El cambio estructural principal ocurre **a partir de 2017**, cuando aparecen **6 variables administrativas adicionales**:

- `OBSOLETEFLAGPATIENTTABLE`
- `PATIENTRECORDID`
- `PATIENTUPDATEDBY`
- `PATIENTUPDATEDATE`
- `PATIENTRECORDSTATUS`
- `PATIENTCHECKSTATUS`

Estas variables parecen corresponder a **metadatos del sistema de captura**, no a variables epidemiológicas.

### Encabezados anómalos detectados

Se detectaron dos encabezados potencialmente problemáticos:

- `__blank_col_2` en **2017**  
  (columna con encabezado vacío reconstruido automáticamente)

- `673668` en **2018**  
  (encabezado numérico posiblemente generado por error de exportación)

Estos campos deberán **revisarse manualmente antes de la armonización final**.

---

# Variable availability by year

## Resumen por año

| Año | Registros | Variables |
|---|---:|---:|
| 2015 | 1,831 | 58 |
| 2016 | 2,429 | 58 |
| 2017 | 2,405 | 64 |
| 2018 | 1,881 | 64 |
| 2019 | 1,491 | 64 |
| 2020 | 1,084 | 64 |
| 2021 | 1,711 | 64 |
| 2022 | 1,672 | 64 |

---

# Resumen global de disponibilidad

- Variables presentes en **todos los años:** **56**
- Variables **no presentes en todos los años:** **10**
- Variables con **inconsistencia de tipo entre años:** **9**
- Variables que requerirán **renombre explícito para armonización:** **14**

---

# Variables no estables o anómalas

### Variables claramente año-específicas

- `blank_col_2`  
  presente solo en **2017**

- `673668`  
  presente solo en **2018**

Estas variables probablemente corresponden a **errores de encabezado o columnas vacías durante exportaciones de Excel**.

### Variables añadidas desde 2017

Las siguientes variables aparecen desde **2017 en adelante**:

- `obsoleteflagpatienttable`
- `patientrecordid`
- `patientupdatedby`
- `patientupdatedate`
- `patientrecordstatus`
- `patientcheckstatus`

Estas variables parecen corresponder a **gestión del sistema de registro** y no a variables epidemiológicas centrales.

---

# Key epidemiological variables

La auditoría estructural detectó **disponibilidad estructural de variables candidatas relevantes para estándares IARC**:

- `sexo`
- `edad`
- `fecha_diagnostico`
- `topografia_icdo`
- `morfologia_icdo`
- `base_diagnostico` *(columna original: `BASE #7`)*
- `grado`
- `estado_vital`
- `fecha_muerte`
- `fecha_ultimo_contacto`
- `residencia__deptres`
- `residencia__provdist`
- `residencia__res`
- `multiple_primary__pmseq`
- `multiple_primary__pmtot`

Estas variables constituyen **candidatos estructurales** para indicadores epidemiológicos estándar.

---

# Variables críticas para indicadores de calidad epidemiológica

Según `quality_indicator_field_availability.csv`, existe disponibilidad estructural para los siguientes indicadores:

### MV — Microscopically Verified

Requiere:

- `morfologia_icdo`
- `base_diagnostico`

### DCO — Death Certificate Only

Requiere:

- `base_diagnostico`
- `fecha_muerte`
- `causa`

### PSU — Primary Site Unknown

Requiere:

- `topografia_icdo`

### Edad desconocida

Requiere:

- `edad`

### Sexo desconocido

Requiere:

- `sexo`

### Distribución por base diagnóstica

Requiere:

- `base_diagnostico`

### Seguimiento / vital status

Requiere:

- `estado_vital`
- `fecha_muerte`
- `fecha_ultimo_contacto`

---

# Advertencias metodológicas

La disponibilidad reportada es **estructural**, no **semántica**.

Antes de calcular indicadores IARC todavía debe confirmarse:

- codificación local de **`BASE #7`**
- codificación de **`SEXO`**
- codificación de **edad desconocida**
- significado operativo de **`CAUSA`**
- reglas de codificación para:
  - `RES`
  - `DEPTRES`
  - `PROVDIST`
- utilización real de:
  - `PMSEQ`
  - `PMTOT`
  - `PMCOD`

---

# Structural risks detected

## 1. Encabezados anómalos

Se detectaron dos campos con riesgo estructural:

- `blank_col_2`
- `673668`

Estos deben revisarse antes de cualquier pipeline analítico.

---

## 2. Cambio estructural desde 2017

El dataset pasa de **58 a 64 variables**.

Esto introduce una **frontera estructural clara** entre:

- 2015–2016 → estructura inicial
- 2017–2022 → estructura ampliada


Este cambio no invalida la base, pero **debe documentarse explícitamente en los pipelines analíticos**.

---

## 3. Necesidad de armonización explícita

Varias variables requerirán estandarización de nombres, por ejemplo:

| Nombre original | Nombre armonizado |
|----------------|------------------|
| `BASE #7` | `base_diagnostico` |
| `FECDIAG` | `fecha_diagnostico` |
| `TOPO` | `topografia_icdo` |
| `MORF` | `morfologia_icdo` |
| `DEPTRES` | `residencia_dept` |
| `PROVDIST` | `residencia_prov_dist` |
| `RES` | `residencia_codigo` |
| `PMSEQ` | `multiple_primary_seq` |
| `PMTOT` | `multiple_primary_total` |

Este renombre **no se realizó automáticamente** durante la auditoría para evitar introducir supuestos.

---

## 4. Inconsistencias de tipo

Algunas variables presentan **más de un `raw_type_guess` entre años**.

Estas inconsistencias deberán resolverse antes de cualquier:

- tipificación final
- normalización de dominios
- modelado analítico

---

## 5. No debe asumirse equivalencia semántica automática

El proceso de auditoría **no impuso equivalencias semánticas automáticas**.

Todas las relaciones de variables permanecen **explícitamente documentadas pero no forzadas** hasta su revisión manual.

---

# Recommendations for the next phase

1. **Congelar los tres archivos maestros generados:**

- `MASTER_DATA_DICTIONARY.csv`
- `MASTER_VARIABLE_MAP.csv`
- `MASTER_DATA_STATUS_REPORT.md`

2. Revisar manualmente los campos con:

- encabezado anómalo
- inconsistencia de tipo
- mapeo IARC de confianza moderada o baja

3. Construir un **diccionario semántico de codificación local** para:

- `BASE #7`
- `SEXO`
- `ESTVIT`
- `RES`
- `PMSEQ`
- `PMTOT`
- `PMCOD`

4. Separar claramente las siguientes fases del proyecto:

- auditoría estructural
- armonización de variables
- recodificación epidemiológica
- cálculo de indicadores


5. Mantener trazabilidad:

Cualquier decisión futura de:

- renombre
- recodificación
- tipificación

debe registrarse en una **bitácora metodológica nueva**, sin sobrescribir los archivos maestros originales.

---

# Nota final

Este reporte documenta exclusivamente:

- la **estructura observada**
- la **disponibilidad estructural de variables**

del registro poblacional de cáncer.

No constituye todavía:

- validación epidemiológica
- limpieza semántica de los datos
- cálculo de indicadores de calidad IARC
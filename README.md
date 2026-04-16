# Registro Poblacional de Cáncer de Arequipa (RCBPA), 2015–2022

Repositorio de código, metadatos estructurales y gobernanza reproducible para la reconstrucción, armonización, estandarización semántica y preparación analítica del Registro Poblacional de Cáncer de Arequipa (RCBPA) para el periodo 2015–2022.

## 1. Propósito del repositorio

Este repositorio **no** es un repositorio de datos abiertos del registro. Su función principal es versionar:

- código fuente reproducible del pipeline;
- manifiestos de gobernanza y prioridad de fuentes;
- maestros tabulares canónicos para decisiones analíticas;
- documentación técnica y reportes estructurales derivados no sensibles.

El objetivo es permitir una reconstrucción **trazable, auditable y metodológicamente prudente** del dataset analítico del RCBPA 2015–2022, respetando la lógica histórica del registro y evitando recodificaciones irreversibles sin evidencia explícita.

## 2. Principios metodológicos

El proyecto sigue estos principios operativos:

1. **Gobernanza antes que narrativa.** La lectura del proyecto debe empezar por los manifiestos en `META_MANIFESTS/`.
2. **Trazabilidad total.** Toda transformación relevante debe quedar documentada en logs, tablas o maestros vinculantes.
3. **Conservadurismo semántico.** La semántica final no se impone automáticamente cuando existe ambigüedad local.
4. **Separación entre fases.** La auditoría estructural, la armonización, la recodificación semántica y la construcción analítica son fases distintas y downstream.
5. **Compatibilidad con práctica histórica del registro.** Para incidencia, la estrategia principal usa denominador de mitad de periodo con referencia 2018, manteniendo una alternativa anual parametrizable.
6. **Reproducibilidad estructural.** El repositorio debe bastar para reconstruir la lógica del pipeline aun cuando los datos sensibles no estén disponibles públicamente.

## 3. Capa de gobernanza obligatoria

Antes de revisar scripts, usar siempre estos archivos como fuente estructural primaria:

- `META_MANIFESTS/MASTER_FILE_INDEX.csv`
- `META_MANIFESTS/DATA_REGISTRY.csv`
- `META_MANIFESTS/PROJECT_SOURCE_PRIORITY.csv`

### Jerarquía de decisión

Si hay conflicto entre fuentes del proyecto, aplicar esta precedencia:

1. **manifiestos** (`META_MANIFESTS/*`)
2. **archivos canónicos**
3. **código fuente**
4. **documentación narrativa**

### Regla de prioridad de fuentes

- **Fuentes del proyecto/chat**: prioridad máxima para datasets no públicos cuando hayan sido cargados al entorno de trabajo.
- **GitHub**: fuente principal para código, manifiestos, CSV maestros, YAML/JSON y documentación técnica versionada.
- **Ruta esperada del proyecto**: fallback reproducible cuando un archivo no está accesible directamente.

## 4. Alcance de los datos y gobernanza de confidencialidad

Los datos individuales del registro **no** se incluyen en GitHub por razones de confidencialidad y gobernanza.

En términos prácticos:

- el repositorio público debe contener principalmente **código, manifiestos, maestros y documentación**;
- los archivos raw y derivados analíticos no públicos pueden existir fuera de GitHub y deben ser cargados al entorno de trabajo cuando se necesiten;
- si un dataset está definido en `DATA_REGISTRY.csv` pero no está accesible en la sesión, **no debe inventarse su contenido**; se debe trabajar con su `path_relative` y `load_instructions`.

## 5. Estructura conceptual del pipeline

El pipeline actual está organizado en fases modulares y secuenciales.

### Fase 00. Auditoría estructural raw
**Script canónico:** `SCRIPTS/00_audit_raw_dictionary.R`

Objetivos principales:

- lectura conservadora del Excel fuente;
- construcción de diccionario por hoja;
- perfil estructural de variables;
- matriz de presencia por año;
- crosswalk preliminar y detección de campos candidatos IARC;
- generación de logs y anexos para auditoría.

Outputs típicos:

- `DATA/DERIVED/rcpa_raw_exact.rds`
- `DATA/DERIVED/rcpa_raw_tagged.rds`
- `REPORTS/data_dictionary_by_sheet.csv`
- `REPORTS/variable_quality_profile.csv`
- `REPORTS/data_dictionary_crosswalk.csv`
- `REPORTS/data_audit_log.json`

### Fase 01. Armonización estructural inicial
**Script canónico:** `SCRIPTS/01_build_harmonization_pipeline.R`

Objetivos principales:

- usar como insumo la auditoría oficial previa;
- armonizar variables sin recalcular la fase 00;
- preservar familias semánticamente delicadas sin colapsarlas prematuramente;
- excluir anomalías estructurales oficiales;
- construir datasets armonizados largo y ancho para la fase semántica.

Outputs típicos:

- `DATA/DERIVED/rcpa_arequipa_2015_2022_harmonized_long.rds`
- `DATA/DERIVED/rcpa_arequipa_2015_2022_harmonized_wide.rds`
- `REPORTS/harmonization_dictionary.csv`
- `REPORTS/harmonization_action_log.csv`
- `REPORTS/harmonization_exclusion_log.csv`
- `REPORTS/harmonization_pending_semantic_review.csv`

### Fase 02. Recodificación semántica inicial y perfilado de dominios
**Script canónico:** `SCRIPTS/02_semantic_recoding_and_domain_profiling.R`

Objetivos principales:

- perfilar dominios globales y por año de variables prioritarias;
- generar candidatos reversibles de recodificación semántica;
- producir frecuencias específicas para variables críticas;
- documentar pendientes manuales antes del cierre analítico.

Variables prioritarias explícitas de esta fase incluyen:

- `sexo`
- `edad`
- `base_diagnostico`
- `estado_vital`
- `lateralidad`
- `residencia__res`
- `residencia__deptres`
- `residencia__provdist`
- `multiple_primary__pmseq`
- `multiple_primary__pmtot`
- `multiple_primary__pmcod`

Outputs típicos:

- `REPORTS/domain_profile_priority_variables.csv`
- `REPORTS/domain_profile_priority_variables_by_year.csv`
- `REPORTS/semantic_recode_candidates.csv`
- `REPORTS/semantic_recode_pending_manual_review.csv`
- `REPORTS/value_frequency_base_diagnostico.csv`
- `REPORTS/value_frequency_estado_vital.csv`
- `REPORTS/value_frequency_lateralidad.csv`
- `REPORTS/value_frequency_sexo.csv`

### Fase 03. Construcción de diccionario semántico
**Script canónico:** `SCRIPTS/03_semantic_dictionary_building.R`

Esta fase consolida decisiones semánticas en reglas reutilizables y maestros tabulares, preservando reversibilidad y trazabilidad.

### Fase 04. Estandarización semántica
**Script disponible:** `SCRIPTS/04_semantic_standardization.R`

Aplica reglas explícitas para estandarización una vez que la evidencia semántica y los diccionarios locales han sido suficientemente revisados.

### Fase 05. Construcción del dataset analítico
**Script canónico:** `SCRIPTS/05_build_analytic_dataset.R`

Objetivos principales:

- leer `harmonized_wide` y maestros tabulares de `DATA/DERIVED/METADATA/`;
- aplicar diccionarios de valores para sexo, base diagnóstica, estado vital y lateralidad;
- derivar edad numérica y grupos etarios;
- construir residencia analítica por jerarquía;
- generar flags de calidad y elegibilidad analítica;
- fijar la regla activa de denominadores según los maestros.

Outputs típicos:

- `DATA/DERIVED/ANALYTIC/rcpa_arequipa_2015_2022_analytic_dataset.csv`
- `DATA/DERIVED/ANALYTIC/rcpa_arequipa_2015_2022_analytic_dataset.rds`
- `DATA/DERIVED/ANALYTIC/rcpa_arequipa_2015_2022_analytic_dataset_flag_summary.csv`
- `DATA/DERIVED/ANALYTIC/rcpa_arequipa_2015_2022_analytic_dataset_active_denominator_rule.csv`

## 6. Decisiones metodológicas ya cerradas

Las decisiones maestras del proyecto están fijadas en `DOCUMENTATION/MASTER/`.

### 6.1 Denominadores
Fuente vinculante:
- `DOCUMENTATION/MASTER/MASTER_DENOMINATOR_AND_STANDARDIZATION_RULES.md`

Regla principal activa:

- modo principal: `mid_period`
- año de referencia: `2018`
- ámbito principal: `Provincia de Arequipa`
- estándar poblacional principal: `international_iarc_segi`

### 6.2 Base diagnóstica
Fuente vinculante:
- `DOCUMENTATION/MASTER/MASTER_ANALYTIC_DECISIONS.md`

Se adopta el catálogo local documentado de `BASE #7`, con derivación analítica explícita de:

- `basis_of_diagnosis_group`
- `flag_mv_candidate`
- `flag_dco_candidate`

### 6.3 Edad
Se conserva la variable raw y se derivan al menos:

- `age_numeric`
- `age_group_iarc`
- `age_group_broad`

### 6.4 Estado vital y lateralidad
Estas variables tienen decisión parcialmente cerrada y requieren validación empírica/documental adicional antes de cualquier recodificación agresiva.

### 6.5 Residencia
La residencia analítica **no** se toma de una sola variable cruda. La jerarquía declarada es:

1. `residence_provdist_raw`
2. `residence_res_raw`
3. `residence_deptres_raw`

La exclusión por no residencia es **analítica**, no física.

## 7. Variables y familias que requieren prudencia especial

No deben colapsarse ni recodificarse agresivamente sin validación específica:

- `base_diagnostico`
- `estado_vital`
- `lateralidad`
- `residencia__res`
- `residencia__deptres`
- `residencia__provdist`
- `multiple_primary__pmseq`
- `multiple_primary__pmtot`
- `multiple_primary__pmcod`

## 8. Maestros tabulares vinculantes para la fase analítica

La fase analítica depende de los siguientes CSV maestros en `DATA/DERIVED/METADATA/`:

- `MASTER_ANALYTIC_DATASET_SPEC.csv`
- `MASTER_VARIABLE_DECISION_TABLE.csv`
- `MASTER_DENOMINATOR_RULES.csv`
- `MASTER_VALUE_DICTIONARY.csv`
- `MASTER_RESIDENCE_RULES.csv`
- `MASTER_ANALYTIC_FLAGS_RULES.csv`
- `MASTER_AGE_GROUP_RULES.csv`

Estos archivos constituyen la capa canónica para parametrizar el dataset analítico y deben preferirse frente a hardcoding en scripts.

## 9. Reportes maestros y documentación de soporte

Documentos maestros actuales:

- `DOCUMENTATION/MASTER/MASTER_ANALYTIC_DECISIONS.md`
- `DOCUMENTATION/MASTER/MASTER_DENOMINATOR_AND_STANDARDIZATION_RULES.md`
- `DOCUMENTATION/MASTER/MASTER_PENDING_USER_DECISIONS.md`
- `DOCUMENTATION/MASTER/MASTER_REPORTING_BLUEPRINT.md`
- `REPORTS/METADATA/MASTER_DATA_STATUS_REPORT.md`

Estos documentos fijan, respectivamente, decisiones analíticas, denominadores y estandarización, decisiones aún abiertas, blueprint mínimo de reporte y estado general de datos/outputs.

## 10. Cómo reproducir el flujo de trabajo

### 10.1 Requisitos mínimos

- R
- paquetes del ecosistema tidyverse y dependencias declaradas por los scripts
- estructura local del proyecto respetando carpetas `DATA/`, `REPORTS/`, `SCRIPTS/` y `META_MANIFESTS/`
- acceso al archivo raw si se desea ejecutar la fase 00

### 10.2 Orden recomendado

1. revisar `META_MANIFESTS/`
2. revisar `DOCUMENTATION/MASTER/`
3. ejecutar, según corresponda:

```r
source("SCRIPTS/00_audit_raw_dictionary.R")
source("SCRIPTS/01_build_harmonization_pipeline.R")
source("SCRIPTS/02_semantic_recoding_and_domain_profiling.R")
source("SCRIPTS/03_semantic_dictionary_building.R")
source("SCRIPTS/04_semantic_standardization.R")
source("SCRIPTS/05_build_analytic_dataset.R")
```

### 10.2.1 Corrida contractual limpia

Para validar reproducibilidad operativa y mantener Git liviano, la secuencia contractual es:

```r
system("Rscript SCRIPTS/run_preflight_checks.R")
system("CLEAN_DRY_RUN=false CLEAN_CONFIRM=YES Rscript SCRIPTS/clean_regenerable_outputs.R")
system("Rscript SCRIPTS/run_pipeline.R --profile full")
```

Los HTML renderizados, el portal web compilado, las figuras, los logs de corrida y las tablas derivadas regenerables se validan localmente y luego se vuelven a limpiar. El repositorio público debe conservar solo fuentes, scripts, configuración y maestros estructurales.

### 10.3 Regla importante

La auditoría raw es una fase formal. Si sus outputs oficiales ya existen y son los vigentes, las fases posteriores deben trabajar **downstream** de esos outputs en lugar de rehacer la auditoría desde cero.

## 11. Qué sí y qué no debe versionarse en GitHub

### Sí debe versionarse

- scripts `.R`
- manifiestos
- maestros CSV estructurales
- documentación técnica en `.md`, `.qmd`, `.yaml`, `.json`
- logs y metadatos no sensibles que sostienen reproducibilidad

### No debe versionarse

- bases raw con información individual del registro
- derivados analíticos sensibles cuando correspondan a datos no públicos
- archivos cargados solo para uso operativo en una sesión privada
- HTML renderizados, portal Quarto compilado, figuras, logs y otros outputs reconstruibles del pipeline

## 12. Alcance analítico del repositorio

Este repositorio está orientado a sostener al menos:

- evaluación de calidad estructural del insumo raw;
- reconstrucción armónica de variables prioritarias del registro;
- preparación de dataset analítico para incidencia y control de calidad;
- derivación de indicadores como candidatos MV, DCO, residencia analítica y PSU provisional;
- producción posterior de tablas, figuras y texto automático según blueprint de reporte.

## 13. Limitaciones explícitas

- El repositorio público no garantiza acceso a los datos fuente confidenciales.
- Algunas decisiones semánticas siguen dependiendo de evidencia documental local o revisión manual.
- El área real de captura puede no coincidir perfectamente con todos los escenarios alternativos de denominadores.
- La existencia de outputs previos no implica que todos deban versionarse en GitHub; la política de versionado depende del nivel de sensibilidad y de su rol estructural.

## 14. Estado actual del proyecto

Al momento de esta versión, el repositorio ya contiene:

- manifiestos de gobernanza;
- scripts canónicos 00, 01, 02, 03, 04 y 05;
- maestros tabulares para cierre del dataset analítico;
- reportes y metadatos de auditoría, armonización y semántica;
- una ruta explícita para construir el dataset analítico final sin romper trazabilidad.

## 15. Nota para futuras iteraciones

Cualquier nueva iteración del proyecto debe:

1. empezar leyendo los manifiestos;
2. respetar la separación entre auditoría, armonización, semántica y analítica;
3. evitar reescribir decisiones ya fijadas en los documentos maestros, salvo evidencia fuerte en contra;
4. mantener reversibilidad cuando una codificación siga siendo preliminar;
5. documentar toda nueva decisión metodológica en maestros o tablas vinculantes, no solo en texto narrativo.

---

**Autor / responsable del repositorio:** Percy Soto Becerra  
MD, epidemiólogo y científico de datos en salud

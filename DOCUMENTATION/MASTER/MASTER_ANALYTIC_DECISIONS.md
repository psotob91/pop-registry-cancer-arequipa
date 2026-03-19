# MASTER_ANALYTIC_DECISIONS
Proyecto: Registro Poblacional de Cáncer de Arequipa 2015–2022  
Versión: v1.1  
Fecha: 2026-03-18

## 1. Propósito

Este documento fija las decisiones metodológicas para cerrar el dataset analítico del RCBPA 2015–2022, preservando trazabilidad, compatibilidad con prácticas históricas del registro y flexibilidad para análisis futuros.

## 2. Principio general

Se adopta un enfoque dual controlado:

- **Modo principal:** población de mitad de periodo, análogo a la práctica histórica del RCBPA 2008–2014.
- **Modo alternativo:** denominadores anuales 2015–2022, activables sin reescribir la lógica del pipeline.

## 3. Decisiones cerradas

### 3.1 Denominadores
- Estrategia principal: **mid_period**
- Año de referencia: **2018**
- Ámbito principal: **Provincia de Arequipa**
- Justificación: continuidad histórica y disponibilidad más robusta de denominadores.

### 3.2 Base diagnóstica
Se adopta el catálogo local documentado de `BASE #7`:
0 DCO; 1 clínico; 2 imágenes; 3 endoscopía; 4 cirugía exploratoria; 5 marcadores tumorales; 6 citología/hematología; 7 histología de metástasis; 8 histología de tumor primario; 9 autopsia; 10 desconocido.

Variables derivadas:
- `basis_of_diagnosis_group`
- `flag_mv_candidate`
- `flag_dco_candidate`

### 3.3 Sexo
- 1 = male
- 2 = female
- otros = unknown

### 3.4 Edad
- conservar `age_raw`
- derivar `age_numeric`
- derivar `age_group_iarc`
- derivar `age_group_broad`

## 4. Decisiones parcialmente cerradas

### 4.1 Estado vital
- 1 = alive
- 2 = dead
- otros = unknown_or_unresolved

Obligatorio: validación cruzada con `date_of_death` y `date_last_contact`.

### 4.2 Lateralidad
- 1 right
- 2 left
- 3 bilateral
- 4 other_or_not_stated
- 9 unknown_or_not_applicable
- 0 unresolved_local

No usar para exclusión; no colapsar automáticamente el código 0.

### 4.3 Residencia
La residencia analítica no se toma de una sola variable cruda. Se deriva por jerarquía:

1. `residence_provdist_raw`
2. `residence_res_raw`
3. `residence_deptres_raw`

La exclusión por no residencia es **analítica**, no física.

## 5. Ámbito geográfico analítico

### Opción principal activa
- `provincia_arequipa`

### Opción alternativa parametrizable
- `arequipa_metropolitana`

## 6. Reglas de missing y unknown
- unknown no implica exclusión automática
- conservar siempre la variable raw
- toda pérdida de información debe ser trazable
- unknown debe alimentar indicadores de calidad

## 7. Indicadores de calidad a derivar
- MV %
- DCO %
- edad desconocida %
- sexo desconocido %
- residencia desconocida %
- consistencia vital
- PSU candidate (provisional)

## 8. Archivos tabulares vinculantes para R
- `MASTER_ANALYTIC_DATASET_SPEC.csv`
- `MASTER_VARIABLE_DECISION_TABLE.csv`
- `MASTER_DENOMINATOR_RULES.csv`
- `MASTER_VALUE_DICTIONARY.csv`
- `MASTER_RESIDENCE_RULES.csv`
- `MASTER_ANALYTIC_FLAGS_RULES.csv`
- `MASTER_AGE_GROUP_RULES.csv`

## 9. Decisiones abiertas
Ver `MASTER_PENDING_USER_DECISIONS.md`.

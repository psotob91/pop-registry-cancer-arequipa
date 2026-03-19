# Semantic standardization summary

Fecha de corrida: 2026-03-17

## Reglas respetadas
- No se sobrescribió ningún output oficial previo.
- Se generaron solo artefactos versionados en REPORTS/SEMANTIC.
- BASE #7 permaneció sin decodificación final.
- Sexo, estado_vital y lateralidad se tratan como reglas provisionales salvo promoción explícita.

## Conteos principales
- Variables pendientes analizadas: 22
- Valores distintos perfilados: 18266
- Updates de diccionario generados: 18266
- Decisiones auto-safe: 34
- Decisiones probables: 9
- Decisiones desconocidas: 18223

## Prioridades sugeridas
1. Resolver BASE #7 con documento local antes de MV/DCO.
2. Resolver significado del código 0 en lateralidad.
3. Confirmar si 1/2 en estado_vital y sexo pueden promocionarse a auto-safe.
4. Construir catálogos para residencia y múltiple primario usando frecuencia + clustering.

## Outputs
- `semantic_dictionary_updates_20260317.csv`
- `semantic_decision_log_20260317.csv`
- `local_unknown_codes_registry_updated_20260317.csv`
- `semantic_pattern_clusters_20260317.csv`
- `semantic_value_profile_20260317.csv`
- `semantic_standardization_preview_long_20260317.rds`

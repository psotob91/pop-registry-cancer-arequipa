# Semantic dictionary summary

Fecha de corrida: 2026-03-17

## Outputs generados
- `local_dictionary_base_diagnostico.csv`
- `local_dictionary_estado_vital.csv`
- `local_dictionary_lateralidad.csv`
- `local_dictionary_sexo.csv`
- `local_unknown_codes_registry.csv`
- `semantic_dictionary_manual_resolution_template.csv`
- `semantic_crosswalk_proposed.csv`
- `semantic_dictionary_rulebook.md`

## Estado
- SEXO: casi cerrable tras validación local corta.
- ESTADO_VITAL: casi cerrable para 1/2, con revisión de valores raros.
- LATERALIDAD: parcialmente cerrable, pero el código 0 requiere revisión manual prioritaria.
- BASE_DIAGNOSTICO: permanece abierto; solo se construye diccionario operativo preliminar.

## Recomendación
Completar la plantilla de resolución manual y congelar un crosswalk final antes de aplicar recodificación semántica a la base harmonized_wide.

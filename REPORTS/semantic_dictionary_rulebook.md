# Semantic dictionary rulebook

Fecha de corrida: 2026-03-17

## Alcance
Esta fase construye diccionarios locales provisionales y reversibles para sexo, estado_vital, lateralidad y base_diagnostico.
No calcula todavía indicadores IARC finales.

## Reglas provisionales sugeridas

### SEXO
- `1` -> `male` (provisional alta confianza)
- `2` -> `female` (provisional alta confianza)

### ESTADO_VITAL
- `1` -> `alive` (provisional alta confianza)
- `2` -> `dead` (provisional alta confianza)
- otros códigos/textos observados -> revisión manual

### LATERALIDAD
- `1` -> `right` (provisional)
- `2` -> `left` (provisional)
- `3` -> `bilateral` (provisional)
- `4` y `9` -> `not_applicable_or_other` (provisional)
- `0` -> NO congelar semántica; requiere revisión manual prioritaria

### BASE_DIAGNOSTICO
- No congelar equivalencias semánticas finales sin documento local de BASE #7.
- Priorizar revisión de códigos `7`, `6`, `8`, `2`, `10` por frecuencia y/o patrón temporal.

## Criterios de resolución manual
1. Priorizar códigos frecuentes y presentes en múltiples años.
2. Contrastar con documentos del registro, práctica operativa local y consistencia con otras variables.
3. Toda decisión final debe registrarse en la plantilla de resolución manual.
4. Mantener siempre una regla reversible raw -> std.

## Próxima fase sugerida
Aplicar diccionarios congelados sobre la base armonizada y recién después construir el script de QC epidemiológico (MV, DCO, PSU, edad/sexo desconocidos, seguimiento).

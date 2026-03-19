# Semantic phase summary

Fecha de corrida: 2026-03-13

## Alcance de esta fase
- Perfilado de dominios global y por año para variables prioritarias.
- Construcción de candidatos preliminares de recodificación semántica para sexo, estado_vital, lateralidad y base_diagnostico.
- Chequeos básicos de consistencia entre estado vital y fechas, plausibilidad de edad y formatos de fechas.
- Preparación de pendientes manuales y trazabilidad exportable.

## Reglas metodológicas respetadas
- No se rehizo la auditoría estructural ni la armonización previa.
- No se impuso semántica final a BASE #7.
- RES, DEPTRES y PROVDIST se mantuvieron separados.
- PMSEQ, PMTOT y PMCOD se mantuvieron separados.
- No se calcularon todavía MV, DCO, PSU ni otros indicadores finales IARC.

## Cobertura
- Variables prioritarias solicitadas: 19
- Variables prioritarias disponibles en harmonized_wide: 15
- Variables prioritarias ausentes en harmonized_wide: residencia__res, multiple_primary__pmseq, multiple_primary__pmtot, multiple_primary__pmcod

## Outputs principales
- `domain_profile_priority_variables.csv`
- `domain_profile_priority_variables_by_year.csv`
- `semantic_recode_candidates.csv`
- `semantic_recode_pending_manual_review.csv`
- `semantic_consistency_checks.csv`

## Próximo paso sugerido
Validar manualmente el diccionario local de BASE #7, ESTVIT y LATE 19, y luego cerrar reglas reversibles de recodificación para sexo, estado_vital y lateralidad antes de pasar al QC epidemiológico.

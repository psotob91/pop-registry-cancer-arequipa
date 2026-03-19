# Harmonization summary

Fecha de corrida: 2026-03-13

## Insumos oficiales usados
- raw_exact: `C:/Users/Usuario/OneDrive/Consultorias_Personales/Fundacion_City_Cancer/pop-registry-cancer-arequipa/DATA/DERIVED/rcpa_raw_exact.rds`
- raw_tagged: `C:/Users/Usuario/OneDrive/Consultorias_Personales/Fundacion_City_Cancer/pop-registry-cancer-arequipa/DATA/DERIVED/rcpa_raw_tagged.rds`
- crosswalk estructural: `C:/Users/Usuario/OneDrive/Consultorias_Personales/Fundacion_City_Cancer/pop-registry-cancer-arequipa/REPORTS/data_dictionary_crosswalk.csv`
- perfil de calidad: `C:/Users/Usuario/OneDrive/Consultorias_Personales/Fundacion_City_Cancer/pop-registry-cancer-arequipa/REPORTS/variable_quality_profile.csv`
- perfil de dominios armonizados previo: `C:/Users/Usuario/OneDrive/Consultorias_Personales/Fundacion_City_Cancer/pop-registry-cancer-arequipa/REPORTS/harmonized_domain_profile.csv`
- disponibilidad estructural para indicadores: `C:/Users/Usuario/OneDrive/Consultorias_Personales/Fundacion_City_Cancer/pop-registry-cancer-arequipa/REPORTS/quality_indicator_field_availability.csv`
- audit log oficial: `C:/Users/Usuario/OneDrive/Consultorias_Personales/Fundacion_City_Cancer/pop-registry-cancer-arequipa/REPORTS/data_audit_log.json`
- MASTER_VARIABLE_MAP: `C:/Users/Usuario/OneDrive/Consultorias_Personales/Fundacion_City_Cancer/pop-registry-cancer-arequipa/DATA/DERIVED/METADATA/MASTER_VARIABLE_MAP.csv`
- MASTER_DATA_DICTIONARY: `C:/Users/Usuario/OneDrive/Consultorias_Personales/Fundacion_City_Cancer/pop-registry-cancer-arequipa/DATA/DERIVED/METADATA/MASTER_DATA_DICTIONARY.csv`

## Resultado de esta fase
- Filas en dataset armonizado ancho: 14423
- Registros en dataset armonizado largo: 433957
- Variables armonizadas distintas: 45
- Exclusiones registradas: 130
- Pendientes de revisión semántica: 26

## Reglas metodológicas respetadas
- BASE #7 se preservó como `base_diagnostico` sin decodificación final.
- RES, DEPTRES y PROVDIST se mantuvieron como familia separada.
- PMSEQ, PMTOT y PMCOD se mantuvieron como familia separada.
- `blank_col_2` y `673668` quedaron excluidas como anomalías estructurales oficiales.
- No se calcularon todavía MV, DCO, PSU ni otros indicadores IARC.

## Próximo paso sugerido
Construir la revisión semántica/codificación local priorizando: BASE #7, ESTVIT, LATE 19, residencia y múltiple primario.

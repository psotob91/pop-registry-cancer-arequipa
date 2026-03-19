# MASTER_DENOMINATOR_AND_STANDARDIZATION_RULES

## 1. Principio general

Se adopta un sistema dual:

- **modo principal:** población de mitad de periodo
- **modo alternativo:** población anual

## 2. Denominador principal

- año de referencia: **2018**
- ámbito geográfico principal: **Provincia de Arequipa**
- periodo analítico: **2015–2022**

## 3. Denominador alternativo

- serie anual 2015–2022
- activable para análisis temporal, sensibilidad o requerimiento institucional

## 4. Fuentes

- Compendio Estadístico Arequipa 2022
- INEI
- tablas históricas del RCBPA para criterio de continuidad metodológica

## 5. Tasas

### 5.1 Crudas
casos / población * 100000

### 5.2 Específicas por edad
casos por grupo quinquenal / población del mismo grupo * 100000

### 5.3 Ajustadas
método directo

## 6. Estándar poblacional

**Principal:** `international_iarc_segi`  
**Secundario opcional:** estándar local adicional si se decide explícitamente.

## 7. Regla operativa para el pipeline

`denominator_mode = "mid_period" | "annual"`

El script analítico debe leer esta decisión desde `MASTER_DENOMINATOR_RULES.csv` y no hardcodearla.

## 8. Limitaciones

- posible discrepancia entre área real de captura y área del denominador
- disponibilidad anual desigual para algunos escenarios alternativos
- el escenario metropolitano queda provisional hasta validación explícita

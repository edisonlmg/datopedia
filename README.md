# datopedia

Suite de scripts en R para descargar, procesar y visualizar datos abiertos de instituciones públicas peruanas.

[![Project Status: Active – The project has reached a stable, usable actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

---

## Objetivo

Automatizar el análisis de información estadística y financiera publicada por entidades del Estado peruano, generando visualizaciones listas para publicar en redes sociales a partir de fuentes oficiales.

## Uso

El proyecto está organizado por subproyecto. Cada uno tiene su propio directorio con scripts, datos y figuras. Para ejecutar un análisis:

1. Abrir el proyecto `datopedia.Rproj` en RStudio.
2. Restaurar el entorno con `renv::restore()`.
3. Descargar manualmente los datos de la fuente correspondiente y guardarlos en `<subproyecto>/data/raw/`.
4. Ejecutar el script principal del subproyecto desde la raíz del proyecto.

Los datos procesados se guardan en `<subproyecto>/data/processed/` y las figuras en `<subproyecto>/figures/`.

## Subproyectos

| Subproyecto | Descripción | Fuente |
|---|---|---|
| `inei_ipc/` | Índice de Precios al Consumidor (IPC) nacional y por departamento | INEI |
| `inei_ipm/` | Índice de Precios al por Mayor (IPM) global y por subsector | INEI |
| `inei_pib/` | Producto Bruto Interno (PBI) mensual y valor agregado por sector | INEI |
| `osinergmin_combustibles/` | Precios promedio mensuales de combustibles en Lima | Osinergmin |
| `sbs_sistema_financiero/` | Resultado neto de banca múltiple, financieras, cajas municipales y cajas rurales | SBS |
| `mef_inversion_publica/` | Inversión pública ejecutada (Invierte.pe) | MEF |
| `mef_gasto_publico/` | Gasto público por programa presupuestal | MEF |

## Módulos compartidos

Los módulos en `modules/` definen funciones reutilizables para todos los subproyectos. Existen dos familias: estándar (exploración y análisis) y social (optimizadas para formato 4:5 de redes sociales, 1080 × 1350 px).

### Estándar

| Módulo | Función principal |
|---|---|
| `bar_chart.R` | `bar_chart()` — barras vertical u horizontal |
| `line_charts.R` | `line_chart()` — líneas con múltiples series |
| `choropleth_map.R` | `mapa_departamentos()` — mapas coropléticos |
| `heatmap.R` | `mapa_calor()` — mapa de calor |
| `scatter_chart.R` | `scatter_chart()` — dispersión |
| `donut_chart.R` | `donut_chart()` — gráfico de dona |
| `gt_table.R` | `gt_table()` — tabla estilizada |

### Social (formato redes sociales)

| Módulo | Función principal |
|---|---|
| `social_bar_chart.R` | `social_bar_chart()` — barras vertical u horizontal |
| `social_stacked_bar_chart.R` | `social_stacked_bar_chart()` — barras apiladas (absolutas o 100%) |
| `social_line_chart.R` | `social_line_chart()` — líneas con múltiples series |
| `social_heatmap.R` | `social_mapa_calor()` — mapa de calor con orden personalizable |
| `social_choropleth_map.R` | `social_mapa_departamentos()` — mapa coroplético departamental |
| `social_scatter_chart.R` | `social_scatter_chart()` — dispersión |
| `social_donut_chart.R` | `social_donut_chart()` — gráfico de dona |

## Estructura del proyecto

```
📁 datopedia/
├── 📁 modules/                        # funciones reutilizables
├── 📁 inei_ipc/
│   ├── 📁 data/{raw,processed}/
│   ├── 📁 figures/
│   └── 📁 src/
│       ├── 📄 calcular_inflacion.R    # calcula variaciones mensual, acumulada y anual
│       └── 📄 ipc.R                   # mapa departamental + mapa de calor por categoría
├── 📁 inei_ipm/
│   ├── 📁 data/{raw,processed}/
│   ├── 📁 figures/
│   └── 📁 src/
│       └── 📄 ipm.R                   # línea general + mapas de calor nacional e importado
├── 📁 inei_pib/
│   ├── 📁 data/{raw,processed}/
│   ├── 📁 figures/
│   └── 📁 src/
│       └── 📄 pib.R                   # validación PBI + mapa de calor valor agregado sector
├── 📁 osinergmin_combustibles/
│   ├── 📁 data/
│   ├── 📁 figures/
│   └── 📁 src/
│       └── 📄 precio_combustibles_liquidos.R  # gasoholes, diésel y GLP en Lima
├── 📁 sbs_sistema_financiero/
│   ├── 📁 data/{raw,processed}/
│   ├── 📁 figures/
│   └── 📁 src/
│       └── 📄 resultado_neto.R        # resultado neto por tipo de institución financiera
├── 📁 mef_inversion_publica/
│   ├── 📁 data/{raw,processed}/
│   ├── 📁 figures/
│   └── 📁 src/
│       └── 📄 mef_inversion_publica.R
├── 📁 mef_gasto_publico/
│   ├── 📁 data/{raw,processed}/
│   ├── 📁 figures/
│   └── 📁 src/
│       ├── 📄 fondes.R
│       └── 📄 pp_126_128.R
├── 📁 renv/
├── 📄 renv.lock
└── 📄 datopedia.Rproj
```

## Fuentes de datos

- **INEI — SIRTOD:** <https://webapp.inei.gob.pe:8443/sirtod-series/>
- **Osinergmin — SCOP:** <https://www.osinergmin.gob.pe/empresas/hidrocarburos/scop/documentos-scop>
- **SBS — Estadísticas del Sistema Financiero:** <https://intranet2.sbs.gob.pe/estadistica/financiera/>
- **MEF — Datos Abiertos:** <https://datosabiertos.mef.gob.pe/>

## Resultados

Los gráficos y tablas generados se publican en:

- X (Twitter): <https://x.com/EdisonMondragon>
- Bluesky: <https://bsky.app/profile/edisonmondragon.bsky.social>

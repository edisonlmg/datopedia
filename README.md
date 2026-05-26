# datopedia

Suite de scripts en R para descargar, procesar y visualizar datos abiertos de instituciones públicas peruanas.

[![Project Status: Active – The project has reached a stable, usable actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

---

## Objetivo

Automatizar el análisis de información estadística y financiera publicada por entidades del Estado peruano, generando visualizaciones listas para publicar a partir de fuentes oficiales.

## Uso

El proyecto está organizado por subproyecto. Cada uno tiene su propio directorio con scripts, datos y figuras. Para ejecutar un análisis:

1.  Abrir el proyecto `datopedia.Rproj` en RStudio.
2.  Restaurar el entorno con `renv::restore()`.
3.  Ejecutar primero el script `importar_datasets.R` del subproyecto para descargar los datos.
4.  Ejecutar el script de análisis principal del subproyecto.

Los datos descargados se guardan en `<subproyecto>/data/raw/`, los procesados en `<subproyecto>/data/processed/` y las figuras en `<subproyecto>/figures/`.

## Subproyectos

| Subproyecto | Descripción | Fuente |
|----------------------------------|--------------------------------------------------|--------|
| `sbs_sistema_financiero/`        | Resultado neto de banca múltiple, financieras, cajas municipales y cajas rurales | SBS |
| `mef_inversion_publica/`         | Pasivo de inversiones y otros aspectos de la inversión pública (Invierte.pe) | MEF |
| `osinergmin_combustibles/`       | Precios de combustibles por departamento y distrito | Osinergmin |
| `mef_gasto_publico/`             | Análisis del gasto público | MEF |

## Módulos compartidos

Los módulos en `modules/` definen funciones reutilizables para todos los subproyectos:

| Módulo | Función principal |
|------------------------|------------------------------------------|
| `bar_chart.R`          | `bar_chart()` — gráfico de barras vertical u horizontal |
| `line_charts.R`        | `line_chart()` — gráfico de líneas con múltiples series |
| `choropleth_map.R`     | `mapa_departamentos()`, `mapa_lima_callao()` — mapas coropléticos |
| `gt_table.R`           | `gt_table()` — tabla estilizada con formato numérico configurable |

## Estructura del proyecto

```
📁 datopedia/
├── 📁 modules/
│   ├── 📄 bar_chart.R
│   ├── 📄 choropleth_map.R
│   ├── 📄 gt_table.R
│   └── 📄 line_charts.R
├── 📁 sbs_sistema_financiero/
│   ├── 📁 data/
│   │   ├── 📁 raw/        # archivos XLS/XLSX descargados de la SBS
│   │   └── 📁 processed/  # dataset procesado en CSV
│   ├── 📁 figures/        # gráficos por tipo de empresa
│   └── 📁 src/
│       ├── 📄 importar_datasets.R  # descarga EEFF desde la SBS
│       └── 📄 resultado_neto.R     # procesa y visualiza el resultado neto
├── 📁 mef_inversion_publica/
│   ├── 📁 data/
│   │   ├── 📁 raw/        # CSVs descargados del MEF
│   │   └── 📁 processed/  # dataset procesado en CSV
│   ├── 📁 figures/        # tablas y gráficos de inversión pública
│   └── 📁 src/
│       ├── 📄 importar_datasets.R  # descarga CSVs desde Datos Abiertos del MEF
│       └── 📄 mef_inversion_publica.R  # procesa y visualiza el pasivo de inversiones
├── 📁 osinergmin_combustibles/
│   ├── 📁 data/
│   │   ├── 📁 raw/        # archivos descargados de Osinergmin
│   │   └── 📁 processed/  # precios procesados en CSV
│   ├── 📁 figures/        # mapas y gráficos de precios
│   └── 📁 src/
│       └── 📄 scop_precio_combustibles.R
├── 📁 mef_gasto_publico/
│   ├── 📁 data/
│   │   ├── 📁 raw/
│   │   └── 📁 processed/
│   ├── 📁 figures/
│   └── 📁 src/
│       └── 📄 mef_gasto_publico.R
├── 📁 renv/               # entorno virtual de paquetes
├── 📄 renv.lock
└── 📄 datopedia.Rproj
```

## Fuentes de datos

- **MEF — Datos Abiertos:** <https://datosabiertos.mef.gob.pe/>
- **SBS — Estadísticas del Sistema Financiero:** <https://intranet2.sbs.gob.pe/estadistica/financiera/>
- **Osinergmin — SCOP:** <https://www.osinergmin.gob.pe/>

## Resultados

Los gráficos y tablas generados se publican en:

- X (Twitter): <https://x.com/EdisonMondragon>
- Bluesky: <https://bsky.app/profile/edisonmondragon.bsky.social>

# datopedia

Suite de scripts en R para descargar, procesar y visualizar datos abiertos de instituciones públicas peruanas.

[![Project Status: Active – The project has reached a stable, usable actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

---

## Objetivo

Automatizar el análisis de información estadística y financiera publicada por entidades del Estado peruano, generando visualizaciones listas para publicar a partir de fuentes oficiales.

## Uso

Cada script en `src/` es independiente y cubre un tema específico. Para ejecutar un análisis:

1.  Abrir el proyecto `datopedia.Rproj` en RStudio.
2.  Restaurar el entorno con `renv::restore()`.
3.  Ejecutar el script correspondiente desde `src/`.

Los datos descargados se guardan en `data/raw/`, los procesados en `data/processed/` y las figuras en `figures/`.

## Scripts disponibles

| Script | Descripción | Fuente |
|--------------------|--------------------------------|--------------------|
| `sbs_sistema_financiero.R` | Resultado neto de banca múltiple, financieras, cajas municipales y cajas rurales | SBS |
| `mef_inversion_publica.R` | Pasivo de inversiones y otros aspectos de la inversión pública (Invierte.pe) | MEF |

## Estructura del proyecto

```
📁 datopedia/
├── 📁 data/
│   ├── 📁 raw/
│   │   ├── 📁 mef_inversion_publica/   # archivos CSV descargados del MEF
│   │   └── 📁 sbs_sistema_financiero/  # archivos XLS/XLSX descargados de la SBS
│   └── 📁 processed/                   # datasets procesados en CSV
├── 📁 figures/
│   ├── 📁 mef_inversion_publica/       # tablas y gráficos de inversión pública
│   └── 📁 sbs_sistema_financiero/      # gráficos del sistema financiero
├── 📁 src/
│   ├── 📄 graphics_functions.R         # funciones reutilizables: bar_chart, line_chart, gt_table
│   ├── 📄 mef_inversion_publica.R      # análisis de inversión pública (MEF)
│   └── 📄 sbs_sistema_financiero.R     # análisis del sistema financiero (SBS)
├── 📁 renv/                            # entorno virtual de paquetes
├── 📄 renv.lock
└── 📄 datopedia.Rproj
```

## Fuentes de datos

- **MEF — Datos Abiertos:** <https://datosabiertos.mef.gob.pe/>
- **SBS — Estadísticas del Sistema Financiero:** <https://intranet2.sbs.gob.pe/estadistica/financiera/>

## Resultados

Los gráficos y tablas generados se publican en:

- X (Twitter): <https://x.com/EdisonMondragon>
- Bluesky: <https://bsky.app/profile/edisonmondragon.bsky.social>

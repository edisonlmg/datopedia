#===============================================================================
# pasivo.R  [mef_inversion_publica]
#
# Objetivo: identificar y visualizar el pasivo de inversiones del sistema
#           Invierte.pe: inversiones activas sin presupuesto en el año actual
#           con antigüedad suficiente para considerarlas abandonadas.
#
# Prerequisito: ejecutar importar_datasets.R para descargar los datos crudos.
#
# Inputs:
#   - mef_inversion_publica/data/raw/DETALLE_INVERSIONES.csv
#   - mef_inversion_publica/data/raw/CIERRE_INVERSIONES.csv
#   - mef_inversion_publica/data/raw/INVERSIONES_DESACTIVADAS.csv
#
# Outputs:
#   - mef_inversion_publica/data/processed/mef_inversion_publica.csv
#   - mef_inversion_publica/figures/pasivo_donut_{fecha_fin}.png
#   - mef_inversion_publica/figures/pasivo_inversiones_{fecha_fin}.png
#
# Fuente: Datos Abiertos del MEF - Invierte.pe
#   https://datosabiertos.mef.gob.pe/organization/inversion-publica
#===============================================================================

source("modules/gt_table.R")
source("modules/social_donut_chart.R")

library(tidyverse)
library(lubridate)
library(janitor)
library(vroom)
library(tools)
library(fs)
library(glue)


# Parámetros --------------------------------------------------------------

fecha_fin    <- "2026-05-16"
titulo_fecha <- toupper(format(as.Date(fecha_fin), "%B %Y"))


# Rutas -------------------------------------------------------------------

dir_subproject <- "mef_inversion_publica"
dir_raw        <- path(dir_subproject, "data/raw")
dir_processed  <- path(dir_subproject, "data/processed")
dir_figures    <- path(dir_subproject, "figures")

dir_create(dir_raw)
dir_create(dir_processed)
dir_create(dir_figures)

path_dataset          <- path(dir_processed, "mef_inversion_publica.csv")
path_fig_pasivo_donut <- path(dir_figures, glue("pasivo_donut_{fecha_fin}.png"))
path_fig_pasivo_tbl   <- path(dir_figures, glue("pasivo_inversiones_{fecha_fin}.png"))


# Abrir datasets ----------------------------------------------------------

path_files <- dir_ls(dir_raw)

datasets_raw <- map(path_files, vroom) %>%
  set_names(file_path_sans_ext(basename(path_files)))

detalle      <- datasets_raw$DETALLE_INVERSIONES
cierre       <- datasets_raw$CIERRE_INVERSIONES
desactivadas <- datasets_raw$INVERSIONES_DESACTIVADAS


# Inversiones activas -----------------------------------------------------

# el archivo raw puede tener filas duplicadas por CODIGO_SNIP; se conserva la primera
activas <- detalle %>%
  distinct(CODIGO_SNIP, .keep_all = TRUE) %>%
  filter(SITUACION != "EN FORMULACION")

write_csv(activas, path_dataset)


# --- verificaciones ---

sum(duplicated(activas$CODIGO_SNIP))
nrow(activas)
sum(is.na(activas$PIM_ANIO_ACTUAL))
class(activas$PIM_ANIO_ACTUAL)
activas %>% filter(PIM_ANIO_ACTUAL > 0) %>% nrow()
activas %>% count(REGISTRADO_PMI)
unique(activas$EXPEDIENTE_TECNICO)
sum(is.na(activas$EXPEDIENTE_TECNICO))
class(activas$FECHA_VIABILIDAD)


# Pasivo de inversiones ---------------------------------------------------

# Criterio de "pasivo": inversión sin presupuesto en el año actual y con antigüedad
# suficiente para considerarla abandonada según el tipo:
#   - VIABLE (proyectos):  más de 3 años desde la fecha de viabilidad
#   - APROBADO (IOARR):    más de 1 año desde la fecha de aprobación
pasivo_inversiones <- activas %>%
  mutate(
    NIVEL         = factor(NIVEL, levels = c("GN", "GR", "GL")),
    DEVENGADO     = DEVEN_ACUMUL_ANIO_ANT + DEV_ANIO_ACTUAL,
    EJECUTO_GASTO = if_else(DEVENGADO > 0, "SI", "NO")
  ) %>%
  filter(
    !(PIM_ANIO_ACTUAL > 0),
    (SITUACION == "VIABLE")    & (FECHA_VIABILIDAD < (today() - years(3)))
    | (SITUACION == "APROBADO") & (FECHA_VIABILIDAD < (today() - years(1)))
  ) %>%
  group_by(NIVEL) %>%
  summarise(
    PASIVO                 = n(),
    `SIN GASTO`            = sum(!(DEVENGADO > 0)),
    `SIN ET`               = sum(EXPEDIENTE_TECNICO == "NO"),
    `SIN EJECUCION FISICA` = sum(TIENE_AVAN_FISICO == "NO"),
    .groups = "drop"
  ) %>%
  adorn_totals() %>%
  mutate(
    `SIN GASTO %`           = round(`SIN GASTO`           / PASIVO * 100, 1),
    `SIN ET %`              = round(`SIN ET`              / PASIVO * 100, 1),
    `SIN EJECUCION FISICA %` = round(`SIN EJECUCION FISICA` / PASIVO * 100, 1)
  ) %>%
  select(
    NIVEL, PASIVO,
    `SIN GASTO`, `SIN GASTO %`,
    `SIN ET`, `SIN ET %`,
    `SIN EJECUCION FISICA`, `SIN EJECUCION FISICA %`
  )


# fig: donut estado presupuestal ------------------------------------------

total_pasivo   <- pasivo_inversiones %>% filter(NIVEL == "Total")
n_pasivo       <- total_pasivo$PASIVO
pct_pasivo     <- round(n_pasivo / nrow(activas) * 100, 1)
pct_sin_gasto  <- total_pasivo$`SIN GASTO %`
pct_sin_et     <- total_pasivo$`SIN ET %`
pct_sin_ejec   <- total_pasivo$`SIN EJECUCION FISICA %`

categorias_activas <- activas %>%
  mutate(
    CATEGORIA = case_when(
      PIM_ANIO_ACTUAL > 0 ~ "Cartera en ejecución",
      (SITUACION == "VIABLE"   & FECHA_VIABILIDAD < (today() - years(3))) |
      (SITUACION == "APROBADO" & FECHA_VIABILIDAD < (today() - years(1))) ~ "Cartera pasiva",
      TRUE ~ "Cartera latente"
    )
  ) %>%
  count(CATEGORIA) %>%
  arrange(match(CATEGORIA, c("Cartera en ejecución", "Cartera latente", "Cartera pasiva")))

fig_pasivo_donut <- social_donut_chart(
  labels       = categorias_activas$CATEGORIA,
  values       = categorias_activas$n,
  colores      = c("verde_claro", "marron", "rojo"),
  title        = glue("PERÚ | INVERSIONES ACTIVAS - {titulo_fecha}"),
  subtitle     = paste(
    glue("El Estado tiene un pasivo de {format(n_pasivo, big.mark = ',')} inversiones"), 
    glue("({pct_pasivo}% del total activo), que no puede ejecutar por limitaciones"),
    glue("operativas o presupuestales, de las cuales el {pct_sin_gasto}%"),
    glue("no ha ejecutado gasto, el {pct_sin_et}% no cuenta con ET o DE aprobado"),
    glue("y el {pct_sin_ejec}% no tiene ejecución física.")
    ),
  caption      = glue(
    "Cartera en ejecución: con PIM; ",
    "Cartera latente: sin PIM y viabilidad hasta 3 años (proyectos) o aprobación hasta 1 año (IOARR); ",
    "Cartera pasiva: sin PIM y antigüedad mayor a cartera latente.\n\n",
    "Nota: el criterio de antigüedad es propio, no es normativo.\n\n",
    "Fuente: Datos Abiertos del MEF al {format(as.Date(fecha_fin), '%d.%m.%Y')} | X: @EdisonMondragon"
  ),
  center_label = paste0(format(nrow(activas), big.mark = ","), "\nactivas"),
  center_size  = 7
)

ggsave(path_fig_pasivo_donut, plot = fig_pasivo_donut, width = 8, height = 10, dpi = 135)


# fig: tabla pasivo por nivel de gobierno ---------------------------------

pasivo_tbl <- gt_table(
  data           = pasivo_inversiones,
  title          = glue("PASIVO DE INVERSIONES - {titulo_fecha}"),
  subtitle       = "Inversiones activas (viables o aprobadas) sin presupuesto 2026 y con antigüedad mayor de 3 años para proyectos y 1 año para IOARR",
  caption        = glue("Fuente: Datos Abiertos del MEF al {format(as.Date(fecha_fin), '%d.%m.%Y')} | Elaborado por: @EdisonMondragon"),
  cols_decimales = c(`SIN GASTO %` = 1, `SIN ET %` = 1, `SIN EJECUCION FISICA %` = 1),
  theme          = "light"
)

gtsave(pasivo_tbl, path_fig_pasivo_tbl, zoom = 2, vwidth = 1200)

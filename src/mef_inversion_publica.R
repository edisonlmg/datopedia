#===============================================================================
# mef-inversion-publica
#
# Objetivo: descargar y analizar diferentes aspectos de la inversión pública
#           del Sistema Nacional de Programación Multianual y Gestión de
#           Inversiones (Invierte.pe).
#
#           Análisis implementados:
#             1. Pasivo de inversiones: inversiones activas sin presupuesto
#                y con antigüedad suficiente para considerarlas abandonadas.
#
# Outputs:
#   - data/processed/mef_inversion_publica.csv
#   - figures/mef_inversion_publica/pasivo_inversiones.png
#
# Fuente: Datos Abiertos del MEF
#   https://datosabiertos.mef.gob.pe/
#===============================================================================


source("src/graphics_functions.R")


if (!require("pacman")) {install.packages("pacman", dependencies = TRUE)}
pacman::p_load(
  tidyverse,  # para manejo y visualización de datos
  lubridate,  # para manejo de fechas
  janitor,    # para adorn_totals(): agrega fila de totales a tablas
  vroom,      # para leer CSVs grandes eficientemente
  tools,      # para file_path_sans_ext()
  fs,         # para manejo de directorios
  httr        # para descarga con write_disk() y stop_for_status()
)


dir_raw       <- path("data/raw/mef_inversion_publica")
dir_processed <- path("data/processed")
dir_figures   <- path("figures/mef_inversion_publica")

path_dataset <- path(dir_processed, "mef_inversion_publica.csv")


#===============================================================================
# extraccion de datos
#===============================================================================


# DETALLE_INVERSIONES:      ficha completa de cada inversión activa (viable/aprobada)
# CIERRE_INVERSIONES:       inversiones que cerraron o culminaron
# INVERSIONES_DESACTIVADAS: inversiones dadas de baja del sistema
urls <- c(
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/DETALLE_INVERSIONES.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/CIERRE_INVERSIONES.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/INVERSIONES_DESACTIVADAS.csv"
)


iwalk(urls, ~{

  file_name   <- basename(.x)
  output_path <- path(dir_raw, file_name)

  message("Intentando descargar: ", file_name)

  tryCatch({
    respuesta <- GET(
      url = .x,
      write_disk(output_path, overwrite = TRUE), # escribe directo a disco sin cargar en memoria
      progress()
    )
    stop_for_status(respuesta) # lanza error si el servidor responde con HTTP 4xx/5xx
    message("  [OK] Descarga exitosa.\n")

  }, error = function(condicion) {
    message("  [ERROR] No se pudo descargar el archivo.\n")
  })
})


#===============================================================================
# abrir datasets
#===============================================================================


path_files <- dir_ls(dir_raw)

datasets_raw <- map(path_files, vroom) %>%
  set_names(file_path_sans_ext(basename(path_files)))

detalle      <- datasets_raw$DETALLE_INVERSIONES
cierre       <- datasets_raw$CIERRE_INVERSIONES
desactivadas <- datasets_raw$INVERSIONES_DESACTIVADAS


#===============================================================================
# procesar detalle_inversiones
#===============================================================================


# --- preparar dataset de inversiones activas ---

# el archivo raw puede tener filas duplicadas por CODIGO_SNIP; se conserva la primera
activas <- detalle %>%
  distinct(CODIGO_SNIP, .keep_all = TRUE) %>%
  filter(SITUACION != "EN FORMULACION")


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


# --- pasivo de inversiones ---

# Criterio de "pasivo": inversión sin presupuesto en el año actual y con antigüedad
# suficiente para considerarla abandonada según el tipo:
#   - VIABLE (proyectos):  más de 3 años desde la fecha de viabilidad
#   - APROBADO (IOARR):    más de 1 año desde la fecha de aprobación
pasivo_inversiones <- activas %>%
  mutate(
    NIVEL         = factor(NIVEL, levels = c("GN", "GR", "GL")),
    DEVENGADO     = DEVEN_ACUMUL_ANIO_ANT + DEV_ANIO_ACTUAL, # gasto acumulado histórico + año actual
    EJECUTO_GASTO = if_else(DEVENGADO > 0, "SI", "NO")
  ) %>%
  filter(
    !(PIM_ANIO_ACTUAL > 0),
    (SITUACION == "VIABLE")    & (FECHA_VIABILIDAD < (today() - years(3)))
    | (SITUACION == "APROBADO") & (FECHA_VIABILIDAD < (today() - years(1)))
  ) %>%
  group_by(NIVEL) %>%
  summarise(
    PASIVO                = n(),
    `SIN GASTO`           = sum(!(DEVENGADO > 0)),
    `SIN ET`              = sum(EXPEDIENTE_TECNICO == "NO"),
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


#===============================================================================
# visualizaciones
#===============================================================================


pasivo_tbl <- gt_table(
  data           = pasivo_inversiones,
  title          = "PASIVO DE INVERSIONES",
  subtitle       = "Inversiones activas (viables o aprobadas) sin presupuesto 2026 y con antigüedad mayor de 3 años para proyectos y 1 año para IOARR",
  caption        = "Fuente: Datos Abiertos del MEF al 16.05.2026 | Elaborado por: @EdisonMondragon",
  cols_decimales = c(`SIN GASTO %` = 1, `SIN ET %` = 1, `SIN EJECUCION FISICA %` = 1),
  theme          = "light"
)


#===============================================================================
# guardar resultados
#===============================================================================


write_csv(activas, path_dataset)

gtsave(pasivo_tbl, path(dir_figures, "pasivo_inversiones.png"), zoom = 2, vwidth = 1200)

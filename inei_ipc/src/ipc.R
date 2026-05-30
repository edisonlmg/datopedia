#===============================================================================
# ipc.R  [inei_ipc]
#
# Objetivo: procesar y visualizar el Índice de Precios al Consumidor (IPC)
#           a nivel nacional y por departamento/indicador del Perú.
#
# Inputs (descarga manual desde la fuente):
#   - inei_ipc/data/raw/ipc_region_2021.xlsx   (IPC por departamento e indicador)
#   - inei_ipc/data/raw/ipc_nacional_2021.xlsx  (IPC nacional por indicador)
#
# Outputs:
#   - inei_ipc/data/processed/ipc_region.csv
#   - inei_ipc/data/processed/ipc_nacional.csv
#   - inei_ipc/figures/mapa_anual_depto.png
#   - inei_ipc/figures/calor_anual.png
#
# Fuente: INEI - Índice de Precios al Consumidor
#   https://webapp.inei.gob.pe:8443/sirtod-series/
#===============================================================================

source("inei_ipc/src/calcular_inflacion.R")
source("modules/social_choropleth_map.R")
source("modules/social_heatmap.R")

library(tidyverse)
library(readxl)
library(glue)
library(fs)


# Parámetros --------------------------------------------------------------

fecha_inicio <- "2022-01-01"
fecha_fin    <- "2026-04-01"

fechas       <- seq(as.Date(fecha_inicio), as.Date(fecha_fin), by = "month")
titulo_fecha <- toupper(format(as.Date(fecha_fin), "%B %Y"))


# Rutas -------------------------------------------------------------------

dir_subproject <- "inei_ipc"
dir_raw        <- path(dir_subproject, "data/raw")
dir_processed  <- path(dir_subproject, "data/processed")
dir_figures    <- path(dir_subproject, "figures")

dir_create(dir_raw)
dir_create(dir_processed)
dir_create(dir_figures)

path_ipc_nacional <- path(dir_processed, "ipc_nacional.csv")
path_ipc_region   <- path(dir_processed, "ipc_region.csv")

path_fig_mapa  <- path(dir_figures, glue("inflacion_departamento_{fecha_fin}.png"))
path_fig_calor <- path(dir_figures, glue("inflacion_categoria_{fecha_fin}.png"))


# IPC nacional ------------------------------------------------------------

# Col 1 = ámbito (siempre "Total Nacional"), col 2 = indicador,
# col 3 = unidad, col 4+ = valores mensuales con punto decimal

raw_ipc_nacional <- read_excel(
  path(dir_raw, "ipc_nacional_2021.xlsx"),
  sheet     = 1,
  skip      = 3,
  col_names = FALSE
)

names(raw_ipc_nacional) <- c(".ambito", "INDICADOR", ".drop", as.character(fechas))

ipc_nacional <- raw_ipc_nacional %>%
  select(-`.ambito`, -`.drop`) %>%
  filter(!is.na(INDICADOR)) %>%
  pivot_longer(
    cols      = -INDICADOR,
    names_to  = "FECHA",
    values_to = "IPC"
  ) %>%
  mutate(
    FECHA     = as.Date(FECHA),
    IPC       = as.numeric(IPC),
    INDICADOR = trimws(gsub("\\(Base Diciembre 2021\\)", "", INDICADOR)),
    INDICADOR = paste0(toupper(substr(INDICADOR, 1, 1)),
                       tolower(substr(INDICADOR, 2, nchar(INDICADOR))))
  )

ipc_nacional <- calcular_inflacion(ipc_nacional, grupos = "INDICADOR")

write_csv(ipc_nacional, path_ipc_nacional)


# IPC por región ----------------------------------------------------------

# Col 1 = departamento (celdas combinadas), col 2 = indicador,
# col 3 = vacía, col 4+ = valores mensuales con coma decimal

raw_ipc_region <- read_excel(
  path(dir_raw, "ipc_region_2021.xlsx"),
  sheet     = 1,
  skip      = 15,
  col_names = FALSE
)

names(raw_ipc_region) <- c("DEPARTAMENTO", "INDICADOR", ".drop", as.character(fechas))

ipc_region <- raw_ipc_region %>%
  select(-`.drop`) %>%
  mutate(DEPARTAMENTO = na_if(DEPARTAMENTO, "NA")) %>%
  fill(DEPARTAMENTO, .direction = "down") %>%
  filter(!is.na(INDICADOR)) %>%
  pivot_longer(
    cols      = -c(DEPARTAMENTO, INDICADOR),
    names_to  = "FECHA",
    values_to = "IPC"
  ) %>%
  mutate(
    FECHA = as.Date(FECHA),
    IPC   = as.numeric(gsub(",", ".", IPC))
  )

ipc_region <- calcular_inflacion(ipc_region, grupos = c("DEPARTAMENTO", "INDICADOR"))

write_csv(ipc_region, path_ipc_region)


# inflacion nacional ------------------------------------------------------

ipc_nacional %>%
  filter(
    INDICADOR == "Índice general de precios al consumidor a nivel nacional"
    ) %>%
  select(-INDICADOR) %>%
  tail()


# inflacion lima metropolitana --------------------------------------------

ipc_region %>%
  filter(
    DEPARTAMENTO == "LIMA",
    INDICADOR == "Índice general de precios al consumidor"
    ) %>%
  select(-INDICADOR) %>%
  tail()


# fig: mapa anual por departamento ----------------------------------------

fig_mapa <- ipc_region %>%
  filter(
    grepl("ndice general", INDICADOR, ignore.case = TRUE),
    FECHA == as.Date(fecha_fin)
  ) %>%
  social_mapa_departamentos(
    value_col    = "ANUAL",
    color_alto   = "rojo",
    fondo        = "beige", 
    title        = paste(
      "INFLACIÓN ANUAL POR DEPARTAMENTO -",
      glue("{titulo_fecha} (%)")
      ),
    subtitle     = paste(
      "La inflación se incrementó fuertemente en Madre de Dios,",
      "seguido de Moquegua y Arequipa"
    ),
    caption      = "Fuente: INEI | X: @EdisonMondragon",
    label_format = "%.1f"
  )

ggsave(path_fig_mapa, plot = fig_mapa, width = 8, height = 10, dpi = 135)


# fig: mapa de calor anual por departamento e indicador -------------------

calor_anual <- ipc_region %>%
  filter(
    !grepl("ndice general", INDICADOR, ignore.case = TRUE),
    FECHA == as.Date(fecha_fin),
    !is.na(ANUAL)
  ) %>%
  social_mapa_calor(
    x_col              = "INDICADOR",
    y_col              = "DEPARTAMENTO",
    value_col          = "ANUAL",
    fondo              = "beige", 
    color_alto         = "rojo",
    title              = paste(
      "INFLACIÓN ANUAL POR DEPARTAMENTO Y CATEGORÍA -",
      glue("{titulo_fecha} (%)")
      ),
    subtitle           = paste(
      "Inflación del periodo es explicada principalmente por la categoría de",
      "'Transportes', siendo Madre de Dios la región más afectada"
    ),
    caption            = paste(
      "Nota: 'Transportes' es la 3ra categoría de mayor peso en la canasta",
      "básica con una participación de 12.2 %.\n\n",
      "Fuente: INEI | X: @EdisonMondragon"
    ),
    label_format       = "%.1f",
    mostrar_etiquetas  = TRUE,
    mostrar_cuadricula = TRUE
  )

ggsave(path_fig_calor, plot = calor_anual, width = 8, height = 10, dpi = 135)



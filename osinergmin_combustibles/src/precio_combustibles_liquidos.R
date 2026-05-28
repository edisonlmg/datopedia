#===============================================================================
# scop_precio_combustibles.R
#
# Objetivo: leer precios promedio mensuales de combustibles del SCOP (Osinergmin)
#           y generar gráficos de líneas de evolución de precios en Lima para los
#           últimos 12 meses, por tipo de combustible:
#             · Gasohol Regular y Gasohol Premium (serie comparativa)
#             · Diésel B5 S-50 UV
#             · GLP vehicular
#
# Input:
#   - osinergmin_combustibles/data/raw/precios_mensuales.csv
#     Formato ancho: columnas PERIODO (dd/mm/AAAA), DEPARTAMENTO y una columna
#     por producto con el precio promedio mensual en S/ por galón.
#     Descarga manual desde el SCOP de Osinergmin.
#
# Outputs (objetos ggplot en memoria — exportar a 1048 × 762 px):
#   - fig_gasoholes_lima  →  figures/gasoholes_lima.png
#   - fig_diesel_lima     →  figures/diesel_lima.png
#   - fig_glp_lima        →  figures/glp_lima.png
#
# Fuente: Osinergmin - SCOP
#   https://www.osinergmin.gob.pe/empresas/hidrocarburos/scop/documentos-scop
#   Carpeta: Reporte de Precios Mensuales
#===============================================================================

source("modules/line_charts.R")

library(tidyverse) # manejo y visualización de datos
library(lubridate) # manejo de fechas
library(fs)        # manejo de rutas y directorios


# rutas -------------------------------------------------------------------

dir_subproject <- "osinergmin_combustibles"
dir_raw        <- path(dir_subproject, "data")
dir_figures    <- path(dir_subproject, "figures")

dir_create(dir_raw)
dir_create(dir_figures)

path_p_mensuales        <- path(dir_raw,     "precios_mensuales.csv")


# abrir dataset -----------------------------------------------------------

raw_p_mensuales <- read_csv(path_p_mensuales)


# transformación ----------------------------------------------------------

# parsea fechas, filtra los últimos 12 meses y convierte a formato largo.
p_mensuales <- raw_p_mensuales %>%
  mutate(PERIODO = parse_date_time(PERIODO, orders = "d/m/Y")) %>%
  filter(
    PERIODO >= max(PERIODO, na.rm = TRUE) - months(11)
  ) %>%
  pivot_longer(
    cols      = -c(PERIODO, DEPARTAMENTO),
    names_to  = "PRODUCTO",
    values_to = "PRECIO"
  )


# fig: Lima - Gasohol Regular y Premium -----------------------------------

gasoholes_lima <- p_mensuales %>%
  filter(
    DEPARTAMENTO == "LIMA",
    PRODUCTO %in% c("GASOHOL PREMIUM", "GASOHOL REGULAR")
  )

fig_gasoholes_lima <- line_chart(
  x           = gasoholes_lima$PERIODO,
  y           = gasoholes_lima$PRECIO,
  group       = gasoholes_lima$PRODUCTO,
  title       = "PRECIO PROMEDIO MENSUAL DE GASOHOLES EN LIMA",
  subtitle    = "(Últimos 12 meses) | Fuente: Osinergmin",
  caption     = "X: @EdisonMondragon",
  y_label     = "S/ por galón",
  series_name = NULL,
  show_legend = TRUE,
  theme       = "light"
)

fig_gasoholes_lima



# fig: Lima - Diésel B5 S-50 UV -------------------------------------------

diesel_lima <- p_mensuales %>%
  filter(
    DEPARTAMENTO == "LIMA",
    PRODUCTO == "DIESEL B5 S-50 UV"
  )

fig_diesel_lima <- line_chart(
  x           = diesel_lima$PERIODO,
  y           = diesel_lima$PRECIO,
  group       = NULL,
  title       = "PRECIO PROMEDIO MENSUAL DE DIÉSEL B5 S-50 UV EN LIMA",
  subtitle    = "(Últimos 12 meses) | Fuente: Osinergmin",
  caption     = "X: @EdisonMondragon",
  y_label     = "S/ por galón",
  series_name = NULL,
  show_legend = FALSE,
  theme       = "light"
)

fig_diesel_lima



# fig: Lima - GLP vehicular -----------------------------------------------

glp_lima <- p_mensuales %>%
  filter(
    DEPARTAMENTO == "LIMA",
    PRODUCTO == "GLP"
  )

fig_glp_lima <- line_chart(
  x           = glp_lima$PERIODO,
  y           = glp_lima$PRECIO,
  title       = "PRECIO PROMEDIO MENSUAL DE GLP VEHICULAR EN LIMA",
  subtitle    = "(Últimos 12 meses) | Fuente: Osinergmin",
  caption     = "X: @EdisonMondragon",
  y_label     = "S/ por galón",
  show_legend = FALSE,
  theme       = "light"
)

fig_glp_lima










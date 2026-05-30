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
# Outputs:
#   - osinergmin_combustibles/figures/combustibles_gasoholes_{fecha_fin}.png
#   - osinergmin_combustibles/figures/combustibles_diesel_{fecha_fin}.png
#   - osinergmin_combustibles/figures/combustibles_glp_{fecha_fin}.png
#
# Fuente: Osinergmin - SCOP
#   https://www.osinergmin.gob.pe/empresas/hidrocarburos/scop/documentos-scop
#   Carpeta: Reporte de Precios Mensuales
#===============================================================================

source("modules/social_line_chart.R")

library(tidyverse) # manejo y visualización de datos
library(lubridate) # manejo de fechas
library(glue)      # interpolación de strings en rutas
library(fs)        # manejo de rutas y directorios


# rutas -------------------------------------------------------------------

dir_subproject <- "osinergmin_combustibles"
dir_raw        <- path(dir_subproject, "data")
dir_figures    <- path(dir_subproject, "figures")

dir_create(dir_raw)
dir_create(dir_figures)

path_p_mensuales <- path(dir_raw, "precios_mensuales.csv")


# abrir dataset -----------------------------------------------------------

raw_p_mensuales <- read_csv(path_p_mensuales)

fecha_fin <- format(
  max(parse_date_time(raw_p_mensuales$PERIODO, orders = "d/m/Y"), na.rm = TRUE),
  "%Y-%m-%d"
)

titulo_fecha <- toupper(format(as.Date(fecha_fin), "%B %Y"))

path_fig_gasoholes <- path(dir_figures, glue("combustibles_gasoholes_{fecha_fin}.png"))
path_fig_diesel    <- path(dir_figures, glue("combustibles_diesel_{fecha_fin}.png"))
path_fig_glp       <- path(dir_figures, glue("combustibles_glp_{fecha_fin}.png"))


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

fig_gasoholes_lima <- social_line_chart(
  x           = gasoholes_lima$PERIODO,
  y           = gasoholes_lima$PRECIO,
  group       = gasoholes_lima$PRODUCTO,
  title       = paste(
    "PERÚ | GASOHOLES: PRECIO PROMEDIO MENSUAL EN LIMA -",
    glue("{titulo_fecha} (S/ POR GALÓN)")
    ),
  subtitle = paste(
    "El incremento sostenido de los precios de los gasoholes regular y",
    "premium podría trasladarse parcialmente a los precios del transporte y",
    "de los alimentos en los próximos periodos."
  ),
  caption     = "Fuente: Osinergmin | X: @EdisonMondragon",
  series_name = NULL,
  show_legend = TRUE
)

ggsave(path_fig_gasoholes, plot = fig_gasoholes_lima, width = 8, height = 10, dpi = 135)



# fig: Lima - Diésel B5 S-50 UV -------------------------------------------

diesel_lima <- p_mensuales %>%
  filter(
    DEPARTAMENTO == "LIMA",
    PRODUCTO == "DIESEL B5 S-50 UV"
  )

fig_diesel_lima <- social_line_chart(
  x           = diesel_lima$PERIODO,
  y           = diesel_lima$PRECIO,
  color       = "rojo",
  group       = NULL,
  title       = paste(
    "PERÚ | DIÉSEL B5 S-50 UV: PRECIO PROMEDIO MENSUAL EN LIMA -",
    glue("{titulo_fecha} (S/ POR GALÓN)")
  ),
  subtitle = paste(
    "El incremento sostenido del precio del diésel podría trasladarse",
    "parcialmente a los precios del transporte y de los alimentos en los",
    "próximos periodos."
  ),
  caption     = "Fuente: Osinergmin | X: @EdisonMondragon",
  series_name = NULL,
  show_legend = FALSE
)

ggsave(path_fig_diesel, plot = fig_diesel_lima, width = 8, height = 10, dpi = 135)



# fig: Lima - GLP vehicular -----------------------------------------------

glp_lima <- p_mensuales %>%
  filter(
    DEPARTAMENTO == "LIMA",
    PRODUCTO == "GLP"
  )

fig_glp_lima <- social_line_chart(
  x           = glp_lima$PERIODO,
  y           = glp_lima$PRECIO,
  color       = "morado",
  title       = paste(
    "PERÚ | GLP VEHICULAR: PRECIO PROMEDIO MENSUAL EN LIMA -",
    glue("{titulo_fecha} (S/ POR GALÓN)")
  ),
  subtitle = paste(
    "El GLP vehicular ha mostrado una importante reducción luego de alcanzar",
    "un pico de S/ 10.0 por galón en marzo de 2026, sin embargo, aun se",
    "mantiene en un nivel alto en comparación con los meses precedentes."
  ),
  caption     = "Fuente: Osinergmin | X: @EdisonMondragon",
  show_legend = FALSE
)

ggsave(path_fig_glp, plot = fig_glp_lima, width = 8, height = 10, dpi = 135)








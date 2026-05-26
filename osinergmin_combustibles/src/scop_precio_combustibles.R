#===============================================================================
# scop_precio_combustibles.R
#
# Objetivo: descargar y calcular el precio promedio, mínimo, máximo y desviación
#           estándar actual de combustibles (Gasohol Regular, Gasohol Premium,
#           Diesel y GLP) por producto, departamento, provincia y distrito.
#
#           La fuente reporta el último precio registrado por grifo, solo cuando
#           varía. Se considera precio vigente todo registro de los últimos 15 días.
#
# Outputs:
#   - osinergmin_combustibles/data/processed/precio_actual_por_departamento.csv
#   - osinergmin_combustibles/data/processed/precio_actual_lima_callao_por_distrito.csv
#   - osinergmin_combustibles/figures/mapa_dpto_<producto>.png       (4 archivos)
#   - osinergmin_combustibles/figures/barras_distrito_<producto>.png  (4 archivos)
#
# Fuente: Osinergmin - SCOP
#   https://www.osinergmin.gob.pe/
#===============================================================================


source("modules/line_charts.R")


library(tidyverse) # para manejo y visualización de datos
library(lubridate) # para manejo de fechas
library(fs)        # para manejo de directorios
library(readxl)    # para abrir excel
library(httr)      # para descarga con write_disk() y stop_for_status()


dir_subproject <- "osinergmin_combustibles"
dir_raw        <- path(dir_subproject, "data/raw")

dir_create(dir_raw)

path_precios_mensuales_raw <- path(dir_raw, "precios_mensuales.csv")



#===============================================================================
# abrir dataset
#===============================================================================


raw_p_mensuales <- read_csv(path_precios_mensuales_raw)


#===============================================================================
# precios mensuales lima
#===============================================================================


pm_lima <- raw_p_mensuales %>%
  mutate(PERIODO = parse_date_time(PERIODO, orders = "d/m/Y")) %>%
  pivot_longer(
    cols = -c(PERIODO, DEPARTAMENTO),
    names_to = "PRODUCTO",
    values_to = "PRECIO"
  ) %>%
  filter(
    DEPARTAMENTO == "LIMA"
  )


line_chart(
  x           = pm_lima[pm_lima$PRODUCTO%in%c("GASOHOL PREMIUM","GASOHOL REGULAR"),]$PERIODO,
  y           = pm_lima[pm_lima$PRODUCTO%in%c("GASOHOL PREMIUM","GASOHOL REGULAR"),]$PRECIO,
  group       = pm_lima[pm_lima$PRODUCTO%in%c("GASOHOL PREMIUM","GASOHOL REGULAR"),]$PRODUCTO,
  title       = "PRECIO PROMEDIO MENSUAL DE GASOHOL EN LIMA",
  subtitle    = "Mayo 2025 a abril 2026 | Fuente: Osinergmin",
  caption     = "X: @EdisonMondragon",
  y_label     = "S/ por galón",
  series_name = NULL,
  show_legend = TRUE,
  theme       = "light"
)

line_chart(
  x           = pm_lima[pm_lima$PRODUCTO=="GLP",]$PERIODO,
  y           = pm_lima[pm_lima$PRODUCTO=="GLP",]$PRECIO,
  group       = pm_lima[pm_lima$PRODUCTO=="GLP",]$PRODUCTO,
  title       = "PRECIO PROMEDIO MENSUAL DE GLP VEHICULAR EN LIMA",
  subtitle    = "Mayo 2025 a abril 2026 | Fuente: Osinergmin",
  caption     = "X: @EdisonMondragon",
  y_label     = "S/ por galón",
  series_name = NULL,
  show_legend = FALSE,
  theme       = "light"
)










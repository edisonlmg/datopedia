#===============================================================================
# pib.R  [inei_pib]
#
# Objetivo: procesar y visualizar el Producto Bruto Interno (PBI) mensual
#           y el valor agregado por sector del Perú.
#
# Inputs (descarga manual desde la fuente):
#   - inei_pib/data/raw/pib_mensual.xlsx           (PBI global: índice y variaciones)
#   - inei_pib/data/raw/valor_agregado_sector.xlsx  (var. mensual por sector productivo)
#
# Outputs:
#   - inei_pib/data/processed/pib_mensual.csv
#   - inei_pib/data/processed/va_sector.csv
#   - inei_pib/figures/pib_calor_sector_{fecha_fin}.png
#
# Nota: las variaciones ya vienen pre-calculadas por el INEI; no se usa
#       calcular_inflacion().
#
# Fuente: INEI
#   https://webapp.inei.gob.pe:8443/sirtod-series/
#===============================================================================

source("modules/social_heatmap.R")

library(tidyverse)
library(readxl)
library(glue)
library(fs)


# Parámetros --------------------------------------------------------------

fecha_fin    <- "2026-03-01"

fechas_pib    <- seq(as.Date("2007-01-01"), as.Date(fecha_fin), by = "month")
fechas_sector <- seq(as.Date("2008-01-01"), as.Date(fecha_fin), by = "month")

titulo_fecha <- toupper(format(as.Date(fecha_fin), "%B %Y"))

meses_calor <- 12


# Rutas -------------------------------------------------------------------

dir_subproject <- "inei_pib"
dir_raw        <- path(dir_subproject, "data/raw")
dir_processed  <- path(dir_subproject, "data/processed")
dir_figures    <- path(dir_subproject, "figures")

dir_create(dir_raw)
dir_create(dir_processed)
dir_create(dir_figures)

path_pib_mensual <- path(dir_processed, "pib_mensual.csv")
path_va_sector   <- path(dir_processed, "valor_actual_sector.csv")

path_fig_sector  <- path(dir_figures, glue("valor_actual_sector_{fecha_fin}.png"))


# PBI mensual global ------------------------------------------------------

# Col 1 = ámbito, col 2 = indicador, col 3 = unidad, col 4+ = valores mensuales.
# El INEI publica directamente el índice y las variaciones; se pivota a ancho
# para tener una columna por tipo (INDICE, MENSUAL, ACUMULADO, ANUAL).

raw_pib <- read_excel(
  path(dir_raw, "pib_mensual.xlsx"),
  sheet = 1, skip = 3, col_names = FALSE
)
names(raw_pib) <- c("AMBITO", "INDICADOR", "UNIDAD", as.character(fechas_pib))

pib_mensual <- raw_pib %>%
  select(-AMBITO, -UNIDAD) %>%
  filter(!is.na(INDICADOR)) %>%
  pivot_longer(cols = -INDICADOR, names_to = "FECHA", values_to = "VALOR") %>%
  mutate(
    FECHA = as.Date(FECHA),
    VALOR = as.numeric(VALOR),
    TIPO  = case_when(
      grepl("Índice Base",  INDICADOR) ~ "INDICE",
      grepl("mensual",      INDICADOR) ~ "MENSUAL",
      grepl("acumulada",    INDICADOR) ~ "ACUMULADO",
      grepl("anualizada",   INDICADOR) ~ "ANUAL",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(TIPO)) %>%
  select(-INDICADOR) %>%
  pivot_wider(names_from = TIPO, values_from = VALOR)

write_csv(pib_mensual, path_pib_mensual)


# Valor agregado por sector -----------------------------------------------

raw_sector <- read_excel(
  path(dir_raw, "valor_agregado_sector.xlsx"),
  sheet = 1, skip = 3, col_names = FALSE
)
names(raw_sector) <- c("AMBITO", "SECTOR", "UNIDAD", as.character(fechas_sector))

va_sector <- raw_sector %>%
  select(-AMBITO, -UNIDAD) %>%
  filter(!is.na(SECTOR)) %>%
  pivot_longer(cols = -SECTOR, names_to = "FECHA", values_to = "VAR_MENSUAL") %>%
  mutate(
    FECHA       = as.Date(FECHA),
    VAR_MENSUAL = as.numeric(VAR_MENSUAL),
    SECTOR      = gsub("^Valor Agregado (del Sector |de )", "", SECTOR),
    SECTOR      = gsub("\\s*\\(Variación porcentual mensual\\)", "", SECTOR),
    SECTOR      = trimws(SECTOR)
  )

write_csv(va_sector, path_va_sector)


# verificación: último período PBI ----------------------------------------

pib_mensual %>%
  filter(FECHA == as.Date(fecha_fin)) %>%
  select(FECHA, INDICE, MENSUAL, ACUMULADO, ANUAL)


# fig: mapa de calor valor agregado por sector ----------------------------

fecha_inicio_graficos <- seq(
  as.Date(fecha_fin), length.out = meses_calor, by = "-1 month"
)[meses_calor]

sector_data <- va_sector %>%
  filter(FECHA >= fecha_inicio_graficos, !is.na(VAR_MENSUAL))

orden_meses <- format(sort(unique(sector_data$FECHA)), "%b %Y")

fig_sector <- sector_data %>%
  mutate(FECHA = format(FECHA, "%b %Y")) %>%
  social_mapa_calor(
    x_col              = "FECHA",
    orden_x            = orden_meses,
    y_col              = "SECTOR",
    value_col          = "VAR_MENSUAL",
    fondo              = "beige",
    color_alto         = "verde_claro",
    label_format       = "%.1f",
    mostrar_etiquetas  = TRUE,
    mostrar_cuadricula = TRUE,
    max_x              = 10,
    max_y              = 44,
    title              = paste(
      "PERÚ | VALOR AGREGADO POR SECTOR: VARIACIÓN MENSUAL -",
      glue("{titulo_fecha} (%)")
    ),
    subtitle = paste(
      "El sector construcción destaca por su sostenido dinamismo y se consolida",
      "como uno de los principales impulsores de la actividad económica."
    ),
    caption = "Fuente: INEI | X: @EdisonMondragon"
  )

ggsave(path_fig_sector, plot = fig_sector, width = 8, height = 10, dpi = 135)



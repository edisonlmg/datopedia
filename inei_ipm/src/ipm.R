#===============================================================================
# ipm.R  [inei_ipm]
#
# Objetivo: procesar y visualizar el Índice de Precios al por Mayor (IPM)
#           a nivel nacional del Perú.
#
# Inputs (descarga manual desde la fuente):
#   - inei_ipm/data/raw/ipm.xlsx            (IPM: nacional, importado, general)
#   - inei_ipm/data/raw/ipm_nacional.xlsx   (IPM por subsector de producción nacional)
#   - inei_ipm/data/raw/ipm_importado.xlsx  (IPM por subsector importado)
#
# Outputs:
#   - inei_ipm/data/processed/ipm_general.csv
#   - inei_ipm/data/processed/ipm_nacional.csv
#   - inei_ipm/data/processed/ipm_importado.csv
#   - inei_ipm/figures/ipm_general.png
#   - inei_ipm/figures/ipm_nacional.png
#   - inei_ipm/figures/ipm_importado.png
#
# Fuente: INEI - Índice de Precios al por Mayor
#   https://webapp.inei.gob.pe:8443/sirtod-series/
#===============================================================================

source("inei_ipc/src/calcular_inflacion.R")
source("modules/social_line_chart.R")
source("modules/social_heatmap.R")

library(tidyverse)
library(readxl)
library(glue)
library(fs)


# Parámetros --------------------------------------------------------------

fecha_inicio <- "2014-01-01"
fecha_fin    <- "2026-04-01"

fechas       <- seq(as.Date(fecha_inicio), as.Date(fecha_fin), by = "month")
titulo_fecha <- toupper(format(as.Date(fecha_fin), "%B %Y"))

meses_calor <- 12   # ventana compartida para ambos gráficos


# Rutas -------------------------------------------------------------------

dir_subproject <- "inei_ipm"
dir_raw        <- path(dir_subproject, "data/raw")
dir_processed  <- path(dir_subproject, "data/processed")
dir_figures    <- path(dir_subproject, "figures")

dir_create(dir_raw)
dir_create(dir_processed)
dir_create(dir_figures)

path_raw_ipm       <- path(dir_raw, "ipm.xlsx")
path_raw_nacional  <- path(dir_raw, "ipm_nacional.xlsx")
path_raw_importado <- path(dir_raw, "ipm_importado.xlsx")

path_ipm_general   <- path(dir_processed, "ipm_general.csv")
path_ipm_nacional  <- path(dir_processed, "ipm_nacional.csv")
path_ipm_importado <- path(dir_processed, "ipm_importado.csv")

path_fig_general   <- path(dir_figures, glue("ipm_general_{fecha_fin}.png"))
path_fig_nacional  <- path(dir_figures, glue("ipm_nacional_{fecha_fin}.png"))
path_fig_importado <- path(dir_figures, glue("ipm_importado_{fecha_fin}.png"))


# Función de lectura ------------------------------------------------------

# Col 1 = ámbito, col 2 = indicador, col 3 = unidad, col 4+ = valores mensuales
# Reutiliza calcular_inflacion() con la columna renombrada como IPC (mismo índice)

.leer_ipm <- function(path_excel, fechas, grupos) {
  raw <- read_excel(path_excel, sheet = 1, skip = 3, col_names = FALSE)
  names(raw) <- c("AMBITO", "INDICADOR", ".drop", as.character(fechas))

  raw %>%
    select(-AMBITO, -.drop) %>%
    filter(!is.na(INDICADOR)) %>%
    pivot_longer(
      cols      = -INDICADOR,
      names_to  = "FECHA",
      values_to = "IPC"
    ) %>%
    mutate(
      FECHA     = as.Date(FECHA),
      IPC       = as.numeric(IPC),
      INDICADOR = trimws(INDICADOR)
    ) %>%
    calcular_inflacion(grupos = grupos)
}


# IPM general (total, nacional, importado) --------------------------------

ipm_general <- .leer_ipm(
  path_raw_ipm,
  fechas = fechas,
  grupos = "INDICADOR"
) %>%
  mutate(
    INDICADOR = case_when(
      grepl("Nacional",  INDICADOR)                    ~ "Nacional",
      grepl("Importado", INDICADOR)                    ~ "Importado",
      grepl("General",   INDICADOR, ignore.case = TRUE) ~ "General",
      TRUE ~ INDICADOR
    )
  )

write_csv(ipm_general, path_ipm_general)


# IPM por subsector nacional ----------------------------------------------

ipm_nacional <- .leer_ipm(
  path_raw_nacional,
  fechas = fechas,
  grupos = "INDICADOR"
) %>%
  mutate(
    INDICADOR = gsub("^Subsector\\s+", "", INDICADOR),
    INDICADOR = gsub(",\\s*Nacional.*$", "", INDICADOR),
    INDICADOR = trimws(INDICADOR)
  )

write_csv(ipm_nacional, path_ipm_nacional)


# IPM por subsector importado ---------------------------------------------

ipm_importado <- .leer_ipm(
  path_raw_importado,
  fechas = fechas,
  grupos = "INDICADOR"
) %>%
  mutate(
    INDICADOR = gsub("^Subsector\\s+", "", INDICADOR),
    INDICADOR = gsub(",\\s*Importado.*$", "", INDICADOR),
    INDICADOR = trimws(INDICADOR)
  )

write_csv(ipm_importado, path_ipm_importado)


# ventana temporal compartida ---------------------------------------------

fecha_inicio_graficos <- seq(as.Date(fecha_fin), length.out = meses_calor, by = "-1 month")[meses_calor]


# verificación ------------------------------------------------------------

ipm_general %>%
  filter(FECHA == as.Date(fecha_fin)) %>%
  select(INDICADOR, IPC, MENSUAL, ANUAL)

ipm_nacional %>%
  filter(FECHA == as.Date(fecha_fin)) %>%
  arrange(desc(ANUAL)) %>%
  select(INDICADOR, ANUAL)

ipm_importado %>%
  filter(FECHA == as.Date(fecha_fin)) %>%
  arrange(desc(ANUAL)) %>%
  select(INDICADOR, ANUAL)


# fig: línea de tiempo (Nacional vs Importado vs General) -----------------

df_general <- ipm_general %>%
  filter(FECHA >= fecha_inicio_graficos, !is.na(ANUAL))

# y_limits explícitos: social_line_chart clampea el mínimo a 0 por defecto
y_range     <- range(df_general$ANUAL, na.rm = TRUE)
y_pad       <- diff(y_range) * 0.15
y_lim_linea <- c(floor(y_range[1] - y_pad), ceiling(y_range[2] + y_pad))

fig_general <- social_line_chart(
  x           = df_general$FECHA,
  y           = df_general$ANUAL,
  group       = df_general$INDICADOR,
  series_name = "Componente",
  y_limits    = y_lim_linea,
  show_labels = FALSE,
  fondo       = "beige",
  colores     = c("azul", "morado", "marron"), 
  title       = paste(
    "PERÚ | ÍNDICE DE PRECIOS AL POR MAYOR (IPM): VARIACIÓN ANUAL -", 
    glue("{titulo_fecha} (%)")
    ),
  subtitle    = paste(
    "Los precios al por mayor de los productos importados lideraron",
    "las presiones inflacionarias al productor durante el ciclo reciente"
  ),
  caption     = paste(
    "Nota: los precios de origen nacional tienen un peso de 75.92 % sobre el",
    "índice general, mientras que los productos importados representan el",
    "24.08 %.\n\n",
    "Fuente: INEI | X: @EdisonMondragon"
    )
)

ggsave(path_fig_general, plot = fig_general, width = 8, height = 10, dpi = 135)


# fig: mapa de calor subsectores nacional 12 últimos meses ----------------

nacional_data  <- ipm_nacional %>%
  filter(FECHA >= fecha_inicio_graficos, !is.na(ANUAL))

orden_meses <- format(sort(unique(nacional_data$FECHA)), "%b %Y")

fig_nacional <- nacional_data %>%
  mutate(FECHA = format(FECHA, "%b %Y")) %>%
  social_mapa_calor(
    x_col              = "FECHA",
    orden_x            = orden_meses,
    y_col              = "INDICADOR",
    value_col          = "ANUAL",
    fondo              = "beige",
    color_alto         = "marron",
    label_format       = "%.1f",
    mostrar_etiquetas  = TRUE,
    mostrar_cuadricula = TRUE,
    max_x              = 10,
    max_y              = 44,
    title              = paste(
      "PERÚ | ÍNDICE DE PRECIOS AL POR MAYOR (IPM): VARIACIÓN ANUAL POR",
      glue("SUBSECTOR NACIONAL - {titulo_fecha} (%)")
    ),
    subtitle = paste(
      "El gran aumento en productos de la refinación del petróleo podría",
      "trasladarse parcialmente al consumidor, principalmente a transporte y",
      "alimentos."
    ),
    caption = paste(
      "Nota: el subsector de productos de la refinación del petróleo de origen",
      "nacional es el cuarto de mayor peso en el índice general, con una",
      "participación de 5.97 %.\n\n",
      "Fuente: INEI | X: @EdisonMondragon"
    )
  )

ggsave(path_fig_nacional, plot = fig_nacional, width = 8, height = 10, dpi = 135)



# fig: mapa de calor subsectores importado 12 últimos meses ---------------

importado_data <- ipm_importado %>%
  filter(FECHA >= fecha_inicio_graficos, !is.na(ANUAL))

fig_importado <- importado_data %>%
  mutate(FECHA = format(FECHA, "%b %Y")) %>%
  social_mapa_calor(
    x_col              = "FECHA",
    orden_x            = orden_meses,
    y_col              = "INDICADOR",
    value_col          = "ANUAL",
    fondo              = "beige",
    color_alto         = "morado",
    label_format       = "%.1f",
    mostrar_etiquetas  = TRUE,
    mostrar_cuadricula = TRUE,
    max_x              = 10,
    max_y              = 44,
    title              = paste(
      "PERÚ | ÍNDICE DE PRECIOS AL POR MAYOR (IPM): VARIACIÓN ANUAL POR",
      glue("SUBSECTOR IMPORTADO - {titulo_fecha} (%)")
    ),
    subtitle = paste(
      "Los subsectores de mayor presión importada reflejan el impacto del",
      "tipo de cambio y precios internacionales de commodities."
    ),
    caption = "Fuente: INEI | X: @EdisonMondragon"
  )

ggsave(path_fig_importado, plot = fig_importado, width = 8, height = 10, dpi = 135)



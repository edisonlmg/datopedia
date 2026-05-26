#===============================================================================
# resultado_neto.R  [sbs_sistema_financiero]
#
# Objetivo: procesar y visualizar el resultado neto de las empresas del sistema
#           financiero peruano: banca múltiple, empresas financieras, cajas
#           municipales y cajas rurales.
#
# Prerequisito: ejecutar importar_datasets.R para descargar los archivos XLS.
#
# Outputs:
#   - sbs_sistema_financiero/data/processed/sbs_sistema_financiero.csv
#   - sbs_sistema_financiero/figures/bm_fig1.png
#   - sbs_sistema_financiero/figures/ef_fig1.png
#   - sbs_sistema_financiero/figures/cm_fig1.png
#   - sbs_sistema_financiero/figures/cr_fig1.png
#
# Fuente: SBS - Estadísticas del Sistema Financiero
#   https://intranet2.sbs.gob.pe/estadistica/financiera/
#===============================================================================


source("modules/bar_chart.R")


library(tidyverse) # para manejo y visualización de datos
library(lubridate) # para manejo de fechas
library(readxl)    # para leer archivos de Excel
library(tools)     # para file_path_sans_ext()
library(glue)      # para texto dinámico en títulos
library(fs)        # para manejo de directorios


dir_subproject <- "sbs_sistema_financiero"
dir_raw        <- path(dir_subproject, "data/raw")
dir_processed  <- path(dir_subproject, "data/processed")
dir_figures    <- path(dir_subproject, "figures")

path_datasets <- path(dir_processed, "sbs_sistema_financiero.csv")


# La SBS publica los EEFF con 2 meses de rezago respecto al mes en curso

months_str <- tibble(
  month = 1:12,
  name  = c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
            "Julio","Agosto","Setiembre","Octubre","Noviembre","Diciembre"),
  abbr  = c("en","fe","ma","ab","my","jn","jl","ag","se","oc","no","di")
)

current_date  <- today()
current_year  <- year(current_date)
current_month <- month(current_date) - 2

month_str       <- months_str$name[current_month]
month_str_short <- months_str$abbr[current_month]


#===============================================================================
# abrir datasets
#===============================================================================


# datasets eeff

path_files <- dir_ls(dir_raw)

datasets_raw <- map(path_files, read_excel, sheet = 2, col_names = FALSE)

names(datasets_raw) <- file_path_sans_ext(basename(path_files))


#===============================================================================
# procesar datasets eeff
#===============================================================================


# --- parametros ---

# row_names:  fila con los nombres de las entidades
# row_result: fila con el resultado neto (en miles de S/)
# row_tc:     fila con el tipo de cambio cierre del periodo

params_fs <- list(
  list(year = current_year, type = "BANCA MULTIPLE",    row_names = 5, row_result = 78, row_tc = 80),
  list(year = current_year, type = "EMPRESA FINANCIERA", row_names = 5, row_result = 77, row_tc = 79),
  list(year = current_year, type = "CAJAS MUNICIPALES",  row_names = 5, row_result = 78, row_tc = 79),
  list(year = current_year, type = "CAJAS RURALES",      row_names = 5, row_result = 76, row_tc = 77)
)

names(params_fs) <- file_path_sans_ext(basename(path_files))


# --- funcion de procesamiento ---


processing_fs <- function(df, name_file, params_file) {
  
  # Parametros
  params <- params_file[[name_file]]
  
  # Nombres de entidades
  names_col <- df %>%
    slice(params$row_names) %>%
    select(where(~ !all(is.na(.)))) %>%
    unlist(use.names = FALSE) %>%
    str_replace_all(fixed("*"), "")
  
  # Resultado neto
  net_results <- df %>%
    slice(params$row_result) %>%
    mutate(across(everything(), as.numeric)) %>%
    select(where(~ !all(is.na(.)))) %>%
    flatten_dbl()
  
  # Tipo de cambio
  tc <- df %>%
    slice(params$row_tc) %>%
    select(where(~ !all(is.na(.)))) %>%
    flatten_chr() %>%
    str_replace_all(",", ".") %>%
    parse_number()
  
  money <- c("MN", "ME", "TOTAL") # moneda nacional, moneda extranjera, total

  # cada entidad aparece 3 veces en el archivo (una fila por moneda)
  tibble(
    PERIODO        = params$year,
    MES            = month_str,
    TIPO           = params$type,
    ENTIDAD        = rep(rep(names_col, each = 3), length.out = length(net_results)),
    MONEDA         = rep(money, length.out = length(net_results)),
    TC             = rep(tc, length.out = length(net_results)),
    RESULTADO_NETO = net_results
  )
}


# --- aplicar funcion a todos los datasets y unir ---

datasets_fs <- datasets_raw %>%
  imap(~ processing_fs(.x, .y, params_fs)) %>%
  list_rbind()


#===============================================================================
# visualizaciones resultado neto total
#===============================================================================


# --- banca multiple ---

bm_fig1_data <- datasets_fs %>%
  filter(
    PERIODO == current_year,
    !str_detect(ENTIDAD, "(?i)total|(?i)sucursal"),
    TIPO == "BANCA MULTIPLE",
    MONEDA == "TOTAL"
  ) %>%
  mutate(
    RESULTADO_NETO = round(RESULTADO_NETO/1e3, 0)
  ) %>%
  arrange(desc(RESULTADO_NETO))


bm_fig1 <- bar_chart(
  x              = bm_fig1_data$ENTIDAD,
  y              = bm_fig1_data$RESULTADO_NETO,
  orientation    = "horizontal",
  title          = glue("RESULTADO NETO (TOTAL) DE BANCA MÚLTIPLE A {str_to_upper(month_str)} {current_year}"),
  subtitle       = "(Millones de S/)",
  caption        = "Fuente: SBS | Elaborado por: @EdisonMondragon",
  x_label        = NULL,
  y_label        = NULL,
  label_decimals =  0,
  label_big_mark = ",",
  theme          = "light"
)

bm_fig1


# --- empresas financieras ---

ef_fig1_data <- datasets_fs %>%
  filter(
    PERIODO == current_year,
    !str_detect(ENTIDAD, "(?i)total"),
    TIPO == "EMPRESA FINANCIERA",
    MONEDA == "TOTAL"
  ) %>%
  arrange(desc(RESULTADO_NETO))


ef_fig1 <- bar_chart(
  x              = ef_fig1_data$ENTIDAD,
  y              = ef_fig1_data$RESULTADO_NETO,
  orientation    = "horizontal",
  title          = glue("RESULTADO NETO (TOTAL) DE EMPRESAS FINANCIERAS A {str_to_upper(month_str)} {current_year}"),
  subtitle       = "(Miles de S/)",
  caption        = "Fuente: SBS | Elaborado por: @EdisonMondragon",
  label_decimals =  0,
  label_big_mark = ",",
  x_label        = NULL,
  y_label        = NULL,
  theme          = "light"
)

ef_fig1


# --- cajas municipales ---

cm_fig1_data <- datasets_fs %>%
  filter(
    PERIODO == current_year,
    !str_detect(ENTIDAD, "(?i)total"),
    TIPO == "CAJAS MUNICIPALES",
    MONEDA == "TOTAL"
  ) %>%
  mutate(
    ENTIDAD = case_when(
      ENTIDAD == "Caja Municipal de Crédito Popular Lima" ~ "Caja Metropolitana",
      TRUE ~ ENTIDAD
    )
  ) %>%
  arrange(desc(RESULTADO_NETO))


cm_fig1 <- bar_chart(
  x              = cm_fig1_data$ENTIDAD,
  y              = cm_fig1_data$RESULTADO_NETO,
  orientation    = "horizontal",
  title          = glue("RESULTADO NETO (TOTAL) DE CAJAS MUNICIPALES A {str_to_upper(month_str)} {current_year}"),
  subtitle       = "(Miles de S/)",
  caption        = "Fuente: SBS | Elaborado por: @EdisonMondragon",
  label_decimals =  0,
  label_big_mark = ",",
  x_label        = NULL,
  y_label        = NULL,
  theme          = "light"
)

cm_fig1


# --- cajas rurales ---

cr_fig1_data <- datasets_fs %>%
  filter(
    PERIODO == current_year,
    !str_detect(ENTIDAD, "(?i)total"),
    TIPO == "CAJAS RURALES",
    MONEDA == "TOTAL"
  ) %>%
  arrange(desc(RESULTADO_NETO))


cr_fig1 <- bar_chart(
  x              = cr_fig1_data$ENTIDAD,
  y              = cr_fig1_data$RESULTADO_NETO,
  orientation    = "horizontal",
  title          = glue("RESULTADO NETO (TOTAL) DE CAJAS RURALES A {str_to_upper(month_str)} {current_year}"),
  subtitle       = "(Miles de S/)",
  caption        = "Fuente: SBS | Elaborado por: @EdisonMondragon",
  label_decimals =  0,
  label_big_mark = ",",
  x_label        = NULL,
  y_label        = NULL,
  theme          = "light"
)

cr_fig1


#===============================================================================
# guardar resultados
#===============================================================================


write_csv(datasets_fs, path_datasets)

ggsave(path(dir_figures, "bm_fig1.png"), plot = bm_fig1, dpi = 120)

ggsave(path(dir_figures, "ef_fig1.png"), plot = ef_fig1, dpi = 120)

ggsave(path(dir_figures, "cm_fig1.png"), plot = cm_fig1, dpi = 120)

ggsave(path(dir_figures, "cr_fig1.png"), plot = cr_fig1, dpi = 120)




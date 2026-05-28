#===============================================================================
# resultado_neto.R  [sbs_sistema_financiero]
#
# Objetivo: procesar y visualizar el resultado neto de las empresas del sistema
#           financiero peruano: banca múltiple, empresas financieras, cajas
#           municipales y cajas rurales.
#
# Input (descarga manual desde la fuente):
#   - sbs_sistema_financiero/data/raw/B-2201*.XLS  (banca múltiple)
#   - sbs_sistema_financiero/data/raw/B-2401*.XLS  (empresas financieras)
#   - sbs_sistema_financiero/data/raw/B-3101*.XLS  (cajas municipales)
#   - sbs_sistema_financiero/data/raw/B-3301*.XLS  (cajas rurales)
#
# Outputs:
#   - sbs_sistema_financiero/data/processed/resultado_neto.csv
#   - fig_banca, fig_financieras, fig_cajas_municipales, fig_cajas_rurales
#     (objetos ggplot en memoria — exportar manualmente a 1048x762 px)
#
# Fuente: SBS - Estadísticas del Sistema Financiero
#   https://intranet2.sbs.gob.pe/estadistica/financiera/
#===============================================================================

source("modules/bar_chart.R")

library(tidyverse) # para manejo y visualización de datos
library(lubridate) # para manejo de fechas
library(readxl)    # para leer archivos de Excel
library(tools)     # para usar file_path_sans_ext
library(glue)      # para texto dinámico en títulos
library(fs)        # para manejo de directorios


# Rutas -------------------------------------------------------------------

dir_subproject <- "sbs_sistema_financiero"
dir_raw        <- path(dir_subproject, "data/raw")
dir_processed  <- path(dir_subproject, "data/processed")
dir_figures    <- path(dir_subproject, "figures")

dir_create(dir_raw)
dir_create(dir_processed)
dir_create(dir_figures)

path_files <- dir_ls(dir_raw)
output_path <- path(dir_processed, "resultado_neto.csv")


# Corregir extensión de archivos ------------------------------------------

# Los archivos B-22xx y B-31xx se descargan como .XLS (formato Excel 97-2003)
# pero read_excel los rechaza porque su extensión correcta es .xlsx

xls_a_xlsx <- function(rutas) {
  archivos_xls <- list.files(
    rutas, 
    pattern = "(B-2201|B-3101|B-3301|B-2401).*\\.XLS$",
    full.names = TRUE, 
    ignore.case = TRUE
    )
  
  walk(archivos_xls, ~ {
    dst <- sub("\\.XLS$", ".xlsx", .x, ignore.case = TRUE)
    if (file.copy(.x, dst)) {
      file.remove(.x)
      message("Renombrado: ", basename(.x), " -> ", basename(dst))
    }
  })
}

xls_a_xlsx(dir_raw)


# abrir datasets ----------------------------------------------------------

df_raw <- map(path_files, read_excel, sheet = 2, col_names = FALSE)

names(df_raw) <- file_path_sans_ext(basename(path_files))


# establecer parametros ---------------------------------------------------

# establecer manualmente periodo y mes
periodo = 2026
mes     = "MARZO"

# establecer manualmente en cada dataset el número de fila donde se encuentran
# los siguientes datos:
# - fila_nombres    : fila con los nombres de las entidades
# - fila_resultados : fila con el resultado neto (en miles de S/)
# - fila_tc         : fila con el tipo de cambio cierre del periodo

parametros <- tribble(
  ~tipo,                 ~fila_nombres, ~fila_resultados, ~fila_tc,
  "BANCA MULTIPLE",                  5,               78,       80,
  "EMPRESA FINANCIERA",              5,               77,       79,
  "CAJAS MUNICIPALES",               5,               78,       79,
  "CAJAS RURALES",                   5,               76,       77
)

parametros$dataset <- file_path_sans_ext(basename(path_files))


# funcion de procesamiento ------------------------------------------------

procesar_datasets <- function(df, dataset_nombre, parametros) {
  
  # parametros
  params <- parametros %>%
    filter(dataset == dataset_nombre)
  
  # nombres de entidades
  entidades <- df %>%
    slice(params$fila_nombres) %>%
    select(where(~ !all(is.na(.)))) %>%
    unlist(use.names = FALSE) %>%
    str_replace_all(fixed("*"), "")
  
  # resultado neto
  resultado_neto <- df %>%
    slice(params$fila_resultados) %>%
    mutate(across(everything(), as.numeric)) %>%
    select(where(~ !all(is.na(.)))) %>%
    flatten_dbl()
  
  # tipo de cambio
  tc <- df %>%
    slice(params$fila_tc) %>%
    select(where(~ !all(is.na(.)))) %>%
    flatten_chr() %>%
    str_replace_all(",", ".") %>%
    parse_number()
  
  # moneda nacional, moneda extranjera, total
  moneda <- c("MN", "ME", "TOTAL")
  
  # largo de tabla
  largo = length(resultado_neto)
  
  # cada entidad aparece 3 veces en el archivo (una fila por moneda)
  tibble(
    TIPO           = params$tipo,
    ENTIDAD        = rep(rep(entidades, each = 3), length.out = largo),
    MONEDA         = rep(moneda, length.out = largo),
    TC             = rep(tc, length.out = largo),
    RESULTADO_NETO = resultado_neto
  )
}


# procesar datasets y guardar ---------------------------------------------

df_procesado <- df_raw %>%
  imap(~ procesar_datasets(.x, .y, parametros)) %>%
  list_rbind()

write_csv(df_procesado, output_path)


# fig: banca multiple -----------------------------------------------------

df_banca <- df_procesado %>%
  filter(
    !str_detect(ENTIDAD, "(?i)total|(?i)sucursal"),
    TIPO == "BANCA MULTIPLE",
    MONEDA == "TOTAL"
  ) %>%
  mutate(
    RESULTADO_NETO = round(RESULTADO_NETO/1e3, 0)
  ) %>%
  arrange(desc(RESULTADO_NETO))

fig_banca <- bar_chart(
  x              = df_banca$ENTIDAD,
  y              = df_banca$RESULTADO_NETO,
  orientation    = "horizontal",
  title          = glue("RESULTADO NETO DE BANCA MÚLTIPLE A {str_to_upper(mes)} {periodo}"),
  subtitle       = "(Millones de S/)",
  caption        = "Fuente: SBS | X: @EdisonMondragon",
  x_label        = NULL,
  y_label        = NULL,
  label_decimals =  0,
  label_big_mark = ",",
  theme          = "light"
)

fig_banca


# fig: empresas financieras -----------------------------------------------

df_financieras <- df_procesado %>%
  filter(
    !str_detect(ENTIDAD, "(?i)total"),
    TIPO == "EMPRESA FINANCIERA",
    MONEDA == "TOTAL"
  ) %>%
  arrange(desc(RESULTADO_NETO))

fig_financieras <- bar_chart(
  x              = df_financieras$ENTIDAD,
  y              = df_financieras$RESULTADO_NETO,
  orientation    = "horizontal",
  title          = glue("RESULTADO NETO DE EMPRESAS FINANCIERAS A {str_to_upper(mes)} {periodo}"),
  subtitle       = "(Miles de S/)",
  caption        = "Fuente: SBS | X: @EdisonMondragon",
  label_decimals =  0,
  label_big_mark = ",",
  x_label        = NULL,
  y_label        = NULL,
  theme          = "light"
)

fig_financieras


# fig: cajas municipales --------------------------------------------------

df_cajas_municipales <- df_procesado %>%
  filter(
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


fig_cajas_municipales <- bar_chart(
  x              = df_cajas_municipales$ENTIDAD,
  y              = df_cajas_municipales$RESULTADO_NETO,
  orientation    = "horizontal",
  title          = glue("RESULTADO NETO DE CAJAS MUNICIPALES A {str_to_upper(mes)} {periodo}"),
  subtitle       = "(Miles de S/)",
  caption        = "Fuente: SBS | X: @EdisonMondragon",
  label_decimals =  0,
  label_big_mark = ",",
  x_label        = NULL,
  y_label        = NULL,
  theme          = "light"
)

fig_cajas_municipales


# fig: cajas rurales ------------------------------------------------------

df_cajas_rurales <- df_procesado %>%
  filter(
    !str_detect(ENTIDAD, "(?i)total"),
    TIPO == "CAJAS RURALES",
    MONEDA == "TOTAL"
  ) %>%
  arrange(desc(RESULTADO_NETO))

fig_cajas_rurales <- bar_chart(
  x              = df_cajas_rurales$ENTIDAD,
  y              = df_cajas_rurales$RESULTADO_NETO,
  orientation    = "horizontal",
  title          = glue("RESULTADO NETO DE CAJAS RURALES A {str_to_upper(mes)} {periodo}"),
  subtitle       = "(Miles de S/)",
  caption        = "Fuente: SBS | X: @EdisonMondragon",
  label_decimals =  0,
  label_big_mark = ",",
  x_label        = NULL,
  y_label        = NULL,
  theme          = "light"
)

fig_cajas_rurales






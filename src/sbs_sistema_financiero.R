#===============================================================================
# sbs-sistema-financiero
#
# Objetivo: descargar, procesar y visualizar el resultado neto de las empresas
#           del sistema financiero peruano: banca múltiple, empresas financieras,
#           cajas municipales y cajas rurales.
#
# Outputs:
#   - data/processed/sbs_sistema_financiero.csv  (datos procesados)
#   - figures/sbs_sistema_financiero/            (gráficos por tipo de empresa)
#
# Fuente: SBS - Estadísticas del Sistema Financiero
#   https://intranet2.sbs.gob.pe/estadistica/financiera/
#===============================================================================


source("src/graphics_functions.R")


if (!require("pacman")) {install.packages("pacman", dependencies = TRUE)}
pacman::p_load(
  tidyverse,  # para manejo y visualización de datos
  lubridate,  # para manejo de fechas
  readxl,     # para leer archivos de Excel
  renv,       # para entorno virtual
  tools,      # para manejo de archivos
  glue,       # para texto dinamico
  fs          # para manejor de directorios
)


dir_raw        <- path("data/raw/sbs_sistema_financiero")
dir_processed  <- path("data/processed")
dir_figures    <- path("figures/sbs_sistema_financiero")


path_datasets <- path(dir_processed, "sbs_sistema_financiero.csv")


#===============================================================================
# extraccion de datos
#===============================================================================


# --- crear URLs ---


# tabla de meses y abreviaturas

months_str <- tibble(
  month = 1:12,
  name  = c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
            "Julio","Agosto","Setiembre","Octubre","Noviembre","Diciembre"),
  abbr  = c("en","fe","ma","ab","my","jn","jl","ag","se","oc","no","di")
)


# La SBS publica los EEFF con 2 meses de rezago respecto al mes en curso

current_date  <- today()
current_year  <- year(current_date)
current_month <- month(current_date) - 2

month_str       <- months_str$name[current_month]
month_str_short <- months_str$abbr[current_month]


# URLs de estados financieros (EEFF):
#   B-2201: banca múltiple       B-3101: empresas financieras
#   C-1101: cajas municipales    C-2101: cajas rurales

urls <- c(
  glue("https://intranet2.sbs.gob.pe/estadistica/financiera/{current_year}/{month_str}/B-2201-{month_str_short}{current_year}.XLS"),
  glue("https://intranet2.sbs.gob.pe/estadistica/financiera/{current_year}/{month_str}/B-3101-{month_str_short}{current_year}.XLS"),
  glue("https://intranet2.sbs.gob.pe/estadistica/financiera/{current_year}/{month_str}/C-1101-{month_str_short}{current_year}.XLS"),
  glue("https://intranet2.sbs.gob.pe/estadistica/financiera/{current_year}/{month_str}/C-2101-{month_str_short}{current_year}.XLS")
)


# --- descargar datasets ---


# limpiar descargas previas para evitar mezclar archivos de distintos periodos
dir_ls(dir_raw, type = "file") %>% file_delete()


# datasets de eeff

iwalk(urls, ~{
  
  file_name   <- basename(.x)
  
  output_path <- path(dir_raw, file_name)
  
  message("Intentando descargar: ", file_name)
  
  tryCatch({
    download.file(
      url      = .x, 
      destfile = output_path, 
      mode     = "wb",
      quiet    = TRUE
    )
    message("  [OK] Descarga exitosa.\n")
    
  }, error = function(condicion) {
    message("  [ERROR] No se pudo descargar el archivo. \n")
  })
})


# --- Corregir extension de archivos ---


# Los archivos B-22xx y B-31xx se descargan como .XLS (formato Excel 97-2003)
# pero read_excel los rechaza por extensión; renombrarlos a .xlsx lo resuelve.
rename_xls_to_xlsx <- function(directory) {
  xls_files <- list.files(directory, pattern = "(B-2201|B-3101|B-3301|B-2401).*\\.XLS$",
                          full.names = TRUE, ignore.case = TRUE)

  for (src in xls_files) {
    dst <- sub("\\.XLS$", ".xlsx", src, ignore.case = TRUE)
    if (file.copy(src, dst)) {
      file.remove(src)
      message("Renombrado: ", basename(src), " -> ", basename(dst))
    }
  }
}

rename_xls_to_xlsx(dir_raw)


# --- abrir datasets ---


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




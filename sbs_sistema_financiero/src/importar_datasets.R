#===============================================================================
# importar_datasets.R  [sbs_sistema_financiero]
#
# Objetivo: descargar los estados financieros (EEFF) de las empresas del
#           sistema financiero peruano desde el portal de la SBS.
#
# Outputs:
#   - sbs_sistema_financiero/data/raw/B-2201-<mes><año>.xlsx  (banca múltiple)
#   - sbs_sistema_financiero/data/raw/B-3101-<mes><año>.xlsx  (financieras)
#   - sbs_sistema_financiero/data/raw/C-1101-<mes><año>.xlsx  (cajas municipales)
#   - sbs_sistema_financiero/data/raw/C-2101-<mes><año>.xlsx  (cajas rurales)
#
# Fuente: SBS - Estadísticas del Sistema Financiero
#   https://intranet2.sbs.gob.pe/estadistica/financiera/
#===============================================================================


library(tidyverse)
library(lubridate)
library(glue)
library(fs)

dir_subproject <- "sbs_sistema_financiero"
dir_raw        <- path(dir_subproject, "data/raw")
dir_temp       <- path(dir_subproject, "data/temp_staging") # Nueva área de preparación

dir_create(dir_raw)
dir_create(dir_temp)

# ===============================================================================
# EXTRACCIÓN DE DATOS
# ===============================================================================

# Tabla de meses y abreviaturas
months_str <- tibble(
  month = 1:12,
  name  = c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
            "Julio","Agosto","Setiembre","Octubre","Noviembre","Diciembre"),
  abbr  = c("en","fe","ma","ab","my","jn","jl","ag","se","oc","no","di")
)

tipos_eeff <- c("B-2201", "B-3101", "C-1101", "C-2101")

fecha_evaluada <- today()
descarga_completa <- FALSE

while (!descarga_completa) {
  
  eval_year  <- year(fecha_evaluada)
  eval_month <- month(fecha_evaluada)
  
  month_name <- months_str$name[eval_month]
  month_abbr <- months_str$abbr[eval_month]
  
  message(str_dup("=", 50))
  message("Buscando datos para el periodo: ", month_name, " ", eval_year)
  
  # Limpiar la carpeta temporal antes de cada intento de mes
  dir_ls(dir_temp, type = "file") %>% file_delete()
  
  periodo_exitoso <- TRUE
  
  for (tipo in tipos_eeff) {
    
    url <- glue("https://intranet2.sbs.gob.pe/estadistica/financiera/{eval_year}/{month_name}/{tipo}-{month_abbr}{eval_year}.XLS")
    file_name <- basename(url)
    
    # Descargamos en la carpeta temporal
    output_path_temp <- path(dir_temp, file_name) 
    
    message("  Intentando: ", file_name)
    
    descarga_ok <- tryCatch({
      suppressWarnings({
        res <- download.file(url, destfile = output_path_temp, mode = "wb", quiet = TRUE)
      })
      
      if (res == 0 && file_info(output_path_temp)$size > 5000) {
        TRUE
      } else {
        FALSE
      }
    }, error = function(e) FALSE)
    
    if (descarga_ok) {
      message("    [OK] Encontrado y descargado al staging.")
    } else {
      message("    [FALLO] Archivo no disponible aún.")
      periodo_exitoso <- FALSE
      break 
    }
  }
  
  # --- LA TRANSACCIÓN ("Commit") ---
  if (periodo_exitoso) {
    message("\n[ÉXITO] Colección completa verificada para ", month_name, " ", eval_year, ".")
    message("Actualizando datos de producción...")
    
    # 1. Es seguro borrar los archivos viejos en dir_raw
    dir_ls(dir_raw, type = "file") %>% file_delete()
    
    # 2. Movemos los archivos nuevos desde la carpeta temporal a la final
    dir_ls(dir_temp, type = "file") %>% file_move(dir_raw)
    
    descarga_completa <- TRUE
    message("[OK] Actualización finalizada con éxito.")
    
  } else {
    message("[INFO] Periodo incompleto. Retrocediendo un mes...\n")
    fecha_evaluada <- fecha_evaluada %m-% months(1)
    
    if (fecha_evaluada < ymd("2010-01-01")) {
      # Borramos el staging antes de lanzar el error crítico
      dir_delete(dir_temp)
      stop("Se retrocedió hasta 2010 sin encontrar un set completo. Operación cancelada.")
    }
  }
}

# Limpieza final: eliminamos el directorio de staging para no dejar rastros
if (dir_exists(dir_temp)) dir_delete(dir_temp)

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


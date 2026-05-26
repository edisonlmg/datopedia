#===============================================================================
# importar_datasets.R  [mef_gasto_publico]
#
# Objetivo: descargar los datasets crudos de gasto público del repositorio de
#           datos abiertos del MEF
#
# Outputs:
#   - mef_inversion_publica/data/raw/DETALLE_INVERSIONES.csv
#   - mef_inversion_publica/data/raw/CIERRE_INVERSIONES.csv
#   - mef_inversion_publica/data/raw/INVERSIONES_DESACTIVADAS.csv
#
# Fuente: Datos Abiertos del MEF
#   https://datosabiertos.mef.gob.pe/
#===============================================================================


library(lubridate) # para manejo de fechas
library(purrr)     # para iwalk()
library(fs)        # para path() y dir_create()
library(httr)      # para GET(), write_disk() y stop_for_status()
library(glue)      # para texto variable


dir_subproject <- "mef_gasto_publico"
dir_raw        <- path(dir_subproject, "data/raw")

dir_create(dir_raw)


#===============================================================================
# extraccion de datos
#===============================================================================


anio_actual <- as.integer(format(Sys.Date(), "%Y"))
periodos <- 2012:anio_actual


walk(periodos, function(periodo) {
  
  url_diario <- glue("https://fs.datosabiertos.mef.gob.pe/datastorefiles/{periodo}-Gasto-Devengado-Diario.csv")
  url_normal <- glue("https://fs.datosabiertos.mef.gob.pe/datastorefiles/{periodo}-Gasto-Devengado.csv")
  
  path_diario <- path(dir_raw, basename(url_diario))
  path_normal <- path(dir_raw, basename(url_normal))
  
  message(paste0(rep("-", 50), collapse = ""))
  message("Procesando periodo: ", periodo)
  
  # --- Función auxiliar que comprueba necesidad de descarga ---
  revisar_y_descargar <- function(url, path_local) {
    
    if (file_exists(path_local)) {
      res_head <- HEAD(url)
      stop_for_status(res_head) 
      
      last_mod <- headers(res_head)$`last-modified`
      if (!is.null(last_mod)) {
        fecha_server <- httr::parse_http_date(last_mod)
        fecha_local  <- file_info(path_local)$modification_time
        
        if (fecha_server <= fecha_local) {
          message("  [SKIP] Archivo local actualizado: ", basename(url))
          return(TRUE) 
        } else {
          message("  [INFO] Nueva versión en el servidor. Descargando...")
        }
      }
    }
    
    # Usar ruta temporal para la descarga
    path_temp <- paste0(path_local, ".tmp")
    
    res_get <- GET(
      url = url,
      write_disk(path_temp, overwrite = TRUE),
      progress()
    )
    
    stop_for_status(res_get)
    
    # La descarga fue exitosa. Reemplazamos el archivo original.
    file_move(path_temp, path_local)
    message("  [OK] Descarga exitosa: ", basename(url))
    return(TRUE)
  }
  
  # --- Lógica principal ---
  
  tryCatch({
    revisar_y_descargar(url_diario, path_diario)
    
  }, error = function(e) {
    # Solo limpiamos el archivo temporal si la descarga quedó a medias
    path_temp_diario <- paste0(path_diario, ".tmp")
    if (file_exists(path_temp_diario)) file_delete(path_temp_diario)
    
    message("  [INFO] Enlace '-Diario' falló o no existe. Intentando sin sufijo...")
    
    tryCatch({
      revisar_y_descargar(url_normal, path_normal)
      
    }, error = function(e2) {
      path_temp_normal <- paste0(path_normal, ".tmp")
      if (file_exists(path_temp_normal)) file_delete(path_temp_normal)
      
      message("  [ERROR] El servidor no respondió para el periodo: ", periodo, ".\n")
    })
  })
})




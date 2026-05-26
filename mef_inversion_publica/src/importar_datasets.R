#===============================================================================
# importar_datasets.R  [mef_inversion_publica]
#
# Objetivo: descargar los datasets crudos de inversión pública del Sistema
#           Nacional de Programación Multianual y Gestión de Inversiones
#           (Invierte.pe).
#
# Outputs:
#   - mef_inversion_publica/data/raw/DETALLE_INVERSIONES.csv
#   - mef_inversion_publica/data/raw/CIERRE_INVERSIONES.csv
#   - mef_inversion_publica/data/raw/INVERSIONES_DESACTIVADAS.csv
#
# Fuente: Datos Abiertos del MEF
#   https://datosabiertos.mef.gob.pe/
#===============================================================================


library(purrr) # para iwalk()
library(fs)    # para path() y dir_create()
library(httr)  # para GET(), write_disk() y stop_for_status()


dir_subproject <- "mef_inversion_publica"
dir_raw        <- path(dir_subproject, "data/raw")

dir_create(dir_raw)


#===============================================================================
# extraccion de datos
#===============================================================================


# DETALLE_INVERSIONES:      ficha completa de cada inversión activa (viable/aprobada)
# CIERRE_INVERSIONES:       inversiones que cerraron o culminaron
# INVERSIONES_DESACTIVADAS: inversiones dadas de baja del sistema
urls <- c(
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/DETALLE_INVERSIONES.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/CIERRE_INVERSIONES.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/INVERSIONES_DESACTIVADAS.csv"
)


iwalk(urls, ~{
  
  file_name   <- basename(.x)
  output_path <- path(dir_raw, file_name)
  
  message("Intentando descargar: ", file_name)
  
  tryCatch({
    respuesta <- GET(
      url = .x,
      write_disk(output_path, overwrite = TRUE),
      progress()
    )
    # lanza error si el servidor responde con HTTP 4xx/5xx
    stop_for_status(respuesta)
    message("  [OK] Descarga exitosa.\n")
    
  }, error = function(condicion) {
    message("  [ERROR] No se pudo descargar el archivo.\n")
  })
})


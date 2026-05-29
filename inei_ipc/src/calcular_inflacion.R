#===============================================================================
# calcular_inflacion.R  [inei_ipc]
#
# Objetivo: define calcular_inflacion(), función reutilizable que agrega las
#           variaciones de inflación mensual, anual y acumulada a un dataset
#           de IPC en formato largo.
#
# Parámetros:
#   df     : data.frame con al menos las columnas FECHA (Date) e IPC (numeric)
#   grupos : vector de nombres de columnas para agrupar. Casos de uso:
#              - character(0)                    → inflación general (sin grupos)
#              - "DEPARTAMENTO"                  → por región
#              - "INDICADOR"                     → por categoría
#              - c("DEPARTAMENTO", "INDICADOR")  → por región y categoría
#
# Retorna:
#   El mismo df con columnas adicionales: MENSUAL, ACUMULADO, ANUAL (en %)
#   - MENSUAL   : variación respecto al mes anterior
#   - ANUAL     : variación respecto al mismo mes del año anterior (lag 12)
#   - ACUMULADO : variación respecto a diciembre del año anterior
#                 (NA para todos los meses del primer año del dataset,
#                  ya que no hay diciembre previo disponible)
#===============================================================================

library(dplyr)


calcular_inflacion <- function(df, grupos = character(0)) {

  result <- df %>%
    arrange(across(all_of(c(grupos, "FECHA")))) %>%
    group_by(across(all_of(grupos))) %>%
    mutate(
      MENSUAL = round((IPC / lag(IPC) - 1) * 100, 1),
      ANUAL   = round((IPC / lag(IPC, 12) - 1) * 100, 1)
    ) %>%
    ungroup()

  # ACUMULADO: lag() no acepta n variable, se extrae diciembre en tabla
  # aparte y se une como referencia para cada mes del año siguiente
  dic_anterior <- result %>%
    filter(format(FECHA, "%m") == "12") %>%
    mutate(ANIO = as.integer(format(FECHA, "%Y")) + 1L) %>%
    select(all_of(c(grupos, "ANIO")), IPC_DIC = IPC)

  result %>%
    mutate(ANIO = as.integer(format(FECHA, "%Y"))) %>%
    left_join(dic_anterior, by = c(grupos, "ANIO")) %>%
    mutate(ACUMULADO = round((IPC / IPC_DIC - 1) * 100, 1)) %>%
    select(all_of(c("FECHA", grupos, "IPC", "MENSUAL", "ACUMULADO", "ANUAL")))
}

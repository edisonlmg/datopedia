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


library(tidyverse)     # para iwalk()
library(fs)        # para path() y dir_create()
library(vroom)
library(janitor)


dir_subproject <- "mef_gasto_publico"
dir_raw_        <- path(dir_subproject, "data/raw")

dir_create(dir_raw)


gasto2026 <- vroom("C:/Users/Edison/myprojects/datopedia/mef_gasto_publico/data/raw/gasto/2026-Gasto-Devengado-Diario.csv")

desastres2026 <- vroom("C:/Users/Edison/myprojects/datopedia/mef_gasto_publico/data/raw/fondes/2026-Gasto-FONDES.csv")



desastres2026 %>%
  distinct(REFERENCIA)



desastres2026 %>%
  filter(
    NIVEL_GOBIERNO != "E",
    GENERICA_NOMBRE != "PENSIONES Y OTRAS PRESTACIONES SOCIALES"
  ) %>%
  group_by(DISPOSITIVO_LEGAL_NOMBRE) %>%
  summarise(
    CANTIDAD = n_distinct(EJECUTORA),
    ASIGNADO = sum(MONTO_ASIGNADO),
    PRESUPUESTO = sum(MARCO_PRESUPUESTAL),
    GASTO = sum(MONTO_DEVENGADO),
    .groups = "drop"
  ) %>%
  adorn_totals() %>%
  mutate(
    ASIGNADO_PROM = ASIGNADO / CANTIDAD,
    PRESUPUESTO = round(PRESUPUESTO / ASIGNADO * 100, 1),
    GASTO = round(GASTO / ASIGNADO * 100, 1)
  )


tabla <- desastres2026 %>%
  filter(
    NIVEL_GOBIERNO != "E",
    GENERICA_NOMBRE != "PENSIONES Y OTRAS PRESTACIONES SOCIALES"
  ) %>%
  group_by(DEPARTAMENTO_EJECUTORA_NOMBRE) %>%
  summarise(
    GL = n_distinct(EJECUTORA),
    ASIGNADO = sum(MONTO_ASIGNADO),
    PRESUPUESTADO = sum(MARCO_PRESUPUESTAL),
    GASTADO = sum(MONTO_DEVENGADO),
    .groups = "drop"
  ) %>%
  arrange(desc(ASIGNADO)) %>%
  adorn_totals() %>%
  mutate(
    ASIGNADO_PROM = ASIGNADO / GL,
    PRESUPUESTADO = round(PRESUPUESTADO / ASIGNADO * 100, 1),
    GASTADO = round(GASTADO / ASIGNADO * 100, 1)
  ) %>%
  select(
    DEPARTAMENTO_EJECUTORA_NOMBRE,
    GL,
    ASIGNADO,
    ASIGNADO_PROM,
    PRESUPUESTADO,
    GASTADO
  ) %>%
  rename(
    `ASIGNACIÓN PROMEDIO` = ASIGNADO_PROM,
    `PRESUPUESTADO %` = PRESUPUESTADO,
    `GASTADO %` = GASTADO,
    DEPARTAMENTO = DEPARTAMENTO_EJECUTORA_NOMBRE,
    MUNICIPIOS = GL
  )


tabla_img <- gt_table(
  data           = tabla,
  title          = "EJECUCIÓN DE TRANSFERENCIAS DEL FONDES 2026",
  subtitle       = "Fuente: Datos Abiertos del MEF al 27/05/2026",
  caption        = "Nota: no se considera PENSIONES Y OTRAS PRESTACIONES SOCIALES | Elaborado por: @EdisonMondragon",
  cols_decimales = c("PRESUPUESTADO %" = 1, "GASTADO %" = 1),
  big_mark       = ",",
  theme          = "light"
  )


desastres2026 %>%
  distinct(FECHA_PUBLICACION)

gtsave(
  data = tabla_img,
  filename = "tabla.png",
  vwidth = 1100,
  vheight = 662
)

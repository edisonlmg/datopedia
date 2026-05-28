library(tidyverse)
library(vroom)
library(fs)


paths_datasets <- dir_ls("C:/Users/Edison/myprojects/datopedia/mef_gasto_publico/data/raw/gasto")


gasto_pp <- map_dfr(
  paths_datasets, function(.x) {
    df <- vroom(.x) %>%
      filter(PROGRAMA_PPTO %in% c("0126","0128"))
  }
    )

gasto_pp_pim <- gasto_pp %>%
  mutate(
    CADENA = str_c(
      GENERICA,
      SUBGENERICA,
      SUBGENERICA_DET,
      ESPECIFICA,
      ESPECIFICA_DET,
      sep = "."
    )
  ) %>%
  filter(
    !startsWith(CADENA, "5.4"),
    !startsWith(CADENA, "4")
    ) %>%
  group_by(ANO_EJE, PROGRAMA_PPTO) %>%
  summarise(
    PIM = sum(MONTO_PIM),
    DEVENGADO = sum(MONTO_DEVENGADO_ANUAL),
    .groups = "drop"
    ) %>%
  mutate(
    PIM = round(PIM / 1e6, 1),
    DEVENGADO = round(DEVENGADO / 1e6, 1)
  )

line_chart(
  x            = gasto_pp_pim$ANO_EJE,
  y            = gasto_pp_pim$PIM,
  group        = gasto_pp_pim$PROGRAMA_PPTO,
  title        = "PRESUPUESTO PÚBLICO CONTRA LA MINERÍA ILEGAL (2015-2026)",
  subtitle     = "Fuente: Datos Abiertos del MEF al 27.05.2026",
  caption      = "Nota: no considera DONACIONES Y TRANSFERENCIAS y PAGO DE IMPUESTOS,  DERECHOS ADMINISTRATIVOS Y MULTAS GUBERNAMENTALES",
  y_label      = "Monto PIM (Millones S/)",
  series_name  = "Programas Presupuestales",
  label_format = "%.1f",
  theme        = "light"
)


gasto_pp_eje <- gasto_pp %>%
  mutate(
    CADENA = str_c(
      GENERICA,
      SUBGENERICA,
      SUBGENERICA_DET,
      ESPECIFICA,
      ESPECIFICA_DET,
      sep = "."
    )
  ) %>%
  filter(
    ANO_EJE != 2026,
    !startsWith(CADENA, "5.4"),
    !startsWith(CADENA, "4")
  ) %>%
  group_by(ANO_EJE, PROGRAMA_PPTO) %>%
  summarise(
    EJECUCION = sum(MONTO_DEVENGADO_ANUAL) / sum(MONTO_PIM) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    EJECUCION = round(EJECUCION, 1)
  )


line_chart(
  x            = gasto_pp_eje$ANO_EJE,
  y            = gasto_pp_eje$EJECUCION,
  group        = gasto_pp_eje$PROGRAMA_PPTO,
  title        = "EJECUCIÓN DE GASTO PÚBLICO CONTRA LA MINERÍA ILEGAL (2015-2025)",
  subtitle     = "Fuente: Datos Abiertos del MEF al 27.05.2026",
  caption      = "Nota: no considera DONACIONES Y TRANSFERENCIAS y PAGO DE IMPUESTOS,  DERECHOS ADMINISTRATIVOS Y MULTAS GUBERNAMENTALES",
  y_label      = "Ejecución (%)",
  series_name  = "Programas Presupuestales",
  label_format = "%.1f",
  theme        = "light"
)

gasto_pp %>%
  distinct(PROGRAMA_PPTO, PROGRAMA_PPTO_NOMBRE)

gasto2026 <- vroom("mef_gasto_publico/data/raw/gasto/2026-Gasto-Devengado-Diario.csv")


gasto_pp_region <- gasto_pp %>%
  mutate(
    CADENA = str_c(
      GENERICA,
      SUBGENERICA,
      SUBGENERICA_DET,
      ESPECIFICA,
      ESPECIFICA_DET,
      sep = "."
    )
  ) %>%
  filter(
    ANO_EJE %in% c(2025,2026),
    PROGRAMA_PPTO == "0128",
    DEPARTAMENTO_META_NOMBRE != "TUMBES",
    !startsWith(CADENA, "5.4"),
    !startsWith(CADENA, "4")
  ) %>%
  mutate(
    DEPARTAMENTO_META_NOMBRE = if_else(
      DEPARTAMENTO_META_NOMBRE == "PROVINCIA CONSTITUCIONAL DEL CALLAO",
      "CALLAO",
      DEPARTAMENTO_META_NOMBRE
    )
    ) %>%
  group_by(DEPARTAMENTO_META_NOMBRE, ANO_EJE) %>%
  summarise(
    PIM = round(sum(MONTO_PIM) / 1e6, 1),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = ANO_EJE,
    values_from = PIM,
    values_fill = 0
  ) %>%
  mutate(
    VAR_PIM = case_when(
      `2025` == 0 ~ 1,  # Si 2025 es 0, la variación es "No Disponible" (NA)
      TRUE        ~ round((`2026` / `2025`), 1) # En cualquier otro caso, divide normal
    )
  ) %>%
  arrange(desc(VAR_PIM)) %>%
  print(n = 50)

gasto_pp_region

scatter_chart(
  x            = gasto_pp_region$`2026`,
  y            = gasto_pp_region$`2025`,
  label        = gasto_pp_region$DEPARTAMENTO_META_NOMBRE,
  title        = "PRESUPUESTO PÚBLICO CONTRA LA MINERÍA ILEGAL (2025 vs 2026)",
  subtitle     = "Programa Presupuestal 0128: Reducción de la minería ilegal",
  caption      = "Nota: no considera DONACIONES Y TRANSFERENCIAS y PAGO DE IMPUESTOS,  DERECHOS ADMINISTRATIVOS Y MULTAS GUBERNAMENTALES | Fuente: Datos Abiertos del MEF al 27.05.2026",
  x_label      = "PIM 2026 (Millones S/)",
  y_label      = "PIM 2026 (Millones S/)",
  show_legend =   FALSE,
  theme        = "light"
) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1)


bar_chart(
  x           = gasto_pp_region$DEPARTAMENTO_META_NOMBRE,
  y           = gasto_pp_region$VAR_PIM,
  orientation = "horizontal",
  title       = "VARIACIÓN DE PRESUPUESTO PÚBLICO CONTRA LA MINERÍA ILEGAL (2025-2026)",
  subtitle    = "Programa Presupuestal 0128: Reducción de la minería ilegal",
  caption     = "Fuente: Datos Abiertos del MEF al 27.5.2026 | X: @EdisonMondragon",
  y_label     = "Variación PIM 2026/2025 (veces)",
  label_decimals = 1,
  theme = "light"
)









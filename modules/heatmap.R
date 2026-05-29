#===============================================================================
# heatmap.R
#
# Funciones:
#   - mapa_calor()  heatmap con ordenamiento diagonal ~45° desde el origen
#
# Colores disponibles: "verde", "rojo", "morado", "marron", "azul", "verde_claro"
# color_alto: color del extremo alto del gradiente (nombre o hex); default = "azul"
#===============================================================================

library(dplyr)
library(ggplot2)
library(ggtext)
library(showtext)

font_add_google("Montserrat", "montserrat")
showtext_auto()

.PALETA <- c(
  verde       = "#195620",
  rojo        = "#C84A54",
  morado      = "#9B509F",
  marron      = "#BD8549",
  azul        = "#3B6294",
  verde_claro = "#4EA58B"
)

.resolver_color <- function(color) {
  if (color %in% names(.PALETA)) .PALETA[[color]] else color
}


mapa_calor <- function(
    data,
    x_col,
    y_col,
    value_col,
    color_alto         = "azul",   # extremo alto del gradiente: nombre de .PALETA o hex
    title              = "",
    subtitle           = "",
    caption            = "",
    label_format       = "%.1f",
    mostrar_etiquetas  = TRUE,
    mostrar_cuadricula = FALSE,
    mostrar_eje_x      = TRUE,
    mostrar_eje_y      = TRUE,
    fondo              = "blanco"  # "blanco" o "beige"
) {
  bg_color   <- if (fondo == "beige") "#F6F5F0" else "white"
  low_color  <- "#dedad2"
  high_color <- .resolver_color(color_alto)
  na_color   <- "#c8c4bc"
  text_color <- "#2a2a2a"
  grid_color <- "#F6F5F0"

  # Ordenar ambos ejes por valor medio ascendente para trayectoria de 45°
  x_order <- data |>
    group_by(.data[[x_col]]) |>
    summarise(.m = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
    arrange(.m) |>
    pull(.data[[x_col]])

  y_order <- data |>
    group_by(.data[[y_col]]) |>
    summarise(.m = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
    arrange(.m) |>
    pull(.data[[y_col]])

  data <- data |>
    mutate(
      across(all_of(x_col), \(x) factor(x, levels = x_order)),
      across(all_of(y_col), \(x) factor(x, levels = y_order))
    )

  tile_color  <- if (mostrar_cuadricula) grid_color else NA
  label_layer <- NULL
  if (mostrar_etiquetas) {
    label_layer <- geom_text(
      aes(x = .data[[x_col]], y = .data[[y_col]],
          label = sprintf(label_format, .data[[value_col]])),
      color = text_color, family = "montserrat", fontface = "bold", size = 3
    )
  }

  ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[value_col]])) +
    geom_tile(color = tile_color, linewidth = 0.5) +
    label_layer +
    scale_fill_gradient(low = low_color, high = high_color, na.value = na_color,
                        name = NULL, labels = \(x) sprintf(label_format, x)) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_minimal(base_family = "montserrat") +
    theme(
      plot.background  = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      panel.grid       = element_blank(),
      text             = element_text(color = text_color),
      axis.text        = element_text(color = text_color, size = 9),
      axis.text.x      = if (mostrar_eje_x) element_text(angle = 45, hjust = 1) else element_blank(),
      axis.text.y      = if (mostrar_eje_y) element_text() else element_blank(),
      axis.title       = element_blank(),
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 22, margin = margin(b = 10)),
      plot.subtitle    = element_markdown(hjust = 0.5, size = 12, margin = margin(b = 20)),
      plot.caption     = element_markdown(hjust = 0.5, size = 10, margin = margin(t = 20)),
      legend.text      = element_text(color = text_color, size = 10),
      plot.margin      = margin(30, 30, 30, 30)
    )
}

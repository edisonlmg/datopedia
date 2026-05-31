#===============================================================================
# social_heatmap.R  —  versión redes sociales
# Formato vertical 4:5  →  ggsave(..., width = 8, height = 10, dpi = 135)
#===============================================================================

library(dplyr)
library(ggplot2)
library(ggtext)
library(showtext)
font_add_google("Montserrat", "montserrat")
font_add_google("Playfair Display", "playfair")
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


social_mapa_calor <- function(
    data,
    x_col,
    y_col,
    value_col,
    color_alto         = "azul",
    title              = "",
    subtitle           = "",
    caption            = "",
    label_format       = "%.1f",
    mostrar_etiquetas  = TRUE,
    mostrar_cuadricula = FALSE,
    mostrar_eje_x      = TRUE,
    mostrar_eje_y      = TRUE,
    max_x              = 30,   # máx. caracteres etiquetas eje x (0 = sin truncar)
    max_y              = 44,   # máx. caracteres etiquetas eje y
    orden_x            = NULL, # vector con orden personalizado para eje x (NULL = por media)
    orden_y            = NULL, # vector con orden personalizado para eje y (NULL = por media)
    fondo              = "beige"
) {
  bg_color   <- if (fondo == "beige") "#F6F5F0" else "white"
  low_color  <- if (fondo == "beige") "#dedad2" else "#e8e8e8"
  high_color <- .resolver_color(color_alto)
  na_color   <- if (fondo == "beige") "#c8c4bc" else "#d0d0d0"
  .mb_titulo <- function(s) (ceiling(nchar(s) / 50) - 1) * 15 + 4

  text_color <- "#2a2a2a"
  sub_color  <- "#555555"
  cap_color  <- "#888888"
  grid_color <- bg_color

  x_order <- if (!is.null(orden_x)) orden_x else {
    data |>
      group_by(.data[[x_col]]) |>
      summarise(.m = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
      arrange(.m) |>
      pull(.data[[x_col]])
  }

  y_order <- if (!is.null(orden_y)) orden_y else {
    data |>
      group_by(.data[[y_col]]) |>
      summarise(.m = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
      arrange(.m) |>
      pull(.data[[y_col]])
  }

  .truncar <- function(s, n) ifelse(nchar(s) > n, paste0(substr(s, 1, n - 1), "…"), s)

  if (max_x > 0) x_order <- .truncar(x_order, max_x)
  if (max_y > 0) y_order <- .truncar(y_order, max_y)

  data <- data |>
    mutate(
      across(all_of(x_col), \(v) {
        lv <- if (max_x > 0) .truncar(as.character(v), max_x) else as.character(v)
        factor(lv, levels = x_order)
      }),
      across(all_of(y_col), \(v) {
        lv <- if (max_y > 0) .truncar(as.character(v), max_y) else as.character(v)
        factor(lv, levels = y_order)
      })
    )

  tile_color  <- if (mostrar_cuadricula) grid_color else NA
  label_layer <- NULL
  if (mostrar_etiquetas) {
    label_layer <- geom_text(
      aes(x = .data[[x_col]], y = .data[[y_col]],
          label = sprintf(label_format, .data[[value_col]])),
      color = text_color, family = "montserrat", fontface = "bold", size = 5
    )
  }

  ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[value_col]])) +
    geom_tile(color = tile_color, linewidth = 0.5) +
    label_layer +
    scale_fill_gradient(low = low_color, high = high_color, na.value = na_color,
                        name = NULL, labels = \(v) sprintf(label_format, v)) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_minimal(base_family = "montserrat") +
    theme(
      plot.background  = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      panel.grid       = element_blank(),
      text             = element_text(color = text_color),

      plot.title = element_textbox_simple(
        family = "playfair", face = "bold", size = 28, color = text_color,
        halign = 0, lineheight = 1.0, margin = margin(b = .mb_titulo(title), t = 8)
      ),
      plot.subtitle = element_textbox_simple(
        family = "montserrat", size = 18, color = sub_color,
        halign = 0, lineheight = 1.0, margin = margin(b = 14)
      ),
      plot.caption = element_textbox_simple(
        family = "montserrat", size = 18, color = cap_color,
        halign = 0, lineheight = 1.0, margin = margin(t = 12)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      axis.text   = element_text(color = sub_color, size = 12, lineheight = 1.3),
      axis.text.x = if (mostrar_eje_x) element_text(angle = 45, hjust = 1) else element_blank(),
      axis.text.y = if (mostrar_eje_y) element_text() else element_blank(),
      axis.title  = element_blank(),

      legend.text = element_text(color = text_color, size = 13),
      plot.margin = margin(10, 10, 10, 10)
    )
}

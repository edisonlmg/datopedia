#===============================================================================
# line_charts.R
#
# Funciones:
#   - line_chart()  gráfico de líneas con etiquetas y líneas de referencia
#
# Colores disponibles: "verde", "rojo", "morado", "marron", "azul", "verde_claro"
# Una sola serie : parámetro color (nombre o hex)
# Múltiples series: parámetro colores (vector de nombres/hex); default = toda la paleta
#===============================================================================

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

.COLORES_DEFAULT <- unname(.PALETA[c("azul", "verde_claro", "rojo", "morado", "marron", "verde")])

.resolver_color  <- function(color) {
  if (color %in% names(.PALETA)) .PALETA[[color]] else color
}

.resolver_colores <- function(colores, n) {
  hex <- sapply(colores, .resolver_color)
  if (length(hex) < n) colorRampPalette(hex)(n) else hex[seq_len(n)]
}


line_chart <- function(
    x,
    y,
    group        = NULL,
    color        = "azul",        # serie única: nombre de .PALETA o hex
    colores      = NULL,          # múltiples series: vector de nombres/hex (NULL = default)
    title        = "",
    subtitle     = "",
    caption      = "",
    y_label      = "Y",
    x_label      = NULL,
    series_name  = "Serie",
    y_limits     = NULL,
    y_breaks     = NULL,
    x_limits     = NULL,
    x_breaks     = NULL,
    hlines       = NULL,
    vlines       = NULL,
    show_labels  = TRUE,
    label_format = "%.1f",
    show_legend  = TRUE,
    fondo        = "blanco"  # "blanco" o "beige"
) {
  bg_color    <- if (fondo == "beige") "#F6F5F0" else "white"
  grid_color  <- "#d8d5cd"
  text_color  <- "#2a2a2a"
  ref_color   <- "#555555"

  multi <- !is.null(group)

  if (multi) {
    df <- data.frame(x = x, y = y, group = as.character(group))
  } else {
    df <- data.frame(x = x, y = y, group = series_name)
  }

  grupos   <- unique(df$group)
  n_grupos <- length(grupos)

  if (multi) {
    base     <- if (is.null(colores)) .COLORES_DEFAULT else colores
    paleta   <- .resolver_colores(base, n_grupos)
  } else {
    paleta <- .resolver_color(color)
  }
  color_map <- setNames(paleta, grupos)

  if (is.null(y_limits)) {
    y_pad    <- diff(range(y)) * 0.2
    y_limits <- c(max(0, min(y) - y_pad), max(y) + y_pad)
  }
  if (is.null(y_breaks)) y_breaks <- pretty(y_limits, n = 6)

  x_breaks_user <- !is.null(x_breaks)
  if (is.null(x_limits)) x_limits <- range(x)
  if (is.null(x_breaks)) x_breaks <- unique(x)

  p <- ggplot(df, aes(x = x, y = y))

  if (!is.null(vlines) && nrow(vlines) > 0) {
    for (i in seq_len(nrow(vlines))) {
      vl_linetype <- if ("linetype" %in% names(vlines)) vlines$linetype[i] else "dashed"
      vl_label    <- if ("label"    %in% names(vlines)) vlines$label[i]    else NA
      p <- p + geom_vline(xintercept = vlines$xintercept[i], color = ref_color,
                          linewidth = 0.8, linetype = vl_linetype)
      if (!is.na(vl_label) && vl_label != "") {
        p <- p + annotate("text", x = vlines$xintercept[i], y = y_limits[2] * 0.97,
                          label = vl_label, color = ref_color, family = "montserrat",
                          size = 3.5, hjust = -0.1, fontface = "bold")
      }
    }
  }

  if (!is.null(hlines) && nrow(hlines) > 0) {
    hline_types <- setNames(
      if ("linetype" %in% names(hlines)) hlines$linetype else rep("dashed", nrow(hlines)),
      hlines$label
    )
    for (i in seq_len(nrow(hlines))) {
      p <- p + geom_hline(aes(yintercept = !!hlines$yintercept[i], linetype = !!hlines$label[i]),
                          color = ref_color, linewidth = 1)
    }
    p <- p + scale_linetype_manual(name = NULL, values = hline_types)
  }

  p <- p +
    geom_line(aes(color = group, group = group), linewidth = 1.5,
              lineend = "round", linejoin = "round") +
    geom_point(aes(color = group), size = 4) +
    geom_point(color = bg_color, size = 1.5)

  if (show_labels) {
    p <- p + geom_text(
      aes(label = sprintf(label_format, y), color = group),
      vjust = -1.5, family = "montserrat", fontface = "bold",
      size = 4.5, show.legend = FALSE
    )
  }

  legend_name <- if (multi) series_name else NULL

  p <- p +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_color_manual(name = legend_name, values = color_map)

  if (inherits(x, "Date")) {
    if (x_breaks_user) {
      p <- p + scale_x_date(limits = x_limits, breaks = x_breaks, date_labels = "%d %b\n%Y")
    } else {
      p <- p + scale_x_date(limits = x_limits, date_breaks = "1 month", date_labels = "%b\n%Y")
    }
  } else if (inherits(x, c("POSIXct", "POSIXlt"))) {
    if (x_breaks_user) {
      p <- p + scale_x_datetime(limits = x_limits, breaks = x_breaks, date_labels = "%d %b\n%Y")
    } else {
      p <- p + scale_x_datetime(limits = x_limits, date_breaks = "1 month", date_labels = "%b\n%Y")
    }
  } else {
    p <- p + scale_x_continuous(limits = x_limits, breaks = x_breaks)
  }

  p +
    labs(title = title, subtitle = subtitle, caption = caption, y = y_label, x = x_label) +
    theme_minimal() +
    theme(
      plot.background  = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      text             = element_text(family = "montserrat", color = text_color),

      plot.title    = element_text(hjust = 0.5, face = "bold", size = 22, margin = margin(b = 10)),
      plot.subtitle = element_markdown(hjust = 0.5, size = 12, margin = margin(b = 20)),
      plot.caption  = element_markdown(hjust = 0.5, size = 10, margin = margin(t = 20)),

      axis.line         = element_line(color = text_color, linewidth = 0.5),
      axis.ticks        = element_line(color = text_color, linewidth = 0.5),
      axis.ticks.length = unit(0.2, "cm"),
      axis.text.y       = element_text(color = text_color, size = 12),
      axis.text.x       = element_text(color = text_color, size = 12, margin = margin(t = 5),
                                       angle = if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) 45 else 0,
                                       hjust = if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) 1 else 0.5),
      axis.title.y      = element_text(size = 12, margin = margin(r = 10)),

      panel.grid.major  = element_line(color = grid_color, linewidth = 0.5),
      panel.grid.minor  = element_blank(),

      legend.position   = if (show_legend && multi) "bottom" else "none",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(color = text_color, size = 12),
      legend.margin     = margin(t = 15),

      plot.margin = margin(30, 30, 30, 30)
    )
}

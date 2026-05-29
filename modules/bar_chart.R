#===============================================================================
# bar_chart.R
#
# Funciones:
#   - bar_chart()   gráfico de barras vertical u horizontal
#
# Colores disponibles: "verde", "rojo", "morado", "marron", "azul", "verde_claro"
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

.resolver_color <- function(color) {
  if (color %in% names(.PALETA)) .PALETA[[color]] else color
}


bar_chart <- function(
    x,
    y,
    color          = "azul",     # nombre de .PALETA o hex directo
    orientation    = "vertical", # "vertical" o "horizontal"
    title          = "",
    subtitle       = "",
    caption        = "",
    y_label        = "Valores",
    x_label        = "Categorías",
    y_limits       = NULL,
    y_breaks       = NULL,
    hlines         = NULL,
    vlines         = NULL,
    show_labels    = TRUE,
    label_decimals = 0,
    label_big_mark = ",",
    fondo          = "blanco"  # "blanco" o "beige"
) {
  bg_color    <- if (fondo == "beige") "#F6F5F0" else "white"
  grid_color  <- "#d8d5cd"
  text_color  <- "#2a2a2a"
  ref_color   <- "#555555"
  label_color <- "#2a2a2a"
  bar_color   <- .resolver_color(color)

  df      <- data.frame(x = as.character(x), y = as.numeric(y))
  df$x    <- factor(df$x, levels = df$x)

  if (is.null(y_limits)) {
    y_pad    <- diff(range(df$y)) * 0.15
    y_limits <- c(min(0, min(df$y)), max(df$y) + y_pad)
  }
  if (is.null(y_breaks)) {
    y_breaks <- pretty(y_limits, n = 6)
  }

  if (orientation == "vertical") {

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_col(fill = bar_color, width = 0.7)

    if (!is.null(hlines) && nrow(hlines) > 0) {
      hline_types <- setNames(
        if ("linetype" %in% names(hlines)) hlines$linetype else rep("dashed", nrow(hlines)),
        hlines$label
      )
      for (i in seq_len(nrow(hlines))) {
        p <- p + geom_hline(
          aes(yintercept = !!hlines$yintercept[i], linetype = !!hlines$label[i]),
          color = ref_color, linewidth = 1
        )
      }
      p <- p + scale_linetype_manual(name = NULL, values = hline_types)
    }

    if (show_labels) {
      p <- p + geom_text(
        aes(label = trimws(format(round(y, label_decimals), nsmall = label_decimals,
                                  big.mark = label_big_mark, scientific = FALSE))),
        vjust = -0.5, color = label_color, family = "montserrat", fontface = "bold", size = 4.5
      )
    }

    p <- p +
      scale_y_continuous(limits = y_limits, breaks = y_breaks) +
      scale_x_discrete()

  } else {

    p <- ggplot(df, aes(x = y, y = factor(x, levels = rev(levels(x))))) +
      geom_col(fill = bar_color, width = 0.7)

    if (!is.null(vlines) && nrow(vlines) > 0) {
      for (i in seq_len(nrow(vlines))) {
        vl_linetype <- if ("linetype" %in% names(vlines)) vlines$linetype[i] else "dashed"
        vl_label    <- if ("label"    %in% names(vlines)) vlines$label[i]    else NA

        p <- p + geom_vline(
          xintercept = vlines$xintercept[i], color = ref_color,
          linewidth = 0.8, linetype = vl_linetype
        )

        if (!is.na(vl_label) && vl_label != "") {
          p <- p + annotate(
            "text", x = vlines$xintercept[i], y = nrow(df) + 0.6,
            label = vl_label, color = ref_color, family = "montserrat",
            size = 3.5, hjust = -0.1, fontface = "bold"
          )
        }
      }
    }

    if (show_labels) {
      p <- p + geom_text(
        aes(label = trimws(format(round(y, label_decimals), nsmall = label_decimals,
                                  big.mark = label_big_mark, scientific = FALSE))),
        hjust = -0.2, color = label_color, family = "montserrat", fontface = "bold", size = 4.5
      )
    }

    p <- p + scale_x_continuous(limits = y_limits, breaks = y_breaks)

    temp_label <- x_label
    x_label    <- y_label
    y_label    <- temp_label
  }

  p +
    labs(title = title, subtitle = subtitle, caption = caption, y = y_label, x = x_label) +
    theme_minimal() +
    theme(
      plot.background  = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      text             = element_text(family = "montserrat", color = text_color),

      plot.title          = element_text(hjust = 0.5, face = "bold", size = 22, margin = margin(b = 10)),
      plot.title.position = "plot",
      plot.subtitle       = element_markdown(hjust = 0.5, size = 18, margin = margin(b = 20)),
      plot.caption        = element_markdown(hjust = 0.5, size = 16, margin = margin(t = 20)),

      axis.line         = element_line(color = text_color, linewidth = 0.5),
      axis.ticks        = element_line(color = text_color, linewidth = 0.5),
      axis.ticks.length = unit(0.2, "cm"),
      axis.text.y       = element_text(color = text_color, size = 12),
      axis.text.x       = element_text(color = text_color, size = 12, margin = margin(t = 5)),
      axis.title.y      = element_text(size = 12, margin = margin(r = 10)),
      axis.title.x      = element_text(size = 12, margin = margin(t = 10)),

      panel.grid.major  = element_line(color = grid_color, linewidth = 0.5),
      panel.grid.minor  = element_blank(),
      legend.position   = "none",
      plot.margin       = margin(30, 30, 30, 30)
    ) +
    coord_cartesian(clip = "off")
}

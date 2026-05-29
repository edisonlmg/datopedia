#===============================================================================
# social_scatter_chart.R  —  versión redes sociales
# Formato vertical 4:5  →  ggsave(..., width = 8, height = 10, dpi = 135)
#===============================================================================

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

.COLORES_DEFAULT <- unname(.PALETA[c("azul", "verde_claro", "rojo", "morado", "marron", "verde")])

.resolver_color  <- function(color) {
  if (color %in% names(.PALETA)) .PALETA[[color]] else color
}

.resolver_colores <- function(colores, n) {
  hex <- sapply(colores, .resolver_color)
  if (length(hex) < n) colorRampPalette(hex)(n) else hex[seq_len(n)]
}


social_scatter_chart <- function(
    x,
    y,
    group        = NULL,
    label        = NULL,
    size         = NULL,
    color        = "azul",
    colores      = NULL,
    title        = "",
    subtitle     = "",
    caption      = "",
    y_label      = "",
    x_label      = "",
    series_name  = "Serie",
    y_limits     = NULL,
    y_breaks     = NULL,
    x_limits     = NULL,
    x_breaks     = NULL,
    hlines       = NULL,
    vlines       = NULL,
    point_size   = 5,
    trend_line   = FALSE,
    trend_method = "lm",
    show_legend  = TRUE,
    fondo        = "blanco"
) {
  bg_color   <- if (fondo == "beige") "#F6F5F0" else "white"
  grid_color <- if (fondo == "beige") "#d8d5cd" else "#e8e8e8"
  text_color <- "#2a2a2a"
  sub_color  <- "#555555"
  cap_color  <- "#888888"
  ref_color  <- "#555555"

  .mb_titulo <- function(s) (ceiling(nchar(s) / 28) - 1) * 22 + 5

  multi <- !is.null(group)
  df    <- data.frame(x = x, y = y,
                      group = if (multi) as.character(group) else series_name)
  if (!is.null(label)) df$label <- as.character(label)
  if (!is.null(size))  df$size  <- as.numeric(size)

  grupos   <- unique(df$group)
  n_grupos <- length(grupos)

  if (multi) {
    base   <- if (is.null(colores)) .COLORES_DEFAULT else colores
    paleta <- .resolver_colores(base, n_grupos)
  } else {
    paleta <- .resolver_color(color)
  }
  color_map <- setNames(paleta, grupos)

  if (is.null(y_limits)) {
    y_pad    <- diff(range(y)) * 0.2
    y_limits <- c(min(y) - y_pad, max(y) + y_pad)
  }
  if (is.null(y_breaks)) y_breaks <- pretty(y_limits, n = 5)
  if (is.null(x_limits)) {
    x_pad    <- diff(range(x)) * 0.2
    x_limits <- c(min(x) - x_pad, max(x) + x_pad)
  }
  if (is.null(x_breaks)) x_breaks <- pretty(x_limits, n = 5)

  p <- ggplot(df, aes(x = x, y = y))

  if (!is.null(vlines) && nrow(vlines) > 0) {
    for (i in seq_len(nrow(vlines))) {
      vl_lt  <- if ("linetype" %in% names(vlines)) vlines$linetype[i] else "dashed"
      vl_lbl <- if ("label"    %in% names(vlines)) vlines$label[i]    else NA
      p <- p + geom_vline(xintercept = vlines$xintercept[i], color = ref_color,
                          linewidth = 0.8, linetype = vl_lt)
      if (!is.na(vl_lbl) && vl_lbl != "") {
        p <- p + annotate("text", x = vlines$xintercept[i], y = y_limits[2] * 0.97,
                          label = vl_lbl, color = ref_color, family = "montserrat",
                          size = 4.5, hjust = -0.1, fontface = "bold")
      }
    }
  }

  if (!is.null(hlines) && nrow(hlines) > 0) {
    hline_types <- setNames(
      if ("linetype" %in% names(hlines)) hlines$linetype else rep("dashed", nrow(hlines)),
      hlines$label
    )
    for (i in seq_len(nrow(hlines))) {
      p <- p + geom_hline(
        aes(yintercept = !!hlines$yintercept[i], linetype = !!hlines$label[i]),
        color = ref_color, linewidth = 0.8
      )
    }
    p <- p + scale_linetype_manual(name = NULL, values = hline_types)
  }

  if (trend_line) {
    p <- p + geom_smooth(aes(color = group, group = group), method = trend_method,
                         formula = y ~ x, se = FALSE, linewidth = 1.2,
                         linetype = "solid", alpha = 0.7, show.legend = FALSE)
  }

  if (!is.null(size)) {
    p <- p +
      geom_point(aes(color = group, size = size), alpha = 0.85) +
      geom_point(aes(size = size * 0.25), color = bg_color, alpha = 0.6) +
      scale_size_continuous(range = c(4, 16), guide = "none")
  } else {
    p <- p +
      geom_point(aes(color = group), size = point_size, alpha = 0.85) +
      geom_point(color = bg_color, size = point_size * 0.35, alpha = 0.6)
  }

  if (!is.null(label)) {
    p <- p + geom_text(aes(label = label, color = group), vjust = -1.2,
                       family = "montserrat", fontface = "bold",
                       size = 5.5, check_overlap = TRUE, show.legend = FALSE)
  }

  legend_name <- if (multi) series_name else NULL

  p +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    scale_color_manual(name = legend_name, values = color_map) +
    labs(title = title, subtitle = subtitle, caption = caption,
         y = y_label, x = x_label) +
    theme_minimal() +
    theme(
      plot.background  = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      text             = element_text(family = "montserrat", color = text_color),

      plot.title = element_textbox_simple(
        family = "playfair", face = "bold", size = 28, color = text_color,
        halign = 0, lineheight = 1.2, margin = margin(b = .mb_titulo(title), t = 8)
      ),
      plot.subtitle = element_textbox_simple(
        family = "montserrat", size = 18, color = sub_color,
        halign = 0, lineheight = 1.3, margin = margin(b = 14)
      ),
      plot.caption = element_textbox_simple(
        family = "montserrat", size = 18, color = cap_color,
        halign = 0, lineheight = 1.4, margin = margin(t = 12)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      axis.line         = element_line(color = "#bbbbbb", linewidth = 0.4),
      axis.ticks        = element_blank(),
      axis.text         = element_text(color = sub_color, size = 13, lineheight = 1.2),
      axis.title        = element_text(size = 13, color = sub_color),
      axis.title.y      = element_text(margin = margin(r = 10)),
      axis.title.x      = element_text(margin = margin(t = 10)),

      panel.grid.major  = element_line(color = grid_color, linewidth = 0.5),
      panel.grid.minor  = element_blank(),

      legend.position   = if (show_legend && multi) "bottom" else "none",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(color = text_color, size = 13),
      legend.margin     = margin(t = 12),

      plot.margin = margin(10, 10, 10, 10)
    ) +
    coord_cartesian(clip = "off")
}

#===============================================================================
# social_donut_chart.R  —  versión redes sociales
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


social_donut_chart <- function(
    labels,
    values,
    colores        = NULL,
    title          = "",
    subtitle       = "",
    caption        = "",
    show_labels    = TRUE,
    show_legend    = TRUE,
    label_decimals = 1,
    hole_size      = 0.5,
    center_label   = NULL,
    center_size    = 8,
    fondo          = "beige"
) {
  .mb_titulo <- function(s) (ceiling(nchar(s) / 28) - 1) * 22 + 5

  bg_color   <- if (fondo == "beige") "#F6F5F0" else "white"
  text_color <- "#2a2a2a"
  sub_color  <- "#555555"
  cap_color  <- "#888888"

  df        <- data.frame(label = as.character(labels), value = as.numeric(values))
  df$label  <- factor(df$label, levels = df$label)
  total     <- sum(df$value)
  df$pct    <- df$value / total * 100
  df$ymax   <- cumsum(df$pct)
  df$ymin   <- c(0, head(df$ymax, -1))
  df$y_mid  <- (df$ymax + df$ymin) / 2
  label_r   <- 1 - hole_size / 2

  base      <- if (is.null(colores)) .COLORES_DEFAULT else colores
  paleta    <- .resolver_colores(base, nrow(df))
  color_map <- setNames(paleta, levels(df$label))

  p <- ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = hole_size, fill = label)) +
    geom_rect(color = bg_color, linewidth = 1) +
    scale_fill_manual(values = color_map, name = NULL) +
    coord_polar(theta = "y", start = 0) +
    xlim(0, 1.05)

  if (show_labels) {
    df$pct_label <- ifelse(
      df$pct >= 4,
      paste0(formatC(df$pct, digits = label_decimals, format = "f"), "%"),
      ""
    )
    p <- p + geom_text(
      data = df, aes(x = label_r, y = y_mid, label = pct_label),
      color = "white", family = "montserrat", fontface = "bold",
      size = 6, inherit.aes = FALSE
    )
  }

  if (!is.null(center_label)) {
    p <- p + annotate("text", x = 0, y = 0, label = center_label,
                      color = text_color, family = "montserrat",
                      fontface = "bold", size = center_size)
  }

  p +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      text             = element_text(family = "montserrat", color = text_color),

      plot.title = element_textbox_simple(
        family = "playfair", face = "bold", size = 28, color = text_color,
        halign = 0, lineheight = 1.0, margin = margin(b = .mb_titulo(title), t = 8)
      ),
      plot.subtitle = element_textbox_simple(
        family = "montserrat", size = 18, color = sub_color,
        halign = 0, lineheight = 1.0, margin = margin(b = 14)
      ),
      plot.caption = element_textbox_simple(
        family = "montserrat", size = 16, color = cap_color,
        halign = 0, lineheight = 1.0, margin = margin(t = 12)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      legend.position   = if (show_legend) "bottom" else "none",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(color = text_color, size = 14),
      legend.margin     = margin(t = 10),

      plot.margin = margin(10, 10, 10, 10)
    )
}

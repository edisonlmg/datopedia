#===============================================================================
# social_stacked_bar_chart.R  —  versión redes sociales
# Formato vertical 4:5  →  ggsave(..., width = 8, height = 10, dpi = 135)
#                           = 1080 × 1350 px
#
# Parámetros principales:
#   x              vector categórico (eje de las barras)
#   y              vector numérico (altura de cada segmento)
#   fill           vector de grupo (define los segmentos apilados)
#   colores        vector de colores (nombres de paleta o hex); por defecto .COLORES_DEFAULT
#   orientation    "vertical" o "horizontal"
#   fill_100       TRUE  = barras al 100 % (proporciones); FALSE = valores absolutos
#   label_min_prop proporción mínima del total para que el segmento muestre etiqueta
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

.resolver_color <- function(color) {
  if (color %in% names(.PALETA)) .PALETA[[color]] else color
}

.resolver_colores <- function(colores, n) {
  hex <- sapply(colores, .resolver_color)
  if (length(hex) < n) colorRampPalette(hex)(n) else hex[seq_len(n)]
}


social_stacked_bar_chart <- function(
    x,
    y,
    fill,
    colores        = NULL,
    orientation    = "vertical",
    fill_100       = FALSE,
    title          = "",
    subtitle       = "",
    caption        = "",
    y_label        = "",
    x_label        = "",
    y_limits       = NULL,
    y_breaks       = NULL,
    show_labels    = TRUE,
    label_decimals = 1,
    label_big_mark = ",",
    label_min_prop = 0.05,
    max_x          = 36,
    show_legend    = TRUE,
    legend_name    = "",
    fondo          = "blanco"
) {
  bg_color   <- if (fondo == "beige") "#F6F5F0" else "white"
  grid_color <- if (fondo == "beige") "#d8d5cd" else "#e8e8e8"
  text_color <- "#2a2a2a"
  sub_color  <- "#555555"
  cap_color  <- "#888888"

  .mb_titulo <- function(s) (ceiling(nchar(s) / 28) - 1) * 22 + 5

  df <- data.frame(
    x    = as.character(x),
    y    = as.numeric(y),
    fill = as.character(fill)
  )
  df$x    <- factor(df$x,    levels = unique(df$x))
  df$fill <- factor(df$fill, levels = unique(df$fill))

  .truncar <- function(s, n) ifelse(nchar(s) > n, paste0(substr(s, 1, n - 1), "…"), s)
  if (max_x > 0) levels(df$x) <- .truncar(levels(df$x), max_x)

  totales  <- tapply(df$y, df$x, sum, na.rm = TRUE)
  df$total <- totales[as.character(df$x)]
  df$prop  <- df$y / df$total

  grupos    <- levels(df$fill)
  n_grupos  <- length(grupos)
  base      <- if (is.null(colores)) .COLORES_DEFAULT else colores
  paleta    <- .resolver_colores(base, n_grupos)
  color_map <- setNames(paleta, grupos)

  pos_bar   <- if (fill_100) "fill"                     else "stack"
  pos_label <- if (fill_100) position_fill(vjust = 0.5) else position_stack(vjust = 0.5)

  fmt_label <- function(v, pct) {
    if (pct) {
      paste0(trimws(format(round(v * 100, label_decimals), nsmall = label_decimals)), "%")
    } else {
      trimws(format(round(v, label_decimals), nsmall = label_decimals,
                    big.mark = label_big_mark, scientific = FALSE))
    }
  }

  if (is.null(y_limits)) {
    y_limits <- if (fill_100) c(0, 1) else c(0, max(totales, na.rm = TRUE) * 1.05)
  }
  if (is.null(y_breaks)) y_breaks <- pretty(y_limits, n = 5)

  axis_labels <- if (fill_100) function(v) paste0(round(v * 100), "%") else waiver()

  if (orientation == "vertical") {

    p <- ggplot(df, aes(x = x, y = y, fill = fill)) +
      geom_col(position = pos_bar, width = 0.65)

    if (show_labels) {
      df_lbl <- df[df$prop >= label_min_prop, ]
      df_lbl$label_txt <- fmt_label(if (fill_100) df_lbl$prop else df_lbl$y, fill_100)
      p <- p + geom_text(
        data     = df_lbl,
        aes(label = label_txt),
        position = pos_label,
        color    = "white", family = "montserrat", fontface = "bold", size = 5
      )
    }

    p <- p +
      scale_y_continuous(limits = y_limits, breaks = y_breaks, labels = axis_labels) +
      scale_x_discrete()

    grid_y <- element_line(color = grid_color, linewidth = 0.5)
    grid_x <- element_blank()

  } else {

    p <- ggplot(df, aes(x = y, y = factor(x, levels = rev(levels(x))), fill = fill)) +
      geom_col(position = pos_bar, width = 0.65)

    if (show_labels) {
      df_lbl <- df[df$prop >= label_min_prop, ]
      df_lbl$label_txt <- fmt_label(if (fill_100) df_lbl$prop else df_lbl$y, fill_100)
      p <- p + geom_text(
        data     = df_lbl,
        aes(label = label_txt),
        position = pos_label,
        color    = "white", family = "montserrat", fontface = "bold", size = 5
      )
    }

    p <- p +
      scale_x_continuous(limits = y_limits, breaks = y_breaks, labels = axis_labels)

    temp    <- x_label
    x_label <- y_label
    y_label <- temp

    grid_y <- element_blank()
    grid_x <- element_line(color = grid_color, linewidth = 0.5)
  }

  p +
    scale_fill_manual(
      name   = if (nchar(legend_name) == 0) NULL else legend_name,
      values = color_map
    ) +
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
      axis.text.x       = element_text(margin = margin(t = 6), lineheight = 1.2),
      axis.text.y       = element_text(lineheight = 1.2),
      axis.title        = element_text(size = 13, color = sub_color),
      axis.title.y      = element_text(margin = margin(r = 10)),
      axis.title.x      = element_text(margin = margin(t = 10)),

      panel.grid.major.y = grid_y,
      panel.grid.major.x = grid_x,
      panel.grid.minor   = element_blank(),

      legend.position   = if (show_legend) "bottom" else "none",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(color = text_color, size = 13),
      legend.margin     = margin(t = 12),

      plot.margin = margin(10, 10, 10, 10)
    ) +
    coord_cartesian(clip = "off")
}

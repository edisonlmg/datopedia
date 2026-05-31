#===============================================================================
# social_line_chart.R  —  versión redes sociales
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

.resolver_color <- function(color) {
  if (color %in% names(.PALETA)) .PALETA[[color]] else color
}

.resolver_colores <- function(colores, n) {
  hex <- sapply(colores, .resolver_color)
  if (length(hex) < n) colorRampPalette(hex)(n) else hex[seq_len(n)]
}


social_line_chart <- function(
    x,
    y,
    group        = NULL,
    color        = "azul",
    colores      = NULL,
    title        = "",
    subtitle     = "",
    caption      = "",
    y_label      = "",
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
    fondo        = "beige"
) {
  bg_color   <- if (fondo == "beige") "#F6F5F0" else "white"
  grid_color <- if (fondo == "beige") "#d8d5cd" else "#e8e8e8"
  text_color <- "#2a2a2a"
  sub_color  <- "#555555"
  cap_color  <- "#888888"
  ref_color  <- "#555555"

  .mb_titulo <- function(s) (ceiling(nchar(s) / 44) - 1) * 16 + 5

  multi <- !is.null(group)
  df    <- data.frame(x = x, y = y,
                      group = if (multi) as.character(group) else if (is.null(series_name)) "Serie" else series_name)

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
    y_limits <- c(max(0, min(y) - y_pad), max(y) + y_pad)
  }
  if (is.null(y_breaks))  y_breaks  <- pretty(y_limits, n = 5)
  x_breaks_user <- !is.null(x_breaks)
  if (is.null(x_limits))  x_limits  <- range(x)
  if (is.null(x_breaks))  x_breaks  <- unique(x)

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

  p <- p +
    geom_line(aes(color = group, group = group), linewidth = 2,
              lineend = "round", linejoin = "round") +
    geom_point(aes(color = group), size = 4.5) +
    geom_point(color = bg_color, size = 2)

  if (show_labels) {
    p <- p + geom_text(
      aes(label = sprintf(label_format, y)),
      vjust = -1.5, color = text_color, family = "montserrat", fontface = "bold",
      size = 5, show.legend = FALSE
    )
  }

  legend_name <- if (multi) series_name else NULL
  p <- p +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_color_manual(name = legend_name, values = color_map)

  if (inherits(x, "Date")) {
    p <- p + if (x_breaks_user)
      scale_x_date(limits = x_limits, breaks = x_breaks, date_labels = "%b\n%Y")
    else
      scale_x_date(limits = x_limits, date_breaks = "1 month", date_labels = "%b\n%Y")
  } else if (inherits(x, c("POSIXct", "POSIXlt"))) {
    p <- p + if (x_breaks_user)
      scale_x_datetime(limits = x_limits, breaks = x_breaks, date_labels = "%b\n%Y")
    else
      scale_x_datetime(limits = x_limits, date_breaks = "1 month", date_labels = "%b\n%Y")
  } else {
    p <- p + scale_x_continuous(limits = x_limits, breaks = x_breaks)
  }

  p +
    labs(title = title, subtitle = subtitle, caption = caption,
         y = y_label, x = x_label) +
    theme_minimal() +
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
        family = "montserrat", size = 18, color = cap_color,
        halign = 0, lineheight = 1.0, margin = margin(t = 12)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      axis.line         = element_line(color = "#bbbbbb", linewidth = 0.4),
      axis.ticks        = element_blank(),
      axis.text         = element_text(color = sub_color, size = 13, lineheight = 1.2),
      axis.text.x       = element_text(
        lineheight = 1.2,
        angle      = if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) 45 else 0,
        hjust      = if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) 1  else 0.5
      ),
      axis.title        = element_text(size = 13, color = sub_color),
      axis.title.y      = element_text(margin = margin(r = 10)),

      panel.grid.major.y = element_line(color = grid_color, linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),

      legend.position   = if (show_legend && multi) "bottom" else "none",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(color = text_color, size = 13),
      legend.margin     = margin(t = 12),

      plot.margin = margin(10, 10, 10, 10)
    )
}

#===============================================================================
# social_bar_chart.R  —  versión redes sociales
# Formato vertical 4:5  →  ggsave(..., width = 8, height = 10, dpi = 135)
#                           = 1080 × 1350 px
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

.resolver_color <- function(color) {
  if (color %in% names(.PALETA)) .PALETA[[color]] else color
}


social_bar_chart <- function(
    x,
    y,
    color          = "azul",
    orientation    = "vertical",
    title          = "",
    subtitle       = "",
    caption        = "",
    y_label        = "",
    x_label        = "",
    y_limits       = NULL,
    y_breaks       = NULL,
    hlines         = NULL,
    vlines         = NULL,
    show_labels    = TRUE,
    label_decimals = 0,
    label_big_mark = ",",
    max_x          = 36,     # máx. caracteres en etiquetas eje x (0 = sin truncar)
    fondo          = "blanco"
) {
  bg_color    <- if (fondo == "beige") "#F6F5F0" else "white"
  grid_color  <- if (fondo == "beige") "#d8d5cd" else "#e8e8e8"
  text_color  <- "#2a2a2a"
  sub_color   <- "#555555"
  cap_color   <- "#888888"
  ref_color   <- "#555555"
  bar_color   <- .resolver_color(color)

  # Margen inferior del título: compensa líneas adicionales de wrap
  # (~28 chars/línea a 28pt en 8 pulgadas; 36pt por línea extra)
  .mb_titulo <- function(s) (ceiling(nchar(s) / 44) - 1) * 22 + 5

  df   <- data.frame(x = as.character(x), y = as.numeric(y))
  df$x <- factor(df$x, levels = df$x)

  .truncar <- function(s, n) ifelse(nchar(s) > n, paste0(substr(s, 1, n - 1), "…"), s)

  if (max_x > 0) levels(df$x) <- .truncar(levels(df$x), max_x)

  if (is.null(y_limits)) {
    y_pad    <- diff(range(df$y)) * 0.18
    y_limits <- c(min(0, min(df$y)), max(df$y) + y_pad)
  }
  if (is.null(y_breaks)) y_breaks <- pretty(y_limits, n = 5)

  fmt_label <- function(v) {
    trimws(format(round(v, label_decimals), nsmall = label_decimals,
                  big.mark = label_big_mark, scientific = FALSE))
  }

  if (orientation == "vertical") {

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_col(fill = bar_color, width = 0.65)

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

    if (show_labels) {
      p <- p + geom_text(
        aes(label = fmt_label(y)),
        vjust = -0.5, color = text_color, family = "montserrat",
        fontface = "bold", size = 5.5
      )
    }

    p <- p +
      scale_y_continuous(limits = y_limits, breaks = y_breaks) +
      scale_x_discrete()

  } else {

    p <- ggplot(df, aes(x = y, y = factor(x, levels = rev(levels(x))))) +
      geom_col(fill = bar_color, width = 0.65)

    if (!is.null(vlines) && nrow(vlines) > 0) {
      for (i in seq_len(nrow(vlines))) {
        vl_linetype <- if ("linetype" %in% names(vlines)) vlines$linetype[i] else "dashed"
        vl_label    <- if ("label"    %in% names(vlines)) vlines$label[i]    else NA
        p <- p + geom_vline(xintercept = vlines$xintercept[i], color = ref_color,
                            linewidth = 0.8, linetype = vl_linetype)
        if (!is.na(vl_label) && vl_label != "") {
          p <- p + annotate("text", x = vlines$xintercept[i], y = nrow(df) + 0.6,
                            label = vl_label, color = ref_color, family = "montserrat",
                            size = 5.5, hjust = -0.1, fontface = "bold")
        }
      }
    }

    if (show_labels) {
      p <- p + geom_text(
        aes(label = fmt_label(y)),
        hjust = -0.2, color = text_color, family = "montserrat",
        fontface = "bold", size = 5.5
      )
    }

    p <- p + scale_x_continuous(limits = y_limits, breaks = y_breaks)

    temp   <- x_label
    x_label <- y_label
    y_label <- temp
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

      panel.grid.major.y = element_line(color = grid_color, linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),

      legend.position = "none",
      plot.margin     = margin(10, 10, 10, 10)
    ) +
    coord_cartesian(clip = "off")
}

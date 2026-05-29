#===============================================================================
# donut_chart.R
#
# Objetivo: define función reutilizable para generar gráficos de donut con
#           estilos visuales consistentes (temas "dark" y "light").
#
# Funciones:
#   - donut_chart()  gráfico de donut con etiquetas y leyenda
#===============================================================================


library(ggplot2)  # para gráficos
library(ggtext)   # para renderizar markdown en títulos y captions
library(showtext) # para cargar fuentes externas en ggplot2


font_add_google("Montserrat", "montserrat")
showtext_auto()


#===============================================================================
# grafico de donut
#===============================================================================


donut_chart <- function(
    labels,
    values,
    title          = "",
    subtitle       = "",
    caption        = "",
    show_labels    = TRUE,       # mostrar porcentajes sobre los segmentos
    show_legend    = TRUE,
    label_decimals = 1,          # decimales en los porcentajes
    hole_size      = 0.5,        # tamaño del agujero central (0 = pie, 1 = sin relleno)
    center_label   = NULL,       # texto en el centro del donut (NULL = sin texto)
    center_size    = 6,          # tamaño de fuente del texto central
    theme          = "dark"      # "dark" o "light"
) {

  # ── Paletas de color ──────────────────────────────────────────────────────
  if (theme == "dark") {
    bg_color    <- "#0a1128"
    text_color  <- "white"
    label_color <- "white"
    base_colors <- c("#00f2ff", "#ff007f", "#7000ff", "#00ff88", "#ffcc00")
  } else {
    bg_color    <- "white"
    text_color  <- "#1a1a2e"
    label_color <- "#1a1a2e"
    base_colors <- c("#0066cc", "#d9534f", "#5cb85c", "#f0ad4e", "#6f42c1")
  }

  # ── Construcción del data.frame ──────────────────────────────────────────
  df <- data.frame(
    label = as.character(labels),
    value = as.numeric(values)
  )
  df$label <- factor(df$label, levels = df$label)

  total      <- sum(df$value)
  df$pct     <- df$value / total * 100
  df$ymax    <- cumsum(df$pct)
  df$ymin    <- c(0, head(df$ymax, -1))
  df$y_mid   <- (df$ymax + df$ymin) / 2

  # Posición angular para las etiquetas (en coordenadas polares, x = 1 es el exterior)
  label_r <- 1 - hole_size / 2  # radio medio del anillo

  # ── Paleta dinámica ───────────────────────────────────────────────────────
  paleta    <- colorRampPalette(base_colors)(nrow(df))
  color_map <- setNames(paleta, levels(df$label))

  # ── Construcción del gráfico ──────────────────────────────────────────────
  p <- ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = hole_size, fill = label)) +
    geom_rect(color = bg_color, linewidth = 0.8) +
    scale_fill_manual(values = color_map, name = NULL) +
    coord_polar(theta = "y", start = 0) +
    xlim(0, 1.05)

  # ── Etiquetas de porcentaje ───────────────────────────────────────────────
  if (show_labels) {
    df$pct_label <- paste0(formatC(df$pct, digits = label_decimals, format = "f"), "%")

    # solo mostrar etiqueta si el segmento es suficientemente grande
    df$pct_label <- ifelse(df$pct >= 3, df$pct_label, "")

    p <- p + geom_text(
      data    = df,
      aes(x = label_r, y = y_mid, label = pct_label),
      color   = label_color,
      family  = "montserrat",
      fontface = "bold",
      size    = 4.5,
      inherit.aes = FALSE
    )
  }

  # ── Texto central ─────────────────────────────────────────────────────────
  if (!is.null(center_label)) {
    p <- p + annotate(
      "text",
      x        = 0,
      y        = 0,
      label    = center_label,
      color    = text_color,
      family   = "montserrat",
      fontface = "bold",
      size     = center_size
    )
  }

  # ── Títulos ───────────────────────────────────────────────────────────────
  p <- p + labs(title = title, subtitle = subtitle, caption = caption)

  # ── Tema ──────────────────────────────────────────────────────────────────
  p <- p +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      text             = element_text(family = "montserrat", color = text_color),

      plot.title          = element_text(hjust = 0.5, face = "bold", size = 22, margin = margin(b = 10)),
      plot.title.position = "plot",
      plot.subtitle       = element_markdown(hjust = 0.5, size = 12, margin = margin(b = 20)),
      plot.caption          = element_markdown(hjust = 0.5, size = 10, margin = margin(t = 20)),
      plot.caption.position = "plot",

      legend.position   = if (show_legend) "right" else "none",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(color = text_color, size = 12),
      legend.margin     = margin(l = 15),

      plot.margin = margin(30, 30, 30, 30)
    )

  return(p)
}


#===============================================================================
# Ejemplos de uso
#===============================================================================


donut_chart(
  labels       = c("Asia", "África", "Europa", "América", "Oceanía"),
  values       = c(4700, 1400, 750, 1050, 45),
  title        = "DISTRIBUCIÓN DE LA POBLACIÓN MUNDIAL",
  subtitle     = "Participación por continente · 2024",
  caption      = "Fuente: UN World Population Prospects | Elaborado con ggplot2",
  center_label = "7.9 B",
  theme        = "light"
)

#===============================================================================
# scatter_chart.R
#
# Objetivo: define función reutilizable para generar gráficos de dispersión con
#           estilos visuales consistentes (temas "dark" y "light").
#
# Funciones:
#   - scatter_chart()  gráfico de dispersión con agrupación, etiquetas y
#                      línea de tendencia opcionales
#===============================================================================


library(ggplot2)  # para gráficos
library(ggtext)   # para renderizar markdown en títulos y captions
library(showtext) # para cargar fuentes externas en ggplot2


font_add_google("Montserrat", "montserrat")
showtext_auto()


#===============================================================================
# grafico de dispersión
#===============================================================================


scatter_chart <- function(
    x,
    y,
    group        = NULL,       # vector de grupos para colorear series (NULL = una sola serie)
    label        = NULL,       # vector de etiquetas por punto (NULL = sin etiquetas)
    size         = NULL,       # vector numérico para escalar el tamaño de puntos (NULL = tamaño fijo)
    title        = "",
    subtitle     = "",
    caption      = "",
    y_label      = "Y",
    x_label      = "X",
    series_name  = "Serie",    # etiqueta en modo una sola serie; título de leyenda en multi-serie
    y_limits     = NULL,
    y_breaks     = NULL,
    x_limits     = NULL,
    x_breaks     = NULL,
    hlines       = NULL,
    vlines       = NULL,
    point_size   = 4,          # tamaño fijo de los puntos cuando size = NULL
    trend_line   = FALSE,      # agregar línea de tendencia
    trend_method = "lm",       # método de la línea de tendencia: "lm" o "loess"
    show_legend  = TRUE,
    theme        = "dark"      # "dark" o "light"
) {

  # ── Paletas de color ──────────────────────────────────────────────────────
  if (theme == "dark") {
    bg_color    <- "#0a1128"
    grid_color  <- "#1f2a48"
    text_color  <- "white"
    ref_color   <- "white"
    label_color <- "white"
    base_colors <- c("#00f2ff", "#ff007f", "#7000ff", "#00ff88", "#ffcc00")
  } else {
    bg_color    <- "white"
    grid_color  <- "#dce3ed"
    text_color  <- "#1a1a2e"
    ref_color   <- "#555555"
    label_color <- "#1a1a2e"
    base_colors <- c("#0066cc", "#d9534f", "#5cb85c", "#f0ad4e", "#6f42c1")
  }

  # ── Construcción del data.frame ──────────────────────────────────────────
  multi <- !is.null(group)

  df <- data.frame(
    x     = x,
    y     = y,
    group = if (multi) as.character(group) else series_name
  )
  if (!is.null(label)) df$label <- as.character(label)
  if (!is.null(size))  df$size  <- as.numeric(size)

  grupos    <- unique(df$group)
  n_grupos  <- length(grupos)
  paleta    <- colorRampPalette(base_colors)(n_grupos)
  color_map <- setNames(paleta, grupos)

  # ── Límites y breaks Y ───────────────────────────────────────────────────
  if (is.null(y_limits)) {
    y_pad    <- diff(range(y)) * 0.2
    y_limits <- c(min(y) - y_pad, max(y) + y_pad)
  }
  if (is.null(y_breaks)) {
    y_breaks <- pretty(y_limits, n = 6)
  }

  # ── Límites y breaks X ───────────────────────────────────────────────────
  if (is.null(x_limits)) {
    x_pad    <- diff(range(x)) * 0.2
    x_limits <- c(min(x) - x_pad, max(x) + x_pad)
  }
  if (is.null(x_breaks)) {
    x_breaks <- pretty(x_limits, n = 6)
  }

  # ── Base del plot ─────────────────────────────────────────────────────────
  p <- ggplot(df, aes(x = x, y = y))

  # ── Líneas verticales ────────────────────────────────────────────────────
  if (!is.null(vlines) && nrow(vlines) > 0) {
    for (i in seq_len(nrow(vlines))) {
      vl_linetype <- if ("linetype" %in% names(vlines)) vlines$linetype[i] else "dashed"
      vl_label    <- if ("label"    %in% names(vlines)) vlines$label[i]    else NA

      p <- p + geom_vline(
        xintercept = vlines$xintercept[i],
        color      = ref_color,
        linewidth  = 0.8,
        linetype   = vl_linetype
      )

      if (!is.na(vl_label) && vl_label != "") {
        p <- p + annotate(
          "text",
          x        = vlines$xintercept[i],
          y        = y_limits[2] * 0.97,
          label    = vl_label,
          color    = ref_color,
          family   = "montserrat",
          size     = 3.5,
          hjust    = -0.1,
          fontface = "bold"
        )
      }
    }
  }

  # ── Líneas horizontales ──────────────────────────────────────────────────
  if (!is.null(hlines) && nrow(hlines) > 0) {
    hline_types <- setNames(
      if ("linetype" %in% names(hlines)) hlines$linetype else rep("dashed", nrow(hlines)),
      hlines$label
    )

    for (i in seq_len(nrow(hlines))) {
      p <- p + geom_hline(
        aes(yintercept = !!hlines$yintercept[i], linetype = !!hlines$label[i]),
        color     = ref_color,
        linewidth = 1
      )
    }

    p <- p + scale_linetype_manual(name = NULL, values = hline_types)
  }

  # ── Línea de tendencia ───────────────────────────────────────────────────
  if (trend_line) {
    p <- p + geom_smooth(
      aes(color = group, group = group),
      method    = trend_method,
      formula   = y ~ x,
      se        = FALSE,
      linewidth = 1,
      linetype  = "solid",
      alpha     = 0.7,
      show.legend = FALSE
    )
  }

  # ── Puntos ────────────────────────────────────────────────────────────────
  if (!is.null(size)) {
    p <- p +
      geom_point(aes(color = group, size = size), alpha = 0.85) +
      geom_point(aes(size  = size * 0.25), color = "white", alpha = 0.6) +
      scale_size_continuous(range = c(3, 14), guide = "none")
  } else {
    p <- p +
      geom_point(aes(color = group), size = point_size, alpha = 0.85) +
      geom_point(color = "white", size = point_size * 0.35, alpha = 0.6)
  }

  # ── Etiquetas por punto ───────────────────────────────────────────────────
  if (!is.null(label)) {
    p <- p + geom_text(
      aes(label = label, color = group),
      vjust         = -1.2,
      family        = "montserrat",
      fontface      = "bold",
      size          = 3.8,
      check_overlap = TRUE,
      show.legend   = FALSE
    )
  }

  # ── Escalas ───────────────────────────────────────────────────────────────
  legend_name <- if (multi) series_name else NULL

  p <- p +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    scale_color_manual(name = legend_name, values = color_map)

  # ── Títulos ───────────────────────────────────────────────────────────────
  p <- p + labs(title = title, subtitle = subtitle, caption = caption, y = y_label, x = x_label)

  # ── Tema ──────────────────────────────────────────────────────────────────
  p <- p +
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
      axis.text.x       = element_text(color = text_color, size = 12, margin = margin(t = 5)),
      axis.title.y      = element_text(size = 12, margin = margin(r = 10)),
      axis.title.x      = element_text(size = 12, margin = margin(t = 10)),

      panel.grid.major  = element_line(color = grid_color, linewidth = 0.5),
      panel.grid.minor  = element_blank(),

      legend.position   = if (show_legend) "bottom" else "none",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(color = text_color, size = 12),
      legend.margin     = margin(t = 15),

      plot.margin = margin(30, 30, 30, 30)
    ) +
    coord_cartesian(clip = "off")

  return(p)
}


#===============================================================================
# Ejemplos de uso
#===============================================================================


# Una sola serie con línea de tendencia
scatter_chart(
  x            = c(6.0, 5.8, 5.5, 4.9, 4.3, 3.7, 3.1, 2.6, 2.3),
  y            = c(2.1, 2.4, 3.0, 4.2, 5.8, 7.9, 9.4, 11.5, 12.8),
  label        = c("1960", "1970", "1975", "1980", "1985", "1990", "2000", "2010", "2020"),
  title        = "FERTILITY vs. CHILD MORTALITY (1960-2020)",
  subtitle     = "As fertility falls, child survival improves",
  caption      = "Source: World Bank | Created with ggplot2",
  x_label      = "Fertility Rate (children per woman)",
  y_label      = "Child Mortality (per 1,000 births)",
  series_name  = "World Average",
  trend_line   = TRUE,
  trend_method = "lm",
  theme        = "dark"
)


# Múltiples series agrupadas
scatter_chart(
  x     = c(2.1, 3.4, 5.0, 1.8, 4.2, 6.3, 3.0, 4.8, 2.5),
  y     = c(8.5, 6.2, 3.1, 9.0, 5.4, 2.0, 7.1, 4.3, 8.8),
  group = c("América", "América", "América", "África", "África", "África", "Asia", "Asia", "Asia"),
  label = c("BRA", "COL", "USA", "NGA", "KEN", "ZAF", "CHN", "IND", "JPN"),
  title        = "FERTILITY vs. CHILD MORTALITY BY REGION",
  subtitle     = "Selección de países, 2020",
  caption      = "Source: World Bank | Created with ggplot2",
  x_label      = "Fertility Rate",
  y_label      = "Child Mortality (per 1,000 births)",
  series_name  = "Región",
  trend_line   = TRUE,
  theme        = "dark"
)

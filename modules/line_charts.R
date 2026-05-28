#===============================================================================
# line_charts.R
#
# Objetivo: define función reutilizable para generar gráficos de líneas con
#           estilos visuales consistentes (temas "dark" y "light").
#
# Funciones:
#   - line_chart()  gráfico de líneas con etiquetas y líneas de referencia
#===============================================================================


library(ggplot2)  # para gráficos
library(ggtext)   # para renderizar markdown en títulos y captions
library(showtext) # para cargar fuentes externas en ggplot2


font_add_google("Montserrat", "montserrat")
showtext_auto()


#===============================================================================
# grafico de lineas 
#===============================================================================


line_chart <- function(
    x,
    y,
    group        = NULL,       # vector de grupos para múltiples series (NULL = una sola serie)
    title        = "",
    subtitle     = "",
    caption      = "",
    y_label      = "Y",
    x_label      = NULL,
    series_name  = "Serie",    # etiqueta en modo una sola serie; título de leyenda en multi-serie
    y_limits     = NULL,
    y_breaks     = NULL,
    x_limits     = NULL,
    x_breaks     = NULL,
    hlines       = NULL,
    vlines       = NULL,
    show_labels  = TRUE,
    label_format = "%.1f",
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
  
  if (multi) {
    df <- data.frame(x = x, y = y, group = as.character(group))
  } else {
    df <- data.frame(x = x, y = y, group = if (is.null(series_name)) "Serie" else series_name)
  }
  
  grupos    <- unique(df$group)
  n_grupos  <- length(grupos)
  paleta    <- colorRampPalette(base_colors)(n_grupos)
  color_map <- setNames(paleta, grupos)
  
  # ── Límites y breaks Y ───────────────────────────────────────────────────
  if (is.null(y_limits)) {
    y_pad    <- diff(range(y)) * 0.2
    y_limits <- c(max(0, min(y) - y_pad), max(y) + y_pad)
  }
  if (is.null(y_breaks)) {
    y_breaks <- pretty(y_limits, n = 6)
  }
  
  # ── Límites y breaks X ───────────────────────────────────────────────────
  x_breaks_user <- !is.null(x_breaks)
  
  if (is.null(x_limits)) {
    x_limits <- range(x)
  }
  if (is.null(x_breaks)) {
    x_breaks <- unique(x)
  }
  
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
  
  # ── Series ────────────────────────────────────────────────────────────────
  p <- p +
    geom_line(aes(color = group, group = group), linewidth = 1.5,
              lineend = "round", linejoin = "round") +
    geom_point(aes(color = group), size = 4) +
    geom_point(color = "white", size = 1.5)   # anillo interior
  
  # ── Etiquetas sobre puntos ────────────────────────────────────────────────
  if (show_labels) {
    p <- p + geom_text(
      aes(label = sprintf(label_format, y), color = group),
      vjust       = -1.5,
      family      = "montserrat",
      fontface    = "bold",
      size        = 4.5,
      show.legend = FALSE
    )
  }
  
  # ── Escalas ───────────────────────────────────────────────────────────────
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
      axis.text.x       = element_text(color = text_color, size = 12, margin = margin(t = 5),
                                       angle = if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) 45 else 0,
                                       hjust = if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) 1 else 0.5),
      axis.title.y      = element_text(size = 12, margin = margin(r = 10)),
      
      panel.grid.major  = element_line(color = grid_color, linewidth = 0.5),
      panel.grid.minor  = element_blank(),
      
      legend.position   = if (show_legend) "bottom" else "none",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(color = text_color, size = 12),
      legend.margin     = margin(t = 15),
      
      plot.margin = margin(30, 30, 30, 30)
    )
  
  return(p)
}

#===============================================================================
# Ejemplos de uso
#===============================================================================

line_chart(
  x           = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, 2025),
  y           = c(5.0, 4.9, 4.7, 3.7, 3.4, 2.7, 2.5, 2.3, 2.3),
  title       = "GLOBAL FERTILITY RATE COLLAPSE (1950-2025)",
  subtitle    = "Average children per woman worldwide",
  caption     = "Source: UN World Population Prospects | Created with ggplot2",
  y_label     = "Fertility Rate",
  series_name = "Global Fertility Rate",
  y_limits    = c(0, 6),
  y_breaks    = seq(0, 6, 1),
  hlines      = data.frame(
    yintercept = 3.0,
    label      = "Replacement Level",
    linetype   = "dashed"
  ),
  theme = "dark"
)




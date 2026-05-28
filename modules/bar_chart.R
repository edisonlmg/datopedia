#===============================================================================
# bar_chart.R
#
# Objetivo: define función reutilizable para generar gráficos de barras con
#           estilos visuales consistentes (temas "dark" y "light").
#
# Funciones:
#   - bar_chart()   gráfico de barras vertical u horizontal
#===============================================================================


library(ggplot2)  # para gráficos
library(ggtext)   # para renderizar markdown en títulos y captions
library(showtext) # para cargar fuentes externas en ggplot2


font_add_google("Montserrat", "montserrat")
showtext_auto() # activa la fuente en todos los dispositivos gráficos automáticamente


#===============================================================================
# grafico de barras 
#===============================================================================


bar_chart <- function(
    x,
    y,
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
    label_decimals = 0,          # decimales en las etiquetas de valor
    label_big_mark = ",",        # separador de miles en las etiquetas de valor
    theme          = "dark"      # "dark" o "light"
) {
  
  df <- data.frame(x = as.character(x), y = as.numeric(y))
  
  # Mantener el orden original de las categorías
  df$x <- factor(df$x, levels = df$x)
  
  # ── Paletas de color dinámicas ───────────────────────────────────────────
  if (theme == "dark") {
    bg_color     <- "#0a1128"
    grid_color   <- "#1f2a48"
    text_color   <- "white"
    ref_color    <- "white"
    label_color  <- "white"
    base_colors  <- c("#00f2ff", "#ff007f", "#7000ff", "#00ff88", "#ffcc00")
  } else {
    bg_color     <- "white"
    grid_color   <- "#dce3ed"
    text_color   <- "#1a1a2e"
    ref_color    <- "#555555"
    label_color  <- "#1a1a2e"
    base_colors  <- c("#0066cc", "#d9534f", "#5cb85c", "#f0ad4e", "#6f42c1")
  }
  
  # interpola los 5 colores base para generar una paleta del tamaño exacto del dataset
  bar_palette <- colorRampPalette(base_colors)(nrow(df))
  
  # ── Límites y breaks del eje Y ──────────────────────────────
  if (is.null(y_limits)) {
    y_pad    <- diff(range(df$y)) * 0.15
    y_limits <- c(min(0, min(df$y)), max(df$y) + y_pad)
  }
  if (is.null(y_breaks)) {
    y_breaks <- pretty(y_limits, n = 6)
  }
  
  # ── Construcción del Plot ──────────────────────────────
  
  if (orientation == "vertical") {
    
    p <- ggplot(df, aes(x = x, y = y, fill = x)) +
      geom_col(width = 0.7, show.legend = FALSE)
    
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
    
    # Etiquetas VERTICALES con formato dinámico seguro
    if (show_labels) {
      p <- p + geom_text(
        aes(label = trimws(format(round(y, label_decimals), nsmall = label_decimals, big.mark = label_big_mark, scientific = FALSE))),
        vjust = -0.5, color = label_color, family = "montserrat",
        fontface = "bold", size = 4.5
      )
    }
    
    p <- p +
      scale_y_continuous(limits = y_limits, breaks = y_breaks) +
      scale_x_discrete()
    
  } else if (orientation == "horizontal") {
    
    p <- ggplot(df, aes(x = y, y = factor(x, levels = rev(levels(x))), fill = x)) +
      geom_col(width = 0.7, show.legend = FALSE)
    
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
            y        = nrow(df) + 0.6,
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
    
    # Etiquetas HORIZONTALES con formato dinámico seguro
    if (show_labels) {
      p <- p + geom_text(
        aes(label = trimws(format(round(y, label_decimals), nsmall = label_decimals, big.mark = label_big_mark, scientific = FALSE))),
        hjust = -0.2, color = label_color, family = "montserrat",
        fontface = "bold", size = 4.5
      )
    }
    
    p <- p + scale_x_continuous(limits = y_limits, breaks = y_breaks)
    
    # en orientación horizontal los ejes visuales están invertidos respecto a los estéticos
    temp_label <- x_label
    x_label    <- y_label
    y_label    <- temp_label
  }
  
  p <- p + scale_fill_manual(values = bar_palette)
  
  p <- p + labs(title = title, subtitle = subtitle, caption = caption, y = y_label, x = x_label)
  
  p <- p +
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
    coord_cartesian(clip = "off") # evita que las etiquetas se corten en los bordes del panel
  
  return(p)
}


#===============================================================================
# Ejemplos de uso
#===============================================================================


bar_chart(
  x           = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, 2025),
  y           = c(5.0, 4.9, 4.7, 3.7, 3.4, 2.7, 2.5, 2.3, 2.3),
  orientation = "vertical",
  title       = "GLOBAL FERTILITY RATE COLLAPSE (1950-2025)",
  subtitle    = "Average children per woman worldwide",
  caption     = "Source: UN World Population Prospects | Created with ggplot2",
  y_label     = "Fertility Rate",
  y_limits    = c(0, 6),
  y_breaks    = seq(0, 6, 1),
  hlines      = data.frame(
    yintercept = 3.0,
    label      = "Replacement Level",
    linetype   = "dashed"
  ),
  theme = "light"
)



#===============================================================================
# graphics_functions.R
#
# Objetivo: define funciones reutilizables para generar gráficos y tablas con
#           estilos visuales consistentes (temas "dark" y "light").
#
# Funciones:
#   - line_chart()  gráfico de líneas con etiquetas y líneas de referencia
#   - bar_chart()   gráfico de barras vertical u horizontal
#   - gt_table()    tabla estilizada con formato numérico configurable
#===============================================================================


if (!require("pacman")) {install.packages("pacman", dependencies = TRUE)}
pacman::p_load(
  ggplot2,  # para gráficos
  ggtext,   # para renderizar markdown en títulos y captions
  showtext, # para cargar fuentes externas en ggplot2
  webshot2, # requerido por gtsave() para exportar tablas gt a PNG
  gt        # para tablas estilizadas
)


font_add_google("Montserrat", "montserrat")
showtext_auto() # activa la fuente en todos los dispositivos gráficos automáticamente


#===============================================================================
# grafico de lineas 
#===============================================================================


line_chart <- function(
    x,
    y,
    title        = "",
    subtitle     = "",
    caption      = "",
    y_label      = "Y",
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
    theme        = "dark"    # "dark" o "light"
) {
  
  # ── Paletas de color ──────────────────────────────────────────────────────
  if (theme == "dark") {
    bg_color     <- "#0a1128"
    grid_color   <- "#1f2a48"
    line_color   <- "#00f2ff"
    text_color   <- "white"
    ref_color    <- "white"
    label_color  <- "white"
  } else {
    bg_color     <- "white"
    grid_color   <- "#dce3ed"
    line_color   <- "#0066cc"
    text_color   <- "#1a1a2e"
    ref_color    <- "#555555"
    label_color  <- "#1a1a2e"
  }
  
  df <- data.frame(x = x, y = y)
  
  # ── Límites y breaks Y ───────────────────────────────────────────────────
  if (is.null(y_limits)) {
    y_pad    <- diff(range(y)) * 0.2
    y_limits <- c(max(0, min(y) - y_pad), max(y) + y_pad)
  }
  if (is.null(y_breaks)) {
    y_breaks <- pretty(y_limits, n = 6)
  }
  
  # ── Límites y breaks X ───────────────────────────────────────────────────
  if (is.null(x_limits)) {
    x_limits <- range(x)
  }
  if (is.null(x_breaks)) {
    x_breaks <- x
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
  
  # ── Serie principal ───────────────────────────────────────────────────────
  p <- p +
    geom_line(aes(color = series_name), linewidth = 1.5,
              lineend = "round", linejoin = "round") +
    geom_point(data = df, aes(x = x, y = y),
               color = line_color, size = 4) +
    geom_point(data = df, aes(x = x, y = y),        # punto blanco encima crea efecto de anillo
               color = "white", size = 1.5)
  
  # ── Etiquetas sobre puntos ────────────────────────────────────────────────
  if (show_labels) {
    p <- p + geom_text(
      data     = df,
      aes(x = x, y = y, label = sprintf(label_format, y)),
      vjust    = -1.5,
      color    = label_color,
      family   = "montserrat",
      fontface = "bold",
      size     = 4.5
    )
  }
  
  # ── Escalas ───────────────────────────────────────────────────────────────
  p <- p +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    scale_color_manual(name = NULL, values = setNames(line_color, series_name))
  
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
      
      panel.grid.major  = element_line(color = grid_color, linewidth = 0.5),
      panel.grid.minor  = element_blank(),
      
      legend.position   = "bottom",
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(color = text_color, size = 12),
      legend.margin     = margin(t = 15),
      
      plot.margin = margin(30, 30, 30, 30)
    )
  
  return(p)
}


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
  
  # ── Validación de máximo 20 barras ───────────────────────────────────────
  df <- data.frame(x = as.character(x), y = as.numeric(y))
  if (nrow(df) > 20) {
    warning("El gráfico está limitado a un máximo de 20 barras. Se truncarán los datos.")
    df <- head(df, 20)
  }
  
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
      
      plot.title    = element_text(hjust = 0.5, face = "bold", size = 22, margin = margin(b = 10)),
      plot.subtitle = element_markdown(hjust = 0.5, size = 18, margin = margin(b = 20)),
      plot.caption  = element_markdown(hjust = 0.5, size = 16, margin = margin(t = 20)),
      
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
# tabla con gt
#===============================================================================


gt_table <- function(
    data,
    title          = "",
    subtitle       = "",
    caption        = "",
    cols_decimales = NULL,  # named vector c("col" = n_decimales); resto: 0 decimales
    dec_mark       = ".",   # separador decimal
    big_mark       = ",",   # separador de miles
    theme          = "light" # "dark" o "light"
) {

  # ── Paletas de color ──────────────────────────────────────────────────────
  if (theme == "dark") {
    bg_color     <- "#0a1128"
    header_bg    <- "#1f2a48"
    stripe_color <- "#0d1830"
    accent_color <- "#00f2ff"
    text_color   <- "white"
    border_color <- "#1f2a48"
  } else {
    bg_color     <- "white"
    header_bg    <- "#dce3ed"
    stripe_color <- "#f5f7fb"
    accent_color <- "#0066cc"
    text_color   <- "#1a1a2e"
    border_color <- "#dce3ed"
  }

  tbl <- gt(data)

  # ── Título y subtítulo ────────────────────────────────────────────────────
  if (nchar(title) > 0 || nchar(subtitle) > 0) {
    tbl <- tbl |>
      tab_header(
        title    = md(paste0("**", title, "**")),
        subtitle = md(subtitle)
      )
  }

  # ── Caption ───────────────────────────────────────────────────────────────
  if (nchar(caption) > 0) {
    tbl <- tbl |> tab_source_note(source_note = md(caption))
  }

  # ── Formato de columnas numéricas ─────────────────────────────────────────
  num_cols <- names(data)[sapply(data, is.numeric)]
  if (length(num_cols) > 0) {
    tbl <- tbl |> fmt_integer(columns = all_of(num_cols), sep_mark = big_mark)
  }

  # reduce2 aplica fmt_number una vez por columna para permitir distintos decimales por columna
  if (!is.null(cols_decimales)) {
    tbl <- reduce2(
      names(cols_decimales),
      cols_decimales,
      function(t, col, dec) {
        fmt_number(t, columns = all_of(col), decimals = dec, dec_mark = dec_mark, sep_mark = big_mark)
      },
      .init = tbl
    )
  }

  # ── Fuente Montserrat ─────────────────────────────────────────────────────
  tbl <- tbl |>
    opt_table_font(font = list(google_font("Montserrat"), default_fonts()))

  # ── Opciones generales ────────────────────────────────────────────────────
  tbl <- tbl |>
    tab_options(
      table.background.color               = bg_color,
      table.border.top.color               = border_color,
      table.border.bottom.color            = border_color,
      table.font.size                      = px(13),

      heading.background.color             = bg_color,
      heading.border.bottom.color          = accent_color,
      heading.border.bottom.width          = px(2),
      heading.title.font.size              = px(18),
      heading.subtitle.font.size           = px(13),

      column_labels.background.color       = header_bg,
      column_labels.border.top.color       = border_color,
      column_labels.border.bottom.color    = accent_color,
      column_labels.border.bottom.width    = px(2),
      column_labels.font.weight            = "bold",
      column_labels.padding                = px(10),
      column_labels.vlines.color           = "transparent",

      row.striping.include_table_body      = TRUE,
      row.striping.background_color        = stripe_color,
      data_row.padding                     = px(8),

      table_body.hlines.color              = border_color,
      table_body.vlines.color              = "transparent",

      source_notes.background.color        = bg_color,
      source_notes.font.size               = px(11)
    )

  # ── Estilos de texto ──────────────────────────────────────────────────────
  tbl <- tbl |>
    tab_style(
      style     = cell_text(color = text_color),
      locations = cells_body()
    ) |>
    tab_style(
      style     = cell_text(color = accent_color, weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style     = cell_text(color = text_color, align = "center"),
      locations = cells_title(groups = c("title", "subtitle"))
    ) |>
    tab_style(
      style     = cell_text(color = text_color, align = "center"),
      locations = cells_source_notes()
    )

  return(tbl)
}


# ── Ejemplos de uso ───────────────────────────────────────────────────────

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


df_ejemplo <- data.frame(
  Año               = c(1950, 1970, 1990, 2010, 2025),
  `Tasa Fertilidad` = c(5.0, 4.7, 3.4, 2.5, 2.3),
  `Población (M)`   = c(2536, 3700, 5327, 6896, 8100),
  check.names = FALSE
)


gt_table(
  data           = df_ejemplo,
  title          = "EVOLUCIÓN DE LA TASA DE FERTILIDAD GLOBAL",
  subtitle       = "Hijos promedio por mujer a nivel mundial",
  caption        = "Fuente: UN World Population Prospects",
  cols_decimales = c("Tasa Fertilidad" = 1),
  theme          = "light"
)


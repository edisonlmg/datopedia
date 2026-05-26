#===============================================================================
# gt_table.R
#
# Objetivo: define función reutilizable para generar tablas estilizadas con
#           formato numérico configurable (temas "dark" y "light").
#
# Funciones:
#   - gt_table()    tabla estilizada con formato numérico configurable
#===============================================================================


library(purrr)    # para reduce2()
library(gt)       # para tablas estilizadas
library(webshot2) # requerido por gtsave() para exportar tablas gt a PNG


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


#===============================================================================
# Ejemplos de uso
#===============================================================================



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


#===============================================================================
# social_choropleth_map.R  —  versión redes sociales
# Formato vertical 4:5  →  ggsave(..., width = 8, height = 10, dpi = 135)
#===============================================================================

library(dplyr)
library(ggplot2)
library(ggtext)
library(showtext)
library(sf)
library(geodata)
library(terra)
library(stringi)

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

.norm_geo <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII") |> toupper() |> trimws()
}

.build_label <- function(nombre, valor_fmt, etiqueta) {
  switch(etiqueta,
    ambos  = paste0(nombre, "\n", valor_fmt),
    nombre = nombre,
    valor  = valor_fmt
  )
}


.mb_titulo <- function(s) (ceiling(nchar(s) / 28) - 1) * 22 + 5

social_mapa_departamentos <- function(
    data,
    value_col,
    color_alto        = "azul",
    title             = "",
    subtitle          = "",
    caption           = "",
    label_format      = "%.1f",
    mostrar_etiquetas = TRUE,
    etiqueta          = "ambos",
    fondo             = "beige"
) {
  bg_color     <- if (fondo == "beige") "#F6F5F0" else "white"
  low_color    <- if (fondo == "beige") "#dedad2" else "#e8e8e8"
  high_color   <- .resolver_color(color_alto)
  na_color     <- if (fondo == "beige") "#c8c4bc" else "#d0d0d0"
  border_color <- bg_color
  text_color   <- "#2a2a2a"
  sub_color    <- "#555555"
  cap_color    <- "#888888"
  label_color  <- "#2a2a2a"

  peru_sf <- geodata::gadm("PER", level = 1, path = tempdir()) |>
    sf::st_as_sf() |>
    mutate(KEY = .norm_geo(NAME_1))

  map_df <- peru_sf |>
    left_join(mutate(data, KEY = .norm_geo(DEPARTAMENTO)), by = "KEY")

  label_layers <- NULL
  if (mostrar_etiquetas) {
    centroids <- map_df |>
      sf::st_centroid() |>
      mutate(
        lon         = sf::st_coordinates(geometry)[, 1],
        lat         = sf::st_coordinates(geometry)[, 2],
        name_label  = if_else(!is.na(.data[[value_col]]), NAME_1, NA_character_),
        value_label = if_else(!is.na(.data[[value_col]]),
                              sprintf(label_format, .data[[value_col]]), NA_character_)
      ) |>
      sf::st_drop_geometry() |>
      filter(!is.na(value_label))

    label_layers <- list(
      if (etiqueta %in% c("ambos", "nombre"))
        geom_text(data = centroids, aes(x = lon, y = lat, label = name_label),
                  color = label_color, family = "montserrat",
                  size = 3.75, vjust = -0.2, lineheight = 0.9),
      if (etiqueta %in% c("ambos", "valor"))
        geom_text(data = centroids, aes(x = lon, y = lat, label = value_label),
                  color = label_color, family = "montserrat", fontface = "bold",
                  size = 7.5, vjust = 1.1, lineheight = 0.9)
    )
  }

  ggplot(map_df) +
    geom_sf(aes(fill = .data[[value_col]]), color = border_color, linewidth = 0.3) +
    label_layers +
    coord_sf(xlim = c(-81.5, -68.5), ylim = c(-18.5, -0.04), expand = FALSE) +
    scale_fill_gradient(low = low_color, high = high_color, na.value = na_color,
                        name = NULL, labels = \(x) sprintf(label_format, x)) +
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
        family = "montserrat", size = 18, color = cap_color,
        halign = 0, lineheight = 1.0, margin = margin(t = 12)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      legend.position  = "right",
      legend.text      = element_text(color = text_color, size = 13),

      plot.margin = margin(20, 10, 10, 10)
    )
}


social_mapa_lima_callao <- function(
    data,
    value_col,
    color_alto        = "azul",
    title             = "",
    subtitle          = "",
    caption           = "",
    label_format      = "%.1f",
    mostrar_etiquetas = TRUE,
    etiqueta          = "ambos",
    fondo             = "beige"
) {
  bg_color     <- if (fondo == "beige") "#F6F5F0" else "white"
  low_color    <- if (fondo == "beige") "#dedad2" else "#e8e8e8"
  high_color   <- .resolver_color(color_alto)
  na_color     <- if (fondo == "beige") "#c8c4bc" else "#d0d0d0"
  border_color <- bg_color
  text_color   <- "#2a2a2a"
  sub_color    <- "#555555"
  cap_color    <- "#888888"
  label_color  <- "#2a2a2a"

  peru_dist_sf <- geodata::gadm("PER", level = 3, path = tempdir()) |>
    sf::st_as_sf() |>
    filter(.norm_geo(NAME_2) %in% c("LIMA", "CALLAO")) |>
    mutate(KEY_PROV = .norm_geo(NAME_2), KEY_DIST = .norm_geo(NAME_3))

  map_df <- peru_dist_sf |>
    left_join(
      mutate(data, KEY_PROV = .norm_geo(PROVINCIA), KEY_DIST = .norm_geo(DISTRITO)),
      by = c("KEY_PROV", "KEY_DIST")
    )

  label_layers <- NULL
  if (mostrar_etiquetas) {
    centroids <- map_df |>
      sf::st_centroid() |>
      mutate(
        lon         = sf::st_coordinates(geometry)[, 1],
        lat         = sf::st_coordinates(geometry)[, 2],
        name_label  = if_else(!is.na(.data[[value_col]]), NAME_3, NA_character_),
        value_label = if_else(!is.na(.data[[value_col]]),
                              sprintf(label_format, .data[[value_col]]), NA_character_)
      ) |>
      sf::st_drop_geometry() |>
      filter(!is.na(value_label))

    label_layers <- list(
      if (etiqueta %in% c("ambos", "nombre"))
        geom_text(data = centroids, aes(x = lon, y = lat, label = name_label),
                  color = label_color, family = "montserrat",
                  size = 2.25, vjust = -0.2, lineheight = 0.9),
      if (etiqueta %in% c("ambos", "valor"))
        geom_text(data = centroids, aes(x = lon, y = lat, label = value_label),
                  color = label_color, family = "montserrat", fontface = "bold",
                  size = 4.5, vjust = 1.1, lineheight = 0.9)
    )
  }

  ggplot(map_df) +
    geom_sf(aes(fill = .data[[value_col]]), color = border_color, linewidth = 0.2) +
    label_layers +
    coord_sf(expand = FALSE) +
    scale_fill_gradient(low = low_color, high = high_color, na.value = na_color,
                        name = NULL, labels = \(x) sprintf(label_format, x)) +
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
        family = "montserrat", size = 18, color = cap_color,
        halign = 0, lineheight = 1.0, margin = margin(t = 12)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      legend.position = "right",
      legend.text     = element_text(color = text_color, size = 13),

      plot.margin = margin(20, 10, 10, 10)
    )
}

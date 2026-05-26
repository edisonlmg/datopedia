#===============================================================================
# choropleth_map.R
#
# Objetivo: define funciones reutilizables para generar mapas coropléticos de
#           Perú con estilos visuales consistentes (temas "dark" y "light").
#
# Funciones:
#   - mapa_departamentos()  mapa coroplético a nivel departamental
#   - mapa_lima_callao()    mapa coroplético a nivel distrital (Lima y Callao)
#===============================================================================


library(dplyr)    # para mutate, left_join, filter, if_else
library(ggplot2)  # para gráficos
library(ggtext)   # para renderizar markdown en títulos y captions
library(showtext) # para cargar fuentes externas en ggplot2
library(sf)       # para datos espaciales
library(geodata)  # para límites administrativos GADM
library(terra)    # requerido por geodata
library(stringi)  # para normalización de nombres (quitar tildes)


font_add_google("Montserrat", "montserrat")
showtext_auto() # activa la fuente en todos los dispositivos gráficos automáticamente


#===============================================================================
# mapas coropléticos
#===============================================================================

# Normaliza nombres para joins geográficos: quita tildes y pasa a mayúsculas
.norm_geo <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII") |> toupper() |> trimws()
}

# Tema base compartido para los mapas
.map_theme <- function(bg_color, text_color) {
  theme_void() +
    theme(
      plot.background  = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      text             = element_text(family = "montserrat", color = text_color),
      plot.title       = element_text(hjust = 0.5, face = "bold", size = 22, margin = margin(b = 10)),
      plot.subtitle    = element_markdown(hjust = 0.5, size = 12, margin = margin(b = 20)),
      plot.caption     = element_markdown(hjust = 0.5, size = 10, margin = margin(t = 20)),
      legend.position  = "right",
      legend.text      = element_text(color = text_color, size = 10),
      plot.margin      = margin(30, 30, 30, 30)
    )
}


mapa_departamentos <- function(
    data,
    value_col,
    title        = "",
    subtitle     = "",
    caption      = "",
    label_format = "%.1f",
    theme        = "dark"
) {
  if (theme == "dark") {
    bg_color     <- "#0a1128"
    low_color    <- "#1f2a48"
    high_color   <- "#00f2ff"
    na_color     <- "#151515"
    border_color <- "#0a1128"
    text_color   <- "white"
    label_color  <- "white"
  } else {
    bg_color     <- "white"
    low_color    <- "#dce3ed"
    high_color   <- "#0066cc"
    na_color     <- "#f0f0f0"
    border_color <- "white"
    text_color   <- "#1a1a2e"
    label_color  <- "#1a1a2e"
  }

  peru_sf <- geodata::gadm("PER", level = 1, path = tempdir()) |>
    sf::st_as_sf() |>
    mutate(KEY = .norm_geo(NAME_1))

  map_df <- peru_sf |>
    left_join(mutate(data, KEY = .norm_geo(DEPARTAMENTO)), by = "KEY")

  centroids <- map_df |>
    sf::st_centroid() |>
    mutate(
      lon   = sf::st_coordinates(geometry)[, 1],
      lat   = sf::st_coordinates(geometry)[, 2],
      label = if_else(
        !is.na(.data[[value_col]]),
        paste0(NAME_1, "\n", sprintf(label_format, .data[[value_col]])),
        NA_character_
      )
    ) |>
    sf::st_drop_geometry() |>
    filter(!is.na(label))

  ggplot(map_df) +
    geom_sf(aes(fill = .data[[value_col]]), color = border_color, linewidth = 0.3) +
    geom_text(
      data       = centroids,
      aes(x = lon, y = lat, label = label),
      color      = label_color,
      family     = "montserrat",
      fontface   = "bold",
      size       = 2.5,
      lineheight = 0.9
    ) +
    scale_fill_gradient(
      low      = low_color,
      high     = high_color,
      na.value = na_color,
      name     = NULL,
      labels   = function(x) sprintf(label_format, x)
    ) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    .map_theme(bg_color, text_color)
}


mapa_lima_callao <- function(
    data,
    value_col,
    title        = "",
    subtitle     = "",
    caption      = "",
    label_format = "%.1f",
    theme        = "dark"
) {
  if (theme == "dark") {
    bg_color     <- "#0a1128"
    low_color    <- "#1f2a48"
    high_color   <- "#00f2ff"
    na_color     <- "#151515"
    border_color <- "#0a1128"
    text_color   <- "white"
    label_color  <- "white"
  } else {
    bg_color     <- "white"
    low_color    <- "#dce3ed"
    high_color   <- "#0066cc"
    na_color     <- "#f0f0f0"
    border_color <- "white"
    text_color   <- "#1a1a2e"
    label_color  <- "#1a1a2e"
  }

  peru_dist_sf <- geodata::gadm("PER", level = 3, path = tempdir()) |>
    sf::st_as_sf() |>
    filter(.norm_geo(NAME_2) %in% c("LIMA", "CALLAO")) |>
    mutate(
      KEY_PROV = .norm_geo(NAME_2),
      KEY_DIST = .norm_geo(NAME_3)
    )

  map_df <- peru_dist_sf |>
    left_join(
      mutate(data, KEY_PROV = .norm_geo(PROVINCIA), KEY_DIST = .norm_geo(DISTRITO)),
      by = c("KEY_PROV", "KEY_DIST")
    )

  centroids <- map_df |>
    sf::st_centroid() |>
    mutate(
      lon   = sf::st_coordinates(geometry)[, 1],
      lat   = sf::st_coordinates(geometry)[, 2],
      label = if_else(
        !is.na(.data[[value_col]]),
        paste0(NAME_3, "\n", sprintf(label_format, .data[[value_col]])),
        NA_character_
      )
    ) |>
    sf::st_drop_geometry() |>
    filter(!is.na(label))

  ggplot(map_df) +
    geom_sf(aes(fill = .data[[value_col]]), color = border_color, linewidth = 0.2) +
    geom_text(
      data       = centroids,
      aes(x = lon, y = lat, label = label),
      color      = label_color,
      family     = "montserrat",
      fontface   = "bold",
      size       = 2,
      lineheight = 0.9
    ) +
    scale_fill_gradient(
      low      = low_color,
      high     = high_color,
      na.value = na_color,
      name     = NULL,
      labels   = function(x) sprintf(label_format, x)
    ) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    .map_theme(bg_color, text_color)
}



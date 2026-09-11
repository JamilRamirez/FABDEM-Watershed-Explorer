options(warn = 2)

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("El test requiere el paquete sf.")
}

source(
  file.path("R", "config.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "helpers.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

# Caso simple: no debe alterarse.
simple_ring <- rbind(
  c(-75.10, -10.10),
  c(-74.90, -10.10),
  c(-74.90,  -9.90),
  c(-75.10,  -9.90),
  c(-75.10, -10.10)
)

simple <- sf::st_sf(
  NAME = "simple",
  geometry = sf::st_sfc(
    sf::st_polygon(list(simple_ring)),
    crs = 4326
  )
)

simple_out <- fabdem_google_earth_geometry(simple)
simple_meta <- attr(simple_out, "fabdem_google_earth")

stopifnot(
  is.list(simple_meta),
  identical(simple_meta$simplified, FALSE),
  fabdem_geometry_vertex_count(simple_out) ==
    fabdem_geometry_vertex_count(simple),
  all(sf::st_is_valid(simple_out))
)

# Caso deliberadamente denso: debe quedar por debajo del objetivo de Google Earth.
theta <- seq(0, 2 * pi, length.out = 16001L)
dense_ring <- cbind(
  -75 + 0.50 * cos(theta),
  -10 + 0.35 * sin(theta)
)

dense <- sf::st_sf(
  NAME = "dense",
  geometry = sf::st_sfc(
    sf::st_polygon(list(dense_ring)),
    crs = 4326
  )
)

dense_out <- fabdem_google_earth_geometry(
  dense,
  trigger_vertices = 9500L,
  target_vertices = 8500L
)
dense_meta <- attr(dense_out, "fabdem_google_earth")

stopifnot(
  is.list(dense_meta),
  identical(dense_meta$simplified, TRUE),
  dense_meta$vertices_before > 9500L,
  dense_meta$vertices_after <= 8500L,
  dense_meta$vertices_after < dense_meta$vertices_before,
  is.finite(dense_meta$tolerance_m),
  dense_meta$tolerance_m > 0,
  is.finite(dense_meta$area_change_pct),
  dense_meta$area_change_pct < 1,
  all(sf::st_is_valid(dense_out)),
  sf::st_crs(dense_out)$epsg == 4326L
)

cat("Google Earth export simplification: PASS\n")

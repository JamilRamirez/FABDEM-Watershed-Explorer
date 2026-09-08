# FABDEM Watershed Explorer - smoke test de carga completa

options(
  shiny.launch.browser = FALSE,
  warn = 1
)

root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

if (!file.exists(file.path(root, "app.R"))) {
  stop("Ejecuta este smoke test desde la raíz del repositorio.")
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(root)

required_packages <- c(
  "shiny",
  "leaflet",
  "sf",
  "terra",
  "DT",
  "readxl"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Faltan paquetes para smoke test: ",
      paste(missing_packages, collapse = ", ")
    )
  )
}

app_env <- new.env(parent = globalenv())

result <- source(
  "app.R",
  local = app_env,
  echo = FALSE,
  print.eval = FALSE,
  encoding = "UTF-8",
  chdir = FALSE
)

if (!exists("ui", envir = app_env, inherits = FALSE)) {
  stop("app.R no creó el objeto ui.")
}

if (!exists("server", envir = app_env, inherits = FALSE)) {
  stop("app.R no creó el objeto server.")
}

if (!is.function(get("server", envir = app_env, inherits = FALSE))) {
  stop("El objeto server no es una función.")
}

if (!inherits(result$value, "shiny.appobj")) {
  stop("app.R no terminó construyendo un objeto shiny.appobj.")
}

modules <- c(
  "delimitacion",
  "morfometria",
  "geologia",
  "geomorfologia",
  "suelos",
  "hidrogeologia",
  "medio_fisico",
  "clima",
  "cobertura",
  "cum",
  "vida",
  "clima_superficie",
  "cuencas",
  "distritos",
  "poblados",
  "contexto_territorial"
)

for (module_name in modules) {
  if (!exists(module_name, envir = app_env, inherits = FALSE)) {
    stop("No se cargó el módulo: ", module_name)
  }

  module <- get(module_name, envir = app_env, inherits = FALSE)

  if (!is.list(module)) {
    stop("El módulo no es una lista: ", module_name)
  }

  if (!is.function(module$ui)) {
    stop("El módulo no expone ui(): ", module_name)
  }

  if (!is.function(module$server)) {
    stop("El módulo no expone server(): ", module_name)
  }
}

required_runtime_objects <- c(
  "RUNTIME_BASE_URL",
  "RUNTIME_ROOT",
  "CORE_DIR",
  "DEM_DIR",
  "ASSET_MANIFEST_CSV",
  "CATALOG_READY_FILE"
)

for (object_name in required_runtime_objects) {
  if (!exists(object_name, envir = app_env, inherits = FALSE)) {
    stop("config.R no dejó disponible: ", object_name)
  }
}

if (!file.exists(app_env$ASSET_MANIFEST_CSV)) {
  stop("ASSET_MANIFEST_CSV no existe después de cargar la app.")
}

if (!file.exists(app_env$CATALOG_READY_FILE)) {
  stop("CATALOG_READY_FILE no existe después de cargar la app.")
}



# ============================================================
# Regresion: continuidad D8 en polygonizacion
# ============================================================
# Una cadena de celdas conectadas solo por esquinas debe seguir
# siendo una unica cuenca vectorial. Un salto real debe fallar.

r_d8 <- terra::rast(
  nrows = 12,
  ncols = 12,
  xmin = 0,
  xmax = 360,
  ymin = 0,
  ymax = 360,
  crs = "EPSG:3857"
)
terra::values(r_d8) <- NA_real_

main_cells <- unlist(
  lapply(
    8:11,
    function(rr) terra::cellFromRowCol(r_d8, rr, 2:5)
  ),
  use.names = FALSE
)

diagonal_cells <- c(
  terra::cellFromRowCol(r_d8, 7, 6),
  terra::cellFromRowCol(r_d8, 6, 7),
  terra::cellFromRowCol(r_d8, 5, 8),
  terra::cellFromRowCol(r_d8, 4, 9)
)

upper_cells <- unlist(
  lapply(
    2:4,
    function(rr) terra::cellFromRowCol(r_d8, rr, 9:11)
  ),
  use.names = FALSE
)

cells_d8 <- unique(
  c(
    main_cells,
    diagonal_cells,
    upper_cells
  )
)

r_d8[cells_d8] <- 1

r_d8_file <- tempfile(fileext = ".tif")
gpkg_d8_file <- tempfile(fileext = ".gpkg")

terra::writeRaster(
  r_d8,
  r_d8_file,
  overwrite = TRUE,
  datatype = "INT1U",
  NAflag = 0
)

poly_d8 <- app_env$polygonize_basin(
  r_d8_file,
  gpkg_d8_file,
  expected_cells = length(cells_d8)
)

poly_parts <- suppressWarnings(
  sf::st_cast(
    sf::st_union(
      sf::st_geometry(poly_d8)
    ),
    "POLYGON"
  )
)

if (length(poly_parts) != 1L) {
  stop(
    "Regresion D8: una cadena diagonal valida siguio produciendo varios poligonos."
  )
}

expected_area_m2 <- length(cells_d8) * 30 * 30
actual_area_m2 <- as.double(
  sf::st_area(
    sf::st_transform(
      poly_d8,
      3857
    )
  )
)

if (
  !is.finite(actual_area_m2) ||
  abs(actual_area_m2 - expected_area_m2) >
    max(10, expected_area_m2 * 1e-4)
) {
  stop(
    "Regresion D8: el cierre diagonal altero excesivamente el area."
  )
}

r_gap <- r_d8
r_gap[terra::cellFromRowCol(r_gap, 1, 1)] <- 1
r_gap_file <- tempfile(fileext = ".tif")
gpkg_gap_file <- tempfile(fileext = ".gpkg")

terra::writeRaster(
  r_gap,
  r_gap_file,
  overwrite = TRUE,
  datatype = "INT1U",
  NAflag = 0
)

gap_failed <- FALSE
tryCatch(
  {
    app_env$polygonize_basin(
      r_gap_file,
      gpkg_gap_file,
      expected_cells = length(cells_d8) + 1L
    )
  },
  error = function(e) {
    gap_failed <<- grepl(
      "CONTINUIDAD D8",
      conditionMessage(e),
      fixed = TRUE
    )
  }
)

if (!isTRUE(gap_failed)) {
  stop(
    "Regresion D8: un salto real no fue rechazado por el control de continuidad."
  )
}


# Fuerza la ruta usada para rasters muy grandes: se omite patches(),
# pero la QA vectorial final debe seguir rechazando cualquier salto.
gap_failed_vector <- FALSE
tryCatch(
  {
    app_env$polygonize_basin(
      r_gap_file,
      gpkg_gap_file,
      expected_cells = length(cells_d8) + 1L,
      connectivity_scan_max_cells = 0
    )
  },
  error = function(e) {
    gap_failed_vector <<- grepl(
      "FALLO VECTORIAL D8",
      conditionMessage(e),
      fixed = TRUE
    )
  }
)

if (!isTRUE(gap_failed_vector)) {
  stop(
    "Regresion D8: la ruta escalable permitio una geometria con salto real."
  )
}

unlink(
  c(
    r_d8_file,
    gpkg_d8_file,
    r_gap_file,
    gpkg_gap_file
  ),
  force = TRUE
)


cat(
  "FABDEM Shiny smoke test: PASS\n",
  "Modules loaded: ",
  length(modules),
  "\n",
  sep = ""
)

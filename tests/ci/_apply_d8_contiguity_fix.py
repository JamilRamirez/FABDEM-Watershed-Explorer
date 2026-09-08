from pathlib import Path
import re

root = Path('.')
helpers_path = root / 'R' / 'helpers.R'
delim_path = root / 'R' / 'delimitacion.R'
smoke_path = root / 'tests' / 'ci' / 'smoke_app.R'

helpers = helpers_path.read_text(encoding='utf-8')
delim = delim_path.read_text(encoding='utf-8')
smoke = smoke_path.read_text(encoding='utf-8')

validation_block = r'''
# ============================================================
# VALIDACION DE CONTINUIDAD D8 DEL RASTER DE CUENCA
# ============================================================
#
# Una cuenca derivada de D8 puede conectarse por lados o por
# esquinas. Antes de vectorizar se exige que todas sus celdas
# formen UN SOLO componente usando vecindad de 8 celdas.
#
# Esto permite conexiones diagonales hidrologicamente validas,
# pero rechaza cualquier salto real de una o mas celdas.
# ============================================================

validate_basin_raster_d8 <- function(
    basin_tif,
    expected_cells = NULL
) {

  if (!file_nonempty(
    basin_tif
  )) {
    stop(
      "No existe un raster de cuenca valido para control de continuidad D8."
    )
  }


  r <- terra::rast(
    basin_tif
  )


  actual_cells <- suppressWarnings(
    as.double(
      terra::global(
        r,
        "sum",
        na.rm = TRUE
      )[1, 1]
    )
  )


  if (
    !is.finite(actual_cells) ||
    actual_cells < 1
  ) {
    stop(
      "El raster de cuenca no contiene celdas validas."
    )
  }


  if (
    !is.null(expected_cells) &&
    length(expected_cells) == 1L &&
    is.finite(expected_cells)
  ) {

    expected_cells <- as.double(
      expected_cells
    )


    if (!isTRUE(
      actual_cells == expected_cells
    )) {
      stop(
        paste0(
          "FALLO DE MATERIALIZACION: el rastreo D8 produjo ",
          format(expected_cells, big.mark = ",", scientific = FALSE),
          " celdas, pero basin.tif contiene ",
          format(actual_cells, big.mark = ",", scientific = FALSE),
          ". La cuenca no sera vectorizada."
        )
      )
    }
  }


  patch_file <- tempfile(
    pattern = "basin_d8_patches_",
    tmpdir = TERRA_TEMP,
    fileext = ".tif"
  )


  on.exit(
    unlink(
      patch_file,
      force = TRUE
    ),
    add = TRUE
  )


  patch_r <- terra::patches(
    r,
    directions = 8,
    values = FALSE,
    zeroAsNA = TRUE,
    allowGaps = FALSE,
    filename = patch_file,
    overwrite = TRUE
  )


  n_patches <- suppressWarnings(
    as.integer(
      terra::global(
        patch_r,
        "max",
        na.rm = TRUE
      )[1, 1]
    )
  )


  if (
    !is.finite(n_patches) ||
    n_patches != 1L
  ) {
    stop(
      paste0(
        "FALLO DE CONTINUIDAD D8: la cuenca contiene ",
        if (is.finite(n_patches)) n_patches else "varios",
        " componentes separados. Se detecto un salto real entre celdas y ",
        "la geometria no sera generada."
      )
    )
  }


  list(
    n_cells = actual_cells,
    n_patches_8 = n_patches
  )
}


# ============================================================
# CIERRE VECTORIAL DE UNIONES DIAGONALES D8
# ============================================================
#
# terra::as.polygons() disuelve bien vecinos por lado, pero una
# cadena D8 que solo se toca por esquinas puede quedar como
# MULTIPOLYGON de cuadrados. Una vez validado que el raster es
# un unico componente 8-conectado, se aplica un cierre geometrico
# de apenas 0.10 m en un CRS metrico global. Ese cierre une solo
# contactos numericamente puntuales y no puede salvar un hueco de
# una celda FABDEM (~30 m), que ya habria sido rechazado arriba.
# ============================================================

close_d8_diagonal_polygon <- function(
    x,
    bridge_m = 0.10
) {

  if (
    is.null(x) ||
    !inherits(x, "sf") ||
    nrow(x) < 1L
  ) {
    stop(
      "No hay geometria valida para cerrar conexiones diagonales D8."
    )
  }


  source_crs <- sf::st_crs(
    x
  )


  if (is.na(
    source_crs
  )) {
    stop(
      "La geometria de cuenca no tiene CRS durante el cierre D8."
    )
  }


  x <- sf::st_make_valid(
    x
  )


  merged <- suppressWarnings(
    sf::st_union(
      sf::st_geometry(
        x
      )
    )
  )


  parts_before <- suppressWarnings(
    sf::st_cast(
      merged,
      "POLYGON"
    )
  )


  if (length(parts_before) <= 1L) {
    return(
      sf::st_sf(
        geometry = merged
      )
    )
  }


  metric <- sf::st_transform(
    sf::st_sf(
      geometry = merged
    ),
    6933
  )


  area_before_m2 <- as.double(
    sf::st_area(
      metric
    )
  )


  closed_metric <- suppressWarnings(
    sf::st_buffer(
      metric,
      dist = bridge_m
    )
  )


  closed_metric <- suppressWarnings(
    sf::st_union(
      closed_metric
    )
  )


  closed_metric <- suppressWarnings(
    sf::st_buffer(
      closed_metric,
      dist = -bridge_m
    )
  )


  closed_metric <- sf::st_make_valid(
    sf::st_sf(
      geometry = closed_metric
    )
  )


  closed_metric <- closed_metric[
    !sf::st_is_empty(
      closed_metric
    ),
    ,
    drop = FALSE
  ]


  if (nrow(
    closed_metric
  ) < 1L) {
    stop(
      "El cierre de conexiones diagonales D8 produjo una geometria vacia."
    )
  }


  closed_union <- suppressWarnings(
    sf::st_union(
      sf::st_geometry(
        closed_metric
      )
    )
  )


  parts_after <- suppressWarnings(
    sf::st_cast(
      closed_union,
      "POLYGON"
    )
  )


  if (length(parts_after) != 1L) {
    stop(
      paste0(
        "FALLO VECTORIAL D8: despues de cerrar contactos diagonales aun quedan ",
        length(parts_after),
        " poligonos separados. La cuenca no sera exportada."
      )
    )
  }


  area_after_m2 <- as.double(
    sf::st_area(
      sf::st_sf(
        geometry = closed_union
      )
    )
  )


  area_delta_m2 <- abs(
    area_after_m2 - area_before_m2
  )


  area_tolerance_m2 <- max(
    10,
    area_before_m2 * 1e-4
  )


  if (
    !is.finite(area_delta_m2) ||
    area_delta_m2 > area_tolerance_m2
  ) {
    stop(
      paste0(
        "FALLO VECTORIAL D8: el cierre diagonal alteraria el area en ",
        format(round(area_delta_m2, 3), scientific = FALSE),
        " m2, por encima de la tolerancia de seguridad."
      )
    )
  }


  closed_source <- sf::st_transform(
    sf::st_sf(
      geometry = closed_union
    ),
    source_crs
  )


  sf::st_make_valid(
    closed_source
  )
}


'''

marker = "# ============================================================\n# ESCRIBIR BASIN.TIF SIN MATRIZ GIGANTE\n# ============================================================"
if validation_block.strip() not in helpers:
    if marker not in helpers:
        raise SystemExit('No se encontro marcador de write_basin_raster')
    helpers = helpers.replace(marker, validation_block + marker, 1)

poly_pattern = re.compile(
    r"# ============================================================\n# POLIGONIZAR\n# ============================================================\n\npolygonize_basin <- function\(.*?\n\}\n\n\n\n# ============================================================\n# 7\. FILAS DE GRILLA Y SNAP A STREAM STRIPES",
    re.S,
)

poly_replacement = r'''# ============================================================
# POLIGONIZAR
# ============================================================

polygonize_basin <- function(
    basin_tif,
    basin_gpkg,
    expected_cells = NULL
) {

  qa <- validate_basin_raster_d8(
    basin_tif = basin_tif,
    expected_cells = expected_cells
  )


  r <- terra::rast(
    basin_tif
  )


  p <- terra::as.polygons(
    r,
    dissolve = TRUE,
    values = FALSE,
    na.rm = TRUE
  )


  x <- sf::st_as_sf(
    p
  )


  x <- sf::st_make_valid(
    x
  )


  x <- x[
    !sf::st_is_empty(
      x
    ),
    ,
    drop = FALSE
  ]


  if (nrow(
    x
  ) < 1L) {
    stop(
      "La polygonizacion de la cuenca produjo una geometria vacia."
    )
  }


  x <- close_d8_diagonal_polygon(
    x
  )


  if (file.exists(
    basin_gpkg
  )) {
    file.remove(
      basin_gpkg
    )
  }


  sf::st_write(
    x,
    basin_gpkg,
    layer = "basin",
    quiet = TRUE
  )


  attr(
    x,
    "d8_contiguity_qa"
  ) <- qa


  x
}



# ============================================================
# 7. FILAS DE GRILLA Y SNAP A STREAM STRIPES'''

helpers2, n_poly = poly_pattern.subn(poly_replacement, helpers, count=1)
if n_poly != 1:
    raise SystemExit(f'polygonize_basin reemplazos: {n_poly}')
helpers = helpers2

old_trace_rm = '''                    rm(
                      trace
                    )'''
new_trace_rm = '''                    trace_cell_count <- as.double(
                      trace$n_cells
                    )


                    rm(
                      trace
                    )'''
if new_trace_rm not in delim:
    if old_trace_rm not in delim:
        raise SystemExit('No se encontro rm(trace)')
    delim = delim.replace(old_trace_rm, new_trace_rm, 1)

old_poly_call = '''                    basin_sf <- polygonize_basin(
                      basin_tif,
                      basin_gpkg
                    )'''
new_poly_call = '''                    basin_sf <- polygonize_basin(
                      basin_tif,
                      basin_gpkg,
                      expected_cells = trace_cell_count
                    )'''
if new_poly_call not in delim:
    if old_poly_call not in delim:
        raise SystemExit('No se encontro llamada polygonize_basin')
    delim = delim.replace(old_poly_call, new_poly_call, 1)

regression = r'''

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

unlink(
  c(
    r_d8_file,
    gpkg_d8_file,
    r_gap_file,
    gpkg_gap_file
  ),
  force = TRUE
)
'''

cat_marker = 'cat(\n  "FABDEM Shiny smoke test: PASS\\n",'
if 'Regresion: continuidad D8 en polygonizacion' not in smoke:
    if cat_marker not in smoke:
        raise SystemExit('No se encontro cat final de smoke_app.R')
    smoke = smoke.replace(cat_marker, regression + '\n\n' + cat_marker, 1)

helpers_path.write_text(helpers, encoding='utf-8')
delim_path.write_text(delim, encoding='utf-8')
smoke_path.write_text(smoke, encoding='utf-8')

print('D8 contiguity fix applied')

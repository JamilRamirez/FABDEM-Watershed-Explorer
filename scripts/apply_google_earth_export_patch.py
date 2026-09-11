from pathlib import Path

root = Path(__file__).resolve().parents[1]
helpers_path = root / "R" / "helpers.R"
delim_path = root / "R" / "delimitacion.R"
ci_path = root / ".github" / "workflows" / "ci.yml"
workflow_path = root / ".github" / "workflows" / "apply-google-earth-export-patch.yml"
self_path = Path(__file__).resolve()

helper_sentinel = "# GOOGLE EARTH EXPORT COMPATIBILITY"
helpers = helpers_path.read_text(encoding="utf-8")

helper_block = r'''


# ============================================================
# GOOGLE EARTH EXPORT COMPATIBILITY
# ============================================================
# KML/KMZ son formatos de visualizacion. Para evitar que Google Earth
# omita o simplifique de forma opaca cuencas con demasiados vertices,
# se genera una copia simplificada solo para esos formatos.
# GPKG/SHP y la geometria interna de la aplicacion no pasan por aqui.

fabdem_geometry_vertex_count <- function(x) {
  geometry <- if (inherits(x, "sf")) {
    sf::st_geometry(x)
  } else {
    x
  }

  if (!inherits(geometry, "sfc")) {
    stop("La geometria debe ser un objeto sf/sfc.")
  }

  if (length(geometry) < 1L) {
    return(0L)
  }

  coordinates <- tryCatch(
    suppressWarnings(
      sf::st_coordinates(geometry)
    ),
    error = function(e) NULL
  )

  if (is.null(coordinates)) {
    return(0L)
  }

  as.integer(nrow(coordinates))
}


fabdem_google_earth_geometry <- function(
    x,
    trigger_vertices = 9500L,
    target_vertices = 8500L,
    max_iterations = 16L
) {
  if (
    is.null(x) ||
    !inherits(x, "sf") ||
    nrow(x) < 1L
  ) {
    stop("Se requiere una geometria sf valida para exportar a Google Earth.")
  }

  if (is.na(sf::st_crs(x))) {
    stop("La geometria para Google Earth no tiene CRS.")
  }

  trigger_vertices <- as.integer(trigger_vertices)
  target_vertices <- as.integer(target_vertices)
  max_iterations <- as.integer(max_iterations)

  if (
    !is.finite(trigger_vertices) ||
    !is.finite(target_vertices) ||
    target_vertices < 4L ||
    trigger_vertices <= target_vertices
  ) {
    stop("Los umbrales de vertices para Google Earth son invalidos.")
  }

  if (!is.finite(max_iterations) || max_iterations < 1L) {
    max_iterations <- 16L
  }

  x_wgs84 <- sf::st_transform(x, 4326)
  vertices_before <- fabdem_geometry_vertex_count(x_wgs84)

  add_metadata <- function(
      object,
      simplified,
      vertices_after,
      tolerance_m = 0,
      area_change_pct = 0,
      target_met = TRUE
  ) {
    attr(object, "fabdem_google_earth") <- list(
      simplified = isTRUE(simplified),
      vertices_before = as.integer(vertices_before),
      vertices_after = as.integer(vertices_after),
      trigger_vertices = as.integer(trigger_vertices),
      target_vertices = as.integer(target_vertices),
      tolerance_m = as.numeric(tolerance_m),
      area_change_pct = as.numeric(area_change_pct),
      target_met = isTRUE(target_met)
    )
    object
  }

  if (
    !is.finite(vertices_before) ||
    vertices_before <= trigger_vertices
  ) {
    return(
      add_metadata(
        x_wgs84,
        simplified = FALSE,
        vertices_after = vertices_before
      )
    )
  }

  # La simplificacion se realiza en metros. EPSG:3857 se usa unicamente
  # como plano de trabajo y nunca se entrega al usuario.
  x_metric <- sf::st_transform(x_wgs84, 3857)
  bbox <- sf::st_bbox(x_metric)

  diagonal_m <- sqrt(
    as.numeric(bbox[["xmax"]] - bbox[["xmin"]]) ^ 2 +
      as.numeric(bbox[["ymax"]] - bbox[["ymin"]]) ^ 2
  )

  if (!is.finite(diagonal_m) || diagonal_m <= 0) {
    return(
      add_metadata(
        x_wgs84,
        simplified = FALSE,
        vertices_after = vertices_before,
        target_met = FALSE
      )
    )
  }

  max_tolerance_m <- min(
    25000,
    max(
      250,
      diagonal_m / 20
    )
  )

  build_candidate <- function(tolerance_m) {
    candidate <- tryCatch(
      suppressWarnings(
        sf::st_simplify(
          x_metric,
          dTolerance = tolerance_m,
          preserveTopology = TRUE
        )
      ),
      error = function(e) NULL
    )

    if (is.null(candidate)) {
      return(NULL)
    }

    validity <- tryCatch(
      sf::st_is_valid(candidate),
      error = function(e) rep(FALSE, nrow(candidate))
    )

    if (!all(validity %in% TRUE)) {
      candidate <- tryCatch(
        suppressWarnings(
          sf::st_make_valid(candidate)
        ),
        error = function(e) NULL
      )
    }

    if (is.null(candidate) || nrow(candidate) < 1L) {
      return(NULL)
    }

    if (any(sf::st_is_empty(candidate))) {
      return(NULL)
    }

    geometry_types <- as.character(
      sf::st_geometry_type(
        candidate,
        by_geometry = TRUE
      )
    )

    if (!all(geometry_types %in% c("POLYGON", "MULTIPOLYGON"))) {
      return(NULL)
    }

    vertices <- fabdem_geometry_vertex_count(candidate)

    if (!is.finite(vertices) || vertices < 4L) {
      return(NULL)
    }

    list(
      data = candidate,
      vertices = as.integer(vertices),
      tolerance_m = as.numeric(tolerance_m)
    )
  }

  upper <- 1
  candidate <- NULL
  best_reduction <- NULL

  while (upper <= max_tolerance_m) {
    trial <- build_candidate(upper)

    if (
      !is.null(trial) &&
      trial$vertices < vertices_before
    ) {
      best_reduction <- trial
    }

    if (
      !is.null(trial) &&
      trial$vertices <= target_vertices
    ) {
      candidate <- trial
      break
    }

    upper <- upper * 2
  }

  if (is.null(candidate)) {
    trial <- build_candidate(max_tolerance_m)

    if (
      !is.null(trial) &&
      trial$vertices < vertices_before
    ) {
      best_reduction <- trial
    }

    candidate <- best_reduction
  }

  if (is.null(candidate)) {
    return(
      add_metadata(
        x_wgs84,
        simplified = FALSE,
        vertices_after = vertices_before,
        target_met = FALSE
      )
    )
  }

  # Si ya se encontro una tolerancia que cumple el objetivo, se busca
  # por biseccion la menor tolerancia posible para conservar mas detalle.
  if (candidate$vertices <= target_vertices) {
    low <- 0
    high <- candidate$tolerance_m

    for (iteration in seq_len(max_iterations)) {
      midpoint <- (low + high) / 2
      trial <- build_candidate(midpoint)

      if (
        !is.null(trial) &&
        trial$vertices <= target_vertices
      ) {
        candidate <- trial
        high <- midpoint
      } else {
        low <- midpoint
      }
    }
  }

  area_before <- suppressWarnings(
    sum(
      as.numeric(
        sf::st_area(x_metric)
      ),
      na.rm = TRUE
    )
  )

  area_after <- suppressWarnings(
    sum(
      as.numeric(
        sf::st_area(candidate$data)
      ),
      na.rm = TRUE
    )
  )

  area_change_pct <- if (
    is.finite(area_before) &&
    area_before > 0 &&
    is.finite(area_after)
  ) {
    abs(area_after - area_before) / area_before * 100
  } else {
    NA_real_
  }

  out <- sf::st_transform(
    candidate$data,
    4326
  )

  add_metadata(
    out,
    simplified = TRUE,
    vertices_after = fabdem_geometry_vertex_count(out),
    tolerance_m = candidate$tolerance_m,
    area_change_pct = area_change_pct,
    target_met = candidate$vertices <= target_vertices
  )
}
'''

if helper_sentinel not in helpers:
    helpers = helpers.rstrip() + helper_block + "\n"
    helpers_path.write_text(helpers, encoding="utf-8")


delim = delim_path.read_text(encoding="utf-8")

old_header = (
    "# v5: admite cuenca delimitada o importada y permite exportar "
    "GPKG/SHP en WGS84 o UTM; KML/KMZ en WGS84"
)
new_header = (
    "# v6: GPKG/SHP conservan geometria original; KML/KMZ generan "
    "copia compatible con Google Earth en WGS84"
)
if old_header in delim:
    delim = delim.replace(old_header, new_header, 1)
elif new_header not in delim:
    raise SystemExit("No se encontro la cabecera esperada de delimitacion.R")

old_note = '"KML y KMZ se exportan siempre en WGS84."'
new_note = (
    '"KML y KMZ se exportan en WGS84 y, si la cuenca es muy compleja, '
    'se simplifican solo para visualizacion en Google Earth. GPKG y '
    'Shapefile conservan el borde original."'
)
if old_note in delim:
    delim = delim.replace(old_note, new_note, 1)
elif new_note not in delim:
    raise SystemExit("No se encontro la nota KML/KMZ esperada")

kml_transform = '''      x_wgs84 <- sf::st_transform(
        x,
        4326
      )


      artifact <- file.path('''
kml_replacement = '''      x_wgs84 <- sf::st_transform(
        x,
        4326
      )


      x_google_earth <- fabdem_google_earth_geometry(
        x_wgs84
      )


      artifact <- file.path('''
if kml_transform in delim:
    delim = delim.replace(kml_transform, kml_replacement, 1)
elif kml_replacement not in delim:
    raise SystemExit("No se encontro el bloque KML esperado")

kmz_transform = '''      x_wgs84 <- sf::st_transform(
        x,
        4326
      )


      kmz_dir <- file.path('''
kmz_replacement = '''      x_wgs84 <- sf::st_transform(
        x,
        4326
      )


      x_google_earth <- fabdem_google_earth_geometry(
        x_wgs84
      )


      kmz_dir <- file.path('''
if kmz_transform in delim:
    delim = delim.replace(kmz_transform, kmz_replacement, 1)
elif kmz_replacement not in delim:
    raise SystemExit("No se encontro el bloque KMZ esperado")

old_kml_write = '''      sf::st_write(
        x_wgs84,
        artifact,'''
new_kml_write = '''      sf::st_write(
        x_google_earth,
        artifact,'''
if old_kml_write in delim:
    delim = delim.replace(old_kml_write, new_kml_write, 1)
elif new_kml_write not in delim:
    raise SystemExit("No se encontro st_write KML esperado")

old_kmz_write = '''      sf::st_write(
        x_wgs84,
        kml_file,'''
new_kmz_write = '''      sf::st_write(
        x_google_earth,
        kml_file,'''
if old_kmz_write in delim:
    delim = delim.replace(old_kmz_write, new_kmz_write, 1)
elif new_kmz_write not in delim:
    raise SystemExit("No se encontro st_write KMZ esperado")

if delim.count("fabdem_google_earth_geometry(") != 2:
    raise SystemExit(
        "La integracion Google Earth debe aparecer exactamente dos veces "
        "en delimitacion.R"
    )

delim_path.write_text(delim, encoding="utf-8")


ci = ci_path.read_text(encoding="utf-8")
ci_step = '''      - name: Test Google Earth KML/KMZ simplification
        run: Rscript tests/ci/test_google_earth_export.R

'''
ci_anchor = "      - name: Build the complete Shiny app without running a server\n"
if ci_step not in ci:
    if ci_anchor not in ci:
        raise SystemExit("No se encontro el ancla esperada en ci.yml")
    ci = ci.replace(ci_anchor, ci_step + ci_anchor, 1)
    ci_path.write_text(ci, encoding="utf-8")

# El parcheador y su workflow son temporales; el arbol final queda limpio.
if workflow_path.exists():
    workflow_path.unlink()
if self_path.exists():
    self_path.unlink()

print("Patch aplicado y validado.")

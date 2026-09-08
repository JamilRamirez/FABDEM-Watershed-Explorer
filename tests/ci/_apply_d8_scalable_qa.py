from pathlib import Path
import re

helpers_path = Path('R/helpers.R')
smoke_path = Path('tests/ci/smoke_app.R')
helpers = helpers_path.read_text(encoding='utf-8')
smoke = smoke_path.read_text(encoding='utf-8')

func_pattern = re.compile(
    r"validate_basin_raster_d8 <- function\(.*?\n\}\n\n\n# ============================================================\n# CIERRE VECTORIAL DE UNIONES DIAGONALES D8",
    re.S,
)

new_func = r'''validate_basin_raster_d8 <- function(
    basin_tif,
    expected_cells = NULL,
    max_patch_cells = 5000000
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


  raster_cells <- as.double(
    terra::ncell(
      r
    )
  )


  max_patch_cells <- suppressWarnings(
    as.double(
      max_patch_cells
    )
  )


  run_patch_check <- (
    is.finite(max_patch_cells) &&
    max_patch_cells >= 1 &&
    is.finite(raster_cells) &&
    raster_cells <= max_patch_cells
  )


  n_patches <- NA_integer_


  if (isTRUE(
    run_patch_check
  )) {

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
  }


  list(
    n_cells = actual_cells,
    raster_cells = raster_cells,
    n_patches_8 = n_patches,
    patch_check = if (isTRUE(run_patch_check)) {
      "FULL_8_CONNECTED"
    } else {
      "SKIPPED_LARGE_RASTER_FINAL_VECTOR_QA_REQUIRED"
    }
  )
}


# ============================================================
# CIERRE VECTORIAL DE UNIONES DIAGONALES D8'''

helpers2, n = func_pattern.subn(new_func, helpers, count=1)
if n != 1:
    raise SystemExit(f'validate_basin_raster_d8 replacements={n}')
helpers = helpers2

old_sig = '''polygonize_basin <- function(
    basin_tif,
    basin_gpkg,
    expected_cells = NULL
) {'''
new_sig = '''polygonize_basin <- function(
    basin_tif,
    basin_gpkg,
    expected_cells = NULL,
    connectivity_scan_max_cells = 5000000
) {'''
if old_sig not in helpers:
    raise SystemExit('polygonize signature not found')
helpers = helpers.replace(old_sig, new_sig, 1)

old_qa = '''  qa <- validate_basin_raster_d8(
    basin_tif = basin_tif,
    expected_cells = expected_cells
  )'''
new_qa = '''  qa <- validate_basin_raster_d8(
    basin_tif = basin_tif,
    expected_cells = expected_cells,
    max_patch_cells = connectivity_scan_max_cells
  )'''
if old_qa not in helpers:
    raise SystemExit('polygonize QA call not found')
helpers = helpers.replace(old_qa, new_qa, 1)

old_tail = '''  closed_source <- sf::st_transform(
    sf::st_sf(
      geometry = closed_union
    ),
    source_crs
  )


  sf::st_make_valid(
    closed_source
  )
}'''
new_tail = '''  closed_source <- sf::st_transform(
    sf::st_sf(
      geometry = closed_union
    ),
    source_crs
  )


  closed_source <- sf::st_make_valid(
    closed_source
  )


  final_union <- suppressWarnings(
    sf::st_union(
      sf::st_geometry(
        closed_source
      )
    )
  )


  final_parts <- suppressWarnings(
    sf::st_cast(
      final_union,
      "POLYGON"
    )
  )


  if (length(final_parts) != 1L) {
    stop(
      "FALLO VECTORIAL D8: la reproyeccion final volvio a separar la cuenca."
    )
  }


  sf::st_sf(
    geometry = final_union
  )
}'''
if old_tail not in helpers:
    raise SystemExit('close_d8 tail not found')
helpers = helpers.replace(old_tail, new_tail, 1)

smoke_anchor = '''if (!isTRUE(gap_failed)) {
  stop(
    "Regresion D8: un salto real no fue rechazado por el control de continuidad."
  )
}
'''
extra_test = r'''

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
'''
if 'Regresion D8: la ruta escalable permitio' not in smoke:
    if smoke_anchor not in smoke:
        raise SystemExit('smoke anchor not found')
    smoke = smoke.replace(smoke_anchor, smoke_anchor + extra_test, 1)

helpers_path.write_text(helpers, encoding='utf-8')
smoke_path.write_text(smoke, encoding='utf-8')
print('Scalable D8 QA applied')

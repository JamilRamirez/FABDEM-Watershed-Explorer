from pathlib import Path
import re


def replace_once(text, old, new, label):
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"{label}: expected 1 match, found {count}")
    return text.replace(old, new, 1)


# ==============================================================
# R/delimitacion.R
# ==============================================================
p = Path("R/delimitacion.R")
text = p.read_text(encoding="utf-8")

text = replace_once(
    text,
    "# v4: admite cuenca delimitada o importada, expone origen y permite exportar KML/KMZ/GPKG/SHP ZIP",
    "# v5: admite cuenca delimitada o importada y permite exportar GPKG/SHP en WGS84 o UTM; KML/KMZ en WGS84",
    "delimitacion header",
)

helper = r'''
  project_basin_export_crs <- function(
      x,
      crs_mode = c(
        "utm",
        "wgs84"
      )
  ) {

    crs_mode <- match.arg(
      crs_mode
    )

    x_wgs84 <- sf::st_transform(
      x,
      4326
    )

    if (identical(
      crs_mode,
      "wgs84"
    )) {
      return(
        list(
          data = x_wgs84,
          epsg = 4326L,
          tag = "WGS84"
        )
      )
    }

    center_geom <- suppressWarnings(
      sf::st_point_on_surface(
        sf::st_union(
          sf::st_geometry(
            x_wgs84
          )
        )
      )
    )

    xy <- sf::st_coordinates(
      center_geom
    )

    if (
      nrow(xy) < 1L ||
      !all(
        is.finite(
          xy[1, c(
            "X",
            "Y"
          )]
        )
      )
    ) {
      stop(
        "No se pudo determinar el centro de la cuenca para seleccionar la zona UTM."
      )
    }

    epsg <- as.integer(
      utm_epsg_point(
        lon = xy[1, "X"],
        lat = xy[1, "Y"]
      )
    )

    list(
      data = sf::st_transform(
        x_wgs84,
        epsg
      ),
      epsg = epsg,
      tag = paste0(
        "EPSG",
        epsg
      )
    )
  }
'''

text = replace_once(
    text,
    "\n\n  writable_sf_driver <- function(",
    "\n\n" + helper + "\n\n  writable_sf_driver <- function(",
    "insert project_basin_export_crs",
)

text = replace_once(
    text,
    """  write_basin_export <- function(
      basin,
      format,
      target_file,
      basin_label = NULL,
      basin_source = NULL
  ) {""",
    """  write_basin_export <- function(
      basin,
      format,
      target_file,
      basin_label = NULL,
      basin_source = NULL,
      crs_mode = "utm"
  ) {""",
    "write_basin_export signature",
)

text = replace_once(
    text,
    """    x <- standardize_basin_export(
      basin = basin,
      basin_label = basin_label,
      basin_source = basin_source
    )


    stem <- export_safe_stem(""",
    """    x <- standardize_basin_export(
      basin = basin,
      basin_label = basin_label,
      basin_source = basin_source
    )


    crs_mode <- match.arg(
      crs_mode,
      c(
        "utm",
        "wgs84"
      )
    )


    projected_export <- project_basin_export_crs(
      x = x,
      crs_mode = crs_mode
    )


    x <- projected_export$data


    stem <- export_safe_stem(""",
    "project vector export",
)

text = replace_once(
    text,
    """            shiny::div(
              class = "coord-note",
              paste0(
                "KML y KMZ se exportan en WGS84. ",
                "GeoPackage y Shapefile conservan el CRS de la cuenca activa."
              )
            ),

            shiny::selectInput(
              session$ns(
                "formato_exportacion"
              ),""",
    """            shiny::div(
              class = "coord-note",
              paste0(
                "GeoPackage y Shapefile pueden descargarse en UTM automática o WGS84. ",
                "KML y KMZ se exportan siempre en WGS84."
              )
            ),

            shiny::selectInput(
              session$ns(
                "crs_exportacion"
              ),
              label = "Sistema de coordenadas",
              choices = c(
                "UTM automática (metros)" = "utm",
                "WGS84 / EPSG:4326 (grados)" = "wgs84"
              ),
              selected = "utm"
            ),

            shiny::selectInput(
              session$ns(
                "formato_exportacion"
              ),""",
    "vector export UI",
)

text = replace_once(
    text,
    """            stem <- export_safe_stem(
              basin_label_result()
            )


            extension <- switch(""",
    """            crs_mode <- input$crs_exportacion


            if (
              is.null(
                crs_mode
              ) ||
              !crs_mode %in%
                c(
                  "utm",
                  "wgs84"
                )
            ) {
              crs_mode <- "utm"
            }


            if (format_value %in%
              c(
                "kml",
                "kmz"
              )
            ) {
              crs_mode <- "wgs84"
            }


            stem <- export_safe_stem(
              basin_label_result()
            )


            crs_suffix <- if (identical(
              crs_mode,
              "utm"
            )) {
              "_UTM"
            } else {
              "_WGS84"
            }


            extension <- switch(""",
    "vector filename CRS",
)

text = replace_once(
    text,
    """            paste0(
              stem,
              extension
            )""",
    """            paste0(
              stem,
              crs_suffix,
              extension
            )""",
    "vector filename suffix",
)

text = replace_once(
    text,
    """            write_basin_export(
              basin = basin_now,
              format = format_value,
              target_file = file,
              basin_label = shiny::isolate(
                basin_label_result()
              ),
              basin_source = shiny::isolate(
                basin_source_result()
              )
            )""",
    """            crs_mode <- shiny::isolate(
              input$crs_exportacion
            )


            if (
              is.null(
                crs_mode
              ) ||
              !crs_mode %in%
                c(
                  "utm",
                  "wgs84"
                )
            ) {
              crs_mode <- "utm"
            }


            if (format_value %in%
              c(
                "kml",
                "kmz"
              )
            ) {
              crs_mode <- "wgs84"
            }


            write_basin_export(
              basin = basin_now,
              format = format_value,
              target_file = file,
              basin_label = shiny::isolate(
                basin_label_result()
              ),
              basin_source = shiny::isolate(
                basin_source_result()
              ),
              crs_mode = crs_mode
            )""",
    "vector handler CRS",
)

p.write_text(text, encoding="utf-8")


# ==============================================================
# R/morfometria.R
# ==============================================================
p = Path("R/morfometria.R")
text = p.read_text(encoding="utf-8")

text = replace_once(
    text,
    "# v37: MORFOMETRIA CONSOLIDADA + LAMINA HIPSOMETRICA ESTABLE",
    "# v38: MORFOMETRIA CONSOLIDADA + EXPORTACION DEM WGS84/UTM",
    "morfometria header",
)

wgs_helpers = r'''
  dem_basin_wgs84_for_download <- function(
      dem_basin,
      basin
  ) {

    if (
      is.null(dem_basin) ||
      is.null(basin)
    ) {
      stop(
        "No hay DEM ni cuenca disponibles para exportar."
      )
    }

    dem_crs <- sf::st_crs(
      terra::crs(
        dem_basin
      )
    )

    if (is.na(
      dem_crs
    )) {
      stop(
        "El DEM activo no tiene un CRS válido."
      )
    }

    target_crs <- sf::st_crs(
      4326
    )

    dem_wgs84 <- if (isTRUE(
      dem_crs == target_crs
    )) {
      dem_basin
    } else {
      terra::project(
        dem_basin,
        "EPSG:4326",
        method = "bilinear",
        threads = TRUE
      )
    }

    basin_wgs84 <- sf::st_transform(
      sf::st_make_valid(
        basin
      ),
      4326
    )

    basin_wgs84 <- basin_wgs84[
      !sf::st_is_empty(
        basin_wgs84
      ),
      ,
      drop = FALSE
    ]

    if (nrow(
      basin_wgs84
    ) == 0L) {
      stop(
        "La cuenca WGS84 está vacía."
      )
    }

    dem_wgs84 <- terra::crop(
      dem_wgs84,
      terra::vect(
        basin_wgs84
      ),
      snap = "out"
    )

    dem_wgs84 <- terra::mask(
      dem_wgs84,
      terra::vect(
        basin_wgs84
      )
    )

    names(
      dem_wgs84
    ) <- "FABDEM_m"

    dem_wgs84
  }


  dem_a3_wgs84_for_download <- function(
      basin,
      map_crs,
      job_id
  ) {

    map_poly <- a3_extent_polygon(
      basin = basin,
      map_crs = map_crs
    )

    map_poly_wgs84 <- sf::st_transform(
      map_poly,
      4326
    )

    tile_index <- get_dem_tile_index()

    selected <- select_dem_tiles(
      basin = map_poly_wgs84,
      tile_index = tile_index
    )

    paths <- vapply(
      file.path(
        DEM_DIR,
        as.character(selected$RELATIVE_PATH)
      ),
      runtime_cache_file,
      character(1)
    )

    missing <- paths[
      !file.exists(
        paths
      )
    ]

    if (length(
      missing
    ) > 0L) {
      stop(
        paste0(
          "Falta una tesela FABDEM necesaria para el DEM A3:\n",
          missing[1]
        )
      )
    }

    if (length(
      paths
    ) == 1L) {
      dem_source <- terra::rast(
        paths
      )
    } else {
      vrt_file <- file.path(
        TERRA_TEMP,
        paste0(
          "morph_dem_a3_wgs84_",
          job_id,
          ".vrt"
        )
      )

      dem_source <- terra::vrt(
        paths,
        filename = vrt_file,
        overwrite = TRUE
      )
    }

    source_crs <- sf::st_crs(
      terra::crs(
        dem_source
      )
    )

    if (is.na(
      source_crs
    )) {
      stop(
        "Las teselas FABDEM no tienen un CRS válido."
      )
    }

    source_window <- sf::st_transform(
      map_poly_wgs84,
      source_crs
    )

    dem_crop <- terra::crop(
      dem_source,
      terra::vect(
        source_window
      ),
      snap = "out"
    )

    target_crs <- sf::st_crs(
      4326
    )

    dem_wgs84 <- if (isTRUE(
      source_crs == target_crs
    )) {
      dem_crop
    } else {
      terra::project(
        dem_crop,
        "EPSG:4326",
        method = "bilinear",
        threads = TRUE
      )
    }

    names(
      dem_wgs84
    ) <- "FABDEM_m"

    dem_wgs84
  }
'''

text = replace_once(
    text,
    "\n\n  # ==========================================================\n  # 4. GEOMETRIA",
    "\n\n" + wgs_helpers + "\n\n  # ==========================================================\n  # 4. GEOMETRIA",
    "insert WGS84 DEM helpers",
)

text = replace_once(
    text,
    """                  shiny::downloadButton(
                    ns(
                      "descargar_dem_cuenca_utm"
                    ),
                    "DEM recortado UTM",
                    class = "btn-default btn-sm"
                  ),
                  shiny::downloadButton(
                    ns(
                      "descargar_dem_utm"
                    ),
                    "DEM mosaico UTM",
                    class = "btn-default btn-sm"
                  ),""",
    """                  shiny::tags$span(
                    "CRS DEM:"
                  ),
                  shiny::selectInput(
                    ns(
                      "crs_descarga_dem"
                    ),
                    label = NULL,
                    choices = c(
                      "UTM automática (m)" = "utm",
                      "WGS84 / FABDEM (grados)" = "wgs84"
                    ),
                    selected = "utm",
                    width = "220px"
                  ),
                  shiny::downloadButton(
                    ns(
                      "descargar_dem_cuenca_utm"
                    ),
                    "DEM recortado",
                    class = "btn-default btn-sm"
                  ),
                  shiny::downloadButton(
                    ns(
                      "descargar_dem_utm"
                    ),
                    "DEM mosaico A3",
                    class = "btn-default btn-sm"
                  ),""",
    "DEM CRS UI",
)

rec_handler = r'''        output$descargar_dem_cuenca_utm <- shiny::downloadHandler(
          filename = function() {

            x_download <- shiny::isolate(
              result()
            )

            crs_mode <- shiny::isolate(
              input$crs_descarga_dem
            )

            if (
              is.null(crs_mode) ||
              !crs_mode %in% c("utm", "wgs84")
            ) {
              crs_mode <- "utm"
            }

            if (identical(
              crs_mode,
              "utm"
            )) {
              epsg_value <- if (
                !is.null(x_download) &&
                !is.null(x_download$geom) &&
                !is.null(x_download$geom$epsg)
              ) {
                as.integer(
                  x_download$geom$epsg
                )
              } else {
                NA_integer_
              }

              epsg_tag <- if (is.finite(
                epsg_value
              )) {
                paste0(
                  "EPSG",
                  epsg_value,
                  "_"
                )
              } else {
                ""
              }

              prefix <- "FABDEM_CUENCA_UTM_"
            } else {
              epsg_tag <- "EPSG4326_"
              prefix <- "FABDEM_CUENCA_WGS84_"
            }

            paste0(
              prefix,
              epsg_tag,
              format(
                Sys.time(),
                "%Y%m%d_%H%M%S"
              ),
              ".tif"
            )
          },
          contentType = "image/tiff",
          content = function(file) {

            x_download <- shiny::isolate(
              result()
            )

            if (
              is.null(x_download) ||
              is.null(x_download$dem) ||
              is.null(x_download$basin_sf)
            ) {
              stop(
                "No hay un DEM de cuenca listo para descargar."
              )
            }

            crs_mode <- shiny::isolate(
              input$crs_descarga_dem
            )

            if (
              is.null(crs_mode) ||
              !crs_mode %in% c("utm", "wgs84")
            ) {
              crs_mode <- "utm"
            }

            if (
              identical(
                crs_mode,
                "utm"
              ) &&
              (
                is.null(x_download$geom) ||
                is.null(x_download$geom$epsg)
              )
            ) {
              stop(
                "No se pudo determinar el CRS UTM de la cuenca."
              )
            }

            dem_export <- shiny::withProgress(
              message = if (identical(
                crs_mode,
                "utm"
              )) {
                "Preparando DEM recortado en UTM..."
              } else {
                "Preparando DEM recortado en WGS84..."
              },
              value = 0.25,
              {
                out <- if (identical(
                  crs_mode,
                  "utm"
                )) {
                  dem_basin_utm_for_download(
                    dem_basin = x_download$dem,
                    basin = x_download$basin_sf,
                    utm_epsg = x_download$geom$epsg
                  )
                } else {
                  dem_basin_wgs84_for_download(
                    dem_basin = x_download$dem,
                    basin = x_download$basin_sf
                  )
                }

                shiny::setProgress(
                  value = 0.80,
                  detail = "Escribiendo GeoTIFF..."
                )

                out
              }
            )

            terra::writeRaster(
              dem_export,
              file,
              overwrite = TRUE,
              datatype = "FLT4S",
              NAflag = -9999,
              gdal = c(
                "COMPRESS=DEFLATE",
                "PREDICTOR=3",
                "TILED=YES",
                "BIGTIFF=IF_SAFER"
              )
            )

            if (
              !file.exists(file) ||
              !is.finite(file.info(file)$size) ||
              file.info(file)$size <= 0
            ) {
              stop(
                "El GeoTIFF recortado de la cuenca no pudo generarse correctamente."
              )
            }
          }
        )

        shiny::outputOptions(
          output,
          "descargar_dem_cuenca_utm",
          suspendWhenHidden = FALSE
        )


'''

pattern_rec = re.compile(
    r"        output\$descargar_dem_cuenca_utm <- shiny::downloadHandler\(.*?        output\$descargar_dem_utm <- shiny::downloadHandler\(",
    re.S,
)
m = pattern_rec.search(text)
if not m:
    raise SystemExit("DEM recortado handler block not found")
text = text[: m.start()] + rec_handler + "        output$descargar_dem_utm <- shiny::downloadHandler(" + text[m.end() :]

mosaic_handler = r'''        output$descargar_dem_utm <- shiny::downloadHandler(
          filename = function() {

            x_download <- shiny::isolate(
              result()
            )

            crs_mode <- shiny::isolate(
              input$crs_descarga_dem
            )

            if (
              is.null(crs_mode) ||
              !crs_mode %in% c("utm", "wgs84")
            ) {
              crs_mode <- "utm"
            }

            if (identical(
              crs_mode,
              "utm"
            )) {
              epsg_value <- if (
                !is.null(x_download) &&
                !is.null(x_download$geom) &&
                !is.null(x_download$geom$epsg)
              ) {
                as.integer(
                  x_download$geom$epsg
                )
              } else {
                NA_integer_
              }

              epsg_tag <- if (is.finite(
                epsg_value
              )) {
                paste0(
                  "EPSG",
                  epsg_value,
                  "_"
                )
              } else {
                ""
              }

              prefix <- "FABDEM_MOSAICO_A3_UTM_"
            } else {
              epsg_tag <- "EPSG4326_"
              prefix <- "FABDEM_MOSAICO_A3_WGS84_"
            }

            paste0(
              prefix,
              epsg_tag,
              format(
                Sys.time(),
                "%Y%m%d_%H%M%S"
              ),
              ".tif"
            )
          },
          contentType = "image/tiff",
          content = function(file) {

            x_download <- shiny::isolate(
              result()
            )

            if (
              is.null(x_download) ||
              is.null(x_download$dem) ||
              is.null(x_download$basin_sf)
            ) {
              stop(
                "No hay un DEM morfométrico listo para descargar."
              )
            }

            crs_mode <- shiny::isolate(
              input$crs_descarga_dem
            )

            if (
              is.null(crs_mode) ||
              !crs_mode %in% c("utm", "wgs84")
            ) {
              crs_mode <- "utm"
            }

            if (
              identical(
                crs_mode,
                "utm"
              ) &&
              (
                is.null(x_download$geom) ||
                is.null(x_download$geom$epsg)
              )
            ) {
              stop(
                "No se pudo determinar el CRS UTM de la cuenca."
              )
            }

            map_crs <- sf::st_crs(
              terra::crs(
                x_download$dem
              )
            )

            if (is.na(
              map_crs
            )) {
              stop(
                "El DEM activo no tiene un CRS válido."
              )
            }

            job_id <- paste0(
              format(
                Sys.time(),
                "%Y%m%d_%H%M%S"
              ),
              "_",
              sample.int(
                1e8,
                1L
              )
            )

            dem_export <- shiny::withProgress(
              message = if (identical(
                crs_mode,
                "utm"
              )) {
                "Preparando DEM A3 en UTM..."
              } else {
                "Preparando DEM A3 en WGS84..."
              },
              value = 0.15,
              {
                out <- if (identical(
                  crs_mode,
                  "utm"
                )) {
                  dem_a3_utm_for_download(
                    basin = x_download$basin_sf,
                    map_crs = map_crs,
                    utm_epsg = x_download$geom$epsg,
                    job_id = job_id
                  )
                } else {
                  dem_a3_wgs84_for_download(
                    basin = x_download$basin_sf,
                    map_crs = map_crs,
                    job_id = job_id
                  )
                }

                shiny::setProgress(
                  value = 0.80,
                  detail = "Escribiendo GeoTIFF..."
                )

                out
              }
            )

            terra::writeRaster(
              dem_export,
              file,
              overwrite = TRUE,
              datatype = "FLT4S",
              NAflag = -9999,
              gdal = c(
                "COMPRESS=DEFLATE",
                "PREDICTOR=3",
                "TILED=YES",
                "BIGTIFF=IF_SAFER"
              )
            )

            if (
              !file.exists(file) ||
              !is.finite(file.info(file)$size) ||
              file.info(file)$size <= 0
            ) {
              stop(
                "El GeoTIFF del DEM no pudo generarse correctamente."
              )
            }
          }
        )

        shiny::outputOptions(
          output,
          "descargar_dem_utm",
          suspendWhenHidden = FALSE
        )


'''

pattern_mosaic = re.compile(
    r"        output\$descargar_dem_utm <- shiny::downloadHandler\(.*?        output\$descargar_mapa_png <- shiny::downloadHandler\(",
    re.S,
)
m = pattern_mosaic.search(text)
if not m:
    raise SystemExit("DEM mosaic handler block not found")
text = text[: m.start()] + mosaic_handler + "        output$descargar_mapa_png <- shiny::downloadHandler(" + text[m.end() :]

p.write_text(text, encoding="utf-8")


# Final textual guards.
del_text = Path("R/delimitacion.R").read_text(encoding="utf-8")
mor_text = Path("R/morfometria.R").read_text(encoding="utf-8")

for token in [
    "crs_exportacion",
    "project_basin_export_crs",
    'crs_mode = "utm"',
]:
    if token not in del_text:
        raise SystemExit(f"Missing in delimitacion.R: {token}")

for token in [
    "crs_descarga_dem",
    "dem_basin_wgs84_for_download",
    "dem_a3_wgs84_for_download",
    "FABDEM_CUENCA_WGS84_",
    "FABDEM_MOSAICO_A3_WGS84_",
]:
    if token not in mor_text:
        raise SystemExit(f"Missing in morfometria.R: {token}")

print("CRS export patch applied successfully")

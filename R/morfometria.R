# ============================================================
# R/morfometria.R
#
# MODULO 02: MORFOMETRIA
# v35: MORFOMETRIA + DEM A3 UTM + SIMBOLOGIA STRAHLER REVISADA
# ============================================================
#
# Calcula de forma defendible con:
#   - basin() de Delimitacion o de una geometria importada
#   - teselas FABDEM originales de Runtime/dem
#   - contexto hidrologico solo cuando la cuenca fue delimitada por la app
#
# La ejecucion se divide en etapas encadenadas:
#   1) geometria
#   2) FABDEM / relieve
#   3) recorrido hidraulico y cauce principal
#   4) tiempos de concentracion
#   5) red de drenaje y Strahler
#
# Cada etapa publica sus resultados antes de iniciar la siguiente.
# Un fallo tardio no elimina los resultados ya calculados.
#
# IMPORTANTE: no vuelve a construir el reverse cache. Reutiliza
# por referencia el cache ya cargado en Delimitacion.
# ============================================================


morfometria <- local({

  # ==========================================================
  # 1. FORMATOS
  # ==========================================================

  fmt <- function(
      x,
      digits = 2
  ) {

    if (
      length(x) == 0L ||
      is.na(x) ||
      !is.finite(x)
    ) {
      return("NA")
    }


    format(
      round(
        as.numeric(x),
        digits
      ),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE,
      nsmall = digits
    )
  }


  metric_row <- function(
      group,
      parameter,
      symbol,
      value,
      unit,
      method
  ) {

    data.frame(
      GRUPO = group,
      PARAMETRO = parameter,
      SIMBOLO = symbol,
      VALOR = value,
      UNIDAD = unit,
      METODO = method,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }


  scalar_global <- function(
      r,
      statistic
  ) {

    statistic <- match.arg(
      statistic,
      c(
        "min",
        "max",
        "mean",
        "median"
      )
    )


    # terra::global() usa evaluacion no estandar para `fun`.
    # Pasar una variable llamada `fun` desde este wrapper puede
    # terminar intentando ejecutar literalmente una funcion
    # llamada `fun`. Se usan llamadas explicitas por estadistico.
    x <- switch(
      statistic,
      min = terra::global(
        r,
        "min",
        na.rm = TRUE
      ),
      max = terra::global(
        r,
        "max",
        na.rm = TRUE
      ),
      mean = terra::global(
        r,
        "mean",
        na.rm = TRUE
      ),
      median = terra::global(
        r,
        stats::median,
        na.rm = TRUE
      )
    )


    value <- suppressWarnings(
      as.numeric(
        x[1, 1]
      )
    )


    if (
      length(value) != 1L ||
      !is.finite(value)
    ) {
      return(
        NA_real_
      )
    }


    value
  }


  # ==========================================================
  # 2. INDICE DE TESELAS FABDEM
  # ==========================================================

  list_dem_tiles <- function() {

    if (!dir.exists(
      DEM_DIR
    )) {
      stop(
        paste0(
          "No existe la carpeta de teselas FABDEM:\n",
          DEM_DIR
        )
      )
    }


    files <- list.files(
      DEM_DIR,
      pattern = "\\.(tif|tiff)$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )


    if (length(files) == 0L) {
      stop(
        paste0(
          "No se encontraron TIF/TIFF en:\n",
          DEM_DIR
        )
      )
    }


    sort(
      normalizePath(
        files,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }


  relative_to_dem <- function(path) {

    root <- normalizePath(
      DEM_DIR,
      winslash = "/",
      mustWork = TRUE
    )


    x <- normalizePath(
      path,
      winslash = "/",
      mustWork = TRUE
    )


    prefix <- paste0(
      root,
      "/"
    )


    if (!startsWith(
      tolower(x),
      tolower(prefix)
    )) {
      stop(
        "Tesela fuera de DEM_DIR."
      )
    }


    substring(
      x,
      nchar(prefix) + 1L
    )
  }


  build_dem_tile_index <- function(tile_files) {

    if (length(tile_files) == 0L) {
      stop("No hay teselas FABDEM para indexar.")
    }


    # --------------------------------------------------------
    # CRS de referencia de las teselas
    # --------------------------------------------------------
    #
    # IMPORTANTE:
    # No construir el bbox mediante e$xmin/e$xmax + st_bbox().
    # Ese patron ya produjo el error:
    #   !anyNA(x) is not TRUE
    # con estas teselas FABDEM en una etapa previa del proyecto.
    # Se usan los accessors de terra y un st_polygon explicito.
    # --------------------------------------------------------

    r0 <- terra::rast(
      tile_files[1]
    )


    tile_crs_ref <- terra::crs(
      r0
    )


    if (
      is.na(tile_crs_ref) ||
      !nzchar(tile_crs_ref)
    ) {
      stop(
        paste0(
          "La primera tesela FABDEM no tiene CRS valido:\n",
          tile_files[1]
        )
      )
    }


    tile_sf_crs <- sf::st_crs(
      tile_crs_ref
    )


    if (is.na(tile_sf_crs)) {
      stop(
        paste0(
          "sf no pudo interpretar el CRS de las teselas FABDEM.\n",
          "Primera tesela:\n",
          tile_files[1]
        )
      )
    }


    rm(
      r0
    )


    out <- vector(
      "list",
      length(tile_files)
    )


    for (i in seq_along(
      tile_files
    )) {

      r <- tryCatch(
        terra::rast(
          tile_files[i]
        ),
        error = function(e) {
          stop(
            paste0(
              "No se pudo abrir la tesela FABDEM:\n",
              tile_files[i],
              "\n\n",
              conditionMessage(e)
            )
          )
        }
      )


      tile_crs_i <- terra::crs(
        r
      )


      if (
        is.na(tile_crs_i) ||
        !nzchar(tile_crs_i)
      ) {
        stop(
          paste0(
            "Tesela FABDEM sin CRS:\n",
            tile_files[i]
          )
        )
      }


      xmn <- terra::xmin(
        r
      )

      xmx <- terra::xmax(
        r
      )

      ymn <- terra::ymin(
        r
      )

      ymx <- terra::ymax(
        r
      )


      coords <- c(
        xmn,
        xmx,
        ymn,
        ymx
      )


      if (
        length(coords) != 4L ||
        anyNA(coords) ||
        any(
          !is.finite(
            coords
          )
        ) ||
        xmn >= xmx ||
        ymn >= ymx
      ) {
        stop(
          paste0(
            "Extension invalida en tesela FABDEM:\n",
            tile_files[i],
            "\n",
            "xmin=", xmn,
            " xmax=", xmx,
            " ymin=", ymn,
            " ymax=", ymx
          )
        )
      }


      ring <- matrix(
        c(
          xmn, ymn,
          xmx, ymn,
          xmx, ymx,
          xmn, ymx,
          xmn, ymn
        ),
        ncol = 2,
        byrow = TRUE
      )


      footprint <- sf::st_sfc(
        sf::st_polygon(
          list(
            ring
          )
        ),
        crs = tile_sf_crs
      )


      footprint <- sf::st_transform(
        footprint,
        4326
      )


      out[[i]] <- sf::st_sf(
        TILE_ID = i,
        TILE_NAME = basename(
          tile_files[i]
        ),
        RELATIVE_PATH = relative_to_dem(
          tile_files[i]
        ),
        geometry = footprint
      )


      rm(
        r,
        ring,
        footprint
      )
    }


    index <- do.call(
      rbind,
      out
    )


    if (
      nrow(index) !=
        length(tile_files)
    ) {
      stop(
        "El numero de huellas FABDEM creadas no coincide con el numero de teselas."
      )
    }


    attr(
      index,
      "tile_set"
    ) <- sort(
      index$RELATIVE_PATH
    )


    attr(
      index,
      "morph_tile_index_version"
    ) <- "v3_explicit_polygon"


    saveRDS(
      index,
      DEM_TILE_INDEX_RDS
    )


    index
  }

  get_dem_tile_index <- function() {
    index_file <- runtime_cache_file(
      DEM_TILE_INDEX_RDS
    )

    index <- readRDS(
      index_file
    )

    if (
      !inherits(index, "sf") ||
      !all(c("TILE_NAME", "RELATIVE_PATH") %in% names(index)) ||
      !identical(
        attr(index, "morph_tile_index_version"),
        "v3_explicit_polygon"
      )
    ) {
      stop("El indice remoto de teselas FABDEM no es compatible.")
    }

    index
  }


  select_dem_tiles <- function(
      basin,
      tile_index
  ) {

    basin_4326 <- sf::st_transform(
      sf::st_make_valid(
        basin
      ),
      4326
    )


    hits <- lengths(
      sf::st_intersects(
        tile_index,
        basin_4326
      )
    ) > 0L


    selected <- tile_index[
      hits,
      ,
      drop = FALSE
    ]


    if (nrow(selected) == 0L) {
      stop(
        "Ninguna tesela FABDEM intersecta la cuenca delimitada."
      )
    }


    selected
  }


  # ==========================================================
  # 3. DEM DE LA CUENCA
  # ==========================================================

  dem_for_basin <- function(
      basin,
      selected_tiles,
      job_id
  ) {

    paths <- vapply(
      file.path(
        DEM_DIR,
        as.character(selected_tiles$RELATIVE_PATH)
      ),
      runtime_cache_file,
      character(1)
    )


    missing <- paths[
      !file.exists(
        paths
      )
    ]


    if (length(missing) > 0L) {
      stop(
        paste0(
          "Falta una tesela FABDEM seleccionada:\n",
          missing[1]
        )
      )
    }


    if (length(paths) == 1L) {

      dem <- terra::rast(
        paths
      )

    } else {

      vrt_file <- file.path(
        TERRA_TEMP,
        paste0(
          "morph_dem_",
          job_id,
          ".vrt"
        )
      )


      dem <- terra::vrt(
        paths,
        filename = vrt_file,
        overwrite = TRUE
      )
    }


    dem_crs <- terra::crs(
      dem,
      proj = TRUE
    )


    basin_dem_crs <- sf::st_transform(
      basin,
      sf::st_crs(
        dem_crs
      )
    )


    basin_v <- terra::vect(
      basin_dem_crs
    )


    dem_crop <- terra::crop(
      dem,
      basin_v,
      snap = "out"
    )


    dem_mask <- terra::mask(
      dem_crop,
      basin_v
    )


    names(
      dem_mask
    ) <- "FABDEM_m"


    dem_mask
  }



  # ==========================================================
  # 3B. EXTENSION A3 Y DEM CONTEXTUAL PARA EXPORTACION
  # ==========================================================

  a3_map_limits <- function(
      basin,
      map_crs
  ) {

    basin_plot <- sf::st_transform(
      sf::st_make_valid(
        basin
      ),
      map_crs
    )

    bb_basin <- sf::st_bbox(
      basin_plot
    )

    xmn <- as.numeric(
      bb_basin["xmin"]
    )
    xmx <- as.numeric(
      bb_basin["xmax"]
    )
    ymn <- as.numeric(
      bb_basin["ymin"]
    )
    ymx <- as.numeric(
      bb_basin["ymax"]
    )

    dx <- xmx - xmn
    dy <- ymx - ymn

    x_pad <- if (
      is.finite(dx) && dx > 0
    ) {
      0.07 * dx
    } else {
      0
    }

    y_pad <- if (
      is.finite(dy) && dy > 0
    ) {
      0.06 * dy
    } else {
      0
    }

    map_xlim <- c(
      xmn - x_pad,
      xmx + x_pad
    )

    map_ylim <- c(
      ymn - y_pad,
      ymx + y_pad
    )

    map_asp <- 1

    if (
      !is.na(
        map_crs
      ) &&
      isTRUE(
        sf::st_is_longlat(
          map_crs
        )
      )
    ) {

      mid_lat <- mean(
        map_ylim
      )

      cos_lat <- cos(
        mid_lat * pi / 180
      )

      if (
        is.finite(cos_lat) &&
        cos_lat > 0.05
      ) {
        map_asp <- 1 / cos_lat
      }
    }

    # Se replica el dispositivo y el panel principal de la lámina A3.
    # plot.window() puede ampliar X o Y para mantener la proporción
    # cartográfica. Esos límites finales son la extensión que se exporta.
    tmp_pdf <- tempfile(
      pattern = "morph_a3_extent_",
      fileext = ".pdf"
    )

    grDevices::pdf(
      file = tmp_pdf,
      width = 420 / 25.4,
      height = 297 / 25.4,
      paper = "special",
      onefile = TRUE
    )

    device_open <- TRUE

    on.exit(
      {
        if (isTRUE(
          device_open
        )) {
          grDevices::dev.off()
        }
        unlink(
          tmp_pdf,
          force = TRUE
        )
      },
      add = TRUE
    )

    graphics::par(
      oma = c(
        0,
        0,
        0,
        0
      ),
      bg = "white"
    )

    graphics::plot.new()

    graphics::par(
      fig = c(
        0.025,
        0.79,
        0.07,
        0.97
      ),
      mar = c(
        3.3,
        3.5,
        1.0,
        1.0
      ),
      new = TRUE,
      xpd = FALSE
    )

    graphics::plot.new()

    graphics::plot.window(
      xlim = map_xlim,
      ylim = map_ylim,
      xaxs = "i",
      yaxs = "i",
      asp = map_asp
    )

    usr <- graphics::par(
      "usr"
    )

    grDevices::dev.off()
    device_open <- FALSE

    unlink(
      tmp_pdf,
      force = TRUE
    )

    list(
      xlim = as.numeric(
        usr[1:2]
      ),
      ylim = as.numeric(
        usr[3:4]
      ),
      crs = map_crs
    )
  }


  a3_extent_polygon <- function(
      basin,
      map_crs
  ) {

    limits <- a3_map_limits(
      basin = basin,
      map_crs = map_crs
    )

    ring <- matrix(
      c(
        limits$xlim[1], limits$ylim[1],
        limits$xlim[2], limits$ylim[1],
        limits$xlim[2], limits$ylim[2],
        limits$xlim[1], limits$ylim[2],
        limits$xlim[1], limits$ylim[1]
      ),
      ncol = 2,
      byrow = TRUE
    )

    sf::st_sfc(
      sf::st_polygon(
        list(
          ring
        )
      ),
      crs = map_crs
    )
  }


  dem_a3_utm_for_download <- function(
      basin,
      map_crs,
      utm_epsg,
      job_id
  ) {

    if (
      length(utm_epsg) != 1L ||
      is.na(utm_epsg) ||
      !is.finite(
        as.numeric(
          utm_epsg
        )
      )
    ) {
      stop(
        "No se pudo determinar el CRS UTM de la cuenca."
      )
    }

    utm_epsg <- as.integer(
      utm_epsg
    )

    map_poly <- a3_extent_polygon(
      basin = basin,
      map_crs = map_crs
    )

    map_poly_utm <- sf::st_transform(
      map_poly,
      utm_epsg
    )

    bb_utm <- sf::st_bbox(
      map_poly_utm
    )

    utm_rect <- sf::st_as_sfc(
      bb_utm
    )

    tile_index <- get_dem_tile_index()

    selected <- select_dem_tiles(
      basin = utm_rect,
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
          "morph_dem_a3_",
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

    # El rectángulo UTM final se transforma al CRS nativo únicamente
    # para reducir la lectura de las teselas antes de reproyectar.
    source_window <- sf::st_transform(
      utm_rect,
      source_crs
    )

    dem_source_crop <- terra::crop(
      dem_source,
      terra::vect(
        source_window
      ),
      snap = "out"
    )

    width_m <- as.numeric(
      bb_utm["xmax"] -
        bb_utm["xmin"]
    )

    height_m <- as.numeric(
      bb_utm["ymax"] -
        bb_utm["ymin"]
    )

    if (
      !is.finite(width_m) ||
      !is.finite(height_m) ||
      width_m <= 0 ||
      height_m <= 0
    ) {
      stop(
        "La extensión A3 calculada para el DEM es inválida."
      )
    }

    # FABDEM es nominalmente de 30 m. Se conserva aproximadamente esa
    # resolución, pero la extensión del raster se fuerza al cuadrante
    # UTM completo derivado de la lámina A3.
    target_res_m <- 30

    target_ncols <- max(
      1L,
      as.integer(
        ceiling(
          width_m / target_res_m
        )
      )
    )

    target_nrows <- max(
      1L,
      as.integer(
        ceiling(
          height_m / target_res_m
        )
      )
    )

    target <- terra::rast(
      nrows = target_nrows,
      ncols = target_ncols,
      xmin = as.numeric(
        bb_utm["xmin"]
      ),
      xmax = as.numeric(
        bb_utm["xmax"]
      ),
      ymin = as.numeric(
        bb_utm["ymin"]
      ),
      ymax = as.numeric(
        bb_utm["ymax"]
      ),
      crs = paste0(
        "EPSG:",
        utm_epsg
      )
    )

    dem_utm <- terra::project(
      dem_source_crop,
      target,
      method = "bilinear",
      threads = TRUE
    )

    names(
      dem_utm
    ) <- "FABDEM_m"

    dem_utm
  }


  dem_basin_utm_for_download <- function(
      dem_basin,
      basin,
      utm_epsg
  ) {

    if (
      is.null(dem_basin) ||
      is.null(basin)
    ) {
      stop(
        "No hay DEM ni cuenca disponibles para exportar."
      )
    }

    if (
      length(utm_epsg) != 1L ||
      is.na(utm_epsg) ||
      !is.finite(
        as.numeric(
          utm_epsg
        )
      )
    ) {
      stop(
        "No se pudo determinar el CRS UTM de la cuenca."
      )
    }

    utm_epsg <- as.integer(
      utm_epsg
    )

    basin_utm <- sf::st_transform(
      sf::st_make_valid(
        basin
      ),
      utm_epsg
    )

    basin_utm <- basin_utm[
      !sf::st_is_empty(
        basin_utm
      ),
      ,
      drop = FALSE
    ]

    if (nrow(basin_utm) == 0L) {
      stop(
        "La cuenca UTM está vacía."
      )
    }

    bb_utm <- sf::st_bbox(
      basin_utm
    )

    width_m <- as.numeric(
      bb_utm["xmax"] -
        bb_utm["xmin"]
    )

    height_m <- as.numeric(
      bb_utm["ymax"] -
        bb_utm["ymin"]
    )

    if (
      !is.finite(width_m) ||
      !is.finite(height_m) ||
      width_m <= 0 ||
      height_m <= 0
    ) {
      stop(
        "La extensión UTM de la cuenca es inválida."
      )
    }

    # FABDEM es nominalmente de 30 m. La salida se lleva a una
    # grilla UTM de aproximadamente 30 m y después se enmascara
    # exactamente con la divisoria activa.
    target_res_m <- 30

    target_ncols <- max(
      1L,
      as.integer(
        ceiling(
          width_m / target_res_m
        )
      )
    )

    target_nrows <- max(
      1L,
      as.integer(
        ceiling(
          height_m / target_res_m
        )
      )
    )

    target <- terra::rast(
      nrows = target_nrows,
      ncols = target_ncols,
      xmin = as.numeric(
        bb_utm["xmin"]
      ),
      xmax = as.numeric(
        bb_utm["xmax"]
      ),
      ymin = as.numeric(
        bb_utm["ymin"]
      ),
      ymax = as.numeric(
        bb_utm["ymax"]
      ),
      crs = paste0(
        "EPSG:",
        utm_epsg
      )
    )

    dem_utm <- terra::project(
      dem_basin,
      target,
      method = "bilinear",
      threads = TRUE
    )

    dem_utm <- terra::mask(
      dem_utm,
      terra::vect(
        basin_utm
      )
    )

    names(
      dem_utm
    ) <- "FABDEM_m"

    dem_utm
  }


  # ==========================================================
  # 4. GEOMETRIA
  # ==========================================================

  geometry_metrics <- function(
      basin,
      outlet
  ) {

    basin <- sf::st_make_valid(
      basin
    )


    basin <- basin[
      !sf::st_is_empty(
        basin
      ),
      ,
      drop = FALSE
    ]


    if (nrow(basin) == 0L) {
      stop(
        "La cuenca esta vacia."
      )
    }


    outlet_4326 <- sf::st_transform(
      outlet,
      4326
    )


    xy_out <- sf::st_coordinates(
      outlet_4326
    )[1, ]


    epsg <- utm_epsg_point(
      lon = xy_out[1],
      lat = xy_out[2]
    )


    basin_utm <- sf::st_transform(
      basin,
      epsg
    )


    outlet_utm <- sf::st_transform(
      outlet,
      epsg
    )


    basin_union <- sf::st_union(
      sf::st_geometry(
        basin_utm
      )
    )


    area_km2 <- as.numeric(
      sf::st_area(
        basin_union
      )
    ) / 1e6


    perimeter_km <- as.numeric(
      sf::st_length(
        sf::st_boundary(
          basin_union
        )
      )
    ) / 1000


    hull <- sf::st_convex_hull(
      basin_union
    )


    hull_xy <- sf::st_coordinates(
      hull
    )


    out_xy <- sf::st_coordinates(
      outlet_utm
    )[1, ]


    distances_m <- sqrt(
      (
        hull_xy[, "X"] -
          out_xy[1]
      )^2 +
        (
          hull_xy[, "Y"] -
            out_xy[2]
        )^2
    )


    far_id <- which.max(
      distances_m
    )


    basin_length_km <- distances_m[
      far_id
    ] / 1000


    far_x <- hull_xy[
      far_id,
      "X"
    ]


    far_y <- hull_xy[
      far_id,
      "Y"
    ]


    dx <- far_x -
      out_xy[1]


    dy <- far_y -
      out_xy[2]


    azimuth_deg <- (
      atan2(
        dx,
        dy
      ) *
        180 /
        pi +
        360
    ) %%
      360


    mean_width_km <- area_km2 /
      basin_length_km


    deq_km <- 2 *
      sqrt(
        area_km2 /
          pi
      )


    ff <- area_km2 /
      basin_length_km^2


    re <- deq_km /
      basin_length_km


    rc <- 4 *
      pi *
      area_km2 /
      perimeter_km^2


    kc <- perimeter_km /
      (
        2 *
          sqrt(
            pi *
              area_km2
          )
      )


    list(
      epsg = epsg,
      area_km2 = area_km2,
      perimeter_km = perimeter_km,
      basin_length_km = basin_length_km,
      mean_width_km = mean_width_km,
      equivalent_diameter_km = deq_km,
      orientation_deg = azimuth_deg,
      form_factor = ff,
      elongation_ratio = re,
      circularity_ratio = rc,
      compactness_coefficient = kc,
      remote_xy_utm = c(
        far_x,
        far_y
      )
    )
  }



  geometry_metrics_imported <- function(
      basin
  ) {

    basin <- sf::st_make_valid(
      basin
    )

    basin <- basin[
      !sf::st_is_empty(
        basin
      ),
      ,
      drop = FALSE
    ]

    if (nrow(
      basin
    ) == 0L) {
      stop(
        "La cuenca importada esta vacia."
      )
    }

    basin_4326 <- sf::st_transform(
      basin,
      4326
    )

    center <- sf::st_point_on_surface(
      sf::st_union(
        sf::st_geometry(
          basin_4326
        )
      )
    )

    center_xy <- sf::st_coordinates(
      center
    )[1, ]

    epsg <- utm_epsg_point(
      lon = center_xy[1],
      lat = center_xy[2]
    )

    basin_utm <- sf::st_transform(
      basin,
      epsg
    )

    basin_union <- sf::st_union(
      sf::st_geometry(
        basin_utm
      )
    )

    area_km2 <- as.numeric(
      sf::st_area(
        basin_union
      )
    ) / 1e6

    perimeter_km <- as.numeric(
      sf::st_length(
        sf::st_boundary(
          basin_union
        )
      )
    ) / 1000

    deq_km <- 2 *
      sqrt(
        area_km2 /
          pi
      )

    rc <- 4 *
      pi *
      area_km2 /
      perimeter_km^2

    kc <- perimeter_km /
      (
        2 *
          sqrt(
            pi *
              area_km2
          )
      )

    list(
      epsg = epsg,
      area_km2 = area_km2,
      perimeter_km = perimeter_km,
      basin_length_km = NA_real_,
      mean_width_km = NA_real_,
      equivalent_diameter_km = deq_km,
      orientation_deg = NA_real_,
      form_factor = NA_real_,
      elongation_ratio = NA_real_,
      circularity_ratio = rc,
      compactness_coefficient = kc,
      remote_xy_utm = c(
        NA_real_,
        NA_real_
      )
    )
  }


  # ==========================================================
  # 5. RELIEVE
  # ==========================================================

  raster_sample <- function(
      r,
      max_n = MORPH_SAMPLE_MAX
  ) {

    valid_count <- terra::global(
      !is.na(r),
      fun = "sum",
      na.rm = TRUE
    )


    valid_count <- as.numeric(
      valid_count[1, 1]
    )


    if (
      !is.finite(valid_count) ||
      valid_count < 1
    ) {
      return(
        numeric(0)
      )
    }


    n <- min(
      as.integer(
        valid_count
      ),
      as.integer(
        max_n
      )
    )


    x <- terra::spatSample(
      r,
      size = n,
      method = "regular",
      na.rm = TRUE,
      values = TRUE,
      as.df = FALSE
    )


    as.numeric(
      x
    )
  }


  relief_metrics <- function(
      dem,
      geom
  ) {

    zmin <- scalar_global(
      dem,
      "min"
    )


    zmax <- scalar_global(
      dem,
      "max"
    )


    zmean <- scalar_global(
      dem,
      "mean"
    )


    zmedian <- scalar_global(
      dem,
      "median"
    )


    relief_m <- zmax -
      zmin


    slope_rad <- terra::terrain(
      dem,
      v = "slope",
      unit = "radians",
      neighbors = 8
    )


    slope_pct <- tan(
      slope_rad
    ) *
      100


    slope_mean_pct <- scalar_global(
      slope_pct,
      "mean"
    )


    slope_median_pct <- scalar_global(
      slope_pct,
      "median"
    )


    hi <- if (
      is.finite(relief_m) &&
      relief_m > 0
    ) {
      (
        zmean -
          zmin
      ) /
        relief_m
    } else {
      NA_real_
    }


    relief_ratio_m_km <- if (
      !is.null(
        geom$basin_length_km
      ) &&
      is.finite(
        geom$basin_length_km
      ) &&
      geom$basin_length_km > 0
    ) {
      relief_m /
        geom$basin_length_km
    } else {
      NA_real_
    }


    melton <- (
      relief_m /
        1000
    ) /
      sqrt(
        geom$area_km2
      )


    z_sample <- raster_sample(
      dem
    )


    slope_sample <- raster_sample(
      slope_pct
    )


    qz <- if (
      length(z_sample) > 0L
    ) {
      stats::quantile(
        z_sample,
        probs = c(
          0.10,
          0.25,
          0.75,
          0.90
        ),
        na.rm = TRUE,
        names = FALSE
      )
    } else {
      rep(
        NA_real_,
        4
      )
    }


    qs <- if (
      length(slope_sample) > 0L
    ) {
      stats::quantile(
        slope_sample,
        probs = c(
          0.10,
          0.50,
          0.90
        ),
        na.rm = TRUE,
        names = FALSE
      )
    } else {
      rep(
        NA_real_,
        3
      )
    }


    list(
      zmin = zmin,
      zmax = zmax,
      zmean = zmean,
      zmedian = zmedian,
      relief_m = relief_m,
      slope_mean_pct = slope_mean_pct,
      slope_median_pct = slope_median_pct,
      hypsometric_integral = hi,
      relief_ratio_m_km = relief_ratio_m_km,
      melton = melton,
      elevation_quantiles = qz,
      slope_quantiles = qs,
      elevation_sample = z_sample,
      slope_sample = slope_sample
    )
  }


  # ==========================================================
  # 6. RECORRIDO HIDRAULICO D8 Y CAUCE PRINCIPAL
  # ==========================================================

  outlet_cell_from_sf <- function(
      grid_template,
      outlet
  ) {

    grid_crs <- sf::st_crs(
      terra::crs(
        grid_template
      )
    )


    if (is.na(
      grid_crs
    )) {
      stop(
        "La grilla hidrologica no tiene CRS valido."
      )
    }


    outlet_grid <- sf::st_transform(
      outlet,
      grid_crs
    )


    xy <- sf::st_coordinates(
      outlet_grid
    )[1, ]


    cell <- terra::cellFromXY(
      grid_template,
      matrix(
        c(
          xy[1],
          xy[2]
        ),
        nrow = 1
      )
    )


    if (
      length(cell) != 1L ||
      is.na(cell)
    ) {
      stop(
        "No se pudo localizar el outlet en la grilla hidrologica."
      )
    }


    as.double(
      cell
    )
  }


  grid_metric_info <- function(
      grid_template
  ) {

    crs_sf <- sf::st_crs(
      terra::crs(
        grid_template
      )
    )


    if (is.na(
      crs_sf
    )) {
      stop(
        "La grilla hidrologica no tiene CRS valido."
      )
    }


    rr <- abs(
      terra::res(
        grid_template
      )
    )


    if (isTRUE(
      sf::st_is_longlat(
        crs_sf
      )
    )) {
      return(
        list(
          mode = "lonlat",
          dx = rr[1],
          dy = rr[2],
          ymax = terra::ymax(
            grid_template
          ),
          ncols = as.double(
            terra::ncol(
              grid_template
            )
          )
        )
      )
    }


    units_gdal <- tolower(
      as.character(
        crs_sf$units_gdal
      )
    )


    if (
      !grepl(
        "metre|meter|metres|meters|m$",
        units_gdal
      )
    ) {
      stop(
        paste0(
          "La grilla hidrologica usa un CRS proyectado con unidades no soportadas: ",
          units_gdal,
          "."
        )
      )
    }


    list(
      mode = "projected_m",
      dx = rr[1],
      dy = rr[2],
      ymax = NA_real_,
      ncols = as.double(
        terra::ncol(
          grid_template
        )
      )
    )
  }


  haversine_segment_m <- function(
      lon1,
      lat1,
      lon2,
      lat2
  ) {

    rad <- pi /
      180


    lon1 <- as.numeric(
      lon1
    ) * rad

    lat1 <- as.numeric(
      lat1
    ) * rad

    lon2 <- as.numeric(
      lon2
    ) * rad

    lat2 <- as.numeric(
      lat2
    ) * rad


    dlon <- lon2 -
      lon1

    dlat <- lat2 -
      lat1


    a <- sin(
      dlat /
        2
    )^2 +
      cos(
        lat1
      ) *
      cos(
        lat2
      ) *
      sin(
        dlon /
          2
      )^2


    a <- pmin(
      1,
      pmax(
        0,
        a
      )
    )


    2 *
      6371008.8 *
      asin(
        sqrt(
          a
        )
      )
  }


  reverse_step_length_m <- function(
      metric_info,
      receptor_cells,
      direction_id
  ) {

    diagonal <- direction_id %in%
      c(
        1L,
        3L,
        6L,
        8L
      )


    horizontal <- direction_id %in%
      c(
        1L,
        3L,
        4L,
        5L,
        6L,
        8L
      )


    vertical <- direction_id %in%
      c(
        1L,
        2L,
        3L,
        6L,
        7L,
        8L
      )


    if (identical(
      metric_info$mode,
      "projected_m"
    )) {

      dx <- if (horizontal) {
        metric_info$dx
      } else {
        0
      }


      dy <- if (vertical) {
        metric_info$dy
      } else {
        0
      }


      return(
        rep(
          sqrt(
            dx^2 +
              dy^2
          ),
          length(
            receptor_cells
          )
        )
      )
    }


    nc <- metric_info$ncols


    receptor_rows <- floor(
      (
        receptor_cells -
          1
      ) /
        nc
    ) +
      1


    receptor_lat <- metric_info$ymax -
      (
        receptor_rows -
          0.5
      ) *
        metric_info$dy


    parent_row_delta <- c(
      -1,
      -1,
      -1,
      0,
      0,
      1,
      1,
      1
    )[
      direction_id
    ]


    parent_col_delta <- c(
      -1,
      0,
      1,
      -1,
      1,
      -1,
      0,
      1
    )[
      direction_id
    ]


    parent_lat <- receptor_lat -
      parent_row_delta *
        metric_info$dy


    receptor_lon <- rep(
      0,
      length(
        receptor_cells
      )
    )


    parent_lon <- receptor_lon +
      parent_col_delta *
        metric_info$dx


    haversine_segment_m(
      receptor_lon,
      receptor_lat,
      parent_lon,
      parent_lat
    )
  }


  stream_values_global <- function(
      hydro,
      cells
  ) {

    if (length(
      cells
    ) == 0L) {
      return(
        numeric(
          0
        )
      )
    }


    nc <- as.double(
      terra::ncol(
        hydro$grid_template
      )
    )


    rows <- floor(
      (
        cells -
          1
      ) /
        nc
    ) +
      1


    cols <- (
      (
        cells -
          1
      ) %%
        nc
    ) +
      1


    stripe_ids <- floor(
      (
        rows -
          1
      ) /
        as.double(
          hydro$stripe_rows
        )
    ) +
      1


    out <- rep(
      NA_real_,
      length(
        cells
      )
    )


    for (sid in unique(
      stripe_ids
    )) {

      idx <- which(
        stripe_ids ==
          sid
      )


      sid_i <- as.integer(
        sid
      )


      stream_r <- load_stream_stripe(
        hydro$stream_cache,
        sid_i
      )


      row_start <- as.double(
        hydro$stream_cache$rows[[
          "ROW_START"
        ]][
          sid_i
        ]
      )


      local_rows <- rows[
        idx
      ] -
        row_start +
        1


      local_cells <- (
        local_rows -
          1
      ) *
        nc +
        cols[
          idx
        ]


      extracted <- terra::extract(
        stream_r,
        local_cells
      )


      out[
        idx
      ] <- as.numeric(
        last_data_column(
          extracted
        )
      )
    }


    out
  }


  farthest_upstream_cell <- function(
      hydro,
      outlet_cell,
      only_stream = FALSE,
      progress_fun = NULL
  ) {

    cache <- hydro$reverse_cache


    nc <- as.double(
      cache$metadata$ncols
    )


    nr <- as.double(
      cache$metadata$nrows
    )


    metric_info <- grid_metric_info(
      hydro$grid_template
    )


    queue_cells <- list(
      as.double(
        outlet_cell
      )
    )


    queue_dist <- list(
      0
    )


    queue_index <- 1L
    n_cells <- 1
    n_batches <- 0L


    far_cell <- as.double(
      outlet_cell
    )


    far_dist <- 0


    offsets <- c(
      -nc - 1,
      -nc,
      -nc + 1,
      -1,
      1,
      nc - 1,
      nc,
      nc + 1
    )


    bits <- c(
      1L,
      2L,
      4L,
      8L,
      16L,
      32L,
      64L,
      128L
    )


    while (
      queue_index <=
        length(
          queue_cells
        )
    ) {

      cells_chunk <- queue_cells[[
        queue_index
      ]]


      dist_chunk <- queue_dist[[
        queue_index
      ]]


      queue_cells[
        queue_index
      ] <- list(
        NULL
      )


      queue_dist[
        queue_index
      ] <- list(
        NULL
      )


      queue_index <- queue_index +
        1L


      chunk_length <- length(
        cells_chunk
      )


      chunk_start <- 1L


      while (
        chunk_start <=
          chunk_length
      ) {

        chunk_end <- min(
          chunk_length,
          chunk_start +
            TRACE_BATCH_SIZE -
            1L
        )


        batch <- cells_chunk[
          chunk_start:
            chunk_end
        ]


        batch_dist <- dist_chunk[
          chunk_start:
            chunk_end
        ]


        n_batches <- n_batches +
          1L


        reverse_values <- get_reverse_values(
          cache,
          batch
        )


        rows <- floor(
          (
            batch -
              1
          ) /
            nc
        ) +
          1


        cols <- (
          (
            batch -
              1
          ) %%
            nc
        ) +
          1


        parent_parts <- vector(
          "list",
          8L
        )


        distance_parts <- vector(
          "list",
          8L
        )


        n_parts <- 0L


        for (direction_id in seq_len(
          8L
        )) {

          valid_edge <- switch(
            as.character(
              direction_id
            ),
            "1" = rows > 1 & cols > 1,
            "2" = rows > 1,
            "3" = rows > 1 & cols < nc,
            "4" = cols > 1,
            "5" = cols < nc,
            "6" = rows < nr & cols > 1,
            "7" = rows < nr,
            "8" = rows < nr & cols < nc
          )


          idx <- which(
            bitwAnd(
              reverse_values,
              bits[
                direction_id
              ]
            ) !=
              0L &
              valid_edge
          )


          if (length(
            idx
          ) == 0L) {
            next
          }


          parents <- batch[
            idx
          ] +
            offsets[
              direction_id
            ]


          if (isTRUE(
            only_stream
          )) {

            stream_value <- stream_values_global(
              hydro,
              parents
            )


            keep <- is.finite(
              stream_value
            ) &
              stream_value >
                0


            if (!any(
              keep
            )) {
              next
            }


            parents <- parents[
              keep
            ]


            idx <- idx[
              keep
            ]
          }


          step_m <- reverse_step_length_m(
            metric_info,
            receptor_cells = batch[
              idx
            ],
            direction_id = direction_id
          )


          parent_dist <- batch_dist[
            idx
          ] +
            step_m


          n_parts <- n_parts +
            1L


          parent_parts[[
            n_parts
          ]] <- parents


          distance_parts[[
            n_parts
          ]] <- parent_dist
        }


        if (n_parts > 0L) {

          parents_all <- unlist(
            parent_parts[
              seq_len(
                n_parts
              )
            ],
            use.names = FALSE
          )


          distances_all <- unlist(
            distance_parts[
              seq_len(
                n_parts
              )
            ],
            use.names = FALSE
          )


          n_cells <- n_cells +
            length(
              parents_all
            )


          if (
            n_cells >
              MAX_BASIN_CELLS
          ) {
            stop(
              paste0(
                "El recorrido D8 supera MAX_BASIN_CELLS = ",
                format(
                  MAX_BASIN_CELLS,
                  big.mark = ","
                )
              )
            )
          }


          local_far <- which.max(
            distances_all
          )


          if (
            length(local_far) == 1L &&
            is.finite(
              distances_all[
                local_far
              ]
            ) &&
            distances_all[
              local_far
            ] >
              far_dist
          ) {
            far_dist <- distances_all[
              local_far
            ]

            far_cell <- parents_all[
              local_far
            ]
          }


          queue_cells[[
            length(
              queue_cells
            ) +
              1L
          ]] <- parents_all


          queue_dist[[
            length(
              queue_dist
            ) +
              1L
          ]] <- distances_all
        }


        if (
          !is.null(
            progress_fun
          ) &&
          n_batches %%
            2L ==
              0L
        ) {
          progress_fun(
            n_batches,
            n_cells,
            far_dist
          )
        }


        chunk_start <- chunk_end +
          1L
      }
    }


    list(
      cell = as.double(
        far_cell
      ),
      distance_m = as.numeric(
        far_dist
      ),
      n_cells = as.double(
        n_cells
      ),
      n_batches = n_batches
    )
  }


  receiver_for_cell <- function(
      reverse_cache,
      cell
  ) {

    nc <- as.double(
      reverse_cache$metadata$ncols
    )


    nr <- as.double(
      reverse_cache$metadata$nrows
    )


    row <- floor(
      (
        cell -
          1
      ) /
        nc
    ) +
      1


    col <- (
      (
        cell -
          1
      ) %%
        nc
    ) +
      1


    receiver_offsets <- c(
      nc + 1,
      nc,
      nc - 1,
      1,
      -1,
      -nc + 1,
      -nc,
      -nc - 1
    )


    expected_bits <- c(
      1L,
      2L,
      4L,
      8L,
      16L,
      32L,
      64L,
      128L
    )


    valid <- c(
      row < nr & col < nc,
      row < nr,
      row < nr & col > 1,
      col < nc,
      col > 1,
      row > 1 & col < nc,
      row > 1,
      row > 1 & col > 1
    )


    candidate <- cell +
      receiver_offsets


    candidate <- candidate[
      valid
    ]


    bits <- expected_bits[
      valid
    ]


    if (length(
      candidate
    ) == 0L) {
      return(
        NA_real_
      )
    }


    reverse_values <- get_reverse_values(
      reverse_cache,
      candidate
    )


    hit <- which(
      bitwAnd(
        reverse_values,
        bits
      ) !=
        0L
    )


    if (length(
      hit
    ) == 0L) {
      return(
        NA_real_
      )
    }


    if (length(
      hit
    ) > 1L) {
      stop(
        paste0(
          "Topologia D8 inconsistente: la celda ",
          format(
            cell,
            scientific = FALSE
          ),
          " tiene mas de un receptor."
        )
      )
    }


    as.double(
      candidate[
        hit
      ]
    )
  }


  reconstruct_path_to_outlet <- function(
      reverse_cache,
      start_cell,
      outlet_cell
  ) {

    start_cell <- as.double(
      start_cell
    )


    outlet_cell <- as.double(
      outlet_cell
    )


    path <- vector(
      "list",
      1024L
    )


    path[[1L]] <- start_cell


    n <- 1L
    current <- start_cell


    while (!identical(
      current,
      outlet_cell
    )) {

      receiver <- receiver_for_cell(
        reverse_cache,
        current
      )


      if (
        length(receiver) != 1L ||
        !is.finite(
          receiver
        )
      ) {
        stop(
          "No se pudo reconstruir el recorrido D8 hasta el outlet."
        )
      }


      n <- n +
        1L


      if (n > MAX_TRACE_LEVELS) {
        stop(
          "El perfil D8 supero MAX_TRACE_LEVELS."
        )
      }


      if (n > length(
        path
      )) {
        length(
          path
        ) <- length(
          path
        ) *
          2L
      }


      path[[
        n
      ]] <- receiver


      current <- receiver
    }


    as.double(
      unlist(
        path[
          seq_len(
            n
          )
        ],
        use.names = FALSE
      )
    )
  }


  path_lonlat <- function(
      grid_template,
      cells
  ) {

    xy <- terra::xyFromCell(
      grid_template,
      cells
    )


    grid_crs <- sf::st_crs(
      terra::crs(
        grid_template
      )
    )


    if (isTRUE(
      sf::st_is_longlat(
        grid_crs
      )
    )) {
      return(
        cbind(
          lon = xy[, 1],
          lat = xy[, 2]
        )
      )
    }


    points <- sf::st_as_sf(
      data.frame(
        x = xy[, 1],
        y = xy[, 2]
      ),
      coords = c(
        "x",
        "y"
      ),
      crs = grid_crs
    )


    points_4326 <- sf::st_transform(
      points,
      4326
    )


    out <- sf::st_coordinates(
      points_4326
    )


    cbind(
      lon = out[, 1],
      lat = out[, 2]
    )
  }


  extract_dem_lonlat <- function(
      dem,
      lonlat
  ) {

    points_4326 <- sf::st_as_sf(
      data.frame(
        lon = lonlat[, 1],
        lat = lonlat[, 2]
      ),
      coords = c(
        "lon",
        "lat"
      ),
      crs = 4326
    )


    dem_crs <- sf::st_crs(
      terra::crs(
        dem
      )
    )


    points_dem <- sf::st_transform(
      points_4326,
      dem_crs
    )


    xy_dem <- sf::st_coordinates(
      points_dem
    )


    extracted <- terra::extract(
      dem,
      xy_dem[, 1:2, drop = FALSE]
    )


    as.numeric(
      last_data_column(
        extracted
      )
    )
  }


  build_path_profile <- function(
      grid_template,
      dem,
      cells
  ) {

    lonlat <- path_lonlat(
      grid_template,
      cells
    )


    n <- nrow(
      lonlat
    )


    if (n < 1L) {
      stop(
        "El recorrido D8 reconstruido esta vacio."
      )
    }


    if (n == 1L) {
      segment_m <- numeric(
        0
      )
    } else {
      segment_m <- haversine_segment_m(
        lonlat[
          -n,
          1
        ],
        lonlat[
          -n,
          2
        ],
        lonlat[
          -1,
          1
        ],
        lonlat[
          -1,
          2
        ]
      )
    }


    distance_from_start_m <- c(
      0,
      cumsum(
        segment_m
      )
    )


    total_m <- if (length(
      distance_from_start_m
    ) > 0L) {
      max(
        distance_from_start_m
      )
    } else {
      0
    }


    elevation_m <- extract_dem_lonlat(
      dem,
      lonlat
    )


    data.frame(
      CELL = cells,
      LON = lonlat[, 1],
      LAT = lonlat[, 2],
      DIST_FROM_START_KM = distance_from_start_m /
        1000,
      DIST_FROM_OUTLET_KM = (
        total_m -
          distance_from_start_m
      ) /
        1000,
      ELEVATION_M = elevation_m,
      stringsAsFactors = FALSE
    )
  }


  interpolate_profile_z <- function(
      profile,
      distance_from_outlet_km
  ) {

    keep <- is.finite(
      profile$DIST_FROM_OUTLET_KM
    ) &
      is.finite(
        profile$ELEVATION_M
      )


    if (sum(
      keep
    ) < 2L) {
      return(
        NA_real_
      )
    }


    x <- profile$DIST_FROM_OUTLET_KM[
      keep
    ]


    y <- profile$ELEVATION_M[
      keep
    ]


    ord <- order(
      x
    )


    stats::approx(
      x = x[
        ord
      ],
      y = y[
        ord
      ],
      xout = distance_from_outlet_km,
      rule = 2,
      ties = "ordered"
    )$y
  }


  profile_metrics <- function(
      profile,
      is_channel = FALSE
  ) {

    total_km <- max(
      profile$DIST_FROM_START_KM,
      na.rm = TRUE
    )


    z_start <- profile$ELEVATION_M[
      1
    ]


    z_out <- profile$ELEVATION_M[
      nrow(
        profile
      )
    ]


    drop_m <- z_start -
      z_out


    total_slope_m_km <- if (
      is.finite(total_km) &&
      total_km > 0
    ) {
      drop_m /
        total_km
    } else {
      NA_real_
    }


    out <- list(
      length_km = total_km,
      z_start = z_start,
      z_out = z_out,
      drop_m = drop_m,
      total_slope_m_km = total_slope_m_km
    )


    if (!isTRUE(
      is_channel
    )) {

      d10 <- 0.10 *
        total_km


      d85 <- 0.85 *
        total_km


      z10 <- interpolate_profile_z(
        profile,
        d10
      )


      z85 <- interpolate_profile_z(
        profile,
        d85
      )


      s10_85 <- if (
        is.finite(total_km) &&
        total_km > 0 &&
        is.finite(z10) &&
        is.finite(z85)
      ) {
        (
          z85 -
            z10
        ) /
          (
            0.75 *
              total_km
          )
      } else {
        NA_real_
      }


      out$length_10_85_km <- 0.75 *
        total_km

      out$z10 <- z10
      out$z85 <- z85
      out$slope_10_85_m_km <- s10_85
    }


    if (isTRUE(
      is_channel
    )) {

      keep <- is.finite(
        profile$DIST_FROM_OUTLET_KM
      ) &
        is.finite(
          profile$ELEVATION_M
        )


      regression_slope_m_km <- NA_real_


      if (sum(
        keep
      ) >= 3L) {
        fit <- stats::lm(
          profile$ELEVATION_M[
            keep
          ] ~
            I(
              profile$DIST_FROM_OUTLET_KM[
                keep
              ] *
                1000
            )
        )


        coef_fit <- stats::coef(
          fit
        )


        if (
          length(coef_fit) >= 2L &&
          is.finite(
            coef_fit[2]
          )
        ) {
          regression_slope_m_km <- as.numeric(
            coef_fit[2]
          ) *
            1000
        }
      }


      straight_m <- haversine_segment_m(
        profile$LON[1],
        profile$LAT[1],
        profile$LON[
          nrow(
            profile
          )
        ],
        profile$LAT[
          nrow(
            profile
          )
        ]
      )


      sinuosity <- if (
        is.finite(straight_m) &&
        straight_m > 0
      ) {
        (
          total_km *
            1000
        ) /
          straight_m
      } else {
        NA_real_
      }


      out$regression_slope_m_km <- regression_slope_m_km
      out$sinuosity <- sinuosity
    }


    out
  }


  hydro_path_metrics <- function(
      hydro,
      outlet,
      dem,
      progress_fun = NULL
  ) {

    outlet_cell <- outlet_cell_from_sf(
      hydro$grid_template,
      outlet
    )


    outlet_stream <- stream_values_global(
      hydro,
      outlet_cell
    )


    if (
      length(outlet_stream) != 1L ||
      !is.finite(outlet_stream) ||
      outlet_stream <= 0
    ) {
      stop(
        "El outlet activo no pertenece a stream_stripes."
      )
    }


    far_hydro <- farthest_upstream_cell(
      hydro = hydro,
      outlet_cell = outlet_cell,
      only_stream = FALSE,
      progress_fun = if (is.null(
        progress_fun
      )) {
        NULL
      } else {
        function(
            batches,
            n_cells,
            far_dist
        ) {
          progress_fun(
            "hydraulic",
            batches,
            n_cells,
            far_dist
          )
        }
      }
    )


    hydraulic_cells <- reconstruct_path_to_outlet(
      reverse_cache = hydro$reverse_cache,
      start_cell = far_hydro$cell,
      outlet_cell = outlet_cell
    )


    hydraulic_profile <- build_path_profile(
      grid_template = hydro$grid_template,
      dem = dem,
      cells = hydraulic_cells
    )


    hydraulic_metrics <- profile_metrics(
      hydraulic_profile,
      is_channel = FALSE
    )


    far_stream <- farthest_upstream_cell(
      hydro = hydro,
      outlet_cell = outlet_cell,
      only_stream = TRUE,
      progress_fun = if (is.null(
        progress_fun
      )) {
        NULL
      } else {
        function(
            batches,
            n_cells,
            far_dist
        ) {
          progress_fun(
            "channel",
            batches,
            n_cells,
            far_dist
          )
        }
      }
    )


    channel_cells <- reconstruct_path_to_outlet(
      reverse_cache = hydro$reverse_cache,
      start_cell = far_stream$cell,
      outlet_cell = outlet_cell
    )


    stream_check <- stream_values_global(
      hydro,
      channel_cells
    )


    if (any(
      !is.finite(stream_check) |
        stream_check <= 0
    )) {
      stop(
        "El cauce principal reconstruido contiene celdas fuera de stream_stripes."
      )
    }


    channel_profile <- build_path_profile(
      grid_template = hydro$grid_template,
      dem = dem,
      cells = channel_cells
    )


    channel_metrics <- profile_metrics(
      channel_profile,
      is_channel = TRUE
    )


    list(
      outlet_cell = outlet_cell,
      hydraulic_remote_cell = far_hydro$cell,
      hydraulic_search_distance_m = far_hydro$distance_m,
      hydraulic_search_cells = far_hydro$n_cells,
      hydraulic = hydraulic_metrics,
      hydraulic_profile = hydraulic_profile,
      channel_head_cell = far_stream$cell,
      channel_search_distance_m = far_stream$distance_m,
      channel_search_cells = far_stream$n_cells,
      channel = channel_metrics,
      channel_profile = channel_profile,
      stream_threshold_km2 = hydro$stream_threshold_km2
    )
  }


  build_flow_channel_table <- function(
      hydro_metrics
  ) {

    h <- hydro_metrics$hydraulic
    c <- hydro_metrics$channel


    rows <- list(
      metric_row(
        "Recorrido hidraulico",
        "Elevacion del punto hidraulicamente remoto",
        "Zremote",
        h$z_start,
        "m",
        "FABDEM en el extremo aguas arriba del recorrido D8 mas largo"
      ),
      metric_row(
        "Recorrido hidraulico",
        "Elevacion del outlet",
        "Zout",
        h$z_out,
        "m",
        "FABDEM en la celda de salida ajustada"
      ),
      metric_row(
        "Recorrido hidraulico",
        "Pendiente total del recorrido",
        "Sh",
        h$total_slope_m_km,
        "m/km",
        "(Zremote-Zout)/Lh"
      ),
      metric_row(
        "Recorrido hidraulico",
        "Longitud del tramo 10-85",
        "L10-85",
        h$length_10_85_km,
        "km",
        "75% de Lh entre posiciones 10% y 85% desde el outlet"
      ),
      metric_row(
        "Recorrido hidraulico",
        "Elevacion al 10%",
        "Z10",
        h$z10,
        "m",
        "Interpolacion lineal del perfil a 10% de Lh desde el outlet"
      ),
      metric_row(
        "Recorrido hidraulico",
        "Elevacion al 85%",
        "Z85",
        h$z85,
        "m",
        "Interpolacion lineal del perfil a 85% de Lh desde el outlet"
      ),
      metric_row(
        "Cauce principal",
        "Elevacion de cabecera del cauce",
        "Zhead",
        c$z_start,
        "m",
        "FABDEM en la celda de cauce mas remota conectada al outlet"
      ),
      metric_row(
        "Cauce principal",
        "Pendiente por extremos del cauce",
        "Sc",
        c$total_slope_m_km,
        "m/km",
        "(Zhead-Zout)/Lc"
      ),
      metric_row(
        "Cauce principal",
        "Pendiente compensada por regresion",
        "SLR",
        c$regression_slope_m_km,
        "m/km",
        "Pendiente OLS de elevacion frente a distancia desde outlet"
      ),
      metric_row(
        "Cauce principal",
        "Sinuosidad",
        "Si",
        c$sinuosity,
        "adim.",
        "Lc/distancia geodesica cabecera-outlet"
      ),
      metric_row(
        "Red de referencia",
        "Umbral de extraccion de cauces",
        "Ath",
        hydro_metrics$stream_threshold_km2,
        "km2",
        "STREAM_THRESHOLD_KM2 del bloque hidrologico"
      )
    )


    do.call(
      rbind,
      rows
    )
  }



  # ==========================================================
  # 7. RED DE DRENAJE Y STRAHLER
  # ==========================================================

  trace_stream_network <- function(
      hydro,
      outlet_cell,
      progress_fun = NULL
  ) {

    cache <- hydro$reverse_cache


    nc <- as.double(
      cache$metadata$ncols
    )


    nr <- as.double(
      cache$metadata$nrows
    )


    metric_info <- grid_metric_info(
      hydro$grid_template
    )


    outlet_stream <- stream_values_global(
      hydro,
      outlet_cell
    )


    if (
      length(outlet_stream) != 1L ||
      !is.finite(outlet_stream) ||
      outlet_stream <= 0
    ) {
      stop(
        "El outlet activo no pertenece a stream_stripes."
      )
    }


    queue_cells <- list(
      as.double(
        outlet_cell
      )
    )


    queue_depth <- list(
      0L
    )


    queue_index <- 1L
    n_batches <- 0L
    n_stream_cells <- 1


    edge_parent_chunks <- list()
    edge_receiver_chunks <- list()
    edge_length_chunks <- list()
    edge_depth_chunks <- list()


    edge_chunk_index <- 0L


    offsets <- c(
      -nc - 1,
      -nc,
      -nc + 1,
      -1,
      1,
      nc - 1,
      nc,
      nc + 1
    )


    bits <- c(
      1L,
      2L,
      4L,
      8L,
      16L,
      32L,
      64L,
      128L
    )


    while (
      queue_index <=
        length(
          queue_cells
        )
    ) {

      cells_chunk <- queue_cells[[
        queue_index
      ]]


      depth_chunk <- queue_depth[[
        queue_index
      ]]


      queue_cells[
        queue_index
      ] <- list(
        NULL
      )


      queue_depth[
        queue_index
      ] <- list(
        NULL
      )


      queue_index <- queue_index +
        1L


      chunk_length <- length(
        cells_chunk
      )


      chunk_start <- 1L


      while (
        chunk_start <=
          chunk_length
      ) {

        chunk_end <- min(
          chunk_length,
          chunk_start +
            TRACE_BATCH_SIZE -
            1L
        )


        batch <- cells_chunk[
          chunk_start:
            chunk_end
        ]


        batch_depth <- depth_chunk[
          chunk_start:
            chunk_end
        ]


        n_batches <- n_batches +
          1L


        reverse_values <- get_reverse_values(
          cache,
          batch
        )


        rows <- floor(
          (
            batch -
              1
          ) /
            nc
        ) +
          1


        cols <- (
          (
            batch -
              1
          ) %%
            nc
        ) +
          1


        parent_parts <- vector(
          "list",
          8L
        )


        receiver_parts <- vector(
          "list",
          8L
        )


        length_parts <- vector(
          "list",
          8L
        )


        depth_parts <- vector(
          "list",
          8L
        )


        n_parts <- 0L


        for (direction_id in seq_len(
          8L
        )) {

          valid_edge <- switch(
            as.character(
              direction_id
            ),
            "1" = rows > 1 & cols > 1,
            "2" = rows > 1,
            "3" = rows > 1 & cols < nc,
            "4" = cols > 1,
            "5" = cols < nc,
            "6" = rows < nr & cols > 1,
            "7" = rows < nr,
            "8" = rows < nr & cols < nc
          )


          idx <- which(
            bitwAnd(
              reverse_values,
              bits[
                direction_id
              ]
            ) !=
              0L &
              valid_edge
          )


          if (length(
            idx
          ) == 0L) {
            next
          }


          parents <- batch[
            idx
          ] +
            offsets[
              direction_id
            ]


          stream_value <- stream_values_global(
            hydro,
            parents
          )


          keep <- is.finite(
            stream_value
          ) &
            stream_value >
              0


          if (!any(
            keep
          )) {
            next
          }


          parents <- parents[
            keep
          ]


          idx <- idx[
            keep
          ]


          step_m <- reverse_step_length_m(
            metric_info,
            receptor_cells = batch[
              idx
            ],
            direction_id = direction_id
          )


          n_parts <- n_parts +
            1L


          parent_parts[[
            n_parts
          ]] <- parents


          receiver_parts[[
            n_parts
          ]] <- batch[
            idx
          ]


          length_parts[[
            n_parts
          ]] <- step_m


          depth_parts[[
            n_parts
          ]] <- as.integer(
            batch_depth[
              idx
            ] +
              1L
          )
        }


        if (n_parts > 0L) {

          parents_all <- unlist(
            parent_parts[
              seq_len(
                n_parts
              )
            ],
            use.names = FALSE
          )


          receivers_all <- unlist(
            receiver_parts[
              seq_len(
                n_parts
              )
            ],
            use.names = FALSE
          )


          lengths_all <- unlist(
            length_parts[
              seq_len(
                n_parts
              )
            ],
            use.names = FALSE
          )


          depths_all <- unlist(
            depth_parts[
              seq_len(
                n_parts
              )
            ],
            use.names = FALSE
          )


          n_stream_cells <- n_stream_cells +
            length(
              parents_all
            )


          if (
            n_stream_cells >
              MAX_BASIN_CELLS
          ) {
            stop(
              paste0(
                "La red de cauces supera MAX_BASIN_CELLS = ",
                format(
                  MAX_BASIN_CELLS,
                  big.mark = ","
                )
              )
            )
          }


          edge_chunk_index <- edge_chunk_index +
            1L


          edge_parent_chunks[[
            edge_chunk_index
          ]] <- parents_all


          edge_receiver_chunks[[
            edge_chunk_index
          ]] <- receivers_all


          edge_length_chunks[[
            edge_chunk_index
          ]] <- lengths_all


          edge_depth_chunks[[
            edge_chunk_index
          ]] <- depths_all


          queue_cells[[
            length(
              queue_cells
            ) +
              1L
          ]] <- parents_all


          queue_depth[[
            length(
              queue_depth
            ) +
              1L
          ]] <- depths_all
        }


        if (
          !is.null(
            progress_fun
          ) &&
          n_batches %%
            2L ==
              0L
        ) {
          progress_fun(
            n_batches,
            n_stream_cells,
            edge_chunk_index
          )
        }


        chunk_start <- chunk_end +
          1L
      }
    }


    if (edge_chunk_index == 0L) {

      return(
        list(
          outlet_cell = as.double(
            outlet_cell
          ),
          cells = as.double(
            outlet_cell
          ),
          depth = 0L,
          parent = numeric(
            0
          ),
          receiver = numeric(
            0
          ),
          edge_length_m = numeric(
            0
          ),
          parent_depth = integer(
            0
          ),
          n_batches = n_batches
        )
      )
    }


    parent <- as.double(
      unlist(
        edge_parent_chunks,
        use.names = FALSE
      )
    )


    receiver <- as.double(
      unlist(
        edge_receiver_chunks,
        use.names = FALSE
      )
    )


    edge_length_m <- as.numeric(
      unlist(
        edge_length_chunks,
        use.names = FALSE
      )
    )


    parent_depth <- as.integer(
      unlist(
        edge_depth_chunks,
        use.names = FALSE
      )
    )


    if (anyDuplicated(
      parent
    ) > 0L) {
      stop(
        "Topologia de stream_stripes inconsistente: una celda de cauce aparece con mas de un receptor."
      )
    }


    cells <- c(
      as.double(
        outlet_cell
      ),
      parent
    )


    depth <- c(
      0L,
      parent_depth
    )


    list(
      outlet_cell = as.double(
        outlet_cell
      ),
      cells = cells,
      depth = depth,
      parent = parent,
      receiver = receiver,
      edge_length_m = edge_length_m,
      parent_depth = parent_depth,
      n_batches = n_batches
    )
  }


  strahler_cell_order <- function(
      cells,
      depth,
      parent,
      receiver
  ) {

    n_cells <- length(
      cells
    )


    if (n_cells == 0L) {
      stop(
        "No hay celdas de red para ordenar."
      )
    }


    if (length(
      parent
    ) == 0L) {
      return(
        list(
          order = 1L,
          indegree = 0L,
          parent_index = integer(
            0
          ),
          receiver_index = integer(
            0
          )
        )
      )
    }


    parent_index <- match(
      parent,
      cells
    )


    receiver_index <- match(
      receiver,
      cells
    )


    if (anyNA(
      parent_index
    ) || anyNA(
      receiver_index
    )) {
      stop(
        "No se pudo indexar la topologia de la red de cauces."
      )
    }


    indegree <- tabulate(
      receiver_index,
      nbins = n_cells
    )


    order_cell <- integer(
      n_cells
    )


    headwaters <- which(
      indegree ==
        0L
    )


    order_cell[
      headwaters
    ] <- 1L


    edge_sort <- order(
      depth[
        parent_index
      ],
      decreasing = TRUE
    )


    depth_sorted <- depth[
      parent_index[
        edge_sort
      ]
    ]


    depth_runs <- rle(
      depth_sorted
    )


    run_end <- cumsum(
      depth_runs$lengths
    )


    run_start <- c(
      1L,
      head(
        run_end,
        -1L
      ) +
        1L
    )


    for (run_i in seq_along(
      run_end
    )) {

      edge_ids <- edge_sort[
        run_start[
          run_i
        ]:
          run_end[
            run_i
          ]
      ]


      parent_orders <- order_cell[
        parent_index[
          edge_ids
        ]
      ]


      if (any(
        parent_orders <
          1L
      )) {
        stop(
          "No se pudo resolver el orden Strahler de una rama aguas arriba."
        )
      }


      rec_idx <- receiver_index[
        edge_ids
      ]


      groups <- split(
        parent_orders,
        rec_idx
      )


      receiver_ids <- as.integer(
        names(
          groups
        )
      )


      receiver_orders <- vapply(
        groups,
        function(v) {
          m <- max(
            v
          )

          as.integer(
            m +
              as.integer(
                sum(
                  v ==
                    m
                ) >=
                  2L
              )
          )
        },
        integer(
          1
        )
      )


      order_cell[
        receiver_ids
      ] <- receiver_orders
    }


    if (any(
      order_cell <
        1L
    )) {
      stop(
        "Quedaron celdas de cauce sin orden Strahler."
      )
    }


    list(
      order = order_cell,
      indegree = as.integer(
        indegree
      ),
      parent_index = as.integer(
        parent_index
      ),
      receiver_index = as.integer(
        receiver_index
      )
    )
  }


  build_stream_links <- function(
      cells,
      indegree,
      order_cell,
      parent_index,
      receiver_index,
      edge_length_m
  ) {

    n_cells <- length(
      cells
    )


    receiver_by_cell <- integer(
      n_cells
    )


    length_by_cell <- numeric(
      n_cells
    )


    if (length(
      parent_index
    ) > 0L) {
      receiver_by_cell[
        parent_index
      ] <- receiver_index

      length_by_cell[
        parent_index
      ] <- edge_length_m
    }


    outlet_index <- 1L


    start_index <- which(
      indegree !=
        1L &
        seq_len(
          n_cells
        ) !=
          outlet_index
    )


    if (length(
      start_index
    ) == 0L) {
      return(
        data.frame(
          LINK_ID = integer(
            0
          ),
          START_CELL = numeric(
            0
          ),
          END_CELL = numeric(
            0
          ),
          STRAHLER = integer(
            0
          ),
          LENGTH_M = numeric(
            0
          ),
          stringsAsFactors = FALSE
        )
      )
    }


    link_start_cell <- numeric(
      length(
        start_index
      )
    )


    link_end_cell <- numeric(
      length(
        start_index
      )
    )


    link_order <- integer(
      length(
        start_index
      )
    )


    link_length_m <- numeric(
      length(
        start_index
      )
    )


    for (i in seq_along(
      start_index
    )) {

      start_i <- start_index[
        i
      ]


      current <- start_i
      total_m <- 0
      steps <- 0L


      repeat {

        next_i <- receiver_by_cell[
          current
        ]


        if (next_i < 1L) {
          stop(
            "Un link de cauce no alcanza un receptor aguas abajo."
          )
        }


        total_m <- total_m +
          length_by_cell[
            current
          ]


        steps <- steps +
          1L


        if (steps > n_cells) {
          stop(
            "Posible ciclo al construir links de la red de cauces."
          )
        }


        if (
          next_i ==
            outlet_index ||
          indegree[
            next_i
          ] !=
            1L
        ) {
          break
        }


        current <- next_i
      }


      link_start_cell[
        i
      ] <- cells[
        start_i
      ]


      link_end_cell[
        i
      ] <- cells[
        next_i
      ]


      link_order[
        i
      ] <- order_cell[
        start_i
      ]


      link_length_m[
        i
      ] <- total_m
    }


    data.frame(
      LINK_ID = seq_along(
        start_index
      ),
      START_CELL = link_start_cell,
      END_CELL = link_end_cell,
      STRAHLER = link_order,
      LENGTH_M = link_length_m,
      stringsAsFactors = FALSE
    )
  }


  build_strahler_table <- function(
      links,
      max_order
  ) {

    if (
      nrow(
        links
      ) == 0L ||
      !is.finite(
        max_order
      ) ||
      max_order <
        1L
    ) {
      return(
        data.frame(
          ORDEN = integer(
            0
          ),
          N_TRAMOS = integer(
            0
          ),
          LONGITUD_TOTAL_KM = numeric(
            0
          ),
          LONGITUD_MEDIA_KM = numeric(
            0
          ),
          RB = numeric(
            0
          ),
          stringsAsFactors = FALSE
        )
      )
    }


    orders <- seq_len(
      as.integer(
        max_order
      )
    )


    n_links <- vapply(
      orders,
      function(u) {
        sum(
          links$STRAHLER ==
            u
        )
      },
      integer(
        1
      )
    )


    total_km <- vapply(
      orders,
      function(u) {
        sum(
          links$LENGTH_M[
            links$STRAHLER ==
              u
          ],
          na.rm = TRUE
        ) /
          1000
      },
      numeric(
        1
      )
    )


    mean_km <- ifelse(
      n_links >
        0L,
      total_km /
        n_links,
      NA_real_
    )


    rb <- rep(
      NA_real_,
      length(
        orders
      )
    )


    if (length(
      orders
    ) > 1L) {
      for (i in seq_len(
        length(
          orders
        ) -
          1L
      )) {
        if (n_links[
          i +
            1L
        ] >
          0L) {
          rb[
            i
          ] <- n_links[
            i
          ] /
            n_links[
              i +
                1L
            ]
        }
      }
    }


    data.frame(
      ORDEN = orders,
      N_TRAMOS = n_links,
      LONGITUD_TOTAL_KM = total_km,
      LONGITUD_MEDIA_KM = mean_km,
      RB = rb,
      stringsAsFactors = FALSE
    )
  }


  stream_network_metrics <- function(
      hydro,
      outlet_cell,
      geom,
      relief,
      progress_fun = NULL
  ) {

    network <- trace_stream_network(
      hydro = hydro,
      outlet_cell = outlet_cell,
      progress_fun = progress_fun
    )


    ordered <- strahler_cell_order(
      cells = network$cells,
      depth = network$depth,
      parent = network$parent,
      receiver = network$receiver
    )


    links <- build_stream_links(
      cells = network$cells,
      indegree = ordered$indegree,
      order_cell = ordered$order,
      parent_index = ordered$parent_index,
      receiver_index = ordered$receiver_index,
      edge_length_m = network$edge_length_m
    )


    total_length_km <- sum(
      network$edge_length_m,
      na.rm = TRUE
    ) /
      1000


    link_length_km <- sum(
      links$LENGTH_M,
      na.rm = TRUE
    ) /
      1000


    tolerance_km <- max(
      0.001,
      total_length_km *
        1e-9
    )


    if (
      nrow(
        links
      ) > 0L &&
      abs(
        total_length_km -
          link_length_km
      ) >
        tolerance_km
    ) {
      stop(
        paste0(
          "La suma de links no reproduce la longitud total de la red: ",
          sprintf(
            "%.6f vs %.6f km.",
            link_length_km,
            total_length_km
          )
        )
      )
    }


    max_order <- max(
      ordered$order,
      na.rm = TRUE
    )


    strahler <- build_strahler_table(
      links = links,
      max_order = max_order
    )


    n_links <- nrow(
      links
    )


    n_junctions <- sum(
      ordered$indegree >=
        2L
    )


    drainage_density <- if (
      is.finite(
        geom$area_km2
      ) &&
      geom$area_km2 >
        0
    ) {
      total_length_km /
        geom$area_km2
    } else {
      NA_real_
    }


    stream_frequency <- if (
      is.finite(
        geom$area_km2
      ) &&
      geom$area_km2 >
        0
    ) {
      n_links /
        geom$area_km2
    } else {
      NA_real_
    }


    drainage_texture <- if (
      is.finite(
        geom$perimeter_km
      ) &&
      geom$perimeter_km >
        0
    ) {
      n_links /
        geom$perimeter_km
    } else {
      NA_real_
    }


    mean_link_length_km <- if (
      n_links >
        0L
    ) {
      link_length_km /
        n_links
    } else {
      NA_real_
    }


    junction_density <- if (
      is.finite(
        geom$area_km2
      ) &&
      geom$area_km2 >
        0
    ) {
      n_junctions /
        geom$area_km2
    } else {
      NA_real_
    }


    rb_values <- strahler$RB[
      is.finite(
        strahler$RB
      )
    ]


    mean_rb <- if (length(
      rb_values
    ) > 0L) {
      mean(
        rb_values
      )
    } else {
      NA_real_
    }


    relief_km <- relief$relief_m /
      1000


    ruggedness_number <- if (
      is.finite(
        drainage_density
      ) &&
      is.finite(
        relief_km
      )
    ) {
      drainage_density *
        relief_km
    } else {
      NA_real_
    }


    drainage_intensity <- if (
      is.finite(
        stream_frequency
      ) &&
      is.finite(
        drainage_density
      ) &&
      drainage_density >
        0
    ) {
      stream_frequency /
        drainage_density
    } else {
      NA_real_
    }


    overland_flow_length <- if (
      is.finite(
        drainage_density
      ) &&
      drainage_density >
        0
    ) {
      1 /
        (
          2 *
            drainage_density
        )
    } else {
      NA_real_
    }


    channel_maintenance <- if (
      is.finite(
        drainage_density
      ) &&
      drainage_density >
        0
    ) {
      1 /
        drainage_density
    } else {
      NA_real_
    }


    infiltration_number <- if (
      is.finite(
        stream_frequency
      ) &&
      is.finite(
        drainage_density
      )
    ) {
      stream_frequency *
        drainage_density
    } else {
      NA_real_
    }


    list(
      network = network,
      cell_order = ordered$order,
      indegree = ordered$indegree,
      links = links,
      strahler = strahler,
      n_stream_cells = length(
        network$cells
      ),
      total_length_km = total_length_km,
      n_links = n_links,
      max_order = as.integer(
        max_order
      ),
      n_junctions = as.integer(
        n_junctions
      ),
      drainage_density_km_km2 = drainage_density,
      stream_frequency_n_km2 = stream_frequency,
      drainage_texture_n_km = drainage_texture,
      mean_link_length_km = mean_link_length_km,
      junction_density_n_km2 = junction_density,
      mean_bifurcation_ratio = mean_rb,
      ruggedness_number = ruggedness_number,
      drainage_intensity = drainage_intensity,
      overland_flow_length_km = overland_flow_length,
      channel_maintenance_km = channel_maintenance,
      infiltration_number = infiltration_number,
      stream_threshold_km2 = hydro$stream_threshold_km2
    )
  }


  build_network_table <- function(
      network_metrics
  ) {

    rows <- list(
      metric_row(
        "Red de drenaje",
        "Longitud total de cauces",
        "Lt",
        network_metrics$total_length_km,
        "km",
        "Suma de longitudes D8 centro a centro de todas las celdas conectadas de stream_stripes"
      ),
      metric_row(
        "Red de drenaje",
        "Densidad de drenaje",
        "Dd",
        network_metrics$drainage_density_km_km2,
        "km/km2",
        "Lt/A"
      ),
      metric_row(
        "Red de drenaje",
        "Numero de links",
        "Nu",
        network_metrics$n_links,
        "n",
        "Tramos topologicos entre cabeceras, confluencias y outlet; no pixeles"
      ),
      metric_row(
        "Red de drenaje",
        "Orden Strahler maximo",
        "Omega",
        network_metrics$max_order,
        "orden",
        "Orden Strahler calculado sobre la red conectada al outlet"
      ),
      metric_row(
        "Red de drenaje",
        "Frecuencia de cauces",
        "Fs",
        network_metrics$stream_frequency_n_km2,
        "links/km2",
        "Nu/A"
      ),
      metric_row(
        "Red de drenaje",
        "Textura de drenaje",
        "Dt",
        network_metrics$drainage_texture_n_km,
        "links/km",
        "Nu/P"
      ),
      metric_row(
        "Red de drenaje",
        "Longitud media de link",
        "Lm",
        network_metrics$mean_link_length_km,
        "km",
        "Longitud total de links/Nu"
      ),
      metric_row(
        "Red de drenaje",
        "Numero de confluencias",
        "Nj",
        network_metrics$n_junctions,
        "n",
        "Celdas de red con dos o mas tributarios directos"
      ),
      metric_row(
        "Red de drenaje",
        "Densidad de confluencias",
        "Jd",
        network_metrics$junction_density_n_km2,
        "confluencias/km2",
        "Nj/A"
      ),
      metric_row(
        "Red de drenaje",
        "Razon media de bifurcacion",
        "Rb",
        network_metrics$mean_bifurcation_ratio,
        "adim.",
        "Media de Nu/Nu+1 entre ordenes Strahler consecutivos"
      ),
      metric_row(
        "Red de drenaje",
        "Numero de rugosidad",
        "Rn",
        network_metrics$ruggedness_number,
        "adim.",
        "Dd * H(km)"
      ),
      metric_row(
        "Red de referencia",
        "Umbral de extraccion de cauces",
        "Ath",
        network_metrics$stream_threshold_km2,
        "km2",
        "STREAM_THRESHOLD_KM2 del bloque; condiciona todas las metricas de red"
      )
    )


    do.call(
      rbind,
      rows
    )
  }


  build_network_derived_table <- function(
      network_metrics
  ) {

    rows <- list(
      metric_row(
        "Indices derivados",
        "Intensidad de drenaje",
        "Id",
        network_metrics$drainage_intensity,
        "links/km",
        "Fs/Dd; transformacion algebraica de Fs y Dd"
      ),
      metric_row(
        "Indices derivados",
        "Longitud de flujo superficial",
        "Lo",
        network_metrics$overland_flow_length_km,
        "km",
        "1/(2*Dd); indice teorico derivado"
      ),
      metric_row(
        "Indices derivados",
        "Constante de mantenimiento de cauce",
        "C",
        network_metrics$channel_maintenance_km,
        "km",
        "1/Dd; indice teorico derivado"
      ),
      metric_row(
        "Indices derivados",
        "Numero de infiltracion",
        "If",
        network_metrics$infiltration_number,
        "indice",
        "Fs*Dd; indice geomorfologico, no infiltracion fisica medida"
      )
    )


    do.call(
      rbind,
      rows
    )
  }



  # ==========================================================
  # 8. TIEMPOS DE CONCENTRACION
  # ==========================================================

  concentration_time_metrics <- function(
      geom,
      relief,
      hydro_metrics
  ) {

    h <- hydro_metrics$hydraulic


    length_km <- as.numeric(
      h$length_km
    )


    length_m <- length_km *
      1000


    drop_m <- as.numeric(
      h$drop_m
    )


    slope_m_m <- if (
      is.finite(length_m) &&
      length_m > 0 &&
      is.finite(drop_m) &&
      drop_m > 0
    ) {
      drop_m /
        length_m
    } else {
      NA_real_
    }


    mean_height_above_outlet_m <- if (
      is.finite(relief$zmean) &&
      is.finite(h$z_out)
    ) {
      relief$zmean -
        h$z_out
    } else {
      NA_real_
    }


    # --------------------------------------------------------
    # Kirpich
    # --------------------------------------------------------
    # Forma SI:
    #   Tc(min) = 0.01947 * L(m)^0.77 * J^(-0.385)
    # Se convierte explicitamente a horas.
    # --------------------------------------------------------

    kirpich_h <- if (
      is.finite(length_m) &&
      length_m > 0 &&
      is.finite(slope_m_m) &&
      slope_m_m > 0
    ) {
      (
        0.01947 *
          length_m^0.77 *
          slope_m_m^(-0.385)
      ) /
        60
    } else {
      NA_real_
    }


    # --------------------------------------------------------
    # Giandotti
    # --------------------------------------------------------
    #   Tc(h) = [4*sqrt(A) + 1.5*L] / [0.8*sqrt(Hm)]
    # A  : km2
    # L  : km (Lh)
    # Hm : elevacion media de la cuenca sobre el outlet, m
    # --------------------------------------------------------

    giandotti_h <- if (
      is.finite(geom$area_km2) &&
      geom$area_km2 > 0 &&
      is.finite(length_km) &&
      length_km > 0 &&
      is.finite(mean_height_above_outlet_m) &&
      mean_height_above_outlet_m > 0
    ) {
      (
        4 *
          sqrt(
            geom$area_km2
          ) +
          1.5 *
            length_km
      ) /
        (
          0.8 *
            sqrt(
              mean_height_above_outlet_m
            )
        )
    } else {
      NA_real_
    }


    # --------------------------------------------------------
    # Temez
    # --------------------------------------------------------
    #   Tc(h) = 0.3 * [L(km) / J^0.25]^0.76
    # J = desnivel / longitud, adimensional.
    # --------------------------------------------------------

    temez_h <- if (
      is.finite(length_km) &&
      length_km > 0 &&
      is.finite(slope_m_m) &&
      slope_m_m > 0
    ) {
      0.3 *
        (
          length_km /
            slope_m_m^0.25
        )^0.76
    } else {
      NA_real_
    }


    values <- c(
      kirpich_h,
      giandotti_h,
      temez_h
    )


    valid_values <- values[
      is.finite(values) &
        values > 0
    ]


    range_min_h <- if (
      length(valid_values) > 0L
    ) {
      min(valid_values)
    } else {
      NA_real_
    }


    range_max_h <- if (
      length(valid_values) > 0L
    ) {
      max(valid_values)
    } else {
      NA_real_
    }


    list(
      kirpich_h = kirpich_h,
      giandotti_h = giandotti_h,
      temez_h = temez_h,
      range_min_h = range_min_h,
      range_max_h = range_max_h,
      n_valid = length(valid_values),
      length_km = length_km,
      drop_m = drop_m,
      slope_m_m = slope_m_m,
      mean_height_above_outlet_m = mean_height_above_outlet_m
    )
  }


  build_concentration_time_table <- function(
      tc_metrics
  ) {

    rows <- list(
      metric_row(
        "Tiempo de concentracion",
        "Tc Kirpich",
        "Tc_K",
        tc_metrics$kirpich_h,
        "h",
        "0.01947*Lh(m)^0.77*J^-0.385 / 60; J=Hh/Lh"
      ),
      metric_row(
        "Tiempo de concentracion",
        "Tc Giandotti",
        "Tc_G",
        tc_metrics$giandotti_h,
        "h",
        "[4*sqrt(A)+1.5*Lh(km)]/[0.8*sqrt(Hm)]; Hm=Zmean-Zout"
      ),
      metric_row(
        "Tiempo de concentracion",
        "Tc Temez",
        "Tc_T",
        tc_metrics$temez_h,
        "h",
        "0.3*[Lh(km)/J^0.25]^0.76; J=Hh/Lh"
      ),
      metric_row(
        "Rango empirico",
        "Limite inferior",
        "Tc_min",
        tc_metrics$range_min_h,
        "h",
        "Minimo de las ecuaciones con resultado valido"
      ),
      metric_row(
        "Rango empirico",
        "Limite superior",
        "Tc_max",
        tc_metrics$range_max_h,
        "h",
        "Maximo de las ecuaciones con resultado valido"
      )
    )


    do.call(
      rbind,
      rows
    )
  }


  # ==========================================================
  # 9. TABLAS
  # ==========================================================

  build_primary_table <- function(
      geom,
      relief,
      hydro_metrics,
      network_metrics,
      tc_metrics
  ) {

    rows <- list(
      metric_row(
        "Geometria",
        "Area",
        "A",
        geom$area_km2,
        "km2",
        "Poligono de cuenca en UTM local"
      ),
      metric_row(
        "Geometria",
        "Perimetro",
        "P",
        geom$perimeter_km,
        "km",
        "Borde del poligono en UTM local"
      ),
      metric_row(
        "Geometria",
        "Longitud de cuenca",
        "Lb",
        geom$basin_length_km,
        "km",
        "Outlet al punto mas lejano de la divisoria"
      ),
      metric_row(
        "Relieve",
        "Elevacion minima",
        "Zmin",
        relief$zmin,
        "m",
        "FABDEM recortado por cuenca"
      ),
      metric_row(
        "Relieve",
        "Elevacion maxima",
        "Zmax",
        relief$zmax,
        "m",
        "FABDEM recortado por cuenca"
      ),
      metric_row(
        "Relieve",
        "Elevacion media",
        "Zmean",
        relief$zmean,
        "m",
        "Media espacial FABDEM"
      ),
      metric_row(
        "Relieve",
        "Elevacion mediana",
        "Z50",
        relief$zmedian,
        "m",
        "Mediana espacial FABDEM"
      ),
      metric_row(
        "Relieve",
        "Rango altitudinal",
        "H",
        relief$relief_m,
        "m",
        "Zmax - Zmin"
      ),
      metric_row(
        "Relieve",
        "Pendiente media de cuenca",
        "Sb",
        relief$slope_mean_pct,
        "%",
        "terrain(FABDEM), pendiente celda a celda"
      ),
      metric_row(
        "Relieve",
        "Integral hipsometrica",
        "HI",
        relief$hypsometric_integral,
        "adim.",
        "(Zmean-Zmin)/(Zmax-Zmin)"
      ),
      metric_row(
        "Recorrido",
        "Recorrido hidraulico mas largo",
        "Lh",
        hydro_metrics$hydraulic$length_km,
        "km",
        "Trayectoria D8 mas larga hasta el outlet"
      ),
      metric_row(
        "Recorrido",
        "Desnivel hidraulico",
        "Hh",
        hydro_metrics$hydraulic$drop_m,
        "m",
        "Zremote - Zout sobre el recorrido D8"
      ),
      metric_row(
        "Recorrido",
        "Pendiente 10-85",
        "S10-85",
        hydro_metrics$hydraulic$slope_10_85_m_km,
        "m/km",
        "Perfil D8 entre 10% y 85% de Lh desde el outlet"
      ),
      metric_row(
        "Cauce",
        "Longitud del cauce principal",
        "Lc",
        hydro_metrics$channel$length_km,
        "km",
        "Recorrido mas largo sobre stream_stripes conectado al outlet"
      ),
      metric_row(
        "Red",
        "Longitud total de cauces",
        "Lt",
        network_metrics$total_length_km,
        "km",
        "Suma de longitudes D8 de la red conectada al outlet"
      ),
      metric_row(
        "Red",
        "Densidad de drenaje",
        "Dd",
        network_metrics$drainage_density_km_km2,
        "km/km2",
        "Lt/A; dependiente del umbral de extraccion de cauces"
      ),
      metric_row(
        "Red",
        "Orden Strahler maximo",
        "Omega",
        network_metrics$max_order,
        "orden",
        "Orden Strahler maximo de la red conectada"
      ),
      metric_row(
        "Red",
        "Numero de links",
        "Nu",
        network_metrics$n_links,
        "n",
        "Tramos topologicos entre cabeceras, confluencias y outlet"
      ),
      metric_row(
        "Forma",
        "Factor de forma",
        "Ff",
        geom$form_factor,
        "adim.",
        "A/Lb2"
      ),
      metric_row(
        "Forma",
        "Relacion de elongacion",
        "Re",
        geom$elongation_ratio,
        "adim.",
        "2*sqrt(A/pi)/Lb"
      ),
      metric_row(
        "Forma",
        "Relacion de circularidad",
        "Rc",
        geom$circularity_ratio,
        "adim.",
        "4*pi*A/P2"
      ),
      metric_row(
        "Forma",
        "Coeficiente de compacidad",
        "Kc",
        geom$compactness_coefficient,
        "adim.",
        "P/(2*sqrt(pi*A))"
      ),
      metric_row(
        "Respuesta",
        "Tc Kirpich",
        "Tc_K",
        tc_metrics$kirpich_h,
        "h",
        "Kirpich con Lh y pendiente hidraulica total; resultado SI convertido de minutos a horas"
      ),
      metric_row(
        "Respuesta",
        "Tc Giandotti",
        "Tc_G",
        tc_metrics$giandotti_h,
        "h",
        "Giandotti con A, Lh y elevacion media sobre el outlet"
      ),
      metric_row(
        "Respuesta",
        "Tc Temez",
        "Tc_T",
        tc_metrics$temez_h,
        "h",
        "Temez con Lh y pendiente hidraulica total"
      )
    )


    do.call(
      rbind,
      rows
    )
  }


  build_advanced_table <- function(
      geom,
      relief
  ) {

    qz <- relief$elevation_quantiles
    qs <- relief$slope_quantiles


    rows <- list(
      metric_row(
        "Geometria avanzada",
        "Ancho medio",
        "B",
        geom$mean_width_km,
        "km",
        "A/Lb"
      ),
      metric_row(
        "Geometria avanzada",
        "Diametro equivalente",
        "Deq",
        geom$equivalent_diameter_km,
        "km",
        "2*sqrt(A/pi)"
      ),
      metric_row(
        "Geometria avanzada",
        "Orientacion principal",
        "theta",
        geom$orientation_deg,
        "grados",
        "Azimut outlet -> punto mas lejano de divisoria"
      ),
      metric_row(
        "Relieve avanzado",
        "Elevacion P10",
        "Z10",
        qz[1],
        "m",
        "Percentil aproximado de muestra regular FABDEM"
      ),
      metric_row(
        "Relieve avanzado",
        "Elevacion P25",
        "Z25",
        qz[2],
        "m",
        "Percentil aproximado de muestra regular FABDEM"
      ),
      metric_row(
        "Relieve avanzado",
        "Elevacion P75",
        "Z75",
        qz[3],
        "m",
        "Percentil aproximado de muestra regular FABDEM"
      ),
      metric_row(
        "Relieve avanzado",
        "Elevacion P90",
        "Z90",
        qz[4],
        "m",
        "Percentil aproximado de muestra regular FABDEM"
      ),
      metric_row(
        "Relieve avanzado",
        "Pendiente P10",
        "S10",
        qs[1],
        "%",
        "Percentil aproximado de muestra regular"
      ),
      metric_row(
        "Relieve avanzado",
        "Pendiente P50",
        "S50",
        qs[2],
        "%",
        "Percentil aproximado de muestra regular"
      ),
      metric_row(
        "Relieve avanzado",
        "Pendiente P90",
        "S90",
        qs[3],
        "%",
        "Percentil aproximado de muestra regular"
      ),
      metric_row(
        "Relieve avanzado",
        "Relacion de relieve",
        "Rr",
        relief$relief_ratio_m_km,
        "m/km",
        "H/Lb"
      ),
      metric_row(
        "Relieve avanzado",
        "Rugosidad de Melton",
        "MRN",
        relief$melton,
        "adim.",
        "H(km)/sqrt(A)"
      )
    )


    do.call(
      rbind,
      rows
    )
  }



  build_imported_primary_table <- function(
      geom,
      relief
  ) {

    rows <- list(
      metric_row(
        "Geometria",
        "Area",
        "A",
        geom$area_km2,
        "km2",
        "Poligono importado en UTM local"
      ),
      metric_row(
        "Geometria",
        "Perimetro",
        "P",
        geom$perimeter_km,
        "km",
        "Borde del poligono importado en UTM local"
      ),
      metric_row(
        "Geometria",
        "Diametro equivalente",
        "Deq",
        geom$equivalent_diameter_km,
        "km",
        "2*sqrt(A/pi)"
      ),
      metric_row(
        "Forma",
        "Relacion de circularidad",
        "Rc",
        geom$circularity_ratio,
        "adim.",
        "4*pi*A/P2"
      ),
      metric_row(
        "Forma",
        "Coeficiente de compacidad",
        "Kc",
        geom$compactness_coefficient,
        "adim.",
        "P/(2*sqrt(pi*A))"
      ),
      metric_row(
        "Relieve",
        "Elevacion minima",
        "Zmin",
        relief$zmin,
        "m",
        "FABDEM recortado por la geometria importada"
      ),
      metric_row(
        "Relieve",
        "Elevacion maxima",
        "Zmax",
        relief$zmax,
        "m",
        "FABDEM recortado por la geometria importada"
      ),
      metric_row(
        "Relieve",
        "Elevacion media",
        "Zmean",
        relief$zmean,
        "m",
        "Media espacial FABDEM"
      ),
      metric_row(
        "Relieve",
        "Elevacion mediana",
        "Z50",
        relief$zmedian,
        "m",
        "Mediana espacial FABDEM"
      ),
      metric_row(
        "Relieve",
        "Rango altitudinal",
        "H",
        relief$relief_m,
        "m",
        "Zmax - Zmin"
      ),
      metric_row(
        "Relieve",
        "Pendiente media de cuenca",
        "Sb",
        relief$slope_mean_pct,
        "%",
        "terrain(FABDEM), pendiente celda a celda"
      ),
      metric_row(
        "Relieve",
        "Pendiente mediana de cuenca",
        "S50",
        relief$slope_median_pct,
        "%",
        "Mediana espacial de terrain(FABDEM)"
      ),
      metric_row(
        "Relieve",
        "Integral hipsometrica",
        "HI",
        relief$hypsometric_integral,
        "adim.",
        "(Zmean-Zmin)/(Zmax-Zmin)"
      )
    )

    do.call(
      rbind,
      rows
    )
  }


  build_imported_advanced_table <- function(
      geom,
      relief
  ) {

    qz <- relief$elevation_quantiles
    qs <- relief$slope_quantiles

    rows <- list(
      metric_row(
        "Relieve avanzado",
        "Elevacion P10",
        "Z10",
        qz[1],
        "m",
        "Percentil aproximado de muestra regular FABDEM"
      ),
      metric_row(
        "Relieve avanzado",
        "Elevacion P25",
        "Z25",
        qz[2],
        "m",
        "Percentil aproximado de muestra regular FABDEM"
      ),
      metric_row(
        "Relieve avanzado",
        "Elevacion P75",
        "Z75",
        qz[3],
        "m",
        "Percentil aproximado de muestra regular FABDEM"
      ),
      metric_row(
        "Relieve avanzado",
        "Elevacion P90",
        "Z90",
        qz[4],
        "m",
        "Percentil aproximado de muestra regular FABDEM"
      ),
      metric_row(
        "Relieve avanzado",
        "Pendiente P10",
        "S10",
        qs[1],
        "%",
        "Percentil aproximado de muestra regular"
      ),
      metric_row(
        "Relieve avanzado",
        "Pendiente P50",
        "S50",
        qs[2],
        "%",
        "Percentil aproximado de muestra regular"
      ),
      metric_row(
        "Relieve avanzado",
        "Pendiente P90",
        "S90",
        qs[3],
        "%",
        "Percentil aproximado de muestra regular"
      ),
      metric_row(
        "Relieve avanzado",
        "Rugosidad de Melton",
        "MRN",
        relief$melton,
        "adim.",
        "H(km)/sqrt(A)"
      )
    )

    do.call(
      rbind,
      rows
    )
  }


  build_imported_primary_progress_table <- function(
      x,
      stages
  ) {

    geom <- x$geom
    relief <- x$relief

    geometry_value <- function(name) {
      stage_value_text(
        stages$geometry,
        if (!is.null(geom)) geom[[name]] else NULL
      )
    }

    relief_value <- function(name) {
      stage_value_text(
        stages$relief,
        if (!is.null(relief)) relief[[name]] else NULL
      )
    }

    data.frame(
      GRUPO = c(
        rep("Geometria", 3),
        rep("Forma", 2),
        rep("Relieve", 8)
      ),
      PARAMETRO = c(
        "Area",
        "Perimetro",
        "Diametro equivalente",
        "Relacion de circularidad",
        "Coeficiente de compacidad",
        "Elevacion minima",
        "Elevacion maxima",
        "Elevacion media",
        "Elevacion mediana",
        "Rango altitudinal",
        "Pendiente media de cuenca",
        "Pendiente mediana de cuenca",
        "Integral hipsometrica"
      ),
      SIMBOLO = c(
        "A", "P", "Deq", "Rc", "Kc",
        "Zmin", "Zmax", "Zmean", "Z50", "H", "Sb", "S50", "HI"
      ),
      VALOR = c(
        geometry_value("area_km2"),
        geometry_value("perimeter_km"),
        geometry_value("equivalent_diameter_km"),
        geometry_value("circularity_ratio"),
        geometry_value("compactness_coefficient"),
        relief_value("zmin"),
        relief_value("zmax"),
        relief_value("zmean"),
        relief_value("zmedian"),
        relief_value("relief_m"),
        relief_value("slope_mean_pct"),
        relief_value("slope_median_pct"),
        relief_value("hypsometric_integral")
      ),
      UNIDAD = c(
        "km2", "km", "km", "adim.", "adim.",
        "m", "m", "m", "m", "m", "%", "%", "adim."
      ),
      METODO = c(
        "Poligono importado en UTM local",
        "Borde del poligono importado en UTM local",
        "2*sqrt(A/pi)",
        "4*pi*A/P2",
        "P/(2*sqrt(pi*A))",
        "FABDEM recortado por la geometria importada",
        "FABDEM recortado por la geometria importada",
        "Media espacial FABDEM",
        "Mediana espacial FABDEM",
        "Zmax - Zmin",
        "terrain(FABDEM), pendiente celda a celda",
        "Mediana espacial de terrain(FABDEM)",
        "(Zmean-Zmin)/(Zmax-Zmin)"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }


  display_table <- function(x) {

    out <- x


    out$VALOR <- vapply(
      out$VALOR,
      fmt,
      character(1),
      digits = 3
    )


    out
  }


  stage_value_text <- function(
      stage_state,
      value = NULL,
      digits = 3
  ) {

    if (identical(
      stage_state,
      "ready"
    )) {
      return(
        fmt(
          value,
          digits = digits
        )
      )
    }


    if (identical(
      stage_state,
      "calculating"
    )) {
      return(
        "Calculando..."
      )
    }


    if (identical(
      stage_state,
      "error"
    )) {
      return(
        "Error"
      )
    }


    "En espera..."
  }


  waiting_table <- function(message) {

    data.frame(
      ESTADO = message,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }


  draw_stage_message <- function(message) {

    graphics::plot.new()
    graphics::box(
      col = "grey85"
    )
    graphics::text(
      0.5,
      0.5,
      labels = message,
      col = "grey40",
      cex = 1.0
    )


    invisible(NULL)
  }


  build_primary_progress_table <- function(
      x,
      stages
  ) {

    if (identical(
      x$basin_source,
      "imported"
    )) {
      return(
        build_imported_primary_progress_table(
          x,
          stages
        )
      )
    }

    geom <- x$geom
    relief <- x$relief
    hydro_metrics <- x$hydro
    network_metrics <- x$network
    tc_metrics <- x$tc


    geometry_value <- function(name) {
      stage_value_text(
        stages$geometry,
        if (!is.null(geom)) geom[[name]] else NULL
      )
    }


    relief_value <- function(name) {
      stage_value_text(
        stages$relief,
        if (!is.null(relief)) relief[[name]] else NULL
      )
    }


    hydro_value <- function(section, name) {
      value <- NULL

      if (!is.null(
        hydro_metrics
      ) && !is.null(
        hydro_metrics[[section]]
      )) {
        value <- hydro_metrics[[section]][[name]]
      }

      stage_value_text(
        stages$hydro,
        value
      )
    }


    network_value <- function(name) {
      stage_value_text(
        stages$network,
        if (!is.null(network_metrics)) network_metrics[[name]] else NULL
      )
    }


    tc_value <- function(name) {
      stage_value_text(
        stages$tc,
        if (!is.null(tc_metrics)) tc_metrics[[name]] else NULL
      )
    }


    data.frame(
      GRUPO = c(
        rep("Geometria", 3),
        rep("Relieve", 7),
        rep("Recorrido", 3),
        "Cauce",
        rep("Red", 4),
        rep("Forma", 4),
        rep("Respuesta", 3)
      ),
      PARAMETRO = c(
        "Area",
        "Perimetro",
        "Longitud de cuenca",
        "Elevacion minima",
        "Elevacion maxima",
        "Elevacion media",
        "Elevacion mediana",
        "Rango altitudinal",
        "Pendiente media de cuenca",
        "Integral hipsometrica",
        "Recorrido hidraulico mas largo",
        "Desnivel hidraulico",
        "Pendiente 10-85",
        "Longitud del cauce principal",
        "Longitud total de cauces",
        "Densidad de drenaje",
        "Orden Strahler maximo",
        "Numero de links",
        "Factor de forma",
        "Relacion de elongacion",
        "Relacion de circularidad",
        "Coeficiente de compacidad",
        "Tc Kirpich",
        "Tc Giandotti",
        "Tc Temez"
      ),
      SIMBOLO = c(
        "A", "P", "Lb",
        "Zmin", "Zmax", "Zmean", "Z50", "H", "Sb", "HI",
        "Lh", "Hh", "S10-85", "Lc",
        "Lt", "Dd", "Omega", "Nu",
        "Ff", "Re", "Rc", "Kc",
        "Tc_K", "Tc_G", "Tc_T"
      ),
      VALOR = c(
        geometry_value("area_km2"),
        geometry_value("perimeter_km"),
        geometry_value("basin_length_km"),
        relief_value("zmin"),
        relief_value("zmax"),
        relief_value("zmean"),
        relief_value("zmedian"),
        relief_value("relief_m"),
        relief_value("slope_mean_pct"),
        relief_value("hypsometric_integral"),
        hydro_value("hydraulic", "length_km"),
        hydro_value("hydraulic", "drop_m"),
        hydro_value("hydraulic", "slope_10_85_m_km"),
        hydro_value("channel", "length_km"),
        network_value("total_length_km"),
        network_value("drainage_density_km_km2"),
        network_value("max_order"),
        network_value("n_links"),
        geometry_value("form_factor"),
        geometry_value("elongation_ratio"),
        geometry_value("circularity_ratio"),
        geometry_value("compactness_coefficient"),
        tc_value("kirpich_h"),
        tc_value("giandotti_h"),
        tc_value("temez_h")
      ),
      UNIDAD = c(
        "km2", "km", "km",
        "m", "m", "m", "m", "m", "%", "adim.",
        "km", "m", "m/km", "km",
        "km", "km/km2", "orden", "n",
        "adim.", "adim.", "adim.", "adim.",
        "h", "h", "h"
      ),
      METODO = c(
        "Poligono de cuenca en UTM local",
        "Borde del poligono en UTM local",
        "Outlet al punto mas lejano de la divisoria",
        "FABDEM recortado por cuenca",
        "FABDEM recortado por cuenca",
        "Media espacial FABDEM",
        "Mediana espacial FABDEM",
        "Zmax - Zmin",
        "terrain(FABDEM), pendiente celda a celda",
        "(Zmean-Zmin)/(Zmax-Zmin)",
        "Trayectoria D8 mas larga hasta el outlet",
        "Zremote - Zout sobre el recorrido D8",
        "Perfil D8 entre 10% y 85% de Lh desde el outlet",
        "Recorrido mas largo sobre stream_stripes conectado al outlet",
        "Suma de longitudes D8 de la red conectada al outlet",
        "Lt/A; dependiente del umbral de extraccion de cauces",
        "Orden Strahler maximo de la red conectada",
        "Tramos topologicos entre cabeceras, confluencias y outlet",
        "A/Lb2",
        "2*sqrt(A/pi)/Lb",
        "4*pi*A/P2",
        "P/(2*sqrt(pi*A))",
        "Kirpich con Lh y pendiente hidraulica total",
        "Giandotti con A, Lh y elevacion media sobre el outlet",
        "Temez con Lh y pendiente hidraulica total"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }



  morph_datatable <- function(
      x,
      export_stem
  ) {

    DT::datatable(
      x,
      rownames = FALSE,
      extensions = "Buttons",
      class = "compact stripe hover cell-border",
      width = "100%",
      fillContainer = FALSE,
      options = list(
        dom = "Brt",
        buttons = list(
          list(
            extend = "copy",
            text = "Copiar"
          ),
          list(
            extend = "csv",
            text = "CSV",
            filename = export_stem,
            title = NULL
          ),
          list(
            extend = "excel",
            text = "Excel",
            filename = export_stem,
            title = NULL
          )
        ),
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = FALSE,
        scrollX = FALSE,
        language = list(
          buttons = list(
            copyTitle = "Tabla copiada",
            copySuccess = list(
              `_` = "%d filas copiadas",
              `1` = "1 fila copiada"
            )
          )
        )
      )
    )
  }


  # ==========================================================
  # 10. UI
  # ==========================================================

  ui <- function(id) {

    ns <- shiny::NS(
      id
    )


    morph_plot_header <- function(
        title,
        plot_id,
        download_id
    ) {

      shiny::div(
        class = "morph-plot-head",
        shiny::tags$h4(
          title
        ),
        shiny::div(
          class = "morph-plot-actions",
          shiny::tags$button(
            type = "button",
            class = "btn btn-default btn-sm morph-copy-btn",
            title = "Copiar gráfico como imagen PNG al portapapeles",
            onclick = sprintf(
              "copyMorphPlot('%s', this);",
              ns(
                plot_id
              )
            ),
            "Copiar"
          ),
          shiny::downloadButton(
            ns(
              download_id
            ),
            "PNG",
            class = "btn-default btn-sm"
          )
        )
      )
    }


    shiny::tagList(

      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".morph-wrap{padding:16px 18px 30px 18px;max-width:1500px;margin:auto;}",
            ".morph-head{display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:12px;}",
            ".morph-note{background:#f6f7f8;border-left:4px solid #607D8B;padding:10px 12px;margin:10px 0 14px 0;}",
            ".morph-map{margin-bottom:16px;}.morph-map-head{display:flex;align-items:center;justify-content:space-between;gap:12px;margin-bottom:8px;}.morph-map-head h4{margin:0;}.morph-a3-frame{width:1188px;max-width:100%;margin:0 auto;aspect-ratio:420/297;}.morph-grid{display:grid;grid-template-columns:1fr 1fr;gap:16px;}.morph-full{margin-top:16px;}",
            ".morph-card{border:1px solid #ddd;border-radius:7px;padding:12px;background:white;}",
            ".morph-card h4{margin-top:0;}.morph-plot-head{display:flex;align-items:center;justify-content:space-between;gap:10px;margin-bottom:6px;}.morph-plot-head h4{margin:0;}.morph-plot-actions{display:flex;gap:6px;align-items:center;flex-wrap:wrap;}.morph-copy-btn{min-width:62px;}",
            ".morph-details{margin-top:12px;border:1px solid #ddd;border-radius:7px;padding:9px 12px;}",
            ".morph-details summary{cursor:pointer;font-weight:600;}",
            ".morph-pending{color:#666;font-size:13px;}",
            ".morph-stagebar{display:grid;grid-template-columns:repeat(5,1fr);gap:8px;margin:8px 0 14px 0;}",
            ".morph-stage{border:1px solid #ddd;border-radius:6px;padding:8px 10px;background:#fff;font-size:12px;}",
            ".morph-stage strong{display:block;font-size:13px;margin-bottom:2px;}",
            ".morph-stage-ready{border-left:4px solid #2E7D32;}",
            ".morph-stage-calculating{border-left:4px solid #F9A825;background:#FFFDE7;}",
            ".morph-stage-waiting{border-left:4px solid #9E9E9E;color:#666;}",
            ".morph-stage-error{border-left:4px solid #C62828;background:#FFEBEE;}",
            "@media(max-width:900px){.morph-grid{grid-template-columns:1fr;}.morph-stagebar{grid-template-columns:1fr 1fr;}}.morph-card .dt-buttons,.morph-details .dt-buttons{margin:0 0 10px 0;display:flex;gap:6px;flex-wrap:wrap;}.morph-card .dt-button,.morph-details .dt-button{padding:5px 10px;border:1px solid #bdbdbd;border-radius:4px;background:#fff;cursor:pointer;font-size:12px;}.morph-card .dt-button:hover,.morph-details .dt-button:hover{background:#f3f3f3;}.morph-card .dataTables_wrapper,.morph-details .dataTables_wrapper{width:100%;margin:0 0 12px 0;overflow-x:auto;}.morph-card table.dataTable,.morph-details table.dataTable{width:100%!important;margin:0!important;table-layout:auto;}.morph-card table.dataTable thead th,.morph-details table.dataTable thead th{white-space:nowrap;vertical-align:bottom;}.morph-card table.dataTable tbody td,.morph-details table.dataTable tbody td{vertical-align:top;}.morph-full .morph-card{padding-bottom:18px;}.morph-details{padding-bottom:14px;}"
          )
        )
      ),

      shiny::tags$script(
        shiny::HTML(
          "
          window.copyMorphPlot = async function(plotId, btn) {
            const originalText = btn ? btn.textContent : 'Copiar';
            try {
              const holder = document.getElementById(plotId);
              if (!holder) throw new Error('No se encontró el gráfico.');
              const img = holder.tagName === 'IMG' ? holder : holder.querySelector('img');
              if (!img || !img.src) throw new Error('El gráfico aún no está disponible.');
              if (!navigator.clipboard || typeof ClipboardItem === 'undefined') {
                throw new Error('Este navegador no permite copiar imágenes directamente.');
              }
              const response = await fetch(img.src, {credentials: 'same-origin'});
              if (!response.ok) throw new Error('No se pudo leer la imagen del gráfico.');
              let blob = await response.blob();
              if (blob.type !== 'image/png') {
                const bitmap = await createImageBitmap(blob);
                const canvas = document.createElement('canvas');
                canvas.width = bitmap.width;
                canvas.height = bitmap.height;
                const ctx = canvas.getContext('2d');
                ctx.drawImage(bitmap, 0, 0);
                blob = await new Promise(function(resolve, reject) {
                  canvas.toBlob(function(b) {
                    if (b) resolve(b); else reject(new Error('No se pudo convertir a PNG.'));
                  }, 'image/png');
                });
              }
              await navigator.clipboard.write([
                new ClipboardItem({'image/png': blob})
              ]);
              if (btn) {
                btn.textContent = 'Copiado';
                setTimeout(function(){ btn.textContent = originalText; }, 1400);
              }
            } catch (err) {
              console.error(err);
              if (btn) {
                btn.textContent = 'Usa PNG';
                setTimeout(function(){ btn.textContent = originalText; }, 1800);
              }
            }
          };
          "
        )
      ),


      shiny::div(
        class = "morph-wrap",


        shiny::div(
          class = "morph-head",

          shiny::actionButton(
            ns(
              "calcular"
            ),
            "Calcular morfometría",
            class = "btn-success"
          ),

          shiny::actionButton(
            ns(
              "limpiar"
            ),
            "Limpiar resultados"
          ),

          shiny::strong(
            shiny::textOutput(
              ns(
                "estado"
              ),
              inline = TRUE
            )
          )
        ),


        shiny::uiOutput(
          ns(
            "source_note"
          )
        ),

        layer_source_ui("fabdem_dem"),

        shiny::uiOutput(
          ns(
            "pipeline_status"
          )
        ),


        shiny::conditionalPanel(
          condition = paste0(
            "output['",
            ns(
              "has_results"
            ),
            "'] === 'true'"
          ),


          shiny::div(
            class = "morph-map",
            shiny::div(
              class = "morph-card",
              shiny::div(
                class = "morph-map-head",
                shiny::tags$h4(
                  "Mapa fisiografico"
                ),
                shiny::div(
                  style = "display:flex;gap:8px;align-items:center;flex-wrap:wrap;",
                  shiny::downloadButton(
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
                  ),
                  shiny::downloadButton(
                    ns(
                      "descargar_mapa_png"
                    ),
                    "Descargar PNG",
                    class = "btn-primary btn-sm"
                  )
                )
              ),
              shiny::div(
                class = "morph-a3-frame",
                shiny::plotOutput(
                  ns(
                    "mapa_fisiografico"
                  ),
                  width = "100%",
                  height = "100%"
                )
              )
            )
          ),

          shiny::div(
            class = "morph-grid",

            shiny::div(
              class = "morph-card",
              morph_plot_header(
                "Curva hipsometrica",
                "hipsometrica",
                "descargar_hipsometrica_png"
              ),
              shiny::plotOutput(
                ns(
                  "hipsometrica"
                ),
                height = "340px"
              )
            ),

            shiny::div(
              class = "morph-card",
              morph_plot_header(
                "Distribucion altitudinal",
                "hist_elevacion",
                "descargar_hist_elevacion_png"
              ),
              shiny::plotOutput(
                ns(
                  "hist_elevacion"
                ),
                height = "340px"
              )
            ),

            shiny::div(
              class = "morph-card",
              morph_plot_header(
                "Distribucion de pendientes",
                "hist_pendiente",
                "descargar_hist_pendiente_png"
              ),
              shiny::plotOutput(
                ns(
                  "hist_pendiente"
                ),
                height = "340px"
              )
            ),

            shiny::conditionalPanel(
              condition = paste0(
                "output['",
                ns(
                  "full_hydro_available"
                ),
                "'] === 'true'"
              ),
              shiny::div(
                class = "morph-card",
                morph_plot_header(
                  "Perfil longitudinal",
                  "perfil_longitudinal",
                  "descargar_perfil_longitudinal_png"
                ),
                shiny::plotOutput(
                  ns(
                    "perfil_longitudinal"
                  ),
                  height = "340px"
                )
              )
            )
          ),

          shiny::div(
            class = "morph-full",
            shiny::div(
              class = "morph-card",
              shiny::tags$h4(
                "Parametros principales"
              ),
              DT::DTOutput(
                ns(
                  "tabla_principal"
                )
              )
            )
          ),


          shiny::tags$details(
            class = "morph-details",
            shiny::tags$summary(
              "Geometria y relieve avanzados"
            ),
            DT::DTOutput(
              ns(
                "tabla_avanzada"
              )
            )
          ),


          shiny::conditionalPanel(
            condition = paste0(
              "output['",
              ns(
                "full_hydro_available"
              ),
              "'] === 'true'"
            ),
          shiny::tags$details(
            class = "morph-details",
            shiny::tags$summary(
              "Recorrido hidraulico y cauce principal"
            ),
            DT::DTOutput(
              ns(
                "tabla_recorrido_cauce"
              )
            )
          ),


          shiny::tags$details(
            class = "morph-details",
            shiny::tags$summary(
              "Red de drenaje y Strahler"
            ),
            shiny::div(
              class = "morph-note",
              "Todas las metricas de red se calculan exclusivamente sobre stream_stripes conectados al outlet y dependen del umbral de extraccion indicado como Ath."
            ),
            DT::DTOutput(
              ns(
                "tabla_red"
              )
            ),
            shiny::tags$h5(
              "Resumen por orden Strahler"
            ),
            DT::DTOutput(
              ns(
                "tabla_strahler"
              )
            )
          ),


          shiny::tags$details(
            class = "morph-details",
            shiny::tags$summary(
              "Indices derivados de la red"
            ),
            shiny::div(
              class = "morph-pending",
              "Son transformaciones algebraicas de Dd y Fs; se muestran como indices secundarios y no como informacion independiente."
            ),
            DT::DTOutput(
              ns(
                "tabla_red_derivada"
              )
            )
          ),


          shiny::tags$details(
            class = "morph-details",
            shiny::tags$summary(
              "Tiempos de concentracion"
            ),
            shiny::div(
              class = "morph-note",
              "Se muestran tres estimaciones empiricas independientes y su rango. No se calcula un Tc adoptado ni se promedian automaticamente las ecuaciones."
            ),
            DT::DTOutput(
              ns(
                "tabla_tc"
              )
            )
          )

          )        )
      )
    )
  }


  # ==========================================================
  # 11. SERVER
  # ==========================================================

  server <- function(
      id,
      basin,
      outlet,
      folder,
      block_id,
      hydro_context,
      basin_source,
      basin_label
  ) {

    shiny::moduleServer(
      id,
      function(
          input,
          output,
          session
      ) {

        result <- shiny::reactiveVal(
          NULL
        )


        status <- shiny::reactiveVal(
          "Delimita o carga una cuenca y pulsa Calcular morfometría."
        )


        stages <- shiny::reactiveValues(
          geometry = "waiting",
          relief = "waiting",
          hydro = "waiting",
          tc = "waiting",
          network = "waiting"
        )


        stage_errors <- shiny::reactiveValues(
          geometry = NULL,
          relief = NULL,
          hydro = NULL,
          tc = NULL,
          network = NULL
        )


        pipeline_trigger <- shiny::reactiveVal(
          NULL
        )


        run_id <- shiny::reactiveVal(
          0L
        )


        snapshot_stages <- function() {

          list(
            geometry = stages$geometry,
            relief = stages$relief,
            hydro = stages$hydro,
            tc = stages$tc,
            network = stages$network
          )
        }


        reset_pipeline <- function() {

          stages$geometry <- "waiting"
          stages$relief <- "waiting"
          stages$hydro <- "waiting"
          stages$tc <- "waiting"
          stages$network <- "waiting"

          stage_errors$geometry <- NULL
          stage_errors$relief <- NULL
          stage_errors$hydro <- NULL
          stage_errors$tc <- NULL
          stage_errors$network <- NULL

          pipeline_trigger(
            NULL
          )


          invisible(NULL)
        }


        update_result <- function(values) {

          x <- result()


          if (is.null(
            x
          )) {
            x <- list()
          }


          for (nm in names(
            values
          )) {
            x[[nm]] <- values[[nm]]
          }


          result(
            x
          )


          invisible(
            x
          )
        }


        schedule_stage <- function(
            stage_name,
            token,
            delay = 0.12
        ) {

          later::later(
            function() {

              current_token <- shiny::isolate(
                run_id()
              )


              if (!identical(
                current_token,
                token
              )) {
                return(
                  invisible(NULL)
                )
              }


              pipeline_trigger(
                list(
                  stage = stage_name,
                  token = token,
                  nonce = stats::runif(
                    1
                  )
                )
              )
            },
            delay = delay
          )


          invisible(NULL)
        }


        current_basin_key <- shiny::reactive({

          b <- basin()


          if (is.null(b)) {
            return(NULL)
          }


          source_value <- basin_source()

          if (
            is.null(
              source_value
            ) ||
            !nzchar(
              as.character(
                source_value
              )
            )
          ) {
            source_value <- if (is.null(
              outlet()
            )) {
              "imported"
            } else {
              "delineated"
            }
          }

          block_value <- block_id()

          if (
            is.null(
              block_value
            ) ||
            length(
              block_value
            ) == 0L
          ) {
            block_value <- "NO_BLOCK"
          }

          paste0(
            source_value,
            "|",
            block_value,
            "|",
            nrow(b),
            "|",
            paste(
              round(
                as.numeric(
                  sf::st_bbox(
                    b
                  )
                ),
                6
              ),
              collapse = "|"
            )
          )
        })


        shiny::observeEvent(
          current_basin_key(),
          {
            run_id(
              shiny::isolate(
                run_id()
              ) +
                1L
            )

            result(
              NULL
            )

            reset_pipeline()

            status(
              "Cuenca disponible. Pulsa Calcular morfometría."
            )
          },
          ignoreInit = TRUE
        )


        shiny::observeEvent(
          input$calcular,
          {

            token <- shiny::isolate(
              run_id()
            ) +
              1L


            run_id(
              token
            )

            result(
              NULL
            )

            reset_pipeline()


            tryCatch(
              {

                b <- basin()
                o <- outlet()
                out_dir <- folder()
                hydro <- hydro_context()
                source_value <- basin_source()
                label_value <- basin_label()


                if (is.null(
                  b
                )) {
                  stop(
                    "Primero delimita o carga una cuenca en el módulo Delimitación."
                  )
                }


                if (
                  is.null(
                    source_value
                  ) ||
                  !nzchar(
                    as.character(
                      source_value
                    )
                  )
                ) {
                  source_value <- if (is.null(
                    o
                  )) {
                    "imported"
                  } else {
                    "delineated"
                  }
                }


                source_value <- as.character(
                  source_value
                )


                if (identical(
                  source_value,
                  "delineated"
                )) {

                  if (is.null(
                    o
                  )) {
                    stop(
                      "La cuenca delimitada no tiene outlet activo."
                    )
                  }

                  if (is.null(
                    hydro
                  )) {
                    stop(
                      "El contexto hidrologico de la cuenca no esta disponible. Vuelve a delimitarla."
                    )
                  }

                  if (!identical(
                    as.character(
                      hydro$block_id
                    ),
                    as.character(
                      block_id()
                    )
                  )) {
                    stop(
                      "El bloque del contexto hidrologico no coincide con la cuenca activa."
                    )
                  }
                }


                stages$geometry <- "calculating"
                status(
                  if (identical(
                    source_value,
                    "imported"
                  )) {
                    "Calculando geometría de la cuenca importada..."
                  } else {
                    "Calculando geometría básica..."
                  }
                )


                geom <- if (identical(
                  source_value,
                  "imported"
                )) {
                  geometry_metrics_imported(
                    b
                  )
                } else {
                  geometry_metrics(
                    b,
                    o
                  )
                }


                stages$geometry <- "ready"


                active_block_value <- block_id()

                if (
                  is.null(
                    active_block_value
                  ) ||
                  length(
                    active_block_value
                  ) == 0L
                ) {
                  active_block_value <- "IMPORTED"
                }


                update_result(
                  list(
                    geom = geom,
                    basin_sf = b,
                    outlet_sf = o,
                    hydro_context = hydro,
                    out_dir = out_dir,
                    active_block_id = as.character(
                      active_block_value
                    ),
                    basin_source = source_value,
                    basin_label = label_value
                  )
                )


                status(
                  "Geometría lista. Preparando FABDEM..."
                )


                schedule_stage(
                  "relief_start",
                  token
                )

              },
              error = function(e) {

                stages$geometry <- "error"
                stage_errors$geometry <- conditionMessage(
                  e
                )

                error_message <- paste0(
                  "Error en geometría: ",
                  conditionMessage(
                    e
                  )
                )

                status(
                  error_message
                )

                shiny::showNotification(
                  error_message,
                  type = "error",
                  duration = NULL
                )
              }
            )
          }
        )


        shiny::observeEvent(
          pipeline_trigger(),
          {

            trig <- pipeline_trigger()


            shiny::req(
              !is.null(
                trig
              )
            )


            pipeline_trigger(
              NULL
            )


            token <- trig$token
            stage_name <- trig$stage


            if (!identical(
              shiny::isolate(
                run_id()
              ),
              token
            )) {
              return()
            }


            if (identical(
              stage_name,
              "relief_start"
            )) {

              stages$relief <- "calculating"
              status(
                "Cargando teselas FABDEM y calculando relieve..."
              )

              schedule_stage(
                "relief_run",
                token
              )

              return()
            }


            if (identical(
              stage_name,
              "relief_run"
            )) {

              tryCatch(
                {

                  x <- result()

                  shiny::req(
                    x,
                    x$geom,
                    x$basin_sf
                  )


                  tile_index <- get_dem_tile_index()

                  selected <- select_dem_tiles(
                    x$basin_sf,
                    tile_index
                  )


                  job_id <- paste0(
                    format(
                      Sys.time(),
                      "%Y%m%d_%H%M%S"
                    ),
                    "_",
                    gsub(
                      "[^A-Za-z0-9_]",
                      "_",
                      x$active_block_id
                    )
                  )


                  dem <- dem_for_basin(
                    basin = x$basin_sf,
                    selected_tiles = selected,
                    job_id = job_id
                  )


                  relief <- relief_metrics(
                    dem,
                    x$geom
                  )


                  advanced <- if (identical(
                    x$basin_source,
                    "imported"
                  )) {
                    build_imported_advanced_table(
                      x$geom,
                      relief
                    )
                  } else {
                    build_advanced_table(
                      x$geom,
                      relief
                    )
                  }


                  update_result(
                    list(
                      relief = relief,
                      dem = dem,
                      advanced = advanced,
                      n_tiles = nrow(
                        selected
                      ),
                      tile_names = as.character(
                        selected$TILE_NAME
                      )
                    )
                  )


                  if (
                    !is.null(
                      x$out_dir
                    ) &&
                    dir.exists(
                      x$out_dir
                    )
                  ) {
                    write.csv(
                      advanced,
                      file.path(
                        x$out_dir,
                        "morfometria_expandida.csv"
                      ),
                      row.names = FALSE,
                      fileEncoding = "UTF-8"
                    )

                    write.csv(
                      data.frame(
                        TILE_NAME = as.character(
                          selected$TILE_NAME
                        ),
                        RELATIVE_PATH = as.character(
                          selected$RELATIVE_PATH
                        ),
                        stringsAsFactors = FALSE
                      ),
                      file.path(
                        x$out_dir,
                        "morfometria_dem_tiles.csv"
                      ),
                      row.names = FALSE,
                      fileEncoding = "UTF-8"
                    )
                  }


                  stages$relief <- "ready"

                  if (identical(
                    x$basin_source,
                    "imported"
                  )) {

                    imported_primary <- build_imported_primary_table(
                      x$geom,
                      relief
                    )

                    update_result(
                      list(
                        primary = imported_primary
                      )
                    )

                    if (
                      !is.null(
                        x$out_dir
                      ) &&
                      dir.exists(
                        x$out_dir
                      )
                    ) {
                      write.csv(
                        imported_primary,
                        file.path(
                          x$out_dir,
                          "morfometria_principal_importada.csv"
                        ),
                        row.names = FALSE,
                        fileEncoding = "UTF-8"
                      )
                    }

                    status(
                      paste0(
                        "Morfometría de cuenca importada lista | ",
                        nrow(selected),
                        " tesela(s) FABDEM. No se infiere outlet, recorrido D8, red ni Tc."
                      )
                    )

                    return()
                  }

                  status(
                    paste0(
                      "Relieve listo | ",
                      nrow(selected),
                      " tesela(s) FABDEM. Preparando recorrido hidráulico..."
                    )
                  )


                  schedule_stage(
                    "hydro_start",
                    token
                  )

                },
                error = function(e) {

                  stages$relief <- "error"
                  stage_errors$relief <- conditionMessage(
                    e
                  )

                  error_message <- paste0(
                    "Error en relieve: ",
                    conditionMessage(
                      e
                    )
                  )

                  status(
                    error_message
                  )

                  shiny::showNotification(
                    error_message,
                    type = "error",
                    duration = NULL
                  )
                }
              )

              return()
            }


            if (identical(
              stage_name,
              "hydro_start"
            )) {

              stages$hydro <- "calculating"
              status(
                "Calculando recorrido hidráulico D8 y cauce principal..."
              )

              schedule_stage(
                "hydro_run",
                token
              )

              return()
            }


            if (identical(
              stage_name,
              "hydro_run"
            )) {

              tryCatch(
                {

                  x <- result()

                  shiny::req(
                    x,
                    x$hydro_context,
                    x$outlet_sf,
                    x$dem
                  )


                  hydro_metrics <- hydro_path_metrics(
                    hydro = x$hydro_context,
                    outlet = x$outlet_sf,
                    dem = x$dem,
                    progress_fun = function(
                        phase,
                        batches,
                        n_cells,
                        far_dist
                    ) {

                      label <- if (identical(
                        phase,
                        "hydraulic"
                      )) {
                        "Recorrido D8"
                      } else {
                        "Cauce principal"
                      }

                      status(
                        paste0(
                          "Calculando ",
                          label,
                          " | ",
                          format(
                            n_cells,
                            big.mark = ",",
                            scientific = FALSE
                          ),
                          " celdas | ",
                          sprintf(
                            "%.1f km",
                            far_dist /
                              1000
                          )
                        )
                      )
                    }
                  )


                  flow_channel <- build_flow_channel_table(
                    hydro_metrics
                  )


                  update_result(
                    list(
                      hydro = hydro_metrics,
                      flow_channel = flow_channel
                    )
                  )


                  if (
                    !is.null(
                      x$out_dir
                    ) &&
                    dir.exists(
                      x$out_dir
                    )
                  ) {
                    write.csv(
                      flow_channel,
                      file.path(
                        x$out_dir,
                        "morfometria_recorrido_cauce.csv"
                      ),
                      row.names = FALSE,
                      fileEncoding = "UTF-8"
                    )

                    hydraulic_profile_out <- hydro_metrics$hydraulic_profile
                    hydraulic_profile_out$TIPO <- "RECORRIDO_HIDRAULICO"

                    channel_profile_out <- hydro_metrics$channel_profile
                    channel_profile_out$TIPO <- "CAUCE_PRINCIPAL"

                    write.csv(
                      rbind(
                        hydraulic_profile_out,
                        channel_profile_out
                      ),
                      file.path(
                        x$out_dir,
                        "morfometria_perfiles.csv"
                      ),
                      row.names = FALSE,
                      fileEncoding = "UTF-8"
                    )
                  }


                  stages$hydro <- "ready"
                  status(
                    "Recorrido hidráulico y cauce principal listos. Calculando tiempos de concentración..."
                  )


                  schedule_stage(
                    "tc_start",
                    token
                  )

                },
                error = function(e) {

                  stages$hydro <- "error"
                  stage_errors$hydro <- conditionMessage(
                    e
                  )

                  error_message <- paste0(
                    "Error en recorrido hidráulico: ",
                    conditionMessage(
                      e
                    )
                  )

                  status(
                    error_message
                  )

                  shiny::showNotification(
                    error_message,
                    type = "error",
                    duration = NULL
                  )
                }
              )

              return()
            }


            if (identical(
              stage_name,
              "tc_start"
            )) {

              stages$tc <- "calculating"
              status(
                "Calculando tiempos de concentración..."
              )

              schedule_stage(
                "tc_run",
                token,
                delay = 0.06
              )

              return()
            }


            if (identical(
              stage_name,
              "tc_run"
            )) {

              tryCatch(
                {

                  x <- result()

                  shiny::req(
                    x,
                    x$geom,
                    x$relief,
                    x$hydro
                  )


                  tc_metrics <- concentration_time_metrics(
                    geom = x$geom,
                    relief = x$relief,
                    hydro_metrics = x$hydro
                  )


                  tc_table <- build_concentration_time_table(
                    tc_metrics
                  )


                  update_result(
                    list(
                      tc = tc_metrics,
                      tc_table = tc_table
                    )
                  )


                  if (
                    !is.null(
                      x$out_dir
                    ) &&
                    dir.exists(
                      x$out_dir
                    )
                  ) {
                    write.csv(
                      tc_table,
                      file.path(
                        x$out_dir,
                        "morfometria_tiempos_concentracion.csv"
                      ),
                      row.names = FALSE,
                      fileEncoding = "UTF-8"
                    )
                  }


                  stages$tc <- "ready"
                  status(
                    "Tiempos de concentración listos. Preparando red de drenaje y Strahler..."
                  )


                  schedule_stage(
                    "network_start",
                    token
                  )

                },
                error = function(e) {

                  stages$tc <- "error"
                  stage_errors$tc <- conditionMessage(
                    e
                  )

                  error_message <- paste0(
                    "Error en tiempos de concentración: ",
                    conditionMessage(
                      e
                    )
                  )

                  status(
                    error_message
                  )

                  shiny::showNotification(
                    error_message,
                    type = "error",
                    duration = NULL
                  )

                  schedule_stage(
                    "network_start",
                    token
                  )
                }
              )

              return()
            }


            if (identical(
              stage_name,
              "network_start"
            )) {

              stages$network <- "calculating"
              status(
                "Calculando red de drenaje y ordenamiento Strahler..."
              )

              schedule_stage(
                "network_run",
                token
              )

              return()
            }


            if (identical(
              stage_name,
              "network_run"
            )) {

              tryCatch(
                {

                  x <- result()

                  shiny::req(
                    x,
                    x$hydro_context,
                    x$hydro,
                    x$geom,
                    x$relief
                  )


                  network_metrics <- stream_network_metrics(
                    hydro = x$hydro_context,
                    outlet_cell = x$hydro$outlet_cell,
                    geom = x$geom,
                    relief = x$relief,
                    progress_fun = function(
                        batches,
                        n_stream_cells,
                        edge_chunks
                    ) {

                      status(
                        paste0(
                          "Calculando red | ",
                          format(
                            n_stream_cells,
                            big.mark = ",",
                            scientific = FALSE
                          ),
                          " celdas de cauce"
                        )
                      )
                    }
                  )


                  network_table <- build_network_table(
                    network_metrics
                  )

                  network_derived <- build_network_derived_table(
                    network_metrics
                  )


                  final_x <- update_result(
                    list(
                      network = network_metrics,
                      network_table = network_table,
                      network_derived = network_derived,
                      strahler = network_metrics$strahler
                    )
                  )


                  stages$network <- "ready"


                  final_primary <- if (!is.null(
                    final_x$tc
                  )) {
                    build_primary_table(
                      final_x$geom,
                      final_x$relief,
                      final_x$hydro,
                      final_x$network,
                      final_x$tc
                    )
                  } else {
                    NULL
                  }


                  if (!is.null(
                    final_primary
                  )) {
                    update_result(
                      list(
                        primary = final_primary
                      )
                    )
                  }


                  if (
                    !is.null(
                      final_x$out_dir
                    ) &&
                    dir.exists(
                      final_x$out_dir
                    )
                  ) {
                    write.csv(
                      network_table,
                      file.path(
                        final_x$out_dir,
                        "morfometria_red.csv"
                      ),
                      row.names = FALSE,
                      fileEncoding = "UTF-8"
                    )

                    write.csv(
                      network_derived,
                      file.path(
                        final_x$out_dir,
                        "morfometria_red_indices_derivados.csv"
                      ),
                      row.names = FALSE,
                      fileEncoding = "UTF-8"
                    )

                    write.csv(
                      network_metrics$strahler,
                      file.path(
                        final_x$out_dir,
                        "morfometria_strahler.csv"
                      ),
                      row.names = FALSE,
                      fileEncoding = "UTF-8"
                    )

                    if (!is.null(
                      final_primary
                    )) {
                      write.csv(
                        final_primary,
                        file.path(
                          final_x$out_dir,
                          "morfometria_principal.csv"
                        ),
                        row.names = FALSE,
                        fileEncoding = "UTF-8"
                      )
                    }
                  }


                  tc_text <- if (!is.null(
                    final_x$tc
                  )) {
                    paste0(
                      " | Tc ",
                      fmt(
                        final_x$tc$range_min_h,
                        2
                      ),
                      "-",
                      fmt(
                        final_x$tc$range_max_h,
                        2
                      ),
                      " h"
                    )
                  } else {
                    ""
                  }


                  status(
                    paste0(
                      "Listo | Lh ",
                      fmt(
                        final_x$hydro$hydraulic$length_km,
                        2
                      ),
                      " km | Lc ",
                      fmt(
                        final_x$hydro$channel$length_km,
                        2
                      ),
                      " km | Dd ",
                      fmt(
                        network_metrics$drainage_density_km_km2,
                        2
                      ),
                      " km/km2 | Strahler ",
                      network_metrics$max_order,
                      tc_text
                    )
                  )

                },
                error = function(e) {

                  stages$network <- "error"
                  stage_errors$network <- conditionMessage(
                    e
                  )

                  error_message <- paste0(
                    "Red/Strahler no pudo completarse: ",
                    conditionMessage(
                      e
                    ),
                    ". Los resultados anteriores se conservan."
                  )

                  status(
                    error_message
                  )

                  shiny::showNotification(
                    error_message,
                    type = "warning",
                    duration = NULL
                  )
                }
              )

              return()
            }
          },
          ignoreNULL = TRUE
        )


        shiny::observeEvent(
          input$limpiar,
          {
            run_id(
              shiny::isolate(
                run_id()
              ) +
                1L
            )

            result(
              NULL
            )

            reset_pipeline()

            status(
              "Resultados limpiados."
            )
          }
        )


        output$estado <- shiny::renderText({
          status()
        })



        output$source_note <- shiny::renderUI({

          source_value <- basin_source()

          if (identical(
            source_value,
            "imported"
          )) {
            return(
              shiny::div(
                class = "morph-note",
                shiny::tags$strong(
                  "Cuenca importada. "
                ),
                "Se calculan únicamente métricas compatibles con la geometría suministrada y con FABDEM. No se infiere outlet, recorrido hidráulico D8, cauce principal, red de drenaje, Strahler ni tiempos de concentración."
              )
            )
          }

          shiny::div(
            class = "morph-note",
            "Calcula geometria, forma y relieve con FABDEM y reutiliza el contexto D8 ya cargado por Delimitacion. Incluye recorrido hidraulico, cauce principal, red de drenaje completa con ordenamiento Strahler y tres estimaciones empiricas de tiempo de concentracion. Las metricas de red dependen del umbral de extraccion de stream_stripes mostrado en los resultados."
          )
        })


        output$full_hydro_available <- shiny::renderText({

          source_value <- basin_source()

          if (identical(
            source_value,
            "imported"
          )) {
            "false"
          } else {
            "true"
          }
        })


        shiny::outputOptions(
          output,
          "full_hydro_available",
          suspendWhenHidden = FALSE
        )


        output$pipeline_status <- shiny::renderUI({

          x <- result()


          state_label <- function(state) {
            switch(
              state,
              ready = "Listo",
              calculating = "Calculando...",
              error = "Error",
              "En espera..."
            )
          }


          stage_box <- function(
              title,
              state,
              detail = NULL
          ) {

            shiny::div(
              class = paste0(
                "morph-stage morph-stage-",
                state
              ),
              shiny::tags$strong(
                title
              ),
              shiny::div(
                state_label(
                  state
                )
              ),
              if (!is.null(
                detail
              )) {
                shiny::div(
                  style = "margin-top:3px;color:#555;",
                  detail
                )
              }
            )
          }


          geometry_detail <- if (
            identical(
              stages$geometry,
              "ready"
            ) &&
            !is.null(
              x$geom
            )
          ) {
            if (identical(
              x$basin_source,
              "imported"
            )) {
              paste0(
                "A ",
                fmt(
                  x$geom$area_km2,
                  1
                ),
                " km2 | P ",
                fmt(
                  x$geom$perimeter_km,
                  1
                ),
                " km"
              )
            } else {
              paste0(
                "A ",
                fmt(
                  x$geom$area_km2,
                  1
                ),
                " km2 | P ",
                fmt(
                  x$geom$perimeter_km,
                  1
                ),
                " km | Lb ",
                fmt(
                  x$geom$basin_length_km,
                  1
                ),
                " km"
              )
            }
          } else {
            NULL
          }


          relief_detail <- if (
            identical(
              stages$relief,
              "ready"
            ) &&
            !is.null(
              x$relief
            )
          ) {
            paste0(
              "Zmed ",
              fmt(
                x$relief$zmean,
                0
              ),
              " m | S ",
              fmt(
                x$relief$slope_mean_pct,
                1
              ),
              "%"
            )
          } else {
            NULL
          }


          hydro_detail <- if (
            identical(
              stages$hydro,
              "ready"
            ) &&
            !is.null(
              x$hydro
            )
          ) {
            paste0(
              "Lh ",
              fmt(
                x$hydro$hydraulic$length_km,
                1
              ),
              " km | Lc ",
              fmt(
                x$hydro$channel$length_km,
                1
              ),
              " km"
            )
          } else {
            NULL
          }


          tc_detail <- if (
            identical(
              stages$tc,
              "ready"
            ) &&
            !is.null(
              x$tc
            )
          ) {
            paste0(
              fmt(
                x$tc$range_min_h,
                2
              ),
              "-",
              fmt(
                x$tc$range_max_h,
                2
              ),
              " h"
            )
          } else {
            NULL
          }


          network_detail <- if (
            identical(
              stages$network,
              "ready"
            ) &&
            !is.null(
              x$network
            )
          ) {
            paste0(
              "Dd ",
              fmt(
                x$network$drainage_density_km_km2,
                2
              ),
              " | Ω ",
              x$network$max_order
            )
          } else {
            NULL
          }


          if (identical(
            x$basin_source,
            "imported"
          )) {
            return(
              shiny::div(
                class = "morph-stagebar",
                style = "grid-template-columns:repeat(2,1fr);",
                stage_box(
                  "Geometría",
                  stages$geometry,
                  geometry_detail
                ),
                stage_box(
                  "Relieve / FABDEM",
                  stages$relief,
                  relief_detail
                )
              )
            )
          }

          shiny::div(
            class = "morph-stagebar",
            stage_box(
              "Geometría",
              stages$geometry,
              geometry_detail
            ),
            stage_box(
              "Relieve / FABDEM",
              stages$relief,
              relief_detail
            ),
            stage_box(
              "Recorrido / cauce",
              stages$hydro,
              hydro_detail
            ),
            stage_box(
              "Tc",
              stages$tc,
              tc_detail
            ),
            stage_box(
              "Red / Strahler",
              stages$network,
              network_detail
            )
          )
        })


        output$has_results <- shiny::renderText({
          if (
            is.null(
              result()
            )
          ) {
            ""
          } else {
            "true"
          }
        })


        shiny::outputOptions(
          output,
          "has_results",
          suspendWhenHidden = FALSE
        )


        output$tabla_principal <- DT::renderDT({

          x <- result()


          shiny::req(
            x
          )


          morph_datatable(
            build_primary_progress_table(
              x,
              snapshot_stages()
            ),
            "parametros_principales"
          )
        })


        output$tabla_avanzada <- DT::renderDT({

          x <- result()


          shiny::req(
            x
          )


          if (is.null(
            x$advanced
          )) {
            return(
              waiting_table(
                if (identical(
                  stages$relief,
                  "calculating"
                )) {
                  "Calculando relieve y parámetros avanzados..."
                } else {
                  "En espera del relieve FABDEM..."
                }
              )
            )
          }


          morph_datatable(
            display_table(
              x$advanced
            ),
            "geometria_relieve_avanzados"
          )
        })


        output$tabla_recorrido_cauce <- DT::renderDT({

          x <- result()


          shiny::req(
            x
          )


          if (is.null(
            x$flow_channel
          )) {
            return(
              waiting_table(
                if (identical(
                  stages$hydro,
                  "calculating"
                )) {
                  "Calculando recorrido hidráulico y cauce principal..."
                } else {
                  "En espera del recorrido hidráulico..."
                }
              )
            )
          }


          morph_datatable(
            display_table(
              x$flow_channel
            ),
            "recorrido_hidraulico_cauce_principal"
          )
        })


        output$tabla_red <- DT::renderDT({

          x <- result()


          shiny::req(
            x
          )


          if (is.null(
            x$network_table
          )) {
            return(
              waiting_table(
                if (identical(
                  stages$network,
                  "calculating"
                )) {
                  "Calculando red de drenaje y Strahler..."
                } else {
                  "En espera del análisis de red..."
                }
              )
            )
          }


          morph_datatable(
            display_table(
              x$network_table
            ),
            "red_drenaje_strahler"
          )
        })


        output$tabla_red_derivada <- DT::renderDT({

          x <- result()


          shiny::req(
            x
          )


          if (is.null(
            x$network_derived
          )) {
            return(
              waiting_table(
                if (identical(
                  stages$network,
                  "calculating"
                )) {
                  "Calculando índices derivados de red..."
                } else {
                  "En espera del análisis de red..."
                }
              )
            )
          }


          morph_datatable(
            display_table(
              x$network_derived
            ),
            "indices_derivados_red"
          )
        })


        output$tabla_tc <- DT::renderDT({

          x <- result()


          shiny::req(
            x
          )


          if (is.null(
            x$tc_table
          )) {
            return(
              waiting_table(
                if (identical(
                  stages$tc,
                  "calculating"
                )) {
                  "Calculando tiempos de concentración..."
                } else {
                  "En espera del recorrido hidráulico..."
                }
              )
            )
          }


          morph_datatable(
            display_table(
              x$tc_table
            ),
            "tiempos_concentracion"
          )
        })


        output$tabla_strahler <- DT::renderDT({

          x <- result()


          shiny::req(
            x
          )


          out <- x$strahler


          if (is.null(
            out
          )) {
            return(
              waiting_table(
                if (identical(
                  stages$network,
                  "calculating"
                )) {
                  "Calculando ordenamiento Strahler..."
                } else {
                  "En espera del análisis de red..."
                }
              )
            )
          }


          if (nrow(
            out
          ) == 0L) {
            return(
              out
            )
          }


          out$LONGITUD_TOTAL_KM <- vapply(
            out$LONGITUD_TOTAL_KM,
            fmt,
            character(1),
            digits = 2
          )


          out$LONGITUD_MEDIA_KM <- vapply(
            out$LONGITUD_MEDIA_KM,
            fmt,
            character(1),
            digits = 3
          )


          out$RB <- vapply(
            out$RB,
            fmt,
            character(1),
            digits = 2
          )


          morph_datatable(
            out,
            "resumen_orden_strahler"
          )
        })

        draw_mapa_fisiografico <- function(x = NULL) {

          if (is.null(
            x
          )) {
            x <- result()
          }


          shiny::req(
            x
          )


          if (is.null(
            x$dem
          )) {
            draw_stage_message(
              if (identical(
                stages$relief,
                "calculating"
              )) {
                "Calculando relieve y preparando mapa..."
              } else {
                "Mapa disponible después del cálculo de relieve."
              }
            )
            return()
          }


          dem <- x$dem
          basin_sf <- x$basin_sf
          outlet_sf <- x$outlet_sf
          hp <- if (!is.null(
            x$hydro
          )) x$hydro$hydraulic_profile else NULL
          cp <- if (!is.null(
            x$hydro
          )) x$hydro$channel_profile else NULL
          network_metrics <- x$network
          hydro_context_map <- x$hydro_context


          shiny::req(
            !is.null(basin_sf)
          )


          old_par <- graphics::par(
            no.readonly = TRUE
          )


          on.exit(
            graphics::par(
              old_par
            ),
            add = TRUE
          )


          dem_crs <- sf::st_crs(
            terra::crs(
              dem
            )
          )


          basin_plot <- tryCatch(
            sf::st_transform(
              basin_sf,
              dem_crs
            ),
            error = function(e) basin_sf
          )


          outlet_plot <- if (!is.null(
            outlet_sf
          )) {
            tryCatch(
              sf::st_transform(
                outlet_sf,
                dem_crs
              ),
              error = function(e) outlet_sf
            )
          } else {
            NULL
          }


          # En la lámina el DEM se vuelve a enmascarar por seguridad.
          # Fuera de la divisoria solo debe verse el fondo gris local.
          dem_mask <- tryCatch(
            terra::mask(
              dem,
              terra::vect(
                basin_plot
              )
            ),
            error = function(e) dem
          )


          bb_basin <- sf::st_bbox(
            basin_plot
          )

          xmn <- as.numeric(
            bb_basin["xmin"]
          )
          xmx <- as.numeric(
            bb_basin["xmax"]
          )
          ymn <- as.numeric(
            bb_basin["ymin"]
          )
          ymx <- as.numeric(
            bb_basin["ymax"]
          )

          dx <- xmx - xmn
          dy <- ymx - ymn

          x_pad <- if (is.finite(dx) && dx > 0) 0.07 * dx else 0
          y_pad <- if (is.finite(dy) && dy > 0) 0.06 * dy else 0

          map_xlim <- c(
            xmn - x_pad,
            xmx + x_pad
          )

          map_ylim <- c(
            ymn - y_pad,
            ymx + y_pad
          )

          ext_plot <- terra::ext(
            map_xlim[1],
            map_xlim[2],
            map_ylim[1],
            map_ylim[2]
          )

          build_pretty_breaks <- function(
              zmin,
              zmax,
              target_n = 8L
          ) {

            raw_breaks <- pretty(
              c(
                zmin,
                zmax
              ),
              n = target_n
            )

            raw_breaks <- raw_breaks[
              raw_breaks >= zmin &
              raw_breaks <= zmax
            ]

            breaks <- sort(
              unique(
                c(
                  zmin,
                  raw_breaks,
                  zmax
                )
              )
            )

            if (length(
              breaks
            ) < 5L) {
              breaks <- seq(
                zmin,
                zmax,
                length.out = 9L
              )
            }

            breaks[length(breaks)] <- breaks[length(breaks)] + max(
              1e-7,
              abs(
                breaks[length(breaks)]
              ) * 1e-12
            )

            breaks
          }

          build_line_coords <- function(profile_df) {

            if (
              is.null(
                profile_df
              ) ||
              nrow(
                profile_df
              ) < 2L
            ) {
              return(NULL)
            }

            line_wgs84 <- sf::st_sfc(
              sf::st_linestring(
                as.matrix(
                  profile_df[, c(
                    "LON",
                    "LAT"
                  )]
                )
              ),
              crs = 4326
            )

            line_local <- sf::st_transform(
              line_wgs84,
              dem_crs
            )

            sf::st_coordinates(
              line_local
            )[, c(
              "X",
              "Y"
            ), drop = FALSE]
          }

          transform_point_df <- function(df_xy, from_crs, to_crs) {

            if (
              is.null(df_xy) ||
              nrow(df_xy) == 0L
            ) {
              return(NULL)
            }

            pts <- sf::st_as_sf(
              data.frame(
                X = df_xy[, 1],
                Y = df_xy[, 2]
              ),
              coords = c(
                "X",
                "Y"
              ),
              crs = from_crs
            )

            pts <- sf::st_transform(
              pts,
              to_crs
            )

            sf::st_coordinates(
              pts
            )[, c(
              "X",
              "Y"
            ), drop = FALSE]
          }


          context_dem_for_map <- function(
              xlim,
              ylim
          ) {

            tile_index <- get_dem_tile_index()

            ring <- matrix(
              c(
                xlim[1], ylim[1],
                xlim[2], ylim[1],
                xlim[2], ylim[2],
                xlim[1], ylim[2],
                xlim[1], ylim[1]
              ),
              ncol = 2,
              byrow = TRUE
            )

            map_poly <- sf::st_sfc(
              sf::st_polygon(
                list(
                  ring
                )
              ),
              crs = dem_crs
            )

            map_poly_4326 <- sf::st_transform(
              map_poly,
              4326
            )

            hits <- lengths(
              sf::st_intersects(
                tile_index,
                map_poly_4326
              )
            ) > 0L

            selected_context <- tile_index[
              hits,
              ,
              drop = FALSE
            ]

            if (nrow(
              selected_context
            ) == 0L) {
              return(NULL)
            }

            context_paths <- vapply(
              file.path(
                DEM_DIR,
                as.character(selected_context$RELATIVE_PATH)
              ),
              runtime_cache_file,
              character(1)
            )

            context_paths <- context_paths[
              file.exists(
                context_paths
              )
            ]

            if (length(
              context_paths
            ) == 0L) {
              return(NULL)
            }

            if (length(
              context_paths
            ) == 1L) {
              context_dem <- terra::rast(
                context_paths
              )
            } else {
              context_vrt <- file.path(
                TERRA_TEMP,
                paste0(
                  "morph_context_",
                  as.integer(
                    Sys.time()
                  ),
                  "_",
                  sample.int(
                    1e6,
                    1L
                  ),
                  ".vrt"
                )
              )

              context_dem <- terra::vrt(
                context_paths,
                filename = context_vrt,
                overwrite = TRUE
              )
            }

            terra::crop(
              context_dem,
              terra::ext(
                xlim[1],
                xlim[2],
                ylim[1],
                ylim[2]
              ),
              snap = "out"
            )
          }

          multidirectional_hillshade <- function(
              dem_context,
              xlim,
              ylim
          ) {

            if (is.null(
              dem_context
            )) {
              return(NULL)
            }

            cache_key <- paste(
              sprintf(
                "%.5f",
                c(
                  xlim,
                  ylim
                )
              ),
              collapse = "_"
            )

            cache_key <- gsub(
              "[^0-9A-Za-z_.-]",
              "_",
              cache_key
            )

            hill_cache_file <- file.path(
              TERRA_TEMP,
              paste0(
                "morph_hill_context_",
                cache_key,
                ".tif"
              )
            )

            if (file.exists(
              hill_cache_file
            )) {
              return(
                terra::rast(
                  hill_cache_file
                )
              )
            }

            slope_context <- terra::terrain(
              dem_context,
              v = "slope",
              unit = "radians",
              neighbors = 8
            )

            aspect_context <- terra::terrain(
              dem_context,
              v = "aspect",
              unit = "radians",
              neighbors = 8
            )

            directions <- c(
              315,
              45,
              135,
              225
            )

            hill_parts <- lapply(
              directions,
              function(direction_i) {
                terra::shade(
                  slope = slope_context,
                  aspect = aspect_context,
                  angle = 40,
                  direction = direction_i,
                  normalize = TRUE
                )
              }
            )

            hill_context <- (
              hill_parts[[1]] +
              hill_parts[[2]] +
              hill_parts[[3]] +
              hill_parts[[4]]
            ) / 4

            try(
              terra::writeRaster(
                hill_context,
                hill_cache_file,
                overwrite = TRUE,
                datatype = "FLT4S",
                gdal = c(
                  "COMPRESS=DEFLATE",
                  "TILED=YES"
                )
              ),
              silent = TRUE
            )

            hill_context
          }


          # ==================================================
          # HOJA A3 APAISADA
          # ==================================================

          graphics::par(
            oma = c(
              0,
              0,
              0,
              0
            ),
            bg = "white"
          )

          graphics::plot.new()
          graphics::rect(
            0,
            0,
            1,
            1,
            col = "white",
            border = NA
          )


          # --------------------------------------------------
          # PANEL PRINCIPAL DEL MAPA
          # --------------------------------------------------

          graphics::par(
            fig = c(
              0.025,
              0.79,
              0.07,
              0.97
            ),
            mar = c(
              3.3,
              3.5,
              1.0,
              1.0
            ),
            new = TRUE,
            xpd = FALSE
          )

          graphics::plot.new()

          # --------------------------------------------------
          # PROPORCION CARTOGRAFICA REAL
          # --------------------------------------------------
          # plot.window() sin asp ajusta X e Y de forma independiente
          # al rectangulo disponible y puede deformar visualmente una
          # cuenca. Para coordenadas geograficas se usa la correccion
          # clasica por latitud media; para CRS proyectados, asp = 1.
          # R expande simetricamente el eje que haga falta, nunca
          # estira ni comprime la geometria.

          map_crs <- sf::st_crs(
            basin_plot
          )

          map_asp <- 1

          if (
            !is.na(
              map_crs
            ) &&
            isTRUE(
              sf::st_is_longlat(
                map_crs
              )
            )
          ) {

            mid_lat <- mean(
              map_ylim
            )

            cos_lat <- cos(
              mid_lat * pi / 180
            )

            if (
              is.finite(
                cos_lat
              ) &&
              cos_lat > 0.05
            ) {
              map_asp <- 1 / cos_lat
            }
          }

          graphics::plot.window(
            xlim = map_xlim,
            ylim = map_ylim,
            xaxs = "i",
            yaxs = "i",
            asp = map_asp
          )

          # plot.window() puede haber ampliado simetricamente X o Y
          # para respetar la proporcion espacial. A partir de aqui todo
          # el mapa usa exactamente esos limites reales.
          map_usr_fixed <- graphics::par(
            "usr"
          )

          map_xlim <- map_usr_fixed[1:2]
          map_ylim <- map_usr_fixed[3:4]

          ext_plot <- terra::ext(
            map_xlim[1],
            map_xlim[2],
            map_ylim[1],
            map_ylim[2]
          )

          # Límites reales de la GRILLA dentro del dispositivo A3.
          # par("fig") describe el panel completo y par("plt") la región
          # útil después de aplicar mar. Las leyendas deben alinearse con
          # esta última, no con fig, para coincidir exactamente con el marco.
          map_fig_now <- graphics::par("fig")
          map_plt_now <- graphics::par("plt")

          map_plot_left <- map_fig_now[1] +
            (map_fig_now[2] - map_fig_now[1]) * map_plt_now[1]
          map_plot_right <- map_fig_now[1] +
            (map_fig_now[2] - map_fig_now[1]) * map_plt_now[2]
          map_plot_bottom <- map_fig_now[3] +
            (map_fig_now[4] - map_fig_now[3]) * map_plt_now[3]
          map_plot_top <- map_fig_now[3] +
            (map_fig_now[4] - map_fig_now[3]) * map_plt_now[4]
          map_plot_height <- map_plot_top - map_plot_bottom

          graphics::rect(
            xleft = map_xlim[1],
            ybottom = map_ylim[1],
            xright = map_xlim[2],
            ytop = map_ylim[2],
            col = "white",
            border = NA
          )


          # --------------------------------------------------
          # Hillshade y elevación.
          # --------------------------------------------------

          slope_rad <- terra::terrain(
            dem_mask,
            v = "slope",
            unit = "radians",
            neighbors = 8
          )

          aspect_rad <- terra::terrain(
            dem_mask,
            v = "aspect",
            unit = "radians",
            neighbors = 8
          )

          hill <- terra::shade(
            slope = slope_rad,
            aspect = aspect_rad,
            angle = 42,
            direction = 315,
            normalize = TRUE
          )

          zmin <- suppressWarnings(
            as.numeric(
              terra::global(
                dem_mask,
                "min",
                na.rm = TRUE
              )[1, 1]
            )
          )

          zmax <- suppressWarnings(
            as.numeric(
              terra::global(
                dem_mask,
                "max",
                na.rm = TRUE
              )[1, 1]
            )
          )

          if (
            !is.finite(zmin) ||
            !is.finite(zmax) ||
            zmax <= zmin
          ) {
            stop(
              "No se pudo construir la clasificación altitudinal del mapa."
            )
          }

          elev_breaks <- build_pretty_breaks(
            zmin = zmin,
            zmax = zmax,
            target_n = 8L
          )

          n_elev_classes <- length(
            elev_breaks
          ) - 1L

          elev_rcl <- cbind(
            elev_breaks[
              seq_len(
                n_elev_classes
              )
            ],
            elev_breaks[
              seq_len(
                n_elev_classes
              ) + 1L
            ],
            seq_len(
              n_elev_classes
            )
          )

          elev_class <- terra::classify(
            dem_mask,
            rcl = elev_rcl,
            include.lowest = TRUE,
            right = FALSE
          )

          elev_cols <- grDevices::colorRampPalette(
            c(
              "#2E7D32",
              "#88AF55",
              "#D8DD8B",
              "#F0D99A",
              "#D49A82",
              "#D8B5C9"
            )
          )(
            n_elev_classes
          )

          # --------------------------------------------------
          # Hillshade contextual LOCAL, sin servicios externos.
          # Se carga FABDEM para todo el rectángulo del mapa, no solo
          # para la cuenca. Esto garantiza textura de relieve también
          # fuera de la divisoria y evita fallbacks silenciosos de red.
          # --------------------------------------------------

          dem_context <- tryCatch(
            context_dem_for_map(
              xlim = map_xlim,
              ylim = map_ylim
            ),
            error = function(e) NULL
          )

          hill_context <- tryCatch(
            multidirectional_hillshade(
              dem_context = dem_context,
              xlim = map_xlim,
              ylim = map_ylim
            ),
            error = function(e) NULL
          )

          if (!is.null(
            hill_context
          )) {
            terra::plot(
              hill_context,
              col = grDevices::adjustcolor(
                grDevices::gray.colors(
                  160,
                  start = 1.00,
                  end = 0.60
                ),
                alpha.f = 1.00
              ),
              ext = ext_plot,
              legend = FALSE,
              axes = FALSE,
              box = FALSE,
              add = TRUE
            )
          }

          # Hipsometría dentro de la cuenca. Es casi opaca para separar
          # claramente el interior del contexto topográfico exterior.
          terra::plot(
            elev_class,
            col = grDevices::adjustcolor(
              elev_cols,
              alpha.f = 0.94
            ),
            ext = ext_plot,
            legend = FALSE,
            axes = FALSE,
            box = FALSE,
            add = TRUE
          )

          # Hillshade FABDEM de la propia cuenca por encima de la
          # hipsometría para conservar cerros, crestas y quebradas.
          terra::plot(
            hill,
            col = grDevices::adjustcolor(
              grDevices::gray.colors(
                140,
                start = 0.99,
                end = 0.38
              ),
              alpha.f = 0.16
            ),
            ext = ext_plot,
            legend = FALSE,
            axes = FALSE,
            box = FALSE,
            add = TRUE
          )


          # --------------------------------------------------
          # Grilla y marco.
          # --------------------------------------------------

          xticks <- pretty(
            map_xlim,
            n = 4
          )
          xticks <- xticks[
            xticks >= map_xlim[1] &
            xticks <= map_xlim[2]
          ]

          yticks <- pretty(
            map_ylim,
            n = 6
          )
          yticks <- yticks[
            yticks >= map_ylim[1] &
            yticks <= map_ylim[2]
          ]

          graphics::abline(
            v = xticks,
            h = yticks,
            col = grDevices::adjustcolor(
              "grey35",
              alpha.f = 0.12
            ),
            lty = 3,
            lwd = 0.55
          )

          graphics::axis(
            1,
            at = xticks,
            labels = format(
              round(
                xticks,
                2
              ),
              nsmall = 2
            ),
            cex.axis = 0.72,
            mgp = c(
              2,
              0.55,
              0
            )
          )

          graphics::axis(
            2,
            at = yticks,
            labels = format(
              round(
                yticks,
                2
              ),
              nsmall = 2
            ),
            las = 2,
            cex.axis = 0.72,
            mgp = c(
              2,
              0.55,
              0
            )
          )

          graphics::axis(
            3,
            at = xticks,
            labels = FALSE,
            tck = -0.008
          )

          graphics::axis(
            4,
            at = yticks,
            labels = FALSE,
            tck = -0.008
          )

          graphics::box(
            lwd = 0.8,
            col = "#424242"
          )


          # --------------------------------------------------
          # Límite de cuenca.
          # --------------------------------------------------

          terra::plot(
            terra::vect(
              basin_plot
            ),
            add = TRUE,
            border = "#111111",
            lwd = 1.9
          )


          # --------------------------------------------------
          # Red completa por orden Strahler.
          # --------------------------------------------------
          # La jerarquia se codifica simultaneamente mediante:
          #   - color
          #   - grosor
          #   - tipo de linea en los ordenes menores
          #
          # El cauce principal NO sustituye el orden Strahler.
          # Se representa como un contorno oscuro por debajo de
          # la red, de modo que el color/patron interior conserva
          # el orden real de cada tramo del cauce principal.

          strahler_style <- function(order_value) {

            order_value <- suppressWarnings(
              as.integer(order_value)
            )

            if (
              !is.finite(order_value) ||
              order_value < 1L
            ) {
              order_value <- 1L
            }

            # Los ordenes > 8 reutilizan el estilo del orden 8.
            idx <- min(
              order_value,
              8L
            )

            cols <- c(
              "#8DD3E8",  # 1: cian claro
              "#4DB6AC",  # 2: turquesa
              "#2A9D8F",  # 3: verde azulado
              "#3182BD",  # 4: azul
              "#3561A7",  # 5: azul profundo
              "#4F46A5",  # 6: indigo
              "#6A51A3",  # 7: violeta
              "#54278F"   # 8+: violeta oscuro
            )

            # Los patrones solo se usan en los ordenes bajos,
            # donde el grosor por si solo resulta poco distinguible.
            # 3 = punteado, 2 = dashed, 4 = dot-dash.
            ltys <- c(
              3,
              2,
              4,
              1,
              1,
              1,
              1,
              1
            )

            lwds <- c(
              0.72,
              0.98,
              1.22,
              1.48,
              1.78,
              2.08,
              2.34,
              2.58
            )

            list(
              col = cols[idx],
              lty = ltys[idx],
              lwd = lwds[idx]
            )
          }

          # Convierte los edges rasterizados de un mismo orden en
          # polilineas continuas. Esto es importante porque graphics::segments()
          # reinicia el patron de linea en cada celda y haria que los estilos
          # dashed/punteado fueran inconsistentes o pareciesen solidos.
          build_strahler_chains <- function(
              parent_cells,
              receiver_cells,
              pxy,
              rxy,
              edge_order
          ) {

            out <- list()

            valid_orders <- sort(
              unique(
                as.integer(
                  edge_order[
                    is.finite(
                      edge_order
                    ) &
                    edge_order >= 1
                  ]
                )
              )
            )

            for (u in valid_orders) {

              edge_ids <- which(
                edge_order == u
              )

              if (length(
                edge_ids
              ) == 0L) {
                next
              }

              parents_u <- parent_cells[
                edge_ids
              ]

              receivers_u <- receiver_cells[
                edge_ids
              ]

              # Cada celda tiene como maximo un receptor aguas abajo.
              # Un inicio de cadena es un edge cuyo parent no es receptor
              # de otro edge del mismo orden.
              start_local <- which(
                !parents_u %in%
                  receivers_u
              )

              visited <- rep(
                FALSE,
                length(
                  edge_ids
                )
              )

              trace_from_local <- function(
                  local_start
              ) {

                chain_xy <- matrix(
                  numeric(0),
                  ncol = 2
                )

                current_local <- local_start
                safety <- 0L

                repeat {

                  if (
                    current_local < 1L ||
                    current_local > length(edge_ids) ||
                    visited[current_local]
                  ) {
                    break
                  }

                  visited[current_local] <<- TRUE

                  edge_i <- edge_ids[
                    current_local
                  ]

                  if (nrow(
                    chain_xy
                  ) == 0L) {
                    chain_xy <- rbind(
                      pxy[
                        edge_i,
                        1:2,
                        drop = FALSE
                      ],
                      rxy[
                        edge_i,
                        1:2,
                        drop = FALSE
                      ]
                    )
                  } else {
                    chain_xy <- rbind(
                      chain_xy,
                      rxy[
                        edge_i,
                        1:2,
                        drop = FALSE
                      ]
                    )
                  }

                  safety <- safety + 1L

                  if (safety > length(
                    edge_ids
                  )) {
                    stop(
                      "Posible ciclo al construir polilineas Strahler para el mapa."
                    )
                  }

                  next_parent <- receiver_cells[
                    edge_i
                  ]

                  next_local <- match(
                    next_parent,
                    parents_u
                  )

                  if (
                    is.na(
                      next_local
                    ) ||
                    visited[next_local]
                  ) {
                    break
                  }

                  current_local <- next_local
                }

                chain_xy
              }

              if (length(
                start_local
              ) > 0L) {

                for (local_i in start_local) {

                  chain_xy <- trace_from_local(
                    local_i
                  )

                  if (nrow(
                    chain_xy
                  ) >= 2L) {
                    out[[
                      length(out) + 1L
                    ]] <- list(
                      order = u,
                      xy = chain_xy
                    )
                  }
                }
              }

              # Salvaguarda: cualquier edge no visitado se traza igualmente.
              remaining <- which(
                !visited
              )

              if (length(
                remaining
              ) > 0L) {

                for (local_i in remaining) {

                  if (visited[
                    local_i
                  ]) {
                    next
                  }

                  chain_xy <- trace_from_local(
                    local_i
                  )

                  if (nrow(
                    chain_xy
                  ) >= 2L) {
                    out[[
                      length(out) + 1L
                    ]] <- list(
                      order = u,
                      xy = chain_xy
                    )
                  }
                }
              }
            }

            out
          }

          hp_xy <- build_line_coords(
            hp
          )

          cp_xy <- build_line_coords(
            cp
          )

          network_segments_xy <- NULL
          network_plot_data <- NULL

          if (
            !is.null(
              network_metrics
            ) &&
            !is.null(
              hydro_context_map
            ) &&
            !is.null(
              network_metrics$network
            ) &&
            length(
              network_metrics$network$parent
            ) > 0L
          ) {

            net <- network_metrics$network
            grid_template <- hydro_context_map$grid_template

            pxy <- terra::xyFromCell(
              grid_template,
              net$parent
            )

            rxy <- terra::xyFromCell(
              grid_template,
              net$receiver
            )

            grid_crs <- sf::st_crs(
              terra::crs(
                grid_template
              )
            )

            same_crs <- !is.na(
              grid_crs
            ) &&
              !is.na(
                dem_crs
              ) &&
              identical(
                grid_crs$wkt,
                dem_crs$wkt
              )

            if (!isTRUE(
              same_crs
            )) {

              all_xy <- rbind(
                pxy,
                rxy
              )

              all_sf <- sf::st_as_sf(
                data.frame(
                  X = all_xy[, 1],
                  Y = all_xy[, 2]
                ),
                coords = c(
                  "X",
                  "Y"
                ),
                crs = grid_crs
              )

              all_sf <- sf::st_transform(
                all_sf,
                dem_crs
              )

              all_xy <- sf::st_coordinates(
                all_sf
              )

              nedge <- nrow(
                pxy
              )

              pxy <- all_xy[
                seq_len(
                  nedge
                ),
                c(
                  "X",
                  "Y"
                ),
                drop = FALSE
              ]

              rxy <- all_xy[
                nedge + seq_len(
                  nedge
                ),
                c(
                  "X",
                  "Y"
                ),
                drop = FALSE
              ]
            }

            network_segments_xy <- cbind(
              x0 = pxy[, 1],
              y0 = pxy[, 2],
              x1 = rxy[, 1],
              y1 = rxy[, 2]
            )

            parent_idx <- match(
              net$parent,
              net$cells
            )

            edge_order <- network_metrics$cell_order[
              parent_idx
            ]

            strahler_chains <- build_strahler_chains(
              parent_cells = net$parent,
              receiver_cells = net$receiver,
              pxy = pxy,
              rxy = rxy,
              edge_order = edge_order
            )

            network_plot_data <- list(
              pxy = pxy,
              rxy = rxy,
              edge_order = edge_order,
              chains = strahler_chains
            )

            max_order_map <- max(
              edge_order,
              na.rm = TRUE
            )

            # 1) Halo neutro de la red. Se dibuja continuo para
            # separar los cauces del hillshade y de la hipsometria.
            for (chain_i in strahler_chains) {

              st <- strahler_style(
                chain_i$order
              )

              xy_i <- chain_i$xy

              graphics::lines(
                xy_i[, 1],
                xy_i[, 2],
                col = grDevices::adjustcolor(
                  "white",
                  alpha.f = 0.88
                ),
                lwd = st$lwd + 0.72,
                lty = 1,
                lend = "round"
              )
            }
          }

          # 2) Contorno del cauce principal. No tapa Strahler:
          # la red coloreada se vuelve a dibujar encima.
          if (!is.null(
            cp_xy
          )) {
            graphics::lines(
              cp_xy[, 1],
              cp_xy[, 2],
              col = "#08306B",
              lwd = 3.30,
              lty = 1,
              lend = "round"
            )
          }

          # 3) Orden Strahler real sobre halo y contorno.
          if (!is.null(
            network_plot_data
          )) {

            edge_order <- network_plot_data$edge_order
            strahler_chains <- network_plot_data$chains

            max_order_map <- max(
              edge_order,
              na.rm = TRUE
            )

            for (chain_i in strahler_chains) {

              st <- strahler_style(
                chain_i$order
              )

              xy_i <- chain_i$xy

              graphics::lines(
                xy_i[, 1],
                xy_i[, 2],
                col = st$col,
                lwd = st$lwd,
                lty = st$lty,
                lend = "round"
              )
            }
          }


          # --------------------------------------------------
          # Recorrido hidráulico y puntos principales.
          # --------------------------------------------------

          if (!is.null(
            hp_xy
          )) {
            graphics::lines(
              hp_xy[, 1],
              hp_xy[, 2],
              col = "#37474F",
              lwd = 1.2,
              lty = 3
            )

            graphics::points(
              hp_xy[1, 1],
              hp_xy[1, 2],
              pch = 21,
              bg = "white",
              col = "#212121",
              cex = 1.30,
              lwd = 1.0
            )
          }

          if (!is.null(
            cp_xy
          )) {
            graphics::points(
              cp_xy[1, 1],
              cp_xy[1, 2],
              pch = 24,
              bg = "#08306B",
              col = "white",
              cex = 1.34,
              lwd = 0.8
            )
          }

          outlet_xy <- if (!is.null(
            outlet_plot
          )) {
            sf::st_coordinates(
              outlet_plot
            )
          } else {
            NULL
          }

          if (!is.null(
            outlet_xy
          )) {
            graphics::points(
              outlet_xy[1, 1],
              outlet_xy[1, 2],
              pch = 21,
              bg = "#D50000",
              col = "black",
              cex = 1.30
            )
          }


          # --------------------------------------------------
          # Escala y flecha norte con posición dinámica.
          # Se conservan el encuadre y el padding simétricos.
          # Los elementos cartográficos prueban posiciones candidatas
          # y eligen la de menor interferencia con la cuenca, los
          # recorridos, la red y los puntos principales.
          # --------------------------------------------------

          usr <- graphics::par(
            "usr"
          )
          ux <- usr[2] - usr[1]
          uy <- usr[4] - usr[3]

          point_in_box <- function(
              xy,
              box
          ) {

            if (
              is.null(xy) ||
              length(xy) == 0L
            ) {
              return(
                logical(0)
              )
            }

            xy <- as.matrix(
              xy
            )

            if (ncol(
              xy
            ) < 2L) {
              return(
                logical(0)
              )
            }

            xy[, 1] >= box[["xmin"]] &
              xy[, 1] <= box[["xmax"]] &
              xy[, 2] >= box[["ymin"]] &
              xy[, 2] <= box[["ymax"]]
          }

          boxes_overlap <- function(
              a,
              b
          ) {

            !(
              a[["xmax"]] <= b[["xmin"]] ||
              a[["xmin"]] >= b[["xmax"]] ||
              a[["ymax"]] <= b[["ymin"]] ||
              a[["ymin"]] >= b[["ymax"]]
            )
          }

          basin_overlap_fraction <- function(
              box,
              nx_sample = 7L,
              ny_sample = 5L
          ) {

            xx <- seq(
              box[["xmin"]] + 0.04 * (
                box[["xmax"]] - box[["xmin"]]
              ),
              box[["xmax"]] - 0.04 * (
                box[["xmax"]] - box[["xmin"]]
              ),
              length.out = nx_sample
            )

            yy <- seq(
              box[["ymin"]] + 0.04 * (
                box[["ymax"]] - box[["ymin"]]
              ),
              box[["ymax"]] - 0.04 * (
                box[["ymax"]] - box[["ymin"]]
              ),
              length.out = ny_sample
            )

            sample_xy <- as.matrix(
              expand.grid(
                X = xx,
                Y = yy
              )
            )

            pts <- sf::st_as_sf(
              data.frame(
                X = sample_xy[, 1],
                Y = sample_xy[, 2]
              ),
              coords = c(
                "X",
                "Y"
              ),
              crs = dem_crs
            )

            inside <- lengths(
              sf::st_intersects(
                pts,
                basin_plot
              )
            ) > 0L

            mean(
              inside
            )
          }

          line_hit_fraction <- function(
              xy,
              box,
              max_points = 2500L
          ) {

            if (
              is.null(xy) ||
              nrow(
                xy
              ) == 0L
            ) {
              return(0)
            }

            if (nrow(
              xy
            ) > max_points) {
              idx <- unique(
                round(
                  seq(
                    1,
                    nrow(
                      xy
                    ),
                    length.out = max_points
                  )
                )
              )
              xy <- xy[
                idx,
                ,
                drop = FALSE
              ]
            }

            hit <- point_in_box(
              xy,
              box
            )

            if (length(
              hit
            ) == 0L) {
              return(0)
            }

            mean(
              hit
            )
          }

          network_hit_fraction <- function(
              segments,
              box,
              max_segments = 3000L
          ) {

            if (
              is.null(segments) ||
              nrow(
                segments
              ) == 0L
            ) {
              return(0)
            }

            if (nrow(
              segments
            ) > max_segments) {
              idx <- unique(
                round(
                  seq(
                    1,
                    nrow(
                      segments
                    ),
                    length.out = max_segments
                  )
                )
              )
              segments <- segments[
                idx,
                ,
                drop = FALSE
              ]
            }

            probe <- rbind(
              cbind(
                x = segments[, "x0"],
                y = segments[, "y0"]
              ),
              cbind(
                x = segments[, "x1"],
                y = segments[, "y1"]
              ),
              cbind(
                x = (
                  segments[, "x0"] + segments[, "x1"]
                ) / 2,
                y = (
                  segments[, "y0"] + segments[, "y1"]
                ) / 2
              )
            )

            mean(
              point_in_box(
                probe,
                box
              )
            )
          }

          box_clearance_fraction <- function(
              box
          ) {

            ring <- matrix(
              c(
                box[["xmin"]], box[["ymin"]],
                box[["xmax"]], box[["ymin"]],
                box[["xmax"]], box[["ymax"]],
                box[["xmin"]], box[["ymax"]],
                box[["xmin"]], box[["ymin"]]
              ),
              ncol = 2,
              byrow = TRUE
            )

            box_sf <- sf::st_sfc(
              sf::st_polygon(
                list(
                  ring
                )
              ),
              crs = dem_crs
            )

            basin_geom <- sf::st_union(
              sf::st_geometry(
                basin_plot
              )
            )

            d <- suppressWarnings(
              as.numeric(
                sf::st_distance(
                  box_sf,
                  basin_geom
                )[1]
              )
            )

            map_diag <- sqrt(
              ux^2 + uy^2
            )

            if (
              !is.finite(d) ||
              !is.finite(map_diag) ||
              map_diag <= 0
            ) {
              return(0)
            }

            pmin(
              1,
              d / (
                0.12 * map_diag
              )
            )
          }

          score_map_box <- function(
              box,
              preference_penalty = 0,
              reserved_box = NULL
          ) {

            score <- as.numeric(
              preference_penalty
            )

            basin_fraction <- basin_overlap_fraction(
              box
            )

            score <- score +
              500 * basin_fraction

            # Si dos posiciones no tapan la cuenca, se favorece
            # la que deja mayor separación visual respecto a ella.
            clearance_fraction <- box_clearance_fraction(
              box
            )

            score <- score -
              120 * clearance_fraction

            cp_fraction <- line_hit_fraction(
              cp_xy,
              box
            )

            hp_fraction <- line_hit_fraction(
              hp_xy,
              box
            )

            score <- score +
              900 * cp_fraction +
              650 * hp_fraction

            network_fraction <- network_hit_fraction(
              network_segments_xy,
              box
            )

            score <- score +
              250 * network_fraction

            key_points <- list(
              outlet = if (!is.null(
                outlet_xy
              )) outlet_xy[1, 1:2, drop = FALSE] else NULL,
              channel_head = if (!is.null(
                cp_xy
              )) cp_xy[1, , drop = FALSE] else NULL,
              remote = if (!is.null(
                hp_xy
              )) hp_xy[1, , drop = FALSE] else NULL
            )

            for (p in key_points) {
              if (
                !is.null(p) &&
                any(
                  point_in_box(
                    p,
                    box
                  )
                )
              ) {
                score <- score + 2500
              }
            }

            if (
              !is.null(
                reserved_box
              ) &&
              boxes_overlap(
                box,
                reserved_box
              )
            ) {
              score <- score + 10000
            }

            score
          }

          choose_box <- function(
              candidates,
              preference,
              reserved_box = NULL
          ) {

            scores <- vapply(
              seq_along(
                candidates
              ),
              function(i) {
                score_map_box(
                  candidates[[i]],
                  preference_penalty = preference[i],
                  reserved_box = reserved_box
                )
              },
              numeric(1)
            )

            candidates[[
              which.min(
                scores
              )
            ]]
          }

          # --------------------------------------------------
          # Flecha norte: 4 esquinas.
          # Preferencia natural: superior izquierda.
          # --------------------------------------------------

          north_w <- 0.080 * ux
          north_h <- 0.105 * uy
          north_margin_x <- 0.018 * ux
          north_margin_y <- 0.018 * uy

          north_candidates <- list(
            TL = c(
              xmin = usr[1] + north_margin_x,
              xmax = usr[1] + north_margin_x + north_w,
              ymin = usr[4] - north_margin_y - north_h,
              ymax = usr[4] - north_margin_y
            ),
            TR = c(
              xmin = usr[2] - north_margin_x - north_w,
              xmax = usr[2] - north_margin_x,
              ymin = usr[4] - north_margin_y - north_h,
              ymax = usr[4] - north_margin_y
            ),
            BL = c(
              xmin = usr[1] + north_margin_x,
              xmax = usr[1] + north_margin_x + north_w,
              ymin = usr[3] + north_margin_y,
              ymax = usr[3] + north_margin_y + north_h
            ),
            BR = c(
              xmin = usr[2] - north_margin_x - north_w,
              xmax = usr[2] - north_margin_x,
              ymin = usr[3] + north_margin_y,
              ymax = usr[3] + north_margin_y + north_h
            )
          )

          north_box <- choose_box(
            candidates = north_candidates,
            preference = c(
              0,
              8,
              14,
              20
            )
          )

          graphics::rect(
            north_box[["xmin"]],
            north_box[["ymin"]],
            north_box[["xmax"]],
            north_box[["ymax"]],
            col = grDevices::adjustcolor(
              "#FFFDF7",
              alpha.f = 0.92
            ),
            border = "#6E6E6E",
            lwd = 0.9
          )

          nx <- mean(
            north_box[c(
              "xmin",
              "xmax"
            )]
          )
          north_box_h <- north_box[["ymax"]] - north_box[["ymin"]]
          north_box_w <- north_box[["xmax"]] - north_box[["xmin"]]

          graphics::text(
            nx,
            north_box[["ymax"]] - 0.16 * north_box_h,
            labels = "N",
            font = 2,
            cex = 1.00
          )

          arrow_tip_y <- north_box[["ymax"]] - 0.30 * north_box_h
          arrow_base_y <- north_box[["ymin"]] + 0.15 * north_box_h
          arrow_half_w <- 0.27 * north_box_w

          graphics::polygon(
            x = c(
              nx,
              nx - arrow_half_w,
              nx,
              nx + arrow_half_w
            ),
            y = c(
              arrow_tip_y,
              arrow_base_y,
              north_box[["ymin"]] + 0.34 * north_box_h,
              arrow_base_y
            ),
            col = "black",
            border = "black"
          )

          # --------------------------------------------------
          # Escala gráfica: 5 posiciones candidatas.
          # Se elige después del norte y evita su caja.
          # --------------------------------------------------

          nice_scale_length_m <- function(total_m) {
            if (!is.finite(total_m) || total_m <= 0) {
              return(1000)
            }
            target <- total_m / 2.5
            pow10 <- 10 ^ floor(
              log10(
                target
              )
            )
            candidates <- c(
              1,
              2,
              5
            ) * pow10
            candidates <- candidates[
              candidates <= target
            ]
            if (length(
              candidates
            ) == 0L) {
              return(pow10)
            }
            max(
              candidates
            )
          }

          if (terra::is.lonlat(
            dem
          )) {
            mid_lat <- mean(
              usr[3:4]
            )
            total_width_m <- haversine_segment_m(
              usr[1],
              mid_lat,
              usr[2],
              mid_lat
            )
            scale_m <- nice_scale_length_m(
              total_width_m
            )
            m_per_deg <- haversine_segment_m(
              usr[1],
              mid_lat,
              usr[1] + 1,
              mid_lat
            )
            scale_dx <- scale_m / m_per_deg
          } else {
            scale_m <- nice_scale_length_m(
              ux
            )
            scale_dx <- scale_m
          }

          n_scale_segments <- 4L
          seg_dx <- scale_dx / n_scale_segments
          bar_h <- 0.022 * uy

          scale_box_w <- scale_dx + 0.095 * ux
          scale_box_h <- bar_h + 0.060 * uy
          scale_margin_x <- 0.018 * ux
          scale_margin_y <- 0.018 * uy

          scale_candidates <- list(
            BL = c(
              xmin = usr[1] + scale_margin_x,
              xmax = usr[1] + scale_margin_x + scale_box_w,
              ymin = usr[3] + scale_margin_y,
              ymax = usr[3] + scale_margin_y + scale_box_h
            ),
            BC = c(
              xmin = mean(
                usr[1:2]
              ) - scale_box_w / 2,
              xmax = mean(
                usr[1:2]
              ) + scale_box_w / 2,
              ymin = usr[3] + scale_margin_y,
              ymax = usr[3] + scale_margin_y + scale_box_h
            ),
            BR = c(
              xmin = usr[2] - scale_margin_x - scale_box_w,
              xmax = usr[2] - scale_margin_x,
              ymin = usr[3] + scale_margin_y,
              ymax = usr[3] + scale_margin_y + scale_box_h
            ),
            TL = c(
              xmin = usr[1] + scale_margin_x,
              xmax = usr[1] + scale_margin_x + scale_box_w,
              ymin = usr[4] - scale_margin_y - scale_box_h,
              ymax = usr[4] - scale_margin_y
            ),
            TC = c(
              xmin = mean(
                usr[1:2]
              ) - scale_box_w / 2,
              xmax = mean(
                usr[1:2]
              ) + scale_box_w / 2,
              ymin = usr[4] - scale_margin_y - scale_box_h,
              ymax = usr[4] - scale_margin_y
            ),
            TR = c(
              xmin = usr[2] - scale_margin_x - scale_box_w,
              xmax = usr[2] - scale_margin_x,
              ymin = usr[4] - scale_margin_y - scale_box_h,
              ymax = usr[4] - scale_margin_y
            )
          )

          scale_box <- choose_box(
            candidates = scale_candidates,
            preference = c(
              0,
              3,
              2,
              3,
              4,
              1
            ),
            reserved_box = north_box
          )

          graphics::rect(
            scale_box[["xmin"]],
            scale_box[["ymin"]],
            scale_box[["xmax"]],
            scale_box[["ymax"]],
            col = grDevices::adjustcolor(
              "#FFFDF7",
              alpha.f = 0.92
            ),
            border = "#6E6E6E",
            lwd = 0.9
          )

          sx0 <- scale_box[["xmin"]] + 0.015 * ux
          sy0 <- scale_box[["ymin"]] + 0.040 * uy

          for (i in 0:(
            n_scale_segments - 1L
          )) {
            graphics::rect(
              sx0 + i * seg_dx,
              sy0,
              sx0 + (
                i + 1L
              ) * seg_dx,
              sy0 + bar_h,
              col = if (i %% 2L == 0L) "black" else "white",
              border = "black",
              lwd = 0.8
            )
          }

          for (i in 0:n_scale_segments) {
            graphics::text(
              sx0 + i * seg_dx,
              sy0 - 0.012 * uy,
              labels = format(
                round(
                  scale_m * i / n_scale_segments / 1000,
                  1
                ),
                trim = TRUE,
                scientific = FALSE
              ),
              cex = 0.95
            )
          }

          graphics::text(
            sx0 + scale_dx + 0.020 * ux,
            sy0 - 0.012 * uy,
            labels = "km",
            cex = 0.95,
            adj = c(
              0,
              0.5
            )
          )


          # --------------------------------------------------
          # PANEL DERECHO SUPERIOR: leyenda altitudinal.
          # --------------------------------------------------

          graphics::par(
            fig = c(
              0.81,
              0.985,
              map_plot_bottom + 0.57 * map_plot_height,
              map_plot_top
            ),
            mar = c(
              0,
              0,
              0,
              0
            ),
            new = TRUE,
            xpd = NA
          )

          graphics::plot.new()
          graphics::plot.window(
            xlim = c(
              0,
              1
            ),
            ylim = c(
              0,
              1
            ),
            xaxs = "i",
            yaxs = "i"
          )

          graphics::rect(
            0,
            0,
            1,
            1,
            col = "#FFFDF7",
            border = "#5F5F5F",
            lwd = 1.1
          )

          graphics::text(
            0.02,
            0.98,
            labels = "Elevación",
            adj = c(
              0,
              1
            ),
            font = 2,
            cex = 1.52
          )

          graphics::text(
            0.02,
            0.925,
            labels = "(m s.n.m.)",
            adj = c(
              0,
              1
            ),
            cex = 1.06
          )

          elev_labels <- paste0(
            format(
              round(
                elev_breaks[
                  seq_len(
                    n_elev_classes
                  )
                ],
                0
              ),
              trim = TRUE,
              scientific = FALSE
            ),
            " - ",
            format(
              round(
                elev_breaks[
                  seq_len(
                    n_elev_classes
                  ) + 1L
                ],
                0
              ),
              trim = TRUE,
              scientific = FALSE
            )
          )

          legend_y_top <- 0.86
          legend_step <- min(
            0.085,
            0.62 / n_elev_classes
          )
          box_x0 <- 0.03
          box_x1 <- 0.27

          for (j in seq_len(
            n_elev_classes
          )) {
            idx <- n_elev_classes + 1L - j
            y1 <- legend_y_top - (
              j - 1L
            ) * legend_step
            y0 <- y1 - 0.050

            graphics::rect(
              box_x0,
              y0,
              box_x1,
              y1,
              col = elev_cols[idx],
              border = NA
            )

            graphics::text(
              0.32,
              mean(c(
                y0,
                y1
              )),
              labels = elev_labels[idx],
              adj = c(
                0,
                0.5
              ),
              cex = 1.04
            )
          }


          # --------------------------------------------------
          # PANEL DERECHO INFERIOR: trazados y Strahler.
          # --------------------------------------------------

          if (!identical(
            x$basin_source,
            "imported"
          )) {

          graphics::par(
            fig = c(
              0.81,
              0.985,
              map_plot_bottom,
              map_plot_bottom + 0.52 * map_plot_height
            ),
            mar = c(
              0,
              0,
              0,
              0
            ),
            new = TRUE,
            xpd = NA
          )

          graphics::plot.new()
          graphics::plot.window(
            xlim = c(
              0,
              1
            ),
            ylim = c(
              0,
              1
            ),
            xaxs = "i",
            yaxs = "i"
          )

          graphics::rect(
            0,
            0,
            1,
            1,
            col = "#FFFDF7",
            border = "#5F5F5F",
            lwd = 1.1
          )

          graphics::text(
            0.02,
            0.97,
            labels = "Trazados",
            adj = c(
              0,
              1
            ),
            font = 2,
            cex = 1.30
          )

          # El cauce principal se identifica mediante un contorno.
          # El color/patron interior del mapa corresponde siempre al
          # orden Strahler real del tramo.
          graphics::segments(
            0.04,
            0.84,
            0.34,
            0.84,
            col = "#08306B",
            lwd = 3.30,
            lty = 1
          )
          graphics::segments(
            0.04,
            0.84,
            0.34,
            0.84,
            col = "white",
            lwd = 1.15,
            lty = 1
          )
          graphics::text(
            0.40,
            0.84,
            labels = "Cauce principal (contorno)",
            adj = c(
              0,
              0.5
            ),
            cex = 0.96
          )

          graphics::segments(
            0.04,
            0.74,
            0.34,
            0.74,
            col = "#37474F",
            lwd = 1.2,
            lty = 3
          )
          graphics::text(
            0.40,
            0.74,
            labels = "Recorrido Lh",
            adj = c(
              0,
              0.5
            ),
            cex = 1.02
          )

          graphics::points(
            0.07,
            0.61,
            pch = 21,
            bg = "#D50000",
            col = "black",
            cex = 1.30
          )
          graphics::text(
            0.15,
            0.61,
            labels = "Outlet",
            adj = c(
              0,
              0.5
            ),
            cex = 1.02
          )

          graphics::points(
            0.07,
            0.52,
            pch = 24,
            bg = "#0B57A4",
            col = "#08306B",
            cex = 1.30
          )
          graphics::text(
            0.15,
            0.52,
            labels = "Cabecera del cauce",
            adj = c(
              0,
              0.5
            ),
            cex = 1.02
          )

          graphics::points(
            0.07,
            0.43,
            pch = 21,
            bg = "white",
            col = "#212121",
            cex = 1.30,
            lwd = 1.0
          )
          graphics::text(
            0.15,
            0.43,
            labels = "Punto remoto",
            adj = c(
              0,
              0.5
            ),
            cex = 1.02
          )

          if (!is.null(
            network_metrics
          ) &&
          is.finite(
            network_metrics$max_order
          ) &&
          network_metrics$max_order >= 1L) {

            max_order_map <- network_metrics$max_order
            n_show <- min(
              max_order_map,
              8L
            )

            graphics::text(
              0.02,
              0.31,
              labels = "Orden Strahler",
              adj = c(
                0,
                1
              ),
              font = 2,
              cex = 1.12
            )

            yy_seq <- seq(
              0.22,
              0.05,
              length.out = n_show
            )

            for (u in seq_len(
              n_show
            )) {
              yy <- yy_seq[u]
              st <- strahler_style(
                u
              )

              # Halo de muestra, igual que en el mapa.
              graphics::segments(
                0.04,
                yy,
                0.34,
                yy,
                col = "white",
                lwd = st$lwd + 0.72,
                lty = 1
              )

              graphics::segments(
                0.04,
                yy,
                0.34,
                yy,
                col = st$col,
                lwd = st$lwd,
                lty = st$lty
              )

              label_order <- if (
                u == 8L &&
                max_order_map > 8L
              ) {
                "Orden 8+"
              } else {
                paste0(
                  "Orden ",
                  u
                )
              }

              graphics::text(
                0.40,
                yy,
                labels = label_order,
                adj = c(
                  0,
                  0.5
                ),
                cex = 1.02
              )
            }
          }


          # --------------------------------------------------
          # MARCO GENERAL DE LA LAMINA A3
          # Contiene mapa, leyendas y elementos cartograficos.
          # --------------------------------------------------

          graphics::par(
            fig = c(
              0,
              1,
              0,
              1
            ),
            mar = c(
              0,
              0,
              0,
              0
            ),
            new = TRUE,
            xpd = NA
          )

          graphics::plot.new()
          graphics::plot.window(
            xlim = c(
              0,
              1
            ),
            ylim = c(
              0,
              1
            ),
            xaxs = "i",
            yaxs = "i"
          )

          graphics::rect(
            0.018,
            0.018,
            0.992,
            0.985,
            col = NA,
            border = "#4A4A4A",
            lwd = 1.25
          )

          }

          # Fuente discreta al pie de la hoja.
          graphics::par(
            fig = c(
              0,
              1,
              0,
              1
            ),
            mar = c(
              0,
              0,
              0,
              0
            ),
            new = TRUE,
            xpd = NA
          )

          graphics::plot.new()
          graphics::text(
            0.975,
            0.028,
            labels = paste0(
              layer_source_map_label("fabdem_dem"),
              if (identical(
                x$basin_source,
                "imported"
              )) {
                " | Cuenca: geometría importada"
              } else {
                " | Red: derivada mediante flujo D8"
              }
            ),
            adj = c(
              1,
              0
            ),
            cex = 0.72,
            col = "grey30"
          )
        }


        output$mapa_fisiografico <- shiny::renderPlot({

          draw_mapa_fisiografico(
            x = result()
          )
        })


        output$descargar_dem_cuenca_utm <- shiny::downloadHandler(
          filename = function() {

            x_download <- shiny::isolate(
              result()
            )

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

            epsg_tag <- if (
              is.finite(
                epsg_value
              )
            ) {
              paste0(
                "EPSG",
                epsg_value,
                "_"
              )
            } else {
              ""
            }

            paste0(
              "FABDEM_CUENCA_UTM_",
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
              is.null(x_download$basin_sf) ||
              is.null(x_download$geom) ||
              is.null(x_download$geom$epsg)
            ) {
              stop(
                "No hay un DEM de cuenca listo para descargar."
              )
            }

            dem_export <- shiny::withProgress(
              message = "Reproyectando y recortando DEM de cuenca...",
              value = 0.25,
              {
                out <- dem_basin_utm_for_download(
                  dem_basin = x_download$dem,
                  basin = x_download$basin_sf,
                  utm_epsg = x_download$geom$epsg
                )

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


        output$descargar_dem_utm <- shiny::downloadHandler(
          filename = function() {

            x_download <- shiny::isolate(
              result()
            )

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

            epsg_tag <- if (
              is.finite(
                epsg_value
              )
            ) {
              paste0(
                "EPSG",
                epsg_value,
                "_"
              )
            } else {
              ""
            }

            paste0(
              "FABDEM_MOSAICO_A3_UTM_",
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
              is.null(
                x_download
              ) ||
              is.null(
                x_download$dem
              ) ||
              is.null(
                x_download$basin_sf
              ) ||
              is.null(
                x_download$geom
              ) ||
              is.null(
                x_download$geom$epsg
              )
            ) {
              stop(
                "No hay un DEM morfométrico listo para descargar."
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
              message = "Preparando DEM A3 en UTM...",
              value = 0.15,
              {
                out <- dem_a3_utm_for_download(
                  basin = x_download$basin_sf,
                  map_crs = map_crs,
                  utm_epsg = x_download$geom$epsg,
                  job_id = job_id
                )

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
              !file.exists(
                file
              ) ||
              !is.finite(
                file.info(
                  file
                )$size
              ) ||
              file.info(
                file
              )$size <= 0
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


        output$descargar_mapa_png <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "mapa_fisiografico_",
              format(
                Sys.time(),
                "%Y%m%d_%H%M%S"
              ),
              ".png"
            )
          },
          contentType = "image/png",
          content = function(file) {

            x_download <- shiny::isolate(
              result()
            )

            if (
              is.null(
                x_download
              ) ||
              is.null(
                x_download$dem
              )
            ) {
              stop(
                "No hay un mapa fisiográfico listo para descargar."
              )
            }

            # A3 apaisado exacto a 300 dpi:
            # 420 mm x 297 mm = 4961 x 3508 px aproximadamente.
            png_width_px <- as.integer(
              round(
                420 / 25.4 * 300
              )
            )

            png_height_px <- as.integer(
              round(
                297 / 25.4 * 300
              )
            )

            device_open <- FALSE

            tryCatch(
              {
                if (isTRUE(
                  capabilities(
                    "cairo"
                  )
                )) {
                  grDevices::png(
                    filename = file,
                    width = png_width_px,
                    height = png_height_px,
                    units = "px",
                    res = 300,
                    bg = "white",
                    type = "cairo-png"
                  )
                } else {
                  grDevices::png(
                    filename = file,
                    width = png_width_px,
                    height = png_height_px,
                    units = "px",
                    res = 300,
                    bg = "white"
                  )
                }

                device_open <- TRUE

                draw_mapa_fisiografico(
                  x = x_download
                )
              },
              finally = {
                if (isTRUE(
                  device_open
                )) {
                  grDevices::dev.off()
                }
              }
            )

            if (
              !file.exists(
                file
              ) ||
              !is.finite(
                file.info(
                  file
                )$size
              ) ||
              file.info(
                file
              )$size <= 0
            ) {
              stop(
                "El PNG no pudo generarse correctamente."
              )
            }
          }
        )

        shiny::outputOptions(
          output,
          "descargar_mapa_png",
          suspendWhenHidden = FALSE
        )


        draw_hipsometrica_plot <- function(x) {

          if (
            is.null(x) ||
            is.null(x$relief)
          ) {
            stop(
              "La curva hipsométrica aún no está disponible."
            )
          }

          z <- x$relief$elevation_sample

          if (length(z) <= 1L) {
            stop(
              "No hay suficientes datos de elevación."
            )
          }

          z <- sort(
            z,
            decreasing = TRUE
          )

          zrange <- max(
            z,
            na.rm = TRUE
          ) -
            min(
              z,
              na.rm = TRUE
            )

          if (
            !is.finite(zrange) ||
            zrange <= 0
          ) {
            stop(
              "El rango altitudinal no permite construir la curva hipsométrica."
            )
          }

          h <- (
            z -
              min(
                z,
                na.rm = TRUE
              )
          ) /
            zrange

          a <- seq(
            0,
            1,
            length.out = length(
              h
            )
          )

          graphics::plot(
            a,
            h,
            type = "l",
            lwd = 2,
            xlab = "Area relativa acumulada",
            ylab = "Elevacion relativa",
            xlim = c(
              0,
              1
            ),
            ylim = c(
              0,
              1
            )
          )

          graphics::grid()
        }


        draw_hist_elevacion_plot <- function(x) {

          if (
            is.null(x) ||
            is.null(x$relief)
          ) {
            stop(
              "La distribución altitudinal aún no está disponible."
            )
          }

          z <- x$relief$elevation_sample

          if (length(z) <= 1L) {
            stop(
              "No hay suficientes datos de elevación."
            )
          }

          graphics::hist(
            z,
            breaks = "FD",
            main = NULL,
            xlab = "Elevacion (m s.n.m.)",
            ylab = "Frecuencia"
          )
        }


        draw_hist_pendiente_plot <- function(x) {

          if (
            is.null(x) ||
            is.null(x$relief)
          ) {
            stop(
              "La distribución de pendientes aún no está disponible."
            )
          }

          s <- x$relief$slope_sample

          if (length(s) <= 1L) {
            stop(
              "No hay suficientes datos de pendiente."
            )
          }

          graphics::hist(
            s,
            breaks = "FD",
            main = NULL,
            xlab = "Pendiente (%)",
            ylab = "Frecuencia"
          )
        }


        draw_perfil_longitudinal_plot <- function(x) {

          if (
            is.null(x) ||
            is.null(x$hydro)
          ) {
            stop(
              "El perfil longitudinal aún no está disponible."
            )
          }

          hp <- x$hydro$hydraulic_profile
          cp <- x$hydro$channel_profile

          if (
            is.null(hp) ||
            nrow(hp) <= 1L
          ) {
            stop(
              "No hay suficientes datos para el perfil longitudinal."
            )
          }

          graphics::plot(
            hp$DIST_FROM_OUTLET_KM,
            hp$ELEVATION_M,
            type = "l",
            lwd = 2,
            xlab = "Distancia desde el outlet (km)",
            ylab = "Elevacion (m s.n.m.)"
          )

          if (
            !is.null(cp) &&
            nrow(cp) > 1L
          ) {
            graphics::lines(
              cp$DIST_FROM_OUTLET_KM,
              cp$ELEVATION_M,
              lwd = 2,
              lty = 2
            )
          }

          h <- x$hydro$hydraulic

          if (
            !is.null(h) &&
            is.finite(h$z10) &&
            is.finite(h$z85)
          ) {
            graphics::points(
              c(
                0.10 * h$length_km,
                0.85 * h$length_km
              ),
              c(
                h$z10,
                h$z85
              ),
              pch = 16
            )
          }

          graphics::legend(
            "topleft",
            legend = c(
              "Recorrido hidraulico D8",
              "Cauce principal"
            ),
            lty = c(
              1,
              2
            ),
            lwd = c(
              2,
              2
            ),
            bty = "n"
          )

          graphics::grid()
        }


        write_morph_plot_png <- function(
            file,
            x,
            draw_fun,
            width_px = 1800L,
            height_px = 1200L,
            res_dpi = 180L
        ) {

          device_open <- FALSE

          tryCatch(
            {
              if (isTRUE(
                capabilities(
                  "cairo"
                )
              )) {
                grDevices::png(
                  filename = file,
                  width = width_px,
                  height = height_px,
                  units = "px",
                  res = res_dpi,
                  bg = "white",
                  type = "cairo-png"
                )
              } else {
                grDevices::png(
                  filename = file,
                  width = width_px,
                  height = height_px,
                  units = "px",
                  res = res_dpi,
                  bg = "white"
                )
              }

              device_open <- TRUE

              draw_fun(
                x
              )

              grDevices::dev.off()
              device_open <- FALSE
            },
            finally = {
              if (device_open) {
                try(
                  grDevices::dev.off(),
                  silent = TRUE
                )
              }
            }
          )

          if (
            !file.exists(file) ||
            !is.finite(file.info(file)$size) ||
            file.info(file)$size <= 0
          ) {
            stop(
              "El PNG no pudo generarse correctamente."
            )
          }
        }


        output$descargar_hipsometrica_png <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "curva_hipsometrica_",
              format(Sys.time(), "%Y%m%d_%H%M%S"),
              ".png"
            )
          },
          contentType = "image/png",
          content = function(file) {
            x_download <- shiny::isolate(result())
            write_morph_plot_png(
              file,
              x_download,
              draw_hipsometrica_plot
            )
          }
        )


        output$descargar_hist_elevacion_png <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "distribucion_altitudinal_",
              format(Sys.time(), "%Y%m%d_%H%M%S"),
              ".png"
            )
          },
          contentType = "image/png",
          content = function(file) {
            x_download <- shiny::isolate(result())
            write_morph_plot_png(
              file,
              x_download,
              draw_hist_elevacion_plot
            )
          }
        )


        output$descargar_hist_pendiente_png <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "distribucion_pendientes_",
              format(Sys.time(), "%Y%m%d_%H%M%S"),
              ".png"
            )
          },
          contentType = "image/png",
          content = function(file) {
            x_download <- shiny::isolate(result())
            write_morph_plot_png(
              file,
              x_download,
              draw_hist_pendiente_plot
            )
          }
        )


        output$descargar_perfil_longitudinal_png <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "perfil_longitudinal_",
              format(Sys.time(), "%Y%m%d_%H%M%S"),
              ".png"
            )
          },
          contentType = "image/png",
          content = function(file) {
            x_download <- shiny::isolate(result())
            write_morph_plot_png(
              file,
              x_download,
              draw_perfil_longitudinal_plot
            )
          }
        )


        for (download_name in c(
          "descargar_hipsometrica_png",
          "descargar_hist_elevacion_png",
          "descargar_hist_pendiente_png",
          "descargar_perfil_longitudinal_png"
        )) {
          shiny::outputOptions(
            output,
            download_name,
            suspendWhenHidden = FALSE
          )
        }


        output$hipsometrica <- shiny::renderPlot({

          x <- result()

          shiny::req(
            x
          )

          if (is.null(
            x$relief
          )) {
            draw_stage_message(
              if (identical(
                stages$relief,
                "calculating"
              )) {
                "Calculando curva hipsométrica..."
              } else {
                "En espera del relieve FABDEM..."
              }
            )
            return(
              invisible(NULL)
            )
          }

          draw_hipsometrica_plot(
            x
          )
        })


        output$hist_elevacion <- shiny::renderPlot({

          x <- result()

          shiny::req(
            x
          )

          if (is.null(
            x$relief
          )) {
            draw_stage_message(
              if (identical(
                stages$relief,
                "calculating"
              )) {
                "Calculando distribución altitudinal..."
              } else {
                "En espera del relieve FABDEM..."
              }
            )
            return(
              invisible(NULL)
            )
          }

          draw_hist_elevacion_plot(
            x
          )
        })


        output$hist_pendiente <- shiny::renderPlot({

          x <- result()

          shiny::req(
            x
          )

          if (is.null(
            x$relief
          )) {
            draw_stage_message(
              if (identical(
                stages$relief,
                "calculating"
              )) {
                "Calculando distribución de pendientes..."
              } else {
                "En espera del relieve FABDEM..."
              }
            )
            return(
              invisible(NULL)
            )
          }

          draw_hist_pendiente_plot(
            x
          )
        })


        output$perfil_longitudinal <- shiny::renderPlot({

          x <- result()

          shiny::req(
            x
          )

          if (is.null(
            x$hydro
          )) {
            draw_stage_message(
              if (identical(
                stages$hydro,
                "calculating"
              )) {
                "Calculando recorrido hidráulico y perfil..."
              } else {
                "En espera del recorrido hidráulico..."
              }
            )
            return(
              invisible(NULL)
            )
          }

          draw_perfil_longitudinal_plot(
            x
          )
        })


        # Mantener activos los outputs aunque el conditionalPanel
        # este oculto durante el calculo. Debe ejecutarse despues
        # de crear todos los output$... anteriores.
        for (output_name in c(
          "tabla_principal",
          "tabla_avanzada",
          "tabla_recorrido_cauce",
          "tabla_red",
          "tabla_red_derivada",
          "tabla_tc",
          "tabla_strahler",
          "mapa_fisiografico",
          "hipsometrica",
          "hist_elevacion",
          "hist_pendiente",
          "perfil_longitudinal"
        )) {
          shiny::outputOptions(
            output,
            output_name,
            suspendWhenHidden = FALSE
          )
        }


        list(
          result = shiny::reactive(
            result()
          )
        )
      }
    )
  }


  list(
    ui = ui,
    server = server
  )
})

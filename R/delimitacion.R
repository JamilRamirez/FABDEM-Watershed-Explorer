# ============================================================
# R/delimitacion.R
#
# MODULO 01: DELIMITACION
# v5: admite cuenca delimitada o importada y permite exportar GPKG/SHP en WGS84 o UTM; KML/KMZ en WGS84
# ============================================================
#
# Interfaz deliberadamente minima:
# - mapa
# - clic
# - boton Delimitar
# - boton Limpiar
# - outlet ajustado
# - poligono final
# - exportacion vectorial de la cuenca activa
#
# NO muestra:
# - cuencas ANA
# - bloques
# - metricas
# - parametros tecnicos
# - diagnosticos
# - DEM
# - red hidrográfica auxiliar
# ============================================================


delimitacion <- local({

  format_cell_count <- function(n) {

    n <- as.numeric(
      n
    )

    if (!is.finite(n)) {
      return(
        "0 celdas"
      )
    }

    if (n >= 1e6) {
      return(
        paste0(
          sprintf(
            "%.2f",
            n / 1e6
          ),
          " M celdas"
        )
      )
    }

    if (n >= 1e3) {
      return(
        paste0(
          sprintf(
            "%.1f",
            n / 1e3
          ),
          " mil celdas"
        )
      )
    }

    paste0(
      format(
        round(
          n
        ),
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      ),
      " celdas"
    )
  }


  format_elapsed <- function(seconds) {

    seconds <- max(
      0,
      floor(
        as.numeric(
          seconds
        )
      )
    )

    hh <- seconds %/% 3600
    mm <- (seconds %% 3600) %/% 60
    ss <- seconds %% 60

    if (hh > 0) {
      return(
        sprintf(
          "%02d:%02d:%02d",
          hh,
          mm,
          ss
        )
      )
    }

    sprintf(
      "%02d:%02d",
      mm,
      ss
    )
  }


  trace_progress_value <- function(n_cells) {

    # El total final de la cuenca es desconocido durante el rastreo.
    # Esta funcion solo anima la barra dentro de la fase 30-70%.
    # El dato real mostrado es el numero de celdas y el tiempo.
    scaled <- log10(
      max(
        as.numeric(
          n_cells
        ),
        1
      )
    ) / log10(
      MAX_BASIN_CELLS
    )

    0.30 +
      min(
        0.40,
        max(
          0,
          scaled * 0.40
        )
      )
  }


  validate_lon_lat <- function(
      lon,
      lat
  ) {

    lon <- suppressWarnings(
      as.numeric(
        lon
      )
    )

    lat <- suppressWarnings(
      as.numeric(
        lat
      )
    )


    if (
      !is.finite(
        lon
      ) ||
      !is.finite(
        lat
      )
    ) {
      stop(
        "Las coordenadas geograficas deben ser numericas."
      )
    }


    if (
      lon < -180 ||
      lon > 180
    ) {
      stop(
        "La longitud debe estar entre -180 y 180 grados."
      )
    }


    if (
      lat < -90 ||
      lat > 90
    ) {
      stop(
        "La latitud debe estar entre -90 y 90 grados."
      )
    }


    list(
      lon = lon,
      lat = lat
    )
  }


  utm_to_lonlat <- function(
      easting,
      northing,
      zone,
      hemisphere
  ) {

    easting <- suppressWarnings(
      as.numeric(
        easting
      )
    )

    northing <- suppressWarnings(
      as.numeric(
        northing
      )
    )

    zone <- suppressWarnings(
      as.integer(
        zone
      )
    )

    hemisphere <- toupper(
      trimws(
        as.character(
          hemisphere
        )
      )
    )


    if (
      !is.finite(
        easting
      ) ||
      !is.finite(
        northing
      )
    ) {
      stop(
        "Este y Norte UTM deben ser numericos."
      )
    }


    if (
      easting <= 0 ||
      northing < 0
    ) {
      stop(
        "Las coordenadas UTM deben ser positivas."
      )
    }


    if (
      !is.finite(
        zone
      ) ||
      zone < 1L ||
      zone > 60L
    ) {
      stop(
        "La zona UTM debe estar entre 1 y 60."
      )
    }


    if (!hemisphere %in%
      c(
        "N",
        "S"
      )
    ) {
      stop(
        "Hemisferio UTM invalido."
      )
    }


    epsg <- if (identical(
      hemisphere,
      "N"
    )) {
      32600L +
        zone
    } else {
      32700L +
        zone
    }


    p_utm <- sf::st_sfc(
      sf::st_point(
        c(
          easting,
          northing
        )
      ),
      crs = epsg
    )


    p_wgs <- sf::st_transform(
      p_utm,
      4326
    )


    xy <- sf::st_coordinates(
      p_wgs
    )[1, ]


    validate_lon_lat(
      lon = xy[1],
      lat = xy[2]
    )
  }


  dms_to_decimal <- function(
      degrees,
      minutes,
      seconds,
      hemisphere,
      axis
  ) {

    degrees <- suppressWarnings(
      as.numeric(
        degrees
      )
    )

    minutes <- suppressWarnings(
      as.numeric(
        minutes
      )
    )

    seconds <- suppressWarnings(
      as.numeric(
        seconds
      )
    )

    hemisphere <- toupper(
      trimws(
        as.character(
          hemisphere
        )
      )
    )

    axis <- match.arg(
      axis,
      c(
        "lon",
        "lat"
      )
    )


    if (
      !is.finite(
        degrees
      ) ||
      !is.finite(
        minutes
      ) ||
      !is.finite(
        seconds
      )
    ) {
      stop(
        "Grados, minutos y segundos deben ser numericos."
      )
    }


    max_degrees <- if (identical(
      axis,
      "lon"
    )) {
      180
    } else {
      90
    }


    if (
      degrees < 0 ||
      degrees > max_degrees
    ) {
      stop(
        paste0(
          "Grados fuera de rango para ",
          if (identical(
            axis,
            "lon"
          )) {
            "longitud"
          } else {
            "latitud"
          },
          "."
        )
      )
    }


    if (
      minutes < 0 ||
      minutes >= 60
    ) {
      stop(
        "Los minutos deben estar entre 0 y menos de 60."
      )
    }


    if (
      seconds < 0 ||
      seconds >= 60
    ) {
      stop(
        "Los segundos deben estar entre 0 y menos de 60."
      )
    }


    valid_hemispheres <- if (identical(
      axis,
      "lon"
    )) {
      c(
        "E",
        "W"
      )
    } else {
      c(
        "N",
        "S"
      )
    }


    if (!hemisphere %in%
      valid_hemispheres
    ) {
      stop(
        "Hemisferio invalido para coordenadas GMS."
      )
    }


    value <- degrees +
      minutes /
        60 +
      seconds /
        3600


    if (hemisphere %in%
      c(
        "W",
        "S"
      )
    ) {
      value <- -value
    }


    value
  }


  dms_pair_to_lonlat <- function(
      lon_deg,
      lon_min,
      lon_sec,
      lon_hemi,
      lat_deg,
      lat_min,
      lat_sec,
      lat_hemi
  ) {

    lon <- dms_to_decimal(
      degrees = lon_deg,
      minutes = lon_min,
      seconds = lon_sec,
      hemisphere = lon_hemi,
      axis = "lon"
    )


    lat <- dms_to_decimal(
      degrees = lat_deg,
      minutes = lat_min,
      seconds = lat_sec,
      hemisphere = lat_hemi,
      axis = "lat"
    )


    validate_lon_lat(
      lon = lon,
      lat = lat
    )
  }


  # ==========================================================
  # EXPORTACION VECTORIAL DE LA CUENCA ACTIVA
  # ==========================================================

  export_safe_stem <- function(x) {

    if (
      length(x) != 1L ||
      is.na(x) ||
      !nzchar(
        trimws(
          as.character(x)
        )
      )
    ) {
      return(
        "cuenca"
      )
    }


    x <- trimws(
      as.character(
        x
      )
    )


    x_ascii <- suppressWarnings(
      iconv(
        x,
        from = "",
        to = "ASCII//TRANSLIT",
        sub = ""
      )
    )


    if (
      is.na(
        x_ascii
      ) ||
      !nzchar(
        x_ascii
      )
    ) {
      x_ascii <- "cuenca"
    }


    x_ascii <- gsub(
      "[^A-Za-z0-9_-]+",
      "_",
      x_ascii
    )


    x_ascii <- gsub(
      "_+",
      "_",
      x_ascii
    )


    x_ascii <- gsub(
      "^_+|_+$",
      "",
      x_ascii
    )


    if (!nzchar(
      x_ascii
    )) {
      x_ascii <- "cuenca"
    }


    substr(
      x_ascii,
      1L,
      80L
    )
  }


  standardize_basin_export <- function(
      basin,
      basin_label = NULL,
      basin_source = NULL
  ) {

    if (
      is.null(
        basin
      ) ||
      !inherits(
        basin,
        "sf"
      ) ||
      nrow(
        basin
      ) < 1L
    ) {
      stop(
        "No hay una cuenca activa valida para exportar."
      )
    }


    if (is.na(
      sf::st_crs(
        basin
      )
    )) {
      stop(
        "La cuenca activa no tiene CRS y no puede exportarse de forma segura."
      )
    }


    x <- sf::st_make_valid(
      basin
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
        "La cuenca activa quedo sin geometria valida."
      )
    }


    geometry_types <- as.character(
      sf::st_geometry_type(
        x,
        by_geometry = TRUE
      )
    )


    if (!all(
      geometry_types %in%
        c(
          "POLYGON",
          "MULTIPOLYGON"
        )
    )) {
      stop(
        "La cuenca activa contiene geometria no poligonal."
      )
    }


    merged_geometry <- suppressWarnings(
      sf::st_union(
        sf::st_geometry(
          x
        )
      )
    )


    label_value <- if (
      length(
        basin_label
      ) == 1L &&
      !is.na(
        basin_label
      ) &&
      nzchar(
        trimws(
          as.character(
            basin_label
          )
        )
      )
    ) {
      trimws(
        as.character(
          basin_label
        )
      )
    } else {
      "Cuenca"
    }


    source_value <- if (
      identical(
        basin_source,
        "delineated"
      )
    ) {
      "DELINEATED"
    } else if (
      identical(
        basin_source,
        "imported"
      )
    ) {
      "IMPORTED"
    } else {
      "ACTIVE"
    }


    out <- sf::st_sf(
      NAME = substr(
        label_value,
        1L,
        200L
      ),
      SOURCE = source_value,
      geometry = merged_geometry
    )


    out <- sf::st_make_valid(
      out
    )


    out <- out[
      !sf::st_is_empty(
        out
      ),
      ,
      drop = FALSE
    ]


    if (nrow(
      out
    ) < 1L) {
      stop(
        "No fue posible preparar la geometria para exportacion."
      )
    }


    out
  }



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


  writable_sf_driver <- function(
      preferred,
      fallback = NULL
  ) {

    drivers <- sf::st_drivers()


    available <- as.character(
      drivers$name[
        drivers$write
      ]
    )


    if (preferred %in% available) {
      return(
        preferred
      )
    }


    if (
      !is.null(
        fallback
      ) &&
      fallback %in% available
    ) {
      return(
        fallback
      )
    }


    stop(
      paste0(
        "El entorno GDAL no dispone del driver de escritura requerido: ",
        preferred,
        "."
      )
    )
  }


  write_zip_archive <- function(
      zip_file,
      files,
      root_dir
  ) {

    files <- normalizePath(
      files,
      winslash = "/",
      mustWork = TRUE
    )


    root_dir <- normalizePath(
      root_dir,
      winslash = "/",
      mustWork = TRUE
    )


    zip_file <- normalizePath(
      zip_file,
      winslash = "/",
      mustWork = FALSE
    )


    relative_files <- basename(
      files
    )


    if (requireNamespace(
      "zip",
      quietly = TRUE
    )) {

      zip::zipr(
        zipfile = zip_file,
        files = relative_files,
        root = root_dir
      )

    } else {

      zip_command <- Sys.which(
        "zip"
      )


      if (!nzchar(
        zip_command
      )) {
        stop(
          paste0(
            "Para generar KMZ o Shapefile ZIP se requiere el paquete R 'zip' ",
            "o el ejecutable del sistema 'zip'."
          )
        )
      }


      old_wd <- getwd()


      on.exit(
        setwd(
          old_wd
        ),
        add = TRUE
      )


      setwd(
        root_dir
      )


      suppressWarnings(
        utils::zip(
          zipfile = zip_file,
          files = relative_files,
          flags = "-q"
        )
      )
    }


    if (
      !file.exists(
        zip_file
      ) ||
      !is.finite(
        file.info(
          zip_file
        )$size
      ) ||
      file.info(
        zip_file
      )$size <= 0
    ) {
      stop(
        "No se pudo generar el archivo comprimido."
      )
    }


    zip_file
  }


  write_basin_export <- function(
      basin,
      format,
      target_file,
      basin_label = NULL,
      basin_source = NULL,
      crs_mode = "utm"
  ) {

    format <- match.arg(
      format,
      c(
        "gpkg",
        "kml",
        "kmz",
        "shp_zip"
      )
    )


    x <- standardize_basin_export(
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


    stem <- export_safe_stem(
      basin_label
    )


    work_dir <- tempfile(
      pattern = "FABDEM_EXPORT_"
    )


    dir.create(
      work_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )


    on.exit(
      unlink(
        work_dir,
        recursive = TRUE,
        force = TRUE
      ),
      add = TRUE
    )


    artifact <- NULL


    if (identical(
      format,
      "gpkg"
    )) {

      writable_sf_driver(
        "GPKG"
      )


      artifact <- file.path(
        work_dir,
        paste0(
          stem,
          ".gpkg"
        )
      )


      sf::st_write(
        x,
        artifact,
        layer = "cuenca",
        driver = "GPKG",
        quiet = TRUE,
        delete_dsn = TRUE
      )
    }


    if (identical(
      format,
      "kml"
    )) {

      kml_driver <- writable_sf_driver(
        "KML",
        fallback = "LIBKML"
      )


      x_wgs84 <- sf::st_transform(
        x,
        4326
      )


      artifact <- file.path(
        work_dir,
        paste0(
          stem,
          ".kml"
        )
      )


      sf::st_write(
        x_wgs84,
        artifact,
        layer = "cuenca",
        driver = kml_driver,
        quiet = TRUE,
        delete_dsn = TRUE
      )
    }


    if (identical(
      format,
      "kmz"
    )) {

      kml_driver <- writable_sf_driver(
        "KML",
        fallback = "LIBKML"
      )


      x_wgs84 <- sf::st_transform(
        x,
        4326
      )


      kmz_dir <- file.path(
        work_dir,
        "kmz"
      )


      dir.create(
        kmz_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )


      kml_file <- file.path(
        kmz_dir,
        "doc.kml"
      )


      sf::st_write(
        x_wgs84,
        kml_file,
        layer = "cuenca",
        driver = kml_driver,
        quiet = TRUE,
        delete_dsn = TRUE
      )


      artifact <- file.path(
        work_dir,
        paste0(
          stem,
          ".kmz"
        )
      )


      write_zip_archive(
        zip_file = artifact,
        files = kml_file,
        root_dir = kmz_dir
      )
    }


    if (identical(
      format,
      "shp_zip"
    )) {

      writable_sf_driver(
        "ESRI Shapefile"
      )


      shp_dir <- file.path(
        work_dir,
        "shapefile"
      )


      dir.create(
        shp_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )


      shp_file <- file.path(
        shp_dir,
        paste0(
          stem,
          ".shp"
        )
      )


      sf::st_write(
        x,
        shp_file,
        layer = stem,
        driver = "ESRI Shapefile",
        quiet = TRUE,
        delete_dsn = TRUE
      )


      shp_members <- list.files(
        shp_dir,
        full.names = TRUE
      )


      shp_members <- shp_members[
        tolower(
          tools::file_ext(
            shp_members
          )
        ) %in%
          c(
            "shp",
            "shx",
            "dbf",
            "prj",
            "cpg"
          )
      ]


      required_extensions <- c(
        "shp",
        "shx",
        "dbf"
      )


      found_extensions <- unique(
        tolower(
          tools::file_ext(
            shp_members
          )
        )
      )


      if (!all(
        required_extensions %in%
          found_extensions
      )) {
        stop(
          "El Shapefile generado esta incompleto."
        )
      }


      artifact <- file.path(
        work_dir,
        paste0(
          stem,
          "_shp.zip"
        )
      )


      write_zip_archive(
        zip_file = artifact,
        files = shp_members,
        root_dir = shp_dir
      )
    }


    if (
      is.null(
        artifact
      ) ||
      !file.exists(
        artifact
      ) ||
      file.info(
        artifact
      )$size <= 0
    ) {
      stop(
        "No se genero correctamente la exportacion."
      )
    }


    copied <- file.copy(
      artifact,
      target_file,
      overwrite = TRUE
    )


    if (
      !isTRUE(
        copied
      ) ||
      !file.exists(
        target_file
      ) ||
      file.info(
        target_file
      )$size <= 0
    ) {
      stop(
        "No se pudo entregar el archivo exportado."
      )
    }


    invisible(
      target_file
    )
  }


  # ==========================================================
  # RESOLUCION EXPLICITA DE AMBIGUEDAD HIDROLOGICA
  # ==========================================================

  AMBIGUITY_JUNCTION_RADIUS_M <- 450
  AMBIGUITY_WIDE_TRIGGER_M <- 350
  AMBIGUITY_WIDE_SCAN_M <- 3000
  AMBIGUITY_MAX_OPTIONS <- 3L


  ambiguity_reverse_parent_cells <- function(
      reverse_cache,
      cell
  ) {

    nc <- as.double(reverse_cache$metadata$ncols)
    nr <- as.double(reverse_cache$metadata$nrows)
    cell <- as.double(cell)

    row <- floor((cell - 1) / nc) + 1
    col <- ((cell - 1) %% nc) + 1

    value <- get_reverse_values(
      reverse_cache,
      cell
    )

    if (
      length(value) != 1L ||
      !is.finite(value)
    ) {
      return(numeric(0))
    }

    cells <- c(
      cell - nc - 1,
      cell - nc,
      cell - nc + 1,
      cell - 1,
      cell + 1,
      cell + nc - 1,
      cell + nc,
      cell + nc + 1
    )

    bits <- c(
      1L, 2L, 4L, 8L,
      16L, 32L, 64L, 128L
    )

    valid <- c(
      row > 1 && col > 1,
      row > 1,
      row > 1 && col < nc,
      col > 1,
      col < nc,
      row < nr && col > 1,
      row < nr,
      row < nr && col < nc
    )

    keep <- valid &
      bitwAnd(
        as.integer(value),
        bits
      ) != 0L

    as.double(cells[keep])
  }


  ambiguity_stream_parent_cells <- function(
      reverse_cache,
      cell,
      grid_template,
      stream_cache,
      stripe_rows,
      n_stripes,
      threshold_cells
  ) {

    parents <- ambiguity_reverse_parent_cells(
      reverse_cache,
      cell
    )

    if (length(parents) == 0L) {
      return(numeric(0))
    }

    keep <- vapply(
      parents,
      function(parent_cell) {

        stream_value <- snap_stream_value_at_cell(
          cell = parent_cell,
          grid_template = grid_template,
          stream_cache = stream_cache,
          stripe_rows = stripe_rows,
          n_stripes = n_stripes
        )

        isTRUE(
          is.finite(stream_value) &&
          stream_value > 0 &&
          snap_upstream_reaches_threshold(
            reverse_cache = reverse_cache,
            outlet_cell = parent_cell,
            threshold_cells = threshold_cells
          )
        )
      },
      logical(1)
    )

    as.double(parents[keep])
  }


  ambiguity_snap_from_cell <- function(
      lon,
      lat,
      outlet_cell,
      role,
      mode,
      reverse_cache,
      grid_template,
      stream_cache,
      stripe_rows,
      n_stripes,
      threshold_cells
  ) {

    outlet_cell <- as.double(outlet_cell)

    if (
      !is.finite(outlet_cell) ||
      !snap_upstream_reaches_threshold(
        reverse_cache = reverse_cache,
        outlet_cell = outlet_cell,
        threshold_cells = threshold_cells
      )
    ) {
      return(NULL)
    }

    stream_value <- snap_stream_value_at_cell(
      cell = outlet_cell,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes
    )

    if (!is.finite(stream_value) || stream_value <= 0) {
      return(NULL)
    }

    out <- snap_build_result(
      lon = lon,
      lat = lat,
      outlet_cell = outlet_cell,
      grid_template = grid_template,
      grid_crs = sf::st_crs(
        terra::crs(grid_template)
      ),
      stream_value = stream_value,
      mode = mode
    )

    out$ambiguity_role <- role
    out
  }


  ambiguity_near_confluence <- function(
      lon,
      lat,
      grid_template,
      stream_cache,
      stripe_rows,
      n_stripes,
      reverse_cache,
      threshold_cells
  ) {

    candidates <- snap_collect_stream_candidates(
      lon = lon,
      lat = lat,
      radius_m = AMBIGUITY_JUNCTION_RADIUS_M,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes
    )

    if (nrow(candidates) == 0L) {
      return(NULL)
    }

    probe <- seq_len(
      min(
        nrow(candidates),
        500L
      )
    )

    for (idx in probe) {

      junction_cell <- as.double(
        candidates$cell[idx]
      )

      parents <- ambiguity_stream_parent_cells(
        reverse_cache = reverse_cache,
        cell = junction_cell,
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes,
        threshold_cells = threshold_cells
      )

      if (length(parents) < 2L) {
        next
      }

      options <- list()

      for (ii in seq_len(
        min(
          2L,
          length(parents)
        )
      )) {

        option <- ambiguity_snap_from_cell(
          lon = lon,
          lat = lat,
          outlet_cell = parents[ii],
          role = paste0(
            'Rama aguas arriba ',
            ii
          ),
          mode = 'AMBIGUOUS_CONFLUENCE_UPSTREAM',
          reverse_cache = reverse_cache,
          grid_template = grid_template,
          stream_cache = stream_cache,
          stripe_rows = stripe_rows,
          n_stripes = n_stripes,
          threshold_cells = threshold_cells
        )

        if (!is.null(option)) {
          options[[length(options) + 1L]] <- option
        }
      }

      downstream <- snap_downstream_cell(
        reverse_cache = reverse_cache,
        cell = junction_cell
      )

      if (is.finite(downstream)) {

        option <- ambiguity_snap_from_cell(
          lon = lon,
          lat = lat,
          outlet_cell = downstream,
          role = 'Aguas abajo de la confluencia',
          mode = 'AMBIGUOUS_CONFLUENCE_DOWNSTREAM',
          reverse_cache = reverse_cache,
          grid_template = grid_template,
          stream_cache = stream_cache,
          stripe_rows = stripe_rows,
          n_stripes = n_stripes,
          threshold_cells = threshold_cells
        )

        if (!is.null(option)) {
          options[[length(options) + 1L]] <- option
        }
      }

      if (length(options) >= 2L) {

        cells <- vapply(
          options,
          function(x) as.double(x$outlet_cell),
          numeric(1)
        )

        options <- options[
          !duplicated(cells)
        ]

        if (length(options) >= 2L) {
          return(
            list(
              status = 'ambiguous',
              reason = 'CONFLUENCE',
              options = head(
                options,
                AMBIGUITY_MAX_OPTIONS
              )
            )
          )
        }
      }
    }

    NULL
  }


  ambiguity_wide_components <- function(
      lon,
      lat,
      primary,
      grid_template,
      stream_cache,
      stripe_rows,
      n_stripes,
      reverse_cache,
      threshold_cells
  ) {

    if (
      is.null(primary) ||
      !is.finite(primary$snap_distance_m) ||
      primary$snap_distance_m < AMBIGUITY_WIDE_TRIGGER_M
    ) {
      return(NULL)
    }

    candidates <- snap_collect_stream_candidates(
      lon = lon,
      lat = lat,
      radius_m = AMBIGUITY_WIDE_SCAN_M,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes
    )

    if (nrow(candidates) < 2L) {
      return(NULL)
    }

    component_id <- snap_stream_component_ids(
      cells = candidates$cell,
      grid_ncols = terra::ncol(grid_template)
    )

    components <- unique(component_id)

    if (length(components) < 2L) {
      return(NULL)
    }

    best <- vapply(
      components,
      function(component_now) {
        idx <- which(
          component_id == component_now
        )
        idx[
          which.min(
            candidates$distance_m[idx]
          )
        ]
      },
      integer(1)
    )

    best <- best[
      order(
        candidates$distance_m[best]
      )
    ]

    primary$ambiguity_role <- 'Cauce inicialmente seleccionado'
    options <- list(primary)

    for (idx in best) {

      cell_now <- as.double(
        candidates$cell[idx]
      )

      if (
        cell_now ==
          as.double(primary$outlet_cell)
      ) {
        next
      }

      option <- ambiguity_snap_from_cell(
        lon = lon,
        lat = lat,
        outlet_cell = cell_now,
        role = 'Cauce alternativo cercano',
        mode = 'AMBIGUOUS_WIDE_MULTICHANNEL',
        reverse_cache = reverse_cache,
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes,
        threshold_cells = threshold_cells
      )

      if (is.null(option)) {
        next
      }

      options[[length(options) + 1L]] <- option

      if (length(options) >= AMBIGUITY_MAX_OPTIONS) {
        break
      }
    }

    if (length(options) >= 2L) {
      return(
        list(
          status = 'ambiguous',
          reason = 'WIDE_OR_MULTICHANNEL',
          options = options
        )
      )
    }

    NULL
  }


  find_ambiguity_options <- function(
      lon,
      lat,
      radius_m,
      grid_template,
      stream_cache,
      stripe_rows,
      n_stripes
  ) {

    block_id <- as.character(
      stream_cache$block_id
    )

    meta <- get_block_metadata(
      block_id
    )

    threshold_cells <- as.double(
      meta[['STREAM_THRESHOLD_CELLS']][1]
    )

    reverse_cache <- new_snap_reverse_cache(
      block_id
    )

    confluence <- ambiguity_near_confluence(
      lon = lon,
      lat = lat,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes,
      reverse_cache = reverse_cache,
      threshold_cells = threshold_cells
    )

    if (!is.null(confluence)) {
      return(confluence)
    }

    primary <- tryCatch(
      snap_to_stream_stripes(
        lon = lon,
        lat = lat,
        radius_m = radius_m,
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes
      ),
      error = function(e) e
    )

    if (inherits(primary, 'error')) {

      message_text <- conditionMessage(primary)

      if (!grepl(
        'entre dos cauces hidrologicos distintos',
        message_text,
        fixed = TRUE
      )) {
        stop(primary)
      }

      radii_m <- snap_progressive_radii(
        radius_m
      )

      candidates <- snap_collect_stream_candidates(
        lon = lon,
        lat = lat,
        radius_m = max(radii_m),
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes
      )

      choice <- snap_select_progressive_candidate(
        candidates = candidates,
        radii_m = radii_m,
        grid_ncols = terra::ncol(grid_template)
      )

      if (!identical(
        choice$status,
        'ambiguous'
      )) {
        stop(primary)
      }

      idxs <- c(
        choice$winner_index,
        choice$alternative_index
      )

      options <- lapply(
        seq_along(idxs),
        function(ii) {
          ambiguity_snap_from_cell(
            lon = lon,
            lat = lat,
            outlet_cell = candidates$cell[idxs[ii]],
            role = paste0(
              'Cauce candidato ',
              ii
            ),
            mode = 'AMBIGUOUS_NEAR_TIE',
            reverse_cache = reverse_cache,
            grid_template = grid_template,
            stream_cache = stream_cache,
            stripe_rows = stripe_rows,
            n_stripes = n_stripes,
            threshold_cells = threshold_cells
          )
        }
      )

      options <- Filter(
        Negate(is.null),
        options
      )

      if (length(options) >= 2L) {
        return(
          list(
            status = 'ambiguous',
            reason = 'NEAR_TIE',
            options = options
          )
        )
      }

      stop(primary)
    }

    wide <- ambiguity_wide_components(
      lon = lon,
      lat = lat,
      primary = primary,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes,
      reverse_cache = reverse_cache,
      threshold_cells = threshold_cells
    )

    if (!is.null(wide)) {
      return(wide)
    }

    list(
      status = 'single',
      reason = 'UNAMBIGUOUS',
      options = list(primary)
    )
  }



  ui <- function(id) {

    ns <- shiny::NS(
      id
    )


    shiny::tagList(

      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".delimitacion-shell{position:relative;width:100%;}",
            ".delimitacion-panel{",
            "position:absolute;top:14px;left:56px;z-index:1000;",
            "width:370px;max-height:calc(100vh - 100px);overflow-y:auto;",
            "background:rgba(255,255,255,0.97);padding:12px;",
            "border-radius:7px;box-shadow:0 1px 7px rgba(0,0,0,0.30);}",
            ".delimitacion-panel .form-group{margin-bottom:9px;}",
            ".delimitacion-actions{display:flex;gap:8px;margin-top:8px;}",
            ".delimitacion-actions .btn{margin:0;flex:1;}",
            ".coord-grid-2{display:grid;grid-template-columns:1fr 1fr;gap:8px;}",
            ".coord-grid-3{display:grid;grid-template-columns:1fr 1fr 1fr;gap:6px;}",
            ".coord-note{font-size:12px;color:#555;margin:4px 0 8px 0;}",
            ".delimitacion-export{margin-top:12px;padding-top:10px;border-top:1px solid #d8d8d8;}",
            ".delimitacion-export .btn{width:100%;margin-top:2px;}",
            ".ambiguity-box{margin:10px 0;padding:10px;border:1px solid #f0ad4e;",
            "border-radius:6px;background:#fff8e8;}"
          )
        )
      ),


      shiny::div(
        class = "delimitacion-shell",


        leaflet::leafletOutput(
          ns(
            "mapa"
          ),
          height = "calc(100vh - 70px)"
        ),


        shiny::div(
          class = "delimitacion-panel",


          shiny::radioButtons(
            ns(
              "origen_cuenca"
            ),
            label = "Origen de cuenca",
            choices = c(
              "Delimitar desde punto" = "delineated",
              "Cargar cuenca" = "imported"
            ),
            selected = "delineated"
          ),


          shiny::uiOutput(
            ns(
              "origin_controls"
            )
          ),


          shiny::uiOutput(
            ns(
              "ambiguity_controls"
            )
          ),


          shiny::uiOutput(
            ns(
              "export_controls"
            )
          ),

          layer_source_ui("fabdem_dem")
        )
      )
    )
  }

  server <- function(id) {

    shiny::moduleServer(
      id,
      function(
          input,
          output,
          session
      ) {

        click_value <- shiny::reactiveVal(
          NULL
        )


        block_selected <- shiny::reactiveVal(
          NULL
        )


        basin_result <- shiny::reactiveVal(
          NULL
        )


        outlet_result <- shiny::reactiveVal(
          NULL
        )


        output_folder <- shiny::reactiveVal(
          NULL
        )


        block_id_result <- shiny::reactiveVal(
          NULL
        )


        basin_source_result <- shiny::reactiveVal(
          NULL
        )


        basin_label_result <- shiny::reactiveVal(
          NULL
        )


        imported_candidates <- shiny::reactiveVal(
          NULL
        )


        upload_work_dir <- file.path(
          TERRA_TEMP,
          paste0(
            "BASIN_UPLOAD_",
            gsub(
              "[^A-Za-z0-9_]",
              "_",
              session$token
            )
          )
        )


        block_cache <- shiny::reactiveValues(
          block_id = NULL,
          grid_template = NULL,
          reverse_cache = NULL,
          stream_cache = NULL,
          stripe_rows = NULL,
          n_stripes = NULL,
          stream_threshold_cells = NULL,
          stream_threshold_km2 = NULL
        )





        ambiguity_previews <- shiny::reactiveVal(NULL)
        ambiguity_context <- shiny::reactiveVal(NULL)

        ambiguity_colors <- c(
          '#1565C0',
          '#2E7D32',
          '#6A1B9A'
        )


        clear_ambiguity_preview <- function(
            clear_map = TRUE,
            remove_files = TRUE
        ) {

          context <- ambiguity_context()
          ambiguity_previews(NULL)
          ambiguity_context(NULL)

          if (
            isTRUE(remove_files) &&
            !is.null(context) &&
            !is.null(context$work_root) &&
            dir.exists(context$work_root)
          ) {
            unlink(
              context$work_root,
              recursive = TRUE,
              force = TRUE
            )
          }

          if (isTRUE(clear_map)) {
            leaflet::leafletProxy(
              'mapa',
              session = session
            ) |>
              leaflet::clearGroup('Cuencas candidatas') |>
              leaflet::clearGroup('Outlets candidatos')
          }

          invisible(NULL)
        }


        ambiguity_area_km2 <- function(basin_sf) {
          sum(
            as.double(
              sf::st_area(
                sf::st_transform(
                  basin_sf,
                  6933
                )
              )
            ),
            na.rm = TRUE
          ) / 1e6
        }


        materialize_ambiguity_option <- function(
            option,
            candidate_index,
            grid_template,
            work_root
        ) {

          candidate_dir <- file.path(
            work_root,
            paste0('candidate_', candidate_index)
          )

          dir.create(
            candidate_dir,
            recursive = TRUE,
            showWarnings = FALSE
          )

          temp_dir <- file.path(
            candidate_dir,
            'terra_tmp'
          )

          dir.create(
            temp_dir,
            recursive = TRUE,
            showWarnings = FALSE
          )

          trace <- trace_upstream(
            cache = block_cache$reverse_cache,
            outlet_cell = option$outlet_cell
          )

          if (
            trace$n_cells <
              block_cache$stream_threshold_cells
          ) {
            stop(
              'La alternativa no alcanza el umbral hidrologico minimo.'
            )
          }

          trace_cells <- as.double(
            trace$n_cells
          )

          trace_engine <- if (
            !is.null(trace$trace_engine)
          ) {
            as.character(trace$trace_engine)
          } else {
            'UNKNOWN'
          }

          basin_tif <- file.path(
            candidate_dir,
            'basin.tif'
          )

          write_basin_raster(
            cells = trace$cells,
            template = grid_template,
            output_file = basin_tif,
            temp_dir = temp_dir,
            bbox = trace$bbox
          )

          rm(trace)
          gc()

          basin_gpkg <- file.path(
            candidate_dir,
            'basin.gpkg'
          )

          basin_sf <- polygonize_basin(
            basin_tif,
            basin_gpkg,
            expected_cells = trace_cells
          )

          area_km2 <- ambiguity_area_km2(
            basin_sf
          )

          outlet_sf <- sf::st_sf(
            OPTION = candidate_index,
            ROLE = if (!is.null(option$ambiguity_role)) {
              option$ambiguity_role
            } else {
              paste0('Opcion ', candidate_index)
            },
            SNAP_MODE = option$snap_mode,
            SNAP_DISTANCE_M = option$snap_distance_m,
            geometry = sf::st_sfc(
              sf::st_point(
                c(
                  option$outlet_lon,
                  option$outlet_lat
                )
              ),
              crs = 4326
            )
          )

          outlet_file <- file.path(
            candidate_dir,
            'outlet.gpkg'
          )

          sf::st_write(
            outlet_sf,
            outlet_file,
            layer = 'outlet',
            quiet = TRUE,
            delete_dsn = TRUE
          )

          basin_map <- basin_sf |>
            sf::st_transform(3857) |>
            sf::st_simplify(
              dTolerance = MAP_SIMPLIFY_M,
              preserveTopology = TRUE
            ) |>
            sf::st_transform(4326)

          list(
            index = candidate_index,
            snap = option,
            basin_sf = basin_sf,
            basin_map = basin_map,
            basin_tif = basin_tif,
            basin_gpkg = basin_gpkg,
            outlet_sf = outlet_sf,
            outlet_file = outlet_file,
            area_km2 = area_km2,
            trace_cells = trace_cells,
            trace_engine = trace_engine
          )
        }


        render_ambiguity_map <- function(previews) {

          proxy <- leaflet::leafletProxy(
            'mapa',
            session = session
          ) |>
            leaflet::clearGroup('Punto') |>
            leaflet::clearGroup('Cuenca delimitada') |>
            leaflet::clearGroup('Cuencas candidatas') |>
            leaflet::clearGroup('Outlets candidatos')

          bboxes <- list()

          for (ii in seq_along(previews)) {

            preview <- previews[[ii]]
            color_now <- ambiguity_colors[ii]

            label_now <- paste0(
              'Opcion ',
              ii,
              ' · ',
              format(
                round(preview$area_km2, 1),
                big.mark = ',',
                scientific = FALSE,
                trim = TRUE
              ),
              ' km²'
            )

            proxy <- proxy |>
              leaflet::addPolygons(
                data = preview$basin_map,
                group = 'Cuencas candidatas',
                layerId = paste0('AMBIG_BASIN_', ii),
                color = color_now,
                weight = 3,
                opacity = 0.95,
                fillColor = color_now,
                fillOpacity = 0.12,
                label = label_now
              ) |>
              leaflet::addCircleMarkers(
                lng = preview$snap$outlet_lon,
                lat = preview$snap$outlet_lat,
                group = 'Outlets candidatos',
                layerId = paste0('AMBIG_', ii),
                radius = 10,
                color = color_now,
                fillColor = color_now,
                fillOpacity = 1,
                weight = 3,
                label = label_now,
                labelOptions = leaflet::labelOptions(
                  noHide = TRUE,
                  direction = 'top'
                )
              )

            bboxes[[ii]] <- sf::st_bbox(
              preview$basin_map
            )
          }

          bbox_matrix <- do.call(
            rbind,
            lapply(bboxes, as.numeric)
          )

          proxy |>
            leaflet::fitBounds(
              min(bbox_matrix[, 1], na.rm = TRUE),
              min(bbox_matrix[, 2], na.rm = TRUE),
              max(bbox_matrix[, 3], na.rm = TRUE),
              max(bbox_matrix[, 4], na.rm = TRUE)
            )

          invisible(NULL)
        }


        output$ambiguity_controls <- shiny::renderUI({

          previews <- ambiguity_previews()

          if (
            is.null(previews) ||
            length(previews) < 2L
          ) {
            return(NULL)
          }

          shiny::div(
            class = 'ambiguity-box',
            shiny::tags$strong(
              'Se detectaron varias cuencas posibles'
            ),
            shiny::div(
              class = 'coord-note',
              paste0(
                'Haz clic en uno de los outlets numerados del mapa ',
                'o elige una opcion aqui. Las alternativas ya fueron ',
                'delimitadas para que puedas comparar sus areas.'
              )
            ),
            lapply(
              seq_along(previews),
              function(ii) {
                shiny::actionButton(
                  session$ns(
                    paste0('elegir_candidato_', ii)
                  ),
                  paste0(
                    'Opcion ',
                    ii,
                    ' · ',
                    format(
                      round(
                        previews[[ii]]$area_km2,
                        1
                      ),
                      big.mark = ',',
                      scientific = FALSE,
                      trim = TRUE
                    ),
                    ' km²'
                  ),
                  width = '100%',
                  style = 'margin-top:5px;'
                )
              }
            )
          )
        })


        activate_ambiguity_candidate <- function(candidate_index) {

          previews <- ambiguity_previews()
          context <- ambiguity_context()
          candidate_index <- as.integer(candidate_index)

          if (
            is.null(previews) ||
            is.null(context) ||
            !is.finite(candidate_index) ||
            candidate_index < 1L ||
            candidate_index > length(previews)
          ) {
            return(invisible(FALSE))
          }

          chosen <- previews[[candidate_index]]

          dir.create(
            context$out_dir,
            recursive = TRUE,
            showWarnings = FALSE
          )

          copied <- c(
            file.copy(
              chosen$basin_tif,
              file.path(context$out_dir, 'basin.tif'),
              overwrite = TRUE
            ),
            file.copy(
              chosen$basin_gpkg,
              file.path(context$out_dir, 'basin.gpkg'),
              overwrite = TRUE
            )
          )

          if (!all(copied)) {
            stop(
              'No fue posible consolidar la alternativa seleccionada.'
            )
          }

          outlet_target <- file.path(
            context$out_dir,
            'outlet.gpkg'
          )

          if (file.exists(outlet_target)) {
            unlink(outlet_target, force = TRUE)
          }

          outlet_sf <- chosen$outlet_sf
          outlet_sf$NAME <- context$nombre
          outlet_sf$BLOCK_ID <- context$block_id

          sf::st_write(
            outlet_sf,
            outlet_target,
            layer = 'outlet',
            quiet = TRUE
          )

          basin_result(chosen$basin_sf)
          outlet_result(outlet_sf)
          output_folder(context$out_dir)
          block_id_result(context$block_id)
          basin_source_result('delineated')
          basin_label_result(context$nombre)

          writeLines(
            c(
              paste(
                'Completed:',
                format(
                  Sys.time(),
                  '%Y-%m-%d %H:%M:%S'
                )
              ),
              paste('Block:', context$block_id),
              paste('Ambiguity reason:', context$reason),
              paste('Selected option:', candidate_index),
              paste('Area km2:', sprintf('%.3f', chosen$area_km2)),
              paste('Snap mode:', chosen$snap$snap_mode),
              paste('Snap distance m:', sprintf('%.1f', chosen$snap$snap_distance_m)),
              paste('Trace cells:', round(chosen$trace_cells)),
              paste('Trace engine:', chosen$trace_engine),
              'Status: OK'
            ),
            file.path(
              context$out_dir,
              'COMPLETADO.txt'
            )
          )

          bb <- sf::st_bbox(
            chosen$basin_map
          )

          leaflet::leafletProxy(
            'mapa',
            session = session
          ) |>
            leaflet::clearGroup('Cuencas candidatas') |>
            leaflet::clearGroup('Outlets candidatos') |>
            leaflet::clearGroup('Punto') |>
            leaflet::clearGroup('Cuenca delimitada') |>
            leaflet::addPolygons(
              data = chosen$basin_map,
              group = 'Cuenca delimitada',
              color = '#D50000',
              weight = 3,
              opacity = 1,
              fillColor = '#FF5252',
              fillOpacity = 0.22
            ) |>
            leaflet::addCircleMarkers(
              lng = chosen$snap$outlet_lon,
              lat = chosen$snap$outlet_lat,
              group = 'Punto',
              radius = 8,
              color = '#B71C1C',
              fillColor = '#EF5350',
              fillOpacity = 1,
              weight = 3,
              popup = 'Outlet seleccionado'
            ) |>
            leaflet::fitBounds(
              bb['xmin'],
              bb['ymin'],
              bb['xmax'],
              bb['ymax']
            )

          shiny::showNotification(
            paste0(
              'Opcion ',
              candidate_index,
              ' seleccionada · ',
              format(
                round(chosen$area_km2, 1),
                big.mark = ',',
                scientific = FALSE,
                trim = TRUE
              ),
              ' km².'
            ),
            type = 'message',
            duration = 6
          )

          clear_ambiguity_preview(
            clear_map = FALSE,
            remove_files = TRUE
          )

          gc()
          invisible(TRUE)
        }



        # ====================================================
        # EXPORTACION DE LA CUENCA ACTIVA
        # ====================================================

        output$export_controls <- shiny::renderUI({

          basin_now <- basin_result()


          if (
            is.null(
              basin_now
            ) ||
            !inherits(
              basin_now,
              "sf"
            ) ||
            nrow(
              basin_now
            ) < 1L
          ) {
            return(
              NULL
            )
          }


          shiny::div(
            class = "delimitacion-export",

            shiny::tags$strong(
              "Exportar cuenca"
            ),

            shiny::div(
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
              ),
              label = "Formato",
              choices = c(
                "GeoPackage (.gpkg)" = "gpkg",
                "KML (.kml)" = "kml",
                "KMZ (.kmz)" = "kmz",
                "Shapefile (.zip)" = "shp_zip"
              ),
              selected = "gpkg"
            ),

            shiny::downloadButton(
              session$ns(
                "descargar_cuenca"
              ),
              "Descargar cuenca",
              class = "btn-primary"
            )
          )
        })


        output$descargar_cuenca <- shiny::downloadHandler(

          filename = function() {

            format_value <- input$formato_exportacion


            if (
              is.null(
                format_value
              ) ||
              !format_value %in%
                c(
                  "gpkg",
                  "kml",
                  "kmz",
                  "shp_zip"
                )
            ) {
              format_value <- "gpkg"
            }


            crs_mode <- input$crs_exportacion


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


            extension <- switch(
              format_value,
              gpkg = ".gpkg",
              kml = ".kml",
              kmz = ".kmz",
              shp_zip = "_shp.zip"
            )


            paste0(
              stem,
              crs_suffix,
              extension
            )
          },

          content = function(file) {

            basin_now <- shiny::isolate(
              basin_result()
            )


            if (
              is.null(
                basin_now
              ) ||
              !inherits(
                basin_now,
                "sf"
              ) ||
              nrow(
                basin_now
              ) < 1L
            ) {
              stop(
                "No hay una cuenca activa para descargar."
              )
            }


            format_value <- shiny::isolate(
              input$formato_exportacion
            )


            if (
              is.null(
                format_value
              ) ||
              !format_value %in%
                c(
                  "gpkg",
                  "kml",
                  "kmz",
                  "shp_zip"
                )
            ) {
              format_value <- "gpkg"
            }


            crs_mode <- shiny::isolate(
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
            )
          }
        )


        shiny::outputOptions(
          output,
          "descargar_cuenca",
          suspendWhenHidden = FALSE
        )


        # ====================================================
        # ORIGEN DE CUENCA
        # ====================================================

        output$origin_controls <- shiny::renderUI({

          source_mode <- input$origen_cuenca


          if (
            is.null(
              source_mode
            ) ||
            identical(
              source_mode,
              "delineated"
            )
          ) {

            return(
              shiny::tagList(

                shiny::radioButtons(
                  session$ns(
                    "punto_metodo"
                  ),
                  label = "Punto de salida",
                  choices = c(
                    "Clic en mapa" = "mapa",
                    "Geografico decimal" = "decimal",
                    "UTM" = "utm",
                    "Grados, minutos y segundos" = "dms"
                  ),
                  selected = "mapa"
                ),

                shiny::uiOutput(
                  session$ns(
                    "coord_inputs"
                  )
                ),

                shiny::div(
                  class = "delimitacion-actions",

                  shiny::actionButton(
                    session$ns(
                      "delimitar"
                    ),
                    "Delimitar",
                    class = "btn-success"
                  ),

                  shiny::actionButton(
                    session$ns(
                      "limpiar"
                    ),
                    "Limpiar"
                  )
                )
              )
            )
          }


          shiny::tagList(

            shiny::fileInput(
              session$ns(
                "archivo_cuenca"
              ),
              label = "Archivo de cuenca",
              multiple = TRUE,
              accept = c(
                ".kml",
                ".kmz",
                ".gpkg",
                ".zip",
                ".shp",
                ".shx",
                ".dbf",
                ".prj"
              ),
              buttonLabel = "Seleccionar",
              placeholder = "KML, KMZ, GPKG, ZIP o SHP+SHX+DBF+PRJ"
            ),

            shiny::div(
              class = "coord-note",
              paste0(
                "Para Shapefile puedes cargar un ZIP o seleccionar a la vez ",
                "SHP + SHX + DBF + PRJ. Si el archivo contiene varias ",
                "geometrias, podras elegir una sin unirlas automaticamente."
              )
            ),

            shiny::uiOutput(
              session$ns(
                "import_options"
              )
            )
          )
        })


        output$import_options <- shiny::renderUI({

          candidates <- imported_candidates()


          if (is.null(
            candidates
          )) {
            return(
              shiny::div(
                class = "coord-note",
                "Carga un archivo espacial para continuar."
              )
            )
          }


          n_candidates <- length(
            candidates
          )


          if (n_candidates < 1L) {
            return(NULL)
          }


          choices <- stats::setNames(
            as.character(
              seq_len(
                n_candidates
              )
            ),
            vapply(
              candidates,
              function(z) z$label,
              character(1)
            )
          )


          shiny::tagList(

            shiny::selectInput(
              session$ns(
                "import_candidate"
              ),
              label = if (n_candidates == 1L) {
                "Geometria detectada"
              } else {
                paste0(
                  "Geometria a usar (",
                  n_candidates,
                  " disponibles)"
                )
              },
              choices = choices,
              selected = "1"
            ),

            shiny::div(
              class = "delimitacion-actions",

              shiny::actionButton(
                session$ns(
                  "usar_cuenca_importada"
                ),
                "Usar cuenca",
                class = "btn-success"
              ),

              shiny::actionButton(
                session$ns(
                  "limpiar"
                ),
                "Limpiar"
              )
            )
          )
        })


        # ====================================================
        # ENTRADA DE COORDENADAS
        # ====================================================

        output$coord_inputs <- shiny::renderUI({

          method <- input$punto_metodo


          if (
            is.null(
              method
            ) ||
            identical(
              method,
              "mapa"
            )
          ) {

            return(
              shiny::div(
                class = "coord-note",
                "Haz clic directamente sobre el mapa para definir el punto."
              )
            )
          }


          if (identical(
            method,
            "decimal"
          )) {

            return(
              shiny::tagList(

                shiny::div(
                  class = "coord-grid-2",

                  shiny::numericInput(
                    session$ns(
                      "decimal_lon"
                    ),
                    "Longitud",
                    value = -75,
                    step = 0.000001
                  ),

                  shiny::numericInput(
                    session$ns(
                      "decimal_lat"
                    ),
                    "Latitud",
                    value = -9.5,
                    step = 0.000001
                  )
                ),

                shiny::actionButton(
                  session$ns(
                    "usar_coordenadas"
                  ),
                  "Ubicar coordenadas",
                  class = "btn-primary",
                  width = "100%"
                )
              )
            )
          }


          if (identical(
            method,
            "utm"
          )) {

            return(
              shiny::tagList(

                shiny::div(
                  class = "coord-grid-2",

                  shiny::numericInput(
                    session$ns(
                      "utm_easting"
                    ),
                    "Este (m)",
                    value = 500000,
                    step = 1
                  ),

                  shiny::numericInput(
                    session$ns(
                      "utm_northing"
                    ),
                    "Norte (m)",
                    value = 8950000,
                    step = 1
                  )
                ),

                shiny::div(
                  class = "coord-grid-2",

                  shiny::numericInput(
                    session$ns(
                      "utm_zone"
                    ),
                    "Zona UTM",
                    value = 18,
                    min = 1,
                    max = 60,
                    step = 1
                  ),

                  shiny::selectInput(
                    session$ns(
                      "utm_hemisphere"
                    ),
                    "Hemisferio",
                    choices = c(
                      "Sur" = "S",
                      "Norte" = "N"
                    ),
                    selected = "S"
                  )
                ),

                shiny::actionButton(
                  session$ns(
                    "usar_coordenadas"
                  ),
                  "Ubicar coordenadas",
                  class = "btn-primary",
                  width = "100%"
                )
              )
            )
          }


          if (identical(
            method,
            "dms"
          )) {

            return(
              shiny::tagList(

                shiny::tags$strong(
                  "Longitud"
                ),

                shiny::div(
                  class = "coord-grid-3",

                  shiny::numericInput(
                    session$ns(
                      "dms_lon_deg"
                    ),
                    "Grados",
                    value = 75,
                    min = 0,
                    max = 180,
                    step = 1
                  ),

                  shiny::numericInput(
                    session$ns(
                      "dms_lon_min"
                    ),
                    "Minutos",
                    value = 0,
                    min = 0,
                    max = 59.999999,
                    step = 1
                  ),

                  shiny::numericInput(
                    session$ns(
                      "dms_lon_sec"
                    ),
                    "Segundos",
                    value = 0,
                    min = 0,
                    max = 59.999999,
                    step = 0.01
                  )
                ),

                shiny::selectInput(
                  session$ns(
                    "dms_lon_hemi"
                  ),
                  "Hemisferio longitud",
                  choices = c(
                    "Oeste (W)" = "W",
                    "Este (E)" = "E"
                  ),
                  selected = "W"
                ),

                shiny::tags$strong(
                  "Latitud"
                ),

                shiny::div(
                  class = "coord-grid-3",

                  shiny::numericInput(
                    session$ns(
                      "dms_lat_deg"
                    ),
                    "Grados",
                    value = 9,
                    min = 0,
                    max = 90,
                    step = 1
                  ),

                  shiny::numericInput(
                    session$ns(
                      "dms_lat_min"
                    ),
                    "Minutos",
                    value = 30,
                    min = 0,
                    max = 59.999999,
                    step = 1
                  ),

                  shiny::numericInput(
                    session$ns(
                      "dms_lat_sec"
                    ),
                    "Segundos",
                    value = 0,
                    min = 0,
                    max = 59.999999,
                    step = 0.01
                  )
                ),

                shiny::selectInput(
                  session$ns(
                    "dms_lat_hemi"
                  ),
                  "Hemisferio latitud",
                  choices = c(
                    "Sur (S)" = "S",
                    "Norte (N)" = "N"
                  ),
                  selected = "S"
                ),

                shiny::actionButton(
                  session$ns(
                    "usar_coordenadas"
                  ),
                  "Ubicar coordenadas",
                  class = "btn-primary",
                  width = "100%"
                )
              )
            )
          }


          NULL
        })


        shiny::observeEvent(
          input$archivo_cuenca,
          {

            imported_candidates(
              NULL
            )


            if (dir.exists(
              upload_work_dir
            )) {
              unlink(
                upload_work_dir,
                recursive = TRUE,
                force = TRUE
              )
            }


            tryCatch(
              {

                candidates <- read_uploaded_basin_candidates(
                  upload_df = input$archivo_cuenca,
                  work_dir = upload_work_dir
                )


                imported_candidates(
                  candidates
                )


                shiny::showNotification(
                  paste0(
                    length(
                      candidates
                    ),
                    " geometria(s) de cuenca detectada(s)."
                  ),
                  type = "message",
                  duration = 4
                )

              },
              error = function(e) {

                imported_candidates(
                  NULL
                )

                shiny::showNotification(
                  conditionMessage(
                    e
                  ),
                  type = "error",
                  duration = NULL
                )
              }
            )
          },
          ignoreNULL = TRUE
        )


        set_selected_point <- function(
            lon,
            lat,
            recenter = FALSE
        ) {

          point <- validate_lon_lat(
            lon = lon,
            lat = lat
          )


          lon <- point$lon
          lat <- point$lat


          clear_ambiguity_preview()


          click_value(
            list(
              lon = lon,
              lat = lat
            )
          )


          basin_result(
            NULL
          )


          outlet_result(
            NULL
          )


          block_id_result(
            NULL
          )


          basin_source_result(
            NULL
          )


          basin_label_result(
            NULL
          )


          selected <- find_block_for_click(
            lon,
            lat
          )


          block_selected(
            selected
          )


          proxy <- leaflet::leafletProxy(
            "mapa",
            session = session
          ) |>

            leaflet::clearGroup(
              "Punto"
            ) |>

            leaflet::clearGroup(
              "Cuenca delimitada"
            ) |>

            leaflet::addCircleMarkers(
              lng = lon,
              lat = lat,
              group = "Punto",
              radius = 7,
              color = "#E65100",
              fillColor = "#FF9800",
              fillOpacity = 1,
              weight = 3,
              popup = paste0(
                "Punto seleccionado<br>",
                sprintf(
                  "%.6f, %.6f",
                  lon,
                  lat
                )
              )
            )


          if (isTRUE(
            recenter
          )) {

            proxy <- leaflet::setView(
              proxy,
              lng = lon,
              lat = lat,
              zoom = 11
            )
          }


          if (is.null(
            selected
          )) {

            shiny::showNotification(
              "El punto esta fuera del dominio disponible.",
              type = "warning",
              duration = 4
            )

          } else {

            shiny::showNotification(
              "Punto seleccionado. Pulsa Delimitar.",
              type = "message",
              duration = 2
            )
          }


          invisible(
            selected
          )
        }


        # ====================================================
        # ACTIVAR CUENCA IMPORTADA
        # ====================================================

        shiny::observeEvent(
          input$usar_cuenca_importada,
          {

            clear_ambiguity_preview()


            tryCatch(
              {

                candidates <- imported_candidates()

                if (
                  is.null(
                    candidates
                  ) ||
                  length(
                    candidates
                  ) < 1L
                ) {
                  stop(
                    "Primero carga un archivo espacial valido."
                  )
                }


                idx <- suppressWarnings(
                  as.integer(
                    input$import_candidate
                  )
                )


                if (
                  !is.finite(
                    idx
                  ) ||
                  idx < 1L ||
                  idx > length(
                    candidates
                  )
                ) {
                  stop(
                    "Selecciona una geometria valida."
                  )
                }


                candidate <- candidates[[idx]]
                basin_sf <- candidate$basin


                if (
                  !inherits(
                    basin_sf,
                    "sf"
                  ) ||
                  nrow(
                    basin_sf
                  ) != 1L
                ) {
                  stop(
                    "La geometria seleccionada no es una cuenca valida."
                  )
                }


                basin_sf <- sf::st_make_valid(
                  basin_sf
                )


                click_value(
                  NULL
                )
                block_selected(
                  NULL
                )
                outlet_result(
                  NULL
                )
                block_id_result(
                  NULL
                )


                out <- next_output_folder()

                dir.create(
                  out$path,
                  recursive = TRUE,
                  showWarnings = FALSE
                )


                output_folder(
                  out$path
                )


                basin_file <- file.path(
                  out$path,
                  "basin.gpkg"
                )

                if (file.exists(
                  basin_file
                )) {
                  unlink(
                    basin_file,
                    force = TRUE
                  )
                }


                sf::st_write(
                  basin_sf,
                  basin_file,
                  layer = "basin",
                  quiet = TRUE
                )


                writeLines(
                  c(
                    paste(
                      "Completed:",
                      format(
                        Sys.time(),
                        "%Y-%m-%d %H:%M:%S"
                      )
                    ),
                    "Source: IMPORTED",
                    paste(
                      "Dataset:",
                      candidate$source_name
                    ),
                    paste(
                      "Layer:",
                      candidate$layer_name
                    ),
                    paste(
                      "Feature:",
                      candidate$feature_index
                    ),
                    "Hydrologic outlet: NOT DEFINED",
                    "Hydrologic context: NOT AVAILABLE",
                    "Status: OK"
                  ),
                  file.path(
                    out$path,
                    "IMPORTADO.txt"
                  )
                )


                basin_result(
                  basin_sf
                )
                basin_source_result(
                  "imported"
                )
                basin_label_result(
                  candidate$label
                )


                basin_map <- sf::st_transform(
                  basin_sf,
                  4326
                )

                bb <- sf::st_bbox(
                  basin_map
                )


                leaflet::leafletProxy(
                  "mapa",
                  session = session
                ) |>
                  leaflet::clearGroup(
                    "Punto"
                  ) |>
                  leaflet::clearGroup(
                    "Cuenca delimitada"
                  ) |>
                  leaflet::addPolygons(
                    data = basin_map,
                    group = "Cuenca delimitada",
                    color = "#1565C0",
                    weight = 3,
                    opacity = 1,
                    fillColor = "#42A5F5",
                    fillOpacity = 0.18,
                    popup = paste0(
                      "Cuenca importada<br>",
                      candidate$label
                    )
                  ) |>
                  leaflet::fitBounds(
                    bb["xmin"],
                    bb["ymin"],
                    bb["xmax"],
                    bb["ymax"]
                  )


                shiny::showNotification(
                  paste0(
                    "Cuenca importada: ",
                    candidate$label
                  ),
                  type = "message",
                  duration = 5
                )

              },
              error = function(e) {

                shiny::showNotification(
                  conditionMessage(
                    e
                  ),
                  type = "error",
                  duration = NULL
                )
              }
            )
          }
        )


        # ====================================================
        # MAPA
        # ====================================================

        output$mapa <- leaflet::renderLeaflet({

          leaflet::leaflet(
            options = leaflet::leafletOptions(
              preferCanvas = TRUE,
              minZoom = 3,
              worldCopyJump = FALSE
            )
          ) |>

            leaflet::addTiles(
              urlTemplate = paste0(
                "https://{s}.tile.openstreetmap.org/",
                "{z}/{x}/{y}.png"
              ),
              attribution = "&copy; OpenStreetMap contributors",
              group = "Mapa base",
              options = leaflet::tileOptions(
                noWrap = TRUE
              )
            ) |>

            leaflet::setView(
              lng = -75,
              lat = -9.5,
              zoom = 5
            )
        })


        # ====================================================
        # CLIC
        # ====================================================

        shiny::observeEvent(
          input$mapa_click,
          {

            shiny::req(
              input$mapa_click$lng,
              input$mapa_click$lat
            )


            set_selected_point(
              lon = input$mapa_click$lng,
              lat = input$mapa_click$lat,
              recenter = FALSE
            )
          }
        )


        shiny::observeEvent(
          input$usar_coordenadas,
          {

            tryCatch(
              {

                method <- input$punto_metodo


                if (identical(
                  method,
                  "decimal"
                )) {

                  point <- validate_lon_lat(
                    lon = input$decimal_lon,
                    lat = input$decimal_lat
                  )

                } else if (identical(
                  method,
                  "utm"
                )) {

                  point <- utm_to_lonlat(
                    easting = input$utm_easting,
                    northing = input$utm_northing,
                    zone = input$utm_zone,
                    hemisphere = input$utm_hemisphere
                  )

                } else if (identical(
                  method,
                  "dms"
                )) {

                  point <- dms_pair_to_lonlat(
                    lon_deg = input$dms_lon_deg,
                    lon_min = input$dms_lon_min,
                    lon_sec = input$dms_lon_sec,
                    lon_hemi = input$dms_lon_hemi,
                    lat_deg = input$dms_lat_deg,
                    lat_min = input$dms_lat_min,
                    lat_sec = input$dms_lat_sec,
                    lat_hemi = input$dms_lat_hemi
                  )

                } else {

                  stop(
                    "Selecciona un metodo de coordenadas."
                  )
                }


                set_selected_point(
                  lon = point$lon,
                  lat = point$lat,
                  recenter = TRUE
                )

              },
              error = function(e) {

                shiny::showNotification(
                  conditionMessage(
                    e
                  ),
                  type = "error",
                  duration = NULL
                )
              }
            )
          }
        )


        # ====================================================
        # ERROR
        # ====================================================

        delim_error_handler <- function(e) {

          shiny::showNotification(
            conditionMessage(
              e
            ),
            type = "error",
            duration = NULL
          )


          invisible(
            NULL
          )
        }


        # ====================================================
        # DELIMITAR
        # ====================================================

        shiny::observeEvent(
          input$delimitar,
          {

            tryCatch(
              {

                click <- click_value()


                if (is.null(
                  click
                )) {
                  stop(
                    "Primero haz clic en el mapa."
                  )
                }


                block_row <- block_selected()


                if (is.null(
                  block_row
                )) {
                  stop(
                    "El punto no pertenece al dominio disponible."
                  )
                }


                block_id <- as.character(
                  block_row[["BLOCK_ID"]][1]
                )


                out <- next_output_folder()

                nombre <- out$name
                out_dir <- out$path


                dir.create(
                  out_dir,
                  recursive = TRUE,
                  showWarnings = FALSE
                )


                output_folder(
                  out_dir
                )


                basin_temp <- file.path(
                  TERRA_TEMP,
                  paste0(
                    "BASIN_",
                    nombre
                  )
                )


                if (dir.exists(
                  basin_temp
                )) {
                  unlink(
                    basin_temp,
                    recursive = TRUE,
                    force = TRUE
                  )
                }


                shiny::withProgress(
                  message = "Delimitando cuenca",
                  value = 0,
                  {

                    total_started <- Sys.time()
                    stage_times <- list()

                    shiny::setProgress(
                      value = 0.10,
                      detail = "10% | Cargando indice"
                    )


                    load_started <- Sys.time()


                    load_block_if_needed(
                      block_id = block_id,
                      block_cache = block_cache
                    )


                    stage_times$load_s <- as.numeric(
                      difftime(
                        Sys.time(),
                        load_started,
                        units = "secs"
                      )
                    )


                    grid_template <- block_cache$grid_template


                    shiny::setProgress(
                      value = 0.20,
                      detail = "20% | Ajustando punto de salida"
                    )


                    snap_started <- Sys.time()



                    clear_ambiguity_preview()

                    ambiguity <- find_ambiguity_options(
                      lon = click$lon,
                      lat = click$lat,
                      radius_m = DEFAULT_SNAP_RADIUS_M,
                      grid_template = grid_template,
                      stream_cache = block_cache$stream_cache,
                      stripe_rows = block_cache$stripe_rows,
                      n_stripes = block_cache$n_stripes
                    )

                    if (identical(
                      ambiguity$status,
                      'ambiguous'
                    )) {

                      ambiguity_root <- file.path(
                        TERRA_TEMP,
                        paste0(
                          'AMBIG_',
                          gsub(
                            '[^A-Za-z0-9_]',
                            '_',
                            session$token
                          ),
                          '_',
                          format(
                            Sys.time(),
                            '%Y%m%d%H%M%S'
                          )
                        )
                      )

                      dir.create(
                        ambiguity_root,
                        recursive = TRUE,
                        showWarnings = FALSE
                      )

                      previews <- list()

                      for (ii in seq_along(
                        ambiguity$options
                      )) {

                        shiny::setProgress(
                          value = min(
                            0.90,
                            0.25 +
                              0.60 *
                                ii /
                                length(ambiguity$options)
                          ),
                          detail = paste0(
                            'Delimitando alternativa ',
                            ii,
                            ' de ',
                            length(ambiguity$options)
                          )
                        )

                        preview <- tryCatch(
                          materialize_ambiguity_option(
                            option = ambiguity$options[[ii]],
                            candidate_index = ii,
                            grid_template = grid_template,
                            work_root = ambiguity_root
                          ),
                          error = function(e) {
                            shiny::showNotification(
                              paste0(
                                'Alternativa ',
                                ii,
                                ': ',
                                conditionMessage(e)
                              ),
                              type = 'warning',
                              duration = 8
                            )
                            NULL
                          }
                        )

                        if (!is.null(preview)) {
                          previews[[length(previews) + 1L]] <- preview
                        }
                      }

                      if (length(previews) >= 2L) {

                        ambiguity_previews(previews)

                        ambiguity_context(
                          list(
                            out_dir = out_dir,
                            nombre = nombre,
                            block_id = block_id,
                            work_root = ambiguity_root,
                            reason = ambiguity$reason
                          )
                        )

                        render_ambiguity_map(previews)

                        shiny::setProgress(
                          value = 1,
                          detail = paste0(
                            'Se detectaron ',
                            length(previews),
                            ' cuencas posibles. Selecciona una.'
                          )
                        )

                        shiny::showNotification(
                          paste0(
                            'Se detectaron ',
                            length(previews),
                            ' cuencas hidrologicamente posibles. ',
                            'Selecciona el outlet numerado en el mapa.'
                          ),
                          type = 'warning',
                          duration = 10
                        )

                        return(invisible(NULL))
                      }

                      if (length(previews) == 1L) {
                        snap <- previews[[1]]$snap
                        unlink(
                          ambiguity_root,
                          recursive = TRUE,
                          force = TRUE
                        )
                      } else {
                        unlink(
                          ambiguity_root,
                          recursive = TRUE,
                          force = TRUE
                        )
                        stop(
                          'No fue posible materializar las alternativas detectadas.'
                        )
                      }

                    } else {
                      snap <- ambiguity$options[[1]]
                    }


                    stage_times$snap_s <- as.numeric(
                      difftime(
                        Sys.time(),
                        snap_started,
                        units = "secs"
                      )
                    )


                    leaflet::leafletProxy(
                      "mapa",
                      session = session
                    ) |>

                      leaflet::clearGroup(
                        "Punto"
                      ) |>

                      leaflet::addCircleMarkers(
                        lng = snap$outlet_lon,
                        lat = snap$outlet_lat,
                        group = "Punto",
                        radius = 8,
                        color = "#B71C1C",
                        fillColor = "#EF5350",
                        fillOpacity = 1,
                        weight = 3,
                        popup = "Outlet"
                      )


                    shiny::setProgress(
                      value = 0.30,
                      detail = "30% | Rastreando aguas arriba | 0 celdas | 00:00"
                    )


                    trace_started <- Sys.time()
                    last_progress_update <- -Inf


                    trace <- trace_upstream(
                      cache = block_cache$reverse_cache,
                      outlet_cell = snap$outlet_cell,
                      progress_fun = function(
                          level,
                          n_cells,
                          frontier_size
                      ) {

                        elapsed_s <- as.numeric(
                          difftime(
                            Sys.time(),
                            trace_started,
                            units = "secs"
                          )
                        )


                        if (
                          elapsed_s -
                            last_progress_update >=
                              0.50
                        ) {

                          progress_value <- trace_progress_value(
                            n_cells
                          )


                          progress_percent <- floor(
                            progress_value * 100
                          )


                          shiny::setProgress(
                            value = progress_value,
                            detail = paste0(
                              progress_percent,
                              "% | Rastreando aguas arriba | ",
                              format_cell_count(
                                n_cells
                              ),
                              " | ",
                              format_elapsed(
                                elapsed_s
                              )
                            )
                          )


                          last_progress_update <<- elapsed_s
                        }
                      }
                    )


                    trace_elapsed_s <- as.numeric(
                      difftime(
                        Sys.time(),
                        trace_started,
                        units = "secs"
                      )
                    )


                    stage_times$trace_s <- trace_elapsed_s


                    shiny::setProgress(
                      value = 0.72,
                      detail = paste0(
                        "72% | Rastreo terminado | ",
                        format_cell_count(
                          trace$n_cells
                        ),
                        " | ",
                        format_elapsed(
                          trace_elapsed_s
                        )
                      )
                    )


                    threshold_cells <- block_cache$stream_threshold_cells


                    if (
                      !isTRUE(
                        snap$stream_mask_value >
                          0
                      )
                    ) {
                      stop(
                        "Fallo de seguridad: outlet fuera de la red de cauces."
                      )
                    }


                    if (
                      trace$n_cells <
                        threshold_cells
                    ) {
                      stop(
                        paste0(
                          "FALLO HIDROLOGICO: el outlet pertenece a la red ",
                          "de cauces pero la cuenca obtenida no alcanza el ",
                          "umbral hidrologico minimo. No se dibujara una ",
                          "microcuenca falsa."
                        )
                      )
                    }


                    shiny::setProgress(
                      value = 0.78,
                      detail = "78% | Generando raster de cuenca"
                    )


                    basin_tif <- file.path(
                      out_dir,
                      "basin.tif"
                    )


                    raster_started <- Sys.time()


                    write_basin_raster(
                      cells = trace$cells,
                      template = grid_template,
                      output_file = basin_tif,
                      temp_dir = basin_temp,
                      bbox = trace$bbox
                    )


                    stage_times$raster_s <- as.numeric(
                      difftime(
                        Sys.time(),
                        raster_started,
                        units = "secs"
                      )
                    )


                    trace_engine <- if (
                      !is.null(
                        trace$trace_engine
                      )
                    ) {
                      as.character(
                        trace$trace_engine
                      )
                    } else {
                      "UNKNOWN"
                    }


                    trace_batches <- if (
                      !is.null(
                        trace$n_batches
                      )
                    ) {
                      trace$n_batches
                    } else {
                      NA_integer_
                    }


                    trace_cell_count <- as.double(
                      trace$n_cells
                    )


                    rm(
                      trace
                    )

                    gc()


                    shiny::setProgress(
                      value = 0.88,
                      detail = "88% | Generando poligono"
                    )


                    basin_gpkg <- file.path(
                      out_dir,
                      "basin.gpkg"
                    )


                    polygon_started <- Sys.time()


                    basin_sf <- polygonize_basin(
                      basin_tif,
                      basin_gpkg,
                      expected_cells = trace_cell_count
                    )


                    stage_times$polygon_s <- as.numeric(
                      difftime(
                        Sys.time(),
                        polygon_started,
                        units = "secs"
                      )
                    )


                    basin_result(
                      basin_sf
                    )


                    basin_source_result(
                      "delineated"
                    )


                    basin_label_result(
                      nombre
                    )


                    outlet_sf <- sf::st_sf(
                      NAME = nombre,
                      BLOCK_ID = block_id,
                      SNAP_MODE = snap$snap_mode,
                      SNAP_DISTANCE_M = snap$snap_distance_m,
                      geometry = sf::st_sfc(
                        sf::st_point(
                          c(
                            snap$outlet_lon,
                            snap$outlet_lat
                          )
                        ),
                        crs = 4326
                      )
                    )


                    outlet_file <- file.path(
                      out_dir,
                      "outlet.gpkg"
                    )


                    if (file.exists(
                      outlet_file
                    )) {
                      unlink(
                        outlet_file,
                        force = TRUE
                      )
                    }


                    sf::st_write(
                      outlet_sf,
                      outlet_file,
                      layer = "outlet",
                      quiet = TRUE
                    )


                    outlet_result(
                      outlet_sf
                    )


                    block_id_result(
                      block_id
                    )


                    shiny::setProgress(
                      value = 0.96,
                      detail = "96% | Actualizando mapa"
                    )


                    basin_map <- basin_sf |>
                      sf::st_transform(
                        3857
                      ) |>
                      sf::st_simplify(
                        dTolerance = MAP_SIMPLIFY_M,
                        preserveTopology = TRUE
                      ) |>
                      sf::st_transform(
                        4326
                      )


                    bb <- sf::st_bbox(
                      basin_map
                    )


                    leaflet::leafletProxy(
                      "mapa",
                      session = session
                    ) |>

                      leaflet::clearGroup(
                        "Cuenca delimitada"
                      ) |>

                      leaflet::addPolygons(
                        data = basin_map,
                        group = "Cuenca delimitada",
                        color = "#D50000",
                        weight = 3,
                        opacity = 1,
                        fillColor = "#FF5252",
                        fillOpacity = 0.22
                      ) |>

                      leaflet::fitBounds(
                        bb["xmin"],
                        bb["ymin"],
                        bb["xmax"],
                        bb["ymax"]
                      )


                    stage_times$total_s <- as.numeric(
                      difftime(
                        Sys.time(),
                        total_started,
                        units = "secs"
                      )
                    )


                    timing_table <- data.frame(
                      STAGE = c(
                        "load",
                        "snap",
                        "trace",
                        "raster",
                        "polygon",
                        "total"
                      ),
                      SECONDS = c(
                        stage_times$load_s,
                        stage_times$snap_s,
                        stage_times$trace_s,
                        stage_times$raster_s,
                        stage_times$polygon_s,
                        stage_times$total_s
                      ),
                      stringsAsFactors = FALSE
                    )


                    write.csv(
                      timing_table,
                      file.path(
                        out_dir,
                        "timings.csv"
                      ),
                      row.names = FALSE
                    )


                    writeLines(
                      c(
                        paste(
                          "Completed:",
                          format(
                            Sys.time(),
                            "%Y-%m-%d %H:%M:%S"
                          )
                        ),
                        paste(
                          "Block:",
                          block_id
                        ),
                        "Runtime: FABDEM_Watershed_Runtime/core",
                        paste(
                          "Trace engine:",
                          trace_engine
                        ),
                        paste(
                          "Trace batches:",
                          trace_batches
                        ),
                        paste(
                          "Load seconds:",
                          sprintf(
                            "%.2f",
                            stage_times$load_s
                          )
                        ),
                        paste(
                          "Trace seconds:",
                          sprintf(
                            "%.2f",
                            stage_times$trace_s
                          )
                        ),
                        paste(
                          "Raster seconds:",
                          sprintf(
                            "%.2f",
                            stage_times$raster_s
                          )
                        ),
                        paste(
                          "Polygon seconds:",
                          sprintf(
                            "%.2f",
                            stage_times$polygon_s
                          )
                        ),
                        paste(
                          "Total seconds:",
                          sprintf(
                            "%.2f",
                            stage_times$total_s
                          )
                        ),
                        "DEM used: NO",
                        "Hydrology recalculated: NO",
                        "Status: OK"
                      ),
                      file.path(
                        out_dir,
                        "COMPLETADO.txt"
                      )
                    )


                    unlink(
                      basin_temp,
                      recursive = TRUE,
                      force = TRUE
                    )


                    shiny::setProgress(
                      value = 1,
                      detail = "100% | Listo"
                    )


                    shiny::showNotification(
                      paste0(
                        "Cuenca delimitada en ",
                        format_elapsed(
                          stage_times$total_s
                        ),
                        " | trace ",
                        format_elapsed(
                          stage_times$trace_s
                        ),
                        " | ",
                        trace_engine
                      ),
                      type = "message",
                      duration = 8
                    )


                    gc()
                  }
                )

              },
              error = delim_error_handler
            )
          }
        )



        # ====================================================
        # SELECCION DE CUENCA AMBIGUA
        # ====================================================

        shiny::observeEvent(
          input$mapa_marker_click,
          {
            marker <- input$mapa_marker_click

            if (
              is.null(marker$id) ||
              !grepl(
                '^AMBIG_[123]$',
                as.character(marker$id)
              )
            ) {
              return()
            }

            idx <- as.integer(
              sub(
                '^AMBIG_',
                '',
                as.character(marker$id)
              )
            )

            tryCatch(
              activate_ambiguity_candidate(idx),
              error = delim_error_handler
            )
          },
          ignoreNULL = TRUE
        )


        lapply(
          seq_len(AMBIGUITY_MAX_OPTIONS),
          function(ii) {
            local({
              idx <- ii
              input_id <- paste0(
                'elegir_candidato_',
                idx
              )

              shiny::observeEvent(
                input[[input_id]],
                {
                  tryCatch(
                    activate_ambiguity_candidate(idx),
                    error = delim_error_handler
                  )
                },
                ignoreInit = TRUE
              )
            })
          }
        )



        # ====================================================
        # LIMPIAR
        # ====================================================

        shiny::observeEvent(
          input$limpiar,
          {

            clear_ambiguity_preview()


            click_value(
              NULL
            )


            block_selected(
              NULL
            )


            basin_result(
              NULL
            )


            outlet_result(
              NULL
            )


            output_folder(
              NULL
            )


            block_id_result(
              NULL
            )


            basin_source_result(
              NULL
            )


            basin_label_result(
              NULL
            )


            leaflet::leafletProxy(
              "mapa",
              session = session
            ) |>

              leaflet::clearGroup(
                "Punto"
              ) |>

              leaflet::clearGroup(
                "Cuenca delimitada"
              )
          }
        )


        list(
          basin = shiny::reactive(
            basin_result()
          ),
          outlet = shiny::reactive(
            outlet_result()
          ),
          folder = shiny::reactive(
            output_folder()
          ),
          block_id = shiny::reactive(
            block_id_result()
          ),
          basin_source = shiny::reactive(
            basin_source_result()
          ),
          basin_label = shiny::reactive(
            basin_label_result()
          ),
          hydro_context = shiny::reactive({

            active_block_id <- block_id_result()

            if (
              is.null(active_block_id) ||
              is.null(block_cache$block_id) ||
              !identical(
                as.character(active_block_id),
                as.character(block_cache$block_id)
              ) ||
              is.null(block_cache$grid_template) ||
              is.null(block_cache$reverse_cache) ||
              is.null(block_cache$stream_cache)
            ) {
              return(NULL)
            }

            list(
              block_id = as.character(block_cache$block_id),
              grid_template = block_cache$grid_template,
              reverse_cache = block_cache$reverse_cache,
              stream_cache = block_cache$stream_cache,
              stripe_rows = block_cache$stripe_rows,
              n_stripes = block_cache$n_stripes,
              stream_threshold_cells = block_cache$stream_threshold_cells,
              stream_threshold_km2 = block_cache$stream_threshold_km2
            )
          })
        )
      }
    )
  }


  list(
    ui = ui,
    server = server
  )
})

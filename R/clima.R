# ============================================================
# R/clima.R
#
# FABDEM Watershed Explorer
# SUBMODULO: CLIMA
# v2: ruta corregida clima_superficie + fondo departamental + exportacion | arquitectura independiente del orden de carga
# ============================================================

# v2: helper geodésico local para escala
# v3: paleta categórica ampliada
# v4: contexto departamental desde layers/fondo_mapa.gpkg
clima <- local({

  # CONFIGURACION LAZY / INDEPENDIENTE DEL ORDEN DE CARGA
  # ------------------------------------------------------
  # Ninguna ruta de config se evalua al sourcear este archivo.
  # LAYERS_DIR, LAYER_METADATA_XLSX y FONDO_MAPA_GPKG se
  # resuelven solo cuando sus funciones son invocadas, despues
  # de que Shiny haya terminado de cargar todos los archivos R/.


  CLIMA_STEM <- "clima_normalizada_disuelto"


  # ==========================================================
  # 1. UTILIDADES
  # ==========================================================

  file_nonempty_local <- function(path) {
    if (
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      !file.exists(path)
    ) {
      return(FALSE)
    }

    info <- file.info(path)
    isTRUE(
      is.finite(info$size) &&
        info$size > 0
    )
  }


  safe_layers_path <- function(relative_path) {
    relative_path <- gsub(
      "\\\\",
      "/",
      as.character(relative_path)
    )

    root <- normalizePath(
      LAYERS_DIR,
      winslash = "/",
      mustWork = TRUE
    )

    candidate <- normalizePath(
      file.path(
        LAYERS_DIR,
        relative_path
      ),
      winslash = "/",
      mustWork = FALSE
    )

    root_prefix <- paste0(
      tolower(root),
      "/"
    )

    if (!startsWith(
      tolower(candidate),
      root_prefix
    )) {
      stop(
        paste0(
          "Ruta de tesela fuera de LAYERS_DIR:\n",
          relative_path
        )
      )
    }

    candidate
  }


  local_utm_epsg <- function(basin_4326) {
    center <- suppressWarnings(
      sf::st_centroid(
        sf::st_union(
          sf::st_geometry(
            basin_4326
          )
        )
      )
    )

    xy <- sf::st_coordinates(center)[1, ]

    utm_epsg_point(
      lon = xy[1],
      lat = xy[2]
    )
  }


  stable_code_color <- function(code) {
    code <- trimws(
      as.character(code)
    )

    if (
      is.na(code) ||
      !nzchar(code)
    ) {
      return("#B0B0B0")
    }

    if (identical(
      toupper(code),
      "SIN_CODIGO"
    )) {
      return("#B39DDB")
    }

    ints <- utf8ToInt(code)

    if (length(ints) == 0L) {
      return("#9E9E9E")
    }

    idx <- seq_along(ints)

    hash1 <- sum(
      ints * (
        idx * 37L + 17L
      )
    )

    hash2 <- sum(
      ints * (
        idx * 19L + 11L
      )
    )

    hash3 <- sum(
      ints * (
        idx * 13L + 29L
      )
    )

    hue_levels <- seq(
      0,
      345,
      by = 15
    )

    chroma_levels <- c(
      45,
      60,
      75
    )

    luminance_levels <- c(
      40,
      52,
      64,
      76
    )

    hue <- hue_levels[
      (hash1 %% length(hue_levels)) + 1L
    ]

    chroma <- chroma_levels[
      (hash2 %% length(chroma_levels)) + 1L
    ]

    luminance <- luminance_levels[
      (hash3 %% length(luminance_levels)) + 1L
    ]

    grDevices::hcl(
      h = hue,
      c = chroma,
      l = luminance,
      fixup = TRUE
    )
  }


  clima_colors <- function(codes) {
    codes <- unique(
      trimws(
        as.character(codes)
      )
    )

    codes <- codes[
      !is.na(codes) &
      nzchar(codes)
    ]

    cols <- vapply(
      codes,
      stable_code_color,
      character(1)
    )

    if (length(cols) > 1L) {

      ord <- order(
        grDevices::col2rgb(cols)[1, ] +
          10 * grDevices::col2rgb(cols)[2, ] +
          100 * grDevices::col2rgb(cols)[3, ]
      )

      cols <- cols[ord]
      codes <- codes[ord]
    }

    stats::setNames(
      cols,
      codes
    )
  }


  # ==========================================================
  # 2. METADATA
  # ==========================================================

  metadata_cache <- new.env(
    parent = emptyenv()
  )


  read_clima_metadata <- function() {
    if (exists(
      "clima",
      envir = metadata_cache,
      inherits = FALSE
    )) {
      return(
        get(
          "clima",
          envir = metadata_cache,
          inherits = FALSE
        )
      )
    }

    if (!file_nonempty_local(
      LAYER_METADATA_XLSX
    )) {
      stop(
        paste0(
          "No existe la metadata normalizada:\n",
          LAYER_METADATA_XLSX
        )
      )
    }

    meta <- as.data.frame(
      readxl::read_excel(
        LAYER_METADATA_XLSX
      ),
      stringsAsFactors = FALSE
    )

    required <- c(
      "GPKG",
      "CODIGO",
      "NOMBRE"
    )

    missing <- required[
      !required %in% names(meta)
    ]

    if (length(missing) > 0L) {
      stop(
        paste0(
          "La metadata no contiene: ",
          paste(
            missing,
            collapse = ", "
          )
        )
      )
    }

    meta$GPKG <- trimws(
      as.character(meta$GPKG)
    )

    meta$CODIGO <- trimws(
      as.character(meta$CODIGO)
    )

    meta$NOMBRE <- trimws(
      as.character(meta$NOMBRE)
    )

    meta <- meta[
      meta$GPKG == CLIMA_STEM &
        !is.na(meta$CODIGO) &
        nzchar(meta$CODIGO),
      c(
        "CODIGO",
        "NOMBRE"
      ),
      drop = FALSE
    ]

    meta <- meta[
      !duplicated(meta$CODIGO),
      ,
      drop = FALSE
    ]

    if (nrow(meta) == 0L) {
      stop(
        paste0(
          "No hay metadata para ",
          CLIMA_STEM,
          "."
        )
      )
    }

    assign(
      "clima",
      meta,
      envir = metadata_cache
    )

    meta
  }


  # ==========================================================
  # 3. CARGA ESPACIAL TESelADA
  # ==========================================================

  clima_tiled_dir <- function() {
    file.path(
      LAYERS_DIR,
      "clima_superficie",
      paste0(
        CLIMA_STEM,
        "__tiles"
      )
    )
  }


  clima_original_file <- function() {
    file.path(
      LAYERS_DIR,
      "clima_superficie",
      paste0(
        CLIMA_STEM,
        ".gpkg"
      )
    )
  }


  clima_index <- function() {
    tiled_dir <- clima_tiled_dir()
    index_file <- file.path(
      tiled_dir,
      "index.gpkg"
    )
    ready_file <- file.path(
      tiled_dir,
      "TILING_READY.txt"
    )

    if (
      file_nonempty_local(index_file) &&
      file_nonempty_local(ready_file)
    ) {
      idx <- sf::st_read(
        index_file,
        layer = "tiles",
        quiet = TRUE
      )

      if (!"RELATIVE_PATH" %in% names(idx)) {
        stop(
          "El index.gpkg de Clima no contiene RELATIVE_PATH."
        )
      }

      return(idx)
    }

    NULL
  }


  read_clima_for_basin <- function(basin) {
    basin <- sf::st_make_valid(basin)
    basin <- basin[
      !sf::st_is_empty(basin),
      ,
      drop = FALSE
    ]

    if (nrow(basin) == 0L) {
      stop("La cuenca activa está vacía.")
    }

    idx <- clima_index()
    pieces <- list()
    source_mode <- NULL
    selected_tiles <- character(0)

    if (!is.null(idx)) {
      basin_idx <- sf::st_transform(
        basin,
        sf::st_crs(idx)
      )

      hits <- lengths(
        sf::st_intersects(
          idx,
          basin_idx
        )
      ) > 0L

      idx_hit <- idx[
        hits,
        ,
        drop = FALSE
      ]

      if (nrow(idx_hit) == 0L) {
        stop(
          "Ninguna tesela de Clima intersecta la cuenca activa."
        )
      }

      selected_tiles <- vapply(
        as.character(
          idx_hit$RELATIVE_PATH
        ),
        safe_layers_path,
        character(1)
      )

      missing_tiles <- selected_tiles[
        !vapply(
          selected_tiles,
          file_nonempty_local,
          logical(1)
        )
      ]

      if (length(missing_tiles) > 0L) {
        stop(
          paste0(
            "Falta una tesela de Clima:\n",
            missing_tiles[1]
          )
        )
      }

      for (i in seq_along(selected_tiles)) {
        g <- sf::st_read(
          selected_tiles[i],
          layer = "data",
          quiet = TRUE
        )

        if (!"CODIGO" %in% names(g)) {
          stop(
            paste0(
              "La tesela no contiene CODIGO:\n",
              selected_tiles[i]
            )
          )
        }

        g <- g[
          ,
          "CODIGO",
          drop = FALSE
        ]

        pieces[[length(pieces) + 1L]] <- g
      }

      source_mode <- "tiles"

    } else {
      original <- clima_original_file()

      if (!file_nonempty_local(original)) {
        stop(
          paste0(
            "No se encontró Clima teselada ni el GPKG original.\n",
            "Esperado:\n",
            clima_tiled_dir(),
            "\nó\n",
            original
          )
        )
      }

      layers <- sf::st_layers(original)$name

      if (length(layers) < 1L) {
        stop("El GPKG de Clima no contiene capas vectoriales.")
      }

      g <- sf::st_read(
        original,
        layer = layers[1],
        quiet = TRUE
      )

      if (!"CODIGO" %in% names(g)) {
        stop("El GPKG de Clima no contiene CODIGO.")
      }

      pieces[[1L]] <- g[
        ,
        "CODIGO",
        drop = FALSE
      ]

      source_mode <- "original"
      selected_tiles <- original
    }

    crs_ref <- sf::st_crs(
      pieces[[1L]]
    )

    if (is.na(crs_ref)) {
      stop("La capa de Clima no tiene CRS válido.")
    }

    pieces <- lapply(
      pieces,
      function(g) {
        if (!isTRUE(
          sf::st_crs(g) == crs_ref
        )) {
          g <- sf::st_transform(
            g,
            crs_ref
          )
        }
        g
      }
    )

    clima_all <- do.call(
      rbind,
      pieces
    )

    clima_all <- sf::st_make_valid(
      clima_all
    )

    clima_all <- clima_all[
      !sf::st_is_empty(clima_all),
      ,
      drop = FALSE
    ]

    basin_local <- sf::st_transform(
      basin,
      crs_ref
    )

    basin_union <- sf::st_union(
      sf::st_geometry(
        basin_local
      )
    )

    keep <- lengths(
      sf::st_intersects(
        clima_all,
        basin_union
      )
    ) > 0L

    clima_all <- clima_all[
      keep,
      ,
      drop = FALSE
    ]

    if (nrow(clima_all) == 0L) {
      stop("Clima no contiene unidades dentro de la cuenca.")
    }

    clipped <- suppressWarnings(
      sf::st_intersection(
        clima_all,
        basin_union
      )
    )

    clipped <- sf::st_make_valid(
      clipped
    )

    clipped <- clipped[
      !sf::st_is_empty(clipped),
      ,
      drop = FALSE
    ]

    geom_type <- as.character(
      sf::st_geometry_type(
        clipped,
        by_geometry = TRUE
      )
    )

    keep_poly <- geom_type %in% c(
      "POLYGON",
      "MULTIPOLYGON"
    )

    clipped <- clipped[
      keep_poly,
      ,
      drop = FALSE
    ]

    if (nrow(clipped) == 0L) {
      stop("La intersección clima no produjo polígonos válidos.")
    }

    clipped$CODIGO <- trimws(
      as.character(
        clipped$CODIGO
      )
    )

    clipped <- clipped[
      !is.na(clipped$CODIGO) &
        nzchar(clipped$CODIGO),
      ,
      drop = FALSE
    ]

    list(
      clima = clipped,
      source_mode = source_mode,
      selected_tiles = selected_tiles
    )
  }


  # ==========================================================
  # 4. TABLA DE UNIDADES
  # ==========================================================

  build_clima_table <- function(
      clima,
      basin
  ) {
    basin_4326 <- sf::st_transform(
      basin,
      4326
    )

    epsg <- local_utm_epsg(
      basin_4326
    )

    clima_utm <- sf::st_transform(
      clima,
      epsg
    )

    basin_utm <- sf::st_transform(
      basin,
      epsg
    )

    basin_area_km2 <- as.numeric(
      sf::st_area(
        sf::st_union(
          sf::st_geometry(
            basin_utm
          )
        )
      )
    ) / 1e6

    piece_area_km2 <- as.numeric(
      sf::st_area(
        clima_utm
      )
    ) / 1e6

    area_by_code <- tapply(
      piece_area_km2,
      clima$CODIGO,
      sum,
      na.rm = TRUE
    )

    tab <- data.frame(
      CODIGO = names(area_by_code),
      AREA_KM2 = as.numeric(area_by_code),
      stringsAsFactors = FALSE
    )

    meta <- read_clima_metadata()

    match_id <- match(
      tab$CODIGO,
      meta$CODIGO
    )

    tab$DESCRIPCION <- meta$NOMBRE[
      match_id
    ]

    tab$DESCRIPCION[
      is.na(tab$DESCRIPCION) |
        !nzchar(tab$DESCRIPCION)
    ] <- "Sin descripción en metadata"

    tab$CUENCA_PCT <- if (
      is.finite(basin_area_km2) &&
      basin_area_km2 > 0
    ) {
      100 * tab$AREA_KM2 / basin_area_km2
    } else {
      NA_real_
    }

    tab <- tab[
      order(
        tab$AREA_KM2,
        decreasing = TRUE
      ),
      c(
        "CODIGO",
        "DESCRIPCION",
        "AREA_KM2",
        "CUENCA_PCT"
      ),
      drop = FALSE
    ]

    rownames(tab) <- NULL

    list(
      table = tab,
      basin_area_km2 = basin_area_km2,
      epsg = epsg
    )
  }


  # ==========================================================
  # 5. CONTEXTO DEPARTAMENTAL PARA EL MAPA
  # ==========================================================

  department_cache <- new.env(
    parent = emptyenv()
  )


  read_department_background <- function() {

    cache_key <- "departments"

    if (exists(
      cache_key,
      envir = department_cache,
      inherits = FALSE
    )) {
      return(
        get(
          cache_key,
          envir = department_cache,
          inherits = FALSE
        )
      )
    }

    if (!file.exists(
      FONDO_MAPA_GPKG
    )) {
      stop(
        paste0(
          "No se encontró el fondo departamental:\n",
          FONDO_MAPA_GPKG
        )
      )
    }

    layers <- sf::st_layers(
      FONDO_MAPA_GPKG
    )$name

    if (length(
      layers
    ) < 1L) {
      stop(
        "fondo_mapa.gpkg no contiene capas legibles."
      )
    }

    departments <- sf::st_read(
      FONDO_MAPA_GPKG,
      layer = layers[1],
      quiet = TRUE,
      stringsAsFactors = FALSE
    )

    if (!inherits(
      departments,
      "sf"
    )) {
      stop(
        "No se pudo leer fondo_mapa.gpkg como objeto sf."
      )
    }

    if (!"NOMBDEP" %in% names(
      departments
    )) {
      stop(
        "fondo_mapa.gpkg no contiene el campo NOMBDEP."
      )
    }

    if (is.na(
      sf::st_crs(
        departments
      )
    )) {
      stop(
        "fondo_mapa.gpkg no tiene CRS válido."
      )
    }

    departments <- sf::st_make_valid(
      departments
    )

    departments <- sf::st_transform(
      departments,
      4326
    )

    departments$NOMBDEP <- trimws(
      as.character(
        departments$NOMBDEP
      )
    )

    departments <- departments[
      !sf::st_is_empty(
        departments
      ),
      c(
        "NOMBDEP",
        attr(
          departments,
          "sf_column"
        )
      ),
      drop = FALSE
    ]

    assign(
      cache_key,
      departments,
      envir = department_cache
    )

    departments
  }


  visible_departments <- function(
      xlim,
      ylim
  ) {

    departments <- read_department_background()

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

    frame <- sf::st_sfc(
      sf::st_polygon(
        list(
          ring
        )
      ),
      crs = 4326
    )

    hits <- lengths(
      sf::st_intersects(
        departments,
        frame
      )
    ) > 0L

    departments[
      hits,
      ,
      drop = FALSE
    ]
  }


  department_label_points <- function(
      departments
  ) {

    if (
      is.null(
        departments
      ) ||
      nrow(
        departments
      ) == 0L
    ) {
      return(NULL)
    }

    pts <- suppressWarnings(
      sf::st_point_on_surface(
        departments
      )
    )

    xy <- sf::st_coordinates(
      pts
    )

    data.frame(
      NOMBDEP = departments$NOMBDEP,
      X = xy[, "X"],
      Y = xy[, "Y"],
      stringsAsFactors = FALSE
    )
  }


  # ==========================================================
  # 6. CARTOGRAFIA
  # ==========================================================

  box_polygon <- function(
      box,
      crs = 4326
  ) {
    ring <- matrix(
      c(
        box["xmin"], box["ymin"],
        box["xmax"], box["ymin"],
        box["xmax"], box["ymax"],
        box["xmin"], box["ymax"],
        box["xmin"], box["ymin"]
      ),
      ncol = 2,
      byrow = TRUE
    )

    sf::st_sfc(
      sf::st_polygon(
        list(ring)
      ),
      crs = crs
    )
  }


  box_overlap_fraction <- function(
      box,
      basin,
      epsg
  ) {
    bx <- box_polygon(box)

    bx_utm <- sf::st_transform(
      bx,
      epsg
    )

    basin_utm <- sf::st_transform(
      basin,
      epsg
    )

    bx_area <- as.numeric(
      sf::st_area(bx_utm)
    )

    if (!is.finite(bx_area) || bx_area <= 0) {
      return(Inf)
    }

    inter <- suppressWarnings(
      sf::st_intersection(
        bx_utm,
        sf::st_union(
          sf::st_geometry(
            basin_utm
          )
        )
      )
    )

    if (length(inter) == 0L) {
      return(0)
    }

    sum(
      as.numeric(
        sf::st_area(inter)
      ),
      na.rm = TRUE
    ) / bx_area
  }


  boxes_intersect <- function(a, b) {
    !(
      a["xmax"] <= b["xmin"] ||
      a["xmin"] >= b["xmax"] ||
      a["ymax"] <= b["ymin"] ||
      a["ymin"] >= b["ymax"]
    )
  }


  choose_box <- function(
      candidates,
      basin,
      epsg,
      reserved = NULL
  ) {
    scores <- numeric(
      length(candidates)
    )

    for (i in seq_along(candidates)) {
      scores[i] <- 1000 * box_overlap_fraction(
        candidates[[i]]$box,
        basin,
        epsg
      ) + candidates[[i]]$preference

      if (
        !is.null(reserved) &&
        boxes_intersect(
          candidates[[i]]$box,
          reserved
        )
      ) {
        scores[i] <- scores[i] + 5000
      }
    }

    candidates[[which.min(scores)]]
  }


  haversine_segment_m <- function(
      lon1,
      lat1,
      lon2,
      lat2
  ) {

    rad <- pi / 180

    lon1 <- as.numeric(lon1) * rad
    lat1 <- as.numeric(lat1) * rad
    lon2 <- as.numeric(lon2) * rad
    lat2 <- as.numeric(lat2) * rad

    dlon <- lon2 - lon1
    dlat <- lat2 - lat1

    a <- sin(dlat / 2)^2 +
      cos(lat1) *
      cos(lat2) *
      sin(dlon / 2)^2

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
        sqrt(a)
      )
  }


  nice_scale_length_m <- function(total_m) {
    if (!is.finite(total_m) || total_m <= 0) {
      return(1000)
    }

    target <- total_m / 3.0
    pow10 <- 10 ^ floor(log10(target))
    candidates <- c(
      1,
      2,
      5
    ) * pow10

    candidates <- candidates[
      candidates <= target
    ]

    if (length(candidates) == 0L) {
      return(pow10)
    }

    max(candidates)
  }


  draw_clima_map <- function(x) {
    basin <- sf::st_transform(
      x$basin,
      4326
    )

    clima <- sf::st_transform(
      x$clima,
      4326
    )

    tab <- x$table
    epsg <- x$epsg

    old_par <- graphics::par(
      no.readonly = TRUE
    )

    on.exit(
      graphics::par(old_par),
      add = TRUE
    )

    # Hoja A3 horizontal.
    graphics::par(
      oma = c(0, 0, 0, 0),
      bg = "white"
    )
    graphics::plot.new()

    graphics::par(
      fig = c(
        0.025,
        0.785,
        0.07,
        0.97
      ),
      mar = c(
        3.2,
        3.5,
        0.8,
        0.8
      ),
      new = TRUE,
      xpd = FALSE
    )

    bb <- sf::st_bbox(basin)
    dx <- as.numeric(bb["xmax"] - bb["xmin"])
    dy <- as.numeric(bb["ymax"] - bb["ymin"])

    xlim0 <- c(
      as.numeric(bb["xmin"]) - 0.07 * dx,
      as.numeric(bb["xmax"]) + 0.07 * dx
    )

    ylim0 <- c(
      as.numeric(bb["ymin"]) - 0.07 * dy,
      as.numeric(bb["ymax"]) + 0.07 * dy
    )

    mid_lat <- mean(ylim0)
    map_asp <- 1 / cos(
      mid_lat * pi / 180
    )

    graphics::plot.new()
    graphics::plot.window(
      xlim = xlim0,
      ylim = ylim0,
      xaxs = "i",
      yaxs = "i",
      asp = map_asp
    )

    usr <- graphics::par("usr")
    map_xlim <- usr[1:2]
    map_ylim <- usr[3:4]
    ux <- usr[2] - usr[1]
    uy <- usr[4] - usr[3]

    # Posición real del plot region para alinear leyenda.
    fig_now <- graphics::par("fig")
    plt_now <- graphics::par("plt")
    fig_w <- fig_now[2] - fig_now[1]
    fig_h <- fig_now[4] - fig_now[3]

    map_plot_bottom <- fig_now[3] + plt_now[3] * fig_h
    map_plot_top <- fig_now[3] + plt_now[4] * fig_h

    graphics::rect(
      map_xlim[1],
      map_ylim[1],
      map_xlim[2],
      map_ylim[2],
      col = "white",
      border = NA
    )

    departments <- tryCatch(
      visible_departments(
        map_xlim,
        map_ylim
      ),
      error = function(e) NULL
    )

    if (
      !is.null(
        departments
      ) &&
      nrow(
        departments
      ) > 0L
    ) {

      graphics::plot(
        sf::st_geometry(
          departments
        ),
        add = TRUE,
        col = "#FCFCFA",
        border = "#B7B7B7",
        lwd = 0.75
      )

      dep_labels <- department_label_points(
        departments
      )

      if (!is.null(
        dep_labels
      )) {
        graphics::text(
          dep_labels$X,
          dep_labels$Y,
          labels = dep_labels$NOMBDEP,
          col = "#8A8A8A",
          cex = 0.72,
          font = 2
        )
      }
    }

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
        "#AFAFAF",
        alpha.f = 0.28
      ),
      lwd = 0.55
    )

    codes <- unique(
      clima$CODIGO
    )

    cols <- clima_colors(codes)

    fill <- grDevices::adjustcolor(
      unname(
        cols[
          clima$CODIGO
        ]
      ),
      alpha.f = 1.00
    )

    graphics::plot(
      sf::st_geometry(clima),
      add = TRUE,
      col = fill,
      border = grDevices::adjustcolor(
        "#4F4F4F",
        alpha.f = 0.40
      ),
      lwd = 0.35
    )

    graphics::plot(
      sf::st_geometry(basin),
      add = TRUE,
      border = "#111111",
      lwd = 2.0
    )

    graphics::axis(
      1,
      at = xticks,
      labels = format(
        round(xticks, 2),
        nsmall = 2
      ),
      cex.axis = 0.72,
      mgp = c(2, 0.55, 0)
    )

    graphics::axis(
      2,
      at = yticks,
      labels = format(
        round(yticks, 2),
        nsmall = 2
      ),
      las = 2,
      cex.axis = 0.72,
      mgp = c(2, 0.55, 0)
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

    # --------------------------------------------------------
    # Norte dinámico.
    # --------------------------------------------------------

    nw <- 0.022 * ux
    nh <- 0.045 * uy
    north_w <- 0.080 * ux
    north_h <- 0.115 * uy

    north_positions <- list(
      list(
        name = "top_left",
        preference = 0,
        box = c(
          xmin = usr[1] + 0.020 * ux,
          xmax = usr[1] + 0.020 * ux + north_w,
          ymin = usr[4] - 0.020 * uy - north_h,
          ymax = usr[4] - 0.020 * uy
        )
      ),
      list(
        name = "top_right",
        preference = 10,
        box = c(
          xmin = usr[2] - 0.020 * ux - north_w,
          xmax = usr[2] - 0.020 * ux,
          ymin = usr[4] - 0.020 * uy - north_h,
          ymax = usr[4] - 0.020 * uy
        )
      ),
      list(
        name = "bottom_left",
        preference = 20,
        box = c(
          xmin = usr[1] + 0.020 * ux,
          xmax = usr[1] + 0.020 * ux + north_w,
          ymin = usr[3] + 0.020 * uy,
          ymax = usr[3] + 0.020 * uy + north_h
        )
      ),
      list(
        name = "bottom_right",
        preference = 30,
        box = c(
          xmin = usr[2] - 0.020 * ux - north_w,
          xmax = usr[2] - 0.020 * ux,
          ymin = usr[3] + 0.020 * uy,
          ymax = usr[3] + 0.020 * uy + north_h
        )
      )
    )

    north_choice <- choose_box(
      north_positions,
      basin,
      epsg
    )

    nb <- north_choice$box
    nx <- mean(nb[c("xmin", "xmax")])
    ny_top <- nb["ymax"] - 0.018 * uy

    graphics::rect(
      nb["xmin"],
      nb["ymin"],
      nb["xmax"],
      nb["ymax"],
      col = grDevices::adjustcolor(
        "#FFFDF7",
        alpha.f = 0.94
      ),
      border = "#6E6E6E",
      lwd = 0.9
    )

    graphics::text(
      nx,
      ny_top,
      labels = "N",
      font = 2,
      cex = 1.00
    )

    arrow_top <- ny_top - 0.012 * uy

    graphics::polygon(
      x = c(
        nx,
        nx - nw,
        nx,
        nx + nw
      ),
      y = c(
        arrow_top,
        arrow_top - nh,
        arrow_top - 0.014 * uy,
        arrow_top - nh
      ),
      col = "black",
      border = "black"
    )

    # --------------------------------------------------------
    # Escala dinámica.
    # --------------------------------------------------------

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
    scale_box_w <- scale_dx + 0.100 * ux
    scale_box_h <- 0.085 * uy

    make_scale_box <- function(xmin, ymin) {
      c(
        xmin = xmin,
        xmax = xmin + scale_box_w,
        ymin = ymin,
        ymax = ymin + scale_box_h
      )
    }

    scale_positions <- list(
      list(
        name = "bottom_left",
        preference = 0,
        box = make_scale_box(
          usr[1] + 0.020 * ux,
          usr[3] + 0.020 * uy
        )
      ),
      list(
        name = "bottom_right",
        preference = 8,
        box = make_scale_box(
          usr[2] - 0.020 * ux - scale_box_w,
          usr[3] + 0.020 * uy
        )
      ),
      list(
        name = "top_left",
        preference = 12,
        box = make_scale_box(
          usr[1] + 0.020 * ux,
          usr[4] - 0.020 * uy - scale_box_h
        )
      ),
      list(
        name = "top_right",
        preference = 16,
        box = make_scale_box(
          usr[2] - 0.020 * ux - scale_box_w,
          usr[4] - 0.020 * uy - scale_box_h
        )
      )
    )

    scale_choice <- choose_box(
      scale_positions,
      basin,
      epsg,
      reserved = nb
    )

    sb <- scale_choice$box

    graphics::rect(
      sb["xmin"],
      sb["ymin"],
      sb["xmax"],
      sb["ymax"],
      col = grDevices::adjustcolor(
        "#FFFDF7",
        alpha.f = 0.94
      ),
      border = "#6E6E6E",
      lwd = 0.9
    )

    sx0 <- sb["xmin"] + 0.025 * ux
    sy0 <- sb["ymin"] + 0.040 * uy
    bar_h <- 0.018 * uy
    nseg <- 4L
    seg_dx <- scale_dx / nseg

    for (i in 0:(nseg - 1L)) {
      graphics::rect(
        sx0 + i * seg_dx,
        sy0,
        sx0 + (i + 1L) * seg_dx,
        sy0 + bar_h,
        col = if (i %% 2L == 0L) "black" else "white",
        border = "black",
        lwd = 0.8
      )
    }

    for (i in 0:nseg) {
      graphics::text(
        sx0 + i * seg_dx,
        sy0 - 0.014 * uy,
        labels = format(
          round(
            scale_m * i / nseg / 1000,
            1
          ),
          trim = TRUE,
          scientific = FALSE
        ),
        cex = 0.78
      )
    }

    graphics::text(
      sx0 + scale_dx + 0.018 * ux,
      sy0 - 0.014 * uy,
      labels = "km",
      cex = 0.78,
      adj = c(0, 0.5)
    )

    # --------------------------------------------------------
    # Leyenda lateral dinamica.
    # La caja se ancla arriba y crece hacia abajo segun
    # el numero de clases presentes.
    # --------------------------------------------------------

    legend_codes <- as.character(
      tab$CODIGO
    )

    draw_dynamic_categorical_legend(
      title = "Clima",
      labels = legend_codes,
      colors = vapply(
        legend_codes,
        stable_code_color,
        character(1)
      ),
      map_plot_bottom = map_plot_bottom,
      map_plot_top = map_plot_top,
      subtitle = paste0(
        nrow(tab),
        " unidades presentes"
      ),
      fig_left = 0.805,
      fig_right = 0.985,
      max_rows = 16L
    )

    # Fuente al pie dentro del marco A3.
    graphics::par(
      fig = c(0, 1, 0, 1),
      mar = c(0, 0, 0, 0),
      new = TRUE,
      xpd = NA
    )

    graphics::plot.new()
    graphics::text(
      0.975,
      0.025,
      labels = layer_source_map_label(CLIMA_STEM),
      adj = c(1, 0),
      cex = 0.66,
      col = "grey30"
    )

    invisible(NULL)
  }


  # ==========================================================
  # 7. DT
  # ==========================================================

  clima_datatable <- function(tab) {
    out <- tab

    out$AREA_KM2 <- round(
      out$AREA_KM2,
      3
    )

    out$CUENCA_PCT <- round(
      out$CUENCA_PCT,
      2
    )

    names(out) <- c(
      "Código",
      "Descripción",
      "Área (km²)",
      "Cuenca (%)"
    )

    DT::datatable(
      out,
      rownames = FALSE,
      extensions = "Buttons",
      class = "compact stripe hover",
      options = list(
        dom = "Bfrtip",
        buttons = list(
          list(
            extend = "copy",
            text = "Copiar"
          ),
          list(
            extend = "csv",
            text = "CSV",
            filename = "clima_cuenca"
          ),
          list(
            extend = "excel",
            text = "Excel",
            filename = "clima_cuenca"
          )
        ),
        pageLength = 15,
        lengthMenu = c(
          10,
          15,
          25,
          50,
          100
        ),
        autoWidth = FALSE
      )
    )
  }


  # ==========================================================
  # 8. UI
  # ==========================================================

  ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".cli-wrap{padding:14px 18px 28px 18px;max-width:1500px;margin:auto;}",
            ".cli-head{display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:12px;}",
            ".cli-note{background:#f6f7f8;border-left:4px solid #607D8B;padding:10px 12px;margin:10px 0 14px 0;}",
            ".cli-card{border:1px solid #ddd;border-radius:7px;padding:12px 12px 18px 12px;background:white;margin-bottom:16px;}",
            ".cli-card h4{margin-top:0;}",
            ".cli-a3-frame{width:1188px;max-width:100%;margin:0 auto;aspect-ratio:420/297;}",
            ".cli-table-wrap{width:100%;overflow-x:auto;padding-bottom:14px;}",
            ".cli-table-wrap .dataTables_wrapper{width:100%;}",
            ".cli-table-wrap table.dataTable{width:100%!important;}"
          )
        )
      ),

      shiny::div(
        class = "cli-wrap",

        shiny::div(
          class = "cli-head",

          shiny::actionButton(
            ns("analizar"),
            "Cargar clima",
            class = "btn-success"
          ),

          shiny::downloadButton(
            ns("descargar_png"),
            "Descargar PNG"
          ),


          shiny::downloadButton(

            ns("descargar_shp_recortado"),

            "SHP recortado"

          ),


          shiny::downloadButton(

            ns("descargar_shp_mosaico"),

            "SHP mosaico"

          ),

          shiny::strong(
            shiny::textOutput(
              ns("estado"),
              inline = TRUE
            )
          )
        ),

        shiny::div(
          class = "cli-note",
          "La capa climática se carga únicamente para las geometrías que intersectan la cuenca activa. La leyenda muestra las clases presentes y la tabla inferior enlaza cada código con la metadata normalizada."
        ),

        layer_source_ui(CLIMA_STEM),

        shiny::conditionalPanel(
          condition = paste0(
            "output['",
            ns("has_results"),
            "'] === 'true'"
          ),

          shiny::div(
            class = "cli-card",
            shiny::tags$h4("Mapa climático"),
            shiny::div(
              class = "cli-a3-frame",
              shiny::plotOutput(
                ns("mapa"),
                width = "100%",
                height = "100%"
              )
            )
          ),

          shiny::div(
            class = "cli-card",
            shiny::tags$h4("Clases climáticas presentes"),
            shiny::div(
              class = "cli-table-wrap",
              DT::DTOutput(
                ns("tabla")
              )
            )
          )
        )
      )
    )
  }


  # ==========================================================
  # 9. SERVER
  # ==========================================================

  server <- function(
      id,
      basin,
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
        result <- shiny::reactiveVal(NULL)

        status <- shiny::reactiveVal(
          "Delimita o carga una cuenca para consultar Clima."
        )

        current_key <- shiny::reactive({
          b <- basin()

          if (is.null(b)) {
            return(NULL)
          }

          paste0(
            nrow(b),
            "|",
            paste(
              round(
                as.numeric(
                  sf::st_bbox(b)
                ),
                6
              ),
              collapse = "|"
            )
          )
        })

        shiny::observeEvent(
          current_key(),
          {
            result(NULL)
            status(
              "Cuenca disponible. Pulsa Cargar clima."
            )
          },
          ignoreInit = TRUE
        )

        shiny::observeEvent(
          input$analizar,
          {
            tryCatch(
              {
                b <- basin()

                if (is.null(b)) {
                  stop(
                    "Primero delimita o carga una cuenca."
                  )
                }

                status(
                  "Localizando capa climática..."
                )

                spatial <- read_clima_for_basin(
                  b
                )

                status(
                  "Calculando áreas y enlazando metadata..."
                )

                tab_info <- build_clima_table(
                  clima = spatial$clima,
                  basin = b
                )

                result(
                  list(
                    basin = b,
                    basin_source = if (!is.null(basin_source)) basin_source() else NULL,
                    basin_label = if (!is.null(basin_label)) basin_label() else NULL,
                    clima = spatial$clima,
                    table = tab_info$table,
                    basin_area_km2 = tab_info$basin_area_km2,
                    epsg = tab_info$epsg,
                    source_mode = spatial$source_mode,
                    selected_tiles = spatial$selected_tiles
                  )
                )

                status(
                  paste0(
                    "Clima listo: ",
                    nrow(tab_info$table),
                    " unidades | ",
                    length(spatial$selected_tiles),
                    if (identical(spatial$source_mode, "tiles")) {
                      " teselas usadas."
                    } else {
                      " archivo usado."
                    }
                  )
                )
              },
              error = function(e) {
                result(NULL)
                msg <- paste0(
                  "Error en Clima: ",
                  conditionMessage(e)
                )
                status(msg)
                shiny::showNotification(
                  msg,
                  type = "error",
                  duration = NULL
                )
              }
            )
          }
        )

        output$estado <- shiny::renderText({
          status()
        })

        output$has_results <- shiny::renderText({
          if (is.null(result())) {
            "false"
          } else {
            "true"
          }
        })

        shiny::outputOptions(
          output,
          "has_results",
          suspendWhenHidden = FALSE
        )

        output$mapa <- shiny::renderPlot({
          x <- result()
          shiny::req(x)
          draw_clima_map(x)
        })

        output$tabla <- DT::renderDT({
          x <- result()
          shiny::req(x)
          clima_datatable(
            x$table
          )
        }, server = FALSE)

        output$descargar_png <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "mapa_climatico_",
              format(
                Sys.Date(),
                "%Y%m%d"
              ),
              ".png"
            )
          },
          content = function(file) {
            x <- shiny::isolate(
              result()
            )

            if (is.null(x)) {
              stop(
                "No hay un mapa climático calculado para descargar."
              )
            }

            grDevices::png(
              filename = file,
              width = 420 / 25.4 * 300,
              height = 297 / 25.4 * 300,
              units = "px",
              res = 300
            )

            on.exit(
              grDevices::dev.off(),
              add = TRUE
            )

            draw_clima_map(x)
          }
        )

        shiny::outputOptions(
          output,
          "descargar_png",
          suspendWhenHidden = FALSE
        )



        output$descargar_shp_recortado <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "clima_normalizado_recortado_",
              format(Sys.Date(), "%Y%m%d"),
              ".zip"
            )
          },
          content = function(file) {
            x <- shiny::isolate(result())

            if (is.null(x)) {
              stop("No hay resultados calculados para exportar.")
            }

            mosaic <- read_vector_export_mosaic(
              selected_files = x$selected_tiles,
              source_mode = x$source_mode,
              basin = x$basin
            )

            clipped <- clip_vector_export_to_basin(
              mosaic,
              x$basin
            )

            write_shapefile_zip_bundle(
              layers = list(
                clima_recortado = clipped
              ),
              target_file = file,
              bundle_stem = "clima_normalizado_recortado"
            )
          }
        )


        output$descargar_shp_mosaico <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "clima_normalizado_mosaico_",
              format(Sys.Date(), "%Y%m%d"),
              ".zip"
            )
          },
          content = function(file) {
            x <- shiny::isolate(result())

            if (is.null(x)) {
              stop("No hay resultados calculados para exportar.")
            }

            mosaic <- read_vector_export_mosaic(
              selected_files = x$selected_tiles,
              source_mode = x$source_mode,
              basin = x$basin
            )

            write_shapefile_zip_bundle(
              layers = list(
                clima_mosaico = mosaic
              ),
              target_file = file,
              bundle_stem = "clima_normalizado_mosaico"
            )
          }
        )


        shiny::outputOptions(
          output,
          "descargar_shp_recortado",
          suspendWhenHidden = FALSE
        )

        shiny::outputOptions(
          output,
          "descargar_shp_mosaico",
          suspendWhenHidden = FALSE
        )

        list(
          result = result
        )
      }
    )
  }


  list(
    ui = ui,
    server = server
  )
})

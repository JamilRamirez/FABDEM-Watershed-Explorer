# ============================================================
# R/cuencas.R
#
# FABDEM Watershed Explorer
# SUBMODULO: CUENCAS
# v9: panel de leyenda lateral más ancho
# ============================================================

cuencas <- local({

  # CONFIGURACION LAZY / INDEPENDIENTE DEL ORDEN DE CARGA
  # ------------------------------------------------------
  # Ninguna ruta de config se evalua al sourcear este archivo.
  # LAYERS_DIR, LAYER_METADATA_XLSX y FONDO_MAPA_GPKG se
  # resuelven solo cuando sus funciones son invocadas, despues
  # de que Shiny haya terminado de cargar todos los archivos R/.


  CUENCAS_STEM <- "cuencas_normalizada_disuelto"
  RIOS_STEM <- "rios_normalizada_disuelto"

  # Jerarquia visual de la red hidrografica.
  RIVER_COLOR <- "#1F5FAF"
  RIVER_MINOR_COLOR <- "#6FA8DC"
  RIVER_OTHER_COLOR <- "#4F86C6"

  # Limite de etiquetas secundarias.
  MAX_SECONDARY_LABELS <- 6L

  # Contrato de datos de la capa de rios.
  # El GPKG corregido contiene los nombres en r_q_text.
  RIVER_LABEL_FIELD <- "r_q_text"


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


  cuencas_colors <- function(codes) {
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


  read_cuencas_metadata <- function() {
    if (exists(
      "cuencas",
      envir = metadata_cache,
      inherits = FALSE
    )) {
      return(
        get(
          "cuencas",
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
      meta$GPKG == CUENCAS_STEM &
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
          CUENCAS_STEM,
          "."
        )
      )
    }

    assign(
      "cuencas",
      meta,
      envir = metadata_cache
    )

    meta
  }


  # ==========================================================
  # 3. CARGA ESPACIAL TESelADA
  # ==========================================================

  cuencas_tiled_dir <- function() {
    file.path(
      CONTEXTO_TERRITORIAL_DIR,
      paste0(
        CUENCAS_STEM,
        "__tiles"
      )
    )
  }


  cuencas_original_file <- function() {
    file.path(
      CONTEXTO_TERRITORIAL_DIR,
      paste0(
        CUENCAS_STEM,
        ".gpkg"
      )
    )
  }


  cuencas_index <- function() {
    tiled_dir <- cuencas_tiled_dir()
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
          "El index.gpkg de Cuencas no contiene RELATIVE_PATH."
        )
      }

      return(idx)
    }

    NULL
  }


  read_cuencas_for_basin <- function(basin) {
    basin <- sf::st_make_valid(basin)
    basin <- basin[
      !sf::st_is_empty(basin),
      ,
      drop = FALSE
    ]

    if (nrow(basin) == 0L) {
      stop("La cuenca activa está vacía.")
    }

    idx <- cuencas_index()
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
          "Ninguna tesela de Cuencas intersecta la cuenca activa."
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
            "Falta una tesela de Cuencas:\n",
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
      original <- cuencas_original_file()

      if (!file_nonempty_local(original)) {
        stop(
          paste0(
            "No se encontró Cuencas teselada ni el GPKG original.\n",
            "Esperado:\n",
            cuencas_tiled_dir(),
            "\nó\n",
            original
          )
        )
      }

      layers <- sf::st_layers(original)$name

      if (length(layers) < 1L) {
        stop("El GPKG de Cuencas no contiene capas vectoriales.")
      }

      g <- sf::st_read(
        original,
        layer = layers[1],
        quiet = TRUE
      )

      if (!"CODIGO" %in% names(g)) {
        stop("El GPKG de Cuencas no contiene CODIGO.")
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
      stop("La capa de Cuencas no tiene CRS válido.")
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

    cuencas_all <- do.call(
      rbind,
      pieces
    )

    cuencas_all <- sf::st_make_valid(
      cuencas_all
    )

    cuencas_all <- cuencas_all[
      !sf::st_is_empty(cuencas_all),
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
        cuencas_all,
        basin_union
      )
    ) > 0L

    cuencas_all <- cuencas_all[
      keep,
      ,
      drop = FALSE
    ]

    if (nrow(cuencas_all) == 0L) {
      stop("Cuencas no contiene unidades dentro de la cuenca.")
    }

    clipped <- suppressWarnings(
      sf::st_intersection(
        cuencas_all,
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
      stop("La intersección de cuencas no produjo polígonos válidos.")
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
      cuencas = clipped,
      source_mode = source_mode,
      selected_tiles = selected_tiles
    )
  }


  # ==========================================================
  # 4. RIOS TEMATICOS Y ETIQUETAS
  # ==========================================================

  rivers_tiled_dir <- function() {
    file.path(
      CONTEXTO_TERRITORIAL_DIR,
      paste0(
        RIOS_STEM,
        "__tiles"
      )
    )
  }


  rivers_original_file <- function() {
    file.path(
      CONTEXTO_TERRITORIAL_DIR,
      paste0(
        RIOS_STEM,
        ".gpkg"
      )
    )
  }


  rivers_index <- function() {
    tiled_dir <- rivers_tiled_dir()
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
          "El index.gpkg de Ríos no contiene RELATIVE_PATH."
        )
      }

      return(idx)
    }

    NULL
  }


  nonempty_text <- function(x) {
    x <- trimws(
      as.character(x)
    )

    !is.na(x) & nzchar(x)
  }


  river_name_class <- function(x) {
    x <- trimws(
      as.character(x)
    )

    out <- rep(
      "other",
      length(x)
    )

    empty <- is.na(x) | !nzchar(x)

    is_river <- !empty & grepl(
      "^\\s*(R[ií]o|Rio)(\\s|$)",
      x,
      ignore.case = TRUE,
      perl = TRUE
    )

    is_quebrada <- !empty & grepl(
      "^\\s*(Qda\\.?|Quebrada)(\\s|$)",
      x,
      ignore.case = TRUE,
      perl = TRUE
    )

    out[is_river] <- "river"
    out[is_quebrada] <- "quebrada"
    out[empty] <- "empty"

    out
  }


  detect_river_label_field <- function(rivers) {
    if (is.null(rivers) || nrow(rivers) == 0L) {
      return(NULL)
    }

    attrs <- setdiff(
      names(rivers),
      attr(
        rivers,
        "sf_column"
      )
    )

    hit <- attrs[
      tolower(attrs) == tolower(
        RIVER_LABEL_FIELD
      )
    ]

    if (length(hit) == 0L) {
      stop(
        paste0(
          "La capa de rios no contiene el campo esperado '",
          RIVER_LABEL_FIELD,
          "'. Campos disponibles: ",
          paste(
            attrs,
            collapse = ", "
          )
        )
      )
    }

    field <- hit[1L]

    if (!any(
      nonempty_text(
        rivers[[field]]
      )
    )) {
      return(NULL)
    }

    field
  }


  read_rivers_for_basin <- function(basin) {
    basin <- sf::st_make_valid(basin)
    basin <- basin[
      !sf::st_is_empty(basin),
      ,
      drop = FALSE
    ]

    if (nrow(basin) == 0L) {
      stop("La cuenca activa está vacía.")
    }

    idx <- rivers_index()
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
          "Ninguna tesela de Ríos intersecta la cuenca activa."
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
            "Falta una tesela de Ríos:\n",
            missing_tiles[1]
          )
        )
      }

      for (i in seq_along(selected_tiles)) {
        pieces[[length(pieces) + 1L]] <- sf::st_read(
          selected_tiles[i],
          layer = "data",
          quiet = TRUE,
          stringsAsFactors = FALSE
        )
      }

      source_mode <- "tiles"

    } else {
      original <- rivers_original_file()

      if (!file_nonempty_local(original)) {
        stop(
          paste0(
            "No se encontró Ríos teselado ni el GPKG original.\n",
            "Esperado:\n",
            rivers_tiled_dir(),
            "\nó\n",
            original
          )
        )
      }

      layers <- sf::st_layers(original)$name

      if (length(layers) < 1L) {
        stop(
          "El GPKG de Ríos no contiene capas vectoriales."
        )
      }

      pieces[[1L]] <- sf::st_read(
        original,
        layer = layers[1],
        quiet = TRUE,
        stringsAsFactors = FALSE
      )

      source_mode <- "original"
      selected_tiles <- original
    }

    crs_ref <- sf::st_crs(
      pieces[[1L]]
    )

    if (is.na(crs_ref)) {
      stop(
        "La capa de Ríos no tiene CRS válido."
      )
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

    rivers <- do.call(
      rbind,
      pieces
    )

    rivers <- sf::st_make_valid(
      rivers
    )

    rivers <- rivers[
      !sf::st_is_empty(
        rivers
      ),
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
        rivers,
        basin_union
      )
    ) > 0L

    rivers <- rivers[
      keep,
      ,
      drop = FALSE
    ]

    if (nrow(rivers) == 0L) {
      rivers$.__LABEL__ <- character(0)
      return(
        list(
          rivers = rivers,
          label_field = NULL,
          source_mode = source_mode,
          selected_tiles = selected_tiles
        )
      )
    }

    rivers <- suppressWarnings(
      sf::st_intersection(
        rivers,
        basin_union
      )
    )

    rivers <- rivers[
      !sf::st_is_empty(
        rivers
      ),
      ,
      drop = FALSE
    ]

    geom_type <- as.character(
      sf::st_geometry_type(
        rivers,
        by_geometry = TRUE
      )
    )

    rivers <- rivers[
      geom_type %in% c(
        "LINESTRING",
        "MULTILINESTRING"
      ),
      ,
      drop = FALSE
    ]

    label_field <- detect_river_label_field(
      rivers
    )

    if (is.null(label_field)) {
      rivers$.__LABEL__ <- NA_character_
    } else {
      rivers$.__LABEL__ <- trimws(
        as.character(
          rivers[[label_field]]
        )
      )

      rivers$.__LABEL__[
        is.na(rivers$.__LABEL__) |
          !nzchar(rivers$.__LABEL__) |
          toupper(rivers$.__LABEL__) %in% c(
            "NA",
            "N/A",
            "NULL"
          )
      ] <- NA_character_
    }

    rivers$.__CLASS__ <- river_name_class(
      rivers$.__LABEL__
    )

    list(
      rivers = rivers,
      label_field = label_field,
      source_mode = source_mode,
      selected_tiles = selected_tiles
    )
  }


  cuenca_label_points <- function(
      cuencas,
      tab
  ) {
    if (is.null(cuencas) || nrow(cuencas) == 0L) {
      return(NULL)
    }

    codes <- unique(
      trimws(
        as.character(
          cuencas$CODIGO
        )
      )
    )

    codes <- codes[
      !is.na(codes) & nzchar(codes)
    ]

    if (length(codes) == 0L) {
      return(NULL)
    }

    out <- vector(
      "list",
      length(codes)
    )

    k <- 0L

    for (code in codes) {
      desc <- tab$DESCRIPCION[
        match(
          code,
          tab$CODIGO
        )
      ]

      if (
        length(desc) != 1L ||
        is.na(desc) ||
        !nzchar(trimws(desc)) ||
        identical(
          desc,
          "Sin descripcion en metadata"
        ) ||
        identical(
          desc,
          "Sin descripción en metadata"
        )
      ) {
        next
      }

      geom <- sf::st_union(
        sf::st_geometry(
          cuencas[
            cuencas$CODIGO == code,
            ,
            drop = FALSE
          ]
        )
      )

      pt <- suppressWarnings(
        sf::st_point_on_surface(
          geom
        )
      )

      xy <- sf::st_coordinates(
        pt
      )

      if (nrow(xy) < 1L) {
        next
      }

      k <- k + 1L
      out[[k]] <- data.frame(
        CODIGO = code,
        LABEL = trimws(desc),
        X = xy[1, "X"],
        Y = xy[1, "Y"],
        stringsAsFactors = FALSE
      )
    }

    if (k == 0L) {
      return(NULL)
    }

    do.call(
      rbind,
      out[seq_len(k)]
    )
  }


  river_label_points <- function(
      rivers,
      epsg
  ) {
    if (
      is.null(rivers) ||
      nrow(rivers) == 0L ||
      !".__LABEL__" %in% names(rivers)
    ) {
      return(NULL)
    }

    labels <- unique(
      trimws(
        as.character(
          rivers$.__LABEL__
        )
      )
    )

    labels <- labels[
      !is.na(labels) & nzchar(labels)
    ]

    if (length(labels) == 0L) {
      return(NULL)
    }

    out <- vector(
      "list",
      length(labels)
    )

    k <- 0L

    for (lab in labels) {

      g <- rivers[
        !is.na(rivers$.__LABEL__) &
          rivers$.__LABEL__ == lab,
        ,
        drop = FALSE
      ]

      if (nrow(g) == 0L) {
        next
      }

      g_utm <- sf::st_transform(
        g,
        epsg
      )

      pieces <- suppressWarnings(
        sf::st_cast(
          sf::st_geometry(
            g_utm
          ),
          "LINESTRING",
          warn = FALSE
        )
      )

      if (length(pieces) == 0L) {
        next
      }

      lens <- suppressWarnings(
        as.numeric(
          sf::st_length(
            pieces
          )
        )
      )

      good <- is.finite(lens) & lens > 0

      if (!any(good)) {
        next
      }

      best <- which.max(
        ifelse(
          good,
          lens,
          -Inf
        )
      )

      line <- pieces[
        best
      ]

      coords <- try(
        sf::st_coordinates(
          line
        ),
        silent = TRUE
      )

      if (
        inherits(coords, "try-error") ||
        is.null(coords) ||
        nrow(coords) < 2L ||
        !all(c("X", "Y") %in% colnames(coords))
      ) {
        next
      }

      xy <- coords[
        ,
        c("X", "Y"),
        drop = FALSE
      ]

      keep_xy <- is.finite(xy[, 1]) & is.finite(xy[, 2])
      xy <- xy[
        keep_xy,
        ,
        drop = FALSE
      ]

      if (nrow(xy) < 2L) {
        next
      }

      delta <- xy[-1, , drop = FALSE] -
        xy[-nrow(xy), , drop = FALSE]

      seg_len <- sqrt(
        rowSums(
          delta^2
        )
      )

      good_seg <- is.finite(seg_len) & seg_len > 0

      if (!any(good_seg)) {
        next
      }

      cum_len <- c(
        0,
        cumsum(
          ifelse(
            good_seg,
            seg_len,
            0
          )
        )
      )

      total_len <- tail(
        cum_len,
        1
      )

      if (!is.finite(total_len) || total_len <= 0) {
        next
      }

      target <- total_len / 2

      seg_i <- which(
        cum_len[-1] >= target & good_seg
      )[1L]

      if (is.na(seg_i)) {
        seg_i <- which(good_seg)[
          ceiling(
            sum(good_seg) / 2
          )
        ]
      }

      seg_start <- cum_len[
        seg_i
      ]

      frac <- (
        target - seg_start
      ) / seg_len[
        seg_i
      ]

      frac <- max(
        0,
        min(
          1,
          frac
        )
      )

      mid_xy <- xy[
        seg_i,
        ,
        drop = FALSE
      ] + frac * delta[
        seg_i,
        ,
        drop = FALSE
      ]

      angle <- atan2(
        delta[seg_i, 2],
        delta[seg_i, 1]
      ) * 180 / pi

      # Mantener la lectura de izquierda a derecha.
      if (angle > 90) {
        angle <- angle - 180
      }

      if (angle < -90) {
        angle <- angle + 180
      }

      pt_utm <- sf::st_sfc(
        sf::st_point(
          as.numeric(
            mid_xy[1, ]
          )
        ),
        crs = epsg
      )

      pt_4326 <- sf::st_transform(
        pt_utm,
        4326
      )

      pt_xy <- sf::st_coordinates(
        pt_4326
      )

      if (nrow(pt_xy) < 1L) {
        next
      }

      cls <- river_name_class(
        lab
      )[1L]

      priority <- switch(
        cls,
        river = 1L,
        other = 2L,
        quebrada = 3L,
        4L
      )

      k <- k + 1L

      out[[k]] <- data.frame(
        LABEL = lab,
        CLASS = cls,
        PRIORITY = priority,
        LENGTH_KM = lens[best] / 1000,
        X = pt_xy[1, "X"],
        Y = pt_xy[1, "Y"],
        ANGLE = angle,
        stringsAsFactors = FALSE
      )
    }

    if (k == 0L) {
      return(NULL)
    }

    do.call(
      rbind,
      out[seq_len(k)]
    )
  }


  river_label_style <- function(x) {

    x$CEX <- ifelse(
      x$CLASS == "river",
      0.60,
      ifelse(
        x$CLASS == "other",
        0.46,
        0.36
      )
    )

    x$FONT <- ifelse(
      x$CLASS == "river",
      2L,
      1L
    )

    x$COL <- ifelse(
      x$CLASS == "river",
      "#FFFFFF",
      ifelse(
        x$CLASS == "other",
        grDevices::adjustcolor(
          "#F7F7F7",
          alpha.f = 0.95
        ),
        grDevices::adjustcolor(
          "#F4F4F4",
          alpha.f = 0.82
        )
      )
    )

    x
  }


  select_nonoverlap_river_labels <- function(
      labels,
      ux,
      uy
  ) {

    if (
      is.null(labels) ||
      nrow(labels) == 0L
    ) {
      return(NULL)
    }

    labels <- river_label_style(
      labels
    )

    primary <- labels[
      labels$CLASS %in% c(
        "river",
        "other"
      ),
      ,
      drop = FALSE
    ]

    primary <- primary[
      order(
        primary$PRIORITY,
        -primary$LENGTH_KM,
        primary$LABEL
      ),
      ,
      drop = FALSE
    ]

    accepted <- vector(
      "list",
      nrow(labels)
    )

    accepted_boxes <- vector(
      "list",
      nrow(labels)
    )

    n_keep <- 0L

    accept_if_free <- function(lab) {

      w <- graphics::strwidth(
        lab$LABEL,
        units = "user",
        cex = lab$CEX,
        font = lab$FONT,
        family = "sans"
      )

      h <- graphics::strheight(
        lab$LABEL,
        units = "user",
        cex = lab$CEX,
        font = lab$FONT,
        family = "sans"
      )

      theta <- abs(
        lab$ANGLE
      ) * pi / 180

      w_rot <- abs(cos(theta)) * w +
        abs(sin(theta)) * h

      h_rot <- abs(sin(theta)) * w +
        abs(cos(theta)) * h

      pad_factor <- if (
        lab$CLASS == "river"
      ) {
        0.0020
      } else if (
        lab$CLASS == "other"
      ) {
        0.0030
      } else {
        0.0055
      }

      box <- c(
        xmin = lab$X - w_rot / 2 - pad_factor * ux,
        xmax = lab$X + w_rot / 2 + pad_factor * ux,
        ymin = lab$Y - h_rot / 2 - pad_factor * uy,
        ymax = lab$Y + h_rot / 2 + pad_factor * uy
      )

      collides <- FALSE

      if (n_keep > 0L) {
        for (j in seq_len(n_keep)) {
          b <- accepted_boxes[[j]]

          overlap <- !(
            box["xmax"] < b["xmin"] ||
            box["xmin"] > b["xmax"] ||
            box["ymax"] < b["ymin"] ||
            box["ymin"] > b["ymax"]
          )

          if (overlap) {
            collides <- TRUE
            break
          }
        }
      }

      if (!collides) {
        n_keep <<- n_keep + 1L
        accepted[[n_keep]] <<- lab
        accepted_boxes[[n_keep]] <<- box
        return(TRUE)
      }

      FALSE
    }

    if (nrow(primary) > 0L) {
      for (i in seq_len(nrow(primary))) {
        accept_if_free(
          primary[i, , drop = FALSE]
        )
      }
    }

    secondary <- labels[
      labels$CLASS == "quebrada",
      ,
      drop = FALSE
    ]

    if (nrow(secondary) > 0L) {
      secondary <- secondary[
        order(
          -secondary$LENGTH_KM,
          secondary$LABEL
        ),
        ,
        drop = FALSE
      ]

      n_secondary <- 0L

      for (i in seq_len(nrow(secondary))) {

        if (n_secondary >= MAX_SECONDARY_LABELS) {
          break
        }

        ok <- accept_if_free(
          secondary[i, , drop = FALSE]
        )

        if (isTRUE(ok)) {
          n_secondary <- n_secondary + 1L
        }
      }
    }

    if (n_keep == 0L) {
      return(NULL)
    }

    out <- do.call(
      rbind,
      accepted[seq_len(n_keep)]
    )

    rownames(out) <- NULL
    out
  }


  draw_halo_text <- function(
      x,
      y,
      labels,
      col,
      halo_col = "white",
      cex = 0.62,
      font = 1,
      dx = 0,
      dy = 0,
      srt = 0,
      outline = FALSE
  ) {
    if (length(labels) == 0L) {
      return(invisible(NULL))
    }

    n <- length(
      labels
    )

    x <- rep(
      x,
      length.out = n
    )

    y <- rep(
      y,
      length.out = n
    )

    labels <- rep(
      labels,
      length.out = n
    )

    col <- rep(
      col,
      length.out = n
    )

    cex <- rep(
      cex,
      length.out = n
    )

    font <- rep(
      font,
      length.out = n
    )

    srt <- rep(
      srt,
      length.out = n
    )

    # 'srt' es un parámetro gráfico escalar en graphics::text().
    # Cada etiqueta se dibuja por separado para conservar su
    # orientación individual sin producir el error de longitud.
    for (i in seq_len(n)) {

      if (
        !is.finite(x[i]) ||
        !is.finite(y[i]) ||
        is.na(labels[i]) ||
        !nzchar(
          trimws(
            as.character(
              labels[i]
            )
          )
        )
      ) {
        next
      }

      angle_i <- srt[i]

      if (
        !is.finite(
          angle_i
        )
      ) {
        angle_i <- 0
      }

      graphics::text(
        x = x[i],
        y = y[i],
        labels = labels[i],
        col = col[i],
        cex = cex[i],
        font = font[i],
        srt = angle_i,
        family = "sans",
        xpd = NA
      )
    }

    invisible(NULL)
  }




  # ==========================================================
  # 5. TABLA DE UNIDADES
  # ==========================================================

  build_cuencas_table <- function(
      cuencas,
      basin
  ) {
    basin_4326 <- sf::st_transform(
      basin,
      4326
    )

    epsg <- local_utm_epsg(
      basin_4326
    )

    cuencas_utm <- sf::st_transform(
      cuencas,
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
        cuencas_utm
      )
    ) / 1e6

    area_by_code <- tapply(
      piece_area_km2,
      cuencas$CODIGO,
      sum,
      na.rm = TRUE
    )

    tab <- data.frame(
      CODIGO = names(area_by_code),
      AREA_KM2 = as.numeric(area_by_code),
      stringsAsFactors = FALSE
    )

    meta <- read_cuencas_metadata()

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
  # 6. FONDO DE UNIDADES HIDROGRAFICAS OFICIALES
  # ==========================================================

  cuencas_background_cache <- new.env(
    parent = emptyenv()
  )


  read_cuencas_background <- function() {

    cache_key <- "cuencas_background"

    if (exists(
      cache_key,
      envir = cuencas_background_cache,
      inherits = FALSE
    )) {
      return(
        get(
          cache_key,
          envir = cuencas_background_cache,
          inherits = FALSE
        )
      )
    }

    path <- cuencas_original_file()

    if (!file_nonempty_local(
      path
    )) {
      stop(
        paste0(
          "No se encontro el GPKG de Cuencas para usarlo como fondo:\n",
          path
        )
      )
    }

    layers <- sf::st_layers(
      path
    )$name

    if (length(layers) < 1L) {
      stop(
        "El GPKG de Cuencas no contiene capas vectoriales."
      )
    }

    bg <- sf::st_read(
      path,
      layer = layers[1],
      quiet = TRUE,
      stringsAsFactors = FALSE
    )

    if (!"CODIGO" %in% names(bg)) {
      stop(
        "El GPKG de Cuencas usado como fondo no contiene CODIGO."
      )
    }

    if (is.na(
      sf::st_crs(
        bg
      )
    )) {
      stop(
        "El GPKG de Cuencas usado como fondo no tiene CRS valido."
      )
    }

    bg <- sf::st_make_valid(
      bg
    )

    bg <- bg[
      !sf::st_is_empty(bg),
      ,
      drop = FALSE
    ]

    bg <- sf::st_transform(
      bg,
      4326
    )

    bg$CODIGO <- trimws(
      as.character(
        bg$CODIGO
      )
    )

    meta <- read_cuencas_metadata()

    bg$NOMBRE <- meta$NOMBRE[
      match(
        bg$CODIGO,
        meta$CODIGO
      )
    ]

    bg$NOMBRE <- trimws(
      as.character(
        bg$NOMBRE
      )
    )

    bg$NOMBRE[
      is.na(bg$NOMBRE) |
        !nzchar(bg$NOMBRE)
    ] <- NA_character_

    assign(
      cache_key,
      bg,
      envir = cuencas_background_cache
    )

    bg
  }


  visible_cuencas_background <- function(
      xlim,
      ylim
  ) {

    bg <- read_cuencas_background()

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
        bg,
        frame
      )
    ) > 0L

    bg[
      hits,
      ,
      drop = FALSE
    ]
  }


  cuencas_background_label_points <- function(
      bg,
      active_codes = character(0)
  ) {

    if (
      is.null(bg) ||
      nrow(bg) == 0L
    ) {
      return(NULL)
    }

    bg <- bg[
      !is.na(bg$NOMBRE) &
        nzchar(bg$NOMBRE) &
        !bg$CODIGO %in% active_codes,
      ,
      drop = FALSE
    ]

    if (nrow(bg) == 0L) {
      return(NULL)
    }

    codes <- unique(
      bg$CODIGO
    )

    out <- vector(
      "list",
      length(codes)
    )

    k <- 0L

    for (code in codes) {

      g <- bg[
        bg$CODIGO == code,
        ,
        drop = FALSE
      ]

      if (nrow(g) == 0L) {
        next
      }

      nm <- g$NOMBRE[
        which(
          !is.na(g$NOMBRE) &
            nzchar(g$NOMBRE)
        )[1L]
      ]

      if (
        length(nm) != 1L ||
        is.na(nm) ||
        !nzchar(nm)
      ) {
        next
      }

      pt <- suppressWarnings(
        sf::st_point_on_surface(
          sf::st_union(
            sf::st_geometry(g)
          )
        )
      )

      xy <- sf::st_coordinates(
        pt
      )

      if (nrow(xy) < 1L) {
        next
      }

      k <- k + 1L

      out[[k]] <- data.frame(
        LABEL = nm,
        X = xy[1, "X"],
        Y = xy[1, "Y"],
        stringsAsFactors = FALSE
      )
    }

    if (k == 0L) {
      return(NULL)
    }

    do.call(
      rbind,
      out[seq_len(k)]
    )
  }


  # ==========================================================
  # 7. CARTOGRAFIA
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


  draw_cuencas_map <- function(x) {
    basin <- sf::st_transform(
      x$basin,
      4326
    )

    cuencas <- sf::st_transform(
      x$cuencas,
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

    cuencas_bg <- tryCatch(
      visible_cuencas_background(
        map_xlim,
        map_ylim
      ),
      error = function(e) NULL
    )

    if (
      !is.null(cuencas_bg) &&
      nrow(cuencas_bg) > 0L
    ) {

      graphics::plot(
        sf::st_geometry(
          cuencas_bg
        ),
        add = TRUE,
        col = "#FCFCFA",
        border = "#B7B7B7",
        lwd = 0.75
      )

      bg_labels <- cuencas_background_label_points(
        cuencas_bg,
        active_codes = unique(
          cuencas$CODIGO
        )
      )

      if (!is.null(bg_labels)) {
        graphics::text(
          bg_labels$X,
          bg_labels$Y,
          labels = bg_labels$LABEL,
          col = "#929292",
          cex = 0.62,
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
      cuencas$CODIGO
    )

    cols <- cuencas_colors(codes)

    fill <- grDevices::adjustcolor(
      unname(
        cols[
          cuencas$CODIGO
        ]
      ),
      alpha.f = 1.00
    )

    graphics::plot(
      sf::st_geometry(cuencas),
      add = TRUE,
      col = fill,
      border = grDevices::adjustcolor(
        "#4F4F4F",
        alpha.f = 0.50
      ),
      lwd = 0.45
    )

    # Etiquetas de todas las unidades hidrográficas con nombre.
    cuenca_labels <- cuenca_label_points(
      cuencas,
      tab
    )

    if (!is.null(cuenca_labels)) {
      draw_halo_text(
        cuenca_labels$X,
        cuenca_labels$Y,
        cuenca_labels$LABEL,
        col = "#202020",
        halo_col = grDevices::adjustcolor(
          "white",
          alpha.f = 0.96
        ),
        cex = 0.64,
        font = 2,
        dx = 0.0016 * ux,
        dy = 0.0016 * uy
      )
    }

    # Ríos: capa temática de apoyo, sin tabla ni interacción.
    rivers <- NULL

    if (!is.null(x$rivers)) {
      rivers <- sf::st_transform(
        x$rivers,
        4326
      )
    }

    if (!is.null(rivers) && nrow(rivers) > 0L) {

      # Toda la red queda visible, pero las quebradas no compiten
      # visualmente con los rios principales.
      graphics::plot(
        sf::st_geometry(rivers),
        add = TRUE,
        col = grDevices::adjustcolor(
          RIVER_MINOR_COLOR,
          alpha.f = 0.72
        ),
        lwd = 0.55
      )

      river_class <- if (
        ".__CLASS__" %in% names(rivers)
      ) {
        rivers$.__CLASS__
      } else {
        river_name_class(
          rivers$.__LABEL__
        )
      }

      main_idx <- river_class == "river"

      if (any(
        main_idx,
        na.rm = TRUE
      )) {
        graphics::plot(
          sf::st_geometry(
            rivers[
              main_idx,
              ,
              drop = FALSE
            ]
          ),
          add = TRUE,
          col = RIVER_COLOR,
          lwd = 1.35
        )
      }

      other_idx <- river_class == "other"

      if (any(
        other_idx,
        na.rm = TRUE
      )) {
        graphics::plot(
          sf::st_geometry(
            rivers[
              other_idx,
              ,
              drop = FALSE
            ]
          ),
          add = TRUE,
          col = grDevices::adjustcolor(
            RIVER_OTHER_COLOR,
            alpha.f = 0.86
          ),
          lwd = 0.78
        )
      }

      rio_labels <- river_label_points(
        rivers,
        epsg
      )

      rio_labels <- select_nonoverlap_river_labels(
        rio_labels,
        ux = ux,
        uy = uy
      )

      if (!is.null(rio_labels)) {

        # Etiquetas limpias sin halo. Los ríos dominan y las
        # quebradas solo sobreviven si no saturan el mapa.
        draw_halo_text(
          rio_labels$X,
          rio_labels$Y,
          rio_labels$LABEL,
          col = rio_labels$COL,
          cex = rio_labels$CEX,
          font = rio_labels$FONT,
          srt = rio_labels$ANGLE,
          outline = FALSE
        )
      }
    }

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
    # Incluye la red hidrografica como fila adicional.
    # --------------------------------------------------------

    legend_codes <- as.character(
      tab$CODIGO
    )

    draw_dynamic_categorical_legend(
      title = "Cuencas oficiales",
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
      fig_left = 0.775,
      fig_right = 0.992,
      max_rows = 16L,
      line_label = "Red hidrográfica",
      line_color = RIVER_COLOR,
      line_lwd = 1.9
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
      labels = layer_source_map_label(
        c(CUENCAS_STEM, RIOS_STEM)
      ),
      adj = c(1, 0),
      cex = 0.66,
      col = "grey30"
    )

    invisible(NULL)
  }


  # ==========================================================
  # 8. DT
  # ==========================================================

  cuencas_datatable <- function(tab) {
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
            filename = "unidades_hidrograficas_cuenca"
          ),
          list(
            extend = "excel",
            text = "Excel",
            filename = "unidades_hidrograficas_cuenca"
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
  # 9. UI
  # ==========================================================

  ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".ctc-wrap{padding:14px 18px 28px 18px;max-width:1500px;margin:auto;}",
            ".ctc-head{display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:12px;}",
            ".ctc-note{background:#f6f7f8;border-left:4px solid #607D8B;padding:10px 12px;margin:10px 0 14px 0;}",".ctc-interpret{background:#f3f8fc;border-left:4px solid #2F6F9F;padding:11px 13px;margin:0 0 16px 0;line-height:1.45;}",
            ".ctc-card{border:1px solid #ddd;border-radius:7px;padding:12px 12px 18px 12px;background:white;margin-bottom:16px;}",
            ".ctc-card h4{margin-top:0;}",
            ".ctc-a3-frame{width:1188px;max-width:100%;margin:0 auto;aspect-ratio:420/297;}",
            ".ctc-table-wrap{width:100%;overflow-x:auto;padding-bottom:14px;}",
            ".ctc-table-wrap .dataTables_wrapper{width:100%;}",
            ".ctc-table-wrap table.dataTable{width:100%!important;}"
          )
        )
      ),

      shiny::div(
        class = "ctc-wrap",

        shiny::div(
          class = "ctc-head",

          shiny::actionButton(
            ns("analizar"),
            "Cargar cuencas y ríos",
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
          class = "ctc-note",
          "Las unidades hidrográficas oficiales se recortan a la cuenca activa y se enlazan por CODIGO con la metadata normalizada. La red hidrográfica es una capa de apoyo: los ríos se etiquetan con prioridad y las quebradas solo se muestran de forma restringida para evitar saturación visual."
        ),

        layer_source_ui(c(CUENCAS_STEM, RIOS_STEM)),

        shiny::conditionalPanel(
          condition = paste0(
            "output['",
            ns("has_results"),
            "'] === 'true'"
          ),

          shiny::div(
            class = "ctc-card",
            shiny::tags$h4("Mapa de cuencas y red hidrográfica"),
            shiny::div(
              class = "ctc-a3-frame",
              shiny::plotOutput(
                ns("mapa"),
                width = "100%",
                height = "100%"
              )
            )
          ),

          shiny::uiOutput(
            ns("interpretacion")
          ),

          shiny::div(
            class = "ctc-card",
            shiny::tags$h4("Unidades hidrográficas presentes"),
            shiny::div(
              class = "ctc-table-wrap",
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
  # 10. SERVER
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
          "Delimita o carga una cuenca para consultar el contexto hidrográfico."
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
              "Cuenca disponible. Pulsa Cargar cuencas y ríos."
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
                  "Cargando unidades hidrográficas oficiales..."
                )

                spatial <- read_cuencas_for_basin(
                  b
                )

                status(
                  "Cargando y recortando la red hidrográfica..."
                )

                river_info <- read_rivers_for_basin(
                  b
                )

                status(
                  "Calculando áreas y enlazando metadata..."
                )

                tab_info <- build_cuencas_table(
                  cuencas = spatial$cuencas,
                  basin = b
                )

                result(
                  list(
                    basin = b,
                    basin_source = if (!is.null(basin_source)) basin_source() else NULL,
                    basin_label = if (!is.null(basin_label)) basin_label() else NULL,
                    cuencas = spatial$cuencas,
                    rivers = river_info$rivers,
                    river_label_field = river_info$label_field,
                    table = tab_info$table,
                    basin_area_km2 = tab_info$basin_area_km2,
                    epsg = tab_info$epsg,
                    source_mode = spatial$source_mode,
                    selected_tiles = spatial$selected_tiles,
                    river_source_mode = river_info$source_mode,
                    river_selected_tiles = river_info$selected_tiles
                  )
                )

                status(
                  paste0(
                    "Contexto hidrográfico listo: ",
                    nrow(tab_info$table),
                    " unidades | ",
                    nrow(river_info$rivers),
                    " tramos de río",
                    if (!is.null(river_info$label_field)) {
                      paste0(" | etiquetas: ", river_info$label_field)
                    } else {
                      " | sin campo nominal de río"
                    }
                  )
                )
              },
              error = function(e) {
                result(NULL)
                msg <- paste0(
                  "Error en Cuencas: ",
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
          draw_cuencas_map(x)
        }, res = 120)

        output$interpretacion <- shiny::renderUI({
          x <- result()
          shiny::req(x)

          tab <- x$table

          if (is.null(tab) || nrow(tab) == 0L) {
            return(NULL)
          }

          principal <- tab[
            which.max(
              tab$CUENCA_PCT
            ),
            ,
            drop = FALSE
          ]

          nombre <- principal$DESCRIPCION[1L]
          pct <- principal$CUENCA_PCT[1L]

          if (
            is.na(nombre) ||
            !nzchar(trimws(nombre))
          ) {
            nombre <- principal$CODIGO[1L]
          }

          pct_txt <- if (is.finite(pct)) {
            paste0(
              formatC(
                pct,
                format = "f",
                digits = 2
              ),
              "%"
            )
          } else {
            "porcentaje no disponible"
          }

          if (
            nrow(tab) == 1L
          ) {
            msg <- paste0(
              "La cuenca delimitada coincide con la unidad hidrográfica oficial ",
              nombre,
              " (",
              pct_txt,
              ")."
            )
          } else if (
            is.finite(pct) &&
            pct >= 90
          ) {
            msg <- paste0(
              "Interpretación: la cuenca delimitada corresponde principalmente a ",
              nombre,
              " (",
              pct_txt,
              "). Las intersecciones menores con unidades vecinas pueden aparecer ",
              "por diferencias entre la delimitación derivada del DEM y los límites ",
              "oficiales de las unidades hidrográficas."
            )
          } else {
            msg <- paste0(
              "Interpretación: la mayor intersección corresponde a ",
              nombre,
              " (",
              pct_txt,
              "), pero la delimitación cruza varias unidades hidrográficas oficiales. ",
              "Revise el mapa y los porcentajes antes de asumir una correspondencia única."
            )
          }

          shiny::div(
            class = "ctc-interpret",
            shiny::HTML(
              msg
            )
          )
        })


        output$tabla <- DT::renderDT({
          x <- result()
          shiny::req(x)
          cuencas_datatable(
            x$table
          )
        }, server = FALSE)

        output$descargar_png <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "mapa_cuencas_rios_",
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
                "No hay un mapa de cuencas calculado para descargar."
              )
            }

            if (requireNamespace(
              "ragg",
              quietly = TRUE
            )) {

              ragg::agg_png(
                filename = file,
                width = 420,
                height = 297,
                units = "mm",
                res = 300,
                background = "white"
              )

            } else {

              grDevices::png(
                filename = file,
                width = 420 / 25.4 * 300,
                height = 297 / 25.4 * 300,
                units = "px",
                res = 300,
                type = if (
                  capabilities("cairo")
                ) {
                  "cairo"
                } else {
                  getOption(
                    "bitmapType",
                    "windows"
                  )
                }
              )
            }

            on.exit(
              grDevices::dev.off(),
              add = TRUE
            )

            draw_cuencas_map(x)
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
              "contexto_hidrografico_normalizado_recortado_",
              format(Sys.Date(), "%Y%m%d"),
              ".zip"
            )
          },
          content = function(file) {
            x <- shiny::isolate(result())
            if (is.null(x)) stop("No hay contexto hidrografico calculado para exportar.")

            cu_mosaic <- read_vector_export_mosaic(
              selected_files = x$selected_tiles,
              source_mode = x$source_mode,
              basin = x$basin
            )

            rv_mosaic <- read_vector_export_mosaic(
              selected_files = x$river_selected_tiles,
              source_mode = x$river_source_mode,
              basin = x$basin
            )

            cu_clip <- clip_vector_export_to_basin(
              cu_mosaic,
              x$basin
            )

            rv_clip <- clip_vector_export_to_basin(
              rv_mosaic,
              x$basin
            )

            write_shapefile_zip_bundle(
              layers = list(
                cuencas_recortado = cu_clip,
                rios_recortado = rv_clip
              ),
              target_file = file,
              bundle_stem = "contexto_hidrografico_normalizado_recortado"
            )
          }
        )

        output$descargar_shp_mosaico <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "contexto_hidrografico_normalizado_mosaico_",
              format(Sys.Date(), "%Y%m%d"),
              ".zip"
            )
          },
          content = function(file) {
            x <- shiny::isolate(result())
            if (is.null(x)) stop("No hay contexto hidrografico calculado para exportar.")

            cu_mosaic <- read_vector_export_mosaic(
              selected_files = x$selected_tiles,
              source_mode = x$source_mode,
              basin = x$basin
            )

            rv_mosaic <- read_vector_export_mosaic(
              selected_files = x$river_selected_tiles,
              source_mode = x$river_source_mode,
              basin = x$basin
            )

            write_shapefile_zip_bundle(
              layers = list(
                cuencas_mosaico = cu_mosaic,
                rios_mosaico = rv_mosaic
              ),
              target_file = file,
              bundle_stem = "contexto_hidrografico_normalizado_mosaico"
            )
          }
        )

        shiny::outputOptions(output, "descargar_shp_recortado", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "descargar_shp_mosaico", suspendWhenHidden = FALSE)

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

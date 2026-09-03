# ============================================================
# R/poblados.R
#
# FABDEM Watershed Explorer
# MODULO 05C: CENTROS POBLADOS
# v2: inventario punto-en-poligono + mapa A3 + tabla territorial + normalizacion UTF-8
# ============================================================

poblados <- local({

  POBLADOS_STEM <- "poblados_normalizada"
  DISTRITOS_STEM <- "distrital_normalizada_disuelto"

  POINT_FILL <- "#C0392B"
  POINT_BORDER <- "#7B241C"
  BASIN_FILL <- "#F8FBFD"
  DISTRICT_BORDER <- "#C4C9CD"


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


  normalize_utf8_text <- function(x) {
    x <- as.character(x)

    if (length(x) == 0L) {
      return(x)
    }

    # Primero conserva las cadenas que ya contienen UTF-8 valido.
    # Algunos nombres del inventario pueden arrastrar bytes CP1252/Latin-1
    # desde la fuente original; graphics::nchar/strwidth/text no toleran
    # esos bytes cuando el servidor trabaja en UTF-8.
    out <- suppressWarnings(
      iconv(
        x,
        from = "UTF-8",
        to = "UTF-8",
        sub = NA
      )
    )

    bad <- !is.na(x) & is.na(out)

    if (any(bad)) {
      out[bad] <- suppressWarnings(
        iconv(
          x[bad],
          from = "CP1252",
          to = "UTF-8",
          sub = ""
        )
      )
    }

    bad <- !is.na(x) & is.na(out)

    if (any(bad)) {
      out[bad] <- suppressWarnings(
        iconv(
          x[bad],
          from = "latin1",
          to = "UTF-8",
          sub = ""
        )
      )
    }

    # Salvaguarda final: nunca se entrega una cadena invalida a graphics/DT.
    bad <- !is.na(x) & is.na(out)

    if (any(bad)) {
      out[bad] <- ""
    }

    Encoding(out[!is.na(out)]) <- "UTF-8"

    out
  }


  clean_text <- function(x) {
    x <- normalize_utf8_text(x)

    x <- trimws(x)

    x[
      is.na(x) |
        !nzchar(x) |
        toupper(x) %in% c(
          "NA",
          "N/A",
          "NULL"
        )
    ] <- NA_character_

    x
  }


  spanish_sentence_case <- function(x) {
    x <- clean_text(x)

    out <- rep(
      NA_character_,
      length(x)
    )

    minor <- c(
      "de",
      "del",
      "la",
      "las",
      "los",
      "el",
      "y",
      "e",
      "en",
      "al"
    )

    for (i in seq_along(x)) {
      s <- x[i]

      if (
        is.na(s) ||
        !nzchar(s)
      ) {
        next
      }

      words <- strsplit(
        tolower(s),
        "\\s+"
      )[[1]]

      words_out <- character(
        length(words)
      )

      for (j in seq_along(words)) {
        w <- words[j]

        if (
          j > 1L &&
          w %in% minor
        ) {
          words_out[j] <- w
        } else {
          parts <- strsplit(
            w,
            "-",
            fixed = TRUE
          )[[1]]

          parts <- vapply(
            parts,
            function(p) {
              if (!nzchar(p)) {
                return(p)
              }

              paste0(
                toupper(
                  substr(
                    p,
                    1,
                    1
                  )
                ),
                substr(
                  p,
                  2,
                  nchar(p)
                )
              )
            },
            character(1)
          )

          words_out[j] <- paste(
            parts,
            collapse = "-"
          )
        }
      }

      out[i] <- paste(
        words_out,
        collapse = " "
      )
    }

    out
  }


  basin_utm_epsg <- function(basin) {
    b4326 <- sf::st_transform(
      basin,
      4326
    )

    center <- suppressWarnings(
      sf::st_centroid(
        sf::st_union(
          sf::st_geometry(
            b4326
          )
        )
      )
    )

    xy <- sf::st_coordinates(
      center
    )[1, ]

    lon <- xy[1]
    lat <- xy[2]

    zone <- floor(
      (lon + 180) / 6
    ) + 1

    zone <- max(
      1,
      min(
        60,
        zone
      )
    )

    if (lat < 0) {
      32700L + zone
    } else {
      32600L + zone
    }
  }


  poblados_file <- function() {
    file.path(
      CONTEXTO_TERRITORIAL_DIR,
      paste0(
        POBLADOS_STEM,
        ".gpkg"
      )
    )
  }


  distritos_file <- function() {
    file.path(
      CONTEXTO_TERRITORIAL_DIR,
      paste0(
        DISTRITOS_STEM,
        ".gpkg"
      )
    )
  }


  # ==========================================================
  # 2. LECTURA DE CENTROS POBLADOS
  # ==========================================================

  poblados_cache <- new.env(
    parent = emptyenv()
  )


  read_poblados_source <- function() {
    key <- "poblados"

    if (exists(
      key,
      envir = poblados_cache,
      inherits = FALSE
    )) {
      return(
        get(
          key,
          envir = poblados_cache,
          inherits = FALSE
        )
      )
    }

    path <- poblados_file()

    if (!file_nonempty_local(path)) {
      stop(
        paste0(
          "No se encontro la capa de centros poblados:\n",
          path
        )
      )
    }

    layers <- sf::st_layers(
      path
    )$name

    if (length(layers) < 1L) {
      stop(
        "El GPKG de centros poblados no contiene capas vectoriales."
      )
    }

    chosen <- NULL

    for (nm in layers) {
      g <- try(
        sf::st_read(
          path,
          layer = nm,
          quiet = TRUE,
          stringsAsFactors = FALSE
        ),
        silent = TRUE
      )

      if (
        inherits(
          g,
          "try-error"
        ) ||
        !inherits(
          g,
          "sf"
        ) ||
        nrow(g) == 0L
      ) {
        next
      }

      required <- c(
        "DEP",
        "PROV",
        "DIST",
        "CODIGO",
        "NOMBRE"
      )

      if (!all(
        required %in% names(g)
      )) {
        next
      }

      gt <- as.character(
        sf::st_geometry_type(
          g,
          by_geometry = TRUE
        )
      )

      keep_point <- gt %in% c(
        "POINT",
        "MULTIPOINT"
      )

      if (!any(keep_point)) {
        next
      }

      chosen <- g[
        keep_point,
        ,
        drop = FALSE
      ]

      break
    }

    if (is.null(chosen)) {
      stop(
        paste0(
          "No se encontro una capa de puntos con los campos ",
          "DEP, PROV, DIST, CODIGO y NOMBRE."
        )
      )
    }

    if (is.na(
      sf::st_crs(
        chosen
      )
    )) {
      stop(
        "La capa de centros poblados no tiene CRS valido."
      )
    }

    chosen <- chosen[
      !sf::st_is_empty(
        chosen
      ),
      ,
      drop = FALSE
    ]

    chosen$CODIGO <- clean_text(
      chosen$CODIGO
    )

    chosen$DEP <- spanish_sentence_case(
      chosen$DEP
    )

    chosen$PROV <- spanish_sentence_case(
      chosen$PROV
    )

    chosen$DIST <- spanish_sentence_case(
      chosen$DIST
    )

    # NOMBRE se conserva tal como viene, salvo limpieza de vacíos.
    chosen$NOMBRE <- clean_text(
      chosen$NOMBRE
    )

    assign(
      key,
      chosen,
      envir = poblados_cache
    )

    chosen
  }


  # ==========================================================
  # 3. PUNTO EN POLIGONO
  # ==========================================================

  build_poblados_result <- function(basin) {
    points <- read_poblados_source()

    basin_local <- sf::st_transform(
      sf::st_make_valid(
        basin
      ),
      sf::st_crs(
        points
      )
    )

    basin_union <- sf::st_union(
      sf::st_geometry(
        basin_local
      )
    )

    # st_intersects incluye puntos sobre el borde de la cuenca.
    inside <- lengths(
      sf::st_intersects(
        points,
        basin_union
      )
    ) > 0L

    hit <- points[
      inside,
      ,
      drop = FALSE
    ]

    if (nrow(hit) == 0L) {
      return(
        list(
          points = sf::st_transform(
            hit,
            4326
          ),
          table = data.frame(
            CODIGO = character(0),
            NOMBRE = character(0),
            DIST = character(0),
            PROV = character(0),
            DEP = character(0),
            LONGITUD = numeric(0),
            LATITUD = numeric(0),
            stringsAsFactors = FALSE
          ),
          n_districts = 0L,
          n_provinces = 0L,
          n_departments = 0L,
          concentration_district = NA_character_,
          concentration_count = 0L,
          epsg = basin_utm_epsg(
            basin
          )
        )
      )
    }

    hit <- sf::st_transform(
      hit,
      4326
    )

    coords <- sf::st_coordinates(
      sf::st_geometry(
        hit
      )
    )

    # El GPKG esperado es POINT. Para una eventual MULTIPOINT se
    # conserva la primera coordenada de cada entidad.
    if (
      nrow(coords) != nrow(hit) &&
      "L1" %in% colnames(coords)
    ) {
      idx_first <- !duplicated(
        coords[
          ,
          "L1"
        ]
      )

      coords2 <- coords[
        idx_first,
        ,
        drop = FALSE
      ]

      if (nrow(coords2) == nrow(hit)) {
        coords <- coords2
      }
    }

    if (nrow(coords) != nrow(hit)) {
      stop(
        "No fue posible obtener una coordenada unica por centro poblado."
      )
    }

    tab <- sf::st_drop_geometry(
      hit
    )

    tab$LONGITUD <- coords[
      ,
      "X"
    ]

    tab$LATITUD <- coords[
      ,
      "Y"
    ]

    tab <- tab[
      ,
      c(
        "CODIGO",
        "NOMBRE",
        "DIST",
        "PROV",
        "DEP",
        "LONGITUD",
        "LATITUD"
      ),
      drop = FALSE
    ]

    tab <- tab[
      order(
        tab$DEP,
        tab$PROV,
        tab$DIST,
        tab$NOMBRE,
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]

    district_counts <- sort(
      table(
        hit$DIST[
          !is.na(hit$DIST) &
            nzchar(hit$DIST)
        ]
      ),
      decreasing = TRUE
    )

    concentration_district <- if (
      length(district_counts) > 0L
    ) {
      names(
        district_counts
      )[1L]
    } else {
      NA_character_
    }

    concentration_count <- if (
      length(district_counts) > 0L
    ) {
      as.integer(
        district_counts[1L]
      )
    } else {
      0L
    }

    list(
      points = hit,
      table = tab,
      n_districts = length(
        unique(
          hit$DIST[
            !is.na(hit$DIST) &
              nzchar(hit$DIST)
          ]
        )
      ),
      n_provinces = length(
        unique(
          hit$PROV[
            !is.na(hit$PROV) &
              nzchar(hit$PROV)
          ]
        )
      ),
      n_departments = length(
        unique(
          hit$DEP[
            !is.na(hit$DEP) &
              nzchar(hit$DEP)
          ]
        )
      ),
      concentration_district = concentration_district,
      concentration_count = concentration_count,
      epsg = basin_utm_epsg(
        basin
      )
    )
  }


  # ==========================================================
  # 4. FONDO DISTRITAL TENUE
  # ==========================================================

  district_cache <- new.env(
    parent = emptyenv()
  )


  read_district_background <- function() {
    key <- "district_bg"

    if (exists(
      key,
      envir = district_cache,
      inherits = FALSE
    )) {
      return(
        get(
          key,
          envir = district_cache,
          inherits = FALSE
        )
      )
    }

    path <- distritos_file()

    if (!file_nonempty_local(path)) {
      return(NULL)
    }

    layers <- sf::st_layers(
      path
    )$name

    chosen <- NULL

    for (nm in layers) {
      g <- try(
        sf::st_read(
          path,
          layer = nm,
          quiet = TRUE,
          stringsAsFactors = FALSE
        ),
        silent = TRUE
      )

      if (
        inherits(
          g,
          "try-error"
        ) ||
        !inherits(
          g,
          "sf"
        ) ||
        nrow(g) == 0L
      ) {
        next
      }

      gt <- as.character(
        sf::st_geometry_type(
          g,
          by_geometry = TRUE
        )
      )

      keep <- gt %in% c(
        "POLYGON",
        "MULTIPOLYGON"
      )

      if (!any(keep)) {
        next
      }

      chosen <- g[
        keep,
        ,
        drop = FALSE
      ]

      break
    }

    if (
      is.null(chosen) ||
      nrow(chosen) == 0L ||
      is.na(
        sf::st_crs(
          chosen
        )
      )
    ) {
      return(NULL)
    }

    chosen <- sf::st_transform(
      sf::st_make_valid(
        chosen
      ),
      4326
    )

    assign(
      key,
      chosen,
      envir = district_cache
    )

    chosen
  }


  visible_district_background <- function(
      xlim,
      ylim
  ) {
    bg <- read_district_background()

    if (is.null(bg)) {
      return(NULL)
    }

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

    keep <- lengths(
      sf::st_intersects(
        bg,
        frame
      )
    ) > 0L

    bg[
      keep,
      ,
      drop = FALSE
    ]
  }


  # ==========================================================
  # 5. ETIQUETAS CON ANTICOLISION
  # ==========================================================

  select_population_labels <- function(
      points,
      ux,
      uy
  ) {
    if (
      is.null(points) ||
      nrow(points) == 0L
    ) {
      return(NULL)
    }

    keep_name <- !is.na(
      points$NOMBRE
    ) & nzchar(
      points$NOMBRE
    )

    p <- points[
      keep_name,
      ,
      drop = FALSE
    ]

    if (nrow(p) == 0L) {
      return(NULL)
    }

    xy <- sf::st_coordinates(
      sf::st_geometry(
        p
      )
    )

    if (nrow(xy) != nrow(p)) {
      return(NULL)
    }

    labels <- data.frame(
      LABEL = p$NOMBRE,
      X = xy[
        ,
        "X"
      ],
      Y = xy[
        ,
        "Y"
      ],
      stringsAsFactors = FALSE
    )

    # Menos texto cuanto mayor sea el inventario.
    cex <- if (
      nrow(p) <= 25L
    ) {
      0.62
    } else if (
      nrow(p) <= 75L
    ) {
      0.52
    } else {
      0.45
    }

    # Primero nombres cortos: ocupan menos espacio y permiten
    # distribuir más información sin crear una jerarquía falsa.
    ord <- order(
      nchar(
        labels$LABEL
      ),
      labels$LABEL
    )

    labels <- labels[
      ord,
      ,
      drop = FALSE
    ]

    boxes <- vector(
      "list",
      nrow(labels)
    )

    keep <- logical(
      nrow(labels)
    )

    nbox <- 0L

    for (i in seq_len(
      nrow(labels)
    )) {
      w <- graphics::strwidth(
        labels$LABEL[i],
        units = "user",
        cex = cex,
        font = 1,
        family = "sans"
      )

      h <- graphics::strheight(
        labels$LABEL[i],
        units = "user",
        cex = cex,
        font = 1,
        family = "sans"
      )

      # Etiqueta ligeramente arriba/derecha del punto.
      x_text <- labels$X[i] +
        0.0045 *
        ux

      y_text <- labels$Y[i] +
        0.0040 *
        uy

      box <- c(
        xmin = x_text -
          0.0015 *
          ux,
        xmax = x_text +
          w +
          0.0015 *
          ux,
        ymin = y_text -
          h / 2 -
          0.0015 *
          uy,
        ymax = y_text +
          h / 2 +
          0.0015 *
          uy
      )

      overlap <- FALSE

      if (nbox > 0L) {
        for (j in seq_len(
          nbox
        )) {
          b <- boxes[[j]]

          if (!(
            box["xmax"] < b["xmin"] ||
              box["xmin"] > b["xmax"] ||
              box["ymax"] < b["ymin"] ||
              box["ymin"] > b["ymax"]
          )) {
            overlap <- TRUE
            break
          }
        }
      }

      if (!overlap) {
        keep[i] <- TRUE
        nbox <- nbox + 1L
        boxes[[nbox]] <- box
        labels$X_TEXT[i] <- x_text
        labels$Y_TEXT[i] <- y_text
      }
    }

    out <- labels[
      keep,
      ,
      drop = FALSE
    ]

    if (nrow(out) == 0L) {
      return(NULL)
    }

    out$CEX <- cex
    out
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
        list(
          ring
        )
      ),
      crs = crs
    )
  }


  box_overlap_fraction <- function(
      box,
      basin,
      epsg
  ) {
    bx <- sf::st_transform(
      box_polygon(
        box
      ),
      epsg
    )

    basin_utm <- sf::st_transform(
      basin,
      epsg
    )

    bx_area <- as.numeric(
      sf::st_area(
        bx
      )
    )

    if (
      !is.finite(bx_area) ||
      bx_area <= 0
    ) {
      return(Inf)
    }

    inter <- suppressWarnings(
      sf::st_intersection(
        bx,
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
        sf::st_area(
          inter
        )
      ),
      na.rm = TRUE
    ) /
      bx_area
  }


  boxes_intersect <- function(
      a,
      b
  ) {
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

    for (i in seq_along(
      candidates
    )) {
      scores[i] <- 1000 *
        box_overlap_fraction(
          candidates[[i]]$box,
          basin,
          epsg
        ) +
        candidates[[i]]$preference

      if (
        !is.null(reserved) &&
        boxes_intersect(
          candidates[[i]]$box,
          reserved
        )
      ) {
        scores[i] <- scores[i] +
          5000
      }
    }

    candidates[[
      which.min(
        scores
      )
    ]]
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


  nice_scale_length_m <- function(total_m) {
    if (
      !is.finite(
        total_m
      ) ||
      total_m <= 0
    ) {
      return(
        1000
      )
    }

    target <- total_m /
      3

    pow10 <- 10 ^
      floor(
        log10(
          target
        )
      )

    candidates <- c(
      1,
      2,
      5
    ) *
      pow10

    candidates <- candidates[
      candidates <=
        target
    ]

    if (length(candidates) == 0L) {
      return(
        pow10
      )
    }

    max(
      candidates
    )
  }


  draw_poblados_map <- function(x) {
    basin <- sf::st_transform(
      x$basin,
      4326
    )

    points <- sf::st_transform(
      x$points,
      4326
    )

    epsg <- x$epsg

    old_par <- graphics::par(
      no.readonly = TRUE
    )

    on.exit(
      graphics::par(
        old_par
      ),
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

    graphics::rect(
      0.01,
      0.01,
      0.99,
      0.99,
      col = "white",
      border = "#595959",
      lwd = 1
    )

    graphics::par(
      fig = c(
        0.025,
        0.755,
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

    bb <- sf::st_bbox(
      basin
    )

    dx <- as.numeric(
      bb["xmax"] -
        bb["xmin"]
    )

    dy <- as.numeric(
      bb["ymax"] -
        bb["ymin"]
    )

    xlim0 <- c(
      as.numeric(
        bb["xmin"]
      ) -
        0.07 *
        dx,
      as.numeric(
        bb["xmax"]
      ) +
        0.07 *
        dx
    )

    ylim0 <- c(
      as.numeric(
        bb["ymin"]
      ) -
        0.07 *
        dy,
      as.numeric(
        bb["ymax"]
      ) +
        0.07 *
        dy
    )

    mid_lat <- mean(
      ylim0
    )

    map_asp <- 1 /
      cos(
        mid_lat *
          pi /
          180
      )

    graphics::plot.new()

    graphics::plot.window(
      xlim = xlim0,
      ylim = ylim0,
      xaxs = "i",
      yaxs = "i",
      asp = map_asp
    )

    usr <- graphics::par(
      "usr"
    )

    map_xlim <- usr[1:2]
    map_ylim <- usr[3:4]
    ux <- usr[2] -
      usr[1]
    uy <- usr[4] -
      usr[3]

    fig_now <- graphics::par(
      "fig"
    )

    plt_now <- graphics::par(
      "plt"
    )

    fig_h <- fig_now[4] -
      fig_now[3]

    map_plot_bottom <- fig_now[3] +
      plt_now[3] *
      fig_h

    map_plot_top <- fig_now[3] +
      plt_now[4] *
      fig_h

    graphics::rect(
      map_xlim[1],
      map_ylim[1],
      map_xlim[2],
      map_ylim[2],
      col = "white",
      border = NA
    )

    bg <- tryCatch(
      visible_district_background(
        map_xlim,
        map_ylim
      ),
      error = function(e) {
        NULL
      }
    )

    if (
      !is.null(bg) &&
      nrow(bg) > 0L
    ) {
      graphics::plot(
        sf::st_geometry(
          bg
        ),
        add = TRUE,
        col = "#FCFCFA",
        border = DISTRICT_BORDER,
        lwd = 0.50
      )
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
        alpha.f = 0.24
      ),
      lwd = 0.50
    )

    graphics::plot(
      sf::st_geometry(
        basin
      ),
      add = TRUE,
      col = grDevices::adjustcolor(
        BASIN_FILL,
        alpha.f = 0.58
      ),
      border = NA
    )

    if (nrow(points) > 0L) {
      graphics::points(
        sf::st_coordinates(
          sf::st_geometry(
            points
          )
        )[
          ,
          "X"
        ],
        sf::st_coordinates(
          sf::st_geometry(
            points
          )
        )[
          ,
          "Y"
        ],
        pch = 21,
        bg = grDevices::adjustcolor(
          POINT_FILL,
          alpha.f = 0.88
        ),
        col = POINT_BORDER,
        cex = 0.72,
        lwd = 0.55
      )

      labels <- select_population_labels(
        points,
        ux,
        uy
      )

      if (!is.null(labels)) {
        graphics::text(
          labels$X_TEXT,
          labels$Y_TEXT,
          labels = labels$LABEL,
          col = "#3A3A3A",
          cex = labels$CEX,
          font = 1,
          family = "sans",
          adj = c(
            0,
            0.5
          )
        )
      }
    }

    graphics::plot(
      sf::st_geometry(
        basin
      ),
      add = TRUE,
      border = "#111111",
      lwd = 2.1
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

    # Norte dinámico.
    nw <- 0.022 *
      ux

    nh <- 0.045 *
      uy

    north_w <- 0.080 *
      ux

    north_h <- 0.115 *
      uy

    north_positions <- list(
      list(
        name = "top_left",
        preference = 0,
        box = c(
          xmin = usr[1] +
            0.020 *
            ux,
          xmax = usr[1] +
            0.020 *
            ux +
            north_w,
          ymin = usr[4] -
            0.020 *
            uy -
            north_h,
          ymax = usr[4] -
            0.020 *
            uy
        )
      ),
      list(
        name = "top_right",
        preference = 10,
        box = c(
          xmin = usr[2] -
            0.020 *
            ux -
            north_w,
          xmax = usr[2] -
            0.020 *
            ux,
          ymin = usr[4] -
            0.020 *
            uy -
            north_h,
          ymax = usr[4] -
            0.020 *
            uy
        )
      ),
      list(
        name = "bottom_left",
        preference = 20,
        box = c(
          xmin = usr[1] +
            0.020 *
            ux,
          xmax = usr[1] +
            0.020 *
            ux +
            north_w,
          ymin = usr[3] +
            0.020 *
            uy,
          ymax = usr[3] +
            0.020 *
            uy +
            north_h
        )
      ),
      list(
        name = "bottom_right",
        preference = 30,
        box = c(
          xmin = usr[2] -
            0.020 *
            ux -
            north_w,
          xmax = usr[2] -
            0.020 *
            ux,
          ymin = usr[3] +
            0.020 *
            uy,
          ymax = usr[3] +
            0.020 *
            uy +
            north_h
        )
      )
    )

    north_choice <- choose_box(
      north_positions,
      basin,
      epsg
    )

    nb <- north_choice$box

    nx <- mean(
      nb[
        c(
          "xmin",
          "xmax"
        )
      ]
    )

    ny_top <- nb["ymax"] -
      0.018 *
      uy

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
      cex = 1
    )

    arrow_top <- ny_top -
      0.012 *
      uy

    graphics::polygon(
      x = c(
        nx,
        nx -
          nw,
        nx,
        nx +
          nw
      ),
      y = c(
        arrow_top,
        arrow_top -
          nh,
        arrow_top -
          0.014 *
          uy,
        arrow_top -
          nh
      ),
      col = "black",
      border = "black"
    )

    # Escala dinámica, solo esquinas.
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
      usr[1] +
        1,
      mid_lat
    )

    scale_dx <- scale_m /
      m_per_deg

    scale_box_w <- scale_dx +
      0.100 *
      ux

    scale_box_h <- 0.085 *
      uy

    make_scale_box <- function(
        xmin,
        ymin
    ) {
      c(
        xmin = xmin,
        xmax = xmin +
          scale_box_w,
        ymin = ymin,
        ymax = ymin +
          scale_box_h
      )
    }

    scale_positions <- list(
      list(
        name = "bottom_left",
        preference = 0,
        box = make_scale_box(
          usr[1] +
            0.020 *
            ux,
          usr[3] +
            0.020 *
            uy
        )
      ),
      list(
        name = "bottom_right",
        preference = 8,
        box = make_scale_box(
          usr[2] -
            0.020 *
            ux -
            scale_box_w,
          usr[3] +
            0.020 *
            uy
        )
      ),
      list(
        name = "top_left",
        preference = 12,
        box = make_scale_box(
          usr[1] +
            0.020 *
            ux,
          usr[4] -
            0.020 *
            uy -
            scale_box_h
        )
      ),
      list(
        name = "top_right",
        preference = 16,
        box = make_scale_box(
          usr[2] -
            0.020 *
            ux -
            scale_box_w,
          usr[4] -
            0.020 *
            uy -
            scale_box_h
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

    sx0 <- sb["xmin"] +
      0.025 *
      ux

    sy0 <- sb["ymin"] +
      0.040 *
      uy

    bar_h <- 0.018 *
      uy

    nseg <- 4L
    seg_dx <- scale_dx /
      nseg

    for (i in 0:(nseg - 1L)) {
      graphics::rect(
        sx0 +
          i *
          seg_dx,
        sy0,
        sx0 +
          (i + 1L) *
          seg_dx,
        sy0 +
          bar_h,
        col = if (
          i %%
            2L ==
            0L
        ) {
          "black"
        } else {
          "white"
        },
        border = "black",
        lwd = 0.8
      )
    }

    for (i in 0:nseg) {
      graphics::text(
        sx0 +
          i *
          seg_dx,
        sy0 -
          0.014 *
          uy,
        labels = format(
          round(
            scale_m *
              i /
              nseg /
              1000,
            1
          ),
          trim = TRUE,
          scientific = FALSE
        ),
        cex = 0.78
      )
    }

    graphics::text(
      sx0 +
        scale_dx +
        0.018 *
        ux,
      sy0 -
        0.014 *
        uy,
      labels = "km",
      cex = 0.78,
      adj = c(
        0,
        0.5
      )
    )

    # Panel lateral.
    graphics::par(
      fig = c(
        0.775,
        0.992,
        map_plot_bottom,
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
      0.04,
      0.975,
      labels = "Centros poblados",
      adj = c(
        0,
        1
      ),
      font = 2,
      cex = 1.12
    )

    graphics::text(
      0.04,
      0.925,
      labels = paste0(
        nrow(
          x$table
        ),
        " centros poblados"
      ),
      adj = c(
        0,
        1
      ),
      cex = 0.70,
      col = "grey30"
    )

    graphics::text(
      0.04,
      0.885,
      labels = paste0(
        x$n_districts,
        " distritos | ",
        x$n_provinces,
        " provincias"
      ),
      adj = c(
        0,
        1
      ),
      cex = 0.62,
      col = "grey40"
    )

    graphics::text(
      0.04,
      0.850,
      labels = paste0(
        x$n_departments,
        " departamentos"
      ),
      adj = c(
        0,
        1
      ),
      cex = 0.62,
      col = "grey40"
    )

    graphics::points(
      0.10,
      0.765,
      pch = 21,
      bg = POINT_FILL,
      col = POINT_BORDER,
      cex = 1.05,
      lwd = 0.7
    )

    graphics::text(
      0.18,
      0.765,
      labels = "Centro poblado",
      adj = c(
        0,
        0.5
      ),
      cex = 0.68
    )

    graphics::segments(
      0.05,
      0.705,
      0.18,
      0.705,
      col = "#111111",
      lwd = 2.0
    )

    graphics::text(
      0.23,
      0.705,
      labels = "Cuenca activa",
      adj = c(
        0,
        0.5
      ),
      cex = 0.66
    )

    graphics::segments(
      0.05,
      0.650,
      0.18,
      0.650,
      col = DISTRICT_BORDER,
      lwd = 0.8
    )

    graphics::text(
      0.23,
      0.650,
      labels = "Límite distrital",
      adj = c(
        0,
        0.5
      ),
      cex = 0.66
    )

    graphics::text(
      0.04,
      0.565,
      labels = "Concentración",
      adj = c(
        0,
        1
      ),
      font = 2,
      cex = 0.76
    )

    if (
      !is.na(
        x$concentration_district
      )
    ) {
      info_lines <- c(
        x$concentration_district,
        paste0(
          x$concentration_count,
          " centros poblados"
        )
      )
    } else {
      info_lines <- c(
        "Sin distrito nominal",
        "0 centros poblados"
      )
    }

    graphics::text(
      rep(
        0.04,
        length(info_lines)
      ),
      c(
        0.525,
        0.487
      ),
      labels = info_lines,
      adj = c(
        0,
        1
      ),
      cex = 0.64,
      col = "grey25"
    )

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
      0.025,
      labels = layer_source_map_label(POBLADOS_STEM),
      adj = c(
        1,
        0
      ),
      cex = 0.64,
      col = "grey30"
    )

    invisible(NULL)
  }


  # ==========================================================
  # 7. TABLA
  # ==========================================================

  poblados_datatable <- function(tab) {
    out <- tab

    out$LONGITUD <- round(
      out$LONGITUD,
      6
    )

    out$LATITUD <- round(
      out$LATITUD,
      6
    )

    names(out) <- c(
      "Código",
      "Centro poblado",
      "Distrito",
      "Provincia",
      "Departamento",
      "Longitud",
      "Latitud"
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
            filename = "centros_poblados_cuenca"
          ),
          list(
            extend = "excel",
            text = "Excel",
            filename = "centros_poblados_cuenca"
          )
        ),
        pageLength = 20,
        lengthMenu = c(
          10,
          20,
          50,
          100,
          250
        ),
        autoWidth = FALSE
      )
    )
  }


  # ==========================================================
  # 8. UI
  # ==========================================================

  ui <- function(id) {
    ns <- shiny::NS(
      id
    )

    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".ctp-wrap{padding:14px 18px 28px 18px;max-width:1500px;margin:auto;}",
            ".ctp-head{display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:12px;}",
            ".ctp-note{background:#f6f7f8;border-left:4px solid #607D8B;padding:10px 12px;margin:10px 0 14px 0;line-height:1.45;}",
            ".ctp-card{border:1px solid #ddd;border-radius:7px;padding:12px 12px 18px 12px;background:white;margin-bottom:16px;}",
            ".ctp-card h4{margin-top:0;}",
            ".ctp-a3-frame{width:1188px;max-width:100%;margin:0 auto;aspect-ratio:420/297;}",
            ".ctp-table-wrap{width:100%;overflow-x:auto;padding-bottom:14px;}",
            ".ctp-table-wrap .dataTables_wrapper{width:100%;}",
            ".ctp-table-wrap table.dataTable{width:100%!important;}"
          )
        )
      ),

      shiny::div(
        class = "ctp-wrap",

        shiny::div(
          class = "ctp-head",

          shiny::actionButton(
            ns("analizar"),
            "Cargar centros poblados",
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
          class = "ctp-note",
          "Se reportan todos los centros poblados cuyo punto cae dentro de la cuenca activa o sobre su límite. Todos los puntos se muestran en el mapa; las etiquetas se reducen automáticamente cuando existe solapamiento visual. DEP, PROV y DIST se presentan en formato oración, mientras NOMBRE se conserva según la fuente."
        ),

        layer_source_ui(POBLADOS_STEM),

        shiny::conditionalPanel(
          condition = paste0(
            "output['",
            ns("has_results"),
            "'] === 'true'"
          ),

          shiny::div(
            class = "ctp-card",
            shiny::tags$h4(
              "Mapa de centros poblados"
            ),
            shiny::div(
              class = "ctp-a3-frame",
              shiny::plotOutput(
                ns("mapa"),
                width = "100%",
                height = "100%"
              )
            )
          ),

          shiny::div(
            class = "ctp-card",
            shiny::tags$h4(
              "Centros poblados dentro de la cuenca"
            ),
            shiny::div(
              class = "ctp-table-wrap",
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
        result <- shiny::reactiveVal(
          NULL
        )

        status <- shiny::reactiveVal(
          "Delimita o carga una cuenca para consultar centros poblados."
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
          current_key(),
          {
            result(
              NULL
            )

            status(
              "Cuenca disponible. Pulsa Cargar centros poblados."
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
                  "Seleccionando centros poblados dentro de la cuenca..."
                )

                info <- build_poblados_result(
                  b
                )

                result(
                  list(
                    basin = b,
                    basin_source = if (
                      !is.null(
                        basin_source
                      )
                    ) {
                      basin_source()
                    } else {
                      NULL
                    },
                    basin_label = if (
                      !is.null(
                        basin_label
                      )
                    ) {
                      basin_label()
                    } else {
                      NULL
                    },
                    points = info$points,
                    table = info$table,
                    n_districts = info$n_districts,
                    n_provinces = info$n_provinces,
                    n_departments = info$n_departments,
                    concentration_district = info$concentration_district,
                    concentration_count = info$concentration_count,
                    epsg = info$epsg
                  )
                )

                status(
                  paste0(
                    "Centros poblados listos: ",
                    nrow(
                      info$table
                    ),
                    " puntos | ",
                    info$n_districts,
                    " distritos | ",
                    info$n_provinces,
                    " provincias | ",
                    info$n_departments,
                    " departamentos"
                  )
                )
              },
              error = function(e) {
                result(
                  NULL
                )

                msg <- paste0(
                  "Error en Centros poblados: ",
                  conditionMessage(
                    e
                  )
                )

                status(
                  msg
                )

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
          if (is.null(
            result()
          )) {
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

          shiny::req(
            x
          )

          draw_poblados_map(
            x
          )
        }, res = 120)

        output$tabla <- DT::renderDT({
          x <- result()

          shiny::req(
            x
          )

          poblados_datatable(
            x$table
          )
        }, server = FALSE)

        output$descargar_png <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "FABDEM_centros_poblados_",
              format(
                Sys.Date(),
                "%Y%m%d"
              ),
              ".png"
            )
          },
          content = function(file) {
            x <- result()

            shiny::req(
              x
            )

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
                res = 300
              )
            }

            on.exit(
              grDevices::dev.off(),
              add = TRUE
            )

            draw_poblados_map(
              x
            )
          }
        )



        output$descargar_shp_recortado <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "centros_poblados_normalizado_recortado_",
              format(Sys.Date(), "%Y%m%d"),
              ".zip"
            )
          },
          content = function(file) {
            x <- shiny::isolate(result())
            if (is.null(x)) stop("No hay centros poblados calculados para exportar.")

            source <- read_poblados_source()
            clipped <- clip_vector_export_to_basin(
              source,
              x$basin
            )

            write_shapefile_zip_bundle(
              layers = list(
                poblados_recortado = clipped
              ),
              target_file = file,
              bundle_stem = "centros_poblados_normalizado_recortado"
            )
          }
        )

        output$descargar_shp_mosaico <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "centros_poblados_normalizado_mosaico_",
              format(Sys.Date(), "%Y%m%d"),
              ".zip"
            )
          },
          content = function(file) {
            x <- shiny::isolate(result())
            if (is.null(x)) stop("No hay centros poblados calculados para exportar.")

            source <- read_poblados_source()
            frame <- vector_export_frame(
              basin = x$basin,
              target_crs = sf::st_crs(source),
              margin = 0.07
            )

            keep <- lengths(
              sf::st_intersects(
                source,
                frame
              )
            ) > 0L

            mosaic <- vector_export_clean_sf(
              source[
                keep,
                ,
                drop = FALSE
              ]
            )

            write_shapefile_zip_bundle(
              layers = list(
                poblados_mosaico = mosaic
              ),
              target_file = file,
              bundle_stem = "centros_poblados_normalizado_mosaico"
            )
          }
        )

        shiny::outputOptions(output, "descargar_shp_recortado", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "descargar_shp_mosaico", suspendWhenHidden = FALSE)

        result
      }
    )
  }


  list(
    ui = ui,
    server = server
  )
})

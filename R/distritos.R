# ============================================================
# R/distritos.R
#
# FABDEM Watershed Explorer
# MODULO 05B: DIVISION DISTRITAL
# v2: distritos oficiales + metadata administrativa, sin red hidrográfica
# ============================================================

distritos <- local({

  DISTRITOS_STEM <- "distrital_normalizada_disuelto"
  DISTRICT_FILL <- "#DCEAF5"
  DISTRICT_BORDER <- "#667784"
  DISTRICT_BG_FILL <- "#FCFCFA"
  DISTRICT_BG_BORDER <- "#C3C7CA"


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


  normalize_district_code <- function(x) {
    x <- trimws(
      as.character(x)
    )

    x[
      is.na(x) |
        !nzchar(x)
    ] <- NA_character_

    x <- sub(
      "\\.0+$",
      "",
      x
    )

    digits <- gsub(
      "[^0-9]",
      "",
      x
    )

    out <- rep(
      NA_character_,
      length(digits)
    )

    ok <- !is.na(digits) & nzchar(digits)

    out[ok] <- vapply(
      digits[ok],
      function(z) {
        if (nchar(z) < 6L) {
          paste0(
            paste(
              rep(
                "0",
                6L - nchar(z)
              ),
              collapse = ""
            ),
            z
          )
        } else {
          z
        }
      },
      character(1)
    )

    out
  }


  spanish_sentence_case <- function(x) {
    x <- trimws(
      as.character(x)
    )

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


  district_file <- function() {
    file.path(
      CONTEXTO_TERRITORIAL_DIR,
      paste0(
        DISTRITOS_STEM,
        ".gpkg"
      )
    )
  }


  district_metadata_file <- function() {
    candidates <- c(
      file.path(
        DATA_DIR,
        "Metadata_distritos_tipo_oracion.xlsx"
      ),
      file.path(
        DATA_DIR,
        "Metadata_distritos.xlsx"
      ),
      file.path(
        DATA_DIR,
        "metadata_distritos.xlsx"
      )
    )

    hit <- candidates[
      file.exists(candidates)
    ]

    if (length(hit) == 0L) {
      stop(
        paste0(
          "No se encontro la metadata distrital. Se esperaba uno de estos archivos:\n",
          paste(
            candidates,
            collapse = "\n"
          )
        )
      )
    }

    hit[1L]
  }


  # ==========================================================
  # 2. METADATA
  # ==========================================================

  metadata_cache <- new.env(
    parent = emptyenv()
  )


  read_district_metadata <- function() {
    key <- "metadata"

    if (exists(
      key,
      envir = metadata_cache,
      inherits = FALSE
    )) {
      return(
        get(
          key,
          envir = metadata_cache,
          inherits = FALSE
        )
      )
    }

    path <- district_metadata_file()

    dat <- readxl::read_excel(
      path
    )

    required <- c(
      "CODIGO",
      "DEPARTAMENTO",
      "PROVINCIA",
      "DISTRITO"
    )

    missing <- setdiff(
      required,
      names(dat)
    )

    if (length(missing) > 0L) {
      stop(
        paste0(
          "La metadata distrital no contiene: ",
          paste(
            missing,
            collapse = ", "
          )
        )
      )
    }

    dat <- as.data.frame(
      dat[
        ,
        required,
        drop = FALSE
      ],
      stringsAsFactors = FALSE
    )

    dat$CODIGO <- normalize_district_code(
      dat$CODIGO
    )

    dat$DEPARTAMENTO <- spanish_sentence_case(
      dat$DEPARTAMENTO
    )

    dat$PROVINCIA <- spanish_sentence_case(
      dat$PROVINCIA
    )

    dat$DISTRITO <- spanish_sentence_case(
      dat$DISTRITO
    )

    dat <- dat[
      !is.na(dat$CODIGO) &
        nzchar(dat$CODIGO),
      ,
      drop = FALSE
    ]

    dat <- dat[
      !duplicated(dat$CODIGO),
      ,
      drop = FALSE
    ]

    assign(
      key,
      dat,
      envir = metadata_cache
    )

    dat
  }


  # ==========================================================
  # 3. CAPA DISTRITAL
  # ==========================================================

  district_cache <- new.env(
    parent = emptyenv()
  )


  read_district_source <- function() {
    key <- "district_source"

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

    path <- district_file()

    if (!file_nonempty_local(path)) {
      stop(
        paste0(
          "No se encontro la capa distrital:\n",
          path
        )
      )
    }

    layer_names <- sf::st_layers(
      path
    )$name

    if (length(layer_names) < 1L) {
      stop(
        "El GPKG distrital no contiene capas vectoriales."
      )
    }

    chosen <- NULL

    for (nm in layer_names) {
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
        nrow(g) == 0L ||
        !"CODIGO" %in% names(g)
      ) {
        next
      }

      geom_types <- unique(
        as.character(
          sf::st_geometry_type(
            g,
            by_geometry = TRUE
          )
        )
      )

      if (!any(
        geom_types %in% c(
          "POLYGON",
          "MULTIPOLYGON"
        )
      )) {
        next
      }

      chosen <- g
      break
    }

    if (is.null(chosen)) {
      stop(
        "No se encontro dentro del GPKG una capa poligonal con CODIGO."
      )
    }

    if (is.na(
      sf::st_crs(
        chosen
      )
    )) {
      stop(
        "La capa distrital no tiene CRS valido."
      )
    }

    chosen <- sf::st_make_valid(
      chosen
    )

    chosen <- chosen[
      !sf::st_is_empty(chosen),
      ,
      drop = FALSE
    ]

    geom_type <- as.character(
      sf::st_geometry_type(
        chosen,
        by_geometry = TRUE
      )
    )

    chosen <- chosen[
      geom_type %in% c(
        "POLYGON",
        "MULTIPOLYGON"
      ),
      ,
      drop = FALSE
    ]

    chosen$CODIGO <- normalize_district_code(
      chosen$CODIGO
    )

    chosen <- chosen[
      !is.na(chosen$CODIGO) &
        nzchar(chosen$CODIGO),
      ,
      drop = FALSE
    ]

    assign(
      key,
      chosen,
      envir = district_cache
    )

    chosen
  }


  read_visible_districts <- function(
      xlim,
      ylim
  ) {
    key <- "district_4326"

    if (exists(
      key,
      envir = district_cache,
      inherits = FALSE
    )) {
      bg <- get(
        key,
        envir = district_cache,
        inherits = FALSE
      )
    } else {
      bg <- sf::st_transform(
        read_district_source(),
        4326
      )

      assign(
        key,
        bg,
        envir = district_cache
      )
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
        list(ring)
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


  build_district_result <- function(basin) {
    source <- read_district_source()

    basin_local <- sf::st_transform(
      sf::st_make_valid(
        basin
      ),
      sf::st_crs(
        source
      )
    )

    basin_union <- sf::st_union(
      sf::st_geometry(
        basin_local
      )
    )

    keep <- lengths(
      sf::st_intersects(
        source,
        basin_union
      )
    ) > 0L

    hit <- source[
      keep,
      ,
      drop = FALSE
    ]

    if (nrow(hit) == 0L) {
      stop(
        "La cuenca activa no intersecta ningun distrito."
      )
    }

    epsg <- basin_utm_epsg(
      basin
    )

    hit_utm <- sf::st_transform(
      hit,
      epsg
    )

    basin_utm <- sf::st_transform(
      basin_local,
      epsg
    )

    basin_utm_union <- sf::st_union(
      sf::st_geometry(
        basin_utm
      )
    )

    codes <- unique(
      hit_utm$CODIGO
    )

    pieces <- vector(
      "list",
      length(codes)
    )

    k <- 0L

    for (code in codes) {
      g <- hit_utm[
        hit_utm$CODIGO == code,
        ,
        drop = FALSE
      ]

      if (nrow(g) == 0L) {
        next
      }

      full_geom <- sf::st_union(
        sf::st_geometry(
          g
        )
      )

      full_area <- sum(
        as.numeric(
          sf::st_area(
            full_geom
          )
        ),
        na.rm = TRUE
      ) / 1e6

      inter <- suppressWarnings(
        sf::st_intersection(
          full_geom,
          basin_utm_union
        )
      )

      if (length(inter) == 0L) {
        next
      }

      inter <- inter[
        !sf::st_is_empty(inter)
      ]

      if (length(inter) == 0L) {
        next
      }

      inter_geom <- sf::st_union(
        inter
      )

      inter_area <- sum(
        as.numeric(
          sf::st_area(
            inter_geom
          )
        ),
        na.rm = TRUE
      ) / 1e6

      if (
        !is.finite(inter_area) ||
        inter_area <= 0
      ) {
        next
      }

      k <- k + 1L

      pieces[[k]] <- sf::st_sf(
        CODIGO = code,
        AREA_DISTRITO_KM2 = full_area,
        AREA_CUENCA_KM2 = inter_area,
        geometry = inter_geom
      )
    }

    if (k == 0L) {
      stop(
        "No fue posible construir intersecciones distritales con area positiva."
      )
    }

    clipped <- do.call(
      rbind,
      pieces[
        seq_len(k)
      ]
    )

    clipped <- sf::st_transform(
      clipped,
      4326
    )

    basin_utm2 <- sf::st_transform(
      basin,
      epsg
    )

    basin_area <- sum(
      as.numeric(
        sf::st_area(
          sf::st_union(
            sf::st_geometry(
              basin_utm2
            )
          )
        )
      ),
      na.rm = TRUE
    ) / 1e6

    meta <- read_district_metadata()

    tab <- sf::st_drop_geometry(
      clipped
    )

    idx <- match(
      tab$CODIGO,
      meta$CODIGO
    )

    tab$DEPARTAMENTO <- meta$DEPARTAMENTO[
      idx
    ]

    tab$PROVINCIA <- meta$PROVINCIA[
      idx
    ]

    tab$DISTRITO <- meta$DISTRITO[
      idx
    ]

    tab$DEPARTAMENTO[
      is.na(tab$DEPARTAMENTO)
    ] <- "Sin metadata"

    tab$PROVINCIA[
      is.na(tab$PROVINCIA)
    ] <- "Sin metadata"

    tab$DISTRITO[
      is.na(tab$DISTRITO)
    ] <- tab$CODIGO[
      is.na(tab$DISTRITO)
    ]

    tab$CUENCA_PCT <- 100 *
      tab$AREA_CUENCA_KM2 /
      basin_area

    tab$DISTRITO_PCT <- 100 *
      tab$AREA_CUENCA_KM2 /
      tab$AREA_DISTRITO_KM2

    tab <- tab[
      order(
        -tab$AREA_CUENCA_KM2,
        tab$DISTRITO
      ),
      c(
        "CODIGO",
        "DEPARTAMENTO",
        "PROVINCIA",
        "DISTRITO",
        "AREA_CUENCA_KM2",
        "CUENCA_PCT",
        "DISTRITO_PCT"
      ),
      drop = FALSE
    ]

    clipped$DEPARTAMENTO <- tab$DEPARTAMENTO[
      match(
        clipped$CODIGO,
        tab$CODIGO
      )
    ]

    clipped$PROVINCIA <- tab$PROVINCIA[
      match(
        clipped$CODIGO,
        tab$CODIGO
      )
    ]

    clipped$DISTRITO <- tab$DISTRITO[
      match(
        clipped$CODIGO,
        tab$CODIGO
      )
    ]

    clipped$AREA_CUENCA_KM2 <- tab$AREA_CUENCA_KM2[
      match(
        clipped$CODIGO,
        tab$CODIGO
      )
    ]

    list(
      districts = clipped,
      table = tab,
      basin_area_km2 = basin_area,
      epsg = epsg
    )
  }


  # ==========================================================
  # 4. ETIQUETAS
  # ==========================================================

  district_label_points <- function(
      districts,
      tab
  ) {
    if (
      is.null(districts) ||
      nrow(districts) == 0L
    ) {
      return(NULL)
    }

    codes <- unique(
      districts$CODIGO
    )

    out <- vector(
      "list",
      length(codes)
    )

    k <- 0L

    for (code in codes) {
      g <- districts[
        districts$CODIGO == code,
        ,
        drop = FALSE
      ]

      if (nrow(g) == 0L) {
        next
      }

      row <- tab[
        tab$CODIGO == code,
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0L) {
        next
      }

      pt <- suppressWarnings(
        sf::st_point_on_surface(
          sf::st_union(
            sf::st_geometry(
              g
            )
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
        CODIGO = code,
        LABEL = row$DISTRITO[1L],
        AREA = row$AREA_CUENCA_KM2[1L],
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
      out[
        seq_len(k)
      ]
    )
  }


  select_district_labels <- function(
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

    labels <- labels[
      order(
        -labels$AREA,
        labels$LABEL
      ),
      ,
      drop = FALSE
    ]

    n <- nrow(labels)

    cex <- max(
      0.47,
      min(
        0.68,
        0.68 * 18 / max(
          n,
          18
        )
      )
    )

    boxes <- list()
    keep <- logical(n)

    nbox <- 0L

    for (i in seq_len(n)) {
      w <- graphics::strwidth(
        labels$LABEL[i],
        units = "user",
        cex = cex,
        font = 2,
        family = "sans"
      )

      h <- graphics::strheight(
        labels$LABEL[i],
        units = "user",
        cex = cex,
        font = 2,
        family = "sans"
      )

      box <- c(
        xmin = labels$X[i] - w / 2 - 0.002 * ux,
        xmax = labels$X[i] + w / 2 + 0.002 * ux,
        ymin = labels$Y[i] - h / 2 - 0.002 * uy,
        ymax = labels$Y[i] + h / 2 + 0.002 * uy
      )

      overlap <- FALSE

      if (nbox > 0L) {
        for (j in seq_len(nbox)) {
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
  # 5. CARTOGRAFIA
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
    bx <- box_polygon(
      box
    )

    bx_utm <- sf::st_transform(
      bx,
      epsg
    )

    basin_utm <- sf::st_transform(
      basin,
      epsg
    )

    bx_area <- as.numeric(
      sf::st_area(
        bx_utm
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
        sf::st_area(
          inter
        )
      ),
      na.rm = TRUE
    ) / bx_area
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

    for (i in seq_along(candidates)) {
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
        scores[i] <- scores[i] + 5000
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
    if (
      !is.finite(total_m) ||
      total_m <= 0
    ) {
      return(1000)
    }

    target <- total_m / 3
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

    if (length(candidates) == 0L) {
      return(pow10)
    }

    max(
      candidates
    )
  }


  draw_district_map <- function(x) {
    basin <- sf::st_transform(
      x$basin,
      4326
    )

    districts <- sf::st_transform(
      x$districts,
      4326
    )

    tab <- x$table
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
      ) - 0.07 * dx,
      as.numeric(
        bb["xmax"]
      ) + 0.07 * dx
    )

    ylim0 <- c(
      as.numeric(
        bb["ymin"]
      ) - 0.07 * dy,
      as.numeric(
        bb["ymax"]
      ) + 0.07 * dy
    )

    mid_lat <- mean(
      ylim0
    )

    map_asp <- 1 / cos(
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
    ux <- usr[2] - usr[1]
    uy <- usr[4] - usr[3]

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
      read_visible_districts(
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
        col = DISTRICT_BG_FILL,
        border = DISTRICT_BG_BORDER,
        lwd = 0.55
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
        alpha.f = 0.25
      ),
      lwd = 0.5
    )

    graphics::plot(
      sf::st_geometry(
        districts
      ),
      add = TRUE,
      col = grDevices::adjustcolor(
        DISTRICT_FILL,
        alpha.f = 0.94
      ),
      border = DISTRICT_BORDER,
      lwd = 0.75
    )

    labels <- district_label_points(
      districts,
      tab
    )

    labels <- select_district_labels(
      labels,
      ux,
      uy
    )

    if (!is.null(labels)) {
      graphics::text(
        labels$X,
        labels$Y,
        labels = labels$LABEL,
        col = "#252525",
        cex = labels$CEX,
        font = 2,
        family = "sans"
      )
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
    nx <- mean(
      nb[
        c(
          "xmin",
          "xmax"
        )
      ]
    )

    ny_top <- nb["ymax"] -
      0.018 * uy

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
      0.012 * uy

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

    # Escala dinámica, solo laterales/esquinas.
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

    scale_dx <- scale_m /
      m_per_deg

    scale_box_w <- scale_dx +
      0.100 * ux

    scale_box_h <- 0.085 * uy

    make_scale_box <- function(
        xmin,
        ymin
    ) {
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

    sx0 <- sb["xmin"] +
      0.025 * ux

    sy0 <- sb["ymin"] +
      0.040 * uy

    bar_h <- 0.018 * uy
    nseg <- 4L
    seg_dx <- scale_dx /
      nseg

    for (i in 0:(nseg - 1L)) {
      graphics::rect(
        sx0 + i * seg_dx,
        sy0,
        sx0 + (i + 1L) * seg_dx,
        sy0 + bar_h,
        col = if (
          i %% 2L == 0L
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
        sx0 + i * seg_dx,
        sy0 - 0.014 * uy,
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
        0.018 * ux,
      sy0 -
        0.014 * uy,
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
      labels = "División distrital",
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
        nrow(tab),
        " distritos intersectados"
      ),
      adj = c(
        0,
        1
      ),
      cex = 0.70,
      col = "grey30"
    )

    nprov <- length(
      unique(
        tab$PROVINCIA[
          tab$PROVINCIA != "Sin metadata"
        ]
      )
    )

    ndep <- length(
      unique(
        tab$DEPARTAMENTO[
          tab$DEPARTAMENTO != "Sin metadata"
        ]
      )
    )

    graphics::text(
      0.04,
      0.885,
      labels = paste0(
        nprov,
        " provincias | ",
        ndep,
        " departamentos"
      ),
      adj = c(
        0,
        1
      ),
      cex = 0.62,
      col = "grey40"
    )

    # Simbología.
    graphics::rect(
      0.05,
      0.805,
      0.18,
      0.835,
      col = DISTRICT_FILL,
      border = DISTRICT_BORDER,
      lwd = 0.8
    )

    graphics::text(
      0.23,
      0.820,
      labels = "Distrito intersectado",
      adj = c(
        0,
        0.5
      ),
      cex = 0.66
    )

    graphics::segments(
      0.05,
      0.750,
      0.18,
      0.750,
      col = "#111111",
      lwd = 2.0
    )

    graphics::text(
      0.23,
      0.750,
      labels = "Cuenca activa",
      adj = c(
        0,
        0.5
      ),
      cex = 0.66
    )

    graphics::segments(
      0.05,
      0.690,
      0.18,
      0.690,
      col = DISTRICT_BG_BORDER,
      lwd = 0.8
    )

    graphics::text(
      0.23,
      0.690,
      labels = "Límite distrital",
      adj = c(
        0,
        0.5
      ),
      cex = 0.66
    )

    graphics::text(
      0.04,
      0.590,
      labels = "Indicadores",
      adj = c(
        0,
        1
      ),
      font = 2,
      cex = 0.76
    )

    principal <- tab[
      which.max(
        tab$AREA_CUENCA_KM2
      ),
      ,
      drop = FALSE
    ]

    info_lines <- c(
      paste0(
        "Mayor área: ",
        principal$DISTRITO[1L]
      ),
      paste0(
        formatC(
          principal$AREA_CUENCA_KM2[1L],
          format = "f",
          digits = 1
        ),
        " km²"
      ),
      paste0(
        formatC(
          principal$CUENCA_PCT[1L],
          format = "f",
          digits = 1
        ),
        "% de la cuenca"
      )
    )

    graphics::text(
      rep(
        0.04,
        length(info_lines)
      ),
      c(
        0.550,
        0.513,
        0.476
      ),
      labels = info_lines,
      adj = c(
        0,
        1
      ),
      cex = 0.62,
      col = "grey25"
    )

    # Fuente al pie.
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
      labels = layer_source_map_label(DISTRITOS_STEM),
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
  # 6. TABLA
  # ==========================================================

  district_datatable <- function(tab) {
    out <- tab

    out$AREA_CUENCA_KM2 <- round(
      out$AREA_CUENCA_KM2,
      3
    )

    out$CUENCA_PCT <- round(
      out$CUENCA_PCT,
      2
    )

    out$DISTRITO_PCT <- round(
      out$DISTRITO_PCT,
      2
    )

    names(out) <- c(
      "Código",
      "Departamento",
      "Provincia",
      "Distrito",
      "Área en cuenca (km²)",
      "Cuenca (%)",
      "Distrito dentro de cuenca (%)"
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
            filename = "distritos_intersectados"
          ),
          list(
            extend = "excel",
            text = "Excel",
            filename = "distritos_intersectados"
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
  # 7. UI
  # ==========================================================

  ui <- function(id) {
    ns <- shiny::NS(
      id
    )

    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".ctd-wrap{padding:14px 18px 28px 18px;max-width:1500px;margin:auto;}",
            ".ctd-head{display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:12px;}",
            ".ctd-note{background:#f6f7f8;border-left:4px solid #607D8B;padding:10px 12px;margin:10px 0 14px 0;line-height:1.45;}",
            ".ctd-card{border:1px solid #ddd;border-radius:7px;padding:12px 12px 18px 12px;background:white;margin-bottom:16px;}",
            ".ctd-card h4{margin-top:0;}",
            ".ctd-a3-frame{width:1188px;max-width:100%;margin:0 auto;aspect-ratio:420/297;}",
            ".ctd-table-wrap{width:100%;overflow-x:auto;padding-bottom:14px;}",
            ".ctd-table-wrap .dataTables_wrapper{width:100%;}",
            ".ctd-table-wrap table.dataTable{width:100%!important;}"
          )
        )
      ),

      shiny::div(
        class = "ctd-wrap",

        shiny::div(
          class = "ctd-head",

          shiny::actionButton(
            ns("analizar"),
            "Cargar división distrital",
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
          class = "ctd-note",
          "Los distritos se intersectan con la cuenca activa. Los nombres de departamento, provincia y distrito se presentan en formato oración. “Cuenca (%)” indica qué parte de la cuenca pertenece a cada distrito; “Distrito dentro de cuenca (%)” indica qué fracción del distrito queda contenida en la cuenca."
        ),

        layer_source_ui(DISTRITOS_STEM),

        shiny::conditionalPanel(
          condition = paste0(
            "output['",
            ns("has_results"),
            "'] === 'true'"
          ),

          shiny::div(
            class = "ctd-card",
            shiny::tags$h4(
              "Mapa distrital"
            ),
            shiny::div(
              class = "ctd-a3-frame",
              shiny::plotOutput(
                ns("mapa"),
                width = "100%",
                height = "100%"
              )
            )
          ),

          shiny::div(
            class = "ctd-card",
            shiny::tags$h4(
              "Distritos intersectados por la cuenca"
            ),
            shiny::div(
              class = "ctd-table-wrap",
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
  # 8. SERVER
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
          "Delimita o carga una cuenca para consultar la división distrital."
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
              "Cuenca disponible. Pulsa Cargar división distrital."
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
                  "Intersectando la cuenca con la división distrital..."
                )

                dist_info <- build_district_result(
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
                    districts = dist_info$districts,
                    table = dist_info$table,
                    basin_area_km2 = dist_info$basin_area_km2,
                    epsg = dist_info$epsg
                  )
                )

                status(
                  paste0(
                    "División distrital lista: ",
                    nrow(
                      dist_info$table
                    ),
                    " distritos | ",
                    length(
                      unique(
                        dist_info$table$PROVINCIA
                      )
                    ),
                    " provincias | ",
                    length(
                      unique(
                        dist_info$table$DEPARTAMENTO
                      )
                    ),
                    " departamentos"
                  )
                )
              },
              error = function(e) {
                result(
                  NULL
                )

                msg <- paste0(
                  "Error en Distritos: ",
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

          draw_district_map(
            x
          )
        }, res = 120)

        output$tabla <- DT::renderDT({
          x <- result()

          shiny::req(
            x
          )

          district_datatable(
            x$table
          )
        }, server = FALSE)

        output$descargar_png <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "FABDEM_division_distrital_",
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

            draw_district_map(
              x
            )
          }
        )



        output$descargar_shp_recortado <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "distritos_normalizado_recortado_",
              format(Sys.Date(), "%Y%m%d"),
              ".zip"
            )
          },
          content = function(file) {
            x <- shiny::isolate(result())
            if (is.null(x)) stop("No hay resultados distritales para exportar.")

            source <- read_district_source()
            clipped <- clip_vector_export_to_basin(
              source,
              x$basin
            )

            write_shapefile_zip_bundle(
              layers = list(
                distritos_recortado = clipped
              ),
              target_file = file,
              bundle_stem = "distritos_normalizado_recortado"
            )
          }
        )

        output$descargar_shp_mosaico <- shiny::downloadHandler(
          filename = function() {
            paste0(
              "distritos_normalizado_mosaico_",
              format(Sys.Date(), "%Y%m%d"),
              ".zip"
            )
          },
          content = function(file) {
            x <- shiny::isolate(result())
            if (is.null(x)) stop("No hay resultados distritales para exportar.")

            source <- read_district_source()
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
                distritos_mosaico = mosaic
              ),
              target_file = file,
              bundle_stem = "distritos_normalizado_mosaico"
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

# ============================================================
# R/morfometria_graficos_v37.R
#
# PARCHE GRAFICO DE MORFOMETRIA
# v37: lamina hipsometrica completa y estable
# ============================================================
#
# Se mantiene intacto R/morfometria.R y se sustituyen en memoria
# solamente las dos funciones de dibujo del relieve:
#   - curva hipsometrica
#   - distribucion altitudinal
#
# El resto de calculos, tablas, exportaciones y pipeline no cambia.
# ============================================================


.morph_draw_hipsometrica_v37 <- function(x) {

  if (
    is.null(x) ||
    is.null(x$relief)
  ) {
    stop(
      "La curva hipsométrica aún no está disponible."
    )
  }


  z <- suppressWarnings(
    as.numeric(
      x$relief$elevation_sample
    )
  )

  z <- z[
    is.finite(z)
  ]


  if (length(z) <= 1L) {
    stop(
      "No hay suficientes datos de elevación."
    )
  }


  z <- sort(
    z,
    decreasing = TRUE
  )

  zmin <- min(
    z,
    na.rm = TRUE
  )

  zmax <- max(
    z,
    na.rm = TRUE
  )

  zrange <- zmax - zmin


  if (
    !is.finite(zrange) ||
    zrange <= 0
  ) {
    stop(
      "El rango altitudinal no permite construir la curva hipsométrica."
    )
  }


  relative_height <- (
    z - zmin
  ) /
    zrange

  relative_area <- seq(
    0,
    1,
    length.out = length(
      relative_height
    )
  )


  hi <- suppressWarnings(
    as.numeric(
      x$relief$hypsometric_integral
    )
  )


  area_km2 <- NA_real_

  if (
    !is.null(x$geom) &&
    !is.null(x$geom$area_km2)
  ) {
    area_km2 <- suppressWarnings(
      as.numeric(
        x$geom$area_km2
      )
    )
  }


  pattern_label <- if (
    is.finite(hi) &&
    hi >= 0.60
  ) {
    "predominantemente convexo"
  } else if (
    is.finite(hi) &&
    hi < 0.35
  ) {
    "predominantemente cóncavo"
  } else {
    "intermedio"
  }


  # Referencias conceptuales normalizadas.
  # No se interpretan como edades cronológicas de la cuenca.
  ref_x <- seq(
    0,
    1,
    length.out = 401L
  )

  ref_convex <- sqrt(
    pmax(
      0,
      1 - ref_x^2
    )
  )

  ref_intermediate <- 1 - ref_x

  ref_concave <- (
    1 - ref_x
  )^2


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
    mar = c(
      5.2,
      5.1,
      3.8,
      1.2
    ),
    mgp = c(
      2.8,
      0.85,
      0
    )
  )


  title_text <- paste0(
    if (is.finite(hi)) {
      sprintf(
        "HI = %.3f",
        hi
      )
    } else {
      "HI = NA"
    },
    if (is.finite(area_km2)) {
      paste0(
        "   |   A = ",
        sprintf(
          "%.2f",
          area_km2
        ),
        " km²"
      )
    } else {
      ""
    },
    "   |   Patrón ",
    pattern_label
  )


  graphics::plot(
    relative_area,
    relative_height,
    type = "n",
    xlim = c(
      0,
      1
    ),
    ylim = c(
      0,
      1
    ),
    xaxs = "i",
    yaxs = "i",
    xlab = "Área relativa acumulada (a/A)",
    ylab = "Altura relativa (h/H)",
    main = title_text,
    cex.main = 0.95
  )


  graphics::grid(
    col = "grey88",
    lty = 1
  )


  graphics::lines(
    ref_x,
    ref_convex,
    col = "#EF5350",
    lty = 2,
    lwd = 1.6
  )

  graphics::lines(
    ref_x,
    ref_intermediate,
    col = "#26A69A",
    lty = 3,
    lwd = 1.6
  )

  graphics::lines(
    ref_x,
    ref_concave,
    col = "#43A047",
    lty = 4,
    lwd = 1.6
  )

  graphics::lines(
    relative_area,
    relative_height,
    col = "#1565C0",
    lwd = 2.8
  )


  graphics::legend(
    "topright",
    legend = c(
      "Cuenca",
      "Convexa (juvenil, ref.)",
      "Intermedia (madura, ref.)",
      "Cóncava (senil, ref.)"
    ),
    col = c(
      "#1565C0",
      "#EF5350",
      "#26A69A",
      "#43A047"
    ),
    lty = c(
      1,
      2,
      3,
      4
    ),
    lwd = c(
      2.8,
      1.6,
      1.6,
      1.6
    ),
    bty = "n",
    cex = 0.80
  )


  graphics::mtext(
    "Referencias geomorfológicas conceptuales; no representan una edad cronológica.",
    side = 1,
    line = 4.0,
    adj = 0,
    cex = 0.70,
    col = "grey40"
  )


  invisible(NULL)
}


.morph_draw_hist_elevacion_v37 <- function(x) {

  if (
    is.null(x) ||
    is.null(x$relief)
  ) {
    stop(
      "La distribución altitudinal aún no está disponible."
    )
  }


  z <- suppressWarnings(
    as.numeric(
      x$relief$elevation_sample
    )
  )

  z <- z[
    is.finite(z)
  ]


  if (length(z) <= 1L) {
    stop(
      "No hay suficientes datos de elevación."
    )
  }


  zmin <- min(
    z,
    na.rm = TRUE
  )

  zmax <- max(
    z,
    na.rm = TRUE
  )


  if (
    !is.finite(zmin) ||
    !is.finite(zmax) ||
    zmax <= zmin
  ) {
    stop(
      "El rango altitudinal no permite construir la distribución."
    )
  }


  # Numero fijo de clases para mantener una lectura comparable y
  # evitar barras excesivamente delgadas en cuencas con muchos pixeles.
  n_bins <- 16L

  breaks <- seq(
    zmin,
    zmax,
    length.out = n_bins + 1L
  )


  hist_data <- graphics::hist(
    z,
    breaks = breaks,
    plot = FALSE,
    include.lowest = TRUE,
    right = TRUE
  )


  total_cells <- sum(
    hist_data$counts
  )


  if (
    !is.finite(total_cells) ||
    total_cells <= 0
  ) {
    stop(
      "No hay celdas válidas para la distribución altitudinal."
    )
  }


  # La muestra FABDEM es espacialmente regular dentro de la cuenca;
  # el porcentaje de celdas se usa como estimador del porcentaje de área.
  area_pct <- 100 *
    hist_data$counts /
    total_cells


  xmax <- max(
    area_pct,
    na.rm = TRUE
  )

  if (
    !is.finite(xmax) ||
    xmax <= 0
  ) {
    xmax <- 1
  }

  xmax_plot <- xmax * 1.18


  area_km2 <- NA_real_

  if (
    !is.null(x$geom) &&
    !is.null(x$geom$area_km2)
  ) {
    area_km2 <- suppressWarnings(
      as.numeric(
        x$geom$area_km2
      )
    )
  }


  zmean <- suppressWarnings(
    as.numeric(
      x$relief$zmean
    )
  )

  zmedian <- suppressWarnings(
    as.numeric(
      x$relief$zmedian
    )
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


  graphics::par(
    mar = c(
      5.2,
      5.4,
      4.8,
      1.0
    ),
    mgp = c(
      2.8,
      0.85,
      0
    )
  )


  graphics::plot(
    NA_real_,
    NA_real_,
    type = "n",
    xlim = c(
      0,
      xmax_plot
    ),
    ylim = c(
      zmin,
      zmax
    ),
    xaxs = "i",
    yaxs = "i",
    xlab = "Área de la cuenca (%)",
    ylab = "Elevación (m s.n.m.)",
    main = "Distribución por clases altitudinales",
    cex.main = 0.95
  )


  graphics::grid(
    col = "grey90",
    lty = 1
  )


  graphics::rect(
    xleft = 0,
    ybottom = hist_data$breaks[
      -length(
        hist_data$breaks
      )
    ],
    xright = area_pct,
    ytop = hist_data$breaks[
      -1L
    ],
    col = "#1E88E5",
    border = "white",
    lwd = 0.8
  )


  if (is.finite(zmean)) {
    graphics::abline(
      h = zmean,
      col = "#C62828",
      lwd = 1.6,
      lty = 2
    )
  }

  if (is.finite(zmedian)) {
    graphics::abline(
      h = zmedian,
      col = "#37474F",
      lwd = 1.6,
      lty = 3
    )
  }


  # Eje superior equivalente en km2. El eje inferior permanece
  # normalizado para que cuencas de tamanos distintos sean comparables.
  if (is.finite(area_km2)) {

    percent_ticks <- graphics::axTicks(
      1
    )

    percent_ticks <- percent_ticks[
      percent_ticks >= 0 &
      percent_ticks <= xmax_plot
    ]


    graphics::axis(
      side = 3,
      at = percent_ticks,
      labels = sprintf(
        "%.2f",
        percent_ticks /
          100 *
          area_km2
      ),
      cex.axis = 0.78
    )

    graphics::mtext(
      "Área por clase (km²)",
      side = 3,
      line = 2.3,
      cex = 0.80
    )
  }


  legend_items <- character(0)
  legend_cols <- character(0)
  legend_lty <- integer(0)


  if (is.finite(zmean)) {
    legend_items <- c(
      legend_items,
      paste0(
        "Media: ",
        sprintf(
          "%.0f",
          zmean
        ),
        " m"
      )
    )

    legend_cols <- c(
      legend_cols,
      "#C62828"
    )

    legend_lty <- c(
      legend_lty,
      2L
    )
  }


  if (is.finite(zmedian)) {
    legend_items <- c(
      legend_items,
      paste0(
        "Mediana: ",
        sprintf(
          "%.0f",
          zmedian
        ),
        " m"
      )
    )

    legend_cols <- c(
      legend_cols,
      "#37474F"
    )

    legend_lty <- c(
      legend_lty,
      3L
    )
  }


  if (length(legend_items) > 0L) {
    graphics::legend(
      "topright",
      legend = legend_items,
      col = legend_cols,
      lty = legend_lty,
      lwd = 1.6,
      bty = "n",
      cex = 0.78
    )
  }


  invisible(NULL)
}


.morph_replace_region_v37 <- function(
    code,
    start_marker,
    end_marker,
    replacement
) {

  start_pos <- regexpr(
    start_marker,
    code,
    fixed = TRUE
  )[1]


  if (start_pos < 1L) {
    stop(
      paste0(
        "No se encontró el marcador inicial del parche morfométrico: ",
        start_marker
      )
    )
  }


  code_from_start <- substring(
    code,
    start_pos
  )

  end_relative <- regexpr(
    end_marker,
    code_from_start,
    fixed = TRUE
  )[1]


  if (end_relative < 1L) {
    stop(
      paste0(
        "No se encontró el marcador final del parche morfométrico: ",
        end_marker
      )
    )
  }


  end_pos <- start_pos +
    end_relative -
    1L


  paste0(
    if (start_pos > 1L) {
      substr(
        code,
        1L,
        start_pos - 1L
      )
    } else {
      ""
    },
    replacement,
    substr(
      code,
      end_pos,
      nchar(code)
    )
  )
}


morph_base_file <- file.path(
  "R",
  "morfometria.R"
)


if (!file.exists(morph_base_file)) {
  stop(
    "No se encontró R/morfometria.R para aplicar el parche gráfico v37."
  )
}


morph_code <- paste(
  readLines(
    morph_base_file,
    warn = FALSE,
    encoding = "UTF-8"
  ),
  collapse = "\n"
)


morph_code <- .morph_replace_region_v37(
  code = morph_code,
  start_marker = "        draw_hipsometrica_plot <- function(x) {",
  end_marker = "        draw_hist_elevacion_plot <- function(x) {",
  replacement = paste0(
    "        draw_hipsometrica_plot <- .morph_draw_hipsometrica_v37\n\n\n"
  )
)


morph_code <- .morph_replace_region_v37(
  code = morph_code,
  start_marker = "        draw_hist_elevacion_plot <- function(x) {",
  end_marker = "        draw_hist_pendiente_plot <- function(x) {",
  replacement = paste0(
    "        draw_hist_elevacion_plot <- .morph_draw_hist_elevacion_v37\n\n\n"
  )
)


morph_connection <- textConnection(
  morph_code
)


tryCatch(
  {
    source(
      morph_connection,
      local = TRUE,
      echo = FALSE,
      print.eval = FALSE,
      encoding = "UTF-8"
    )
  },
  finally = {
    close(
      morph_connection
    )
  }
)

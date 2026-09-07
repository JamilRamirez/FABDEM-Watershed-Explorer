# ============================================================
# R/snap_topologico.R
#
# DIAGNOSTICO DEL OUTLET Y AJUSTE D8 LOCAL
# ============================================================
#
# Principio:
# - el clic manda;
# - primero se evalua la celda D8 exacta bajo el clic;
# - si su area aportante alcanza el minimo, NO se hace snap;
# - si no alcanza el minimo, se informa el area estimada;
# - solo se permite una correccion D8 muy corta cuando el punto
#   ya esta cerca del umbral, para corregir uno o pocos pixeles;
# - nunca se busca el cauce de mayor acumulacion ni el cauce mas
#   cercano dentro de un buffer grande.
#
# El diagnostico usa el mismo REVERSE_D8 del trazado final y el
# mismo umbral del bloque, por lo que la decision es consistente
# con la delimitacion posterior.
# ============================================================


SNAP_D8_CORRECTION_MAX_M <- 90
SNAP_D8_CORRECTION_MIN_FRACTION <- 0.70


new_snap_reverse_cache <- function(block_id) {

  meta <- get_block_metadata(
    block_id
  )

  reverse_rows <- get_block_assets(
    block_id,
    "reverse"
  )


  e <- new.env(
    parent = emptyenv()
  )

  e$block_id <- block_id

  e$metadata <- list(
    nrows = as.double(meta[["NROWS"]][1]),
    ncols = as.double(meta[["NCOLS"]][1]),
    stripe_rows = as.double(meta[["STRIPE_ROWS"]][1]),
    n_stripes = as.integer(meta[["N_STRIPES"]][1])
  )

  e$stripe_files <- as.character(
    reverse_rows[["LOCAL_PATH"]]
  )

  e$values <- new.env(
    hash = TRUE,
    parent = emptyenv()
  )

  e$lru <- character(0)
  e$fast_mode <- FALSE
  e$fast_preloaded <- FALSE
  e$edge_safe <- FALSE
  e$fast_objects <- NULL
  e$fast_files <- NULL
  e$fast_manifest <- NULL
  e$full_raw <- NULL
  e$full_raw_ready <- FALSE

  e
}


count_upstream_until_threshold <- function(
    cache,
    outlet_cell,
    threshold_cells
) {

  threshold_cells <- as.double(
    threshold_cells
  )

  if (
    !is.finite(threshold_cells) ||
    threshold_cells < 1
  ) {
    stop(
      "Umbral de celdas invalido para diagnosticar el punto de salida."
    )
  }


  nc <- as.double(
    cache$metadata$ncols
  )

  nr <- as.double(
    cache$metadata$nrows
  )

  total_cells <- nc * nr


  outlet_cell <- as.double(
    outlet_cell
  )

  if (
    !is.finite(outlet_cell) ||
    outlet_cell < 1 ||
    outlet_cell > total_cells
  ) {
    stop(
      "La celda del punto de salida quedo fuera de la grilla hidrologica."
    )
  }


  frontier <- outlet_cell
  n_cells <- 1
  level <- 0L


  repeat {

    if (n_cells >= threshold_cells) {
      return(
        list(
          reaches_threshold = TRUE,
          n_cells = n_cells,
          levels = level
        )
      )
    }


    if (length(frontier) == 0L) {
      break
    }


    level <- level + 1L

    if (level > MAX_TRACE_LEVELS) {
      stop(
        "Se alcanzo MAX_TRACE_LEVELS durante el diagnostico del outlet."
      )
    }


    values_reverse <- get_reverse_values(
      cache,
      frontier
    )


    rows <- floor(
      (
        frontier - 1
      ) /
        nc
    ) + 1

    cols <- (
      (
        frontier - 1
      ) %% nc
    ) + 1


    parents <- numeric(0)


    use <- bitwAnd(values_reverse, 1L) != 0L & rows > 1 & cols > 1
    if (any(use)) {
      parents <- c(parents, frontier[use] - nc - 1)
    }

    use <- bitwAnd(values_reverse, 2L) != 0L & rows > 1
    if (any(use)) {
      parents <- c(parents, frontier[use] - nc)
    }

    use <- bitwAnd(values_reverse, 4L) != 0L & rows > 1 & cols < nc
    if (any(use)) {
      parents <- c(parents, frontier[use] - nc + 1)
    }

    use <- bitwAnd(values_reverse, 8L) != 0L & cols > 1
    if (any(use)) {
      parents <- c(parents, frontier[use] - 1)
    }

    use <- bitwAnd(values_reverse, 16L) != 0L & cols < nc
    if (any(use)) {
      parents <- c(parents, frontier[use] + 1)
    }

    use <- bitwAnd(values_reverse, 32L) != 0L & rows < nr & cols > 1
    if (any(use)) {
      parents <- c(parents, frontier[use] + nc - 1)
    }

    use <- bitwAnd(values_reverse, 64L) != 0L & rows < nr
    if (any(use)) {
      parents <- c(parents, frontier[use] + nc)
    }

    use <- bitwAnd(values_reverse, 128L) != 0L & rows < nr & cols < nc
    if (any(use)) {
      parents <- c(parents, frontier[use] + nc + 1)
    }


    if (length(parents) == 0L) {
      break
    }


    parents <- unique(
      parents
    )

    parents <- parents[
      parents >= 1 &
        parents <= total_cells
    ]


    if (length(parents) == 0L) {
      break
    }


    n_cells <- n_cells + length(parents)
    frontier <- parents
  }


  list(
    reaches_threshold = n_cells >= threshold_cells,
    n_cells = n_cells,
    levels = level
  )
}


stream_value_at_global_cell <- function(
    cell,
    grid_template,
    stream_cache,
    stripe_rows,
    n_stripes
) {

  nc <- as.double(
    terra::ncol(
      grid_template
    )
  )

  row <- floor(
    (
      as.double(cell) - 1
    ) /
      nc
  ) + 1


  sid <- stripe_id_from_row(
    row = as.integer(row),
    stripe_rows = as.integer(stripe_rows),
    n_stripes = as.integer(n_stripes)
  )


  stream_r <- load_stream_stripe(
    stream_cache,
    sid
  )


  xy <- terra::xyFromCell(
    grid_template,
    as.double(cell)
  )


  extract_stream_value_at_xy(
    stream_r,
    c(
      xy[1, 1],
      xy[1, 2]
    )
  )
}


snap_downstream_cell <- function(
    reverse_cache,
    cell
) {

  nc <- as.double(
    reverse_cache$metadata$ncols
  )

  nr <- as.double(
    reverse_cache$metadata$nrows
  )


  cell <- as.double(
    cell
  )

  row <- floor(
    (
      cell - 1
    ) /
      nc
  ) + 1

  col <- (
    (
      cell - 1
    ) %% nc
  ) + 1


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

  required_bits <- c(
    128L,
    64L,
    32L,
    16L,
    8L,
    4L,
    2L,
    1L
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


  targets <- cell + offsets[valid]
  bits <- required_bits[valid]


  if (length(targets) == 0L) {
    return(NA_real_)
  }


  values_reverse <- get_reverse_values(
    reverse_cache,
    targets
  )

  hit <- which(
    bitwAnd(
      values_reverse,
      bits
    ) != 0L
  )


  if (length(hit) != 1L) {
    return(NA_real_)
  }


  as.double(
    targets[hit]
  )
}


cell_distance_from_click_m <- function(
    cell,
    click_wgs,
    grid_template,
    grid_crs,
    epsg
) {

  xy <- terra::xyFromCell(
    grid_template,
    as.double(cell)
  )

  cell_grid <- sf::st_sfc(
    sf::st_point(
      c(
        xy[1, 1],
        xy[1, 2]
      )
    ),
    crs = grid_crs
  )

  click_utm <- sf::st_transform(
    click_wgs,
    epsg
  )

  cell_utm <- sf::st_transform(
    cell_grid,
    epsg
  )

  as.numeric(
    sf::st_distance(
      click_utm,
      cell_utm
    )
  )
}


build_snap_result <- function(
    lon,
    lat,
    outlet_cell,
    grid_template,
    grid_crs,
    actual_stream_value,
    mode,
    diagnosis,
    threshold_cells,
    threshold_km2
) {

  outlet_xy <- terra::xyFromCell(
    grid_template,
    as.double(outlet_cell)
  )

  outlet_grid <- sf::st_sfc(
    sf::st_point(
      c(
        outlet_xy[1, 1],
        outlet_xy[1, 2]
      )
    ),
    crs = grid_crs
  )

  outlet_wgs <- sf::st_transform(
    outlet_grid,
    4326
  )

  outlet_wgs_xy <- sf::st_coordinates(
    outlet_wgs
  )[1, ]


  click_wgs <- sf::st_sfc(
    sf::st_point(
      c(
        lon,
        lat
      )
    ),
    crs = 4326
  )

  epsg <- utm_epsg_point(
    lon,
    lat
  )

  snap_distance_m <- as.numeric(
    sf::st_distance(
      sf::st_transform(click_wgs, epsg),
      sf::st_transform(outlet_wgs, epsg)
    )
  )


  area_est_km2 <- if (isTRUE(diagnosis$reaches_threshold)) {
    as.numeric(threshold_km2)
  } else {
    as.numeric(threshold_km2) *
      as.numeric(diagnosis$n_cells) /
      as.numeric(threshold_cells)
  }


  # delimitacion.R historicamente usa stream_mask_value > 0 como
  # guardia. Desde esta version la validez se determina por area
  # D8, no necesariamente por STREAM_MASK. Se mantiene 1 como
  # bandera de compatibilidad y se conserva el valor real aparte.
  compatibility_stream_value <- if (
    is.finite(actual_stream_value) &&
    actual_stream_value > 0
  ) {
    as.numeric(actual_stream_value)
  } else {
    1
  }


  list(
    clicked_lon = lon,
    clicked_lat = lat,
    outlet_cell = as.double(outlet_cell),
    outlet_x = outlet_xy[1, 1],
    outlet_y = outlet_xy[1, 2],
    outlet_lon = outlet_wgs_xy[1],
    outlet_lat = outlet_wgs_xy[2],
    snap_mode = mode,
    snap_distance_m = snap_distance_m,
    stream_mask_value = compatibility_stream_value,
    actual_stream_mask_value = as.numeric(actual_stream_value),
    hydrologic_valid = isTRUE(diagnosis$reaches_threshold),
    diagnostic_n_cells = as.numeric(diagnosis$n_cells),
    diagnostic_area_km2 = area_est_km2,
    minimum_area_km2 = as.numeric(threshold_km2)
  )
}


snap_to_stream_stripes <- function(
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

  threshold_cells <- suppressWarnings(
    as.double(
      meta[["STREAM_THRESHOLD_CELLS"]][1]
    )
  )

  threshold_km2 <- suppressWarnings(
    as.numeric(
      meta[["STREAM_THRESHOLD_KM2"]][1]
    )
  )


  if (
    !is.finite(threshold_cells) ||
    threshold_cells < 1 ||
    !is.finite(threshold_km2) ||
    threshold_km2 <= 0
  ) {
    stop(
      "El bloque no contiene un umbral hidrologico valido."
    )
  }


  reverse_cache <- new_snap_reverse_cache(
    block_id
  )


  grid_crs <- sf::st_crs(
    terra::crs(
      grid_template
    )
  )

  if (is.na(grid_crs)) {
    stop(
      "La grilla del bloque no tiene CRS valido."
    )
  }


  click_wgs <- sf::st_sfc(
    sf::st_point(
      c(
        lon,
        lat
      )
    ),
    crs = 4326
  )

  click_grid <- sf::st_transform(
    click_wgs,
    grid_crs
  )

  click_xy <- sf::st_coordinates(
    click_grid
  )[1, ]


  click_cell <- terra::cellFromXY(
    grid_template,
    matrix(
      c(
        click_xy[1],
        click_xy[2]
      ),
      nrow = 1
    )
  )

  if (is.na(click_cell)) {
    stop(
      "El clic quedo fuera de la grilla hidrologica del bloque."
    )
  }


  diagnosis <- count_upstream_until_threshold(
    cache = reverse_cache,
    outlet_cell = click_cell,
    threshold_cells = threshold_cells
  )


  click_stream_value <- stream_value_at_global_cell(
    cell = click_cell,
    grid_template = grid_template,
    stream_cache = stream_cache,
    stripe_rows = stripe_rows,
    n_stripes = n_stripes
  )


  # Caso principal: la celda exacta del clic ya es hidrologicamente valida.
  # No se mueve el outlet, aunque la mascara de cauces no la marque.
  if (isTRUE(diagnosis$reaches_threshold)) {
    return(
      build_snap_result(
        lon = lon,
        lat = lat,
        outlet_cell = click_cell,
        grid_template = grid_template,
        grid_crs = grid_crs,
        actual_stream_value = click_stream_value,
        mode = "CLICK_CELL_D8_VALID",
        diagnosis = diagnosis,
        threshold_cells = threshold_cells,
        threshold_km2 = threshold_km2
      )
    )
  }


  exact_area_km2 <- threshold_km2 *
    as.numeric(diagnosis$n_cells) /
    threshold_cells


  # Correccion extremadamente local. Solo se activa si el clic ya
  # representa al menos 70% del umbral, evitando que una quebrada
  # pequena salte a un rio grande en una confluencia cercana.
  min_fraction_cells <- threshold_cells *
    SNAP_D8_CORRECTION_MIN_FRACTION


  if (
    diagnosis$n_cells >= min_fraction_cells
  ) {

    epsg <- utm_epsg_point(
      lon,
      lat
    )

    correction_limit_m <- min(
      as.numeric(radius_m),
      SNAP_D8_CORRECTION_MAX_M
    )

    current <- as.double(
      click_cell
    )

    seen <- current
    step <- 0L


    repeat {

      step <- step + 1L

      if (step > 12L) {
        break
      }


      next_cell <- snap_downstream_cell(
        reverse_cache = reverse_cache,
        cell = current
      )

      if (
        !is.finite(next_cell) ||
        next_cell %in% seen
      ) {
        break
      }

      seen <- c(
        seen,
        next_cell
      )


      distance_m <- cell_distance_from_click_m(
        cell = next_cell,
        click_wgs = click_wgs,
        grid_template = grid_template,
        grid_crs = grid_crs,
        epsg = epsg
      )

      if (
        !is.finite(distance_m) ||
        distance_m > correction_limit_m
      ) {
        break
      }


      next_diagnosis <- count_upstream_until_threshold(
        cache = reverse_cache,
        outlet_cell = next_cell,
        threshold_cells = threshold_cells
      )


      if (isTRUE(next_diagnosis$reaches_threshold)) {

        next_stream_value <- stream_value_at_global_cell(
          cell = next_cell,
          grid_template = grid_template,
          stream_cache = stream_cache,
          stripe_rows = stripe_rows,
          n_stripes = n_stripes
        )

        return(
          build_snap_result(
            lon = lon,
            lat = lat,
            outlet_cell = next_cell,
            grid_template = grid_template,
            grid_crs = grid_crs,
            actual_stream_value = next_stream_value,
            mode = "D8_LOCAL_CORRECTION",
            diagnosis = next_diagnosis,
            threshold_cells = threshold_cells,
            threshold_km2 = threshold_km2
          )
        )
      }


      current <- next_cell
    }
  }


  stop(
    paste0(
      "Area aportante estimada en el punto: ",
      sprintf("%.2f", exact_area_km2),
      " km2. El minimo actualmente soportado es ",
      sprintf("%.2f", threshold_km2),
      " km2. El punto seleccionado no alcanza el umbral de delimitacion."
    )
  )
}

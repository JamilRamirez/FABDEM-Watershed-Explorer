# ============================================================
# R/snap_topologico.R
#
# SNAP LOCAL + TOPOLOGICO DEL PUNTO DE SALIDA
# ============================================================
#
# Objetivo:
# - evitar saltos laterales a un cauce mayor cerca de confluencias;
# - permitir cuencas pequenas aunque el punto no coincida con la
#   mascara de cauces precomputada;
# - usar REVERSE_D8 como referencia hidrologica principal.
#
# Estrategia:
# 1) si el clic cae sobre stream_stripes, se conserva;
# 2) si la propia celda clicada ya tiene al menos el area minima
#    de aporte, se usa directamente;
# 3) en caso contrario se sigue SOLO la trayectoria D8 aguas abajo
#    y se toma la primera celda que alcanza el umbral minimo;
# 4) la busqueda topologica se limita localmente para no cruzar
#    grandes distancias ni saltar de rama;
# 5) como ultimo respaldo se permite el snap geometrico original,
#    pero solo dentro de un radio local pequeno.
#
# helpers.R se carga antes de este archivo.
# ============================================================


snap_to_stream_stripes_nearest <- snap_to_stream_stripes


SNAP_TOPOLOGY_MAX_M <- 300
SNAP_LOCAL_FALLBACK_M <- 120


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
    nrows = as.double(
      meta[["NROWS"]][1]
    ),
    ncols = as.double(
      meta[["NCOLS"]][1]
    ),
    stripe_rows = as.double(
      meta[["STRIPE_ROWS"]][1]
    ),
    n_stripes = as.integer(
      meta[["N_STRIPES"]][1]
    )
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


snap_stream_value_at_cell <- function(
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
    ) / nc
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

  cell <- as.double(cell)

  row <- floor(
    (cell - 1) / nc
  ) + 1

  col <- (
    (cell - 1) %% nc
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

  # En REVERSE_D8 cada receptor codifica de que posiciones
  # vecinas recibe flujo. Para averiguar el receptor de la celda
  # actual hay que consultar el bit opuesto en cada vecino.
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


snap_upstream_reaches_threshold <- function(
    reverse_cache,
    outlet_cell,
    threshold_cells
) {

  threshold_cells <- as.double(
    threshold_cells
  )

  if (
    !is.finite(threshold_cells) ||
    threshold_cells <= 1
  ) {
    return(TRUE)
  }

  nc <- as.double(
    reverse_cache$metadata$ncols
  )

  nr <- as.double(
    reverse_cache$metadata$nrows
  )

  frontier <- as.double(
    outlet_cell
  )

  n_cells <- 1
  level <- 0L

  repeat {

    if (n_cells >= threshold_cells) {
      return(TRUE)
    }

    if (length(frontier) == 0L) {
      return(FALSE)
    }

    level <- level + 1L

    if (level > MAX_TRACE_LEVELS) {
      return(FALSE)
    }

    values_reverse <- get_reverse_values(
      reverse_cache,
      frontier
    )

    rows <- floor(
      (frontier - 1) / nc
    ) + 1

    cols <- (
      (frontier - 1) %% nc
    ) + 1

    parents <- numeric(0)

    use <- (
      bitwAnd(values_reverse, 1L) != 0L &
        rows > 1 &
        cols > 1
    )
    if (any(use)) {
      parents <- c(
        parents,
        frontier[use] - nc - 1
      )
    }

    use <- (
      bitwAnd(values_reverse, 2L) != 0L &
        rows > 1
    )
    if (any(use)) {
      parents <- c(
        parents,
        frontier[use] - nc
      )
    }

    use <- (
      bitwAnd(values_reverse, 4L) != 0L &
        rows > 1 &
        cols < nc
    )
    if (any(use)) {
      parents <- c(
        parents,
        frontier[use] - nc + 1
      )
    }

    use <- (
      bitwAnd(values_reverse, 8L) != 0L &
        cols > 1
    )
    if (any(use)) {
      parents <- c(
        parents,
        frontier[use] - 1
      )
    }

    use <- (
      bitwAnd(values_reverse, 16L) != 0L &
        cols < nc
    )
    if (any(use)) {
      parents <- c(
        parents,
        frontier[use] + 1
      )
    }

    use <- (
      bitwAnd(values_reverse, 32L) != 0L &
        rows < nr &
        cols > 1
    )
    if (any(use)) {
      parents <- c(
        parents,
        frontier[use] + nc - 1
      )
    }

    use <- (
      bitwAnd(values_reverse, 64L) != 0L &
        rows < nr
    )
    if (any(use)) {
      parents <- c(
        parents,
        frontier[use] + nc
      )
    }

    use <- (
      bitwAnd(values_reverse, 128L) != 0L &
        rows < nr &
        cols < nc
    )
    if (any(use)) {
      parents <- c(
        parents,
        frontier[use] + nc + 1
      )
    }

    if (length(parents) == 0L) {
      return(FALSE)
    }

    parents <- unique(
      parents
    )

    n_cells <- n_cells + length(parents)
    frontier <- parents
  }
}


snap_cell_utm_xy <- function(
    cell,
    grid_template,
    grid_crs,
    epsg
) {

  xy <- terra::xyFromCell(
    grid_template,
    as.double(cell)
  )

  p_grid <- sf::st_sfc(
    sf::st_point(
      c(
        xy[1, 1],
        xy[1, 2]
      )
    ),
    crs = grid_crs
  )

  p_utm <- sf::st_transform(
    p_grid,
    epsg
  )

  as.numeric(
    sf::st_coordinates(
      p_utm
    )[1, ]
  )
}


snap_build_result <- function(
    lon,
    lat,
    outlet_cell,
    grid_template,
    grid_crs,
    stream_value,
    mode
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

  click_utm <- sf::st_transform(
    click_wgs,
    epsg
  )

  outlet_utm <- sf::st_transform(
    outlet_wgs,
    epsg
  )

  distance_final <- as.numeric(
    sf::st_distance(
      click_utm,
      outlet_utm
    )
  )

  list(
    clicked_lon = lon,
    clicked_lat = lat,
    outlet_cell = as.double(outlet_cell),
    outlet_x = outlet_xy[1, 1],
    outlet_y = outlet_xy[1, 2],
    outlet_lon = outlet_wgs_xy[1],
    outlet_lat = outlet_wgs_xy[2],
    snap_mode = mode,
    snap_distance_m = distance_final,
    stream_mask_value = as.numeric(stream_value)
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

  if (
    !is.finite(radius_m) ||
    radius_m <= 0
  ) {
    stop(
      "El radio interno de ajuste debe ser mayor que cero."
    )
  }

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

  click_stream_value <- snap_stream_value_at_cell(
    cell = click_cell,
    grid_template = grid_template,
    stream_cache = stream_cache,
    stripe_rows = stripe_rows,
    n_stripes = n_stripes
  )

  if (
    is.finite(click_stream_value) &&
    click_stream_value > 0
  ) {
    return(
      snap_build_result(
        lon = lon,
        lat = lat,
        outlet_cell = click_cell,
        grid_template = grid_template,
        grid_crs = grid_crs,
        stream_value = click_stream_value,
        mode = "CLICK_ON_STREAM_CELL"
      )
    )
  }

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

  if (
    !is.finite(threshold_cells) ||
    threshold_cells < 1
  ) {
    stop(
      "STREAM_THRESHOLD_CELLS invalido para el bloque."
    )
  }

  reverse_cache <- new_snap_reverse_cache(
    block_id
  )

  # La mascara de stream es auxiliar. Si el propio pixel clicado
  # ya posee el aporte minimo, se acepta aunque la mascara no lo
  # haya marcado exactamente.
  if (
    snap_upstream_reaches_threshold(
      reverse_cache = reverse_cache,
      outlet_cell = click_cell,
      threshold_cells = threshold_cells
    )
  ) {
    return(
      snap_build_result(
        lon = lon,
        lat = lat,
        outlet_cell = click_cell,
        grid_template = grid_template,
        grid_crs = grid_crs,
        # Sentinel interno de validez hidrologica. La comprobacion
        # definitiva vuelve a hacerse con trace$n_cells.
        stream_value = 1,
        mode = "CLICK_CELL_D8_VALIDATED"
      )
    )
  }

  epsg <- utm_epsg_point(
    lon,
    lat
  )

  click_utm <- sf::st_transform(
    click_wgs,
    epsg
  )

  click_utm_xy <- as.numeric(
    sf::st_coordinates(
      click_utm
    )[1, ]
  )

  current <- as.double(
    click_cell
  )

  current_utm_xy <- snap_cell_utm_xy(
    cell = current,
    grid_template = grid_template,
    grid_crs = grid_crs,
    epsg = epsg
  )

  travelled_m <- sqrt(
    sum(
      (current_utm_xy - click_utm_xy)^2
    )
  )

  topology_limit_m <- min(
    as.double(radius_m),
    SNAP_TOPOLOGY_MAX_M
  )

  seen <- new.env(
    hash = TRUE,
    parent = emptyenv()
  )

  assign(
    as.character(current),
    TRUE,
    envir = seen
  )

  step <- 0L

  while (
    step < 256L &&
    travelled_m <= topology_limit_m
  ) {

    step <- step + 1L

    next_cell <- snap_downstream_cell(
      reverse_cache = reverse_cache,
      cell = current
    )

    if (!is.finite(next_cell)) {
      break
    }

    key <- as.character(
      next_cell
    )

    if (exists(
      key,
      envir = seen,
      inherits = FALSE
    )) {
      break
    }

    assign(
      key,
      TRUE,
      envir = seen
    )

    next_utm_xy <- snap_cell_utm_xy(
      cell = next_cell,
      grid_template = grid_template,
      grid_crs = grid_crs,
      epsg = epsg
    )

    step_distance_m <- sqrt(
      sum(
        (next_utm_xy - current_utm_xy)^2
      )
    )

    if (!is.finite(step_distance_m)) {
      break
    }

    travelled_m <- travelled_m + step_distance_m

    if (travelled_m > topology_limit_m) {
      break
    }

    if (
      snap_upstream_reaches_threshold(
        reverse_cache = reverse_cache,
        outlet_cell = next_cell,
        threshold_cells = threshold_cells
      )
    ) {
      stream_value <- snap_stream_value_at_cell(
        cell = next_cell,
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes
      )

      if (
        !is.finite(stream_value) ||
        stream_value <= 0
      ) {
        stream_value <- 1
      }

      return(
        snap_build_result(
          lon = lon,
          lat = lat,
          outlet_cell = next_cell,
          grid_template = grid_template,
          grid_crs = grid_crs,
          stream_value = stream_value,
          mode = "D8_LOCAL_THRESHOLD_VALIDATED"
        )
      )
    }

    current <- next_cell
    current_utm_xy <- next_utm_xy
  }

  # Respaldo geometrico muy local. A diferencia del comportamiento
  # anterior, nunca se usa el radio completo de 1500 m para buscar
  # lateralmente un cauce, porque eso favorecia saltos a ramas
  # principales cerca de confluencias.
  fallback_radius_m <- min(
    as.double(radius_m),
    SNAP_LOCAL_FALLBACK_M
  )

  fallback <- tryCatch(
    snap_to_stream_stripes_nearest(
      lon = lon,
      lat = lat,
      radius_m = fallback_radius_m,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes
    ),
    error = function(e) NULL
  )

  if (!is.null(fallback)) {
    fallback$snap_mode <- paste0(
      "LOCAL_FALLBACK_",
      fallback$snap_mode
    )

    return(fallback)
  }

  stop(
    paste0(
      "No se encontro un punto de salida hidrologicamente valido cerca del clic. ",
      "El ajuste queda limitado a ",
      round(topology_limit_m),
      " m sobre la trayectoria D8 y ",
      round(fallback_radius_m),
      " m de busqueda lateral para evitar saltar a otra quebrada."
    )
  )
}

# ============================================================
# R/snap_topologico.R
#
# SNAP TOPOLOGICO DEL PUNTO DE SALIDA
# ============================================================
#
# Corrige el caso clasico cerca de confluencias: el ajuste no
# debe saltar lateralmente a un cauce mayor solo porque una celda
# de la red este mas cerca del clic.
#
# Estrategia:
# 1) Si el clic ya cae sobre stream_stripes, se conserva.
# 2) Si no, se reconstruye el receptor D8 desde REVERSE_D8 y se
#    sigue UNICAMENTE la trayectoria aguas abajo del clic.
# 3) Se usa la primera celda de stream_stripes encontrada dentro
#    del radio de snap.
# 4) Si no puede resolverse la trayectoria, se conserva como
#    respaldo el snap geometrico anterior.
#
# helpers.R se carga antes de este archivo. Guardamos la version
# anterior para disponer de un fallback compatible.
# ============================================================


snap_to_stream_stripes_nearest <- snap_to_stream_stripes


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
      as.double(cell) -
        1
    ) /
      nc
  ) +
    1


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


  # Candidatos receptores respecto de la celda actual:
  # NW, N, NE, W, E, SW, S, SE.
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


  # REVERSE_D8 guarda, en cada receptor, donde estan sus padres.
  # Si el receptor esta al E de la celda actual, por ejemplo,
  # la celda actual es el padre W del receptor y debe estar
  # encendido el bit 8. De ahi el orden inverso siguiente.
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


  targets <- cell +
    offsets[valid]


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
    ) !=
      0L
  )


  if (length(hit) != 1L) {
    return(NA_real_)
  }


  as.double(
    targets[hit]
  )
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


  # Si ya estamos sobre la red, no hay nada que corregir.
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


  reverse_cache <- new_snap_reverse_cache(
    block_id
  )


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


  path_distance_m <- sqrt(
    sum(
      (
        current_utm_xy -
          click_utm_xy
      )^2
    )
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


  max_steps <- min(
    10000L,
    max(
      64L,
      as.integer(
        ceiling(
          radius_m / 10
        )
      ) +
        64L
    )
  )


  step <- 0L
  followed_steps <- 0L


  while (
    step < max_steps &&
    path_distance_m <= radius_m
  ) {

    step <- step +
      1L


    next_cell <- snap_downstream_cell(
      reverse_cache = reverse_cache,
      cell = current
    )


    if (!is.finite(next_cell)) {
      break
    }


    followed_steps <- followed_steps +
      1L


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
        (
          next_utm_xy -
            current_utm_xy
        )^2
      )
    )


    if (!is.finite(step_distance_m)) {
      break
    }


    path_distance_m <- path_distance_m +
      step_distance_m


    if (path_distance_m > radius_m) {
      break
    }


    stream_value <- snap_stream_value_at_cell(
      cell = next_cell,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes
    )


    if (
      is.finite(stream_value) &&
      stream_value > 0
    ) {
      return(
        snap_build_result(
          lon = lon,
          lat = lat,
          outlet_cell = next_cell,
          grid_template = grid_template,
          grid_crs = grid_crs,
          stream_value = stream_value,
          mode = "DOWNSTREAM_TOPOLOGICAL_STREAM_CELL"
        )
      )
    }


    current <- next_cell
    current_utm_xy <- next_utm_xy
  }


  # Si la topologia D8 pudo seguirse, NO se permite saltar a
  # otra rama por proximidad geometrica. Es preferible pedir un
  # clic mas cercano que devolver una cuenca hidrologicamente
  # distinta a la solicitada.
  if (followed_steps > 0L) {
    stop(
      paste0(
        "No se alcanzo una celda de cauce siguiendo la trayectoria D8 ",
        "aguas abajo dentro de ",
        radius_m,
        " m. Acerca el punto de salida al cauce que deseas delimitar."
      )
    )
  }


  # Respaldo excepcional: solo si REVERSE_D8 no permitio obtener
  # ni un receptor desde la celda clicada (borde o indice anomalo).
  fallback <- snap_to_stream_stripes_nearest(
    lon = lon,
    lat = lat,
    radius_m = radius_m,
    grid_template = grid_template,
    stream_cache = stream_cache,
    stripe_rows = stripe_rows,
    n_stripes = n_stripes
  )


  fallback$snap_mode <- paste0(
    "TOPOLOGY_FALLBACK_",
    fallback$snap_mode
  )


  fallback
}

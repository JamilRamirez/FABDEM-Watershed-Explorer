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
# - tolerar el desfase cartografico entre el rio visible en el mapa
#   y el eje hidrologico D8, especialmente en rios anchos;
# - usar REVERSE_D8 como referencia hidrologica principal.
#
# Estrategia:
# 1) si el clic cae sobre stream_stripes, se conserva;
# 2) si la propia celda clicada ya tiene al menos el area minima
#    de aporte, se usa directamente;
# 3) en caso contrario se sigue SOLO la trayectoria D8 aguas abajo
#    hasta SNAP_TOPOLOGY_MAX_M;
# 4) si aun no hay salida valida, se examina la red en radios
#    progresivos y se detiene en el primer radio con candidatos;
# 5) dentro de ese primer radio se elige el cauce mas cercano, no
#    el de mayor acumulacion;
# 6) si dos componentes de red distintos son casi igual de cercanos,
#    el punto se considera ambiguo y se pide un clic mas preciso.
#
# helpers.R se carga antes de este archivo.
# ============================================================


# Conserva la implementacion geometrica original de helpers.R por
# compatibilidad y como referencia, aunque el fallback progresivo
# actual inspecciona directamente stream_stripes.
snap_to_stream_stripes_nearest <- snap_to_stream_stripes


SNAP_TOPOLOGY_MAX_M <- 300
SNAP_PROGRESSIVE_RADII_M <- c(120, 250, 500, 1000, 1500)
SNAP_AMBIGUITY_ABS_M <- 40
SNAP_AMBIGUITY_RATIO <- 1.25


new_snap_reverse_cache <- function(block_id) {

  meta <- get_block_metadata(block_id)
  reverse_rows <- get_block_assets(block_id, "reverse")

  e <- new.env(parent = emptyenv())
  e$block_id <- block_id
  e$metadata <- list(
    nrows = as.double(meta[["NROWS"]][1]),
    ncols = as.double(meta[["NCOLS"]][1]),
    stripe_rows = as.double(meta[["STRIPE_ROWS"]][1]),
    n_stripes = as.integer(meta[["N_STRIPES"]][1])
  )
  e$stripe_files <- as.character(reverse_rows[["LOCAL_PATH"]])
  e$values <- new.env(hash = TRUE, parent = emptyenv())
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

  nc <- as.double(terra::ncol(grid_template))
  row <- floor((as.double(cell) - 1) / nc) + 1

  sid <- stripe_id_from_row(
    row = as.integer(row),
    stripe_rows = as.integer(stripe_rows),
    n_stripes = as.integer(n_stripes)
  )

  stream_r <- load_stream_stripe(stream_cache, sid)
  xy <- terra::xyFromCell(grid_template, as.double(cell))

  extract_stream_value_at_xy(
    stream_r,
    c(xy[1, 1], xy[1, 2])
  )
}


snap_downstream_cell <- function(
    reverse_cache,
    cell
) {

  nc <- as.double(reverse_cache$metadata$ncols)
  nr <- as.double(reverse_cache$metadata$nrows)
  cell <- as.double(cell)

  row <- floor((cell - 1) / nc) + 1
  col <- ((cell - 1) %% nc) + 1

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

  # En REVERSE_D8 cada receptor codifica desde que posiciones
  # vecinas recibe flujo. Para hallar el receptor de la celda
  # actual se consulta el bit opuesto en cada vecino.
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

  values_reverse <- get_reverse_values(reverse_cache, targets)

  hit <- which(
    bitwAnd(values_reverse, bits) != 0L
  )

  if (length(hit) != 1L) {
    return(NA_real_)
  }

  as.double(targets[hit])
}


snap_upstream_reaches_threshold <- function(
    reverse_cache,
    outlet_cell,
    threshold_cells
) {

  threshold_cells <- as.double(threshold_cells)

  if (!is.finite(threshold_cells) || threshold_cells <= 1) {
    return(TRUE)
  }

  nc <- as.double(reverse_cache$metadata$ncols)
  nr <- as.double(reverse_cache$metadata$nrows)

  frontier <- as.double(outlet_cell)
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

    rows <- floor((frontier - 1) / nc) + 1
    cols <- ((frontier - 1) %% nc) + 1
    parents <- numeric(0)

    use <- bitwAnd(values_reverse, 1L) != 0L & rows > 1 & cols > 1
    if (any(use)) parents <- c(parents, frontier[use] - nc - 1)

    use <- bitwAnd(values_reverse, 2L) != 0L & rows > 1
    if (any(use)) parents <- c(parents, frontier[use] - nc)

    use <- bitwAnd(values_reverse, 4L) != 0L & rows > 1 & cols < nc
    if (any(use)) parents <- c(parents, frontier[use] - nc + 1)

    use <- bitwAnd(values_reverse, 8L) != 0L & cols > 1
    if (any(use)) parents <- c(parents, frontier[use] - 1)

    use <- bitwAnd(values_reverse, 16L) != 0L & cols < nc
    if (any(use)) parents <- c(parents, frontier[use] + 1)

    use <- bitwAnd(values_reverse, 32L) != 0L & rows < nr & cols > 1
    if (any(use)) parents <- c(parents, frontier[use] + nc - 1)

    use <- bitwAnd(values_reverse, 64L) != 0L & rows < nr
    if (any(use)) parents <- c(parents, frontier[use] + nc)

    use <- bitwAnd(values_reverse, 128L) != 0L & rows < nr & cols < nc
    if (any(use)) parents <- c(parents, frontier[use] + nc + 1)

    if (length(parents) == 0L) {
      return(FALSE)
    }

    parents <- unique(parents)
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
    sf::st_point(c(xy[1, 1], xy[1, 2])),
    crs = grid_crs
  )

  p_utm <- sf::st_transform(p_grid, epsg)
  as.numeric(sf::st_coordinates(p_utm)[1, ])
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
    sf::st_point(c(outlet_xy[1, 1], outlet_xy[1, 2])),
    crs = grid_crs
  )

  outlet_wgs <- sf::st_transform(outlet_grid, 4326)
  outlet_wgs_xy <- sf::st_coordinates(outlet_wgs)[1, ]

  click_wgs <- sf::st_sfc(
    sf::st_point(c(lon, lat)),
    crs = 4326
  )

  epsg <- utm_epsg_point(lon, lat)
  click_utm <- sf::st_transform(click_wgs, epsg)
  outlet_utm <- sf::st_transform(outlet_wgs, epsg)

  distance_final <- as.numeric(
    sf::st_distance(click_utm, outlet_utm)
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


# ============================================================
# FALLBACK PROGRESIVO SOBRE STREAM_STRIPES
# ============================================================

snap_progressive_radii <- function(max_radius_m) {

  max_radius_m <- suppressWarnings(as.numeric(max_radius_m))

  if (!is.finite(max_radius_m) || max_radius_m <= 0) {
    stop("El radio maximo del snap progresivo debe ser mayor que cero.")
  }

  radii <- SNAP_PROGRESSIVE_RADII_M[
    SNAP_PROGRESSIVE_RADII_M <= max_radius_m
  ]

  if (length(radii) == 0L) {
    return(max_radius_m)
  }

  if (tail(radii, 1) < max_radius_m) {
    radii <- c(radii, max_radius_m)
  }

  unique(sort(as.numeric(radii)))
}


snap_stream_component_ids <- function(
    cells,
    grid_ncols
) {

  cells <- as.double(cells)
  grid_ncols <- as.double(grid_ncols)

  if (length(cells) == 0L) {
    return(integer(0))
  }

  if (length(cells) == 1L) {
    return(1L)
  }

  rows <- floor((cells - 1) / grid_ncols) + 1
  cols <- ((cells - 1) %% grid_ncols) + 1

  rmin <- min(rows)
  rmax <- max(rows)
  cmin <- min(cols)
  cmax <- max(cols)

  local_nrows <- as.integer(rmax - rmin + 1)
  local_ncols <- as.integer(cmax - cmin + 1)

  local_pos <- as.integer(
    (rows - rmin) * local_ncols +
      (cols - cmin) +
      1
  )

  local_r <- terra::rast(
    nrows = local_nrows,
    ncols = local_ncols,
    xmin = 0,
    xmax = local_ncols,
    ymin = 0,
    ymax = local_nrows
  )

  vals <- rep(NA_integer_, terra::ncell(local_r))
  vals[local_pos] <- 1L
  terra::values(local_r) <- vals

  patches <- terra::patches(
    local_r,
    directions = 8,
    values = FALSE,
    zeroAsNA = TRUE,
    allowGaps = FALSE
  )

  patch_vals <- terra::values(
    patches,
    mat = FALSE
  )[local_pos]

  patch_vals <- as.integer(patch_vals)

  if (anyNA(patch_vals)) {
    stop("No fue posible identificar componentes de la red durante el snap.")
  }

  patch_vals
}


snap_collect_stream_candidates <- function(
    lon,
    lat,
    radius_m,
    grid_template,
    stream_cache,
    stripe_rows,
    n_stripes
) {

  grid_crs <- sf::st_crs(
    terra::crs(grid_template)
  )

  if (is.na(grid_crs)) {
    stop("La grilla del bloque no tiene CRS valido.")
  }

  click_wgs <- sf::st_sfc(
    sf::st_point(c(lon, lat)),
    crs = 4326
  )

  epsg <- utm_epsg_point(lon, lat)
  click_utm <- sf::st_transform(click_wgs, epsg)

  search_utm <- sf::st_buffer(
    click_utm,
    dist = radius_m
  )

  search_grid <- sf::st_transform(
    search_utm,
    grid_crs
  )

  bb <- sf::st_bbox(search_grid)

  search_extent <- terra::ext(
    as.numeric(bb["xmin"]),
    as.numeric(bb["xmax"]),
    as.numeric(bb["ymin"]),
    as.numeric(bb["ymax"])
  )

  row_top <- grid_row_from_y(
    grid_template,
    as.numeric(bb["ymax"])
  )

  row_bottom <- grid_row_from_y(
    grid_template,
    as.numeric(bb["ymin"])
  )

  sid_first <- stripe_id_from_row(
    row = min(row_top, row_bottom),
    stripe_rows = as.integer(stripe_rows),
    n_stripes = as.integer(n_stripes)
  )

  sid_last <- stripe_id_from_row(
    row = max(row_top, row_bottom),
    stripe_rows = as.integer(stripe_rows),
    n_stripes = as.integer(n_stripes)
  )

  candidate_parts <- list()
  sid <- sid_first

  while (sid <= sid_last) {

    stream_r <- load_stream_stripe(
      stream_cache,
      sid
    )

    local_mask <- tryCatch(
      terra::crop(
        stream_r,
        search_extent,
        snap = "out"
      ),
      error = function(e) NULL
    )

    if (
      !is.null(local_mask) &&
      terra::ncell(local_mask) > 0
    ) {

      local_values <- terra::values(
        local_mask,
        mat = FALSE
      )

      local_cells <- which(
        !is.na(local_values) & local_values > 0
      )

      if (length(local_cells) > 0L) {

        xy_part <- terra::xyFromCell(
          local_mask,
          local_cells
        )

        global_cells <- terra::cellFromXY(
          grid_template,
          xy_part
        )

        candidate_parts[[length(candidate_parts) + 1L]] <- data.frame(
          cell = as.double(global_cells),
          x = as.numeric(xy_part[, 1]),
          y = as.numeric(xy_part[, 2]),
          stringsAsFactors = FALSE
        )
      }
    }

    sid <- sid + 1L
  }

  if (length(candidate_parts) == 0L) {
    return(
      data.frame(
        cell = numeric(0),
        x = numeric(0),
        y = numeric(0),
        distance_m = numeric(0),
        stringsAsFactors = FALSE
      )
    )
  }

  candidates <- do.call(
    rbind,
    candidate_parts
  )

  candidates <- candidates[
    is.finite(candidates$cell),
    ,
    drop = FALSE
  ]

  candidates <- candidates[
    !duplicated(candidates$cell),
    ,
    drop = FALSE
  ]

  if (nrow(candidates) == 0L) {
    candidates$distance_m <- numeric(0)
    return(candidates)
  }

  candidate_sf <- sf::st_as_sf(
    candidates,
    coords = c("x", "y"),
    crs = grid_crs,
    remove = FALSE
  )

  candidate_utm <- sf::st_transform(
    candidate_sf,
    epsg
  )

  candidates$distance_m <- as.numeric(
    sf::st_distance(
      candidate_utm,
      click_utm
    )
  )

  candidates <- candidates[
    is.finite(candidates$distance_m) &
      candidates$distance_m <= radius_m,
    ,
    drop = FALSE
  ]

  candidates <- candidates[
    order(candidates$distance_m),
    ,
    drop = FALSE
  ]

  rownames(candidates) <- NULL
  candidates
}


snap_select_progressive_candidate <- function(
    candidates,
    radii_m,
    grid_ncols,
    ambiguity_abs_m = SNAP_AMBIGUITY_ABS_M,
    ambiguity_ratio = SNAP_AMBIGUITY_RATIO
) {

  if (
    is.null(candidates) ||
    nrow(candidates) == 0L
  ) {
    return(list(status = "none"))
  }

  required <- c("cell", "distance_m")

  if (!all(required %in% names(candidates))) {
    stop("Candidatos incompletos para el snap progresivo.")
  }

  radii_m <- sort(unique(as.numeric(radii_m)))
  radii_m <- radii_m[is.finite(radii_m) & radii_m > 0]

  if (length(radii_m) == 0L) {
    stop("No hay radios validos para el snap progresivo.")
  }

  for (radius_now in radii_m) {

    inside <- which(
      is.finite(candidates$distance_m) &
        candidates$distance_m <= radius_now
    )

    if (length(inside) == 0L) {
      next
    }

    component_ids <- snap_stream_component_ids(
      cells = candidates$cell[inside],
      grid_ncols = grid_ncols
    )

    components <- unique(component_ids)

    best_local <- vapply(
      components,
      function(component_id) {
        idx_component <- which(component_ids == component_id)
        idx_component[
          which.min(candidates$distance_m[inside[idx_component]])
        ]
      },
      integer(1)
    )

    best_global <- inside[best_local]

    best_global <- best_global[
      order(candidates$distance_m[best_global])
    ]

    winner_index <- best_global[1]
    winner_distance <- candidates$distance_m[winner_index]

    if (length(best_global) >= 2L) {

      alternative_index <- best_global[2]
      alternative_distance <- candidates$distance_m[alternative_index]

      close_absolute <- (
        alternative_distance <=
          winner_distance + ambiguity_abs_m
      )

      close_relative <- (
        alternative_distance <=
          winner_distance * ambiguity_ratio
      )

      if (isTRUE(close_absolute && close_relative)) {
        return(
          list(
            status = "ambiguous",
            radius_m = radius_now,
            winner_index = winner_index,
            alternative_index = alternative_index,
            winner_distance_m = winner_distance,
            alternative_distance_m = alternative_distance,
            n_components = length(components)
          )
        )
      }
    }

    return(
      list(
        status = "ok",
        radius_m = radius_now,
        winner_index = winner_index,
        winner_distance_m = winner_distance,
        n_components = length(components)
      )
    )
  }

  list(status = "none")
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

  if (!is.finite(radius_m) || radius_m <= 0) {
    stop("El radio interno de ajuste debe ser mayor que cero.")
  }

  grid_crs <- sf::st_crs(
    terra::crs(grid_template)
  )

  if (is.na(grid_crs)) {
    stop("La grilla del bloque no tiene CRS valido.")
  }

  click_wgs <- sf::st_sfc(
    sf::st_point(c(lon, lat)),
    crs = 4326
  )

  click_grid <- sf::st_transform(
    click_wgs,
    grid_crs
  )

  click_xy <- sf::st_coordinates(click_grid)[1, ]

  click_cell <- terra::cellFromXY(
    grid_template,
    matrix(
      c(click_xy[1], click_xy[2]),
      nrow = 1
    )
  )

  if (is.na(click_cell)) {
    stop("El clic quedo fuera de la grilla hidrologica del bloque.")
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

  block_id <- as.character(stream_cache$block_id)
  meta <- get_block_metadata(block_id)

  threshold_cells <- suppressWarnings(
    as.double(meta[["STREAM_THRESHOLD_CELLS"]][1])
  )

  if (!is.finite(threshold_cells) || threshold_cells < 1) {
    stop("STREAM_THRESHOLD_CELLS invalido para el bloque.")
  }

  reverse_cache <- new_snap_reverse_cache(block_id)

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
        stream_value = 1,
        mode = "CLICK_CELL_D8_VALIDATED"
      )
    )
  }

  epsg <- utm_epsg_point(lon, lat)
  click_utm <- sf::st_transform(click_wgs, epsg)
  click_utm_xy <- as.numeric(sf::st_coordinates(click_utm)[1, ])

  current <- as.double(click_cell)

  current_utm_xy <- snap_cell_utm_xy(
    cell = current,
    grid_template = grid_template,
    grid_crs = grid_crs,
    epsg = epsg
  )

  travelled_m <- sqrt(
    sum((current_utm_xy - click_utm_xy)^2)
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

    key <- as.character(next_cell)

    if (exists(key, envir = seen, inherits = FALSE)) {
      break
    }

    assign(key, TRUE, envir = seen)

    next_utm_xy <- snap_cell_utm_xy(
      cell = next_cell,
      grid_template = grid_template,
      grid_crs = grid_crs,
      epsg = epsg
    )

    step_distance_m <- sqrt(
      sum((next_utm_xy - current_utm_xy)^2)
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

      if (!is.finite(stream_value) || stream_value <= 0) {
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

  # Si la trayectoria D8 local no alcanza una salida valida, se
  # permite una busqueda lateral progresiva. La red se inspecciona
  # una sola vez hasta el radio maximo y luego se escoge el primer
  # anillo que contiene candidatos.
  radii_m <- snap_progressive_radii(radius_m)

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

  if (identical(choice$status, "ambiguous")) {
    stop(
      paste0(
        "El punto queda entre dos cauces hidrologicos distintos a distancias muy similares (",
        round(choice$winner_distance_m),
        " y ",
        round(choice$alternative_distance_m),
        " m). Para evitar seleccionar una rama incorrecta cerca de una confluencia, ",
        "haz clic un poco mas cerca del cauce deseado."
      )
    )
  }

  if (!identical(choice$status, "ok")) {
    stop(
      paste0(
        "No se encontro un punto de salida hidrologicamente valido cerca del clic. ",
        "Se siguio la trayectoria D8 hasta ",
        round(topology_limit_m),
        " m y se busco la red progresivamente hasta ",
        round(max(radii_m)),
        " m sin encontrar un cauce valido."
      )
    )
  }

  winner <- candidates[
    choice$winner_index,
    ,
    drop = FALSE
  ]

  outlet_cell <- as.double(winner$cell[1])

  # Verificacion independiente: el candidato de stream_stripes
  # debe alcanzar realmente el umbral usando REVERSE_D8.
  if (
    !snap_upstream_reaches_threshold(
      reverse_cache = reverse_cache,
      outlet_cell = outlet_cell,
      threshold_cells = threshold_cells
    )
  ) {
    stop(
      "Inconsistencia hidrologica: el candidato de stream_stripes no alcanza el umbral D8 esperado."
    )
  }

  stream_value <- snap_stream_value_at_cell(
    cell = outlet_cell,
    grid_template = grid_template,
    stream_cache = stream_cache,
    stripe_rows = stripe_rows,
    n_stripes = n_stripes
  )

  if (!is.finite(stream_value) || stream_value <= 0) {
    stop(
      "Fallo de seguridad: el candidato progresivo no pertenece a stream_stripes."
    )
  }

  snap_build_result(
    lon = lon,
    lat = lat,
    outlet_cell = outlet_cell,
    grid_template = grid_template,
    grid_crs = grid_crs,
    stream_value = stream_value,
    mode = paste0(
      "PROGRESSIVE_NEAREST_",
      round(choice$radius_m),
      "M"
    )
  )
}


# ============================================================
# HELPERS DE AMBIGUEDAD TOPOLOGICA
# ============================================================
# Trabajan sobre caminos D8 ordenados aguas abajo. Se mantienen
# fuera del modulo Shiny para poder probarlos en CI sin Runtime.

snap_ambiguity_first_merge <- function(path_a, path_b) {

  path_a <- as.double(path_a)
  path_b <- as.double(path_b)

  if (length(path_a) == 0L || length(path_b) == 0L) {
    return(NULL)
  }

  common <- intersect(path_a, path_b)

  if (length(common) == 0L) {
    return(NULL)
  }

  ia <- match(common, path_a)
  ib <- match(common, path_b)
  pick <- which.min(ia + ib)

  list(
    cell = as.double(common[pick]),
    index_a = as.integer(ia[pick]),
    index_b = as.integer(ib[pick]),
    same_lineage = isTRUE(ia[pick] == 1L || ib[pick] == 1L)
  )
}


snap_ambiguity_same_lineage <- function(path_a, path_b) {
  merge <- snap_ambiguity_first_merge(path_a, path_b)
  !is.null(merge) && isTRUE(merge$same_lineage)
}


from pathlib import Path

DELIM = Path('R/delimitacion.R')
SNAP = Path('R/snap_topologico.R')
SMOKE = Path('tests/ci/smoke_app.R')

# ------------------------------------------------------------
# 1. Helpers topologicos puros y testeables
# ------------------------------------------------------------
snap_text = SNAP.read_text(encoding='utf-8')
helper_marker = '# ============================================================\n# HELPERS DE AMBIGUEDAD TOPOLOGICA\n# ============================================================'
helper_code = r'''


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
'''

if helper_marker not in snap_text:
    snap_text = snap_text.rstrip() + helper_code + '\n'
    SNAP.write_text(snap_text, encoding='utf-8')

# ------------------------------------------------------------
# 2. Reemplazar detector heuristico por detector ligado a D8
# ------------------------------------------------------------
delim_text = DELIM.read_text(encoding='utf-8')
start_marker = '  # ==========================================================\n  # RESOLUCION EXPLICITA DE AMBIGUEDAD HIDROLOGICA\n  # =========================================================='
end_marker = '\n\n\n  ui <- function(id) {'

start = delim_text.find(start_marker)
end = delim_text.find(end_marker, start)
if start < 0 or end < 0:
    raise SystemExit('No se encontro el bloque de ambiguedad en R/delimitacion.R')

new_block = r'''  # ==========================================================
  # RESOLUCION EXPLICITA DE AMBIGUEDAD HIDROLOGICA
  # ==========================================================
  # Principio:
  # - primero se obtiene el snap estable;
  # - una confluencia solo es ambigua si DOS caminos D8 asociados
  #   al entorno inmediato del clic convergen cerca del clic;
  # - para rios anchos/multicanal no se usan componentes espaciales:
  #   se comparan caminos D8, aunque pertenezcan a una misma red;
  # - celdas sobre la misma trayectoria se colapsan como una sola
  #   alternativa.

  AMBIGUITY_JUNCTION_MAX_M <- 450
  AMBIGUITY_JUNCTION_MARGIN_M <- 120
  AMBIGUITY_WIDE_TRIGGER_M <- 300
  AMBIGUITY_WIDE_SCAN_M <- 2200
  AMBIGUITY_WIDE_PATH_M <- 6000
  AMBIGUITY_WIDE_MIN_PARALLEL <- 0.20
  AMBIGUITY_MAX_OPTIONS <- 3L


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
      grid_crs = sf::st_crs(terra::crs(grid_template)),
      stream_value = stream_value,
      mode = mode
    )

    out$ambiguity_role <- role
    out
  }


  ambiguity_downstream_path <- function(
      start_cell,
      reverse_cache,
      grid_template,
      click_lon,
      click_lat,
      max_distance_m = 1500,
      max_steps = 256L
  ) {

    grid_crs <- sf::st_crs(terra::crs(grid_template))
    epsg <- utm_epsg_point(click_lon, click_lat)

    click_wgs <- sf::st_sfc(
      sf::st_point(c(click_lon, click_lat)),
      crs = 4326
    )
    click_xy <- as.numeric(
      sf::st_coordinates(sf::st_transform(click_wgs, epsg))[1, ]
    )

    current <- as.double(start_cell)
    current_xy <- snap_cell_utm_xy(
      cell = current,
      grid_template = grid_template,
      grid_crs = grid_crs,
      epsg = epsg
    )

    cells <- current
    along_m <- 0
    click_distance_m <- sqrt(sum((current_xy - click_xy)^2))
    xy <- matrix(current_xy, nrow = 1)
    seen <- new.env(hash = TRUE, parent = emptyenv())
    assign(as.character(current), TRUE, envir = seen)

    for (step in seq_len(max_steps)) {
      next_cell <- snap_downstream_cell(reverse_cache, current)

      if (!is.finite(next_cell)) {
        break
      }

      key <- as.character(next_cell)
      if (exists(key, envir = seen, inherits = FALSE)) {
        break
      }
      assign(key, TRUE, envir = seen)

      next_xy <- snap_cell_utm_xy(
        cell = next_cell,
        grid_template = grid_template,
        grid_crs = grid_crs,
        epsg = epsg
      )

      step_m <- sqrt(sum((next_xy - current_xy)^2))
      if (!is.finite(step_m)) {
        break
      }

      new_along <- tail(along_m, 1) + step_m
      if (new_along > max_distance_m) {
        break
      }

      cells <- c(cells, as.double(next_cell))
      along_m <- c(along_m, new_along)
      click_distance_m <- c(
        click_distance_m,
        sqrt(sum((next_xy - click_xy)^2))
      )
      xy <- rbind(xy, next_xy)

      current <- as.double(next_cell)
      current_xy <- next_xy
    }

    list(
      cells = cells,
      along_m = along_m,
      click_distance_m = click_distance_m,
      xy = xy
    )
  }


  ambiguity_direction_similarity <- function(path_a, path_b) {

    direction <- function(path) {
      if (is.null(path$xy) || nrow(path$xy) < 2L) {
        return(c(NA_real_, NA_real_))
      }
      idx <- min(8L, nrow(path$xy))
      as.numeric(path$xy[idx, ] - path$xy[1, ])
    }

    va <- direction(path_a)
    vb <- direction(path_b)
    na <- sqrt(sum(va^2))
    nb <- sqrt(sum(vb^2))

    if (!is.finite(na) || !is.finite(nb) || na <= 0 || nb <= 0) {
      return(NA_real_)
    }

    max(-1, min(1, sum(va * vb) / (na * nb)))
  }


  ambiguity_thin_candidates <- function(
      candidates,
      grid_template,
      lon,
      lat,
      min_separation_m = 90,
      max_seeds = 36L
  ) {

    if (is.null(candidates) || nrow(candidates) <= 1L) {
      return(candidates)
    }

    grid_crs <- sf::st_crs(terra::crs(grid_template))
    epsg <- utm_epsg_point(lon, lat)

    sf_candidates <- sf::st_as_sf(
      candidates,
      coords = c('x', 'y'),
      crs = grid_crs,
      remove = FALSE
    )

    xy <- sf::st_coordinates(sf::st_transform(sf_candidates, epsg))
    ord <- order(candidates$distance_m)
    keep <- integer(0)

    for (idx in ord) {
      if (length(keep) == 0L) {
        keep <- idx
      } else {
        d <- sqrt(
          (xy[keep, 1] - xy[idx, 1])^2 +
          (xy[keep, 2] - xy[idx, 2])^2
        )
        if (all(d >= min_separation_m)) {
          keep <- c(keep, idx)
        }
      }

      if (length(keep) >= max_seeds) {
        break
      }
    }

    candidates[keep, , drop = FALSE]
  }


  ambiguity_near_confluence <- function(
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

    primary_distance <- if (
      !is.null(primary) && is.finite(primary$snap_distance_m)
    ) primary$snap_distance_m else 0

    merge_limit_m <- min(
      AMBIGUITY_JUNCTION_MAX_M,
      max(120, primary_distance + AMBIGUITY_JUNCTION_MARGIN_M)
    )

    seed_radius_m <- min(600, merge_limit_m + 180)

    candidates <- snap_collect_stream_candidates(
      lon = lon,
      lat = lat,
      radius_m = seed_radius_m,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes
    )

    if (nrow(candidates) < 2L) {
      return(NULL)
    }

    seeds <- ambiguity_thin_candidates(
      candidates,
      grid_template = grid_template,
      lon = lon,
      lat = lat,
      min_separation_m = 75,
      max_seeds = 28L
    )

    if (nrow(seeds) < 2L) {
      return(NULL)
    }

    paths <- lapply(
      seeds$cell,
      function(cell) ambiguity_downstream_path(
        start_cell = cell,
        reverse_cache = reverse_cache,
        grid_template = grid_template,
        click_lon = lon,
        click_lat = lat,
        max_distance_m = 1600,
        max_steps = 128L
      )
    )

    best <- NULL
    best_score <- Inf

    for (ii in seq_len(length(paths) - 1L)) {
      for (jj in seq.int(ii + 1L, length(paths))) {

        merge <- snap_ambiguity_first_merge(
          paths[[ii]]$cells,
          paths[[jj]]$cells
        )

        if (
          is.null(merge) ||
          isTRUE(merge$same_lineage) ||
          merge$index_a <= 1L ||
          merge$index_b <= 1L
        ) {
          next
        }

        merge_distance <- paths[[ii]]$click_distance_m[merge$index_a]

        if (
          !is.finite(merge_distance) ||
          merge_distance > merge_limit_m
        ) {
          next
        }

        score <- merge_distance +
          0.10 * (seeds$distance_m[ii] + seeds$distance_m[jj])

        if (score < best_score) {
          best_score <- score
          best <- list(
            ii = ii,
            jj = jj,
            merge = merge,
            merge_distance_m = merge_distance
          )
        }
      }
    }

    if (is.null(best)) {
      return(NULL)
    }

    path_a <- paths[[best$ii]]
    path_b <- paths[[best$jj]]

    branch_a <- path_a$cells[best$merge$index_a - 1L]
    branch_b <- path_b$cells[best$merge$index_b - 1L]
    downstream <- snap_downstream_cell(reverse_cache, best$merge$cell)

    option_cells <- c(branch_a, branch_b)
    roles <- c('Rama aguas arriba 1', 'Rama aguas arriba 2')
    modes <- c(
      'AMBIGUOUS_CONFLUENCE_UPSTREAM',
      'AMBIGUOUS_CONFLUENCE_UPSTREAM'
    )

    if (is.finite(downstream)) {
      option_cells <- c(option_cells, downstream)
      roles <- c(roles, 'Aguas abajo de la confluencia')
      modes <- c(modes, 'AMBIGUOUS_CONFLUENCE_DOWNSTREAM')
    }

    options <- lapply(
      seq_along(option_cells),
      function(ii) ambiguity_snap_from_cell(
        lon = lon,
        lat = lat,
        outlet_cell = option_cells[ii],
        role = roles[ii],
        mode = modes[ii],
        reverse_cache = reverse_cache,
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes,
        threshold_cells = threshold_cells
      )
    )

    options <- Filter(Negate(is.null), options)

    if (length(options) < 2L) {
      return(NULL)
    }

    cells <- vapply(options, function(x) as.double(x$outlet_cell), numeric(1))
    options <- options[!duplicated(cells)]

    if (length(options) < 2L) {
      return(NULL)
    }

    list(
      status = 'ambiguous',
      reason = 'CONFLUENCE_D8_LOCAL',
      merge_distance_m = best$merge_distance_m,
      options = head(options, AMBIGUITY_MAX_OPTIONS)
    )
  }


  ambiguity_wide_multichannel <- function(
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

    seeds <- ambiguity_thin_candidates(
      candidates,
      grid_template = grid_template,
      lon = lon,
      lat = lat,
      min_separation_m = 120,
      max_seeds = 40L
    )

    primary_path <- ambiguity_downstream_path(
      start_cell = primary$outlet_cell,
      reverse_cache = reverse_cache,
      grid_template = grid_template,
      click_lon = lon,
      click_lat = lat,
      max_distance_m = AMBIGUITY_WIDE_PATH_M,
      max_steps = 256L
    )

    matches <- list()

    for (ii in seq_len(nrow(seeds))) {

      seed_cell <- as.double(seeds$cell[ii])
      if (seed_cell == as.double(primary$outlet_cell)) {
        next
      }

      path_now <- ambiguity_downstream_path(
        start_cell = seed_cell,
        reverse_cache = reverse_cache,
        grid_template = grid_template,
        click_lon = lon,
        click_lat = lat,
        max_distance_m = AMBIGUITY_WIDE_PATH_M,
        max_steps = 256L
      )

      merge <- snap_ambiguity_first_merge(
        primary_path$cells,
        path_now$cells
      )

      if (
        is.null(merge) ||
        isTRUE(merge$same_lineage) ||
        merge$index_a <= 1L ||
        merge$index_b <= 1L
      ) {
        next
      }

      merge_distance <- primary_path$click_distance_m[merge$index_a]
      if (!is.finite(merge_distance) || merge_distance > AMBIGUITY_WIDE_PATH_M) {
        next
      }

      parallel <- ambiguity_direction_similarity(primary_path, path_now)
      if (is.finite(parallel) && parallel < AMBIGUITY_WIDE_MIN_PARALLEL) {
        next
      }

      score <- seeds$distance_m[ii] +
        0.20 * merge_distance +
        if (is.finite(parallel)) 300 * (1 - parallel) else 150

      matches[[length(matches) + 1L]] <- list(
        seed_index = ii,
        path = path_now,
        merge = merge,
        parallel = parallel,
        merge_distance_m = merge_distance,
        score = score
      )
    }

    if (length(matches) == 0L) {
      return(NULL)
    }

    ord <- order(vapply(matches, function(x) x$score, numeric(1)))
    best <- matches[[ord[1]]]

    alternative_cell <- best$path$cells[best$merge$index_b - 1L]
    downstream_cell <- snap_downstream_cell(reverse_cache, best$merge$cell)

    primary$ambiguity_role <- 'Cauce inicialmente seleccionado'
    options <- list(primary)

    alt <- ambiguity_snap_from_cell(
      lon = lon,
      lat = lat,
      outlet_cell = alternative_cell,
      role = 'Rama/cauce alternativo',
      mode = 'AMBIGUOUS_WIDE_ALTERNATIVE',
      reverse_cache = reverse_cache,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes,
      threshold_cells = threshold_cells
    )

    if (!is.null(alt)) {
      options[[length(options) + 1L]] <- alt
    }

    if (is.finite(downstream_cell)) {
      combined <- ambiguity_snap_from_cell(
        lon = lon,
        lat = lat,
        outlet_cell = downstream_cell,
        role = 'Cauce combinado aguas abajo',
        mode = 'AMBIGUOUS_WIDE_COMBINED',
        reverse_cache = reverse_cache,
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes,
        threshold_cells = threshold_cells
      )
      if (!is.null(combined)) {
        options[[length(options) + 1L]] <- combined
      }
    }

    cells <- vapply(options, function(x) as.double(x$outlet_cell), numeric(1))
    options <- options[!duplicated(cells)]

    if (length(options) < 2L) {
      return(NULL)
    }

    list(
      status = 'ambiguous',
      reason = 'WIDE_MULTICHANNEL_D8',
      merge_distance_m = best$merge_distance_m,
      direction_similarity = best$parallel,
      options = head(options, AMBIGUITY_MAX_OPTIONS)
    )
  }


  ambiguity_from_near_tie <- function(
      lon,
      lat,
      radius_m,
      grid_template,
      stream_cache,
      stripe_rows,
      n_stripes,
      reverse_cache,
      threshold_cells,
      original_error
  ) {

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

    if (!identical(choice$status, 'ambiguous')) {
      stop(original_error)
    }

    idxs <- c(choice$winner_index, choice$alternative_index)
    options <- lapply(
      seq_along(idxs),
      function(ii) ambiguity_snap_from_cell(
        lon = lon,
        lat = lat,
        outlet_cell = candidates$cell[idxs[ii]],
        role = paste0('Cauce candidato ', ii),
        mode = 'AMBIGUOUS_NEAR_TIE',
        reverse_cache = reverse_cache,
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes,
        threshold_cells = threshold_cells
      )
    )

    options <- Filter(Negate(is.null), options)
    if (length(options) < 2L) {
      stop(original_error)
    }

    list(
      status = 'ambiguous',
      reason = 'NEAR_TIE',
      options = options
    )
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

    block_id <- as.character(stream_cache$block_id)
    meta <- get_block_metadata(block_id)
    threshold_cells <- as.double(meta[['STREAM_THRESHOLD_CELLS']][1])
    reverse_cache <- new_snap_reverse_cache(block_id)

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

      return(
        ambiguity_from_near_tie(
          lon = lon,
          lat = lat,
          radius_m = radius_m,
          grid_template = grid_template,
          stream_cache = stream_cache,
          stripe_rows = stripe_rows,
          n_stripes = n_stripes,
          reverse_cache = reverse_cache,
          threshold_cells = threshold_cells,
          original_error = primary
        )
      )
    }

    # Solo despues de resolver el snap principal se busca una
    # confluencia ligada a SU entorno. Esto elimina falsos positivos
    # provocados por cruces cercanos pero ajenos al clic.
    confluence <- ambiguity_near_confluence(
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

    if (!is.null(confluence)) {
      return(confluence)
    }

    # Rios anchos y brazos trenzados: dos brazos pueden formar una
    # sola componente espacial. Se comparan caminos D8 y se ofrece
    # el brazo alternativo mas compatible, mas el cauce combinado
    # aguas abajo cuando existe.
    wide <- ambiguity_wide_multichannel(
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
'''

delim_text = delim_text[:start] + new_block + delim_text[end:]
DELIM.write_text(delim_text, encoding='utf-8')

# ------------------------------------------------------------
# 3. Regresiones puras para semantica de ramas D8
# ------------------------------------------------------------
smoke_text = SMOKE.read_text(encoding='utf-8')
smoke_marker = '# Regresion: semantica topologica de ambiguedad D8'
if smoke_marker not in smoke_text:
    insert_at = smoke_text.rfind('\n\ncat(')
    if insert_at < 0:
        raise SystemExit('No se encontro cat() final en smoke_app.R')

    smoke_code = r'''


# ============================================================
# Regresion: semantica topologica de ambiguedad D8
# ============================================================
# Dos brazos que convergen deben ser ramas distintas. Dos puntos
# sobre la misma trayectoria no deben crear dos opciones.

merge_distinct <- app_env$snap_ambiguity_first_merge(
  c(101, 102, 103, 104, 105),
  c(201, 202, 203, 104, 105)
)

if (
  is.null(merge_distinct) ||
  merge_distinct$cell != 104 ||
  merge_distinct$index_a != 4L ||
  merge_distinct$index_b != 4L ||
  isTRUE(merge_distinct$same_lineage)
) {
  stop('Regresion ambiguedad: dos brazos convergentes no fueron reconocidos como distintos.')
}

merge_same <- app_env$snap_ambiguity_first_merge(
  c(101, 102, 103, 104, 105),
  c(103, 104, 105)
)

if (
  is.null(merge_same) ||
  !isTRUE(merge_same$same_lineage) ||
  !isTRUE(app_env$snap_ambiguity_same_lineage(
    c(101, 102, 103, 104, 105),
    c(103, 104, 105)
  ))
) {
  stop('Regresion ambiguedad: dos puntos del mismo cauce fueron tratados como ramas distintas.')
}

merge_none <- app_env$snap_ambiguity_first_merge(
  c(1, 2, 3),
  c(10, 11, 12)
)

if (!is.null(merge_none)) {
  stop('Regresion ambiguedad: caminos D8 independientes inventaron una confluencia.')
}
'''
    smoke_text = smoke_text[:insert_at] + smoke_code + smoke_text[insert_at:]
    SMOKE.write_text(smoke_text, encoding='utf-8')

print('Refinamiento topologico aplicado.')

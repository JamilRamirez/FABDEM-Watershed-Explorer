from pathlib import Path

p = Path('R/delimitacion.R')
text = p.read_text(encoding='utf-8')

start = text.find('  ambiguity_near_confluence <- function(')
end = text.find('\n\n\n  ambiguity_wide_multichannel <- function(', start)
if start < 0 or end < 0:
    raise SystemExit('No se encontro ambiguity_near_confluence')

new_fun = r'''  ambiguity_near_confluence <- function(
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

    primary_distance <- ambiguity_snap_distance_m(
      primary,
      default = 0
    )

    # Una confluencia solo puede crear opciones si involucra la
    # trayectoria D8 del outlet que el snap estable selecciono.
    # Esto evita falsos positivos por cruces/tributarios cercanos
    # que no pertenecen a la interpretacion inmediata del clic.
    primary_path <- ambiguity_downstream_path(
      start_cell = primary$outlet_cell,
      reverse_cache = reverse_cache,
      grid_template = grid_template,
      click_lon = lon,
      click_lat = lat,
      max_distance_m = 1600,
      max_steps = 128L
    )

    merge_limit_m <- min(
      AMBIGUITY_JUNCTION_MAX_M,
      max(90, primary_distance + AMBIGUITY_JUNCTION_MARGIN_M)
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

    best <- NULL
    best_score <- Inf

    for (ii in seq_len(nrow(seeds))) {

      seed_cell <- as.double(seeds$cell[ii])

      # El propio outlet y otros puntos de su misma trayectoria no
      # constituyen una segunda opcion.
      if (seed_cell == as.double(primary$outlet_cell)) {
        next
      }

      path_now <- ambiguity_downstream_path(
        start_cell = seed_cell,
        reverse_cache = reverse_cache,
        grid_template = grid_template,
        click_lon = lon,
        click_lat = lat,
        max_distance_m = 1600,
        max_steps = 128L
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
      candidate_distance <- as.numeric(seeds$distance_m[ii])

      if (
        !is.finite(merge_distance) ||
        merge_distance > merge_limit_m ||
        !is.finite(candidate_distance)
      ) {
        next
      }

      # Si el clic esta claramente sobre el cauce primario, un
      # tributario mas lejano que desemboca cerca NO vuelve ambiguo
      # el clic. La tolerancia crece con la incertidumbre del snap.
      closeness_tolerance_m <- max(
        45,
        0.35 * max(primary_distance, candidate_distance)
      )

      if (
        primary_distance < AMBIGUITY_WIDE_TRIGGER_M &&
        abs(candidate_distance - primary_distance) > closeness_tolerance_m
      ) {
        next
      }

      score <- merge_distance +
        0.20 * candidate_distance +
        0.10 * abs(candidate_distance - primary_distance)

      if (score < best_score) {
        best_score <- score
        best <- list(
          path = path_now,
          merge = merge,
          candidate_distance_m = candidate_distance,
          merge_distance_m = merge_distance
        )
      }
    }

    if (is.null(best)) {
      return(NULL)
    }

    # Opcion 1: rama donde realmente cayo el snap principal,
    # inmediatamente antes de la union.
    branch_primary <- primary_path$cells[best$merge$index_a - 1L]
    # Opcion 2: la otra rama, inmediatamente antes de la union.
    branch_alternate <- best$path$cells[best$merge$index_b - 1L]
    # Opcion 3: cauce combinado inmediatamente aguas abajo.
    downstream <- snap_downstream_cell(reverse_cache, best$merge$cell)

    option_cells <- c(branch_primary, branch_alternate)
    roles <- c('Rama seleccionada por el clic', 'Rama alternativa')
    modes <- c(
      'AMBIGUOUS_CONFLUENCE_PRIMARY',
      'AMBIGUOUS_CONFLUENCE_ALTERNATIVE'
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
      reason = 'CONFLUENCE_D8_PRIMARY_PATH',
      merge_distance_m = best$merge_distance_m,
      primary_distance_m = primary_distance,
      alternative_distance_m = best$candidate_distance_m,
      options = head(options, AMBIGUITY_MAX_OPTIONS)
    )
  }
'''
text = text[:start] + new_fun + text[end:]

# Para snaps lejanos se prueba primero el caso de rio ancho/multicanal.
old_order = r'''    # Solo despues de resolver el snap principal se busca una
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
'''

new_order = r'''    # Si el snap es lejano, primero se resuelve el caso de rio
    # ancho/multicanal. Asi una union local secundaria no intercepta
    # el diagnostico del tronco principal (caso Napo/Mazan).
    primary_distance <- ambiguity_snap_distance_m(
      primary,
      default = 0
    )

    if (primary_distance >= AMBIGUITY_WIDE_TRIGGER_M) {
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
    }

    # Para clicks bien alineados con la red, una confluencia solo es
    # ambigua si la otra rama esta tambien suficientemente cerca del
    # clic y converge con la trayectoria D8 del outlet principal.
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
'''

if old_order not in text:
    raise SystemExit('No se encontro orden actual de detectores')
text = text.replace(old_order, new_order, 1)
p.write_text(text, encoding='utf-8')

# QA: no abortar al primer caso incorrecto; reportar toda la bateria.
q = Path('tools/ci/qa_ambiguity_runtime.R')
qa = q.read_text(encoding='utf-8')
qa = qa.replace("results <- list()\n", "results <- list()\nfailures <- character(0)\n", 1)
old_assert = r'''  if (!identical(observed$status, case$expected)) {
    stop(
      case$name, ': esperado ', case$expected,
      ' pero se obtuvo ', observed$status,
      ' (', observed$reason, ')'
    )
  }
'''
new_assert = r'''  if (!identical(observed$status, case$expected)) {
    failures <- c(
      failures,
      paste0(
        case$name, ': esperado ', case$expected,
        ' pero se obtuvo ', observed$status,
        ' (', observed$reason, ')'
      )
    )
  }
'''
if old_assert in qa:
    qa = qa.replace(old_assert, new_assert, 1)

old_final = "cat('\\nREAL RUNTIME AMBIGUITY QA: PASS\\n')\n"
new_final = r'''if (length(failures) > 0L) {
  cat('\nREAL RUNTIME AMBIGUITY QA: FAIL\n')
  cat(paste0(' - ', failures, collapse = '\n'), '\n')
  stop('Fallaron ', length(failures), ' casos de QA Runtime.')
}

cat('\nREAL RUNTIME AMBIGUITY QA: PASS\n')
'''
if old_final not in qa:
    raise SystemExit('No se encontro cierre de QA Runtime')
qa = qa.replace(old_final, new_final, 1)
q.write_text(qa, encoding='utf-8')

print('Primary-path ambiguity refinement applied')

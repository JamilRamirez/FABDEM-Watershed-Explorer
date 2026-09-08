options(shiny.launch.browser = FALSE, warn = 1)

required <- c('shiny','leaflet','sf','terra','DT','readxl')
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop('Faltan paquetes: ', paste(missing, collapse=', '))

app_env <- new.env(parent = globalenv())
source('app.R', local = app_env, echo = FALSE, print.eval = FALSE, encoding = 'UTF-8')

module_env <- environment(app_env$delimitacion$server)
if (is.null(module_env) || !exists('find_ambiguity_options', envir = module_env, inherits = TRUE)) {
  stop('No se pudo acceder al detector interno find_ambiguity_options del modulo delimitacion.')
}
ambiguity_fun <- get('find_ambiguity_options', envir = module_env, inherits = TRUE)

cases <- data.frame(
  name = c('Socsi', 'Huaycoloro', 'Chira_Sullana', 'Napo_Mazan_DHN'),
  lon = c(-76.1945000, -76.9520300, -80.6911111, -73.0916944),
  lat = c(-13.0283000, -12.0191500, -5.8913889, -3.4965278),
  expected = c('single', 'single', 'single', 'ambiguous'),
  stringsAsFactors = FALSE
)

results <- list()
failures <- character(0)

for (kk in seq_len(nrow(cases))) {
  case <- cases[kk, ]
  observed <- NULL

  shiny::testServer(app_env$delimitacion$server, {
    block_row <- app_env$find_block_for_click(case$lon, case$lat)
    if (is.null(block_row) || nrow(block_row) < 1L) {
      stop(case$name, ': punto fuera del dominio Runtime')
    }

    block_id <- as.character(block_row[['BLOCK_ID']][1])
    app_env$load_block_if_needed(block_id = block_id, block_cache = block_cache)

    detected <- ambiguity_fun(
      lon = case$lon,
      lat = case$lat,
      radius_m = app_env$DEFAULT_SNAP_RADIUS_M,
      grid_template = block_cache$grid_template,
      stream_cache = block_cache$stream_cache,
      stripe_rows = block_cache$stripe_rows,
      n_stripes = block_cache$n_stripes
    )

    roles <- vapply(
      detected$options,
      function(x) if (!is.null(x$ambiguity_role)) x$ambiguity_role else '',
      character(1)
    )
    distances <- vapply(
      detected$options,
      function(x) as.numeric(x$snap_distance_m),
      numeric(1)
    )
    modes <- vapply(
      detected$options,
      function(x) as.character(x$snap_mode),
      character(1)
    )

    large_area_est <- NA_real_

    if (identical(case$name, 'Napo_Mazan_DHN')) {
      if (!identical(detected$reason, 'WIDE_MULTICHANNEL_D8')) {
        stop(
          'Napo_Mazan_DHN: se esperaba ambiguedad multicanal D8 y se obtuvo ',
          detected$reason,
          '.'
        )
      }

      if (length(detected$options) != 3L) {
        stop(
          'Napo_Mazan_DHN: se esperaban tres opciones hidrologicas y se obtuvieron ',
          length(detected$options),
          '.'
        )
      }

      combined_idx <- grep(
        'combinado|aguas abajo',
        roles,
        ignore.case = TRUE
      )

      if (length(combined_idx) == 0L) {
        stop('Napo_Mazan_DHN: el selector no produjo una opcion combinada aguas abajo.')
      }

      if (!any(grepl('alternativ', roles, ignore.case = TRUE))) {
        stop('Napo_Mazan_DHN: el selector no produjo una rama/cauce alternativo.')
      }

      chosen_idx <- combined_idx[1]
      tr <- app_env$trace_upstream(
        cache = block_cache$reverse_cache,
        outlet_cell = detected$options[[chosen_idx]]$outlet_cell
      )

      meta <- app_env$get_block_metadata(block_id)
      km2_per_threshold_cell <- as.numeric(meta[['STREAM_THRESHOLD_KM2']][1]) /
        as.numeric(meta[['STREAM_THRESHOLD_CELLS']][1])
      large_area_est <- as.numeric(tr$n_cells) * km2_per_threshold_cell

      if (!is.finite(large_area_est) || large_area_est < 80000) {
        stop(
          'Napo_Mazan_DHN: la opcion combinada sigue siendo demasiado pequena: ',
          sprintf('%.1f km2', large_area_est)
        )
      }
    }

    observed <<- list(
      name = case$name,
      status = detected$status,
      reason = detected$reason,
      n_options = length(detected$options),
      roles = roles,
      distances = distances,
      modes = modes,
      combined_area_est_km2 = large_area_est
    )
  })

  if (!identical(observed$status, case$expected)) {
    failures <- c(
      failures,
      paste0(
        case$name, ': esperado ', case$expected,
        ' pero se obtuvo ', observed$status,
        ' (', observed$reason, ')'
      )
    )
  }

  if (identical(case$expected, 'single') && observed$n_options != 1L) {
    failures <- c(
      failures,
      paste0(case$name, ': un caso single debe producir exactamente una opcion.')
    )
  }

  if (
    identical(case$expected, 'ambiguous') &&
    (observed$n_options < 2L || observed$n_options > 3L)
  ) {
    failures <- c(
      failures,
      paste0(case$name, ': un caso ambiguo debe producir entre dos y tres opciones.')
    )
  }

  results[[length(results) + 1L]] <- observed

  cat('\nCASE ', observed$name, '\n', sep='')
  cat('status=', observed$status, ' reason=', observed$reason,
      ' options=', observed$n_options, '\n', sep='')
  if (length(observed$roles)) {
    for (ii in seq_along(observed$roles)) {
      cat('  ', ii, ': ', observed$roles[ii],
          ' | ', observed$modes[ii],
          ' | ', sprintf('%.1f m', observed$distances[ii]), '\n', sep='')
    }
  }
  if (is.finite(observed$combined_area_est_km2)) {
    cat('combined_area_est_km2=', sprintf('%.1f', observed$combined_area_est_km2), '\n', sep='')
  }
}

if (length(failures) > 0L) {
  cat('\nREAL RUNTIME AMBIGUITY QA: FAIL\n')
  cat(paste0(' - ', failures, collapse = '\n'), '\n')
  stop('Fallaron ', length(failures), ' casos de QA Runtime.')
}

cat('\nREAL RUNTIME AMBIGUITY QA: PASS\n')

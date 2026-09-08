from pathlib import Path

PATH = Path('R/delimitacion.R')
text = PATH.read_text(encoding='utf-8')


def replace_once(src, old, new, label):
    n = src.count(old)
    if n != 1:
        raise SystemExit(f'{label}: expected 1 anchor, found {n}')
    return src.replace(old, new, 1)


AMBIGUITY_LOGIC = r'''

  # ==========================================================
  # RESOLUCION EXPLICITA DE AMBIGUEDAD HIDROLOGICA
  # ==========================================================

  AMBIGUITY_JUNCTION_RADIUS_M <- 450
  AMBIGUITY_WIDE_TRIGGER_M <- 350
  AMBIGUITY_WIDE_SCAN_M <- 3000
  AMBIGUITY_MAX_OPTIONS <- 3L


  ambiguity_reverse_parent_cells <- function(
      reverse_cache,
      cell
  ) {

    nc <- as.double(reverse_cache$metadata$ncols)
    nr <- as.double(reverse_cache$metadata$nrows)
    cell <- as.double(cell)

    row <- floor((cell - 1) / nc) + 1
    col <- ((cell - 1) %% nc) + 1

    value <- get_reverse_values(
      reverse_cache,
      cell
    )

    if (
      length(value) != 1L ||
      !is.finite(value)
    ) {
      return(numeric(0))
    }

    cells <- c(
      cell - nc - 1,
      cell - nc,
      cell - nc + 1,
      cell - 1,
      cell + 1,
      cell + nc - 1,
      cell + nc,
      cell + nc + 1
    )

    bits <- c(
      1L, 2L, 4L, 8L,
      16L, 32L, 64L, 128L
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

    keep <- valid &
      bitwAnd(
        as.integer(value),
        bits
      ) != 0L

    as.double(cells[keep])
  }


  ambiguity_stream_parent_cells <- function(
      reverse_cache,
      cell,
      grid_template,
      stream_cache,
      stripe_rows,
      n_stripes,
      threshold_cells
  ) {

    parents <- ambiguity_reverse_parent_cells(
      reverse_cache,
      cell
    )

    if (length(parents) == 0L) {
      return(numeric(0))
    }

    keep <- vapply(
      parents,
      function(parent_cell) {

        stream_value <- snap_stream_value_at_cell(
          cell = parent_cell,
          grid_template = grid_template,
          stream_cache = stream_cache,
          stripe_rows = stripe_rows,
          n_stripes = n_stripes
        )

        isTRUE(
          is.finite(stream_value) &&
          stream_value > 0 &&
          snap_upstream_reaches_threshold(
            reverse_cache = reverse_cache,
            outlet_cell = parent_cell,
            threshold_cells = threshold_cells
          )
        )
      },
      logical(1)
    )

    as.double(parents[keep])
  }


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
      grid_crs = sf::st_crs(
        terra::crs(grid_template)
      ),
      stream_value = stream_value,
      mode = mode
    )

    out$ambiguity_role <- role
    out
  }


  ambiguity_near_confluence <- function(
      lon,
      lat,
      grid_template,
      stream_cache,
      stripe_rows,
      n_stripes,
      reverse_cache,
      threshold_cells
  ) {

    candidates <- snap_collect_stream_candidates(
      lon = lon,
      lat = lat,
      radius_m = AMBIGUITY_JUNCTION_RADIUS_M,
      grid_template = grid_template,
      stream_cache = stream_cache,
      stripe_rows = stripe_rows,
      n_stripes = n_stripes
    )

    if (nrow(candidates) == 0L) {
      return(NULL)
    }

    probe <- seq_len(
      min(
        nrow(candidates),
        500L
      )
    )

    for (idx in probe) {

      junction_cell <- as.double(
        candidates$cell[idx]
      )

      parents <- ambiguity_stream_parent_cells(
        reverse_cache = reverse_cache,
        cell = junction_cell,
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes,
        threshold_cells = threshold_cells
      )

      if (length(parents) < 2L) {
        next
      }

      options <- list()

      for (ii in seq_len(
        min(
          2L,
          length(parents)
        )
      )) {

        option <- ambiguity_snap_from_cell(
          lon = lon,
          lat = lat,
          outlet_cell = parents[ii],
          role = paste0(
            'Rama aguas arriba ',
            ii
          ),
          mode = 'AMBIGUOUS_CONFLUENCE_UPSTREAM',
          reverse_cache = reverse_cache,
          grid_template = grid_template,
          stream_cache = stream_cache,
          stripe_rows = stripe_rows,
          n_stripes = n_stripes,
          threshold_cells = threshold_cells
        )

        if (!is.null(option)) {
          options[[length(options) + 1L]] <- option
        }
      }

      downstream <- snap_downstream_cell(
        reverse_cache = reverse_cache,
        cell = junction_cell
      )

      if (is.finite(downstream)) {

        option <- ambiguity_snap_from_cell(
          lon = lon,
          lat = lat,
          outlet_cell = downstream,
          role = 'Aguas abajo de la confluencia',
          mode = 'AMBIGUOUS_CONFLUENCE_DOWNSTREAM',
          reverse_cache = reverse_cache,
          grid_template = grid_template,
          stream_cache = stream_cache,
          stripe_rows = stripe_rows,
          n_stripes = n_stripes,
          threshold_cells = threshold_cells
        )

        if (!is.null(option)) {
          options[[length(options) + 1L]] <- option
        }
      }

      if (length(options) >= 2L) {

        cells <- vapply(
          options,
          function(x) as.double(x$outlet_cell),
          numeric(1)
        )

        options <- options[
          !duplicated(cells)
        ]

        if (length(options) >= 2L) {
          return(
            list(
              status = 'ambiguous',
              reason = 'CONFLUENCE',
              options = head(
                options,
                AMBIGUITY_MAX_OPTIONS
              )
            )
          )
        }
      }
    }

    NULL
  }


  ambiguity_wide_components <- function(
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

    component_id <- snap_stream_component_ids(
      cells = candidates$cell,
      grid_ncols = terra::ncol(grid_template)
    )

    components <- unique(component_id)

    if (length(components) < 2L) {
      return(NULL)
    }

    best <- vapply(
      components,
      function(component_now) {
        idx <- which(
          component_id == component_now
        )
        idx[
          which.min(
            candidates$distance_m[idx]
          )
        ]
      },
      integer(1)
    )

    best <- best[
      order(
        candidates$distance_m[best]
      )
    ]

    primary$ambiguity_role <- 'Cauce inicialmente seleccionado'
    options <- list(primary)

    for (idx in best) {

      cell_now <- as.double(
        candidates$cell[idx]
      )

      if (
        cell_now ==
          as.double(primary$outlet_cell)
      ) {
        next
      }

      option <- ambiguity_snap_from_cell(
        lon = lon,
        lat = lat,
        outlet_cell = cell_now,
        role = 'Cauce alternativo cercano',
        mode = 'AMBIGUOUS_WIDE_MULTICHANNEL',
        reverse_cache = reverse_cache,
        grid_template = grid_template,
        stream_cache = stream_cache,
        stripe_rows = stripe_rows,
        n_stripes = n_stripes,
        threshold_cells = threshold_cells
      )

      if (is.null(option)) {
        next
      }

      options[[length(options) + 1L]] <- option

      if (length(options) >= AMBIGUITY_MAX_OPTIONS) {
        break
      }
    }

    if (length(options) >= 2L) {
      return(
        list(
          status = 'ambiguous',
          reason = 'WIDE_OR_MULTICHANNEL',
          options = options
        )
      )
    }

    NULL
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

    block_id <- as.character(
      stream_cache$block_id
    )

    meta <- get_block_metadata(
      block_id
    )

    threshold_cells <- as.double(
      meta[['STREAM_THRESHOLD_CELLS']][1]
    )

    reverse_cache <- new_snap_reverse_cache(
      block_id
    )

    confluence <- ambiguity_near_confluence(
      lon = lon,
      lat = lat,
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

      radii_m <- snap_progressive_radii(
        radius_m
      )

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

      if (!identical(
        choice$status,
        'ambiguous'
      )) {
        stop(primary)
      }

      idxs <- c(
        choice$winner_index,
        choice$alternative_index
      )

      options <- lapply(
        seq_along(idxs),
        function(ii) {
          ambiguity_snap_from_cell(
            lon = lon,
            lat = lat,
            outlet_cell = candidates$cell[idxs[ii]],
            role = paste0(
              'Cauce candidato ',
              ii
            ),
            mode = 'AMBIGUOUS_NEAR_TIE',
            reverse_cache = reverse_cache,
            grid_template = grid_template,
            stream_cache = stream_cache,
            stripe_rows = stripe_rows,
            n_stripes = n_stripes,
            threshold_cells = threshold_cells
          )
        }
      )

      options <- Filter(
        Negate(is.null),
        options
      )

      if (length(options) >= 2L) {
        return(
          list(
            status = 'ambiguous',
            reason = 'NEAR_TIE',
            options = options
          )
        )
      }

      stop(primary)
    }

    wide <- ambiguity_wide_components(
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

SERVER_HELPERS = r'''
        ambiguity_previews <- shiny::reactiveVal(NULL)
        ambiguity_context <- shiny::reactiveVal(NULL)

        ambiguity_colors <- c(
          '#1565C0',
          '#2E7D32',
          '#6A1B9A'
        )


        clear_ambiguity_preview <- function(
            clear_map = TRUE,
            remove_files = TRUE
        ) {

          context <- ambiguity_context()
          ambiguity_previews(NULL)
          ambiguity_context(NULL)

          if (
            isTRUE(remove_files) &&
            !is.null(context) &&
            !is.null(context$work_root) &&
            dir.exists(context$work_root)
          ) {
            unlink(
              context$work_root,
              recursive = TRUE,
              force = TRUE
            )
          }

          if (isTRUE(clear_map)) {
            leaflet::leafletProxy(
              'mapa',
              session = session
            ) |>
              leaflet::clearGroup('Cuencas candidatas') |>
              leaflet::clearGroup('Outlets candidatos')
          }

          invisible(NULL)
        }


        ambiguity_area_km2 <- function(basin_sf) {
          sum(
            as.double(
              sf::st_area(
                sf::st_transform(
                  basin_sf,
                  6933
                )
              )
            ),
            na.rm = TRUE
          ) / 1e6
        }


        materialize_ambiguity_option <- function(
            option,
            candidate_index,
            grid_template,
            work_root
        ) {

          candidate_dir <- file.path(
            work_root,
            paste0('candidate_', candidate_index)
          )

          dir.create(
            candidate_dir,
            recursive = TRUE,
            showWarnings = FALSE
          )

          temp_dir <- file.path(
            candidate_dir,
            'terra_tmp'
          )

          dir.create(
            temp_dir,
            recursive = TRUE,
            showWarnings = FALSE
          )

          trace <- trace_upstream(
            cache = block_cache$reverse_cache,
            outlet_cell = option$outlet_cell
          )

          if (
            trace$n_cells <
              block_cache$stream_threshold_cells
          ) {
            stop(
              'La alternativa no alcanza el umbral hidrologico minimo.'
            )
          }

          trace_cells <- as.double(
            trace$n_cells
          )

          trace_engine <- if (
            !is.null(trace$trace_engine)
          ) {
            as.character(trace$trace_engine)
          } else {
            'UNKNOWN'
          }

          basin_tif <- file.path(
            candidate_dir,
            'basin.tif'
          )

          write_basin_raster(
            cells = trace$cells,
            template = grid_template,
            output_file = basin_tif,
            temp_dir = temp_dir,
            bbox = trace$bbox
          )

          rm(trace)
          gc()

          basin_gpkg <- file.path(
            candidate_dir,
            'basin.gpkg'
          )

          basin_sf <- polygonize_basin(
            basin_tif,
            basin_gpkg,
            expected_cells = trace_cells
          )

          area_km2 <- ambiguity_area_km2(
            basin_sf
          )

          outlet_sf <- sf::st_sf(
            OPTION = candidate_index,
            ROLE = if (!is.null(option$ambiguity_role)) {
              option$ambiguity_role
            } else {
              paste0('Opcion ', candidate_index)
            },
            SNAP_MODE = option$snap_mode,
            SNAP_DISTANCE_M = option$snap_distance_m,
            geometry = sf::st_sfc(
              sf::st_point(
                c(
                  option$outlet_lon,
                  option$outlet_lat
                )
              ),
              crs = 4326
            )
          )

          outlet_file <- file.path(
            candidate_dir,
            'outlet.gpkg'
          )

          sf::st_write(
            outlet_sf,
            outlet_file,
            layer = 'outlet',
            quiet = TRUE,
            delete_dsn = TRUE
          )

          basin_map <- basin_sf |>
            sf::st_transform(3857) |>
            sf::st_simplify(
              dTolerance = MAP_SIMPLIFY_M,
              preserveTopology = TRUE
            ) |>
            sf::st_transform(4326)

          list(
            index = candidate_index,
            snap = option,
            basin_sf = basin_sf,
            basin_map = basin_map,
            basin_tif = basin_tif,
            basin_gpkg = basin_gpkg,
            outlet_sf = outlet_sf,
            outlet_file = outlet_file,
            area_km2 = area_km2,
            trace_cells = trace_cells,
            trace_engine = trace_engine
          )
        }


        render_ambiguity_map <- function(previews) {

          proxy <- leaflet::leafletProxy(
            'mapa',
            session = session
          ) |>
            leaflet::clearGroup('Punto') |>
            leaflet::clearGroup('Cuenca delimitada') |>
            leaflet::clearGroup('Cuencas candidatas') |>
            leaflet::clearGroup('Outlets candidatos')

          bboxes <- list()

          for (ii in seq_along(previews)) {

            preview <- previews[[ii]]
            color_now <- ambiguity_colors[ii]

            label_now <- paste0(
              'Opcion ',
              ii,
              ' · ',
              format(
                round(preview$area_km2, 1),
                big.mark = ',',
                scientific = FALSE,
                trim = TRUE
              ),
              ' km²'
            )

            proxy <- proxy |>
              leaflet::addPolygons(
                data = preview$basin_map,
                group = 'Cuencas candidatas',
                layerId = paste0('AMBIG_BASIN_', ii),
                color = color_now,
                weight = 3,
                opacity = 0.95,
                fillColor = color_now,
                fillOpacity = 0.12,
                label = label_now
              ) |>
              leaflet::addCircleMarkers(
                lng = preview$snap$outlet_lon,
                lat = preview$snap$outlet_lat,
                group = 'Outlets candidatos',
                layerId = paste0('AMBIG_', ii),
                radius = 10,
                color = color_now,
                fillColor = color_now,
                fillOpacity = 1,
                weight = 3,
                label = label_now,
                labelOptions = leaflet::labelOptions(
                  noHide = TRUE,
                  direction = 'top'
                )
              )

            bboxes[[ii]] <- sf::st_bbox(
              preview$basin_map
            )
          }

          bbox_matrix <- do.call(
            rbind,
            lapply(bboxes, as.numeric)
          )

          proxy |>
            leaflet::fitBounds(
              min(bbox_matrix[, 1], na.rm = TRUE),
              min(bbox_matrix[, 2], na.rm = TRUE),
              max(bbox_matrix[, 3], na.rm = TRUE),
              max(bbox_matrix[, 4], na.rm = TRUE)
            )

          invisible(NULL)
        }


        output$ambiguity_controls <- shiny::renderUI({

          previews <- ambiguity_previews()

          if (
            is.null(previews) ||
            length(previews) < 2L
          ) {
            return(NULL)
          }

          shiny::div(
            class = 'ambiguity-box',
            shiny::tags$strong(
              'Se detectaron varias cuencas posibles'
            ),
            shiny::div(
              class = 'coord-note',
              paste0(
                'Haz clic en uno de los outlets numerados del mapa ',
                'o elige una opcion aqui. Las alternativas ya fueron ',
                'delimitadas para que puedas comparar sus areas.'
              )
            ),
            lapply(
              seq_along(previews),
              function(ii) {
                shiny::actionButton(
                  session$ns(
                    paste0('elegir_candidato_', ii)
                  ),
                  paste0(
                    'Opcion ',
                    ii,
                    ' · ',
                    format(
                      round(
                        previews[[ii]]$area_km2,
                        1
                      ),
                      big.mark = ',',
                      scientific = FALSE,
                      trim = TRUE
                    ),
                    ' km²'
                  ),
                  width = '100%',
                  style = 'margin-top:5px;'
                )
              }
            )
          )
        })


        activate_ambiguity_candidate <- function(candidate_index) {

          previews <- ambiguity_previews()
          context <- ambiguity_context()
          candidate_index <- as.integer(candidate_index)

          if (
            is.null(previews) ||
            is.null(context) ||
            !is.finite(candidate_index) ||
            candidate_index < 1L ||
            candidate_index > length(previews)
          ) {
            return(invisible(FALSE))
          }

          chosen <- previews[[candidate_index]]

          dir.create(
            context$out_dir,
            recursive = TRUE,
            showWarnings = FALSE
          )

          copied <- c(
            file.copy(
              chosen$basin_tif,
              file.path(context$out_dir, 'basin.tif'),
              overwrite = TRUE
            ),
            file.copy(
              chosen$basin_gpkg,
              file.path(context$out_dir, 'basin.gpkg'),
              overwrite = TRUE
            )
          )

          if (!all(copied)) {
            stop(
              'No fue posible consolidar la alternativa seleccionada.'
            )
          }

          outlet_target <- file.path(
            context$out_dir,
            'outlet.gpkg'
          )

          if (file.exists(outlet_target)) {
            unlink(outlet_target, force = TRUE)
          }

          outlet_sf <- chosen$outlet_sf
          outlet_sf$NAME <- context$nombre
          outlet_sf$BLOCK_ID <- context$block_id

          sf::st_write(
            outlet_sf,
            outlet_target,
            layer = 'outlet',
            quiet = TRUE
          )

          basin_result(chosen$basin_sf)
          outlet_result(outlet_sf)
          output_folder(context$out_dir)
          block_id_result(context$block_id)
          basin_source_result('delineated')
          basin_label_result(context$nombre)

          writeLines(
            c(
              paste(
                'Completed:',
                format(
                  Sys.time(),
                  '%Y-%m-%d %H:%M:%S'
                )
              ),
              paste('Block:', context$block_id),
              paste('Ambiguity reason:', context$reason),
              paste('Selected option:', candidate_index),
              paste('Area km2:', sprintf('%.3f', chosen$area_km2)),
              paste('Snap mode:', chosen$snap$snap_mode),
              paste('Snap distance m:', sprintf('%.1f', chosen$snap$snap_distance_m)),
              paste('Trace cells:', round(chosen$trace_cells)),
              paste('Trace engine:', chosen$trace_engine),
              'Status: OK'
            ),
            file.path(
              context$out_dir,
              'COMPLETADO.txt'
            )
          )

          bb <- sf::st_bbox(
            chosen$basin_map
          )

          leaflet::leafletProxy(
            'mapa',
            session = session
          ) |>
            leaflet::clearGroup('Cuencas candidatas') |>
            leaflet::clearGroup('Outlets candidatos') |>
            leaflet::clearGroup('Punto') |>
            leaflet::clearGroup('Cuenca delimitada') |>
            leaflet::addPolygons(
              data = chosen$basin_map,
              group = 'Cuenca delimitada',
              color = '#D50000',
              weight = 3,
              opacity = 1,
              fillColor = '#FF5252',
              fillOpacity = 0.22
            ) |>
            leaflet::addCircleMarkers(
              lng = chosen$snap$outlet_lon,
              lat = chosen$snap$outlet_lat,
              group = 'Punto',
              radius = 8,
              color = '#B71C1C',
              fillColor = '#EF5350',
              fillOpacity = 1,
              weight = 3,
              popup = 'Outlet seleccionado'
            ) |>
            leaflet::fitBounds(
              bb['xmin'],
              bb['ymin'],
              bb['xmax'],
              bb['ymax']
            )

          shiny::showNotification(
            paste0(
              'Opcion ',
              candidate_index,
              ' seleccionada · ',
              format(
                round(chosen$area_km2, 1),
                big.mark = ',',
                scientific = FALSE,
                trim = TRUE
              ),
              ' km².'
            ),
            type = 'message',
            duration = 6
          )

          clear_ambiguity_preview(
            clear_map = FALSE,
            remove_files = TRUE
          )

          gc()
          invisible(TRUE)
        }
'''

SNAP_FLOW = r'''
                    clear_ambiguity_preview()

                    ambiguity <- find_ambiguity_options(
                      lon = click$lon,
                      lat = click$lat,
                      radius_m = DEFAULT_SNAP_RADIUS_M,
                      grid_template = grid_template,
                      stream_cache = block_cache$stream_cache,
                      stripe_rows = block_cache$stripe_rows,
                      n_stripes = block_cache$n_stripes
                    )

                    if (identical(
                      ambiguity$status,
                      'ambiguous'
                    )) {

                      ambiguity_root <- file.path(
                        TERRA_TEMP,
                        paste0(
                          'AMBIG_',
                          gsub(
                            '[^A-Za-z0-9_]',
                            '_',
                            session$token
                          ),
                          '_',
                          format(
                            Sys.time(),
                            '%Y%m%d%H%M%S'
                          )
                        )
                      )

                      dir.create(
                        ambiguity_root,
                        recursive = TRUE,
                        showWarnings = FALSE
                      )

                      previews <- list()

                      for (ii in seq_along(
                        ambiguity$options
                      )) {

                        shiny::setProgress(
                          value = min(
                            0.90,
                            0.25 +
                              0.60 *
                                ii /
                                length(ambiguity$options)
                          ),
                          detail = paste0(
                            'Delimitando alternativa ',
                            ii,
                            ' de ',
                            length(ambiguity$options)
                          )
                        )

                        preview <- tryCatch(
                          materialize_ambiguity_option(
                            option = ambiguity$options[[ii]],
                            candidate_index = ii,
                            grid_template = grid_template,
                            work_root = ambiguity_root
                          ),
                          error = function(e) {
                            shiny::showNotification(
                              paste0(
                                'Alternativa ',
                                ii,
                                ': ',
                                conditionMessage(e)
                              ),
                              type = 'warning',
                              duration = 8
                            )
                            NULL
                          }
                        )

                        if (!is.null(preview)) {
                          previews[[length(previews) + 1L]] <- preview
                        }
                      }

                      if (length(previews) >= 2L) {

                        ambiguity_previews(previews)

                        ambiguity_context(
                          list(
                            out_dir = out_dir,
                            nombre = nombre,
                            block_id = block_id,
                            work_root = ambiguity_root,
                            reason = ambiguity$reason
                          )
                        )

                        render_ambiguity_map(previews)

                        shiny::setProgress(
                          value = 1,
                          detail = paste0(
                            'Se detectaron ',
                            length(previews),
                            ' cuencas posibles. Selecciona una.'
                          )
                        )

                        shiny::showNotification(
                          paste0(
                            'Se detectaron ',
                            length(previews),
                            ' cuencas hidrologicamente posibles. ',
                            'Selecciona el outlet numerado en el mapa.'
                          ),
                          type = 'warning',
                          duration = 10
                        )

                        return(invisible(NULL))
                      }

                      if (length(previews) == 1L) {
                        snap <- previews[[1]]$snap
                        unlink(
                          ambiguity_root,
                          recursive = TRUE,
                          force = TRUE
                        )
                      } else {
                        unlink(
                          ambiguity_root,
                          recursive = TRUE,
                          force = TRUE
                        )
                        stop(
                          'No fue posible materializar las alternativas detectadas.'
                        )
                      }

                    } else {
                      snap <- ambiguity$options[[1]]
                    }
'''

OBSERVERS = r'''
        # ====================================================
        # SELECCION DE CUENCA AMBIGUA
        # ====================================================

        shiny::observeEvent(
          input$mapa_marker_click,
          {
            marker <- input$mapa_marker_click

            if (
              is.null(marker$id) ||
              !grepl(
                '^AMBIG_[123]$',
                as.character(marker$id)
              )
            ) {
              return()
            }

            idx <- as.integer(
              sub(
                '^AMBIG_',
                '',
                as.character(marker$id)
              )
            )

            tryCatch(
              activate_ambiguity_candidate(idx),
              error = delim_error_handler
            )
          },
          ignoreNULL = TRUE
        )


        lapply(
          seq_len(AMBIGUITY_MAX_OPTIONS),
          function(ii) {
            local({
              idx <- ii
              input_id <- paste0(
                'elegir_candidato_',
                idx
              )

              shiny::observeEvent(
                input[[input_id]],
                {
                  tryCatch(
                    activate_ambiguity_candidate(idx),
                    error = delim_error_handler
                  )
                },
                ignoreInit = TRUE
              )
            })
          }
        )
'''

# Add module-level hydrologic ambiguity logic before UI.
if 'find_ambiguity_options <- function' not in text:
    text = replace_once(
        text,
        '\n\n\n\n  ui <- function(id) {',
        AMBIGUITY_LOGIC + '\n\n\n  ui <- function(id) {',
        'ambiguity logic'
    )

# CSS and UI slot.
text = replace_once(
    text,
    '".delimitacion-export .btn{width:100%;margin-top:2px;}"',
    '".delimitacion-export .btn{width:100%;margin-top:2px;}",\n'
    '            ".ambiguity-box{margin:10px 0;padding:10px;border:1px solid #f0ad4e;",\n'
    '            "border-radius:6px;background:#fff8e8;}"',
    'ambiguity css'
)

text = replace_once(
    text,
    '''          shiny::uiOutput(
            ns(
              "origin_controls"
            )
          ),


          shiny::uiOutput(
            ns(
              "export_controls"
            )
          ),''',
    '''          shiny::uiOutput(
            ns(
              "origin_controls"
            )
          ),


          shiny::uiOutput(
            ns(
              "ambiguity_controls"
            )
          ),


          shiny::uiOutput(
            ns(
              "export_controls"
            )
          ),''',
    'ambiguity ui slot'
)

# Insert server helpers immediately before export controls.
if 'materialize_ambiguity_option <- function' not in text:
    text = replace_once(
        text,
        '''        # ====================================================
        # EXPORTACION DE LA CUENCA ACTIVA
        # ====================================================
''',
        SERVER_HELPERS + '''\n\n\n        # ====================================================
        # EXPORTACION DE LA CUENCA ACTIVA
        # ====================================================
''',
        'server ambiguity helpers'
    )

# Clear pending alternatives when the user changes point or switches to import.
text = replace_once(
    text,
    '''          lon <- point$lon
          lat <- point$lat


          click_value(''',
    '''          lon <- point$lon
          lat <- point$lat


          clear_ambiguity_preview()


          click_value(''',
    'clear on new point'
)

text = replace_once(
    text,
    '''        shiny::observeEvent(
          input$usar_cuenca_importada,
          {

            tryCatch(''',
    '''        shiny::observeEvent(
          input$usar_cuenca_importada,
          {

            clear_ambiguity_preview()


            tryCatch(''',
    'clear on import'
)

# Replace the direct snap by ambiguity-aware resolution.
text = replace_once(
    text,
    '''                    snap <- snap_to_stream_stripes(
                      lon = click$lon,
                      lat = click$lat,
                      radius_m = DEFAULT_SNAP_RADIUS_M,
                      grid_template = grid_template,
                      stream_cache = block_cache$stream_cache,
                      stripe_rows = block_cache$stripe_rows,
                      n_stripes = block_cache$n_stripes
                    )
''',
    SNAP_FLOW,
    'ambiguity-aware snap flow'
)

# Add selection observers before Limpiar.
if 'SELECCION DE CUENCA AMBIGUA' not in text:
    text = replace_once(
        text,
        '''        # ====================================================
        # LIMPIAR
        # ====================================================
''',
        OBSERVERS + '''\n\n\n        # ====================================================
        # LIMPIAR
        # ====================================================
''',
        'ambiguity selection observers'
    )

text = replace_once(
    text,
    '''        shiny::observeEvent(
          input$limpiar,
          {

            click_value(''',
    '''        shiny::observeEvent(
          input$limpiar,
          {

            clear_ambiguity_preview()


            click_value(''',
    'clear on reset'
)

PATH.write_text(text, encoding='utf-8')
print('Ambiguity choice patch applied to R/delimitacion.R')

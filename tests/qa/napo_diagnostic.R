# Diagnostico dirigido para el Rio Napo.
# Objetivo: separar error de snap en rio ancho de una limitacion del Runtime.

options(warn = 1)
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(root)

app_env <- new.env(parent = globalenv())
source("app.R", local = app_env, echo = FALSE, print.eval = FALSE, encoding = "UTF-8")

out_dir <- "qa_results/napo_diagnostic"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cases <- data.frame(
  CASE_ID = c("NDA-001", "NDA-002", "NDA-003", "NDA-004"),
  NAME = c(
    "Mazan DHN - configuracion actual",
    "Bellavista Mazan SENAMHI - configuracion actual",
    "Santa Clotilde - configuracion actual",
    "Mazan DHN - trayectoria D8 hasta 1500 m"
  ),
  LON = c(-73.091694, -73.073000, -73.630000, -73.091694),
  LAT = c(-3.496528, -3.482000, -2.520000, -3.496528),
  TOPOLOGY_MAX_M = c(300, 300, 300, 1500),
  stringsAsFactors = FALSE
)

new_cache <- function() {
  e <- new.env(parent = emptyenv())
  e$block_id <- NULL
  e$grid_template <- NULL
  e$reverse_cache <- NULL
  e$stream_cache <- NULL
  e$stripe_rows <- NULL
  e$n_stripes <- NULL
  e$stream_threshold_cells <- NULL
  e$stream_threshold_km2 <- NULL
  e
}

cache <- new_cache()

run_case <- function(row) {
  id <- row$CASE_ID
  lon <- row$LON
  lat <- row$LAT
  old_topology <- app_env$SNAP_TOPOLOGY_MAX_M
  app_env$SNAP_TOPOLOGY_MAX_M <- row$TOPOLOGY_MAX_M
  on.exit({ app_env$SNAP_TOPOLOGY_MAX_M <- old_topology }, add = TRUE)

  res <- data.frame(
    CASE_ID = id,
    NAME = row$NAME,
    LON = lon,
    LAT = lat,
    TOPOLOGY_MAX_M = row$TOPOLOGY_MAX_M,
    BLOCK_ID = NA_character_,
    SNAP_MODE = NA_character_,
    SNAP_DISTANCE_M = NA_real_,
    OUTLET_LON = NA_real_,
    OUTLET_LAT = NA_real_,
    TRACE_CELLS = NA_real_,
    AREA_KM2 = NA_real_,
    BBOX_XMIN = NA_real_,
    BBOX_XMAX = NA_real_,
    BBOX_YMIN = NA_real_,
    BBOX_YMAX = NA_real_,
    CROSSES_ECUADOR_LONGITUDE = NA,
    TOTAL_TIME_S = NA_real_,
    OUTCOME = NA_character_,
    ERROR = NA_character_,
    stringsAsFactors = FALSE
  )

  t0 <- Sys.time()

  tryCatch({
    block <- app_env$find_block_for_click(lon, lat)
    if (is.null(block) || nrow(block) < 1L) stop("Punto fuera del catalogo")
    block_id <- as.character(block$BLOCK_ID[1])
    res$BLOCK_ID <- block_id
    app_env$load_block_if_needed(block_id, cache)

    snap <- app_env$snap_to_stream_stripes(
      lon = lon,
      lat = lat,
      radius_m = app_env$DEFAULT_SNAP_RADIUS_M,
      grid_template = cache$grid_template,
      stream_cache = cache$stream_cache,
      stripe_rows = cache$stripe_rows,
      n_stripes = cache$n_stripes
    )

    res$SNAP_MODE <- snap$snap_mode
    res$SNAP_DISTANCE_M <- snap$snap_distance_m
    res$OUTLET_LON <- snap$outlet_lon
    res$OUTLET_LAT <- snap$outlet_lat

    trace <- app_env$trace_upstream(
      cache = cache$reverse_cache,
      outlet_cell = snap$outlet_cell,
      progress_fun = function(...) invisible(NULL)
    )
    res$TRACE_CELLS <- trace$n_cells

    tmp <- tempfile(pattern = paste0(id, "_"))
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    basin_tif <- file.path(tmp, "basin.tif")
    basin_gpkg <- file.path(tmp, "basin.gpkg")

    app_env$write_basin_raster(
      cells = trace$cells,
      template = cache$grid_template,
      output_file = basin_tif,
      temp_dir = file.path(tmp, "stripes"),
      bbox = trace$bbox
    )

    basin <- app_env$polygonize_basin(
      basin_tif,
      basin_gpkg,
      expected_cells = trace$n_cells
    )

    basin_wgs <- sf::st_transform(basin, 4326)
    area <- as.numeric(sf::st_area(sf::st_union(sf::st_transform(basin, 6933)))) / 1e6
    bb <- sf::st_bbox(basin_wgs)

    res$AREA_KM2 <- area
    res$BBOX_XMIN <- as.numeric(bb["xmin"])
    res$BBOX_XMAX <- as.numeric(bb["xmax"])
    res$BBOX_YMIN <- as.numeric(bb["ymin"])
    res$BBOX_YMAX <- as.numeric(bb["ymax"])
    # El Napo cruza hacia Ecuador bastante al oeste de los resultados erroneos
    # observados en Mazan. Este indicador es diagnostico, no una frontera politica exacta.
    res$CROSSES_ECUADOR_LONGITUDE <- is.finite(res$BBOX_XMIN) && res$BBOX_XMIN < -75.0

    sf::st_write(
      sf::st_simplify(basin_wgs, dTolerance = 0.001, preserveTopology = TRUE),
      file.path(out_dir, paste0(id, ".gpkg")),
      layer = "basin",
      delete_dsn = TRUE,
      quiet = TRUE
    )

    grDevices::png(file.path(out_dir, paste0(id, ".png")), width = 1400, height = 1000, res = 140)
    plot(sf::st_geometry(basin_wgs), col = "grey90", border = "black", axes = TRUE,
         main = paste0(id, " | ", row$NAME, " | ", sprintf("%.1f km2", area)))
    graphics::points(snap$outlet_lon, snap$outlet_lat, pch = 19)
    graphics::points(lon, lat, pch = 4, lwd = 2)
    grDevices::dev.off()

    res$OUTCOME <- "SUCCESS"
    unlink(tmp, recursive = TRUE, force = TRUE)
    rm(trace, basin, basin_wgs)
    gc()
  }, error = function(e) {
    res$OUTCOME <<- "ERROR"
    res$ERROR <<- conditionMessage(e)
  })

  res$TOTAL_TIME_S <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  res
}

results <- lapply(seq_len(nrow(cases)), function(i) {
  x <- run_case(cases[i, , drop = FALSE])
  print(x)
  x
})
results <- do.call(rbind, results)
write.csv(results, file.path(out_dir, "results_napo_diagnostic.csv"), row.names = FALSE, na = "")
print(results)

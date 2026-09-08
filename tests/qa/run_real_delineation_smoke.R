# QA funcional real de delimitacion FABDEM
# Ejecuta la misma ruta hidrologica del Explorer sin servidor Shiny.

options(warn = 1)

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(root)

app_env <- new.env(parent = globalenv())
source("app.R", local = app_env, echo = FALSE, print.eval = FALSE, encoding = "UTF-8")

cases <- read.csv(
  "tests/qa/delineation_cases.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

group_filter <- Sys.getenv("QA_GROUP", unset = "all")
if (!identical(group_filter, "all")) {
  cases <- cases[cases$GROUP == group_filter, , drop = FALSE]
}

if (nrow(cases) < 1L) {
  stop("No hay casos QA para el grupo: ", group_filter)
}

out_dir <- file.path("qa_results", group_filter)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

new_block_cache <- function() {
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

block_cache <- new_block_cache()

safe_num <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  if (length(y) == 0L || !is.finite(y[1])) NA_real_ else y[1]
}

one_case <- function(case_row) {
  id <- as.character(case_row$CASE_ID)
  name <- as.character(case_row$FEATURE_NAME)
  lon <- safe_num(case_row$LON)
  lat <- safe_num(case_row$LAT)
  expected_result <- as.character(case_row$EXPECTED_RESULT)
  expected_area <- safe_num(case_row$EXPECTED_AREA_KM2)
  area_tol <- safe_num(case_row$AREA_TOLERANCE_FRAC)

  cat("\n===== ", id, " | ", name, " =====\n", sep = "")

  started <- Sys.time()
  snap_started <- NA
  snap_finished <- NA
  trace_started <- NA
  trace_finished <- NA
  polygon_started <- NA
  polygon_finished <- NA

  result <- data.frame(
    CASE_ID = id,
    GROUP = as.character(case_row$GROUP),
    FEATURE_NAME = name,
    CASE_TYPE = as.character(case_row$CASE_TYPE),
    CLICK_LON = lon,
    CLICK_LAT = lat,
    EXPECTED_RESULT = expected_result,
    EXPECTED_AREA_KM2 = expected_area,
    AREA_TOLERANCE_FRAC = area_tol,
    BLOCK_ID = NA_character_,
    SNAP_MODE = NA_character_,
    SNAP_DISTANCE_M = NA_real_,
    OUTLET_LON = NA_real_,
    OUTLET_LAT = NA_real_,
    TRACE_CELLS = NA_real_,
    TRACE_ENGINE = NA_character_,
    BASIN_AREA_KM2 = NA_real_,
    AREA_REL_ERROR = NA_real_,
    BBOX_XMIN = NA_real_,
    BBOX_XMAX = NA_real_,
    BBOX_YMIN = NA_real_,
    BBOX_YMAX = NA_real_,
    N_POLYGON_PARTS = NA_integer_,
    SNAP_TIME_S = NA_real_,
    TRACE_TIME_S = NA_real_,
    POLYGON_TIME_S = NA_real_,
    TOTAL_TIME_S = NA_real_,
    OUTCOME = NA_character_,
    ERROR_MESSAGE = NA_character_,
    stringsAsFactors = FALSE
  )

  tryCatch({
    block <- app_env$find_block_for_click(lon, lat)
    if (is.null(block) || nrow(block) < 1L) {
      stop("El punto no pertenece a ningun bloque hidrologico del catalogo.")
    }

    block_id <- as.character(block[["BLOCK_ID"]][1])
    result$BLOCK_ID <- block_id

    app_env$load_block_if_needed(block_id, block_cache)

    snap_started <- Sys.time()
    snap <- app_env$snap_to_stream_stripes(
      lon = lon,
      lat = lat,
      radius_m = app_env$DEFAULT_SNAP_RADIUS_M,
      grid_template = block_cache$grid_template,
      stream_cache = block_cache$stream_cache,
      stripe_rows = block_cache$stripe_rows,
      n_stripes = block_cache$n_stripes
    )
    snap_finished <- Sys.time()

    result$SNAP_MODE <- as.character(snap$snap_mode)
    result$SNAP_DISTANCE_M <- safe_num(snap$snap_distance_m)
    result$OUTLET_LON <- safe_num(snap$outlet_lon)
    result$OUTLET_LAT <- safe_num(snap$outlet_lat)

    trace_started <- Sys.time()
    trace <- app_env$trace_upstream(
      cache = block_cache$reverse_cache,
      outlet_cell = snap$outlet_cell,
      progress_fun = function(...) invisible(NULL)
    )
    trace_finished <- Sys.time()

    result$TRACE_CELLS <- safe_num(trace$n_cells)
    result$TRACE_ENGINE <- as.character(trace$trace_engine)

    if (trace$n_cells < block_cache$stream_threshold_cells) {
      stop(
        paste0(
          "Rastreo inferior al umbral del bloque: ",
          trace$n_cells,
          " < ",
          block_cache$stream_threshold_cells
        )
      )
    }

    case_tmp <- tempfile(pattern = paste0("qa_", id, "_"))
    dir.create(case_tmp, recursive = TRUE, showWarnings = FALSE)
    basin_tif <- file.path(case_tmp, "basin.tif")
    basin_gpkg <- file.path(case_tmp, "basin.gpkg")

    app_env$write_basin_raster(
      cells = trace$cells,
      template = block_cache$grid_template,
      output_file = basin_tif,
      temp_dir = file.path(case_tmp, "stripes"),
      bbox = trace$bbox
    )

    polygon_started <- Sys.time()
    basin <- app_env$polygonize_basin(
      basin_tif = basin_tif,
      basin_gpkg = basin_gpkg,
      expected_cells = trace$n_cells
    )
    polygon_finished <- Sys.time()

    basin <- sf::st_make_valid(basin)
    basin_wgs <- sf::st_transform(basin, 4326)
    basin_eq <- sf::st_transform(basin, 6933)

    area_km2 <- as.numeric(sf::st_area(sf::st_union(basin_eq))) / 1e6
    result$BASIN_AREA_KM2 <- area_km2

    if (is.finite(expected_area) && expected_area > 0) {
      result$AREA_REL_ERROR <- abs(area_km2 - expected_area) / expected_area
    }

    bb <- sf::st_bbox(basin_wgs)
    result$BBOX_XMIN <- as.numeric(bb["xmin"])
    result$BBOX_XMAX <- as.numeric(bb["xmax"])
    result$BBOX_YMIN <- as.numeric(bb["ymin"])
    result$BBOX_YMAX <- as.numeric(bb["ymax"])

    parts <- suppressWarnings(
      sf::st_cast(
        sf::st_union(sf::st_geometry(basin_wgs)),
        "POLYGON"
      )
    )
    result$N_POLYGON_PARTS <- length(parts)

    png_file <- file.path(out_dir, paste0(id, ".png"))
    grDevices::png(png_file, width = 1400, height = 1000, res = 140)
    op <- graphics::par(mar = c(4, 4, 3, 1))
    plot(
      sf::st_geometry(basin_wgs),
      border = "black",
      col = "grey90",
      main = paste0(id, " | ", name, " | ", sprintf("%.1f km2", area_km2)),
      axes = TRUE
    )
    graphics::points(
      result$OUTLET_LON,
      result$OUTLET_LAT,
      pch = 19,
      cex = 1.1
    )
    graphics::points(
      lon,
      lat,
      pch = 4,
      cex = 1.1,
      lwd = 2
    )
    graphics::legend(
      "topright",
      legend = c("Outlet FABDEM", "Clic"),
      pch = c(19, 4),
      bty = "n"
    )
    graphics::par(op)
    grDevices::dev.off()

    # Guarda una geometria simplificada para inspeccion posterior.
    simp <- tryCatch(
      sf::st_simplify(basin_wgs, dTolerance = 0.001, preserveTopology = TRUE),
      error = function(e) basin_wgs
    )
    sf::st_write(
      simp,
      file.path(out_dir, paste0(id, ".gpkg")),
      layer = "basin",
      delete_dsn = TRUE,
      quiet = TRUE
    )

    result$OUTCOME <- "SUCCESS"

    rm(trace, basin, basin_wgs, basin_eq, parts, simp)
    gc()
    unlink(case_tmp, recursive = TRUE, force = TRUE)

  }, error = function(e) {
    result$ERROR_MESSAGE <<- conditionMessage(e)
    result$OUTCOME <<- "ERROR"
    cat("ERROR: ", conditionMessage(e), "\n", sep = "")
  })

  ended <- Sys.time()

  if (!is.na(snap_started) && !is.na(snap_finished)) {
    result$SNAP_TIME_S <- as.numeric(difftime(snap_finished, snap_started, units = "secs"))
  }
  if (!is.na(trace_started) && !is.na(trace_finished)) {
    result$TRACE_TIME_S <- as.numeric(difftime(trace_finished, trace_started, units = "secs"))
  }
  if (!is.na(polygon_started) && !is.na(polygon_finished)) {
    result$POLYGON_TIME_S <- as.numeric(difftime(polygon_finished, polygon_started, units = "secs"))
  }
  result$TOTAL_TIME_S <- as.numeric(difftime(ended, started, units = "secs"))

  # Evaluacion automatica orientativa. La revision visual sigue siendo obligatoria.
  if (identical(expected_result, "FAIL_EXPECTED")) {
    result$QA_AUTO <- if (identical(result$OUTCOME, "ERROR")) "PASS" else "REVIEW"
  } else if (identical(expected_result, "FAIL_OR_LONG_SNAP")) {
    result$QA_AUTO <- if (
      identical(result$OUTCOME, "ERROR") ||
      (is.finite(result$SNAP_DISTANCE_M) && result$SNAP_DISTANCE_M >= 500)
    ) "PASS" else "REVIEW"
  } else if (identical(result$OUTCOME, "SUCCESS")) {
    area_ok <- TRUE
    if (is.finite(expected_area) && expected_area > 0 && is.finite(area_tol)) {
      area_ok <- is.finite(result$AREA_REL_ERROR) && result$AREA_REL_ERROR <= area_tol
    }
    geom_ok <- isTRUE(result$N_POLYGON_PARTS == 1L)
    snap_ok <- is.finite(result$SNAP_DISTANCE_M) && result$SNAP_DISTANCE_M <= app_env$DEFAULT_SNAP_RADIUS_M
    result$QA_AUTO <- if (area_ok && geom_ok && snap_ok) "PASS" else "REVIEW"
  } else {
    result$QA_AUTO <- "FAIL"
  }

  result
}

results <- vector("list", nrow(cases))

for (i in seq_len(nrow(cases))) {
  results[[i]] <- one_case(cases[i, , drop = FALSE])
  interim <- do.call(rbind, results[seq_len(i)])
  write.csv(
    interim,
    file.path(out_dir, paste0("results_", group_filter, ".csv")),
    row.names = FALSE,
    na = ""
  )
}

results <- do.call(rbind, results)
write.csv(
  results,
  file.path(out_dir, paste0("results_", group_filter, ".csv")),
  row.names = FALSE,
  na = ""
)

print(results[, c(
  "CASE_ID", "FEATURE_NAME", "BLOCK_ID", "SNAP_MODE", "SNAP_DISTANCE_M",
  "BASIN_AREA_KM2", "AREA_REL_ERROR", "N_POLYGON_PARTS", "TOTAL_TIME_S",
  "OUTCOME", "QA_AUTO", "ERROR_MESSAGE"
)])

cat("\nQA real terminado para grupo: ", group_filter, "\n", sep = "")

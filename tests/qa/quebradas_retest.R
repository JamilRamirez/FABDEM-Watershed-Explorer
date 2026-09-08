# Retest dirigido de quebradas usando puntos documentados sobre cauce/thalweg.

options(warn = 1)
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(root)
app_env <- new.env(parent = globalenv())
source("app.R", local = app_env, echo = FALSE, print.eval = FALSE, encoding = "UTF-8")

out_dir <- "qa_results/quebradas_retest"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cases <- data.frame(
  CASE_ID = c("QRT-001", "QRT-002", "QRT-003"),
  NAME = c(
    "Huaycoloro - puente tramo bajo documentado",
    "San Ildefonso - thalweg oficial",
    "San Carlos - cauce documentado Laredo"
  ),
  LON = c(-76.9519475, -79.0001380, -78.9415105),
  LAT = c(-12.0191751, -8.0657458, -8.0757427),
  EXPECTED_AREA = c(481.0, 10.7, 39.72),
  stringsAsFactors = FALSE
)

new_cache <- function() {
  e <- new.env(parent = emptyenv())
  e$block_id <- NULL; e$grid_template <- NULL; e$reverse_cache <- NULL
  e$stream_cache <- NULL; e$stripe_rows <- NULL; e$n_stripes <- NULL
  e$stream_threshold_cells <- NULL; e$stream_threshold_km2 <- NULL
  e
}
cache <- new_cache()

run_case <- function(r) {
  ans <- data.frame(
    CASE_ID=r$CASE_ID, NAME=r$NAME, LON=r$LON, LAT=r$LAT,
    EXPECTED_AREA_KM2=r$EXPECTED_AREA, BLOCK_ID=NA_character_,
    SNAP_MODE=NA_character_, SNAP_DISTANCE_M=NA_real_,
    OUTLET_LON=NA_real_, OUTLET_LAT=NA_real_, TRACE_CELLS=NA_real_,
    AREA_KM2=NA_real_, AREA_REL_ERROR=NA_real_, N_POLYGON_PARTS=NA_integer_,
    TOTAL_TIME_S=NA_real_, OUTCOME=NA_character_, ERROR=NA_character_,
    stringsAsFactors=FALSE
  )
  t0 <- Sys.time()
  tryCatch({
    block <- app_env$find_block_for_click(r$LON, r$LAT)
    if (is.null(block) || nrow(block)<1) stop("Punto fuera del catalogo")
    bid <- as.character(block$BLOCK_ID[1]); ans$BLOCK_ID <- bid
    app_env$load_block_if_needed(bid, cache)
    snap <- app_env$snap_to_stream_stripes(
      r$LON, r$LAT, app_env$DEFAULT_SNAP_RADIUS_M,
      cache$grid_template, cache$stream_cache, cache$stripe_rows, cache$n_stripes
    )
    ans$SNAP_MODE <- snap$snap_mode; ans$SNAP_DISTANCE_M <- snap$snap_distance_m
    ans$OUTLET_LON <- snap$outlet_lon; ans$OUTLET_LAT <- snap$outlet_lat
    trace <- app_env$trace_upstream(cache$reverse_cache, snap$outlet_cell, function(...) invisible(NULL))
    ans$TRACE_CELLS <- trace$n_cells
    tmp <- tempfile(); dir.create(tmp)
    tif <- file.path(tmp,"basin.tif"); gpkg <- file.path(tmp,"basin.gpkg")
    app_env$write_basin_raster(trace$cells, cache$grid_template, tif, file.path(tmp,"stripes"), trace$bbox)
    basin <- app_env$polygonize_basin(tif, gpkg, expected_cells=trace$n_cells)
    bw <- sf::st_transform(basin,4326)
    area <- as.numeric(sf::st_area(sf::st_union(sf::st_transform(basin,6933))))/1e6
    ans$AREA_KM2 <- area; ans$AREA_REL_ERROR <- abs(area-r$EXPECTED_AREA)/r$EXPECTED_AREA
    parts <- suppressWarnings(sf::st_cast(sf::st_union(sf::st_geometry(bw)),"POLYGON"))
    ans$N_POLYGON_PARTS <- length(parts)
    sf::st_write(sf::st_simplify(bw,0.001,preserveTopology=TRUE), file.path(out_dir,paste0(r$CASE_ID,".gpkg")), layer="basin", delete_dsn=TRUE, quiet=TRUE)
    grDevices::png(file.path(out_dir,paste0(r$CASE_ID,".png")),1400,1000,res=140)
    plot(sf::st_geometry(bw),col="grey90",border="black",axes=TRUE,main=paste0(r$CASE_ID," | ",r$NAME," | ",sprintf("%.1f km2",area)))
    graphics::points(snap$outlet_lon,snap$outlet_lat,pch=19); graphics::points(r$LON,r$LAT,pch=4,lwd=2)
    grDevices::dev.off()
    ans$OUTCOME <- "SUCCESS"
    unlink(tmp,recursive=TRUE,force=TRUE)
    rm(trace,basin,bw); gc()
  },error=function(e){ans$OUTCOME <<- "ERROR"; ans$ERROR <<- conditionMessage(e)})
  ans$TOTAL_TIME_S <- as.numeric(difftime(Sys.time(),t0,units="secs"))
  ans
}

results <- do.call(rbind,lapply(seq_len(nrow(cases)),function(i){x<-run_case(cases[i,,drop=FALSE]);print(x);x}))
write.csv(results,file.path(out_dir,"results_quebradas_retest.csv"),row.names=FALSE,na="")
print(results)

# ============================================================
# R/config.R
#
# FABDEM Watershed Explorer
# CONFIGURACION REMOTA
# v9: Runtime GitHub + cache temporal bajo demanda
# ============================================================
#
# Todos los recursos de Runtime se descargan desde GitHub solo
# cuando una operacion los necesita. No se usa una carpeta hermana.
# ============================================================


APP_ROOT <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)


DATA_DIR <- file.path(
  APP_ROOT,
  "data"
)


REMOTE_SOURCES_CSV <- file.path(
  DATA_DIR,
  "remote_sources.csv"
)


runtime_sources <- read.csv(
  REMOTE_SOURCES_CSV,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


if (
  nrow(runtime_sources) != 1L ||
  !all(c("SOURCE_ID", "BASE_URL") %in% names(runtime_sources)) ||
  !nzchar(trimws(runtime_sources[["BASE_URL"]][1]))
) {
  stop("data/remote_sources.csv no contiene una URL base valida.")
}


RUNTIME_BASE_URL <- paste0(
  sub("/+$", "", trimws(runtime_sources[["BASE_URL"]][1])),
  "/"
)


RUNTIME_ROOT <- file.path(
  tempdir(),
  "FABDEM_Watershed_Runtime"
)


CORE_DIR <- file.path(
  RUNTIME_ROOT,
  "core"
)


DEM_DIR <- file.path(
  RUNTIME_ROOT,
  "dem"
)



# Indice liviano creado/reutilizado por Morfometria.
# Guarda rutas RELATIVAS a DEM_DIR y huellas en EPSG:4326.
DEM_TILE_INDEX_RDS <- file.path(
  DEM_DIR,
  "_tile_index_morfometria.rds"
)


# ------------------------------------------------------------
# CAPAS TEMATICAS NORMALIZADAS
# ------------------------------------------------------------

LAYERS_DIR <- file.path(
  RUNTIME_ROOT,
  "layers"
)


MEDIO_FISICO_DIR <- file.path(
  LAYERS_DIR,
  "medio_fisico"
)


CLIMA_SUPERFICIE_DIR <- file.path(
  LAYERS_DIR,
  "clima_superficie"
)


CONTEXTO_TERRITORIAL_DIR <- file.path(
  LAYERS_DIR,
  "contexto_territorial"
)


FONDO_MAPA_GPKG <- file.path(
  LAYERS_DIR,
  "fondo_mapa.gpkg"
)


LAYER_METADATA_XLSX <- file.path(
  DATA_DIR,
  "Metadata_normalizada.xlsx"
)


LAYER_SOURCES_CSV <- file.path(
  DATA_DIR,
  "fuentes_capas.csv"
)


BLOCK_LOOKUP_GPKG <- file.path(
  DATA_DIR,
  "block_lookup.gpkg"
)


BLOCK_METADATA_CSV <- file.path(
  DATA_DIR,
  "block_metadata.csv"
)


ASSET_MANIFEST_CSV <- file.path(
  DATA_DIR,
  "remote_manifest.csv"
)


CATALOG_READY_FILE <- file.path(
  DATA_DIR,
  "CATALOG_READY.txt"
)


OUTPUT_DIR <- file.path(
  tempdir(),
  "FABDEM_WATERSHED_OUTPUTS"
)


TERRA_TEMP <- file.path(
  tempdir(),
  "FABDEM_WATERSHED_EXPLORER"
)


# ------------------------------------------------------------
# PARAMETROS INTERNOS
# No aparecen como controles de la interfaz.
# ------------------------------------------------------------

DEFAULT_SNAP_RADIUS_M <- 1500
MAX_CACHED_REVERSE_STRIPES <- 32L
MAX_CACHED_STREAM_STRIPES <- 8L

FAST_CACHE_BLOCKS <- character(0)

PRELOAD_FAST_CACHE <- FALSE

# BLOCK_004: una vez precargados los RDS raw, se intenta
# concatenarlos en un unico vector raw de ~2.8 GiB.
# Si R no puede reservarlo, cae automaticamente al cache
# preloaded por franjas sin detener la app.
USE_FULL_REVERSE_RAM <- FALSE

# Recorrido por lotes en lugar de profundidad/nivel.
USE_BATCH_TRACE <- TRUE

# Celdas receptoras procesadas por lote.
TRACE_BATCH_SIZE <- 500000L

# Callback de progreso cada N lotes. La UI ademas limita
# visualmente sus refrescos a ~0.5 s.
TRACE_PROGRESS_EVERY_BATCHES <- 1L

# El raster de salida ya se limita al bounding box de la cuenca.
# Se conserva explicito para documentar la arquitectura.
CROP_BASIN_RASTER <- TRUE

BASIN_WRITE_ROWS <- 256L
MAX_TRACE_LEVELS <- 300000L
MAX_BASIN_CELLS <- 500000000
MAP_SIMPLIFY_M <- 60

# Morfometria: numero maximo de valores usados solamente para
# graficos y percentiles secundarios. Las metricas principales
# min/max/mean/median se calculan sobre el raster completo.
MORPH_SAMPLE_MAX <- 250000L


# ------------------------------------------------------------
# CACHE REMOTA
# ------------------------------------------------------------

dir.create(
  RUNTIME_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


runtime_is_lfs_pointer <- function(path) {
  if (
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(path) ||
    !file.exists(path)
  ) {
    return(FALSE)
  }

  size <- suppressWarnings(
    as.numeric(file.info(path)$size)
  )

  if (!is.finite(size) || size <= 0 || size > 2048) {
    return(FALSE)
  }

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  prefix_raw <- readBin(
    con,
    what = "raw",
    n = min(512L, as.integer(size))
  )

  prefix <- tryCatch(
    rawToChar(prefix_raw),
    error = function(e) ""
  )

  startsWith(
    prefix,
    "version https://git-lfs.github.com/spec/v1"
  )
}


runtime_raw_base_url <- function() {
  base <- RUNTIME_BASE_URL

  pattern <- paste0(
    "^https://media\\.githubusercontent\\.com/media/",
    "([^/]+)/([^/]+)/(.*)$"
  )

  if (!grepl(pattern, base)) {
    return(NULL)
  }

  sub(
    pattern,
    "https://raw.githubusercontent.com/\\1/\\2/\\3",
    base
  )
}


runtime_cache_file <- function(path) {
  if (
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(path)
  ) {
    stop("Ruta Runtime vacia.")
  }

  root <- normalizePath(
    RUNTIME_ROOT,
    winslash = "/",
    mustWork = TRUE
  )

  candidate <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )

  prefix <- paste0(tolower(root), "/")

  if (!startsWith(tolower(candidate), prefix)) {
    stop(paste0("Ruta fuera del cache Runtime:\n", path))
  }

  if (
    file.exists(candidate) &&
    is.finite(file.info(candidate)$size) &&
    file.info(candidate)$size > 0 &&
    !runtime_is_lfs_pointer(candidate)
  ) {
    return(candidate)
  }

  # Elimina cualquier puntero LFS que hubiera quedado de un
  # intento anterior para no tratarlo como un recurso valido.
  if (file.exists(candidate)) {
    unlink(candidate, force = TRUE)
  }

  relative_path <- substring(candidate, nchar(root) + 2L)
  encoded_path <- utils::URLencode(relative_path)

  primary_url <- paste0(
    RUNTIME_BASE_URL,
    encoded_path
  )

  raw_base <- runtime_raw_base_url()

  urls <- primary_url

  if (
    length(raw_base) == 1L &&
    !is.na(raw_base) &&
    nzchar(raw_base)
  ) {
    raw_url <- paste0(
      raw_base,
      encoded_path
    )

    if (!identical(raw_url, primary_url)) {
      urls <- c(urls, raw_url)
    }
  }

  dir.create(
    dirname(candidate),
    recursive = TRUE,
    showWarnings = FALSE
  )

  partial <- paste0(candidate, ".part")
  unlink(partial, force = TRUE)

  errors <- character(0)

  for (remote_url in urls) {
    unlink(partial, force = TRUE)

    status <- tryCatch(
      utils::download.file(
        remote_url,
        partial,
        mode = "wb",
        quiet = TRUE
      ),
      error = function(e) {
        errors <<- c(
          errors,
          paste0(
            remote_url,
            " -> ",
            conditionMessage(e)
          )
        )
        NA_integer_
      }
    )

    valid_download <- (
      identical(status, 0L) &&
      file.exists(partial) &&
      is.finite(file.info(partial)$size) &&
      file.info(partial)$size > 0
    )

    if (!isTRUE(valid_download)) {
      if (!is.na(status)) {
        errors <- c(
          errors,
          paste0(remote_url, " -> descarga incompleta")
        )
      }
      next
    }

    # raw.githubusercontent devuelve el puntero de Git LFS para
    # binarios LFS. Eso NO es un archivo utilizable y nunca debe
    # guardarse en el cache. El endpoint media es el que entrega
    # el contenido LFS real.
    if (runtime_is_lfs_pointer(partial)) {
      errors <- c(
        errors,
        paste0(remote_url, " -> puntero Git LFS, no contenido")
      )
      next
    }

    if (!file.rename(partial, candidate)) {
      errors <- c(
        errors,
        paste0(remote_url, " -> no se pudo guardar en cache")
      )
      next
    }

    return(candidate)
  }

  unlink(partial, force = TRUE)

  stop(
    paste0(
      "No se pudo descargar el recurso Runtime:\n",
      relative_path,
      if (length(errors) > 0L) {
        paste0(
          "\n\nIntentos:\n",
          paste(errors, collapse = "\n")
        )
      } else {
        ""
      }
    )
  )
}


runtime_file_nonempty <- function(path) {
  if (
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(path)
  ) {
    return(FALSE)
  }

  root <- normalizePath(
    RUNTIME_ROOT,
    winslash = "/",
    mustWork = TRUE
  )

  candidate <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )

  is_runtime <- startsWith(
    tolower(candidate),
    paste0(tolower(root), "/")
  )

  ready <- if (is_runtime) {
    tryCatch(
      runtime_cache_file(candidate),
      error = function(e) NULL
    )
  } else {
    candidate
  }

  length(ready) == 1L &&
    !is.na(ready) &&
    file.exists(ready) &&
    is.finite(file.info(ready)$size) &&
    file.info(ready)$size > 0
}


dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  TERRA_TEMP,
  recursive = TRUE,
  showWarnings = FALSE
)


terra::terraOptions(
  tempdir = TERRA_TEMP,
  memfrac = 0.15,
  memmax = 4,
  progress = 0
)

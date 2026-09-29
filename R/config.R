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


# ------------------------------------------------------------
# CATALOGO DE ASSETS SIN GIT LFS
# ------------------------------------------------------------
#
# Los binarios publicados como assets de GitHub Releases tienen
# prioridad sobre el Runtime historico alojado en Git LFS.
# El manifiesto es texto liviano y vive en el repositorio Runtime.
# Mientras la migracion no haya sido ejecutada, la aplicacion
# conserva el origen LFS como fallback para no romper produccion.
# ------------------------------------------------------------

RUNTIME_RELEASE_MANIFEST_URL <- paste0(
  "https://raw.githubusercontent.com/",
  "JamilRamirez/FABDEM-Watershed-Runtime/main/",
  "runtime_release_manifest.csv"
)

RUNTIME_RELEASE_CACHE <- new.env(
  parent = emptyenv()
)

RUNTIME_RELEASE_CACHE$loaded <- FALSE
RUNTIME_RELEASE_CACHE$data <- NULL


runtime_release_catalog <- function() {

  if (isTRUE(RUNTIME_RELEASE_CACHE$loaded)) {
    return(RUNTIME_RELEASE_CACHE$data)
  }

  RUNTIME_RELEASE_CACHE$loaded <- TRUE

  tmp <- tempfile(
    pattern = "runtime_release_manifest_",
    fileext = ".csv"
  )

  on.exit(
    unlink(tmp, force = TRUE),
    add = TRUE
  )

  status <- tryCatch(
    utils::download.file(
      RUNTIME_RELEASE_MANIFEST_URL,
      tmp,
      mode = "wb",
      quiet = TRUE
    ),
    error = function(e) NA_integer_
  )

  if (
    !identical(status, 0L) ||
    !file.exists(tmp) ||
    !is.finite(file.info(tmp)$size) ||
    file.info(tmp)$size <= 0
  ) {
    RUNTIME_RELEASE_CACHE$data <- NULL
    return(NULL)
  }

  out <- tryCatch(
    utils::read.csv(
      tmp,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )

  required <- c(
    "RELATIVE_PATH",
    "REMOTE_URL",
    "SIZE_BYTES"
  )

  if (
    is.null(out) ||
    nrow(out) < 1L ||
    !all(required %in% names(out))
  ) {
    RUNTIME_RELEASE_CACHE$data <- NULL
    return(NULL)
  }

  out$RELATIVE_PATH <- gsub(
    "\\\\",
    "/",
    as.character(out$RELATIVE_PATH)
  )

  out$REMOTE_URL <- trimws(
    as.character(out$REMOTE_URL)
  )

  out$SIZE_BYTES <- suppressWarnings(
    as.numeric(out$SIZE_BYTES)
  )

  out <- out[
    nzchar(out$RELATIVE_PATH) &
      nzchar(out$REMOTE_URL),
    ,
    drop = FALSE
  ]

  if (nrow(out) < 1L) {
    RUNTIME_RELEASE_CACHE$data <- NULL
    return(NULL)
  }

  RUNTIME_RELEASE_CACHE$data <- out
  out
}


runtime_release_entry <- function(relative_path) {

  catalog <- runtime_release_catalog()

  if (
    is.null(catalog) ||
    nrow(catalog) < 1L
  ) {
    return(NULL)
  }

  rel <- gsub(
    "\\\\",
    "/",
    as.character(relative_path)
  )

  hit <- match(
    rel,
    catalog$RELATIVE_PATH
  )

  if (is.na(hit)) {
    return(NULL)
  }

  catalog[
    hit,
    ,
    drop = FALSE
  ]
}


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

  relative_path <- substring(
    candidate,
    nchar(root) + 2L
  )

  relative_path <- gsub(
    "\\\\",
    "/",
    relative_path
  )

  release_entry <- runtime_release_entry(
    relative_path
  )

  expected_size <- if (
    !is.null(release_entry) &&
    nrow(release_entry) == 1L
  ) {
    suppressWarnings(
      as.numeric(release_entry$SIZE_BYTES[1])
    )
  } else {
    NA_real_
  }

  cached_size <- if (file.exists(candidate)) {
    suppressWarnings(
      as.numeric(file.info(candidate)$size)
    )
  } else {
    NA_real_
  }

  cached_size_ok <- (
    is.finite(cached_size) &&
    cached_size > 0 &&
    (
      !is.finite(expected_size) ||
      expected_size <= 0 ||
      identical(cached_size, expected_size)
    )
  )

  if (
    file.exists(candidate) &&
    cached_size_ok &&
    !runtime_is_lfs_pointer(candidate)
  ) {
    return(candidate)
  }

  # Elimina cualquier puntero LFS que hubiera quedado de un
  # intento anterior para no tratarlo como un recurso valido.
  if (file.exists(candidate)) {
    unlink(candidate, force = TRUE)
  }

  encoded_path <- utils::URLencode(relative_path)

  primary_url <- paste0(
    RUNTIME_BASE_URL,
    encoded_path
  )

  raw_base <- runtime_raw_base_url()

  release_url <- if (
    !is.null(release_entry) &&
    nrow(release_entry) == 1L
  ) {
    trimws(
      as.character(
        release_entry$REMOTE_URL[1]
      )
    )
  } else {
    ""
  }

  urls <- if (nzchar(release_url)) {
    # Si el asset ya fue migrado, Release es la unica fuente.
    # No se vuelve a consumir Git LFS como fallback.
    release_url
  } else {
    primary_url
  }

  if (
    !nzchar(release_url) &&
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

    partial_size <- if (file.exists(partial)) {
      suppressWarnings(
        as.numeric(file.info(partial)$size)
      )
    } else {
      NA_real_
    }

    valid_download <- (
      identical(status, 0L) &&
      file.exists(partial) &&
      is.finite(partial_size) &&
      partial_size > 0 &&
      (
        !is.finite(expected_size) ||
        expected_size <= 0 ||
        identical(partial_size, expected_size)
      )
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

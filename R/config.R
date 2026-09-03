# ============================================================
# R/config.R
#
# FABDEM Watershed Explorer
# CONFIGURACION LOCAL
# v8: configuracion integrada DEM + capas tematicas
# ============================================================
#
# Durante desarrollo TODO funciona con dos carpetas hermanas:
#
#   .../FABDEM_Watershed_Explorer/
#   .../FABDEM_Watershed_Runtime/
#
# Delimitacion usa Runtime/core.
# Morfometria usa directamente las teselas originales en
# Runtime/dem, sin crear DEM por bloque.
# ============================================================


APP_ROOT <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)


PROJECT_ROOT <- dirname(
  APP_ROOT
)


RUNTIME_ROOT <- file.path(
  PROJECT_ROOT,
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


DATA_DIR <- file.path(
  APP_ROOT,
  "data"
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
  "metadata_normalizada.xlsx"
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
  APP_ROOT,
  "outputs"
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

FAST_CACHE_BLOCKS <- c(
  "BLOCK_004"
)

PRELOAD_FAST_CACHE <- TRUE

# BLOCK_004: una vez precargados los RDS raw, se intenta
# concatenarlos en un unico vector raw de ~2.8 GiB.
# Si R no puede reservarlo, cae automaticamente al cache
# preloaded por franjas sin detener la app.
USE_FULL_REVERSE_RAM <- TRUE

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
# DIRECTORIOS LOCALES
# ------------------------------------------------------------

if (!dir.exists(RUNTIME_ROOT)) {
  stop(
    paste0(
      "No existe la carpeta runtime esperada:\n",
      RUNTIME_ROOT,
      "\n\nLas carpetas FABDEM_Watershed_Explorer y ",
      "FABDEM_Watershed_Runtime deben estar al mismo nivel.\n",
      "Runtime: https://github.com/JamilRamirez/",
      "FABDEM-Watershed-Runtime"
    )
  )
}


if (!dir.exists(CORE_DIR)) {
  stop(
    paste0(
      "No existe:\n",
      CORE_DIR
    )
  )
}


# No se detiene la app si falta DEM_DIR: Delimitacion
# debe seguir funcionando. Morfometria valida esta ruta al usarla.


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

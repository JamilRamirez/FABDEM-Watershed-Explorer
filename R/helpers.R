# ============================================================
# R/helpers.R
#
# FABDEM Watershed Explorer
# HELPERS LOCALES DEL RUNTIME
# ============================================================
#
# Este archivo contiene:
# - catalogo local
# - lookup BLOCK_ID por clic
# - grilla virtual por metadata
# - lectura local de stream_stripes
# - lectura local de reverse_stripes
# - snap a cauce
# - recorrido aguas arriba
# - materializacion de la cuenca
#
# NO contiene UI Shiny.
# NO usa DEM.
# NO usa STREAM_MASK.tif completo.
# NO usa REVERSE_D8.vrt.
# ============================================================


# ============================================================
# 1. HELPERS GENERALES
# ============================================================

utm_epsg_point <- function(
    lon,
    lat
) {

  zone <- floor(
    (
      lon +
        180
    ) /
      6
  ) +
    1


  if (lat >= 0) {
    32600 +
      zone
  } else {
    32700 +
      zone
  }
}


last_data_column <- function(x) {

  if (is.data.frame(x)) {
    x[[ncol(x)]]
  } else {
    x
  }
}


file_nonempty <- function(path) {

  if (
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(path) ||
    !file.exists(path)
  ) {
    return(
      FALSE
    )
  }


  info <- file.info(
    path
  )


  isTRUE(
    is.finite(
      info$size
    ) &&
      info$size > 0
  )
}


safe_normalize <- function(
    path,
    must_work = FALSE
) {

  normalizePath(
    path,
    winslash = "/",
    mustWork = must_work
  )
}


runtime_asset_path <- function(relative_path) {

  if (
    length(relative_path) != 1L ||
    is.na(relative_path) ||
    !nzchar(relative_path)
  ) {
    stop(
      "RELATIVE_PATH vacio en el manifest."
    )
  }


  relative_path <- gsub(
    "\\\\",
    "/",
    relative_path
  )


  if (
    grepl(
      "^[A-Za-z]:/",
      relative_path
    ) ||
    startsWith(
      relative_path,
      "/"
    )
  ) {
    stop(
      paste0(
        "El manifest debe contener rutas relativas, no absolutas:\n",
        relative_path
      )
    )
  }


  candidate <- file.path(
    RUNTIME_ROOT,
    relative_path
  )


  candidate_norm <- safe_normalize(
    candidate,
    must_work = FALSE
  )


  root_norm <- safe_normalize(
    RUNTIME_ROOT,
    must_work = TRUE
  )


  root_prefix <- paste0(
    tolower(
      root_norm
    ),
    "/"
  )


  if (!startsWith(
    tolower(
      candidate_norm
    ),
    root_prefix
  )) {
    stop(
      paste0(
        "Ruta fuera de FABDEM_Watershed_Runtime:\n",
        relative_path
      )
    )
  }


  candidate_norm
}


# ============================================================
# 2. CARGAR CATALOGO LOCAL
# ============================================================

required_catalog_files <- c(
  BLOCK_LOOKUP_GPKG,
  BLOCK_METADATA_CSV,
  ASSET_MANIFEST_CSV
)


missing_catalog_files <- required_catalog_files[
  !vapply(
    required_catalog_files,
    file_nonempty,
    logical(1)
  )
]


if (length(missing_catalog_files) > 0L) {
  stop(
    paste0(
      "Faltan archivos del catalogo local:\n",
      paste(
        missing_catalog_files,
        collapse = "\n"
      )
    )
  )
}


lookup_layers <- sf::st_layers(
  BLOCK_LOOKUP_GPKG
)$name


if (!"blocks" %in% lookup_layers) {
  stop(
    "block_lookup.gpkg no contiene la capa 'blocks'."
  )
}


block_lookup <- sf::st_read(
  BLOCK_LOOKUP_GPKG,
  layer = "blocks",
  quiet = TRUE
)


block_lookup <- sf::st_make_valid(
  block_lookup
)


block_lookup <- block_lookup[
  !sf::st_is_empty(
    block_lookup
  ),
  ,
  drop = FALSE
]


if (
  !"BLOCK_ID" %in%
    names(
      block_lookup
    )
) {
  stop(
    "block_lookup.gpkg no contiene BLOCK_ID."
  )
}


if (is.na(
  sf::st_crs(
    block_lookup
  )
)) {
  stop(
    "block_lookup.gpkg no tiene CRS."
  )
}


block_metadata <- read.csv(
  BLOCK_METADATA_CSV,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


asset_manifest <- read.csv(
  ASSET_MANIFEST_CSV,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


required_metadata_fields <- c(
  "BLOCK_ID",
  "NROWS",
  "NCOLS",
  "RES_X",
  "RES_Y",
  "XMIN",
  "XMAX",
  "YMIN",
  "YMAX",
  "CRS",
  "STRIPE_ROWS",
  "N_STRIPES",
  "STREAM_THRESHOLD_CELLS",
  "STREAM_THRESHOLD_KM2",
  "INDEX_ALGORITHM"
)


missing_metadata_fields <- required_metadata_fields[
  !required_metadata_fields %in%
    names(
      block_metadata
    )
]


if (length(missing_metadata_fields) > 0L) {
  stop(
    paste0(
      "block_metadata.csv no contiene: ",
      paste(
        missing_metadata_fields,
        collapse = ", "
      )
    )
  )
}


required_manifest_fields <- c(
  "BLOCK_ID",
  "ASSET_TYPE",
  "STRIPE_ID",
  "ROW_START",
  "ROW_END",
  "NROWS",
  "NCOLS",
  "RELATIVE_PATH"
)


missing_manifest_fields <- required_manifest_fields[
  !required_manifest_fields %in%
    names(
      asset_manifest
    )
]


if (length(missing_manifest_fields) > 0L) {
  stop(
    paste0(
      "remote_manifest.csv no contiene: ",
      paste(
        missing_manifest_fields,
        collapse = ", "
      )
    )
  )
}


block_lookup[["BLOCK_ID"]] <- as.character(
  block_lookup[["BLOCK_ID"]]
)


block_metadata[["BLOCK_ID"]] <- as.character(
  block_metadata[["BLOCK_ID"]]
)


asset_manifest[["BLOCK_ID"]] <- as.character(
  asset_manifest[["BLOCK_ID"]]
)


asset_manifest[["ASSET_TYPE"]] <- tolower(
  trimws(
    as.character(
      asset_manifest[["ASSET_TYPE"]]
    )
  )
)


catalog_block_ids <- sort(
  unique(
    block_lookup[["BLOCK_ID"]]
  )
)


metadata_block_ids <- sort(
  unique(
    block_metadata[["BLOCK_ID"]]
  )
)


if (!identical(
  catalog_block_ids,
  metadata_block_ids
)) {
  stop(
    "BLOCK_ID de block_lookup y block_metadata no coinciden."
  )
}


reverse_block_ids <- sort(
  unique(
    asset_manifest[["BLOCK_ID"]][
      asset_manifest[["ASSET_TYPE"]] ==
        "reverse"
    ]
  )
)


stream_block_ids <- sort(
  unique(
    asset_manifest[["BLOCK_ID"]][
      asset_manifest[["ASSET_TYPE"]] ==
        "stream"
    ]
  )
)


if (
  !identical(
    catalog_block_ids,
    reverse_block_ids
  ) ||
  !identical(
    catalog_block_ids,
    stream_block_ids
  )
) {
  stop(
    "El manifest no contiene reverse y stream para todos los bloques."
  )
}


# ============================================================
# 3. LOOKUP DEL BLOQUE
# ============================================================

find_block_for_click <- function(
    lon,
    lat
) {

  p <- sf::st_sfc(
    sf::st_point(
      c(
        lon,
        lat
      )
    ),
    crs = 4326
  )


  p_lookup <- sf::st_transform(
    p,
    sf::st_crs(
      block_lookup
    )
  )


  hits <- sf::st_intersects(
    p_lookup,
    block_lookup
  )[[1]]


  if (length(hits) == 0L) {
    return(
      NULL
    )
  }


  if (length(hits) == 1L) {
    return(
      block_lookup[
        hits,
        ,
        drop = FALSE
      ]
    )
  }


  candidates <- block_lookup[
    hits,
    ,
    drop = FALSE
  ]


  areas <- as.numeric(
    sf::st_area(
      sf::st_transform(
        candidates,
        6933
      )
    )
  )


  candidates[
    which.min(
      areas
    ),
    ,
    drop = FALSE
  ]
}


# ============================================================
# 4. METADATA Y ASSETS POR BLOQUE
# ============================================================

get_block_metadata <- function(block_id) {

  x <- block_metadata[
    block_metadata[["BLOCK_ID"]] ==
      block_id,
    ,
    drop = FALSE
  ]


  if (nrow(x) != 1L) {
    stop(
      paste0(
        block_id,
        ": block_metadata debe contener exactamente una fila."
      )
    )
  }


  x
}


get_block_assets <- function(
    block_id,
    asset_type
) {

  x <- asset_manifest[
    asset_manifest[["BLOCK_ID"]] ==
      block_id &
      asset_manifest[["ASSET_TYPE"]] ==
        asset_type,
    ,
    drop = FALSE
  ]


  if (nrow(x) == 0L) {
    stop(
      paste0(
        block_id,
        ": no hay assets tipo ",
        asset_type,
        "."
      )
    )
  }


  if (
    asset_type %in%
      c(
        "reverse",
        "stream"
      )
  ) {

    x[["STRIPE_ID"]] <- as.integer(
      x[["STRIPE_ID"]]
    )


    x <- x[
      order(
        x[["STRIPE_ID"]]
      ),
      ,
      drop = FALSE
    ]
  }


  x[["LOCAL_PATH"]] <- vapply(
    as.character(
      x[["RELATIVE_PATH"]]
    ),
    runtime_asset_path,
    character(1)
  )


  x
}


create_grid_template <- function(meta) {

  nr <- as.integer(
    meta[["NROWS"]][1]
  )

  nc <- as.integer(
    meta[["NCOLS"]][1]
  )


  if (
    !is.finite(nr) ||
    !is.finite(nc) ||
    nr < 1L ||
    nc < 1L
  ) {
    stop(
      paste0(
        meta[["BLOCK_ID"]][1],
        ": dimensiones de grilla invalidas."
      )
    )
  }


  terra::rast(
    nrows = nr,
    ncols = nc,
    xmin = as.numeric(
      meta[["XMIN"]][1]
    ),
    xmax = as.numeric(
      meta[["XMAX"]][1]
    ),
    ymin = as.numeric(
      meta[["YMIN"]][1]
    ),
    ymax = as.numeric(
      meta[["YMAX"]][1]
    ),
    crs = as.character(
      meta[["CRS"]][1]
    )
  )
}


validate_stripe_sequence <- function(
    rows,
    expected_n,
    block_id,
    label
) {

  ids <- as.integer(
    rows[["STRIPE_ID"]]
  )


  expected <- seq_len(
    expected_n
  )


  if (!identical(
    ids,
    expected
  )) {
    stop(
      paste0(
        block_id,
        ": secuencia ",
        label,
        " incompleta o desordenada."
      )
    )
  }


  missing_files <- rows[["LOCAL_PATH"]][
    !vapply(
      rows[["LOCAL_PATH"]],
      file_nonempty,
      logical(1)
    )
  ]


  if (length(missing_files) > 0L) {
    stop(
      paste0(
        block_id,
        ": faltan archivos ",
        label,
        " locales. Primero:\n",
        missing_files[1]
      )
    )
  }


  invisible(TRUE)
}


# ============================================================
# 5. CACHE STREAM STRIPES
# ============================================================

new_stream_cache <- function(
    block_id,
    stream_rows
) {

  e <- new.env(
    parent = emptyenv()
  )


  e$block_id <- block_id

  e$rows <- stream_rows

  e$rasters <- new.env(
    hash = TRUE,
    parent = emptyenv()
  )

  e$lru <- character(
    0
  )


  e
}


stream_cache_touch <- function(
    cache,
    key
) {

  cache$lru <- c(
    cache$lru[
      cache$lru !=
        key
    ],
    key
  )


  while (
    length(
      cache$lru
    ) >
      MAX_CACHED_STREAM_STRIPES
  ) {

    old_key <- cache$lru[1]


    if (exists(
      old_key,
      envir = cache$rasters,
      inherits = FALSE
    )) {
      rm(
        list = old_key,
        envir = cache$rasters
      )
    }


    cache$lru <- cache$lru[
      -1
    ]
  }


  invisible(NULL)
}


load_stream_stripe <- function(
    cache,
    stripe_id
) {

  key <- as.character(
    stripe_id
  )


  if (exists(
    key,
    envir = cache$rasters,
    inherits = FALSE
  )) {

    stream_cache_touch(
      cache,
      key
    )


    return(
      get(
        key,
        envir = cache$rasters,
        inherits = FALSE
      )
    )
  }


  if (
    stripe_id < 1L ||
    stripe_id >
      nrow(
        cache$rows
      )
  ) {
    stop(
      paste0(
        "STREAM stripe fuera de rango: ",
        stripe_id
      )
    )
  }


  path <- cache$rows[["LOCAL_PATH"]][
    stripe_id
  ]


  if (!file_nonempty(path)) {
    stop(
      paste0(
        "No existe stream stripe:\n",
        path
      )
    )
  }


  r <- terra::rast(
    path
  )


  assign(
    key,
    r,
    envir = cache$rasters
  )


  stream_cache_touch(
    cache,
    key
  )


  r
}


# ============================================================
# 6. CACHE REVERSE STRIPES
# ============================================================

new_reverse_cache <- function(
    block_id,
    meta,
    reverse_rows
) {

  compatible_index <- identical(
    as.character(
      meta[["INDEX_ALGORITHM"]][1]
    ),
    "1.3-reverse-d8-stream-mask"
  )


  if (!isTRUE(
    compatible_index
  )) {
    stop(
      paste0(
        block_id,
        ": INDEX_ALGORITHM no compatible: ",
        as.character(
          meta[["INDEX_ALGORITHM"]][1]
        )
      )
    )
  }


  e <- new.env(
    parent = emptyenv()
  )


  e$block_id <- block_id


  e$metadata <- list(
    nrows = as.double(
      meta[["NROWS"]][1]
    ),
    ncols = as.double(
      meta[["NCOLS"]][1]
    ),
    stripe_rows = as.double(
      meta[["STRIPE_ROWS"]][1]
    ),
    n_stripes = as.integer(
      meta[["N_STRIPES"]][1]
    )
  )


  e$stripe_files <- as.character(
    reverse_rows[["LOCAL_PATH"]]
  )


  e$values <- new.env(
    hash = TRUE,
    parent = emptyenv()
  )


  e$lru <- character(
    0
  )


  e$fast_mode <- FALSE
  e$fast_preloaded <- FALSE
  e$edge_safe <- FALSE
  e$fast_objects <- NULL
  e$fast_files <- NULL
  e$fast_manifest <- NULL
  e$full_raw <- NULL
  e$full_raw_ready <- FALSE


  if (!block_id %in% FAST_CACHE_BLOCKS) {
    return(
      e
    )
  }


  fast_dir <- file.path(
    CORE_DIR,
    block_id,
    "fast_cache"
  )


  fast_raw_dir <- file.path(
    fast_dir,
    "reverse_raw"
  )


  fast_ready_file <- file.path(
    fast_dir,
    "FAST_CACHE_READY.txt"
  )


  fast_manifest_file <- file.path(
    fast_dir,
    "fast_cache_manifest.csv"
  )


  fast_metadata_file <- file.path(
    fast_dir,
    "fast_cache_metadata.rds"
  )


  if (
    !file_nonempty(
      fast_ready_file
    ) ||
    !file_nonempty(
      fast_manifest_file
    ) ||
    !file_nonempty(
      fast_metadata_file
    )
  ) {

    warning(
      paste0(
        block_id,
        ": fast cache no encontrado; se usara TIFF normal."
      )
    )


    return(
      e
    )
  }


  fast_manifest <- read.csv(
    fast_manifest_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )


  fast_metadata <- readRDS(
    fast_metadata_file
  )


  required_fast_fields <- c(
    "STRIPE_ID",
    "SOURCE_SIZE",
    "FAST_FILE",
    "NROWS",
    "NCOLS",
    "NVALUES"
  )


  missing_fast_fields <- required_fast_fields[
    !required_fast_fields %in%
      names(
        fast_manifest
      )
  ]


  if (length(
    missing_fast_fields
  ) > 0L) {
    stop(
      paste0(
        block_id,
        ": fast_cache_manifest.csv incompleto: ",
        paste(
          missing_fast_fields,
          collapse = ", "
        )
      )
    )
  }


  fast_manifest <- fast_manifest[
    order(
      as.integer(
        fast_manifest[["STRIPE_ID"]]
      )
    ),
    ,
    drop = FALSE
  ]


  fast_ids <- unname(
    as.integer(
      fast_manifest[["STRIPE_ID"]]
    )
  )


  expected_ids <- seq_len(
    e$metadata$n_stripes
  )


  if (!identical(
    fast_ids,
    expected_ids
  )) {
    stop(
      paste0(
        block_id,
        ": fast cache incompleto."
      )
    )
  }


  if (!isTRUE(
    fast_metadata[["edge_safe"]]
  )) {
    stop(
      paste0(
        block_id,
        ": fast cache sin validacion de bordes."
      )
    )
  }


  fast_files <- file.path(
    fast_raw_dir,
    as.character(
      fast_manifest[["FAST_FILE"]]
    )
  )


  missing_fast <- fast_files[
    !vapply(
      fast_files,
      file_nonempty,
      logical(1)
    )
  ]


  if (length(
    missing_fast
  ) > 0L) {
    stop(
      paste0(
        block_id,
        ": falta fast cache: ",
        missing_fast[1]
      )
    )
  }


  source_sizes_now <- as.numeric(
    file.info(
      e$stripe_files
    )$size
  )


  source_sizes_cache <- as.numeric(
    fast_manifest[["SOURCE_SIZE"]]
  )


  if (
    length(
      source_sizes_now
    ) !=
      length(
        source_sizes_cache
      ) ||
    any(
      source_sizes_now !=
        source_sizes_cache
    )
  ) {
    stop(
      paste0(
        block_id,
        ": reverse_stripes cambiaron despues del 08. ",
        "Regenera el fast cache."
      )
    )
  }


  e$fast_mode <- TRUE
  e$edge_safe <- TRUE
  e$fast_files <- fast_files
  e$fast_manifest <- fast_manifest


  if (isTRUE(
    PRELOAD_FAST_CACHE
  )) {

    cat(
      "\n",
      block_id,
      ": precargando fast cache en RAM...\n",
      sep = ""
    )


    fast_objects <- vector(
      "list",
      length(
        fast_files
      )
    )


    for (i in seq_along(
      fast_files
    )) {

      obj <- readRDS(
        fast_files[[i]]
      )


      expected_values <- as.double(
        fast_manifest[["NVALUES"]][i]
      )


      if (
        !is.list(
          obj
        ) ||
        !is.raw(
          obj[["values"]]
        ) ||
        length(
          obj[["values"]]
        ) !=
          expected_values
      ) {
        stop(
          paste0(
            block_id,
            ": fast cache corrupto en stripe ",
            i,
            "."
          )
        )
      }


      fast_objects[[i]] <- obj


      if (
        i %%
          20L ==
          0L ||
        i ==
          length(
            fast_files
          )
      ) {
        cat(
          "\rFast cache ",
          block_id,
          ": ",
          i,
          "/",
          length(
            fast_files
          ),
          sep = ""
        )
      }
    }


    cat(
      "\n"
    )


    e$fast_objects <- fast_objects
    e$fast_preloaded <- TRUE


    if (isTRUE(
      USE_FULL_REVERSE_RAM
    )) {

      total_cells <- as.double(
        e$metadata$nrows
      ) *
        as.double(
          e$metadata$ncols
        )


      cat(
        block_id,
        ": construyendo reverse_full en RAM (",
        sprintf(
          "%.2f",
          total_cells /
            1024^3
        ),
        " GiB raw)...\n",
        sep = ""
      )


      full_raw <- tryCatch(
        vector(
          mode = "raw",
          length = total_cells
        ),
        error = function(e) e
      )


      if (inherits(
        full_raw,
        "error"
      )) {

        warning(
          paste0(
            block_id,
            ": no se pudo reservar reverse_full; ",
            "se mantendra el fast cache por franjas. ",
            conditionMessage(
              full_raw
            )
          )
        )

      } else {

        offset <- 1


        for (i in seq_along(
          fast_objects
        )) {

          values_i <- fast_objects[[i]][["values"]]

          n_i <- length(
            values_i
          )


          end_i <- offset +
            n_i -
            1


          # Cada stripe tiene ~12 millones de celdas. La
          # secuencia temporal evita mantener un indice global.
          index_i <- seq.int(
            from = offset,
            to = end_i
          )


          full_raw[
            index_i
          ] <- values_i


          rm(
            index_i,
            values_i
          )


          offset <- end_i +
            1


          if (
            i %%
              20L ==
              0L ||
            i ==
              length(
                fast_objects
              )
          ) {
            cat(
              "\rreverse_full ",
              block_id,
              ": ",
              i,
              "/",
              length(
                fast_objects
              ),
              sep = ""
            )
          }
        }


        cat(
          "\n"
        )


        if (
          offset -
            1 !=
              total_cells
        ) {
          stop(
            paste0(
              block_id,
              ": reverse_full quedo con longitud inconsistente."
            )
          )
        }


        e$full_raw <- full_raw
        e$full_raw_ready <- TRUE


        # El vector unico sustituye a la lista de stripes raw y
        # recupera ~2.8 GiB antes de iniciar el trace.
        e$fast_objects <- NULL

        rm(
          fast_objects,
          full_raw
        )

        gc()
      }
    }
  }


  e
}


cache_touch <- function(
    cache,
    key
) {

  if (isTRUE(
    cache$fast_mode
  )) {
    return(
      invisible(
        NULL
      )
    )
  }


  cache$lru <- c(
    cache$lru[
      cache$lru !=
        key
    ],
    key
  )


  while (
    length(
      cache$lru
    ) >
      MAX_CACHED_REVERSE_STRIPES
  ) {

    old_key <- cache$lru[1]


    if (exists(
      old_key,
      envir = cache$values,
      inherits = FALSE
    )) {

      rm(
        list = old_key,
        envir = cache$values
      )
    }


    cache$lru <- cache$lru[
      -1
    ]
  }


  invisible(
    NULL
  )
}


load_reverse_stripe <- function(
    cache,
    stripe_id
) {

  stripe_id <- as.integer(
    stripe_id
  )


  if (
    stripe_id <
      1L ||
    stripe_id >
      length(
        cache$stripe_files
      )
  ) {
    stop(
      paste0(
        "Stripe ID fuera de rango: ",
        stripe_id
      )
    )
  }


  if (
    isTRUE(
      cache$fast_mode
    ) &&
    isTRUE(
      cache$fast_preloaded
    )
  ) {
    return(
      cache$fast_objects[[
        stripe_id
      ]]
    )
  }


  key <- as.character(
    stripe_id
  )


  if (exists(
    key,
    envir = cache$values,
    inherits = FALSE
  )) {

    cache_touch(
      cache,
      key
    )


    return(
      get(
        key,
        envir = cache$values,
        inherits = FALSE
      )
    )
  }


  if (isTRUE(
    cache$fast_mode
  )) {

    obj <- readRDS(
      cache$fast_files[[
        stripe_id
      ]]
    )


    if (
      !is.list(
        obj
      ) ||
      !is.raw(
        obj[["values"]]
      )
    ) {
      stop(
        paste0(
          cache$block_id,
          ": fast cache invalido en stripe ",
          stripe_id,
          "."
        )
      )
    }


    assign(
      key,
      obj,
      envir = cache$values
    )


    return(
      obj
    )
  }


  r <- terra::rast(
    cache$stripe_files[
      stripe_id
    ]
  )


  v <- terra::values(
    r,
    mat = FALSE
  )


  v[
    is.na(
      v
    )
  ] <- 0


  v <- as.raw(
    as.integer(
      v
    )
  )


  obj <- list(
    values = v,
    nrows = terra::nrow(
      r
    ),
    ncols = terra::ncol(
      r
    )
  )


  assign(
    key,
    obj,
    envir = cache$values
  )


  cache_touch(
    cache,
    key
  )


  obj
}


get_reverse_values <- function(
    cache,
    cells
) {

  if (length(
    cells
  ) == 0L) {
    return(
      integer(
        0
      )
    )
  }


  if (
    isTRUE(
      cache$full_raw_ready
    ) &&
    !is.null(
      cache$full_raw
    )
  ) {

    return(
      as.integer(
        cache$full_raw[
          cells
        ]
      )
    )
  }


  nc <- as.double(
    cache$metadata$ncols
  )


  stripe_rows <- as.double(
    cache$metadata$stripe_rows
  )


  # En franjas horizontales regulares, el ID de stripe puede
  # obtenerse directamente desde el cell ID sin calcular
  # primero fila y columna globales.
  stripe_cells <- stripe_rows *
    nc


  stripe_ids <- floor(
    (
      cells -
        1
    ) /
      stripe_cells
  ) +
    1


  local_cells <- (
    (
      cells -
        1
    ) %%
      stripe_cells
  ) +
    1


  out <- integer(
    length(
      cells
    )
  )


  unique_stripes <- unique(
    stripe_ids
  )


  k <- 1L


  while (
    k <=
      length(
        unique_stripes
      )
  ) {

    sid <- unique_stripes[k]


    idx <- which(
      stripe_ids ==
        sid
    )


    obj <- load_reverse_stripe(
      cache,
      sid
    )


    out[
      idx
    ] <- as.integer(
      obj$values[
        local_cells[
          idx
        ]
      ]
    )


    k <- k +
      1L
  }


  out
}


# ============================================================
# RECORRIDO AGUAS ARRIBA
# ============================================================
#
# No necesita bitmap global visited.
#
# En un D8 valido, cada celda tiene UN SOLO receptor aguas
# abajo. Por lo tanto una celda aguas arriba no puede ser padre
# de dos ramas distintas del recorrido inverso.
#
# unique() se mantiene como proteccion local.
# ============================================================

trace_upstream_safe <- function(
    cache,
    outlet_cell,
    progress_fun = NULL
) {

  nc <- as.double(
    cache$metadata$ncols
  )


  nr <- as.double(
    cache$metadata$nrows
  )


  frontier <- as.double(
    outlet_cell
  )


  chunks <- list(
    frontier
  )


  n_cells <- 1


  level <- 0L


  repeat {

    level <- level +
      1L


    if (
      level >
        MAX_TRACE_LEVELS
    ) {

      stop(
        "Se alcanzo MAX_TRACE_LEVELS. Posible ciclo o indice corrupto."
      )
    }


    if (length(frontier) == 0) {

      break
    }


    values_reverse <- get_reverse_values(
      cache,
      frontier
    )


    rows <- floor(
      (
        frontier -
          1
      ) /
        nc
    ) +
      1


    cols <- (
      (
        frontier -
          1
      ) %%
        nc
    ) +
      1


    parents <- numeric(
      0
    )


    # NW
    use <- (
      bitwAnd(
        values_reverse,
        1L
      ) != 0L &
        rows > 1 &
        cols > 1
    )


    if (any(use)) {

      parents <- c(
        parents,
        frontier[
          use
        ] -
          nc -
          1
      )
    }


    # N
    use <- (
      bitwAnd(
        values_reverse,
        2L
      ) != 0L &
        rows > 1
    )


    if (any(use)) {

      parents <- c(
        parents,
        frontier[
          use
        ] -
          nc
      )
    }


    # NE
    use <- (
      bitwAnd(
        values_reverse,
        4L
      ) != 0L &
        rows > 1 &
        cols < nc
    )


    if (any(use)) {

      parents <- c(
        parents,
        frontier[
          use
        ] -
          nc +
          1
      )
    }


    # W
    use <- (
      bitwAnd(
        values_reverse,
        8L
      ) != 0L &
        cols > 1
    )


    if (any(use)) {

      parents <- c(
        parents,
        frontier[
          use
        ] -
          1
      )
    }


    # E
    use <- (
      bitwAnd(
        values_reverse,
        16L
      ) != 0L &
        cols < nc
    )


    if (any(use)) {

      parents <- c(
        parents,
        frontier[
          use
        ] +
          1
      )
    }


    # SW
    use <- (
      bitwAnd(
        values_reverse,
        32L
      ) != 0L &
        rows < nr &
        cols > 1
    )


    if (any(use)) {

      parents <- c(
        parents,
        frontier[
          use
        ] +
          nc -
          1
      )
    }


    # S
    use <- (
      bitwAnd(
        values_reverse,
        64L
      ) != 0L &
        rows < nr
    )


    if (any(use)) {

      parents <- c(
        parents,
        frontier[
          use
        ] +
          nc
      )
    }


    # SE
    use <- (
      bitwAnd(
        values_reverse,
        128L
      ) != 0L &
        rows < nr &
        cols < nc
    )


    if (any(use)) {

      parents <- c(
        parents,
        frontier[
          use
        ] +
          nc +
          1
      )
    }


    if (length(parents) == 0) {

      break
    }


    parents <- unique(
      parents
    )


    n_cells <- n_cells +
      length(
        parents
      )


    if (
      n_cells >
        MAX_BASIN_CELLS
    ) {

      stop(
        paste0(
          "La cuenca supera MAX_BASIN_CELLS = ",
          format(
            MAX_BASIN_CELLS,
            big.mark = ","
          )
        )
      )
    }


    chunks[[length(chunks) + 1L]] <- parents


    frontier <- parents


    if (
      !is.null(
        progress_fun
      ) &&
      level %%
        250L ==
        0L
    ) {

      progress_fun(
        level,
        n_cells,
        length(
          frontier
        )
      )
    }
  }


  cells <- unlist(
    chunks,
    use.names = FALSE
  )


  list(
    cells = cells,
    n_cells = length(
      cells
    ),
    levels = level
  )
}


# ============================================================
# RECORRIDO RAPIDO PURO R
# ============================================================

trace_upstream_fast <- function(
    cache,
    outlet_cell,
    progress_fun = NULL
) {

  if (!isTRUE(
    cache$edge_safe
  )) {
    stop(
      "trace_upstream_fast requiere edge_safe=TRUE."
    )
  }


  nc <- as.double(
    cache$metadata$ncols
  )


  frontier <- as.double(
    outlet_cell
  )


  chunks <- list(
    frontier
  )


  n_cells <- 1
  level <- 0L


  offsets <- c(
    -nc - 1,
    -nc,
    -nc + 1,
    -1,
    1,
    nc - 1,
    nc,
    nc + 1
  )


  bits <- c(
    1L,
    2L,
    4L,
    8L,
    16L,
    32L,
    64L,
    128L
  )


  repeat {

    level <- level +
      1L


    if (
      level >
        MAX_TRACE_LEVELS
    ) {
      stop(
        "Se alcanzo MAX_TRACE_LEVELS. Posible ciclo o indice corrupto."
      )
    }


    if (length(
      frontier
    ) == 0L) {
      break
    }


    values_reverse <- get_reverse_values(
      cache,
      frontier
    )


    parent_parts <- vector(
      "list",
      8L
    )


    n_parts <- 0L
    direction_id <- 1L


    while (
      direction_id <=
        8L
    ) {

      idx <- which(
        bitwAnd(
          values_reverse,
          bits[
            direction_id
          ]
        ) !=
          0L
      )


      if (length(
        idx
      ) > 0L) {

        n_parts <- n_parts +
          1L


        parent_parts[[
          n_parts
        ]] <- frontier[
          idx
        ] +
          offsets[
            direction_id
          ]
      }


      direction_id <- direction_id +
        1L
    }


    if (n_parts == 0L) {
      break
    }


    if (n_parts == 1L) {

      parents <- parent_parts[[1L]]

    } else {

      parents <- unlist(
        parent_parts[
          seq_len(
            n_parts
          )
        ],
        use.names = FALSE
      )
    }


    n_cells <- n_cells +
      length(
        parents
      )


    if (
      n_cells >
        MAX_BASIN_CELLS
    ) {
      stop(
        paste0(
          "La cuenca supera MAX_BASIN_CELLS = ",
          format(
            MAX_BASIN_CELLS,
            big.mark = ","
          )
        )
      )
    }


    chunks[[
      length(
        chunks
      ) +
        1L
    ]] <- parents


    frontier <- parents


    if (
      !is.null(
        progress_fun
      ) &&
      level %%
        FAST_TRACE_PROGRESS_LEVELS ==
        0L
    ) {

      progress_fun(
        level,
        n_cells,
        length(
          frontier
        )
      )
    }
  }


  cells <- unlist(
    chunks,
    use.names = FALSE
  )


  list(
    cells = cells,
    n_cells = length(
      cells
    ),
    levels = level,
    trace_engine = "FAST_R"
  )
}


# ============================================================
# RECORRIDO POR LOTES PURO R
# ============================================================
#
# A diferencia del recorrido por niveles, procesa una cola de
# celdas en lotes grandes. El D8 valido garantiza que cada
# celda aguas arriba tiene un unico receptor, por lo que no
# necesita unique() ni bitmap visited durante la expansion.
#
# El algoritmo tambien acumula el bounding box de la cuenca
# durante el recorrido para no volver a recorrer todos los cell
# IDs al iniciar la rasterizacion.
# ============================================================

trace_upstream_batch <- function(
    cache,
    outlet_cell,
    progress_fun = NULL
) {

  if (
    !is.finite(
      TRACE_BATCH_SIZE
    ) ||
    TRACE_BATCH_SIZE <
      1L
  ) {
    stop(
      "TRACE_BATCH_SIZE debe ser >= 1."
    )
  }


  nc <- as.double(
    cache$metadata$ncols
  )


  nr <- as.double(
    cache$metadata$nrows
  )


  total_grid_cells <- nr *
    nc


  outlet_cell <- as.double(
    outlet_cell
  )


  if (
    !is.finite(
      outlet_cell
    ) ||
    outlet_cell <
      1 ||
    outlet_cell >
      total_grid_cells
  ) {
    stop(
      "Outlet cell fuera de la grilla."
    )
  }


  offsets <- c(
    -nc - 1,
    -nc,
    -nc + 1,
    -1,
    1,
    nc - 1,
    nc,
    nc + 1
  )


  bits <- c(
    1L,
    2L,
    4L,
    8L,
    16L,
    32L,
    64L,
    128L
  )


  queue_chunks <- list(
    outlet_cell
  )


  result_chunks <- list(
    outlet_cell
  )


  queue_index <- 1L
  result_index <- 1L
  n_cells <- 1
  n_batches <- 0L


  outlet_row <- floor(
    (
      outlet_cell -
        1
    ) /
      nc
  ) +
    1


  outlet_col <- (
    (
      outlet_cell -
        1
    ) %%
      nc
  ) +
    1


  bbox_rmin <- outlet_row
  bbox_rmax <- outlet_row
  bbox_cmin <- outlet_col
  bbox_cmax <- outlet_col


  while (
    queue_index <=
      length(
        queue_chunks
      )
  ) {

    chunk <- queue_chunks[[
      queue_index
    ]]


    # Libera la referencia de la cola. result_chunks conserva la
    # misma informacion para la materializacion final.
    queue_chunks[
      queue_index
    ] <- list(
      NULL
    )


    queue_index <- queue_index +
      1L


    chunk_length <- length(
      chunk
    )


    chunk_start <- 1L


    while (
      chunk_start <=
        chunk_length
    ) {

      chunk_end <- min(
        chunk_length,
        chunk_start +
          TRACE_BATCH_SIZE -
          1L
      )


      batch <- chunk[
        chunk_start:
          chunk_end
      ]


      n_batches <- n_batches +
        1L


      values_reverse <- get_reverse_values(
        cache,
        batch
      )


      parent_parts <- vector(
        "list",
        8L
      )


      n_parts <- 0L
      direction_id <- 1L


      while (
        direction_id <=
          8L
      ) {

        idx <- which(
          bitwAnd(
            values_reverse,
            bits[
              direction_id
            ]
          ) !=
            0L
        )


        if (length(
          idx
        ) > 0L) {

          n_parts <- n_parts +
            1L


          parent_parts[[
            n_parts
          ]] <- batch[
            idx
          ] +
            offsets[
              direction_id
            ]
        }


        direction_id <- direction_id +
          1L
      }


      if (n_parts > 0L) {

        if (n_parts == 1L) {

          parents <- parent_parts[[1L]]

        } else {

          parents <- unlist(
            parent_parts[
              seq_len(
                n_parts
              )
            ],
            use.names = FALSE
          )
        }


        # Protección final. En edge-safe no debería filtrarse
        # ninguna celda, pero evita índices corruptos.
        parents <- parents[
          parents >=
            1 &
            parents <=
              total_grid_cells
        ]


        if (length(
          parents
        ) > 0L) {

          n_cells <- n_cells +
            length(
              parents
            )


          if (
            n_cells >
              MAX_BASIN_CELLS
          ) {
            stop(
              paste0(
                "La cuenca supera MAX_BASIN_CELLS = ",
                format(
                  MAX_BASIN_CELLS,
                  big.mark = ","
                )
              )
            )
          }


          rows_parent <- floor(
            (
              parents -
                1
            ) /
              nc
          ) +
            1


          cols_parent <- (
            (
              parents -
                1
            ) %%
              nc
          ) +
            1


          bbox_rmin <- min(
            bbox_rmin,
            min(
              rows_parent
            )
          )


          bbox_rmax <- max(
            bbox_rmax,
            max(
              rows_parent
            )
          )


          bbox_cmin <- min(
            bbox_cmin,
            min(
              cols_parent
            )
          )


          bbox_cmax <- max(
            bbox_cmax,
            max(
              cols_parent
            )
          )


          rm(
            rows_parent,
            cols_parent
          )


          queue_chunks[[
            length(
              queue_chunks
            ) +
              1L
          ]] <- parents


          result_index <- result_index +
            1L


          result_chunks[[
            result_index
          ]] <- parents
        }
      }


      if (
        !is.null(
          progress_fun
        ) &&
        n_batches %%
          TRACE_PROGRESS_EVERY_BATCHES ==
            0L
      ) {

        progress_fun(
          n_batches,
          n_cells,
          length(
            batch
          )
        )
      }


      rm(
        batch,
        values_reverse,
        parent_parts
      )


      chunk_start <- chunk_end +
        1L
    }


    rm(
      chunk
    )
  }


  cells <- unlist(
    result_chunks,
    use.names = FALSE
  )


  list(
    cells = cells,
    n_cells = length(
      cells
    ),
    levels = NA_integer_,
    n_batches = n_batches,
    trace_engine = "BATCH_R",
    bbox = c(
      rmin = as.integer(
        bbox_rmin
      ),
      rmax = as.integer(
        bbox_rmax
      ),
      cmin = as.integer(
        bbox_cmin
      ),
      cmax = as.integer(
        bbox_cmax
      )
    )
  )
}


trace_upstream <- function(
    cache,
    outlet_cell,
    progress_fun = NULL
) {

  if (
    isTRUE(
      USE_BATCH_TRACE
    ) &&
    isTRUE(
      cache$fast_mode
    ) &&
    isTRUE(
      cache$edge_safe
    )
  ) {

    return(
      trace_upstream_batch(
        cache = cache,
        outlet_cell = outlet_cell,
        progress_fun = progress_fun
      )
    )
  }


  if (
    isTRUE(
      cache$fast_mode
    ) &&
    isTRUE(
      cache$edge_safe
    )
  ) {

    return(
      trace_upstream_fast(
        cache = cache,
        outlet_cell = outlet_cell,
        progress_fun = progress_fun
      )
    )
  }


  out <- trace_upstream_safe(
    cache = cache,
    outlet_cell = outlet_cell,
    progress_fun = progress_fun
  )


  out$trace_engine <- "SAFE_R"
  out$n_batches <- NA_integer_
  out$bbox <- NULL


  out
}


# ============================================================
# ESCRIBIR BASIN.TIF SIN MATRIZ GIGANTE
# ============================================================
#
# Version reescrita para evitar los errores de parser que
# aparecian alrededor de los bucles for() de la version previa.
#
# Se trabaja por franjas horizontales y con while().
# No se crea una matriz del tamaño completo del bloque.
# ============================================================

write_basin_raster <- function(
    cells,
    template,
    output_file,
    temp_dir,
    bbox = NULL
) {

  if (length(
    cells
  ) == 0L) {
    stop(
      "No hay celdas para materializar la cuenca."
    )
  }


  dir.create(
    temp_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )


  # En un D8 valido el trace inverso no repite celdas.
  # Evitamos unique(), que para cuencas de cientos de millones
  # de celdas era muy costoso. El sort sigue siendo necesario
  # para localizar eficientemente los cell IDs por franja.
  cells <- sort(
    as.double(
      cells
    ),
    method = "radix"
  )


  nc_global <- as.double(
    ncol(
      template
    )
  )


  rr <- res(
    template
  )


  if (
    !is.null(
      bbox
    ) &&
    all(
      c(
        "rmin",
        "rmax",
        "cmin",
        "cmax"
      ) %in%
        names(
          bbox
        )
    )
  ) {

    rmin <- as.integer(
      bbox[["rmin"]]
    )

    rmax <- as.integer(
      bbox[["rmax"]]
    )

    cmin <- as.integer(
      bbox[["cmin"]]
    )

    cmax <- as.integer(
      bbox[["cmax"]]
    )

  } else {

    all_rows <- floor(
      (
        cells -
          1
      ) /
        nc_global
    ) +
      1


    all_cols <- (
      (
        cells -
          1
      ) %%
        nc_global
    ) +
      1


    rmin <- as.integer(
      min(
        all_rows
      )
    )

    rmax <- as.integer(
      max(
        all_rows
      )
    )

    cmin <- as.integer(
      min(
        all_cols
      )
    )

    cmax <- as.integer(
      max(
        all_cols
      )
    )


    rm(
      all_rows,
      all_cols
    )

    gc()
  }


  local_ncols <- cmax -
    cmin +
    1L


  row_starts <- seq.int(
    from = rmin,
    to = rmax,
    by = BASIN_WRITE_ROWS
  )


  n_stripes <- length(
    row_starts
  )


  stripe_files <- character(
    n_stripes
  )


  i <- 1L


  while (
    i <=
      n_stripes
  ) {

    global_row_start <- row_starts[i]


    global_row_end <- min(
      rmax,
      global_row_start +
        BASIN_WRITE_ROWS -
        1L
    )


    nrows_chunk <- global_row_end -
      global_row_start +
      1L


    first_possible <- (
      (
        global_row_start -
          1
      ) *
        nc_global
    ) +
      cmin


    last_possible <- (
      (
        global_row_end -
          1
      ) *
        nc_global
    ) +
      cmax


    lo <- findInterval(
      first_possible -
        1,
      cells
    ) +
      1L


    hi <- findInterval(
      last_possible,
      cells
    )


    vals <- integer(
      as.double(
        nrows_chunk
      ) *
        as.double(
          local_ncols
        )
    )


    if (
      lo <=
        hi &&
      lo >=
        1L &&
      hi <=
        length(
          cells
        )
    ) {

      subset_cells <- cells[
        lo:
          hi
      ]


      rows <- floor(
        (
          subset_cells -
            1
        ) /
          nc_global
      ) +
        1


      cols <- (
        (
          subset_cells -
            1
        ) %%
          nc_global
      ) +
        1


      keep <- (
        rows >=
          global_row_start &
        rows <=
          global_row_end &
        cols >=
          cmin &
        cols <=
          cmax
      )


      rows <- rows[
        keep
      ]


      cols <- cols[
        keep
      ]


      local_pos <- (
        (
          rows -
            global_row_start
        ) *
          local_ncols
      ) +
        (
          cols -
            cmin
        ) +
        1


      vals[
        local_pos
      ] <- 1L
    }


    xmn <- xmin(
      template
    ) +
      (
        cmin -
          1L
      ) *
        rr[1]


    xmx <- xmin(
      template
    ) +
      cmax *
        rr[1]


    ymx <- ymax(
      template
    ) -
      (
        global_row_start -
          1L
      ) *
        rr[2]


    ymn <- ymax(
      template
    ) -
      global_row_end *
        rr[2]


    stripe <- rast(
      nrows = nrows_chunk,
      ncols = local_ncols,
      xmin = xmn,
      xmax = xmx,
      ymin = ymn,
      ymax = ymx,
      crs = crs(
        template
      )
    )


    values(
      stripe
    ) <- vals


    stripe_file <- file.path(
      temp_dir,
      sprintf(
        "basin_%05d.tif",
        i
      )
    )


    writeRaster(
      stripe,
      stripe_file,
      overwrite = TRUE,
      datatype = "INT1U",
      NAflag = 0,
      gdal = c(
        "COMPRESS=DEFLATE",
        "TILED=YES",
        "BIGTIFF=IF_SAFER"
      )
    )


    stripe_files[i] <- stripe_file


    rm(
      stripe,
      vals
    )


    gc()


    i <- i +
      1L
  }


  basin_vrt_file <- file.path(
    temp_dir,
    "basin.vrt"
  )


  basin_vrt <- vrt(
    stripe_files,
    filename = basin_vrt_file,
    overwrite = TRUE
  )


  writeRaster(
    basin_vrt,
    output_file,
    overwrite = TRUE,
    datatype = "INT1U",
    NAflag = 0,
    gdal = c(
      "COMPRESS=DEFLATE",
      "TILED=YES",
      "BIGTIFF=IF_SAFER"
    )
  )


  rm(
    basin_vrt
  )


  gc()


  output_file
}


# ============================================================
# POLIGONIZAR
# ============================================================

polygonize_basin <- function(
    basin_tif,
    basin_gpkg
) {

  r <- rast(
    basin_tif
  )


  p <- as.polygons(
    r,
    dissolve = TRUE,
    values = FALSE,
    na.rm = TRUE
  )


  x <- st_as_sf(
    p
  )


  x <- st_make_valid(
    x
  )


  x <- x[
    !st_is_empty(
      x
    ),
    ,
    drop = FALSE
  ]


  if (file.exists(basin_gpkg)) {

    file.remove(
      basin_gpkg
    )
  }


  st_write(
    x,
    basin_gpkg,
    layer = "basin",
    quiet = TRUE
  )


  x
}



# ============================================================
# 7. FILAS DE GRILLA Y SNAP A STREAM STRIPES
# ============================================================

grid_row_from_y <- function(
    template,
    y
) {

  rr_y <- abs(
    terra::res(
      template
    )[2]
  )


  row <- floor(
    (
      terra::ymax(
        template
      ) -
        y
    ) /
      rr_y
  ) +
    1


  row <- max(
    1,
    min(
      terra::nrow(
        template
      ),
      row
    )
  )


  as.integer(
    row
  )
}


stripe_id_from_row <- function(
    row,
    stripe_rows,
    n_stripes
) {

  sid <- floor(
    (
      row -
        1L
    ) /
      stripe_rows
  ) +
    1L


  sid <- max(
    1L,
    min(
      as.integer(
        n_stripes
      ),
      as.integer(
        sid
      )
    )
  )


  sid
}


extract_stream_value_at_xy <- function(
    stream_r,
    xy
) {

  cell <- terra::cellFromXY(
    stream_r,
    matrix(
      c(
        xy[1],
        xy[2]
      ),
      nrow = 1
    )
  )


  if (is.na(cell)) {
    return(
      NA_real_
    )
  }


  value <- terra::extract(
    stream_r,
    cell
  )


  value <- last_data_column(
    value
  )


  if (length(value) != 1L) {
    return(
      NA_real_
    )
  }


  as.numeric(
    value[1]
  )
}


snap_to_stream_stripes <- function(
    lon,
    lat,
    radius_m,
    grid_template,
    stream_cache,
    stripe_rows,
    n_stripes
) {

  if (
    !is.finite(radius_m) ||
    radius_m <= 0
  ) {
    stop(
      "El radio interno de ajuste debe ser mayor que cero."
    )
  }


  grid_crs <- sf::st_crs(
    terra::crs(
      grid_template
    )
  )


  if (is.na(
    grid_crs
  )) {
    stop(
      "La grilla del bloque no tiene CRS valido."
    )
  }


  click_wgs <- sf::st_sfc(
    sf::st_point(
      c(
        lon,
        lat
      )
    ),
    crs = 4326
  )


  click_grid <- sf::st_transform(
    click_wgs,
    grid_crs
  )


  click_xy <- sf::st_coordinates(
    click_grid
  )[1, ]


  global_click_cell <- terra::cellFromXY(
    grid_template,
    matrix(
      c(
        click_xy[1],
        click_xy[2]
      ),
      nrow = 1
    )
  )


  if (is.na(
    global_click_cell
  )) {
    stop(
      "El clic quedo fuera de la grilla hidrologica del bloque."
    )
  }


  click_row <- floor(
    (
      as.double(
        global_click_cell
      ) -
        1
    ) /
      as.double(
        terra::ncol(
          grid_template
        )
      )
  ) +
    1


  click_sid <- stripe_id_from_row(
    row = as.integer(
      click_row
    ),
    stripe_rows = as.integer(
      stripe_rows
    ),
    n_stripes = as.integer(
      n_stripes
    )
  )


  click_stream_r <- load_stream_stripe(
    stream_cache,
    click_sid
  )


  click_value <- extract_stream_value_at_xy(
    click_stream_r,
    click_xy
  )


  if (
    is.finite(
      click_value
    ) &&
    click_value > 0
  ) {

    local_cell <- terra::cellFromXY(
      click_stream_r,
      matrix(
        c(
          click_xy[1],
          click_xy[2]
        ),
        nrow = 1
      )
    )


    outlet_xy <- terra::xyFromCell(
      click_stream_r,
      local_cell
    )


    mode <- "CLICK_ON_STREAM_CELL"

  } else {

    epsg <- utm_epsg_point(
      lon,
      lat
    )


    click_utm <- sf::st_transform(
      click_wgs,
      epsg
    )


    search_utm <- sf::st_buffer(
      click_utm,
      dist = radius_m
    )


    search_grid <- sf::st_transform(
      search_utm,
      grid_crs
    )


    bb <- sf::st_bbox(
      search_grid
    )


    search_extent <- terra::ext(
      as.numeric(
        bb["xmin"]
      ),
      as.numeric(
        bb["xmax"]
      ),
      as.numeric(
        bb["ymin"]
      ),
      as.numeric(
        bb["ymax"]
      )
    )


    row_top <- grid_row_from_y(
      grid_template,
      as.numeric(
        bb["ymax"]
      )
    )


    row_bottom <- grid_row_from_y(
      grid_template,
      as.numeric(
        bb["ymin"]
      )
    )


    sid_first <- stripe_id_from_row(
      row = min(
        row_top,
        row_bottom
      ),
      stripe_rows = as.integer(
        stripe_rows
      ),
      n_stripes = as.integer(
        n_stripes
      )
    )


    sid_last <- stripe_id_from_row(
      row = max(
        row_top,
        row_bottom
      ),
      stripe_rows = as.integer(
        stripe_rows
      ),
      n_stripes = as.integer(
        n_stripes
      )
    )


    candidate_xy_parts <- list()


    sid <- sid_first


    while (sid <= sid_last) {

      stream_r <- load_stream_stripe(
        stream_cache,
        sid
      )


      local_mask <- tryCatch(
        terra::crop(
          stream_r,
          search_extent,
          snap = "out"
        ),
        error = function(e) NULL
      )


      if (
        !is.null(
          local_mask
        ) &&
        terra::ncell(
          local_mask
        ) >
          0
      ) {

        local_values <- terra::values(
          local_mask,
          mat = FALSE
        )


        local_cells <- which(
          !is.na(
            local_values
          ) &
            local_values >
              0
        )


        if (length(local_cells) > 0L) {

          xy_part <- terra::xyFromCell(
            local_mask,
            local_cells
          )


          candidate_xy_parts[[
            length(
              candidate_xy_parts
            ) +
              1L
          ]] <- xy_part
        }
      }


      sid <- sid +
        1L
    }


    if (length(candidate_xy_parts) == 0L) {
      stop(
        paste0(
          "No se encontro una celda de cauce dentro de ",
          radius_m,
          " m del clic."
        )
      )
    }


    candidate_xy <- do.call(
      rbind,
      candidate_xy_parts
    )


    candidate_sf <- sf::st_as_sf(
      data.frame(
        x = candidate_xy[, 1],
        y = candidate_xy[, 2]
      ),
      coords = c(
        "x",
        "y"
      ),
      crs = grid_crs
    )


    candidate_utm <- sf::st_transform(
      candidate_sf,
      epsg
    )


    distances <- as.numeric(
      sf::st_distance(
        candidate_utm,
        click_utm
      )
    )


    inside <- which(
      is.finite(
        distances
      ) &
        distances <=
          radius_m
    )


    if (length(inside) == 0L) {
      stop(
        paste0(
          "No se encontro una celda de cauce dentro del radio exacto de ",
          radius_m,
          " m."
        )
      )
    }


    winner <- inside[
      which.min(
        distances[inside]
      )
    ]


    outlet_xy <- candidate_xy[
      winner,
      ,
      drop = FALSE
    ]


    mode <- "NEAREST_STREAM_CELL"
  }


  outlet_cell <- terra::cellFromXY(
    grid_template,
    matrix(
      c(
        outlet_xy[1, 1],
        outlet_xy[1, 2]
      ),
      nrow = 1
    )
  )


  if (is.na(
    outlet_cell
  )) {
    stop(
      "La celda de cauce elegida quedo fuera de la grilla del bloque."
    )
  }


  outlet_row <- floor(
    (
      as.double(
        outlet_cell
      ) -
        1
    ) /
      as.double(
        terra::ncol(
          grid_template
        )
      )
  ) +
    1


  outlet_sid <- stripe_id_from_row(
    row = as.integer(
      outlet_row
    ),
    stripe_rows = as.integer(
      stripe_rows
    ),
    n_stripes = as.integer(
      n_stripes
    )
  )


  outlet_stream_r <- load_stream_stripe(
    stream_cache,
    outlet_sid
  )


  outlet_stream_value <- extract_stream_value_at_xy(
    outlet_stream_r,
    c(
      outlet_xy[1, 1],
      outlet_xy[1, 2]
    )
  )


  if (
    !is.finite(
      outlet_stream_value
    ) ||
    outlet_stream_value <= 0
  ) {
    stop(
      "Fallo de seguridad: el outlet seleccionado no pertenece a stream_stripes."
    )
  }


  outlet_grid <- sf::st_sfc(
    sf::st_point(
      c(
        outlet_xy[1, 1],
        outlet_xy[1, 2]
      )
    ),
    crs = grid_crs
  )


  outlet_wgs <- sf::st_transform(
    outlet_grid,
    4326
  )


  outlet_wgs_xy <- sf::st_coordinates(
    outlet_wgs
  )[1, ]


  epsg_final <- utm_epsg_point(
    lon,
    lat
  )


  click_utm_final <- sf::st_transform(
    click_wgs,
    epsg_final
  )


  outlet_utm_final <- sf::st_transform(
    outlet_wgs,
    epsg_final
  )


  distance_final <- as.numeric(
    sf::st_distance(
      click_utm_final,
      outlet_utm_final
    )
  )


  list(
    clicked_lon = lon,
    clicked_lat = lat,
    outlet_cell = as.double(
      outlet_cell
    ),
    outlet_x = outlet_xy[1, 1],
    outlet_y = outlet_xy[1, 2],
    outlet_lon = outlet_wgs_xy[1],
    outlet_lat = outlet_wgs_xy[2],
    snap_mode = mode,
    snap_distance_m = distance_final,
    stream_mask_value = as.numeric(
      outlet_stream_value
    )
  )
}


# ============================================================
# 8. CARGAR BLOQUE LOCAL
# ============================================================

load_block_if_needed <- function(
    block_id,
    block_cache
) {

  if (
    !is.null(
      block_cache$block_id
    ) &&
    identical(
      block_cache$block_id,
      block_id
    )
  ) {
    return(
      invisible(
        TRUE
      )
    )
  }


  meta <- get_block_metadata(
    block_id
  )


  n_stripes <- as.integer(
    meta[["N_STRIPES"]][1]
  )


  stripe_rows <- as.integer(
    meta[["STRIPE_ROWS"]][1]
  )


  threshold_cells <- suppressWarnings(
    as.numeric(
      meta[["STREAM_THRESHOLD_CELLS"]][1]
    )
  )


  threshold_km2 <- suppressWarnings(
    as.numeric(
      meta[["STREAM_THRESHOLD_KM2"]][1]
    )
  )


  if (
    !is.finite(
      threshold_cells
    ) ||
    threshold_cells <
      1
  ) {
    stop(
      paste0(
        block_id,
        ": STREAM_THRESHOLD_CELLS invalido."
      )
    )
  }


  if (
    !is.finite(
      threshold_km2
    ) ||
    threshold_km2 <=
      0
  ) {
    stop(
      paste0(
        block_id,
        ": STREAM_THRESHOLD_KM2 invalido."
      )
    )
  }


  reverse_rows <- get_block_assets(
    block_id,
    "reverse"
  )


  stream_rows <- get_block_assets(
    block_id,
    "stream"
  )


  validate_stripe_sequence(
    reverse_rows,
    expected_n = n_stripes,
    block_id = block_id,
    label = "reverse_stripes"
  )


  validate_stripe_sequence(
    stream_rows,
    expected_n = n_stripes,
    block_id = block_id,
    label = "stream_stripes"
  )


  if (!identical(
    as.integer(
      reverse_rows[["ROW_START"]]
    ),
    as.integer(
      stream_rows[["ROW_START"]]
    )
  )) {
    stop(
      paste0(
        block_id,
        ": ROW_START reverse/stream no coincide."
      )
    )
  }


  if (!identical(
    as.integer(
      reverse_rows[["ROW_END"]]
    ),
    as.integer(
      stream_rows[["ROW_END"]]
    )
  )) {
    stop(
      paste0(
        block_id,
        ": ROW_END reverse/stream no coincide."
      )
    )
  }


  grid_template <- create_grid_template(
    meta
  )


  reverse_cache <- new_reverse_cache(
    block_id = block_id,
    meta = meta,
    reverse_rows = reverse_rows
  )


  stream_cache <- new_stream_cache(
    block_id = block_id,
    stream_rows = stream_rows
  )


  block_cache$block_id <- block_id
  block_cache$grid_template <- grid_template
  block_cache$reverse_cache <- reverse_cache
  block_cache$stream_cache <- stream_cache
  block_cache$stripe_rows <- stripe_rows
  block_cache$n_stripes <- n_stripes
  block_cache$stream_threshold_cells <- threshold_cells
  block_cache$stream_threshold_km2 <- threshold_km2


  invisible(
    TRUE
  )
}


# ============================================================
# 9. NOMBRE AUTOMATICO DE SALIDA
# ============================================================

next_output_folder <- function() {

  stem <- paste0(
    "cuenca_",
    format(
      Sys.time(),
      "%Y%m%d_%H%M%S"
    )
  )


  candidate <- file.path(
    OUTPUT_DIR,
    stem
  )


  if (!dir.exists(
    candidate
  )) {
    return(
      list(
        name = stem,
        path = candidate
      )
    )
  }


  i <- 1L


  repeat {

    name_i <- paste0(
      stem,
      "_",
      sprintf(
        "%02d",
        i
      )
    )


    candidate <- file.path(
      OUTPUT_DIR,
      name_i
    )


    if (!dir.exists(
      candidate
    )) {
      return(
        list(
          name = name_i,
          path = candidate
        )
      )
    }


    i <- i +
      1L
  }
}


# ============================================================
# 10. IMPORTACION DE CUENCAS VECTORIALES
# ============================================================
#
# Formatos admitidos por el modulo Delimitacion:
# - KML
# - KMZ
# - GeoPackage
# - Shapefile en ZIP
# - Shapefile como SHP + SHX + DBF + PRJ
#
# La salida es una lista de candidatos. Cada candidato contiene
# exactamente una geometria POLYGON/MULTIPOLYGON valida en EPSG:4326.
# No se hace union automatica de multiples entidades.
# ============================================================

sanitize_upload_name <- function(x) {

  x <- basename(
    as.character(
      x
    )
  )

  x <- gsub(
    "[^A-Za-z0-9._-]",
    "_",
    x
  )

  if (!nzchar(
    x
  )) {
    x <- "upload"
  }

  x
}


safe_unzip_spatial <- function(
    zip_file,
    exdir
) {

  listing <- utils::unzip(
    zip_file,
    list = TRUE
  )

  if (
    nrow(
      listing
    ) == 0L
  ) {
    stop(
      "El archivo comprimido esta vacio."
    )
  }

  members <- gsub(
    "\\\\",
    "/",
    as.character(
      listing$Name
    )
  )

  unsafe <- grepl(
    "(^|/)\\.\\.(/|$)|^/|^[A-Za-z]:/",
    members
  )

  if (any(
    unsafe
  )) {
    stop(
      "El archivo comprimido contiene rutas no seguras."
    )
  }

  dir.create(
    exdir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  utils::unzip(
    zip_file,
    exdir = exdir
  )

  invisible(
    exdir
  )
}


uploaded_spatial_workspace <- function(
    upload_df,
    work_dir
) {

  if (
    is.null(
      upload_df
    ) ||
    nrow(
      upload_df
    ) < 1L
  ) {
    stop(
      "No se recibieron archivos para importar."
    )
  }

  dir.create(
    work_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  copied <- character(
    nrow(
      upload_df
    )
  )

  for (i in seq_len(
    nrow(
      upload_df
    )
  )) {

    original_name <- sanitize_upload_name(
      upload_df$name[i]
    )

    destination <- file.path(
      work_dir,
      original_name
    )

    ok <- file.copy(
      upload_df$datapath[i],
      destination,
      overwrite = TRUE
    )

    if (!isTRUE(
      ok
    )) {
      stop(
        paste0(
          "No se pudo preparar el archivo cargado: ",
          original_name
        )
      )
    }

    copied[i] <- destination
  }

  copied
}


validate_shapefile_bundle <- function(
    shp_file
) {

  stem <- tools::file_path_sans_ext(
    shp_file
  )

  required <- paste0(
    stem,
    c(
      ".shp",
      ".shx",
      ".dbf",
      ".prj"
    )
  )

  # Windows y algunos ZIP pueden conservar mayusculas distintas.
  folder_files <- list.files(
    dirname(
      shp_file
    ),
    full.names = TRUE
  )

  folder_lower <- tolower(
    basename(
      folder_files
    )
  )

  required_names <- tolower(
    basename(
      required
    )
  )

  missing <- required_names[
    !required_names %in%
      folder_lower
  ]

  if (length(
    missing
  ) > 0L) {
    stop(
      paste0(
        "Shapefile incompleto. Faltan: ",
        paste(
          toupper(
            tools::file_ext(
              missing
            )
          ),
          collapse = ", "
        ),
        ". Se requieren SHP + SHX + DBF + PRJ."
      )
    )
  }

  # Devuelve la ruta real del SHP por si la extension venia en mayusculas.
  hit <- which(
    folder_lower ==
      tolower(
        basename(
          shp_file
        )
      )
  )[1]

  folder_files[
    hit
  ]
}


spatial_candidate_name <- function(
    x,
    row_id,
    layer_name,
    source_name
) {

  nms <- names(
    x
  )

  geometry_col <- attr(
    x,
    "sf_column"
  )

  fields <- nms[
    nms != geometry_col
  ]

  preferred <- c(
    "nombre",
    "name",
    "id",
    "codigo",
    "code",
    "cuenca",
    "basin"
  )

  field_lower <- tolower(
    fields
  )

  hit <- match(
    preferred,
    field_lower,
    nomatch = 0L
  )

  hit <- hit[
    hit > 0L
  ]

  label_value <- NULL

  if (length(
    hit
  ) > 0L) {
    field <- fields[
      hit[1]
    ]

    value <- x[[field]][row_id]

    if (
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(
        trimws(
          as.character(
            value
          )
        )
      )
    ) {
      label_value <- trimws(
        as.character(
          value
        )
      )
    }
  }

  if (is.null(
    label_value
  )) {
    label_value <- paste0(
      "Entidad ",
      row_id
    )
  }

  paste0(
    source_name,
    " | ",
    layer_name,
    " | ",
    label_value
  )
}


polygon_candidates_from_sf <- function(
    x,
    source_name,
    layer_name
) {

  if (!inherits(
    x,
    "sf"
  )) {
    stop(
      "La capa importada no pudo interpretarse como un objeto espacial."
    )
  }

  if (is.na(
    sf::st_crs(
      x
    )
  )) {
    stop(
      paste0(
        source_name,
        " | ",
        layer_name,
        ": la capa no tiene CRS definido."
      )
    )
  }

  x <- sf::st_make_valid(
    x
  )

  x <- x[
    !sf::st_is_empty(
      x
    ),
    ,
    drop = FALSE
  ]

  if (nrow(
    x
  ) == 0L) {
    return(
      list()
    )
  }

  types <- as.character(
    sf::st_geometry_type(
      x,
      by_geometry = TRUE
    )
  )

  polygon_rows <- types %in% c(
    "POLYGON",
    "MULTIPOLYGON"
  )

  collection_rows <- types ==
    "GEOMETRYCOLLECTION"

  pieces <- list()

  if (any(
    polygon_rows
  )) {
    pieces[[length(
      pieces
    ) + 1L]] <- x[
      polygon_rows,
      ,
      drop = FALSE
    ]
  }

  if (any(
    collection_rows
  )) {
    collection_x <- suppressWarnings(
      sf::st_collection_extract(
        x[
          collection_rows,
          ,
          drop = FALSE
        ],
        "POLYGON",
        warn = FALSE
      )
    )

    collection_x <- collection_x[
      !sf::st_is_empty(
        collection_x
      ),
      ,
      drop = FALSE
    ]

    if (nrow(
      collection_x
    ) > 0L) {
      pieces[[length(
        pieces
      ) + 1L]] <- collection_x
    }
  }

  if (length(
    pieces
  ) == 0L) {
    return(
      list()
    )
  }

  x_poly <- do.call(
    rbind,
    pieces
  )

  x_poly <- sf::st_transform(
    x_poly,
    4326
  )

  x_poly <- sf::st_make_valid(
    x_poly
  )

  out <- vector(
    "list",
    nrow(
      x_poly
    )
  )

  for (i in seq_len(
    nrow(
      x_poly
    )
  )) {

    one <- x_poly[
      i,
      ,
      drop = FALSE
    ]

    # Conserva atributos, pero normaliza la geometria a una sola entidad.
    geom <- sf::st_geometry(
      one
    )

    if (length(
      geom
    ) != 1L) {
      next
    }

    label <- spatial_candidate_name(
      x = x_poly,
      row_id = i,
      layer_name = layer_name,
      source_name = source_name
    )

    out[[i]] <- list(
      label = label,
      source_name = source_name,
      layer_name = layer_name,
      feature_index = i,
      basin = one
    )
  }

  Filter(
    Negate(
      is.null
    ),
    out
  )
}


read_polygon_dataset <- function(
    dataset_path
) {

  ext <- tolower(
    tools::file_ext(
      dataset_path
    )
  )

  source_name <- basename(
    dataset_path
  )

  layers <- NULL

  if (identical(
    ext,
    "shp"
  )) {
    dataset_path <- validate_shapefile_bundle(
      dataset_path
    )
    layers <- tools::file_path_sans_ext(
      basename(
        dataset_path
      )
    )
  } else if (ext %in% c(
    "gpkg",
    "kml"
  )) {
    layers <- sf::st_layers(
      dataset_path
    )$name
  } else {
    return(
      list()
    )
  }

  if (length(
    layers
  ) == 0L) {
    return(
      list()
    )
  }

  out <- list()

  for (layer_name in layers) {

    x <- tryCatch(
      sf::st_read(
        dataset_path,
        layer = layer_name,
        quiet = TRUE,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(
      x
    )) {
      next
    }

    candidates <- polygon_candidates_from_sf(
      x = x,
      source_name = source_name,
      layer_name = as.character(
        layer_name
      )
    )

    if (length(
      candidates
    ) > 0L) {
      out <- c(
        out,
        candidates
      )
    }
  }

  out
}


read_uploaded_basin_candidates <- function(
    upload_df,
    work_dir
) {

  copied <- uploaded_spatial_workspace(
    upload_df = upload_df,
    work_dir = work_dir
  )

  all_files <- copied

  for (path in copied) {

    ext <- tolower(
      tools::file_ext(
        path
      )
    )

    if (identical(
      ext,
      "kmz"
    )) {
      kmz_dir <- file.path(
        work_dir,
        paste0(
          "kmz_",
          tools::file_path_sans_ext(
            basename(
              path
            )
          )
        )
      )

      safe_unzip_spatial(
        path,
        kmz_dir
      )

      all_files <- c(
        all_files,
        list.files(
          kmz_dir,
          full.names = TRUE,
          recursive = TRUE
        )
      )
    }

    if (identical(
      ext,
      "zip"
    )) {
      zip_dir <- file.path(
        work_dir,
        paste0(
          "zip_",
          tools::file_path_sans_ext(
            basename(
              path
            )
          )
        )
      )

      safe_unzip_spatial(
        path,
        zip_dir
      )

      all_files <- c(
        all_files,
        list.files(
          zip_dir,
          full.names = TRUE,
          recursive = TRUE
        )
      )
    }
  }

  dataset_files <- all_files[
    tolower(
      tools::file_ext(
        all_files
      )
    ) %in% c(
      "shp",
      "gpkg",
      "kml"
    )
  ]

  dataset_files <- unique(
    normalizePath(
      dataset_files,
      winslash = "/",
      mustWork = TRUE
    )
  )

  if (length(
    dataset_files
  ) == 0L) {
    stop(
      paste0(
        "No se encontro un dataset espacial compatible. ",
        "Usa KML, KMZ, GPKG, SHP+SHX+DBF+PRJ o un ZIP con Shapefile."
      )
    )
  }

  candidates <- list()

  errors <- character(
    0
  )

  for (dataset_path in dataset_files) {

    result_i <- tryCatch(
      read_polygon_dataset(
        dataset_path
      ),
      error = function(e) {
        errors <<- c(
          errors,
          paste0(
            basename(
              dataset_path
            ),
            ": ",
            conditionMessage(
              e
            )
          )
        )
        list()
      }
    )

    if (length(
      result_i
    ) > 0L) {
      candidates <- c(
        candidates,
        result_i
      )
    }
  }

  if (length(
    candidates
  ) == 0L) {
    detail <- if (length(
      errors
    ) > 0L) {
      paste0(
        " Detalle: ",
        paste(
          unique(
            errors
          ),
          collapse = " | "
        )
      )
    } else {
      ""
    }

    stop(
      paste0(
        "No se encontraron geometrías POLYGON/MULTIPOLYGON válidas.",
        detail
      )
    )
  }

  # Etiquetas unicas para el selectInput.
  labels <- vapply(
    candidates,
    function(z) z$label,
    character(1)
  )

  labels <- make.unique(
    labels,
    sep = " #"
  )

  for (i in seq_along(
    candidates
  )) {
    candidates[[i]]$label <- labels[i]
  }

  candidates
}



# ============================================================
# 11. EXPORTACION SHAPEFILE NORMALIZADA
# ============================================================
# Utilidades comunes para los modulos tematicos:
# - reconstruir el mosaico normalizado a partir de las teselas usadas;
# - recortar cualquier geometria vectorial a la cuenca activa;
# - entregar uno o varios Shapefiles dentro de un ZIP portable.
#
# El mosaico NO se recorta por la divisoria. Cuando la fuente esta
# teselada, une las teselas seleccionadas por el modulo. Cuando solo
# existe el archivo original, limita la lectura exportable al marco
# rectangular de la cuenca con un 7 % de margen para evitar exportar
# innecesariamente una capa nacional completa.
# ============================================================

vector_export_safe_stem <- function(x) {

  x <- as.character(x)[1]

  if (is.na(x) || !nzchar(trimws(x))) {
    x <- "capa"
  }

  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT",
    sub = "_"
  )

  x <- gsub(
    "[^A-Za-z0-9_]+",
    "_",
    x
  )

  x <- gsub(
    "_+",
    "_",
    x
  )

  x <- gsub(
    "^_+|_+$",
    "",
    x
  )

  if (!nzchar(x)) {
    x <- "capa"
  }

  substr(x, 1L, 40L)
}


vector_export_frame <- function(
    basin,
    target_crs,
    margin = 0.07
) {

  if (
    is.null(basin) ||
    !inherits(basin, "sf") ||
    nrow(basin) < 1L
  ) {
    stop("No hay una cuenca valida para construir el marco del mosaico.")
  }

  b <- sf::st_make_valid(basin)
  b <- b[
    !sf::st_is_empty(b),
    ,
    drop = FALSE
  ]

  if (nrow(b) < 1L) {
    stop("La cuenca esta vacia.")
  }

  b <- sf::st_transform(
    b,
    target_crs
  )

  bb <- sf::st_bbox(b)

  dx <- as.numeric(
    bb[["xmax"]] - bb[["xmin"]]
  )

  dy <- as.numeric(
    bb[["ymax"]] - bb[["ymin"]]
  )

  if (!is.finite(dx) || dx <= 0) {
    dx <- 1
  }

  if (!is.finite(dy) || dy <= 0) {
    dy <- 1
  }

  bb[["xmin"]] <- bb[["xmin"]] - margin * dx
  bb[["xmax"]] <- bb[["xmax"]] + margin * dx
  bb[["ymin"]] <- bb[["ymin"]] - margin * dy
  bb[["ymax"]] <- bb[["ymax"]] + margin * dy

  sf::st_as_sfc(bb)
}


vector_export_clean_sf <- function(x) {

  if (
    is.null(x) ||
    !inherits(x, "sf")
  ) {
    stop("El objeto a exportar no es una capa sf valida.")
  }

  if (nrow(x) < 1L) {
    return(x)
  }

  if (is.na(sf::st_crs(x))) {
    stop("La capa a exportar no tiene CRS valido.")
  }

  x <- sf::st_make_valid(x)
  x <- x[
    !sf::st_is_empty(x),
    ,
    drop = FALSE
  ]

  # Los campos internos de dibujo/etiquetado no pertenecen a la
  # capa normalizada que se entrega al usuario.
  internal_fields <- grep(
    "^\\.__",
    names(x),
    value = TRUE
  )

  if (length(internal_fields) > 0L) {
    x[internal_fields] <- NULL
  }

  geom_col <- attr(x, "sf_column")
  attrs <- setdiff(names(x), geom_col)

  for (nm in attrs) {
    value <- x[[nm]]

    if (is.list(value)) {
      x[[nm]] <- vapply(
        value,
        function(z) paste(as.character(z), collapse = "; "),
        character(1)
      )
    }
  }

  x
}


read_vector_export_mosaic <- function(
    selected_files,
    source_mode,
    basin,
    tile_layer = "data",
    margin = 0.07
) {

  selected_files <- unique(
    as.character(selected_files)
  )

  selected_files <- selected_files[
    !is.na(selected_files) &
      nzchar(selected_files)
  ]

  if (length(selected_files) < 1L) {
    stop("No hay archivos fuente registrados para reconstruir el mosaico.")
  }

  missing <- selected_files[
    !file.exists(selected_files)
  ]

  if (length(missing) > 0L) {
    stop(
      paste0(
        "Falta un archivo normalizado para exportar:\n",
        missing[1]
      )
    )
  }

  source_mode <- as.character(source_mode)[1]

  pieces <- list()

  if (identical(source_mode, "tiles")) {

    for (path in selected_files) {
      layers <- sf::st_layers(path)$name

      layer_use <- if (tile_layer %in% layers) {
        tile_layer
      } else if (length(layers) > 0L) {
        layers[1L]
      } else {
        NA_character_
      }

      if (is.na(layer_use)) {
        next
      }

      g <- sf::st_read(
        path,
        layer = layer_use,
        quiet = TRUE,
        stringsAsFactors = FALSE
      )

      if (inherits(g, "sf") && nrow(g) > 0L) {
        pieces[[length(pieces) + 1L]] <- g
      }
    }

  } else {

    path <- selected_files[1L]
    layers <- sf::st_layers(path)$name

    if (length(layers) < 1L) {
      stop("El archivo normalizado no contiene capas vectoriales.")
    }

    g <- sf::st_read(
      path,
      layer = layers[1L],
      quiet = TRUE,
      stringsAsFactors = FALSE
    )

    if (!inherits(g, "sf")) {
      stop("No se pudo leer la capa normalizada como objeto sf.")
    }

    if (nrow(g) > 0L) {
      frame <- vector_export_frame(
        basin = basin,
        target_crs = sf::st_crs(g),
        margin = margin
      )

      keep <- lengths(
        sf::st_intersects(
          g,
          frame
        )
      ) > 0L

      g <- g[
        keep,
        ,
        drop = FALSE
      ]
    }

    pieces[[1L]] <- g
  }

  if (length(pieces) < 1L) {
    stop("No se obtuvo geometria para el mosaico normalizado.")
  }

  crs_ref <- sf::st_crs(pieces[[1L]])

  if (is.na(crs_ref)) {
    stop("La capa normalizada no tiene CRS valido.")
  }

  pieces <- lapply(
    pieces,
    function(g) {
      if (!isTRUE(sf::st_crs(g) == crs_ref)) {
        g <- sf::st_transform(g, crs_ref)
      }
      g
    }
  )

  out <- do.call(
    rbind,
    pieces
  )

  vector_export_clean_sf(out)
}


clip_vector_export_to_basin <- function(
    x,
    basin
) {

  x <- vector_export_clean_sf(x)

  if (nrow(x) < 1L) {
    return(x)
  }

  b <- sf::st_make_valid(basin)
  b <- b[
    !sf::st_is_empty(b),
    ,
    drop = FALSE
  ]

  if (nrow(b) < 1L) {
    stop("La cuenca esta vacia.")
  }

  b <- sf::st_transform(
    b,
    sf::st_crs(x)
  )

  b_union <- sf::st_union(
    sf::st_geometry(b)
  )

  keep <- lengths(
    sf::st_intersects(
      x,
      b_union
    )
  ) > 0L

  x <- x[
    keep,
    ,
    drop = FALSE
  ]

  if (nrow(x) < 1L) {
    return(x)
  }

  out <- suppressWarnings(
    sf::st_intersection(
      x,
      b_union
    )
  )

  vector_export_clean_sf(out)
}


zip_vector_export_files <- function(
    zip_file,
    files,
    root_dir
) {

  files <- normalizePath(
    files,
    winslash = "/",
    mustWork = TRUE
  )

  root_dir <- normalizePath(
    root_dir,
    winslash = "/",
    mustWork = TRUE
  )

  if (requireNamespace("zip", quietly = TRUE)) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(root_dir)

    zip::zipr(
      zipfile = zip_file,
      files = basename(files),
      recurse = FALSE
    )
  } else {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(root_dir)

    utils::zip(
      zipfile = zip_file,
      files = basename(files),
      flags = "-j"
    )
  }

  if (
    !file.exists(zip_file) ||
    !is.finite(file.info(zip_file)$size) ||
    file.info(zip_file)$size <= 0
  ) {
    stop("No se pudo crear el ZIP de Shapefile.")
  }

  invisible(zip_file)
}


write_shapefile_zip_bundle <- function(
    layers,
    target_file,
    bundle_stem = "capa_normalizada"
) {

  if (!is.list(layers) || length(layers) < 1L) {
    stop("No se recibieron capas para exportar.")
  }

  layer_names <- names(layers)

  if (is.null(layer_names)) {
    layer_names <- paste0("capa_", seq_along(layers))
  }

  work_dir <- tempfile(
    pattern = "shp_export_"
  )

  dir.create(
    work_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  on.exit(
    unlink(
      work_dir,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  written <- character(0)

  for (i in seq_along(layers)) {

    x <- layers[[i]]

    if (
      is.null(x) ||
      !inherits(x, "sf") ||
      nrow(x) < 1L
    ) {
      next
    }

    x <- vector_export_clean_sf(x)

    if (nrow(x) < 1L) {
      next
    }

    layer_stem <- vector_export_safe_stem(
      layer_names[i]
    )

    shp_file <- file.path(
      work_dir,
      paste0(layer_stem, ".shp")
    )

    suppressWarnings(
      sf::st_write(
        x,
        shp_file,
        layer = layer_stem,
        driver = "ESRI Shapefile",
        quiet = TRUE,
        delete_dsn = TRUE
      )
    )

    members <- list.files(
      work_dir,
      full.names = TRUE
    )

    member_stems <- tools::file_path_sans_ext(
      basename(members)
    )

    members <- members[
      tolower(member_stems) == tolower(layer_stem) &
        tolower(tools::file_ext(members)) %in%
          c("shp", "shx", "dbf", "prj", "cpg")
    ]

    required <- c("shp", "shx", "dbf")
    found <- unique(
      tolower(
        tools::file_ext(members)
      )
    )

    if (!all(required %in% found)) {
      stop(
        paste0(
          "El Shapefile '",
          layer_stem,
          "' se genero incompleto."
        )
      )
    }

    written <- c(
      written,
      members
    )
  }

  written <- unique(written)

  if (length(written) < 1L) {
    stop("No hay entidades espaciales para exportar.")
  }

  bundle_stem <- vector_export_safe_stem(
    bundle_stem
  )

  zip_tmp <- file.path(
    tempdir(),
    paste0(
      bundle_stem,
      "_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      "_",
      sample.int(1e8, 1L),
      ".zip"
    )
  )

  on.exit(
    unlink(
      zip_tmp,
      force = TRUE
    ),
    add = TRUE
  )

  zip_vector_export_files(
    zip_file = zip_tmp,
    files = written,
    root_dir = work_dir
  )

  ok <- file.copy(
    zip_tmp,
    target_file,
    overwrite = TRUE
  )

  if (
    !isTRUE(ok) ||
    !file.exists(target_file) ||
    !is.finite(file.info(target_file)$size) ||
    file.info(target_file)$size <= 0
  ) {
    stop("No se pudo entregar el ZIP de Shapefile.")
  }

  invisible(target_file)
}


# ============================================================
# FUENTES DE LAS CAPAS
# ============================================================

.layer_sources_cache <- new.env(parent = emptyenv())


# Normaliza textos del catalogo de fuentes sin convertir cadenas validas en
# secuencias UTF-8 invalidas. El CSV historico puede contener campos Latin-1
# y otros ya codificados en UTF-8, por lo que la reparacion se hace por valor.
layer_source_safe_utf8 <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""

  normalize_one <- function(value) {
    if (!nzchar(value)) {
      return("")
    }

    # Si el objeto ya contiene bytes invalidos para UTF-8, se reinterpretan
    # como Latin-1 y se convierten de forma explicita.
    if (!isTRUE(validUTF8(value))) {
      latin1_value <- value
      Encoding(latin1_value) <- "latin1"

      converted <- suppressWarnings(
        iconv(
          latin1_value,
          from = "latin1",
          to = "UTF-8",
          sub = ""
        )
      )

      if (!is.na(converted)) {
        value <- converted
      }
    }

    # Repara mojibake tipico (p. ej. "DivisiÃ³n") solo cuando la conversion
    # candidata sigue siendo UTF-8 valida. Esto evita corromper celdas mixtas.
    if (
      isTRUE(validUTF8(value)) &&
        grepl("Ã|Â", value, perl = TRUE)
    ) {
      candidate <- suppressWarnings(
        iconv(
          value,
          from = "UTF-8",
          to = "latin1"
        )
      )

      if (!is.na(candidate)) {
        Encoding(candidate) <- "UTF-8"

        if (isTRUE(validUTF8(candidate))) {
          value <- candidate
        }
      }
    }

    # Salvaguarda final para cualquier dispositivo grafico / Shiny.
    if (!isTRUE(validUTF8(value))) {
      latin1_value <- value
      Encoding(latin1_value) <- "latin1"
      value <- suppressWarnings(
        iconv(
          latin1_value,
          from = "latin1",
          to = "UTF-8",
          sub = ""
        )
      )
    }

    if (is.na(value)) "" else value
  }

  out <- vapply(
    x,
    normalize_one,
    character(1),
    USE.NAMES = FALSE
  )

  trimws(
    sub(
      "^\\t",
      "",
      out,
      useBytes = TRUE
    )
  )
}


# Distribucion vertical comun para leyendas categoricas A3.
# Hasta 16 clases se usan siempre 16 ranuras virtuales: la primera clase queda
# anclada arriba y las siguientes crecen hacia abajo sin estirarse para llenar
# todo el panel. Entre 17 y 32 se habilita una segunda columna con el mismo paso.
# Si excepcionalmente hay mas de 32 clases, el paso se reduce solo lo necesario.
categorical_legend_layout <- function(
    n_items,
    max_rows = 16L,
    max_cols = 2L,
    y_top = 0.86,
    y_bottom = 0.06
) {
  n_items <- max(0L, as.integer(n_items)[1])
  max_rows <- max(1L, as.integer(max_rows)[1])
  max_cols <- max(1L, as.integer(max_cols)[1])

  ncols <- if (n_items <= max_rows) {
    1L
  } else {
    min(
      max_cols,
      max(1L, ceiling(n_items / max_rows))
    )
  }

  if (n_items <= max_rows * max_cols) {
    rows_per_col <- max_rows
  } else {
    ncols <- max_cols
    rows_per_col <- ceiling(n_items / ncols)
  }

  dy <- (y_top - y_bottom) / rows_per_col

  cex <- max(
    0.48,
    min(
      0.82,
      0.72 * max_rows / max(rows_per_col, max_rows)
    )
  )

  list(
    ncols = as.integer(ncols),
    rows_per_col = as.integer(rows_per_col),
    y_top = y_top,
    y_bottom = y_bottom,
    dy = dy,
    cex = cex
  )
}


read_layer_sources <- function() {
  if (exists("data", envir = .layer_sources_cache, inherits = FALSE)) {
    return(get("data", envir = .layer_sources_cache, inherits = FALSE))
  }

  required <- c(
    "CAPA_ID",
    "MODULO",
    "NOMBRE_CAPA",
    "INSTITUCION",
    "PRODUCTO",
    "ANIO_VERSION",
    "ESCALA_RESOLUCION",
    "URL",
    "LICENCIA",
    "NOTA_PROCESAMIENTO"
  )

  empty <- as.data.frame(
    stats::setNames(
      replicate(length(required), character(), simplify = FALSE),
      required
    ),
    stringsAsFactors = FALSE
  )

  if (!file.exists(LAYER_SOURCES_CSV)) {
    warning("No existe el catalogo de fuentes: ", LAYER_SOURCES_CSV)
    return(empty)
  }

  header <- readLines(
    LAYER_SOURCES_CSV,
    n = 1L,
    encoding = "latin1",
    warn = FALSE
  )
  separator <- if (grepl(";", header, fixed = TRUE)) ";" else ","

  sources <- tryCatch(
    utils::read.table(
      LAYER_SOURCES_CSV,
      header = TRUE,
      sep = separator,
      quote = "\"",
      fill = TRUE,
      comment.char = "",
      fileEncoding = "latin1",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) {
      warning("No se pudo leer el catalogo de fuentes: ", conditionMessage(e))
      empty
    }
  )

  if (ncol(sources) > 0L) {
    names(sources)[1] <- sub(
      "^\\ufeff|^ï»¿",
      "",
      names(sources)[1]
    )
  }

  missing <- setdiff(required, names(sources))
  if (length(missing) > 0L) {
    warning(
      "El catalogo de fuentes no contiene: ",
      paste(missing, collapse = ", ")
    )
    return(empty)
  }

  sources <- sources[, required, drop = FALSE]

  sources[] <- lapply(
    sources,
    layer_source_safe_utf8
  )
  sources <- sources[nzchar(sources[["CAPA_ID"]]), , drop = FALSE]
  sources <- sources[!duplicated(tolower(sources[["CAPA_ID"]])), , drop = FALSE]
  rownames(sources) <- NULL

  assign("data", sources, envir = .layer_sources_cache)
  sources
}


layer_source_rows <- function(layer_ids = NULL) {
  sources <- read_layer_sources()

  if (is.null(layer_ids)) {
    return(sources)
  }

  positions <- match(
    tolower(as.character(layer_ids)),
    tolower(sources[["CAPA_ID"]])
  )
  positions <- positions[!is.na(positions)]
  sources[positions, , drop = FALSE]
}


layer_source_ui <- function(
    layer_ids = NULL,
    title = "Fuentes de datos",
    show_module = FALSE
) {
  sources <- layer_source_rows(layer_ids)

  if (nrow(sources) < 1L) {
    return(NULL)
  }

  entries <- lapply(seq_len(nrow(sources)), function(i) {
    source <- sources[i, , drop = FALSE]

    field <- function(label, value) {
      if (!nzchar(value)) {
        return(NULL)
      }
      shiny::tags$p(
        shiny::tags$strong(paste0(label, ": ")),
        value
      )
    }

    shiny::tags$article(
      class = "layer-source-entry",
      shiny::tags$div(
        class = "layer-source-name",
        source[["NOMBRE_CAPA"]]
      ),
      if (isTRUE(show_module)) {
        shiny::tags$div(
          class = "layer-source-module",
          source[["MODULO"]]
        )
      },
      field("Institución", source[["INSTITUCION"]]),
      field("Producto", source[["PRODUCTO"]]),
      field("Año / versión", source[["ANIO_VERSION"]]),
      field("Escala / resolución", source[["ESCALA_RESOLUCION"]]),
      field("Procesamiento", source[["NOTA_PROCESAMIENTO"]])
    )
  })

  shiny::tags$details(
    class = "layer-source-details",
    shiny::tags$summary(title),
    shiny::div(
      class = "layer-source-list",
      do.call(shiny::tagList, entries)
    )
  )
}



draw_dynamic_categorical_legend <- function(
    title,
    labels,
    colors,
    map_plot_bottom,
    map_plot_top,
    subtitle = NULL,
    fig_left = 0.805,
    fig_right = 0.985,
    max_rows = 16L,
    line_label = NULL,
    line_color = NULL,
    line_lwd = 1.9
) {
  labels <- as.character(labels)
  colors <- as.character(colors)

  keep <- !is.na(labels) & nzchar(labels)
  labels <- labels[keep]
  colors <- colors[keep]

  nleg <- length(labels)

  if (is.null(subtitle)) {
    subtitle <- paste0(
      nleg,
      " unidades presentes"
    )
  }

  max_rows <- max(
    1L,
    as.integer(max_rows)
  )

  has_line <- (
    length(line_label) == 1L &&
      !is.na(line_label) &&
      nzchar(as.character(line_label))
  )

  ncols <- if (nleg <= max_rows) {
    1L
  } else {
    2L
  }

  per_col <- if (nleg > 0L) {
    ceiling(nleg / ncols)
  } else {
    0L
  }

  full_h <- map_plot_top - map_plot_bottom

  if (!is.finite(full_h) || full_h <= 0) {
    stop("Altura de mapa invalida para la leyenda.")
  }

  # Fracciones respecto a la altura completa del panel de mapa.
  # Con 16 clases, la caja ocupa exactamente la altura completa.
  header_h_ref <- 0.14
  bottom_h_ref <- if (has_line) 0.05 else 0.06
  line_h_ref <- if (has_line) 0.05 else 0.00
  row_h_ref <- if (has_line) 0.0475 else 0.05

  # Para mas de max_rows por columna se comprime solo lo necesario.
  if (per_col > max_rows) {
    available_rows_ref <- 1 - header_h_ref - bottom_h_ref - line_h_ref
    row_h_ref <- available_rows_ref / per_col
  }

  legend_h_ref <- (
    header_h_ref +
      line_h_ref +
      per_col * row_h_ref +
      bottom_h_ref
  )

  legend_h_ref <- min(
    1,
    max(
      0.20,
      legend_h_ref
    )
  )

  legend_bottom <- map_plot_top - full_h * legend_h_ref

  graphics::par(
    fig = c(
      fig_left,
      fig_right,
      legend_bottom,
      map_plot_top
    ),
    mar = c(0, 0, 0, 0),
    new = TRUE,
    xpd = NA
  )

  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0, 1),
    ylim = c(0, 1),
    xaxs = "i",
    yaxs = "i"
  )

  graphics::rect(
    0,
    0,
    1,
    1,
    col = "#FFFDF7",
    border = "#5F5F5F",
    lwd = 1.1
  )

  # Convertir offsets de la caja de referencia a coordenadas
  # internas de la caja dinamica, conservando el tamano fisico.
  rel <- function(ref_value) {
    ref_value / legend_h_ref
  }

  graphics::text(
    0.03,
    1 - rel(0.025),
    labels = title,
    adj = c(0, 1),
    font = 2,
    cex = 1.30
  )

  graphics::text(
    0.03,
    1 - rel(0.075),
    labels = subtitle,
    adj = c(0, 1),
    cex = 0.72,
    col = "grey30"
  )

  current_top_ref <- header_h_ref

  if (has_line) {
    y_line <- 1 - rel(
      current_top_ref + line_h_ref / 2
    )

    graphics::segments(
      0.05,
      y_line,
      0.19,
      y_line,
      col = line_color,
      lwd = line_lwd
    )

    graphics::text(
      0.23,
      y_line,
      labels = as.character(line_label),
      adj = c(0, 0.5),
      cex = 0.70,
      col = "grey20"
    )

    current_top_ref <- current_top_ref + line_h_ref
  }

  cex_leg <- max(
    0.48,
    min(
      0.82,
      0.72 * max_rows / max(per_col, max_rows)
    )
  )

  if (nleg > 0L) {
    for (i in seq_along(labels)) {
      col_id <- floor((i - 1L) / max(per_col, 1L))
      row_id <- (i - 1L) %% max(per_col, 1L)

      x0 <- if (ncols == 1L) {
        0.05
      } else {
        0.04 + 0.49 * col_id
      }

      x1 <- x0 + if (ncols == 1L) {
        0.18
      } else {
        0.14
      }

      tx <- x1 + 0.035

      y <- 1 - rel(
        current_top_ref +
          (row_id + 0.5) * row_h_ref
      )

      bh <- min(
        rel(0.030),
        0.72 * rel(row_h_ref)
      )

      graphics::rect(
        x0,
        y - bh / 2,
        x1,
        y + bh / 2,
        col = colors[i],
        border = NA
      )

      graphics::text(
        tx,
        y,
        labels = labels[i],
        adj = c(0, 0.5),
        cex = cex_leg
      )
    }
  }

  invisible(
    list(
      n_classes = nleg,
      n_columns = ncols,
      rows_per_column = per_col,
      legend_bottom = legend_bottom,
      legend_top = map_plot_top,
      legend_height_fraction = legend_h_ref
    )
  )
}


layer_source_map_label <- function(layer_ids, width = 150L) {
  sources <- layer_source_rows(layer_ids)

  if (nrow(sources) < 1L) {
    return("Fuente de datos no registrada")
  }

  short_institution <- function(x) {
    x <- layer_source_safe_utf8(x)

    # La busqueda del acronimo es deliberadamente bytewise: aun si una fuente
    # externa arrastrara un byte extraño, nunca obliga a gsub/gregexpr a hacer
    # una traduccion a wide string.
    matches <- regmatches(
      x,
      gregexpr(
        "\\([A-Z0-9]{2,}\\)",
        x,
        perl = TRUE,
        useBytes = TRUE
      )
    )[[1]]

    if (length(matches) > 0L && !identical(matches, "")) {
      return(
        paste(
          gsub(
            "[()]",
            "",
            matches,
            useBytes = TRUE
          ),
          collapse = "/"
        )
      )
    }

    x
  }


  descriptions <- vapply(seq_len(nrow(sources)), function(i) {
    source <- sources[i, , drop = FALSE]
    institution <- short_institution(source[["INSTITUCION"]])
    details <- c(
      source[["ANIO_VERSION"]],
      source[["ESCALA_RESOLUCION"]]
    )
    details <- details[nzchar(details)]

    if (nrow(sources) == 1L) {
      product <- source[["PRODUCTO"]]
      main_parts <- c(institution, product)
      main <- paste(main_parts[nzchar(main_parts)], collapse = " — ")
    } else {
      main <- institution
    }

    if (length(details) > 0L) {
      paste0(main, " (", paste(details, collapse = "; "), ")")
    } else {
      main
    }
  }, character(1))

  label <- paste0(
    if (length(descriptions) == 1L) "Fuente: " else "Fuentes: ",
    paste(descriptions, collapse = " | ")
  )

  paste(strwrap(label, width = width), collapse = "\n")
}

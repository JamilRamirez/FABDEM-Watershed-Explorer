from pathlib import Path

# ------------------------------------------------------------
# 1) Robust Runtime downloader: media first, raw fallback only
#    for ordinary Git files. Reject Git LFS pointer fallbacks.
# ------------------------------------------------------------
config_path = Path("R/config.R")
config = config_path.read_text(encoding="utf-8")

start = config.index("runtime_cache_file <- function(path) {")
end = config.index("runtime_file_nonempty <- function(path) {", start)

new_runtime_cache = r'''runtime_is_lfs_pointer <- function(path) {
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


'''

config = config[:start] + new_runtime_cache + config[end:]
config_path.write_text(config, encoding="utf-8")


# ------------------------------------------------------------
# 2) Geology and geomorphology: index.gpkg is the runtime
#    contract. TILING_READY.txt is a build marker and should not
#    be required by the deployed app. Download only the selected
#    tile GPKGs after spatial index selection.
# ------------------------------------------------------------
def patch_thematic(path_str, prefix, label):
    path = Path(path_str)
    text = path.read_text(encoding="utf-8")

    index_fn = f"{prefix}_index <- function() {{"
    read_fn = f"read_{prefix}_for_basin <- function(basin) {{"

    i0 = text.index(index_fn)
    i1 = text.index(read_fn, i0)

    if prefix == "geology":
        index_label = "Geología"
        index_fun = r'''geology_index <- function() {
    index_file <- file.path(
      geology_tiled_dir(),
      "index.gpkg"
    )

    index_file <- tryCatch(
      runtime_cache_file(index_file),
      error = function(e) {
        stop(
          paste0(
            "No se pudo cargar el índice teselado de Geología.\n",
            conditionMessage(e)
          )
        )
      }
    )

    idx <- sf::st_read(
      index_file,
      layer = "tiles",
      quiet = TRUE
    )

    if (!"RELATIVE_PATH" %in% names(idx)) {
      stop(
        "El index.gpkg de Geología no contiene RELATIVE_PATH."
      )
    }

    idx
  }


'''
    else:
        index_label = "Geomorfología"
        index_fun = r'''geomorphology_index <- function() {
    index_file <- file.path(
      geomorphology_tiled_dir(),
      "index.gpkg"
    )

    index_file <- tryCatch(
      runtime_cache_file(index_file),
      error = function(e) {
        stop(
          paste0(
            "No se pudo cargar el índice teselado de Geomorfología.\n",
            conditionMessage(e)
          )
        )
      }
    )

    idx <- sf::st_read(
      index_file,
      layer = "tiles",
      quiet = TRUE
    )

    if (!"RELATIVE_PATH" %in% names(idx)) {
      stop(
        "El index.gpkg de Geomorfología no contiene RELATIVE_PATH."
      )
    }

    idx
  }


'''

    text = text[:i0] + index_fun + text[i1:]

    selected_anchor = '''      selected_tiles <- vapply(
        as.character(
          idx_hit$RELATIVE_PATH
        ),
        safe_layers_path,
        character(1)
      )
'''

    if selected_anchor not in text:
        raise SystemExit(f"Could not locate selected tile block in {path_str}")

    selected_replacement = selected_anchor + f'''

      selected_tiles <- vapply(
        selected_tiles,
        function(tile_path) {{
          tryCatch(
            runtime_cache_file(tile_path),
            error = function(e) {{
              stop(
                paste0(
                  "No se pudo descargar una tesela de {index_label}.\\n",
                  conditionMessage(e)
                )
              )
            }}
          )
        }},
        character(1)
      )
'''

    text = text.replace(selected_anchor, selected_replacement, 1)

    path.write_text(text, encoding="utf-8")


patch_thematic("R/geologia.R", "geology", "Geología")
patch_thematic("R/geomorfologia.R", "geomorphology", "Geomorfología")


# ------------------------------------------------------------
# 3) Text-level validation
# ------------------------------------------------------------
config = Path("R/config.R").read_text(encoding="utf-8")
geol = Path("R/geologia.R").read_text(encoding="utf-8")
geom = Path("R/geomorfologia.R").read_text(encoding="utf-8")

checks = [
    ("runtime_raw_base_url <- function()", config),
    ("runtime_is_lfs_pointer <- function(path)", config),
    ("puntero Git LFS, no contenido", config),
    ("No se pudo cargar el índice teselado de Geología", geol),
    ("No se pudo descargar una tesela de Geología", geol),
    ("No se pudo cargar el índice teselado de Geomorfología", geom),
    ("No se pudo descargar una tesela de Geomorfología", geom),
]

for needle, haystack in checks:
    if needle not in haystack:
        raise SystemExit(f"Validation failed: {needle}")

# Build marker must no longer gate either spatial index.
geol_idx = geol[geol.index("geology_index <- function() {"):geol.index("read_geology_for_basin <- function(basin) {")]
geom_idx = geom[geom.index("geomorphology_index <- function() {"):geom.index("read_geomorphology_for_basin <- function(basin) {")]

if "TILING_READY.txt" in geol_idx or "TILING_READY.txt" in geom_idx:
    raise SystemExit("TILING_READY.txt still gates a thematic index")

print("Patched Runtime download fallback and thematic tile lazy loading")

from pathlib import Path

path = Path("R/helpers.R")
text = path.read_text(encoding="utf-8")
original = text

# 1) Manifest resolution must only construct a safe local cache path.
#    Downloading here caused every reverse + stream stripe in a block
#    to be fetched before delineation even started.
start = text.index("runtime_asset_path <- function(")
end = text.index("# ============================================================\n# 2. CARGAR CATALOGO LOCAL", start)
block = text[start:end]
old = "  runtime_cache_file(candidate_norm)\n"
if old in block:
    block = block.replace(old, "  candidate_norm\n", 1)
elif "  candidate_norm\n" not in block:
    raise SystemExit("Could not patch runtime_asset_path()")
text = text[:start] + block + text[end:]

# 2) Validate manifest structure, not local existence. Assets are now
#    intentionally absent until the stripe is actually requested.
start = text.index("validate_stripe_sequence <- function(")
end = text.index("# ============================================================\n# 5. CACHE STREAM STRIPES", start)
new_validate = '''validate_stripe_sequence <- function(
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


  local_paths <- as.character(
    rows[["LOCAL_PATH"]]
  )


  if (
    length(local_paths) != expected_n ||
    anyNA(local_paths) ||
    any(!nzchar(local_paths))
  ) {
    stop(
      paste0(
        block_id,
        ": rutas locales invalidas en ",
        label,
        "."
      )
    )
  }


  invisible(TRUE)
}


'''
text = text[:start] + new_validate + text[end:]

# 3) Stream stripes: download only on the first cache miss for that stripe.
start = text.index("load_stream_stripe <- function(")
end = text.index("# ============================================================\n# 6. CACHE REVERSE STRIPES", start)
block = text[start:end]
anchor = '''  path <- cache$rows[["LOCAL_PATH"]][
    stripe_id
  ]
'''
if anchor not in block:
    raise SystemExit("Could not locate stream stripe path")
if "  path <- runtime_cache_file(\n    path\n  )\n" not in block:
    replacement = anchor + '''

  path <- runtime_cache_file(
    path
  )
'''
    block = block.replace(anchor, replacement, 1)
text = text[:start] + block + text[end:]

# 4) Reverse stripes: same lazy fetch behavior. The in-memory raw LRU stays
#    unchanged, so repeated accesses do not re-read or re-download the stripe.
start = text.index("load_reverse_stripe <- function(")
end = text.index("get_reverse_values <- function(", start)
block = text[start:end]
old_rast = '''  r <- terra::rast(
    cache$stripe_files[
      stripe_id
    ]
  )
'''
new_rast = '''  stripe_path <- runtime_cache_file(
    cache$stripe_files[[
      stripe_id
    ]]
  )


  r <- terra::rast(
    stripe_path
  )
'''
if old_rast in block:
    block = block.replace(old_rast, new_rast, 1)
elif "  stripe_path <- runtime_cache_file(" not in block:
    raise SystemExit("Could not patch load_reverse_stripe()")
text = text[:start] + block + text[end:]

# Validation of the intended architecture.
runtime_start = text.index("runtime_asset_path <- function(")
runtime_end = text.index("# ============================================================\n# 2. CARGAR CATALOGO LOCAL", runtime_start)
runtime_block = text[runtime_start:runtime_end]
if "runtime_cache_file(candidate_norm)" in runtime_block:
    raise SystemExit("runtime_asset_path() still downloads assets")

validate_start = text.index("validate_stripe_sequence <- function(")
validate_end = text.index("# ============================================================\n# 5. CACHE STREAM STRIPES", validate_start)
validate_block = text[validate_start:validate_end]
if "file_nonempty(" in validate_block:
    raise SystemExit("validate_stripe_sequence() still requires downloaded files")

stream_start = text.index("load_stream_stripe <- function(")
stream_end = text.index("# ============================================================\n# 6. CACHE REVERSE STRIPES", stream_start)
if "path <- runtime_cache_file(" not in text[stream_start:stream_end]:
    raise SystemExit("Stream stripes are not lazily downloaded")

reverse_start = text.index("load_reverse_stripe <- function(")
reverse_end = text.index("get_reverse_values <- function(", reverse_start)
if "stripe_path <- runtime_cache_file(" not in text[reverse_start:reverse_end]:
    raise SystemExit("Reverse stripes are not lazily downloaded")

if text == original:
    print("helpers.R already had the lazy runtime architecture")
else:
    path.write_text(text, encoding="utf-8")
    print("Patched R/helpers.R for true per-stripe lazy loading")

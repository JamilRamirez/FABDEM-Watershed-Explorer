from pathlib import Path
import re

ROOT = Path('.')


def replace_once(text, old, new, label):
    n = text.count(old)
    if n != 1:
        raise SystemExit(f"{label}: expected 1 match, found {n}")
    return text.replace(old, new, 1)


def find_matching_paren(text, open_idx):
    depth = 0
    quote = None
    escape = False
    i = open_idx
    while i < len(text):
        ch = text[i]
        if quote is not None:
            if escape:
                escape = False
            elif ch == '\\':
                escape = True
            elif ch == quote:
                quote = None
            i += 1
            continue
        if ch in ('"', "'"):
            quote = ch
            i += 1
            continue
        if ch == '#':
            nl = text.find('\n', i)
            if nl == -1:
                return None
            i = nl + 1
            continue
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
            if depth == 0:
                return i
        i += 1
    return None


# ------------------------------------------------------------------
# 1. Helper compartido: reproyecta todos los SHP del bundle.
# ------------------------------------------------------------------
hp = ROOT / 'R' / 'helpers.R'
h = hp.read_text(encoding='utf-8')
old_sig = '''write_shapefile_zip_bundle <- function(\n    layers,\n    target_file,\n    bundle_stem = "capa_normalizada"\n) {'''
new_sig = '''write_shapefile_zip_bundle <- function(\n    layers,\n    target_file,\n    bundle_stem = "capa_normalizada",\n    crs_mode = "utm"\n) {'''
h = replace_once(h, old_sig, new_sig, 'helpers signature')
anchor = '''  if (!is.list(layers) || length(layers) < 1L) {\n    stop("No se recibieron capas para exportar.")\n  }\n\n  layer_names <- names(layers)'''
insert = '''  if (!is.list(layers) || length(layers) < 1L) {\n    stop("No se recibieron capas para exportar.")\n  }\n\n  crs_mode <- match.arg(\n    as.character(crs_mode)[1],\n    c(\n      "utm",\n      "wgs84"\n    )\n  )\n\n  valid_layer_ids <- which(\n    vapply(\n      layers,\n      function(x) {\n        inherits(x, "sf") &&\n          nrow(x) > 0L &&\n          !is.na(sf::st_crs(x))\n      },\n      logical(1)\n    )\n  )\n\n  if (length(valid_layer_ids) < 1L) {\n    stop("No hay capas sf con CRS valido para exportar.")\n  }\n\n  reference_layer <- vector_export_clean_sf(\n    layers[[valid_layer_ids[1L]]]\n  )\n\n  reference_wgs84 <- sf::st_transform(\n    reference_layer,\n    4326\n  )\n\n  target_epsg <- if (identical(crs_mode, "wgs84")) {\n    4326L\n  } else {\n    center_geom <- suppressWarnings(\n      sf::st_point_on_surface(\n        sf::st_union(\n          sf::st_geometry(reference_wgs84)\n        )\n      )\n    )\n\n    center_xy <- sf::st_coordinates(center_geom)\n\n    if (\n      nrow(center_xy) < 1L ||\n      !all(is.finite(center_xy[1L, c("X", "Y")]))\n    ) {\n      stop("No se pudo determinar la zona UTM para el Shapefile tematico.")\n    }\n\n    as.integer(\n      utm_epsg_point(\n        lon = center_xy[1L, "X"],\n        lat = center_xy[1L, "Y"]\n      )\n    )\n  }\n\n  layers <- lapply(\n    layers,\n    function(x) {\n      if (\n        is.null(x) ||\n        !inherits(x, "sf") ||\n        nrow(x) < 1L\n      ) {\n        return(x)\n      }\n\n      x <- vector_export_clean_sf(x)\n\n      if (nrow(x) < 1L) {\n        return(x)\n      }\n\n      sf::st_transform(\n        x,\n        target_epsg\n      )\n    }\n  )\n\n  layer_names <- names(layers)'''
h = replace_once(h, anchor, insert, 'helpers CRS block')
hp.write_text(h, encoding='utf-8')


# ------------------------------------------------------------------
# 2. Modulos tematicos con botones SHP.
# ------------------------------------------------------------------
targets = []
for path in sorted((ROOT / 'R').glob('*.R')):
    if path.name in {'helpers.R', 'delimitacion.R', 'morfometria.R'}:
        continue
    text = path.read_text(encoding='utf-8')
    if 'descargar_shp_' not in text:
        continue

    if 'write_shapefile_zip_bundle(' not in text:
        raise SystemExit(f"{path}: tiene exportacion SHP pero no usa write_shapefile_zip_bundle")

    targets.append(path)

    if 'ns("crs_descarga_shp")' not in text:
        m = re.search(
            r'(?P<indent>^[ \t]+)shiny::downloadButton\(\s*\n\s*ns\("descargar_shp_[^"]+"\)',
            text,
            flags=re.M,
        )
        if not m:
            raise SystemExit(f"{path}: no se encontro el primer boton SHP para insertar selector CRS")
        indent = m.group('indent')
        selector = (
            f'{indent}shiny::selectInput(\n'
            f'{indent}  ns("crs_descarga_shp"),\n'
            f'{indent}  label = "CRS de los Shapefile",\n'
            f'{indent}  choices = c(\n'
            f'{indent}    "UTM automática (metros)" = "utm",\n'
            f'{indent}    "WGS84 / EPSG:4326 (grados)" = "wgs84"\n'
            f'{indent}  ),\n'
            f'{indent}  selected = "utm",\n'
            f'{indent}  width = "230px"\n'
            f'{indent}),\n\n'
        )
        text = text[:m.start()] + selector + text[m.start():]

    # Añadir crs_mode a todas las llamadas del helper dentro del modulo.
    search_from = 0
    patches = 0
    while True:
        idx = text.find('write_shapefile_zip_bundle(', search_from)
        if idx < 0:
            break
        open_idx = idx + len('write_shapefile_zip_bundle')
        close_idx = find_matching_paren(text, open_idx)
        if close_idx is None:
            raise SystemExit(f"{path}: llamada write_shapefile_zip_bundle sin cierre")
        segment = text[idx:close_idx+1]
        if 'crs_mode' not in segment:
            line_start = text.rfind('\n', 0, close_idx) + 1
            closing_indent = re.match(r'[ \t]*', text[line_start:close_idx]).group(0)
            insertion = (
                ',\n'
                f'{closing_indent}  crs_mode = shiny::isolate(\n'
                f'{closing_indent}    input$crs_descarga_shp\n'
                f'{closing_indent}  )\n'
                f'{closing_indent}'
            )
            text = text[:close_idx] + insertion + text[close_idx:]
            close_idx += len(insertion)
            patches += 1
        search_from = close_idx + 1

    if patches < 1 and 'crs_mode = shiny::isolate(' not in text:
        raise SystemExit(f"{path}: no se parcheo ninguna llamada SHP")

    path.write_text(text, encoding='utf-8')

if not targets:
    raise SystemExit('No se encontraron modulos tematicos con exportacion SHP')

# Verificaciones posteriores.
report = []
for path in targets:
    text = path.read_text(encoding='utf-8')
    n_buttons = text.count('descargar_shp_')
    n_calls = text.count('write_shapefile_zip_bundle(')
    n_crs = text.count('crs_mode = shiny::isolate(')
    if 'ns("crs_descarga_shp")' not in text:
        raise SystemExit(f"{path}: falta selector CRS")
    if n_crs != n_calls:
        raise SystemExit(f"{path}: {n_calls} llamadas SHP pero {n_crs} argumentos crs_mode")
    report.append(f"{path.as_posix()} | shp_refs={n_buttons} | writer_calls={n_calls} | crs_calls={n_crs}")

Path('THEMATIC_SHP_CRS_PATCH_REPORT.txt').write_text(
    'TARGETS=' + str(len(targets)) + '\n' + '\n'.join(report) + '\n',
    encoding='utf-8'
)

print('Patched thematic SHP CRS in:')
for p in targets:
    print(' -', p)

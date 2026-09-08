from pathlib import Path

p = Path('R/delimitacion.R')
text = p.read_text(encoding='utf-8')

const = '''  AMBIGUITY_WIDE_MIN_PARALLEL <- 0.20\n  AMBIGUITY_MAX_OPTIONS <- 3L\n'''
helper = '''  AMBIGUITY_WIDE_MIN_PARALLEL <- 0.20\n  AMBIGUITY_MAX_OPTIONS <- 3L\n\n\n  ambiguity_snap_distance_m <- function(primary, default = 0) {\n\n    value <- suppressWarnings(\n      as.numeric(primary$snap_distance_m)\n    )\n\n    if (length(value) < 1L || !is.finite(value[1])) {\n      return(default)\n    }\n\n    as.numeric(value[1])\n  }\n'''

if 'ambiguity_snap_distance_m <- function' not in text:
    if const not in text:
        raise SystemExit('No se encontro bloque de constantes de ambiguedad')
    text = text.replace(const, helper, 1)

old_confluence = '''    primary_distance <- if (\n      !is.null(primary) && is.finite(primary$snap_distance_m)\n    ) primary$snap_distance_m else 0\n'''
new_confluence = '''    primary_distance <- ambiguity_snap_distance_m(\n      primary,\n      default = 0\n    )\n'''
if old_confluence in text:
    text = text.replace(old_confluence, new_confluence, 1)

old_wide = '''    if (\n      is.null(primary) ||\n      !is.finite(primary$snap_distance_m) ||\n      primary$snap_distance_m < AMBIGUITY_WIDE_TRIGGER_M\n    ) {\n      return(NULL)\n    }\n'''
new_wide = '''    primary_distance <- ambiguity_snap_distance_m(\n      primary,\n      default = NA_real_\n    )\n\n    if (\n      is.null(primary) ||\n      !is.finite(primary_distance) ||\n      primary_distance < AMBIGUITY_WIDE_TRIGGER_M\n    ) {\n      return(NULL)\n    }\n'''
if old_wide in text:
    text = text.replace(old_wide, new_wide, 1)

if old_confluence in text or old_wide in text:
    raise SystemExit('Quedaron patrones inseguros sin reemplazar')

p.write_text(text, encoding='utf-8')
print('Distance NULL guard applied')

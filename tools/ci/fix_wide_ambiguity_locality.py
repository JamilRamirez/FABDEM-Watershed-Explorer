from pathlib import Path

path = Path('R/delimitacion.R')
text = path.read_text(encoding='utf-8')

old = """      merge_distance <- primary_path$click_distance_m[merge$index_a]\n      if (!is.finite(merge_distance) || merge_distance > AMBIGUITY_WIDE_PATH_M) {\n        next\n      }\n\n      parallel <- ambiguity_direction_similarity(primary_path, path_now)\n"""

new = """      merge_distance <- primary_path$click_distance_m[merge$index_a]\n\n      # En un rio ancho/multicanal, una segunda trayectoria solo\n      # representa una ambiguedad real del clic si la reunion D8\n      # ocurre todavia en el entorno local. Un canal que converge\n      # mucho mas abajo puede ser hidrologicamente relacionado,\n      # pero no compite con el outlet que el usuario senalo.\n      merge_limit_m <- min(\n        AMBIGUITY_WIDE_PATH_M,\n        max(700, 2 * primary_distance)\n      )\n\n      if (!is.finite(merge_distance) || merge_distance > merge_limit_m) {\n        next\n      }\n\n      parallel <- ambiguity_direction_similarity(primary_path, path_now)\n"""

if new in text:
    print('wide ambiguity locality patch already applied')
    raise SystemExit(0)

if old not in text:
    raise SystemExit('expected wide ambiguity merge block not found')

path.write_text(text.replace(old, new, 1), encoding='utf-8')
print('applied local D8 merge gate for wide ambiguity')

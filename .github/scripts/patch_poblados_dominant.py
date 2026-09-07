from pathlib import Path

path = Path("R/poblados.R")
text = path.read_text(encoding="utf-8")
original = text

# ------------------------------------------------------------
# 1) Helper: territorio mas frecuente con manejo determinista
#    de empates y etiqueta compacta para el panel lateral.
# ------------------------------------------------------------
anchor = '''  build_poblados_result <- function(basin) {
'''
helper = r'''  dominant_territory <- function(x) {
    values <- x[
      !is.na(x) &
        nzchar(x)
    ]

    if (length(values) == 0L) {
      return(
        list(
          name = NA_character_,
          count = 0L
        )
      )
    }

    counts <- sort(
      table(values),
      decreasing = TRUE
    )

    max_count <- as.integer(
      counts[1L]
    )

    winners <- sort(
      names(
        counts[
          counts == max_count
        ]
      )
    )

    label <- if (length(winners) <= 2L) {
      paste(
        winners,
        collapse = " / "
      )
    } else {
      paste0(
        winners[1L],
        " / ",
        winners[2L],
        " +",
        length(winners) - 2L
      )
    }

    list(
      name = label,
      count = max_count
    )
  }


  build_poblados_result <- function(basin) {
'''

if "  dominant_territory <- function(x) {" not in text:
    if anchor not in text:
        raise SystemExit("Could not locate build_poblados_result()")
    text = text.replace(anchor, helper, 1)

# ------------------------------------------------------------
# 2) Empty result: expose the same fields as a populated result.
# ------------------------------------------------------------
old_empty = '''          concentration_district = NA_character_,
          concentration_count = 0L,
          epsg = basin_utm_epsg(
'''
new_empty = '''          concentration_district = NA_character_,
          concentration_count = 0L,
          dominant_department = NA_character_,
          dominant_department_count = 0L,
          dominant_province = NA_character_,
          dominant_province_count = 0L,
          dominant_district = NA_character_,
          dominant_district_count = 0L,
          epsg = basin_utm_epsg(
'''
if old_empty in text:
    text = text.replace(old_empty, new_empty, 1)
elif "          dominant_department = NA_character_," not in text:
    raise SystemExit("Could not patch empty result fields")

# ------------------------------------------------------------
# 3) Compute dominant department/province/district by number of
#    population centres inside the active basin.
# ------------------------------------------------------------
calc_anchor = '''    concentration_count <- if (
      length(district_counts) > 0L
    ) {
      as.integer(
        district_counts[1L]
      )
    } else {
      0L
    }

    list(
'''
calc_new = '''    concentration_count <- if (
      length(district_counts) > 0L
    ) {
      as.integer(
        district_counts[1L]
      )
    } else {
      0L
    }

    dominant_department <- dominant_territory(
      hit$DEP
    )

    dominant_province <- dominant_territory(
      hit$PROV
    )

    dominant_district <- dominant_territory(
      hit$DIST
    )

    list(
'''
if "    dominant_department <- dominant_territory(" not in text:
    if calc_anchor not in text:
        raise SystemExit("Could not locate concentration_count calculation")
    text = text.replace(calc_anchor, calc_new, 1)

# ------------------------------------------------------------
# 4) Publish dominance fields in result object.
# ------------------------------------------------------------
old_result = '''      concentration_district = concentration_district,
      concentration_count = concentration_count,
      epsg = basin_utm_epsg(
'''
new_result = '''      concentration_district = concentration_district,
      concentration_count = concentration_count,
      dominant_department = dominant_department$name,
      dominant_department_count = dominant_department$count,
      dominant_province = dominant_province$name,
      dominant_province_count = dominant_province$count,
      dominant_district = dominant_district$name,
      dominant_district_count = dominant_district$count,
      epsg = basin_utm_epsg(
'''
if old_result in text:
    text = text.replace(old_result, new_result, 1)
elif "      dominant_department = dominant_department$name," not in text:
    raise SystemExit("Could not publish dominance fields")

# ------------------------------------------------------------
# 5) Replace the old single-district concentration block with a
#    vertically stacked territorial dominance summary.
# ------------------------------------------------------------
start_marker = '''    graphics::text(
      0.04,
      0.565,
      labels = "Concentración",
'''
end_marker = '''    graphics::par(
      fig = c(
        0,
        1,
        0,
        1
      ),
'''

start = text.find(start_marker)
if start == -1:
    if 'labels = "Concentración territorial"' not in text:
        raise SystemExit("Could not locate old concentration panel")
else:
    end = text.find(end_marker, start)
    if end == -1:
        raise SystemExit("Could not locate end of concentration panel")

    new_panel = r'''    graphics::text(
      0.04,
      0.565,
      labels = "Concentración territorial",
      adj = c(
        0,
        1
      ),
      font = 2,
      cex = 0.76
    )

    draw_dominant_level <- function(
        y,
        label,
        name,
        count
    ) {
      graphics::text(
        0.04,
        y,
        labels = label,
        adj = c(
          0,
          1
        ),
        font = 2,
        cex = 0.60,
        col = "grey30"
      )

      value_name <- if (
        length(name) == 1L &&
        !is.na(name) &&
        nzchar(name)
      ) {
        name
      } else {
        "Sin dato nominal"
      }

      graphics::text(
        0.04,
        y - 0.038,
        labels = value_name,
        adj = c(
          0,
          1
        ),
        cex = 0.66,
        col = "grey20"
      )

      graphics::text(
        0.04,
        y - 0.072,
        labels = paste0(
          as.integer(count),
          " centros poblados"
        ),
        adj = c(
          0,
          1
        ),
        cex = 0.59,
        col = "grey45"
      )
    }

    draw_dominant_level(
      0.515,
      "Departamento",
      x$dominant_department,
      x$dominant_department_count
    )

    draw_dominant_level(
      0.395,
      "Provincia",
      x$dominant_province,
      x$dominant_province_count
    )

    draw_dominant_level(
      0.275,
      "Distrito",
      x$dominant_district,
      x$dominant_district_count
    )

'''
    text = text[:start] + new_panel + text[end:]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------
required = [
    "dominant_territory <- function(x)",
    "dominant_department = dominant_department$name",
    "dominant_province = dominant_province$name",
    "dominant_district = dominant_district$name",
    'labels = "Concentración territorial"',
    '"Departamento"',
    '"Provincia"',
    '"Distrito"',
]
missing = [x for x in required if x not in text]
if missing:
    raise SystemExit(f"Patch validation failed: {missing}")

if text == original:
    print("R/poblados.R already contains territorial dominance panel")
else:
    path.write_text(text, encoding="utf-8")
    print("Patched R/poblados.R with dominant department/province/district summary")

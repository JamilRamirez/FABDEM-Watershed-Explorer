from pathlib import Path

path = Path("R/morfometria.R")
text = path.read_text(encoding="utf-8")

anchor = "          uy <- usr[4] - usr[3]\n"
if "          score_epsg <- suppressWarnings(" not in text:
    if text.count(anchor) != 1:
        raise SystemExit(f"Expected one map usr anchor, found {text.count(anchor)}")
    insertion = r'''

          # --------------------------------------------------
          # CRS metrico para el scoring de elementos cartograficos.
          # El scoring se lleva al UTM de la cuenca para evitar
          # distancias geograficas que dependan de lwgeom y para
          # mantener la normalizacion enteramente en metros.
          # --------------------------------------------------

          score_epsg <- suppressWarnings(
            as.integer(
              x$geom$epsg
            )
          )

          if (
            length(score_epsg) != 1L ||
            is.na(score_epsg) ||
            !is.finite(score_epsg)
          ) {
            stop(
              "No se pudo determinar el CRS UTM para posicionar los elementos cartograficos."
            )
          }

          score_crs <- sf::st_crs(
            score_epsg
          )

          basin_score <- sf::st_transform(
            sf::st_make_valid(
              basin_plot
            ),
            score_crs
          )

          basin_score_geom <- sf::st_union(
            sf::st_geometry(
              basin_score
            )
          )

          map_corners <- sf::st_as_sf(
            data.frame(
              X = c(
                usr[1],
                usr[2]
              ),
              Y = c(
                usr[3],
                usr[4]
              )
            ),
            coords = c(
              "X",
              "Y"
            ),
            crs = dem_crs
          )

          map_corners <- sf::st_transform(
            map_corners,
            score_crs
          )

          map_corners_xy <- sf::st_coordinates(
            map_corners
          )

          map_diag_m <- sqrt(
            (
              map_corners_xy[2, "X"] -
                map_corners_xy[1, "X"]
            )^2 +
              (
                map_corners_xy[2, "Y"] -
                  map_corners_xy[1, "Y"]
              )^2
          )
'''
    text = text.replace(anchor, anchor + insertion, 1)

basin_start = text.index("          basin_overlap_fraction <- function(")
basin_end = text.index("          line_hit_fraction <- function(", basin_start)
basin_block = text[basin_start:basin_end]
old_inside = '''            inside <- lengths(\n              sf::st_intersects(\n                pts,\n                basin_plot\n              )\n            ) > 0L\n'''
new_inside = '''            pts <- sf::st_transform(\n              pts,\n              score_crs\n            )\n\n            inside <- lengths(\n              sf::st_intersects(\n                pts,\n                basin_score_geom\n              )\n            ) > 0L\n'''
if old_inside in basin_block:
    basin_block = basin_block.replace(old_inside, new_inside, 1)
    text = text[:basin_start] + basin_block + text[basin_end:]
elif "                basin_score_geom" not in basin_block:
    raise SystemExit("Could not patch basin_overlap_fraction()")

box_start = text.index("          box_clearance_fraction <- function(")
box_end = text.index("          score_map_box <- function(", box_start)
current_box = text[box_start:box_end]
new_box = r'''          box_clearance_fraction <- function(
              box
          ) {

            ring <- matrix(
              c(
                box[["xmin"]], box[["ymin"]],
                box[["xmax"]], box[["ymin"]],
                box[["xmax"]], box[["ymax"]],
                box[["xmin"]], box[["ymax"]],
                box[["xmin"]], box[["ymin"]]
              ),
              ncol = 2,
              byrow = TRUE
            )

            box_sf <- sf::st_sfc(
              sf::st_polygon(
                list(
                  ring
                )
              ),
              crs = dem_crs
            )

            box_score <- sf::st_transform(
              box_sf,
              score_crs
            )

            d_m <- suppressWarnings(
              as.numeric(
                sf::st_distance(
                  box_score,
                  basin_score_geom
                )[1]
              )
            )

            if (
              !is.finite(d_m) ||
              !is.finite(map_diag_m) ||
              map_diag_m <= 0
            ) {
              return(0)
            }

            pmin(
              1,
              d_m / (
                0.12 * map_diag_m
              )
            )
          }

'''
if "            basin_geom <- sf::st_union(" in current_box or "                  box_sf," in current_box:
    text = text[:box_start] + new_box + text[box_end:]
elif "                  box_score," not in current_box:
    raise SystemExit("Could not patch box_clearance_fraction()")

required = [
    "score_epsg <- suppressWarnings(",
    "basin_score_geom <- sf::st_union(",
    "map_diag_m <- sqrt(",
    "box_score <- sf::st_transform(",
]
missing = [item for item in required if item not in text]
if missing:
    raise SystemExit(f"Patch validation failed: {missing}")

if "sf::st_distance(\n                  box_sf,\n                  basin_geom" in text:
    raise SystemExit("Old geographic st_distance() block still present")

path.write_text(text, encoding="utf-8")

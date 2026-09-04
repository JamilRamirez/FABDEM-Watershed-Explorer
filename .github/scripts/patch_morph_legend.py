from pathlib import Path

path = Path("R/morfometria.R")
text = path.read_text(encoding="utf-8")
original = text

# Widen the right-hand legend column of the A3 morphometry sheet.
# Keep the temp extent device and the real plotting device consistent.
main_panel_old = '''            fig = c(
              0.025,
              0.79,
              0.07,
              0.97
            ),'''
main_panel_new = '''            fig = c(
              0.025,
              0.735,
              0.07,
              0.97
            ),'''

# The same panel definition also exists in the A3 extent helper with
# a slightly different indentation level.
main_panel_old_2 = '''      fig = c(
        0.025,
        0.79,
        0.07,
        0.97
      ),'''
main_panel_new_2 = '''      fig = c(
        0.025,
        0.735,
        0.07,
        0.97
      ),'''

right_top_old = '''            fig = c(
              0.81,
              0.985,
              map_plot_bottom + 0.57 * map_plot_height,
              map_plot_top
            ),'''
right_top_new = '''            fig = c(
              0.755,
              0.985,
              map_plot_bottom + 0.57 * map_plot_height,
              map_plot_top
            ),'''

right_bottom_old = '''            fig = c(
              0.81,
              0.985,
              map_plot_bottom,
              map_plot_bottom + 0.52 * map_plot_height
            ),'''
right_bottom_new = '''            fig = c(
              0.755,
              0.985,
              map_plot_bottom,
              map_plot_bottom + 0.52 * map_plot_height
            ),'''

replacements = [
    (main_panel_old, main_panel_new, "main map panel"),
    (main_panel_old_2, main_panel_new_2, "A3 extent panel"),
    (right_top_old, right_top_new, "upper right legend"),
    (right_bottom_old, right_bottom_new, "lower right legend"),
]

for old, new, label in replacements:
    if old in text:
        text = text.replace(old, new, 1)
    elif new not in text:
        raise SystemExit(f"Could not patch {label}")

# Make the symbol-to-text allocation inside the lower legend slightly more
# efficient without reducing font size. This affects only the long main-channel
# label and keeps all other entries aligned as before.
long_label_old = '''          graphics::text(
            0.40,
            0.84,
            labels = "Cauce principal (contorno)",'''
long_label_new = '''          graphics::text(
            0.35,
            0.84,
            labels = "Cauce principal (contorno)",'''

if long_label_old in text:
    text = text.replace(long_label_old, long_label_new, 1)
elif long_label_new not in text:
    raise SystemExit("Could not shift long main-channel legend label")

# Shorten the sample line slightly so the longer label gains extra room.
segment_old = '''          graphics::segments(
            0.04,
            0.84,
            0.34,
            0.84,'''
segment_new = '''          graphics::segments(
            0.04,
            0.84,
            0.29,
            0.84,'''

if segment_old in text:
    text = text.replace(segment_old, segment_new, 1)
elif segment_new not in text:
    raise SystemExit("Could not shorten main-channel legend sample")

if text == original:
    print("morfometria.R already contains the widened legend layout")
else:
    path.write_text(text, encoding="utf-8")
    print("Patched morphometry A3 layout: wider right legend column")

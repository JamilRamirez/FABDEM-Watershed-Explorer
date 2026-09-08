from pathlib import Path

path = Path("R/poblados.R")
text = path.read_text(encoding="utf-8")
original = text

old = '''                    concentration_district = info$concentration_district,
                    concentration_count = info$concentration_count,
                    epsg = info$epsg
'''
new = '''                    concentration_district = info$concentration_district,
                    concentration_count = info$concentration_count,
                    dominant_department = info$dominant_department,
                    dominant_department_count = info$dominant_department_count,
                    dominant_province = info$dominant_province,
                    dominant_province_count = info$dominant_province_count,
                    dominant_district = info$dominant_district,
                    dominant_district_count = info$dominant_district_count,
                    epsg = info$epsg
'''

if old in text:
    text = text.replace(old, new, 1)
elif '                    dominant_department = info$dominant_department,' not in text:
    raise SystemExit("Could not locate server result copy block")

required = [
    'dominant_department = info$dominant_department',
    'dominant_department_count = info$dominant_department_count',
    'dominant_province = info$dominant_province',
    'dominant_province_count = info$dominant_province_count',
    'dominant_district = info$dominant_district',
    'dominant_district_count = info$dominant_district_count',
]
missing = [x for x in required if x not in text]
if missing:
    raise SystemExit(f"Patch validation failed: {missing}")

if text == original:
    print("R/poblados.R already propagates dominant territory fields")
else:
    path.write_text(text, encoding="utf-8")
    print("Patched poblados server to propagate dominant territory fields")

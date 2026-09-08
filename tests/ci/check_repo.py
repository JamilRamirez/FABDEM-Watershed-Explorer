from __future__ import annotations

import csv
import hashlib
import re
import time
import urllib.request
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RUNTIME_RAW = (
    "https://raw.githubusercontent.com/"
    "JamilRamirez/FABDEM-Watershed-Runtime/main/posit_data/"
)

EXPECTED_APP_SOURCES = [
    "R/config.R",
    "R/helpers.R",
    "R/snap_topologico.R",
    "R/delimitacion.R",
    "R/morfometria.R",
    "R/geologia.R",
    "R/geomorfologia.R",
    "R/suelos.R",
    "R/hidrogeologia.R",
    "R/medio_fisico.R",
    "R/clima.R",
    "R/cobertura.R",
    "R/cum.R",
    "R/vida.R",
    "R/clima_superficie.R",
    "R/cuencas.R",
    "R/distritos.R",
    "R/poblados.R",
    "R/contexto_territorial.R",
]

EXPECTED_MODULES = {
    "delimitacion": "R/delimitacion.R",
    "morfometria": "R/morfometria.R",
    "geologia": "R/geologia.R",
    "geomorfologia": "R/geomorfologia.R",
    "suelos": "R/suelos.R",
    "hidrogeologia": "R/hidrogeologia.R",
    "medio_fisico": "R/medio_fisico.R",
    "clima": "R/clima.R",
    "cobertura": "R/cobertura.R",
    "cum": "R/cum.R",
    "vida": "R/vida.R",
    "clima_superficie": "R/clima_superficie.R",
    "cuencas": "R/cuencas.R",
    "distritos": "R/distritos.R",
    "poblados": "R/poblados.R",
    "contexto_territorial": "R/contexto_territorial.R",
}

REQUIRED_FILES = [
    "app.R",
    "manifest.json",
    "data/CATALOG_READY.txt",
    "data/remote_manifest.csv",
    "data/remote_sources.csv",
    "data/block_metadata.csv",
    "data/block_lookup.gpkg",
    "www/fabdem.css",
    "www/fabdem_logo.png",
    "www/morfometria_layout.js",
    *EXPECTED_APP_SOURCES,
]


def fail(message: str) -> None:
    raise AssertionError(message)


def fetch_bytes(url: str, *, range_bytes: int | None = None, attempts: int = 3) -> bytes:
    last_error: Exception | None = None
    for attempt in range(1, attempts + 1):
        headers = {"User-Agent": "FABDEM-Watershed-Explorer-CI/1.0"}
        if range_bytes is not None:
            headers["Range"] = f"bytes=0-{range_bytes - 1}"
        request = urllib.request.Request(url, headers=headers)
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                if range_bytes is None:
                    return response.read()
                return response.read(range_bytes)
        except Exception as exc:  # pragma: no cover - network retry path
            last_error = exc
            if attempt < attempts:
                time.sleep(attempt * 2)
    raise RuntimeError(f"No se pudo consultar {url}: {last_error}")


def runtime_candidate_urls(base: str, relative_path: str) -> list[str]:
    base = base.rstrip("/") + "/"
    relative_path = relative_path.lstrip("/")
    urls = [base + relative_path]

    media_prefix = "https://media.githubusercontent.com/media/"
    if base.startswith(media_prefix):
        raw_base = "https://raw.githubusercontent.com/" + base[len(media_prefix) :]
        raw_url = raw_base + relative_path
        if raw_url not in urls:
            urls.append(raw_url)

    return urls


def fetch_runtime_asset_prefix(base: str, relative_path: str) -> bytes:
    errors: list[str] = []
    for url in runtime_candidate_urls(base, relative_path):
        try:
            prefix = fetch_bytes(url, range_bytes=512, attempts=2)
        except RuntimeError as exc:
            errors.append(str(exc))
            continue

        if not prefix:
            errors.append(f"Respuesta vacía: {url}")
            continue

        if prefix.startswith(b"version https://git-lfs.github.com/spec/v1"):
            errors.append(f"Puntero Git LFS, no contenido: {url}")
            continue

        return prefix

    fail(
        "No se pudo obtener un asset Runtime utilizable siguiendo el mismo "
        "fallback media/raw de config.R:\n"
        + "\n".join(errors)
    )
    return b""  # unreachable, keeps type checkers happy


def check_required_files() -> None:
    missing = [path for path in REQUIRED_FILES if not (ROOT / path).is_file()]
    if missing:
        fail("Faltan archivos requeridos:\n" + "\n".join(missing))


def extract_app_sources(text: str) -> list[str]:
    pattern = re.compile(
        r"source\s*\(\s*file\.path\s*\(\s*['\"]([^'\"]+)['\"]\s*,\s*['\"]([^'\"]+)['\"]\s*\)",
        flags=re.DOTALL,
    )
    return [f"{a}/{b}" for a, b in pattern.findall(text)]


def check_architecture() -> None:
    app_text = (ROOT / "app.R").read_text(encoding="utf-8")
    sources = extract_app_sources(app_text)
    if sources != EXPECTED_APP_SOURCES:
        fail(
            "El orden de source() de app.R cambió.\n"
            f"Esperado: {EXPECTED_APP_SOURCES}\n"
            f"Actual:   {sources}"
        )

    for source_path in sources:
        if not (ROOT / source_path).is_file():
            fail(f"app.R referencia un source inexistente: {source_path}")

    for module, path in EXPECTED_MODULES.items():
        text = (ROOT / path).read_text(encoding="utf-8")
        if not re.search(rf"\b{re.escape(module)}\s*<-", text):
            fail(f"{path} no define el objeto de módulo '{module}'.")

    forbidden = [
        "morfometria_graficos_v37.R",
        ".morph_replace_region_v37",
        ".morph_draw_hipsometrica_v37",
        ".morph_draw_hist_elevacion_v37",
    ]
    r_text = "\n".join(
        path.read_text(encoding="utf-8", errors="strict")
        for path in [ROOT / "app.R", *sorted((ROOT / "R").glob("*.R"))]
    )
    for token in forbidden:
        if token in r_text:
            fail(f"Reapareció una referencia de parche retirada: {token}")

    if (ROOT / "R/morfometria_graficos_v37.R").exists():
        fail("R/morfometria_graficos_v37.R no debe volver a existir.")


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def parse_catalog_ready(text: str) -> dict[str, str]:
    result: dict[str, str] = {}
    for line in text.splitlines():
        if ":" not in line:
            continue
        key, value = line.split(":", 1)
        result[key.strip()] = value.strip()
    return result


def check_catalog() -> tuple[list[dict[str, str]], dict[str, str]]:
    manifest_path = ROOT / "data/remote_manifest.csv"
    rows = read_csv(manifest_path)
    if not rows:
        fail("data/remote_manifest.csv está vacío.")

    required_columns = {
        "BLOCK_ID",
        "ASSET_TYPE",
        "STRIPE_ID",
        "ROW_START",
        "ROW_END",
        "NROWS",
        "NCOLS",
        "SOURCE_ID",
        "RELATIVE_PATH",
        "SIZE_BYTES",
        "REMOTE_URL",
    }
    missing_columns = required_columns.difference(rows[0])
    if missing_columns:
        fail(f"Faltan columnas del manifest: {sorted(missing_columns)}")

    counts = Counter(row["ASSET_TYPE"].strip().lower() for row in rows)
    blocks = sorted({row["BLOCK_ID"].strip() for row in rows})

    expected_counts = {
        "reverse": 1128,
        "stream": 1128,
        "index_metadata": 21,
        "run_info": 21,
        "hydrography": 0,
    }
    if len(rows) != 2298:
        fail(f"El manifest debe tener 2298 filas; tiene {len(rows)}.")
    if len(blocks) != 21:
        fail(f"El manifest debe contener 21 bloques; contiene {len(blocks)}.")
    for asset_type, expected in expected_counts.items():
        actual = counts.get(asset_type, 0)
        if actual != expected:
            fail(f"ASSET_TYPE={asset_type}: esperado {expected}, actual {actual}.")

    sources = read_csv(ROOT / "data/remote_sources.csv")
    source_ids = {row["SOURCE_ID"].strip() for row in sources}
    if not source_ids:
        fail("data/remote_sources.csv no define fuentes.")

    seen = set()
    by_block_type: dict[tuple[str, str], list[dict[str, str]]] = defaultdict(list)
    total_bytes = 0
    for row in rows:
        block_id = row["BLOCK_ID"].strip()
        asset_type = row["ASSET_TYPE"].strip().lower()
        source_id = row["SOURCE_ID"].strip()
        relative_path = row["RELATIVE_PATH"].strip()
        if source_id not in source_ids:
            fail(f"SOURCE_ID desconocido: {source_id}")
        if not relative_path:
            fail(f"RELATIVE_PATH vacío en {block_id}/{asset_type}.")
        if "hydrography_block.gpkg" in relative_path.lower():
            fail(f"Ruta hydrography obsoleta en manifest: {relative_path}")
        key = (block_id, asset_type, row["STRIPE_ID"].strip(), relative_path)
        if key in seen:
            fail(f"Fila duplicada en manifest: {key}")
        seen.add(key)
        by_block_type[(block_id, asset_type)].append(row)
        try:
            total_bytes += int(float(row["SIZE_BYTES"]))
        except ValueError as exc:
            raise AssertionError(f"SIZE_BYTES inválido: {row['SIZE_BYTES']}") from exc

    for block_id in blocks:
        reverse = sorted(
            by_block_type[(block_id, "reverse")],
            key=lambda r: int(r["STRIPE_ID"]),
        )
        stream = sorted(
            by_block_type[(block_id, "stream")],
            key=lambda r: int(r["STRIPE_ID"]),
        )
        if len(reverse) != len(stream):
            fail(f"{block_id}: reverse y stream tienen distinta cantidad de franjas.")
        reverse_ids = [int(row["STRIPE_ID"]) for row in reverse]
        stream_ids = [int(row["STRIPE_ID"]) for row in stream]
        if reverse_ids != stream_ids:
            fail(f"{block_id}: STRIPE_ID de reverse y stream no coinciden.")
        if reverse_ids != list(range(1, len(reverse_ids) + 1)):
            fail(f"{block_id}: STRIPE_ID no es una secuencia continua desde 1.")
        for rev, stm in zip(reverse, stream):
            geometry_fields = ["ROW_START", "ROW_END", "NROWS", "NCOLS"]
            if any(rev[field] != stm[field] for field in geometry_fields):
                fail(
                    f"{block_id}/stripe {rev['STRIPE_ID']}: geometría reverse/stream no coincide."
                )

    ready_text = (ROOT / "data/CATALOG_READY.txt").read_text(encoding="utf-8")
    ready = parse_catalog_ready(ready_text)
    expected_ready = {
        "Blocks": str(len(blocks)),
        "Reverse stripes": str(counts["reverse"]),
        "Stream stripes": str(counts["stream"]),
        "Manifest rows": str(len(rows)),
        "Logical runtime assets GB": f"{total_bytes / 1024**3:.3f}",
    }
    for key, expected in expected_ready.items():
        actual = ready.get(key)
        if actual != expected:
            fail(f"CATALOG_READY '{key}': esperado {expected}, actual {actual}.")

    return rows, {row["SOURCE_ID"].strip(): row["BASE_URL"].strip() for row in sources}


def check_runtime_canonical(local_rows: list[dict[str, str]], sources: dict[str, str]) -> None:
    local_manifest = (ROOT / "data/remote_manifest.csv").read_bytes()
    remote_manifest = fetch_bytes(RUNTIME_RAW + "remote_manifest.csv")
    if hashlib.sha256(local_manifest).digest() != hashlib.sha256(remote_manifest).digest():
        fail("data/remote_manifest.csv no es idéntico al manifest canónico del Runtime.")

    local_ready = (ROOT / "data/CATALOG_READY.txt").read_bytes()
    remote_ready = fetch_bytes(RUNTIME_RAW + "CATALOG_READY.txt")
    if hashlib.sha256(local_ready).digest() != hashlib.sha256(remote_ready).digest():
        fail("data/CATALOG_READY.txt no es idéntico al catálogo canónico del Runtime.")

    sample_types = ["index_metadata", "run_info", "reverse", "stream"]
    for asset_type in sample_types:
        candidates = [
            row for row in local_rows if row["ASSET_TYPE"].strip().lower() == asset_type
        ]
        if not candidates:
            fail(f"No hay assets para muestrear: {asset_type}")

        row = candidates[0] if asset_type in {"index_metadata", "reverse"} else candidates[-1]
        source_id = row["SOURCE_ID"].strip()
        relative_path = row["RELATIVE_PATH"].strip()
        prefix = fetch_runtime_asset_prefix(sources[source_id], relative_path)
        if not prefix:
            fail(f"Asset Runtime vacío: {relative_path}")


def main() -> None:
    check_required_files()
    print("OK required files")
    check_architecture()
    print("OK architecture/source order")
    rows, sources = check_catalog()
    print("OK local catalog invariants")
    check_runtime_canonical(rows, sources)
    print("OK Explorer <-> Runtime coherence and remote asset samples")
    print("FABDEM static CI: PASS")


if __name__ == "__main__":
    main()

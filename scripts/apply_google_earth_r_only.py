from pathlib import Path
import subprocess
import sys

root = Path(__file__).resolve().parents[1]
ci_path = root / ".github" / "workflows" / "ci.yml"
workflow_path = root / ".github" / "workflows" / "apply-google-earth-export-patch.yml"
patch_path = root / "scripts" / "apply_google_earth_export_patch.py"

preserve = {
    ci_path: ci_path.read_bytes(),
    workflow_path: workflow_path.read_bytes(),
    patch_path: patch_path.read_bytes(),
}

subprocess.run(
    [sys.executable, str(patch_path)],
    cwd=root,
    check=True,
)

for path, content in preserve.items():
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(content)

print("Parche funcional R aplicado; workflows y scripts temporales restaurados.")

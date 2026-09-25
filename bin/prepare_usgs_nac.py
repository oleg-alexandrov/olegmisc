#!/usr/bin/env python3
# Fetch and prepare USGS-controlled LRO NAC images with USGS South Pole
# custom SPICE kernels and CSM model state generation.
#
# Usage:
#   python3 prepare_usgs_nac.py [--list image_list.txt] [PRODUCT_ID ...]

import argparse
import json
import os
import subprocess
import sys

# Paths on pfe
ISISROOT = "/swbuild/oalexan1/miniconda3/envs/isis10"
ISISDATA = "/nobackupnfs1/oalexan1/projects/isis3data"
SP = "/vast_swbuild/swbuild/oalexan1/projects/BinaryBuilder/StereoPipeline"
SPK_MK = f"{ISISDATA}/lro_south/usgs_polar_spk.mk"
CK_MK = f"{ISISDATA}/lro_south/usgs_polar_ck.mk"

def run_cmd(cmd, env=None, cwd=None):
  print(f"Running: {cmd}")
  res = subprocess.run(cmd, shell=True, env=env, cwd=cwd,
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                       universal_newlines=True)
  if res.returncode != 0:
    print(res.stdout)
    raise RuntimeError(f"Command failed with returncode {res.returncode}: {cmd}")
  return res.stdout

def fix_distortion_coeff(json_path):
  with open(json_path, "r") as f:
    d = json.load(f)
  od = d.get("optical_distortion", {})
  lro = od.get("lrolrocnac", {})
  coeff = lro.get("coefficients")
  if isinstance(coeff, (int, float)):
    lro["coefficients"] = [coeff]
    with open(json_path, "w") as f:
      json.dump(d, f, indent=2)
    print(f"Patched scalar coefficient {coeff} -> [{coeff}] in {os.path.basename(json_path)}")

def process_product(pid, out_dir):
  pid = pid.strip().upper()
  if not pid:
    return
  cub_path = os.path.join(out_dir, f"{pid}.cub")
  json_path = os.path.join(out_dir, f"{pid}.json")
  img_path = os.path.join(out_dir, f"{pid}.IMG")

  print(f"\n=======================================================")
  print(f"Processing USGS Controlled NAC image: {pid}")
  print(f"=======================================================")

  if os.path.exists(cub_path) and os.path.exists(json_path) and os.path.getsize(json_path) > 1000:
    print(f"Already prepared: {cub_path} and {json_path}")
    return

  # Environment for ISIS tools
  env_isis = os.environ.copy()
  env_isis["ISISROOT"] = ISISROOT
  env_isis["ISISDATA"] = ISISDATA
  env_isis["ALESPICEROOT"] = ISISDATA
  env_isis["PATH"] = f"{ISISROOT}/bin:{os.path.expanduser('~/bin')}:{env_isis.get('PATH', '')}"

  # Step 1: Fetch EDR .IMG via fetch_lro_nac.sh if cub not present
  if not os.path.exists(cub_path):
    if not os.path.exists(img_path):
      fetch_script = os.path.expanduser("~/bin/fetch_lro_nac.sh")
      cmd_fetch = f"{fetch_script} {pid} {out_dir}"
      run_cmd(cmd_fetch, env=env_isis)

    # Step 2: Ingest with lronac2isis
    cmd_ingest = f"{ISISROOT}/bin/lronac2isis from={img_path} to={cub_path}"
    run_cmd(cmd_ingest, env=env_isis)

  # Step 3: spiceinit with USGS custom polar kernels
  cmd_spice = (
    f"{ISISROOT}/bin/spiceinit from={cub_path} "
    f"spk={SPK_MK} ck={CK_MK} web=false"
  )
  out_spice = run_cmd(cmd_spice, env=env_isis)
  print("spiceinit attached kernels successfully.")

  # Step 4: isd_generate
  cmd_isd = f"{ISISROOT}/bin/isd_generate -k {cub_path} {cub_path} -o {json_path}"
  run_cmd(cmd_isd, env=env_isis)
  print(f"Generated raw ISD JSON: {json_path}")

  # Step 5: Patch distortion coefficient for USGSCSM compatibility
  fix_distortion_coeff(json_path)

  # Step 6: Validate with cam_test
  env_asp = os.environ.copy()
  env_asp["PATH"] = f"{SP}/bin:{SP}/libexec:{env_asp.get('PATH', '')}"
  isis_asp = "/swbuild/oalexan1/miniconda3/envs/isis10asp"
  env_asp["ISISROOT"] = isis_asp
  env_asp["LD_LIBRARY_PATH"] = f"{isis_asp}/lib:{SP}/lib:{SP}/lib/csmplugins:{env_asp.get('LD_LIBRARY_PATH', '')}"
  env_asp["GDAL_DATA"] = f"{SP}/share/gdal"
  env_asp["PROJ_DATA"] = f"{SP}/share/proj"
  env_asp["ISISDATA"] = ISISDATA

  cmd_camtest = f"cam_test --image {cub_path} --cam1 {cub_path} --cam2 {json_path} --sample-rate 5000"
  out_test = run_cmd(cmd_camtest, env=env_asp, cwd=out_dir)
  for line in out_test.splitlines():
    if "pixel diff" in line or "diff norm" in line or "diff (meters)" in line or "Median:" in line:
      print(f"  {line}")

  # Step 7: Clean up .IMG to conserve space
  if os.path.exists(img_path):
    os.remove(img_path)
    print(f"Removed temporary EDR {img_path} to save storage.")

  print(f"Successfully finished {pid}!")

def main():
  parser = argparse.ArgumentParser(description="Fetch and prepare USGS controlled LRO NAC images.")
  parser.add_argument("products", nargs="*", help="Product IDs (e.g. M135007317RE)")
  parser.add_argument("--list", help="File with list of product IDs")
  parser.add_argument("--outdir", default="/nobackupp19/oalexan1/projects/sfs_BCU2314-BDU1224-MM/usgs_south",
                      help="Output directory on pfe")
  args = parser.parse_args()

  pids = list(args.products)
  if args.list and os.path.exists(args.list):
    with open(args.list) as f:
      for line in f:
        line = line.strip()
        if line and not line.startswith("#"):
          pids.append(line.split()[0])

  if not pids:
    sys.exit("No product IDs specified.")

  os.makedirs(args.outdir, exist_ok=True)
  for pid in pids:
    try:
      process_product(pid, args.outdir)
    except Exception as e:
      print(f"Error processing {pid}: {e}")

if __name__ == "__main__":
  main()

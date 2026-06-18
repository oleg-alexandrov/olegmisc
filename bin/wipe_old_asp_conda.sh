#!/bin/bash
# wipe_old_asp_conda.sh
# Prune the recent asp_* build churn on the nasa-ames-stereo-pipeline anaconda
# channel, keeping ONLY the highest asp_N build per (package, platform) and
# removing the older asp_N duplicates.
#
# HARD SAFETY INVARIANT (Oleg's rule): never remove an older build unless the
# KEEPER (the newer build we intend to retain) is confirmed to exist on the
# channel RIGHT NOW. Implemented per-target with:
#     anaconda show  <keeper spec>   &&   anaconda remove -f  <older spec>
# `anaconda show` exits 0 only if that exact file exists (404 -> exit 1), so a
# missing keeper aborts that removal. We never delete blind.
#
# Discriminators for a wipe candidate (ALL must hold):
#   - build string starts with "asp_"      (the 2026 ASP-channel churn only)
#   - upload date >= CUTOFF                 (never touch >=1-week-old stable)
#   - it is NOT the max asp_N for its (package, platform)   (keep the latest)
#
# Dry-run by default: prints, per platform, the KEEPER and each older build it
# would remove, and actually runs `anaconda show` on every keeper so you see the
# guard pass/fail live. Pass --go to also run the removals.
set -u
GO=0; [ "${1:-}" = "--go" ] && GO=1
USER=nasa-ames-stereo-pipeline
CUTOFF=2026-06-06
PKGS="stereo-pipeline visionworkbench isis usgscsm ale spiceql cgal_tools theia voxblox texrecon"

python3 - "$GO" "$USER" "$CUTOFF" $PKGS <<'PY'
import sys, json, re, urllib.request, subprocess, os
go     = sys.argv[1] == "1"
user   = sys.argv[2]
cutoff = sys.argv[3]
pkgs   = sys.argv[4:]
AN     = os.path.expanduser("~/anaconda3/bin/anaconda")

def buildnum(b):
    m = re.match(r"asp_(\d+)$", b or "")
    return int(m.group(1)) if m else None

# group recent asp_* files by (pkg, subdir)
groups = {}   # (pkg, subdir) -> list of dicts
for p in pkgs:
    url = f"https://api.anaconda.org/package/{user}/{p}"
    try:
        d = json.load(urllib.request.urlopen(url, timeout=30))
    except Exception as e:
        print(f"  WARN {p}: {e}"); continue
    for f in d.get("files", []):
        a     = f.get("attrs", {})
        build = a.get("build") or ""
        date  = (f.get("upload_time","") or "")[:10]
        n     = buildnum(build)
        if n is None or date < cutoff:
            continue   # not our churn, or old stable -> never touched
        groups.setdefault((p, a.get("subdir","?")), []).append(
            {"ver": f.get("version","?"), "base": f.get("basename","?"),
             "build": build, "n": n, "date": date})

# decide keeper (max n) and wipe list (the rest) per group
plan = []   # (pkg, subdir, keeper_dict, [older_dicts])
for (p, sub), fs in sorted(groups.items()):
    fs.sort(key=lambda x: x["n"])
    keeper = fs[-1]
    older  = fs[:-1]
    plan.append((p, sub, keeper, older))

def show_exists(pkg, ver, base):
    # base already includes the subdir prefix (e.g. linux-64/foo-...conda)
    spec = f"{user}/{pkg}/{ver}/{base}"
    r = subprocess.run([AN, "show", spec], capture_output=True, text=True)
    return r.returncode == 0, spec

total_wipe = sum(len(o) for _,_,_,o in plan)
print(f"=== keep-latest plan (cutoff {cutoff}) : {total_wipe} older files to remove ===\n")

removed = errors = 0
for p, sub, keeper, older in plan:
    if not older:
        print(f"[{p:16} {sub:14}] keep {keeper['build']:8} (only one asp_* build - nothing to prune)")
        continue
    ok, kspec = show_exists(p, keeper["ver"], keeper["base"])
    guard = "GUARD OK " if ok else "GUARD FAIL"
    print(f"[{p:16} {sub:14}] keep {keeper['build']:8}  {guard}  ({kspec})")
    for o in older:
        ospec = f"{user}/{p}/{o['ver']}/{o['base']}"
        if not ok:
            print(f"      SKIP (keeper missing!) {o['build']:8} {ospec}")
            errors += 1
            continue
        if not go:
            print(f"      WOULD REMOVE          {o['build']:8} {ospec}")
            continue
        r = subprocess.run([AN, "remove", "-f", ospec], capture_output=True, text=True)
        if r.returncode == 0:
            print(f"      REMOVED               {o['build']:8} {ospec}")
            removed += 1
        else:
            print(f"      ERR                   {o['build']:8} {ospec} :: "
                  f"{(r.stderr or r.stdout).strip()[:120]}")
            errors += 1

print()
if not go:
    print("DRY RUN - nothing removed. Guards above were really queried. Re-run with --go to execute.")
else:
    print(f"DONE - removed {removed}, errors/skips {errors}.")
PY

---
name: sfs-usgs-registered
description: >-
  USGS South Pole controlled (registered) LRO NAC cameras for Shape-from-Shading - the USGS master controlled-image list and where it lives, the USGS custom polar SPICE kernels (usgs_polar_spk.mk / usgs_polar_ck.mk, valid 2009-2013), how to identify which images over a site are registered (intersect the ODE candidate list with the master list), how to build the registered CSM cameras from fresh or already-prepared cubs (batch_prepare_lro.sh --usgs-polar, or redo_spiceinit_usgs.sh on existing .cal.echo.cub), and how the registered cameras are used as fixed bundle-adjustment anchors. Load whenever working with USGS-registered / controlled / polar-registered LRO NAC images, a usgs_south dir, the controlled-image list, the usgs_polar meta-kernels, or deciding which NAC images anchor a south-polar SfS solve. The reference implementation is ~/projects/SfsPipeline/bin.
---

# USGS South Pole Registered (Controlled) LRO NAC Cameras

For a lunar south-polar Shape-from-Shading (SfS) site, a subset of the covering LRO
NAC images have **USGS-provided registered (controlled) camera geometry**: USGS
Astrogeology ran a South Pole control network (bundle-adjusted and controlled to LOLA)
and ships custom polar SPICE kernels that place those observations in a single consistent
registered frame. Those registered cameras are used as **fixed anchors** in bundle
adjustment: holding them fixed pulls every free (vanilla-spiceinit) camera into the
registered frame, which is what gives the whole site its absolute localization.

The reference implementation for every script named here is `~/projects/SfsPipeline/bin/`
(see [[sfs]]). The per-site work notes live in each project dir (e.g.
`~/projects/sfs_south/`).

## The USGS master controlled-image list (the thing we keep needing)

**File: `updated_SPole_2023_ImageGCPAdd_root.lis`** - the authoritative list of LRO NAC
product ids that have USGS-provided registered geometry for the South Pole. 18323 root
product ids, one per line (e.g. `M103039846LE`), both 9- and 10-digit numeric ids, LE and
RE. These are the south-polar NAC images (July 2009 to August 2013) that USGS Astrogeology
bundle-adjusted and controlled to LOLA (~0.48 px RMS). It is a USGS product, NOT something
we derive.

- **Canonical copy ships in the public reference impl:** `~/projects/SfsPipeline/data/`
  (committed, with `data/README.md` carrying the citation). Also mirrored in the
  `~/projects` repo under each south-polar project's `lists/` (`PNCB`, `sfs_dgkm`,
  `sfs_m2m_mp`, `sfs_m2m_sp`, `sfs_south`) for self-containment (~240 KB text).
- **Citation / where external users get it + the kernels:** USGS data release Weller,
  L.A., et al., 2025, "Improved camera pointing and spacecraft ephemeris data for LROC NAC
  images of the lunar poles," USGS data release, https://doi.org/10.5066/P1OASYRV. The
  south-pole SPICE kernels (CK `.bc` + SPK `.bsp`) download from the ScienceBase south-pole
  item https://www.sciencebase.gov/catalog/item/688a51d7d4be02367c9c1271 . ASP docs:
  https://stereopipeline.readthedocs.io/en/latest/sfs_usage.html#usgs-reg . Describe the
  source as "USGS-provided list of registered south-polar images"; cite the data release,
  not an individual.

## The USGS custom polar SPICE kernels

On pfe they live in `$ISISDATA/lro_south/`:
- `usgs_polar_spk.mk` - meta-kernel of registered position (SPK) kernels
- `usgs_polar_ck.mk` - meta-kernel of registered pointing (CK) kernels
- `kernels/` - the actual kernel files the two meta-kernels reference
- `*.mk.bk` - backups from when the meta-kernels were last augmented

They are **valid only for the 2009-2013 controlled observations**. An image outside that
coverage will fail `spiceinit` with the polar kernels. `$ISISDATA` on pfe is
`$HOME/projects/isis3data` (do not delete - see CLAUDE.md). If a box lacks them, the CK
`.bc` + SPK `.bsp` download from the USGS data release south-pole item
(https://www.sciencebase.gov/catalog/item/688a51d7d4be02367c9c1271), organized by camera
LE/RE and acquisition year.

Augmenting the meta-kernels (rarely needed): if newly controlled images reference kernels
not yet in the .mk, append those `lro/kernel...` lines, `sort -u`, keep the header lines
at the top, back up the old .mk first, and `vimdiff` against the backup. Full recipe:
`~/projects/sfs_m2m_sp/sfs_m2m_sp_notes.sh` (the section that built the current .mk).

## Step 1 - identify which images over a site are registered

The master list spans the whole south pole; per site you want only the registered ids that
cover that site. Intersect the site's ODE candidate list with the master list.

```bash
# siteN_cubes.txt = all NAC product ids intersecting the site box (query_lro.py).
# Normalize first: query_lro.py can emit a stray C-prefixed CDR id; rewrite C->M and keep
# only real EDR ids (see [[lro-nac]]), then whole-line intersect with the master list.
M=lists/updated_SPole_2023_ImageGCPAdd_root.lis
sed 's/^C/M/' lists/siteN_cubes.txt | grep -E '^M[0-9]+(LE|RE)$' | sort -u > lists/siteN_norm.txt
grep -Fxf "$M" lists/siteN_norm.txt | sort -u > lists/siteN_usgs.txt   # the registered subset
```

Use `grep -Fxf` (fixed-string, whole-line) or `comm`, never a bare `grep` (substring ids
would cross-match). Keep every list in `lists/` and git-track it (tiny text, defines
lineage - see [[sfs]]). Make the per-site registered lists for ALL sites, including ids not
yet downloaded, so the site's registered total is known up front; the downloaded subset is
what you can actually build now.

## Step 2 - build the registered CSM cameras

The registered camera is the **CSM .json**; it carries the registered geometry. The
.cal.echo.cub only supplies pixels and is byte-identical in content to the vanilla cub
(same calibration) - only the SPICE blobs differ. Two cases:

**A. Fresh ingest (image not yet prepared):** use the `--usgs-polar` path of the standard
ingest, which runs `spiceinit from=... spk=usgs_polar_spk.mk ck=usgs_polar_ck.mk web=false`
in place of the vanilla spiceinit:

```bash
~/projects/SfsPipeline/bin/batch_prepare_lro.sh lists/siteN_usgs.txt usgs_south --usgs-polar
```

**B. Already prepared with vanilla kernels (the usual case - the fetcher already ran
`--no-usgs-polar`):** do NOT re-fetch or re-calibrate. Re-spiceinit a **copy** of the
existing `.cal.echo.cub` with the polar kernels and regenerate the json, keeping the
vanilla cub in `img/` untouched for the non-registered path:

```bash
# redo_spiceinit_usgs.sh <idList> <srcDir> <outDir> <workDir>
#   cp srcDir/<id>.cal.echo.cub -> outDir/ ; spiceinit polar ; isd_generate ;
#   patch scalar distortion coeff -> list ; cam_test (non-fatal). Resumable, serial,
#   1 proc / 1 thread. web=false means NO internet, so it belongs on a COMPUTE node.
~/projects/SfsPipeline/bin/redo_spiceinit_usgs.sh lists/usgs_downloaded.txt img usgs_south $(pwd)
```

**Run it via qsub, not on the head node.** `spiceinit web=false` reads only local kernels,
so there is no reason to occupy a head node (and the NAC fetchers usually already do). The
tools (spiceinit, isd_generate, cam_test) are single-thread and light on RAM, so a handful
run safely in parallel on one node (unlike mapproject/bundle_adjust - see [[pfe-nas]]).
`~/projects/sfs_south/usgs_south_job.sh` is the pattern: split the id list into N disjoint
sublists, run N `redo_spiceinit_usgs.sh` instances concurrently (each cd'd into its own
subdir so ISIS `print.prt` does not collide), `wait`. ~86 s/image serial; N=6 finishes a
few hundred images in ~1 h. Submit with `-r y` (idempotent/resumable).

cam_test on each output prints the json-vs-cub pixel diff (expect << 1 px, e.g. ~1e-3);
that is the per-camera sanity gate that the registered json reproduces the cub geometry.

## Step 3 - use the registered cameras as fixed anchors

In the controlled refinement (see [[sfs]] "Controlled Refinement Family" and
[[coregister-linescan]] / [[image-gcp-gen]]), the registered-image subset is the
`FIXED_LIST` in stage 1 (`bundle_adjust_refine.sh`): their cameras point at the
`usgs_south/<id>.cal.echo.json`, are held fixed, and pull the free cameras into the
registered frame. The free images keep their vanilla `img/<id>.cal.echo.json`. Assemble
the mixed camera list 1-to-1 with the image list (anchor lines -> usgs_south json, others
-> img json). Prior art with exact invocations: `~/projects/sfs_m2m_sp/sfs_m2m_sp_notes.sh`
("Run bundle adjustment while keeping USGS images fixed").

## Gotchas

- A `usgs_south/` dir duplicates the cub (~0.5 GB each) so the vanilla `img/` cub stays
  intact. If storage is tight, only the json is needed downstream (ASP `-t csm` reads
  geometry from the json, pixels from the identical `img/` cub), so the `usgs_south` cub
  can be deleted after the json + cam_test pass - but the default (and what the notes
  assume) is to keep it.
- `prepare_lro_nac.py` short-circuits if `<id>.cal.echo.cub` AND `.json` both exist, and
  otherwise tries to FETCH when no raw cub is present - so it is the wrong tool for
  re-registering an already-prepared cub. Use `redo_spiceinit_usgs.sh` for that.
- The registered set can be a small fraction of the site's images (only 2009-2013
  controlled ones), e.g. tens out of hundreds at a northern site, a few hundred near the
  pole. That is expected; a handful of good anchors is enough to fix the frame.

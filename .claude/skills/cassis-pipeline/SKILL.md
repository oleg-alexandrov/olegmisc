---
name: cassis-pipeline
description: The TGO CaSSIS pushframe-as-framing stereo pipeline in ASP - the CassisPipeline repo and its bin/ stage scripts, the ms_cassis.py per-pair stereo engine (and its swap to the shipped multi_stereo tool), the pfe run layout and replicas, the build-floor date check, and where the CaSSIS notes live. Load whenever working on CaSSIS - CassisPipeline, ms_cassis, cassis_stereo/cassis_process, framelets, pushframe, or a CaSSIS DEM replica on pfe. Complements multi-stereo (the engine), ctx-processing (the CTX reference), pc-align, dem-comparison.
metadata:
  type: reference
---

CaSSIS (TGO) acquires PAN pushframe framelets that ASP treats as many small FRAMING
cameras. The pipeline builds a frame DEM per observation, tied to a CTX reference DEM.

## READ THE REPO README FIRST - IT IS THE SOURCE OF TRUTH, NOT THIS SKILL

**The authoritative instructions are the CassisPipeline README (`~/projects/CassisPipeline/README.md`).
Read it FIRST, every time, and drive the pipeline AS AN EXTERNAL USER would by following it. This skill
is only HINTS; the README and the repo scripts are reality. If the README and this skill ever disagree,
the README wins and this skill is stale - fix it.**

**PRIMARY GOAL whenever doing CaSSIS work: IMPROVE THE REPO (the `bin/` scripts and the README).** The
pipeline is where external folk will use this, so that is where every lesson must land. When you drive a
site and something breaks or is unclear, that is a REPO BUG: trace it, FIX the repo script AND the README,
do NOT paper over it with a workaround or a one-off scratch script. AVOID writing custom scratch scripts -
edit the repo scripts instead (the only non-repo file that is acceptable is thin pfe qsub glue that sets up
the environment and calls repo tools, with zero pipeline logic of its own).

**Knowledge-flow direction for everything you learn or that fails: FAIL -> project NOTES -> this SKILL ->
the README.** The README is the final destination of every hard-won lesson.

**Workflow: the MAC is authoritative.** Edit the repo (`~/projects/CassisPipeline`) and the project notes
on the Mac, test/`bash -n` there, then RSYNC to pfe to launch (head node = fetch/network only; everything
else = qsub compute). The rsync (chmod +x FIRST at the source, and again on pfe - rsync can carry it but
verify, a missing x-bit makes PBS/nohup fail):
```
chmod +x ~/projects/CassisPipeline/bin/*.sh
rsync -a --exclude '.git' --exclude '__pycache__' ~/projects/CassisPipeline/bin/ pfx:/home6/oalexan1/projects/CassisPipeline/bin/
rsync -a ~/projects/CassisPipeline/README.md ~/projects/CassisPipeline/config/ pfx:/home6/oalexan1/projects/CassisPipeline/
```

**THE REPO LIVES IN ONE PLACE PER MACHINE, ALWAYS KEPT MIRRORED - no second pfe copy.** Canonical:
Mac `/Users/oalexan1/projects/CassisPipeline` (git, authoritative) <-> pfe `/home6/oalexan1/projects/CassisPipeline`
(a real independent clone in the HOME dir - it is small, so it stays in home6, NOT on /nobackupp19). ALWAYS
keep the two mirrored (edit on the Mac, rsync to pfe home6 before every launch). There must be NO
`/nobackupp19/oalexan1/projects/CassisPipeline*` copy - a second pfe mirror (e.g. the old
`CassisPipeline_main` on nobackup) is pure confusion about which is current and was removed 2026-11-02. If
one reappears, wipe it and use the home6 clone. (l1 may also hold a clone; same rule - one per machine.)

## VENDOR-FREE NEW SITE (no prior CaSSIS DEM) - the README "Processing a new site from scratch"

The repo can process a brand-new stereo pair with NO vendor DEM. The pieces (all repo):
- `cassis_find_looks.sh <orbit>` - discover the two stereo looks (the consecutive acq-id pair in the PSA
  orbit listing). Fills the old manual "looks come from the PSA" gap.
- `cassis_ctx_fetch.sh` accepts the footprint as `bbox:minLon,minLat,maxLon,maxLat` (east lon) when there
  is no DEM yet - it synthesizes a constant stand-in raster over the bbox. This breaks the chicken-and-egg
  (the linescan DEM needs the CTX grid, so the CTX must be built first, from the observation footprint, not
  from a DEM you do not have). The bbox + clat/clon come from ODE / the pair inventory
  (`~/projects/cassis_asp/cassis_stereo_pairs_band.json`, with `nctx` = controlled-CTX intersect count = a
  quick "is there CTX here" estimate).
- `cassis_fetch.sh <site.conf> <workdir>` - the one FETCH command (head node): look-discovery + framelets
  (`cassis_fetch_pair.sh`) + CTX DTMs (`cassis_ctx_fetch.sh`). The site config carries `orbit`, `clat`,
  `clon`, `footprintBbox` so it is self-contained for a vendor-free site.
Then the COMPUTE tier (qsub, NO network): ingest (isis10 env - the ONLY env with a WORKING `tgocassis2isis`;
it is absent in usgscsm_cassis and broken in isis10asp) -> cameras + `cassis_ctx_blend.sh` + stages
(usgscsm_cassis env + ASP bin + ISISROOT=isis10asp). On a cluster, one qsub worker activates each env in
turn and calls the repo tools - that worker is pure glue, no logic.

## CTX FETCH ROBUSTNESS (fixes made 2026-10-31, in `cassis_ctx_fetch.sh`)

The controlled-CTX STAC fetch had three real failure modes, all fixed - watch for regressions:
- DENSE regions (e.g. Coprates ~2176 DTMs over a 3x box): a single `limit:1000` bbox query 502s
  consistently (gateway times out gathering it). FIX = PAGINATE with `STAC_PAGE` (default 250) following the
  pgstac `next` link token, up to `STAC_MAXPAGES`. Small pages always return fast.
- TRANSIENT 502/503/timeout: retry the STAC query with backoff (6 attempts); `curl --retry` on downloads.
- pfe's SYSTEM curl is OLD: it does NOT know `--retry-all-errors` (curl 7.71+). Use only `--retry`/
  `--retry-delay`. And a bare `curl` under `set -e` aborts the whole download loop on one blip - use
  `curl ... || true` and let the gdalinfo result-check decide per DTM.

## SITE YIELD AND DROP PATTERNS (vendor-free campaigns, hunt3 2026-11-01: 15 attempted -> 10 good)

Expect ~2/3 yield, so OVER-FETCH candidates (fetch ~15 to ship 10). Rank the pool by `nctx` (the
controlled-CTX intersect count in `cassis_stereo_pairs_band.json`) - it predicts CTX COVERAGE but NOT
texture, so a high-nctx site can still fail on low texture. The drops are almost all LOW-TEXTURE plains
(equatorial 151/167E, VM-outflow 267-268E), failing at one of three places:
- strip assembly: `stack_strip_gen.py` measures along-track advance ~0 -> clear "too low-texture" error.
- linescan stereo: RANSAC "not enough matches / failed epipolar rectification".
- framelet stereo (stages 6-7): the FINAL DEM comes out FRAGMENTED into disconnected patches at partial
  coverage, even when the linescan DEM + align were fine (3459: dz 187 m AND 43% coverage; 3920: no DEM).
**The EVAL COVERAGE-CHECK is the arbiter, not the dz/dd medians.** Row 1 of `cassis_eval_plots.py`
(CTX | aligned linescan | final) + a valid-pixel ratio (final vs aligned linescan) catches a fragmented or
under-covered final DEM that the dz/dd stats HIDE. Always compare the final framelet DEM's coverage to the
aligned linescan before calling a site good. A healthy site: final coverage ~= linescan, dz NMAD ~2-10 m,
dd sub-pixel. The campaign driver/eval flow (per site): head `cassis_fetch.sh` -> qsub compute (ingest
isis10 -> cameras+ctx_blend usgscsm_cassis -> `cassis_process.sh` 1 2) -> inspect align residual -> qsub
`cassis_process.sh` 3 7 for keepers -> `cassis_eval_plots.py` + `cassis_gallery.py`. A ~17 km sparse-IP
wrong-lock is the tell that the NCC primary did not run (see the CTX-ALIGN note) - a good NCC align is a few
km, vertical-dominated, dd sub-pixel.

**ALWAYS HILLSHADE THE FINAL FRAMELET DEM AND WATCH FOR DISJOINT FRAMELETS AND SEAMS - THE MEDIAN STATS HIDE
THEM (burned 2026-11-05).** The whole point of CaSSIS QC is a seamless strip, so disjoint/wild framelets and
framelet-to-framelet seams are THE failure to catch. Two signatures: (1) WILD END FRAMELETS - saturated
black/white bands at the strip ends, the single-look ends that have no cross-look stereo partner so they float
on a weak pose leash (geometry, not GCP or lens). (2) SEAMS - height steps between adjacent framelets mid-strip,
from inconsistent per-framelet GCP control. CRITICAL: dz NMAD and a top/bottom-half dz "step" are MEDIAN-based
and SHRUG OFF these - a top where 30-50% of pixels are >50 m off CTX can still report a ~6 m NMAD and a ~1 m
step. So NEVER call a result good on the NMAD/step alone. HILLSHADE the final DEM and LOOK, and quantify the
wild fraction directly: the percent of pixels with |dz to CTX| > 50 m, computed PER REGION (top third vs
middle vs bottom), not a whole-strip median. A healthy strip is seamless with a near-zero wild fraction
everywhere. Neither the lens (refit/reorder) nor a GCP-noise filter fixes wild END framelets - that is
under-constrained single-look geometry (trim the ends, or lean on stage 8 / pc_align), distinct from the
mid-strip seams that a softer GCP (float + robust threshold) can heal.

## RE-RUNNING WHEN A PARAMETER CHANGES BUT THE INPUT DOES NOT (be smart, don't recompute)

The numbered stages depend on different inputs, so know which stage a knob touches and re-run only that,
into a FRESH outDir (never clobber the old result - you want to compare), reusing the unchanged earlier
stages. The STAGE-7 knobs - `gcpSigma`, `htUncLoose`, `htUncTight`, `robust`, `maxDisp`, `maxGcp` (and
`--fix-gcp-xyz`) - affect ONLY stage 7 (pass1 = `cassis_pass.sh`: the linescan->CTX bootstrap GCP, the tic/toc
bundle_adjust, the per-pair stereo). Stages 1 (linescan+align), 2 (align-cams), 3-5 (framelet cams), 6 (dense
matches) are INDEPENDENT of those knobs and deterministic, so they do NOT need recomputing when you sweep a
stage-7 value. Two ways to re-run:
- EFFICIENT (big sweeps): build stages 1-6 ONCE in a base outDir, then for each value re-run stage 7 only.
  cassis_process.sh skips a stage whose output exists, so the clobber-free trick is: in the SAME outDir, MOVE
  the existing `frame/pass1`, `frame/pass1_tic`, `frame/pass1_stereo` ASIDE (rename, e.g. `pass1_gs1`), then
  `cassis_process.sh <conf> 7 7 <same outDir> <B>` rebuilds pass1 fresh while reusing stages 1-6 in place; the
  old result is preserved under the renamed dir for comparison. (Rename, do NOT symlink/copy stage dirs.)
- SIMPLE but WASTEFUL (what hunt3 gcpSigma sweep used): a full `0 1 7` in a SEPARATE outDir per value - clean
  and independent, but recomputes the ~10 min dense stage each time. Fine when compute nodes are free; prefer
  the efficient path for a large sweep. Pass the knob via a per-run CONFIG variant (copy the site conf, append
  e.g. `gcpSigma=5`; the site conf is sourced AFTER cassis_common.conf so it overrides). Keep the shared CTX +
  cubes + cameras (data/, ref/) - do NOT refetch or rebuild the CTX; only the stage-7 outputs change.

## CONTINUITY / SEAM FAILURE = A HARD GCP BUILT FROM A SOFT REGISTRATION (hunt3 5681 diagnosis, 2026-11-01)

When a site's framelet DEM has SEAMS / a fragmented strip but the dense matches are complete (same-look ties
fine), the cause is usually NOT matches or a broken camera - it is the stage-7 GCP. `cassis_pass.sh` builds
GCP0 from the aligned-linescan -> CTX dh/dv disparity, then the TIC bundle_adjust uses `--fix-gcp-xyz` + a
TIGHT `gcpSigma` (1) = HARD control. If the linescan registers to CTX only loosely (high-relief terrain,
noisy hillshade disparity, a residual vertical bias from the align), GCP0 is noisy/biased and the fix-gcp
solver fights its own good match-ties against inconsistent control (TIC per-camera reprojection residuals of
4-7 px instead of ~1 px), scattering individual framelets -> per-pair DEMs disagree -> seams. DIAGNOSE by:
(a) the boot dh/dv NMAD (`frame/pass1/boot/run-F-H.tif`,`run-F-V.tif`) - a bad site is NMAD ~5-8 px and a dv
median of several px vs a good site's ~1.5-2 px near zero; (b) the TIC per-camera mean reprojection error
(`frame/pass1_tic/run-final_residuals_stats.txt` col 2) - 4-7 px = infeasible control. FIX = make the GCP a
SOFT anchor so it still MOVES the cameras toward CTX but does not override continuity: sweep `gcpSigma`
(1 -> 3 -> 5 -> 10) on fresh runs and pick the softest that KEEPS registration (dz/dd stay small). Too soft
and the cameras do not register to CTX at all (the opposite failure Oleg has hit). If gcpSigma does not bite
under `--fix-gcp-xyz`, relax fix-gcp (float GCP with a sigma) and/or the TOC `htUncTight`. Full worked
diagnosis + plots: `~/projects/cassis_hunt/hunt3/diag5681/cassis_5681_diagnosis.html`.

**Repo: `~/projects/CassisPipeline`** (`NeoGeographyToolkit/CassisPipeline`, pushable;
also cloned on pfe at `~/projects/CassisPipeline` and on l1). All logic is shell +
Python in `bin/`. This is the real pipeline - use it from there, do not hand-roll.

Stage scripts in `bin/` (driven by `cassis_process.sh`, the top-level driver):
- `cassis_process.sh` - orchestrates the stages. Enforces a BUILD FLOOR: reads
  `parallel_stereo --version` "Build date:" (dashes stripped to YYYYMMDD) and refuses
  an ASP older than a required date. Bump this when a needed ASP feature lands.
- `cassis_stereo.sh` - the frame-DEM step: mapproject the framelets onto the blurred
  CTX at native GSD (~4.59 m), build a cross-look overlap list, run per-pair stereo +
  point2dem + DEM mosaic, then put the frame DEM on the CTX grid (+ hillshade, dz
  geodiff). Driven by ASP's shipped `multi_stereo` tool (formerly `ms_cassis.py`).
- `cassis_corr.sh` - correlation-only pass (`parallel_stereo --correlator-mode`).
- `cassis_ctx_stereo.sh`, `cassis_ctx_align.sh` - build/align the CTX reference DEM.
- `cassis_linescan_dem.sh` - linescan CTX DEM helper.
- `gen_gcp.sh`, `refit_transverse.sh`, `cassis_metrics_run.sh`, `cassis_pass.sh`.

**`multi_stereo` in `cassis_stereo.sh`**:
`multi_stereo` (dem_mosaic mode) is the shipped ASP per-pair stereo engine.
It replaces the older prototype `ms_cassis.py` (now deleted). It takes
`--overlap-list`, `--dem`, `--output-prefix`, `--processes`, `--blunder-tol`,
`--stereo-options`, and `--point2dem-options`.

**Framelet pairing (`pairs_lr.txt`)**:
In dense mode (`cassis_stereo.sh` stage 6), cross-look pairs are computed from mapprojected
footprints (`out/maps/*.tif`) sorted along-track by footprint centroid Y. For each left look
framelet L, it selects the top overlapping right look framelets R (top 6 by overlap fraction
where overlap > 4%, plus top 3 regardless for thin-overlap survival). Same-look pairs are
adjacent along-strip framelets. If an observation's track does not align well with the
projected Y axis or has missing framelets, pair generation must be checked carefully.
Pairing is by FOOTPRINT OVERLAP, never by framelet index, so the along-orbit offset
between looks (the two looks are seconds apart with the telescope rotated 180 deg, so
left framelet i overlaps right framelet i+4 or i+5) is handled automatically.

**Dropout framelets**: the PSA/PDS archive can deliver framelets that are instrument
dropout frames with ZERO valid pixels. A 100% nodata framelet makes the 0-iteration
bundle_adjust in `linescan2framelets.sh` fail when computing image statistics, which
aborts the whole frame-camera stage (the script runs `set -e`). The script now prunes
zero-valid-pixel framelets first (keep a cube only if `np.any(a > -1e30)`), commit
ffadea7. These framelets show as gaps in the stitched strips. Note this is a CRASH, not
a coverage truncation (the two look different).

**pfe**: replicas live under `~/projects/cassis_dist_no_tilt` and `/nobackupp19/oalexan1`;
production is a qsub job (heavy 143-pair runs belong on pfe/l1, never the Mac). To
validate a new stereo engine, reuse an existing replica, move its old mosaicked DEM and
max-tri-err aside, inject only the new stereo step, and expect the same DEM.

**CTX reference coverage**: the reference is a mosaic of USGS controlled CTX DTMs (STAC
collection `mro_ctx_controlled_usgs_dtms`). The controlled inventory is sparse in places,
and a generous snapped bounding box does NOT imply good coverage. Check the CTX valid-pixel
fraction INSIDE the CaSSIS footprint (crop the CTX to the footprint grid first, not the
padded canvas). Gaps there mean no vertical or horizontal control for that part of the
strip, so the pc_align fit and the dz / dh-dv stats are driven only by where CTX exists.
Fill options when short: build a CTX DTM from a stereo pair over the gap (ctx-processing),
use HRSC for continuous coverage, or MOLA as a coarse last resort.

**CANONICAL ROBUST CTX-REFERENCE RECIPE (the proven winner, from BOTH 2026-10-30 vendor-free campaigns;
this supersedes the interim gdalbuildvrt / median-all notes below).** Build the controlled-CTX reference
per site in this exact order - fast (the cull is at sub-8), hole-free (cull only true outliers, blend the
rest), smooth (one full-res feather), and framelet-safe (regenerate the blur5):
  1. BOX = 3x the linescan-DEM footprint. The MEASURED CaSSIS->CTX pointing offset is ~8-15 km, so a 3x
     box (+-60 km) covers it ~4x over; 6x is wasteful (hundreds of unused far-margin tiles, slow blend).
     Snap the -te to the 18 m grid at odd-multiples-of-9.
  2. DOWNLOAD every controlled-CTX DTM intersecting the box (STAC mro_ctx_controlled_usgs_dtms, limit 1000,
     NO nearest-first cap - the cap silently starves the left/right EDGE coverage; fetch them all).
  3. WARP each DTM to the 18 m local-stereographic grid at its NATIVE extent (-tap -r cubicspline), parallel
     (throttle ~20 concurrent via `while [ $(jobs -r|wc -l) -ge N ]; do wait -n; done`).
  4. SUB-8 OUTLIER CULL (cheap): downsample each 18 m warp to 144 m (sub-8); dem_mosaic --median the sub-8
     tiles = consensus; geodiff EACH sub-8 tile vs the sub-8 median (TILE first, CONSENSUS last - geodiff
     operates on the FIRST arg's domain); CULL a tile if |STATISTICS_MEAN| > 30 m OR STATISTICS_STDDEV > 40 m.
     Keep an ALL-CULLED FALLBACK (if everything is culled, use all warps).
  5. BLEND the SURVIVORS' full-res 18 m warps ONCE with dem_mosaic DEFAULT (feathered weighted-average =
     smooth; NOT --median at full res). Crop to the box -te at 18 m -> <tag>_ctx_18m.tif.
  6. REGENERATE the mapprojDem: dem_mosaic --dem-blur-sigma 5 of <tag>_ctx_18m.tif -> <tag>_ctx_18m_blur5.tif.
     MANDATORY every time the CTX changes (a stale blur5 caps framelet coverage - see the blur5 bullet).
  CANONICAL IMPLEMENTATION = two repo tools, split FETCH vs PROCESS (think of all CTX work this way):
  `cassis_ctx_fetch.sh <ctxDir> <footprintDEM> <clat> <clon>` runs on the HEAD/front-end node (needs net):
  steps 1-2, compute the box + STAC download the DTMs -> dl/*_dem.tif + box_te.txt. Then
  `cassis_ctx_blend.sh <ctxDir> <tag> <clat> <clon>` runs on a COMPUTE node (qsub): steps 3-6, warp + crop +
  cull + feather + blur5. The warps cache as dl/*_gw.tif, so re-running the blend to try a combination costs
  minutes. (cassis_ctx_build.sh is the older all-in-one, same blend as CTX_STACK=sub8; the prototype was
  cassis_hunt2's ctx_search_and_download.py + ctx_sub8_blend_node.sh.) VERIFIED 2026-10-31: produced a
  seamless 002340 reference (99.96% clean by a 7x7-median local-outlier check, ~0.02% spike pixels, 100%
  footprint coverage).

**CaSSIS-to-CTX ALIGNMENT is the most fragile step - NCC global correlation is the robust primary (method
0), pc_align ICP is the fallback.** The stage-1 align (cassis_linescan_dem.sh, applied to the cameras in
stage 2 cassis_align_cams.sh) registers the linescan DEM to the CTX reference. The old sparse-IP pc_align
(ICP from scratch) can lock onto a WRONG correspondence and return it confidently: on 002340 it jumped ~20
km onto the high massif (~4400 m off vertically) and broke every framelet camera (all mapproject-fail,
stage-6 dense=0). The fix (wired 2026-10-31): the registration is a known RIGID SHIFT and the raw strip
already sits inside the 3x CTX box, so a GLOBAL exhaustive hillshade cross-correlation cannot get lost.
`cassis_ctx_ncc_align.py <ctx> <cassis_dem> <res> <outdir>` (numpy hillshade + scipy FFT NCC, no matplotlib,
runs in the pipeline env) emits a local NED shift; `pc_align --num-iterations 0 --initial-ned-translation
"<N E D>"` turns it into the honest ECEF cassis->ctx transform (same step as the asp_bm fallback). It is
method 0 in cassis_linescan_dem.sh; methods 1 (sparse-IP) and 2 (asp_bm dense) remain as fall-through if the
dd eval does not pass. VALIDATED across all 10 recent sites: NCC gives a sensible 0.58-4.83 km shift with dd
MEDIAN 0 everywhere (no wrong locks), vs pc_align's 20 km garbage (dd NMAD 19 px) on 002340. The ARBITER is
the dh/dv hillshade disparity (median + NMAD + validity): a good align is small median, NMAD within a few px,
~50% validity; a large residual or tiny validity is junk. Cost ~1 min, one process. NCC gives a pixel/NED
shift in the LOCAL stereographic frame - the cameras need the ECEF (around-planet-center) transform, which
is exactly what the pc_align --initial-ned-translation step produces, so do NOT hand-apply the local shift to
cameras.

**CTX reference: prefer a plain gdalbuildvrt STACK over curate+blend (vendor-free pair-hunt
campaign, 2026-10-30).** Hard-won. The controlled-CTX reference is best built as a plain
first-valid gdalbuildvrt stack of the warped DTMs, NOT the curate-then-mean/median blend that
`cassis_ctx_build.sh` does by default. Every lesson here is about the REFERENCE, not the CaSSIS
stereo (the stereo captures real terrain almost everywhere - a "failed" site is almost always a
CTX-coverage problem):
- **Do NOT blend / average the controlled strips WITHOUT culling first.** (Reconciled with the canonical
  recipe above: the winning method CULLS the offset/artifact tiles at sub-8 FIRST, then feather-blends the
  consistent SURVIVORS - that is good. The warning here is against blending UNCULLED strips.) They carry
  differing vertical pointing offsets, so a mean/median mosaic blurs and corrupts the surface, and the
  geodiff-vs-consensus FULL-RES curation then DROPS genuinely-good strips as "outliers", punching holes in
  coverage (saw 83 inputs collapse to 1
  at one site). A plain gdalbuildvrt stack (first-valid, no averaging, no curation) keeps ALL coverage
  and is far faster.
- **Include EVERY overlapping DTM - never cap the input set by name order.** A first-N-by-filename cap,
  or `cassis_ctx_build.sh`'s first-K-per-0.1deg-lat-bin-by-fid (K=5), can EXCLUDE exactly the strips
  that overlap the footprint if they sort late. That makes a well-covered site look like a "true data
  gap" (burned on site 003837: looked mostly-nodata, was actually ~93% covered once all DTMs were kept).
  Warp them all and let the box crop decide, or select by footprint overlap.
- **The box must catch the pointing offset, and the offset is ~8-15 km (MEASURED), not ~1 km.** The
  un-aligned CaSSIS linescan DEM sits offset from the ODE footprint center by the full pointing error.
  Measured from 4 vendor-free sites' stage-1 pc_align translation magnitudes: 8.5 / 8.8 / 9.9 / 15.1 km
  (so ~8-15 km, max ~15 km). The CTX box must contain the footprint + ~2x(max offset) + margin ~=
  footprint + ~40 km. For a ~40 km footprint that is about a **3x box** (covers +-60 km, 4x the observed
  max offset). Do NOT blindly use 6x - it is ~4x too big, pulling in HUNDREDS of far-margin DTMs that are
  never displayed (the zoomed-out view is ~1.6x the footprint) or used for align/eval (both crop to the
  footprint), burning download + blend cycles for nothing. Size the box from the measured offset: BOXF~3,
  or box = footprint + a fixed ~40 km margin. (If a site's own align shows a bigger offset, grow it.)
- **HRSC as a continuous-coverage reference (per Gemini, UNVERIFIED - verify before relying).** Where
  controlled CTX is genuinely sparse, HRSC DEMs give continuous coverage. Claim to verify vs ESA PSA /
  DLR HRSC specs: the HRSC regional block mosaics (HMC30, HRSC Mars Chart) carry a DEM/DTM at exactly
  50 m/px, downsampled to co-register with the 50 m/px controlled regional image mosaics (per-orbit
  single-track HRSC DTMs are a different, finer/variable resolution, ~50-200 m). A 50 m HRSC reference is
  coarser than the ~18 m CTX, so judge whether it is good enough to register CaSSIS (~4.6 m) against.
- **Efficient warp:** warp each DTM to the 18 m local-stereo grid at its OWN NATIVE extent (`-tap`, so
  tiles share a grid), gdalbuildvrt, then ONE gdalwarp crop to the box `-te`. Never warp each DTM to the
  full box `-te` (a non-overlapping DTM then becomes a full-box nodata tile, huge at a 5-6x box).
  Native-extent warps are box-INDEPENDENT, so they cache across box sizes.
- **Judge quality by PURE grayscale hillshade** at matched extent, 1-to-1 visual check, NOT the pc_align
  residual (that measures absolute-registration difficulty, not DEM quality). Color-hillshade washes out
  the subtle ridges/waves that prove a match.
- **The mapprojDem (CTX blur5) must be REGENERATED whenever the CTX changes, or the framelet DEM silently
  loses coverage (proven 2026-10-30).** The frame-camera stages (dense matches + pass1 pairwise stereo)
  MAPPROJECT every framelet onto the blurred CTX (`<tag>_ctx_18m_blur5.tif`, config `mapprojDem`). A framelet
  that lands where the blur5 is NODATA mapprojects to EMPTY and is DROPPED. So a STALE blur5 (left from an
  earlier, gappier CTX build) caps the final framelet DEM: 003837 had a complete sharp CTX (100% over the
  footprint) but a stale blur5 (51%), and the framelet DEM came out 22% - two-thirds MISSING - while the
  aligned LINESCAN DEM (which uses native stereo, NO mapproject) stayed complete. The bundle was clean
  (sub-pixel toc residuals, cameras shifted only by the ~8-15 km pointing offset) - the loss was ENTIRELY
  the stale mapprojDem. ALWAYS rebuild the blur5 FROM the current ctx_18m (`dem_mosaic --dem-blur-sigma 5`)
  as part of the CTX build; fast_ctx.sh now does this. When the CTX is redone, WIPE the frame stages that
  mapprojected onto the old blur5 (frame/dense, frame/pass1*) and reprocess stages 6-7.
- **ALWAYS INSPECT THE FINAL (framelet/pass1) DEM FOR EXTENT + COVERAGE vs the ALIGNED LINESCAN DEM AND the
  CTX - not just the dz/dh-dv numbers.** The dz/dd eval measures registration ONLY where the DEM has data,
  so it HIDES missing coverage entirely (003837 scored dz NMAD 2.2 m while two-thirds of the DEM was gone).
  The aligned linescan DEM (`linescan/linescan_dem/align/aligned_oncoarse.tif`) is the known-good ~complete
  baseline (it is not mapproject-limited); the framelet pass1 DEM (`frame/pass1_stereo/cassis_dem_on_ctx
  .tif`) MUST cover a similar extent. Make a 3-panel hillshade at a shared ZOOMED-OUT extent (the linescan
  footprint + margin): CTX | aligned linescan | framelet pass1, and compare valid% over the footprint. If
  the framelet DEM covers much less than the linescan, something upstream dropped framelets (first suspect:
  a stale/eroded mapprojDem). Do this at the END of every run, per site.
- **PLOTTING / RESULTS TOOLS live in CassisPipeline/bin (the repo, not campaign scratch).** `cassis_eval_plots
  .py <tag> <ctx_18m> <aligned_linescan> <final_on_ctx> <dz-diff> <ddH> <ddV> <outdir>` makes the six
  evaluation panels per site (row 1 CTX | aligned linescan | final stage-7 DEM = the coverage check above;
  row 2 dz | dd-H | dd-V) as single images + a `<tag>_stats.tsv`. `cassis_gallery.py <manifest.tsv>
  <panels_dir> <out.html>` assembles a self-contained HTML gallery (3 panels per row) from a per-site
  manifest (tag/id/coord/conv/group/note) + those stats files. Both run with the `geo` conda env. These
  supersede the earlier ad-hoc make_cassis_plots.py / make_diag.py / make_site6.py / build_*.py.
- **After ANY CTX / blur5 / reference change, REPROCESS THE FULL PIPELINE (stages 1-7), not just the
  downstream stages (6-7).** A 6-7-only reprocess (fresh matches + stereo, but REUSING the old stage-1-5
  cameras) recovers coverage but can leave a BUNDLE DATUM SPLIT: if the recovered framelets form a group
  disconnected from the original ones (a mid-strip gap), the tic-toc BA solves the two groups at different
  heights and you get a ~100 m VERTICAL STEP between blocks (003837: 6-7-only gave dz median 82.8 m; full
  1-7 gave -0.13 m / NMAD 2.2 m). A full 1-7 rebuilds the cameras from scratch so the framelets stay one
  connected block at one datum. cassis_e2e_node.sh FROM<=1 wipes frame/ + cams_aligned and force-realigns
  for exactly this.
- **KEEP vs WIPE on a CTX-reference rebuild (so a redo never re-investigates the dependency split).** When
  the CTX reference DEM changes, redo FRESH with NO cache reuse (cassis_process.sh skips a stage whose
  output exists, so the stale outputs must be gone first). Do NOT redo in place. KEEP the CTX-INDEPENDENT
  inputs and the expensive linescan products: the `.cub` and ISD/`.json` (ingested inputs, never wiped);
  the linescan strips (`*_strip.tif`) + linescan jsons; the linescan STEREO DEM with its BA states
  (`linescan/linescan_dem/ba`) and stereo point cloud (`linescan/linescan_dem/stereo`) - the linescan DEM
  is built from the two linescan cubes with NO CTX, so it is CTX-independent; and the cached per-DTM CTX
  warps (`ref/site<S>_ctx/dl/*_gw.tif`, box-independent, reusable across CTX rebuilds). WIPE everything that
  derives from the CTX: the CTX reference products themselves (`site<S>_ctx_18m.tif`, `_blur5`, `_hillshade`
  - rebuild them); the linescan-to-CTX alignment (`linescan/linescan_dem/align/aligned_oncoarse.tif` +
  `run-transform.txt`); the CTX-aligned cameras (`linescan/linescan_dem/cams_aligned/`); and the WHOLE
  `frame/` tree (stages 3-7: aligned_framelets, distortion_corrected_cassis_cams, registered_cassis_cams,
  dense, pass1, pass1_tic, pass1_stereo) - all of it hangs off the CTX-aligned cameras and the mapproject
  onto the CTX. Then regenerate blur5 from the new ctx_18m and reprocess 1-7. Do the file removals in python
  (shutil.rmtree / os.remove), NOT a shell `rm` with a path variable. cassis_e2e_node.sh FROM<=1 already
  does this exact split; a CTX-only fix is: rebuild the reference (cassis_ctx_blend.sh / the repo's
  cassis_ctx_build.sh CTX_STACK=sub8) then run cassis_e2e_node.sh <site> 1 7.
- **Head node = downloads only.** STAC query + DTM download is head-node-legal (network, single process);
  the warp/mosaic MUST be qsub (a head-node gdalwarp batch trips the NAS front-end policy gate and emails
  a "process killed" notice - the compute jobs are fine, it is the login-node steps that get killed).
- **ALWAYS display CaSSIS-vs-CTX ZOOMED OUT, with context around the DEM - never cropped tight to the DEM
  bounding box.** The un-aligned CaSSIS DEM can be shifted from the CTX by its full pointing error (hundreds
  of m to ~km), so a tight crop can push the matching terrain right out of frame and you see NO common
  overlap (one panel is simply shifted off the other). Render BOTH panels at a shared, padded SQUARE extent
  centered on the DEM (side = max(dem_w, dem_h) * ~1.6), so the CaSSIS DEM reads as a small thumbnail and the
  CTX fills the surrounding "real estate" - the matching landforms then stay visible despite the offset, and
  the eye can shift to line them up. `make_matched_hs.py` (env `PAD`) does this.
- **Keep BOTH a no-blend and a blend path (fast_ctx `BLEND` env).** Build the plain gdalbuildvrt first-valid
  stack FIRST - it is fast and answers the only question that matters early: is there enough CTX coverage?
  (Blending hard before you know coverage is pointless - it was wasted effort on the first pass.) THEN, once
  coverage is confirmed, run the outlier-curated BLEND to clean the result: dem_mosaic consensus (median for a
  robust first pass) -> geodiff each native warp vs the consensus -> drop the tiles that disagree by height
  (|mean| large) OR carry a stripe/noise artifact (std large) -> re-mosaic, with a floor on how many to keep.
  Two rounds (gross vs median, then mild vs mean) removes the junk tiles (e.g. a diagonal-dune controlled DTM)
  without gutting coverage, because the box is big and coverage is already solid.
- **The FINAL full-res blend must FEATHER, never `dem_mosaic --median`.** A per-pixel median gives full
  coverage and rejects outlier VALUES, but it does NOT feather across tile boundaries, so where the set of
  contributing DTMs changes you get HARD TILE SEAMS (straight rectangular lines in the hillshade), and those
  seams then show up as artifacts in the dz difference. fast_ctx `BLEND=1` used `--median` and left exactly
  these seams (002340, confirmed from the build log `--threads 24 --median` and the output
  `_blend-tile-0-median.tif`, versus the seamless feather sites whose output is `_blend-tile-0.tif`). Use
  median ONLY for the sub-8 (144 m) consensus that drives the outlier cull, then do ONE full-resolution
  FEATHERED blend (plain `dem_mosaic`, no `--median`) of the surviving tiles. This is the robust recipe in
  cassis_ctx_blend.sh and the repo's cassis_ctx_build.sh CTX_STACK=sub8. To tell which blend a built CTX
  used, look at the tile name: a `-median` suffix means seams, plain `_blend-tile-0.tif` means feathered.
The campaign builder that encodes all of this: `fast_ctx.sh` (env `BOXF`/`MAXN`/`MAXDL`/`STAC_LIMIT`/
`EXPAND`/`DL_ONLY`/`WARP_JOBS`; head node runs `DL_ONLY=1` downloads, a qsub compute node does the
warp+crop). It lives in the campaign dir, not yet in CassisPipeline - see the migration plan next.

**Patching `cassis_ctx_build.sh` to adopt this WITHOUT breaking it.** The shipped builder (step [3]
curation + step [4] mean-mosaic, BOX_FACTOR default 6, STAC K=5 per lat bin, each DTM warped to the full
box) still runs and its D8 coverage gate is useful - so migrate by ADDITION, not rewrite:
1. Add an opt-in mode env (e.g. `CTX_STACK=vrt`), DEFAULT OFF so current behavior is unchanged. When set,
   skip the gross/mild curation AND the step-[4] mean-mosaic, and instead warp-native + gdalbuildvrt +
   crop-to-box (first-valid). Validate on several sites against the blend output, THEN consider flipping
   the default - never change it silently.
2. Fix the STAC selection: the per-0.1deg-bin K=5 cap can starve coverage. Raise K (or drop the cap)
   and/or add a footprint-overlap filter so no overlapping DTM is excluded.
3. Replace the per-DTM full-box warp (step [3], `-te` on every DTM) with native-extent warp + one final
   crop - strictly faster and makes a 5-6x box affordable.
4. Keep the D8 coverage gate (it measures footprint coverage, exactly what the vrt stack maximizes). In
   the vendor-free path the `$1` vendor arg is a constant STAND-IN raster over the ODE footprint bbox
   (`gdal_create`), so the gate still reads correctly.
Each item is a mode addition or a bounded fix, independently testable against the existing output - low
risk. Campaign log + full result: `~/projects/cassis_jay/cassis_hunt_log.sh`.

**The linescan is an APPROXIMATION - the pushframe cannot be a linescan beyond a point.**
The pipeline stacks the framelets into a synthetic linescan ONLY to get a smooth
trajectory and a rigid pc_align to CTX (stages 1-2). `pushframe2linescan.py` relabels the
assembled strip as a linescan ISD: the pose tables are copied verbatim and Lagrange-
interpolated continuously, and the two models agree EXACTLY only at framelet CENTERS and
diverge (smoothly) toward framelet edges. So the linescan removes the per-framelet pose-
STEP seam but is not geometrically faithful off-center. The HONEST final DEM MUST use the
per-framelet FRAME ("baby frame") cameras (stages 3-7) - that is the truth model. Do not
expect the linescan DEM to be the deliverable, and do not push the linescan approximation
past the trajectory+alignment role.

**DIAGNOSING ROUGH SITES (seams, "wild" patches, missing bands) - the funnel-first method,
and what the problem usually is NOT (full worked diagnosis: cassis_seam_coverage_diag_notes.sh,
2026-10-31, on 005571 + 004038).** Before blaming coverage or matches, walk the funnel - it is
usually HEALTHY and the rough look is a QUALITY problem in the covered area:
- Count framelets at each stage: `aligned_framelets/<sid>/aligned-*.json`, `registered_cassis_cams`,
  `distortion_corrected_cassis_cams`, and `frame/dense/matches/run-disp-*.match`. A dropped framelet
  shows as a count shortfall; usually none drop.
- Match density: `ls -la run-disp-*.match` sizes (~50 bytes/IP). Dense pairs are ~1-4 MB (tens of
  thousands of IP). A thin-overlap pair can be ~70 KB, but the strip is normally densely matched.
- BA health: in `frame/pass1_tic|pass1/run-log-bundle_adjust-*.txt` read the percentile outlier
  bounds (sub-pixel ~ +-0.4 px = healthy) and the "Removed N outliers out of M" (<1% = healthy).
  No cameras dropped = the fixed-GCP tic did NOT wreck the solution.
- Coverage: compare final `cassis_dem_on_ctx.tif` valid pixels to the aligned linescan
  `aligned_oncoarse.tif` IN THE VALID BBOX (both are ~50% for a diagonal strip - equal means full).
  A whole-padded-canvas percentage is MEANINGLESS (the footprint is ~1% of a 6x CTX box).
- The usual culprit is the per-framelet-pair HEIGHT SEAM: the final per-pair stereo DEMs disagree
  slightly where they overlap and dem_mosaic blends a STEP. Tell it from a stereo blunder by tri-error:
  render `max_tri_err.tif` next to `cassis_dem_ctxdiff-diff.tif` (dz). LOW tri-err (median ~1-3 m, at
  the 8 m blunder-tol cap) + a razor-straight dz step = a COHERENT geometric seam (survives blunder
  removal), NOT a blunder. It is worst on high-relief, low-texture terrain (a crater floor), where a
  tiny pointing/geometry difference or a small horizontal shift over a steep wall makes a big +-dz
  dipole. Levers: toc `htUncTight` (5-10 is the seam-free regime, 1 over-tight - test 20 per site),
  the `--ip-match-radius` reroute that killed coherent false locks once, or inject the final distortion
  earlier. This is NOT fixed by "more matches" - the matches are already dense.
- One GENUINE coverage gap is real and unfixable by matching: the strip ENDS are single-look. The two
  looks are offset along-track (telescope rotated 180 deg, L framelet i overlaps R framelet ~i+4/5),
  so the last few framelets of a look have no cross-look partner -> no stereo -> an end gap. Expect it.

**THE SEAM ROOT CAUSE is a BROKEN SAME-LOOK ADJACENCY TIE (confirmed on 005571, 2026-10-31).** Two
overlaps matter and are DIFFERENT: cross-look (L vs R) gives the stereo baseline; same-look (L framelet
i vs i+1) gives the along-track TIE that keeps the trajectory continuous. The frame-camera BA ties
consecutive same-look framelets ONLY through local image matches, and when a tie fails that framelet is
held only by cross-look stereo + the CTX height pin + the loose 500 m pose leash, so it FLOATS into a
height STEP = the seam. The decisive test and fix:
- The stitched LINESCAN STRIP IS CONTINUOUS, which PROVES the same-look framelets physically overlap
  EVERYWHERE (the data is there). Framelet sub-latitudes are evenly spaced (no jump). So a "missing"
  same-look tie is NEVER a real gap - it is a MATCH failure on present data, and is recoverable.
- Count it: the dense log prints `pairs: LR N  same-look M`; compare M to the same-look `.match` files
  produced. The shortfall = the number of broken ties = the number of seams. On 005571, 9 of 58 failed,
  at the low-texture crater floor (BOTH looks fail at the SAME ground - an independent-look coincidence
  that can only be terrain) and the faint strip start. Rich-texture framelets tie fine, so "works some,
  fails others" is TEXTURE, not overlap - the overlap is identical for every pair.
- FIX (a local hack in cassis_stereo_pair.sh mode `same`, CassisPipeline main): affineepipolar FIRST (keeps the
  textured pairs it already matches), then on abort fall back to a CROP-WINDOW matcher restricted to the ~57-row
  overlap band, whose end is chosen deterministically from each look's framelet_order_reversed, with a disparity
  outlier-filter-kernel retry (default, then a smaller rm-half-kernel) for the thinnest low-contrast pairs. The
  FULL why (the two filter stages, why the default rm kernel over-scrubs the thin band's D_sub, the fwd/aft
  overlap-end rule, the uniform-crop-win regression) lives in the CODE COMMENTS - do not re-explain it here.
  Deep paper trail: ~/projects/cassis_asp/cassis_seam_coverage_diag_notes.sh.
- The tie recovery reconnects the broken-tie segments (coverage/continuity), but the crater DZ DIPOLE is a
  SEPARATE problem: a +/- split across a steep crater wall is a horizontal misregistration (dem2gcp/BA-
  horizontal or crater-floor CTX error), NOT a tie issue, and ties do not fix it. htUncTight (vertical, tested
  worse at 20) and fix-gcp are also SECONDARY - fix the TIE first, then the horizontal/CTX separately.
  Full arc (alignment-none misstep, root cause, fallback): ~/projects/cassis_asp/cassis_seam_coverage_diag_notes.sh.

**A dz TILT in a stage-7 DEM = the MISSING SECOND PASS, not a bad CTX (cassis_jay tilt work, 2026-11-02).**
A stage-7-only CaSSIS DEM can show a smooth cross-track (or along-track) RAMP in dz vs CTX. Root cause: the
stage-1 CTX alignment (cassis_linescan_dem.sh) is TRANSLATION-ONLY (NCC NED shift -> pc_align
--num-iterations 0 --initial-ned-translation), so it CANNOT remove a residual ROTATION between the CaSSIS
DEM and CTX - that rotation survives as the tilt. The fix is a low-frequency re-pin to CTX, and there are
TWO equivalent ways:
  - STAGE 8 (pass2, the 2nd tic-toc dem2gcp pass) removes it natively. PROVEN on hunt3 4864: stage-7 tilt
    gradient 3.83 m/km / ramp 30.8 m -> stage-8 0.385 m/km / 3.6 m (10x), dz NMAD 9.75 -> 5.30. So a
    stage-7-only site is tilted PRECISELY BECAUSE it never got stage 8.
  - pc_align point-to-plane (rigid, solves rotation) of the final DEM to CTX is a CUBE-FREE EQUIVALENT -
    use it when the cubs are gone (e.g. a consolidated/distilled deliverable). pc_align CTX(ref,FIRST)
    finalDEM(src,SECOND) --save-transformed-source-points -> point2dem -> re-eval. On 002257 it gave the
    same flattening (grad 2.90 -> 0.38 m/km, ramp 33.7 -> 7.5 m, dd stayed sub-pixel). Worker +
    plane-fit metric: ~/projects/cassis_asp/cassis_tilt_pcalign.sh + cassis_plane_fit.py.
Judge a tilt QUANTITATIVELY by plane-fitting the dz (gradient m/km + end-to-end ramp + de-tilted NMAD): a
real tilt has a large raw NMAD but a SMALL de-tilted NMAD (the scatter IS the ramp); after the fix the ramp
collapses and raw NMAD ~= de-tilted NMAD (only terrain scatter left). What pc_align/stage-8 do NOT fix: a
cross-track BOWL (the two-look BA near-degeneracy, non-rigid - 5150) or framelet SEAMS (broken same-look
ties) - those are separate, harder.
  - **point2dem --t_srs needs a PROJ4 string, NEVER a WKT carrying the D_MARS datum name** (it fails
    "D_MOON and D_MARS are not official projection strings"). Use `gdalsrsinfo -o proj4` (as the repo does
    in cassis_linescan_dem.sh / cassis_stereo.sh). gdalwarp -t_srs accepts the WKT fine - only point2dem checks.

**DO NOT BLANKET-REPROCESS consolidated sites to "fix" them - a fresh run is FRAGILE and often WORSE (verified
2026-11-02).** Tempting logic: "the current pipeline fixed 002257's tilt on a fresh reprocess, so reprocess the
other problem sites too." It BACKFIRED - of 4 reprocessed (full stages 1-8, reusing each site's CTX), only
002257 improved (its tilt was a genuine stale-processing artifact: grad 2.90 -> 0.16 m/km). 002440/005571/002456
came out WORSE than their shipped deliverables - high DE-TILTED NMAD (10-30 m) = broken DEMs, and rp2440's
hillshade showed a hard mid-strip DATUM SPLIT (the tic-toc BA solved two disconnected framelet blocks at
different heights) plus WILD END FRAMELETS. A fresh run is fragile: the NCC align can lock suboptimally, the
BA can split the datum, bad end framelets go wild. So: reprocess ONLY when the original is a KNOWN bad-align
artifact AND VERIFY the fresh result (plane-fit + eyeball the hillshade for datum steps / wild framelets)
before adopting. For a plain TILT, prefer pc_align - it only rigidly rotates the shipped DEM, so it CANNOT
introduce a datum split or wild framelets, whereas a reprocess can. Gate box_te.txt first: the no-cull_diff
sites (gdalbuildvrt CTX) lack it, and ctx_blend fails without it - synthesize it from the _18m extent
(ULx LRy LRx ULy) or skip ctx_blend and reuse the existing _18m/_blur5.

**IS THE CTX REFERENCE ITSELF BAD? - the MOLA tilt check (decisive, cheap).** When a site shows a tilt or
seams, the dilemma is "bad CTX vs CaSSIS-side (fixable by align/stage-8)". The controlled CTX is MOLA-tied,
so a GOOD CTX is FLAT vs MOLA. Geodiff CTX_18m against global MOLA (isis3data/base/dems/
molaMarsPlanetaryRadius0005.cub, warped to the CTX grid) and plane-fit the result - read ONLY the GRADIENT
(MOLA is a planetary RADIUS, so the ~3.4e6 m median is just the sphere offset, and the big NMAD is only the
463m-vs-18m resolution gap). Worker: ~/projects/cassis_asp/cassis_ctx_mola_check.sh. VERIFIED 2026-11-02:
all 5 problem sites read sub-1 m/km vs MOLA (CTX GOOD everywhere), so the tilts/seams are CaSSIS-side and NO
CTX rebuild was warranted. A site whose CTX-vs-MOLA gradient rivals its CaSSIS-vs-CTX tilt would be the one
to rebuild. Full worked analysis: ~/projects/cassis_asp/cassis_tilt_notes.sh.

**A SEAM / defect / datum-split is almost NEVER "not enough same-look matches" - VERIFY the ties are
complete FIRST, then look at the GCP CONTROL (traced on the cassis_jay reprocs, 2026-11-03).** The intuitive
theory "continuity breaks because consecutive framelets don't match" is appealing and usually WRONG. On four
reprocessed sites (one clean, three broken: a datum split, seams, a stage-7 break) EVERY same-look adjacency
tie was present and DENSE - 29/29 per look, zero missing, 1-2 MB each (tens of thousands of IPs) - on the
broken sites too. So before blaming matches, DISPROVE it cheaply: classify the dense .match files into
same-look (both cubs share the PAN-<acqid>, adjacency ties) vs cross-look (L vs R stereo), and check the
per-look chain i,i+1 is COMPLETE with no gaps and the tie sizes are large (the cassis_jay reproc classifier:
~/projects/cassis_asp/cassis_match_classify.py). If the ties are complete+dense, the matches are NOT the
cause. The REAL cause of the seam/split is the HARD fix-gcp control built from a LOOSE linescan->CTX
registration (the "hard GCP from soft registration" item above), confirmed by two numbers: the BOOT dh/dv
(frame/pass1/boot/run-F-{H,V}.tif stddev) - clean site ~3 / ~0.9 px, bad site ~7 / ~3 px; and the TIC
per-camera reprojection residual (frame/pass1_tic/run-final_residuals_stats.txt col 2, avg) - clean ~2 px,
INFEASIBLE control ~4-7 px (the solver cannot satisfy the hard GCP without distorting, so it scatters
framelets / splits the datum). The TRUE root is UPSTREAM: a bad GCP0 from a POOR boot registration, and NO
gcp weighting fixes a bad GCP0 cleanly - HARD transmits it (seam/split), SOFT discards it (the cameras stop
registering to CTX, the opposite failure Oleg has hit, the DEM drifts off the reference). So softening the
GCP (gcpSigma 1->3->5->10 / relax fix-gcp / relax htUncTight) is a NARROW-SWEET-SPOT band-aid worth ONE try
per site, NOT a reliable fix. The principled fix is a BETTER linescan->CTX boot registration (NCC align /
linescan DEM quality); on low-texture high-relief sites (5571 boot dh ~7 px) the hillshade correlation that
drives both the align and GCP0 is inherently noisy, so a clean tie may be at the DATA's texture limit. Stage
8 sometimes rescues a site by rebuilding control (002456), not reliably. And it is NOT more matches.
(Strip-END framelets are a separate, real floater: single-look ends have no cross-look partner, so the
first/last framelets of a look can go wild regardless - that is geometry, not matches or GCP.)

**Distortion**: ALE emits the CaSSIS distortion (USGSCSM DistortionType CASSIS=9). A
residual cross-track BOWL is a two-look BA near-degeneracy, not a distortion bug; real
fix needs external low-freq control (LOLA/CTX). Alloc e2305.

**Notes** (our working memory, not shipped): `~/projects/cassis_asp/cassis_notes.sh`
(hub) -> `cassis_reprocess_relax_htdem_notes.sh` (reprocess plan, ms_cassis status),
`cassis_cleanup_plan.sh` (tape archive inventory + per-dir wipe log).

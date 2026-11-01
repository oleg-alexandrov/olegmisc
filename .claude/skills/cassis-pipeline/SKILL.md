---
name: cassis-pipeline
description: The TGO CaSSIS pushframe-as-framing stereo pipeline in ASP - the CassisPipeline repo and its bin/ stage scripts, the ms_cassis.py per-pair stereo engine (and its swap to the shipped multi_stereo tool), the pfe run layout and replicas, the build-floor date check, and where the CaSSIS notes live. Load whenever working on CaSSIS - CassisPipeline, ms_cassis, cassis_stereo/cassis_process, framelets, pushframe, or a CaSSIS DEM replica on pfe. Complements multi-stereo (the engine), ctx-processing (the CTX reference), pc-align, dem-comparison.
metadata:
  type: reference
---

CaSSIS (TGO) acquires PAN pushframe framelets that ASP treats as many small FRAMING
cameras. The pipeline builds a frame DEM per observation, tied to a CTX reference DEM.

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
  SEPARATE caveat: a de-seamed reference can shift the CaSSIS-to-CTX pc_align solution (002340's align
  jumped to ~20 km and broke the framelet cameras) - that is a coarse-align fragility, NOT a CTX defect.

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
- Code cause: full-frame `--alignment-method affineepipolar` ABORTS ("IPs left after filtering is 17 < 20
  required") when IP MATCHING (not detection - detection finds thousands) starves on the low-texture crater,
  because it needs >=20 matched IPs to estimate the search range over the ~220-line raw disparity.
- THE SHIPPED FIX (CassisPipeline main a12b622, cassis_stereo_pair.sh mode `same`): affineepipolar FIRST
  (keeps every textured pair it matches = the baseline set), and ONLY on abort/no-match FALL BACK to a
  CROP-WINDOW matcher that restricts the correlator to the ~57-row overlap band
  (`--left/right-image-crop-win`, `--alignment-method none`, small `--corr-search -24 -55 24 55`,
  `--num-matches-from-disparity`). Inside the band the overlap is ~full and the disparity is a few pixels, so
  it locks densely with NO interest points, and ASP emits matches in FULL-FRAME coords (no crop undo). The
  OVERLAP END is deterministic from each look's `framelet_order_reversed` (recorded by the stitch, now passed
  via the pair env): reversed=1 -> lower-index framelet overlaps at its TOP (higher at BOTTOM), reversed=0 ->
  opposite. VERIFIED on both looks (they are the 180-deg pair, always opposite). DO NOT use crop-win wholesale:
  tested 2026-10-31, uniform crop-win REGRESSED (same-look 49->28) because at the ~0.16-deg same-look
  convergence the num-matches-from-disparity TRIANGULATION fails "Empty bounding box" on the lowest-convergence
  pairs that affineepipolar's full-frame geometry handles fine. With the fallback, 005571 went 49->55 ties
  (affine 49 + crop-win rescued 6), only 3 deepest-convergence pairs (618 crater, ~0.16 deg) still unmatched -
  those need a NON-triangulating tie path (ipmatch gave ~16 ties on that terrain) as a future third tier.
- The tie recovery reconnects the broken-tie segments (coverage/continuity), but the crater DZ DIPOLE is a
  SEPARATE problem: a +/- split across a steep crater wall is a horizontal misregistration (dem2gcp/BA-
  horizontal or crater-floor CTX error), NOT a tie issue, and ties do not fix it. htUncTight (vertical, tested
  worse at 20) and fix-gcp are also SECONDARY - fix the TIE first, then the horizontal/CTX separately.
  Full arc (alignment-none misstep, root cause, fallback): ~/projects/cassis_asp/cassis_seam_coverage_diag_notes.sh.

**Distortion**: ALE emits the CaSSIS distortion (USGSCSM DistortionType CASSIS=9). A
residual cross-track BOWL is a two-look BA near-degeneracy, not a distortion bug; real
fix needs external low-freq control (LOLA/CTX). Alloc e2305.

**Notes** (our working memory, not shipped): `~/projects/cassis_asp/cassis_notes.sh`
(hub) -> `cassis_reprocess_relax_htdem_notes.sh` (reprocess plan, ms_cassis status),
`cassis_cleanup_plan.sh` (tape archive inventory + per-dir wipe log).

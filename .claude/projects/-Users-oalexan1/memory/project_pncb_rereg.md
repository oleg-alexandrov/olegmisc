---
name: PNCB re-registration key paths
description: Where the LOLA-aligned GCP file, the final BA output, and the plan notes live for the PNCB re-registration (spring 2026).
type: project
originSessionId: 4e721443-3adf-4e71-8d76-772f811b9dfa
---
PNCB ("Peak near Cabeus B") re-registration run begun 2026-04-20. Purpose: fix residual misalignment in the delivered SfS DEM (pncb_results/sfs_dem_blend.tif) flagged by Ross.

**Why:** The delivered SfS blend had a +4.6 m systematic V-bias vs LOLA (per SfS vs LOLA dense correlator, 2026-04-20). Sim-align per-image + dem2gcp (transform GCPs via the SfS->LOLA disparity) + final BA against LOLA GCPs removes that bias.

**How to apply:** When asked about PNCB re-registration outputs or need to redo any step, start with the plan file `~/projects/PNCB/pncb_registration.sh` (reorganized 2026-04-21 - context sections on top, step details in execution order below). Key on-disk artifacts on pfx (`/nobackupp19/oalexan1/projects/PNCB/`):

- **`ref/pncb_lola_gcp.gcp`** - the merged LOLA-aligned GCP file. 2.5M points (truncated from 5.84M), gcp-sigma=1.0 embedded. Produced by `dem2gcp_pncb.sh` (job 24390239, 2026-04-21) from 260 sim-align per-image GCPs transformed via `sfs_corr_vs_lola/run/run-F.tif`. 350 MB on disk. Reuse directly as `--gcp` input for any future BA.
- **`ba_final/`** - final re-registered BA output (job 24390343, 2026-04-21, 2h01m). All 301 cameras free (no `--fixed-image-list`), htdem-uncertainty=25 (loose), GCPs dominant. `ba_final/run-*.adjusted_state.json` are the amended cameras.
- **`ba_htdem2/`** - prior DEM-constrained BA (starting point for ba_final). 237 cameras fixed, htdem-uncertainty=10 (tight).
- **`ba_regen/`** - fresh match regeneration on top of extracted tarball.
- Excluded-from-mosaic images (bad mapproj offsets post-ba_final, 2026-04-21): M1158377570RE, M1158377570LE, M1106497568RE.
- **Canonical SFS image list post-re-reg:** `lists/mapproj_final_selected.txt` (234 ids) = `pncb_results/sfs_image_ids.txt` (237) minus `lists/mapproj_excluded.txt` (the 3 above). When regenerating anything in `pncb_results/` (sfs_dem_blend.tif, max_lit_mosaic.tif, height_uncertainty.tif, sfs_image_ids.txt), the source list MUST be `lists/mapproj_final_selected.txt`, not the old `sfs_image_ids.txt`.

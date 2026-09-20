---
name: sun-illumination
description: Sun azimuth/elevation analysis for camera co-registration and SfS work - compute per-camera sun az/el (ASP sfs --query on cub+json, or from the CSM m_sunPosition, validated identical), make the sun-azimuth ROSE plot (polar scatter of the reference set vs the target set), and reason about it (grazing polar sun -> shadows dominate -> cross-set matching needs SAME-azimuth pairs; is the target's trouble illumination diversity or not). Load ANY time you do lunar (or other) camera registration / co-registration / SfS work, or need to explain a matching difficulty by illumination. Complements csm-models, dem-comparison, sfs work.
---

Load this for ANY SfS / lunar (or other planetary) camera registration work - there is
a lot of it and this "funny kind of analysis" recurs. The sun-azimuth rose (below) is a
strongly-encouraged standard figure: make it every time you co-register two image sets.

## Why sun geometry matters for registration

At grazing sun (polar sites, elevation ~1 deg) appearance is dominated by SHADOWS, which
point along the sun AZIMUTH (elevation barely matters for the shadow DIRECTION). So:
- Two images co-register (correlate / IP-match) well only when their sun AZIMUTHS are
  close - matched shadows. A "maxlit" mosaic blends azimuths and is poor for matching.
- Cross-instrument matching (e.g. OHRC vs LRO-NAC) must pair SAME-azimuth images.
- If a reference set spans all azimuths and still co-registers, then illumination
  DIVERSITY is NOT the target set's problem - look elsewhere (pose/jitter/distortion).

## Compute per-camera sun az/el

Two equivalent ways (validated to agree exactly):
1. ASP tool: `sfs -i DEM.tif --image-list imgs.txt --camera-list cams.txt -o x -n 1 --query`
   prints "Sun position ..." and "Sun azimuth and elevation for: <img> are AZ and EL
   degrees." at the DEM. Needs image+camera (cub+json).
2. From the CSM state json (`m_sunPosition`, body-fixed meters, a 3*N time series - take
   the MIDDLE triplet) at the scene ground point P, in a local East-North-Up frame
   (az from North, +East). Parse the state by dropping the plugin-name header line
   (see [[csm-models]]). Formula that reproduces sfs exactly:
```
# P = ground point body-fixed XYZ (R * [cos b cos l, cos b sin l, sin b]); R=body radius
d = S - P ; up = P/|P| ; east = normalize(z_axis x up) ; north = up x east
el = asin(d.up / |d|) ; az = atan2(d.east, d.north)   # degrees
```
Use method 2 when you have jsons but no cubs (e.g. only camera states in hand).

## The sun-azimuth ROSE plot (the figure Oleg loves - make it every time)

Polar scatter, sun azimuth on the angle, the two sets at two fixed radii JUST for
legibility (radius is not data). theta zero at North, clockwise. Reference set one color,
target set another; legend with counts. matplotlib:
```
ax = plt.subplot(111, projection='polar'); ax.set_theta_zero_location('N'); ax.set_theta_direction(-1)
for vals,c,lab,r in [(ref_az,'#1f77b4','REF (n)',1.0),(tgt_az,'#d62728','TGT (n)',1.25)]:
    ax.scatter(np.radians(vals), [r]*len(vals), c=c, s=45, alpha=0.75, edgecolors='k', linewidths=0.3, label=lab)
ax.set_rmax(1.5); ax.set_yticklabels([]); ax.set_rticks([]); ax.legend()
```
Reference recipe used on OHRC/NAC: `~/projects/ohrc_lronac_align/corr/` (rose + sfs-query
tables in sun_analysis.txt). Put ALL numbers/interpretation in the caption, none baked in
the figure (see [[visual-inspection]]). Report az SPREAD per set, mean elevation, and the
nearest-azimuth cross-set pairs (for choosing which images to match/correlate).

## Reading it

- Reference spans full 360 deg, target a sub-arc: illumination diversity is not the
  target's problem; but for each target image find its nearest-azimuth reference twin
  (tabulate the deg-difference) and match THOSE, not blended mosaics.
- Watch for azimuth BANDS where the reference is sparse (e.g. NAC thin in 0-45 deg):
  target images there have looser twins - flag it (not necessarily fatal).

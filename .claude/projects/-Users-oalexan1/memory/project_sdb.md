---
name: SDB (shallow depth bathymetry)
description: Current active project - shallow depth bathymetry with ASP. FY26 USGS task with Palaseanu/Bhushan. Work dir ~/projects/sdb/
type: project
originSessionId: 832e7be3-8a95-415f-acd4-cdee708fcff1
---
Current active project starting 2026-04-20.

**Work dir:** `~/projects/sdb/` - notes in `sdb_notes.sh`.

**Goal:** Shallow water bathymetry pipeline improvements (FY26, 2 months effort).
Stakeholders: PI Palaseanu, Co-Is Bhushan, Shean, Swinski, Alexandrov.

**Why:** FY26 funded task. Three-year project 2025-07-01 through 2028-06-30.
Deliverables include improved land/water masking, cloud masking, refractive
index models, water surface elevation, and jitter mitigation using ICESat-2.

**How to apply:**
- Study sites: KeyWest1 (2015, bad jitter), KeyWest2 (2021, same-area change
  analysis), PR1 (Puerto Rico), WCaboRojo, CocosLagoon (Guam).
- Imagery: WorldView-3, 8 multispectral bands. Band 3 = green (bathymetry),
  band 7 = NIR (water/land mask), band 5 = red, band 2 = blue, band 8 = NIR2.
- Data on NAS: `/nobackupp28/sbhusha1/stv2/bathymetry_data_version1`.
  Reference lidar copied locally to `projects/sdb/ref`.
- Compute allocation: **s3319** (5000 hours, new). Also can use e2305 (SFS).
- Submit jobs from athfe01 for tur_ath, pfe for bro_ele. Helper scripts in
  `~/projects/sdb/`: `bundle_adjust.sh`, `stereo.sh`, `stereo_bm.sh`,
  `stereo_map*.sh`, `mapproject_tr1.sh`, `wait_run.sh`, `ba_bathy.sh`.
- Masking benchmark in progress: NDWI and RNDVI look best; OSI poor. Otsu
  thresholding to replace KDE. NDWI = (G-NIR)/(G+NIR), RNDVI = (R-NIR)/(R+NIR),
  OSI = (G+R)/B.
- Stereo observations: asp_bm gives more coverage than asp_mgm in deep water
  but more artifacts; mapprojected images remove artifacts but hide jitter;
  only KeyWest1 shows clear jitter (PAN regular, green piecewise-shifted).
- List of data (Google Sheets): see sdb_notes.sh line 58.

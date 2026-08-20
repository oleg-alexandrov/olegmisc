---
name: isis-data
description: ISIS mission data and SPICE kernels - the LRO NAC end-to-end ingest pipeline, CSM JSON via isd_generate, ODE search, illumination analysis, and fetching kernels with downloadIsisData or targeted rclone. Load only when working with ISIS mission cubes, spiceinit, or kernel downloads.
---

## ISIS Mission Data and Kernels

**LRO NAC end-to-end + generic ISIS kernel fetch: `~/projects/lronac_processing.sh`.**
Full ingest pipeline (lronac2isis → spiceinit → lronaccal → lronacecho), CSM JSON
via isd_generate, ODE search, illumination/azimuth analysis, and failure modes
(missing CK, ALE driver crash, sub-solar lon vs ground azimuth). Kernel fetch
(section 5): `downloadIsisData <mission> $ISISDATA` for a full sync, or targeted
`rclone --config $ISISROOT/etc/isis/rclone.conf copy <mission>:kernels/ck/ ...
--include="<file>" --no-traverse -P` for a single missing CK. Update on any new
gotcha.

---
name: lro-nac
description: >-
  Fetch and prepare LRO NAC images from a product id: the ODE REST codes that work
  (pt=EDRNAC4), the fetch tool, the ISIS ingest pipeline, the isis10-vs-isis10asp env
  gotcha, CSM JSON via isd_generate, and pairing a fresh cub with an existing
  adjusted-state json. Load when fetching, preparing, or calibrating an LRO NAC image.
---

We keep relearning how to prepare NAC - this skill is the durable version. Canonical
long-form notes: `~/projects/lronac_processing.sh`. Doc: ASP `examples/lronac.rst`.
Reusable tool: `~/bin/fetch_lro_nac.sh <PRODUCTID> [outdir] [--prep]`.

## Fetching the EDR via ODE (poorly scriptable - EXACT codes matter)

Always fetch the **EDR** (raw .IMG) and do our OWN radiometric calibration + echo.
The ODE REST API silently returns "No Products Found" for wrong codes. The ones
that WORK for raw NAC:
- `pt=EDRNAC4`  (NOT `EDRNAC`, NOT `LROCNACL`; `CDRNAC4` = calibrated radiance)
- `target=moon  ihid=LRO  iid=LROC`
- `pdsid = "nac." + lowercase(productid)`  e.g. `nac.m167222041re`

Get the .IMG URL then curl it:
```
q="https://oderest.rsl.wustl.edu/live2/?query=product&output=JSON&target=moon&ihid=LRO&iid=LROC&pt=EDRNAC4&pdsid=nac.m167222041re&results=pdf"
curl -sS "$q" -o prod.json          # parse ODEResults.Products.Product.Product_files
                                    # .Product_file[*].URL ending .IMG
curl -sS -L -O "$url"               # ~125-260 MB, from pds.lroc.im-ldi.com
```
`~/bin/fetch_lro_nac.sh M167222041RE . --prep` does all of this (and the ingest).

## ISIS ingest pipeline (order matters, per ASP examples/lronac.rst)

```
lronac2isis from=$f.IMG      to=$f.cub          # PDS3 -> cub; NO kernels needed
spiceinit   from=$f.cub                         # attach SPICE (needs populated ISISDATA)
lronaccal   from=$f.cub      to=$f.cal.cub       # radiometric (needs lro/calibration)
lronacecho  from=$f.cal.cub  to=$f.cal.echo.cub  # echo correction
isd_generate -k $f.cal.echo.cub $f.cal.echo.cub -o $f.cal.echo.json # CSM model state
```
- `lronac2isis` alone gives the correct raw cub + **dimensions** (a full NAC frame
  is **2532 samples x 52224 lines**) and needs no kernels.
- `spiceinit`/`lronaccal` need a POPULATED ISISDATA (base kernels + lro/calibration).
- **Intermediate file cleanup discipline**: Always wipe intermediate files (`$f.IMG`,
  `$f.cub`, `$f.cal.cub`, `$f.lbl`), keeping ONLY the final calibrated product
  (`$f.cal.echo.cub` and `$f.cal.echo.json`) to conserve storage and prevent using
  uncalibrated data in SfS.

## ENV GOTCHA on pfe (burned 2026-09-20)

Use the **full ISIS env** `isis10` (`ISISROOT=/swbuild/oalexan1/miniconda3/envs/isis10`),
NOT `isis10asp`: isis10asp is ASP-linkage-only and **lacks libLinearMath**, so
`lronac2isis` dies with `libLinearMath-float64.so.3.25: cannot open shared object`.
`isis10` has the lib and the ISIS apps. Set `ISISROOT`, `PATH=$ISISROOT/bin:$PATH`,
`ISISDATA`. (For ASP tools - mapproject/bundle_adjust/stereo - keep using the ASP
build + isis10asp libs; only the ISIS ingest apps need isis10.)

## SfsPipeline batch fetch/prep scripts: EXPORT the env explicitly (burned 2026-10-23)

`SfsPipeline/bin/prepare_lro_nac.py` (and `batch_prepare_lro.sh` that drives it)
DEFAULT `ISISROOT` to `~/miniconda3/envs/asp_deps`, which does not exist on pfe, and
`conda activate isis10` alone does NOT set `ISISROOT` - so the scripts build tool
paths against the wrong/absent env and `lronac2isis` dies (rc 127 / missing
libLinearMath). EXPORT the full env before calling them on pfe:
    source /swbuild/oalexan1/miniconda3/etc/profile.d/conda.sh && conda activate isis10
    export ISISROOT=/swbuild/oalexan1/miniconda3/envs/isis10
    export ISISDATA=$HOME/projects/isis3data
    export LD_LIBRARY_PATH=$ISISROOT/lib:$LD_LIBRARY_PATH
    export PATH=$HOME/projects/BinaryBuilder/StereoPipeline/bin:$PATH   # isd_generate, cam_test
The end-to-end fetch flow for a multi-site SfS job (see [[sfs]] for context,
[[pfe-nas]] for head-node/detached rules): query each site box with
`query_lro.py --lat-lon <minlat maxlat westlon eastlon> --output-products lists/siteN.txt`
(no incidence/resolution filters = maximum cubes), `sort -u` the per-site lists into a
union (one cube can cover several sites, so prep each once), then run
`batch_prepare_lro.sh lists/all.txt img --no-usgs-polar` DETACHED on the head node
(serial, resumable, and the download + spiceinit both need head-node internet).

BULK-FETCH ETIQUETTE (burned 2026-10-23): split across AT MOST TWO front-end nodes,
ONE fetcher each, on DISJOINT sublists all writing the same `img/` (batch_prepare skips
finished ids, so disjoint lists never collide). Do NOT run more than two - more fetchers
anger BOTH the pfe admins (head-node load) and the remote PDS/NAC download server. There
is no hurry; two gentle serial streams is the norm. (Split: `awk 'NR%2' ...` into two
sublists, `ssh pfeNN "nohup bash prep_cubes.sh sublist img &"` on each of two UP nodes.)

C-PREFIX -> M RECOVERY (burned 2026-10-23): `query_lro.py` (pt=EDRNAC4) occasionally
emits a few CDR ids with a leading "C" (e.g. C109733986LE) instead of the EDR "M" id.
Real NAC EDRs are ALWAYS M<digits>LE/RE. A C-id is the SAME observation as its M EDR, so
REWRITE C->M (`sed 's/^C/M/'` then `sort -u`), do NOT drop it - dropping loses a real
observation (here the C-ids had no M twin in the list). The C-id also fails spiceinit
("Can not find [BODY_CODE]"); the rewritten M EDR ingests fine. Worth a pipeline fix:
filter/normalize query_lro.py output to `^M[0-9]+(LE|RE)$`.

SPICEINIT "Can not find [BODY_CODE] in text kernels" (burned 2026-10-23): even with
every listed kernel present on disk, spiceinit can fail camera init this way when
ISISDATA is incompletely populated. Fix by populating base + lro:
`downloadIsisData base $ISISDATA` then `downloadIsisData lro $ISISDATA` (detached on the
head node; lro ck/spk are large but needed so every observation date resolves). web=true
is NOT a reliable fallback (the ISIS spice server returned errors).

## ISISDATA on pfe is often NOT populated

`/nobackupp19/oalexan1/isis3data` (= pfe `$HOME/projects/isis3data`) has been found
essentially EMPTY (no `base/kernels/lsk/*.tls`, no `lro/calibration`), so
`spiceinit`/`lronaccal` fail there. Fix: `downloadIsisData` to populate it (see
[[isis-data]] section 5), or targeted rclone of the needed base + lro kernels.

## Pairing a fresh cub with an EXISTING adjusted-state json (no re-bundle)

Common situation (OHRC/NAC handoff): we HAVE the bundle-adjusted CSM json but NOT
the raw cub. Fetch the EDR and `lronac2isis` it; if the fresh cub's dims MATCH the
json's `m_nLines`/`m_nSamples` (e.g. 52224 x 2532), you can pair the raw cub with
the existing adjusted json directly - the json IS the camera (self-contained CSM),
the cub is only the pixel/dims container. NO re-bundle, NO spiceinit needed (we pass
the json as the camera, `-t csm`). Always CROSS-CHECK cub dims vs json rows/cols
first (`gdalinfo` Size vs json m_nLines/m_nSamples). Calibration (cal/echo) does not
change dims; skip it if you only need geometry/matching (matching is done on
mapprojected images). See [[csm-models]] for the json format.

## CSM JSON from a cub (when you need a fresh camera)

`isd_generate -k -v $f.cub` (ALE) -> `$f.json` CSM model state. Needs a spiceinit'd
cub and a populated ISISDATA/ALESPICEROOT. Driver: LroLrocNac. Detail + failure
modes in `~/projects/lronac_processing.sh` and [[isis-data]].

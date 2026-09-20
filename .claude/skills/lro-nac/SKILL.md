---
name: lro-nac
description: Fetch and prepare LRO NAC (Lunar Reconnaissance Orbiter Narrow Angle Camera) images from a product id - the ODE REST codes that actually work (pt=EDRNAC4, not EDRNAC), the fetch tool, the ISIS ingest pipeline (lronac2isis / spiceinit / lronaccal / lronacecho), the isis10-vs-isis10asp env gotcha, CSM JSON via isd_generate, and pairing a freshly-fetched raw cub with an existing adjusted-state json (no re-bundle when dims match). Load whenever fetching, preparing, calibrating, or CSM-ing an LRO NAC image, or when you need a NAC raw cub that is missing. Complements isis-data (broader ISIS/kernel hub + lronac_processing.sh) and csm-models.
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

## ISIS ingest pipeline (order matters)

```
lronac2isis from=$f.IMG      to=$f.cub          # PDS3 -> cub; NO kernels needed
spiceinit   from=$f.cub                         # attach SPICE (needs populated ISISDATA)
lronaccal   from=$f.cub      to=$f.cal.cub       # radiometric (needs lro/calibration)
lronacecho  from=$f.cal.cub  to=$f.cal.echo.cub  # echo correction
```
- `lronac2isis` alone gives the correct raw cub + **dimensions** (a full NAC frame
  is **2532 samples x 52224 lines**) and needs no kernels.
- `spiceinit`/`lronaccal` need a POPULATED ISISDATA (base kernels + lro/calibration).

## ENV GOTCHA on pfe (burned 2026-09-20)

Use the **full ISIS env** `isis10` (`ISISROOT=/swbuild/oalexan1/miniconda3/envs/isis10`),
NOT `isis10asp`: isis10asp is ASP-linkage-only and **lacks libLinearMath**, so
`lronac2isis` dies with `libLinearMath-float64.so.3.25: cannot open shared object`.
`isis10` has the lib and the ISIS apps. Set `ISISROOT`, `PATH=$ISISROOT/bin:$PATH`,
`ISISDATA`. (For ASP tools - mapproject/bundle_adjust/stereo - keep using the ASP
build + isis10asp libs; only the ISIS ingest apps need isis10.)

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

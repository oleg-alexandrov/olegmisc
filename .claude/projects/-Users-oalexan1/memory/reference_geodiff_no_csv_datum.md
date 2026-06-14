---
name: geodiff has no --csv-datum flag (that is stereo_gui only)
description: ASP geodiff accepts --csv-format and --csv-srs but NOT --csv-datum; using it triggers the help-and-exit path with no error message.
type: reference
originSessionId: 37b98be3-16e5-4752-af1b-25aa07b6585d
---
`geodiff` does NOT have `--csv-datum`. That flag exists only on
`stereo_gui` for visualizing CSV point clouds. Pass `--csv-datum` to
`geodiff` and it silently prints its full `--help` text and exits
without computing anything.

For lunar / Mars CSV with lon, lat, radius_km columns, the working
form is:

```bash
geodiff dem.tif lola.csv --csv-format 2:lon,3:lat,4:radius_km -o out
```

The CSV is interpreted in the DEM's projection by default, so the
datum gets inherited from the DEM geoheader. To override, use
`--csv-srs <proj-string>` (or its alias `--csv-proj4`).

Other geodiff gotchas worth remembering:

- Without `--absolute`, output is signed (DEM minus reference). Take
  abs in post if needed.
- The CSV-vs-DEM form writes `<prefix>-diff.csv` (not a .tif).
- geodiff prints Min / Mean / Median / StdDev to stdout directly --
  no need to compute them from the diff CSV with awk.

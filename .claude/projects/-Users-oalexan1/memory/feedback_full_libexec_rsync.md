---
name: full libexec rsync L1 -> pfx
description: Always rsync the FULL install/bin to pfx libexec, never selective. One stale stereo_* worker silently breaks the whole pipeline.
type: feedback
---

When syncing an L1 ASP build to pfx, **always rsync the entire
`~/projects/StereoPipeline/install/bin/` directory to
`~/projects/BinaryBuilder/StereoPipeline/libexec/` on pfx**, not a selective
list of binaries.

```bash
# CORRECT: full sync
ssh l1 "rsync -avz --checksum ~/projects/StereoPipeline/install/bin/ \
  pfx:/home6/oalexan1/projects/BinaryBuilder/StereoPipeline/libexec/"
```

**Why:** parallel_stereo is a Python wrapper that fans out to many C++
workers (`stereo_parse`, `stereo_pprc`, `stereo_corr`, `stereo_blend`,
`stereo_rfne`, `stereo_fltr`, `stereo_tri`, `stereo_gui`, plus
`bundle_adjust`, `point2dem`, `geodiff`, etc.). On 2026-04-26 a selective
rsync that only refreshed `cam_test`, `parallel_stereo`, `image_calc`,
`point2dem`, `geodiff` left `stereo_parse` 6 days stale. cam_test smoke
test passed (it's standalone), but parallel_stereo failed on a wl.tif
input because the old stereo_parse linked to an old libVwCartography
that lacked the readBathyPlane raster dispatcher. Took 30 min to
diagnose; would have been 0 min with a full sync.

The lib rsync also has to be the full lib dir (not a subset), because the
RPATH on these binaries is `$ORIGIN/../lib` and they expect a complete VW
+ ASP lib tree at runtime.

**How to apply:** Whenever a code change goes from Mac through L1 build to
pfx for any reason (not just the binary you changed), rsync the FULL
install/bin and FULL install/lib trees. Even a small "I just changed
cam_test" warrants the full bin sync, because (a) the lib those binaries
share may also have changed, and (b) other workers in the pipeline that
share that lib will break the moment they run with mismatched shared
objects.

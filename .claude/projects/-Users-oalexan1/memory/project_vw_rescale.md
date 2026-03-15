---
name: VW default_rescale migration
description: VW DiskImageResource::default_rescale=true silently rescales integer pixels to [0,1] when read as float, causing bugs. Report and migration plan in ~/projects/vw_rescale_default.sh.
type: project
---

VW's `DiskImageResource::default_rescale = true` silently rescales integer pixel
types (uint8, int16, uint16) to [0,1] or [-1,1] when read via
`DiskImageView<float>`. This caused colormap to produce constant output on .cub
(Int16) files when user supplies --min/--max in original pixel units.

**Why:** The rescaling is invisible and every tool must independently discover
and work around it with `set_rescale(false)`. Already 5 call sites had to add
this workaround.

**How to apply:** Full analysis and migration plan in
`~/projects/vw_rescale_default.sh`. The proposed fix is to flip the default to
`false`. Impact analysis shows low risk - most reads are float32 (no-op), and
code that reads integer images either normalizes explicitly or already had the
workaround. The one edge case is standalone `ipfind` on raw integer images
without `--normalize`.

**Status as of 2026-03-14:** Report written, colormap bug fixed with per-tool
`set_rescale(false)`. Global default flip deferred to next session.

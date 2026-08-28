---
name: feedback_fixed_workdir_no_nested_cd
description: One fixed work dir per project; cd into it ONCE at the top, keep every path relative to it (or absolute); never a second cd into a subdir mid-script.
metadata:
  type: feedback
---

Each project has ONE fixed work dir. A script cds into it ONCE at the top
(`cd "$W"`) and thereafter uses paths RELATIVE to that work dir (inputs like
`data/<sid>/img.tif`, outputs like `v5/out.tif`) or absolute paths. NEVER do a
second `cd` into a subdir partway through - it silently re-bases every relative
path established earlier and breaks the inputs.

**Why:** the `cd`-into-subdir gimmick has bitten us repeatedly. Burned 2026-08-28:
a `cd v5` mid-script turned the earlier relative `data/...` input paths into
`v5/data/...`, so every input went missing and bundle_adjust died - the whole qsub
job wasted. (A [[feedback_dry_test_qsub]] would also have caught it.)

**How to apply:** to write products into a subdir, reference the subdir by relative
path (`v5/gL.tif`, `mkdir -p v5`) - do NOT `cd v5` first. Keep exactly one `cd`
(into the work dir) per script. Inputs and outputs both hang off the fixed work dir.

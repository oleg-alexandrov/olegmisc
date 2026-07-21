---
name: feedback_dont_cover_bugs
description: "Don't mask a bug with a workaround that mutes the symptom; find and fix the actual defect."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: b6fbfdd1-830e-4282-940e-159f0309d81f
---

The recurring failure mode: when something errors, reach for a change that makes
the error message disappear (a symlink, a fallback path, a copied file, a
fixture, a try/except, a special-cased env) instead of first asking WHY it is
broken and fixing the real cause. That is cheating - the symptom is muted, the
bug lives on somewhere quieter, and a false "it works" is presented.

Canonical example (2026-07): a stripped asp_deps threw "Cannot find Qt plugins in
.../plugins". The workaround was `ln -s lib/qt6/plugins plugins`, which silenced
the error but CLOBBERED `plugins/stereo`, so parallel_stereo silently lost every
external stereo algorithm (mgm, mgm_multi, msmw, msmw2, libelas). The real bug
was one line: ASP's EnvUtils.cc set QT_PLUGIN_PATH to the Qt5 path
`$PREFIX/plugins` instead of the Qt6 path `$PREFIX/lib/qt6/plugins`. The honest
fix was that one line; the symlink just hid it for months.

**Why:** Oleg repeatedly has to catch me muting symptoms. A muted symptom is
worse than a loud one - it reads as fixed while the defect persists and spreads.

**How to apply:** When an error appears, NAME the actual defect before touching
anything. The tell that I am about to cheat: I am reaching for something that
makes the error go away (symlink, fallback, copy, fixture, broadened catch,
special case) without having stated the root cause. Stop, find the cause, fix
THAT. A temporary workaround is legitimate ONLY if I (a) say so explicitly and
(b) ensure the real problem gets fixed eventually - fix the root cause in scope
if I can, else REPORT it to the user so it is not lost (especially when we are
not busy). MUST report problems to the user always, even mid long-running /
nightly / autonomous runs - never silently work around and move on. Owning known
breakage is the job: e.g. preparing a release when the release bumped Qt5->Qt6,
handling that break IS release maintenance, not something to paper over. See also
[[feedback_test_on_real_data.md]] and the "Report Shortcuts and Temp Fixes"
section of CLAUDE.md.

---
name: remote-ci
description: Diagnose a remote/cloud/nightly ASP build or CI failure via gh. Load the MOMENT any of these come up - "nightly build failed", "cloud build", "remote build", "the ARM build broke", "check my email about the build", a GitHub Actions failure email, "CI failed", "the run failed", or ANY use of `gh` at all. Carries the gh conda-env path (gh is NOT on PATH - never run bare `gh`), the email -> run-id -> `--log-failed` diagnosis recipe, and how to read past the cascade to the ONE root failure. Complements git-repos (repo slugs, REST/GraphQL recipes, github_notes.sh) and asp-regressions (judging acceptable-vs-real, regold, re-run).
---

# Diagnosing a remote / cloud / nightly CI failure

Load this whenever a build/CI failure is reported OR `gh` is needed. The whole
point is to skip the confusion of hunting for `gh` and go straight to the log.

## gh is NOT on PATH - use the conda-env path (never run bare `gh`)

`gh` lives in a dedicated `gh` conda env on every box. Running bare `gh` gives
`command not found` and wastes a call. Go straight to:

```bash
gh=$(ls -d $HOME/*conda3/envs/gh/bin/gh | head -1)   # portable
```
Concrete paths:
- Mac mini:  `/Users/oalexan1/anaconda3/envs/gh/bin/gh`
- lunokhod1: `/home/oalexan1/miniconda3/envs/gh/bin/gh`

Authed as `oleg-alexandrov` (keyring). Full REST/GraphQL recipes and the
`gh pr/issue view` GraphQL breakage: `~/projects/github_notes.sh` (and the
`git-repos` skill). Repo slug for ASP nightlies:
`NeoGeographyToolkit/StereoPipeline`.

## The nightly-build-failure email -> log path

Oleg gets a nightly status email "ASP build YYYY-MM-DD status is Fail" (from his
own address, bcc) listing the four machines: localLinux, cloudMacX64,
cloudMacArm64, cloudLinuxArm64. GitHub also sends a per-workflow
"Run failed: build_test_* - master (SHA)" notification from notifications@github.com
carrying the Actions **run URL** (…/actions/runs/<RUN_ID>).

1. Find the emails (Gmail): `newer_than:3d (build OR nightly OR failed OR Actions)`.
   The status email names WHICH machine failed; the GitHub email gives the RUN_ID.
2. Pull the failing log - do NOT stop at the email's "3 annotations":
   ```bash
   gh=$(ls -d $HOME/*conda3/envs/gh/bin/gh | head -1)
   "$gh" run list -R NeoGeographyToolkit/StereoPipeline --limit 8   # if no RUN_ID
   "$gh" run view <RUN_ID> --log-failed -R NeoGeographyToolkit/StereoPipeline
   ```

## Read PAST the cascade to the ONE root failure

Cloud logs bury the real defect under a cascade. A packaging/env failure early
on leaves every ASP tool uninstalled, so the tail is dozens of
`<tool>: command not found` and many `Test ... returned 1` - all NOISE. Do not
report those as the problem. Grep the WHOLE log for the first real error:

```bash
log=$("$gh" run view <RUN_ID> --log-failed -R NeoGeographyToolkit/StereoPipeline 2>&1)
echo "$log" | grep -iE "Error:|Exception|Traceback|make-dist|Cannot find|make.*Error|error:" | head
echo "$log" | grep -oE "[a-z0-9_]+: command not found" | sort -u   # confirms cascade, not cause
```
Then classify the root cause:
- **Compile break** (`error:` from g++, `make ... Error`) -> real source regression.
- **Packaging break** (`make-dist.py failed`, `Cannot find python at .../envs/<name>`)
  -> BinaryBuilder / cloud-deps ENV mismatch, usually one-arch-only. E.g.
  2026-08-26 ARM64: `Cannot find python at .../envs/python_isis10/bin/python` -
  the arm asp_deps env-setup lacked `python_isis10` (an isis bump not carried to
  the arm deps tarball). Compile was fine; the 15 test failures were downstream.
- **Test drift** (tool ran, output differs from gold) -> hand to `asp-regressions`
  to judge acceptable-vs-real and regold.

A break on ONE arch while the other three pass is almost always an
env/deps/packaging issue for that arch, not a source regression.

## Then

- Report to Oleg: which machine, the SHA, the ONE root cause, and whether it is a
  compile / packaging-env / test-drift class. Offer the fix; do NOT push, regold,
  or re-run without an explicit instruction.
- Triage judgement, regold, and relaunching the nightly: `asp-regressions` skill.
- Repo table, remotes, `gh api` REST recipes: `git-repos` skill.

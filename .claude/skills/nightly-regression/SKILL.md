---
name: nightly-regression
description: How the ASP nightly build/regression works end-to-end and how to drive it - the l1 launch_master.sh orchestrator, its four "children" (localLinux built on l1 + the three cloud GitHub-Actions builds cloudMacX64 / cloudMacArm64 / cloudLinuxArm64), where deps come from, the status-file + email + daily-build-release flow, and the exact commands to RETRIGGER (full nightly or one platform), MONITOR, and diagnose a failure. Load whenever the ASP nightly is mentioned - "retrigger the nightly", "relaunch launch_master", "wake up the cloud children", "the nightly failed", "daily build", "regold and re-run" - or when driving the cloud CI. Complements remote-ci (gh --log-failed diagnosis), asp-regressions (judge test drift / regold), build-env, git-repos.
---

# ASP nightly build + regression

The nightly builds and tests ASP on four targets, uploads a daily release, and
emails Oleg a status line. Full detail: `~/projects/nightly_regression.sh`.

## Architecture: one orchestrator, four children

**l1 cron** (`05 23 * * *` UTC) runs the orchestrator:
`~/projects/BinaryBuilder/auto_build/launch_master.sh`. It builds/tests all four
platforms, polls their status files, then emails and (on success) uploads a
GitHub release. The four children (the "machines"):

| Platform | Where it builds | Deps source |
|---|---|---|
| **localLinux** | on l1 itself (build.sh `build_local_linux`) | l1's LOCAL `asp_deps` conda env (NOT a tarball) |
| **cloudMacX64** | GitHub Actions, `build_test_mac_x64.yml` -> `build_test.sh` (Intel branch) | BinaryBuilder release `asp_deps_mac_x64_v4` |
| **cloudMacArm64** | GitHub Actions, `build_test_mac_arm64.yml` -> `build_test.sh` (Arm branch) | BinaryBuilder release `asp_deps_mac_arm64_v4` |
| **cloudLinuxArm64** | GitHub Actions, `build_test_linux_arm.yml` -> `build_test_linux_arm.sh` | BinaryBuilder release `asp_deps_linux_arm_v1` |

Key facts that trip people up:
- **The cloud Actions run on the GOD repo** (NeoGeographyToolkit/StereoPipeline)
  and build from **god/master**. A CI-script fix must be pushed to **god** before
  a retrigger picks it up (origin/fork alone does nothing for the nightly).
- `build_test.sh` is **Mac-only** (both Mac arches, via `uname -m`). Linux-arm is
  a separate script. localLinux uses neither - it builds against l1's local env,
  which is why localLinux can pass while all three cloud builds fail (and vice
  versa).
- Cloud deps are conda-pack tarballs (`asp_deps_p1.tar.gz` [+ `p2` if split] +
  `python_isis10.tar.gz`) fetched from the BinaryBuilder release by the workflow
  `.sh`. make-dist.py needs the `python_isis10` env; an empty/partial deps
  download there = a "Cannot find python" make-dist failure (see remote-ci).

## Key files on l1 (all under `~/projects/BinaryBuilder/`)

- `auto_build/launch_master.sh` - orchestrator (launch, poll, email, release).
- `auto_build/build.sh` - per-platform build (`build_local_linux`,
  `build_cloud_macos` -> `gh workflow run` + poll + download artifacts).
- `auto_build/run_tests.sh` - unpack tarball, run StereoPipelineTest, status file.
- `auto_build/utils.sh` - helpers; sets isisEnv / pythonEnv.
- `output_master.txt` - live log of the latest run. `status_master.txt` +
  `status_<platform>.txt` - progress/result. `asp_tarballs/` - recent tarballs.

Status file protocol: `NoTarballYet now_building` -> `<tar> build_done Success`
(or `Fail build_failed`) -> `<tar> now_testing` -> `<tar> test_done Success|Fail`.

## Retrigger

Full nightly (all four children), from l1 - matches the cron:
```bash
ssh l1 'cd ~/projects/BinaryBuilder && nohup ./auto_build/launch_master.sh \
   > output_master.txt 2>&1 < /dev/null & echo PID $!'
```
`launch_master.sh resume` skips platforms already at `test_done Success` (use to
re-run only the failed ones / force-publish). Before launching, confirm none is
already running: `ssh l1 'pgrep -fa launch_master.sh'`; and check the 23:05 UTC
cron won't collide.

One cloud platform only (e.g. after a deps re-spin) - fires the Action directly:
```bash
gh=$(ls -d $HOME/*conda3/envs/gh/bin/gh)
$gh workflow run build_test_linux_arm.yml -R NeoGeographyToolkit/StereoPipeline --ref master
$gh workflow run build_test_mac_arm64.yml -R NeoGeographyToolkit/StereoPipeline --ref master
$gh workflow run build_test_mac_x64.yml   -R NeoGeographyToolkit/StereoPipeline --ref master
```

## Monitor

```bash
ssh l1 'pgrep -fa launch_master.sh; cat ~/projects/BinaryBuilder/status_master.txt; \
        tail -20 ~/projects/BinaryBuilder/output_master.txt'
gh=$(ls -d $HOME/*conda3/envs/gh/bin/gh)
$gh run list -R NeoGeographyToolkit/StereoPipeline --limit 6
```
launch_master no longer in `pgrep` = finished (and it has emailed). Wall time is
roughly mac_x64 ~1h20m, mac_arm64 ~40m, linux_arm ~25m; a full run ~1.5-2h.
When arming an in-session CronCreate heartbeat to watch it, see autonomous-ops.

**The done-signal is the PROCESS, not the status file.** `status_master.txt`
persists between runs and shows the PREVIOUS run's per-platform Success/Fail
until the current run overwrites each line as its result lands. Early in a run it
still holds yesterday's numbers, so reading it mid-run will make you falsely
conclude "done, cloudLinuxArm64 Fail" when the run is only minutes in and still
building. ALWAYS gate "done" on `pgrep -fa launch_master.sh` returning nothing;
only then read status_master.txt for the real result. (Burned 2026-08-26.)

**zsh gotchas in the l1 recon ssh** (l1's login shell is zsh): a `?` in an echo
(`echo RUNNING?`) triggers a no-match glob error that ABORTS the whole remote
line, and a leading `=` (`echo ===STATUS===`) triggers `=cmd` filename expansion
and errors. Use plain separators with no `?` and no leading `=` (e.g.
`echo ----status----`).

## Email + release (done by launch_master)

- Email via msmtp: `Subject: ASP build <date> status is <Success|Fail>`, body is
  `status_master.txt`. Sending email yourself: see machines-tools / send_email_notes.
- On overall success: uploads a `<date>-daily-build` GitHub release (keeps last 2).

## Diagnose a failure

- Fetch the real error and read PAST the cascade -> **remote-ci** skill
  (`gh run view <id> --log-failed`, then grep for the first `Error:/Exception/
  make-dist`; ignore the downstream "command not found" / failed-test noise).
- Distinguish a BUILD/packaging break (compile error, make-dist env mismatch)
  from TEST DRIFT (tool ran, output differs from gold). Test-drift judgement,
  regold, and relaunch: **asp-regressions** skill. Build mechanics: **build-env**.
- A break on ONE arch while the others pass is almost always a per-arch
  deps/packaging issue, not a source regression.

---
name: nightly-regression
description: >-
  How the ASP nightly build/regression works and how to drive it: the l1 launch_master
  orchestrator, its localLinux plus three cloud children, status files, and the
  retrigger/monitor/republish commands. Load whenever the ASP nightly is mentioned or you
  drive the cloud CI.
---

# ASP nightly build + regression

The nightly builds and tests ASP on four targets, uploads a daily release, and
emails Oleg a status line. Full detail: `~/projects/nightly_regression.sh`.

## Architecture: one orchestrator, four children

**l1 cron** (`05 23 * * *` in l1's LOCAL time, which is **America/Los_Angeles** -
so it fires **23:05 Pacific** = 06:05 UTC in PDT / 07:05 in PST, NOT 23:05 UTC;
`crontab -l` has no `TZ=` override, so cron uses the system zone) runs the
orchestrator: `~/projects/BinaryBuilder/auto_build/launch_master.sh`. It
builds/tests all four platforms, polls their status files, then emails and (on
success) uploads a GitHub release. The four children (the "machines"):

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

## localLinux build cache (done.txt) - why a rebuild can look "suspiciously fast"

localLinux builds into a PERSISTENT tree `build_asp/` (reused across runs), NOT a
clean checkout. `build.py` keeps `build_asp/done.txt` listing `<pkg> <git-hash>`
for `visionworkbench` and `stereopipeline`; on each run it SKIPS any package whose
current god/master hash matches done.txt ("Package X was already built, skipping").
So right after a good build (done.txt holds the current hashes), a relaunch does
NOT recompile - it goes straight to make-dist and repackages in minutes. And even a
hash MISMATCH triggers an INCREMENTAL cmake build (only changed files), so from-
scratch is rare: a normal VW+ASP build takes ~an hour, but an incremental one is
minutes. Do not mistake a fast build for "it didn't build" (or vice versa) - VERIFY
by reading `output_localLinux.txt` (it prints each `Building CXX object`; a clone
line `Cloning into '.../visionworkbench-git'` means a true from-scratch).

**Skip nights ship a STALE-dated tarball (harmless, but know it).** On a SKIP no
compile and no cmake CONFIGURE runs, so make-dist re-packages the UNCHANGED binaries:
the tarball FILENAME date advances nightly but the binaries' embedded `--version`
Build date + Build ID stay FROZEN at the last real build. So on a quiet stretch the
daily tarballs are byte-identical, just re-dated. This is harmless - the code IS the
latest god/master and `--version` honestly reports the true (older) build date/commit;
only the filename is optimistic. (Seen 2026-09-19: the 09-13..09-17 daily tarballs all
reported `Build date 2026-09-12 / ca0d06d6e` because god/master had no new commits in
that window.) The embedded date is fresh ONLY when a build actually RUNS - because
`string(TIMESTAMP ASP_BUILD_DATE "%Y-%m-%d" UTC)` at ASP `src/CMakeLists.txt` is
evaluated at cmake CONFIGURE, and configure runs only when the package is not skipped.
done.txt is also BLIND to conda-deps (asp_deps) changes - see
`~/projects/nightly_regression.sh` "done.txt skip cache" for that (real) gotcha: a
usgscsm/ale respin with unchanged VW/ASP hashes ships binaries linked against OLD deps.

**Force a genuine from-scratch build** (Oleg sometimes insists, to be 100% sure the
new code is really in the tarball): delete the cache AND the build+install trees,
then relaunch. These are 3 literal absolute paths (delegate the wipe to a subagent
per the standing bulk-wipe rule):
```
rm -f  ~/projects/BinaryBuilder/build_asp/done.txt
rm -rf ~/projects/BinaryBuilder/build_asp/build
rm -rf ~/projects/BinaryBuilder/build_asp/install
```
(`build_asp/misc` and the top-level `last-completed-run` symlink -> `build_asp` stay;
make-dist reads `last-completed-run/install`.) Then a normal `launch_master.sh` re-
clones and compiles from zero. To confirm the tarball really has your change, check
the fresh build clone HEAD (`git -C build_asp/build/stereopipeline/stereopipeline-git
log --oneline -1`) and `strings <tarball>/libexec/<tool>` for a new symbol - NOT
`bin/<tool>` (that is a tiny wrapper script; the real ELF is in `libexec/`).

**Killing a running nightly:** launch_master's children run via `ssh lunokhod1
bash build.sh`, so they are NOT in launch_master's process group and a kill of the
top PID leaves build.py/make/cc1plus AND the test tools (parallel_stereo,
bundle_adjust, GNU parallel) running - they reparent to init. Walk descendants and
kill BY PID (`pgrep -P` recursively from each root), never `pkill -f` a pattern that
also matches your own kill command. Re-scan for `run_tests.sh` and stereo/BA/parallel
survivors after the first pass.

## Retrigger

Full nightly (all four children), from l1 - matches the cron:
```bash
ssh l1 'cd ~/projects/BinaryBuilder && nohup ./auto_build/launch_master.sh \
   > output_master.txt 2>&1 < /dev/null & echo PID $!'
```
`launch_master.sh resume` re-runs each platform whose `status_<platform>.txt` is
NOT `test_done Success` (i.e. `build_failed`, empty, or `test_done Fail`), and
skips those already at `test_done Success`. Before launching, confirm none is
already running: `pgrep -fa launch_master.sh`; and check the 23:05 Pacific-time
cron won't collide.

**FAKE SUCCESS FIRST, then resume - never a bare `resume` to republish.** A bare
`resume` on a `test_done Fail` platform immediately writes `NoTarballYet
now_building` and launches `build.sh` - it does NOT just re-run the failed test. Due
to the done.txt cache (above), that build.sh usually SKIPS recompiling (hashes match)
and only re-runs `make-dist.py` (minutes), but it STILL OVERWRITES the good tarball
(and if done.txt is stale/absent it does a full ~1.5-2h recompile). Either way you
do not want it: flip the status to Success first so resume launches no build.sh at
all.
Recovery if you slip and it starts building: kill the tree (`build.sh`, its `ssh`,
and the `make-dist.py` PID), confirm the existing tarball's date/size are unchanged
(make-dist overwrites only at the very end), then fake success and resume as below.
(Burned 2026-09-12: a bare `resume` on a localLinux test-drift Fail was mid-
`make-dist.py` before I caught it.)

**Publish an already-built nightly when the only failure is minor (recurring ask).**
Oleg will often say "publish today's build in resume mode, the failing test is
minor" - he wants the existing on-disk tarballs shipped and the usual Success email,
NO rebuild. Mechanism: `launch_master.sh resume` re-BUILDS any platform whose status
is `Fail` (its skip condition is `progress != build_failed && progress != "" &&
status != Fail`), so a bare resume would rebuild from scratch. To publish as-is,
first flip the failing platform's status file from `test_done Fail` to `test_done
Success` (KEEP the same tarball name on the line), then resume - now all four are
`test_done Success`, resume launches no builds, the poll loop breaks at once, and it
goes straight to upload-to-GitHub-release + Success email. Recipe:
```bash
ssh l1 'cd ~/projects/BinaryBuilder && \
  echo "asp_tarballs/StereoPipeline-<ver>-<date>-x86_64-Linux.tar.bz2 test_done Success" \
    > status_localLinux.txt'
ssh l1 'cd ~/projects/BinaryBuilder && nohup ./auto_build/launch_master.sh resume \
  > output_master.txt 2>&1 < /dev/null & echo PID $!'
```
Use the EXACT tarball basename already in that status file (read it first). This is
honest here only because the failure is known-minor and the shipped tarball is
otherwise good - say so; do not silently mask a real breakage. Confirm none running
and the 23:05 Pacific cron won't collide first. (localLinux is the usual culprit since
it tests on l1; a cloud platform's status file is `status_cloud<Plat>.txt`.)

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

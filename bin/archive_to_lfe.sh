#!/bin/bash
# archive_to_lfe.sh
# Archive a project tree (or a named part of it) from NAS /nobackup to lfe (Lou)
# tape as a single plain tar. Reusable across projects. Companion to the notes in
# ~/projects/lfe_archive.sh (read that for the full lfe policy: log every archive
# in the project notes, plain tar not gzip, restore via dmget/shiftc, lfe
# front-end flakiness).
#
# PARENT-DIR-PREFIX PACKING POLICY (important, the whole point):
#   The tar is ALWAYS created from the projects root with the PROJECT DIR as the
#   leading path component, so it extracts into a dir named after the project.
#   For project <proj>, the tar contains <proj>/... and `tar xf` recreates
#   <proj>/... . This means you can archive DIFFERENT parts of one project as
#   SEPARATE tarballs (inputs, cameras, results, ...) and they ALL unpack into the
#   SAME local <proj>/ dir, each part landing at its correct relative place. Name
#   the part in the tarball basename, e.g. <proj>_cubs.tar extracting <proj>/lronac_all,
#   <proj>_results.tar extracting <proj>/<results dir>. Never tar from inside the
#   subdir (that would lose the <proj>/ prefix and parts would not reassemble).
#
# Runs ON lfe, where /u (tape) is local and /nobackupp19 is mounted. Invoke it
# detached so the tar survives ssh exit AND the lfe load balancer (learned the
# hard way: a plain `nohup ... &` over ssh gets killed at session exit, and
# `ssh lfe` round-robins across front-ends so cross-node pid checks are bogus).
#
#   ssh lfe 'setsid bash /nobackupp19/oalexan1/projects/archive_to_lfe.sh \
#     <project> [<tarball_basename>] [--subdir <relpath>] </dev/null >/dev/null 2>&1 &'
#
# Then poll the log (on shared nobackup, visible from any lfe/pfe node):
#   ssh pfe21 'cat /nobackupp19/oalexan1/projects/<tarball_basename>_archive.log'
#   # done when the last line is:  END <date> rc=0
#
# Args:
#   <project>           dir under /nobackupp19/oalexan1/projects to archive.
#   <tarball_basename>  optional tar name without .tar; default = <project>.
#                       Use it to name a part or a snapshot, e.g. <proj>_cubs,
#                       <proj>_results, lunamaps_v2_20260618.
#   --subdir <relpath>  archive only <project>/<relpath> (still with the <project>/
#                       prefix). Omit to archive the whole project dir.
#   --force             allow overwriting an existing tarball on tape.
#
# Canonical copy: ~/bin/archive_to_lfe.sh (tracked in the home repo, on PATH).
# Deploy to the lfe-visible nobackup before running:
#   rsync -av ~/bin/archive_to_lfe.sh pfe21:/nobackupp19/oalexan1/projects/
#
# SAFETY: never deletes anything; refuses to clobber an existing tape tarball
# unless --force; always plain `tar cf` (not gzip, per lfe_archive.sh).
set -u
umask 022

force=0
subdir=""
positional=()
while [ "$#" -gt 0 ]; do
  case "$1" in
    --force)  force=1; shift ;;
    --subdir) subdir=${2:-}; shift 2 ;;
    *)        positional+=("$1"); shift ;;
  esac
done
proj=${positional[0]:-}
tarbase=${positional[1]:-$proj}

if [ -z "$proj" ]; then
  echo "Usage: archive_to_lfe.sh <project> [<tarball_basename>] [--subdir <relpath>] [--force]"
  exit 1
fi

root=/nobackupp19/oalexan1/projects
dst_dir=/u/oalexan1/projects
tarball=$dst_dir/$tarbase.tar
log=$root/${tarbase}_archive.log

# What to tar, always with the <proj>/ prefix (parent-dir-prefix policy).
if [ -n "$subdir" ]; then
  target="$proj/$subdir"
else
  target="$proj"
fi
src="$root/$target"

if [ ! -e "$src" ]; then
  echo "ERROR: source does not exist: $src"
  exit 1
fi
if [ -e "$tarball" ] && [ "$force" != 1 ]; then
  echo "ERROR: tarball already exists, refusing to clobber tape: $tarball"
  echo "       pass --force to overwrite."
  exit 1
fi

cd "$root" || { echo "ERROR: cannot cd to $root"; exit 1; }
echo "START $(date) -> $tarball  (target: $target)" > "$log"
du -sh "$target" >> "$log" 2>&1
tar cf "$tarball" "$target" >> "$log" 2>&1
rc=$?
ls -l "$tarball" >> "$log" 2>&1
echo "END $(date) rc=$rc" >> "$log"
exit $rc

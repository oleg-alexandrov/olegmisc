#!/bin/bash
# archive_to_lfe.sh
# Archive a project tree from NAS /nobackup to lfe (Lou) tape as a single plain
# tar. Reusable across projects. Companion to the notes in ~/projects/lfe_archive.sh
# (read that for the full lfe policy: log every archive in the project notes,
# plain tar not gzip, restore via dmget/shiftc, lfe front-end flakiness).
#
# Runs ON lfe, where /u (tape) is local and /nobackupp19 is mounted. Invoke it
# detached so the tar survives ssh exit AND the lfe load balancer (learned the
# hard way: a plain `nohup ... &` over ssh gets killed at session exit, and
# `ssh lfe` round-robins across front-ends so cross-node pid checks are bogus).
#
#   ssh lfe 'setsid bash /nobackupp19/oalexan1/projects/archive_to_lfe.sh \
#     <project> [<tarball_basename>] </dev/null >/dev/null 2>&1 &'
#
# Then poll the log (on shared nobackup, visible from any lfe/pfe node):
#   ssh pfe21 'cat /nobackupp19/oalexan1/projects/<tarball_basename>_archive.log'
#   # done when the last line is:  END <date> rc=0
#
# Args:
#   <project>           dir under /nobackupp19/oalexan1/projects to archive.
#   <tarball_basename>  optional tar name without .tar; default = <project>.
#                       Use it for versioned snapshots, e.g. lunamaps_v2_20260618.
#   --force             allow overwriting an existing tarball on tape.
#
# Canonical copy: ~/projects/archive_to_lfe.sh (tracked; symlinked ~/bin/).
# Deploy to the lfe-visible nobackup before running:
#   rsync -av ~/projects/archive_to_lfe.sh pfe21:/nobackupp19/oalexan1/projects/
#
# SAFETY: never deletes anything; refuses to clobber an existing tape tarball
# unless --force; always plain `tar cf` (not gzip, per lfe_archive.sh).
set -u
umask 022

force=0
positional=()
for a in "$@"; do
  if [ "$a" = "--force" ]; then force=1; else positional+=("$a"); fi
done
proj=${positional[0]:-}
tarbase=${positional[1]:-$proj}

if [ -z "$proj" ]; then
  echo "Usage: archive_to_lfe.sh <project> [<tarball_basename>] [--force]"
  exit 1
fi

root=/nobackupp19/oalexan1/projects
dst_dir=/u/oalexan1/projects
src=$root/$proj
tarball=$dst_dir/$tarbase.tar
log=$root/${tarbase}_archive.log

if [ ! -d "$src" ]; then
  echo "ERROR: source dir does not exist: $src"
  exit 1
fi
if [ -e "$tarball" ] && [ "$force" != 1 ]; then
  echo "ERROR: tarball already exists, refusing to clobber tape: $tarball"
  echo "       pass --force to overwrite."
  exit 1
fi

cd "$root" || { echo "ERROR: cannot cd to $root"; exit 1; }
echo "START $(date) -> $tarball" > "$log"
du -sh "$proj" >> "$log" 2>&1
tar cf "$tarball" "$proj/" >> "$log" 2>&1
rc=$?
ls -l "$tarball" >> "$log" 2>&1
echo "END $(date) rc=$rc" >> "$log"
exit $rc

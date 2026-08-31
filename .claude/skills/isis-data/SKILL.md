---
name: isis-data
description: ISIS mission data and SPICE kernels - the LRO NAC end-to-end ingest pipeline, CSM JSON via isd_generate, ODE search, illumination analysis, and fetching kernels with downloadIsisData or targeted rclone. Load only when working with ISIS mission cubes, spiceinit, or kernel downloads.
---

## Never run an ISIS app with no arguments (it pops a GUI)

Running any ISIS app bare (e.g. `spiceinit`, `campt`) with no arguments launches
its Qt GUI and hangs the session. To see parameters, use `<app> -h`, or read the
parameter XML at `$ISISROOT/bin/xml/<app>.xml`. Always pass real arguments (or
`-h`) to an ISIS app.

## ISIS Mission Data and Kernels

**LRO NAC end-to-end + generic ISIS kernel fetch: `~/projects/lronac_processing.sh`.**
Full ingest pipeline (lronac2isis → spiceinit → lronaccal → lronacecho), CSM JSON
via isd_generate, ODE search, illumination/azimuth analysis, and failure modes
(missing CK, ALE driver crash, sub-solar lon vs ground azimuth). Kernel fetch
(section 5): `downloadIsisData <mission> $ISISDATA` for a full sync, or targeted
`rclone --config $ISISROOT/etc/isis/rclone.conf copy <mission>:kernels/ck/ ...
--include="<file>" --no-traverse -P` for a single missing CK. Update on any new
gotcha.

## NEVER hand-build a local metakernel to make isd_generate / ALE run (CRITICAL)

Reaching to write your own local metakernel (`.tm`) so that `isd_generate` or
`ale.loads` will produce an ISD is almost always CHEATING. It is the SPICE analog
of the symlink hack. It papers over a real defect and makes a broken path look like
it works. The Cassini cheat was exactly this: a hand-authored `.tm` masked a
NaifSpice driver that had never run end to end (2026-08-30 notes). When you catch
yourself about to assemble a kernel list into a `.tm`, STOP and name the real reason
`isd_generate` cannot find kernels, then fix THAT.

The honest ways to furnish kernels, in order of preference:
1. Web SpiceQL. If the mission has a non-empty `spiceql_mission` in
   `ale/base/__init__.py` `spiceql_mission_map`, use `spiceinit ... web=true` or
   `isd_generate -w`. No local kernels, no metakernel.
2. Local, from the cube's own spiceinit. Download the mission kernels into
   `$ISISDATA` (`downloadIsisData` or targeted rclone), run `spiceinit` normally so
   ISIS resolves the exact kernels from the kernel `.db` files, then let ALE read the
   cube's own resolved Kernels group with `ale.kernel_access.generate_kernels_from_cube(cube, expand=True)`
   and pass that via `props={"kernels": ...}`. This is reading spiceinit's honest
   resolution, NOT hand-picking, so it is fine.
3. An official mission metakernel that ships in the data area is fine to use. You did
   not author it.

Common real root causes when `isd_generate` fails with "No viable Driver" or cannot
find kernels, each with the honest fix, not a metakernel:
- The driver has an EMPTY `spiceql_mission` (seen for NEAR MSI, and the Voyager and
  OSIRIS-REx OCAMS drivers). SpiceQL web and local search cannot load it. Fix: add
  the `spiceql_mission`, or furnish via option 2 above.
- The mission CK/SPK/IK are simply not in `$ISISDATA` (seen for JUNO: only iak and
  tspk are on the isisdata rclone mirror; the rest is at NAIF). Fix: download them.
- The driver itself is broken. Fix the driver.

If a driver genuinely needs to stop depending on a hand-fed kernel list, the fix is
in the DRIVER (give it a `spiceql_mission`, or make it read the cube's attached
tables), never a `.tm` sitting in the work dir. Report the root cause to Oleg.

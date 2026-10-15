---
name: feedback-load-product-skill-before-shipping
description: "Writing into a delivery/results dir means producing a shipped product - load the delivery/product skill BEFORE building it, even when the task started narrow."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 4d987659-6598-43e3-ab3e-c24ff4ea3bf3
---

When a task crosses from scratch/inspection into producing a SHIPPED artifact,
load the relevant product skill (for SfS: [[sfs-delivery]]) BEFORE building the
artifact, not after. The tell is concrete: you are writing into, or rsyncing to,
a delivery/results dir, or calling something "the delivery X".

**Why:** entering via a narrow topic (e.g. "fix sfs_blend", "make a hillshade to
view") and then being nudged to a bigger topic does not auto-load the product
skill, so a scratch convention gets shipped. This happened 2026-10-14: an
inspection hillshade (default altitude) was rsynced as the SfS delivery
hillshade, which uses `-alt 10`. It had to be redone. The miss was not loading
sfs-delivery when the hillshade became the delivery hillshade.

**How to apply:** before producing any file that lands in a handoff/results dir,
pause and load the product skill for its conventions (altitude, nodata, naming,
single-thread-on-head-node regen). Cross-links added in [[sfs-blend]] and
visual-inspection point at sfs-delivery for exactly this.

# David's notes
# -Just got email from Teledyne on their latest TDI linescan detector offerings.
# - But if the "cross-talk" width near the outer edges of the input images with different scan azimuths is 2x the jitter wavelength, would that be enough? 
# - Also wondering about any value added by having two linescan detectors in the focal plane offset in the along-track direction (like WV PAN and MS sensors, or HiRISE solution)
# Shashank wants --reference-terrain work with jitter solver.

# STV work: August 2024

Performed a study of a rig consisting of a linescan camera looking nadir and a
linescan camera looking backward. It was shown that if one of the cameras is
rotated around its view axes so that the scanned image lines meet at an angle
with the other camera, that results in improved solving for jitter. An
empirical relationship between rotation angle and jitter frquency to produce
best results was determined and validated.

Added the ability to simulate a blur in images created with sat_sim. 

Made the jitter solver do two passes of camera refinement. This was shown 
to remove residual jitter in the cameras.

Added to the jitter solver the ability to respect user-specified horizintal
and vertical camera position uncertainties. 

Modeled a rig having two linescan sensors at an angle to each other. This was
shown to improve jitter solving, as each sensor can constrain many scan lines
in the other sensor. 

Validated that given a linescan camera rig with parallel scan lines, the rig constraint
greatly helps solve correcting the jitter, as opposed to not using it, but not all
jitter gets eliminated, but only for some frequences depending on the spacing between
the rig sensor footprints on the ground.

Added the ability for the jiter solver to use a sparse set of lidar measurements
as a constraint.

# ASP maintanence work: August 2024

Added options to enable atmospheric refraction correction and velocity
aberration correction for Pleiades linescan cameras.

Made stereo processing skip tiles that have no data
 
Enforce that input mapprojected images have same resolution. 
A mistake here can result in poor results.

Evaluated ASP on a difficult stereo dataset having vegetation and a large
amount of clouds.

Added a discussion of various ways ASP can make use of existing terrain data.
https://stereopipeline.readthedocs.io/en/latest/next_steps.html#existing-terrain

Implemented creating an initial stereo disparty for mapprojected images
that takes into account a prior DEM. This is very helpful when the terrain
has a lot of clouds.  https://stereopipeline.readthedocs.io/en/latest/correlation.html#d-sub-dem

Made 3D CRS fully supported for all ASP options that use a projection string.

Fixed a bug in the mapprojection program when the current directory has spaces
in it.

# Old notes:

# STV: 

# Performed a large-scale experiment with 1000 SkySat images (3 rigs with 3 sensors each). Validated that accpetable results were produced. Found out that merely adding more datasets does not improve the absolute geolocation accuracy, as each of these datasets has its own systematic bias, which does not get averaged away. Additional alignment to the ground is still necessary after a self-consistent solution is produced.

# Made bundle_adjust export CSM Frame camera information to the NVM format used by rig_calibrator. This allows modeling the rig constraint for SkySat images.

# Ensured that the rig_calibrator, ASP, and CSM are in full agreement when it comes to the radial-tangential lens distortion model and that conversions are handled properly. All these are separate implementations so this needed some work.

# Validated the bundle_adjust versatility on a collection of aerial images with very strong distortion, using our internal RPC (ratio of polynomials) distortion model (this not a usual model in the computer vision community). The results were very good.
 
# Many improvements to the rig calibration software to have it handle orbital images.
# Addressed issues: ground counstraints, filtering out outliers consistent with bundle_adjust, constrain rotation and/or translation of rig transforms.

# Validated on a large SkySat set (900 images, split among 3 sensors and 3 perspectives for each) that these can be fit on a rig using our rig calibrator program. SkySat does not assume all images on the rig are acquired simultaneously, so this was a good test for this feature of our software.

# Added the ability to create simulated images as if produced with a rig.
# The rig can have a mix of linescan and frame cameras.
# // https://stereopipeline.readthedocs.io/en/latest/tools/sat_sim.html#modeling-a-rig

# Added ability to model acquistion time along the oribit in the satellite simulator,
# for frame and linescan img ages. Creation time is saved as part of frame camera
# file name and in the metadata of the linescan images.
# // https://stereopipeline.readthedocs.io/en/latest/tools/sat_sim.html#modeling-time

# Found a problem in the CSM model implementation when it comes to handling time. A fix
# was accepted by the upstream project.

# Added the ability to create camera files given longitude, latitude, height, and roll, pitch, yaw. This is useful for creating cameras based on measurments from an aircraft.
# // https://stereopipeline.readthedocs.io/en/latest/tools/cam_gen.html#cam-gen-extrinsics

# Moved rig_calibrator to the ASP repository (it was originally a codebase in the context of indoor robotics). This needed refactoring and integration work.

# Added to the jtiter solver the ability to model a set of rigs, with each rig having a linescan sensor and which can also have other combinations of linescan and frame sensors. This was a significant amount of new software.

# Validated in a simple experiment that a rig setup improves the control of yaw, as compared to sensors not being tied by a rig constraint. Will research this more going forward.

# Added a tool that can greatly help solve  for lens distortion that manifests itself as large horizontal warping in the  DEM. https://stereopipeline.readthedocs.io/en/latest/tools/dem2gcp.html#dem2gcp 


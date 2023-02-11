Fix quaternion logic

Mapproject MSL data on both MOLA and on the higher res DEM. See how well it goes
as these differ by a vertical shift.

Ship with ASP tool for breaking up maps.
   
Ship with ASP the tool for merging SfM maps, including with fixing first map.
   
Add to sfs --image-list and --camera-list

Fix the quaternion bug! See the VW code.

Add ba option --allow-missing-adjustments.

Overlaying in the gui nobile_1/nobile_5.0 and nobile_6 sfs DEMs fail. Likely issue
with reprojection. Enable region of projection and revisit GeoTransform.

Rename nadirpinhole to orbitalpinhole.

Break up the documentation in examples.rst.

CSM: Worked for 2.5 days. Own David time. 
   
Give warning when left and right mapprojected images have different size.

Note that Pleiades RPC has forward and inverse model.

Implement dgmapdg and maybe remove dgmaprpc?

Add velocity aberration and atmospheric correction for CSM (with
DG). Compare with RPC.

xerces-c is 10 times slower than before, especially with spot 5.
Compare ASP 3.1.0 vs 3.2.0.
cd ~/projects/StereoPipelineTest/ss_stereo_spot5
time cam_test --image ../data/spot5_front.bil --cam1 ../data/spot5_front.dim --cam2 ../data/spot5_front.dim --session1 spot5 --session2 spot5

Make pc_merge read all keywords from the individual files and write them to output file.

stereo_gui: Test if changing the color from the choose table on the left changes
the color of the polygon being saved and drawn (two different functions).
   
stereo_gui: Allow reading separte polygons, and document reading/writing polygons in plain text.

Explain that one must set --style poly --csv-format 1:x,2:y.

Examine this: https://www.mdpi.com/2072-4292/9/7/737/htm

Look at Jay's binary format.

rig_calibrator: Allow fixing the scale even when affine depth to image is used.
Then we cannot do final registration as that will change the scale?

Add tool to merge cnets (merge_maps)

GeoTransform may fail with multiple threads? See the datum_convert
tool. It is not clear if this happens only if the datum needs
changing.
      
When plotting in GUI, don't draw pixel already shown on top!

Bring back ImageMagick! Also in whitelist and the list of libs.

Document the new env. Create this env. Use it in BB.

% Ensure the switch to pytnon 3.9. Doucment in release notes how to get
% such a python. Do not copy to BB, just create it with conda and copy
it. Also with matplotlib.

Add tests for historical helper. See what to do about ImageMagic.
   
Build GDAL with python bindings, as it is needed by datum_convert.
Then update the datum_convert logic, also sparse_disp, bathy logic.
   
streo_gui on OSX has issues. Could be a compiler thing. Maybe Qt compatibility issue.

Update python3.6 to 3.9. Document how to use it.

Remove imagemagick from the distrib. Document about how to install it. Or maybe ship
it with conda.

Test historical_helper.py, and manual doc page, and document the ImageMagic issue.

Support ISIS control network in bundle_adjust and rig_calibrator, on reading and writing.

Check if there is support for NTF in gdal!
      
Test building VW and ASP with the conda compilers when creating conda packages!

More testing for rig calibrator!

rig_calibrator: Redo registration transform after last pass.
   
Fix local alignment bug with large perspective difference

Study why for the nightly regression there is disagreement among depth and stereo clouds! Maybe the stereo angle is too narrow? 
   
Enable OpenMP in pc_align on the Mac. Maybe in other places too!

Add mesh blurring after multi_stereo, and to rig_calibrator doc, also
to documentation!

Spell out the dependencies for MultiView on Linux and OSX. Update the doc.
   
Remove dependency on OpenImageIO from Theia, use instead OpenCV.

See if can use OpenMP with ASP on OSX with multiple threads.
(Only after sync up with latest ISIS).

# TODO(oalexan1): Document all dependencies of all packages and how to install
them with conda

cleanup CameraUtilties.h, move stuff to RPCDistortion. Also fix the sampling.

bundle_adjust: More clean matches are saved than the expected --max-pairwise-matches!

bundle_adjust: Save camera report with positions and orientations.

stereo_gui: Plot curves and graphs.

Check for scale drift in pc_align for skysat.   

Save tsai files even without inline adjustments.

See if using inline adjustments makes it harder to solve the problem.
   
Wipe /home6/oalexan1/projects/GM_20191019 from L1 and pfe. It is archived on lou at
/u/oalexan1/projects/GrandMesa_20191019_WV03.

Wipe the 5 TB and 1 TB on various nobackup drives. See the output.txt for what takes storage.

Wipe data on astrobeast.
   
cdg; sg c2/n1000.tiff c2/n1009.tiff ba/run-n1000__n1009-clean.match
Put bugfix for the above!
   
Must archive and wipe the WV3 runs! Both two recent ones and older ones (grand mesa, BPR).
But need to fix the local aligment issues first, with interpolation and missing tiles.

In CSM ls sensor model, the stopping criterion is wrong! Must be the incremenent!
   
Use bicublic interplation in local alignemnt? See how that affects subpixel artifacts.

See why v2 version of run 1_2 in $HOME/projects/GM_20191019 failed with local alignment!
Could be the same issue as  in the older run in BPR_20210604 or grand_mesa.
A great chance to fix local alignment!

Make hillshade use the centered norm. Do not hillshade at edges.

Add new rig_calibrator/multi_stereo/voxblox/texrecon tools to nightly regression

cam_gen: Can get the camera wrong with just four conrers with no initial
camera. Try to use RANSAC.

Implement reading csv file with x, y, z, and no georef

Implement reading graphs and polygons

Accomplishments:

Added ability to visualize interest point matches produced by rig_calibrator (nvm files) on top of images.

Can visualize residual errors produced by bundle_adjustment (and overall, any scatter plots in geographic coordinates), which help evaluate the acuracy of using this tool.

- Added to ASP CGAL-based tools for mesh smoothing, hole-filling, simplification, and removal of connected components.

- Added the ability to use the exact linescan model for the Pleiades 1A/1B satellites.
  Used the CSM linescan model as an implementation. Compared with using the homegrown
  ASP linescan model. Their results agree to within 2.0e-6 pixels (for pixel operations)
  and similarly small numbers when comparing camera centers and ground intersections.

  The CSM-based implementation is 21 times faster than the ASP implementation.
  In fact, it is twice as fast compared to using the RPC model. Note that what
  was compared here was the combined operation of going from camera to ground
  and back. The RPC model is likely somewhat faster if just doing ground-to-image
  operations.

- The CSM-based and RPC camera models for Pleiades disagree by 0.003 pixels
  (after projecting from image to ground with one model, and back with another one).
     

Add Pleiades example to nightly build!

Add PeruSat to nightly build!

Look at Open Drone Map.

The libelas algorithm does quite well with ISAAC.

Consider some filtering to remove the junk, for example in pc_filter! 
Maybe some median-based filtering, 3D points which differ too much by
neighbrhood of their median need to be wiped.

voxblox_mesh: Is single-threaded for now. Very slow.  MultiView: Add
CGAL as dependency. That will require depending on MPRF.

Add tools for mesh operations to ASP

Add tool for merging meshes
      
Adjust the CMake version to be 3.15.

voxblox_mesh: Save to binary. Much faster that way.

texrecon: save to binary. Much faster that way.

jitter_solve: Revisit creation of anchor points.

Add to rig_calibrator logic for how to plot the residuals in stereo_gui.

Add to rig_calibrator example for how to call stereo_gui to see nvm file.
   
Add rig_calibrator example with no rig

Update the rig_calibrator dataset! Likely it no longer works!

pc_filter: Filter by median with window to remove cables

pc_filter: Add --outlier-removal-params.

rig_calibrator: Get negative distortion with fisheye for queen!!!

multi_stereo: Use --first-image-index, --last-image-index

multi_stereo: Read nvm

rig_calibrator: Apply registration one more time when finished (to the rig too).
Not when there's a mesh!

rig_calibrator: Add pairwise convergence angle report, to help select stereo pairs.

stereo_gui: Add ability to read datum from pointmap. Document this functionality.

Add rergression test with optical bar stereo. See perhaps /home/oalexan1/projects/data/KH4_convert_pinhole_model on lunokhod2.

Trey's request for 2048 texture size

Make build_map save nvm and read nvm. Document that, including how to rebuild to get descriptors.
   
stereo_gui: Polygon gets hidden if in wrong layer after it gets created. Create it on visible layer.

stereo_gui: Insert vertex creates new polygon rather than appending to new polygon which is being
created.
   
ssh for the new mac

polyview needs to be built on various mac platforms, per Bayram's email.
   
Document and test more rpc distortion with rig_calibrator

Make point2mesh write .ply, and also in binary.

Meshes in text format are very slow. Make point2mesh write binary.

Make voxblox_mesh write binary ply file.

Make pc_align read ply (text and binary) and also obj files.

pointmesh with low sampling give inconsistent results, but with hgh sampling gives
consistent results.

Something is wrong with the --inline-adjustments option, it moves cameras way too
far and takes forever.

For each image in run-mapproj_match_offsets.txt find the median of all offets with it
and the other ones. Also print the gsd.

Make applying scale work with with linescan models

Add optical bar nightly testcase

Save --save-mapprojected-matches-offsets

Save convergence angle for each 3D point

Filter outliers by not throwing away triplets if only one of the 3 projections is bad.
      
jitter_solve: Make the pointmap work

Make --match-files-prefix work with --crop-win. Just crop the matches.

Given an htdem run, find the geodiff of pointmap with residuals. Should
given an idea where the triangulated points and DEM are too far.

Bring back the OSX build!

Document the xyzi2csv tool and ship it? 

Try Rosetta data.

Document jitter_solve, add testcase to the nightly build, and notify relevant people.

Look at David's testcase.

Ship numpy with ASP! Add test for rig_calibrator! Also theia_sfm,
multi_stereo, voxblox_mesh.

Bring the build back. Make release tying
to recent isis. Upgrade compiler.

Add note about how to use image_align to find the displacement!

There seems to be a bug in applying transform with scale to .adjust files!

Make the Mac work!

sfs_usage: Add to doc --camera-weight 0, --robust-threshold 5.
Also that one should check the residual_stats.txt file.

Integrate the several bundle_adjust calls!

Add convergence angle!

# How many threads does bundle_adjust use with CSM?
# TODO(oalexan1): --camera-weight may be too high and prevent convergence!

# Gross outliers can prevent convergence in bundle_adjust. Need some kind
# of preliminary outlier removal.

--pairwise-matches: Need to load file2__file1 if file1__file2 is missing.

ba --match-first-to-last. Actually matches last to first!
   
These two are poorly aligned:

Pixel and value for M1315066257LE.mapproject.v4_ba.tif: 1611 997 0.000651754
Pixel and value for M162587237LE.mapproject.v4_ba.tif: 3240 2245 0.0243696

stereo_gui: Implement the hugin functionality for pairwise point picking!
   
Per Andrew, better rectification for fisheye. https://doi.org/10.1117/1.JRS.15.03451.

Per Andrew, remove assumption that filename be a timestamp.
   
Add to bundle_adjust:
--image-list
--camera-list
--mapprojected-data-list
Pairswise convergence angle and count per pair, based on clean matches

Must rewrite StereoSessionPinhole.cc. The tx_left() and
camera_models() functions must return unaligned cameras and proper
alignment transform in epipolar mode, rather than aligned cameras and
identity transform. Must handle correctly crop wins.  Must compare
with affineepipolar alignment after that with crop wins on/off. This
will be quite some work.

Eliminate L__R.match from alignment method none and epipolar!

Add filtering of D_sub and of matches based on cameras even without datum!

Add convergence angle to stereo_pprc for all sessions!
   
Do one more test with epipolar vs affineepipolar for pinhole and nadirpinhole using crops. See if the DEMs agree.
      
image_align: Avoid cutting when second image moves too much left. Add an option which would say --co-alignment.

Need image_picker to read from bag. Should read from list too. Maybe two tools,
called bag_image_braketer, and image_bracketer, using same underlying logic in a shared util file,
but one of these also depnding on ROS.

rig_calibrator: stereo_gui can make sub2 files which may be incorported in calibration!

rig_calibrator: Add a simple example with two sensors, two images for each, and no timestamp interpolation.
   
rig_calibrator: When saving clouds for fusing, take into account distorted_crop_size.
   
rig_calibrator: Make --affine-depth-to-image the default. Deal with scale issues.

rig_calibrator: Filter out points in depth cloud at boundary. Also give weights to them.

rig_config: Add post-registration.

Do we need a robust threshold for triangulation?
   
Ship and document fit_rpc. See if to merge with other tools.
   
Ship .vwrc and increase there the cache size.

Add links to new tools in introduction and other places! Maybe even in the NEWS.

Move the example from the multi_stereo page to its own chapter. Link to it from
the SfM section.

Write tool named rig_bracket.

Write tool to write images and clouds. Also to query for timestamps, which later
can be used by rig_bracket, and to extract only images/clouds with given timestamp.

stereo_gui: Show pairwise matches.

Examine little ISS run. Why so noisy?

Put a picture for the ISS stereo mesh fusion run.

Check in the voxblox recent changes. Same for rig calib. Update doc.

Speedup voxblox.

Document fit_rpc. Show example. Evaluate acuracy.

Replace rig_config and rig_sensor with config and sensor.

Remove the need for undistortion from multi_stereo.
   
Test --alignment-method none with step-303_d1.0_ceil_000779_step-303_d1.0_ceil_000785_v2.

Implement alignment-method nonlinear. Use the above as testcase.

Read and write pcd, ply, obj, in point2dem, pc_align, point2las.

Add mesh_project tool in MultiView. Ensure all libraries are dynamic
as this is being used in other tools.

Add mgm_multi stereo algorithm, micmac, tv-l1 (but need to fix the boundary issue). adcensus.

AliceVision algorithm and blunder handling:
https://meshroom-manual.readthedocs.io/en/v2019.2/node-reference/nodes/DepthMap.html

Fix bug with asp_bm correlation on boundary. Look at one of the
nightly LRO NAC testcases with local epipolar!

Look at AliceVision, particularly their blunder rejection techniques!

Make voxblox multithreaded!

Make ply file binary!

Fix caching bug!

pc_filter: Find correct surface normal direction and filter away
patches with their back at the camera.

Fix sfs bug on OSX with top!

Update the solved example to use distorted_crop_size.

Look at David's runs. Grand mesa and the other one, and see if local epiplar
alignment can be improved. Particularly, too much of D_sub is filtered out.

Archive and wipe the Puerto Rico and Florida data sets.

Have rig_calibrator output individual depth cloud meshes, for inspection
   
stereo --ip-inlier-factor seems to be needed. Need to evaluate how to carefully
filter outliers with the nadirpinhole session and no datum.

bundle_adjust: Write the number of pairwise matches.

Bundle_adjust: Write convergence angle, per pair.

Clean output of parallel_stereo. --keep-and-merge 'PC.tif F.tif'.

Read Theia nvm matches. Merge new matches with nvm. Write nvm.

Don't do bracketing when there is no rig.

updateRpcUndistortion: Expose those params? How about num threads?

Must add dist_crop_size, undist_crop_size!

rig: Add option --apply_transform. Add option --post_registration

Save texture in subdir if rig_out dir called "texture"
When it comes to excluding pixels, do nav_cam:100 sci_cam:100 haz_cam:10. Otherwise it is confusing.

Let voxblox mesh take as input a tsai file.

Fix cache issue.

Experiment with various texrecon options

Make rig_calibrator and texrecon save and load .tsai.

Add utils for reading bag.

Add ability to read and write pcd

voxblox: Should be able to read just xyz and xyzi

Add image_picker. It must be pick and bracket all cameras, not just sci cam,
so rig_calibrator has less work to do, and so Theia can be called right away.

Add tests for new tools!
   
Make Theia and the rest work on OSX

Ensure that radtan is the same as the tsai distortion model!

Add support for fisheye in ASP
   
Read and write tsai. Read and write nvm. Read matches.

Write voxblox.

Write undistorted images.

Write texrecon.

# Explain that images must be 8 bit and that will be read as grayscale.
# That is good enough for voxblox. for texrecon we will need color.
      
# TODO(oalexan1): Theia installs too many things.

# TODO(oalexan1): May need to add --pre-registration and --post-registration flags
# with the latter scaling depth to image, if there are depths.

# TODO(oalexan1): Think more about floating scale and floating ref cam poses. 
# Maybe that should be allowed. Now it is not, by default.

TODO(oalexan1): Modularize this code!

When doing Theia it is unwise to refine the intrinsics right way! Maybe the software should
implement that!

Update export_reconstruction_vw.

Ship with ASP. Add there the doc.

Note that Theia can write ply files as well. Aslo patch up export_reconstruction_vw.

Clarify directory convention. Use sensor_id rather than cam_name for consistency?
Or vice versa? Use cam name instead of sensor
   
Must set OIIO

Must set paths in rig_calibrator.py, then document all that.

Support multiple match files in GUI and selecting which images to show

Must use Theia matches!

Must read back .match files if present!
   
Bracketing is not necessary and will fail if there is no rig constraint!
   
Parse the output from Theia, put back the timestamps

To have libGL, conda install -c anaconda mesa-libgl-cos6-x86_64
Then may need to link with
/usr/lib/x86_64-linux-gnu/libXdamage.so /usr/lib/x86_64-linux-gnu/libXxf86vm.so /usr/lib/x86_64-linux-gnu/libXext.so /usr/lib/x86_64-linux-gnu/libX11.so

Theia: Remove dependence on the OpenImageIO, ues instead OpenCV.

rig_calibrator: Load existing matches if present!
   
rig_calibrator: Move a lot of things to utils!

Qhull on conda forge has only static libraries. Need dynamic ones for PCL.

Need to install tbb-devel in the isis6 env
   
Test pinhole and nadirpinhole with local_epipolar, epipolar, and affineepipolar.

Archive PuertoRico2 WV03 dataset.

Wipe a lot of data from astrobeast, lunokhod1, and pfe. Ensure it is archived properly though.

ASP does not like very tall images, it complains about caching and such. For now need to use gdal_translate to convert it.

pc_align: Introduce option --apply-transform-only
Then avoid Computing the intersection of the bounding boxes of the reference and source points using 4000000 sample points, also --max-displacement

Remove duplicate code in StereoSessionGdal.cc, StereoSessionNadirPinhole.cc, StereoSessionPinhole.cc, StereoSessionIsis.cc

exr writes floats! This is a problem as the alignemnt transforms may not be
accurate enough!

ISS model: http://raiznewmedia.com/iss.html

When copying the calibrator code, add note in the acknowledgements and doc!

In PR03_ASP you have the result with bundle adjust and alignment, and the run without any bathy module of any sort:

ASP topo: GR_NoBMod_DEM_3Q3-DEM.tif
ASP bathy: PR03_GRNBA1k_L05lim30k_Otsu_TB_ali100m_TB_bx-DEM.tif 

Lidar: topo: PR_2018_Topo_Geoid12b_USGS.tif
Lidar bathy: PR_2019_Bathy_Geoid12b_NOAA_NGS.tif

Need to figure out if this his with or without geoid correction. Monica also has the input WV
data, apparently.

See all this in /nobackupnfs2/oalexan1/projects/PuertoRico on Pleiades

Relax --ip-inlier-factor in stereo!

Do filtering based on reprojection error even when there is no datum!

Nice blog on various multiview solutions
https://peterfalkingham.com/2020/07/10/free-and-commercial-photogrammetry-software-review-2020/
https://github.com/flanggut/smvs
https://peterfalkingham.com/2016/10/29/photogrammetry-testing-7-smvs-mve/

Some dependencies are picked up from system, not from conda.

Must build the rig calibrator for Mac
Get a wrapper around texrecon
Sync up to latest tbb, boost, mapmap
TODO(oalexan1): Replace --float_sparse_map with something else
TODO(oalexan1): Modularize camera_calibrator
Remove all mentions of nav_cam, sci_cam, haz_cam.
Add example.
Compile theia.

Send report to Jay.

Update point2las to pdal.

points2dem to add stats to it like for points2las
Collected a sample of 925593 positive triangulation errors.
For this sample: min = 3.2038e-05, mean = 1.26303, stdev = 0.288904, max = 8.8603.
Error percentiles: Q1 (25%): 1.08396, Q2 (50%): 1.25892, Q3 (75%): 1.43848.
Using as outlier cutoff the 75 percentile times 3.
Found the maximum valid triangulation error (outlier cutoff): 4.31543.
   
Bundle_adjust: Add convergence angle.

Add tests for cloud filter tool. Add ability to remove noise by median, also blur and fill holes. Look at the isaac code.
   
Good run step-303_001895_step-303_001902
Bad run step-303_001914_step-303_001919
Why?
What works is parallel_stereo --alignment-method local_epipolar --corr-tile-size 300 --sgm-collar-size 100. Presumably bad distortion makes for bad alignment, and then mgm gets confused. Local alignment forces it to operate on a line so it does not get lost.

It appears that writing pcd is slow. See how to optimize it. Maybe write
in binary. Also make voxblox multithreaded if it is slow.
   
Try opencv_sgbm on problematic testcases! Verify that there is epipolar alignemnt!

Build voxblox without ROS on lunokhod1.


pcl-1.9 does not build on Mac!
vl /Users/oalexan1/miniconda3/envs/isis6/include/pcl-1.9/pcl/impl/point_types.hpp:697
Change per https://github.com/leggedrobotics/tensorflow-cpp/blob/master/ISSUES.md

gen_pcd: Estimate the triangulation error by percentile and factor.

Smaller triangulation error needs to be given more weight.

Import PCL, PLY, and Obj in point2dem, point2mesh, and point2las

Need to remove small blobs!

Use blending weight, based on signed distance function and distance to point.

# TODO(oalexan1): Rig code needs to be compiled a few times till it succeeds.
# This should be fixed by making mve, mapmap, and rayint submodules.
# But note that rayint builds its own copy of mve which complicates things.

MultiView: Update to latest tbb, boost, mapmap, and mve.

MultiView: Add environment.yaml. Expand build instructions. Tell how to set path to conda.

# When x264 fails
conda install -c conda-forge x264=20131218 --force-reinstall

Look at umve! Look at the logic for loading and saving meshes.

Add texrecon dependencies as submodules! Otherwise compilation needs to be restarted!

point2dem and mapproject: Unify GSD determination. Point2dem one uses median only, which is fragile, especially for LRO NAC.

Look at mve's SfM and multivew stereo! It features structure from Motion, Multi-View Stereo and Surface Reconstruction.

Add fisheye lens distortion to VW

Mac tests are failing!

Make the Mac work with newer compiler and OpenMP. There seems to be a problem in stereo_pprc.

Epipolar and homography alignment produces strange results with the ISS bumble dataset.

Implement algignment method nonlinear

For local_epipolar, try with not filtering the disparity for David's testcase.
      
Ship with ASP the rig calibrator, mesh fusion, and seamless texture
      
Tell Jay about what got done in the remaining 3 weeks.
- Added to ASP rig calibrator, tools for fusing point clouds and creating meshes,
   and for seamless textures. These were ported from a robotics project and a lot
   of refactoring and changing of dependencies as necessary. Point him to doc.    

- Monica: Wants some stats things in point2las as I recall.

- Monica: Poster.

Must charge the ISAAC time!
Bathy 1 week and 1 day left.
Stereo2SWE 0.35 months left for August
CSM 1 weeks left
ISAAC 2 weeks left
PDART 2 weeks left?
Cryo 2 days left

Look at the texrecon and mve utilties for dealing with meshes, including mesh 

# Test datum with Csm mapprojected images!
# Add nightly testcases for --alignment-method local_epipolar msmw, msmw2.
# Clean msmw to remove debug info.
# Why PRC for PeruSat has a 15 shift compared to the exact model?
# cam_test shows that increasing the height offset in the RPC
# model reduces it to 2 m or so.
# Do release!
# Autoguess aster
# TODO: Port these changes to the base class
# Figure out the right degree of Lagrange interpolation to use with PeruSat
# Add aster testcase!
# PeruSatCameraModel::point_to_pixel uses levenberg_marquardtFixed which
# needs to be dealt with.
# When running stereo l.tif r.tif l.xml r.xml it fails unless session is dg.
# How about having it auto-detected?
# Use the usgscsm functions to apply transforms, once that is released
# Remove the datum from IsisInterfaceSAR.cc. Find better function for camera direction.
# Gotcha: Should the region of valid pixels be expanded after gotcha?
# Note the params in 
# /home/oalexan1/projects/CASP-GO/casp-go-params/stereo.ctx
# used with CASP-GO. The dataset is G03_019369_1400_XN_40S236W.map.cub
# G02_018947_1401_XI_39S236W.map.cub
# Ilmbase imath may need upgrade, see Ross's email.
# Fix camera_solve on the mac!
# The weights used for blending could be smoother! Examine them again!
# Study Monica's testcase! Use the D_sub filtering! It is essential
# for local alignment as we use it to find the range.
# Tiles which have no final correlation in the central area must
# not be used!
# Examine disparities in tiles. They may have edge artifacts! Need to do
# some filtering on blending!
# Add report at the end of parallel stereo showing much much ram and time
# each node took.
# Theia is broken in this release! Also enable the tests on decoder and lunokhod1!
# Move all OGR (grep for it) from ASP to VW.
# Hide msmw debug info
# Unit tests are broken in both VW and ASP!
# Libelas outlier filtering may be fragile
# ASP ships too many things in its base dir coming from python!
# Bundle adjustment: add option for max-num-ip-per-image! Look also at --ip-per-image!
# Add a single using_tiles() function!
# In stereo_corr the match file being loaded can be wrong! Load the one being
# written, not by using heuristic!
# Go back to having a plugins directory, reflect this in conda, packaged asp, and doc.
# Do not load the cameras so many times. In stereo_tri it happens three times!
# Merge StereoSessionGdal with StereoSessionNadirPinhole.
# Remove special pixels in CSM images and consolidate special pixel removal in several places.
# Make a brief list of all ASP dependencies outside of ISIS.
# Check local_epipolar with mapproj images.
# Revisit all tutorials, and see how to mention the stereo algorithms and point2dem
# creation.
# Check default tile size and padding size. Fail for zero padding size with local_alignment.
# Must the aligned tiles always have no tiling as for msmw?
# TODO(oalexan1): Interpolation creates holes. Does it creates artifacts?
# Duplicate of hidden function!
# TODO: Unify with function in vw/src/InterestPoint/Matcher.h!
# Use parallel_stereo for everything!
# TODO(oalexan1): May want to do erosion! Only where there is overlap!
# TODO(oalexan1): Unwarping the disparity makes the holes bigger!
# How about bicubic interpoation?
# TODO(oalexan1): Should lStats.tif and rStats.tif be recomputed
# when the crop window changes?
# TODO(oalexan1): Document local_epipolar in more detail.
# Merge StereoSessionSpot into StereoSessionGdal.
# TODO(oalexan1): In local_epipolar, for the ISIS session,
# remove ISIS special pixels before applying local alignment.
# TODO(oalexan1): Document the plugin business!
# External algorithms with mapprojected images!
# OMP_NUM_THREADS=4 with MGM?
# TODO(oalexan1): How about using an input mask?
# TODO(oalexan1): Upgrade ISIS
# TODO(oalexan1): OpenEXR to version 3, see if to remove, imbase. Build that locally first.
# TODO(oalexan1): Remove some versions from ASP packages, rely on ISIS basically
# Point2dem is too aggressive in filtering outliers by range
# See how changing the collar size affects the results!
# Look at Terry's email about release
# Finish the experiment on astrobeast and 
# cleanup the data (there's a lot of it).
# ISAAC fix for sci cam (publish that exif topic)
# ISAT2 or Aster or SRTM DEM for Monica
# Monica's florida dataset times out. Figure out why.
# IP are noisy!
# Upgrade OpenEXR to version 3, see if to remove imbase
# upgrade usgcsm in the plugins directory
# relax point2dem outlier filtering by extent before release!
# Fix the subpix3 artifacts at the boundary!!!
# Add a tool to erode the boundary!
# Use pcl logic for filtering!
# OpenMP on OSX
# Gui work for shapes and projections
# Remove Unicode from ASP doc
# Make a list of all ASP conda dependencies, including chrpath, OpenEXR, libglvnd-cos7-x86_64, etc.
# Need to be careful with ceres version!
# Need to be careful with fftw version! fftw=3.3.8=nompi
# Use PeruSat as testcase! In steep terrain! See the readme.txt there.
# The imagemagic package for isis4.4 on OSX is misbehaving. It is missing gm or something.
# BinaryBuilder gets duplicates of some conda libraries from the system!
# Killing parent process makes the system calls continue!
# Must not allow the collar size get too small!
# Must not allow a tile failure to ruin the whole run!
# Elas: See about disp_max! Now it is hard-coded to 256!
# It is sensitive to the size of the padding!

sfs: Look at improving the RPC approximation. The code is also slow.
Add robust threshold for the intensity values. Maybe also for the other penalties.
sfs: There is a bug with crop-input-images, it creates an intensity of zero instead of not-a-number. It occurs only when we are outside of the cropped area.
      
Advertise camera_solve better in the doc!

build_map does not obey num_threads.

Study Daisy, dense image matching.

Bundle_adjust: Examine all point_to_pixel functions for accuracy.
   
jitter:

The percentiles for jitter need to be done more cleverly, based on how many image lines are there!

Jitter: When using BA as initial guess, instead of passing R, T, pass R, T + C  R*C.
   
Fix D_sub based on ip matches

Filter D_sub based on triangulation error

point2dem: The projection center (for stereographic proj etc) needs to be computed automatically.
   
mapproject has a bug sometimes with the cropping window.
   
Warn if a new dem file is used during stereo as compared to during mapprojection.
   
If input images are newer than the run directory, must regenerate l, r, ip, etc.

Put a Pluto testcase

Fix bug in Map2CamTrans around the pole.

Make sure pieceiswe adjustment does not work with crop win and adjusted cams.

Make sure the stereo_pprc valgrind log is clean.

Integrate duplicated functions for reading and writing adjustments.

if csvproj4 is specified, datum is not necessary

write the datum on output point clouds
      
Move ssISIS_alignHom_seedMode1_mapProj0_subPix1_crop_left_right to
ssISIS_alignHom_seedMode1_mapProj0_subPix1_crop_left_right_parallel

Create a single stereo_settings().nodata_val.

Replace everywhere float nodata with double nodata!

common.h: Remove some reduntant write_image functions.

ssPinHole_alignHom_seedMode1_mapProj0_subPix1

Move low level functions from the Widget class, especially the georef ...
   functions.

Integrate in vw the various approaches of traveling on the box to find
the bdbox. Complete the x.

Also maybe push to VW the pyramid class, by stripping
out any qt things.

bug in geodiff with nodata!~/projects/geodiff_bug

Get rid of compiler warnings!

mv ip from tif_mosaic and lronacjitreg!

rename stereosssiondg to stereosessiongdal. Clean a lot of stuff there.

Study if to skip the stereo_fltr step.

datum_convert, why does it offset by 360 degrees?

datum_convert: Does not convert properly to itself. Also does not work with mars.

dem_mosaic: study if the the weights never have discontinuities.

dem_mosaic: Investigate Sshaped weights.
   
mv rpcmaproject to mapproject in tests

image_calc needs to support compression

dem_mosaic: Does not behave well if given incorrect options
   
image_calc c '0*var_0+0' runfinebigDEM.tif o runfinebigheight0.tif d 5

gives wrong results, says all image is nan!

stereo_gui print pixel location and pixel value

image_calc: if no output nodata value specified, use the minimum of the input ones.
   
Fix for dem_mosaic with many files.
   
Convert the gui pyramids to gdal. Should be much faster presumably.

Fix bug, if there is Lcrop and Rcrop, then wipe these, and L.tif, R.tif, D_sub.tif, etc. from zeroth stage of stereo.
   
Upgrade to newest gdal

launch_master: Fix bug in which editing launch_master makes the tests fail!

List somewhere all features of ASP, on the website and readme!

fix crash with stereo_gui with singleimage

dem_mosaic must use a caching system.

make hied2mosaic also download things.

image2qtree works only on uint8 images. Make it work on all images by rescaling.

stereo_gui: Give errors if failed to parse the options!

There is a problem with stereo model vs rpc stereo model!!! Also see how ba and stereo_pprc react.

Add regression tests for cam2map4stereo.py and for hier2mosaic.py and all the tools.
      
stereo_gui: Make it show interest points for cropped runs.
   
stereo_gui: When images are displayed first time, display them at no finer than the natural resolution.

stereo_gui: Dump screenshot.

Get rid of the GdalIo error about spread file not existing, except when it is actually an error.

replace float with double in any control network and ba code in asp
and vw. See where else there are floats.

When compiling ASP, too many headers and libs are used. for example, point2dem
   must not link to qt or include qt headers in compilation.

stereo_gui: Must try to write sub images in path/to/image_sub.tif, unless this fails
   and unless the path is absolute. Otherwise get a problem with two files resulting
   in same file on disk, eg, run/file.tif and gold/file.tif
   
Must change the Wikipedia link to say that ASP supports multiview.

point2dem crashes when the input is a DEM. Fix this!

stereo_gui must be able to display interest points generated with cropped
images and downscaled images!

rename dem_mosaic to dem2dem, and add options for denoising and bluring.

Advertise more that dem_mosaic can fill in holes.
   
Test the effect of the noproj step.

Running stereo after stereo_gui will cause problems as many things are cached!

Add tool to resample images with antialiasing

parallel stereo is very slow for triangulation for nonISIS cams. May need to crop! And use just one thread and many processes!

paralle_stero if CPU is not fully utilized, start more processes! Use the KH9 testcase at 25% for esting! Or use double the number of desired processes!      
      
The case when input images are stored columnwise rather than blockwise makes stereo_pprc very slow.

pc_align: if actual displacement turn out to be smaller than the specified displacement,
must redo the calculation!
   
Introduce adaptive correlator size.

Investigate the bug David keeps on seeing, always a 2.1 meter systematic offset in ASP. Does it show up with RPC, DG, or during point2dem?

parallel_stereo triangulation step is unnecessarily slow. Use more processes maybe,
or maybe do it as one single process, for nonISIS.
   
Add tool to fill holes in dems, blur DEMs, remove noise, erode pixels.

Make point2dem take as input DEMs, for the purpose of resampling, blurring, hole filling, noise removal, changing projection.

Implement mapproject with antialiasing.

find the mapproject grid automatically.

Say how to use projwin to crop images for the purpose of fast results.
   
Filter by slope in dem creation somehow.

See what ways of filtering of clouds exist there.

Fix boost warnings in ASP compilation.

Run the linter through ASP to convert to google style.

In the MER dataset, wget fails for second dataset.
   
ASP:
VW Error: Reading from images with more than 6 channels is not implemented.
8 bands total in the M1BS ntf.  Is this an easy fix?

mapproject assumes that the image has just one channel. It will simply read the first channel and ignore the other ones, this is probably not what you want.

Fix this! Die if there's more than one channel! Suggest dg_mosaic should be used.

pc_align: Add nodata value field.

Theia notes:

need to create output directory for /u/oalexan1/projects/mlp/rec/theia3/matches/  img_output_dir=
document calibration file
 always principal point is at center of image?
 note about suitesparse being required rather than optional
 FindSuiteSparse() ... needed.
const double kPoseTolerance = 1e3;
 document calibration file
 is out_dir being created?

http://cs.ucsb.edu/~cmsweeney/theia/applications.html
./bin/extract_features input_imgs=/path/to/images/*.jpg img_output_dir=/path/to/output num_threads=4 descriptor=SIFT
Need to say about other matcher, CASCADE_HASHING, not mentioned at all
Need to say about using quotes, '/path/to/images/*.jpg'
         
bundle_adjust in ASP: Add outlier removal step.

Add erosion in point2dem!

to do: Holefilling in dem_mosaic.

Better holefilling in point2dem.

Smooth weights in dem_mosaic.

Add to documentation suggestions about how to improve quality with
outlier removal and how to improve speed.

Implement Sun's tool.

convert the *PC.tif to a ply file with potentially a 'color' array taken from the *L.tif file
   
In computing disparities in CorrelationView.h, the zone is a bounding
box of all disparities. The actual number of disparities could be much
less! Need to exploit that.

See other correlation metrics except for NCC!
   
orbitviz documentation needs synching with code!

If launch_master.sh has local modifications, it will fail each night!

Bundle adjust: There can be a huge number of interest points, but just a few cameras, and not some many gcp. Some normalization is in order.

Add tool for resampling with antialiasing. Also put that in mapproject!

pc_align: Don't load entire csv file, could be huge. Maybe do like for pc and las!
      
Implement emblend functionality.

dem_mosaic: Clean up the code a bit where the offset is handled.

To do: Test bundle adjust with rpc images!!!

How many threads to use by default? Now 8 are used in some applications and num_cpus in other.

Wipe RPCStereoModel, use StereoModel for everything, it is good enough!
   
To do: Speed up parallel_stereo by making filtering happen only in current crop win (with loi). So don't go through the entire RD.tif.

To do: fix bug with too fine resolution in mapproject!!!

To do: Mapproject should remember the DEM used for projection!
   
Add note in tutorial about improving the acuracy on steep slopes.

corrtimeout needs improvement. The whole algorithm for splitting into blocks needs to be better.

Error cannot be a vector when triangulating with more than two points.

parallel_stereo, if no enablefillholes, make this step parallel.
   
Look at nonlinear local homography!!!

point2dem: Play with search radius factor and how much should gaussian decay!

ASP: Make the interest points finder for mapprojected images
be the same as for nonprojected images. Care with normalization.

geodetic_to_cartesian and cartesian_to_geodetic differ by 360 degrees!
   
Make sparse_disp work with developer build after make install.
      
In libnabo and libpointmatcher, the g O3 flags are not necessary.

make stereo_fltr multiprocess if disablefillholes is on

throw error about missing DEM right away if seedmode 2 is on.
      
Integrate libpointmatcher.

need to intersect mapprojected images to do alignment method none

See places where machines are hardcoded in auto_build.

More robust search for whether the documentation was built successfuly.

Remove the reverse port forwarding, do an honest connection (need sudo!).

Need to make parallel_stereo robust to not being able to connect, ...
etc. Particularly when the name is localhost.

Use "test_passed" "test_failed" instead of "test_done success"

Get rid of skipBuild

Expand the storage for all vrts, see if we can share storage

Robusty status line to extra spaces in auto_build.

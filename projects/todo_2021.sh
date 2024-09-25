FY22.

Monica (bathy): 1 week in November, 2/3 in January, and half in February.

Bathy:
# Stopped working on bathy 3/12, that is 3 weeks of bathy work in total.
# March 23, 24, 25, 26, 29, 30 is bathy day. 21 days so far.
# April 7, 8, 9, 12, 13, 14, 15, 16, 19, 20. 31 days so far.
# April 30, May 3. 33 days so far. 7 left.
May 18, 19, 20. 4 days left.

# CSM. Have 1.8 months, which is 8 weeks.
# Started working on CSM on March 15. Stopped on morning of March 19. 4 days.
# June 25, 28, 29, 30, July 1, 2. 10 days.
# July 5, 6, 7, 8, 9. 15 days.
# July 12 13 14 15 16, 20 days.

# PDART: Have 2.3 months (10 weeks, 50 days).
# This year the job is to do local alignment with lots of testing!
# Started working on PDART on March 19. Have 2.3 months. 2 days so far.
# May 5, 6, 7, 10, 13, 14, 17. 9 days so far.
# May 21, 24, 25, 26, 27, 28. 15 days so far.
# May 31, June 1, 2, 3, 4: 20 days so far.
# June 11. 21 days so far.
# June 18: 26 days.
# June 21, 22, 23, 24, 30 days.
# July 19, 20, 21, 22, 23, 35 days.
# August 18, 19, 20. 38 days.
# August 23 24 25 26 27, 43 days.
# I will need to work 50 days. 

# ISAAC:
March: 3 days.
April: March 31. April 1, 2, 4, 5, 6. 6 days.
More April: April 22, 23, 26 27, 28, 29. 6 days.
Ran out of time. Also worked on May 4, on my own time.
May 11, 12, deficit of 2 days.

For IAAC actually have a 1 week deficit, because I started a
couple of days earlier than planned in October 2020, and
used extra 3 days to finish some remaining mapping.

15 weeks left
3 vacation
7 csm
5 pdart

To do the expenses:
perl -pi -e 's/[^[:ascii:]]//g' ~/projects/todo2.m; perl -pi -e 's/\-//g' ~/projects/todo2.m; perl -pi -e 's/\$//g' ~/projects/todo2.m

# Make SfS work with csm model or another kind of model.
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

 make point2dem take as input DEMs, for the purpose of resampling, blurring, hole filling, noise removal, changing projection.

 implement mapproject with antialiasing.

find the mapproject grid automatically.

Say how to use projwin to crop images for the purpose of fast results.
   
Filter by slope in dem creation somehow.

See what ways of filtering of clouds exist there.

Fix boost warnings in ASP compilation.

Run the linter through ASP to convert to google style.

In the MER dataset, wget fails for second dataset.
   
ASP:
VW Error: Reading from images with more than 6 channels is not implemented.
8 bands total in the M1BS ntf.  Is this an easy fix?  Thanks.

mapproject assumes that the image has just one channel. It will simply read the first channel and ignore the other ones, this is probably not what you want.

Fix this! Die if there's more than one channel! Suggest dg_mosaic should be used.

pc_align: Add nodata value field.

Thea notes:

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
   
password for centos is centos.
   
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

Document the user names on vrt machines

Robusty status line to extra spaces in auto_build.


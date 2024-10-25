# See the OSTFL document in my ASP Google drive.
# See the FY25 plan in the same place.

# Task 1: Maintenance

This proposal has allowed ASP to be actively maintained. Issues raised on the mailing
list are prompty addressed, bugs fixed, and documentation updated as need be. 

# Task 2: Software Release

Each year a new release is being produced. The release is tested on a regression test suite that consists variety of datasets and scenarios, whose numbers grows as more functionality is added. All changes and additions to the software are noted in the release notes and the documentation is updated. Each day a new build is published, after compiling the latest code and running it through the regression test suite. 

# Task 3: Tutorials

The ASP documentation is being updated with detailed examples for how to use the tools in various scnearios that we support. The commands are listed, together with things to note, illustrations of output, and links to other relevant sections.

To go into more detail, many examples were added for how to use the bundle_adjust, jitter_solve, and parallel_stereo programs. Many existing examples were also updated and illustration added.

Documentation:

https://stereopipeline.readthedocs.io/en/latest/tools/bundle_adjust.html
https://stereopipeline.readthedocs.io/en/latest/tools/jitter_solve.html
https://stereopipeline.readthedocs.io/en/latest/examples.html#stereo-processing-examples
https://stereopipeline.readthedocs.io/en/latest/examples.html#sfm-examples-using-a-robot-rig
https://stereopipeline.readthedocs.io/en/latest/sfm.html

Did Shashank do any work here?

# Task 4: Improvements

# This task contains seven sub-tasks, each of which corresponds to an effort of
# one month or less. They aren’t particularly interconnected, but they also
# don’t easily fit into any larger effort. They are prime examples of software
# development tasks that independently are small, but in the aggregate,
# represent general improvements for users of the software suite, making it more
# useful to a greater number of people.

# Task 4a: Error Estimate Improvement One of the most frequently requested
# features in ASP is robust, per-pixel error estimates for output products.
# Currently ASP only provides the triangulation error for each pixel, an error
# measure that does not take into account factors such as the terrain, image
# correlation quality, and uncertainty in the camera positions. This task will
# establish algorithms to provide an uncertainty estimate for each point in the
# point cloud produced by the stereo program based on covariance values bundled
# with camera position and orientation tables (or CSM equivalent, e.g., Dolloff
# and Theiss 2012; Zhang and Boult 2011). This will provide a major improvement
# in interoperability of satellite stereo derived DEMs with LiDAR elevation time
# series, filling an important gap identified in the Surface Topography and
# Vegetation mission incubation study white paper (STV Incubation Study Team
# 2020). 

Task 4a: 

Uncertainty estimate errors implemented. The input camera covariances get propagated
to the output point cloud and DEM. The input covariances can either be read automatically
from the vendor-provided camera model files, or specified by the user as specific values.
This works with the stereo and bundle adjustment tools.

Detailed documentation added at:  

- https://stereopipeline.readthedocs.io/en/latest/error_propagation.html
- https://stereopipeline.readthedocs.io/en/latest/tools/bundle_adjust.html#error-propagation


Added to bundle_adjust and jitter_solve the option --camera-position-uncertainty, in units of meter, that allows the user to specify how much camera centers can move. Added to both tools the option --heights-from-dem-uncertainty, in units of meter, for when an external DEM is used as a constraint.

Documentation: https://stereopipeline.readthedocs.io/en/latest/tools/bundle_adjust.html#error-propagation

# Task 4b: Improvements in output point clouds The products that ASP
# traditionally creates are point clouds in a custom 4-band TIFF format, gridded
# DEMs, orthoimages, and plain text meshes. A widely used format for lidar point
# clouds in the geographic information system (GIS) and remote sensing
# communities is the LASer (LAS) format (or the compressed LAZ format). ASP
# supports basic conversions from our point cloud format to LAS and creating
# DEMs from LAS. However, this conversion capability is very basic and there is
# no support for additional point attributes (like error values). Under this
# task we will expand this support by exporting to LAS not just the Cartesian
# coordinates of each point in a cloud, but also the intensity, RGB color,
# and/or multi-spectral reflectance values for each point, which can be
# retrieved from the corresponding input stereo images. When ASP creates a point
# cloud, it also produces additional information per point, such as the norm of
# the closest distance between the rays that give rise to the point, or the
# vector between the closest points on these rays. These metrics and other
# uncertainty measures will be addressed under Task 4a, and we will add
# functionality to export these metrics in the output LAS/LAZ point clouds. The
# dense stereo point clouds can be very large, with a point for every input
# image pixel. Some WorldView-3 stereo point clouds contain over 14 billion
# points, leading to significant runtimes for single-threaded I/O and
# unnecessarily large data products, even with compression. To fix this
# limitation, we will streamline processing (i.e., fast global statistics and
# bounding box determination, intelligent culling/thinning) and add the
# capability to write the LAS tiles in parallel. We will switch from the
# deprecated LibLAS library to the PDAL library for manipulating LAS files, and
# limit the precision of point coordinates and attributes which can result in
# significant storage savings. The PDAL library does not have full support for
# parallelization on its own, but the input file is already tiled, and we will
# spawn multiple PDAL threads in our process to do the work, all streamlined for
# the user. Overall, these improvements to the conversion will support ASP users
# who are more familiar with LAS point cloud data rather than gridded DEM, and
# simplify integration of stereo and lidar point cloud processing using other
# specialized tools (e.g., CloudCompare). This new functionality will be
# especially relevant for vegetation studies and research investigations
# involving stereo photogrammetry vs. lidar performance for terrain mapping
# across the Earth’s diverse landscapes.

Task 4b: Moved from the deprecated LibLAS library to the PDAL library for manipulating LAS files. Contributed to the PDAL library documentation and examples for how to process clouds in streaming mode.

Added the ability to filter point clouds using varous outlier removal schemes before saving them to LAS.

Added the ability to export to LAS the triangulation error of point clouds produced by ASP. 

Added the ability to export the image intensity (from the left image) to the LAS file, alongside the triangulated points.

Added the ability to export the horizontal and vertical components of uncertainty
from the point cloud to the LAS file.

The same approach can be easily extended going forward for any other bands the user requests.

Documentation: 

https://stereopipeline.readthedocs.io/en/latest/tools/point2las.html

# Task 4c: Alignment Improvements ASP provides the widely used pc align tool
# (Beyer et al. 2014; Shean et al. 2016), which can align (or co-register) two
# point clouds, whether they are in the ASP point cloud format, raster DEMs, CSV
# files, LAS files, or any mix of these. This tool is very useful for correcting
# any residual registration issues due to uncertainty in the positions and
# orientations of the satellite cameras used during stereo triangulation and
# point cloud creation. Currently this tool offers a limited subset of point
# cloud alignment methods, including variations of iterative closest point
# (ICP), with the implementation in libpointmatcher (Pomerleau et al. 2013;
# Pomerleau and Magnenat 2019), and the Fast Global Registration (FGR) algorithm
# (Zhou et al. 2016). Point cloud co-registration is a common task in the
# rapidly evolving computer vision community, and there are many other
# algorithms available that could improve alignment robustness for satellite
# stereo point clouds (https://github.com/NUAAXQ/
# awesome-point-cloud-analysis-2020). We will incorporate the proven Coherent
# Point Drift algorithm (Myronenko and Song 2010; Khallaghi et al. 2020) and
# research the latest algorithms for other promising options (e.g. PCRNet,
# Sarode et al. 2019). Input point clouds often have spatially variable density,
# which can create issues for automated co-registration. In addition, when one
# or both input point clouds are very large, a subsample must be extracted to
# ensure optimal performance during alignment. The pc align tool currently uses
# random sampling algorithms with default sample sizes that can be adjusted by
# the user if desired. However, there are improved algorithms in other point
# cloud libraries (e.g., uniform sampling in the Point Clound Library, Rusu and
# Cousins 2011) that offer more intelligent, spatially uniform sampling
# approaches, and we will integrate these approaches to improve co-registration
# performance. We will also improve current approaches to detect input point
# cloud density, and automatically select optimum processing options. 

# TODO(oalexan1): Must add alignment algorithms to pc_align.

# Task 4d: Bundle Adjustment Improvements ASP has a powerful set of tools for
# performing bundle adjustment to collectively optimize position and pose
# parameters for large sets of input images acquired by the same camera or
# completely independent cameras. The current functionality, however, lacks
# customization abilities that would allow the user to impose additional
# constraints for the optimization, like more common stereo “rig” configurations
# with fixed baseline as on recent Mars rovers. For example, each of the 19
# Planet Skysat instruments has 3 detectors in the focal plane, rigidly
# separated by a few cm (e.g., d’Angelo et al. 2016; Bhushan et al. 2021; Aati
# and Avouac 2020), but the current ASP bundle adjustment routines treat them as
# independent cameras, and they can end up kilometers apart for more challenging
# images. We will overhaul ASP’s bundle adjustment software to allow users to
# flexibly specify combinations of linked and free cameras while also choosing
# which combination of camera parameters to optimize. Most input images have
# approximate estimates for camera position in linear units of meters (e.g.,
# differential GPS position error at time of acquisition). The current bundle
# adjustment routines allow for user-defined weights to be specified, but we
# will implement improved user-facing uncertainty specification in common units
# (meters, degrees/radians for orientation error) Finally, we will add support
# for basic constraints using modern reference DEM (e.g., SRTM, MOLA) and
# refined satellite orbital data to prevent camera positions from moving to
# physically impossible locations based solely on image feature matches and
# weights. Taken together, these improvements will offer a much more robust
# optimization of camera parameters needed to produce output point clouds and
# DEMs with the highest possible geolocation accuracy. While the nonlinear
# bundle adjustment process is relatively complex, we will automate as much as
# possible, so the user will be presented with a limited set of well-documented
# options for high-level control. Beyer et al. 11 Improving and Sustaining ASP

Task 4d: A rig calibrator program was implemented. It was thoroughly validated
with a 3-camera rig for a robot (Astrobee, including a modeling depth sensor),
a satellite 3-camera rig (Planet SkySat), and a VIPER rover prototype (stereo
rig). 

A separate ASP tool for solving for jitter also implements rig constraint (for a mix of linescan and frame cameras). The satellite simulator program in ASP supports creating images with a rig constraint.

Added to the bundle adjustment program the abiliity to divide the cameras
into groups, by sensor, and during optimization, share the intrinsics of the
cameras in the same group. For each group can specify which intrinsics to share
or keep fixed.

Added the ability to constrain the cameras to a reference DEM during bundle adjustment with a user-specified uncertainty, measured in meters.

Documentation:

https://stereopipeline.readthedocs.io/en/latest/tools/rig_calibrator.html
https://stereopipeline.readthedocs.io/en/latest/tools/jitter_solve.html
https://stereopipeline.readthedocs.io/en/latest/tools/sat_sim.html#sat-sim-rig
https://stereopipeline.readthedocs.io/en/latest/bundle_adjustment.html#refining-the-intrinsics-per-sensor
https://stereopipeline.readthedocs.io/en/latest/bundle_adjustment.html#using-the-heights-from-a-reference-dem

# Task 4e: Correlation Improvements from Image Texture As users continue to
# apply ASP to different satellite data sources, they are increasingly
# interested in more automated measures of stereo correlation performance across
# different site conditions. This is particularly important for images acquired
# over surfaces with uniform texture such as sand/dust fields and fresh snow,
# which are notoriously difficult or impossible to correlate. We will research
# existing algorithms to analyze input image texture and also investigate
# methods for ASP to quantify and report correlation performance for each pixel
# of the resulting disparity maps. Examples of comparable metrics include image
# texture/roughness and signal to noise ratio in COSI-CORR (Leprince et al.
# 2007), validity flag in Medicis (Cournet et al. 2016), or correlation
# confidence in MicMac (Rosu et al. 2015). This work is distinct from error
# estimation and will take place entirely in the image domain, though these
# metrics could also be exported as point cloud attributes in task 4b for
# subsequent filtering and processing steps. 

Task 4e: Added a metric to measure the reliability of stereo correlation.
The metric is the discrepancy between left-to-right and right-to-left correlation,
which grows with correlation uncertainty. Option name is "--save-left-right-disparity-difference", and is documented in 
https://stereopipeline.readthedocs.io/en/latest/stereodefault.html

# Task 4f: Multi-band Improvements The ASP support for multi-band images is
# currently limited to a maximum of 6 bands. This task will remove this
# limitation so that multispectral images with more bands can be processed using
# core ASP utilities. Multi-band support will be extended to algorithms for
# orthorectification, image mosaicking and raster calculations, streamlining the
# pre-processing pipeline for several datasets (e.g., Maxar/DigitalGlobe
# WorldView-2/3). In a related effort, we will also implement ASP support for
# radiometric correction steps during multi-threaded orthorectification
# (mapproject utility) using image metadata (e.g., gain, offset).

Increased the number of bands in ASP from 6 to 12. This is enough to handle
multi-spectral images. 

ASP can now produce stereo terrain from any band (channel) of a multi-spectral images.
The user can specify the band to use with the --band option.

# TODO(oalexan1): Check how this works out with mapprojection.

# TODO(oalexan1): Implement gain and offset in mapprojection?

# TODO(oalexan1): Explain how later the individual bands can be stacked.
# It is not practical to have dem_mosaic natively support multiple bands,
# as this would result in a large code change and for the moment this feature
# is not something users ask for. The ability to invoke the tool multiple
# times with separate bands, and then stack the bands accomplishes the same
# goal, with a little more user effort.

# Task 4g: Adaptive Kernel Improvements When ASP performs stereo correlation it
# uses a fixed kernel size to map disparity across the entire intersecting
# dimension between the two images. This strategy works well when the image
# texture and surface roughness/relief is more or less homogeneous for the
# entire domain (e.g., ice sheets, flat plains in Mars). However, for many
# situations, large images might cover a diverse landscape in one scene (e.g.,
# glaciers, exposed bedrock, shallow river valleys, dense conifer forests), with
# varying surface texture and roughness. In such cases, a fixed correlator
# kernel size is able to resolve features for certain landscapes, but can fail
# for adjacent landscapes with different surface properties. An adaptive
# correlation kernel, with variable size and shape based on input texture and
# known surface roughness/relief from existing reference DEMs, would provide
# improved output products for the entire scene, with fewer data gaps and
# blunders/noise. An existing implementation of this adaptive kernel
# functionality is available from the NASA JPL autoRIFT software (e.g., Gardner
# et al. 2018, 2021) which can perform correlation using multiple kernel sizes
# to produce an improved merged final output product. We will research this and
# related algorithms to implement an enhanced, efficient and multi-threaded
# adaptive correlation kernel approach in ASP to improve performance across all
# supported input image sources and landscapes.

Task 4g: Added the Multi-Scale-Multi-Window (MSMW) algorithm to ASP. It implements
an adaptive multi-scale correlation kernel. The algorithm is documented at:
https://stereopipeline.readthedocs.io/en/latest/stereo_algorithms.html#msmw

Task 5: Management

Ross fills this in.


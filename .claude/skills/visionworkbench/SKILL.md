---
name: visionworkbench
description: >-
  Map of the VisionWorkbench (VW) repo that ASP is built on: its libraries,
  its command-line tools, and where the camera, lens distortion, stereo, and
  cartography logic lives. Load when working in ~/projects/visionworkbench,
  when an ASP feature actually lives in VW (lens distortion models, pinhole
  camera read/write, interest points, image correlation, georeferencing), or
  when a tool you expect in ASP (convert_pinhole_model, undistort_image,
  ipfind, ipmatch) is missing from the ASP source because it is a VW tool.
---

# VisionWorkbench (VW)

VW is the C++ library layer under ASP. ASP links against VW's INSTALLED libs
(libVw*.dylib / .so), so a VW source change needs a VW build and install
BEFORE the ASP build sees it. Repo at `~/projects/visionworkbench`, installs
into its own `install/`. Build mechanics are in the [[mac-asp-build]] skill
(check VW first, build and install it, then ASP).

Key lesson that keeps biting: several things that feel like ASP features are
actually in VW. If you grep only the ASP repo for them you will not find them.
When a camera model, lens distortion, interest-point, correlation, or
georeferencing detail is missing from ASP, look in VW next.

## Library map (src/vw/)

- Core: logging (vw_out, vw_log), exceptions (vw_throw, ArgumentErr, IOErr),
  threads, stopwatch, settings. Foundation everything else uses.
- Math: Vector, Matrix, quaternions, LevenbergMarquardt, NewtonRaphson,
  least squares, geometry solvers. (There is no Math/LeastSquares.h.)
- Image: image views, pixel types, filtering, interpolation, edge extension,
  the lazy-evaluation image-view framework.
- Camera: camera models and the lens distortion library. PinholeModel,
  OpticalBarModel, CAHV family, and LensDistortion.{h,cc} with all the
  distortion classes (Null, Tsai, FOV, Fisheye, Fisheye equisolid, Fisheye
  stereographic, BrownConrady, AdjustableTsai, Photometrix, RPC). The
  pinhole .tsai read/write and the distortion-name dispatch
  (construct_lens_distortion) are in PinholeModel.cc. CameraUtilities.cc has
  fitPinholeModel and the generic create_approx_pinhole_model template that
  converts one camera to an approximating distortion model.
- Stereo: the correlation core. CostFunctions.h (ABSOLUTE_DIFFERENCE,
  SQUARED_DIFFERENCE, CROSS_CORRELATION, CENSUS_TRANSFORM,
  TERNARY_CENSUS_TRANSFORM), PreFilter.h (none, subtracted mean, Laplacian of
  Gaussian), correlation and subpixel views, SGM/MGM support.
- Cartography: GeoReference, datums, map projections (via PROJ/GDAL), the
  georeferenced image read/write (block_write_gdal_image lives here).
- FileIO: GDAL and other raster IO.
- InterestPoint: detectors and descriptors, matching (ipfind/ipmatch guts).
- Mosaic: image mosaicking, quadtree generation (image2qtree).
- Geometry: shapefiles, convex hulls, 3D transforms.
- BundleAdjustment: VW's own bundle-adjustment classes (ASP's bundle_adjust
  tool is in ASP, but reuses VW machinery).

## Tools (src/vw/tools/)

These build to `install/bin` and are easy to mistake for ASP tools:

- convert_pinhole_model: convert a pinhole or optical-bar camera to a chosen
  output distortion type (TsaiLensDistortion, BrownConradyDistortion, RPC)
  via fitPinholeModel. This is a VW tool, NOT in the ASP Tools dir, even
  though it is documented in the ASP manual.
- undistort_image: remove lens distortion from an image.
- ipfind, ipmatch: interest-point detection and matching.
- correlate: stereo correlation front-end. gpu_correlate: an experimental GPU
  correlation tool (VW has no production GPU path, this is the only GPU code).
- colormap, hillshade: raster colorization and hillshading (the engines behind
  the ASP/stereo_gui equivalents).
- image2qtree: build a quadtree image pyramid.
- print_exif, georef_query, detect_water, clean_sentinel1_borders.

## Adding or editing a lens distortion model (worked path)

1. Declare the class in src/vw/Camera/LensDistortion.h, mirroring an existing
   one (FisheyeLensDistortion is the template for angle-based fisheye models).
   Give it a unique class_name() string.
2. Implement in LensDistortion.cc: a closed-form forward normalized function,
   distorted_coordinates (fast, used by bundle adjustment per residual, so no
   solver here), undistorted_coordinates via NewtonRaphson, plus
   copy/write/read/scale/init_distortion_param_names.
3. Register the name in PinholeModel.cc construct_lens_distortion. The lookup
   is a case-insensitive SUBSTRING match, so a name that contains another
   name as a substring (FISHEYE_EQUISOLID contains FISHEYE) must be checked
   BEFORE the shorter one.
4. Optionally add it as an output type in fitPinholeModel (CameraUtilities.cc)
   if convert_pinhole_model should fit to it. Reading it in works for free
   once step 3 is done.
5. Build and install VW, then ASP. Test with the ASP cam_test tool: pass the
   same camera as --cam1 and --cam2 to measure the pixel to ground to pixel
   round trip (distort/undistort inverse consistency). A test lives at
   StereoPipelineTest/ss_cam_test_fisheye_proj.
6. Document in docs/pinholemodels.rst (model section plus a sample block with
   a numref label) and add a NEWS.rst bullet.

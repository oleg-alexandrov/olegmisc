# FY25 CSM plan by Jay.

# Task 2. Development tasks:

# i. Ongoing ASP maintenance and release including nightly builds and releases, conda releases, and updated to dependencies as ISIS dependencies update.

# ii. Enhancement to ASP to be able to use streaming point clouds in pc_align.

# iii. Enhancement to ASP with the inclusion of a lunar geoid to support geoid correction of ASP derived lunar DTMs.

# iv. Updates to the ISIS dependencies to support native ARM builds.

# v. Enhancement to ASP adding data snooping and reliability metrics to the bundle adjustment code.

# vi. Paper reviews for manuscripts for which NASA/SETI personnel are co-authors.

# vii. Stretch Goal: Improved documentation around the data snooping and reliability metrics added to ASP.

# viii. Stretch Goal: Updates to ASP, that depend on updates to ISIS, to support native ARM builds.

# ix. Stretch Goal: Enhancements to ASP integrating community proposed improvements to pc_align to support more robust results statistics.

# CSM work

# June, July, 2024

#  Made the ASP bundle_adjust program load and save the cameras in parallel
#  bundle adjustment. This sped up loading by a factor of 4.
 
#  Produced a new calibration for Kaguya Terrain Camera (wide and narrow images)
#  that do not change the focal length. Validated that it does just as well as
#  the previous calibration that did change the focal length.
 
#  Added to the ASP documentation an example of solving for jitter with Kaguya images.
#  This was shown to greatly reduce the disagreement with LOLA in some situations.
  
#  Worked with USGS staff at outlier rejection for their bundle adjustment solution
#  with a sparse solver.
   
#  Worked on evaluation of the CTX stereo DEMs to MOLA. 
  
#  Worke on handling user bug reports and maintanence for the Ames Stereo Pipeline.
  
#  Made a new Stereo Pipeline release incorporating the latest ISIS and many
#  improvements in ASP itself.
  
#  Made a thorough study of CTX distortion and jitter, and comparison with
#  HiRISE. It appears that the CTX lens distortion is good, though there still
#  small scale factor difference between these two sensors, which could be an
#  issue with either one.

#  Showed that the CSM linescan model can produce good results for Earth KH-7
#  declassified satellite images.
  
#  Much maintanance work in ASP
         
# CSM work in December 2023

# Replaced the ray-to-DEM intersection code in ISIS with a faster and more robust implementation based on a different algorithm that can handle very oblique rays, such as for a rover on the ground.

# Solved for distortion for Kaguya Terrain Camera with a narrower image width (3496 pixels).
# This required collecting new data, redoing the work and also the methdology, compared to what was used for the wider image width (4096 pixels).

# Added to ASP support for PDAL. This is essential for ingestion of LAS 1.4 (latest version) point clouds (the obsolete libLAS library that was used before could not handle this version). 

# Added to CSM cameras support for radial + tangential distortion. Essential for future detailed modeling of frame + linescan sensor designs.

# Added to ASP support for the ISIS binary control network model, which is more compact than the match files used by ASP and allows for exchange of control networks between ASP and ISIS and mixed use of tools. Supported in bundle adjustment,
# jitter solving, and stereo_gui.

# Added to ASP the ability to read and write CSM state embedded in .cub files, in addition to the existing support for external .json files. 

# Fixed several problems in generation of CSM cameras for MSL Curiosity Nav
# and Mast images. Added a large example of how to create terrain models and mosaics
# from these images using ASP.
     
# Made the operation of projecting into the implementation of the ASP ISIS
# linescan camera 2.2-2.6 times faster by using the secant method to find the
# best sensor line. This is along the lines of what is already done for CSM cameras.

# Helped with understanding issues with large-scale aligment of Kaguya data.
         
# Here is what I sent to Ross in April 2024:
 
#  Added a PushFrame sensor to CSM, along the lines of the existing Linescan sensor, to support the WAC camera. Custom code was needed. Jointly developed and tested by USGS and Ames staff.

#    Replaced the CSM Linescan ground-to-image algorithm with a faster and more reliable implementation which fixed some cases when it was failing.
#    Much testing and refinements for the CSM SAR sensor.

#    Added a standalone program that is shipped with the USGSCSM library which can interface with it and expose some of its functionality to the user.

#    Worked with USGS staff to fix and validate logic to ingest Mars MSL ground-level images to be used with the CSM Frame sensor. 

#    Added to ASP the ability to use the CSM Linescan sensor for Earth satellites, namely for Maxar Worldview and Airbus Pleiades satellites.

#    Added to ASP a jitter solver for CSM Linescan cameras. Validated and documented its use with CTX Mars cameras and WorldView and Pleiades Earth cameras.

#    Implemented uncertainty propagation for Linescan CSM sensors at each pixel. It starts with the uncertainties in satellite positions and orientations, propagates them to the ground, through the triangulation operation, and creates raster horizontal and vertical uncertainty images in one-to-one correspondences with a produced DEM. Currently tied to the Maxar WorldView CSM sensor, which is the only one for which input uncertainties exist, but can be easily extended to any Linescan CSM sensor.

# Updates to the ASP point2las tool allow improved export of point clouds to the LAS file format.

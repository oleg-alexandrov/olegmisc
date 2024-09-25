MPR

// See Ernie's Task 206 on OneDrive.

Old notes:

VIPER
-Added support for the fisheye lens distortion model, and validated that the Ames Stereo Pipleine software bundle adjustment and rig calibraton solvers work with this model.
-Added the abilitytto import and export NVM files. These are a rather well-known format for structure for motion.
-Added the ability to mix images that have an slog filter applied (for effeciency of transmission) with images that are raw. This will be useful for upcoming rover missions.
-Made a through study a set of orbital 9 sensors, with 3000 images in total using bundle adjustment. 
# Past MPR
Performed experiments to show accurate modeling of a large system of cameras using bundle adjustment. 
Improved the software support for calibration of a rig and for the OpenCV ubiquitous lens distortion model. Added support for the rig calibration software to export its outputs in a format usable by bundle adjustment.
Developed a process for creating terrain models from VIPER data acquired with
the forward and aft camera in the sandbox. Validated with ground truth.
This required additional functionality for the righ calibration
and bundle adjustment programs. 


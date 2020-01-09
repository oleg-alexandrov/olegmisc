// __BEGIN_LICENSE__
//  Copyright (c) 2009-2013, United States Government as represented by the
//  Administrator of the National Aeronautics and Space Administration. All
//  rights reserved.
//
//  The NGT platform is licensed under the Apache License, Version 2.0 (the
//  "License"); you may not use this file except in compliance with the
//  License. You may obtain a copy of the License at
//  http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
// __END_LICENSE__

/// \file mapproject.cc
///
/// This program will project a camera image onto a DEM using the
/// camera model.

#include <vw/Core.h>
#include <vw/FileIO.h>
#include <vw/Image.h>
#include <vw/Cartography.h>
using namespace vw;
using namespace vw::cartography;
using namespace std;

#include <asp/Core/Macros.h>
#include <asp/Core/Common.h>
#include <asp/Sessions/DG/StereoSessionDG.h>
#include <asp/Sessions/DG/XML.h>
namespace po = boost::program_options;
namespace fs = boost::filesystem;

#include "ogr_spatialref.h"

typedef PixelMask<float> PMaskT;

struct Options : asp::BaseOptions {
  // Input
  std::string dem_file, image_file, camera_model_file, output_file, stereo_session;
  bool isQuery;

  // Settings
  std::string target_srs_string;
  double nodata_value, target_resolution, mpp, ppd;
  BBox2 target_projwin, target_pixelwin;
};

void handle_arguments( int argc, char *argv[], Options& opt ) {
  po::options_description general_options("");
  double NaN = std::numeric_limits<double>::quiet_NaN();
  general_options.add_options()
    ("nodata-value",     po::value(&opt.nodata_value)->default_value(-32768),
     "No-data value to use unless specified in the input image.")
    ("t_srs",            po::value(&opt.target_srs_string)->default_value(""),
     "Target spatial reference set. This mimics the GDAL option. If not provided use the one from the DEM.")
    ("tr",               po::value(&opt.target_resolution)->default_value(NaN),
     "Set the output file resolution in target georeferenced units per pixel.")
    ("mpp",              po::value(&opt.mpp)->default_value(NaN),
     "Set the output file resolution in meters per pixel.")
    ("ppd",              po::value(&opt.ppd)->default_value(NaN),
     "Set the output file resolution in pixels per degree.")
    ("query-projection", po::bool_switch(&opt.isQuery)->default_value(false),
      "Just display the computed projection information without actually doing the projection.")
    ("session-type,t",   po::value(&opt.stereo_session)->default_value(""),
     "Select the stereo session type to use for processing. Choose 'rpc' if it is desired to later do stereo with the 'dg' session. [options: pinhole isis rpc]")
    ("t_projwin",        po::value(&opt.target_projwin),
     "Selects a subwindow from the source image for copying, with the corners given in georeferenced coordinates (xmin ymin xmax ymax). Max is exclusive.")
    ("t_pixelwin",       po::value(&opt.target_pixelwin),
      "Selects a subwindow from the source image for copying, with the corners given in georeferenced pixel coordinates (xmin ymin xmax ymax). Max is exclusive.");

  general_options.add( asp::BaseOptionsDescription(opt) );

  po::options_description positional("");
  positional.add_options()
    ("dem",          po::value(&opt.dem_file))
    ("camera-image", po::value(&opt.image_file))
    ("camera-model", po::value(&opt.camera_model_file))
    ("output-file" , po::value(&opt.output_file));

  po::positional_options_description positional_desc;
  positional_desc.add("dem",         1);
  positional_desc.add("camera-image",1);
  positional_desc.add("camera-model",1);
  positional_desc.add("output-file", 1);

  std::string usage("[options] <dem> <camera-image> <camera-model> <output>");
  po::variables_map vm =
    asp::check_command_line( argc, argv, opt, general_options, general_options,
                             positional, positional_desc, usage );

  if ( !vm.count("dem") || !vm.count("camera-image") || !vm.count("camera-model") )
    vw_throw( ArgumentErr() << usage << general_options );

  // We support map-projecting using the DG camera model, however, these images
  // cannot be used later to do stereo, as that process expects the images
  // to be map-projected using the RPC model. 
  if (boost::to_lower_copy(opt.stereo_session) == "dg"){
    vw_out(WarningMessage) << "Images map-projected using the 'dg' camera model cannot be used later to run stereo with the 'dg' session. If that is desired, please specify here the 'rpc' camera model instead.\n";
  }

  if ( boost::iends_with(boost::to_lower_copy(opt.camera_model_file), ".xml") &&
       opt.stereo_session == "" ){
    opt.stereo_session = "rpc";
  }
  
}

template <class ImageT>
void write_parallel_cond( std::string              const& filename,
                          ImageViewBase<ImageT>    const& image,
                          GeoReference             const& georef,
                          bool has_nodata, double nodata_val,
                          Options                  const& opt,
                          TerminalProgressCallback const& tpc ) {

  // Save the session type. Later in stereo we will check that we use
  // only images written by mapproject with the -t rpc session.
  std::map<std::string, std::string> keywords;
  keywords["CAMERA_MODEL_TYPE" ] = opt.stereo_session;

  // ISIS is not thread safe so we must switch out base on what the session is.

  vw_out() << "Writing: " << filename << "\n";
  if (has_nodata){
    if ( opt.stereo_session == "isis" ) {
      asp::write_gdal_georeferenced_image(filename, image.impl(), georef,
                                          nodata_val, opt, tpc, keywords);
    } else {
      asp::block_write_gdal_image(filename, image.impl(), georef,
                                  nodata_val, opt, tpc, keywords);
    }
  }else{ // Does not have nodata
    if ( opt.stereo_session == "isis" ) {
      asp::write_gdal_georeferenced_image(filename, image.impl(), georef,
                                          opt, tpc, keywords);
    } else {
      asp::block_write_gdal_image(filename, image.impl(), georef,
                                  opt, tpc, keywords);
    }
  }

}

/// Compute output georeference to use
void calc_target_geom(// Inputs
                      bool first_pass,
                      bool calc_target_res,
                      Vector2i const& image_size,
                      boost::shared_ptr<camera::CameraModel> const& camera_model,
                      ImageViewRef<PMaskT> const& dem,
                      GeoReference dem_georef, // make copy on purpose
                      // Outputs
                      Options & opt, BBox2 & cam_box, GeoReference & target_georef
                      ){

  // Find the camera bbox and the target resolution unless user-supplied.
  // - This call returns the bounding box of the camera view on the ground.
  // - The bounding box is in units defined by dem_georef and might not be meters.
  // - auto_res is an estimate of the ground resolution visible by the camera.
  //   This is in a unit defined by dem_georef and also might not be meters.
  float auto_res;
  cam_box = camera_bbox(dem, dem_georef, camera_model,
                        image_size.x(), image_size.y(), auto_res);

  if (first_pass){
    // Convert bounding box from dem_georef coordinate system to
    //  target_georef coordinate system
    cam_box = target_georef.lonlat_to_point_bbox
      (dem_georef.point_to_lonlat_bbox(cam_box));

  }

  // Use auto-calculated ground resolution if that option was selected
  if (calc_target_res) opt.target_resolution = auto_res;

  // If an image bounding box (projected coordinates) was passed in,
  //  override the camera's view on the ground with the custom box.
  // - The user needs to know the georeference projected coordinate system (using the query command) to do this
  if ( opt.target_projwin != BBox2() ) {
    cam_box = opt.target_projwin;
    if ( cam_box.min().y() > cam_box.max().y() )
      std::swap( cam_box.min().y(), cam_box.max().y() );
    cam_box.max().x() -= opt.target_resolution; //TODO: What are these adjustments?
    cam_box.min().y() += opt.target_resolution;
  }

  // In principle the corners of the projection box can be
  // arbitrary.  However, we will force them to be at integer
  // multiples of pixel dimensions. This is needed if we want to do
  // tiling, that is break the DEM into tiles, project on individual
  // tiles, and then combine the tiles nicely without seams into a
  // single projected image. The tiling solution provides a nice
  // speedup when dealing with ISIS images, when projection runs
  // only with one thread.
  double s = opt.target_resolution;
  int min_x         = (int)round(cam_box.min().x() / s);
  int min_y         = (int)round(cam_box.min().y() / s);
  int output_width  = (int)round(cam_box.width()   / s);
  int output_height = (int)round(cam_box.height()  / s);
  cam_box = s * BBox2(min_x, min_y, output_width, output_height);

  // Transform is from DEM georef coords to target_georef (probably projected) coordinates
  Matrix3x3 T = target_georef.transform();
  // This polarity checking is to make sure the output has been
  // transposed after going through reprojection. Normally this is
  // the case. Yet with grid data from GMT, it is not.
  if ( T(0,0) < 0 ) // If X coefficient of affine transform is negative
    T(0,2) = cam_box.max().x();  // Add in an X offset such that all transformed pixels will be positive in X
  else
    T(0,2) = cam_box.min().x(); // ????
  T(0,0) =  opt.target_resolution;  // meters to pixels???
  T(1,1) = -opt.target_resolution;  // meters to pixels with image Y axis flip?
  T(1,2) = cam_box.max().y();       // Add in an Y offset such that all transformed pixels will be positive in Y
  if ( target_georef.pixel_interpretation() ==
       GeoReference::PixelAsArea ) { // Meaning point [0][0] equals location (0.5, 0.5)
    T(0,2) -= 0.5 * opt.target_resolution; //
    T(1,2) += 0.5 * opt.target_resolution;
  }
  target_georef.set_transform( T );

  return;
}

int main( int argc, char* argv[] ) {

  if (argc < 3){
    std::cerr << "Usage: " << argv[0] << " input.cub output.txt" << std::endl;
    exit(0);
  }
  std::string input_cub ( argv[1] );
  std::string output_txt( argv[2] );

  std::cout << "doing " << input_cub << ' ' << output_txt << std::endl;

  std::string sessionStr;
  Options opt;
  typedef boost::scoped_ptr<asp::StereoSession> SessionPtr;
  SessionPtr session( asp::StereoSession::create(sessionStr, // in-out
                                                 opt,
                                                 input_cub, input_cub,
                                                 input_cub, input_cub,
                                                 "", ""));

  std::cout << "session is " << sessionStr << std::endl;

  // Initialize a camera model
  boost::shared_ptr<camera::CameraModel> camera_model =
    session->camera_model(input_cub, input_cub);
  
  DiskImageView<float> cub(input_cub);

  ofstream out(output_txt.c_str());
  out.precision(18);
  out << "# pixel_col pixel_row pixel_to_vec"  << std::endl;
  std::cout << "writing: " << output_txt << std::endl;
  for (int col = 0; col < cub.cols(); col++){
    for (int row = 0; row < cub.rows(); row++){
      Vector3 v = camera_model->pixel_to_vector(Vector2(col, row));
      out << ' ' << col << ' ' << row << ' ' << v[0] << ' ' << v[1] << ' ' << v[2] << std::endl;
    }
  }
  out.close();
  
  return 0;  
}

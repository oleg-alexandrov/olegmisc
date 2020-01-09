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

  if (argc < 4){
    std::cerr << "Usage: " << argv[0] << " input.cub inputDEM.tif output.cub" << std::endl;
    exit(0);
  }
  std::string input_cub( argv[1] );
  std::string input_dem( argv[2] );
  std::string output_cub( argv[3] );

  std::cout << "doing " << input_cub << ' ' << input_dem << ' ' << output_cub << std::endl;

  DiskImageResourceGDAL in_rsrc(input_dem);
  float nodata_val = -32768;
  if ( in_rsrc.has_nodata_read() ) {
    nodata_val = in_rsrc.nodata_read();
    vw_out() << "\tFound input nodata value: " << nodata_val << std::endl;
  }else{
    std::cerr << "Nodata value not found in: " << input_dem << std::endl;
    exit(1);
  }
  DiskImageView<float> dem(in_rsrc);
  GeoReference georef;
  read_georeference(georef, in_rsrc);

  std::string sessionStr;
  Options opt;
  typedef boost::scoped_ptr<asp::StereoSession> SessionPtr;
  SessionPtr session( asp::StereoSession::create(sessionStr, // in-out
                                                 opt,
                                                 input_cub, input_cub,
                                                 input_cub, input_cub,
                                                 output_cub,
                                                 input_dem
                                                 ) );

  std::cout << "session is " << sessionStr << std::endl;

  // Initialize a camera model
  boost::shared_ptr<camera::CameraModel> camera_model =
    session->camera_model(input_cub, input_cub);
  
  BBox2i cube_box;
  int spacing = 1;
  for (int col = 0; col < dem.cols(); col += spacing ){
    for (int row = 0; row < dem.rows(); row += spacing ){
      if (dem(col, row) == nodata_val) continue;
      Vector2 lonlat = georef.pixel_to_lonlat(Vector2(col, row));
      Vector3 llh(lonlat[0], lonlat[1], dem(col, row));
      Vector3 xyz = georef.datum().geodetic_to_cartesian(llh);
      //std::cout << "pix llh xyz " << col << ' ' << row << ' ' << llh << ' ' << xyz << std::endl;
      Vector2 pix = camera_model->point_to_pixel(xyz);
      //std::cout << "camera pix is " << pix << std::endl;
      cube_box.grow(pix);
    }
  }

  int len = std::max(cube_box.width(), cube_box.height());
  cube_box.expand(0.2*len + 100);
  DiskImageView<float> cub(input_cub);
  cube_box.crop(bounding_box(cub));
  cube_box += Vector2(1, 1); // isis starts from 1
  
  std::cout << "the cube box is " << cube_box << std::endl;
  std::ostringstream os; os << "crop from=" << input_cub << " to=" << output_cub << " sample=" << cube_box.min().x() << " line=" << cube_box.min().y() << " nsamples=" << cube_box.width() << " nlines=" << cube_box.height();
  std::cout << os.str() << std::endl;
  system(os.str().c_str());

#if 0
  if (session->name() == "isis" && opt.output_file.empty() ){
    // The user did not provide an output file. Then the camera
    // information is contained within the image file and what is in
    // the camera file is actually the output file.
    opt.output_file = opt.camera_model_file;
    opt.camera_model_file.clear();
  }
  if ( opt.output_file.empty() )
    vw_throw( ArgumentErr() << "Missing output filename.\n" );
  
  
  std::cout << "camera is " << camera_model.get() << std::endl;
  
  std::cout << "dem is " << opt.dem_file << std::endl;

#endif  
  
  return 0;
#if 0  
  Options opt;
  try {
    handle_arguments( argc, argv, opt );

    // We create a stereo session where both of the cameras and images
    // are the same, because we want to take advantage of the stereo
    // pipeline's ability to generate camera models for various
    // missions.  Hence, we create two identical camera models, but
    // only one is used.
    typedef boost::scoped_ptr<asp::StereoSession> SessionPtr;
    SessionPtr session( asp::StereoSession::create(opt.stereo_session, // in-out
                                                   opt,
                                                   opt.image_file, opt.image_file,
                                                   opt.camera_model_file,
                                                   opt.camera_model_file,
                                                   opt.output_file,
                                                   opt.dem_file
                                                   ) );

    if (session->name() == "isis" && opt.output_file.empty() ){
      // The user did not provide an output file. Then the camera
      // information is contained within the image file and what is in
      // the camera file is actually the output file.
      opt.output_file = opt.camera_model_file;
      opt.camera_model_file.clear();
    }
    if ( opt.output_file.empty() )
      vw_throw( ArgumentErr() << "Missing output filename.\n" );

    // Initialize a camera model
    boost::shared_ptr<camera::CameraModel> camera_model =
      session->camera_model(opt.image_file, opt.camera_model_file);

    std::cout << "camera is " << camera_model.get() << std::endl;

    std::cout << "dem is " << opt.dem_file << std::endl;

    DiskImageResourceGDAL dem_rsrc(opt.dem_file);
    DiskImageView<float> dem(dem_rsrc);
    GeoReference dem_georef;
    read_georeference(dem_georef, dem_rsrc);
    
    
    // Safety check that the users are not trying to map project map projected images.
    {
      GeoReference dummy_georef;
      VW_ASSERT( !read_georeference( dummy_georef, opt.image_file ),
                 ArgumentErr() << "Your input camera image is already map "
                 << "projected. The expected input is required to be "
                 << "unprojected or raw camera imagery." );
    }

    // Load the DEM
    GeoReference dem_georef;
    ImageViewRef<PMaskT> dem;
    boost::shared_ptr<DiskImageResource> dem_rsrc;

    if ( fs::path(opt.dem_file).extension() == "" ) {
      // This option allows the user to just project directly unto to
      // the datum. This seems like a stop gap. This should be
      // addressed after a rewrite of this tool.
      //
      // Valid values are well known Datums like D_MOON D_MARS WGS84 and so on.
      Vector3 llr_camera_loc =
        XYZtoLonLatRadFunctor::apply
        ( camera_model->camera_center(Vector2()) );
      if ( llr_camera_loc[0] < 0 ) llr_camera_loc[0] += 360;

      dem_georef =
        GeoReference(Datum(opt.dem_file),
                     Matrix3x3(1, 0,
                               (llr_camera_loc[0] < 90 ||
                                llr_camera_loc[0] > 270) ? -180 : 0,
                               0, -1, 90, 0, 0, 1) );
      dem = constant_view(PMaskT(0), 360, 180 );
      vw_out() << "\t--> Using flat datum \"" << opt.dem_file
               << "\" as elevation model.\n";
    }else{ // User has provided a DEM to use.
      bool has_georef = read_georeference(dem_georef, opt.dem_file);
      if (!has_georef)
        vw_throw( ArgumentErr() << "There is no georeference information in: "
                  << opt.dem_file << ".\n" );

      dem_rsrc = boost::shared_ptr<DiskImageResource>
        ( DiskImageResource::open( opt.dem_file ) );

      // If we have a nodata value, create a mask.
      DiskImageView<float> dem_disk_image(dem_rsrc);
      if (dem_rsrc->has_nodata_read()){
        dem = create_mask(dem_disk_image, dem_rsrc->nodata_read());
      }else{
        dem = pixel_cast<PMaskT>(dem_disk_image);
      }
    }

    // Find the target resolution based on mpp or ppd if provided. Do
    // the math to convert pixel-per-degree to meter-per-pixel and vice-versa.
    int sum = (!std::isnan(opt.target_resolution)) + (!std::isnan(opt.mpp))
      + (!std::isnan(opt.ppd));
    if (sum >= 2){
      vw_throw( ArgumentErr() << "Must specify at most one of the options: "
                << "--tr, --mpp, --ppd.\n" );
    }
    double radius = dem_georef.datum().semi_major_axis();
    if ( !std::isnan(opt.mpp) ){ // Meters per pixel was set
      opt.ppd = 2.0*M_PI*radius/(360.0*opt.mpp);
    }else if ( !std::isnan(opt.ppd) ){ // Pixels per degree was set
      opt.mpp = 2.0*M_PI*radius/(360.0*opt.ppd);
    }
    if ( !std::isnan(opt.ppd) ) { // pixels per degree now available
      if (dem_georef.is_projected()) {
        opt.target_resolution = opt.mpp; // Use units of meters
      } else { // Not projected, GDC coordinates only.
        opt.target_resolution = 1/opt.ppd; // Use units of degrees
                                           // Lat/lon degrees are different so we never want to do this!
      }
    }

    // Read projection. Work out output bounding box in points using
    // original camera model.
    GeoReference target_georef = dem_georef;
    if (opt.target_srs_string != ""){
      boost::replace_first(opt.target_srs_string,
                           "IAU2000:","DICT:IAU2000.wkt,");
      VW_OUT(DebugMessage,"asp") << "Asking GDAL to decipher: \""
                                 << opt.target_srs_string << "\"\n";
      OGRSpatialReference gdal_spatial_ref;
      if (gdal_spatial_ref.SetFromUserInput( opt.target_srs_string.c_str() ))
        vw_throw( ArgumentErr() << "Failed to parse: \"" << opt.target_srs_string
                  << "\"." );
      char *wkt = NULL;
      gdal_spatial_ref.exportToWkt( &wkt );
      std::string wkt_string(wkt);
      delete[] wkt;
      target_georef.set_wkt( wkt_string );
    }

    // We compute the target_georef and camera box in two passes,
    // first in the DEM coordinate system and we rotate it to target's
    // coordinate system (which makes it grow), and then we tighten it
    // in target's coordinate system.
    bool calc_target_res = std::isnan(opt.target_resolution);
    Vector2i image_size = asp::file_image_size( opt.image_file );
    BBox2 cam_box;
    // First pass
    bool first_pass = true;
    calc_target_geom(// Inputs
                     first_pass, calc_target_res, image_size, camera_model,
                     dem, dem_georef,
                     // Outputs
                     opt, cam_box, target_georef);
    // Second pass
    first_pass = false;
    ImageViewRef<PMaskT> trans_dem // Get transform from DEM georeference to output georeference
      = geo_transform(dem, dem_georef, target_georef,
                      ValueEdgeExtension<PMaskT>(PMaskT()),
                      BilinearInterpolation());
    calc_target_geom(// Inputs
                     first_pass, calc_target_res, image_size, camera_model,
                     trans_dem, target_georef,
                     // Outputs
                     opt, cam_box, target_georef);

    // Compute output image size in pixels using bounding box in output projected space
    BBox2i target_image_size = target_georef.point_to_pixel_bbox( cam_box );

    vw_out() << "Cropping to " << cam_box << " pt. " << std::endl;

    // Shrink output image BB if an output image BB was passed in
    GeoReference croppedGeoRef  = target_georef;
    BBox2i       croppedImageBB = target_image_size;
    if ( opt.target_pixelwin != BBox2() ) {
      // Replace with passed in bounding box
      croppedImageBB = opt.target_pixelwin;

      // Update output georeference to match the reduced image size
      croppedGeoRef = vw::cartography::crop(target_georef, croppedImageBB);
    }

    vw_out() << "Output georeference:\n"        << croppedGeoRef << std::endl;
    vw_out() << "Output image bounding box:\n";
    vw_out() << "(Origin: (" << croppedImageBB.min()[0] << ", " << croppedImageBB.min()[1] << ") width: "
             << croppedImageBB.width() << " height: " << croppedImageBB.height() << ")" << std::endl;

    if (opt.isQuery){ // Quit before we do any image work
      vw_out() << "Query finished, exiting mapproject tool.\n";
      return 0;
    }
    
    // Create handle to input image to be projected on to the map
    boost::shared_ptr<DiskImageResource>
      img_rsrc( DiskImageResource::open( opt.image_file ) );
    
    // Write the output image. Use the nodata passed in by the user
    // if it is not available in the input file.
    if (img_rsrc->has_nodata_read()) opt.nodata_value = img_rsrc->nodata_read();
    asp::create_out_dir(opt.output_file);
    bool has_img_nodata = true;
    PMaskT nodata_mask = PMaskT(); // invalid value for a PixelMask
    bool call_from_mapproject = true;
    write_parallel_cond
      ( // Write to the output file
       opt.output_file,
       crop( // Apply crop (only happens if --t_pixelwin was specified)
            apply_mask
            ( // Handle nodata
             transform_nodata( // Apply the output from Map2CamTrans
                              create_mask(DiskImageView<float>(img_rsrc),
                                          opt.nodata_value), // Handle nodata
                              Map2CamTrans
                              ( // Converts coordinates in DEM
                                // georeference to camera pixels
                               camera_model.get(), target_georef,
                               dem_georef, dem_rsrc, image_size,
                               call_from_mapproject
                               ),
                              target_image_size.width(),
                              target_image_size.height(),
                              ValueEdgeExtension<PMaskT>(nodata_mask),
                              BicubicInterpolation(), nodata_mask
                              ),
             opt.nodata_value
             ),
            croppedImageBB
            ),
       croppedGeoRef, has_img_nodata, opt.nodata_value, opt,
       TerminalProgressCallback("","")
       );
    
  } ASP_STANDARD_CATCHES;

#endif
  return 0;
}

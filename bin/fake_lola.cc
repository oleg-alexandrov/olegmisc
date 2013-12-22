// __BEGIN_LICENSE__
//  Copyright (c) 2006-2012, United States Government as represented by the
//  Administrator of the National Aeronautics and Space Administration. All
//  rights reserved.
//
//  The NASA Vision Workbench is licensed under the Apache License,
//  Version 2.0 (the "License"); you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
// __END_LICENSE__

#ifdef _MSC_VER
#pragma warning(disable:4244)
#pragma warning(disable:4267)
#pragma warning(disable:4996)
#endif

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cmath>
#include <boost/tokenizer.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/numeric/conversion/cast.hpp>
#include <boost/program_options.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/foreach.hpp>
namespace fs = boost::filesystem;
namespace po = boost::program_options;

#include <vw/Core/Functors.h>
#include <vw/Image/Algorithms.h>
#include <vw/Image/ImageMath.h>
#include <vw/Image/ImageViewRef.h>
#include <vw/Image/PerPixelViews.h>
#include <vw/Image/PixelMask.h>
#include <vw/Image/MaskViews.h>
#include <vw/Image/PixelTypes.h>
#include <vw/Image/Statistics.h>
#include <vw/FileIO/DiskImageView.h>
#include <vw/Cartography/GeoReference.h>
#include <vw/tools/Common.h>
#include <vw/FileIO/DiskImageResourceGDAL.h>
#include <vw/Image/Interpolation.h>
#include <asp/Core/Macros.h>
#include <asp/Core/Common.h>
#include <asp/Tools/point2dem.h>
#include <cstdlib>
using namespace vw;
using namespace vw::cartography;
using namespace std;

// Allows FileIO to correctly read/write these pixel types
namespace vw {
  typedef Vector<float64,6> Vector6;
  template<> struct PixelFormatID<Vector3>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector3f>  { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector4>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_4_CHANNEL; };
  template<> struct PixelFormatID<Vector6>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
}

struct Options : asp::BaseOptions {};

int main( int argc, char *argv[] ){

  double shiftx = 0, shifty = 0, angle = 0;
  int num;
  if (argc < 9){
    std::cout << "Usage: input.tif input_lola.csv output_lola.csv "
              << "shiftx shifty angle num datum"
              << std::endl;
    return 1;
  }

  std::string in_dem_file = argv[1];
  std::string in_lola_file = argv[2];
  std::string output_name = argv[3];
  std::cout << "real shift + rotation" << std::endl;
  shiftx = atof(argv[4]);
  shifty = atof(argv[5]);
  angle  = atof(argv[6]);
  num    = atof(argv[7]);

  std::string datum_str = argv[8];

  Vector3 shift(shiftx, shifty, 0);
  std::cout << "pc shift and angle " << in_dem_file << " "
            << shiftx << ' ' << shifty << ' ' << angle << std::endl;
  std::cout << "Reading: " << in_dem_file << std::endl;
  std::cout << "Number of points is " << num << std::endl;
  std::cout << "datum: " << datum_str << std::endl;

  cartography::GeoReference dem_georef;
  cartography::read_georeference( dem_georef, in_dem_file );
  DiskImageView<float> dem( in_dem_file );

  // To do: Make it bigger
  double nodata = -32768;
  boost::shared_ptr<DiskImageResource>
    dem_rsrc( new DiskImageResourceGDAL(in_dem_file) );
  typedef PixelMask<float> PMaskT;
  ImageViewRef<PMaskT> in_dem;
  if (dem_rsrc->has_nodata_read()){
    nodata = dem_rsrc->nodata_read();
    in_dem = create_mask(dem, nodata);
    cout<<"nodata =" << nodata << std::endl;
  }else{
    in_dem = pixel_cast<PMaskT>(dem);
  }

  InterpolationView<EdgeExtensionView< ImageViewRef <PMaskT>, ValueEdgeExtension<PMaskT> >, BilinearInterpolation> interp_dem = interpolate(in_dem, BilinearInterpolation(), ValueEdgeExtension<PMaskT>(PMaskT()));


  double theta = angle*M_PI/360;
  Matrix3x3 M;
  M(0, 0) = cos(theta);
  M(0, 1) = -sin(theta);
  M(1, 0) = sin(theta);
  M(1, 1) = cos(theta);
  M(2, 2) = 1.0;
  std::cout << "rotation matrix is " << M << std::endl;

  cartography::Datum datum;
  datum.set_well_known_datum(datum_str);

  vector<Vector3> points;
  Vector3 mean_center;
  string line;
  int year, month, day, hour, min;
  double lon, lat, rad, sec, is_invalid;
  bool is_first_line = true;

  std::cout << "Will write: " << output_name << std::endl;
  ofstream outfile2( output_name.c_str() );
  outfile2.precision(16);

  const int bufSize = 1024;
  char temp[bufSize];
  std::cout << "reading " << in_lola_file << std::endl;
  ifstream file(in_lola_file.c_str());
  if( !file ) {
    vw_throw( vw::IOErr() << "Unable to open track file \"" << in_lola_file
              << "\"" );
  }
  while( getline(file, line, '\n') ) {

    // We went with C-style file reading instead of C++ in this instance
    // because we found it to be significantly faster on large files.

    vector<string> tokens;

    strncpy(temp, line.c_str(), bufSize);
    const char* token = strtok(temp, ","); tokens.push_back(token);

    int ret = sscanf(token, "%d-%d-%dT%d:%d:%lg", &year, &month, &day, &hour,
                     &min, &sec);
    if( year <= 0 ) { continue; }

    token = strtok(NULL, ","); tokens.push_back(token);
    ret += sscanf(token, "%lg", &lon);

    token = strtok(NULL, ","); tokens.push_back(token);
    ret = ret + sscanf(token, "%lg", &lat);
    token = strtok(NULL, ","); tokens.push_back(token);
    ret = ret + sscanf(token, "%lg", &rad);
    rad *= 1000; // km to meters

    // Scan 7 more fields, until we get to the is_invalid flag.
    for (int i = 0; i < 7; i++){
      token = strtok(NULL, ","); tokens.push_back(token);
    }
    ret = ret + sscanf(token, "%lg", &is_invalid);

    // Be prepared for the fact that the first line may be the header.
    if (ret != 10){
      if (!is_first_line){
        vw_throw( vw::IOErr() << "Failed to read line: " << line << "\n" );
      }else{
        std::cout << "will write line: " << line << std::endl;
        outfile2 << line << std::endl;
        is_first_line = false;
        continue;
      }
    }
    is_first_line = false;

    while(1){
      token = strtok(NULL, ",");
      if (token == NULL) break;
      tokens.push_back(token);
    }

    if (is_invalid) continue;

    double height = rad - datum.semi_major_axis();
    Vector2 pix = dem_georef.lonlat_to_pixel(Vector2(lon, lat));
    pix = round(pix);
    Vector2 lonlat = dem_georef.pixel_to_lonlat(pix);
    if (pix[0] >= 0 && pix[0] <= interp_dem.cols() - 1 &&
        pix[1] >= 0 && pix[1] <= interp_dem.rows() - 1 ){
      PMaskT v = interp_dem(pix[0], pix[1]);
      if (is_valid(v) && v.child() != nodata){
        height = v.child();

        Vector3 v = dem_georef.datum().geodetic_to_cartesian(Vector3(lonlat[0], lonlat[1], height));
        v = M*v + shift;

        //out_point_image(col, row) = M*in_point_image(col, row) + shift;
        Vector3 llh = dem_georef.datum().cartesian_to_geodetic(v);
        //outfile2 << lonlat[1]  << " " << lonlat[0] << " " << height << std::endl;
        //outfile2 << llh[1]  << " " << llh[0] << " " << llh[2] << std::endl;
        std::ostringstream o1, o2, o3;
        o1.precision(16); o1 << llh[0]; tokens[1] = o1.str();
        o2.precision(16); o2 << llh[1]; tokens[2] = o2.str();
        o3.precision(16); o3 << norm_2(v)/1000; tokens[3] = o3.str();
      }
    }else{
      //outfile2 << lat  << " " << lon << " " << height << std::endl;
    }

    for (int k = 0; k < tokens.size(); k++) outfile2 << tokens[k] << ',';
    outfile2 << std::endl;

    Vector3 lonlatrad( lon, lat, 0 );
    Vector3 xyz = datum.geodetic_to_cartesian( lonlatrad );
    if ( xyz == Vector3() || !(xyz == xyz) ) continue; // invalid and NaN check

    // Adjust the point so that it is at the right distance from
    // planet center. This will work even if the planet is Earth,
    // which is an ellipsoid rather than a sphere.
    xyz = rad*(xyz/norm_2(xyz));
    points.push_back(xyz);
    mean_center += xyz;
  }

  mean_center = mean_center/points.size();
  std::cout << "mean is " << mean_center << std::endl;

  return 0;

}

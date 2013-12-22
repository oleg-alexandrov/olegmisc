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
#include <asp/Core/Macros.h>
#include <asp/Core/Common.h>

using namespace vw;
using namespace vw::cartography;

struct Options : asp::BaseOptions {};
double sign(double x){
  if (x < 0) return -1; else return 1;
}

int main( int argc, char *argv[] ){

  // How the areoid was obtained:
  // Data source: http://geo.pds.nasa.gov/missions/mgs/megdr.html
  // pds2isis from=mega90n000eb.lbl to=mega90n000eb.cub
  // gdal_translate mega90n000eb.cub mega90n000eb.tif

  // Read the areoid
  std::string input_areoid_file = "mega90n000eb.tif";
  std::cout << "Reading " << input_areoid_file << std::endl;
  ImageView<float> areoid = DiskImageView<float>(input_areoid_file);

  // Its georeference is messed up, we will create our own
  GeoReference input_areoid_georef;
  read_georeference(input_areoid_georef, input_areoid_file);

  GeoReference areoid_georef;
  areoid_georef.set_well_known_geogcs("D_MARS");
  Matrix3x3 M;
  M.set_identity();
  double spacing = 360.0/areoid.cols();
  M(0,0) = spacing;
  M(1,1) = -spacing;
  M(0, 2) = 0;  // x coord of upper left corner
  M(1, 2) = 90; // y coord of upper left corner
  areoid_georef.set_transform(M);

  //We must adjust the areoid to the datum of the new areoid
  double delta = input_areoid_georef.datum().semi_major_axis() - areoid_georef.datum().semi_major_axis();
  std::cout << "adding " << delta << " to the areoid " << std::endl;
  areoid += delta;

  // Extra padding to make it easier to interpolate
  int pad = 5;
  GeoReference areoid2_georef = crop(areoid_georef, -pad, -pad);
  ImageView<float> areoid2(areoid.cols() + 2*pad, areoid.rows() + 2*pad);
  for (int col = 0; col < areoid2.cols(); col++){
    for (int row = 0; row < areoid2.rows(); row++){
      Vector2 pix2(col, row);
      Vector2 lonlat = areoid2_georef.pixel_to_lonlat(pix2);

      // Need to carefully wrap lonlat to the [0, 360) x [-90, 90) box.
      // Note that lon = 25, lat = 91 is the same as lon = 180 + 25, lat = 89
      // as we go through the North pole and show up on the other side.
      while ( std::abs(lonlat[1]) > 90.0 ){
        if ( lonlat[1] > 90.0 ){
          lonlat[1] = 180.0 - lonlat[1];
        lonlat[0] += 180.0;
        }
        if ( lonlat[1] < -90.0 ){
          lonlat[1] = -180.0 - lonlat[1];
        lonlat[0] += 180.0;
        }
      }
      while( lonlat[0] <   0.0  ) lonlat[0] += 360.0;
      while( lonlat[0] >= 360.0 ) lonlat[0] -= 360.0;

      Vector2 pix = round(areoid_georef.lonlat_to_pixel(lonlat));
      areoid2(col, row) = areoid((int)pix[0], (int)pix[1]);

    }
  }

  std::cout << "areoid georef: " << areoid_georef << std::endl;
  std::cout << "pixel 0 0 lonlat is " << areoid_georef.pixel_to_lonlat(Vector2(0, 0)) << std::endl;
  std::cout << "pixel -1 -1 lonlat is " << areoid_georef.pixel_to_lonlat(Vector2(areoid.cols()-1, areoid.rows()-1)) << std::endl;

  std::string areoid_file = "areoid.tif";
  double areoid_nodata_val = -32767;

  std::cout << "Writing: " << areoid_file << std::endl;
  Options opt;
  boost::scoped_ptr<DiskImageResourceGDAL> rsrc( asp::build_gdal_rsrc(areoid_file,
                                                                      areoid2, opt ) );
  rsrc->set_nodata_write( areoid_nodata_val );
  write_georeference( *rsrc, areoid2_georef );
  block_write_image( *rsrc, areoid2,
                     TerminalProgressCallback("asp", "\t--> Writing the areoid: ") );

  return 0;

}

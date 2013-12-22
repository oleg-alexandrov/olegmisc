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

  // Adjust the values by wrapping around properly

  std::string delta_file = "/home/oalexan1/projects/base_system/share/geoids/egm96-5.tif";
  std::cout << "reading: " << delta_file << std::endl;

  double delta_nodata_val = std::numeric_limits<float>::quiet_NaN();
  DiskImageResourceGDAL delta_rsrc(delta_file);
  if ( delta_rsrc.has_nodata_read() ) {
    delta_nodata_val = delta_rsrc.nodata_read();
  }
  std::cout << "nodata is " << delta_nodata_val << std::endl;

  ImageView<float> delta_img = DiskImageView<float>(delta_rsrc);
  GeoReference delta_georef;
  read_georeference(delta_georef, delta_rsrc);

  ImageView<float> delta_out = delta_img;

  for (int col = 0; col < delta_out.cols(); col++){
    for (int row = 0; row < delta_out.rows(); row++){

      Vector2 lonlat = delta_georef.pixel_to_lonlat(Vector2(col, row));
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

      Vector2 pix = round(delta_georef.lonlat_to_pixel(lonlat));
      int c = (int)pix[0];
      int r = (int)pix[1];

      delta_out(col, row) = delta_img(c, r);
    }
  }


  std::string adj_dem_file = "adjusted.tif";
  std::cout << "Writing: " << adj_dem_file << std::endl;

  Options opt;

  boost::scoped_ptr<DiskImageResourceGDAL> rsrc( asp::build_gdal_rsrc(adj_dem_file,
                                                                      delta_out, opt ) );
  rsrc->set_nodata_write( delta_nodata_val );
  write_georeference( *rsrc, delta_georef );
  block_write_image( *rsrc, delta_out,
                     TerminalProgressCallback("asp", "\t--> Applying DEM adjustment: ") );

  return 0;

}

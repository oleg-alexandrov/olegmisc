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
#include <vw/Image/Interpolation.h>
#include <asp/Core/Macros.h>
#include <asp/Core/Common.h>

using namespace vw;
using namespace vw::cartography;

struct Options : asp::BaseOptions {};

// Find the mean and std dev DEM of the given input sets of DEMs.
// Also output count.tif, showing for each pixel how many times
// it was encountered in the stack of DEMs.

int main( int argc, char *argv[] ){

  Options opt;

  if (argc < 4){
    std::cerr << "Usage: " << argv[0] << " input.tif fill_dist output.tif"
              << std::endl;
    exit(0);
  }
  
  std::string file = argv[1];
  int hole_fill_len = atoi(argv[2]);
  std::string outfile = argv[3];
  
  std::cout << "Reading: " << file << std::endl;
  std::cout << "hole fill len is " << hole_fill_len << std::endl;
  
  DiskImageResourceGDAL in_rsrc(file);
  float nodata_val = -32767;
  if ( in_rsrc.has_nodata_read() ) {
    nodata_val = in_rsrc.nodata_read();
    vw_out() << "\tFound input nodata value: " << nodata_val << std::endl;
  }else{
    std::cerr << "Nodata value not found in: " << file << std::endl;
    exit(1);
  }
  
  
  DiskImageView<float> dem(in_rsrc);
  ImageView<float> filled_dem = dem;

  for (int row = 0; row < dem.rows(); row++){
    for (int col = 0; col < dem.cols(); col++){

      if (dem(col, row) != nodata_val) continue; // skip valid
        
      // Look left, right, up, down, and find the closest valid pixels
      int hl = hole_fill_len; // shorten
      double r0 = -1, r1 = -1, c0 = -1, c1 = -1; // no good indices yet
      for (int k = row-1; k >= std::max(row-hl, 0); k--)
        if (dem(col, k) != nodata_val){ r0 = k; break; }
      if (r0 >=0){
        // Found a point to the left, try to also find one to the right
        for (int k = row+1; k <= std::min(row + hl, dem.rows()-1); k++)
          if (dem(col, k) != nodata_val){ r1 = k; break; }
      }
      for (int k = col-1; k >= std::max(col-hl, 0); k--)
        if (dem(k, row) != nodata_val){ c0 = k; break; }
      if (c0 >=0){
        // Found a point up, try to also find one down
        for (int k = col+1; k <= std::min(col + hl, dem.cols()-1); k++)
          if (dem(k, row) != nodata_val){ c1 = k; break; }
      }
      
      // Interpolate between left and right, then between top and
      // bottom.  Average the results.
      int num_good = 0;
      double V = 0.0;
      if (r0 >= 0 && r1 >= 0){
        V += ((r1-row)*dem(col, r0) + (row-r0)*dem(col, r1))/(r1-r0);
        num_good++;
      }
      if (c0 >= 0 && c1 >= 0){
        V += ((c1-col)*dem(c0, row) + (col-c0)*dem(c1, row))/(c1-c0);
        num_good++;
      }

      if (num_good > 0)
        filled_dem(col, row) = V/num_good;
         
    }
  }

  GeoReference georef;
  read_georeference(georef, in_rsrc);
  
  std::cout << "Writing: " << outfile << std::endl;
  block_write_gdal_image(outfile, filled_dem,
                         georef, nodata_val, opt);
  return 0;
  
}

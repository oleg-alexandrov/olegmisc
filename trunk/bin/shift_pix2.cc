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
#include <vector>
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
using namespace std;
using namespace vw;
using namespace vw::cartography;

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
  
  // Reverse CCD artifacts read from files
  
  std::string in_file  = argv[1];
  std::string out_file = argv[2];
  std::string ccdx     = argv[3];
  std::string ccdy     = argv[4];
  //  std::string factor      = argv[5];

  std::cout << "Reading: " << in_file << ' ' << ccdx << ' ' << ccdy << std::endl;
  
  vector<double> vx, vy;
  double val;
  ifstream fx(ccdx.c_str()); while(fx >> val) vx.push_back(val);
  ifstream fy(ccdy.c_str()); while(fy >> val) vy.push_back(val);
  int lx = vx.size()/2, ly = vy.size()/2;
  std::cout << "Num corrections in x and y: " << lx << ' ' << ly << std::endl;
  double factorx = 1.0;
  std::cout << "factorx is " << factorx << std::endl;
  
  ImageView<float> D = copy(DiskImageView<float>(in_file));
  ImageView<float> E = copy(DiskImageView<float>(in_file));
  
  InterpolationView<EdgeExtensionView< ImageView<float>, ValueEdgeExtension<float> >, BilinearInterpolation> interp_E
    = interpolate(E, BilinearInterpolation(),
                  ValueEdgeExtension<float>(0));
  
  int num_cols = D.cols();
  int num_rows = D.rows();
  
  for (int col = 0; col < num_cols; col++){
    if (col%100 == 0) std::cout << "col is " << col << "/" << num_cols << std::endl;
    for (int row = 0; row < num_rows; row++){
      double valx = 0, valy = 0;
      for (int ix = 0; ix < lx; ix++)
        if ( vx[ix] <= col ) valx -= factorx*vx[ix + lx];
      for (int iy = 0; iy < ly; iy++)
        if ( vy[iy] <= col ) valy -= factorx*vy[iy + ly];
      D(col, row) = interp_E(col + valx, row + valy);
      if (col == 1301 && row == 3000){
        //if (col <= 5000 && row == 0){
        std::cout << "correction is " << valx << ' ' << valy << std::endl;
        //std::cout << "col zzzoffsets: " << col << ' ' << valx << ' ' << valy << std::endl;
      }
    }
  }
  
  Options opt;
  std::cout << "Writing: " << out_file << std::endl;
  asp::block_write_gdal_image(out_file, D, opt,
                              TerminalProgressCallback("asp", "\t-->: "));

  return 0;
}

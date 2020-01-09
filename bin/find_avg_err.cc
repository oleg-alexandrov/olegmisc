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
#include <asp/Tools/point2dem.h>
#include <cstdlib>

#include <asp/Tools/stereo.h>
#include <vw/Stereo/DisparityMap.h>

#include <asp/Core/BlobIndexThreaded.h>
#include <asp/Core/InpaintView.h>
#include <asp/Core/ErodeView.h>
#include <asp/Core/ThreadedEdgeMask.h>

using namespace vw;
using namespace asp;
using namespace vw::cartography;
using namespace std;

// Allows FileIO to correctly read/write these pixel types
namespace vw {
  typedef Vector<float64,6> Vector6;
  template<> struct PixelFormatID<Vector3>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector3f>  { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector4>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_4_CHANNEL; };
  template<> struct PixelFormatID<Vector6>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
  template<> struct PixelFormatID<PixelMask<Vector<float, 5> > >   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
}

struct Options : asp::BaseOptions {};

int main( int argc, char *argv[] ){
  
  // Find the average error in each column
  
  std::string in_file  = argv[1], outx = argv[2];

  std::cout << "Reading: " << in_file << std::endl;
  DiskImageView <Vector4> D(in_file);

  int cols = D.cols(), rows = D.rows();
  std::cout << "cols and rows is " << cols << ' ' << rows << std::endl;

  vector<double> Dx(cols, 0);
  for (int col = 0; col < cols; col++){
    if (col%100 == 0){
      std::cout << "column " << col << std::endl;
    }
    vector<double> px;
    for (int row = 0; row < rows; row++){
      Vector4 p = D(col, row);
      if (p[3] == 0) continue;
      px.push_back(p[3]);
    }
    std::sort(px.begin(), px.end());
    int len = px.size();
    int num_valid = 0;
    Dx[col] = 0;
    for (int k = 0; k < len; k++){
      num_valid++;
      Dx[col] += px[k];
    }
    
    if (num_valid > 0){
      Dx[col] /= num_valid;
    }
  }

  ofstream dx(outx.c_str());
  dx.precision(16);
  std::cout << "Writing: " << outx << std::endl;
  for (int col = 0; col < cols; col++) dx << Dx[col] << std::endl;
  
  return 0;
}

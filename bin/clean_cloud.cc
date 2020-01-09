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

// Turn off warnings from boost and other packages
#if defined(__GNUC__) || defined(__GNUG__)
#define LOCAL_GCC_VERSION (__GNUC__ * 10000                    \
                           + __GNUC_MINOR__ * 100              \
                           + __GNUC_PATCHLEVEL__)
#if LOCAL_GCC_VERSION >= 40600
#pragma GCC diagnostic push
#endif
#if LOCAL_GCC_VERSION >= 40202
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#endif
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
#include <vw/Math/LinearAlgebra.h>
#include <vw/FileIO/DiskImageView.h>
#include <vw/Cartography/GeoReference.h>
#include <vw/tools/Common.h>
#include <vw/FileIO/DiskImageResourceGDAL.h>
#include <vw/Image/Interpolation.h>
#include <asp/Core/Macros.h>
#include <asp/Core/Common.h>

#if defined(__GNUC__) || defined(__GNUG__)
#if LOCAL_GCC_VERSION >= 40600
#pragma GCC diagnostic pop
#endif
#undef LOCAL_GCC_VERSION
#endif

// Allows FileIO to correctly read/write these pixel types
namespace vw {
  typedef Vector<float64,6> Vector6;
  template<> struct PixelFormatID<Vector3>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector3f>  { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector4>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_4_CHANNEL; };
  template<> struct PixelFormatID<Vector6>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
}

using namespace vw;
using namespace vw::cartography;

// Remove large outliers from cloud


struct Options : asp::BaseOptions {};

int main( int argc, char *argv[] ){

  std::string in_file = argv[1];
  std::string out_file = argv[2];
  double factor = atof(argv[3]);
  double pct = atof(argv[4]);
  std::cout << "input pc is " << in_file << std::endl;

  std::cout << "--factor is " << factor << std::endl;
  std::cout << "pct is " << pct << std::endl;

  ImageView<Vector4> in = asp::read_cloud<4>(in_file);

  // Find the error threshold
  std::vector<double> errors;
  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      if (in(col, row) == Vector3()) continue;
      errors.push_back(in(col, row)[3]);
    }
  }
  std::sort(errors.begin(), errors.end());
  double thresh = factor*errors[errors.size()*pct/100.0];
  std::cout << "--thresh is " << thresh << std::endl;

  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      Vector3 P = subvector(in(col, row), 0, 3);
      if (P == Vector3()) continue;
      if (in(col, row)[3] > thresh) {
        in(col, row) = Vector4();
      }
    }
  }

  std::cout << "Writing: " << out_file << std::endl;
  Options opt;
  block_write_gdal_image( out_file, in, opt);

  return 0;
}

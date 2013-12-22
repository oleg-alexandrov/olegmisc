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

// Take the square root of the input image file, round to int, and
// clamp to uint8.

int main( int argc, char *argv[] ){

  if (argc <= 2){
    std::cout << "Usage: " << argv[0] << " file_in.tif file_out.tif" << std::endl;
    exit(1);
  }

  std::string in_file  = argv[1];
  std::string out_file = argv[2];
  std::cout << "Reading: " << in_file << std::endl;

  DiskImageResourceGDAL in_rsrc(in_file);
  float nodata_val = -32767;
  if ( in_rsrc.has_nodata_read() ) nodata_val = in_rsrc.nodata_read();
  Options opt;
  GeoReference georef;
  read_georeference(georef, in_rsrc);
  ImageView<float> err = DiskImageView<float>(in_rsrc);
  ImageView<uint8> err_sqrt(err.cols(), err.rows());

  for (int row = 0; row < err.rows(); row++){
    for (int col = 0; col < err.cols(); col++){
      double val = err(col, row);
      if ( val == nodata_val || val <= 0.0 ) val = 0.0;
      err_sqrt(col, row) = std::min(255, (int)round(sqrt(val)));
    }
  }

  std::cout << "Writing: " << out_file << std::endl;
  block_write_gdal_image(out_file, err_sqrt,
                         georef, opt);

  return 0;

}

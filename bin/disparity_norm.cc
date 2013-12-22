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
#include <vw/Stereo/DisparityMap.h>

using namespace vw;
using namespace vw::cartography;

class disparityNorm: public ReturnFixedType<double> {

public:
  disparityNorm(){}

  double operator()(PixelMask<Vector2f> const& pix) const {
    if (!is_valid(pix)) return 0;
    return norm_2(pix.child());
  }
};

template <class ViewT>
UnaryPerPixelView<ViewT, disparityNorm> disparity_norm(ImageViewBase<ViewT> const& view) {
  return UnaryPerPixelView<ViewT, disparityNorm>( view.impl(), disparityNorm() );
}

struct Options : asp::BaseOptions {};

int main( int argc, char *argv[] ){
  
  if (argc <= 2){
    std::cout << "Usage: " << argv[0] << ' ' << " in_file.tif out_file.tif" << std::endl;
    exit(1);
  }
  
  std::string in_file  = argv[1];
  std::string out_file = argv[2];
  
  DiskImageView< PixelMask<Vector2i> > in(in_file);

  Options opt;
  ImageViewRef<double> out = disparity_norm(in);
  std::cout << "Writing: " << out_file << std::endl;

  asp::block_write_gdal_image(out_file,
                              out, opt,
                              TerminalProgressCallback("asp", "\t--> Norm: ") );
  
  
  return 0;
}

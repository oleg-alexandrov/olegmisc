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

namespace vw {
  typedef Vector<double, 6> Vector6;
  template<> struct PixelFormatID<PixelMask<Vector<float, 5> > >   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
  template<> struct PixelFormatID<Vector<double, 6> >   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
  template<> struct PixelFormatID<Vector<double, 4> >   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_4_CHANNEL; };
  template<> struct PixelFormatID<Vector<float, 6> >   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
  template<> struct PixelFormatID<Vector<float, 4> >   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_4_CHANNEL; };
  template<> struct PixelFormatID<Vector<float, 2> > { static const PixelFormatEnum value = VW_PIXEL_GENERIC_2_CHANNEL; };
}

void handle_arguments( int argc, char *argv[], Options& opt ) {

  po::options_description manipulation_options("Manipulation options");
  po::options_description projection_options("Projection options");
  po::options_description general_options("General Options");
  general_options.add( manipulation_options );
  general_options.add( projection_options );
  general_options.add( asp::BaseOptionsDescription(opt) );
}

int main( int argc, char *argv[] ){

  // Adjust the values by wrapping around properly

  std::string file = argv[1];
  std::cout << "reading: " << file << std::endl;

  DiskImageView<Vector4> img(file);

  std::string adj_file = "adjusted.tif";
  std::cout << "Writing: " << adj_file << std::endl;

  Options opt;
  handle_arguments( argc, argv, opt );
  
  boost::scoped_ptr<DiskImageResourceGDAL> rsrc( asp::build_gdal_rsrc(adj_file,
                                                                      img, opt ) );
  block_write_image( *rsrc, img,
                     TerminalProgressCallback("asp", "\t--> ") );

  return 0;

}

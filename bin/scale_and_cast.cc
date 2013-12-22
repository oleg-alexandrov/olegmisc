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

class ScaleAndCast: public ReturnFixedType<uint8> {
  double m_min_val, m_max_val, m_nodata_val;

public:
  ScaleAndCast(double min_val, double max_val, double nodata_val):
    m_min_val(min_val), m_max_val(max_val), m_nodata_val(nodata_val){}

  uint8 operator() (double const& pix) const {
    double lpix = pix;
    if (lpix == m_nodata_val) return m_nodata_val;
    if (lpix > m_max_val) lpix = m_max_val;
    if (lpix < m_min_val) lpix = m_min_val;
    //return uint8( std::min(255.0, round(255.0*(lpix - m_min_val)/(m_max_val - m_min_val))) );
    return uint8( std::min(255.0, 255.0*(lpix - m_min_val)/(m_max_val - m_min_val)) );
  }
};
template <class ViewT>
UnaryPerPixelView<ViewT, ScaleAndCast> scale_and_cast(ImageViewBase<ViewT> const& view,
                                                double min_val, double max_val, double nodata_val) {
  return UnaryPerPixelView<ViewT, ScaleAndCast>( view.impl(), ScaleAndCast(min_val, max_val, nodata_val) );
}

struct Options : asp::BaseOptions {};

int main( int argc, char *argv[] ){

  // Take the provided input min and max values, and scale the image
  // so that values <= min_val map to 0, and values >= max_val map to
  // 255. Convert the resulting image to int8.
  
  double min_val = atof(argv[1]);
  double max_val = atof(argv[2]);

  std::string in_file  = argv[3];
  std::string out_file = argv[4];

  std::cout << min_val << ' ' << max_val << ' ' << in_file << ' ' << out_file << std::endl;

  if (min_val > max_val){
    std::cerr << "Expecting min_val <= max_val" << std::endl;
    exit(1);
  }

  DiskImageResourceGDAL in_rsrc(in_file);
  double nodata_val = -32767;
  if ( in_rsrc.has_nodata_read() ) {
    nodata_val = in_rsrc.nodata_read();
    vw_out() << "\tFound input nodata value: " << nodata_val << std::endl;
  }
  
  GeoReference georef;
  read_georeference(georef, in_rsrc);

  DiskImageView<double> in(in_file);

  Options opt;
  ImageViewRef<uint8> out = scale_and_cast(in, min_val, max_val, nodata_val);
  boost::scoped_ptr<DiskImageResourceGDAL> rsrc( asp::build_gdal_rsrc( out_file,
                                                                       out, opt ) );
  rsrc->set_nodata_write( nodata_val );
  write_georeference( *rsrc, georef );
  block_write_image( *rsrc, out,
                     TerminalProgressCallback("asp", "\t--> Casting: ") );
  
   return 0;
   
}

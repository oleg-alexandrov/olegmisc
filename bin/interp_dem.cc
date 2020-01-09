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

int main( int argc, char *argv[] ){

  if (argc < 3){
    std::cout << "usage inputDEM.tif imgGridToInterpTo.tif outputDEM.tif" << std::endl;
    return 1;
  }

  std::string in_dem_file  = argv[1];
  std::string in_img_file  = argv[2];
  std::string out_dem_file = argv[3];

  std::cout << "files are " << in_dem_file << ' ' << in_img_file << ' ' << out_dem_file << std::endl;

  // Read DEM, nodata, and georef
  Options opt;
  typedef PixelMask<float> PMaskT;
  float nodata_val = -32768;
  DiskImageResourceGDAL dem_rsrc(in_dem_file);
  DiskImageView<float> dem(dem_rsrc);
  ImageViewRef<PMaskT> masked_dem;
  if ( dem_rsrc.has_nodata_read() ) {
    nodata_val = dem_rsrc.nodata_read();
    masked_dem = create_mask(dem, nodata_val);
    vw_out() << "\tFound input nodata value: " << nodata_val << std::endl;
  }else{
    masked_dem = pixel_cast<PMaskT>(dem);
  }
  InterpolationView<EdgeExtensionView< ImageViewRef <PMaskT>,
    ValueEdgeExtension<PMaskT> >, BilinearInterpolation> interp_dem
    = interpolate(masked_dem, BilinearInterpolation(),
                  ValueEdgeExtension<PMaskT>(PMaskT()));

  GeoReference dem_georef;
  read_georeference(dem_georef, dem_rsrc);

  DiskImageResourceGDAL img_rsrc(in_img_file);
  DiskImageView<float> img(img_rsrc);
  GeoReference img_georef;
  read_georeference(img_georef, img_rsrc);

  ImageView< PixelMask<float> > out_dem(img.cols(), img.rows());
  for (int c = 0; c < out_dem.cols(); c++){
    std::cout << "col " << c << ' ' << out_dem.cols() << std::endl;
    for (int r = 0; r < out_dem.rows(); r++){
      Vector2 pix(c, r);
      Vector2 dem_pix = dem_georef.lonlat_to_pixel(img_georef.pixel_to_lonlat(pix));
      out_dem(c, r) = interp_dem(dem_pix[0], dem_pix[1]);
    }
  }
  
  std::cout << "Writing: " << out_dem_file << std::endl;
  block_write_gdal_image(out_dem_file, apply_mask(out_dem, nodata_val),
                         img_georef, nodata_val, opt);
  
  return 0;

}

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

  bool pixel_shift = true;
  double shiftx = 0, shifty = 0, angle = 0;
  std::string in_dem_file = argv[1];
  std::string out_dem_file = argv[2];
  if (argc == 5){
    std::cout << "pixel shift" << std::endl;
    shiftx = atoi(argv[3]);
    shifty = atoi(argv[4]);
  }else if (argc == 6){
    std::cout << "real shift + rotation" << std::endl;
    pixel_shift = false;
    shiftx = atof(argv[3]);
    shifty = atof(argv[4]);
    angle  = atof(argv[5]);
  }else{
    std::cout << "Must specify shift and rotation angle" << std::endl;
    return 1;
  }

  std::cout << "dem shift and angle " << in_dem_file << " "
            << shiftx << ' ' << shifty << ' ' << angle << std::endl;
  std::cout << "Reading: " << in_dem_file << std::endl;

  // Read DEM, nodata, and georef
  Options opt;
  typedef PixelMask<float> PMaskT;
  float nodata_val = -32768;
  DiskImageResourceGDAL dem_rsrc(in_dem_file);
  DiskImageView<float> dem_disk_image(dem_rsrc);
  ImageViewRef<PMaskT> in_dem;
  if ( dem_rsrc.has_nodata_read() ) {
    nodata_val = dem_rsrc.nodata_read();
    in_dem = create_mask(dem_disk_image, nodata_val);
    vw_out() << "\tFound input nodata value: " << nodata_val << std::endl;
  }else{
    in_dem = pixel_cast<PMaskT>(dem_disk_image);
  }
  InterpolationView<EdgeExtensionView< ImageViewRef <PMaskT>,
    ValueEdgeExtension<PMaskT> >, BilinearInterpolation> interp_dem
    = interpolate(in_dem, BilinearInterpolation(),
                  ValueEdgeExtension<PMaskT>(PMaskT()));
  GeoReference georef;
  read_georeference(georef, dem_rsrc);

  //int n = 1024;
  int n = 512;
  //int sx = 2300, sy = 3000;
  int sx = 0, sy = 0;
  ImageView<PMaskT> dem;
  if (pixel_shift){
    dem = crop(edge_extend(in_dem,
                           ValueEdgeExtension<PMaskT>(PMaskT())),
               BBox2i(sx + shiftx, sy + shifty, n, n));
    georef = crop(georef, sx, sy);
  }else{
    double theta = angle*M_PI/360;
    Matrix2x2 M;
    M(0, 0) = cos(theta);
    M(0, 1) = -sin(theta);
    M(1, 0) = sin(theta);
    M(1, 1) = cos(theta);
    dem = ImageView<float>(n, n);
    for (int c = 0; c < n; c++){
      for (int r = 0; r < n; r++){
        Vector2 px = M*Vector2(c, r) + Vector2(shiftx, shifty);
        dem(c, r) = interp_dem(px[0], px[1]);
      }
    }
  }

  std::cout << "Writing: " << out_dem_file << std::endl;
  block_write_gdal_image(out_dem_file, apply_mask(dem, nodata_val),
                         georef, nodata_val, opt);

#if 0
    ImageViewRef<PMaskT> masked_dem = create_mask(dem, nodata_val);

    InterpolationView<EdgeExtensionView< ImageViewRef <PMaskT>, ConstantEdgeExtension>, BilinearInterpolation> interp_dem = interpolate(masked_dem);


    if (i == start){
      dem.set_size    ( dem.cols(), dem.rows() );
      std_dev_dem.set_size ( dem.cols(), dem.rows() );
      count_dem.set_size   ( dem.cols(), dem.rows() );
      georef = georef;
    }

    for (int col = 0; col < dem.cols(); col++){
      for (int row = 0; row < dem.rows(); row++){
        Vector2 pix(col, row);

        if (i != start){
          pix = georef.lonlat_to_pixel(georef.pixel_to_lonlat(pix));
        }

        double x = pix[0], y = pix[1];
        double interp_val;
        bool is_in_box = ( 0 <= x && x <= interp_dem.cols() - 1 &&
                           0 <= y && y <= interp_dem.rows() - 1 );
        if (!is_in_box) continue;

        // The "if" statement below is to work around a bug in
        // interpolation with PixelMask.
       PMaskT val;
        if (x == col && y == row)
          val = masked_dem(col, row);
        else
          val = interp_dem(x, y);
        if (!is_valid(val)) continue;
        interp_val = val.child();

        dem    (col, row) += interp_val;
        std_dev_dem (col, row) += interp_val*interp_val;
        count_dem   (col, row) += 1;
      }

    }
  }

  for (int col = 0; col < dem.cols(); col++){
    for (int row = 0; row < dem.rows(); row++){

      if (count_dem (col, row) == 0){
        dem    (col, row) = nodata_val;
        std_dev_dem (col, row) = nodata_val;
        count_dem   (col, row) = nodata_val;
        continue;
      }

      double mean    = dem(col, row)/count_dem(col, row);
      double std_dev = sqrt( std_dev_dem(col, row)/count_dem(col, row) - mean*mean );

      dem    (col, row) = mean;
      std_dev_dem (col, row) = std_dev;
    }
  }

  std::string mean_file = "mean.tif";
  std::cout << "Writing: " << mean_file << std::endl;
  block_write_gdal_image(mean_file, pixel_cast<float>(dem),
                         georef, nodata_val, opt);

  std::string std_dev_file = "std_dev.tif";
  std::cout << "Writing: " << std_dev_file << std::endl;
  block_write_gdal_image(std_dev_file, pixel_cast<float>(std_dev_dem),
                         georef, nodata_val, opt);

  std::string count_file = "count.tif";
  std::cout << "Writing: " << count_file << std::endl;
  block_write_gdal_image(count_file, pixel_cast<float>(count_dem),
                         georef, nodata_val, opt);

#endif
  return 0;

}

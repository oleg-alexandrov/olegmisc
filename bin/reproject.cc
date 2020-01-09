// __BEGIN_LICENSE__
//  Copyright (c) 2009-2013, United States Government as represented by the
//  Administrator of the National Aeronautics and Space Administration. All
//  rights reserved.
//
//  The NGT platform is licensed under the Apache License, Version 2.0 (the
//  "License"); you may not use this file except in compliance with the
//  License. You may obtain a copy of the License at
//  http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
// __END_LICENSE__

/// \file dem_mosaic.cc
///

// A tool to mosaic and blend DEMs, and output the mosaic as tiles.

// Note 1: In practice, the tool may be more efficient if the entire
// mosaic is written out as one single large image, rather than being
// broken up into tiles. To achieve that, just specify to the tool a
// very large tile size, and use 0 for the tile index in the command
// line options.

// Note 2: The tool can be high on memory usage, so processes for
// individual tiles may need to be run on separate machines.

#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>
#include <vector>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <limits>
using namespace std;

#include <vw/FileIO.h>
#include <vw/Image.h>
#include <vw/Cartography.h>
#include <vw/Math.h>
#include <asp/Core/Macros.h>
#include <asp/Core/Common.h>

using namespace vw;
using namespace vw::cartography;

#include <boost/math/special_functions/fpclassify.hpp>
#include <boost/program_options.hpp>
namespace po = boost::program_options;

#include <boost/filesystem/convenience.hpp>
namespace fs = boost::filesystem;

typedef float RealT; // Use double for debugging
typedef PixelMask<RealT> PMaskT;

int main( int argc, char *argv[] ) {

  if (argc < 4){
    std::cerr << "Usage: " << argv[0] << " input.tif t_src output.tif" << std::endl;
    exit(0);
  }

  std::string infile = argv[1];
  std::string srs = argv[2];
  std::string outfile = argv[3];
  std::cout << "File is " << infile << std::endl;
  std::cout << "srs is " << srs << std::endl;

  RealT nodata = -std::numeric_limits<RealT>::max();
  DiskImageResourceGDAL in_rsrc(infile);
  if (in_rsrc.has_nodata_read()) nodata = in_rsrc.nodata_read();
  vw_out() << "Using no-data value: " << nodata << endl;
  
  cartography::GeoReference oldgeo;
  bool is_good = read_georeference(oldgeo, infile);
  if (!is_good)
    vw_throw(ArgumentErr() << "No georeference found in "
             << infile << ".\n");

  DiskImageView<RealT> img(infile);
  BBox2 imgbox = bounding_box(img);
  BBox2 lonlat_box = oldgeo.pixel_to_lonlat_bbox(imgbox);
  std::cout << "lonlat box is " << lonlat_box << std::endl;

  GeoReference newgeo = oldgeo;
  bool have_user_datum = false;
  Datum user_datum;
  asp::set_srs_string(srs, have_user_datum, user_datum, newgeo);

  BBox2 point_box = newgeo.lonlat_to_point_bbox(lonlat_box, 1000);
  
  double grid = std::max(point_box.width(), point_box.height())
    /std::max(imgbox.width(), imgbox.height());
  std::cout << "grid in points is " << grid << std::endl;

  Matrix3x3 T = identity_matrix<3>();
  T(0,0) = grid;
  T(1,1) = -grid;
  T(0,2) = 0;
  T(1,2) = 0;
  newgeo.set_transform(T);
  std::cout << "---lonlat is " << lonlat_box << std::endl;
  BBox2 pixbox = newgeo.lonlat_to_pixel_bbox(lonlat_box, 1000);
  std::cout << "box1 is " << pixbox << std::endl;
  newgeo = crop(newgeo, pixbox.min().x(), pixbox.min().y());
  pixbox = newgeo.lonlat_to_pixel_bbox(lonlat_box, 1000);
  std::cout << "pix box is " << pixbox << std::endl;
  
  cartography::GeoTransform trans(oldgeo, newgeo);
  BBox2 output_bbox = trans.forward_bbox( imgbox );
  vw_out() << "output_bbox3 = " << output_bbox << std::endl;
  
  ImageViewRef<PMaskT> masked_source = crop( transform( create_mask(img, nodata), trans, ValueEdgeExtension<PMaskT>(PMaskT()), BilinearInterpolation() ), output_bbox );

  std::cout << "Writing: " << outfile << std::endl;
  asp::block_write_gdal_image(outfile, apply_mask(masked_source, nodata),
                              newgeo, nodata, asp::BaseOptions(),
                              TerminalProgressCallback("asp", "\t--> "));
  
  
  return 0;
}


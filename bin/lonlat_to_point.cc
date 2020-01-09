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

/// \file dem_clean.cc
///

// A tool to mosaic and blend DEMs, and output the mosaic as tiles.
// Unless USE_GRASSFIRE is set to 0 below, it expects DEMs to have an
// alpha channel with the grassfire weights, which are used for
// blending the DEMs. Such a DEM can be obtained from a regular DEM
// using the VisionWorkbench grassfirealpha command.

// Note 1: In practice, the tool may be more efficient if the entire
// mosaic is written out as one single large image, rather than being
// broken up into tiles. To achieve that, just specify to the tool a
// very large tile size, and use 0 for the tile index in the command
// line options.

// Note 2: The tool can be high on memory usage, so processes for
// individual tiles may need to be run on separate machines.

// To do:
// Deal with the protobuf dependency in the build system.
// Fix cmake to build in build/ and not in base dir.
// Add unit tests.

#define USE_GRASSFIRE 1

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
using namespace vw;

#include <boost/math/special_functions/fpclassify.hpp>
#include <boost/program_options.hpp>
namespace po = boost::program_options;

#include <boost/filesystem/convenience.hpp>
namespace fs = boost::filesystem;

#if USE_GRASSFIRE
typedef PixelGrayA<float> DemPixelT;
#else
typedef float DemPixelT;
#endif

int main( int argc, char *argv[] ) {

  if (argc < 6){
    std::cout << "usage inputDEM.tif lon_min lat_min lon_max lat_max" << std::endl;
    return 1;
  }

  std::string input_dem = argv[1];

  // Form the georef. The georef of the first DEM is used as initial guess.
  cartography::GeoReference in_georef;
  bool is_good = read_georeference(in_georef, input_dem);
  if (!is_good){
    std::cerr << "No georeference found in " << input_dem << std::endl;
    exit(1);
  }

  std::cout << "georef is " << in_georef << std::endl;
  
  Vector2 c1(atof(argv[2]), atof(argv[3]));
  std::cout << "corner is " << c1 << std::endl;
  c1 = in_georef.lonlat_to_point(c1);
  
  Vector2 c2(atof(argv[4]), atof(argv[5]));
  std::cout << "corner is " << c2 << std::endl;
  c2 = in_georef.lonlat_to_point(c2);

  std::cout << "meters is " << c1[0] << ' ' << c1[1] << ' ' << c2[0] << ' ' << c2[1] << std::endl;

  return 0;
}


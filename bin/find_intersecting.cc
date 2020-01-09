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
using namespace std;
struct Options : asp::BaseOptions {};

int main( int argc, char *argv[] ){

  // From the tifs from the list, find those whose boxes intersect the current tif.
  if (argc < 3){
    std::cout << "usage input.tif other_list.txt" << std::endl;
    return 1;
  }
  
  std::string in_img_file = argv[1];
  std::string list_file   = argv[2];

  std::cout << "files are " << in_img_file << ' ' << list_file << std::endl;
  DiskImageResourceGDAL img_rsrc(in_img_file);
  DiskImageView<float> img(img_rsrc);
  GeoReference img_georef;
  read_georeference(img_georef, img_rsrc);

  BBox2 box = img_georef.lonlat_bounding_box(img);
  std::cout << "box is " << box << std::endl;

  vector<string> files;
  string file;
  ifstream ifs(list_file.c_str());
  while (ifs >> file){
    //std::cout << "reading: " << file << std::endl;
    DiskImageResourceGDAL rsrc(file);
    DiskImageView<float> img(rsrc);
    GeoReference georef;
    read_georeference(georef, rsrc);
    BBox2 lbox = georef.lonlat_bounding_box(img);
    //std::cout << "box is " << lbox << std::endl;
    lbox.crop(box);
    if (lbox.empty()) continue;
    std::cout << "intersecting: " << file << std::endl;
  }
  
  return 0;

}

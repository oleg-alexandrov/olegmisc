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
#include <vector>
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
using namespace std;

int main( int argc, char *argv[] ){

  // Take an image and sum the pixels in each column.
  
  std::string in_file  = argv[1];
  std::string out_file = argv[2];

  std::cout << "Reading: " << in_file << std::endl;
  
  DiskImageView<float> D(in_file);

  int num_cols = D.cols();
  int num_rows = D.rows();

  std::cout << "cols and rows are " << num_cols << ' ' << num_rows << std::endl;


  vector<double> V(num_cols, 0);

  for (int col = 0; col < num_cols; col++){
    for (int row = 0; row < num_rows; row++){
      V[col] += D(col, row);
    }
  }

  std::cout << "Writing: " << out_file << std::endl;
  ofstream fh(out_file.c_str());
  fh.precision(16);
  for (int col = 0; col < num_cols; col++){
    fh << V[col] << std::endl;
  }
  fh.close();
  
  return 0;
}

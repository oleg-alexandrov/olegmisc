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

// Find the mean and std dev DEM of the given input sets of DEMs

int main( int argc, char *argv[] ){

  std::string out_prefix = "res/out";
  std::string file1 = "res/out-L.tif";

  std::string ss = out_prefix + "-";
  std::cout << "pref is " << out_prefix << std::endl;
  std::cout << "file1 is " << file1 << std::endl;
  std::string start = fs::path(file1).stem().string();
  std::cout << "stem is " << start << std::endl;


  size_t found = file1.find(ss);
  if (found != std::string::npos) {
    std::cout << "found is " << found << std::endl;
    file1.erase(found, ss.length());
  }

  std::cout << "file 1 after: " << file1 << std::endl;

  return 0;


}

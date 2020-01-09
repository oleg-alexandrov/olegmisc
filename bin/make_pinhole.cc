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


// Given a file containing camera rotation matrix R and translation matrix t,
// such that R*x+t goes from world to camera coordinates,
// as well as f, cx, and cy, dump a pinhole model.

#include <iostream>
#include <fstream>
#include <vw/Core.h>
#include <vw/Image.h>
#include <vw/FileIO.h>
#include <vw/Cartography.h>
#include <vw/Camera/PinholeModel.h>

using namespace vw;
using namespace vw::camera;

int main( int argc, char**argv ){
  if (argc < 2){
    std::cerr << "Usage: " << argv[0] << " input.txt out.pinhole" << std::endl;
    exit(0);
  }

  std::string in_file = argv[1];
  std::cout << "Reading: " << in_file << std::endl;
  std::ifstream ifs(in_file.c_str());

  vw::Matrix3x3 R;
  vw::Vector3 t;
  double f, cx, cy;

  for (int row = 0; row < 3; row++) {
    for (int col = 0; col < 3; col++) {
      ifs >> R(row, col);
    }
  }
  for (int row = 0; row < 3; row++) {
    ifs >> t[row];
  }

  ifs >> f >>  cx >> cy;

  std::cout << "Rotation: " << R << std::endl;
  std::cout << "Translation: " << t << std::endl;
  std::cout << "cx, cy, f: " << cx << ' ' << cy << ' ' << f << std::endl;

  PinholeModel P(-inverse(R)*t, inverse(R), f, f, cx, cy);

  std::cout << "cam is " << P << std::endl;

  std::string out_file = argv[2];
  std::cout << "Writing: " << out_file << std::endl;
  P.write(out_file);

}

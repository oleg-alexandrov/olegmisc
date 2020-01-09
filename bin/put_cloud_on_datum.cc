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

// Turn off warnings from boost and other packages
#if defined(__GNUC__) || defined(__GNUG__)
#define LOCAL_GCC_VERSION (__GNUC__ * 10000                    \
                           + __GNUC_MINOR__ * 100              \
                           + __GNUC_PATCHLEVEL__)
#if LOCAL_GCC_VERSION >= 40600
#pragma GCC diagnostic push
#endif
#if LOCAL_GCC_VERSION >= 40202
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#endif
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
#include <vw/Math/LinearAlgebra.h>
#include <vw/FileIO/DiskImageView.h>
#include <vw/Cartography/GeoReference.h>
#include <vw/tools/Common.h>
#include <vw/FileIO/DiskImageResourceGDAL.h>
#include <vw/Image/Interpolation.h>
#include <asp/Core/Macros.h>
#include <asp/Core/Common.h>

#if defined(__GNUC__) || defined(__GNUG__)
#if LOCAL_GCC_VERSION >= 40600
#pragma GCC diagnostic pop
#endif
#undef LOCAL_GCC_VERSION
#endif

// Allows FileIO to correctly read/write these pixel types
namespace vw {
  typedef Vector<float64,6> Vector6;
  template<> struct PixelFormatID<Vector3>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector3f>  { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector4>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_4_CHANNEL; };
  template<> struct PixelFormatID<Vector6>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
}

using namespace vw;
using namespace vw::cartography;

// TODO: Must move this function to some better place!

// Compute the plane that best fits a set of 3D points.
// - The plane is described as z = ax + by + c
//( the output vector contains [a, b, c]
bool fitPlaneToPoints(const std::vector<Vector3> &points, Vector3 &planeDesc){
  const size_t X = 0; // Convenience constants
  const size_t Y = 1;
  const size_t Z = 2;
  const size_t numPoints = points.size();

  // Compute values in a matrix A and vector b
  //A: [xx  xy  x]     B: [xz]
  //   [xy  yy  y]        [yz]
  //   [ x   y  n]        [ z]
  Matrix3x3 matA(0, 0, 0, 0, 0, 0, 0, 0, 0); // A symmetric matrix, A' = A
  Vector3   vecB(0, 0, 0);
  for (size_t i=0; i<numPoints; ++i)
    {
      matA[0][0] += points[i][X] * points[i][X]; // sum xx
      matA[0][1] += points[i][X] * points[i][Y]; // sum xy
      matA[0][2] += points[i][X];                // sum x
      matA[1][0] += points[i][X] * points[i][Y]; // sum xy
      matA[1][1] += points[i][Y] * points[i][Y]; // sum yy
      matA[1][2] += points[i][Y];                // sum y
      matA[2][0] += points[i][X];                // sum x
      matA[2][1] += points[i][Y];                // sum y

      vecB[0]    += points[i][X] * points[i][Z]; // sum xz
      vecB[1]    += points[i][Y] * points[i][Z]; // sum yz
      vecB[2]    += points[i][Z];                // sum z
    }
  matA[2][2] = numPoints; // n

  // Now solve Ax = b (3x3)*(3x1) = (3x1)
  planeDesc = vw::math::solve(matA, vecB); // Throws!

  return true;
}

struct Options : asp::BaseOptions {};

// Find the mean and std dev DEM of the given input sets of DEMs.
// Also output count.tif, showing for each pixel how many times
// it was encountered in the stack of DEMs.

int main( int argc, char *argv[] ){

  std::string in_file = argv[1];
  std::string out_file = argv[2];
  std::cout << "input pc is " << in_file << std::endl;

  ImageView<Vector4> in = asp::read_cloud<4>(in_file);

  // Find the error threshold
  std::vector<double> errors;
  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      if (in(col, row) == Vector3()) continue;
      errors.push_back(in(col, row)[3]);
    }
  }
  std::sort(errors.begin(), errors.end());
  double thresh = 4*errors[errors.size()*0.5];
  std::cout << "--thresh is " << thresh << std::endl;

  // Cloud center
  int num = 0;
  Vector3 CT;
  std::vector<Vector3> points;
  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      Vector3 P = subvector(in(col, row), 0, 3);
      if (P == Vector3()) continue;
      if (in(col, row)[3] > thresh) continue;
      points.push_back(P);
      CT += P;
      num+= 1;
    }
  }
  CT /= num;

  // - The plane is described as z = V[0]*x + V[1]*y + V[2]
  Vector3 V;
  fitPlaneToPoints(points, V);
  std::cout << "---plane is " << V << std::endl;

  // Closest point
  Vector3 CP = -V[2]*Vector3(V[0], V[1], -1)
    /(V[0]*V[0] + V[1]*V[1] + 1);

  std::cout << "value is " << V[0]*CP[0] + V[1]*CP[1] + V[2] - CP[2] << std::endl;

  std::cout << "value is " << V[0]*CT[0] + V[1]*CT[1] + V[2] - CT[2] << std::endl;

  std::cout << "closest is " << CP << std::endl;
  std::cout << "center is " << CT << std::endl;


  // Move the points so that the center is now CP.
  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      Vector3 P = subvector(in(col, row), 0, 3);
      if (P == Vector3()) continue;
      subvector(in(col, row), 0, 3) += CP - CT;
    }
  }

  // Verify that the center is at CP
  // Cloud center
  num = 0;
  CT = Vector3();
  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      Vector3 P = subvector(in(col, row), 0, 3);
      if (P == Vector3()) continue;
      if (in(col, row)[3] > thresh) continue;
      CT += P;
      num+= 1;
    }
  }
  CT /= num;

  std::cout << "center is " << CT << std::endl;
  std::cout << "closest is " << CP << std::endl;

  Datum datum;
  datum.set_well_known_datum("D_MOON");
  std::cout << "datum is " << datum << std::endl;
  double rad = datum.semi_major_axis();
  std::cout << "--radius is " << rad << std::endl;

  // Put the points on moon surface
  double ratio = rad/norm_2(CP);
  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      Vector3 P = subvector(in(col, row), 0, 3);
      if (P == Vector3()) continue;
      subvector(in(col, row), 0, 3) *= ratio;
    }
  }
  CP *= ratio;
  CT *= ratio;

  // Find the diameter of the points projected to the plane
  double d = 0;
  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      Vector3 P = subvector(in(col, row), 0, 3);
      if (P == Vector3()) continue;
      if (in(col, row)[3] > thresh)
        continue;

      d = std::max(d, norm_2(P - CP));
    }
  }
  std::cout << "diam is " << d << std::endl;

  // Don't let the diam be more than that, in km
  double max_diam = 1000;
  ratio = max_diam/d;
  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      Vector3 P = subvector(in(col, row), 0, 3);
      if (P == Vector3()) continue;
      subvector(in(col, row), 0, 3) = ratio*(P - CP) + CP;
    }
  }

  // Verify the diam
  // Find the diameter of the points projected to the plane
  d = 0;
  for (int col = 0; col < in.cols(); col++) {
    for (int row = 0; row < in.rows(); row++) {
      Vector3 P = subvector(in(col, row), 0, 3);
      if (P == Vector3()) continue;
      if (in(col, row)[3] > thresh) continue;
      d = std::max(d, norm_2(P - CP));
    }
  }
  std::cout << "diam is " << d << std::endl;
  std::cout << "norm of center is " << norm_2(CP) << std::endl;

  std::cout << "Writing: " << out_file << std::endl;
  Options opt;
  block_write_gdal_image( out_file, in, opt);

  return 0;
}

#include <iostream>
#include <fstream>
#include <vw/Core.h>
#include <vw/Image.h>
#include <vw/FileIO.h>
#include <vw/Cartography.h>
#include <asp/Core/Common.h>
#include <asp/Core/Macros.h>

using namespace std;
using namespace vw;
using namespace vw::cartography;

// Allows FileIO to correctly read/write these pixel types
namespace vw {
  typedef Vector<float64,6> Vector6;
  template<> struct PixelFormatID<Vector3>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector3f>  { static const PixelFormatEnum value = VW_PIXEL_GENERIC_3_CHANNEL; };
  template<> struct PixelFormatID<Vector4>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_4_CHANNEL; };
  template<> struct PixelFormatID<Vector6>   { static const PixelFormatEnum value = VW_PIXEL_GENERIC_6_CHANNEL; };
}

int main( int argc, char**argv ){

  if (argc < 4){
    std::cerr << "Usage: " << argv[0] << " input-PC.tif utm output.csv" << std::endl;
    exit(0);
  }
  std::string input_name( argv[1] );
  int utm = atoi(argv[2]);
  std::string output_name( argv[3] );
  if ( input_name.empty() ) {
    std::cerr << "Please provide an input file." << std::endl;
    return 1;
  }
  
  GeoReference geo;
  geo.set_UTM(utm, true); // north = true
  std::cout << "georef is " << geo << std::endl;

  std::cout << "Will write: " << output_name << std::endl;
  ofstream outfile( output_name.c_str() );
  outfile.precision(18);
  
  ImageViewRef<Vector3> pc = asp::read_cloud<3>(input_name);
  for (int row = 0; row < pc.rows(); row++){
    for (int col = 0; col < pc.cols(); col++){
      Vector3 xyz = pc(col, row);
      if (xyz == Vector3()) continue;
      
      Vector3 llh = geo.datum().cartesian_to_geodetic(xyz);
      Vector2 pt = geo.lonlat_to_point(subvector(llh, 0, 2));
      outfile << pt[0] << ", " << pt[1] << ", " << llh[2] << std::endl;
    }
  }
  outfile.close();

  return 0;
}

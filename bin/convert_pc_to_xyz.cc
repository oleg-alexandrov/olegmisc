// This writes an uncompress binary PCD
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

struct Options : public asp::BaseOptions {};

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
    std::cerr << "Usage: " << argv[0] << " input-PC.tif output.csv skip_pts" << std::endl;
    exit(0);
  }
  
  std::string input_name( argv[1] );
  std::string output_name( argv[2] );
  if ( input_name.empty()  || output_name.empty() ) {
    std::cerr << "Please provide an input and output file." << std::endl;
    return 1;
  }
  int skip_pts = atoi(argv[3]);
  std::cout << "Values: " << input_name << ' ' << output_name << ' ' << skip_pts << std::endl;

  ImageViewRef<Vector3> pc = asp::read_cloud<3>(input_name);
  
  cartography::Datum datum;
  datum.set_well_known_datum("D_MARS");
  
  std::cout << "Will write: " << output_name << std::endl;
  ofstream outfile( output_name.c_str() );
  outfile.precision(18);
  outfile << "# lat, lon, height"  << std::endl;
  
  int count = -1;
  for (int row = 0; row < pc.rows(); row++){
    for (int col = 0; col < pc.cols(); col++){
      Vector3 xyz = pc(col, row);
      if (xyz == Vector3()) continue;
      count++;
      if (count % skip_pts != 0) continue;
      
      outfile << xyz[0] << ", " << xyz[1] << ", " << xyz[2] << std::endl;
    }
  }

  outfile.close();
  
  return 0;
}

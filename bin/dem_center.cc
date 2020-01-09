// This writes an uncompress binary PCD
#include <iostream>
#include <fstream>
#include <vw/Core.h>
#include <vw/Image.h>
#include <vw/FileIO.h>
#include <vw/Cartography.h>

using namespace std;
using namespace vw;

int main( int argc, char**argv ){

  if (argc < 2){
    std::cerr << "Usage: " << argv[0] << " input.tif" << std::endl;
    exit(0);
  }
  std::string input_name( argv[1] );
  std::cout << "reading " << input_name << std::endl;
  
  cartography::GeoReference dem_georef;
  cartography::read_georeference( dem_georef, input_name );
  DiskImageView<float> dem( input_name );
  
  // To do: Make it bigger
  double nodata = -32768;
  boost::shared_ptr<DiskImageResource> dem_rsrc( new DiskImageResourceGDAL(input_name) );
  if (dem_rsrc->has_nodata_read()){
    nodata = dem_rsrc->nodata_read();
    cout<<"nodata =" << nodata << std::endl;
  }

  int cols = dem.cols();
  int rows = dem.rows();
  std::cout << "cols and rows are " << cols << ' ' << rows << std::endl;

  Vector2 ll = dem_georef.pixel_to_lonlat(Vector2(cols/2, rows/2));
  std::cout << "center ll is " << ll << std::endl;
  std::cout << "center height is " << dem(cols/2, rows/2) << std::endl;
  Vector3 xyz = dem_georef.datum().geodetic_to_cartesian(Vector3(ll[0], ll[1], dem(cols/2, rows/2)));
  std::cout << "xyz is " << xyz << std::endl;
  
  return 0;
}

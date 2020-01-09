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

  if (argc < 3){
    std::cerr << "Usage: " << argv[0] << " input.tif output.csv" << std::endl;
    exit(0);
  }
  std::string input_name( argv[1] );
  std::string output_name( argv[2] );
  if ( input_name.empty() ) {
    std::cerr << "Please provide an input file." << std::endl;
    return 1;
  }

  // Open the DEM. We need the following:
  //  Width, Height = 0,
  //  Mean center
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

  std::cout << "Will write: " << output_name << std::endl;
  ofstream outfile( output_name.c_str() );
  outfile.precision(18);

  // Convert to cartesian (lazy and recalculated many times)
  size_t count = 0;
  Vector3 mean_center;

  {
    TerminalProgressCallback tpc("","");
    double inc_amount = 1.0 / double(dem.rows() );
    tpc.report_progress(0);
    for (int j = 0; j < dem.rows(); j++ ) {
      int local_count = 0;
      Vector3 local_mean;
      for ( int i = 0; i < dem.cols(); i++ ) {
        if (dem(i, j) == nodata) continue;
        Vector2 lonlat = dem_georef.pixel_to_lonlat( Vector2(i,j) );

	Vector3 lonlatrad( lonlat.x(), lonlat.y(), dem(i,j) );
        Vector3 xyz = dem_georef.datum().geodetic_to_cartesian( lonlatrad );
        if ( xyz != Vector3() && xyz == xyz ) {
          outfile << lonlat[0] << ' ' << lonlat[1] << ' ' << norm_2(xyz) << std::endl;
          local_mean += xyz;
          local_count++;
        }
      }
      if ( local_count > 0 ) {
        local_mean /= double(local_count);
        double afraction = double(count) / double(count + local_count);
        double bfraction = double(local_count) / double(count + local_count);
        mean_center = afraction*mean_center + bfraction*local_mean;
        count += local_count;
      }
      tpc.report_incremental_progress( inc_amount );
    }
    tpc.report_finished();
  }

  return 0;

}

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
    std::cerr << "Usage: " << argv[0] << " input.tif output.pcd" << std::endl;
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

  // Convert to cartesian (lazy and recalculated many times)
  ImageView<Vector3> point_cloud(dem.cols(), dem.rows());
  size_t count = 0;
  Vector3 mean_center;

  vw_out() << "2Calculating the number of points in the file!" << std::endl;
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
          point_cloud(i, j) = xyz;
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
  //mean_center = Vector3(-2.63289e+06,-4.3668e+06,3.81894e+06);
  std::cout << "Found " << count << " valid points\n";
  std::cout << "Center is " << mean_center << std::endl;

  // Start writing the output
#if 0
  int max_count = atof(getenv("n")), count2=0;
  max_count = std::min((int)count, max_count);
#endif
  std::cout << "Will write: " << output_name << std::endl;
  ofstream outfile( output_name.c_str() );
#if 0
#else
  outfile.precision(16);
#endif
  outfile << "# .PCD v0.7 - Point Cloud Data file format" << std::endl;
  outfile << "VERSION 0.7" << std::endl;
  outfile << "FIELDS x y z\n";
  //outfile << "SIZE 4 4 4\n"
  //  "TYPE F F F\n"
  //  "COUNT 1 1 1\n"
  //"WIDTH " << max_count << "\n"
  //"HEIGHT 1 \n"
#if 0
  outfile << "VIEWPOINT " << std::setprecision(20)
          << mean_center.x() << " " << mean_center.y() << " "
          << mean_center.z() << " 1 0 0 0\n";
#endif
#if 0
  outfile << "POINTS " << max_count << "\n";
#else
  outfile << "POINTS " << count << "\n";
#endif
#if 0
  outfile << "DATA binary\n";
#else
  outfile << "DATA ascii\n";
#endif

#if 0
  Vector3 shift(atof(getenv("a")), atof(getenv("b")), atof(getenv("c")));
  double s = atof(getenv("s"));
  std::cout << "scale is " << s << std::endl;
  std::cout << "shift is: " << shift << std::endl;

  Matrix3x3 A = math::identity_matrix<3>();
  double theta = atof(getenv("t"));
  if (shift[0] != 0){
    A(0, 0) = cos(theta);
    A(0, 1) = -sin(theta);
    A(1, 0) = sin(theta);
    A(1, 1) = cos(theta);
    //A(0, 0) = 0.9; A(1, 1) = 1.1; A(2, 2) = 0.93;
//     A(2, 1) = 0.01; A(2, 0) = -0.03; A(0, 1) = 0.02;
//     A(1, 2) = 0.015;
  }
  std::cout << "Matrix is " << A << std::endl;
  std::cout << "will subtract: " << mean_center << std::endl;
#endif
  // Write out the floats
  {
    TerminalProgressCallback tpc("","");
    double inc_amount = 1.0 / double(dem.rows() );
    tpc.report_progress(0);

    for (int j = 0; j < dem.rows(); j++ ) {
      for ( int i = 0; i < dem.cols(); i++ ) {
        Vector3 xyz = subvector(point_cloud(i,j),0,3);
        if ( xyz != Vector3() ) {
#if 0
          Vector3f xyzf = xyz - mean_center; // Subtract in double .. then cast to float.
          outfile.write(reinterpret_cast<char*>(&xyzf[0]), 12);
#else

#if 0
          xyz -= mean_center;  // temporary!!!
          if (count2 >= max_count) break;

          // temporary!!!
          //xyz[0] = i; xyz[1] = j; xyz[2] = 5*pow(i/100.0, 2) + 18*cos(j/100.0);

          xyz = A*xyz; // temporary!!!

          xyz += shift; // temporary!!!
          xyz *= s;     // temporary!!!
#endif
          outfile << xyz[0] << ' ' << xyz[1] << ' ' << xyz[2] << std::endl;
#if 0
          count2++;
#endif
#endif
        }
      }
      tpc.report_incremental_progress( inc_amount );

    }
    tpc.report_finished();
  }

  outfile.close();

  return 0;
}

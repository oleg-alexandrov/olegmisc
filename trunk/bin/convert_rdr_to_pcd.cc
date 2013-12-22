// This writes an uncompress binary PCD
#include <iostream>
#include <fstream>
#include <vw/Core.h>
#include <vw/Image.h>
#include <vw/FileIO.h>
#include <vw/Cartography.h>

using namespace std;
using namespace vw;

// Load a RDR_*PointPerRow_csv_table.csv file used for LOLA. Code
// copied from Ara Nefian's lidar2dem tool.
// We will ignore lines which do not start with year (or a value that
// cannot be converted into an integer greater than zero, specifically).

int main( int argc, char**argv ){

  if (argc < 3){
    std::cerr << "Usage: " << argv[0] << " input.csv output.pcd" << std::endl;
    exit(0);
  }
  std::string input_name( argv[1] );
  std::string output_name( argv[2] );
  if ( input_name.empty() ) {
    std::cerr << "Please provide an input file." << std::endl;
    return 1;
  }

  const int bufSize = 1024;
  char temp[bufSize];
  ifstream file(input_name.c_str());
  if( !file ) {
    vw_throw( vw::IOErr() << "Unable to open track file \"" << input_name << "\"" );
  }

  cartography::Datum datum;
  datum.set_well_known_datum("D_MOON");

  vector<Vector3> points;
  Vector3 mean_center;
  string line;
  int year, month, day, hour, min;
  double lon, lat, rad, sec, is_invalid;
  bool is_first_line = true;
  while( getline(file, line, '\n') ) {

    // We went with C-style file reading instead of C++ in this instance
    // because we found it to be significantly faster on large files.

    strncpy(temp, line.c_str(), bufSize);
    const char* token = strtok(temp, ",");

    int ret = sscanf(token, "%d-%d-%dT%d:%d:%lg", &year, &month, &day, &hour,
                     &min, &sec);
    if( year <= 0 ) { continue; }

    token = strtok(NULL, ",");
    ret += sscanf(token, "%lg", &lon);

    token = strtok(NULL, ",");
    ret = ret + sscanf(token, "%lg", &lat);
    token = strtok(NULL, ",");
    ret = ret + sscanf(token, "%lg", &rad);
    rad *= 1000; // km to m

    // Scan 7 more fields, until we get to the is_invalid flag.
    for (int i = 0; i < 7; i++)
      token = strtok(NULL, ",");
    ret = ret + sscanf(token, "%lg", &is_invalid);

    // Be prepared for the fact that the first line may be the header.
    if (ret != 10){
      if (!is_first_line){
        vw_throw( vw::IOErr() << "Failed to read line: " << line << "\n" );
      }else{
        is_first_line = false;
        continue;
      }
    }
    is_first_line = false;

    if (is_invalid) continue;

    Vector3 lonlatrad( lon, lat, 0 );
    Vector3 xyz = datum.geodetic_to_cartesian( lonlatrad );
    if ( xyz == Vector3() || !(xyz == xyz) ) continue; // invalid and NaN check

    // Adjust the point so that it is at the right distance from
    // planet center. This will work even if the planet is Earth,
    // which is an ellipsoid rather than a sphere.
    xyz = rad*(xyz/norm_2(xyz));
    points.push_back(xyz);
    mean_center += xyz;
  }

  mean_center = mean_center/points.size();
  std::cout << "mean is " << mean_center << std::endl;
  int count = points.size();

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

    for (int i = 0; i < (int)points.size(); i++) {
      Vector3 xyz = points[i];
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

  }

  outfile.close();

  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <vw/Core/FundamentalTypes.h>
#include <vw/Core/Exception.h>
#include <vw/Camera/ExifData.h>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/iostreams/device/mapped_file.hpp>

using namespace std;
using namespace vw;

bool MotorolaOrder;

int Get16u(const void * Short) {
  if (MotorolaOrder){
    return (((uint8 *)Short)[0] << 8) | ((uint8 *)Short)[1];
  } else {
    return (((uint8 *)Short)[1] << 8) | ((uint8 *)Short)[0];
  }
}

// Convert a 32 bit signed value from file's native byte order
int Get32s(const void * Long) {
  if (MotorolaOrder){
    return  ((( char *)Long)[0] << 24) | (((uint8 *)Long)[1] << 16)
      | (((uint8 *)Long)[2] << 8 ) | (((uint8 *)Long)[3] << 0 );
  } else {
    return  ((( char *)Long)[3] << 24) | (((uint8 *)Long)[2] << 16)
      | (((uint8 *)Long)[1] << 8 ) | (((uint8 *)Long)[0] << 0 );
  }
}

// Convert a 32 bit unsigned value from file's native byte order
unsigned Get32u(const void * Long) {
  return (unsigned)Get32s(Long) & 0xffffffff;
}

int main(int argc, char **argv) {

  if (argc <= 1){
    std::cout << "Usage: " << argv[0] << " file.tif" << std::endl;
    exit(1);
  }

  std::string file = argv[1];
  boost::iostreams::mapped_file_source tiff_file(file.c_str());
  const unsigned char *buffer = (const unsigned char*) tiff_file.data();

  // Bytes 0-1 of TIFF header indicate byte order
  if (memcmp(buffer, "II", 2) == 0) {
    MotorolaOrder = 0; //Intel order
    std::cout << "intel order" << std::endl;
  } else {
    std::cout << "motorolla order" << std::endl;
    VW_ASSERT( memcmp(buffer, "MM", 2) == 0, IOErr() << "Invalid Exif alignment marker." );
    MotorolaOrder = 1; //Motorola order
  }

  char a = buffer[0], b = buffer[1];
  std::cout << "start is " << a << ' ' << b << std::endl;

  int version = Get16u(buffer + 2);
  std::cout << "version is " << version << std::endl;

  // Bytes 4-7 contain offset of first IFD
  int first_offset = Get32u(buffer+4);
  if (first_offset < 8 || first_offset > 16){
    printf("Warning: suspicious offset of first IFD value.\n");
  }
  std::cout << "first offset is " << first_offset << std::endl;

  int ifd_length = Get16u(buffer + first_offset);
  std::cout << "ifd length is " << ifd_length << std::endl;

  int tag = Get16u(buffer + first_offset + 2);
  std::cout << "tag is " << tag << std::endl;

  int type = Get16u(buffer + first_offset + 2 + 2);
  std::cout << "type is " << type << std::endl;

  int type2 = Get32u(buffer + first_offset + 2 + 2 + 2);
  std::cout << "type2 is " << type2 << std::endl;

  int wid = Get32u(buffer + first_offset + 2 + 2 + 2 + 4);
  std::cout << "wid is " << wid << std::endl;

//   int tag2 = Get16u(buffer + first_offset + 2 + 2 + 2 + 4);
//   std::cout << "tag2 is " << tag2 << std::endl;

//   int hgt = Get32u(buffer + first_offset + 2 + 2 + 4 + 2);
//   std::cout << "hgt is " << hgt << std::endl;

  return 0;
}

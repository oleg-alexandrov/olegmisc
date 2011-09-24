#include "boost/filesystem.hpp"   
#include <iostream>               
using namespace boost::filesystem;
using namespace std;
int main(){

  string path = "polyView";
  cout << "Is directory: " << path << " "
       << is_directory(path) << endl;

  path = "Makefile";
  cout << "Is directory: " << path << " "
       << is_directory(path) << endl;

  return 0;

}

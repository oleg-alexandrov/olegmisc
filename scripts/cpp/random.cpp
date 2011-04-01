#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

int main() {

  ifstream in("/dev/urandom", ios::in | ios::binary); 
  if(!in) { 
    cout << "Cannot open file.\n"; 
    return 1; 
  } 

  
  char n;

  if ( in.read((char *) &n, sizeof(n)) ){
    cout << "Read: " << (int)(n) << endl;
  }else{
    cout << "Could not read" << endl;
  }
  
  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

int main() {
 
  double * A;
  cout << "size is " << (char*)(A+1) - (char*)A << endl;
  
  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
using namespace std;

int main() {

  int k = sizeof(unsigned int);
  char * a = new char [k];

  for (int i = 0; i < k; i++) a[i] = (char)0;
  a[0] = (char)1;

  unsigned int v = *((unsigned int*)a);

  // Will return 1 in small endian, and 2^24=16777216
  // in big endian.
  cout << "v is " << v << endl;

}

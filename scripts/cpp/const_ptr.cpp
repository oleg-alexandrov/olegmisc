#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

int main() {

  char a[] = "hi";
  a[1] = 'z';

  double B = 1, C = 2;
  const double *b  = &B;
  double * const c = &C;

  cout << "*b and *c are " << *b << ' ' << *c << endl;

  b = &C;
  c[0] = 4;
  cout << "*b and *c are " << *b << ' ' << *c << endl;

  const double * const e = &C;
  
  return 0;
}

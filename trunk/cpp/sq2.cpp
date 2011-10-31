#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

template<int flag, int val, int n>
struct sq{
  static const int f = (val*val <= n) ? 1 : 0;
  static const int v = sq<f, (val + n/val)/2, n>::v;
};

template<int val, int n>
struct sq<1, val, n>{
  static const int f = 1;
  static const int v = val;
};

int main() {
 

  const int n = 8;
  cout << "sqrt of " << n << " is " << sq<0, n, n>::v << endl;
  
  return 0;
}

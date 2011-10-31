#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

template<int n>
struct sum{
  static double val(const double * x){
    return x[n-1] + sum<n-1>::val(x);
  }
};

template<>
struct sum<0>{
  static double val(const double * x){
    return 0;
  }
};


int main() {
 
  const double x[] = {3, 2, 7};
  const int n = sizeof(x)/sizeof(double);
  cout << "sum is " << sum<n>::val(x) << endl;
  
  return 0;
}

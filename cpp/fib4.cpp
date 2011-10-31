#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

template<int n>
struct fib{
  static const int value = fib<n-1>::value + fib<n-2>::value;
};

template<>
struct fib<0>{
  static const int value = 1;
};

template<>
struct fib<1>{
  static const int value = 1;
};


int main() {
  
  cout << "value is " << fib<5>::value << endl;
  return 0;
}

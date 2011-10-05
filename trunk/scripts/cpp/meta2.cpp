#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

template<int n>
int fib(){
  return fib<n-1>() + fib<n-2>();
}

template<>
int fib<1>(){
  return 1;
}

template<>
int fib<0>(){
  return 1;
}

template<int n>
void myfun(){
  cout << "n is " << n << endl;
  myfun<n-1>();
}

template<>
void myfun<0>(){
  cout << "base is " << 0 << endl;
}

int main() {

  const int n = 4;
  cout << "Value at " << n << " is " << fib<n>() << endl;
    
  return 0;
}

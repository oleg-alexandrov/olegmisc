#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
template<class T, int n>
struct dot{
  static T eval(T* a, T* b){
    return dot<T, n - 1>::eval(a, b) + *(a + n - 1) * *(b + n - 1);
  }
};
  
template<class T>
struct dot<T, 0>{
  static T eval(T* a, T* b){
    return 0;
  }
};


using namespace std;

int main() {

  double a[] = {3, 2, 1};
  double b[] = {1, 2, 5};
  const int n = sizeof(a)/sizeof(double);

  cout << "Dot value is " << dot<double, n>::eval(a, b) << endl;
  
  return 0;
}

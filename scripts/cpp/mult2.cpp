template <int a, int b, bool isNonNegative>
struct multAux{
  enum { value = multAux<a-1, b, isNonNegative>::value + b };
};
 
template <int b, bool isNonNegative>
struct multAux<0, b, isNonNegative>{
  enum { value = 0 };
};

template <int a, int b>
struct multAux<a, b, false>{
  enum { value = -multAux< -a, b, true >::value };
};

template <int a, int b>
struct mult{
  enum { value = multAux<a, b, (a >= 0)>::value };
};

#include <iostream>
using namespace std;

int main(){

  const int a = -3, b = 4;
  
  cout << "(" << a << ")" << "*" << "(" << b << ")" << "="
       << mult<a, b>::value << endl; 
  
  return 0;
}

template <int N>
struct fib{
    enum { value = fib<N-1>::value + fib<N-2>::value };
};
 
template <>
struct fib<0>{
    enum { value = 1 };
};

template <>
struct fib<1>{
    enum { value = 1 };
};

#include <iostream>
using namespace std;

int main(){

  const int n = 4;
  int fn = fib<n>::value;

  cout << "fib(" << n << ") = " << fn << endl;

  return 0;
}

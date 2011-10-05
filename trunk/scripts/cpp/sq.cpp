#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

template<int M, int n, int k>
struct sq{
  static const int val  = (n + M/n)/2;
  static const int low  = (n < val)? n : val;
  static const int high = (n > val)? n : val;
  static const int val2 = sq < M, val, high - low>::val2;
};

// template<int M, int n>
// struct sq<M, n, 1>{
//   enum { val = n };
// };
  
// template<int M, int n>
// struct sq<M, n, 0>{
//   enum { val = n };
// };

int main() {

  const int M = 9;
  cout << "Sqrt of " << M << " is " << sq<M, M, 0>::val << endl;

  return 0;
}

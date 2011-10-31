#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

template<class A, class B>
struct sum{
  sum(A a, B b){
    m_a = a;
    m_b = b;
  }
  inline double eval(int k){
    return m_a.eval(k) + m_b.eval(k);
  }
  
  A m_a;
  B m_b;
};

struct vec{
  vec(int n, double val = 0){ v.assign(n, val); }
  vector<double> v;
};
  
int main() {

  int n = 3;
  vec a(n, 2), b(n, 4), c(n);
  
  c = sum<vec, vec>(a, b);
  
  return 0;
}

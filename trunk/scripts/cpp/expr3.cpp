#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

class lit{
public:
  lit(double v): m_v(v){}
  inline double eval(){
    return m_v;
  }
private:
  double m_v;
};

template<class A, class B, class Op>
class expr{
public:
  expr(A & a, B & b, Op & op):m_a(a), m_b(b), m_op(op){}
  inline double eval(){
    return m_op(m_a.eval(), m_b.eval());
  }
private:
  A  & m_a;
  B  & m_b;
  Op & m_op;
};


plus<double> p;

template<class T1, class T2>
expr< T1, T2, plus<double> >operator+(T1 t1, T2 t2){
  return expr< T1, T2, plus<double> >( t1, t2, p );
}


int main() {

  lit a(3), b(4), c(5);
  cout << "Value is " << (a + b + c).eval() << endl;

  return 0;
}

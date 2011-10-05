#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

template<class X, class Y, class Op>
struct expression{
  expression(){}
  expression(X & x, Y & y, Op & op):m_x(x), m_y(y), m_op(op){}
public:
  X  m_x;
  Y  m_y;
  Op m_op;
};

int main() {

  plus<double> add;
  expression< double, double, plus<double> > E;

  return 0;
}

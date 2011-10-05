#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

struct op{

  op& operator=(const op& t){
    x = t.x;
    cout << "now in =, val is " << x << endl;
    cout << "ptr is " << this << endl;
    return *this;
  }

  op & operator+(const op & t){
    x += t.x;
    cout << "now in +, val is " << x << endl;
    cout << "ptr is " << this << endl;
    return *this;
  }
  
  double x;
};

int main() {
 

  op a, b, c;
  a.x = 1;
  b.x = 6;
  c.x = 3;

  op t;
  t = a + b + c;

  cout << "Sum is " << t.x << endl;
  cout << "ptr to t, a, b, c are " << &t << ' ' << &a << ' ' << &b << ' ' << &c
       << endl;
  
  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include "boost/scoped_ptr.hpp"
#include "boost/scoped_array.hpp"
#include "boost/shared_ptr.hpp"
using namespace std;
using namespace boost;

int main() {

  // Scoped ptr
  scoped_ptr<string> p(new string("Hello"));
  cout << "value is " << p->c_str() << endl;
  *p = "Bye now";
  cout << "Value is " << p->c_str() << endl;
  // Gets de-allocated automatically
  // Cannot be copied
  // scoped_ptr<string> q(p);

  // Scoped array
  scoped_array<int> a(new int[100]);
  a[0] = 10;
  cout << "value is " << a[0] << endl;

  // Shared ptr can be copied. It uses reference counting.
  // It can be put in containers.
  shared_ptr<int> b(new int);
  *b = 7;
  cout << "Value of b is " << *b << endl;
  shared_ptr<int> c = b;
  cout << "Value of c is " << *c << endl;
  vector< shared_ptr<int> > V;
  V.clear();
  V.push_back(c);
  cout << "Value of V is " << *(V[0]) << endl;
  cout << "is the pointer NULL? " << (b == NULL) << endl;
  
  return 0;
}

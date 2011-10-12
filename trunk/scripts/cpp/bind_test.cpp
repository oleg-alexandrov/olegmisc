#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include "boost/bind.hpp"
using namespace std;
using namespace boost;

struct d{
public:
  d(double v_in):v(v_in){}
  double v;
  void val(){
    cout << "v is " << v << endl;
  }
};

struct e{
public:
  e(double v_in):v(v_in){}
  double v;
  void val2(int q){
    cout << "v is " << v + q<< endl;
  }
};


double f(int x, int y){
  return x + y;
}

int main() {

  vector<d> S;
  S.clear();
  S.push_back(d(5));
  S.push_back(d(4));
  S.push_back(d(2));

  for (vector<d>::iterator i = S.begin(); i != S.end(); ++i){
    i->val();
  }

  cout << endl;

  // Notice that _1 here is 'this'
  cout << "version 2" << endl;
  for_each( S.begin(), S.end(), bind(&d::val, _1) );

  cout << endl;
  cout << "value is " << bind(f, _1, 7)(2) << endl; // Same as f(2, 7)

  cout << endl;
  vector<e> T;
  T.clear();
  T.push_back(e(5));
  T.push_back(e(4));
  T.push_back(e(2));


  cout << "version 1" << endl;
  for (vector<e>::iterator i = T.begin(); i != T.end(); ++i){
    i->val2(9);
  }

  // Two arguments, the first being this
  cout << endl;
  cout << "version 2" << endl;
  for_each(T.begin(), T.end(), bind(&e::val2, _1, 9)); // same as i->val(9)
    
  return 0;
}

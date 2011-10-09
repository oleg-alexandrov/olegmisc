#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include <boost/any.hpp>
using namespace std;
using namespace boost;

int main() {

  any a = 5;
  any b = string("hi there");

  vector<any> vals;
  vals.clear();
  vals.push_back(a);
  vals.push_back(b);
  
  int *ptr_a = any_cast<int>(&a);
  if (ptr_a){
    cout << "value is " << *ptr_a << endl;
  }
  
  int *ptr_b = any_cast<int>(&b);
  if (ptr_b){
    cout << "int value is " << *ptr_b << endl;
  }else{
    cout << "b is not an int" << endl;
  }
  
  string *ptr_b2 = any_cast<string>(&b);
  if (ptr_b2){
    cout << "string value is " << *ptr_b2 << endl;
  }

  return 0;
}

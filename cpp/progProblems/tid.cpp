#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include <typeinfo>
using namespace std;

class Az{

};

class Wt: public Az{

};

int main() {

  Az q;
  cout << "Type id is " << typeid(q).name() << endl;

  Wt s;
  cout << "Type id is " << typeid(s).name() << endl;
  
  return 0;
}

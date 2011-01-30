#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
using namespace std;

int main() {

  int a = 4, b = 7;
  cout << "a and b are " << a << ' ' << b << endl;

  a = a + b; // a + b, b
  b = a - b; // a + b, a
  a = a - b; // b, a

  cout << "a and b are " << a << ' ' << b << endl;
  

  a = a ^ b; // a ^ b, b
  b = a ^ b; // a ^ b, a
  a = a ^ b; // b,     a

  cout << "a and b are " << a << ' ' << b << endl;
  
}

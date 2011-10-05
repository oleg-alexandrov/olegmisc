int mult(int a, int b){

  if (a == 0) return 0;
    
  if (a < 0) return -mult(-a, b);

  int sum = 0;
  for (int s = 0; s < a; s++) sum += b;
  return sum;
}

#include <iostream>
using namespace std;
int main() {

  const int a = -3, b = 4;
  
  cout << "(" << a << ")" << "*" << "(" << b << ")" << "="
       << mult(a, b) << endl; 

  return 0;
}

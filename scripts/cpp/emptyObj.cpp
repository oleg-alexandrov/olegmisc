#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

class emptyClass{};

int main() {
 
  emptyClass A, B;
  vector<emptyClass> C, D;
  C.resize(100);
  D.reserve(100);
  
  
  cout << "values are " << int(*((char*)(&A))) << endl;
  cout << "values are " << int(*((char*)(&B))) << endl;

  cout << "Print C" << endl;
  for (int s = 0; s < (int)C.size(); s++){
    cout << int(*((char*)(&C[s]))) << endl;
  }
  
  cout << "Print D" << endl;
  for (int s = 0; s < 10; s++){
    cout << int(*((char*)(&D[s]))) << endl;
  }

  cout << "B - A is " << (&B) - (&A) << endl;

  for (int s = 0; s < (int)C.size(); s++){
    cout << "Val is " << (&C[s]) - (&C[0]) << endl;
  }
  
  emptyClass E;

  cout << "E - A is " << (&E) - (&A) << endl;
  cout << "E - B is " << (&E) - (&B) << endl;

  cout << "E - C is " << (int)(&E) - (int)(&C[0]) << endl;
  cout << "E - pC is " << (int)(&E) - (int)(&C) << endl;
  
  return 0;
}

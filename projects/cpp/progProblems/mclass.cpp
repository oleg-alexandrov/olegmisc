#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
using namespace std;

class mp {
public:
  mp(int xin){
    cout << "Now in the constructor" << endl;
    x = xin;
  }

  ~mp(){
    cout << "Now in the destructor" << endl;
  }

  mp& operator=(const mp& A){

    cout << "now in assignmment" << endl;
    if (this == &A){
      return *this;
    }

    x = 0;
    x = A.x;
    return *this;
  }

  mp(const mp &A ){
    cout << "now in copy constructor" << endl;
    //this->operator=(A);
    cout << "-- will now copy" << endl;
    *this = A;
    cout << "-- done with copying" << endl;
    return;
  }
      
public:
  mp(){}
  int x;
};
  
int main() {

  mp v(1);

  mp w = v;

  mp z = v;
  z = w;

  z = z;

  cout << "z val is " << z.x << endl;
  
  return 0;
}

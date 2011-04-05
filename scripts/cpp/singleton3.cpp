#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

class singleton{

private:

  singleton(){
    cout << "-- Will init singleton" << endl;
  }

  singleton(const singleton &){}
  singleton & operator=(const singleton & ){}
  
public:
  
  static singleton * getInstance() {
    static singleton * instance = NULL;
    if (instance == NULL) instance = new singleton;
    return instance;
  }
  
};
  
int main() {
 

  cout << "Getting first instance" << endl;
  singleton * A = singleton::getInstance();
  cout << "Answer is " << A << endl;
  
  cout << "Getting second instance" << endl;
  singleton *B = singleton::getInstance();
  cout << "Answer is " << B << endl;
  
  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
using namespace std;

class singleton{
public:

  static singleton * get_singleton(){
    if (m_instance == NULL){
      cout << "Now will create a new singleton" << endl;
      m_instance = new singleton;
    }
    return m_instance;
  }

  ~singleton(){
    cout << "now in the destructor" << endl;
    m_instance = NULL;
  }
  
private:

  singleton(){
    cout << "now in the constructor" << endl;
    m_instance = NULL;
  }
  
  singleton & operator=(const singleton& A);
  singleton(const singleton & A);
  
  static singleton * m_instance;

};

singleton* singleton::m_instance = NULL;

int main() {

  singleton *A = singleton::get_singleton();
  
  singleton *B = singleton::get_singleton();

  
  cout << "singletons are " << A << ' ' << B << endl;

  delete A;
  //delete B;
  
}

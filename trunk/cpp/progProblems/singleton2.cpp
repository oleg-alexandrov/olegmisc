#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

class single{

public:

  static single * get_single(){

    cout << "Now in get_single" << endl;
    static int count = 0;
    count++;

    if (count == 1){ // First time in this function
      m_ptr = new single;
    }

    cout << "Returning " << m_ptr << endl;
    
    return m_ptr;
  }
  
private:
  
  static single * m_ptr;
  int    m_val;
  single(){
    cout << "Now in the single constructor" << endl;
    m_val = 0;
  }
  
};

single* single::m_ptr = NULL;

int main() {

  single * s = single::get_single();
  single * t = single::get_single();

  cout << "We are getting the pointers: " << s << ' ' << t << endl;

  return 0;
  
}

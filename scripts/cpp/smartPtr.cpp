#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

class smartPtr{

public:

  smartPtr(int* ptr = NULL){
    m_numRefs  = new int;
    *m_numRefs = 1;
    m_ptr      = ptr;
    cout << "Now in the constructor, num of refs is " << *m_numRefs << endl;
  }

  smartPtr(smartPtr & P){
    m_numRefs = P.m_numRefs;
    (*m_numRefs)++;
    m_ptr = P.m_ptr;
    cout << "Now in the copy constructor, num refs is " << *m_numRefs << endl;
  }

  smartPtr & operator=(smartPtr & P){
    m_numRefs = P.m_numRefs;
    (*m_numRefs)++;
    m_ptr = P.m_ptr;
    cout << "Now in the assignment operator, numRefs is "
         << *m_numRefs << endl;
    return *this;
  }


  ~smartPtr(){
    (*m_numRefs)--;
    cout << "Now in the destructor, num refs is " << *m_numRefs << endl;
    if (*m_numRefs == 0){
      cout << "Wiping the object" << endl;
      delete m_numRefs;
      delete [] m_ptr;
    }
    
  }
  
private:
  int * m_numRefs;
  int   * m_ptr;
};

void allocAndReturn(int n, smartPtr & W){
  int * a = new int[n];
  smartPtr Q(a);
  W = Q;
}

int main() {

  int n = 4;

  smartPtr Q(NULL);

  cout << "Now will copy" << endl;
  smartPtr T = Q;
  cout << "Done" << endl;

  smartPtr L;
  L = T; 
  
  cout << "----Now will alloc" << endl;
  smartPtr W;
  allocAndReturn(n, W);
  cout << "----Done allocating " << endl;

  {
    cout << "Now will copy in block" << endl;
    smartPtr R = T;
    cout << "Done copying in block" << endl;
  }

  cout << "Now will exit" << endl;
  return 0;
}

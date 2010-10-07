#include <iostream>
#include <vector>
using namespace std;

class A {

public:
  void myfun1(int z) { cout << "A fun 1" << endl;  }
  int myfun1(int w, int q)  { cout << "A fun 2 " << endl; return 0; }
  
};

class B: public A{
public:
  void myfun1(int z) { cout << "B fun 1" << endl;  }
};

int main()
{

  A a;
  cout << "Printing A functions" << endl;
  a.myfun1(2);
  a.myfun1(3, 3);

  // This should not compile due to name-hiding
  B b;
  cout << "Printing B functions" << endl;
  b.myfun1(2);
  b.myfun1(3, 3);

}

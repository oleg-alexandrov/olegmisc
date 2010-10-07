#include <iostream>
#include <vector>
using namespace std;

class A{
  double q;
  double z;
};

class B{
public:

  double t;
  int l;
  bool s;
  virtual void myfun(){
    cout << "B" << endl;
  }
  ~B(){;}
};

class C: public B{
public:

  virtual void myfun(){
    cout << "C" << endl;
  }
  
};
  
class D: public C{
public:
  virtual void myfun(){
    cout << "D" << endl;
  }
  
};

class E: public D{
public:
  void myfun(){
    cout << "E" << endl;
  }
  
};

int main(){

  int a = 2;

  cout << "size of int* is " << sizeof(&a) << endl;
  cout << "size of A* is   " << sizeof(A*) << endl;
  cout << "size of B* is   " << sizeof(B*) << endl;
  cout << "size of C* is   " << sizeof(C*) << endl;


  B* b = new B; cout << "B fun is "; b->myfun();
  C* c = new C; cout << "C fun is "; c->myfun();
  C* d = new D; cout << "D fun is "; d->myfun();
  D* e = new E; cout << "E fun is "; e->myfun();

  cout << "size of B is " << sizeof(B) << endl;
  cout << "size of C is " << sizeof(C) << endl;
  cout << "size of D is " << sizeof(D) << endl;
  cout << "size of E is " << sizeof(E) << endl;

}

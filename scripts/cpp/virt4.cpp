#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
using namespace std;

class A {
private:
       long num1;
};

class B : public A {
private:
       void f1();
};

class C : public B {
public:
       virtual void f2();
};

class D : public C {
public:
       void f3();
       virtual void f2();
       virtual void f4();
       int num2;
};

class Q{
  virtual ~Q();
};

class R{
  virtual ~R();
};

class E: private D{
  void f3();
  virtual void f2();
  virtual void f4();
  virtual void f5();
  virtual void f6(int q, int r, int s);
  virtual ~E();
  E();
};

class F: public E, public Q, public R{

};

int main()
{
  cout << "sizeof(A) = " << sizeof(A) << ' ' << sizeof(long) << endl;
  cout << "sizeof(B) = " << sizeof(B) << ' ' << sizeof(long) << endl;
  cout << "sizeof(C) = " << sizeof(C) << ' '
       << sizeof(long) + sizeof(void*) << endl;
  cout << "sizeof(D) = " << sizeof(D) << ' '
       << sizeof(long) + sizeof(void*)+sizeof(int) << endl;
  
  cout << "sizeof(E) = " << sizeof(E) << ' '
       << sizeof(long) + sizeof(void*)+sizeof(int) << endl;
  
  cout << "sizeof(F) = " << sizeof(F) << ' '
       << sizeof(long) + 3*sizeof(void*) + sizeof(int) << endl;

  return 0;
}

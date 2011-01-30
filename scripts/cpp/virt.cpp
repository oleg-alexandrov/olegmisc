#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
using namespace std;

class A
{
public:
  void foo() {cout<<"a\n";}
};

class B: public A
{
public:
  void foo() {cout<<"b\n";}
};

class C: public B
{
public:
  void foo() {cout<<"c\n";}
};

int main(char *argv[], int argc)
{
 C *c = new C;
 B *b = c;
 A *a = b;
 a->foo();
 b->foo();
 c->foo();
}


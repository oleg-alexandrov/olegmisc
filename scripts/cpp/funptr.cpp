#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;


class A{
public:
  double myfun(int x){ return x*x/2.0;}
  double myfun2(int x){

    double (A::*f1)(int s) = &A::myfun;
    double (A::*f2)(int s) = &A::myfun2;
    cout << "+ size is " << sizeof(f1) << ' ' << sizeof(f2) << endl;
    cout << "ptrs are: " << (void*)f1 << ' ' << (void*)f2 << endl;
    cout << "diff is: " << (int*)(void*)f1 - (int*)(void*)f2 << endl;
    
    return x*4;
  }
};

int g(int s){
  return s;
}

std::vector<double> g3(int q){
  vector<double> a;
  a.push_back(q);
  return a;
}

int gg(int s){
  return s * s;
}


int main() {

  int b = 9;
  int * a = &b;
  int * c = new int;
  //  double (*f)(int s) = A::myfun;
  
//   f = &g;  cout << "answer is " << f << " " << (*f)(5) << endl;
//   f = &gg; cout << "answer is " << f << " " << (*f)(5) << endl;
//   f = &g;  cout << "answer is " << &g << " " << g(5) << endl;
//   cout << "---answer is " << (void*)gg << " " << (void*)(*gg) << endl;
//   cout << "diff is " << (int*)((void*)gg) - (int*)a << endl;
//   cout << "diff is " << (int*)((void*)gg) - (int*)c << endl;
//   cout << "diff is " << (int*)((void*)gg) - (int*)((void*)(g)) << endl;
//   cout << "diff is " << (int*)((void*)gg) - (int*)((void*)(g3)) << endl;

//   cout << "val and size is " << (void*)f << ' ' << sizeof(f) << endl;

  A q;
  q.myfun2(8);
  
  //cout << "sizes are " << sizeof(a) << ' ' << sizeof(f) << endl;
  //cout << "vals are: " << a << ' ' << f << endl;
  //cout << "c - a is " << c - a << endl;
  return 0;
}

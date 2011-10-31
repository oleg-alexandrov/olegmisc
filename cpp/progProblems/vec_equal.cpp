#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <cassert>
#include <map>
#include <vector>
using namespace std;

int main(){

  vector<double> A; A.push_back(-4); A.push_back(3);
  vector<double> B; B.push_back(-4); B.push_back(3); B.push_back(8);
  vector<double> C; C.push_back(-4); C.push_back(3); C.push_back(8);
  vector<double> D; D.push_back( 4); D.push_back(3); D.push_back(8);

  cout << "size of int"        << sizeof(int)       << endl;
  cout << "size of long "      << sizeof(long)      << endl;
  cout << "size of long long " << sizeof(long long) << endl;
  cout << "size of double: "   << sizeof(double)    << endl;
  
  cout << "A == A 1: " << (A == A) << endl;
  cout << "A == B 0: " << (A == B) << endl;
  cout << "B == A 0: " << (B == A) << endl;
  cout << "B == C 1: " << (B == C) << endl;
  cout << "C == D 0: " << (C == D) << endl;
  
  //mymap.insert ( pair<char,int>('a',100) );

}

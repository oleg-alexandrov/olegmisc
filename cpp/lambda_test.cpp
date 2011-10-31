#include <iostream>
#include <vector>
#include "boost/lambda/lambda.hpp"
#include "boost/function.hpp"
#include "boost/bind.hpp"
using namespace std;
using namespace boost;
//using namespace boost;

double f2(int x, int y){
  return x + y;
}

int main() {

  vector<double> a;
  a.push_back(3);
  a.push_back(2);
  a.push_back(4);

  cout << "original values" << endl;
  for_each( a.begin(), a.end(), std::cout << lambda::_1 << "\n" );
  for_each( a.begin(), a.end(), lambda::_1 += 10 );
  cout << "added values" << endl;
  for_each( a.begin(), a.end(), std::cout << lambda::_1 << "\n" );

  cout << "value is " << (lambda::_1 * lambda::_1 + lambda::_2)(3, 4) << endl;
  
  boost::function<int(int,int)> f = lambda::_1 * lambda::_1 + lambda::_2;
  cout << "value 2 is " << f(3, 4) << endl;
  
  boost::function<int(int,int)> g = lambda::_1 - lambda::_2;

  vector< boost::function<int(int,int)> > V;
  V.push_back(f);
  V.push_back(g);

  for (int s = 0; s < (int)V.size(); s++){
    cout << "value is " << V[s](3, 4) << endl;
  }

  cout << "value before is " << f(3, 4) << endl;
  cout << "pointers before: " << &f << ' ' << &g << endl;
  f = g;
  cout << "value after is " << f(3, 4) << endl;
  cout << "pointers after: " << &f << ' ' << &g << endl;
  
  //(std::cout << lambda::_1 << "\n")("xuxa");
  //(std::cout << lambda::_1 << std::endl)("xuxa");
//   (std::cout << lambda::_1 << " " << _3 << " " << lambda::_2 << "!\n")
//     ("Hello","friend","my");


  // Mixing bind and lambda
  boost::function<int(int,int)> h = lambda::_1 + lambda::_2;

  cout << "value 1 is " << bind(f2, _1, 7)(2) << endl; // Same as f(2, 7)
  cout << "value 2 is " << bind(h,  _1, 7)(2) << endl; // Same as f(2, 7)
  
  boost::function<int(int)>  k = bind(h, _1, 7);
  cout << "value 3 is " << k(2) << endl;

  //boost::function<int(int,int)>  k2 = function<int(int,int)>(h, lambda::_1, lambda::_2);
  
  return 0;
}

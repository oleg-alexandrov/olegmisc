#include <iostream>
#include <vector>
#include "boost/lambda/lambda.hpp"
#include "boost/function.hpp"
using namespace std;
using namespace boost::lambda;

int main() {

  vector<double> a;
  a.push_back(3);
  a.push_back(2);
  a.push_back(4);

  for_each( a.begin(), a.end(), std::cout << _1 << "\n" );

  //(std::cout << _1 << "\n")("xuxa");
  //(std::cout << _1 << std::endl)("xuxa");
//   (std::cout << _1 << " " << _3 << " " << _2 << "!\n")
//     ("Hello","friend","my");


    
  return 0;
}

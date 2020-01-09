#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include "boost/utility/enable_if.hpp"
#include "boost/type_traits.hpp"
#include "boost/type_traits/is_integral.hpp"
#include "boost/type_traits/is_float.hpp"

using namespace std;
using namespace boost;

template <typename T>
struct extract_caller
{
    static void extract(const T& val, typename boost::enable_if< boost::is_integral<T> >::type* p=0)
    {
      std::cout << "integral" << std::endl;
      //extractor::extract(val);
    }

    static void extract(const T& val, typename boost::enable_if< boost::is_float<T> >::type* p=0)
    {
      std::cout << "double" << std::endl;
      //extract_generic(val);
    }
};


int main() {

  extract_caller<int> A; A.extract(2);

//   A.extract(2.0);
//   A.extract(2);
  
//   int i=11;
//   double s=12;
  
//   some_func(i);
//   some_func(s);
  //  some_func(some_class());

  return 0;
}

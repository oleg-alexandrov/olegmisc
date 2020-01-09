#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

int main()
{
  int a[] = {5, 6, 4, 3, 2, 6, 7, 9, 3};

  std::vector<int> v;
  for (size_t i = 0; i < sizeof(a)/sizeof(int); i++) {
    v.push_back(a[i]);
    std::cout << "--adding " << a[i] << std::endl;
  }

  int len = v.size();
  std::nth_element(v.begin(), v.begin() + len, v.end());
  std::cout << "The median is " << v[len] << '\n';

//   std::nth_element(v.begin(), v.begin()+1, v.end(), std::greater<int>());
//   std::cout << "The second largest element is " << v[1] << '\n';
}

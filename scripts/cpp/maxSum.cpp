#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
using namespace std;

int main() {

  //double a[] = {31, -41, 59, 26, -53, 58, 97, -93, -23, 84};
  //double a[] = {31, 41, 59, 26, 53, 58, 97, 93, 23, 84};
  double a[] = {-31, -41, -59, -26, -53, -58, -97, -93, -23, -84};

  int bestMinIndex = -1, bestMaxIndex = 0, minSumIndex = -1;
  double bestDiff = a[0], minSum = 0, currSum = 0;

  int n = sizeof(a)/sizeof(double);
  
  for (int s = 0; s < n; s++){
    currSum += a[s];

    if (currSum - minSum > bestDiff){
      bestDiff = currSum - minSum;
      bestMinIndex = minSumIndex;
      bestMaxIndex = s;
    }

    if (currSum < minSum){
      minSum = currSum;
      minSumIndex = s;
    }
    
  }
  
  cout << "There are " << n << " elements" << endl;
  cout << "The best diff is " << bestDiff << endl;
  cout << "It goes from " << bestMinIndex + 1 << " to " << bestMaxIndex
       << endl;
  
}

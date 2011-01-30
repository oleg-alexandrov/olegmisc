#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
using namespace std;

// Find the contiguous subsequence with the largest sum
int main() {

  double a[] = {2, -8, 3, -2, 4, -10};

  int n = sizeof(a)/sizeof(a[0]);

  vector<double> sums;
  sums.resize(n+1);

  // Find the partial sums
  double sum = 0;
  for (int s = 0; s < n; s++){
    sum += a[s];
    sums[s + 1] = sum;
  }


  // Find the max sum
  double maxSum = 0.0; int maxIndex = 0;
  for (int s = 1; s < n + 1; s++){
    if (sums[s] >= maxSum || s == 1){
      maxIndex = s;
      maxSum = sums[s];
    }
  }

  
  //  Find the min sum
  double minSum = 0.0; int minIndex = 0;
  for (int s = 0; s < maxIndex; s++){
    if (sums[s] < minSum){
      minIndex = s;
      minSum = sums[s];
    }
  }

  double maxSubSeqSum = sums[maxIndex] - sums[minIndex];
  cout << "the max sum is " << maxSubSeqSum << endl;
  cout << "Its elems are: ";
  for (int s = minIndex + 1; s < maxIndex; s++){
    cout << a[s] << " ";
  }
  cout << endl;
  
  return 0;
}

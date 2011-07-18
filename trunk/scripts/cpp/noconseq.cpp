#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

void print_nonconseq(int * a, int n, int k){

  assert(n >= 0 && k >= -1);

  if (k >= n - 1){
    for (int s = 0; s < n; s++) cout << a[s] << " ";
    cout << endl;
    return;
  }

  assert(n > 0);
  k++;

  a[k] = 0;
  print_nonconseq(a, n, k);

  if (k >= 1 && a[k - 1] == 1) return;
  a[k] = 1;
  print_nonconseq(a, n, k);
    
}


int main() {
 
  int n = 4, a[n];
  print_nonconseq(a, n, -1);
  
  return 0;
}

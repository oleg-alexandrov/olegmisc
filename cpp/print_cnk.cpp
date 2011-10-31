#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

// Print all subsets of k elements of a set of n elements

void print_arr(const int * a, int n){
  cout << "[ ";
  for (int s = 0; s < n; s++) cout << a[s] << " ";
  cout << "]" << endl;
  return;
}

void print_cnk(int n, int k){

  assert(0 <= k && k <= n);

  cout << "n and k are " << n << ' ' << k << endl;
  
  if (k == 0 || n == 0){
    print_arr(NULL, 0);
    return;
  }

  int a[k];
  for (int s = 0; s < k; s++) a[s] = s; // starting array

  int iter  = 1;
  while (1){

    cout << iter << ": ";
    iter++;
    
    print_arr(a, k);

    int pos = k - 1;
    for (int s = 0; s < k - 1; s++){
      if (a[s] + 1 < a[s + 1]){
        pos = s;
        break;
      }
    }

    a[pos]++;
    for (int s = 0; s < pos; s++) a[s] = s;
      
    if (a[pos] >= n) break;
    
  }
  
  return;  
}

int main() {

  cout << "enter k and n" << endl;
  int n, k;
  while (1){
    cin >> k >> n;
    print_cnk(n, k);
    cout << endl;
  }
  return 0;
}

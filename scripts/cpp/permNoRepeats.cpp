#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include <set>
using namespace std;

void printPerm(int s, int prev_i, int n, std::set<int> & I,
               int * P, int * A){

  set<int> C = I;
  
  for (set<int>::iterator k = C.begin(); k != C.end(); k++){
    
    int i = *k;
    
    if (s > 0 && P[prev_i] == A[s] && prev_i >= i){
      continue;
    }
    P[i] = A[s];

    if (s == n - 1){
      static int l = 0;
      l++;
      cout << l << ": ";
      for (int t = 0; t < n; t++) cout << P[t] << " ";
      cout << endl;
      return;
    }

    I.erase(i);
    printPerm(s + 1, i, n, I, P, A);
    I.insert(i);
    
  }
  
}

int main() {
 
  int A[] = {5, 4, 4, 5, 7};
  int n = sizeof(A)/sizeof(int);
  sort(A, A + n);
  
  int P[n];
  
  set<int> I;
  for (int i = 0; i < n; i++) I.insert(i);
    
  printPerm(0, -1, n, I, P, A);

  return 0;
}

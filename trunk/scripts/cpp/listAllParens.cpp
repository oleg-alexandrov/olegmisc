#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

void printParens(int p, int q, int n, char * A){

  assert(p <= n && q <= n);
  
  if (p == n && q == n){
    for (int s = 0; s < 2*n; s++) cout << A[s] << "";
    cout << endl;
    return;
  }

  if (p + 1 <= n){
    A[p + q] = '(';
    printParens(p + 1, q, n, A);
  }

  if (q + 1 <= p){
    A[p + q] = ')';
    printParens(p, q + 1, n, A);
  }
  
  return;
}

int main() {

  int n = 3;
  char A[2*n];
  printParens(0, 0, n, A);

  return 0;
}

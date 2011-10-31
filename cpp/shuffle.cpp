#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <ctime>
using namespace std;

int randn(int n){
  // Return an integer in [0, n)
  assert(n >= 1);
  return rand()%n;
}

int main(){

  srand ( time(NULL) );

  int n = 10;
  
  cout << "val is " << randn(n) << endl;
  int A[n];
  for (int i = 0; i < n; i++) A[i] = i;

  cout << "before: " << endl;
  for (int i = 0; i < n; i++) cout << A[i] << " ";
  cout << endl;

  for (int i = 0; i < n - 1; i++){
    int j = i + 1 + randn(n - 1 - i);
    assert( i + 1 <= j && j < n);
    swap(A[i], A[j]);
  }
  
  cout << "after: " << endl;
  for (int i = 0; i < n; i++) cout << A[i] << " ";
  cout << endl;

  return 0;
}

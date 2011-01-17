#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

#define NDEBUG (1)

int randn(int n){
  return rand()%n;
}

void mqsort(int * a, int n){

  if (n <= 1) return;

  int pi = randn(n);
  int p  = a[pi]; // pivot
  int i = 0, j = 0, k = 0; // find i < j with a[i] > p >= a[j]

  bool successi = false;

  for (i = 0; i < n; i++){

    if (a[i] <= p) continue;
    successi = true;
    k = i;
    
    bool successj = false;
    for (j  = max(i, j) + 1; j < n; j++){
      if (a[j] > p) continue;
      successj = true;
      swap(a[i], a[j]);
      break;
    }

    if (!successj) break;
    assert(i + 1 < n);
    k = i + 1;
    
  }

  if (successi){
    
    assert(0 < k && k < n);
    for (int s = 0; s < k; s++) assert( a[s] <= p);
    for (int s = k; s < n; s++) assert (a[s] >  p);
    mqsort(a,     k);
    mqsort(a + k, n - k);
    
  }else{
    
    assert(a[pi] == p);
    swap(a[pi], a[n-1]);
    mqsort(a, n - 1);
    
  }

}

#define DO_DEBUG (0)
  
int main() {

  srand ( time(NULL) );

#ifndef NDEBUG
  cout << "Assertions are on" << endl;
  //assert(false);
#else
  cout << "Assertions are off" << endl;
#endif
  
  int n = 7000;//0*2*2*2*2;//*2*2*2;
  int num = 5000; // Number of times to repeat the test
  int a[n], b[n], c[n];
  for (int s = 0; s < n; s++) {
    a[s] = randn(n/160/2);
    b[s] = a[s];
    c[s] = a[s];
  }

#if DO_DEBUG
  cout << "Before sort: " << endl;
  for (int s = 0; s < n; s++) cout << a[s] << " ";
  cout << endl;
#endif

  mqsort(a, n);
  std::sort(b, b + n);

#if DO_DEBUG
  cout << "After sort: " << endl;
  for (int s = 0; s < n; s++) cout << a[s] << " ";
  cout << endl;
#endif

  for (int s = 0; s < n; s++){
    if (a[s] != b[s]){
      cout << "Error: " << s << ' ' << a[s] << ' ' << b[s] << endl;
      exit(1);
    }
  }
  

  clock_t start, finish;

  // qsort
  start = clock();
  for (int q = 0; q < num; q++){
    for (int s = 0; s < n; s++) a[s] = c[s];
    mqsort(a, n);
  }
  finish = clock();

  double t1 = ( (finish - start)/CLOCKS_PER_SEC );

  // std sort
  start = clock();
  for (int q = 0; q < num; q++){
    for (int s = 0; s < n; s++) a[s] = c[s];
    std::sort(a, a + n);
  }
  finish = clock();

  double t2 = ( (finish - start)/CLOCKS_PER_SEC );

  cout << "n is "                         << n     << endl;
  cout << "Elapsed time for qsort is "    << t1    << endl;
  cout << "Elapsed time for std sort is " << t2    << endl;
  cout << endl;
  cout << "qsort/std::sort time is "      << t1/t2 << endl;

  return 0;
}

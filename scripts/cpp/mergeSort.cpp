#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

void mergeLists(int * a1, int l1, int * a2, int l2, int * w) {

  int i1 = 0, i2 = 0, i = 0;

  while (i1 < l1 || i2 < l2){

    if (i1 == l1){ w[i] = a2[i2]; i++; i2++; continue; }
    if (i2 == l2){ w[i] = a1[i1]; i++; i1++; continue; }


    assert(i1 < l1 && i2 < l2);

    if   (a1[i1] < a2[i2]){ w[i] = a1[i1]; i1++; i++; }
    else                  { w[i] = a2[i2]; i2++; i++; }
  }
  
}

void mergeSort(int * a, int n, int * w){

  assert(a != w);
  int * la = a;
  int * lw = w;
  
  int bs = 1;
  
  while (1){

    // Merge blocks of size at most bs into blocks of size at most 2*bs

    for (int k = 0; true; k++){
      
      // Now work at position 2*k*bs
      int beg1 = min(2*k*bs, n);
      if (beg1 >= n) break;

      int end1 = min(beg1 + bs, n);
      int beg2 = end1;
      int end2 = min(beg2 + bs, n);

      // Merge the blocks [beg1, end1) and [beg2, end2)
      mergeLists(la + beg1, end1 - beg1,
                 la + beg2, end2 - beg2,
                 lw + beg1);
    }
    
    bs*= 2;
    if (bs >= n) break;
    swap(la, lw);
    
  }
  
  if (lw == w){
    for (int s = 0; s < n; s++) a[s] = w[s];
  }

  return;
}

int main(){

  const int n = 1;
  int a[n], w[n];
  for (int s = 0; s < n; s++) a[s] = n-s;
  
  for (int s = 0; s < n; s++) cout << a[s] << " ";
  cout << endl;

  mergeSort(a, n, w);

  for (int s = 0; s < n; s++) cout << a[s] << " ";
  cout << endl;

  return 0;
  
}

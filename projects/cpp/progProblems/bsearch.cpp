#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

int randn(int n){
  return rand()%max(n, 1);
}

int naive_search(int * a, int n, int val){

 for (int s = 0; s < n; s++){
    if (a[s] == val) return s;
 }
 return -1;

}

int bsearch(int * a, int n, int val){

  int ans = -1;
  if (n <= 0) return ans;

  int beg = 0, end = n;
  int levels = 0;
  
  while(1){

    levels++;
    
    if (end == beg + 1){
      if ( a[beg] == val ){
        assert( beg == 0 || a[beg - 1] < val );
        ans = beg;
        break;
      }	else {
        ans = -1;
        break;
      }
    }

    int mid = (beg + end)/2;
    assert(beg < mid && mid < end);
    if      (a[mid] < val) beg = mid;
    else if (a[mid] > val) end = mid;
    else{
      if (a[mid - 1] < val){
        ans = mid;
        break;
      }else{
        assert( a[mid - 1] == val );
        end = mid;
      }
    }
    
  }
  

  int mx = ceil(log(max(n, 1))/log(2)) + 1;

  if (levels > mx){
    cout << "\nn and levels are: " << n << ' '
         << levels  << ' ' << mx << endl;
    ans = -2;
  }
  
  return ans;
  
}


int main(){

  srand ( time(NULL) );

  int k = 2000;

  for (int q = 0; q < 1000; q++){
    
    int n = randn(k) - 3, m = randn(k) - 3;
    int val = randn(m + 4) - 3;

    int a[max(n, 1)];
    
    for (int s = 0; s < n; s++) a[s] = randn(m);
    if (n > 0) sort(a, a + n);

    int i = naive_search(a, n, val);
    int j = bsearch(a, n, val);
    if (i != j){
      for (int s = 0; s < n; s++) cout << " " << s << " " << a[s] << endl;
      cout << "val = " << val << endl;
      cout << "ans = " << i  << ' ' << j << endl;
    
    }

  }
  
  return 0;
}


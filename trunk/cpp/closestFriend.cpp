#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

int main() {

  int a[] = { 1, 2, 3, 4, 6, 7, 8, 9 }, n = sizeof(a)/sizeof(int);
 
  n = 5;
  
  int max_nf = 3;

  for (int s = 0; s < n; s++) cout << a[s] << " ";
  cout << endl;
  
  for (int i = 0; i < n; i++){

    int nf = 0, l = i - 1, r = i + 1;

    cout << a[i] << ": ";

    while(1){

      if (nf == max_nf)    break;
      if (l < 0 && r >= n) break;

      if       ( l < 0   || ( 0 <= l && r < n && a[r] - a[i] < a[i] - a[l]  ) ){
        cout << a[r] << " ";
        nf++; r++;
      }else if ( r >= n  || ( 0 <= l && r < n && a[i] - a[l] <= a[r] - a[l] ) ){
        cout << a[l] << " ";
        nf++; l--;
      }
      
    }

    cout << endl;
  }
  
  return 0;

}

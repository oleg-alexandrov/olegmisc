#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

int main() {

  srand(time(NULL));
  for (int q = 0; q < 1000; q++){
    
  int n = 1000, big = 200, i = 0, j = 0, k = 0, l = 0, s = 0;
  int a[n];
  for (k = 0; k < n; k++){
    a[k] = rand()%big;
    //cout << k << ' ' << a[k] << endl;
  }

  for (k = 0; k < n; k++){
    for (l = k + 1; l < n; l++){

      if (a[l] - a[k] > a[j] - a[i]){
        i = k; j = l;
      }
    }
  }

  int i0 = i, j0 = j;
  //cout << "Brute force max is " << i0 << ' ' << j0 << ' '
  //     << a[j0] - a[i0] << endl;


  bool success = false;
  for (k = 0; k < n - 1; k++){
    if ( a[k] <= a[k+1] ){
      i = k; s = k; j = k + 1;
      success = true;
      break;
    }
  }

  //cout << "Success is " << success << endl;

  for (k = k + 1; k < n; k++){

    if ( k == n - 1 || a[k] > a[k + 1] ){

      if (a[k] > a[j]){ j = k; }
        
      if (a[k] - a[s] > a[j] - a[i]){ j = k; i = s; }
      
    }

    if (a[k] < a[s]){ s = k; }
    
  }
  
  //cout << "Smart max is " << i << ' ' << j << ' '
  //     << a[j] - a[i] << endl;


  if (success && a[j0] - a[i0] != a[j]  - a[i] ){
    cerr << "Failure!"   << endl;
    cout << "Diffs are:" << endl;
    cout << "Brute: " << i0  << ' ' << j0  << ' ' << a[j0] - a[i0] << endl;
    cout << "Smart: " << i   << ' ' << j   << ' ' << a[j]  - a[i]  << endl;
    exit(1);
  }else{
    //cout << "Success!" << endl;
  }

  }
  
  return 0;
}

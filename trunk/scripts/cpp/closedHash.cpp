#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

int main() {

  int n0 = 7, n = 10, a[n], hTable[n], empty = -1;

  for (int s = 0; s < n; s++) a[s] = s*s;

  for (int s = 0; s < n; s++) hTable[s] = empty;

  // Add to closed hash table
  for (int s = 0; s < n0; s++){
    
    int val = a[s];
    
    for (int q = 0; q < n; q++){
      int key = (abs(val)+q)%n;
      if (hTable[key] == empty){
        hTable[key] = val;
        break;
      }
    }
    
  }

  cout << "The closed hash is: " << endl;
  for (int key = 0; key < n; key++) cout << key  << ' ' <<  hTable[key] << endl;

  // Search the hash table
  for (int val = -4; val < (n0-1)*(n0-1)+4; val++){
    
    for (int q = 0; q < n; q++){

      int key = (abs(val)+q)%n;

      cout << "Searching for " << val << " at position " << key << endl;

      if (hTable[key] == empty){
        cout << "Not found" << endl;
        break;
      }

      if (hTable[key] == val){
        cout << "Found" << endl;
        break;
      }
      
    }
    
  }
  
  return 0;
  
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

int main() {
 
  const int a[] = {1, 8, 2, 9, 3, 10, 14, 11};
  const int b[] = {1, 5, 2, 5, 3, 5, 8, 6, 9, 6, 10, 7, 11};

  int m = sizeof(a)/sizeof(int);
  int n = sizeof(b)/sizeof(int);


  vector< vector< vector<int> > > T;
  T.resize(m);
  for (int i = 0; i < m; i++) T[i].resize(n);

  for (int i = m - 1; i >= 0; i--){
    for (int j = n - 1; j >= 0; j--){

      T[i][j].clear();
      if (a[i] != b[j]) continue;
        
      int max_i = -1, max_j = -1, max_len = -1;
      for (int l = i + 1; l < m; l++){
        for (int t = j + 1; t < n; t++){
          
          int len = T[l][t].size();
          if (len > max_len){
            max_i   = l;
            max_j   = t;
            max_len = len;
          }
          
        }
      }

      T[i][j].push_back(a[i]);
      if (max_len <= 0) continue;
        
      for (int s = 0; s < (int)T[max_i][max_j].size(); s++){
       T[i][j].push_back(T[max_i][max_j][s]); 
      }
    }
  }
  
  int max_i = -1, max_j = -1, max_len = -1;
  for (int l = 0; l < m; l++){
    for (int t = 0; t < n; t++){
      
      int len = T[l][t].size();
      if (len > max_len){
        max_i   = l;
        max_j   = t;
        max_len = len;
      }
      
    }
  }

  cout << "largest shared subsequence: [ ";
  for (int s = 0; s < (int)T[max_i][max_j].size(); s++){
    cout << T[max_i][max_j][s] << " "; 
  }
  cout << "]" << endl;
  
  
  return 0;
}

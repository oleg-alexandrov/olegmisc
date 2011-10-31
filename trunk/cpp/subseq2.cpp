#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

void printSeq(int l, int * a){
  cout << "[ ";
  for (int s = 0; s < l; s++) cout << a[s] << " ";
  cout << "]" << endl;
}

int main() {

  int a[] = {10, 6, -5, 3, 2, -5, 4, 33, 3};
  int n = sizeof(a)/sizeof(int);
  
  vector< vector<int> > S;
  S.resize(n);

  for (int s = n - 1; s >= 0; s--){
    
    int pos = -1, len = -1;
    for (int t = s + 1; t < n; t++){
      int l = S[t].size();
      if (l + 1 > len && l >= 1 && a[s] <= S[t][0]){
        pos = t; len = l + 1;
      }

    }

    S[s].push_back(a[s]);
    if (pos >= 0){
      for (int t = 0; t < (int)S[pos].size(); t++){
        S[s].push_back(S[pos][t]);
      }
    }
    
    
  }


  cout << "Seq is " << endl;
  printSeq(n, a);

  int l = 0;
  int * ptr = NULL;
  for (int s = 0; s < n; s++){
    if ((int)S[s].size() > l){
      l = S[s].size();
      ptr = &S[s][0];
    }
  }

  cout << "Largest increasing subseq is " << endl;
  printSeq(l, ptr);
  
  return 0;
}

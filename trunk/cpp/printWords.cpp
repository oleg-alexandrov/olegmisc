#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

int main() {
 
  const int numW = 10;
  const char W[][numW] = {"", "", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz"};

  int nL = 'z' - 'a' + 1;
  int  next  [nL];
  bool isLast[nL];

  for (int iW = 0; iW < numW; iW++){
    
    int lW = strlen(W[iW]);
    for (int iL = 0; iL < lW; iL++){

      char L = W[iW][iL];
      int pos = L - 'a';
        
      if (iL < lW - 1){
        next[pos]   = W[iW][iL + 1];
        isLast[pos] = false;
      }else{
        next[pos]   = W[iW][0];
        isLast[pos] = true;
      }
      
    }
  }

  const char digits[] = "09119";
  cout << digits << endl;
  
  int nD_max = strlen(digits);
  int nD = 0;
  
  char word[nD_max];
  
  for (int iD = 0; iD < nD_max; iD++){
    int d = digits[iD] - '0';
    assert(0 <= d && d <= 9);
    if (strlen(W[d]) == 0) continue;
    word[nD] = W[d][0];
    nD++;
  }
  word[nD] = 0;

  if (nD == 0) return 0;
    
  while(1){

    cout << "--" << word << "--" << endl;
    bool carry = true;
    for (int dIter = nD - 1; dIter >= 0; dIter--){

      char L  = word[dIter];
      int pos = L - 'a';
      if (! isLast[pos] ) carry  = false;
      word[dIter] = next[pos];
      if (!carry) break;
      
    }

    if (carry) break;
  }
  
  return 0;
}

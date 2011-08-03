#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

int main() {

  vector<string> A;
  string a;
  
  const char S[] = "xabccbabcz";
  int n = strlen(S);
  cout << "len is " << n << endl;

  for (int k = 0; k < n; k++){
    for (int s = 0; s <= 1; s++){

      int beg = k, end = k + s;
      int gbeg = -1, gend = -1;

      while(1){
        if (beg < 0 || end >= n || S[beg] != S[end]) break;
        gbeg = beg;
        gend = end;
        beg--; end++;
        if (gbeg >= gend) continue;

        a = "";
        for (int t = gbeg; t <= gend ; t++){
          a.push_back(S[t]);
        }
        cout << "Anagram is: " << a << endl;
      }
    }
      
  }

  return 0;
}

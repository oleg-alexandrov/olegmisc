#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

int main() {

  char a[] = "aaaaaaaaaaaaaaaaa";
  char w[] = "a";

  assert(a != NULL && w != NULL);
  
  int la = strlen(a), lw = strlen(w);
  int i = 0, j = 0;
  cout << "input:  '" << a << "' '" << w << "'" << endl;

  while (1){

    if (i >= la) break;

    bool hasW = (lw > 0);
    if (i + lw - 1 >= la ){
      hasW = false;
    }else{
      for (int s = 0; s < lw; s++){
        if (a[i + s] != w[s]){
          hasW = false;
          break;
        }
      }
    }

    if (hasW){
      i += lw;
    }else{
      a[j] = a[i];
      i++;
      j++;
    }
    
  }

  a[j] = 0;
  
  cout << "output: '" << a << "'" << endl;
  
  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>

using namespace std;

void rv_word(char * a, int beg, int end){
  while (beg < end){
    swap(a[beg], a[end]);
    beg++;
    end--;
  }
}

int main() {

  char a[] = " 1 23 45 678 9 ";
  //char *a = " 1 23 45 678 9 ";
  
  int n = strlen(a);
  
  cout << "There are " << n << " letters." << endl;
  cout << "'" << a << "'" << endl;
  
  rv_word(a, 0, n - 1);
  cout << "'" << a << "'" << endl;

  int bw = 0, ew = 0;
  while(1){

    while (bw < n && a[bw] == ' ') bw++; // find beginning of word
    if (bw >= n) break;

    ew = bw;
    while (ew + 1 < n && a[ew + 1] != ' ') ew++; // find end of word

    rv_word(a, bw, ew);

    bw = ew + 1;
  }
  
  cout << "'" << a << "'" << endl;

  return 0;

}

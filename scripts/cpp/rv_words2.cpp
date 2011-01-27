#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>

using namespace std;

void rv_str(char * a, int beg, int end) {
  // Rv the characters of a in the interval [beg, end]

  //cout << "---We are in " << beg << ' ' << end << endl;
  //cout << "---before" << endl;
  for (int i = beg; i <= end; i++){
    int j = beg + end - i;
    if (i >= j) break;
    //cout << "---i j are " << i << ' ' << j << endl;
    //cout << "---vals are " << a[i] << endl;
    //cout << "---vals are " << a[j] << endl;
    char q = a[i], r = a[j];
    //cout << "--- q, r are " << q << ' ' << r << endl;
    a[i] = r;
    a[j] = q;
    //swap(a[i], a[j]);
  }
  //cout << "---after" << endl;
  return;
}

void rv_words(char * a, int n){

  //cout << "n is " << n << endl;
  
  rv_str(a, 0, n - 1);

  int begWord = 0, endWord = 0;

  while(1){

    // Find beginning of word
    bool success = false;
    while(1){
      if (begWord >= n) break;
      if (a[begWord] != ' '){
        success = true;
        break;
      }
      begWord++;
    }
    if (!success) break;

    // Find end of word
    endWord = begWord;
    success = false;
    while(1){
      if (endWord >= n) break;
      if ( (endWord == n - 1 && a[endWord]     != ' ' ) ||
           (endWord <  n - 1 && a[endWord + 1] == ' ' ) ){
        success = true;
        break;
      }
      endWord++;
    }
    if (!success) break;

    rv_str(a, begWord, endWord);

    begWord = endWord + 1;
  }

  return;
}

int main(){

  char * a = new char[100];
  sprintf(a, "%s", "ab cd");
  
  cout << "Before is: '" << a << "'" << endl;
  rv_words(a, strlen(a));
  cout << "After is:  '" << a << "'" << endl;
  
  
  return 0;
}

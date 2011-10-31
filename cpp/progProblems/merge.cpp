#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

void printArr(const int * a, int len){

  cout << "[ ";
  for (int s = 0; s < len; s++) cout << a[s] << " ";
  cout << "]" << endl;
}

int main() {

  int a[] = {2, 5, 8, 10, 10, 23}, len_a = sizeof(a)/sizeof(int);
  int b[] = {2, 4, 9, 9, 11, 20},  len_b = sizeof(b)/sizeof(int);
  int len_c = len_a + len_b, c[len_c];
  
  int pos_a = 0, pos_b = 0, pos_c = 0;
  while (1){

    if (pos_a == len_a && pos_b == len_b) break;

    if (pos_a == len_a){
      c[pos_c] = b[pos_b];
      pos_c++;   pos_b++;
    }else if (pos_b == len_b){
      c[pos_c] = a[pos_a];
      pos_c++;   pos_a++;
    }else{

      if (a[pos_a] <= b[pos_b]){
        c[pos_c] = a[pos_a];
        pos_c++;   pos_a++;
      }else{
        c[pos_c] = b[pos_b];
        pos_c++;   pos_b++;
      }
      
    }
  }

  printArr(a, len_a);
  printArr(b, len_b);
  printArr(c, len_c);

  return 0;
}

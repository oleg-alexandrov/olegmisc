#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include <climits>
using namespace std;

int edit_dist(const char * a, int la, const char * b, int lb){

  assert(a != NULL && b != NULL);

  if (la == 0 && lb == 0) return 0;
    
  int v1 = INT_MAX, v2 = INT_MAX, v3 = INT_MAX;

  if (la > 0) v1 = 1 + edit_dist(a, la - 1, b, lb);
  if (lb > 0) v2 = 1 + edit_dist(a, la, b, lb - 1);

  if (la > 0 && lb > 0){
    v3 = edit_dist(a, la - 1, b, lb - 1) + (a[la - 1] != b[lb - 1]);
  }

  return min(min(v1, v2), v3);
}
  

int main() {
 
  char a[] = "xyb";
  char b[] = "abc";

  cout << "Edit distance is " << edit_dist(a, strlen(a), b, strlen(b)) << endl;
  
  return 0;
}

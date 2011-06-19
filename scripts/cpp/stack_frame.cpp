#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

int find_stack_size(const char * start, const char* pos){

  cout << "just got here, pos is " << (int)pos << endl;
  char q = '4' + (int)start;
  cout << "q is " << q << endl;
  char val = 'p';
  
  if (pos != NULL){
    return (&val - pos);
  }

  char t[1000000 + sizeof(q)];
  cout << "val is " << (int)t << endl;
  cout << "---diff is " << &q - start << endl;
  return find_stack_size(start, &val);
}

int main() {

  char * pos = NULL;
  char v = '6';
  char * start = &v;
  int size = find_stack_size(start, pos);
  cout << "size is " << (int)size << endl;

  
  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>

using namespace std;

int atoi(char * str) {

  int ans = 0, len = strlen(str), sign = 1;

  int pos = 0; // position in string

  while ( pos < len && str[pos] == ' ' ) pos++;

  if (pos >= len) return ans;

  if (str[pos] == '-'){
    sign = -1;
    pos++;
  }

  while (pos < len && str[pos] >= '0' && str[pos] <= '9'){
    int digit = str[pos] - '0';
    ans = 10*ans + digit;
    pos++;
  }

  ans = sign*ans;

  return ans;
}

int main(){

  char a[] = "   -2344.3";
  int val = atoi(a);
  cout << "'" << a << "' becomes the number " << val << endl;
  
  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

int main() {

  vector<int> V;
  V.clear();
  
  char memblock;

  ifstream fi;
  fi.open("uninit", ios::in | ios::binary);

  while ( fi.read(&memblock, sizeof(memblock)) ){
    V.push_back((int)memblock);
  }
  fi.close();

  ofstream f1("out1.txt");
  for (int s = int(V.size()) - 1; s >= 0; s--){
    f1 << V[s] << endl;
    s--;
  }
  f1.close();

  ofstream f2("out2.txt");
  
  int count = 0;
  char *p = new char;
  while (1){
    int val = p[-count];
    f2 << val << endl;
    count++;
  }
  
  return 0;
  
}

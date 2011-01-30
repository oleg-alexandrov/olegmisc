#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
using namespace std;

int addOne(int a){

  int mask = 1, prevMask = 0;
  while(1){
    
    if ( (a&mask) == 0){
      return ( (a|mask)^prevMask );
    }
    
    mask = (mask << 1);
    prevMask = ( (prevMask << 1) | 1 );
  }

  return 0;
}

int main() {

  for (int a = 0; a < 100; a++){

    int b = addOne(a);
    if (b != a + 1){
      cerr << "Error!" << endl;
      exit(0);
    }else{
      cout << "Looks good: " << b  << ' ' << a + 1 << endl;
    }
    
  }

  return 0;
}

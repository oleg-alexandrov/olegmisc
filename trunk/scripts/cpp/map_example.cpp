#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <cassert>
#include <map>
using namespace std;

int main(){

  map<int, int> z;
  
  z[5] = 12;
  z[2] = -54;
  cout << "z[5] is " << z[5] << endl;

  for (int s = 0; s < 7; s++){
    if ( z.find(s) != z.end() ){
      cout << "Found     z[" << s << "] = " << z[s] << endl;
    }else{
      cout << "Not found z[" << s << "]" << endl;
    }
  }
  
}

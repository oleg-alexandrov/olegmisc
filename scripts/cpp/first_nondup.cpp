#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
using namespace std;


int main(){

  string textStr = "   c  aa bbb ccddcc qq rrst uuvv  ooppoo   lll  ";
  char * text = (char*)textStr.c_str();
  
  int len = strlen(text);

  cout << "before: '" << text << "'" << endl;

  const int num = 256;
  int Map[num];

  for (int s = 0; s < num; s++) Map[s] = 0;
  
  for (int s = 0; s < len; s++){
    int toi = int(text[s]);
    assert(0 <= toi && toi < num);
    Map[toi]++;
  }
  
  
  bool success = false;
  for (int s = 0; s < len; s++){

    int toi = int(text[s]);
    if (Map[toi] == 1){
       success = true;
       cout << "The first non dup is " << text[s] << endl;
       break;
    }

  }

  if (!success){
    cout << "All chars are dups" << endl;
  }
  
  return 0;
  
}

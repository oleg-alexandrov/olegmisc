#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
using namespace std;


int main(){

  string textStr = "   c  aa bbb ccddcc qq rst uuvv  ooppoo   lll  ";
  char * text = (char*)textStr.c_str();
  
  int len = strlen(text);

  cout << "before: '" << text << "'" << endl;

  int start = 1;
  
  for (int s = 1; s < len; s++){
    if (text[s] != text[s-1]){
      text[start] = text[s];
      start++;
    }
    
  }

  text[start] = '\0';
  
  cout << "after: '" << text << "'" << endl;

  return 0;
  
}

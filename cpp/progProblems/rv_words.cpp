#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
using namespace std;

void rv_str(char * str, int len){

  // rv chars
  int beg = 0, end = len - 1;

  while (beg < end){
    swap(str[beg], str[end]);
    beg++; end--;
  }
  
  return;
  
}


int main(){

  string textStr = "   so  i  start   with   some  text ";
  char * text = (char*)textStr.c_str();
  
  int len = strlen(text);

  cout << "before: " << text << endl;

  cout << "length is " << len << endl;
  rv_str(text, len);
  cout << "midpt: '" << text << "'" << endl;
  

  int begWord = 0, endWord = 0;
  while(1){

    if (begWord >= len) break;
    if (text[begWord] == ' '){
      begWord++;
      continue;
    }

    endWord = begWord + 1;
    while(1){
      if (endWord >= len || text[endWord] == ' ') break;
      endWord++;
    }

    rv_str(text + begWord, endWord - begWord);
    begWord = endWord;
    
  }

  cout << "after: '" << text << "'" << endl;
  
  return 0;
  
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

char * my_strtk(char * str, const char * delim){

  static char *beg = NULL, *end = NULL;

  int ldelim = 0;
  if (delim != NULL) ldelim = strlen(delim);
  
  if (str != NULL){

    beg = str;
    end = str + strlen(str);
    
    // Split into tokens
    for (char *iter = beg; iter != end; iter++){
      for (int t = 0; t < ldelim; t++){
        if ( *iter == delim[t] ){
          *iter = '\0';
          break;
        }
      }
    }
    
  }

  // Advance to the beginning of a token
  bool success = false;
  while (beg != end){
    if (*beg != '\0'){
      success = true;
      break;
    }
    beg++;
  }
  if (!success){
    beg = end;
    return NULL;
  }
  
  char *rtn = beg;

  // Put the beg pointer to the end of the token we'll return
  while (beg != end && *beg != '\0') beg++;

  return rtn;
}

int main() {

  char str[]   = ",a,b,,c; d,; par,t , ;, e,,";
  char delim[] = ",, ;cdeabcedx";

  cout << "str is: '";
  if (str != NULL){
    cout << str;
  }
  cout << "'" << endl;
  
  cout << "delim is: '";
  if (delim != NULL){
    cout << delim;
  }
  cout << "'" << endl;
  
  cout << "Dumping tokens!" << endl;
  char *pos = str;
  while(1){
    char *token = my_strtk(pos, delim);
    if (token == NULL) break;
    pos = NULL;
    cout << "Token is: --" << token << "---" << endl;
  }
  
  return 0;
}

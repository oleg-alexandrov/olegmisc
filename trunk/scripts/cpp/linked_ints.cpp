#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
using namespace std;

struct strList {
 int intVal;
 strList * ptrToNext;
};


inline bool isDigit(const char p){
  return ( '0' <= p && p <= '9' );
}

int main(){

  
  const char* numStr = "x12 hello34 world 567s";
  cout << numStr << endl;
  
  strList * firstElem = NULL, * prevElem = NULL;

  for (int s = 0; s < (int)strlen(numStr); s++){

    // Skip unless at the beginning of a number
    if ( !isDigit(numStr[s]) ) continue;
    if ( s >= 1 && isDigit( numStr[s - 1] ) ) continue;

    // Parse the current number (atoi is smart enough to stop when
    // the first non-number character is encountered).
    int val = atoi(numStr + s);

    // Form the current linked list element
    strList * currElem = new strList;
    currElem->intVal    = val;
    currElem->ptrToNext = NULL;

    // Connect to the previous element in the linked list
    if (prevElem != NULL){
      prevElem->ptrToNext = currElem;
    }else{
      // This will be true only at the first iteration
      firstElem = currElem;
    }

    // Prepare for the next iteration
    prevElem = currElem;
    
  }

  // Traverse the list and print the results
  strList *currElem = firstElem;
  while (currElem != NULL){

    cout << "Element is "
         << currElem->intVal << ' ' << currElem->ptrToNext << endl;
    currElem = currElem->ptrToNext;
    
  }
  
  return 0;
}

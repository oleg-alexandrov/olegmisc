#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
using namespace std;

struct strList {
 int intVal;
 strList * ptrToNext;
};

void formList(strList*& head, int maxVal){

  int val = 0;
  head = NULL;
  strList *ptrToPrev = NULL;
  while (val <= maxVal){

    strList * ptrToCurr = new strList;
    ptrToCurr->intVal    = val;
    ptrToCurr->ptrToNext = NULL;

    if (ptrToPrev == NULL){
      assert(head == NULL);
      head = ptrToCurr;
    }else{
      assert(head != NULL);
      ptrToPrev->ptrToNext = ptrToCurr;
    }
    
    ptrToPrev = ptrToCurr;
    val++;
    
  }
  
}

void traverseList(strList * head){
  
  // Traverse the list and print the results
  strList *currElem = head;
  while (currElem != NULL){

    cout << "Element is "
         << currElem->intVal << ' ' << currElem << endl;
    currElem = currElem->ptrToNext;
  }

  cout << endl;
}

void reverseList(strList*& head){

  if (head == NULL){
    return;
  }

  strList * currElem = head, *newNext = NULL;

  while(currElem != NULL){

    strList * oldNext   = currElem->ptrToNext;
    currElem->ptrToNext = newNext;
    newNext             = currElem;
    currElem            = oldNext;
  }

  head = newNext;
}

int main(){

  strList * head;

  int maxVal = -1;
  formList(head, maxVal);
  
  traverseList(head);
  
  reverseList(head);

  traverseList(head);
  
  return 0;
}

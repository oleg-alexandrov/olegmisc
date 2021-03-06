#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
using namespace std;

struct list {
 int intVal;
 list * ptrToNext;
};

void formList(list*& head, int maxVal){

  int val = 0;
  head = NULL;
  list *ptrToPrev = NULL;
  while (val <= maxVal){

    list * ptrToCurr = new list;
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

void printList(list * head){
  
  // Traverse the list and print the results
  list *currElem = head;
  while (currElem != NULL){

    cout << "Element is "
         << currElem->intVal << ' ' << currElem << endl;
    currElem = currElem->ptrToNext;
  }

  cout << endl;
}

void printList2(list * head){
  
  // Traverse the list and print the results
  list *currElem = head;
  cout << "list: [ ";
  while (currElem != NULL){
    cout << currElem->intVal << " ";
    currElem = currElem->ptrToNext;
  }
  cout << "]" << endl;
}

void reverseList(list*& head){

  if (head == NULL){
    return;
  }

  list * currElem = head, *newNext = NULL;

  while(currElem != NULL){

    list * oldNext   = currElem->ptrToNext;
    currElem->ptrToNext = newNext;
    newNext             = currElem;
    currElem            = oldNext;
  }

  head = newNext;
}

int insert(list*& head, int val){

  list * elem     = new list;
  elem->ptrToNext = head;
  elem->intVal    = val;
  head            = elem;
  return 0;
}

int deleteAtBeg(list*& head){

  assert(head != NULL);
  list * nHead = head->ptrToNext;
  delete head;
  head = nHead;

  return 0;
}

int deleteOneElem(list*& head, int val){

  // Return 1 on failure, 0 on success
  if (head == NULL) return 1;

  list * currPtr = head, *prevPtr = NULL;
   while(currPtr != NULL){
    if (currPtr->intVal == val){

      if (prevPtr == NULL){ // we are at the beg of the list

        assert(currPtr == head);
        list *ptr = currPtr->ptrToNext;
        delete head;
        head = ptr;
        
      }else{
        prevPtr->ptrToNext = currPtr->ptrToNext;
        delete currPtr;
      }

       return 0; // success
    }
    
    prevPtr = currPtr;
    currPtr = currPtr->ptrToNext;
  }
  
  return 1;
}


void test1(){
  list * head = NULL;

  int maxVal = -1;
  formList(head, maxVal);
  
  printList(head);
  
  reverseList(head);

  cout << "Original list:" << endl;
  printList(head);

  insert(head, 7);
  cout << "One element inserted:" << endl;
  printList(head);
  
  insert(head, -4);
  cout << "One element inserted:" << endl;
  printList(head);

  deleteAtBeg(head);
  cout << "One element deleted:" << endl;
  printList(head);

  insert(head, 99);
  insert(head, 12);
  insert(head, 9);
  insert(head, 18);
  cout << "Three elements inserted:" << endl;
  printList(head);

  deleteOneElem(head, 9);
  cout << "One element deleted:" << endl;
  printList(head);
  
  deleteOneElem(head, 33);
  cout << "One element deleted:" << endl;
  printList(head);

  deleteOneElem(head, 12);
  cout << "One element deleted:" << endl;
  printList(head);

  deleteOneElem(head, 7);
  cout << "One element deleted:" << endl;
  printList(head);

  deleteOneElem(head, 18);
  cout << "One element deleted:" << endl;
  printList(head);

  deleteOneElem(head, 99);
  cout << "One element deleted:" << endl;
  printList(head);

  deleteOneElem(head, 99);
  cout << "One element deleted:" << endl;
  printList(head);

  return;
}

void splitAtPivot(list * head, int pivot, list*& list1, list*& list2){

  list1 = NULL;
  list2 = NULL;

  bool addToFirstList = true;
  list * prevOutPtr   = NULL;
  list * currPtr      = head;

  while (currPtr != NULL){

    if (currPtr->intVal == pivot){
      addToFirstList = false;
      currPtr        = currPtr->ptrToNext;
      prevOutPtr     = NULL;
      continue;
    }

    list * ptr = new list;
    ptr->ptrToNext = NULL;
    ptr->intVal    = currPtr->intVal;

    if (prevOutPtr != NULL){
      prevOutPtr->ptrToNext = ptr;
    }else if (addToFirstList){
      assert(list1 == NULL);
      list1 = ptr;
    }else{
      assert(list2 == NULL);
      list2 = ptr;
    }

    prevOutPtr = ptr;
    currPtr    = currPtr->ptrToNext;
  }
  
}

int main(){

  list * head = NULL;

  int maxVal = 10, pivot = 11;
  formList(head, maxVal);

  printList2(head);

  list * list1, * list2;
  splitAtPivot(head, pivot, list1, list2);

  cout << "Pivot is: " << pivot << ". Printing the split lists below:"<< endl;
  printList2(list1);
  printList2(list2);
  
  
}

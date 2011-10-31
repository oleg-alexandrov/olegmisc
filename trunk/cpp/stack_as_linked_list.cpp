#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <cassert>
using namespace std;

struct list{
  int   val;
  list *next;
};

struct stack{
  
  static const int success = 0, failed = 1;
  
  list * head;
  list * tail;
  stack(){
    head = NULL;
    tail = NULL;
  }
  
  ~stack(){

    cout << "Calling the stack destructor" << endl;
    
    list * curr = head;
    
    while(curr != NULL){
      head = curr->next;
      cout << "deleting elem " << curr->val << endl;
      delete curr;
      curr = head;
    }

    tail = head;
    assert(tail == NULL);
  }

  int push(int val){

    list * nextElem = new list;
    if (nextElem == NULL) return failed;

    nextElem->val  = val;
    nextElem->next = NULL;

    
    if (head == NULL){
      assert(tail == NULL);
      head = nextElem;
      tail = nextElem;
    }else{
      assert(tail != NULL);
      tail->next = nextElem;
      tail = nextElem;
    }

    return success;
  }

  int pop(int & val){
    
    if (tail == NULL){
      assert(head == NULL);
      return failed;
    }

    val = tail->val;

    if (head == tail){
      delete head;
      head = NULL;
      tail = NULL;
      return success;
    }
    
    list * prev = head;
    while(prev->next != tail){
      assert(prev != NULL);
      prev = prev->next;
    }

    delete tail;
    prev->next = NULL;
    tail = prev;
    
    return success;
  }

  void print(){
    
    list * curr = head;
    cout << "[";
    
    while (curr != NULL){
      cout << " " << curr->val;
      curr = curr->next;
    }
    cout << " ]" << endl;
  }

  
};

int main(){

  stack A;
  A.print();
  
  A.push(1);
  A.print();

  A.push(2);
  A.print();

  A.push(3);
  A.print();
  
  int val;
  int success = stack::success;
  while(1){
    success = A.pop(val);
    if (success == stack::success){
      cout << "We popped: " << val << endl;
      A.print();
    }else{
      cout << "Stack is empty: cannot pop more elements" << endl;
      break;
    }
  }

  A.push(7);
  A.print();
  
  return 0;
}


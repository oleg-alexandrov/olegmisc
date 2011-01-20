#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

struct node{
  int    val;
  node * next;
};

void pushToListFront(node*& head, int val){
  node * elem = new node;
  elem->val = val;
  elem->next = head;
  head = elem;
}

bool isInList(node * head, int val){

  bool isIn   = false;
  node * elem = head;

  while (elem != NULL){
    if (elem->val == val){
      isIn = true;
      break;
    }
    elem = elem->next;
  }
  
  return isIn;
}

void printList(node * head){
  
  cout << "[ ";
  node * elem = head;
  while (elem != NULL){
    cout << elem->val << " ";
    elem = elem->next;
  }
  cout << "]" << endl;
}

int main() {

  int n = 8, a[n];
  int k = 6;
  node* hTable[k];

  for (int s = 0; s < n; s++) a[s] = s*s;

  for (int key = 0; key < k; key++) hTable[key] = NULL;
  
  for (int s = 0; s < n; s++){
    int val = a[s];
    int key = abs(val)%k;
    pushToListFront(hTable[key], val);
  }

  cout << endl;
  cout << "Put the squares from " << 0*0 << " to "
       << (n-1)*(n-1) << " in the hash table" << endl;
  cout << "The hash function is the remainder mod " << k << endl;
  cout << endl;
  
  cout << "The hash table is: " << endl;
  for (int key = 0; key < k; key++){
    cout << key << ": ";
    printList(hTable[key]);
  }
  cout << endl;
  
  for (int val = -5; val < n*n + 5; val++){

    int key   = abs(val)%k;
    bool isIn = isInList(hTable[key], val);
    
    if (isIn) cout << "The value " << val << " is in"     << endl;
    //else    cout << "The value " << val << " is NOT in" << endl;
  }


  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

class node{
public:
  int val;
  node * ptr;
};

bool isDigit(const char s){

  return ('0' <= s && s <= '9');
}
  
int main() {

  //const char * mystring = "12 hello34 world 567";
  const char * mystring = "x4x000-3";

  node * head     = NULL; // points to zeroth elem of the list
  node * prevNode = NULL;
  
  
  for (int s = 0; s < (int)strlen(mystring); s++){

    if (!isDigit(mystring[s]) ) continue;

    // Extract the integer starting at the current position
    int val = atoi(mystring + s);

    node * currNode = new node;
    currNode->ptr = NULL;
    currNode->val = val;

    if (prevNode == NULL){
      assert(head == NULL);
      head = currNode;
    }else{
      prevNode->ptr = currNode;
    }

    prevNode = currNode;
    
    // Skip all the digits of the current integer
    while(s < (int)strlen(mystring) && isDigit(mystring[s])){
      s++;
    }
    
  }

  // print the result
  node * currNode = head;
  while(currNode != NULL){
    cout << "Value is " << currNode->val << endl;
    currNode = currNode->ptr;
  }

  return 0;
}

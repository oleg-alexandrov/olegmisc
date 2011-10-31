#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

struct node{
  node * next;
  int val;
};

void print_list(node * head){
  node * n = head;
  cout << "[ ";
  while (n != NULL){
    cout << n->val << ' ';
    n = n->next;
  }
  cout << "]" << endl;
}

void form_list(node *& head, const int * a, int len){

  if (len == 0){
    head = NULL;
    return;
  }

  node * prev = NULL;
  for (int s = 0; s < len; s++){
    node * n = new node;
    n->val  = a[s];
    n->next = NULL;
    if (prev == NULL) head = n;
    else              prev->next = n;
    prev = n;
  }
  
}

void insert_into_sorted(node*& head, int v){

  node * b = NULL;
  node * a = head;

  while (a != NULL && a->val < v){
    b = a;
    a = a->next;
    continue;
  }

  node * n = new node;
  n->val = v;
  n->next = a;
  if (b != NULL) b->next = n;
  else           head    = n;
    
  return;
}

int main() {
  
  const int a[] = {3, 9, 11};
  int v = 33;
  
  int len = sizeof(a)/sizeof(int);

  node * head;
  form_list(head, a, len);
  print_list(head);
  insert_into_sorted(head, v);
  print_list(head);
  
  return 0;
}

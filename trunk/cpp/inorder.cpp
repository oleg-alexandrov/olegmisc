#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

struct node{

  int val;
  node * left;
  node * right;
  
};

void printTree(node * root){

  node * empty = (node*)(-1);
  
  vector<node*> currLevel, nextLevel;
  currLevel.clear(); nextLevel.clear();
  currLevel.push_back(root);

  int level = 0;

  while(1){

    int rootPos     = 64;
    int numEl       = currLevel.size();
    int numGood     = 0;
    int numPrinted  = 0;
    
    for (int s = 0; s < numEl; s++){

      int wid = rootPos/numEl;
      int pos = rootPos + (int)((2.0*s - numEl + 1)*wid/1.5 + 0.5);
      while( numPrinted < pos - 1 ){
        cout << " ";
        numPrinted++;
      }
      
      node * curr = currLevel[s];
      
      if (curr == NULL){
        cout << "--";
        nextLevel.push_back(empty);
        nextLevel.push_back(empty);
      }
      else if (curr == empty){
        cout << "  ";
        nextLevel.push_back(empty);
        nextLevel.push_back(empty);
      }else{
        numGood++;
        if (abs(curr->val) < 10) cout << " ";
        cout << curr->val;
        nextLevel.push_back(curr->left);
        nextLevel.push_back(curr->right);
      }

      numPrinted +=2;

    }

    cout << endl << endl << endl;

    level++;
    currLevel = nextLevel;
    nextLevel.clear();
    
    if (numGood == 0) break;
  }
  

  return;
}

void formBST(node* & root, int * A, int num){

  if (num == 0){
    root = NULL;
    return;
  }

  int mid = num/2;
  root = new node;
  root->val = A[mid];

  formBST(root->left,  A,           mid          );
  formBST(root->right, A + mid + 1, num - mid - 1);
  
}


void printInOrder(node *root){

  if (root == NULL) return;
  if (root->left != NULL) printInOrder(root->left);
  cout << root->val << " ";
  if (root->right != NULL) printInOrder(root->right);

}

void printInOrderNonRec(node *root){

  if (root == NULL) return;
  int nv = 0, lv = 1, av = 2;
  vector<node*> N; N.clear();
  vector<int>   V; V.clear();
  N.push_back(root);
  V.push_back(nv);

  while(1){
    
    if (N.empty()) return;
    
    node * n = N.back(); N.pop_back();
    int    v = V.back(); V.pop_back();

    if (v == av) continue;
      
    if (v == nv){
      if (n->left != NULL){
        N.push_back(n);
        V.push_back(lv);
        N.push_back(n->left);
        V.push_back(nv);
        continue;
      }
      v = lv;
    }

    assert(v == lv);

    cout << n->val << " ";
    
    if (n->right != NULL){
      N.push_back(n);
      V.push_back(av);
      N.push_back(n->right);
      V.push_back(nv);
    }
    
  }

  return;
}

int main(){

  int A[] = { -2, 0, 4, 2, -3, 13, -17};
  int num = sizeof(A)/sizeof(A[0]);

  cout << "number is " << num << endl;
  sort(A, A + num);

  for (int s = 0; s < num; s++) cout << A[s] << " ";
  cout << endl;
  
  node * root = NULL;
  formBST(root, A, num);

  printTree(root);

  cout << "Printing tree in-order" << endl;
  printInOrder(root);
  cout << endl;
  
  cout << "Printing tree in-order non-recursively" << endl;
  printInOrderNonRec(root);
  cout << endl;
  
  return 0;
  
}

#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
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

    int rootPos     = 70;
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

void insertToBST(node* & root, int val){

  if (root == NULL){
    root        = new node;
    root->val   = val;
    root->left  = NULL;
    root->right = NULL;
    return;
  }

  if (val < root->val ) insertToBST(root->left,  val);
  else                  insertToBST(root->right, val);

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

  int val = 14;
  insertToBST(root, val);
  cout << "Inserting " << val << " into the BST." << endl;

  val = -18;
  insertToBST(root, val);
  cout << "Inserting " << val << " into the BST." << endl;

  val = -16;
  insertToBST(root, val);
  cout << "Inserting " << val << " into the BST." << endl;

  printTree(root);

  return 0;
  
}

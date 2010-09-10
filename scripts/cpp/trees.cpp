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

node * getPtrToParentOfLargestElem(node * root){

  assert(root        != NULL);
  assert(root->right != NULL);

  if (root->right->right == NULL) return root;
  return getPtrToParentOfLargestElem(root->right);

}

void deleteFromToBST(node* & root, int val){

  if (root == NULL) return;
  if (val < root->val){ deleteFromToBST(root->left, val);  return; }
  if (val > root->val){ deleteFromToBST(root->right, val); return; }

  if (root->left == NULL){
    node * child = root->right;
    delete root;
    root = child;
    return;
  }
    
  if (root->right == NULL){
    node * child = root->left;
    delete root;
    root = child;
    return;
  }
  
  if (root->left->right == NULL){
    node * right = root->right;
    node * left  = root->left;
    delete root;
    root = left;
    root->right = right;
    return;
  }

  if (root->right->left == NULL){
    node * right = root->right;
    node * left  = root->left;
    delete root;
    root = right;
    root->left = left;
    return;
  }

  
  node * parent = getPtrToParentOfLargestElem(root->left);
  assert(parent->right != NULL);
  assert(parent->right->right == NULL);
  root->val = parent->right->val;
  node * child = parent->right->left;
  delete(parent->right);
  parent->right = child;

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

  val = -1;
  insertToBST(root, val);
  cout << "Inserting " << val << " into the BST." << endl;

  val = -2;
  insertToBST(root, val);
  cout << "Inserting " << val << " into the BST." << endl;

  val = -2;
  insertToBST(root, val);
  cout << "Inserting " << val << " into the BST." << endl;

  printTree(root);

  val = 0;
  cout << "Deleting " << val << " from BST." << endl;
  deleteFromToBST(root, val);
  printTree(root);
  
  return 0;
  
}

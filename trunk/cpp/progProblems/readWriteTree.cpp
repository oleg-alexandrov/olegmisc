#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
#include <cassert>
#include <queue>
using namespace std;

int randn(int n){
  return rand()%n;
}

struct node{

  int val;
  node * left;
  node * right;
  node(){
    left  = NULL;
    right = NULL;
  }
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

  int mid = randn(num);
  
  root = new node;
  root->val = A[mid];

  formBST(root->left,  A,           mid          );
  formBST(root->right, A + mid + 1, num - mid - 1);
  
}

void writeTree(const node * root, const char * file){

  queue<const node*> Q;
  ofstream fs(file);
  cout << "Writing to " << file << endl;
  
  if (root == NULL) return;

  Q.push(root);
  while(!Q.empty()){
    const node * N = Q.front(); Q.pop();

    fs << N->val << " ";
    int flag = 0;
    if (N->left != NULL){
      Q.push(N->left);
      flag |= 1; // 01
    }
    if (N->right != NULL){
      Q.push(N->right);
      flag |= 2; // 10
    }
    fs << flag << endl;
  }

  return;
}

void readTree(node *& root, const char * file){

  root = NULL; // Initialize the output
  
  int val, flag;

  cout << "Reading " << file << endl;
  ifstream fs(file);
  if (! (fs >> val >> flag) ){
    return;
  }

  root = new node;
  queue<node*> Q;
  Q.push(root);

  do{

    assert( !Q.empty() );
    node * N = Q.front(); Q.pop();

    N->val = val;

    if (flag & 1){
      node * L = new node;
      Q.push(L);
      N->left = L;
    }
    if (flag & 2){
      node * R = new node;
      Q.push(R);
      N->right = R;
    }
  
  } while(fs >> val >> flag);

  assert( Q.empty() );

  return;
}


int main(){

  srand ( time(NULL) );

  //int A[] = { -2, 0, 4, 2, -3, 13, -17};
  int A[] = { -2, 0, 4, 2, -3};
  int num = sizeof(A)/sizeof(A[0]);

  cout << "number is " << num << endl;
  sort(A, A + num);

  for (int s = 0; s < num; s++) cout << A[s] << " ";
  cout << endl;
  
  node * root = NULL;
  formBST(root, A, num);

  printTree(root);

  const char file[] = "tree.txt";
  writeTree(root, file);

  node * root2;
  readTree(root2, file);

  cout << "Printing tree read from file" << endl;
  printTree(root2);

  return 0;
  
}

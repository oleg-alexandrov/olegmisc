#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
using namespace std;

typedef vector<int> heap;

void printHeap(heap & H){

  cout << "printing heap" << endl;
  
  int numTotalEl = H.size();
  if (numTotalEl == 0) return;

  int root = 0, empty = -1;
  
  vector<int> currLevel, nextLevel;
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
      
      int curr = currLevel[s];
      
      if (curr == empty){
        cout << " ";
        nextLevel.push_back(empty);
        nextLevel.push_back(empty);
      }else{
        numGood++;
        int val = H[curr];
        if (abs(val) < 10) cout << " ";
        cout << val;
        int left = 2*curr + 1, right = 2*curr + 2;
        if (left < numTotalEl)  nextLevel.push_back(left);
        else                    nextLevel.push_back(empty);  
        if (right < numTotalEl) nextLevel.push_back(right);
        else                    nextLevel.push_back(empty);  
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


void insertToHeap(heap & H, int val){

  int childId = H.size();
  H.push_back(val);

  while(childId > 0){

    int parentId = (childId - 1)/2; // integer division
    if (H[parentId] < H[childId]) swap(H[parentId], H[childId]);
    else break;
    
    childId = parentId;
    
  }

  return;
}

int rmLargestFromHeap(heap & H){

  int numEl = H.size();
  assert(numEl > 0);

  int val = H[0];
  numEl--;
  H[0] = H[numEl];
  H.resize(numEl);

  int curr = 0;
  while(1){
    int left = 2*curr + 1, right = 2*curr + 2;
    if (left >= numEl) break;

    int maxInd;
    if (right >= numEl || H[right] < H[left]) maxInd = left;
    else                                      maxInd = right;

    if (H[curr] < H[maxInd]) swap(H[curr], H[maxInd]);
    else break;

    curr = maxInd;
  }
  
  cout << "removing " << val << endl;
  return val;
}

int main(){

  int A[] = { -2, 0, 4, 2, -3, 13, -17, 23, -24};
  int num = sizeof(A)/sizeof(A[0]);

  for (int s = 0; s < num; s++) cout << A[s] << " ";
  cout << endl;

  heap H;

  for (int s = 0; s < num; s++) insertToHeap(H, A[s]);
  printHeap(H);

  while (!H.empty()){
    rmLargestFromHeap(H);
    printHeap(H);
  }
  
  return 0;
  
}

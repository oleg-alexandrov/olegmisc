#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

int randn(int n){
  return rand()%n;
}

int parent(int i){
  assert(i > 0);
  return (i - 1)/2;
}

int leftChild(int i){
  return 2*i + 1;
}

int rightChild(int i){
  return 2*i + 2;

}

bool isHeap(const int * a, int n){

  for (int s = 1; s < n; s++){
    if (a[parent(s)] < a[s]) return false; // parent val must be >= children val
  }

  return true;
}

void printHeap(const int * a, int n){

  int power = 1;
  while(1){
    // Current level has elements power - 1, power, ..., 2*power - 2
    if (power - 1 >= n) break; // reached the end of the array

    for (int s = power - 1; s < min(n, 2*power - 1); s++){
      cout << a[s] << " ";
    }
    cout << endl;
    power *= 2; // Go to the next level
  }
  cout << endl;
}

void addToHeap(int * a, int & n, int val){

  a[n] = val;

  int k = n;
  while(k > 0){
    int p = parent(k);
    if (a[p] < a[k]){
      swap( a[p], a[k] );
      k = p;
    }
    else break; // Percolated a[n] as far as high as it could go
  }
  
  n++; // Heap got larger
}

int delFromHeap(int * a, int & n){

  assert(n > 0);
  int val = a[0];
  a[0] = a[n - 1]; n--; // Heap got shorter, but is unbalanced
  

  int p = 0;
  while(1){

    int l = leftChild (p);
    int r = rightChild(p);

    if (l >= n) break; // p is on the last level of the heap

    if (r >= n && a[p] < a[l] ){
      // l is the last element in the heap and need one percolation
      swap( a[p], a[l] );
      break;
    }

    if (a[r] >= a[l] && a[p] < a[r]){ // percolate to right child
      swap( a[p], a[r] );
      p = r;
    }else if (a[r] <= a[l] && a[p] < a[l]){ // percolate to the right child
      swap( a[p], a[l] );
      p = l;
    }else{
      break;
      // Cannot percolate further down
    }
    
  }
  
  return val;
}

void heapSort(int *a, int n){

  int heapSize = 0;
  for (int i = 0; i < n; i++){
    addToHeap(a, heapSize, a[i]);
  }

  assert(heapSize == n);
  
  for (int i = 0; i < n; i++){
    int val = delFromHeap(a, heapSize);
    a[heapSize] = val;
  }

  assert(heapSize == 0);
}

int main() {

  int a[10000];
  int b[10000];

  int n = 0;

  for (int k = 0; k < 8; k++){
    addToHeap(a, n, k); 
    assert ( isHeap(a, n) );
    //printHeap(a, n);
  }

  cout << "Now deleting from the heap" << endl;
  while (n > 0){
    delFromHeap(a, n);
    assert ( isHeap(a, n) );
    //printHeap(a, n);
  }


  srand ( time(NULL) );

  n = 10;
  for (int i = 0; i < n; i++){
    a[i] = randn(2*n) - n;
    b[i] = a[i];
  }
  
  cout << "Before sort:" << endl;
  for (int i = 0; i < n; i++) cout << a[i] << " "; cout << endl;
  
  heapSort(a, n);
  std::sort(b, b + n);
  
  cout << "After sort:" << endl;
  for (int i = 0; i < n; i++){
    cout << a[i] << " " << b[i] << endl;
  }
  
  for (int i = 0; i < n; i++){
    if (a[i] != b[i]){
      cout << "Error at index i" << endl;
      exit(1);
    }
  }
  
  return 0;

}

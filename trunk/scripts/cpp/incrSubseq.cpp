#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
using namespace std;

int randn(int n){
  return rand()%max(n, 1);
}

void naiveIncrSubSeq(int * a, int n, vector<int> & out){

  out.clear();
  
  if (n <= 0) return;

  assert(n < 31); // to avoid overflow
  
  vector<int> seq;

  for (int k = 0; k < (1 << n); k++){ // Iterate over all subsets of a set of n elements

    seq.clear();
    for (int s = 0; s < n; s++){
      int bit = ( ( k & (1 << s) ) != 0 ); // bit number s
      int l = seq.size();
      if (bit && ( l == 0 || a[seq[l - 1]] <= a[s] ) ){
        seq.push_back(s);
      }
    }

    if ( out.size() < seq.size() ) out = seq; 
      
  }

  for (int s = 1; s < (int)out.size(); s++){
    if ( out[s - 1] >= out[s] || a[out[s - 1]] > a[out[s]] ){
      cerr << "Error: sequence not increasing." << endl;
      for (int t = 0; t < (int)out.size(); t++) cout << a[out[t]] << " ";
      assert(false);
    }
  }
  
  return;
}

void incrSubSeq(int * a, int n, vector<int> & out){

  vector< vector<int> > S;
  S.resize(n);
  
  // For each k in 0, ..., n - 1, find the largest increasing
  // subsequence starting with a[k].  Store the indices of that
  // subsequence in S[k].  At the end, return the longest of the
  // subsequences stored in S[0], ..., S[n - 1].
  
  for (int k = n - 1; k >= 0; k--){
    
    int maxPos = -1, maxLen = -1;
    for (int l = k + 1; l < n; l++){ // largest subsequence starting with a[l]
      if (a[k] <= a[l] && (int)S[l].size() >= maxLen){
        maxLen = S[l].size();
        maxPos = l;
      }
    }

    if (maxPos >= 0) S[k] = S[maxPos];
    S[k].insert(S[k].begin(), k);
    
  }

  int maxPos = -1, maxLen = -1;
  for (int k = 0; k < n; k++){
    if ((int)S[k].size() >= maxLen){
      maxLen = S[k].size();
      maxPos = k;
    }
  }

  if (maxPos >= 0) out = S[maxPos];
  else             out.clear(); 
    
  return;
}


int main(){

  srand ( time(NULL) );

  int k = 25;
  int n = randn(k), m = randn(2*k);
  int T = 10;
  bool debug = false;
  
  int a[max(n, 1)];
  
  vector<int> seq1, seq2;

  for (int t = 0; t < T; t++){
    
    for (int s = 0; s < n; s++) a[s] = randn(m);

    naiveIncrSubSeq(a, n, seq1);
    int l1 = seq1.size();

    incrSubSeq(a, n, seq2);
    int l2 = seq2.size();

    if (debug || l1 != l2){
      
      if (!debug)
        cerr << "Error: Inconsistent result for largest increasing subsequence" << endl;
      
      for (int s = 0; s < n; s++) cout << a[s] << " "; cout << endl;
      
      cout << "Largest increasing subsequence:" << endl;
      for (int s = 0; s < (int)seq1.size(); s++) cout << a[seq1[s]] << " "; cout << endl;
      
      cout << "Largest increasing subsequence:" << endl;
      for (int s = 0; s < (int)seq2.size(); s++) cout << a[seq2[s]] << " "; cout << endl;
      
      cout << endl;

      if (!debug) assert(false);
      
    }
    
  }
  
  return 0;
}


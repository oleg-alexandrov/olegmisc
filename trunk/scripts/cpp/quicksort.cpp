#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <ctime>
using namespace std;

int randn(int n){
  return rand()%n;
}

#define DO_DEBUG (0)

void qsort2(int * a, int n){

#if DO_DEBUG
  cout << "Before sort: " << endl;
  for (int s = 0; s < n; s++) cout << a[s] << " ";
  cout << endl;
#endif

  if (n <= 1) return;

  int p    = randn(n);
  int pval = a[p];

  int sep = n;

  //cout << "n is " << n << endl;
  //cout << "Pivot is " << p << ' ' << pval << endl;
  
  // Will shuffle the array elements so that
  // a[0], ..., a[sep-1] < pval and
  // a[sep], ..., a[n-1] >= pval.

  // Shuffling is done as follows.
  // For each pair ibig < ismall with a[ibig] >= pval and a[ismall] < pval
  // swap these two values.
  int ismall = 0;
  for (int ibig = 0; ibig < n; ibig++){

    if (a[ibig] < pval) continue;

    //cout << "ibig is " << ibig << endl;

    sep = ibig;
    //cout << "sep is " << sep << endl;
    
    // We found ibig. Now find ismall.
    bool success = false;
    ismall = max(ismall, ibig) + 1;
    while (ismall < n){
      //cout << "ismall is " << ismall  << ' ' << a[ismall] << endl;
      if (a[ismall] < pval){
        success = true;
        break;
      }
      ismall++;
    }
    if (!success) break;

    swap(a[ismall], a[ibig]);
    sep = ibig + 1;
    //cout << "after swap: " << a[ibig] << ' ' << a[ismall] << endl;
  }

  if (sep == 0 && a[sep+1] == a[sep]){ // guard against infinite loop
    sep = 1;
  }
  
  //cout << "sep is " << sep << ' ' << a[sep] << endl;
  //return;
  
  qsort2(a,       sep);
  qsort2(a + sep, n - sep);
}


void doSort(int * a, int s, int e){

  // Sort the values a[i] with i in [s, e)
  
  if (s + 1 >= e){
    return;
  }

#if DO_DEBUG
  cout << "\nBefore iter: " << endl;
  for (int t = s; t < e; t++) cout << a[t] << " "; cout << endl;
#endif
  
  int p = s + randn(e - s); // pivot
  int pval = a[p];
  
#if DO_DEBUG
  cout << "Pivot is " << p << ' ' << pval << endl;
#endif
  
  int il = s, ig = s, separator = s;

  while (1){
    
    // Find il > ig with a[il] <= pval and a[ig] >= pval.
    // If successful, swap a[il] and a[ig].

    bool success = false;
    while (ig < e){
      if (a[ig] >= pval){
        separator = ig - 1;
#if DO_DEBUG
        cout << "ig: " << ig  << ' ' << a[ig] << endl;
        cout << "1 separator is " << ig << endl;
#endif
        success = true;
        break;
      }
      ig++;
#if DO_DEBUG
      cout << "incremented ig: " << ig << endl;
#endif
    }
    if (!success) break;

    success = false;
    il = max(il, ig + 1);
    while (il < e){
      if (a[il] <= pval){
#if DO_DEBUG
        cout << "il: " << il  << ' ' << a[il] << endl;
#endif
        success = true;
        break;
      }
      il++;
#if DO_DEBUG
      cout << "incremented il: " << il << endl;
#endif
    }
    if (!success) break;
      
    swap(a[il], a[ig]);
    separator = ig;
#if DO_DEBUG
    cout << "2 separator is " << separator << ' ' << a[separator] << endl;
#endif
    ig++;

#if DO_DEBUG
    cout << "During iter: " << endl;
    for (int t = s; t < e; t++) cout << a[t] << " "; cout << endl;
#endif
  }
  
#if DO_DEBUG
  cout << "After iter: " << endl;
  for (int t = s; t < separator + 1; t++) cout << a[t] << " ";
  cout << " --- ";
  for (int t = separator + 1; t < e; t++) cout << a[t] << " ";
  cout << endl;
#endif
  
  //if (0 <= separator && separator < e ){
    doSort(a + s, 0, separator + 1 - s);
    doSort(a + separator + 1, 0, e - separator - 1);
    //}

  return;
}

int main() {

  srand ( time(NULL) );

  int n = 18;//0*2*2*2*2;//*2*2*2;
  int num = 1;//100000/8/2/2;
  int a[n], b[n], c[n];
  for (int s = 0; s < n; s++) {
    a[s] = randn(n/2);
    b[s] = a[s];
    c[s] = a[s];
  }
  
#if 0
  std::sort(a, a + n);
  std::sort(b, b + n);
  std::sort(c, c + n);
#endif
  
#if DO_DEBUG
  cout << "Before sort: " << endl;
  for (int s = 0; s < n; s++) cout << a[s] << " ";
  cout << endl;
#endif
  
  clock_t start, finish;
  
  start = clock();
  for (int q = 0; q < num; q++){
    for (int s = 0; s < n; s++) a[s] = c[s];
    doSort(a, 0, n);
  }
  finish = clock();

  double t1 = ( (finish - start)/CLOCKS_PER_SEC );
  cout << "Elapsed time quicksort is " << t1 << endl;

  start = clock();
  for (int q = 0; q < num; q++){
    for (int s = 0; s < n; s++) b[s] = c[s];
    qsort2(b, n);
  }
  finish = clock();

  double t2 = ( (finish - start)/CLOCKS_PER_SEC );
  cout << "Elapsed time std sort is " << t2 << endl;
  cout << "n is " << n << endl;
  cout << "Ratio is " << t1/t2 << endl;
#if DO_DEBUG
  cout << "After sort: " << endl;
  for (int s = 0; s < n; s++) cout << a[s] << " ";
  cout << endl;
#endif
  
  for (int q = 0; q < n; q++){
#if DO_DEBUG
    cout << "numbers are " << a[q] << ' ' << b[q] << endl;
#endif
    if (a[q] != b[q]){
      cout << "ERROR!!!" << endl;
      exit(1);
    }
  }
  
}

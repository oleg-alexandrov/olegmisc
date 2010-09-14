#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <ctime>
using namespace std;

int randn(int n){
  // Return an integer in [0, n)
  assert(n >= 1);
  return rand()%n;
}

class queue{

public:
  // The queue has elems in [beg, end) where
  // these numbers could be negative.
  int beg, end;
  vector<int> lq, rq;

  queue(){
    beg = end = 0;
    lq.clear();
    rq.clear();
  }

  void print(){

    cout << "[ ";
    for (int s = beg; s < end; s++) cout << s << "\t";
    cout << "]" << endl;
    
    
    cout << "[ ";
    for (int s = beg; s < end; s++){
      if (s >= 0) cout << rq[s]    << "\t";
      else        cout << lq[-s-1] << "\t";
    }
    cout << "]" << endl;
    
  }

  void push_back(int val){

    cout << "push_back " << val << endl;

    end++;
    if (end > 0){
      rq.resize(end);
      rq[end-1] = val;
    }else{
      int ind = -end;
      assert((int)lq.size() >= ind + 1);
      lq[ind] = val;
    }

    print();

  }

  int pop_back(){

    assert(beg < end);
    end--;
    int val; 
    if (end >= 0){
      val = rq[end];
    }else{
      val = lq[-end-1];
    }

    cout << "pop_back " << val << endl;
    print();
    return val;
  }

  void push_front(int val){
    cout << "push_front " << val << endl;
    beg--;
    if (beg >= 0){
      rq[beg] = val;
    }else{
      int ind = -beg-1;
      lq.resize(ind+1);
      lq[ind] = val;
    }
    print();
  }

  int pop_front(){
    assert(beg < end);
    beg++;
    int val;
    if (beg > 0){
      val = rq[beg-1];
    }else{
      val = lq[-beg];
    }
    cout << "pop_front " << val << endl;
    print();
    return val;
  }

  
};


int main(){

  srand ( time(NULL) );

  queue A;
  A.print();
  
  A.push_back(3);
  A.push_back(4);
  A.push_back(5);
  A.push_front(2);
  A.push_front(1);
  A.push_back(6);

  A.pop_back();
  A.pop_front();
  A.pop_front();
  A.pop_front();
  A.pop_front();

  A.push_front(4);
  A.push_front(3);
  A.push_front(2);
  A.push_front(1);

  A.pop_back();
  A.pop_back();
  A.pop_back();
  A.pop_back();
  
  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

class cq{

public:
  cq();
  ~cq();
  void push_back(int val);
  int pop_front();
  bool isEmpty();
private:
  int * buffer;
  int N, b, e;
  bool m_isEmpty;
};

cq::cq(){
  b = 0; e = 0;
  N = 1;
  buffer = new int[N];
  m_isEmpty = true;
}

cq::~cq(){
  delete[] buffer; buffer = NULL;
}

bool cq::isEmpty(){
  return m_isEmpty;
}

void cq::push_back(int val){

  cout << "pushing: " << val << endl;
  
  if (!m_isEmpty && b == e){
    cout << "Queue is full, will resize" << endl;
    int N2 = 2*N;
    int * nb = new int [N2];
    int i = b, j = 0;
    while(1){
      nb[j] = buffer[i];
      j++;
      i = (i+1)%N;
      if (i == e) break;
    }
    delete buffer; buffer = nb;
    b = 0; e = j; N = N2;
  }

  cout << "N = " << N << endl;
  
  m_isEmpty = false;
  buffer[e] = val;
  e = (e + 1)%N;

  cout << "push: b and e are " << b << ' ' << e << endl << endl;
}

int cq::pop_front(){

  if (m_isEmpty){
    cout << "Queue is empty" << endl;
    return 0;
  }

  int val = buffer[b];
  b = (b + 1) % N;
  if (b == e) m_isEmpty = true;

  cout << "Popping: " << val << endl;
  cout << "b and e are " << b << ' ' << e << endl << endl;
  return val;
}

int main() {
  

  cq Q;
  Q.pop_front();

  Q.push_back(1);
  Q.pop_front();
  
  Q.push_back(2);
  Q.push_back(3);
  Q.push_back(4);
  Q.push_back(5);
  Q.push_back(6);
  Q.push_back(7);

  Q.pop_front();
  Q.pop_front();
  Q.pop_front();
  Q.pop_front();

  Q.push_back(8);
  Q.push_back(9);
  Q.push_back(10);
  Q.push_back(11);
  Q.push_back(12);
  Q.push_back(13);
  Q.push_back(14);
  Q.push_back(15);
  
  return 0;
}

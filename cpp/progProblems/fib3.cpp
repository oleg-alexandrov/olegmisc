#include <vector>
#include <iostream>
#include <cassert>
using namespace std;

int fib(int n, std::vector<int> & Table){

  assert(n >= 0);

  if ((int)Table.size() > n && Table[n] > 0) return Table[n];

  int val;
  if (n == 0 || n == 1) val = 1;
  else val = fib(n - 1, Table) + fib(n - 2, Table);

  Table.resize(n + 1);
  Table[n] = val;

  return val;
}

int main() {
 
  int n = 4;
  vector<int> Table;
  cout << "fib(" << n << ") = " << fib(n, Table) << endl;
  return 0;
  
}

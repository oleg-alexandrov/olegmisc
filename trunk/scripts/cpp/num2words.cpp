#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include <map>
using namespace std;

void init_numbers(map<int, string> & W){
  W.clear();
  W[0 ]   = "zero";
  W[1 ]   = "one";
  W[2 ]   = "two";
  W[3 ]   = "three";
  W[4 ]   = "four";
  W[5 ]   = "five";
  W[6 ]   = "six";
  W[7 ]   = "seven";
  W[8 ]   = "eight";
  W[9 ]   = "nine";
  W[10]   = "ten";
  W[11]   = "eleven";
  W[12]   = "twelve";
  W[13]   = "thirteen";
  W[14]   = "fourteen";
  W[15]   = "fifteen";
  W[16]   = "sixteen";
  W[17]   = "seventeen";
  W[18]   = "eighteen";
  W[19]   = "nineteen";
  W[20]   = "twenty";
  W[30]   = "thirty";
  W[40]   = "forty";
  W[50]   = "fifty";
  W[60]   = "sixty";
  W[70]   = "seventy";
  W[80]   = "eighty";
  W[90]   = "ninety";
  W[100]  = "hundred";
  W[1000] = "thousand";
}

string num2word(int n, map<int, string> & W){

  assert (n >= 0);

  if (n <= 20) return W[n];

  if (n <= 99){
    int d = n % 10;
    n -= d;
    string a = W[n];
    string b = (d == 0)? "" : " " + W[d];
    return a + b;
  }

  if (n <= 999){
    int d = n % 100;
    n = (n - d)/100;
    string a = W[n];
    string b = (d == 0)? "" : " and " + num2word(d, W);
    return a + " " + W[100] + b;
  }

  if (n <= 999999){
    int d = n % 1000;
    n = (n - d)/1000;
    string a = num2word(n, W);
    string b = (d == 0)? "" : " " + num2word(d, W);
    return a + " " + W[1000] + b;
  }
  
  return "";
}

int main(){

  map<int, string> W;
  init_numbers(W);
  
  for (int s = 41000; s <= 43227; s++){
    cout << s << ' ' << num2word(s, W) << endl;
  }
  
  return 0;
}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

// Parse an expression and extract the numerical value.

double eval_expr(const char * expr, int beg, int end){

  if (beg >= end) return 0;
    
  int plus_pos  = -1;
  int minus_pos = -1;
  int times_pos = -1;
  int div_pos   = -1;
  int nspc_pos  = -1;
  int lpar_pos  = -1;
  int rpar_pos  = -1;
  int par_diff  =  0;
  int par_mode  =  0;
  for (int s = beg; s < end; s++){

    if (expr[s] != ' '){
      if (nspc_pos < 0) nspc_pos = s;
    }
    
    //cout << expr[s] << endl;
    
    if (par_mode){
      if      (expr[s] == '(') par_diff++;
      else if (expr[s] == ')') par_diff--;
      
      if (par_diff == 0){
        assert(expr[s] == ')');
        if (rpar_pos < 0) rpar_pos = s;
        par_mode = 0;
      }
      continue;
    }

    if (expr[s] == '('){
      par_mode = 1;
      par_diff++;
      if (lpar_pos < 0) lpar_pos = s;
      continue;
    }

    assert(expr[s] != ')' && par_mode == 0 && par_diff == 0);

    if (expr[s] == '+' ){
      if (plus_pos < 0) plus_pos = s;
      continue;
    }
    
    if (expr[s] == '-'){
      minus_pos = s;
      continue;
    }

    if (expr[s] == '*'){
      if (times_pos < 0) times_pos = s;
      continue;
    }
    
    if (expr[s] == '/'){
      div_pos = s;
      continue;
    }

  }

  if (plus_pos >= 0){
    return eval_expr(expr, beg, plus_pos) + eval_expr(expr, plus_pos + 1, end);
  }

  if (minus_pos >= 0){
    return eval_expr(expr, beg, minus_pos) - eval_expr(expr, minus_pos + 1, end);
  }
  
  if (times_pos >= 0){
    return eval_expr(expr, beg, times_pos) * eval_expr(expr, times_pos + 1, end);
  }
  
  if (div_pos >= 0){
    return eval_expr(expr, beg, div_pos) / eval_expr(expr, div_pos + 1, end);
  }
  
  if (lpar_pos >= 0){
    return eval_expr(expr, lpar_pos + 1, rpar_pos);
  }

  if (nspc_pos < 0) return 0; // has only spaces
  
  return atof(expr + beg);
}

int main() {

  
  double val1 = - ((-19) + (-7) - 5 + (+8)/2.0 - (9 + 3.0/2.0) - 2.0/(-2.0/8.0/(3.0))*2.0 + 7 ); char expr[] = " - ((-19) + (-7) - 5 + (+8)/2.0 - (9 + 3.0/2.0) - 2.0/(-2.0/8.0/(3.0))*2.0 + 7 )";

  double val2 = eval_expr(expr, 0, strlen(expr));
  

  cout << "Vals are " << val1 << ' ' << val2 << endl;
  
  return 0;
}

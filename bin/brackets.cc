// Move this to VW
template<class T>
void find_outlier_brackets(std::vector<T> const& p, double k,
                           double & b, double & e){
  
  // Using the quartile range, determine the values b and e so that
  // all elements in p outsize of [b, e] are outliers.

  // If Q1 and Q3 are the lower and upper quartiles, keep points
  // between Q1-k*(Q3-Q1) and Q3 + k*(Q3-Q1).

  // This algorithm works best if the data is distributed rather
  // uniformly.
  
  // Suggested value of k: 1.5.
  
  b = 0.0; e = 0.0; // initialize
  std::vector<T> q = p;
  std::sort(q.begin(), q.end());
  int len = q.size();
  if (len <= 0) return;
  b = q[0]; e = q[len-1];
  if (len <= 3) return; // to few points for analysis
      
  int bn = int(round(0.25*len));
  int en = int(round(0.75*len))-1;
      
  double Q1 = q[bn];
  double Q3 = q[en];
  b = Q1 - k*(Q3-Q1);
  e = Q3 + k*(Q3-Q1);

  return;
}

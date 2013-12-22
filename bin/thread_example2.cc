#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
using namespace std;

#include <boost/thread.hpp>
#include <threadpool.hpp>

long long maxNumThreads = 10;

boost::mutex WriteLock; // Will manage data being written by threads

struct myClass{

  double m_sum;

  myClass(){
    m_sum = 0;
  }

  void fuseRange(long long first, long long last){
    double sum = 0;
    for (long long i = first; i < last; i++){
      sum += 1.0/sqrt(i*1.0+1);
    }

    {
      boost::unique_lock<boost::mutex> w_lock(WriteLock);
      m_sum += sum;
    }

  }

  double doWork(){
    long long big = (long long)atof(getenv("VAL"));
    std::cout << "big is " << big << std::endl;
    long long chunk = (long long)1e+6;

    boost::threadpool::pool tp(maxNumThreads);

    for (long long i = 0; i < big/chunk-1; i++){
      //tp.schedule(boost::bind(&myClass::fuseRange, this, i*chunk, i*chunk + chunk));
      fuseRange(i*chunk, i*chunk + chunk);
    }

    return m_sum;
  }

};

int main() {

  std::cout << "now in main nt!" << std::endl;
  myClass A;
  std::cout << "value is " << A.doWork() << std::endl;

  return 0;
}

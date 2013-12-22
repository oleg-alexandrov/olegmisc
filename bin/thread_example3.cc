#include <boost/asio.hpp>
#include <boost/thread.hpp>
#include <boost/thread.hpp>
#include <threadpool.hpp>

// Compute the sum 1/sqrt(i) using a thread pool
// Usage:
// export N=1e+10; export CHUNK=1e+3; export USE_THREADS=1; time_run.sh ./thread_example3

boost::mutex WriteLock; // Will manage data being written by threads

struct myClass{

  double m_sum;

  myClass(){
    m_sum = 0;
  }

  void calcPartialSum(long long first, long long last){

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
    long long big = (long long)atof(getenv("N"));
    long long chunk = (long long)atof(getenv("CHUNK"));
    bool useThreads = atof(getenv("USE_THREADS"));
    std::cout << "big and chunk is " << big << ' ' << chunk  << ' ' << useThreads << std::endl;
    {
      boost::threadpool::pool tp(12);
      if (useThreads){
        for (long long i = 0; i < big/chunk-1; i++){
          tp.schedule(boost::bind(&myClass::calcPartialSum, this, i*chunk, i*chunk + chunk));
        }
      }else{
        for (long long i = 0; i < big/chunk-1; i++){
          calcPartialSum(i*chunk, i*chunk + chunk);
        }
      }
    }

    return m_sum;
  }

};

int main()
{
  myClass A;
  std::cout << "value is " << A.doWork() << std::endl;

}

#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include <iostream>
#include <boost/thread.hpp>

using namespace std;

template<typename T>
class ThreadSomething{
private:
  T x;
  boost::thread t_;
  int first_, last_;

  void doSomething(int first, int last)
  {
    cout << boost::this_thread::get_id() << ": doing something" << endl;
    boost::this_thread::sleep(boost::posix_time::seconds(first));
  }

public:
  ThreadSomething(int first, int last) : first_(first) {}

  void start()
  {
    t_ = boost::thread(&ThreadSomething::doSomething, this, first_, last_);
  }

  void join()
  {
    t_.join();
  }
};


// void t06()
// {
//     cout << boost::this_thread::get_id() << ": creating a new thread"  << endl;
//     ThreadSomething ts(2);

//     cout << boost::this_thread::get_id() << ": starting a new thread"  << endl;
//     ts.start();

//     cout << boost::this_thread::get_id() << ": joining"  << endl;
//     ts.join();
//     cout << boost::this_thread::get_id() << ": done"  << endl;
// }

int main() {

//   if (argc <= 1){
//     std::cout << "Usage: " << argv[0] << " inputArg" << std::endl;
//     exit(1);
//   }

  return 0;
}

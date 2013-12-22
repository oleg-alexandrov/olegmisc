#include <boost/shared_array.hpp>

template <class T>
class MyClass{
  boost::shared_array<T> m_data;

  typedef typename boost::shared_array<T>::unspecified_bool_type unspecified_bool_type;
  operator unspecified_bool_type() const {
    return m_data;
  }
public:
  int get(){
    return 2;
  }
};


int main(){

  MyClass<int> M;
  std::cout << "value is " << M.get() << std::endl;
  return 0;
}

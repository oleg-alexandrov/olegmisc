#include <iostream>
#include <vector>
using namespace std;

struct A {};

struct B : virtual public A {};

struct C {};

struct D : public A, public C {};

int main()
{
    cout << "fun A:" << sizeof(void (A::*)()) << endl;
    cout << "fun B:" << sizeof(void (B::*)()) << endl;
    cout << "fun D:" << sizeof(void (D::*)()) << endl;

    cout << "size of A is " << sizeof(A) << endl;
    cout << "size of B is " << sizeof(B) << endl;
    cout << "size of C is " << sizeof(C) << endl;
    cout << "size of D is " << sizeof(D) << endl;

    cout << "size of A* is " << sizeof(A*) << endl;
    cout << "size of B* is " << sizeof(B*) << endl;
    cout << "size of C* is " << sizeof(C*) << endl;
    cout << "size of D* is " << sizeof(D*) << endl;

    cout << "size of size_t is " << sizeof(size_t) << endl;
}

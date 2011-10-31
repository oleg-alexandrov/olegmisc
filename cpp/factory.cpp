#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

class shape{
public:
  shape(){}
  virtual void plot() = 0;
};

class genAndPlotShapes{

public:
  
  void genAndPlot(int n){
    for (int q = 0; q < n; q++){
      shape * S = getShape();
      S->plot();
    }
    return;
  }

protected:
  virtual shape * getShape() = 0;

};


class triangle: public shape{
public:
  void plot(){cout << "Will plot a triangle" << endl;}
};

class genAndPlotTriangles: public genAndPlotShapes{
private:
  shape * getShape(){
    triangle * T = new triangle();
    return T;
  }
  
};

int main() {

  genAndPlotTriangles Obj;
  int n = 10;
  Obj.genAndPlot(n);
  
  return 0;
}

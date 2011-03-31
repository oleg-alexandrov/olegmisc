#ifndef GEOMUTILS_H
#define GEOMUTILS_H
#include <sstream>
#include <vector>
#include <fstream>
#include <cmath>
#include <set>
#include <cassert>
#include "dPoint.h"

struct anno {
  
  double x;
  double y;
  std::string label;
  
  void appendTo(std::ofstream & outfile) const{
    outfile << "anno " << x << ' ' << y << ' ' << label << std::endl;
  }

};

namespace utils{

  template<class T>
  const T * vecPtr(const std::vector<T>& X){
    if (X.size() == 0) return NULL;
    else               return &X.front();   
  }

  template<class T>
  T * vecPtr(std::vector<T>& X){
    if (X.size() == 0) return NULL;
    else               return &X.front();   
  }

  template<class T>
  std::string num2str(T num){
    std::ostringstream S;
    S << num;
    return S.str();
  }

  inline double distance(double x0, double y0, double x1, double y1){
    return sqrt( (x1 - x0)*(x1 - x0) + (y1 - y0)*(y1 - y0) );
  }
  
  inline int iround(double x){ return (int)round(x); }
  inline int iceil (double x){ return (int)ceil( x); }
  inline int ifloor(double x){ return (int)floor(x); }
  inline int isign (double x){
    if (x > 0) return  1;
    if (x < 0) return -1;
    return 0;
  }

  void snapPolyLineTo45DegAngles(bool isClosedPolyLine,
                                 int numVerts, double * xv, double * yv);
  void snapOneEdgeTo45(int numAngles, double* xs, double* ys,
                       bool snap2ndClosest, 
                       double & x0, double & y0,
                       double & x1, double & y1);
  
  void minDistFromPtToSeg(//inputs
                          double xin, double yin,
                          double x0, double y0,
                          double x1, double y1,
                          // outputs
                          double & minX, double & minY,
                          double & minDist
                          );
  
  void searchForLayer(std::string   lineStr, // input
                      std::string & layer    // output
                      );
  
  double signedPolyArea(int numV, const double* xv, const double* yv);
  
  void searchForColor(std::string lineStr, // input, not a reference on purpose
                      std::string & color  // output
                      );

  bool searchForAnnotation(std::string lineStr, anno & annotation);

  void expandBoxToGivenRatio(// inputs
                             double aspectRatio, 
                             // inputs/outputs
                             double & xll,  double & yll,
                             double & widx, double & widy);
  

  struct dRect{
    
    dRect(double xl_in = 0, double yl_in = 0,
          double xh_in = 0, double yh_in = 0): 
      xl(xl_in), yl(yl_in), xh(xh_in), yh(yh_in) {}
    
    double xl;
    double yl;
    double xh;
    double yh;
  };


  struct segDist{
    double begx, begy, endx, endy, dist;
    segDist(double begx_in, double begy_in, double endx_in, double endy_in, double dist_in):
      begx(begx_in), begy(begy_in), endx(endx_in), endy(endy_in), dist(dist_in){}
  };

  inline bool segDistGreaterThan(segDist s, segDist t){
    return (s.dist > t.dist);
  }

  bool boxesIntersect(double xl1, double yl1, double xh1, double yh1,
                      double xl2, double yl2, double xh2, double yh2
                      );

  
}
  

#endif
  

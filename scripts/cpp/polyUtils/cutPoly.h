#ifndef CUT_POLY_H
#define CUT_POLY_H

#include <vector>
#include <iostream>
#include <cmath>
#include <cstdlib>
#include <sstream>
#include <string>

namespace utils{

  // To do: move these low-level utilities to a different file
  template<class T>
  const T * vecPtr(const std::vector<T>& X){
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
  
  struct valIndex{
    double val;
    int    index;
    bool   isOutward;
    int    nextIndexInward; // Useful only when isOutward is true
  };

  inline int iround(double x){ return (int)round(x); }
  inline int iceil (double x){ return (int)ceil( x); }
  inline int ifloor(double x){ return (int)floor(x); }
  inline int isign (double x){
    if (x > 0) return  1;
    if (x < 0) return -1;
    return 0;
  }

  
  class dRect{
    
  public:
    dRect(double xll = 0, double yll = 0, double xur = 0, double yur = 0): 
      m_xll(xll), m_yll(yll), m_xur(xur), m_yur(yur) {}

    double left()   const { return m_xll; }
    double top()    const { return m_yll; }
    double right()  const { return m_xur; }
    double bottom() const { return m_yur; }
    double width()  const { return m_xur - m_xll;}
    double height() const { return m_yur - m_yll;}

  private:

    double m_xll;
    double m_yll;
    double m_xur;
    double m_yur;
  };

  inline void normalize(dRect & R){
    double left  = std::min(R.left(), R.right());
    double right = std::max(R.left(), R.right());
    double top   = std::min(R.top(), R.bottom());
    double bot   = std::max(R.top(), R.bottom());
    R            = dRect(left, top, right, bot);
  }
  
  void cutPoly(// inputs -- the polygons
               int numPolys, const int * numVerts,
               const double * xv, const double * yv,
               // inputs -- the cutting window
               double xll, double yll,
               double xur, double yur,
               // outputs -- the cut polygons
               std::vector< double> & cutX,
               std::vector< double> & cutY,
               std::vector< int>    & cutNumPolys);

  inline bool lessThan (valIndex A, valIndex B){ return A.val < B.val; }
  
  void processPointsOnCutline(std::vector<valIndex> & ptsOnCutline);

  void cutEdge(double x0, double y0, double x1, double y1,
               double nx, double ny, double H,
               double & cutx, double & cuty);
  
  
  void cutToHalfSpace(// inputs 
                      double nx, double ny, double dotH,
                      int numV, 
                      const double * xv, const double * yv,
                      // outputs -- the cut polygons
                      std::vector< double> & cutX,
                      std::vector< double> & cutY,
                      std::vector< int>    & cutNumPolys);

  
}
#endif

#ifndef CUT_POLY_H
#define CUT_POLY_H

#include <vector>
#include <iostream>
#include <cmath>
#include <cstdlib>

template<class T>
const T * vecPtr(const std::vector<T>& X){
  if (X.size() == 0) return NULL;
  else               return &X.front();   
}

namespace utils{

  struct valIndex{
    double val;
    int    index;
    bool   isOutward;
    bool   isDuplicate;
  };

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
  
  void procPtsOnCutline(std::vector<valIndex> & ptsOnCutline);

  double polygonArea(int n, const double * x, const double * y);

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

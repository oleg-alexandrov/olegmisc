#ifndef CUT_POLY_H
#define CUT_POLY_H

#include <vector>
#include <iostream>
#include <cmath>
#include <cstdlib>
#include <sstream>
#include <string>
#include "geomUtils.h"

namespace utils{

  struct valIndex{
    double val;
    int    index;
    bool   isOutward;
    int    nextIndexInward; // Useful only when isOutward is true
  };

  void cutPoly(// inputs -- the polygons
               int numPolys, const int * numVerts,
               const double * xv, const double * yv,
               // inputs -- the cutting window
               double xll, double yll, double xur, double yur,
               bool isPolyClosed,
               // outputs -- the cut polygons
               std::vector< double> & cutX,
               std::vector< double> & cutY,
               std::vector< int>    & cutNumPolys);

  inline bool lessThan (valIndex A, valIndex B){ return A.val < B.val; }
  
  void processPointsOnCutline(std::vector<valIndex> & ptsOnCutline);

  void cutToHalfSpace(// inputs 
                      double nx, double ny, double dotH,
                      int numV, 
                      const double * xv, const double * yv,
                      bool isPolyClosed,
                      // outputs -- the cut polygons
                      std::vector<double> & cutX,
                      std::vector<double> & cutY,
                      std::vector<int>    & cutNumPolys);

  
}
#endif

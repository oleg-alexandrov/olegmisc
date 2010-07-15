#ifndef CUT_POLY_H
#define CUT_POLY_H

#include <vector>
#include <iostream>
#include <cmath>
#include <cstdlib>

namespace utils{
  void cutEdge(double x0, double y0, double x1, double y1, double H,
             double & cutx, double & cuty);
  
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
  
void cutToHalfSpace(// inputs 
                    int numV, 
                    const double * xv, const double * yv,
                    double H, // cutting line -- cut to the left
                    // outputs -- the cut polygons
                    std::vector< double> & cutX,
                    std::vector< double> & cutY,
                    std::vector< int>    & cutNumPolys);
}
#endif

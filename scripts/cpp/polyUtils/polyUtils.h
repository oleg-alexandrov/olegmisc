#ifndef POLY_UTILS_H
#define POLY_UTILS_H
#include <sstream>
#include <vector>
#include <fstream>
#include <cmath>
#include <set>
#include "dPoint.h"
#include "dPoly.h"
#include "geomUtils.h"

namespace utils{

  void findPolyDiff(const dPoly & P, const dPoly & Q, // inputs
                    std::vector<dPoint> & vP, std::vector<dPoint> & vQ // outputs
                    );
  

  void findClosestPointAndDist(// inputs
                               double x0, double y0,
                               std::vector<dPoly> & polyVec,
                               // outputs
                               double & min_x, double & min_y,
                               double & min_dist
                               );
  
  void putPolyInMultiSet(const dPoly & P, std::multiset<dPoint> & mP);

  void findClosestPolyAndDist(// inputs
                              double x0, double y0,
                              std::vector<dPoly> & polyVec,
                              // outputs
                              int & minVecIndex, int & minPolyIndex,
                              double & min_dist
                              );


}
  

#endif
  

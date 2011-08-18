#ifndef POLY_UTILS_H
#define POLY_UTILS_H
#include <sstream>
#include <vector>
#include <fstream>
#include <cmath>
#include <set>
#include "dPoly.h"
#include "geomUtils.h"
#include "dTree.h"

namespace utils{

  void findPolyDiff(const dPoly & P, const dPoly & Q, // inputs
                    std::vector<dPoint> & vP, std::vector<dPoint> & vQ // outputs
                    );
  

  void findClosestPolyVertex(// inputs
                             double x0, double y0,
                             const std::vector<dPoly> & polyVec,
                             // outputs
                             int & polyVecIndex,
                             int & polyIndexInCurrPoly,
                             int & vertIndexInCurrPoly,
                             double & minX, double & minY,
                             double & minDist
                             );
  
  void findDistanceBwPolys(// inputs
                           const dPoly & poly1,
                           const dPoly & poly2,
                           // outputs
                           std::vector<segDist> & distVec
                           );
  
  void findDistanceFromPoly1ToPoly2(// inputs
                                    const dPoly & poly1,
                                    const dPoly & poly2,
                                    // outputs
                                    std::vector<segDist> & distVec
                                    );

  void findDistanceBwPolysBruteForce(// inputs
                                     const dPoly & poly1,
                                     const dPoly & poly2,
                                     // outputs
                                     std::vector<segDist> & distVec
                                     );
  
  void putPolyInMultiSet(const dPoly & P, std::multiset<dPoint> & mP);

  void findClosestPolyEdge(// inputs
                           double x0, double y0,
                           const std::vector<dPoly> & polyVec,
                           // outputs
                           int & vecIndex, int & polyIndex,
                           int & vertIndex,
                           double & minX, double & minY, double & minDist
                           );
  void alignPoly1ToPoly2(dPoly       & poly1,
                              const dPoly & poly2);
  void findDistanceFromVertsOfPoly1ToVertsPoly2(// inputs
                                                const dPoly & poly1,
                                                const dPoly & poly2,
                                                // outputs
                                                std::vector<segDist> & distVec
                                                );


  void setUpViewBox(// inputs
                    const std::vector<dPoly> & polyVec,
                    // outputs
                    double & xll,  double & yll,
                    double & widx, double & widy);
  
}
  

#endif
  

#ifndef POLY_UTILS_H
#define POLY_UTILS_H
#include <sstream>
#include <vector>
#include <fstream>
#include <cmath>
#include <set>
#include "dPoly.h"
#include "geomUtils.h"
#include "boxTree.h"

namespace utils{

  void findPolyDiff(const dPoly & P, const dPoly & Q, // inputs
                    std::vector<dPoint> & vP, std::vector<dPoint> & vQ // outputs
                    );
  

  void findClosestPolyVertex(// inputs
                             double x0, double y0,
                             const std::vector<dPoly> & polyVec,
                             // outputs
                             double & minX, double & minY,
                             double & minDist
                             );

  void findAndSortDistsBwPolysNew(// inputs
                                  const dPoly & poly1,
                                  const dPoly & poly2,
                                  // outputs
                                  std::vector<segDist> & distVec
                                  );

  void findAndSortDistsBwPolys(// inputs
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
                           int & minVecIndex, int & minPolyIndex,
                           double & minX, double & minY, double & minDist
                           );


}
  
class edgeTree{
  
public:
  
  void putPolyEdgesInTree(const dPoly & poly);
  void findPolyEdgesInBox(// inputs
                          double xl, double yl,
                          double xh, double yh,
                          std::vector<utils::seg> & edgesInBox
                          );
  
private:

  // Internal data structures
  boxTree<utils::dRectWithId>      m_boxesTree;
  std::vector<utils::dRectWithId>  m_allEdges; 
  std::vector<utils::dRectWithId>  m_boxesInRegion;
  
};


#endif
  

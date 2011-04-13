#ifndef EDGE_TREE_H
#define EDGE_TREE_H

#include "boxTree.h"
#include "geomUtils.h"

class dPoly;

class edgeTree{
  
public:
  
  void putPolyEdgesInTree(const dPoly & poly);

  void findPolyEdgesInBox(// inputs
                          double xl, double yl,
                          double xh, double yh,
                          // outputs
                          std::vector<utils::seg> & edgesInBox
                          );
  
  void findClosestEdgeToPoint(// inputs
                              double x0, double y0,
                              // outputs
                              utils::seg & closestEdge,
                              double     & closestDist,
                              // The location on the closest
                              // edge where closestDist is achieved
                              double & closestX, double & closestY
                              );
  
private:

  inline void edgeToBox(//inputs
                        double bx, double by, double ex, double ey,
                        // outputs
                        utils::dRectWithId & R
                        ){
    // Given an edge, compute its bounding box and the info necessary info
    // to reverse this transformation, see boxToEdge.
    int id = 0;
    if (bx > ex){ std::swap(bx, ex); id |= 1; } // id = id | 01
    if (by > ey){ std::swap(by, ey); id |= 2; } // id = id | 10
    R = utils::dRectWithId(bx, by, ex, ey, id);
    return;
  }
  
  inline void boxToEdge(// inputs
                        const utils::dRectWithId & R, 
                        // outputs
                        double & bx, double & by, double & ex, double & ey
                        ){
    // Recover the edge based on the bounding box and id. The inverse of edgeToBox.
    bx = R.xl; by = R.yl; ex = R.xh; ey = R.yh;
    int id = R.id;
    if (id & 1) std::swap (bx, ex);
    if (id & 2) std::swap (by, ey);
  }
  
  void findClosestEdgeToPointInternal(// inputs
                                      double x0, double y0,
                                      boxNode<utils::dRectWithId> * root,
                                      // outputs
                                      utils::seg & closestEdge,
                                      double     & closestDist
                                      );
  // Internal data structures
  boxTree<utils::dRectWithId>      m_boxTree;
  std::vector<utils::dRectWithId>  m_allEdges; 
  std::vector<utils::dRectWithId>  m_boxesInRegion;
  
};


#endif
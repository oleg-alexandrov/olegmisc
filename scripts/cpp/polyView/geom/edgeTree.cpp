#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstring>
#include "edgeUtils.h"
#include "edgeTree.h"
#include "geomUtils.h"
#include "dPoly.h"
using namespace std;
using namespace utils;

// Utilities for 
// * putting the edges of a polygon in a tree
// * fast searching for the edges in a given box
// * fast searching for the closest edge to the given point.

void edgeTree::putPolyEdgesInTree(const dPoly & poly){

  const double * xv       = poly.get_xv();
  const double * yv       = poly.get_yv();
  const int    * numVerts = poly.get_numVerts();
  int numPolys            = poly.get_numPolys();
  int totalNumVerts       = poly. get_totalNumVerts();
  
  m_allEdges.resize(totalNumVerts);
  
  int start = 0;
  for (int pIter = 0; pIter < numPolys; pIter++){
      
    if (pIter > 0) start += numVerts[pIter - 1];

    for (int vIter = 0; vIter < numVerts[pIter]; vIter++){

      int vIter2 = (vIter + 1) % numVerts[pIter];
      double bx = xv[start + vIter ], by = yv[start + vIter ];
      double ex = xv[start + vIter2], ey = yv[start + vIter2];

      // Transform an edge into a box, with the id storing
      // the information necessary to reverse this later.
      dRectWithId R; 
      edgeToBdBox(//inputs
                  bx, by, ex, ey, // inputs  
                  R               // output
                  );
      m_allEdges[start + vIter] = R;
      
    }
  }
  
  // Form the tree. Boxes will be reordered but otherwise unchanged
  // inside of this function. Do not modify the vector m_allEdges
  // afterward.
  m_boxTree.formTree(m_allEdges);

  return;
}

void edgeTree::findPolyEdgesInBox(// inputs
                                  double xl, double yl,
                                  double xh, double yh,
                                  std::vector<utils::seg> & edgesInBox
                                  ){
  
  // Search the tree
  m_boxTree.getBoxesInRegion(xl, yl, xh, yh, m_boxesInRegion);

  // Save the edges in the box
  edgesInBox.clear();
  for (int s = 0; s < (int)m_boxesInRegion.size(); s++){

    const dRectWithId & R = m_boxesInRegion[s]; // alias
    double bx, by, ex, ey;
    bdBoxToEdge(R,             // input
                bx, by, ex, ey // outputs
                );

    bool res = edgeIntersectsBox(bx, by, ex, ey,  // arbitrary edge (input)
                                 xl, yl, xh, yh   // box to intersect (input)
                                 );
    if (res) edgesInBox.push_back(seg(bx, by, ex, ey));
  }

  return;
}

void edgeTree::findClosestEdgeToPoint(// inputs
                                      double x0, double y0,
                                      // outputs
                                      utils::seg & closestEdge,
                                      double     & closestDistance
                                      ){

  // Fast searching for the closest edge to a given point. We assume
  // that the tree of edges is formed by now. The idea is the
  // following: putting the edges in a tree creates a hierarchical
  // partition of the plane. When searching for the closest edge
  // through the regions in this partition, search first the regions
  // closest to the point, and skip altogether the regions which are
  // too far.

  // This function returns DBL_MAX for the closest distance if there
  // are no edges to search.
  
  // Note: This function uses internal details of the boxTree
  // implementation. There's got to be better ways of doing this.
  
  boxNode<dRectWithId> * root = m_boxTree.getTreeRoot();
  closestDistance = DBL_MAX;
  edgeTree::findClosestEdgeToPointInternal(// inputs
                                           x0, y0, root,  
                                           // outputs
                                           closestEdge, closestDistance
                                           );
}

void edgeTree::findClosestEdgeToPointInternal(// inputs
                                              double x0, double y0,
                                              boxNode<utils::dRectWithId> * root,
                                              // outputs
                                              utils::seg & closestEdge,
                                              double     & closestDistance
                                              ){

  if (root == NULL) return;
  
  dRectWithId R = root->Rect;
  double bx, by, ex, ey;
  bdBoxToEdge(R,             // input
              bx, by, ex, ey // outputs
              );
  
}
